//! Struktur zum Speichern aller Gleise

use std::{collections::HashMap, fmt::Debug, iter, marker::PhantomData};

use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree, RTreeObject, SelectionFunction, AABB,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Reserviere, Reserviert, Serialisiere},
        polarität::Fließend,
    },
    application::{
        gleis::{
            gerade::{Gerade, GeradeSerialisiert},
            gleise::{
                id::{AnyId, AnyIdRef, GleisIdRef, StreckenabschnittId, StreckenabschnittIdRef},
                StreckenabschnittFehler,
            },
            kreuzung::{Kreuzung, KreuzungSerialisiert},
            kurve::{Kurve, KurveSerialisiert},
            verbindung::Verbindung,
            weiche::{
                dreiwege::{DreiwegeWeiche, DreiwegeWeicheSerialisiert},
                gerade::{Weiche, WeicheSerialisiert},
                kurve::{KurvenWeiche, KurvenWeicheSerialisiert},
                s_kurve::{SKurvenWeiche, SKurvenWeicheSerialisiert},
            },
        },
        typen::*,
    },
    lookup::Lookup,
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit},
        plan::Plan,
        streckenabschnitt::{self, Streckenabschnitt},
    },
};

use super::GeschwindigkeitEntferntFehler;

pub mod de_serialisieren;
pub mod v2;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
}

impl<T: Serialisiere> Serialisiere for Gleis<T> {
    type Serialisiert = Gleis<T::Serialisiert>;

    fn serialisiere(&self) -> Self::Serialisiert {
        Gleis { definition: self.definition.serialisiere(), position: self.position.clone() }
    }

    fn anschlüsse(
        self,
    ) -> (
        Vec<crate::anschluss::pwm::Pin>,
        Vec<crate::anschluss::OutputAnschluss>,
        Vec<crate::anschluss::InputAnschluss>,
    ) {
        self.definition.anschlüsse()
    }
}

impl<R, T: Reserviere<R>> Reserviere<Gleis<R>> for Gleis<T> {
    fn reserviere(
        self,
        anschlüsse: &mut crate::anschluss::Anschlüsse,
        pwm_pins: Vec<crate::anschluss::pwm::Pin>,
        output_anschlüsse: Vec<crate::anschluss::OutputAnschluss>,
        input_anschlüsse: Vec<crate::anschluss::InputAnschluss>,
    ) -> anschluss::de_serialisieren::Result<Gleis<R>> {
        let Reserviert {
            anschluss: definition_reserviert,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.definition.reserviere(
            anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(Reserviert {
            anschluss: Gleis { definition: definition_reserviert, position: self.position },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

pub(in crate::application::gleis::gleise) type StreckenabschnittMap<Z> =
    HashMap<streckenabschnitt::Name, (Streckenabschnitt, Fließend, GleiseDaten<Z>)>;
type GeschwindigkeitMap<Z: Zugtyp> =
    HashMap<geschwindigkeit::Name, (Geschwindigkeit<Z::Leiter>, StreckenabschnittMap<Z>)>;
pub struct Zustand<Z: Zugtyp> {
    pub(crate) ohne_streckenabschnitt: GleiseDaten<Z>,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMap<Z>,
    pub(crate) geschwindigkeiten: GeschwindigkeitMap<Z>,
}

impl<Z> Debug for Zustand<Z>
where
    Z: Zugtyp,
    <Z as Zugtyp>::Leiter: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Zustand")
            .field("ohne_streckenabschnitt", &self.ohne_streckenabschnitt)
            .field("ohne_geschwindigkeit", &self.ohne_geschwindigkeit)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .finish()
    }
}

impl<Z: Zugtyp> Zustand<Z> {
    pub fn neu() -> Self {
        Zustand {
            ohne_streckenabschnitt: GleiseDaten::neu(),
            ohne_geschwindigkeit: StreckenabschnittMap::new(),
            geschwindigkeiten: GeschwindigkeitMap::new(),
        }
    }

    pub(in crate::application::gleis::gleise) fn streckenabschnitt_map(
        &self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
    ) -> Result<&StreckenabschnittMap<Z>, GeschwindigkeitEntferntFehler> {
        Ok(if let Some(name) = geschwindigkeit {
            &self
                .geschwindigkeiten
                .get(name)
                .ok_or_else(|| GeschwindigkeitEntferntFehler(name.clone()))?
                .1
        } else {
            &self.ohne_geschwindigkeit
        })
    }

    pub(in crate::application::gleis::gleise) fn streckenabschnitt_map_mut(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
    ) -> Result<&mut StreckenabschnittMap<Z>, GeschwindigkeitEntferntFehler> {
        Ok(if let Some(name) = geschwindigkeit {
            &mut self
                .geschwindigkeiten
                .get_mut(name)
                .ok_or_else(|| GeschwindigkeitEntferntFehler(name.clone()))?
                .1
        } else {
            &mut self.ohne_geschwindigkeit
        })
    }

    pub(in crate::application::gleis::gleise) fn daten(
        &self,
        streckenabschnitt: &Option<StreckenabschnittId>,
    ) -> Result<&GleiseDaten<Z>, StreckenabschnittFehler> {
        Ok(if let Some(streckenabschnitt_id) = streckenabschnitt {
            let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt_id;
            let streckenabschnitt_map = self.streckenabschnitt_map(geschwindigkeit.as_ref())?;
            &streckenabschnitt_map
                .get(name)
                .ok_or_else(|| {
                    StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt_id.clone())
                })?
                .2
        } else {
            &self.ohne_streckenabschnitt
        })
    }
    pub(in crate::application::gleis::gleise) fn daten_mut(
        &mut self,
        streckenabschnitt: &Option<StreckenabschnittId>,
    ) -> Result<&mut GleiseDaten<Z>, StreckenabschnittFehler> {
        Ok(if let Some(streckenabschnitt_id) = streckenabschnitt {
            let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt_id;
            let streckenabschnitt_map = self.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
            &mut streckenabschnitt_map
                .get_mut(name)
                .ok_or_else(|| {
                    StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt_id.clone())
                })?
                .2
        } else {
            &mut self.ohne_streckenabschnitt
        })
    }

    pub(crate) fn alle_streckenabschnitt_daten<'t>(
        &'t self,
    ) -> impl Iterator<Item = (Option<StreckenabschnittIdRef<'t>>, &'t GleiseDaten<Z>)> {
        iter::once((None, &self.ohne_streckenabschnitt))
            .chain(self.ohne_geschwindigkeit.iter().map(
                |(name, (_streckenabschnitt, _fließend, daten))| {
                    (Some(StreckenabschnittIdRef { geschwindigkeit: None, name }), daten)
                },
            ))
            .chain(self.geschwindigkeiten.iter().flat_map(
                |(geschwindigkeit_name, (_geschwindigkeit, map))| {
                    map.iter().map(move |(name, (_streckenabschnitt, _fließend, daten))| {
                        (
                            Some(StreckenabschnittIdRef {
                                geschwindigkeit: Some(geschwindigkeit_name),
                                name,
                            }),
                            daten,
                        )
                    })
                },
            ))
    }

    pub(crate) fn alle_streckenabschnitt_und_daten<'t>(
        &'t self,
    ) -> impl Iterator<
        Item = (
            StreckenabschnittIdRef<'t>,
            &'t Streckenabschnitt,
            &'t Fließend,
            &'t GleiseDaten<Z>,
        ),
    > {
        let iter_map = |geschwindigkeit: Option<&'t _>| {
            move |(name, (streckenabschnitt, fließend, daten)): (&'t _, &'t (_, _, _))| {
                (
                    StreckenabschnittIdRef { geschwindigkeit, name },
                    streckenabschnitt,
                    fließend,
                    daten,
                )
            }
        };
        self.ohne_geschwindigkeit.iter().map(iter_map(None)).chain(
            self.geschwindigkeiten.iter().flat_map(
                move |(geschwindigkeit_name, (_geschwindigkeit, map))| {
                    map.iter().map(iter_map(Some(geschwindigkeit_name)))
                },
            ),
        )
    }

    pub(crate) fn alle_geschwindigkeit_streckenabschnitt_daten<'t>(
        &'t self,
    ) -> impl Iterator<Item = (Option<StreckenabschnittIdRef<'t>>, &'t GleiseDaten<Z>)> {
        iter::once((None, &self.ohne_streckenabschnitt))
            .chain(self.ohne_geschwindigkeit.iter().map(
                |(name, (_streckenabschnitt, _fließend, daten))| {
                    (Some(StreckenabschnittIdRef { geschwindigkeit: None, name }), daten)
                },
            ))
            .chain(self.geschwindigkeiten.iter().flat_map(
                |(geschwindigkeit, (_geschwindigkeit, map))| {
                    map.iter().map(move |(name, (_streckenabschnitt, _fließend, daten))| {
                        (
                            Some(StreckenabschnittIdRef {
                                geschwindigkeit: Some(geschwindigkeit),
                                name,
                            }),
                            daten,
                        )
                    })
                },
            ))
    }

    /// Alle Verbindungen in der Nähe der übergebenen Position.
    /// Der erste Rückgabewert sind alle `Verbindung`en in der Nähe,
    /// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
    pub(in crate::application::gleis::gleise) fn überlappende_verbindungen<'t>(
        &'t self,
        verbindung: &'t Verbindung,
        eigene_id: &'t AnyIdRef<'t, Z>,
        gehalten_id: Option<&'t AnyId<Z>>,
    ) -> (impl Iterator<Item = Verbindung> + 't, bool) {
        let mut gehalten = false;
        let überlappend = self.alle_geschwindigkeit_streckenabschnitt_daten().flat_map(
            move |(streckenabschnitt, daten)| {
                macro_rules! überlappende_verbindungen {
                    ($gleis: ident) => {{
                        let (überlappend_daten, gehalten_daten) = daten
                            .überlappende_verbindungen::<$gleis<Z>>(
                                verbindung,
                                streckenabschnitt.clone(),
                                &eigene_id,
                                gehalten_id,
                            );
                        gehalten = gehalten || gehalten_daten;
                        überlappend_daten
                    }};
                }
                let überlappend_gerade = überlappende_verbindungen!(Gerade);
                let überlappend_kurve = überlappende_verbindungen!(Kurve);
                let überlappend_weiche = überlappende_verbindungen!(Weiche);
                let überlappend_dreiwege_weiche = überlappende_verbindungen!(DreiwegeWeiche);
                let überlappend_kurven_weiche = überlappende_verbindungen!(KurvenWeiche);
                let überlappend_s_kurven_weiche = überlappende_verbindungen!(SKurvenWeiche);
                let überlappend_kreuzung = überlappende_verbindungen!(Kreuzung);
                überlappend_gerade
                    .chain(überlappend_kurve)
                    .chain(überlappend_weiche)
                    .chain(überlappend_dreiwege_weiche)
                    .chain(überlappend_kurven_weiche)
                    .chain(überlappend_s_kurven_weiche)
                    .chain(überlappend_kreuzung)
            },
        );
        (überlappend, gehalten)
    }
}

pub(crate) type RStern<T> = RTree<GeomWithData<Rectangle<Vektor>, Gleis<T>>>;
#[derive(zugkontrolle_derive::Debug)]
pub(crate) struct GleiseDaten<Z> {
    pub(crate) geraden: RStern<Gerade<Z>>,
    pub(crate) kurven: RStern<Kurve<Z>>,
    pub(crate) weichen: RStern<Weiche<Z>>,
    pub(crate) dreiwege_weichen: RStern<DreiwegeWeiche<Z>>,
    pub(crate) kurven_weichen: RStern<KurvenWeiche<Z>>,
    pub(crate) s_kurven_weichen: RStern<SKurvenWeiche<Z>>,
    pub(crate) kreuzungen: RStern<Kreuzung<Z>>,
}

impl<Z> GleiseDaten<Z> {
    /// Erstelle eine leere `GleiseDaten`-Struktur.
    pub fn neu() -> Self {
        GleiseDaten {
            geraden: RStern::new(),
            kurven: RStern::new(),
            weichen: RStern::new(),
            dreiwege_weichen: RStern::new(),
            kurven_weichen: RStern::new(),
            s_kurven_weichen: RStern::new(),
            kreuzungen: RStern::new(),
        }
    }

    /// Füge alle Elemente von `other` zu `self` hinzu.
    pub fn verschmelze(&mut self, mut other: GleiseDaten<Z>) {
        macro_rules! extend {
            ($($rstern: ident),*) => {
                $(while let Some(gleis) = other.$rstern.remove_with_selection_function(SelectAll) {
                    self.$rstern.insert(gleis)
                })*
            };
        }
        extend! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
    }

    /// Alle Verbindungen in der Nähe der übergebenen Position im zugehörigen `RStern`.
    /// Der erste Rückgabewert sind alle `Verbindung`en in der Nähe,
    /// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
    fn überlappende_verbindungen<'t, T>(
        &'t self,
        verbindung: &'t Verbindung,
        streckenabschnitt: Option<StreckenabschnittIdRef<'t>>,
        eigene_id: &'t AnyIdRef<'t, Z>,
        gehalten_id: Option<&'t AnyId<Z>>,
    ) -> (impl Iterator<Item = Verbindung> + 't, bool)
    where
        T: Zeichnen + DatenAuswahl<Z> + 't,
        AnyIdRef<'t, Z>: From<GleisIdRef<'t, T>>,
    {
        let vektor_genauigkeit = Vektor {
            x: ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT,
            y: ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT,
        };
        let kandidaten_rechteck = Rechteck {
            ecke_a: verbindung.position + vektor_genauigkeit,
            ecke_b: verbindung.position - vektor_genauigkeit,
        };
        let kandidaten = self
            .rstern::<T>()
            .locate_in_envelope_intersecting(&Rectangle::from(kandidaten_rechteck).envelope());
        let mut gehalten = false;
        let überlappend = kandidaten.flat_map(move |kandidat| {
            let rectangle = kandidat.geom();
            let kandidat_id = AnyIdRef::from(GleisIdRef {
                rectangle,
                streckenabschnitt: streckenabschnitt.clone(),
                phantom: PhantomData::<fn() -> T>,
            });
            let mut überlappend = Vec::new();
            if &kandidat_id != eigene_id {
                let kandidat_verbindungen = kandidat
                    .data
                    .definition
                    .verbindungen_an_position(kandidat.data.position.clone());
                for (_kandidat_name, kandidat_verbindung) in kandidat_verbindungen.refs() {
                    if (verbindung.position - kandidat_verbindung.position).länge()
                        < ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT
                    {
                        überlappend.push(kandidat_verbindung.clone())
                    }
                }
            }
            if !gehalten && gehalten_id.map_or(false, |id| id == &kandidat_id) {
                gehalten = true;
            }
            überlappend.into_iter()
        });
        (überlappend, gehalten)
    }
}

const ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT: Skalar = Skalar(5.);

/// SelectionFunction, die jedes Element akzeptiert.
/// Haupt-Nutzen ist das vollständiges Leeren eines RTree (siehe `GleiseDaten::verschmelze`).
pub(crate) struct SelectAll;
impl<T: RTreeObject> SelectionFunction<T> for SelectAll {
    fn should_unpack_parent(&self, _envelope: &T::Envelope) -> bool {
        true
    }
}

/// SelectionFunction, die einen bestimmten Envelope sucht.
pub(crate) struct SelectEnvelope(pub(crate) AABB<Vektor>);
impl<T> SelectionFunction<T> for SelectEnvelope
where
    T: RTreeObject<Envelope = AABB<Vektor>>,
{
    fn should_unpack_parent(&self, envelope: &T::Envelope) -> bool {
        let self_upper = self.0.upper();
        let self_lower = self.0.lower();
        let upper = envelope.upper();
        let lower = envelope.lower();
        // der gesuchte Envelope muss komplett in den parent passen
        lower.x <= self_lower.x
            && lower.y <= self_lower.y
            && upper.x >= self_upper.x
            && upper.y >= self_upper.y
    }

    fn should_unpack_leaf(&self, leaf: &T) -> bool {
        self.0 == leaf.envelope()
    }
}

/// Trait um eine Referenz auf die Map für den jeweiligen Typ zu bekommen.
/// Kein schönes API, daher nur crate-public.
pub(crate) trait DatenAuswahl<Z>: Sized {
    fn rstern(gleise: &GleiseDaten<Z>) -> &RStern<Self>;
    fn rstern_mut(gleise: &mut GleiseDaten<Z>) -> &mut RStern<Self>;
}
impl<Z> GleiseDaten<Z> {
    #[inline(always)]
    pub(crate) fn rstern<T: DatenAuswahl<Z>>(&self) -> &RStern<T> {
        T::rstern(self)
    }
    #[inline(always)]
    pub(crate) fn rstern_mut<T: DatenAuswahl<Z>>(&mut self) -> &mut RStern<T> {
        T::rstern_mut(self)
    }
}
impl<Z> DatenAuswahl<Z> for Gerade<Z> {
    fn rstern(GleiseDaten { geraden, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        geraden
    }
    fn rstern_mut(GleiseDaten { geraden, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        geraden
    }
}
impl<Z> DatenAuswahl<Z> for Kurve<Z> {
    fn rstern(GleiseDaten { kurven, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kurven
    }
    fn rstern_mut(GleiseDaten { kurven, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        kurven
    }
}
impl<Z> DatenAuswahl<Z> for Weiche<Z> {
    fn rstern(GleiseDaten { weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        weichen
    }
    fn rstern_mut(GleiseDaten { weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        weichen
    }
}
impl<Z> DatenAuswahl<Z> for DreiwegeWeiche<Z> {
    fn rstern(GleiseDaten { dreiwege_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        dreiwege_weichen
    }
    fn rstern_mut(GleiseDaten { dreiwege_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        dreiwege_weichen
    }
}
impl<Z> DatenAuswahl<Z> for KurvenWeiche<Z> {
    fn rstern(GleiseDaten { kurven_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kurven_weichen
    }
    fn rstern_mut(GleiseDaten { kurven_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        kurven_weichen
    }
}
impl<Z> DatenAuswahl<Z> for SKurvenWeiche<Z> {
    fn rstern(GleiseDaten { s_kurven_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        s_kurven_weichen
    }
    fn rstern_mut(GleiseDaten { s_kurven_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        s_kurven_weichen
    }
}
impl<Z> DatenAuswahl<Z> for Kreuzung<Z> {
    fn rstern(GleiseDaten { kreuzungen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kreuzungen
    }
    fn rstern_mut(GleiseDaten { kreuzungen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        kreuzungen
    }
}
