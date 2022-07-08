//! Struktur zum Speichern aller Gleise.

use std::{collections::HashMap, fmt::Debug, iter, marker::PhantomData};

use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree, RTreeObject, SelectionFunction, AABB,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    },
    gleis::{
        gerade::Gerade,
        gleise::{
            id::{
                AnyId, AnyIdRef, GleisId, GleisIdRef, StreckenabschnittId, StreckenabschnittIdRef,
            },
            GeschwindigkeitEntferntFehler, GleisIdFehler, StreckenabschnittIdFehler,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        verbindung,
        verbindung::Verbindung,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    nachschlagen::Nachschlagen,
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        plan::{self, Plan},
        streckenabschnitt::{self, Streckenabschnitt},
    },
    typen::{
        canvas::Position,
        mm::Spurweite,
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        Zeichnen,
    },
    zugtyp::Zugtyp,
};

pub mod de_serialisieren;
pub mod v2;

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [Anschlüsse](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [Canvas](crate::application::touch_canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

impl<T: Serialisiere<S>, S> Serialisiere<S> for Gleis<T> {
    fn serialisiere(&self) -> Gleis<S> {
        Gleis { definition: self.definition.serialisiere(), position: self.position.clone() }
    }

    fn anschlüsse(self) -> Anschlüsse {
        self.definition.anschlüsse()
    }
}

impl<R, T: Reserviere<R>> Reserviere<Gleis<R>> for Gleis<T> {
    type Arg = <T as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<Gleis<R>> {
        let Gleis { definition, position } = self;
        definition
            .reserviere(lager, anschlüsse, arg)
            .konvertiere(|definition| Gleis { definition, position })
    }
}

pub(crate) type StreckenabschnittMap =
    HashMap<streckenabschnitt::Name, (Streckenabschnitt, GleiseDaten)>;
type GeschwindigkeitMap<Leiter> =
    HashMap<geschwindigkeit::Name, (Geschwindigkeit<Leiter>, StreckenabschnittMap)>;

/// Alle Gleise, [Geschwindigkeiten](Geschwindigkeit) und [Streckenabschnitte](Streckenabschnitt),
/// sowie der verwendete [Zugtyp].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub(in crate::gleis::gleise) struct Zustand<L: Leiter> {
    pub(in crate::gleis::gleise) zugtyp: Zugtyp<L>,
    pub(in crate::gleis::gleise) ohne_streckenabschnitt: GleiseDaten,
    pub(in crate::gleis::gleise) ohne_geschwindigkeit: StreckenabschnittMap,
    pub(in crate::gleis::gleise) geschwindigkeiten: GeschwindigkeitMap<L>,
    pub(in crate::gleis::gleise) pläne: HashMap<plan::Name, Plan<L>>,
}

impl<L: Leiter> Zustand<L> {
    /// Erstelle einen neuen [Zustand].
    pub(in crate::gleis::gleise) fn neu(zugtyp: Zugtyp<L>) -> Self {
        Zustand {
            zugtyp,
            ohne_streckenabschnitt: GleiseDaten::neu(),
            ohne_geschwindigkeit: StreckenabschnittMap::new(),
            geschwindigkeiten: GeschwindigkeitMap::new(),
            pläne: HashMap::new(),
        }
    }

    pub(in crate::gleis::gleise) fn streckenabschnitt_map(
        &self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
    ) -> Result<&StreckenabschnittMap, GeschwindigkeitEntferntFehler> {
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

    pub(in crate::gleis::gleise) fn streckenabschnitt_map_mut(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
    ) -> Result<&mut StreckenabschnittMap, GeschwindigkeitEntferntFehler> {
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

    pub(in crate::gleis::gleise) fn daten(
        &self,
        streckenabschnitt: &Option<StreckenabschnittId>,
    ) -> Result<&GleiseDaten, StreckenabschnittIdFehler> {
        Ok(if let Some(streckenabschnitt_id) = streckenabschnitt {
            let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt_id;
            let streckenabschnitt_map = self.streckenabschnitt_map(geschwindigkeit.as_ref())?;
            &streckenabschnitt_map
                .get(name)
                .ok_or_else(|| {
                    StreckenabschnittIdFehler::StreckenabschnittEntfernt(
                        streckenabschnitt_id.klonen(),
                    )
                })?
                .1
        } else {
            &self.ohne_streckenabschnitt
        })
    }

    pub(in crate::gleis::gleise) fn daten_mut(
        &mut self,
        streckenabschnitt: &Option<StreckenabschnittId>,
    ) -> Result<&mut GleiseDaten, StreckenabschnittIdFehler> {
        Ok(if let Some(streckenabschnitt_id) = streckenabschnitt {
            let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt_id;
            let streckenabschnitt_map = self.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
            &mut streckenabschnitt_map
                .get_mut(name)
                .ok_or_else(|| {
                    StreckenabschnittIdFehler::StreckenabschnittEntfernt(
                        streckenabschnitt_id.klonen(),
                    )
                })?
                .1
        } else {
            &mut self.ohne_streckenabschnitt
        })
    }

    pub(in crate::gleis::gleise) fn alle_streckenabschnitt_daten<'t>(
        &'t self,
    ) -> impl Iterator<Item = (Option<StreckenabschnittIdRef<'t>>, &'t GleiseDaten)> {
        iter::once((None, &self.ohne_streckenabschnitt))
            .chain(self.ohne_geschwindigkeit.iter().map(|(name, (_streckenabschnitt, daten))| {
                (Some(StreckenabschnittIdRef { geschwindigkeit: None, name }), daten)
            }))
            .chain(self.geschwindigkeiten.iter().flat_map(
                |(geschwindigkeit_name, (_geschwindigkeit, map))| {
                    map.iter().map(move |(name, (_streckenabschnitt, daten))| {
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

    pub(in crate::gleis::gleise) fn alle_streckenabschnitte_und_daten<'t>(
        &'t self,
    ) -> impl Iterator<
        Item = (Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>, &'t GleiseDaten),
    > {
        let iter_map = |geschwindigkeit: Option<&'t _>| {
            move |(name, (streckenabschnitt, daten)): (&'t _, &'t (_, _))| {
                (Some((StreckenabschnittIdRef { geschwindigkeit, name }, streckenabschnitt)), daten)
            }
        };
        iter::once((None, &self.ohne_streckenabschnitt))
            .chain(self.ohne_geschwindigkeit.iter().map(iter_map(None)))
            .chain(self.geschwindigkeiten.iter().flat_map(
                move |(geschwindigkeit_name, (_geschwindigkeit, map))| {
                    map.iter().map(iter_map(Some(geschwindigkeit_name)))
                },
            ))
    }

    /// Alle Verbindungen in der Nähe der übergebenen Position.
    /// Der erste Rückgabewert sind alle `Verbindung`en in der Nähe,
    /// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
    pub(in crate::gleis::gleise) fn überlappende_verbindungen<'t>(
        &'t self,
        verbindung: &'t Verbindung,
        eigene_id: Option<&'t AnyIdRef<'t>>,
        gehalten_id: Option<&'t AnyId>,
    ) -> (impl Iterator<Item = Verbindung> + 't, bool) {
        let mut gehalten = false;
        let überlappend =
            self.alle_streckenabschnitt_daten().flat_map(move |(streckenabschnitt, daten)| {
                macro_rules! überlappende_verbindungen {
                    ($gleis: ident) => {{
                        let (überlappend_daten, gehalten_daten) = daten
                            .überlappende_verbindungen::<$gleis>(
                                self.zugtyp.spurweite,
                                verbindung,
                                streckenabschnitt.clone(),
                                eigene_id,
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
            });
        (überlappend, gehalten)
    }

    fn einraste_position<T: Zeichnen>(&self, definition: &T, position: Position) -> Position {
        let spurweite = self.zugtyp.spurweite;
        let mut snap = None;
        let verbindungen = definition.verbindungen_an_position(spurweite, position.clone());
        verbindungen.für_alle(|verbindung_name, verbindung| {
            if snap.is_none() {
                let (mut überlappende, _gehalten) =
                    self.überlappende_verbindungen(verbindung, None, None);
                snap = überlappende.next().map(|überlappend| (verbindung_name, überlappend));
            }
        });
        snap.map_or(position, |(einrasten_name, einrasten_verbindung)| {
            Position::anliegend_position(
                spurweite,
                definition,
                &einrasten_name,
                einrasten_verbindung,
            )
        })
    }

    /// Füge ein neues Gleis an der `Position` mit dem gewählten `streckenabschnitt` hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen<T: Zeichnen + DatenAuswahl>(
        &mut self,
        definition: T,
        mut position: Position,
        streckenabschnitt: Option<StreckenabschnittId>,
        einrasten: bool,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler> {
        let spurweite = self.zugtyp.spurweite;
        if einrasten {
            position = self.einraste_position(&definition, position)
        }
        // Berechne Bounding Box.
        let rectangle = Rectangle::from(definition.rechteck_an_position(spurweite, &position));
        // Füge zu RStern hinzu.
        self.daten_mut(&streckenabschnitt)?
            .rstern_mut()
            .insert(GeomWithData::new(rectangle.clone(), Gleis { definition, position }));
        // Rückgabewert
        Ok(GleisId { rectangle, streckenabschnitt, phantom: PhantomData })
    }

    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen_anliegend<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<StreckenabschnittId>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        T: Zeichnen + DatenAuswahl,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let spurweite = self.zugtyp.spurweite;
        // berechne neue position
        let position =
            Position::anliegend_position(spurweite, &definition, verbindung_name, ziel_verbindung);
        // füge neues Gleis hinzu
        self.hinzufügen(definition, position, streckenabschnitt, false)
    }

    /// Bewege ein Gleis an die neue position.
    fn bewegen_aux<T: Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        berechne_position: impl FnOnce(&Zustand<L>, &Gleis<T>) -> Position,
    ) -> Result<(), GleisIdFehler> {
        let spurweite = self.zugtyp.spurweite;
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = &*gleis_id;
        // Entferne aktuellen Eintrag.
        let rstern = self.daten_mut(&streckenabschnitt)?.rstern_mut::<T>();
        let gleis = rstern
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        // Füge an neuer Position hinzu.
        // Wegen Referenz auf self muss rstern kurzfristig vergessen werden.
        let position_neu = berechne_position(self, &gleis);
        let definition = gleis.definition;
        let rectangle = Rectangle::from(definition.rechteck_an_position(spurweite, &position_neu));
        let rstern = self.daten_mut(&streckenabschnitt)?.rstern_mut::<T>();
        rstern.insert(GeomWithData::new(
            rectangle.clone(),
            Gleis { definition, position: position_neu },
        ));
        // Aktualisiere GleisId
        gleis_id.rectangle = rectangle;
        Ok(())
    }

    /// Bewege ein Gleis an die neue position.
    #[inline(always)]
    pub(in crate::gleis::gleise) fn bewegen<T: Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        mut position_neu: Position,
        einrasten: bool,
    ) -> Result<(), GleisIdFehler> {
        self.bewegen_aux(gleis_id, |zustand, Gleis { definition, position: _ }| {
            if einrasten {
                position_neu = zustand.einraste_position(definition, position_neu)
            }
            position_neu
        })
    }

    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(in crate::gleis::gleise) fn bewegen_anliegend<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<(), GleisIdFehler>
    where
        T: Zeichnen + DatenAuswahl,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let spurweite = self.zugtyp.spurweite;
        self.bewegen_aux(gleis_id, |_zustand, Gleis { definition, position: _ }| {
            Position::anliegend_position(spurweite, definition, verbindung_name, ziel_verbindung)
        })
    }

    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(in crate::gleis::gleise) fn entfernen<T: Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Gleis<T>, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        // Entferne aktuellen Eintrag.
        let data = self
            .daten_mut(&streckenabschnitt)?
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        Ok(data)
    }
}

impl Position {
    /// Position damit Verbindungen übereinander mit entgegengesetzter Richtung liegen
    fn anliegend_position<T>(
        spurweite: Spurweite,
        definition: &T,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Position
    where
        T: Zeichnen,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let verbindungen = definition.verbindungen(spurweite);
        let verbindung = verbindungen.erhalte(verbindung_name);
        let winkel: Winkel = winkel::PI - verbindung.richtung + ziel_verbindung.richtung;
        Position {
            punkt: Vektor {
                x: ziel_verbindung.position.x - verbindung.position.x * winkel.cos()
                    + verbindung.position.y * winkel.sin(),
                y: ziel_verbindung.position.y
                    - verbindung.position.x * winkel.sin()
                    - verbindung.position.y * winkel.cos(),
            },
            winkel,
        }
    }
}

pub(in crate::gleis::gleise) type RStern<T> = RTree<GeomWithData<Rectangle<Vektor>, Gleis<T>>>;

#[derive(Debug)]
pub(crate) struct GleiseDaten {
    pub(in crate::gleis::gleise) geraden: RStern<Gerade>,
    pub(in crate::gleis::gleise) kurven: RStern<Kurve>,
    pub(in crate::gleis::gleise) weichen: RStern<Weiche>,
    pub(in crate::gleis::gleise) dreiwege_weichen: RStern<DreiwegeWeiche>,
    pub(in crate::gleis::gleise) kurven_weichen: RStern<KurvenWeiche>,
    pub(in crate::gleis::gleise) s_kurven_weichen: RStern<SKurvenWeiche>,
    pub(in crate::gleis::gleise) kreuzungen: RStern<Kreuzung>,
}

impl GleiseDaten {
    /// Erstelle eine leere `GleiseDaten`-Struktur.
    pub(in crate::gleis::gleise) fn neu() -> Self {
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

    /// Alle Verbindungen in der Nähe der übergebenen Position im zugehörigen `RStern`.
    /// Der erste Rückgabewert sind alle `Verbindung`en in der Nähe,
    /// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
    fn überlappende_verbindungen<'t, T>(
        &'t self,
        spurweite: Spurweite,
        verbindung: &'t Verbindung,
        streckenabschnitt: Option<StreckenabschnittIdRef<'t>>,
        eigene_id: Option<&'t AnyIdRef<'t>>,
        gehalten_id: Option<&'t AnyId>,
    ) -> (impl Iterator<Item = Verbindung> + 't, bool)
    where
        T: Zeichnen + DatenAuswahl + 't,
        AnyIdRef<'t>: From<GleisIdRef<'t, T>>,
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
            if Some(&kandidat_id) != eigene_id {
                let kandidat_verbindungen = kandidat
                    .data
                    .definition
                    .verbindungen_an_position(spurweite, kandidat.data.position.clone());
                for (_kandidat_name, kandidat_verbindung) in kandidat_verbindungen.referenzen() {
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

    pub(in crate::gleis::gleise) fn ist_leer(&self) -> bool {
        [
            self.geraden.size(),
            self.kurven.size(),
            self.weichen.size(),
            self.dreiwege_weichen.size(),
            self.kurven_weichen.size(),
            self.s_kurven_weichen.size(),
            self.kreuzungen.size(),
        ]
        .iter()
        .all(|size| *size == 0)
    }

    /// Füge alle Gleise von `neu` zu `self` hinzu.
    fn verschmelze(&mut self, mut neu: GleiseDaten) {
        macro_rules! kombinierte_rstern {
            ($($rstern:ident),* $(,)?) => {$(
                for geom_with_data in neu.$rstern.drain_with_selection_function(SelectAll) {
                    self.$rstern.insert(geom_with_data);
                }
            )*};
        }
        kombinierte_rstern! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}

const ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT: Skalar = Skalar(5.);

/// SelectionFunction, die jedes Element akzeptiert.
/// Haupt-Nutzen ist das vollständiges Leeren eines RTree (siehe [GleiseDaten::verschmelze]).
struct SelectAll;

impl<T: RTreeObject> SelectionFunction<T> for SelectAll {
    fn should_unpack_parent(&self, _envelope: &T::Envelope) -> bool {
        true
    }
}

/// SelectionFunction, die einen bestimmten Envelope sucht.
pub(in crate::gleis::gleise) struct SelectEnvelope(pub(in crate::gleis::gleise) AABB<Vektor>);

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
pub(crate) trait DatenAuswahl: Sized {
    fn rstern(gleise: &GleiseDaten) -> &RStern<Self>;
    fn rstern_mut(gleise: &mut GleiseDaten) -> &mut RStern<Self>;
}
impl GleiseDaten {
    #[inline(always)]
    pub(in crate::gleis::gleise) fn rstern<T: DatenAuswahl>(&self) -> &RStern<T> {
        T::rstern(self)
    }
    #[inline(always)]
    pub(in crate::gleis::gleise) fn rstern_mut<T: DatenAuswahl>(&mut self) -> &mut RStern<T> {
        T::rstern_mut(self)
    }
}
impl DatenAuswahl for Gerade {
    fn rstern(GleiseDaten { geraden, .. }: &GleiseDaten) -> &RStern<Self> {
        geraden
    }
    fn rstern_mut(GleiseDaten { geraden, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        geraden
    }
}
impl DatenAuswahl for Kurve {
    fn rstern(GleiseDaten { kurven, .. }: &GleiseDaten) -> &RStern<Self> {
        kurven
    }
    fn rstern_mut(GleiseDaten { kurven, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kurven
    }
}
impl DatenAuswahl for Weiche {
    fn rstern(GleiseDaten { weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        weichen
    }
    fn rstern_mut(GleiseDaten { weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        weichen
    }
}
impl DatenAuswahl for DreiwegeWeiche {
    fn rstern(GleiseDaten { dreiwege_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        dreiwege_weichen
    }
    fn rstern_mut(GleiseDaten { dreiwege_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        dreiwege_weichen
    }
}
impl DatenAuswahl for KurvenWeiche {
    fn rstern(GleiseDaten { kurven_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        kurven_weichen
    }
    fn rstern_mut(GleiseDaten { kurven_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kurven_weichen
    }
}
impl DatenAuswahl for SKurvenWeiche {
    fn rstern(GleiseDaten { s_kurven_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        s_kurven_weichen
    }
    fn rstern_mut(GleiseDaten { s_kurven_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        s_kurven_weichen
    }
}
impl DatenAuswahl for Kreuzung {
    fn rstern(GleiseDaten { kreuzungen, .. }: &GleiseDaten) -> &RStern<Self> {
        kreuzungen
    }
    fn rstern_mut(GleiseDaten { kreuzungen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kreuzungen
    }
}
