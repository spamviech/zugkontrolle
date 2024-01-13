//! Struktur zum Speichern aller Gleise.

use std::{any::TypeId, collections::hash_map::HashMap, fmt::Debug};

use iced::{
    widget::canvas::{
        fill::{self, Fill},
        stroke::{self, Stroke},
        Text,
    },
    Color,
};
use log::error;
use nonempty::{nonempty, NonEmpty};
use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree, RTreeObject, SelectionFunction, AABB,
};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        polarität::Fließend,
        Lager,
    },
    application::fonts::standard_text,
    gleis::{
        gerade::Gerade,
        gleise::{
            id::{
                eindeutig::KeineIdVerfügbar, erzeuge_any_enum, mit_any_id, AnyDefinitionId,
                AnyDefinitionIdSteuerung, AnyGleisDefinitionId, AnyId, AnyIdSteuerung,
                AnyIdSteuerungSerialisiert, DefinitionId, GleisId,
            },
            steuerung::{MitSteuerung, SomeAktualisierenSender},
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        verbindung::{self, Verbindung},
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        plan::{self, Plan},
        streckenabschnitt::{self, Streckenabschnitt},
    },
    typen::{
        canvas::{
            pfad::{self, Transformation},
            Frame, Position,
        },
        farbe::Farbe,
        mm::Spurweite,
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        Transparenz, Zeichnen,
    },
    util::nachschlagen::Nachschlagen,
    zugtyp::Zugtyp,
};

use self::v4::GleisSerialisiert;

use super::id;

pub mod de_serialisieren;
pub mod v2;
pub mod v3;
pub mod v4;

/// Definition und Position eines Gleises.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<T as MitSteuerung>::Steuerung: Debug)]
#[zugkontrolle_clone(<T as MitSteuerung>::Steuerung: Clone)]
#[zugkontrolle_clone(<T as MitSteuerung>::SelfUnit: Clone)]
pub struct Gleis<T: MitSteuerung>
where
    <T as MitSteuerung>::SelfUnit: 'static,
{
    /// Die [`Zeichnen`]-Definition des Gleises.
    pub definition: DefinitionId<T>,
    /// Die [`Anschlüsse`](anschluss::Anschluss) des Gleises.
    pub steuerung: <T as MitSteuerung>::Steuerung,
    /// Die Position des Gleises auf dem [`Canvas`](iced::widget::canvas::Canvas).
    pub position: Position,
    /// Der [`Streckenabschnitt`] des Gleises.
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

erzeuge_any_enum! {
    (pub) AnyGleis,
    "Ein beliebiges Gleis.",
    [Debug, Clone],
    (Gleis<[]>),
}

impl<T> Serialisiere<GleisSerialisiert<T>> for Gleis<T>
where
    T: MitSteuerung,
    <T as MitSteuerung>::Steuerung: Serialisiere<<T as MitSteuerung>::Serialisiert>,
{
    fn serialisiere(&self) -> GleisSerialisiert<T> {
        GleisSerialisiert {
            definition: self.definition.repräsentation(),
            position: self.position.clone(),
            steuerung: self.steuerung.serialisiere(),
            streckenabschnitt: self.streckenabschnitt.clone(),
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        self.steuerung.anschlüsse()
    }
}

impl<T> Reserviere<Gleis<T>> for GleisSerialisiert<T>
where
    T: 'static + MitSteuerung,
    <T as MitSteuerung>::Serialisiert: Reserviere<<T as MitSteuerung>::Steuerung, RefArg = ()>,
{
    type MoveArg =
        <<T as MitSteuerung>::Serialisiert as Reserviere<<T as MitSteuerung>::Steuerung>>::MoveArg;
    type RefArg = HashMap<id::Repräsentation, DefinitionId<T>>;
    type MutRefArg = <<T as MitSteuerung>::Serialisiert as Reserviere<
        <T as MitSteuerung>::Steuerung,
    >>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg_steuerung: Self::MoveArg,
        bekannte_definition_ids: &Self::RefArg,
        mut_ref_arg_steuerung: &mut Self::MutRefArg,
    ) -> Ergebnis<Gleis<T>> {
        let GleisSerialisiert { definition, position, steuerung, streckenabschnitt } = self;
        let Some(id) = bekannte_definition_ids.get(&definition) else {
            return Ergebnis::Fehler {
                fehler: nonempty![anschluss::Fehler::UnbekannteGespeicherteDefinition {
                    id: definition,
                    type_id: TypeId::of::<T>(),
                    type_name: std::any::type_name::<T>()
                }],
                anschlüsse,
            };
        };
        steuerung
            .reserviere(lager, anschlüsse, move_arg_steuerung, &(), mut_ref_arg_steuerung)
            .konvertiere(|steuerung| Gleis {
                definition: id.clone(),
                position,
                steuerung,
                streckenabschnitt,
            })
    }
}

pub(crate) type StreckenabschnittMap =
    HashMap<streckenabschnitt::Name, (Streckenabschnitt, Option<geschwindigkeit::Name>)>;
type GeschwindigkeitMap<Leiter> = HashMap<geschwindigkeit::Name, Geschwindigkeit<Leiter>>;

/// Alle [Gleise](Gleis), [Geschwindigkeiten](Geschwindigkeit) und [`Streckenabschnitte`](Streckenabschnitt),
/// sowie der verwendete [`Zugtyp`].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub(in crate::gleis::gleise) struct Zustand<L: Leiter> {
    zugtyp: Zugtyp<L>,
    geschwindigkeiten: GeschwindigkeitMap<L>,
    streckenabschnitte: StreckenabschnittMap,
    gleise: GleiseDaten,
    pläne: HashMap<plan::Name, Plan<L>>,
}

/// Die gesuchte [`Geschwindigkeit`] wurde entfernt.
#[derive(Debug)]
pub struct GeschwindigkeitEntferntFehler(pub geschwindigkeit::Name);

/// Der gesuchte [`Streckenabschnitt`] wurde entfernt.
#[derive(Debug)]
pub struct StreckenabschnittEntferntFehler(pub streckenabschnitt::Name);

impl<L: Leiter> Zustand<L> {
    /// Erstelle einen neuen [`Zustand`].
    pub(in crate::gleis::gleise) fn neu(zugtyp: Zugtyp<L>) -> Self {
        Zustand {
            zugtyp,
            geschwindigkeiten: GeschwindigkeitMap::new(),
            streckenabschnitte: StreckenabschnittMap::new(),
            gleise: GleiseDaten::neu(),
            pläne: HashMap::new(),
        }
    }

    pub(in crate::gleis::gleise) fn zugtyp(&self) -> &Zugtyp<L> {
        &self.zugtyp
    }

    pub(in crate::gleis::gleise) fn geschwindigkeiten(&self) -> &GeschwindigkeitMap<L> {
        &self.geschwindigkeiten
    }

    pub(in crate::gleis::gleise) fn geschwindigkeit(
        &self,
        name: &geschwindigkeit::Name,
    ) -> Result<&Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        self.geschwindigkeiten.get(name).ok_or_else(|| GeschwindigkeitEntferntFehler(name.clone()))
    }

    pub(in crate::gleis::gleise) fn geschwindigkeit_mut(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<&mut Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        self.geschwindigkeiten
            .get_mut(name)
            .ok_or_else(|| GeschwindigkeitEntferntFehler(name.clone()))
    }

    pub(in crate::gleis::gleise) fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
    ) -> Option<Geschwindigkeit<L>> {
        self.geschwindigkeiten.insert(name, geschwindigkeit)
    }

    pub(in crate::gleis::gleise) fn geschwindigkeit_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        for (_name, (_streckenabschnitt, geschwindigkeit)) in self.streckenabschnitte.iter_mut() {
            if Some(name) == geschwindigkeit.as_ref() {
                *geschwindigkeit = None;
            }
        }
        self.geschwindigkeiten
            .remove(name)
            .ok_or_else(|| GeschwindigkeitEntferntFehler(name.clone()))
    }

    pub(in crate::gleis::gleise) fn streckenabschnitte(&self) -> &StreckenabschnittMap {
        &self.streckenabschnitte
    }

    pub(in crate::gleis::gleise) fn streckenabschnitt(
        &self,
        name: &streckenabschnitt::Name,
    ) -> Result<&(Streckenabschnitt, Option<geschwindigkeit::Name>), StreckenabschnittEntferntFehler>
    {
        self.streckenabschnitte
            .get(name)
            .ok_or_else(|| StreckenabschnittEntferntFehler(name.clone()))
    }

    pub(in crate::gleis::gleise) fn streckenabschnitt_mut(
        &mut self,
        name: &streckenabschnitt::Name,
    ) -> Result<
        &mut (Streckenabschnitt, Option<geschwindigkeit::Name>),
        StreckenabschnittEntferntFehler,
    > {
        self.streckenabschnitte
            .get_mut(name)
            .ok_or_else(|| StreckenabschnittEntferntFehler(name.clone()))
    }

    pub(in crate::gleis::gleise) fn streckenabschnitt_hinzufügen(
        &mut self,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
        geschwindigkeit: Option<geschwindigkeit::Name>,
    ) -> Option<(Streckenabschnitt, Option<geschwindigkeit::Name>)> {
        self.streckenabschnitte.insert(name, (streckenabschnitt, geschwindigkeit))
    }

    pub(in crate::gleis::gleise) fn streckenabschnitt_entfernen(
        &mut self,
        name: &streckenabschnitt::Name,
    ) -> Result<(Streckenabschnitt, Option<geschwindigkeit::Name>), StreckenabschnittEntferntFehler>
    {
        self.gleise.entferne_streckenabschnitt(name);
        self.streckenabschnitte
            .remove(name)
            .ok_or_else(|| StreckenabschnittEntferntFehler(name.clone()))
    }

    /// Füge ein neues [Gleis] an der [Position] mit dem gewählten [`Streckenabschnitt`] hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen(
        &mut self,
        definition_steuerung: impl Into<AnyDefinitionIdSteuerung>,
        position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId, HinzufügenFehler2> {
        self.gleise.hinzufügen(
            &self.zugtyp,
            definition_steuerung.into(),
            position,
            streckenabschnitt,
            einrasten,
        )
    }

    /// Bewege ein [Gleis] an die neue [`Position`].
    pub(in crate::gleis::gleise) fn bewegen(
        &mut self,
        gleis_id: impl Into<AnyId>,
        position: Position,
        einrasten: bool,
    ) -> Result<(), BewegenFehler> {
        self.gleise.bewegen(&self.zugtyp, gleis_id.into(), position, einrasten)
    }

    /// Entferne das [Gleis] assoziiert mit der [`GleisId`].
    pub(in crate::gleis::gleise) fn entfernen(
        &mut self,
        gleis_id: impl Into<AnyId>,
    ) -> Result<AnyGleis, EntfernenFehler> {
        self.gleise.entfernen(gleis_id.into())
    }

    /// Setzte (oder entferne) den [Streckenabschnitt] für das [Gleis] assoziiert mit der [`GleisId`].
    ///
    /// Rückgabewert ist der [`Name`](streckenabschnitt::Name) des bisherigen
    /// [`Streckenabschnittes`](Streckenabschnitt) (falls einer gesetzt war).
    pub(in crate::gleis::gleise) fn setze_streckenabschnitt(
        &mut self,
        gleis_id: impl Into<AnyId>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, SetzteStreckenabschnittFehler> {
        self.gleise.setze_streckenabschnitt(gleis_id.into(), streckenabschnitt)
    }

    /// Aktualisiere die Steuerung für ein [`Gleis`].
    pub(in crate::gleis::gleise) fn steuerung_aktualisieren(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: AnyIdSteuerungSerialisiert,
        sender: SomeAktualisierenSender,
    ) -> Result<(), SteuerungAktualisierenFehler> {
        self.gleise.steuerung_aktualisieren(lager, gleis_steuerung, sender)
    }

    /// Sind für ein Gleis Anschlüsse definiert?
    pub(in crate::gleis::gleise) fn hat_steuerung(
        &self,
        gleis: AnyId,
    ) -> Result<bool, GleisNichtGefunden> {
        self.gleise.hat_steuerung(gleis)
    }

    /// Füge die Darstellung aller Gleise dem Frame hinzu.
    pub(in crate::gleis::gleise) fn darstellen_aller_gleise(
        &self,
        frame: &mut Frame<'_>,
        transparent_hintergrund: impl Fn(AnyId, Fließend) -> Transparenz,
        ist_gehalten: impl Fn(AnyId) -> bool,
        farbe: Farbe,
        skalieren: Skalar,
    ) {
        self.gleise.darstellen_aller_gleise(
            frame,
            &self.zugtyp,
            &self.streckenabschnitte,
            transparent_hintergrund,
            ist_gehalten,
            farbe,
            skalieren,
        )
    }

    /// Erhalte die Id, Steuerung, relative Klick-Position, Winkel und Streckenabschnitt des Gleises an der gesuchten Position.
    pub(in crate::gleis::gleise) fn gleis_an_position(
        &self,
        canvas_pos: Vektor,
    ) -> Option<(
        AnyIdSteuerung,
        Vektor,
        Winkel,
        Option<(
            streckenabschnitt::Name,
            &Streckenabschnitt,
            Option<(geschwindigkeit::Name, &Geschwindigkeit<L>)>,
        )>,
    )> {
        let (id_steuerung, position, winkel, streckenabschnitt_id) =
            self.gleise.gleis_an_position(&self.zugtyp, canvas_pos)?;
        let streckenabschnitt = streckenabschnitt_id.and_then(|streckenabschnitt_id| {
            self.streckenabschnitte.get(&streckenabschnitt_id).map(
                |(streckenabschnitt, geschwindigkeit_id)| {
                    (
                        streckenabschnitt_id,
                        streckenabschnitt,
                        geschwindigkeit_id.as_ref().and_then(|geschwindigkeit_id| {
                            self.geschwindigkeiten.get(geschwindigkeit_id).map(|geschwindigkeit| {
                                (geschwindigkeit_id.clone(), geschwindigkeit)
                            })
                        }),
                    )
                },
            )
        });
        Some((id_steuerung, position, winkel, streckenabschnitt))
    }
}

type GleisMap<T> = HashMap<GleisId<T>, (Gleis<T>, Rectangle<Vektor>)>;

pub(in crate::gleis::gleise) type RStern =
    RTree<GeomWithData<Rectangle<Vektor>, (AnyGleisDefinitionId, Position)>>;

#[derive(Debug)]
pub(crate) struct GleiseDaten {
    geraden: GleisMap<Gerade>,
    kurven: GleisMap<Kurve>,
    weichen: GleisMap<Weiche>,
    dreiwege_weichen: GleisMap<DreiwegeWeiche>,
    kurven_weichen: GleisMap<KurvenWeiche>,
    s_kurven_weichen: GleisMap<SKurvenWeiche>,
    kreuzungen: GleisMap<Kreuzung>,
    // Invariante: Jeder Eintrag hat einen zur Position passenden Eintrag in der RStern-Struktur
    rstern: RStern,
}

impl GleiseDaten {
    /// Erstelle eine leere [`GleiseDaten`]-Struktur.
    pub(in crate::gleis::gleise) fn neu() -> Self {
        GleiseDaten {
            geraden: GleisMap::new(),
            kurven: GleisMap::new(),
            weichen: GleisMap::new(),
            dreiwege_weichen: GleisMap::new(),
            kurven_weichen: GleisMap::new(),
            s_kurven_weichen: GleisMap::new(),
            kreuzungen: GleisMap::new(),
            rstern: RStern::new(),
        }
    }
}

const ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT: Skalar = Skalar(5.);

/// Alle Verbindungen in der Nähe der übergebenen Position im zugehörigen [`RStern`].
/// Der erste Rückgabewert sind alle [`Verbindungen`](Verbindung) in der Nähe,
/// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
fn überlappende_verbindungen<'t, L: Leiter>(
    rstern: &'t RStern,
    zugtyp: &'t Zugtyp<L>,
    verbindung: &'t Verbindung,
    eigene_id: Option<&'t AnyId>,
    ist_gehalten: impl 't + Fn(AnyId) -> bool,
) -> (impl 't + Iterator<Item = Verbindung>, bool) {
    let vektor_genauigkeit =
        Vektor { x: ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT, y: ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT };
    let kandidaten_rechteck = Rechteck {
        ecke_a: verbindung.position + vektor_genauigkeit,
        ecke_b: verbindung.position - vektor_genauigkeit,
    };
    let kandidaten =
        rstern.locate_in_envelope_intersecting(&Rectangle::from(kandidaten_rechteck).envelope());
    let mut gehalten = false;
    let überlappend = kandidaten.flat_map(move |kandidat| {
        let (kandidat_ids, position) = &kandidat.data;
        let mut überlappend = Vec::new();

        fn alle_verbindungen<T, Name>(t: T) -> Vec<Verbindung>
        where
            T: verbindung::Nachschlagen<Name>,
        {
            t.referenzen().into_iter().map(|(_name, verbindung)| *verbindung).collect()
        }

        macro_rules! erhalte_alle_verbindungen {
            ($definitionen:expr, $gleis_id: expr, $definition_id: expr) => {{
                let kandidat_id = AnyId::from($gleis_id.clone());
                let nicht_eigene_id = Some(&kandidat_id) != eigene_id;
                nicht_eigene_id.then(|| {
                    $definitionen.get(&$definition_id).map(|definition| {
                        (
                            alle_verbindungen(definition.verbindungen_an_position(
                                &(),
                                zugtyp.spurweite,
                                position.clone(),
                            )),
                            ist_gehalten(kandidat_id),
                        )
                    })
                })
            }};
        }

        let (kandidat_verbindungen, kandidat_ist_gehalten) = {
            mit_any_id!({ref zugtyp}, [AnyGleisDefinitionId => gleis_id, definition_id] &kandidat_ids => erhalte_alle_verbindungen!())
        }
        .flatten()
        .unwrap_or((Vec::new(), false));
        for kandidat_verbindung in kandidat_verbindungen {
            if (verbindung.position - kandidat_verbindung.position).länge()
                < ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT
            {
                überlappend.push(kandidat_verbindung.clone());
                gehalten = gehalten || kandidat_ist_gehalten;
            }
        }
        überlappend
    });
    (überlappend, gehalten)
}

fn einraste_position<L: Leiter, U: Zeichnen<Z>, Z>(
    rstern: &RStern,
    zugtyp: &Zugtyp<L>,
    definition: &U,
    id: Option<AnyId>,
    z: &Z,
    position: Position,
) -> Position {
    let mut snap = None;
    let verbindungen = definition.verbindungen_an_position(z, zugtyp.spurweite, position.clone());
    verbindungen.für_alle(|verbindung_name, verbindung| {
        if snap.is_none() {
            let (mut überlappende, _gehalten) =
                überlappende_verbindungen(rstern, zugtyp, verbindung, id.as_ref(), |_gleis_id| {
                    false
                });
            snap = überlappende.next().map(|überlappend| (verbindung_name, überlappend));
        }
    });
    snap.map_or(position, |(einrasten_name, einrasten_verbindung)| {
        Position::anliegend_position(
            definition,
            z,
            zugtyp.spurweite,
            &einrasten_name,
            einrasten_verbindung,
        )
    })
}

/// Fehler beim [`hinzufügen`](crate::gleis::gleise::Gleise::hinzufügen) eines Gleises.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum HinzufügenFehler2 {
    /// Unbekannte Definition-Id für das neue Gleis.
    DefinitionNichtGefunden(AnyDefinitionId),
    /// Es ist aktuell keine [`GleisId`] verfügbar.
    KeineIdVerfügbar(KeineIdVerfügbar),
}

impl GleiseDaten {
    /// Füge ein neues [`Gleis`] hinzu.
    fn hinzufügen<L: Leiter>(
        &mut self,
        zugtyp: &Zugtyp<L>,
        definition_steuerung: AnyDefinitionIdSteuerung,
        mut position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId, HinzufügenFehler2> {
        macro_rules! hinzufügen_aux {
            ($gleise: expr, $definitionen: expr, $definition_id: expr, $steuerung: expr) => {{
                // Erhalte Definition.
                let definition = match $definitionen.get(&$definition_id) {
                    Some(definition) => definition,
                    None => {
                        return Err(HinzufügenFehler2::DefinitionNichtGefunden(
                            AnyDefinitionId::from($definition_id),
                        ))
                    },
                };
                // Passe Position an, wenn es eine Verbindung in der Nähe gibt.
                if einrasten {
                    position =
                        einraste_position(&self.rstern, zugtyp, definition, None, &(), position)
                }
                // Erzeuge neue Id.
                let id = GleisId::neu()?;
                // Berechne Bounding Box.
                let rectangle = Rectangle::from(definition.rechteck_an_position(
                    &(),
                    zugtyp.spurweite,
                    &position,
                ));
                // Füge zu RStern hinzu.
                self.rstern.insert(GeomWithData::new(
                    rectangle.clone(),
                    (
                        AnyGleisDefinitionId::from((id.clone(), $definition_id.clone())),
                        position.clone(),
                    ),
                ));
                // Füge zu GleiseDaten hinzu.
                let bisher = $gleise.insert(
                    id.clone(),
                    (
                        Gleis {
                            definition: $definition_id,
                            steuerung: $steuerung,
                            position,
                            streckenabschnitt,
                        },
                        rectangle,
                    ),
                );
                if let Some(bisher) = bisher {
                    error!("Gleis {bisher:?} mit Id {id:?} ersetzt!");
                }
                // Rückgabewert
                Ok(AnyId::from(id))
            }};
        }
        mit_any_id!(
            {mut self, ref zugtyp},
            [AnyDefinitionIdSteuerung => definition, steuerung] definition_steuerung
             =>hinzufügen_aux!()
        )
    }
}

/// Fehler beim [`bewegen`](crate::gleis::gleise::Gleise::bewegen) eines Gleises.
#[derive(Debug, Clone)]
pub enum BewegenFehler {
    /// Unbekannte Definition-Id für das bewegte Gleis.
    DefinitionNichtGefunden(AnyDefinitionId),
    /// Ein unbekanntes Gleis sollte bewegt werden.
    GleisNichtGefunden(AnyId),
}

impl GleiseDaten {
    /// Bewege ein [`Gleis`] an die `neue_position`.
    fn bewegen<L: Leiter>(
        &mut self,
        zugtyp: &Zugtyp<L>,
        gleis_id: AnyId,
        mut neue_position: Position,
        einrasten: bool,
    ) -> Result<(), BewegenFehler> {
        macro_rules! bewegen_aux {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr) => {{
                // Erhalte Referenz auf das Gleis.
                let (gleis, rectangle) = match $gleise.get_mut(&$gleis_id) {
                    Some(entry) => entry,
                    None => {
                        return Err(BewegenFehler::GleisNichtGefunden(AnyId::from($gleis_id)));
                    },
                };
                let neues_rectangle = match $definitionen.get(&gleis.definition) {
                    Some(definition) => {
                        // Passe Position an, wenn es eine Verbindung in der Nähe gibt.
                        if einrasten {
                            neue_position = einraste_position(
                                &self.rstern,
                                zugtyp,
                                definition,
                                Some(AnyId::from($gleis_id.clone())),
                                &(),
                                neue_position,
                            );
                        }
                        // Berechne neue Bounding Box.
                        Rectangle::from(definition.rechteck_an_position(
                            &(),
                            zugtyp.spurweite,
                            &neue_position,
                        ))
                    },
                    None => {
                        return Err(BewegenFehler::DefinitionNichtGefunden(AnyDefinitionId::from(
                            gleis.definition.clone(),
                        )))
                    },
                };
                // Entferne alten Eintrag aus RStern.
                let result = self.rstern.remove(&GeomWithData::new(
                    rectangle.clone(),
                    (
                        AnyGleisDefinitionId::from(($gleis_id.clone(), gleis.definition.clone())),
                        gleis.position.clone(),
                    ),
                ));
                if result.is_none() {
                    error!(
                        "Rectangle für Gleis mit Id {:?} konnte nicht entfernt werden!",
                        $gleis_id
                    );
                }
                // Füge neuen Eintrag zu RStern hinzu.
                self.rstern.insert(GeomWithData::new(
                    neues_rectangle.clone(),
                    (
                        AnyGleisDefinitionId::from(($gleis_id.clone(), gleis.definition.clone())),
                        neue_position.clone(),
                    ),
                ));
                // Aktualisiere gespeicherte Position und Bounding Box.
                gleis.position = neue_position;
                *rectangle = neues_rectangle;
                Ok(())
            }};
        }
        mit_any_id!({mut self, ref zugtyp}, [AnyId => id] gleis_id => bewegen_aux!())
    }
}

/// Fehler beim [`entfernen`](crate::gleis::gleise::Gleise::entfernen) eines Gleises.
#[derive(Debug, Clone)]
pub struct EntfernenFehler(AnyId);

impl GleiseDaten {
    /// Entferne ein [`Gleis`].
    fn entfernen(&mut self, gleis_id: AnyId) -> Result<AnyGleis, EntfernenFehler> {
        macro_rules! entfernen_aux {
            ($gleise: expr, $gleis_id: expr) => {{
                let (gleis, rectangle) = match $gleise.remove(&$gleis_id) {
                    Some(entry) => entry,
                    None => return Err(EntfernenFehler(AnyId::from($gleis_id))),
                };
                let id_clone = $gleis_id.clone();
                let result = self.rstern.remove(&GeomWithData::new(
                    rectangle,
                    (
                        AnyGleisDefinitionId::from(($gleis_id, gleis.definition.clone())),
                        gleis.position.clone(),
                    ),
                ));
                if result.is_none() {
                    error!(
                        "Rectangle für Gleis {gleis:?} mit Id {id_clone:?} konnte nicht entfernt werden!"
                    );
                }
                Ok(AnyGleis::from(gleis))
            }};
        }
        mit_any_id!({mut self}, [AnyId => id] gleis_id => entfernen_aux!())
    }
}

/// Fehler beim [setzen des Streckenabschnitts](crate::gleis::gleise::Gleise::setzte_streckenabschnitt) eines Gleises.
#[derive(Debug, Clone)]
pub struct SetzteStreckenabschnittFehler(AnyId, Option<streckenabschnitt::Name>);

impl GleiseDaten {
    /// Setzte (oder entferne) den [Streckenabschnitt] für das [Gleis] assoziiert mit der [`GleisId`].
    ///
    /// Rückgabewert ist der [`Name`](streckenabschnitt::Name) des bisherigen
    /// [`Streckenabschnittes`](Streckenabschnitt) (falls einer gesetzt war).
    fn setze_streckenabschnitt(
        &mut self,
        gleis_id: AnyId,
        mut streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, SetzteStreckenabschnittFehler> {
        macro_rules! setze_streckenabschnitt_aux {
            ($gleise: expr, $gleis_id: expr) => {{
                let (gleis, _rectangle) = match $gleise.get_mut(&$gleis_id) {
                    Some(entry) => entry,
                    None => {
                        return Err(SetzteStreckenabschnittFehler(
                            AnyId::from($gleis_id),
                            streckenabschnitt,
                        ))
                    },
                };
                std::mem::swap(&mut gleis.streckenabschnitt, &mut streckenabschnitt);
                Ok(streckenabschnitt)
            }};
        }
        mit_any_id!({mut self}, [AnyId => id] gleis_id => setze_streckenabschnitt_aux!())
    }

    /// Entferne einen [`Streckenabschnitt`] aus allen Gleisen.
    fn entferne_streckenabschnitt(&mut self, streckenabschnitt: &streckenabschnitt::Name) {
        for (_id, (gleis, _rectangle)) in self.geraden.iter_mut() {
            if Some(streckenabschnitt) == gleis.streckenabschnitt.as_ref() {
                gleis.streckenabschnitt = None;
            }
        }
        macro_rules! entferne_streckenabschnitt_aux {
            ($($gleis_map: ident),* $(,)?) => {{$(
                for (_id, (gleis, _rectangle)) in self.$gleis_map.iter_mut() {
                    if Some(streckenabschnitt) == gleis.streckenabschnitt.as_ref() {
                        gleis.streckenabschnitt = None;
                    }
                }
            )*}};
        }
        entferne_streckenabschnitt_aux!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        )
    }
}

/// Fehler beim aktualisieren der Steuerung eines Gleises.
#[derive(Debug)]
pub enum SteuerungAktualisierenFehler {
    /// Das Gleis wurde nicht gefunden.
    GleisNichtGefunden(AnyId),
    /// Ein Fehler beim [Reservieren](crate::anschluss::Reserviere::reserviere) der [`Anschlüsse`](anschluss::Anschluss).
    Deserialisieren {
        /// Der Fehler beim reservieren der neuen Anschlüsse.
        fehler: NonEmpty<anschluss::Fehler>,
        /// Ein Fehler beim Wiederherstellen der ursprünglichen Anschlüsse,
        /// sowie eine Repräsentation der ursprünglichen Anschlüsse.
        wiederherstellen_fehler: Option<(NonEmpty<anschluss::Fehler>, String)>,
    },
}

/// Das Gleis wurde nicht gefunden.
#[derive(Debug, Clone)]
pub struct GleisNichtGefunden(AnyId);

impl GleiseDaten {
    /// Aktualisiere die Steuerung für ein [`Gleis`].
    fn steuerung_aktualisieren(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: AnyIdSteuerungSerialisiert,
        sender: SomeAktualisierenSender,
    ) -> Result<(), SteuerungAktualisierenFehler> {
        macro_rules! steuerung_aktualisieren_aux {
            ($gleise: expr, $gleis_id: expr, $anschlüsse_serialisiert: expr) => {{
                let (Gleis { steuerung, .. }, _rectangle) =
                    $gleise.get_mut(&$gleis_id).ok_or(
                        SteuerungAktualisierenFehler::GleisNichtGefunden(AnyId::from($gleis_id)),
                    )?;

                let anschlüsse_serialisiert =
                    if let Some(anschlüsse_serialisiert) = $anschlüsse_serialisiert {
                        anschlüsse_serialisiert
                    } else {
                        let _ = steuerung.take();
                        return Ok(());
                    };
                let (steuerung_serialisiert, anschlüsse) = if let Some(s) = steuerung.take() {
                    (Some(s.serialisiere()), s.anschlüsse())
                } else {
                    (None, Anschlüsse::default())
                };
                use Ergebnis::*;
                let (fehler, anschlüsse) =
                    match anschlüsse_serialisiert.reserviere(lager, anschlüsse, sender.clone(), &(), &mut ()) {
                        Wert { anschluss, .. } => {
                            let _ = steuerung.insert(anschluss);
                            return Ok(());
                        },
                        FehlerMitErsatzwert { anschluss, fehler, mut anschlüsse } => {
                            anschlüsse.anhängen(anschluss.anschlüsse());
                            (fehler, anschlüsse)
                        },
                        Fehler { fehler, anschlüsse } => (fehler, anschlüsse),
                    };
                let mut wiederherstellen_fehler = None;
                if let Some(steuerung_serialisiert) = steuerung_serialisiert {
                    let serialisiert_string = format!("{steuerung_serialisiert:?}");
                    match steuerung_serialisiert.reserviere(lager, anschlüsse, sender, &(), &mut ()) {
                        Wert { anschluss, .. } => {
                            let _ = steuerung.insert(anschluss);
                        },
                        FehlerMitErsatzwert { anschluss, fehler, .. } => {
                            let _ = steuerung.insert(anschluss);
                            wiederherstellen_fehler = Some((fehler, serialisiert_string));
                        },
                        Fehler { fehler, .. } => {
                            wiederherstellen_fehler = Some((fehler, serialisiert_string))
                        },
                    }
                }
                Err(SteuerungAktualisierenFehler::Deserialisieren {
                    fehler,
                    wiederherstellen_fehler,
                })
            }};
        }
        mit_any_id!(
            {mut self},
            [AnyIdSteuerungSerialisiert => gleis_id, steuerung_serialisiert] gleis_steuerung
            => steuerung_aktualisieren_aux!()
        )
    }

    /// Sind für ein Gleis Anschlüsse definiert?
    fn hat_steuerung(&self, gleis: AnyId) -> Result<bool, GleisNichtGefunden> {
        macro_rules! hat_steuerung_aux {
            ($gleise: expr, $gleis_id: expr) => {{
                let (Gleis { steuerung, .. }, _rectangle) =
                    $gleise.get(&$gleis_id).ok_or(GleisNichtGefunden(AnyId::from($gleis_id)))?;
                Ok(steuerung.is_some())
            }};
        }
        mit_any_id!(
            {ref self},
            [AnyId => gleis_id] gleis
            => hat_steuerung_aux!()
        )
    }
}

pub(crate) fn bewege_an_position(frame: &mut Frame<'_>, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

/// Färbe den Hintergrund eines Gleises.
fn fülle_gleis<T>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    definition: &<T as MitSteuerung>::SelfUnit,
    steuerung: &<T as MitSteuerung>::Steuerung,
    gleis_id: &GleisId<T>,
    transparent: impl Fn(AnyId, Fließend) -> Transparenz,
    streckenabschnitt: Option<(Farbe, Fließend)>,
) where
    AnyId: From<GleisId<T>>,
    T: MitSteuerung,
    <T as MitSteuerung>::SelfUnit: Zeichnen<<T as MitSteuerung>::Steuerung>,
{
    for (pfad, farbe, transparenz) in definition.fülle(steuerung, spurweite) {
        let farbe_alpha = match (farbe, streckenabschnitt) {
            (None, None) => None,
            (None, Some((farbe, fließend))) => Some((
                farbe,
                transparent(AnyId::from(gleis_id.clone()), fließend)
                    .kombiniere(transparenz)
                    .alpha(),
            )),
            (Some(farbe), None) => Some((farbe, 1.)),
            (Some(farbe), Some((_farbe, fließend))) => Some((
                farbe,
                transparent(AnyId::from(gleis_id.clone()), fließend)
                    .kombiniere(transparenz)
                    .alpha(),
            )),
        };
        if let Some((Farbe { rot, grün, blau }, alpha)) = farbe_alpha {
            frame.with_save(|frame| {
                let color = Color { r: rot, g: grün, b: blau, a: alpha };
                frame.fill(
                    &pfad,
                    Fill { style: fill::Style::Solid(color), rule: fill::Rule::EvenOdd },
                );
            });
        }
    }
}

/// Zeichne die Kontur eines Gleises.
fn zeichne_gleis<T>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    definition: &<T as MitSteuerung>::SelfUnit,
    steuerung: &<T as MitSteuerung>::Steuerung,
    gleis_id: &GleisId<T>,
    ist_gehalten: impl Fn(AnyId) -> bool,
    farbe: Farbe,
) where
    AnyId: From<GleisId<T>>,
    T: MitSteuerung,
    <T as MitSteuerung>::SelfUnit: Zeichnen<<T as MitSteuerung>::Steuerung>,
{
    for path in definition.zeichne(steuerung, spurweite) {
        frame.with_save(|frame| {
            let a =
                Transparenz::true_reduziert(ist_gehalten(AnyId::from(gleis_id.clone()))).alpha();
            frame.stroke(
                &path,
                Stroke {
                    style: stroke::Style::Solid(Color { a, ..Color::from(farbe) }),
                    width: 1.5,
                    ..Stroke::default()
                },
            );
        });
    }
}

pub(in crate::gleis::gleise) struct GehaltenVerbindung {
    gehalten: bool,
    andere_entgegengesetzt: bool,
    andere_gehalten: bool,
}

fn ist_gehalten_und_andere_verbindung<L: Leiter>(
    rstern: &RStern,
    zugtyp: &Zugtyp<L>,
    ist_gehalten: impl Fn(AnyId) -> bool,
    gleis_id: AnyId,
    verbindung: Verbindung,
) -> GehaltenVerbindung {
    let gehalten = ist_gehalten(gleis_id.clone());
    let (mut überlappende, andere_gehalten) =
        überlappende_verbindungen(rstern, zugtyp, &verbindung, Some(&gleis_id), &ist_gehalten);
    let ist_entgegengesetzt = |überlappend: &Verbindung| {
        (winkel::PI + verbindung.richtung - überlappend.richtung).normalisiert().abs() < Winkel(0.1)
    };
    let andere_entgegengesetzt = überlappende.find(ist_entgegengesetzt).is_some();
    GehaltenVerbindung { gehalten, andere_entgegengesetzt, andere_gehalten }
}

/// Zeichne die Verbindungen eines Gleises.
fn zeichne_verbindungen<T, L: Leiter>(
    frame: &mut Frame<'_>,
    rstern: &RStern,
    zugtyp: &Zugtyp<L>,
    definition: &<T as MitSteuerung>::SelfUnit,
    steuerung: &<T as MitSteuerung>::Steuerung,
    gleis_id: &GleisId<T>,
    ist_gehalten: impl Fn(AnyId) -> bool,
    position: &Position,
) where
    AnyId: From<GleisId<T>>,
    T: MitSteuerung,
    <T as MitSteuerung>::SelfUnit: Zeichnen<<T as MitSteuerung>::Steuerung>,
{
    // zeichne Verbindungen
    definition.verbindungen(steuerung, zugtyp.spurweite).für_alle(|_name, &verbindung| {
        let verbindung_an_position = Verbindung {
            position: position.transformation(verbindung.position),
            richtung: position.winkel + verbindung.richtung,
        };
        let GehaltenVerbindung { gehalten, andere_entgegengesetzt, andere_gehalten } =
            ist_gehalten_und_andere_verbindung(
                rstern,
                zugtyp,
                &ist_gehalten,
                AnyId::from(gleis_id.clone()),
                verbindung_an_position,
            );
        frame.with_save(|frame| {
            let a = Transparenz::true_reduziert(gehalten).alpha();
            let g = if andere_entgegengesetzt { 1. } else { 0. };
            let color = Color { r: 0., g, b: 1. - g, a };
            let richtung = Vektor::polar_koordinaten(Skalar(5.), verbindung.richtung);
            let richtung_seite = Skalar(0.5) * richtung.rotiert(winkel::FRAC_PI_2);
            let verbindung_position = verbindung.position;
            let mut path_builder = pfad::Erbauer::neu();
            path_builder.move_to(verbindung_position + richtung_seite);
            path_builder.line_to(verbindung_position + richtung);
            path_builder.line_to(verbindung_position - richtung_seite);
            let path = path_builder.baue();
            frame.stroke(
                &path,
                Stroke { style: stroke::Style::Solid(color), width: 1.5, ..Stroke::default() },
            );
            // Füllen für verbundene/einrasten bei drag&drop
            if andere_gehalten {
                frame.fill(&path, Fill { style: fill::Style::Solid(color), ..Fill::default() });
            }
        });
    });
}

/// Erzeuge den Text für Beschreibung und Name eines Gleises.
fn schreibe_gleis_beschreibung_name<T>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    definition: &<T as MitSteuerung>::SelfUnit,
    steuerung: &<T as MitSteuerung>::Steuerung,
    gleis_id: &GleisId<T>,
    ist_gehalten: impl Fn(AnyId) -> bool,
    farbe: Farbe,
    skalieren: Skalar,
) where
    AnyId: From<GleisId<T>>,
    T: MitSteuerung,
    <T as MitSteuerung>::SelfUnit: Zeichnen<<T as MitSteuerung>::Steuerung>,
{
    let (relative_position, beschreibung, name) =
        definition.beschreibung_und_name(steuerung, spurweite);
    if let Some(content) = match (beschreibung, name) {
        (Some(beschreibung), Some(name)) => Some(format!("{} ({})", name, beschreibung)),
        (None, Some(name)) => Some(String::from(name)),
        (Some(beschreibung), None) => Some(String::from(beschreibung)),
        (None, None) => None,
    } {
        frame.with_save(|frame| {
            bewege_an_position(frame, &relative_position);
            let a =
                Transparenz::true_reduziert(ist_gehalten(AnyId::from(gleis_id.clone()))).alpha();
            let mut text =
                Text { content, color: Color { a, ..Color::from(farbe) }, ..standard_text() };
            text.size *= skalieren.0;
            frame.fill_text(text);
        })
    }
}

impl GleiseDaten {
    /// Füge die Darstellung aller Gleise dem Frame hinzu.
    pub(in crate::gleis::gleise) fn darstellen_aller_gleise<L: Leiter>(
        &self,
        frame: &mut Frame<'_>,
        zugtyp: &Zugtyp<L>,
        streckenabschnitte: &StreckenabschnittMap,
        transparent_hintergrund: impl Fn(AnyId, Fließend) -> Transparenz,
        ist_gehalten: impl Fn(AnyId) -> bool,
        farbe: Farbe,
        skalieren: Skalar,
    ) {
        macro_rules! färbe_hintergrund {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr, $definition_id: expr, $spurweite: expr, $position: expr) => {{
                let (gleis, _rectangle) = match $gleise.get($gleis_id) {
                    Some(gleis) => gleis,
                    None => {
                        error!("Gleis mit Id '{:?}' nicht gefunden!", $gleis_id);
                        continue;
                    },
                };
                let definition = match $definitionen.get(&gleis.definition) {
                    Some(definition) => definition,
                    None => {
                        error!("Definition mit Id '{:?}' nicht gefunden!", $definition_id);
                        continue;
                    },
                };
                let streckenabschnitt = gleis
                    .streckenabschnitt
                    .as_ref()
                    .and_then(|name| streckenabschnitte.get(name))
                    .map(|(streckenabschnitt, _geschwindigkeit)| {
                        (streckenabschnitt.farbe, streckenabschnitt.fließend())
                    });
                frame.with_save(|frame| {
                    bewege_an_position(frame, $position);
                    // Färbe Hintergrund.
                    fülle_gleis(
                        frame,
                        $spurweite,
                        definition,
                        &gleis.steuerung,
                        $gleis_id,
                        &transparent_hintergrund,
                        streckenabschnitt,
                    );
                })
            }};
        }
        macro_rules! zeichne_kontur {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr, $definition_id: expr, $spurweite: expr, $position: expr) => {{
                let (gleis, _rectangle) = match $gleise.get($gleis_id) {
                    Some(gleis) => gleis,
                    None => {
                        error!("Gleis mit Id '{:?}' nicht gefunden!", $gleis_id);
                        continue;
                    },
                };
                let definition = match $definitionen.get(&gleis.definition) {
                    Some(definition) => definition,
                    None => {
                        error!("Definition mit Id '{:?}' nicht gefunden!", $definition_id);
                        continue;
                    },
                };
                frame.with_save(|frame| {
                    bewege_an_position(frame, $position);
                    // Zeichne Kontur.
                    zeichne_gleis(
                        frame,
                        $spurweite,
                        definition,
                        &gleis.steuerung,
                        $gleis_id,
                        &ist_gehalten,
                        farbe,
                    );
                })
            }};
        }
        macro_rules! zeichne_verbindungen {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr, $definition_id: expr, $spurweite: expr, $position: expr) => {{
                let (gleis, _rectangle) = match $gleise.get($gleis_id) {
                    Some(gleis) => gleis,
                    None => {
                        error!("Gleis mit Id '{:?}' nicht gefunden!", $gleis_id);
                        continue;
                    },
                };
                let definition = match $definitionen.get(&gleis.definition) {
                    Some(definition) => definition,
                    None => {
                        error!("Definition mit Id '{:?}' nicht gefunden!", $definition_id);
                        continue;
                    },
                };
                frame.with_save(|frame| {
                    bewege_an_position(frame, $position);
                    // Zeichne Verbindungen.
                    zeichne_verbindungen(
                        frame,
                        &self.rstern,
                        zugtyp,
                        definition,
                        &gleis.steuerung,
                        $gleis_id,
                        &ist_gehalten,
                        $position,
                    );
                })
            }};
        }
        macro_rules! schreibe_name_und_beschreibung {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr, $definition_id: expr, $spurweite: expr, $position: expr) => {{
                let (gleis, _rectangle) = match $gleise.get($gleis_id) {
                    Some(gleis) => gleis,
                    None => {
                        error!("Gleis mit Id '{:?}' nicht gefunden!", $gleis_id);
                        continue;
                    },
                };
                let definition = match $definitionen.get(&gleis.definition) {
                    Some(definition) => definition,
                    None => {
                        error!("Definition mit Id '{:?}' nicht gefunden!", $definition_id);
                        continue;
                    },
                };
                frame.with_save(|frame| {
                    bewege_an_position(frame, $position);
                    // Schreibe Name und Beschreibung.
                    schreibe_gleis_beschreibung_name(
                        frame,
                        $spurweite,
                        definition,
                        &gleis.steuerung,
                        $gleis_id,
                        &ist_gehalten,
                        farbe,
                        skalieren,
                    );
                })
            }};
        }
        for geom_with_data in self.rstern.iter() {
            let (gleis_definition_id, position) = &geom_with_data.data;
            mit_any_id!(
                {ref self, ref zugtyp},
                [AnyGleisDefinitionId => gleis_id, definition_id] gleis_definition_id
                => färbe_hintergrund!(zugtyp.spurweite, position)
            );
            mit_any_id!(
                {ref self, ref zugtyp},
                [AnyGleisDefinitionId => gleis_id, definition_id] gleis_definition_id
                => zeichne_kontur!(zugtyp.spurweite, position)
            );
            mit_any_id!(
                {ref self, ref zugtyp},
                [AnyGleisDefinitionId => gleis_id, definition_id] gleis_definition_id
                => zeichne_verbindungen!(zugtyp.spurweite, position)
            );
            mit_any_id!(
                {ref self, ref zugtyp},
                [AnyGleisDefinitionId => gleis_id, definition_id] gleis_definition_id
                => schreibe_name_und_beschreibung!(zugtyp.spurweite, position)
            );
        }
    }
}

// TODO innerhalb auf enum umstellen, dass zwischen
// wirklich_innerhalb und innerhalb_toleranz unterscheidet?
const KLICK_GENAUIGKEIT: Skalar = Skalar(5.);

impl GleiseDaten {
    /// Erhalte die Id, Steuerung, relative Klick-Position, Winkel und Streckenabschnitt des Gleises an der gesuchten Position.
    fn gleis_an_position<L: Leiter>(
        &self,
        zugtyp: &Zugtyp<L>,
        canvas_pos: Vektor,
    ) -> Option<(AnyIdSteuerung, Vektor, Winkel, Option<streckenabschnitt::Name>)> {
        for geom_with_data in self.rstern.locate_all_at_point(&canvas_pos) {
            let (gleis_definition_id, position) = &geom_with_data.data;
            let relative_pos = canvas_pos - position.punkt;
            let rotated_pos = relative_pos.rotiert(-position.winkel);
            macro_rules! gleis_an_position_aux {
                ($gleise: expr, $definitionen: expr, $gleis_id: expr, $definition_id: expr) => {{
                    let (gleis, _rectangle) = $gleise.get($gleis_id)?;
                    let definition = $definitionen.get($definition_id)?;
                    if definition.innerhalb(&(), zugtyp.spurweite, rotated_pos, KLICK_GENAUIGKEIT) {
                        return Some((
                            AnyIdSteuerung::from(($gleis_id.clone(), gleis.steuerung.clone())),
                            relative_pos,
                            position.winkel,
                            gleis.streckenabschnitt.clone(),
                        ));
                    }
                }};
            }
            mit_any_id!(
                {ref self, ref zugtyp},
                [AnyGleisDefinitionId => gleis_id, definition_id] gleis_definition_id
                => gleis_an_position_aux!()
            );
        }
        None
    }
}

/// SelectionFunction, die jedes Element akzeptiert.
/// Haupt-Nutzen ist das vollständiges Leeren eines RTree (siehe [`GleiseDaten::verschmelze`]).
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
    fn should_unpack_parent(&self, envelope: &AABB<Vektor>) -> bool {
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
