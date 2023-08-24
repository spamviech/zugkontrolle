//! Struktur zum Speichern aller Gleise.

use std::{collections::HashMap, fmt::Debug, iter, marker::PhantomData};

use log::error;
use rstar::{
    primitives::{GeomWithData, Rectangle},
    Envelope, RTree, RTreeObject, SelectionFunction, AABB,
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
                eindeutig::KeineIdVerfügbar, mit_any_id, AnyDefinitionId2,
                AnyDefinitionIdSteuerung2, AnyGleisDefinitionId2, AnyId, AnyId2, AnyIdRef,
                DefinitionId2, GleisId, GleisId2, GleisIdRef, StreckenabschnittId,
                StreckenabschnittIdRef,
            },
            steuerung::MitSteuerung,
            GeschwindigkeitEntferntFehler, GleisIdFehler, StreckenabschnittIdFehler,
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
        canvas::Position, mm::Spurweite, rechteck::Rechteck, skalar::Skalar, vektor::Vektor,
        Zeichnen,
    },
    util::nachschlagen::Nachschlagen,
    zugtyp::{DefinitionMap2, Zugtyp, Zugtyp2},
};

pub mod de_serialisieren;
pub mod v2;
pub mod v3;

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [Anschlüsse](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [Canvas](iced::widget::canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

/// Definition und Position eines Gleises.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<T as MitSteuerung>::Steuerung: Debug)]
#[zugkontrolle_clone(<T as MitSteuerung>::Steuerung: Clone)]
#[zugkontrolle_clone(<T as MitSteuerung>::SelfUnit: Clone)]
pub struct Gleis2<T: MitSteuerung>
where
    <T as MitSteuerung>::SelfUnit: 'static,
{
    /// Die [Zeichnen]-Definition des Gleises.
    pub definition: DefinitionId2<T>,
    /// Die [Anschlüsse](anschluss::Anschluss) des Gleises.
    pub steuerung: <T as MitSteuerung>::Steuerung,
    /// Die Position des Gleises auf dem [Canvas](iced::widget::canvas::Canvas).
    pub position: Position,
    /// Der [Streckenabschnitt] des Gleises.
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

#[allow(single_use_lifetimes)]
impl<T, S> Serialisiere<Gleis<S>> for Gleis<T>
where
    T: Serialisiere<S>,
    S:,
{
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

/// Alle [Gleise](Gleis), [Geschwindigkeiten](Geschwindigkeit) und [Streckenabschnitte](Streckenabschnitt),
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
        gehalten_id: Option<&'t AnyIdRef<'t>>,
    ) -> (impl 't + Iterator<Item = Verbindung>, bool) {
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

pub(crate) type StreckenabschnittMap2 =
    HashMap<streckenabschnitt::Name, (Streckenabschnitt, Option<geschwindigkeit::Name>)>;
type GeschwindigkeitMap2<Leiter> = HashMap<geschwindigkeit::Name, Geschwindigkeit<Leiter>>;

/// Alle [Gleise](Gleis), [Geschwindigkeiten](Geschwindigkeit) und [Streckenabschnitte](Streckenabschnitt),
/// sowie der verwendete [Zugtyp].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub(in crate::gleis::gleise) struct Zustand2<L: Leiter> {
    pub(in crate::gleis::gleise) zugtyp: Zugtyp2<L>,
    pub(in crate::gleis::gleise) geschwindigkeiten: GeschwindigkeitMap2<L>,
    pub(in crate::gleis::gleise) streckenabschnitte: StreckenabschnittMap2,
    pub(in crate::gleis::gleise) gleise: GleiseDaten2,
    pub(in crate::gleis::gleise) pläne: HashMap<plan::Name, Plan<L>>,
}

impl<L: Leiter> Zustand2<L> {
    /// Erstelle einen neuen [Zustand].
    pub(in crate::gleis::gleise) fn neu(zugtyp: Zugtyp2<L>) -> Self {
        Zustand2 {
            zugtyp,
            geschwindigkeiten: GeschwindigkeitMap2::new(),
            streckenabschnitte: StreckenabschnittMap2::new(),
            gleise: GleiseDaten2::neu(),
            pläne: HashMap::new(),
        }
    }

    pub(in crate::gleis::gleise) fn zugtyp(&self) -> &Zugtyp2<L> {
        &self.zugtyp
    }

    pub(in crate::gleis::gleise) fn geschwindigkeiten(&self) -> &GeschwindigkeitMap2<L> {
        &self.geschwindigkeiten
    }

    pub(in crate::gleis::gleise) fn streckenabschnitte(&self) -> &StreckenabschnittMap2 {
        &self.streckenabschnitte
    }

    pub(in crate::gleis::gleise) fn rstern(&self) -> &RStern2 {
        &self.gleise.rstern
    }

    /// Füge ein neues Gleis an der [Position] mit dem gewählten [Streckenabschnitt] hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen<T>(
        &mut self,
        definition_steuerung: impl Into<AnyDefinitionIdSteuerung2>,
        position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId2, HinzufügenFehler> {
        self.gleise.hinzufügen(
            &self.zugtyp,
            definition_steuerung.into(),
            position,
            streckenabschnitt,
            einrasten,
        )
    }

    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen_anliegend<T>(
        &mut self,
        definition_steuerung: impl Into<AnyDefinitionIdSteuerung2>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        verbindung_name: &<<T as MitSteuerung>::SelfUnit as Zeichnen>::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<GleisId2<T>, HinzufügenFehler>
    where
        T: MitSteuerung + DatenAuswahl2,
        <T as MitSteuerung>::SelfUnit: Zeichnen,
        <<T as MitSteuerung>::SelfUnit as Zeichnen>::Verbindungen:
            verbindung::Nachschlagen<<<T as MitSteuerung>::SelfUnit as Zeichnen>::VerbindungName>,
        <T as MitSteuerung>::Steuerung: Debug,
        AnyGleisDefinitionId2: From<(GleisId2<T>, DefinitionId2<T>)>,
    {
        // let spurweite = self.zugtyp.spurweite;
        // let definition = match self.zugtyp.definition_map::<T>().get(&definition_id) {
        //     Some(definition) => definition,
        //     None => return Err(HinzufügenFehler::DefinitionNichtGefunden(definition_id)),
        // };
        // // berechne neue position
        // let position =
        //     Position::anliegend_position(spurweite, definition, verbindung_name, ziel_verbindung);
        // füge neues Gleis hinzu
        // self.hinzufügen(definition_steuerung, position, streckenabschnitt, false)
        todo!()
    }

    /// Bewege ein Gleis an die neue position.
    fn bewegen_aux<T: Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: GleisId2<T>,
        berechne_position: impl FnOnce(&Zustand2<L>, &Gleis2<T>) -> Position,
    ) -> Result<(), GleisIdFehler> {
        // let spurweite = self.zugtyp.spurweite;
        // let GleisId2 { rectangle, streckenabschnitt, phantom: _ } = &*gleis_id;
        // // Entferne aktuellen Eintrag.
        // let rstern = self.daten_mut(&streckenabschnitt)?.rstern_mut::<T>();
        // let gleis = rstern
        //     .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
        //     .ok_or(GleisIdFehler::GleisEntfernt)?
        //     .data;
        // // Füge an neuer Position hinzu.
        // // Wegen Referenz auf self muss rstern kurzfristig vergessen werden.
        // let position_neu = berechne_position(self, &gleis);
        // let definition = gleis.definition;
        // let rectangle = Rectangle::from(definition.rechteck_an_position(spurweite, &position_neu));
        // let rstern = self.daten_mut(&streckenabschnitt)?.rstern_mut::<T>();
        // rstern.insert(GeomWithData::new(
        //     rectangle.clone(),
        //     Gleis { definition, position: position_neu },
        // ));
        // // Aktualisiere GleisId
        // gleis_id.rectangle = rectangle;
        // Ok(())
        todo!()
    }

    /// Bewege ein Gleis an die neue position.
    pub(in crate::gleis::gleise) fn bewegen<T>(
        &mut self,
        gleis_id: GleisId2<T>,
        position: Position,
        einrasten: bool,
    ) -> Result<(), BewegenFehler2>
    where
        AnyId2: From<GleisId2<T>>,
    {
        self.gleise.bewegen(&self.zugtyp, AnyId2::from(gleis_id), position, einrasten)
    }

    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(in crate::gleis::gleise) fn bewegen_anliegend<T>(
        &mut self,
        gleis_id: &mut GleisId2<T>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<(), GleisIdFehler>
    where
        T: Zeichnen + DatenAuswahl,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        // let spurweite = self.zugtyp.spurweite;
        // self.bewegen_aux(gleis_id, |_zustand, Gleis { definition, position: _ }| {
        //     Position::anliegend_position(spurweite, definition, verbindung_name, ziel_verbindung)
        // })
        todo!()
    }

    /// Entferne das Gleis assoziiert mit der [GleisId].
    pub(in crate::gleis::gleise) fn entfernen<T: Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: GleisId2<T>,
    ) -> Result<Gleis2<T>, EntfernenFehler2>
    where
        T: MitSteuerung + DatenAuswahl2,
        <T as MitSteuerung>::Steuerung: Debug,
        AnyId2: From<GleisId2<T>>,
        AnyGleisDefinitionId2: From<(GleisId2<T>, DefinitionId2<T>)>,
    {
        self.gleise.entfernen(gleis_id)
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
        gehalten_id: Option<&'t AnyIdRef<'t>>,
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

type GleisMap2<T> = HashMap<GleisId2<T>, (Gleis2<T>, Rectangle<Vektor>)>;

pub(in crate::gleis::gleise) type RStern2 =
    RTree<GeomWithData<Rectangle<Vektor>, (AnyGleisDefinitionId2, Position)>>;

#[derive(Debug)]
pub(crate) struct GleiseDaten2 {
    geraden: GleisMap2<Gerade>,
    kurven: GleisMap2<Kurve>,
    weichen: GleisMap2<Weiche>,
    dreiwege_weichen: GleisMap2<DreiwegeWeiche>,
    kurven_weichen: GleisMap2<KurvenWeiche>,
    s_kurven_weichen: GleisMap2<SKurvenWeiche>,
    kreuzungen: GleisMap2<Kreuzung>,
    // Invariante: Jeder Eintrag hat einen zur Position passenden Eintrag in der RStern-Struktur
    rstern: RStern2,
}

impl GleiseDaten2 {
    /// Erstelle eine leere [GleiseDaten]-Struktur.
    pub(in crate::gleis::gleise) fn neu() -> Self {
        GleiseDaten2 {
            geraden: GleisMap2::new(),
            kurven: GleisMap2::new(),
            weichen: GleisMap2::new(),
            dreiwege_weichen: GleisMap2::new(),
            kurven_weichen: GleisMap2::new(),
            s_kurven_weichen: GleisMap2::new(),
            kreuzungen: GleisMap2::new(),
            rstern: RStern2::new(),
        }
    }

    pub(in crate::gleis::gleise) fn ist_leer(&self) -> bool {
        [
            self.geraden.len(),
            self.kurven.len(),
            self.weichen.len(),
            self.dreiwege_weichen.len(),
            self.kurven_weichen.len(),
            self.s_kurven_weichen.len(),
            self.kreuzungen.len(),
        ]
        .iter()
        .all(|size| *size == 0)
    }

    /// Füge alle Gleise von `neu` zu `self` hinzu.
    fn verschmelze(&mut self, mut neu: GleiseDaten2) {
        macro_rules! kombinierte_map {
            ($($map: ident),* $(,)?) => {$(
                self.$map.extend(neu.$map.drain());
            )*};
        }
        kombinierte_map! {
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

/// Alle Verbindungen in der Nähe der übergebenen Position im zugehörigen [RStern].
/// Der erste Rückgabewert sind alle [Verbindungen](Verbindung) in der Nähe,
/// der zweite, ob eine Verbindung der `gehalten_id` darunter war.
fn überlappende_verbindungen<'t, L: Leiter>(
    rstern: &'t RStern2,
    zugtyp: &'t Zugtyp2<L>,
    verbindung: &'t Verbindung,
    eigene_id: Option<&'t AnyId2>,
    gehalten_id: Option<&'t AnyId2>,
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
            ($gleis_id: expr, $definition_id: expr, $gleisart: ident) => {{
                let kandidat_id = AnyId2::from($gleis_id.clone());
                let nicht_eigene_id = Some(&kandidat_id) != eigene_id;
                nicht_eigene_id.then(|| {
                    zugtyp.$gleisart.get(&$definition_id).map(|definition| {
                        (
                            alle_verbindungen(
                                definition
                                    .verbindungen_an_position(zugtyp.spurweite, position.clone()),
                            ),
                            gehalten_id.map_or(false, |id| *id == kandidat_id),
                        )
                    })
                })
            }};
        }

        let (kandidat_verbindungen, kandidat_ist_gehalten) = {
            use AnyGleisDefinitionId2::*;
            match &kandidat_ids {
                Gerade(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, geraden)
                },
                Kurve(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, kurven)
                },
                Weiche(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, weichen)
                },
                DreiwegeWeiche(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, dreiwege_weichen)
                },
                KurvenWeiche(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, kurven_weichen)
                },
                SKurvenWeiche(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, s_kurven_weichen)
                },
                Kreuzung(gleis_id, definition_id) => {
                    erhalte_alle_verbindungen!(gleis_id, definition_id, kreuzungen)
                },
            }
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

fn einraste_position<L: Leiter, T: Zeichnen>(
    rstern: &RStern2,
    zugtyp: &Zugtyp2<L>,
    definition: &T,
    position: Position,
) -> Position {
    let mut snap = None;
    let verbindungen = definition.verbindungen_an_position(zugtyp.spurweite, position.clone());
    verbindungen.für_alle(|verbindung_name, verbindung| {
        if snap.is_none() {
            let (mut überlappende, _gehalten) =
                überlappende_verbindungen(rstern, zugtyp, verbindung, None, None);
            snap = überlappende.next().map(|überlappend| (verbindung_name, überlappend));
        }
    });
    snap.map_or(position, |(einrasten_name, einrasten_verbindung)| {
        Position::anliegend_position(
            zugtyp.spurweite,
            definition,
            &einrasten_name,
            einrasten_verbindung,
        )
    })
}

macro_rules! daten_mit_any_id2 {
    ($daten: expr, $zugtyp: expr, [$id: ty => $($ident: ident),+] $any_id: expr => $macro: ident ! ( $($extra_arg: expr),* $(,)? ) ) => {{
        use $id::*;
        match $any_id {
            Gerade( $($ident),+ ) => {
                $macro! (&mut $daten.geraden, & $zugtyp.geraden, $($ident),+ $(, $extra_arg)*)
            }
            Kurve( $($ident),+ ) => {
                $macro! (&mut $daten.kurven, & $zugtyp.kurven, $($ident),+ $(, $extra_arg)*)
            }
            Weiche( $($ident),+ ) => {
                $macro! (&mut $daten.weichen, & $zugtyp.weichen, $($ident),+ $(, $extra_arg)*)
            }
            DreiwegeWeiche( $($ident),+ ) => {
                $macro! (&mut $daten.dreiwege_weichen, & $zugtyp.dreiwege_weichen, $($ident),+ $(, $extra_arg)*)
            }
            KurvenWeiche( $($ident),+ ) => {
                $macro! (&mut $daten.kurven_weichen, & $zugtyp.kurven_weichen, $($ident),+ $(, $extra_arg)*)
            }
            SKurvenWeiche( $($ident),+ ) => {
                $macro! (&mut $daten.s_kurven_weichen, & $zugtyp.s_kurven_weichen, $($ident),+ $(, $extra_arg)*)
            }
            Kreuzung( $($ident),+ ) => {
                $macro! (&mut $daten.kreuzungen, & $zugtyp.kreuzungen, $($ident),+ $(, $extra_arg)*)
            }
        }
    }};
}

#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub(in crate::gleis::gleise) enum HinzufügenFehler {
    DefinitionNichtGefunden(AnyDefinitionId2),
    KeineIdVerfügbar(KeineIdVerfügbar),
}

impl GleiseDaten2 {
    fn hinzufügen<L: Leiter>(
        &mut self,
        zugtyp: &Zugtyp2<L>,
        definition_steuerung: AnyDefinitionIdSteuerung2,
        mut position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId2, HinzufügenFehler> {
        macro_rules! hinzufügen_aux {
            ($gleise: expr, $definitionen: expr, $definition_id: expr, $steuerung: expr) => {{
                // Erhalte Definition.
                let definition = match $definitionen.get(&$definition_id) {
                    Some(definition) => definition,
                    None => {
                        return Err(HinzufügenFehler::DefinitionNichtGefunden(
                            AnyDefinitionId2::from($definition_id),
                        ))
                    },
                };
                // Passe Position an, wenn es eine Verbindung in der Nähe gibt.
                if einrasten {
                    position = einraste_position(&self.rstern, zugtyp, definition, position)
                }
                // Erzeuge neue Id.
                let id = GleisId2::neu()?;
                // Berechne Bounding Box.
                let rectangle =
                    Rectangle::from(definition.rechteck_an_position(zugtyp.spurweite, &position));
                // Füge zu RStern hinzu.
                self.rstern.insert(GeomWithData::new(
                    rectangle.clone(),
                    (
                        AnyGleisDefinitionId2::from((id.clone(), $definition_id.clone())),
                        position.clone(),
                    ),
                ));
                // Füge zu GleiseDaten hinzu.
                let bisher = self.map_mut().insert(
                    id.clone(),
                    (
                        Gleis2 {
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
                Ok(AnyId2::from(id))
            }};
        }
        daten_mit_any_id2!(self, zugtyp, [AnyDefinitionIdSteuerung2 => definition, steuerung] definition_steuerung => hinzufügen_aux!())
    }
}

#[derive(Debug, Clone)]
pub(in crate::gleis::gleise) enum BewegenFehler2 {
    DefinitionNichtGefunden(AnyDefinitionId2),
    GleisNichtGefunden(AnyId2),
}

impl GleiseDaten2 {
    fn bewegen<L: Leiter>(
        &mut self,
        zugtyp: &Zugtyp2<L>,
        gleis_id: AnyId2,
        mut neue_position: Position,
        einrasten: bool,
    ) -> Result<(), BewegenFehler2> {
        macro_rules! bewegen_aux {
            ($gleise: expr, $definitionen: expr, $gleis_id: expr) => {
                // Erhalte Referenz auf das Gleis.
                match $gleise.get_mut(&$gleis_id) {
                    Some((gleis, rectangle)) => {
                        let neues_rectangle = match $definitionen.get(&gleis.definition) {
                            Some(definition) => {
                                // Passe Position an, wenn es eine Verbindung in der Nähe gibt.
                                if einrasten {
                                    neue_position = einraste_position(
                                        &self.rstern,
                                        zugtyp,
                                        definition,
                                        neue_position,
                                    );
                                }
                                // Berechne neue Bounding Box.
                                Rectangle::from(
                                    definition
                                        .rechteck_an_position(zugtyp.spurweite, &neue_position),
                                )
                            },
                            None => {
                                return Err(BewegenFehler2::DefinitionNichtGefunden(
                                    AnyDefinitionId2::from(gleis.definition.clone()),
                                ))
                            },
                        };
                        // Entferne alten Eintrag aus RStern.
                        let result = self.rstern.remove(&GeomWithData::new(
                            rectangle.clone(),
                            (
                                AnyGleisDefinitionId2::from((
                                    $gleis_id.clone(),
                                    gleis.definition.clone(),
                                )),
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
                                AnyGleisDefinitionId2::from((
                                    $gleis_id.clone(),
                                    gleis.definition.clone(),
                                )),
                                gleis.position.clone(),
                            ),
                        ));
                        // Aktualisiere gespeicherte Position und Bounding Box.
                        gleis.position = neue_position;
                        *rectangle = neues_rectangle;
                        Ok(())
                    },
                    None => return Err(BewegenFehler2::GleisNichtGefunden(AnyId2::from($gleis_id))),
                }
            };
        }
        daten_mit_any_id2!(self, zugtyp, [crate::gleis::gleise::id::AnyId2 => id] gleis_id => bewegen_aux!())
    }
}

#[derive(Debug, Clone)]
pub(in crate::gleis::gleise) struct EntfernenFehler2(AnyId2);

impl GleiseDaten2 {
    fn entfernen<T>(&mut self, gleis_id: GleisId2<T>) -> Result<Gleis2<T>, EntfernenFehler2>
    where
        T: MitSteuerung + DatenAuswahl2,
        <T as MitSteuerung>::Steuerung: Debug,
        AnyId2: From<GleisId2<T>>,
        AnyGleisDefinitionId2: From<(GleisId2<T>, DefinitionId2<T>)>,
    {
        // TODO DatenAuswahl2 entfernen
        let (gleis, rectangle) = match self.map_mut().remove(&gleis_id) {
            Some(entry) => entry,
            None => return Err(EntfernenFehler2(AnyId2::from(gleis_id))),
        };
        let id_clone = gleis_id.clone();
        let result = self.rstern.remove(&GeomWithData::new(
            rectangle,
            (
                AnyGleisDefinitionId2::from((gleis_id, gleis.definition.clone())),
                gleis.position.clone(),
            ),
        ));
        if result.is_none() {
            error!(
                "Rectangle für Gleis {gleis:?} mit Id {id_clone:?} konnte nicht entfernt werden!"
            );
        }
        Ok(gleis)
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

/// Trait um eine Referenz auf die Map für den jeweiligen Typ zu bekommen.
/// Kein schönes API, daher nur crate-public.
pub(crate) trait DatenAuswahl2: MitSteuerung + Sized {
    fn map(gleise: &GleiseDaten2) -> &GleisMap2<Self>;
    fn map_mut(gleise: &mut GleiseDaten2) -> &mut GleisMap2<Self>;

    fn definition_map<L: Leiter>(zugtyp: &Zugtyp2<L>) -> &DefinitionMap2<Self>;
    fn definition_map_mut<L: Leiter>(zugtyp: &mut Zugtyp2<L>) -> &mut DefinitionMap2<Self>;
}

impl GleiseDaten2 {
    #[inline]
    pub(in crate::gleis::gleise) fn map<T: DatenAuswahl2>(&self) -> &GleisMap2<T> {
        <T as DatenAuswahl2>::map(self)
    }
    #[inline]
    pub(in crate::gleis::gleise) fn map_mut<T: DatenAuswahl2>(&mut self) -> &mut GleisMap2<T> {
        <T as DatenAuswahl2>::map_mut(self)
    }
}

impl<L: Leiter> Zugtyp2<L> {
    #[inline]
    pub(in crate::gleis::gleise) fn definition_map<T: DatenAuswahl2>(&self) -> &DefinitionMap2<T> {
        <T as DatenAuswahl2>::definition_map(self)
    }

    #[inline]
    pub(in crate::gleis::gleise) fn definition_map_mut<T: DatenAuswahl2>(
        &mut self,
    ) -> &mut DefinitionMap2<T> {
        <T as DatenAuswahl2>::definition_map_mut(self)
    }
}

macro_rules! impl_daten_auswahl {
    ($type: ty, $feld: ident) => {
        impl DatenAuswahl2 for $type {
            fn map(GleiseDaten2 { $feld, .. }: &GleiseDaten2) -> &GleisMap2<Self> {
                $feld
            }

            fn map_mut(GleiseDaten2 { $feld, .. }: &mut GleiseDaten2) -> &mut GleisMap2<Self> {
                $feld
            }

            fn definition_map<L: Leiter>(
                Zugtyp2 { $feld, .. }: &Zugtyp2<L>,
            ) -> &DefinitionMap2<Self> {
                $feld
            }

            fn definition_map_mut<L: Leiter>(
                Zugtyp2 { $feld, .. }: &mut Zugtyp2<L>,
            ) -> &mut DefinitionMap2<Self> {
                $feld
            }
        }
    };
}

impl_daten_auswahl! {Gerade, geraden}
impl_daten_auswahl! {Kurve, kurven}
impl_daten_auswahl! {Weiche, weichen}
impl_daten_auswahl! {DreiwegeWeiche, dreiwege_weichen}
impl_daten_auswahl! {KurvenWeiche, kurven_weichen}
impl_daten_auswahl! {SKurvenWeiche, s_kurven_weichen}
impl_daten_auswahl! {Kreuzung, kreuzungen}
