//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{collections::HashMap, fmt::Debug};

use associated_list::{entry::Entry, AssocList};
use log::error;
use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::{Gerade, GeradeUnit},
        gleise::{
            daten::{
                v3::{
                    gerade::GeradeSerialisiert,
                    kreuzung::KreuzungSerialisiert,
                    kurve::KurveSerialisiert,
                    weiche::{
                        dreiwege::DreiwegeWeicheSerialisiert, gerade::WeicheSerialisiert,
                        kurve::KurvenWeicheSerialisiert, s_kurve::SKurvenWeicheSerialisiert,
                    },
                    zugtyp::ZugtypSerialisiert,
                },
                v4,
            },
            id::{self, eindeutig::KeineIdVerfügbar},
        },
        kreuzung::{Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert, Leiter},
        plan::{self, PlanSerialisiert},
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    typen::canvas::Position,
};

pub mod gerade;
pub mod kreuzung;
pub mod kurve;
pub mod weiche;
pub mod zugtyp;

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [`Anschlüsse`](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [`Canvas`](iced::widget::canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

/// Die serialisierte Darstellung aller Gleise, wie sie in Version 3 verwendet wurde.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct GleiseDatenSerialisiert {
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
}

impl GleiseDatenSerialisiert {
    /// Erzeuge ein neues, leeres [`GleiseDatenSerialisiert`]
    pub(crate) const fn neu() -> GleiseDatenSerialisiert {
        GleiseDatenSerialisiert {
            geraden: Vec::new(),
            kurven: Vec::new(),
            weichen: Vec::new(),
            dreiwege_weichen: Vec::new(),
            kurven_weichen: Vec::new(),
            s_kurven_weichen: Vec::new(),
            kreuzungen: Vec::new(),
        }
    }
}

/// Mapping von der serialisierten Darstellung zur assoziierten [`id::Repräsentation`]
/// für eine Definition. Verwendet in [`GleiseDatenSerialisiert::v4`].
#[derive(Debug)]
struct DefinitionMaps {
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: AssocList<GeradeSerialisiert, (id::Repräsentation, GeradeUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: AssocList<KurveSerialisiert, (id::Repräsentation, KurveUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: AssocList<WeicheSerialisiert, (id::Repräsentation, WeicheUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen:
        AssocList<DreiwegeWeicheSerialisiert, (id::Repräsentation, DreiwegeWeicheUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: AssocList<KurvenWeicheSerialisiert, (id::Repräsentation, KurvenWeicheUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: AssocList<SKurvenWeicheSerialisiert, (id::Repräsentation, SKurvenWeicheUnit)>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: AssocList<KreuzungSerialisiert, (id::Repräsentation, KreuzungUnit)>,
}

impl DefinitionMaps {
    /// Erzeuge neue, leere [`DefinitionMaps`].
    fn neu() -> DefinitionMaps {
        DefinitionMaps {
            geraden: AssocList::new(),
            kurven: AssocList::new(),
            weichen: AssocList::new(),
            dreiwege_weichen: AssocList::new(),
            kurven_weichen: AssocList::new(),
            s_kurven_weichen: AssocList::new(),
            kreuzungen: AssocList::new(),
        }
    }
}

/// Die nächste freie [`id::Repräsentation`] für eine Definition.
#[derive(Debug)]
struct NächsteDefinitionIds {
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: Option<id::Repräsentation>,
}

impl NächsteDefinitionIds {
    /// Erzeuge eine neue [`NächsteDefinitionIds`], die alle mit [`Some(0)`] initialisiert wurden.
    fn neu() -> NächsteDefinitionIds {
        NächsteDefinitionIds {
            geraden: Some(0),
            kurven: Some(0),
            weichen: Some(0),
            dreiwege_weichen: Some(0),
            kurven_weichen: Some(0),
            s_kurven_weichen: Some(0),
            kreuzungen: Some(0),
        }
    }
}

/// Die nächste freie [`id::Repräsentation`] für ein Gleis oder eine Definition.
#[derive(Debug)]
struct NächsteIds {
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: Option<id::Repräsentation>,
    #[allow(clippy::missing_docs_in_private_items)]
    definitionen: NächsteDefinitionIds,
}

impl NächsteIds {
    /// Erzeuge eine neue [`NächsteIds`], die alle mit [`Some(0)`] initialisiert wurden.
    fn neu() -> NächsteIds {
        NächsteIds {
            geraden: Some(0),
            kurven: Some(0),
            weichen: Some(0),
            dreiwege_weichen: Some(0),
            kurven_weichen: Some(0),
            s_kurven_weichen: Some(0),
            kreuzungen: Some(0),
            definitionen: NächsteDefinitionIds::neu(),
        }
    }
}

impl GleiseDatenSerialisiert {
    /// Konvertiere in die serialisierbare Darstellung der Version 4.
    fn v4<L: Leiter>(
        self,
        zugtyp: &mut v4::ZugtypSerialisiert<L>,
        definition_maps: &mut DefinitionMaps,
        nächste_ids: &mut NächsteIds,
        fehler: &mut Vec<KeineIdVerfügbar>,
        streckenabschnitt: Option<&streckenabschnitt::Name>,
    ) -> v4::GleiseDatenSerialisiert {
        /// Erstelle die Maps für die übergebenen `(gleis_art, steuerung_ident, Typ)`-Tripel.
        macro_rules! erstelle_maps {
            ($($gleis_art: ident - $steuerung: ident : $typ: ident),* $(,)?) => {{
                let GleiseDatenSerialisiert { $($gleis_art),* } = self;
                $(
                    let definitionen = &mut zugtyp.$gleis_art;
                    let definitionen_invertiert = &mut definition_maps.$gleis_art;
                    let ($gleis_art, nächste_definition_id, nächste_gleis_id) = $gleis_art.into_iter().fold(
                        (HashMap::new(), nächste_ids.definitionen.$gleis_art, nächste_ids.$gleis_art),
                        |(mut elemente, mut nächste_definition_id, gleis_id), gleis| {
                            let Gleis { definition, position } = gleis;
                            let Some(gleis_id) = gleis_id else {
                                fehler.push(KeineIdVerfügbar::für_ref(&definition));
                                return (elemente, nächste_definition_id, gleis_id);
                            };
                            let steuerung = definition.$steuerung.clone().map(Into::into);
                            let (definition_id, definition)
                                = match definitionen_invertiert.entry(definition.clone()) {
                                    Entry::Occupied(occupied) => occupied.get().clone(),
                                    Entry::Vacant(vacant) => {
                                        let id = if let Some(id) = nächste_definition_id {
                                            nächste_definition_id = id.checked_add(1);
                                            id
                                        } else {
                                            fehler.push(KeineIdVerfügbar::für::<$typ<()>>());
                                            return (elemente, nächste_definition_id, Some(gleis_id));
                                        };
                                        let definition: $typ<()> = definition.into();
                                        let _ = vacant.insert((id, definition.clone()));
                                        (id, definition)
                                    },
                                };
                            let _ = definitionen.insert(definition_id, definition);
                            let v4_gleis: v4::GleisSerialisiert<$typ> = v4::GleisSerialisiert {
                                definition: definition_id,
                                steuerung,
                                position,
                                streckenabschnitt: streckenabschnitt.cloned(),
                            };
                            // gleis_id ist eindeutig, da es durch enumerate erzeugt wurde
                            let _ = elemente.insert(gleis_id, v4_gleis);
                            (elemente, nächste_definition_id, gleis_id.checked_add(1))
                        },
                    );
                    nächste_ids.$gleis_art = nächste_gleis_id;
                    nächste_ids.definitionen.$gleis_art = nächste_definition_id;
                )*
                v4::GleiseDatenSerialisiert { $($gleis_art),* }
            }};
        }

        erstelle_maps!(
            geraden - kontakt : Gerade,
            kurven - kontakt : Kurve,
            weichen - steuerung : Weiche,
            dreiwege_weichen - steuerung : DreiwegeWeiche,
            kurven_weichen - steuerung : KurvenWeiche,
            s_kurven_weichen - steuerung : SKurvenWeiche,
            kreuzungen - steuerung : Kreuzung,
        )
    }
}

/// Serialisierbare Streckenabschnitte mit Name.
pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
/// Serialisierbare Geschwindigkeiten mit Name.
pub(in crate::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<LeiterSerialisiert> =
    HashMap<
        geschwindigkeit::Name,
        (GeschwindigkeitSerialisiert<LeiterSerialisiert>, StreckenabschnittMapSerialisiert),
    >;

/// Die serialisierbare Darstellung des aktuelle Zustands, wie sie in Version 3 verwendet wurde.
#[derive(zugkontrolle_macros::Debug, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[serde(bound(
    serialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize, <L as Leiter>::Fahrtrichtung: Serialize, S: Serialize",
    deserialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>, S: Deserialize<'de>",
))]
pub(in crate::gleis::gleise) struct ZustandSerialisiert<L: Leiter, S> {
    /// Der serialisierbare Zugtyp.
    pub(crate) zugtyp: ZugtypSerialisiert<L>,
    /// Gleise ohne einen assoziierten Streckenabschnitt.
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    /// Streckenabschnitte ohne assoziierte Geschwindigkeit, sowie zugehörige Gleise.
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    /// Geschwindigkeiten und assoziierte Streckenabschnitte und Gleise.
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<S>,
    /// Pläne.
    pub(crate) pläne: HashMap<plan::Name, PlanSerialisiert<L, S>>,
}

impl<L: Leiter, S> ZustandSerialisiert<L, S> {
    /// Konvertiere in die serialisierbare Darstellung der Version 4.
    pub(crate) fn v4(self, fehler: &mut Vec<KeineIdVerfügbar>) -> v4::ZustandSerialisiert<L, S> {
        let ZustandSerialisiert {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne,
        } = self;
        let mut zugtyp = zugtyp.v4(fehler);
        let mut definition_maps = DefinitionMaps::neu();
        let mut nächste_ids = NächsteIds::neu();
        let mut gleise = ohne_streckenabschnitt.v4(
            &mut zugtyp,
            &mut definition_maps,
            &mut nächste_ids,
            fehler,
            None,
        );
        let mut konvertiere_streckenabschnitt_map =
            |streckenabschnitt_map: StreckenabschnittMapSerialisiert,
             geschwindigkeit: Option<&geschwindigkeit::Name>| {
                let mut streckenabschnitte = HashMap::new();
                for (name, (streckenabschnitt, gleise_daten)) in streckenabschnitt_map {
                    gleise.verschmelze(gleise_daten.v4(
                        &mut zugtyp,
                        &mut definition_maps,
                        &mut nächste_ids,
                        fehler,
                        Some(&name),
                    ));
                    // name ist eindeutig, da er aus einer HashMap kommt
                    // (und Assoziation zu einer Geschwindigkeit in v3 nicht möglich ist)
                    let bisher = streckenabschnitte
                        .insert(name.clone(), (streckenabschnitt, geschwindigkeit.cloned()));
                    if let Some(bisher) = bisher {
                        error!("Streckenabschnitt {} überschrieben!\n{:?}", name.0, bisher);
                    }
                }
                streckenabschnitte
            };
        let mut streckenabschnitte = konvertiere_streckenabschnitt_map(ohne_geschwindigkeit, None);
        let mut v4_geschwindigkeiten = HashMap::new();
        for (name, (geschwindigkeit, streckenabschnitt_map)) in geschwindigkeiten {
            let neue_streckenabschnitte =
                konvertiere_streckenabschnitt_map(streckenabschnitt_map, Some(&name));
            streckenabschnitte.extend(neue_streckenabschnitte);
            // name ist eindeutig, da er aus einer HashMap kommt
            let _ = v4_geschwindigkeiten.insert(name, geschwindigkeit);
        }
        v4::ZustandSerialisiert {
            zugtyp,
            geschwindigkeiten: v4_geschwindigkeiten,
            streckenabschnitte,
            gleise,
            pläne,
        }
    }
}
