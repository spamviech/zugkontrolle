//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{collections::HashMap, fmt::Debug};

use assoc::vec::{AssocExt, Entry};
use nonempty::NonEmpty;
use num_traits::{bounds::LowerBounded, CheckedAdd, One};
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
    util::enumerate_checked::EnumerateCheckedExt,
};

pub mod gerade;
pub mod kreuzung;
pub mod kurve;
pub mod weiche;
pub mod zugtyp;

type AssocList<K, V> = Vec<(K, V)>;

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [Anschlüsse](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [Canvas](iced::widget::canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct GleiseDatenSerialisiert {
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
}

impl GleiseDatenSerialisiert {
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

#[derive(Debug)]
struct DefinitionMaps {
    geraden: AssocList<GeradeSerialisiert, (id::Repräsentation, GeradeUnit)>,
    kurven: AssocList<KurveSerialisiert, (id::Repräsentation, KurveUnit)>,
    weichen: AssocList<WeicheSerialisiert, (id::Repräsentation, WeicheUnit)>,
    dreiwege_weichen:
        AssocList<DreiwegeWeicheSerialisiert, (id::Repräsentation, DreiwegeWeicheUnit)>,
    kurven_weichen: AssocList<KurvenWeicheSerialisiert, (id::Repräsentation, KurvenWeicheUnit)>,
    s_kurven_weichen: AssocList<SKurvenWeicheSerialisiert, (id::Repräsentation, SKurvenWeicheUnit)>,
    kreuzungen: AssocList<KreuzungSerialisiert, (id::Repräsentation, KreuzungUnit)>,
}

impl DefinitionMaps {
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

impl GleiseDatenSerialisiert {
    fn v4<L: Leiter>(
        self,
        zugtyp: &mut v4::ZugtypSerialisiert2<L>,
        definition_maps: &mut DefinitionMaps,
        streckenabschnitt: &Option<streckenabschnitt::Name>,
    ) -> (v4::GleiseDatenSerialisiert, Option<NonEmpty<KeineIdVerfügbar>>) {
        let mut fehler = Vec::new();

        macro_rules! erstelle_maps {
            ($($gleis_art: ident - $steuerung: ident : $typ: ident),* $(,)?) => {{
                let GleiseDatenSerialisiert { $($gleis_art),* } = self;
                $(
                    let definitionen = &mut zugtyp.$gleis_art;
                    let definitionen_invertiert = &mut definition_maps.$gleis_art;
                    let ($gleis_art, _nächste_id) = $gleis_art.into_iter().enumerate_checked().fold(
                        (HashMap::new(), Some(0)),
                        |(mut elemente, mut nächste_definition_id), (gleis_id, gleis)| {
                            let Gleis { definition, position } = gleis;
                            let Some(gleis_id) = gleis_id else {
                                fehler.push(KeineIdVerfügbar::für_ref(&definition));
                                return (elemente, nächste_definition_id);
                            };
                            let steuerung = definition.$steuerung.clone().map(Into::into);
                            let (definition_id, definition)
                                = match definitionen_invertiert.entry(definition.clone()) {
                                    Entry::Occupied(occupied) => occupied.get().clone(),
                                    Entry::Vacant(vacant) => {
                                        let id = if let Some(id) = nächste_definition_id {
                                            nächste_definition_id = id.checked_add(&1);
                                            id
                                        } else {
                                            fehler.push(KeineIdVerfügbar::für::<$typ<()>>());
                                            return (elemente, nächste_definition_id);
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
                                streckenabschnitt: streckenabschnitt.clone(),
                            };
                            // gleis_id ist eindeutig, da es durch enumerate erzeugt wurde
                            let _ = elemente.insert(gleis_id, v4_gleis);
                            (elemente, nächste_definition_id)
                        },
                    );
                )*
                v4::GleiseDatenSerialisiert { $($gleis_art),* }
            }};
        }

        let v4 = erstelle_maps!(
            geraden - kontakt : Gerade,
            kurven - kontakt : Kurve,
            weichen - steuerung : Weiche,
            dreiwege_weichen - steuerung : DreiwegeWeiche,
            kurven_weichen - steuerung : KurvenWeiche,
            s_kurven_weichen - steuerung : SKurvenWeiche,
            kreuzungen - steuerung : Kreuzung,
        );

        (v4, NonEmpty::from_vec(fehler))
    }
}

pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
pub(in crate::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<LeiterSerialisiert> =
    HashMap<
        geschwindigkeit::Name,
        (GeschwindigkeitSerialisiert<LeiterSerialisiert>, StreckenabschnittMapSerialisiert),
    >;

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
    pub(crate) zugtyp: ZugtypSerialisiert<L>,
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<S>,
    pub(crate) pläne: HashMap<plan::Name, PlanSerialisiert<L, S>>,
}

impl<L: Leiter, S> From<ZustandSerialisiert<L, S>> for v4::ZustandSerialisiert<L, S> {
    fn from(value: ZustandSerialisiert<L, S>) -> Self {
        todo!()
    }
}
