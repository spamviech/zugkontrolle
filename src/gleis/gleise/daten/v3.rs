//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
};

use assoc::vec::{AssocExt, Entry};
use num_traits::{bounds::LowerBounded, CheckedAdd, One};
use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::Gerade,
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

type AssocList<K, V> = Vec<(K, V)>;

struct EnumerateChecked<C, I> {
    next: Option<C>,
    iterator: I,
}

trait EnumerateCheckedExt<C, I> {
    fn enumerate_checked(self) -> EnumerateChecked<C, I>;
}

impl<C: LowerBounded, I: Iterator> EnumerateCheckedExt<C, I> for I {
    fn enumerate_checked(self) -> EnumerateChecked<C, I> {
        EnumerateChecked { next: Some(<C as LowerBounded>::min_value()), iterator: self }
    }
}

impl<C, I> Iterator for EnumerateChecked<C, I>
where
    C: Clone + CheckedAdd + One,
    I: Iterator,
{
    type Item = (Option<C>, <I as Iterator>::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let count = self.next.clone();
        self.next = self.next.clone().and_then(|next| next.checked_add(&<C as One>::one()));
        self.iterator.next().map(|item| (count, item))
    }
}

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

impl GleiseDatenSerialisiert {
    fn v4(
        self,
        streckenabschnitt: &Option<streckenabschnitt::Name>,
    ) -> (v4::GleiseDatenSerialisiert, Vec<KeineIdVerfügbar>) {
        // TODO als argument (Sammel-Struktur, je Gleis-Art neue Map)?
        let mut definitionen: BTreeMap<id::Repräsentation, _> = BTreeMap::new();
        let mut definitionen_invertiert = AssocList::new();

        let GleiseDatenSerialisiert {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = self;

        let mut fehler = Vec::new();

        let mut v4_geraden = HashMap::new();
        for (gleis_id, gleis) in geraden.into_iter().enumerate_checked() {
            let Some(gleis_id) = gleis_id else {
                fehler.push(KeineIdVerfügbar);
                continue;
            };
            let Gleis { definition, position } = gleis;
            let steuerung = definition.kontakt.clone();
            let definition_id = match definitionen_invertiert.entry(definition.clone()) {
                Entry::Occupied(occupied) => *occupied.get(),
                Entry::Vacant(vacant) => {
                    let Some(id) =
                        definitionen.last_key_value().map_or(Some(0), |(k, _v)| k.checked_add(&1))
                    else {
                        fehler.push(KeineIdVerfügbar);
                        continue;
                    };
                    let _ = definitionen.insert(id, definition);
                    let _ = vacant.insert(id);
                    id
                },
            };
            let v4_gleis: v4::GleisSerialisiert<Gerade> = v4::GleisSerialisiert {
                definition: definition_id,
                steuerung,
                position,
                streckenabschnitt: streckenabschnitt.clone(),
            };
            // gleis_id ist eindeutig, da es durch enumerate erzeugt wurde
            let _ = v4_geraden.insert(gleis_id, v4_gleis);
        }

        let v4 = v4::GleiseDatenSerialisiert {
            geraden: v4_geraden,
            kurven: todo!(),
            weichen: todo!(),
            dreiwege_weichen: todo!(),
            kurven_weichen: todo!(),
            s_kurven_weichen: todo!(),
            kreuzungen: todo!(),
        };
        (v4, fehler)
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
