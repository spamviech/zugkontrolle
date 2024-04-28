//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden.

use std::collections::HashMap;

use serde::Deserialize;

use zugkontrolle_gleis::steuerung::plan;
use zugkontrolle_typen::canvas::Position;
use zugkontrolle_util::void::Void;

use crate::daten::{
    de_serialisieren::LadenFehler,
    v2::{
        gerade::GeradeSerialisiert,
        geschwindigkeit::{BekannterZugtyp, GeschwindigkeitMapSerialisiert},
        kreuzung::KreuzungSerialisiert,
        kurve::KurveSerialisiert,
        streckenabschnitt::StreckenabschnittMapSerialisiert,
        weiche::{
            dreiwege::DreiwegeWeicheSerialisiert, gerade::WeicheSerialisiert,
            kurve::KurvenWeicheSerialisiert, s_kurve::SKurvenWeicheSerialisiert,
        },
    },
    v3,
};

pub mod anschluss;
pub mod gerade;
pub mod geschwindigkeit;
pub mod kreuzung;
pub mod kurve;
pub mod streckenabschnitt;
pub mod weiche;

/// Darstellung eines [`Gleises`](aktuell::Gleis) bei Version 2.
#[derive(Deserialize)]
pub(in crate::daten::v2) struct Gleis<T> {
    /// Die Definition des Gleises.
    definition: T,
    /// Die Position des Gleises.
    position: Position,
    /// Der Name des assoziierten Streckenabschnittes.
    streckenabschnitt: Option<streckenabschnitt::Name>,
}

/// Der serialisierbare Zustand, wie er in Version 2 verwendet wurde.
#[derive(Deserialize)]
pub(crate) struct GleiseVecs<LeiterV2> {
    /// Der Name des gespeicherten Zugtyps.
    name: String,
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: Vec<Gleis<GeradeSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: Vec<Gleis<KurveSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: Vec<Gleis<WeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    streckenabschnitte: StreckenabschnittMapSerialisiert,
    #[allow(clippy::missing_docs_in_private_items)]
    geschwindigkeiten: GeschwindigkeitMapSerialisiert<LeiterV2>,
    /// Die Pläne. In Version 2 wurden keine Pläne unterstützt.
    #[allow(clippy::zero_sized_map_values)]
    pläne: HashMap<plan::Name, Void>,
}

impl<L: 'static + BekannterZugtyp, S: From<<L as BekannterZugtyp>::V2>>
    TryFrom<GleiseVecs<<L as BekannterZugtyp>::V2>> for v3::ZustandSerialisiert<L, S>
{
    type Error = LadenFehler<S>;

    fn try_from(v2: GleiseVecs<<L as BekannterZugtyp>::V2>) -> Result<Self, Self::Error> {
        let Some(zugtyp) = L::bekannter_zugtyp(&v2.name) else {
            return Err(LadenFehler::UnbekannterZugtyp { zugtyp: v2.name, leiter: L::NAME });
        };

        let mut ohne_streckenabschnitt = v3::GleiseDatenSerialisiert::neu();
        let mut streckenabschnitte: HashMap<_, _> = v2
            .streckenabschnitte
            .into_iter()
            .map(|(name, streckenabschnitt)| {
                (name.into(), (streckenabschnitt.into(), v3::GleiseDatenSerialisiert::neu()))
            })
            .collect();
        /// Verteile die Gleise in den `GleiseDaten` passend zum assoziiertem Streckenabschnitt.
        macro_rules! verteile_gleise {
            ($($gleis: ident),*) => {
                $(for Gleis { definition, position, streckenabschnitt } in v2.$gleis.into_iter() {
                    let daten = if let Some((_streckenabschnitt, daten)) =
                        streckenabschnitt.and_then(|name| streckenabschnitte.get_mut(&name.into()))
                    {
                        daten
                    } else {
                        &mut ohne_streckenabschnitt
                    };
                    daten.$gleis.push(v3::Gleis {definition: definition.into(), position})
                })*
            };
        }
        verteile_gleise! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
        let geschwindigkeiten = v2
            .geschwindigkeiten
            .into_iter()
            .map(|(name, geschwindigkeit)| {
                (name.into(), (geschwindigkeit.into(), v3::StreckenabschnittMapSerialisiert::new()))
            })
            .collect();
        Ok(v3::ZustandSerialisiert {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne.into_values().map(|void| void.unreachable()).collect(),
        })
    }
}
