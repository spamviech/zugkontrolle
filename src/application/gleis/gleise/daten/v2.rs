//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    application::{
        gleis::{
            gerade::GeradeSerialisiert,
            gleise::daten as aktuell,
            kreuzung::KreuzungSerialisiert,
            kurve::KurveSerialisiert,
            weiche::{
                dreiwege::DreiwegeWeicheSerialisiert, gerade::WeicheSerialisiert,
                kurve::KurvenWeicheSerialisiert, s_kurve::SKurvenWeicheSerialisiert,
            },
        },
        typen::canvas::Position,
    },
    steuerung::{
        geschwindigkeit, plan::Plan, streckenabschnitt,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    zugtyp::Zugtyp,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}
impl<T> From<Gleis<T>> for aktuell::Gleis<T> {
    fn from(Gleis { definition, position, streckenabschnitt: _ }: Gleis<T>) -> Self {
        aktuell::Gleis { definition, position }
    }
}

pub(crate) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

#[derive(Serialize, Deserialize)]
pub(crate) struct GleiseVecs<Z: Zugtyp> {
    pub(crate) name: String,
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert<Z>>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert<Z>>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert<Z>>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert<Z>>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert<Z>>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert<Z>>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert<Z>>>,
    pub(crate) streckenabschnitte: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: geschwindigkeit::MapSerialisiert<Z::Leiter>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Z: Zugtyp> From<GleiseVecs<Z>> for aktuell::de_serialisieren::ZustandSerialisiert<Z> {
    fn from(v2: GleiseVecs<Z>) -> Self {
        // aktuell::de_serialisieren::ZustandSerialisiert {
        //     zugtyp: v2.name,
        //     geraden: v2.geraden.into_iter().map(aktuell::Gleis::from).collect(),
        //     kurven: v2.kurven.into_iter().map(aktuell::Gleis::from).collect(),
        //     weichen: v2.weichen.into_iter().map(aktuell::Gleis::from).collect(),
        //     dreiwege_weichen: v2.dreiwege_weichen.into_iter().map(aktuell::Gleis::from).collect(),
        //     kurven_weichen: v2.kurven_weichen.into_iter().map(aktuell::Gleis::from).collect(),
        //     s_kurven_weichen: v2.s_kurven_weichen.into_iter().map(aktuell::Gleis::from).collect(),
        //     kreuzungen: v2.kreuzungen.into_iter().map(aktuell::Gleis::from).collect(),
        //     streckenabschnitte: v2.streckenabschnitte,
        //     geschwindigkeiten: v2.geschwindigkeiten,
        //     pläne: v2.pläne,
        // }
        todo!()
    }
}
