//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::gleis::{
        gerade::GeradeSerialisiert,
        gleise::maps::{self as aktuell, Gleis},
        kreuzung::KreuzungSerialisiert,
        kurve::KurveSerialisiert,
        weiche::{
            dreiwege::DreiwegeWeicheSerialisiert, gerade::WeicheSerialisiert,
            kurve::KurvenWeicheSerialisiert, s_kurve::SKurvenWeicheSerialisiert,
        },
    },
    steuerung::{geschwindigkeit, plan::Plan, streckenabschnitt},
    zugtyp::Zugtyp,
};

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
    pub(crate) streckenabschnitte: streckenabschnitt::MapSerialisiert,
    pub(crate) geschwindigkeiten:
        geschwindigkeit::MapSerialisiert<<Z::Leiter as Serialisiere>::Serialisiert>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Z: Zugtyp> From<GleiseVecs<Z>> for aktuell::GleiseVecs<Z> {
    fn from(v2: GleiseVecs<Z>) -> Self {
        aktuell::GleiseVecs {
            name: v2.name,
            geraden: v2.geraden,
            kurven: v2.kurven,
            weichen: v2.weichen,
            dreiwege_weichen: v2.dreiwege_weichen,
            kurven_weichen: v2.kurven_weichen,
            s_kurven_weichen: v2.s_kurven_weichen,
            kreuzungen: v2.kreuzungen,
            streckenabschnitte: v2.streckenabschnitte,
            geschwindigkeiten: v2.geschwindigkeiten,
            pläne: v2.pläne,
        }
    }
}
