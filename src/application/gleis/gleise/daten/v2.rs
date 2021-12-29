//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
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

pub(crate) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

#[derive(Serialize, Deserialize)]
pub(crate) struct GleiseVecs<Leiter: Serialisiere> {
    pub(crate) name: String,
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
    pub(crate) streckenabschnitte: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: geschwindigkeit::MapSerialisiert<Leiter>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Leiter: Serialisiere> From<GleiseVecs<Leiter>>
    for aktuell::de_serialisieren::ZustandSerialisiert<Leiter>
{
    fn from(v2: GleiseVecs<Leiter>) -> Self {
        let mut ohne_streckenabschnitt = aktuell::de_serialisieren::GleiseDatenSerialisiert::neu();
        let mut streckenabschnitte: HashMap<_, _> = v2
            .streckenabschnitte
            .into_iter()
            .map(|(name, streckenabschnitt)| {
                (
                    name,
                    (streckenabschnitt, aktuell::de_serialisieren::GleiseDatenSerialisiert::neu()),
                )
            })
            .collect();
        macro_rules! verteile_gleise {
            ($($gleis: ident),*) => {
                $(for Gleis { definition, position, streckenabschnitt } in v2.$gleis.into_iter() {
                    let daten = if let Some((_streckenabschnitt, daten)) =
                        streckenabschnitt.and_then(|name| streckenabschnitte.get_mut(&name))
                    {
                        daten
                    } else {
                        &mut ohne_streckenabschnitt
                    };
                    daten.$gleis.push(aktuell::Gleis {definition, position})
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
                (
                    name,
                    (
                        geschwindigkeit,
                        aktuell::de_serialisieren::StreckenabschnittMapSerialisiert::new(),
                    ),
                )
            })
            .collect();
        aktuell::de_serialisieren::ZustandSerialisiert {
            zugtyp: match &v2.name {
                "Märklin" => Zugtyp::märklin(),
                "Lego" => Zugtyp::lego(),
                _ => todo!("Fehler!"),
            },
            leiter: todo!("leiter"),
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne,
        }
    }
}
