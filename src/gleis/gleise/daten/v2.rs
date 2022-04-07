//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::{collections::HashMap, marker::PhantomData};

use serde::{
    de::{self, Deserializer, SeqAccess, Visitor},
    Deserialize,
};

use crate::{
    anschluss::{self, de_serialisieren::Serialisiere, level::Level, pcf8574},
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
    steuerung::{
        geschwindigkeit::{self, BekannterLeiter, GeschwindigkeitSerialisiert},
        streckenabschnitt,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    typen::canvas::Position,
    void::Void,
    zugtyp::FalscherLeiter,
};

#[derive(Debug, Clone, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

pub(crate) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

type GeschwindigkeitMapSerialisiert<Leiter> =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert<Leiter>>;

/// Beschreibung eines [anschluss::pcf85747::Pcf8574].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
struct Beschreibung {
    /// Anliegendes [Level] an das `A0` Adress-Bit.
    a0: Level,
    /// Anliegendes [Level] an das `A1` Adress-Bit.
    a1: Level,
    /// Anliegendes [Level] an das `A2` Adress-Bit.
    a2: Level,
    /// Variante des [anschluss::pcf85747::Pcf8574], beeinflusst die I2C-Adresse.
    variante: pcf8574::Variante,
}

// #[derive(Deserialize)]
// #[serde(bound = "Leiter: Serialisiere")]
pub(crate) struct GleiseVecs<Leiter: Serialisiere> {
    name: String,
    geraden: Vec<Gleis<GeradeSerialisiert>>,
    kurven: Vec<Gleis<KurveSerialisiert>>,
    weichen: Vec<Gleis<WeicheSerialisiert>>,
    dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
    streckenabschnitte: StreckenabschnittMapSerialisiert,
    geschwindigkeiten: GeschwindigkeitMapSerialisiert<Leiter>,
    pläne: Vec<Void>,
}

// Explizite serde-Implementierung, damit Leiter kein automatisches Constraint bekommt
// https://serde.rs/deserialize-struct.html
#[derive(Deserialize)]
#[allow(non_camel_case_types)]
enum GleiseVecsField {
    name,
    geraden,
    kurven,
    weichen,
    dreiwege_weichen,
    kurven_weichen,
    s_kurven_weichen,
    kreuzungen,
    streckenabschnitte,
    geschwindigkeiten,
    pläne,
}

struct GleiseVecsVisitor<Leiter>(PhantomData<fn() -> Leiter>);

impl<'de, Leiter: Serialisiere> Visitor<'de> for GleiseVecsVisitor<Leiter> {
    type Value = GleiseVecs<Leiter>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("struct Zustand")
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<Self::Value, V::Error> {
        println!("name");
        let name = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(0, &self))?;
        println!("geraden");
        let geraden = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("kurven");
        let kurven = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("weichen");
        let weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("dreiwege");
        let dreiwege_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("kurven_weichen");
        let kurven_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("s_kurven");
        let s_kurven_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("kreuzung");
        let kreuzungen = seq.next_element()?.unwrap_or_else(Vec::new);
        println!("streckenabschnitte");
        let streckenabschnitte = seq.next_element()?.unwrap_or_else(HashMap::new);
        println!("geschwindigkeiten");
        let geschwindigkeiten = seq.next_element()?.unwrap_or_else(HashMap::new);
        println!("pläne");
        let pläne = seq.next_element()?.unwrap_or_else(Vec::new);
        Ok(GleiseVecs {
            name,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            streckenabschnitte,
            geschwindigkeiten,
            pläne,
        })
    }
}

impl<'de, Leiter: Serialisiere> Deserialize<'de> for GleiseVecs<Leiter> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_struct(
            "zustand",
            &[
                "name",
                "geraden",
                "kurven",
                "weichen",
                "dreiwege_weichen",
                "kurven_weichen",
                "s_kurven_weichen",
                "kreuzungen",
                "streckenabschnitte",
                "geschwindigkeiten",
                "pläne",
            ],
            GleiseVecsVisitor::<Leiter>(PhantomData),
        )
    }
}

impl<Leiter: Serialisiere + BekannterLeiter> TryFrom<GleiseVecs<Leiter>>
    for aktuell::de_serialisieren::ZustandSerialisiert<Leiter>
{
    type Error = anschluss::Fehler;

    fn try_from(v2: GleiseVecs<Leiter>) -> Result<Self, Self::Error> {
        let leiter = Leiter::NAME;
        let zugtyp = Leiter::bekannter_zugtyp(leiter)
            .ok_or_else(|| anschluss::Fehler::FalscherLeiter(FalscherLeiter(leiter.to_owned())))?;
        if zugtyp.name != v2.name {
            return Err(anschluss::Fehler::FalscherLeiter(FalscherLeiter(leiter.to_owned())));
        }

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
        Ok(aktuell::de_serialisieren::ZustandSerialisiert {
            zugtyp: zugtyp.into(),
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne.into_iter().map(|void| void.unreachable()).collect(),
        })
    }
}
