//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::{collections::HashMap, marker::PhantomData};

use serde::{
    de::{self, MapAccess, SeqAccess, Visitor},
    Deserialize,
};

use crate::{
    anschluss::{self, de_serialisieren::Serialisiere},
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
        geschwindigkeit, plan::PlanSerialisiert, streckenabschnitt,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    typen::canvas::Position,
    zugtyp::{BekannterLeiter, FalscherLeiter},
};

#[derive(Debug, Clone, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

pub(crate) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

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
    pub(crate) pläne: Vec<PlanSerialisiert>,
}

// Explizite serde-Implementierung, damit Leiter kein automatisches Constraint bekommt
// https://serde.rs/deserialize-struct.html
#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
#[allow(non_camel_case_types)]
enum GleiseVecsField {
    Name,
    Geraden,
    Kurven,
    Weichen,
    Dreiwege_Weichen,
    Kurven_Weichen,
    S_Kurven_Weichen,
    Kreuzungen,
    Streckenabschnitte,
    Geschwindigkeiten,
    Pläne,
}

struct GleiseVecsVisitor<Leiter>(PhantomData<fn() -> Leiter>);

impl<'de, Leiter: Serialisiere> Visitor<'de> for GleiseVecsVisitor<Leiter> {
    type Value = GleiseVecs<Leiter>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("struct Zustand")
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<GleiseVecs<Leiter>, V::Error> {
        let name = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let geraden = seq.next_element()?.unwrap_or_else(Vec::new);
        let kurven = seq.next_element()?.unwrap_or_else(Vec::new);
        let weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        let dreiwege_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        let kurven_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        let s_kurven_weichen = seq.next_element()?.unwrap_or_else(Vec::new);
        let kreuzungen = seq.next_element()?.unwrap_or_else(Vec::new);
        let streckenabschnitte = seq.next_element()?.unwrap_or_else(HashMap::new);
        let geschwindigkeiten = seq.next_element()?.unwrap_or_else(HashMap::new);
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

    fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<GleiseVecs<Leiter>, V::Error> {
        let mut name = None;
        let mut geraden = None;
        let mut kurven = None;
        let mut weichen = None;
        let mut dreiwege_weichen = None;
        let mut kurven_weichen = None;
        let mut s_kurven_weichen = None;
        let mut kreuzungen = None;
        let mut streckenabschnitte = None;
        let mut geschwindigkeiten = None;
        let mut pläne = None;
        while let Some(key) = map.next_key()? {
            match key {
                GleiseVecsField::Name => {
                    if name.is_some() {
                        return Err(de::Error::duplicate_field("name"));
                    }
                    name = Some(map.next_value()?)
                },
                GleiseVecsField::Geraden => {
                    if geraden.is_some() {
                        return Err(de::Error::duplicate_field("geraden"));
                    }
                    geraden = Some(map.next_value()?)
                },
                GleiseVecsField::Kurven => {
                    if kurven.is_some() {
                        return Err(de::Error::duplicate_field("kurven"));
                    }
                    kurven = Some(map.next_value()?)
                },
                GleiseVecsField::Weichen => {
                    if weichen.is_some() {
                        return Err(de::Error::duplicate_field("weichen"));
                    }
                    weichen = Some(map.next_value()?)
                },
                GleiseVecsField::Dreiwege_Weichen => {
                    if dreiwege_weichen.is_some() {
                        return Err(de::Error::duplicate_field("dreiwege_weichen"));
                    }
                    dreiwege_weichen = Some(map.next_value()?)
                },
                GleiseVecsField::Kurven_Weichen => {
                    if kurven_weichen.is_some() {
                        return Err(de::Error::duplicate_field("kurven_weichen"));
                    }
                    kurven_weichen = Some(map.next_value()?)
                },
                GleiseVecsField::S_Kurven_Weichen => {
                    if s_kurven_weichen.is_some() {
                        return Err(de::Error::duplicate_field("s_kurven_weichen"));
                    }
                    s_kurven_weichen = Some(map.next_value()?)
                },
                GleiseVecsField::Kreuzungen => {
                    if kreuzungen.is_some() {
                        return Err(de::Error::duplicate_field("kreuzungen"));
                    }
                    kreuzungen = Some(map.next_value()?)
                },
                GleiseVecsField::Streckenabschnitte => {
                    if streckenabschnitte.is_some() {
                        return Err(de::Error::duplicate_field("streckenabschnitte"));
                    }
                    streckenabschnitte = Some(map.next_value()?)
                },
                GleiseVecsField::Geschwindigkeiten => {
                    if geschwindigkeiten.is_some() {
                        return Err(de::Error::duplicate_field("geschwindigkeiten"));
                    }
                    geschwindigkeiten = Some(map.next_value()?)
                },
                GleiseVecsField::Pläne => {
                    if pläne.is_some() {
                        return Err(de::Error::duplicate_field("pläne"));
                    }
                    pläne = Some(map.next_value()?)
                },
            }
        }
        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
        let geraden = geraden.unwrap_or_else(Vec::new);
        let kurven = kurven.unwrap_or_else(Vec::new);
        let weichen = weichen.unwrap_or_else(Vec::new);
        let dreiwege_weichen = dreiwege_weichen.unwrap_or_else(Vec::new);
        let kurven_weichen = kurven_weichen.unwrap_or_else(Vec::new);
        let s_kurven_weichen = s_kurven_weichen.unwrap_or_else(Vec::new);
        let kreuzungen = kreuzungen.unwrap_or_else(Vec::new);
        let streckenabschnitte = streckenabschnitte.unwrap_or_else(HashMap::new);
        let geschwindigkeiten = geschwindigkeiten.unwrap_or_else(HashMap::new);
        let pläne = pläne.unwrap_or_else(Vec::new);
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
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
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
            pläne: v2.pläne,
        })
    }
}
