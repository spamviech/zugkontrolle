//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use std::{collections::HashMap, marker::PhantomData};

use serde::{
    de::{self, MapAccess, SeqAccess, Visitor},
    Deserialize,
};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::{
        gleis::{
            gerade::GeradeSerialisiert,
            gleise::{
                self,
                daten::{self as aktuell, de_serialisieren::BekannterLeiter},
            },
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
    pub(crate) pläne: Vec<Plan>,
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
        let geraden = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(1, &self))?;
        let kurven = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(2, &self))?;
        let weichen = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(3, &self))?;
        let dreiwege_weichen =
            seq.next_element()?.ok_or_else(|| de::Error::invalid_length(4, &self))?;
        let kurven_weichen =
            seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
        let s_kurven_weichen =
            seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
        let kreuzungen = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
        let streckenabschnitte =
            seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
        let geschwindigkeiten =
            seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
        let pläne = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(5, &self))?;
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
                }
                GleiseVecsField::Geraden => {
                    if geraden.is_some() {
                        return Err(de::Error::duplicate_field("geraden"));
                    }
                    geraden = Some(map.next_value()?)
                }
                GleiseVecsField::Kurven => {
                    if kurven.is_some() {
                        return Err(de::Error::duplicate_field("kurven"));
                    }
                    kurven = Some(map.next_value()?)
                }
                GleiseVecsField::Weichen => {
                    if weichen.is_some() {
                        return Err(de::Error::duplicate_field("weichen"));
                    }
                    weichen = Some(map.next_value()?)
                }
                GleiseVecsField::Dreiwege_Weichen => {
                    if dreiwege_weichen.is_some() {
                        return Err(de::Error::duplicate_field("dreiwege_weichen"));
                    }
                    dreiwege_weichen = Some(map.next_value()?)
                }
                GleiseVecsField::Kurven_Weichen => {
                    if kurven_weichen.is_some() {
                        return Err(de::Error::duplicate_field("kurven_weichen"));
                    }
                    kurven_weichen = Some(map.next_value()?)
                }
                GleiseVecsField::S_Kurven_Weichen => {
                    if s_kurven_weichen.is_some() {
                        return Err(de::Error::duplicate_field("s_kurven_weichen"));
                    }
                    s_kurven_weichen = Some(map.next_value()?)
                }
                GleiseVecsField::Kreuzungen => {
                    if kreuzungen.is_some() {
                        return Err(de::Error::duplicate_field("kreuzungen"));
                    }
                    kreuzungen = Some(map.next_value()?)
                }
                GleiseVecsField::Streckenabschnitte => {
                    if streckenabschnitte.is_some() {
                        return Err(de::Error::duplicate_field("streckenabschnitte"));
                    }
                    streckenabschnitte = Some(map.next_value()?)
                }
                GleiseVecsField::Geschwindigkeiten => {
                    if geschwindigkeiten.is_some() {
                        return Err(de::Error::duplicate_field("geschwindigkeiten"));
                    }
                    geschwindigkeiten = Some(map.next_value()?)
                }
                GleiseVecsField::Pläne => {
                    if pläne.is_some() {
                        return Err(de::Error::duplicate_field("pläne"));
                    }
                    pläne = Some(map.next_value()?)
                }
            }
        }
        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
        let geraden = geraden.ok_or_else(|| de::Error::missing_field("geraden"))?;
        let kurven = kurven.ok_or_else(|| de::Error::missing_field("kurven"))?;
        let weichen = weichen.ok_or_else(|| de::Error::missing_field("weichen"))?;
        let dreiwege_weichen =
            dreiwege_weichen.ok_or_else(|| de::Error::missing_field("dreiwege_weichen"))?;
        let kurven_weichen =
            kurven_weichen.ok_or_else(|| de::Error::missing_field("kurven_weichen"))?;
        let s_kurven_weichen =
            s_kurven_weichen.ok_or_else(|| de::Error::missing_field("s_kurven_weichen"))?;
        let kreuzungen = kreuzungen.ok_or_else(|| de::Error::missing_field("kreuzungen"))?;
        let streckenabschnitte =
            streckenabschnitte.ok_or_else(|| de::Error::missing_field("streckenabschnitte"))?;
        let geschwindigkeiten =
            geschwindigkeiten.ok_or_else(|| de::Error::missing_field("geschwindigkeiten"))?;
        let pläne = pläne.ok_or_else(|| de::Error::missing_field("pläne"))?;
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
    type Error = gleise::Fehler;

    fn try_from(v2: GleiseVecs<Leiter>) -> Result<Self, gleise::Fehler> {
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
        let leiter = Leiter::NAME;
        let zugtyp = Leiter::bekannter_zugtyp(leiter)
            .ok_or_else(|| gleise::Fehler::FalscherLeiter(leiter.to_string()))?;
        Ok(aktuell::de_serialisieren::ZustandSerialisiert {
            zugtyp,
            leiter: leiter.to_string(),
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne,
        })
    }
}
