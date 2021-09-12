//! Struktur zum Speichern aller Gleise

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    iter,
};

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        polarität::Fließend,
    },
    application::{
        gleis::{gleise::id::GleisId, *},
        typen::*,
    },
    steuerung::{
        geschwindigkeit,
        plan::Plan,
        streckenabschnitt::{self, Streckenabschnitt},
    },
};

pub mod v2;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

impl<T: Serialisiere> Serialisiere for Gleis<T> {
    type Serialisiert = Gleis<T::Serialisiert>;

    fn serialisiere(&self) -> Self::Serialisiert {
        Gleis {
            definition: self.definition.serialisiere(),
            position: self.position.clone(),
            streckenabschnitt: self.streckenabschnitt.clone(),
        }
    }

    fn anschlüsse(
        self,
    ) -> (
        Vec<crate::anschluss::pwm::Pin>,
        Vec<crate::anschluss::OutputAnschluss>,
        Vec<crate::anschluss::InputAnschluss>,
    ) {
        self.definition.anschlüsse()
    }
}

impl<R, T: Reserviere<R>> Reserviere<Gleis<R>> for Gleis<T> {
    fn reserviere(
        self,
        anschlüsse: &mut crate::anschluss::Anschlüsse,
        pwm_pins: Vec<crate::anschluss::pwm::Pin>,
        output_anschlüsse: Vec<crate::anschluss::OutputAnschluss>,
        input_anschlüsse: Vec<crate::anschluss::InputAnschluss>,
    ) -> de_serialisieren::Result<Gleis<R>> {
        let Reserviert {
            anschluss: definition_reserviert,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.definition.reserviere(
            anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(Reserviert {
            anschluss: Gleis {
                definition: definition_reserviert,
                position: self.position,
                streckenabschnitt: self.streckenabschnitt,
            },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

pub struct Zustand<Z: Zugtyp> {
    pub(crate) ohne_streckenabschnitt: GleiseMaps<Z>,
    pub(crate) streckenabschnitte:
        HashMap<streckenabschnitt::Name, (Streckenabschnitt, Fließend, GleiseMaps<Z>)>,
    pub(crate) geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
}
impl<Z: Zugtyp> Zustand<Z> {
    pub fn neu() -> Self {
        Zustand {
            ohne_streckenabschnitt: GleiseMaps::neu(),
            streckenabschnitte: HashMap::new(),
            geschwindigkeiten: geschwindigkeit::Map::new(),
        }
    }

    pub(crate) fn alle_gleise_maps(&self) -> impl Iterator<Item = &GleiseMaps<Z>> {
        iter::once(&self.ohne_streckenabschnitt).chain(
            self.streckenabschnitte.values().map(|(_streckenabschnitt, _fließend, maps)| maps),
        )
    }
}

impl<Z> Debug for Zustand<Z>
where
    Z: Zugtyp,
    <Z as Zugtyp>::Leiter: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Zustand")
            .field("ohne_streckenabschnitt", &self.ohne_streckenabschnitt)
            .field("streckenabschnitte", &self.streckenabschnitte)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .finish()
    }
}

// BTreeMap could use `split_off(GleisId::initial())` instead of `drain()`
pub(crate) type Map<T> = BTreeMap<GleisId<T>, Gleis<T>>;
#[derive(zugkontrolle_derive::Debug)]
pub(crate) struct GleiseMaps<Z> {
    pub(crate) geraden: Map<Gerade<Z>>,
    pub(crate) kurven: Map<Kurve<Z>>,
    pub(crate) weichen: Map<Weiche<Z>>,
    pub(crate) dreiwege_weichen: Map<DreiwegeWeiche<Z>>,
    pub(crate) kurven_weichen: Map<KurvenWeiche<Z>>,
    pub(crate) s_kurven_weichen: Map<SKurvenWeiche<Z>>,
    pub(crate) kreuzungen: Map<Kreuzung<Z>>,
}
impl<Z> GleiseMaps<Z> {
    pub(crate) fn neu() -> Self {
        GleiseMaps {
            geraden: Map::new(),
            kurven: Map::new(),
            weichen: Map::new(),
            dreiwege_weichen: Map::new(),
            kurven_weichen: Map::new(),
            s_kurven_weichen: Map::new(),
            kreuzungen: Map::new(),
        }
    }
}

/// Trait um eine Referenz auf die Map für den jeweiligen Typ zu bekommen.
/// Kein schönes API, daher nur crate-public.
pub(crate) trait MapSelector<Z>: Sized {
    fn get_map(gleise: &GleiseMaps<Z>) -> &Map<Self>;
    fn get_map_mut(gleise: &mut GleiseMaps<Z>) -> &mut Map<Self>;
}
impl<Z> GleiseMaps<Z> {
    #[inline(always)]
    pub(crate) fn get_map<T: MapSelector<Z>>(&self) -> &Map<T> {
        T::get_map(self)
    }
    #[inline(always)]
    pub(crate) fn get_map_mut<T: MapSelector<Z>>(&mut self) -> &mut Map<T> {
        T::get_map_mut(self)
    }
}
impl<Z> MapSelector<Z> for Gerade<Z> {
    fn get_map(GleiseMaps { geraden, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        geraden
    }
    fn get_map_mut(GleiseMaps { geraden, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        geraden
    }
}
impl<Z> MapSelector<Z> for Kurve<Z> {
    fn get_map(GleiseMaps { kurven, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        kurven
    }
    fn get_map_mut(GleiseMaps { kurven, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kurven
    }
}
impl<Z> MapSelector<Z> for Weiche<Z> {
    fn get_map(GleiseMaps { weichen, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        weichen
    }
    fn get_map_mut(GleiseMaps { weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        weichen
    }
}
impl<Z> MapSelector<Z> for DreiwegeWeiche<Z> {
    fn get_map(GleiseMaps { dreiwege_weichen, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        dreiwege_weichen
    }
    fn get_map_mut(GleiseMaps { dreiwege_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        dreiwege_weichen
    }
}
impl<Z> MapSelector<Z> for KurvenWeiche<Z> {
    fn get_map(GleiseMaps { kurven_weichen, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        kurven_weichen
    }
    fn get_map_mut(GleiseMaps { kurven_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kurven_weichen
    }
}
impl<Z> MapSelector<Z> for SKurvenWeiche<Z> {
    fn get_map(GleiseMaps { s_kurven_weichen, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        s_kurven_weichen
    }
    fn get_map_mut(GleiseMaps { s_kurven_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        s_kurven_weichen
    }
}
impl<Z> MapSelector<Z> for Kreuzung<Z> {
    fn get_map(GleiseMaps { kreuzungen, .. }: &GleiseMaps<Z>) -> &Map<Self> {
        kreuzungen
    }
    fn get_map_mut(GleiseMaps { kreuzungen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kreuzungen
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Serialisiert<Z: Zugtyp> {
    pub(crate) zugtyp: String,
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

impl<Z> Debug for Serialisiert<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Serialisiert")
            .field("zugtyp", &self.zugtyp)
            .field("geraden", &self.geraden)
            .field("kurven", &self.kurven)
            .field("weichen", &self.weichen)
            .field("dreiwege_weichen", &self.dreiwege_weichen)
            .field("kurven_weichen", &self.kurven_weichen)
            .field("s_kurven_weichen", &self.s_kurven_weichen)
            .field("kreuzungen", &self.kreuzungen)
            .field("streckenabschnitte", &self.streckenabschnitte)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .field("pläne", &self.pläne)
            .finish()
    }
}

impl<Z: Zugtyp> From<&Zustand<Z>> for Serialisiert<Z> {
    fn from(
        Zustand { ohne_streckenabschnitt, streckenabschnitte, geschwindigkeiten }: &Zustand<Z>,
    ) -> Self {
        // macro_rules! hashmaps_to_vecs {
        //     ($($map:ident),* $(,)?) => {
        //         Serialisiert {
        //             zugtyp: Z::NAME.to_string(),
        //             streckenabschnitte: maps.streckenabschnitte.iter().map(
        //                 |(name, (streckenabschnitt, _fließend))|
        //                     (name.clone(), streckenabschnitt.serialisiere())
        //                 ).collect(),
        //             geschwindigkeiten,
        //             // TODO wirkliche Konvertierung, sobald Plan implementiert ist
        //             pläne: Vec::new(),
        //             $($map: maps.$map.values().map(
        //                 |Gleis {position, definition, streckenabschnitt}|
        //                 Gleis {
        //                     position: position.clone(),
        //                     definition: definition.serialisiere(),
        //                     streckenabschnitt: streckenabschnitt.clone()
        //                 })
        //                 .collect()
        //             ),*
        //         }
        //     };
        // }
        // hashmaps_to_vecs!(
        //     geraden,
        //     kurven,
        //     weichen,
        //     dreiwege_weichen,
        //     kurven_weichen,
        //     s_kurven_weichen,
        //     kreuzungen,
        // )
        todo!()
    }
}
