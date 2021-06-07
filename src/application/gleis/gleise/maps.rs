//! Struktur zum Speichern aller Gleise

use std::collections::HashMap;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use super::id::{GleisId, GleisIdLock};
use crate::{
    anschluss::{OutputSave, ToSave},
    application::{gleis::*, typen::*},
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit},
        streckenabschnitt::{self, Streckenabschnitt},
    },
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

pub type Map<T> = HashMap<GleisId<T>, (Gleis<T>, GleisIdLock<T>)>;
#[derive(zugkontrolle_derive::Debug)]
pub struct GleiseMaps<Z> {
    pub(crate) geraden: Map<Gerade<Z>>,
    pub(crate) kurven: Map<Kurve<Z>>,
    pub(crate) weichen: Map<Weiche<Z>>,
    pub(crate) dreiwege_weichen: Map<DreiwegeWeiche<Z>>,
    pub(crate) kurven_weichen: Map<KurvenWeiche<Z>>,
    pub(crate) s_kurven_weichen: Map<SKurvenWeiche<Z>>,
    pub(crate) kreuzungen: Map<Kreuzung<Z>>,
    pub(crate) streckenabschnitte: streckenabschnitt::Map,
}
impl<Z> GleiseMaps<Z> {
    pub(crate) fn neu() -> Self {
        GleiseMaps {
            geraden: HashMap::new(),
            kurven: HashMap::new(),
            weichen: HashMap::new(),
            dreiwege_weichen: HashMap::new(),
            kurven_weichen: HashMap::new(),
            s_kurven_weichen: HashMap::new(),
            kreuzungen: HashMap::new(),
            streckenabschnitte: HashMap::new(),
        }
    }
}

pub trait GleiseMap<Z>: Sized {
    fn get_map_mut(gleise: &mut GleiseMaps<Z>) -> &mut Map<Self>;
}
impl<Z> GleiseMap<Z> for Gerade<Z> {
    fn get_map_mut(GleiseMaps { geraden, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        geraden
    }
}
impl<Z> GleiseMap<Z> for Kurve<Z> {
    fn get_map_mut(GleiseMaps { kurven, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kurven
    }
}
impl<Z> GleiseMap<Z> for Weiche<Z> {
    fn get_map_mut(GleiseMaps { weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        weichen
    }
}
impl<Z> GleiseMap<Z> for DreiwegeWeiche<Z> {
    fn get_map_mut(GleiseMaps { dreiwege_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        dreiwege_weichen
    }
}
impl<Z> GleiseMap<Z> for KurvenWeiche<Z> {
    fn get_map_mut(GleiseMaps { kurven_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for SKurvenWeiche<Z> {
    fn get_map_mut(GleiseMaps { s_kurven_weichen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        s_kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for Kreuzung<Z> {
    fn get_map_mut(GleiseMaps { kreuzungen, .. }: &mut GleiseMaps<Z>) -> &mut Map<Self> {
        kreuzungen
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct GleiseVecs<Z: Zugtyp> {
    pub(crate) name: String,
    pub(crate) geraden: Vec<Gleis<GeradeSave<Z>>>,
    pub(crate) kurven: Vec<Gleis<KurveSave<Z>>>,
    pub(crate) weichen: Vec<Gleis<WeicheSave<Z>>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSave<Z>>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSave<Z>>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSave<Z>>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSave<Z>>>,
    pub(crate) streckenabschnitte: streckenabschnitt::Map<OutputSave>,
    pub(crate) geschwindigkeiten: geschwindigkeit::Map<Z::LeiterSave>,
    /* TODO
     * steuerung-Typen bei Gleisen (kontakt, kupplung, weiche)
     * pläne, wegstrecken
     */
}

impl<Z: Zugtyp> From<(&GleiseMaps<Z>, &geschwindigkeit::Map<Z::Leiter>)> for GleiseVecs<Z> {
    fn from((maps, geschwindigkeiten): (&GleiseMaps<Z>, &geschwindigkeit::Map<Z::Leiter>)) -> Self {
        macro_rules! hashmaps_to_vecs {
            ($($map:ident),* $(,)?) => {
                GleiseVecs {
                    name: Z::NAME.to_string(),
                    // TODO
                    streckenabschnitte: maps.streckenabschnitte.iter().map(
                        |(name, Streckenabschnitt {farbe, anschluss})|
                            (name.clone(), Streckenabschnitt {farbe: *farbe, anschluss: anschluss.to_save()})
                        ).collect(),
                    geschwindigkeiten:geschwindigkeiten.iter().map(
                        |(name, Geschwindigkeit { leiter })| {
                            (name.clone(), Geschwindigkeit { leiter: leiter.to_save() })
                        },
                    ).collect(),
                    $($map: maps.$map.values().map(
                        |(Gleis {position, definition, streckenabschnitt}, _id_lock)|
                        Gleis {
                            position: position.clone(),
                            definition: definition.to_save(),
                            streckenabschnitt: streckenabschnitt.clone()
                        })
                        .collect()
                    ),*
                }
            };
        }
        hashmaps_to_vecs!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        )
    }
}
