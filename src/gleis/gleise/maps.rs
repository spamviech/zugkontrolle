//! Struktur zum Speichern aller Gleise

use std::collections::HashMap;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use super::id::GleisId;
use crate::gleis::typen::*;
use crate::gleis::{
    gerade::Gerade,
    kreuzung::Kreuzung,
    kurve::Kurve,
    weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
}

// TODO Konvertierungsfunktion von/zu Gleise<Z>
#[derive(zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct GleiseVecs<Z> {
    geraden: Vec<Gleis<Gerade<Z>>>,
    kurven: Vec<Gleis<Kurve<Z>>>,
    kreuzungen: Vec<Gleis<Kreuzung<Z>>>,
    weichen: Vec<Gleis<Weiche<Z>>>,
    dreiwege_weichen: Vec<Gleis<DreiwegeWeiche<Z>>>,
    kurven_weichen: Vec<Gleis<KurvenWeiche<Z>>>,
    s_kurven_weichen: Vec<Gleis<SKurvenWeiche<Z>>>,
}

#[derive(zugkontrolle_derive::Debug)]
pub struct GleiseMaps<Z> {
    pub(crate) geraden: HashMap<GleisId<Gerade<Z>>, Gleis<Gerade<Z>>>,
    pub(crate) kurven: HashMap<GleisId<Kurve<Z>>, Gleis<Kurve<Z>>>,
    pub(crate) kreuzungen: HashMap<GleisId<Kreuzung<Z>>, Gleis<Kreuzung<Z>>>,
    pub(crate) weichen: HashMap<GleisId<Weiche<Z>>, Gleis<Weiche<Z>>>,
    pub(crate) dreiwege_weichen: HashMap<GleisId<DreiwegeWeiche<Z>>, Gleis<DreiwegeWeiche<Z>>>,
    pub(crate) kurven_weichen: HashMap<GleisId<KurvenWeiche<Z>>, Gleis<KurvenWeiche<Z>>>,
    pub(crate) s_kurven_weichen: HashMap<GleisId<SKurvenWeiche<Z>>, Gleis<SKurvenWeiche<Z>>>,
}

pub trait GleiseMap<Z>: Sized {
    fn get_map_mut(gleise: &mut GleiseMaps<Z>) -> &mut HashMap<GleisId<Self>, Gleis<Self>>;
}
impl<Z> GleiseMap<Z> for Gerade<Z> {
    fn get_map_mut(
        GleiseMaps { geraden, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        geraden
    }
}
impl<Z> GleiseMap<Z> for Kurve<Z> {
    fn get_map_mut(
        GleiseMaps { kurven, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven
    }
}
impl<Z> GleiseMap<Z> for Weiche<Z> {
    fn get_map_mut(
        GleiseMaps { weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        weichen
    }
}
impl<Z> GleiseMap<Z> for KurvenWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { kurven_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for DreiwegeWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { dreiwege_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        dreiwege_weichen
    }
}
impl<Z> GleiseMap<Z> for SKurvenWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { s_kurven_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        s_kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for Kreuzung<Z> {
    fn get_map_mut(
        GleiseMaps { kreuzungen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kreuzungen
    }
}
