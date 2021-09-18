//! Struktur zum Speichern aller Gleise

use std::{collections::HashMap, fmt::Debug, iter};

use rstar::{primitives::GeomWithData, RTree, RTreeObject, SelectionFunction};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        polarität::Fließend,
    },
    application::{gleis::*, typen::*},
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
    pub(crate) ohne_streckenabschnitt: GleiseDaten<Z>,
    pub(crate) streckenabschnitte:
        HashMap<streckenabschnitt::Name, (Streckenabschnitt, Fließend, GleiseDaten<Z>)>,
    pub(crate) geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
}
impl<Z: Zugtyp> Zustand<Z> {
    pub fn neu() -> Self {
        Zustand {
            ohne_streckenabschnitt: GleiseDaten::neu(),
            streckenabschnitte: HashMap::new(),
            geschwindigkeiten: geschwindigkeit::Map::new(),
        }
    }

    pub(crate) fn alle_gleise_maps(
        &self,
    ) -> impl Iterator<Item = (Option<&streckenabschnitt::Name>, &GleiseDaten<Z>)> {
        iter::once((None, &self.ohne_streckenabschnitt)).chain(
            self.streckenabschnitte
                .iter()
                .map(|(name, (_streckenabschnitt, _fließend, maps))| (Some(name), maps)),
        )
    }

    pub(crate) fn alle_gleise_maps_mut(
        &mut self,
    ) -> impl Iterator<Item = (Option<&streckenabschnitt::Name>, &mut GleiseDaten<Z>)> {
        let Zustand { ohne_streckenabschnitt, streckenabschnitte, .. } = self;
        iter::once((None, ohne_streckenabschnitt)).chain(
            streckenabschnitte
                .iter_mut()
                .map(|(name, (_streckenabschnitt, _fließend, maps))| (Some(name), maps)),
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

// FIXME Rechteck verwenden
pub(crate) type RStern<T> = RTree<GeomWithData<Vektor, (T, Winkel)>>;
#[derive(zugkontrolle_derive::Debug)]
pub(crate) struct GleiseDaten<Z> {
    pub(crate) geraden: RStern<Gerade<Z>>,
    pub(crate) kurven: RStern<Kurve<Z>>,
    pub(crate) weichen: RStern<Weiche<Z>>,
    pub(crate) dreiwege_weichen: RStern<DreiwegeWeiche<Z>>,
    pub(crate) kurven_weichen: RStern<KurvenWeiche<Z>>,
    pub(crate) s_kurven_weichen: RStern<SKurvenWeiche<Z>>,
    pub(crate) kreuzungen: RStern<Kreuzung<Z>>,
}
impl<Z> GleiseDaten<Z> {
    pub fn neu() -> Self {
        GleiseDaten {
            geraden: RStern::new(),
            kurven: RStern::new(),
            weichen: RStern::new(),
            dreiwege_weichen: RStern::new(),
            kurven_weichen: RStern::new(),
            s_kurven_weichen: RStern::new(),
            kreuzungen: RStern::new(),
        }
    }

    pub fn merge(&mut self, other: GleiseDaten<Z>) {
        macro_rules! extend {
            ($($rstern: ident),*) => {
                $(while let Some(gleis) = other.$rstern.remove_with_selection_function(SelectAll) {
                    self.$rstern.insert(gleis)
                })*
            };
        }
        extend! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
    }
}
/// SelectionFunction, die jedes Element akzeptiert.
/// Haupt-Nutzen ist das vollständiges Leeren eines RTree (siehe `GleiseDaten::merge`).
struct SelectAll;
impl<T: RTreeObject> SelectionFunction<T> for SelectAll {
    fn should_unpack_parent(&self, envelope: &T::Envelope) -> bool {
        true
    }
}

/// Trait um eine Referenz auf die Map für den jeweiligen Typ zu bekommen.
/// Kein schönes API, daher nur crate-public.
pub(crate) trait DatenAuswahl<Z>: Sized {
    fn rstern(gleise: &GleiseDaten<Z>) -> &RStern<Self>;
    fn rstern_mut(gleise: &mut GleiseDaten<Z>) -> &mut RStern<Self>;
}
impl<Z> GleiseDaten<Z> {
    #[inline(always)]
    pub(crate) fn rstern<T: DatenAuswahl<Z>>(&self) -> &RStern<T> {
        T::rstern(self)
    }
    #[inline(always)]
    pub(crate) fn rstern_mut<T: DatenAuswahl<Z>>(&mut self) -> &mut RStern<T> {
        T::rstern_mut(self)
    }
}
impl<Z> DatenAuswahl<Z> for Gerade<Z> {
    fn rstern(GleiseDaten { geraden, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        geraden
    }
    fn rstern_mut(GleiseDaten { geraden, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        geraden
    }
}
impl<Z> DatenAuswahl<Z> for Kurve<Z> {
    fn rstern(GleiseDaten { kurven, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kurven
    }
    fn rstern_mut(GleiseDaten { kurven, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        kurven
    }
}
impl<Z> DatenAuswahl<Z> for Weiche<Z> {
    fn rstern(GleiseDaten { weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        weichen
    }
    fn rstern_mut(GleiseDaten { weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        weichen
    }
}
impl<Z> DatenAuswahl<Z> for DreiwegeWeiche<Z> {
    fn rstern(GleiseDaten { dreiwege_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        dreiwege_weichen
    }
    fn rstern_mut(GleiseDaten { dreiwege_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        dreiwege_weichen
    }
}
impl<Z> DatenAuswahl<Z> for KurvenWeiche<Z> {
    fn rstern(GleiseDaten { kurven_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kurven_weichen
    }
    fn rstern_mut(GleiseDaten { kurven_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        kurven_weichen
    }
}
impl<Z> DatenAuswahl<Z> for SKurvenWeiche<Z> {
    fn rstern(GleiseDaten { s_kurven_weichen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        s_kurven_weichen
    }
    fn rstern_mut(GleiseDaten { s_kurven_weichen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
        s_kurven_weichen
    }
}
impl<Z> DatenAuswahl<Z> for Kreuzung<Z> {
    fn rstern(GleiseDaten { kreuzungen, .. }: &GleiseDaten<Z>) -> &RStern<Self> {
        kreuzungen
    }
    fn rstern_mut(GleiseDaten { kreuzungen, .. }: &mut GleiseDaten<Z>) -> &mut RStern<Self> {
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
    pub(crate) geschwindigkeiten: geschwindigkeit::MapSerialisiert<Z::Leiter>,
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
