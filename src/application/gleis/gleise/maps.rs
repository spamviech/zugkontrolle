//! Struktur zum Speichern aller Gleise

use std::collections::HashMap;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        OutputSerialisiert,
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

pub type Map<T> = HashMap<GleisId<T>, Gleis<T>>;
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
pub(crate) struct GleiseVecs<Z: Zugtyp> {
    pub(crate) name: String,
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert<Z>>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert<Z>>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert<Z>>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert<Z>>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert<Z>>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert<Z>>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert<Z>>>,
    pub(crate) streckenabschnitte:
        HashMap<streckenabschnitt::Name, Streckenabschnitt<OutputSerialisiert>>,
    pub(crate) geschwindigkeiten: geschwindigkeit::Map<<Z::Leiter as Serialisiere>::Serialisiert>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Z: Zugtyp>
    From<(&GleiseMaps<Z>, geschwindigkeit::Map<<Z::Leiter as Serialisiere>::Serialisiert>)>
    for GleiseVecs<Z>
{
    fn from(
        (maps, geschwindigkeiten): (
            &GleiseMaps<Z>,
            geschwindigkeit::Map<<Z::Leiter as Serialisiere>::Serialisiert>,
        ),
    ) -> Self {
        macro_rules! hashmaps_to_vecs {
            ($($map:ident),* $(,)?) => {
                GleiseVecs {
                    name: Z::NAME.to_string(),
                    streckenabschnitte: maps.streckenabschnitte.iter().map(
                        |(name, (Streckenabschnitt {farbe, anschluss}, _fließend))|
                            (name.clone(), Streckenabschnitt {farbe: *farbe, anschluss: anschluss.serialisiere()} )
                        ).collect(),
                    geschwindigkeiten,
                    // TODO wirkliche Konvertierung, sobald Plan implementiert ist
                    pläne: Vec::new(),
                    $($map: maps.$map.values().map(
                        |Gleis {position, definition, streckenabschnitt}|
                        Gleis {
                            position: position.clone(),
                            definition: definition.serialisiere(),
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
