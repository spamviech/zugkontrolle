//! Struktur zum Speichern aller Gleise

use std::collections::HashMap;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        speichern::{self, Reserviere, Reserviert, ToSave},
        OutputSave,
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

impl<T: ToSave> ToSave for Gleis<T> {
    type Save = Gleis<T::Save>;

    fn to_save(&self) -> Self::Save {
        Gleis {
            definition: self.definition.to_save(),
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
    ) -> speichern::Result<Gleis<R>> {
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
    pub(crate) streckenabschnitte: HashMap<streckenabschnitt::Name, Streckenabschnitt<OutputSave>>,
    pub(crate) geschwindigkeiten: geschwindigkeit::Map<<Z::Leiter as ToSave>::Save>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Z: Zugtyp> From<(&GleiseMaps<Z>, geschwindigkeit::Map<<Z::Leiter as ToSave>::Save>)>
    for GleiseVecs<Z>
{
    fn from(
        (maps, geschwindigkeiten): (
            &GleiseMaps<Z>,
            geschwindigkeit::Map<<Z::Leiter as ToSave>::Save>,
        ),
    ) -> Self {
        macro_rules! hashmaps_to_vecs {
            ($($map:ident),* $(,)?) => {
                GleiseVecs {
                    name: Z::NAME.to_string(),
                    streckenabschnitte: maps.streckenabschnitte.iter().map(
                        |(name, (Streckenabschnitt {farbe, anschluss}, _fließend))|
                            (name.clone(), Streckenabschnitt {farbe: *farbe, anschluss: anschluss.to_save()} )
                        ).collect(),
                    geschwindigkeiten,
                    // TODO wirkliche Konvertierung, sobald Plan implementiert ist
                    pläne: Vec::new(),
                    $($map: maps.$map.values().map(
                        |Gleis {position, definition, streckenabschnitt}|
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
