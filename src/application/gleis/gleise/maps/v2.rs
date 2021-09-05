//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::gleis::{
        gerade::GeradeSerialisiert, gleise::maps::Gleis, kurve::KurveSerialisiert,
    },
    steuerung::{
        geschwindigkeit,
        plan::Plan,
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    zugtyp::Zugtyp,
};

use self::gleis::{kreuzung::*, weiche::*};

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
    pub(crate) streckenabschnitte: HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>,
    pub(crate) geschwindigkeiten:
        geschwindigkeit::MapSerialisiert<<Z::Leiter as Serialisiere>::Serialisiert>,
    pub(crate) pläne: Vec<Plan>,
}

pub mod steuerung {
    use super::*;

    use crate::steuerung::weiche;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Weiche<Richtung, Anschlüsse> {
        pub name: weiche::Name,
        pub aktuelle_richtung: Richtung,
        pub letzte_richtung: Richtung,
        pub anschlüsse: Anschlüsse,
    }
}

pub mod gleis {
    use std::marker::PhantomData;

    use zugkontrolle_derive as zug_derive;

    use super::*;
    use crate::application::typen::*;

    pub mod weiche {
        use super::*;
        use crate::application::gleis::weiche::{dreiwege, gerade, kurve, s_kurve, Orientierung};

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct WeicheSerialisiert<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub winkel: Winkel,
            pub orientierung: Orientierung,
            pub beschreibung: Option<String>,
            pub steuerung:
                Option<steuerung::Weiche<gerade::Richtung, gerade::RichtungAnschlüsseSerialisiert>>,
        }

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct DreiwegeWeicheSerialisiert<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub winkel: Winkel,
            pub beschreibung: Option<String>,
            pub steuerung: Option<
                steuerung::Weiche<dreiwege::Richtung, dreiwege::RichtungAnschlüsseSerialisiert>,
            >,
        }

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct KurvenWeicheSerialisiert<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub winkel: Winkel,
            pub orientierung: Orientierung,
            pub beschreibung: Option<String>,
            pub steuerung:
                Option<steuerung::Weiche<kurve::Richtung, kurve::RichtungAnschlüsseSerialisiert>>,
        }

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct SKurvenWeicheSerialisiert<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub winkel: Winkel,
            pub radius_reverse: Skalar,
            pub winkel_reverse: Winkel,
            pub orientierung: Orientierung,
            pub beschreibung: Option<String>,
            pub steuerung: Option<
                steuerung::Weiche<s_kurve::Richtung, s_kurve::RichtungAnschlüsseSerialisiert>,
            >,
        }
    }

    pub mod kreuzung {
        use super::*;
        use crate::application::gleis::kreuzung::{
            Richtung, RichtungAnschlüsseSerialisiert, Variante,
        };

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct KreuzungSerialisiert<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub variante: kreuzung::Variante,
            pub beschreibung: Option<String>,
            pub steuerung: Option<steuerung::Weiche<Richtung, RichtungAnschlüsseSerialisiert>>,
        }
    }
}
