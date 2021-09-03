//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden

use serde::{Deserialize, Serialize};

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

    use super::*;

    pub mod weiche {
        use super::*;
        use crate::application::{
            gleis::weiche::{dreiwege, gerade, kurve, s_kurve, Orientierung},
            typen::*,
        };

        #[derive(
            zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize,
        )]
        pub struct Weiche<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub winkel: Winkel,
            pub orientierung: Orientierung,
            pub beschreibung: Option<String>,
            pub steuerung: Option<steuerung::Weiche<gerade::Richtung, gerade::RichtungAnschlüsse>>,
        }
    }
}
