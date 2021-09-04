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

    use zugkontrolle_derive as zug_derive;

    use super::*;
    use crate::application::typen::*;

    pub mod weiche {
        use super::*;
        use crate::application::gleis::weiche::{dreiwege, gerade, kurve, s_kurve, Orientierung};

        #[derive(zug_derive::Clone, zug_derive::Debug, Serialize, Deserialize)]
        pub struct Weiche<Z> {
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
        pub struct DreiwegeWeiche<Z> {
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
        pub struct KurvenWeiche<Z> {
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
        pub struct SKurvenWeiche<Z> {
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
        pub struct Kreuzung<Z> {
            pub zugtyp: PhantomData<fn() -> Z>,
            pub länge: Skalar,
            pub radius: Skalar,
            pub variante: kreuzung::Variante,
            pub beschreibung: Option<String>,
            pub steuerung: Option<steuerung::Weiche<Richtung, RichtungAnschlüsseSerialisiert>>,
        }
    }
}
