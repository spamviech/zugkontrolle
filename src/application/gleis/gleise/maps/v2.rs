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
