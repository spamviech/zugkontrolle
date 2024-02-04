//! Steuerung einer Model-Eisenbahn über einen Raspberry Pi.

// Werden in der lib verwendet. Alles explizit nicht zu benutzen führt nur zu Duplikation mit der `Cargo.toml`.
#![allow(unused_crate_dependencies)]
// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use zugkontrolle::application::{ausführen_aus_env, Fehler};

fn main() -> Result<(), Fehler> {
    ausführen_aus_env()
}
