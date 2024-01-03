//! Steuerung einer Model-Eisenbahn über einen Raspberry Pi.

#![allow(unused_crate_dependencies)]

use zugkontrolle::application::{ausführen_aus_env, Fehler};

fn main() -> Result<(), Fehler> {
    ausführen_aus_env()
}
