//! unit tests für das anschluss-Modul

use num_x::u3;
use simple_logger::SimpleLogger;

use crate::anschluss::{
    level::Level,
    pcf8574::{Beschreibung, I2cBus, InVerwendung, Port, ReservierenFehler, Variante},
};

#[test]
fn drop_semantics() {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    let llln_beschreibung = Beschreibung {
        i2c_bus: I2cBus::I2c0_1,
        a0: Level::Low,
        a1: Level::Low,
        a2: Level::Low,
        variante: Variante::Normal,
    };
    let port0 = u3::new(0);
    let port1 = u3::new(1);

    let llln = reserviere_erwarte_erfolg(llln_beschreibung.clone(), port0, "1. Aufruf von llln.");
    reserviere_erwarte_in_verwendung(
        llln_beschreibung.clone(),
        port0,
        "Aufruf von llln mit vorherigem Ergebnis in scope.",
    );
    drop(llln);

    let llln0 =
        reserviere_erwarte_erfolg(llln_beschreibung.clone(), port0, "Aufruf von llln nach drop.");
    let llln1 = reserviere_erwarte_erfolg(
        llln_beschreibung.clone(),
        port1,
        "Aufruf von llln nach drop, alternativer port.",
    );
    reserviere_erwarte_in_verwendung(
        llln_beschreibung.clone(),
        port1,
        "Aufruf von llln1 mit vorherigem Ergebnis in scope.",
    );
    drop(llln0);
    drop(llln1);

    let _ = reserviere_erwarte_erfolg(
        llln_beschreibung.clone(),
        port0,
        "Aufruf von llln nach erneutem drop.",
    );
}

fn reserviere_erwarte_erfolg(beschreibung: Beschreibung, port: u3, assert_nachricht: &str) -> Port {
    let llln = Port::reserviere(beschreibung.clone(), port).expect(assert_nachricht);
    assert_eq!(llln.beschreibung(), &beschreibung, "{}", assert_nachricht);
    assert_eq!(llln.port(), port, "{}", assert_nachricht);
    llln
}
fn reserviere_erwarte_in_verwendung(beschreibung: Beschreibung, port: u3, assert_nachricht: &str) {
    assert!(in_verwendung_eq(beschreibung, port), "{}", assert_nachricht)
}
fn in_verwendung_eq(beschreibung: Beschreibung, port: u3) -> bool {
    if let Err(ReservierenFehler::InVerwendung(in_verwendung)) =
        Port::reserviere(beschreibung, port)
    {
        in_verwendung == InVerwendung { beschreibung, port }
    } else {
        false
    }
}
