//! Unit tests für das anschluss-Modul

use flexi_logger::{LogSpecBuilder, Logger};

use crate::{
    anschluss::{
        level::Level,
        pcf8574::{Beschreibung, I2cBus, I2cSettings, InVerwendung, Lager, Port, Variante},
        pin,
    },
    eingeschränkt::kleiner_8,
};

#[test]
// FIXME Test verursacht stack overflow
// kein Problem in main code, sollte aber trotzdem behoben werden
fn drop_semantics() {
    let mut log_spec_builder = LogSpecBuilder::new();
    let _ = log_spec_builder
        .default(log::LevelFilter::Error)
        .module("zugkontrolle", log::LevelFilter::Debug);
    let log_spec = log_spec_builder.finalize();
    let log_handle = Logger::with(log_spec)
        .log_to_stderr()
        .start()
        .expect("Logging initialisieren fehlgeschlagen!");

    let llln_beschreibung = Beschreibung {
        i2c_bus: I2cBus::I2c0_1,
        a0: Level::Low,
        a1: Level::Low,
        a2: Level::Low,
        variante: Variante::Normal,
    };
    let port0 = kleiner_8::MIN;
    let port7 = kleiner_8::MAX;

    let i2c_settings =
        I2cSettings { i2c0_1: true, i2c3: false, i2c4: false, i2c5: false, i2c6: false };
    let mut pin_lager = pin::Lager::neu().expect("pin::Lager erstellen fehlgeschlagen!");
    let mut lager =
        Lager::neu(&mut pin_lager, i2c_settings).expect("pcf8574::Lager erstellen fehlgeschlagen!");

    let llln =
        lager.reserviere_erwarte_erfolg(llln_beschreibung.clone(), port0, "1. Aufruf von llln.");
    lager.reserviere_erwarte_in_verwendung(
        llln_beschreibung.clone(),
        port0,
        "Aufruf von llln mit vorherigem Ergebnis in scope.",
    );
    drop(llln);

    let llln0 = lager.reserviere_erwarte_erfolg(
        llln_beschreibung.clone(),
        port0,
        "Aufruf von llln nach drop.",
    );
    let llln1 = lager.reserviere_erwarte_erfolg(
        llln_beschreibung.clone(),
        port7,
        "Aufruf von llln nach drop, alternativer port.",
    );
    lager.reserviere_erwarte_in_verwendung(
        llln_beschreibung.clone(),
        port7,
        "Aufruf von llln1 mit vorherigem Ergebnis in scope.",
    );
    drop(llln0);
    drop(llln1);

    let _ = lager.reserviere_erwarte_erfolg(
        llln_beschreibung.clone(),
        port0,
        "Aufruf von llln nach erneutem drop.",
    );

    drop(log_handle);
}

impl Lager {
    fn reserviere_erwarte_erfolg(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
        assert_nachricht: &str,
    ) -> Port {
        let llln =
            self.reserviere_pcf8574_port(beschreibung.clone(), port).expect(assert_nachricht);
        assert_eq!(llln.beschreibung(), &beschreibung, "{}", assert_nachricht);
        assert_eq!(llln.port(), port, "{}", assert_nachricht);
        llln
    }
    fn reserviere_erwarte_in_verwendung(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
        assert_nachricht: &str,
    ) {
        assert!(self.in_verwendung_eq(beschreibung, port), "{}", assert_nachricht)
    }
    fn in_verwendung_eq(&mut self, beschreibung: Beschreibung, port: kleiner_8) -> bool {
        if let Err(in_verwendung) = self.reserviere_pcf8574_port(beschreibung, port) {
            in_verwendung == InVerwendung { beschreibung, port }
        } else {
            false
        }
    }
}
