//! Unit tests für das anschluss-Modul.

use zugkontrolle_test_util::init_test_logging;
use zugkontrolle_util::eingeschränkt::kleiner_8;

use crate::{
    level::Level,
    pcf8574::{Beschreibung, I2cBus, I2cSettings, InVerwendung, Lager, Port, Variante},
    pin,
};

#[test]
fn drop_semantics() {
    init_test_logging();

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

    let llln = lager.reserviere_erwarte_erfolg(llln_beschreibung, port0, "1. Aufruf von llln.");
    lager.reserviere_erwarte_in_verwendung(
        llln_beschreibung,
        port0,
        "Aufruf von llln mit vorherigem Ergebnis in scope.",
    );
    drop(llln);

    let llln0 =
        lager.reserviere_erwarte_erfolg(llln_beschreibung, port0, "Aufruf von llln nach drop.");
    let llln1 = lager.reserviere_erwarte_erfolg(
        llln_beschreibung,
        port7,
        "Aufruf von llln nach drop, alternativer port.",
    );
    lager.reserviere_erwarte_in_verwendung(
        llln_beschreibung,
        port7,
        "Aufruf von llln1 mit vorherigem Ergebnis in scope.",
    );
    drop(llln0);
    drop(llln1);

    let _ = lager.reserviere_erwarte_erfolg(
        llln_beschreibung,
        port0,
        "Aufruf von llln nach erneutem drop.",
    );
}

// nur für Tests
#[allow(clippy::multiple_inherent_impl)]
impl Lager {
    fn reserviere_erwarte_erfolg(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
        assert_nachricht: &str,
    ) -> Port {
        let llln = self.reserviere_pcf8574_port(beschreibung, port).expect(assert_nachricht);
        assert_eq!(llln.beschreibung(), &beschreibung, "{assert_nachricht}",);
        assert_eq!(llln.port(), port, "{assert_nachricht}",);
        llln
    }

    fn reserviere_erwarte_in_verwendung(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
        assert_nachricht: &str,
    ) {
        assert!(self.in_verwendung_eq(beschreibung, port), "{assert_nachricht}",);
    }

    fn in_verwendung_eq(&mut self, beschreibung: Beschreibung, port: kleiner_8) -> bool {
        if let Err(in_verwendung) = self.reserviere_pcf8574_port(beschreibung, port) {
            in_verwendung == InVerwendung { beschreibung, port }
        } else {
            false
        }
    }
}
