//! unit tests für das anschluss-Modul

use std::thread::sleep;
use std::time::Duration;

use num_x::u3;
use simple_logger::SimpleLogger;

use crate::anschluss::{
    anschlüsse::{AnschlussBeschreibung, Anschlüsse, SyncFehler},
    level::Level,
    pcf8574,
};

#[test]
fn drop_semantics() {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    let mut lock_count: usize = 0;
    let mut lock_anschlüsse = || {
        lock_count += 1;
        Anschlüsse::lock().expect(&format!("{}.ter Aufruf von lock.", lock_count))
    };
    let llln = lock_anschlüsse()
        .reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0),
        )
        .expect("1. Aufruf von llln.");
    assert_eq!(
        llln.beschreibung(),
        &pcf8574::Beschreibung {
            a0: Level::Low,
            a1: Level::Low,
            a2: Level::Low,
            variante: pcf8574::Variante::Normal
        }
    );
    assert_eq!(llln.port(), u3::new(0));
    assert_eq!(
        lock_anschlüsse().reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0)
        ),
        Err(SyncFehler::AnschlussInVerwendung(AnschlussBeschreibung::Pcf8574Port {
            a0: Level::Low,
            a1: Level::Low,
            a2: Level::Low,
            variante: pcf8574::Variante::Normal,
            port: u3::new(0)
        })),
        "2. Aufruf von llln."
    );
    drop(llln);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln = lock_anschlüsse()
        .reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0),
        )
        .expect("Aufruf von llln nach drop.");

    // jetzt sollte Anschlüsse wieder verfügbar sein
    assert_eq!(
        lock_anschlüsse().reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0)
        ),
        Err(SyncFehler::AnschlussInVerwendung(AnschlussBeschreibung::Pcf8574Port {
            a0: Level::Low,
            a1: Level::Low,
            a2: Level::Low,
            variante: pcf8574::Variante::Normal,
            port: u3::new(0)
        })),
        "Aufruf von llln mit vorherigem Ergebnis in scope."
    );
    drop(llln);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln0 = lock_anschlüsse()
        .reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0),
        )
        .expect("Aufruf von llln nach drop.");
    let llln1 = lock_anschlüsse()
        .reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(1),
        )
        .expect("Aufruf von llln nach drop, alternativer port.");
    drop(llln0);
    drop(llln1);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln = lock_anschlüsse()
        .reserviere_pcf8574_port(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal,
            u3::new(0),
        )
        .expect("Aufruf von llln nach drop.");
    drop(llln);
}
