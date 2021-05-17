//! unittests für das anschluss-Modul

use std::thread::sleep;
use std::time::Duration;

use simple_logger::SimpleLogger;

use super::{pcf8574, Anschlüsse, Level, Ports, SyncError};

#[test]
fn drop_semantics() {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    let mut anschlüsse = Anschlüsse::neu().expect("1.ter Aufruf von neu.");
    Anschlüsse::neu().expect_err("2.ter Aufruf von neu.");
    let llln = anschlüsse
        .reserviere_pcf8574(Level::Low, Level::Low, Level::Low, pcf8574::Variante::Normal)
        .expect("1. Aufruf von llln.");
    assert_eq!([llln.a0, llln.a1, llln.a2], [Level::Low, Level::Low, Level::Low]);
    assert_eq!(llln.variante, pcf8574::Variante::Normal);
    assert_eq!(
        anschlüsse.reserviere_pcf8574(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal
        ),
        Err(SyncError::InVerwendung),
        "2. Aufruf von llln."
    );
    drop(llln);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln = anschlüsse
        .reserviere_pcf8574(Level::Low, Level::Low, Level::Low, pcf8574::Variante::Normal)
        .expect("Aufruf von llln nach drop.");
    drop(anschlüsse);

    // jetzt sollte Anschlüsse wieder verfügbar sein
    let mut anschlüsse = Anschlüsse::neu().expect("Aufruf von neu nach drop.");
    assert_eq!(
        anschlüsse.reserviere_pcf8574(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal
        ),
        Err(SyncError::InVerwendung),
        "Aufruf von llln mit vorherigem Ergebnis in scope."
    );
    drop(llln);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln = anschlüsse
        .reserviere_pcf8574(Level::Low, Level::Low, Level::Low, pcf8574::Variante::Normal)
        .expect("Aufruf von llln nach drop.");
    let Ports { p0, p1, p2, p3, p4, p5, p6, p7 } = llln.into();
    assert_eq!(
        anschlüsse.reserviere_pcf8574(
            Level::Low,
            Level::Low,
            Level::Low,
            pcf8574::Variante::Normal
        ),
        Err(SyncError::InVerwendung),
        "Aufruf von llln mit ports in scope."
    );
    drop(p0);
    drop(p1);
    drop(p2);
    drop(p3);
    drop(p4);
    drop(p5);
    drop(p6);
    drop(p7);
    // Warte etwas, damit der restore-thread genug Zeit hat.
    sleep(Duration::from_secs(1));
    let llln = anschlüsse
        .reserviere_pcf8574(Level::Low, Level::Low, Level::Low, pcf8574::Variante::Normal)
        .expect("Aufruf von llln nach drop.");
    drop(llln);
    drop(anschlüsse);
}
