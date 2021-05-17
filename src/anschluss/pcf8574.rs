//! PCF8574, gesteuert über I2C.

use std::sync::{mpsc::Sender, Arc, RwLock};

use log::debug;
use ux::u3;

use super::level::Level;
use super::pin::InputPin;

/// Ein PCF8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    pub(super) a0: Level,
    pub(super) a1: Level,
    pub(super) a2: Level,
    pub(super) variante: Variante,
    pub(super) wert: u8,
    pub(super) sender: Sender<(Level, Level, Level, Variante, u8)>,
}
impl PartialEq for Pcf8574 {
    fn eq(&self, other: &Self) -> bool {
        self.a0 == other.a0
            && self.a1 == other.a1
            && self.a2 == other.a2
            && self.variante == other.variante
    }
}
impl Eq for Pcf8574 {}
impl Drop for Pcf8574 {
    fn drop(&mut self) {
        let Pcf8574 { a0, a1, a2, variante, wert, sender } = self;
        debug!("dropped {:?} {:?} {:?} {:?}", a0, a1, a2, variante);
        // Schicke Werte als Tupel, um keine Probleme mit dem Drop-Handler zu bekommen.
        // (Ein Klon würde bei send-Fehler eine Endlos-Schleife erzeugen)
        if let Err(err) = sender.send((*a0, *a1, *a2, *variante, *wert)) {
            debug!("send error while dropping: {}", err)
        }
    }
}
/// Variante eines Pcf8574, beeinflusst die I2C-Adresse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variante {
    Normal,
    A,
}
/// Ein Pcf8574, konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPcf8574(Pcf8574);
/// Ein Pcf8574, konfiguriert für Input inklusive InterruptPin.
#[derive(Debug)]
pub struct InputPcf8574 {
    pcf8574: Pcf8574,
    interrupt: InputPin,
}

/// Ein Port eines PCF8574.
#[derive(Debug)]
pub struct Port<T> {
    pcf8574: Arc<RwLock<T>>,
    port: u3,
}
impl<T> Port<T> {
    pub fn port(&self) -> u3 {
        self.port
    }
}
/// Alle Ports eines PCF8574.
#[derive(Debug)]
pub struct Ports<T> {
    pub p0: Port<T>,
    pub p1: Port<T>,
    pub p2: Port<T>,
    pub p3: Port<T>,
    pub p4: Port<T>,
    pub p5: Port<T>,
    pub p6: Port<T>,
    pub p7: Port<T>,
}
impl<T> From<T> for Ports<T> {
    fn from(t: T) -> Self {
        // for Pcf8574 as T, drop will be called when last Arc goes out of scope
        let arc = Arc::new(RwLock::new(t));
        Ports {
            p0: Port { pcf8574: arc.clone(), port: u3::new(0) },
            p1: Port { pcf8574: arc.clone(), port: u3::new(1) },
            p2: Port { pcf8574: arc.clone(), port: u3::new(2) },
            p3: Port { pcf8574: arc.clone(), port: u3::new(3) },
            p4: Port { pcf8574: arc.clone(), port: u3::new(4) },
            p5: Port { pcf8574: arc.clone(), port: u3::new(5) },
            p6: Port { pcf8574: arc.clone(), port: u3::new(6) },
            p7: Port { pcf8574: arc, port: u3::new(7) },
        }
    }
}
