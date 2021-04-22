//! Zugtyp Trait + Phantom-Typen + Spurweite

use crate::gleis::{
    gerade::Gerade,
    kreuzung::Kreuzung,
    kurve::Kurve,
    weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
};

/// TODO
/// Wirkliche Implementierung in eigenem Modul erstellen
/// Pin, I2C/PCF8574Port, weitere?
// Als Inspiration:
// The GPIO war: macro bunkers for typestate explosions
// https://www.ecorax.net/macro-bunker-1/
pub struct Anschluss;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp: Sized {
    /// Spurweite in mm.
    const SPURWEITE: Spurweite;

    fn geraden() -> Vec<Gerade<Self>>;
    fn kurven() -> Vec<Kurve<Self>>;
    fn weichen() -> Vec<Weiche<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeiche<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeiche<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeiche<Self>>;
    fn kreuzungen() -> Vec<Kreuzung<Self>>;
}

// TODO vermutlich nur 2 Varianten, evtl. besser Methoden zu verwenden.
pub trait Geschwindigkeit {
    /// Mögliche Geschwindigkeitswerte
    type Wert;
    /// Anschluss/Anschlüsse zur Geschwindigkeitskontrolle
    type Anschluss;
    /// Einstellen der Geschwindigkeit
    fn geschwindigkeit(anschluss: &mut Self::Anschluss, wert: Self::Wert);
    // Fahrtrichtung (falls vorhanden)
    type Fahrtrichtung;
    /// Umdrehen
    fn umdrehen(anschluss: &mut Self::Anschluss, fahrtrichtung: Self::Fahrtrichtung);
}
