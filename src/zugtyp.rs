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
    /// Mögliche Geschwindigkeitswerte
    type Geschwindigkeit;
    /// Anschluss zur Geschwindigkeitskontrolle
    type GeschwindigkeitsAnschluss;
    /// Einstellen der Geschwindigkeit
    fn geschwindigkeit(
        anschluss: &mut Self::GeschwindigkeitsAnschluss,
        wert: Self::Geschwindigkeit,
    );
    /// Umdrehen
    fn umdrehen(anschluss: &mut Self::GeschwindigkeitsAnschluss);

    fn geraden() -> Vec<Gerade<Self>>;
    fn kurven() -> Vec<Kurve<Self>>;
    fn weichen() -> Vec<Weiche<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeiche<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeiche<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeiche<Self>>;
    fn kreuzungen() -> Vec<Kreuzung<Self>>;
}
