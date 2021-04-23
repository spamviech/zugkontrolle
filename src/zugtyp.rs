//! Zugtyp Trait + Phantom-Typen + Spurweite

use serde::{Deserialize, Serialize};

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
#[derive(Serialize, Deserialize)]
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

pub mod geschwindigkeit {
    use non_empty_vec::NonEmpty;

    use super::*;

    // TODO vermutlich nur 2 Varianten, evtl. besser Methoden zu verwenden.
    pub enum Geschwindigkeit {
        Pwm(Anschluss),
        FesteSpannung(NonEmpty<Anschluss>),
    }
    impl Geschwindigkeit {
        pub fn geschwindigkeit(&mut self, wert: u8) {
            //TODO
            unimplemented!("geschwindigkeit({:?})", wert)
        }
    }

    // TODO in zwei Datentypen aufteilen, Summentyp behalten?
    // unterschiedliche assoziierte Typen zum Geschwindigkeit einstellen, keine Eigenschaft der Geschwindigkeit
    // Nicht wirklich Eigenschaft eines Zugtyps, beide können parallel existieren.
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Pwm;
    #[derive(Debug, Serialize, Deserialize)]
    pub struct FesteSpannung;
}

pub mod value {
    use std::time::Duration;

    use super::geschwindigkeit::*;
    use super::*;
    use crate::gleis::types::*;

    // TODO in Zugtyp hinzufügen
    // einziger wirklicher Unterschied zwischen Märklin/Lego
    // in zwei Datentypen aufteilen, Summentyp behalten?
    // unterschiedliche Methoden: umdrehen(), fahrtrichtung_einstellen(Fahrtrichtung)
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Uberspannung {
        zeit: Duration,
    }
    impl Uberspannung {
        // TODO FesteSpannung benötigt einen zusätzlichen Anschluss!
        // über associated Type regeln?
        pub fn umdrehen(&self, geschwindigkeit: &mut Geschwindigkeit) {
            // TODO
            unimplemented!("umdrehen")
        }
    }

    #[derive(Debug)]
    pub enum Fahrtrichtung {
        Vorwaerts,
        Rueckwaerts,
    }
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Schalter;
    impl Schalter {
        pub fn fahrtrichtung_einstellen(
            fahrtrichtung: Fahrtrichtung,
            anschluss: &mut Anschluss,
            geschwindigkeit: &mut Geschwindigkeit,
        ) {
            // TODO
            unimplemented!("fahrtrichtung_einstellen({:?})", fahrtrichtung)
        }
    }
    #[derive(Debug, Serialize, Deserialize)]
    pub enum Umdrehen {
        Uberspannung(Uberspannung),
        Schalter(Schalter),
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct Zugtyp {
        pub spurweite: canvas::Abstand<canvas::Y>,
        pub geraden: Vec<Gerade<()>>,
        pub kurven: Vec<Kurve<()>>,
        pub weichen: Vec<Weiche<()>>,
        pub dreiwege_weichen: Vec<DreiwegeWeiche<()>>,
        pub kurven_weichen: Vec<KurvenWeiche<()>>,
        pub s_kurven_weichen: Vec<SKurvenWeiche<()>>,
        pub kreuzungen: Vec<Kreuzung<()>>,
    }

    // TODO Anstatt generic über <Z: Zugtyp> erhalten alle Zeichnen-Methoden einen &Zugtyp-Parameter
    impl Zugtyp {
        /// Spurweite für den gegebenen Zugtyp
        pub fn spurweite(&self) -> canvas::Abstand<canvas::Y> {
            self.spurweite
        }

        /// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
        pub fn abstand(&self) -> canvas::Abstand<canvas::Y> {
            self.spurweite / 3.
        }
        /// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
        pub fn beschraenkung(&self) -> canvas::Abstand<canvas::Y> {
            self.spurweite + 2. * self.abstand()
        }
        /// Äußerster Radius (inklusive Beschränkung) einer Kurve
        pub fn radius_begrenzung_aussen(
            &self,
            radius: canvas::Abstand<canvas::Radius>,
        ) -> canvas::Abstand<canvas::Radius> {
            radius + 0.5 * self.spurweite.as_radius() + self.abstand().as_radius()
        }
        /// Innerster Radius (inklusive Beschränkung) einer Kurve
        pub fn radius_begrenzung_innen(
            &self,
            radius: canvas::Abstand<canvas::Radius>,
        ) -> canvas::Abstand<canvas::Radius> {
            radius - 0.5 * self.spurweite.as_radius() - self.abstand().as_radius()
        }
    }

    // TODO use walkdir to go through all yaml/toml/? files in a subdirectory
    // yaml feels more natural (mostly no quotes for strings/enums)
    // toml doesn't add new dependencies (toml-crate already required)
}

pub mod deserialize {
    use std::marker::PhantomData;

    use serde::Deserialize;

    use crate::gleis::types::*;
    use crate::gleis::{gerade, kreuzung, kurve, weiche};

    // TODO erstelle via Macro
    /// mockup-Variante von super::value::Zugtyp, für schönere toml-Darstellung
    /// f32-Werte sind mm-Größen
    #[derive(Debug, Deserialize)]
    pub struct Zugtyp {
        pub spurweite: f32,
        pub geraden: Vec<Gerade>,
        pub kurven: Vec<Kurve>,
        pub weichen: Vec<Weiche>,
        pub dreiwege_weichen: Vec<DreiwegeWeiche>,
        pub kurven_weichen: Vec<KurvenWeiche>,
        pub s_kurven_weichen: Vec<SKurvenWeiche>,
        pub kreuzungen: Vec<Kreuzung>,
    }

    #[derive(Debug, Deserialize)]
    pub struct Gerade {
        pub laenge: f32,
        pub beschreibung: Option<String>,
    }
    impl<Z> From<Gerade> for gerade::Gerade<Z> {
        fn from(Gerade { laenge, beschreibung }: Gerade) -> Self {
            gerade::Gerade {
                zugtyp: PhantomData,
                laenge: Length::new(laenge).to_abstand(),
                beschreibung,
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Kurve {
        pub radius: f32,
        pub winkel: f32,
        pub beschreibung: Option<String>,
    }
    impl<Z> From<Kurve> for kurve::Kurve<Z> {
        fn from(Kurve { radius, winkel, beschreibung }: Kurve) -> Self {
            kurve::Kurve {
                zugtyp: PhantomData,
                radius: Radius::new(radius).to_abstand(),
                winkel: AngleDegrees::new(winkel).into(),
                beschreibung,
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Weiche;
    impl<Z> From<Weiche> for weiche::Weiche<Z> {
        fn from(_: Weiche) -> Self {
            weiche::Weiche::new(
                Length::new(0.),
                Radius::new(0.),
                Angle::new(0.),
                weiche::Richtung::Links,
            )
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct DreiwegeWeiche;
    impl<Z> From<DreiwegeWeiche> for weiche::DreiwegeWeiche<Z> {
        fn from(_: DreiwegeWeiche) -> Self {
            weiche::DreiwegeWeiche::new(Length::new(0.), Radius::new(0.), Angle::new(0.))
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct KurvenWeiche;
    impl<Z> From<KurvenWeiche> for weiche::KurvenWeiche<Z> {
        fn from(_: KurvenWeiche) -> Self {
            weiche::KurvenWeiche::new(
                Length::new(0.),
                Radius::new(0.),
                Angle::new(0.),
                weiche::Richtung::Links,
            )
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct SKurvenWeiche;
    impl<Z> From<SKurvenWeiche> for weiche::SKurvenWeiche<Z> {
        fn from(_: SKurvenWeiche) -> Self {
            weiche::SKurvenWeiche::new(
                Length::new(0.),
                Radius::new(0.),
                Angle::new(0.),
                Radius::new(0.),
                Angle::new(0.),
                weiche::Richtung::Links,
            )
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Kreuzung;
    impl<Z> From<Kreuzung> for kreuzung::Kreuzung<Z> {
        fn from(_: Kreuzung) -> Self {
            kreuzung::Kreuzung::new(Length::new(0.), Radius::new(0.), kreuzung::Variante::OhneKurve)
        }
    }
}
