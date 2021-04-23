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
#[derive(Debug, Serialize, Deserialize)]
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

    /// Pwm-basierte Geschwindigkeitskontrolle
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Pwm(Anschluss);

    /// Geschwindigkeitskontrolle über mehrere Stromquellen mit fester Spannung
    #[derive(Debug, Serialize, Deserialize)]
    pub struct FesteSpannung(NonEmpty<Anschluss>);

    /// Mögliche Geschwindigkeitswerte
    ///
    /// Pwm: wert/maximaler_wert bestimmt duty_cycle
    /// FesteSpannung: wähle Anschluss wert (als Index), größere Werte verwenden immer den letzten Anschluss
    #[derive(Debug)]
    pub struct Wert(pub u8);

    // TODO vermutlich nur 2 Varianten, evtl. besser Methoden zu verwenden.
    pub enum Geschwindigkeit {
        Pwm(Pwm),
        FesteSpannung(FesteSpannung),
    }
    impl Geschwindigkeit {
        pub fn geschwindigkeit(&mut self, wert: Wert) {
            //TODO
            unimplemented!("geschwindigkeit({:?})", wert)
        }
    }
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
    pub struct Ueberspannung {
        pub zeit: Duration,
    }
    impl Ueberspannung {
        // TODO FesteSpannung benötigt einen zusätzlichen Anschluss (mit Überspannung), Pwm nicht (verwendet volle pwm duty_cycle)!
        pub fn umdrehen_pwm(&self, geschwindigkeit: &mut Pwm) {
            // TODO
            unimplemented!("umdrehen_pwm")
        }
        pub fn umdrehen_feste_spannung(
            &self,
            anschluss: &mut Anschluss,
            geschwindigkeit: &mut FesteSpannung,
        ) {
            // TODO
            unimplemented!("umdrehen_feste_spannung")
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
        Ueberspannung(Ueberspannung),
        Schalter(Schalter),
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct Zugtyp {
        pub spurweite: canvas::Abstand<canvas::Y>,
        pub umdrehen: Umdrehen,
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
    use std::collections::HashMap;
    use std::fs::File;
    use std::path::Path;
    use std::{ffi::OsStr, marker::PhantomData};

    use log::*;
    use serde::Deserialize;
    use walkdir::WalkDir;

    use super::value::{self, Schalter};
    use crate::gleis::types::*;
    use crate::gleis::{gerade, kreuzung, kurve, weiche};

    // TODO erstelle via Macro
    /// mockup-Variante von super::value::Zugtyp, für schönere toml-Darstellung
    /// f32-Werte sind mm-Größen
    #[derive(Debug, Deserialize)]
    pub struct Zugtyp {
        pub spurweite: f32,
        pub umdrehen: Umdrehen,
        pub geraden: Vec<Gerade>,
        pub kurven: Vec<Kurve>,
        pub weichen: Vec<Weiche>,
        pub dreiwege_weichen: Vec<DreiwegeWeiche>,
        pub kurven_weichen: Vec<KurvenWeiche>,
        pub s_kurven_weichen: Vec<SKurvenWeiche>,
        pub kreuzungen: Vec<Kreuzung>,
    }
    impl Zugtyp {
        pub fn load_all_from_dir<P: AsRef<Path>>(dir: P) -> HashMap<String, Self> {
            let mut zugtypen = HashMap::new();

            for entry in
                WalkDir::new(dir).min_depth(1).max_depth(1).into_iter().filter_entry(|dir_entry| {
                    dir_entry.path().extension().and_then(OsStr::to_str) == Some("yaml")
                })
            {
                if let Ok(dir_entry) = entry {
                    let path = dir_entry.path();
                    if let Some(name_ref) = path.file_stem().and_then(OsStr::to_str) {
                        let name = name_ref.to_string();
                        if let Ok(file) = File::open(path) {
                            match serde_yaml::from_reader(file) {
                                Ok(zugtyp) => {
                                    zugtypen.insert(name, zugtyp);
                                }
                                Err(err) => {
                                    warn!(
                                        "failed to parse yaml file \"{}\": {:?}",
                                        path.display(),
                                        err
                                    )
                                }
                            }
                        } else {
                            warn!("failed to open file: {:?}", path)
                        }
                    } else {
                        warn!("no file_stem: {:?}", path)
                    }
                } else {
                    warn!("no dir entry?")
                }
            }
            zugtypen
        }
    }
    impl From<Zugtyp> for value::Zugtyp {
        fn from(
            Zugtyp {
                spurweite,
                umdrehen,
                geraden,
                kurven,
                weichen,
                dreiwege_weichen,
                kurven_weichen,
                s_kurven_weichen,
                kreuzungen,
            }: Zugtyp,
        ) -> Self {
            value::Zugtyp {
                spurweite: Spurweite(spurweite).to_abstand(),
                umdrehen: umdrehen.into(),
                geraden: geraden.into_iter().map(Into::into).collect(),
                kurven: kurven.into_iter().map(Into::into).collect(),
                weichen: weichen.into_iter().map(Into::into).collect(),
                dreiwege_weichen: dreiwege_weichen.into_iter().map(Into::into).collect(),
                kurven_weichen: kurven_weichen.into_iter().map(Into::into).collect(),
                s_kurven_weichen: s_kurven_weichen.into_iter().map(Into::into).collect(),
                kreuzungen: kreuzungen.into_iter().map(Into::into).collect(),
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub enum Duration {
        Seconds(u64),
        Millis(u64),
        Micros(u64),
        Nanos(u64),
    }
    impl From<Duration> for std::time::Duration {
        fn from(duration: Duration) -> Self {
            match duration {
                Duration::Seconds(t) => std::time::Duration::from_secs(t),
                Duration::Millis(t) => std::time::Duration::from_millis(t),
                Duration::Micros(t) => std::time::Duration::from_micros(t),
                Duration::Nanos(t) => std::time::Duration::from_nanos(t),
            }
        }
    }
    #[derive(Debug, Deserialize)]
    pub struct Ueberspannung {
        pub zeit: Duration,
    }
    impl From<Ueberspannung> for value::Ueberspannung {
        fn from(Ueberspannung { zeit }: Ueberspannung) -> Self {
            value::Ueberspannung { zeit: zeit.into() }
        }
    }
    #[derive(Debug, Deserialize)]
    pub enum Umdrehen {
        Ueberspannung(Ueberspannung),
        Schalter(Schalter),
    }
    impl From<Umdrehen> for value::Umdrehen {
        fn from(umdrehen: Umdrehen) -> Self {
            match umdrehen {
                Umdrehen::Ueberspannung(ueberspannung) => {
                    value::Umdrehen::Ueberspannung(ueberspannung.into())
                }
                Umdrehen::Schalter(schalter) => value::Umdrehen::Schalter(schalter),
            }
        }
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
