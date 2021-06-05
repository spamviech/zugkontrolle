//! Zugtyp Trait + Phantom-Typen + Spurweite

use crate::application::gleis::{gerade::Gerade, kreuzung::*, kurve::Kurve, weiche::*};

pub mod lego;
pub use lego::Lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;
pub use märklin::Märklin;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp: Sized {
    /// Spurweite in mm.
    const SPURWEITE: Spurweite;
    const NAME: &'static str;

    /// Art der Stromzufuhr.
    type Leiter;

    fn geraden() -> Vec<Gerade<Self>>;
    fn kurven() -> Vec<Kurve<Self>>;
    fn weichen() -> Vec<WeicheUnit<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeicheUnit<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeicheUnit<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeicheUnit<Self>>;
    fn kreuzungen() -> Vec<KreuzungUnit<Self>>;
}

/*
use serde::{Deserialize, Serialize};

/// TODO
/// Wirkliche Implementierung in eigenem Modul erstellen
/// Pin, I2C/PCF8574Port, weitere?
// Als Inspiration:
// The GPIO war: macro bunkers for typestate explosions
// https://www.ecorax.net/macro-bunker-1/
#[derive(Debug, Serialize, Deserialize)]
pub struct Anschluss;

pub mod geschwindigkeit {
    use non_empty_vec::NonEmpty;

    use super::*;

    /// Pwm-basierte Geschwindigkeitskontrolle (Märklin)
    #[derive(Debug, Serialize, Deserialize)]
    pub struct PwmMärklin(Anschluss);

    /// Fahrtrichtung
    #[derive(Debug)]
    pub enum Fahrtrichtung {
        Vorwärts,
        Rückwärts,
    }
    /// Pwm-basierte Geschwindigkeitskontrolle (Lego)
    #[derive(Debug, Serialize, Deserialize)]
    pub struct PwmLego {
        geschwindigkeit: Anschluss,
        umdrehen: Anschluss,
    }

    /// Geschwindigkeitskontrolle über mehrere Stromquellen mit fester Spannung
    #[derive(Debug, Serialize, Deserialize)]
    pub struct FesteSpannung {
        geschwindigkeit: NonEmpty<Anschluss>,
        umdrehen: Anschluss,
    }

    /// Mögliche Geschwindigkeitswerte
    ///
    /// Pwm: wert/maximaler_wert bestimmt duty_cycle
    /// FesteSpannung: wähle Anschluss wert (als Index), größere Werte verwenden immer den letzten
    /// Anschluss
    #[derive(Debug)]
    pub struct Wert(pub u8);

    #[derive(Debug)]
    pub enum Geschwindigkeit<Pwm> {
        Pwm(Pwm),
        FesteSpannung(FesteSpannung),
    }
    impl<Pwm: std::fmt::Debug> Geschwindigkeit<Pwm> {
        pub fn geschwindigkeit(&mut self, wert: Wert) {
            //TODO
            unimplemented!("{:?}.geschwindigkeit({:?})", self, wert)
        }
    }
    impl Geschwindigkeit<PwmMärklin> {
        pub fn umdrehen(&mut self) {
            //TODO
            unimplemented!("{:?}.umdrehen()", self)
        }
    }
    impl Geschwindigkeit<PwmLego> {
        pub fn fahrtrichtung_einstellen(&mut self, fahrtrichtung: Fahrtrichtung) {
            //TODO
            unimplemented!("{:?}.fahrtrichtung_einstellen({:?})", self, fahrtrichtung)
        }
    }
}

pub mod value {
    use std::time::Duration;

    use super::*;
    use crate::application::typen::*;

    // TODO in Zugtyp hinzufügen
    // einziger wirklicher Unterschied zwischen Märklin/Lego
    // in zwei Datentypen aufteilen, Summentyp behalten?
    // unterschiedliche Methoden: umdrehen(), fahrtrichtung_einstellen(Fahrtrichtung)
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Überspannung {
        pub zeit: Duration,
    }

    #[derive(Debug)]
    pub enum Fahrtrichtung {
        Vorwärts,
        Rückwärts,
    }
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Schalter;
    #[derive(Debug, Serialize, Deserialize)]
    pub enum Umdrehen {
        Überspannung(Überspannung),
        Schalter(Schalter),
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct Zugtyp {
        pub spurweite: Skalar,
        pub umdrehen: Umdrehen,
        pub geraden: Vec<Gerade<()>>,
        pub kurven: Vec<Kurve<()>>,
        pub weichen: Vec<WeicheUnit<()>>,
        pub dreiwege_weichen: Vec<DreiwegeWeiche<()>>,
        pub kurven_weichen: Vec<KurvenWeiche<()>>,
        pub s_kurven_weichen: Vec<SKurvenWeiche<()>>,
        pub kreuzungen: Vec<Kreuzung<()>>,
    }

    // TODO Anstatt generic über <Z: Zugtyp> erhalten alle Zeichnen-Methoden einen &Zugtyp-Parameter
    impl Zugtyp {
        /// Spurweite für den gegebenen Zugtyp
        pub fn spurweite(&self) -> Skalar {
            self.spurweite
        }

        /// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
        pub fn abstand(&self) -> Skalar {
            self.spurweite / Skalar(3.)
        }

        /// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
        pub fn beschränkung(&self) -> Skalar {
            self.spurweite + self.abstand().doppelt()
        }

        /// Äußerster Radius (inklusive Beschränkung) einer Kurve
        pub fn radius_begrenzung_außen(&self, radius: Skalar) -> Skalar {
            radius + self.spurweite.halbiert() + self.abstand()
        }

        /// Innerster Radius (inklusive Beschränkung) einer Kurve
        pub fn radius_begrenzung_innen(&self, radius: Skalar) -> Skalar {
            radius - self.spurweite.halbiert() - self.abstand()
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
    use crate::application::gleis::{gerade, kreuzung, kurve, weiche};
    use crate::application::typen::*;

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
                                },
                                Err(err) => {
                                    warn!(
                                        "failed to parse yaml file \"{}\": {:?}",
                                        path.display(),
                                        err
                                    )
                                },
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
                spurweite: Spurweite(spurweite).als_skalar(),
                umdrehen: umdrehen.into(),
                geraden: geraden.into_iter().map(Into::into).collect(),
                kurven: kurven.into_iter().map(Into::into).collect(),
                weichen: weichen.into_iter().flat_map(Weiche::into).collect(),
                dreiwege_weichen: dreiwege_weichen.into_iter().map(Into::into).collect(),
                kurven_weichen: kurven_weichen.into_iter().map(Into::into).collect(),
                s_kurven_weichen: s_kurven_weichen
                    .into_iter()
                    .flat_map(SKurvenWeiche::into)
                    .collect(),
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
    pub struct Überspannung {
        pub zeit: Duration,
    }
    impl From<Überspannung> for value::Überspannung {
        fn from(Überspannung { zeit }: Überspannung) -> Self {
            value::Überspannung { zeit: zeit.into() }
        }
    }
    #[derive(Debug, Deserialize)]
    pub enum Umdrehen {
        Überspannung(Überspannung),
        Schalter(Schalter),
    }
    impl From<Umdrehen> for value::Umdrehen {
        fn from(umdrehen: Umdrehen) -> Self {
            match umdrehen {
                Umdrehen::Überspannung(überspannung) => {
                    value::Umdrehen::Überspannung(überspannung.into())
                },
                Umdrehen::Schalter(schalter) => value::Umdrehen::Schalter(schalter),
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Gerade {
        pub länge: f32,
        pub beschreibung: Option<String>,
    }
    impl<Z> From<Gerade> for gerade::Gerade<Z> {
        fn from(Gerade { länge, beschreibung }: Gerade) -> Self {
            gerade::Gerade {
                zugtyp: PhantomData,
                länge: Länge::neu(länge).als_skalar(),
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
                radius: Radius::neu(radius).als_skalar(),
                winkel: WinkelGradmaß::neu(winkel).into(),
                beschreibung,
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Weiche {
        pub länge: f32,
        pub radius: f32,
        pub winkel: f32,
        pub richtung: Option<weiche::Orientierung>,
        pub beschreibung: Option<String>,
    }
    impl Weiche {
        fn into<Z>(self) -> Vec<weiche::gerade::WeicheUnit<Z>> {
            let Weiche { länge, radius, winkel, richtung, beschreibung } = self;
            let konstruktor = |richtung| weiche::gerade::WeicheUnit {
                zugtyp: PhantomData,
                länge: Länge::neu(länge).als_skalar(),
                radius: Radius::neu(radius).als_skalar(),
                winkel: WinkelGradmaß::neu(winkel).into(),
                richtung,
                beschreibung: beschreibung.map(|s| {
                    s + match richtung {
                        weiche::Orientierung::Links => "L",
                        weiche::Orientierung::Rechts => "R",
                    }
                }),
                steuerung: (),
            };
            match richtung {
                Some(richtung) => vec![konstruktor(richtung)],
                None => vec![
                    konstruktor.clone()(weiche::Orientierung::Links),
                    konstruktor(weiche::Orientierung::Rechts),
                ],
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct DreiwegeWeiche;
    impl<Z> From<DreiwegeWeiche> for weiche::DreiwegeWeiche<Z> {
        fn from(_: DreiwegeWeiche) -> Self {
            weiche::DreiwegeWeiche::neu(
                Länge::neu(0.),
                Radius::neu(0.),
                WinkelGradmaß::neu(0.).into(),
            )
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct KurvenWeiche;
    impl<Z> From<KurvenWeiche> for weiche::KurvenWeiche<Z> {
        fn from(_: KurvenWeiche) -> Self {
            weiche::KurvenWeiche::neu(
                Länge::neu(0.),
                Radius::neu(0.),
                WinkelGradmaß::neu(0.).into(),
                weiche::Orientierung::Links,
            )
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct SKurvenWeiche {
        pub länge: f32,
        pub radius: f32,
        pub winkel: f32,
        pub radius_reverse: f32,
        pub winkel_reverse: f32,
        pub richtung: Option<weiche::Orientierung>,
        pub beschreibung: Option<String>,
    }
    impl SKurvenWeiche {
        fn into<Z>(self) -> Vec<weiche::SKurvenWeiche<Z>> {
            let SKurvenWeiche {
                länge,
                radius,
                winkel,
                radius_reverse,
                winkel_reverse,
                richtung,
                beschreibung,
            } = self;
            let konstruktor = |richtung| weiche::SKurvenWeiche {
                zugtyp: PhantomData,
                länge: Länge::neu(länge).als_skalar(),
                radius: Radius::neu(radius).als_skalar(),
                winkel: WinkelGradmaß::neu(winkel).into(),
                radius_reverse: Radius::neu(radius_reverse).als_skalar(),
                winkel_reverse: WinkelGradmaß::neu(winkel_reverse).into(),
                richtung,
                beschreibung: beschreibung.map(|s| {
                    s + match richtung {
                        weiche::Orientierung::Links => "L",
                        weiche::Orientierung::Rechts => "R",
                    }
                }),
            };
            match richtung {
                Some(richtung) => vec![konstruktor(richtung)],
                None => vec![
                    konstruktor.clone()(weiche::Orientierung::Links),
                    konstruktor(weiche::Orientierung::Rechts),
                ],
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Kreuzung;
    impl<Z> From<Kreuzung> for kreuzung::Kreuzung<Z> {
        fn from(_: Kreuzung) -> Self {
            kreuzung::Kreuzung::neu(Länge::neu(0.), Radius::neu(0.), kreuzung::Variante::OhneKurve)
        }
    }
}
*/
