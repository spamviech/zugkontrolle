//! Kommandozeilen-Argumente.

use std::{
    env,
    fmt::{Debug, Display},
    num::NonZeroI32,
};

use kommandozeilen_argumente::{Argumente, Beschreibung, Parse, ParseArgument};

use crate::application::{
    gleis::gleise::Modus,
    typen::{skalar::Skalar, winkel::Winkel},
};

pub use kommandozeilen_argumente::ArgEnum;

#[derive(Debug, Clone, Parse)]
// subcommand umd direkte Verwendung (impl TopLevelCommand) von `argh::from_env` zu verhindern.
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
#[kommandozeilen_argumente(deutsch, version, hilfe(hilfe, help; h))]
pub struct Args {
    /// Verwendeter Zugtyp
    #[kommandozeilen_argumente(standard: Zugtyp::Märklin, kurz)]
    pub zugtyp: Zugtyp,

    /// Lade bei Programmstart die angegebene Datei
    #[kommandozeilen_argumente(kurz)]
    pub pfad: Option<String>,

    /// Modus bei Programmstart
    #[kommandozeilen_argumente(standard: Modus::Bauen, kurz)]
    pub modus: Modus,

    /// Zoom bei Programmstart
    #[kommandozeilen_argumente(standard: Skalar(1.))]
    pub zoom: Skalar,

    /// X-Position bei Programmstart
    #[kommandozeilen_argumente(standard: Skalar(0.), kurz)]
    pub x: Skalar,

    /// Y-Position bei Programmstart
    #[kommandozeilen_argumente(standard: Skalar(0.), kurz)]
    pub y: Skalar,

    /// Winkel bei Programmstart
    #[kommandozeilen_argumente(standard: Winkel(0.))]
    pub winkel: Winkel,

    #[kommandozeilen_argumente(glätten)]
    pub i2c_settings: I2cSettings,

    /// Zeige zusätzliche Informationen in der Konsole an
    pub verbose: bool,

    /// Speichere Log-Nachrichten zusätzlich in einer Datei
    #[kommandozeilen_argumente(kurz: l)]
    pub erstelle_log_datei: bool,
}

/// Einstellung über aktivierte I2c-Channel
#[derive(Debug, Clone, Copy, Parse)]
#[kommandozeilen_argumente(deutsch)]
pub struct I2cSettings {
    /// I2C channel auf pins 2 und 3 (bus 0 oder 1)
    #[kommandozeilen_argumente(standard: true)]
    pub i2c0_1: bool,
    // /// I2C channel auf pins 2? und ? (bus 2)
    // pub i2c2: bool,
    /// I2C channel auf pins 4 und 5 (bus 3)
    pub i2c3: bool,
    /// I2C channel auf pins 8 und 9 (bus 4)
    pub i2c4: bool,
    /// I2C channel auf pins 12 und 13 (bus 5)
    pub i2c5: bool,
    /// I2C channel auf pins 22 und 23 (bus 6)
    pub i2c6: bool,
}

impl ParseArgument for Winkel {
    fn argumente(
        beschreibung: Beschreibung<Self>,
        invertiere_prefix: &'static str,
        meta_var: &str,
    ) -> Argumente<Self, String> {
        Argumente::konvertiere(
            Winkel,
            f32::argumente(
                beschreibung.konvertiere(|winkel| winkel.0),
                invertiere_prefix,
                meta_var,
            ),
        )
    }

    fn standard() -> Option<Self> {
        Some(Winkel(0.))
    }
}

impl ParseArgument for Skalar {
    fn argumente(
        beschreibung: Beschreibung<Self>,
        invertiere_prefix: &'static str,
        meta_var: &str,
    ) -> Argumente<Self, String> {
        Argumente::konvertiere(
            Skalar,
            f32::argumente(
                beschreibung.konvertiere(|winkel| winkel.0),
                invertiere_prefix,
                meta_var,
            ),
        )
    }

    fn standard() -> Option<Self> {
        None
    }
}

impl Args {
    /// Parse Kommandozeilen-Argumente.
    /// Ein einzelnes Argument (das nicht mit "-" beginnt) wird als Pfad interpretiert.
    pub fn parse_aus_env() -> Self {
        let mut args: Vec<_> = env::args_os().skip(1).collect();
        if args.len() == 1 {
            if !args
                .first()
                .and_then(|os_string| os_string.to_str())
                .map(|string| string.starts_with('-'))
                .unwrap_or(false)
            {
                // Einzelnes Argument, dass nicht mit '-' beginnt.
                args.insert(0, "--pfad".to_owned().into());
            }
        }
        Args::parse_mit_fehlermeldung(args.into_iter(), NonZeroI32::new(1).expect("1 != 0"))
    }
}

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum Zugtyp {
    Märklin,
    Lego,
}

impl Display for Zugtyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
