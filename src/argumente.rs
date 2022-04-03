//! Kommandozeilen-Argumente.

use std::{
    env,
    fmt::{Debug, Display},
    num::NonZeroI32,
};

use kommandozeilen_argumente::{Beschreibung, EnumArgument, Parse, ParseArgument, Vergleich};

use crate::{
    gleis::gleise::Modus,
    typen::{skalar::Skalar, winkel::Winkel},
};

#[derive(Debug, Clone, Parse)]
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
#[kommandozeilen_argumente(sprache: deutsch, version, hilfe(lang: [hilfe, help], kurz: h))]
pub struct Argumente {
    /// Verwendeter Zugtyp.
    #[kommandozeilen_argumente(standard: ZugtypArgument::Märklin, kurz, meta_var: ZUGTYP)]
    pub zugtyp: ZugtypArgument,

    /// Lade bei Programmstart die angegebene Datei.
    #[kommandozeilen_argumente(kurz, meta_var: DATEI)]
    pub pfad: Option<String>,

    /// Modus bei Programmstart.
    #[kommandozeilen_argumente(standard: Modus::Bauen, kurz, meta_var: MODUS)]
    pub modus: Modus,

    /// Zoom bei Programmstart.
    #[kommandozeilen_argumente(standard: Skalar(1.), meta_var: ZOOM)]
    pub zoom: Skalar,

    /// X-Position bei Programmstart.
    #[kommandozeilen_argumente(standard: Skalar(0.), kurz, meta_var: X)]
    pub x: Skalar,

    /// Y-Position bei Programmstart.
    #[kommandozeilen_argumente(standard: Skalar(0.), kurz, meta_var: Y)]
    pub y: Skalar,

    /// Winkel bei Programmstart.
    #[kommandozeilen_argumente(standard: Winkel(0.), meta_var: WINKEL)]
    pub winkel: Winkel,

    /// I2CSettings für die Programmdauer.
    #[kommandozeilen_argumente(glätten)]
    pub i2c_settings: I2cSettings,

    /// Zeige zusätzliche Informationen in der Konsole an.
    pub verbose: bool,

    /// Speichere Log-Nachrichten zusätzlich in einer Datei.
    #[kommandozeilen_argumente(kurz: l, invertiere_präfix: keine)]
    pub log_datei: bool,
}

/// Einstellung über aktivierte I2c-Channel.
#[derive(Debug, Clone, Copy, Parse)]
#[kommandozeilen_argumente(sprache: deutsch)]
pub struct I2cSettings {
    /// I2C channel auf pins 2 und 3 (bus 0 oder 1).
    #[kommandozeilen_argumente(standard: true, lang: [i2c0_1, i2c0, i2c1])]
    pub i2c0_1: bool,
    // /// I2C channel auf pins ? und ? (bus 2).
    // pub i2c2: bool,
    /// I2C channel auf pins 4 und 5 (bus 3).
    pub i2c3: bool,
    /// I2C channel auf pins 8 und 9 (bus 4).
    pub i2c4: bool,
    /// I2C channel auf pins 12 und 13 (bus 5).
    pub i2c5: bool,
    /// I2C channel auf pins 22 und 23 (bus 6).
    pub i2c6: bool,
}

impl ParseArgument for Winkel {
    fn argumente<'t>(
        beschreibung: Beschreibung<'t, Self>,
        invertiere_präfix: impl Into<Vergleich<'t>>,
        invertiere_infix: impl Into<Vergleich<'t>>,
        wert_infix: impl Into<Vergleich<'t>>,
        meta_var: &'t str,
    ) -> kommandozeilen_argumente::Argumente<'t, Self, String> {
        kommandozeilen_argumente::Argumente::konvertiere(
            Winkel,
            f32::argumente(
                beschreibung.konvertiere(|winkel| winkel.0),
                invertiere_präfix,
                invertiere_infix,
                wert_infix,
                meta_var,
            ),
        )
    }

    fn standard() -> Option<Self> {
        Some(Winkel(0.))
    }
}

impl ParseArgument for Skalar {
    fn argumente<'t>(
        beschreibung: Beschreibung<'t, Self>,
        invertiere_präfix: impl Into<Vergleich<'t>>,
        invertiere_infix: impl Into<Vergleich<'t>>,
        wert_infix: impl Into<Vergleich<'t>>,
        meta_var: &'t str,
    ) -> kommandozeilen_argumente::Argumente<'t, Self, String> {
        kommandozeilen_argumente::Argumente::konvertiere(
            Skalar,
            f32::argumente(
                beschreibung.konvertiere(|winkel| winkel.0),
                invertiere_präfix,
                invertiere_infix,
                wert_infix,
                meta_var,
            ),
        )
    }

    fn standard() -> Option<Self> {
        None
    }
}

impl Argumente {
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
        Argumente::parse_mit_fehlermeldung(args.into_iter(), NonZeroI32::new(1).expect("1 != 0"))
    }
}

// TODO erlaube Laden/Deserialisieren aus einer Datei.
#[derive(Debug, Clone, Copy, EnumArgument)]
#[kommandozeilen_argumente(case: insensitive)]
pub enum ZugtypArgument {
    Märklin,
    Lego,
}

impl Display for ZugtypArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
