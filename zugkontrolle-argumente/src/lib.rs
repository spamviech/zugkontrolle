//! Kommandozeilen-Argumente.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]
// TODO durch derive-Macro für Parse ausgelöst.
#![allow(clippy::shadow_unrelated)]

use std::{
    env,
    fmt::{self, Debug, Display, Formatter},
    num::NonZeroI32,
};

use kommandozeilen_argumente::{EnumArgument, Parse};

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

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
    #[kommandozeilen_argumente(standard: ModusArgument::Bauen, kurz, meta_var: MODUS)]
    pub modus: ModusArgument,

    /// Thema bei Programmstart.
    #[kommandozeilen_argumente(standard: ThemaArgument::Hell, kurz, meta_var: THEMA)]
    pub thema: ThemaArgument,

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
#[allow(clippy::struct_excessive_bools)]
pub struct I2cSettings {
    /// I2C channel auf pins 2 und 3 (bus 0 oder 1).
    #[kommandozeilen_argumente(lang: [i2c0_1, i2c0, i2c1])]
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

impl Argumente {
    /// Parse Kommandozeilen-Argumente.
    /// Ein einzelnes Argument (das nicht mit "-" beginnt) wird als Pfad interpretiert.
    ///
    /// ## Panics
    ///
    /// Programmierfehler, wenn [`NonZeroI32::new`] [`None`] für den Exit-Code zurückgibt.
    #[must_use]
    pub fn parse_aus_env_einzelnes_als_pfad() -> Self {
        let mut args: Vec<_> = env::args_os().skip(1).collect();
        if args.len() == 1
            && !args
                .first()
                .and_then(|os_string| os_string.to_str())
                .is_some_and(|string| string.starts_with('-'))
        {
            // Einzelnes Argument, dass nicht mit '-' beginnt.
            args.insert(0, "--pfad".to_owned().into());
        }
        Argumente::parse_mit_fehlermeldung(args.into_iter(), NonZeroI32::new(1).expect("1 != 0"))
    }
}

/// [`Zugtyp`](crate::zugtyp::Zugtyp) für die aktuelle Session.
#[derive(Debug, Clone, Copy, EnumArgument)]
#[kommandozeilen_argumente(case: insensitive)]
pub enum ZugtypArgument {
    /// [`Märklin`](crate::zugtyp::Zugtyp::märklin)
    Märklin,
    /// [`Lego`](crate::zugtyp::Zugtyp::lego)
    Lego,
}

impl Display for ZugtypArgument {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, formatter)
    }
}

/// Initialer Modus beim Start.
#[derive(Debug, Clone, Copy, EnumArgument)]
#[kommandozeilen_argumente(case: insensitive)]
pub enum ModusArgument {
    /// Beginne im Bauen-Modus.
    Bauen,
    /// Beginne im Fahren-Modus.
    Fahren,
}

impl Display for ModusArgument {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, formatter)
    }
}

/// Initialer Thema beim Start.
#[derive(Debug, Clone, Copy, EnumArgument)]
#[kommandozeilen_argumente(case: insensitive)]
pub enum ThemaArgument {
    /// Beginne im hellen Thema.
    Hell,
    /// Beginne im dunklen Thema.
    Dunkel,
}

impl Display for ThemaArgument {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, formatter)
    }
}
