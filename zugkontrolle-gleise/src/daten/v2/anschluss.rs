//! Serialisierbare Darstellung von Anschlüssen in Version 2.

use serde::Deserialize;

use zugkontrolle_anschluss::{
    level::Level,
    pcf8574::{self, I2cBus},
    polarität::Polarität,
    trigger::Trigger,
    InputSerialisiert as V4_InputSerialisiert, OutputSerialisiert as V4_OutputSerialisiert,
};
use zugkontrolle_gleis::steuerung::kontakt;
use zugkontrolle_util::eingeschränkt::kleiner_8;

/// Beschreibung eines [`anschluss::pcf85747::Pcf8574`].
#[derive(Deserialize)]
pub(in crate::daten::v2) struct Pcf8574Beschreibung {
    /// Anliegendes [`Level`] an das `A0` Adress-Bit.
    a0: Level,
    /// Anliegendes [`Level`] an das `A1` Adress-Bit.
    a1: Level,
    /// Anliegendes [`Level`] an das `A2` Adress-Bit.
    a2: Level,
    /// Variante des [`anschluss::pcf85747::Pcf8574`], beeinflusst die I2C-Adresse.
    variante: pcf8574::Variante,
}

impl From<Pcf8574Beschreibung> for pcf8574::Beschreibung {
    fn from(Pcf8574Beschreibung { a0, a1, a2, variante }: Pcf8574Beschreibung) -> Self {
        pcf8574::Beschreibung { i2c_bus: I2cBus::I2c0_1, a0, a1, a2, variante }
    }
}

/// Serialisierbare Informationen eines [`OutputAnschluss`]es.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
pub(in crate::daten::v2) enum OutputSerialisiert {
    /// Ein [`Pin`](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
}

impl From<OutputSerialisiert> for zugkontrolle_anschluss::OutputSerialisiert {
    fn from(input: OutputSerialisiert) -> Self {
        match input {
            OutputSerialisiert::Pin { pin, polarität } => {
                V4_OutputSerialisiert::Pin { pin, polarität }
            },
            OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                V4_OutputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    polarität,
                }
            },
        }
    }
}

/// Serialisierbare Informationen eines [`InputAnschlusses`](anschluss::InputAnschluss).
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
pub(in crate::daten::v2) enum InputSerialisiert {
    /// Ein [`Pin`](input::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::InputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Der konfigurierte Interrupt-Pin des Pcf8574.
        interrupt: Option<u8>,
    },
}

impl From<InputSerialisiert> for zugkontrolle_anschluss::InputSerialisiert {
    fn from(input: InputSerialisiert) -> Self {
        match input {
            InputSerialisiert::Pin { pin } => V4_InputSerialisiert::Pin { pin },
            InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt } => {
                V4_InputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    interrupt,
                }
            },
        }
    }
}

/// Serialisierte Variante eines [`Kontaktes`](Kontakt).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct KontaktSerialisiert {
    /// Der Name des Kontaktes.
    name: kontakt::Name,
    /// Der Anschluss des Kontaktes.
    anschluss: InputSerialisiert,
    /// Wann wird der Kontakt ausgelöst.
    trigger: Trigger,
}

impl From<KontaktSerialisiert> for kontakt::KontaktSerialisiert {
    fn from(input: KontaktSerialisiert) -> Self {
        let KontaktSerialisiert { name, anschluss, trigger } = input;
        kontakt::KontaktSerialisiert { name, anschluss: anschluss.into(), trigger }
    }
}
