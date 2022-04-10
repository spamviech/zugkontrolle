//! Traits zum serialisieren und reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, pwm, InputAnschluss, OutputAnschluss};

/// Es existiert einer serialisierbare Repräsentation.
pub trait Serialisiere: Sized {
    /// Die serialisierbare Repräsentation.
    #[allow(single_use_lifetimes)]
    type Serialisiert: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    /// Erstelle eine serialisierbare Repräsentation.
    fn serialisiere(&self) -> Self::Serialisiert;

    /// Erhalte alle Anschlüsse.
    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>);
}

/// Erfolgreiches Ergebnis von [reserviere](Reserviere::reserviere) inklusive aller
/// nicht benötigten [Anschlüsse](crate::anschluss::Anschluss).
#[derive(Debug)]
pub struct Reserviert<R> {
    /// Das Ergebnis.
    pub anschluss: R,
    /// Nicht benötigte Pwm-Pins.
    pub pwm_nicht_benötigt: Vec<pwm::Pin>,
    /// Nicht benötigte Output-Anschlüsse.
    pub output_nicht_benötigt: Vec<OutputAnschluss>,
    /// Nicht benötigte Input-Anschlüsse.
    pub input_nicht_benötigt: Vec<InputAnschluss>,
}

impl<R> Reserviert<R> {
    /// Konvertiere den `anschluss` mit der übergebenen Funktion.
    pub fn konvertiere<T>(self, f: impl FnOnce(R) -> T) -> Reserviert<T> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self;
        Reserviert {
            anschluss: f(anschluss),
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        }
    }
}

/// Fehler der bei [reserviere](Reserviere::reserviere) auftreten kann.
#[derive(Debug)]
pub struct Fehler {
    /// Der aufgetretene Fehler.
    pub fehler: anschluss::Fehler,
    /// Reservierte Pwm-Pins.
    pub pwm_pins: Vec<pwm::Pin>,
    /// Reservierte Output-Anschlüsse.
    pub output_anschlüsse: Vec<OutputAnschluss>,
    /// Reservierte Input-Anschlüsse.
    pub input_anschlüsse: Vec<InputAnschluss>,
}

/// Ergebnis von [reserviere](Reserviere::reserviere).
pub type Result<R> = std::result::Result<Reserviert<R>, Fehler>;

/// Erlaube reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).
pub trait Reserviere<R> {
    /// Reserviere die benötigten [Anschlüsse](crate::anschluss::Anschluss),
    /// potentiell unter Verwendung bereits reservierter Anschlüsse,
    /// um den gewünschten Typ zu erzeugen.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<R>;
}
