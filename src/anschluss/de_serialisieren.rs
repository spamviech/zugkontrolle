//! Traits zum serialisieren und reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).

use nonempty::NonEmpty;
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

impl<T: Serialisiere> Reserviert<T> {
    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs.
    #[inline(always)]
    pub fn reserviere_ebenfalls<S: Reserviere<R>, R>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
    ) -> Result<(T, R)> {
        self.reserviere_ebenfalls_mit(lager, serialisiert, |t, r| (t, r))
    }

    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs und kombiniere beide Ergebnisse mit der
    /// übergebenen Funktion.
    pub fn reserviere_ebenfalls_mit<S: Reserviere<R>, R, U>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        kombiniere: impl FnOnce(T, R) -> U,
    ) -> Result<U> {
        let Reserviert {
            anschluss: t,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self;
        let reserviert = match serialisiert.reserviere(
            lager,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        ) {
            Ok(reserviert) => reserviert,
            Err(mut fehler) => {
                let (pwm_pins, output_anschlüsse, input_anschlüsse) = t.anschlüsse();
                fehler.pwm_pins.extend(pwm_pins);
                fehler.output_anschlüsse.extend(output_anschlüsse);
                fehler.input_anschlüsse.extend(input_anschlüsse);
                return Err(fehler);
            },
        };
        Ok(reserviert.konvertiere(|r| kombiniere(t, r)))
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere for Vec<R>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    type Serialisiert = Vec<S>;

    fn serialisiere(&self) -> Self::Serialisiert {
        self.iter().map(Serialisiere::serialisiere).collect()
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.into_iter().fold((Vec::new(), Vec::new(), Vec::new()), |mut acc, r| {
            let (pwm_pins, output_anschlüsse, input_anschlüsse) = r.anschlüsse();
            acc.0.extend(pwm_pins);
            acc.1.extend(output_anschlüsse);
            acc.2.extend(input_anschlüsse);
            acc
        })
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<Vec<R>> for Vec<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_nicht_benötigt: Vec<pwm::Pin>,
        output_nicht_benötigt: Vec<OutputAnschluss>,
        input_nicht_benötigt: Vec<InputAnschluss>,
    ) -> Result<Vec<R>> {
        self.into_iter().fold(
            Ok(Reserviert {
                anschluss: Vec::new(),
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            }),
            |acc, serialisiert| {
                let reserviert = acc?;
                reserviert.reserviere_ebenfalls_mit(lager, serialisiert, |mut vec, r| {
                    vec.push(r);
                    vec
                })
            },
        )
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere for NonEmpty<R>
where
    S: Clone + Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    type Serialisiert = NonEmpty<S>;

    fn serialisiere(&self) -> Self::Serialisiert {
        let head = self.head.serialisiere();
        let tail = self.tail.serialisiere();
        NonEmpty { head, tail }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.into_iter().fold((Vec::new(), Vec::new(), Vec::new()), |mut acc, r| {
            let (pwm_pins, output_anschlüsse, input_anschlüsse) = r.anschlüsse();
            acc.0.extend(pwm_pins);
            acc.1.extend(output_anschlüsse);
            acc.2.extend(input_anschlüsse);
            acc
        })
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<NonEmpty<R>> for NonEmpty<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<NonEmpty<R>> {
        let head = self.head.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        head.reserviere_ebenfalls_mit(lager, self.tail, |head, tail| NonEmpty { head, tail })
    }
}
