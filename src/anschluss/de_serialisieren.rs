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
    pub pwm_pins: Vec<pwm::Pin>,
    /// Nicht benötigte Output-Anschlüsse.
    pub output_anschlüsse: Vec<OutputAnschluss>,
    /// Nicht benötigte Input-Anschlüsse.
    pub input_anschlüsse: Vec<InputAnschluss>,
}

impl<R> Reserviert<R> {
    /// Konvertiere den `anschluss` mit der übergebenen Funktion.
    pub fn konvertiere<T>(self, f: impl FnOnce(R) -> T) -> Reserviert<T> {
        let Reserviert { anschluss, pwm_pins, output_anschlüsse, input_anschlüsse } = self;
        Reserviert { anschluss: f(anschluss), pwm_pins, output_anschlüsse, input_anschlüsse }
    }
}

/// Fehler der bei [reserviere](Reserviere::reserviere) auftreten kann.
#[derive(Debug)]
pub struct Fehler {
    /// Der aufgetretene Fehler.
    pub fehler: Vec<anschluss::Fehler>,
    /// Reservierte Pwm-Pins.
    pub pwm_pins: Vec<pwm::Pin>,
    /// Reservierte Output-Anschlüsse.
    pub output_anschlüsse: Vec<OutputAnschluss>,
    /// Reservierte Input-Anschlüsse.
    pub input_anschlüsse: Vec<InputAnschluss>,
}

/// Ergebnis von [reserviere](Reserviere::reserviere).
pub struct Ergebnis<R> {
    /// Das Ergebnis.
    pub anschluss: Option<R>,
    /// Beim reservieren aufgetretene Fehler.
    pub fehler: Option<NonEmpty<anschluss::Fehler>>,
    /// Nicht verwendete Pwm-Pins.
    pub pwm_pins: Vec<pwm::Pin>,
    /// Nicht verwendete Output-Anschlüsse.
    pub output_anschlüsse: Vec<OutputAnschluss>,
    /// Nicht verwendete Input-Anschlüsse.
    pub input_anschlüsse: Vec<InputAnschluss>,
}

impl<R> Ergebnis<R> {
    /// Konvertiere den `anschluss` mit der übergebenen Funktion.
    pub fn konvertiere<T>(self, f: impl FnOnce(R) -> T) -> Ergebnis<T> {
        let Ergebnis { anschluss, fehler, pwm_pins, output_anschlüsse, input_anschlüsse } = self;
        Ergebnis {
            anschluss: anschluss.map(f),
            fehler,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
    }
}

impl<T: Serialisiere> Ergebnis<T> {
    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs.
    #[inline(always)]
    pub fn reserviere_ebenfalls<S: Reserviere<R>, R>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        arg: <S as Reserviere<R>>::Arg,
    ) -> Ergebnis<(T, R)> {
        self.reserviere_ebenfalls_mit(lager, serialisiert, arg, |t, r| (t, r))
    }

    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs und kombiniere beide Ergebnisse mit der
    /// übergebenen Funktion.
    pub fn reserviere_ebenfalls_mit<S: Reserviere<R>, R, U>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        arg: <S as Reserviere<R>>::Arg,
        kombiniere: impl FnOnce(T, R) -> U,
    ) -> Ergebnis<U> {
        let Ergebnis {
            anschluss: t,
            fehler: fehler_t,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        } = self;
        let Ergebnis {
            anschluss: r,
            fehler: fehler_r,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        } = serialisiert.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg);
        let kombiniert = t.and_then(|t| r.map(|r| kombiniere(t, r)));
        let fehler_kombiniert = if let Some(mut fehler_t) = fehler_t {
            if let Some(fehler_r) = fehler_r {
                fehler_t.tail.extend(fehler_r);
            }
            Some(fehler_t)
        } else {
            fehler_r
        };
        Ergebnis {
            anschluss: kombiniert,
            fehler: fehler_kombiniert,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
    }
}

/// Erlaube reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).
pub trait Reserviere<R> {
    /// Extra-Argument zum reservieren der Anschlüsse.
    type Arg;

    /// Reserviere die benötigten [Anschlüsse](crate::anschluss::Anschluss),
    /// potentiell unter Verwendung bereits reservierter Anschlüsse,
    /// um den gewünschten Typ zu erzeugen.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        arg: Self::Arg,
    ) -> Ergebnis<R>;
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere for Option<R>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    type Serialisiert = Option<S>;

    fn serialisiere(&self) -> Self::Serialisiert {
        self.as_ref().map(Serialisiere::serialisiere)
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut acc = (Vec::new(), Vec::new(), Vec::new());
        if let Some(r) = self {
            let (pwm_pins, output_anschlüsse, input_anschlüsse) = r.anschlüsse();
            acc.0.extend(pwm_pins);
            acc.1.extend(output_anschlüsse);
            acc.2.extend(input_anschlüsse);
        }
        acc
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<Option<R>> for Option<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<Serialisiert = S>,
{
    type Arg = <S as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        arg: Self::Arg,
    ) -> Ergebnis<Option<R>> {
        if let Some(s) = self {
            s.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg)
                .konvertiere(Some)
        } else {
            Ergebnis {
                anschluss: None,
                fehler: None,
                pwm_pins,
                output_anschlüsse,
                input_anschlüsse,
            }
        }
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere for Vec<R>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    <S as Reserviere<R>>::Arg: Clone,
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
    <S as Reserviere<R>>::Arg: Clone,
{
    type Arg = <S as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        arg: Self::Arg,
    ) -> Ergebnis<Vec<R>> {
        self.into_iter().fold(
            Ergebnis {
                anschluss: Some(Vec::new()),
                fehler: None,
                pwm_pins,
                output_anschlüsse,
                input_anschlüsse,
            },
            |acc, serialisiert| {
                acc.reserviere_ebenfalls_mit(lager, serialisiert, arg.clone(), |mut vec, r| {
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
    <S as Reserviere<R>>::Arg: Clone,
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
    <S as Reserviere<R>>::Arg: Clone,
    R: Serialisiere<Serialisiert = S>,
{
    type Arg = <S as Reserviere<R>>::Arg;
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        arg: Self::Arg,
    ) -> Ergebnis<NonEmpty<R>> {
        let head =
            self.head.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg.clone());
        head.reserviere_ebenfalls_mit(lager, self.tail, arg, |head, tail| NonEmpty { head, tail })
    }
}

macro_rules! impl_serialisiere_tuple {
    ($($name: ident - $arg_name: ident : $type: ident - $serialisiert: ident),+) => {
        #[allow(single_use_lifetimes)]
        impl<A0, S0, $($type, $serialisiert),+> Serialisiere for (A0, $($type),+)
        where
            A0: Serialisiere<Serialisiert=S0>,
            S0: Reserviere<A0> + Serialize + for<'de> Deserialize<'de>,
            $(
                $type: Serialisiere<Serialisiert=$serialisiert>,
                $serialisiert: Reserviere<$type> + Serialize + for<'de> Deserialize<'de>,
            )+
        {
            type Serialisiert = (S0, $($serialisiert),+);
            fn serialisiere(&self) -> Self::Serialisiert {
                let (a0, $($name),+) = self;
                let s0 = a0.serialisiere();
                $(
                    let $name = $name.serialisiere();
                )+
                (s0, $($name),+)
            }
            fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
                let (a0, $($name),+) = self;
                let mut acc = a0.anschlüsse();
                $(
                    let (pwm_pins, output_anschlüsse, input_anschlüsse) = $name.anschlüsse();
                    acc.0.extend(pwm_pins);
                    acc.1.extend(output_anschlüsse);
                    acc.2.extend(input_anschlüsse);
                )+
                acc
            }
        }
        impl<A0, S0, $($type, $serialisiert),+> Reserviere<(A0, $($type),+)> for (S0, $($serialisiert),+)
        where
            A0: Serialisiere,
            S0: Reserviere<A0>,
            $(
                $type: Serialisiere,
                $serialisiert: Reserviere<$type>,
            )+
        {
            #[allow(unused_parens)]
            type Arg = (<S0 as Reserviere<A0>>::Arg, $(<$serialisiert as Reserviere<$type>>::Arg),+);
            fn reserviere(
                self,
                lager: &mut anschluss::Lager,
                pwm_pins: Vec<pwm::Pin>,
                output_anschlüsse: Vec<OutputAnschluss>,
                input_anschlüsse: Vec<InputAnschluss>,
                arg: Self::Arg,
            ) -> Ergebnis<(A0, $($type),+)> {
                let (a0, $($name),+) = self;
                let (arg_0, $($arg_name),+) = arg;
                let reserviert
                    = a0.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg_0);
                reserviert.reserviere_ebenfalls_mit(
                    lager,
                    ($($name),+),
                    ($($arg_name),+),
                    #[allow(unused_parens)]
                    |a0, ($($name),+)| (a0, $($name),+)
                )
            }
        }
    };
}

impl_serialisiere_tuple! {a-aa: A-SA}
impl_serialisiere_tuple! {a-aa: A-SA, b-bb: B-SB}
impl_serialisiere_tuple! {a-aa: A-SA, b-bb: B-SB, c-cc: C-SC}
impl_serialisiere_tuple! {a-aa: A-SA, b-bb: B-SB, c-cc: C-SC, d-dd: D-SD}
impl_serialisiere_tuple! {a-aa: A-SA, b-bb: B-SB, c-cc: C-SC, d-dd: D-SD, e-ee: E-SE}
impl_serialisiere_tuple! {a-aa: A-SA, b-bb: B-SB, c-cc: C-SC, d-dd: D-SD, e-ee: E-SE, f-ff: F-SF}
