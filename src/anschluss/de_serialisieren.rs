//! Traits zum serialisieren und reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).

use either::Either;
use log::error;
use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};

use crate::anschluss::{self, pwm, InputAnschluss, OutputAnschluss};

/// Alle [Anschlüsse](anschluss::Anschluss).
#[derive(Debug, Default)]
pub struct Anschlüsse {
    /// Pwm-Pins.
    pub pwm_pins: Vec<pwm::Pin>,
    /// Output-Anschlüsse.
    pub output_anschlüsse: Vec<OutputAnschluss>,
    /// Input-Anschlüsse.
    pub input_anschlüsse: Vec<InputAnschluss>,
}

impl Anschlüsse {
    /// Füge weitere Anschlüsse zu den jeweiligen [Vec] hinzu.
    pub fn anhängen(&mut self, andere: Anschlüsse) {
        let Anschlüsse { pwm_pins, output_anschlüsse, input_anschlüsse } = andere;
        self.pwm_pins.extend(pwm_pins);
        self.output_anschlüsse.extend(output_anschlüsse);
        self.input_anschlüsse.extend(input_anschlüsse);
    }
}

/// Es existiert einer serialisierbare Repräsentation.
///
/// Wenn `R: Serialisiere<S>, S: Reserviere<R>`, dann müssen folgende Gesetze gelten
/// (angenommen `r: R`, `s: S`, `lager: Lager`, `anschlüsse: Anschlüsse`
/// und `arg: <R as Reserviere<S>>::Arg`):
///
/// - Wenn [reserviere](Reserviere::reserviere) erfolgreich war,
/// dann ist [serialisiere](Serialisiere::serialisiere) das inverse davon.
/// ```
/// let clone = s.clone();
/// if let Ergebnis::Wert {anschluss, ..} = s.reserviere(lager, anschlüsse, arg) {
///     assert_eq!(anschluss.serialisiere(), clone)
/// }
/// ```
/// - Sofern kein Klon von `r` existiert ist [reserviere](Reserviere::reserviere) das inverse zu
/// [serialisiere](Serialisiere::serialisiere) durch Zuhilfenahme von `r.anschlüsse()`.
/// ```
/// let s = r.serialisiere();
/// s.reserviere(lager, r.anschlüsse(), arg) == Ergebnis::Wert {anschluss, anschlüsse}
///     && anschluss == r
/// ```
#[allow(single_use_lifetimes)]
pub trait Serialisiere<S: Serialize + for<'de> Deserialize<'de>>: Sized {
    /// Erstelle eine serialisierbare Repräsentation.
    fn serialisiere(&self) -> S;

    /// Erhalte alle Anschlüsse.
    fn anschlüsse(self) -> Anschlüsse;
}

/// Ergebnis von [reserviere](Reserviere::reserviere).
#[derive(Debug)]
pub enum Ergebnis<R> {
    /// Keine Probleme beim Reservieren.
    Wert {
        /// Das Ergebnis.
        anschluss: R,
        /// Nicht verwendete anschlüsse.
        anschlüsse: Anschlüsse,
    },
    /// Es sind Probleme aufgetreten, aber es kann ein Ersatzwert bereitgestellt werden.
    FehlerMitErsatzwert {
        /// Der Ersatzwert
        anschluss: R,
        /// Beim reservieren aufgetretene Fehler.
        fehler: NonEmpty<anschluss::Fehler>,
        /// Nicht verwendete anschlüsse.
        anschlüsse: Anschlüsse,
    },
    /// Es sind Probleme aufgetreten. Es ist nicht möglich einen Ersatzwert zu erzeugen.
    Fehler {
        /// Beim reservieren aufgetretene Fehler.
        fehler: NonEmpty<anschluss::Fehler>,
        /// Nicht verwendete anschlüsse.
        anschlüsse: Anschlüsse,
    },
}

impl<R> Ergebnis<R> {
    /// Konvertiere den `anschluss` mit der übergebenen Funktion.
    pub fn konvertiere<T>(self, f: impl FnOnce(R) -> T) -> Ergebnis<T> {
        use Ergebnis::*;
        match self {
            Wert { anschluss, anschlüsse } => Wert { anschluss: f(anschluss), anschlüsse },
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                FehlerMitErsatzwert { anschluss: f(anschluss), fehler, anschlüsse }
            },
            Fehler { fehler, anschlüsse } => Fehler { fehler, anschlüsse },
        }
    }
}

/// Erlaube reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).
///
/// Wenn `R: Serialisiere<S>, S: Reserviere<R>`, dann müssen folgende Gesetze gelten
/// (angenommen `r: R`, `s: S`, `lager: Lager`, `anschlüsse: Anschlüsse`
/// und `arg: <R as Reserviere<S>>::Arg`):
///
/// - Wenn [reserviere](Reserviere::reserviere) erfolgreich war,
/// dann ist [serialisiere](Serialisiere::serialisiere) das inverse davon.
/// ```
/// let clone = s.clone();
/// if let Ergebnis::Wert {anschluss, ..} = s.reserviere(lager, anschlüsse, arg) {
///     assert_eq!(anschluss.serialisiere(), clone)
/// }
/// ```
/// - Sofern kein Klon von `r` existiert ist [reserviere](Reserviere::reserviere) das inverse zu
/// [serialisiere](Serialisiere::serialisiere) durch Zuhilfenahme von `r.anschlüsse()`.
/// ```
/// let s = r.serialisiere();
/// s.reserviere(lager, r.anschlüsse(), arg) == Ergebnis::Wert {anschluss, anschlüsse}
///     && anschluss == r
/// ```
pub trait Reserviere<R> {
    /// Extra-Argument zum reservieren der Anschlüsse.
    type Arg;

    /// Reserviere die benötigten [Anschlüsse](crate::anschluss::Anschluss),
    /// potentiell unter Verwendung bereits reservierter Anschlüsse,
    /// um den gewünschten Typ zu erzeugen.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<R>;
}

impl<T: Serialisiere<TS>, TS> Ergebnis<T> {
    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs.
    #[inline(always)]
    pub fn reserviere_ebenfalls<S: Reserviere<R>, R>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        arg: <S as Reserviere<R>>::Arg,
    ) -> Ergebnis<(T, R)> {
        self.reserviere_ebenfalls_mit(lager, serialisiert, arg, |t, r| (t, r), |_| None)
    }

    /// Reserviere weitere Anschlüsse, ausgehend von dem Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs und kombiniere beide Ergebnisse mit der
    /// übergebenen Funktion.
    /// Wenn mindestens ein Wert nicht vorhanden ist ([Ergebnis::Fehler]) wird stattdessen
    /// mit `fehlerbehandlung` versucht ein Ersatzergebnis zu erzeugen.
    pub fn reserviere_ebenfalls_mit<S: Reserviere<R>, R, U>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        arg: <S as Reserviere<R>>::Arg,
        kombiniere: impl FnOnce(T, R) -> U,
        fehlerbehandlung: impl FnOnce(Either<Option<T>, R>) -> Option<U>,
    ) -> Ergebnis<U> {
        use Ergebnis::*;
        let (t, fehler_t, anschlüsse) = match self {
            Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                (Some(anschluss), Some(fehler), anschlüsse)
            },
            Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
        };
        let (r, fehler_r, anschlüsse) = match serialisiert.reserviere(lager, anschlüsse, arg) {
            Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                (Some(anschluss), Some(fehler), anschlüsse)
            },
            Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
        };
        let kombiniert = match (t, r) {
            (Some(t), Some(r)) => Some(kombiniere(t, r)),
            (None, Some(r)) => fehlerbehandlung(Either::Right(r)),
            (t, None) => fehlerbehandlung(Either::Left(t)),
        };
        let fehler_kombiniert = if let Some(mut fehler_t) = fehler_t {
            if let Some(fehler_r) = fehler_r {
                fehler_t.extend(fehler_r);
            }
            Some(fehler_t)
        } else {
            fehler_r
        };
        match (kombiniert, fehler_kombiniert) {
            (None, None) => unreachable!("Wert und Fehler können nicht gleichzeitig None sein!"),
            (None, Some(fehler)) => Fehler { fehler, anschlüsse },
            (Some(anschluss), None) => Wert { anschluss, anschlüsse },
            (Some(anschluss), Some(fehler)) => {
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse }
            },
        }
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere<Option<S>> for Option<R>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> Option<S> {
        self.as_ref().map(Serialisiere::serialisiere)
    }

    fn anschlüsse(self) -> Anschlüsse {
        if let Some(r) = self {
            r.anschlüsse()
        } else {
            Anschlüsse::default()
        }
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<Option<R>> for Option<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<S>,
{
    type Arg = <S as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<Option<R>> {
        use Ergebnis::*;
        if let Some(s) = self {
            match s.reserviere(lager, anschlüsse, arg) {
                Wert { anschluss, anschlüsse } => Wert { anschluss: Some(anschluss), anschlüsse },
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    FehlerMitErsatzwert { anschluss: Some(anschluss), fehler, anschlüsse }
                },
                Fehler { fehler, anschlüsse } => {
                    FehlerMitErsatzwert { anschluss: None, fehler, anschlüsse }
                },
            }
        } else {
            Wert { anschluss: None, anschlüsse }
        }
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere<Vec<S>> for Vec<R>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    <S as Reserviere<R>>::Arg: Clone,
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> S {
        self.iter().map(Serialisiere::serialisiere).collect()
    }

    fn anschlüsse(self) -> Anschlüsse {
        let mut anschlüsse = Anschlüsse::default();
        for r in self {
            anschlüsse.anhängen(r.anschlüsse());
        }
        anschlüsse
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<Vec<R>> for Vec<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere<S>,
    <S as Reserviere<R>>::Arg: Clone,
{
    type Arg = <S as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<Vec<R>> {
        let len = self.len();
        self.into_iter().fold(
            Ergebnis::Wert { anschluss: Vec::with_capacity(len), anschlüsse },
            |acc, serialisiert| {
                acc.reserviere_ebenfalls_mit(
                    lager,
                    serialisiert,
                    arg.clone(),
                    |mut vec, r| {
                        vec.push(r);
                        vec
                    },
                    |either| match either {
                        Either::Left(vec) => vec,
                        Either::Right(anschluss) => {
                            error!("Leerer Akkumulator in Reserviere-Implementierung von Vec<T>!");
                            Some(vec![anschluss])
                        },
                    },
                )
            },
        )
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Serialisiere<NonEmpty<S>> for NonEmpty<R>
where
    S: Clone + Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    <S as Reserviere<R>>::Arg: Clone,
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> S {
        let head = self.head.serialisiere();
        let tail = self.tail.serialisiere();
        NonEmpty { head, tail }
    }

    fn anschlüsse(self) -> Anschlüsse {
        let mut anschlüsse = Anschlüsse::default();
        for r in self {
            anschlüsse.anhängen(r.anschlüsse());
        }
        anschlüsse
    }
}

#[allow(single_use_lifetimes)]
impl<S, R> Reserviere<NonEmpty<R>> for NonEmpty<S>
where
    S: Reserviere<R> + Serialize + for<'de> Deserialize<'de>,
    <S as Reserviere<R>>::Arg: Clone,
    R: Serialisiere<S>,
{
    type Arg = <S as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<NonEmpty<R>> {
        let head = self.head.reserviere(lager, anschlüsse, arg.clone());
        head.reserviere_ebenfalls_mit(
            lager,
            self.tail,
            arg,
            |head, tail| NonEmpty { head, tail },
            |either| match either {
                Either::Left(None) => None,
                Either::Left(Some(head)) => Some(NonEmpty::singleton(head)),
                Either::Right(tail) => NonEmpty::from_vec(tail),
            },
        )
    }
}

macro_rules! impl_serialisiere_tuple {
    ($($name: ident - $arg_name: ident : $type: ident - $serialisiert: ident),+) => {
        #[allow(single_use_lifetimes)]
        impl<A0, S0, $($type, $serialisiert),+> Serialisiere<(S0, $($serialisiert),+)> for (A0, $($type),+)
        where
            A0: Serialisiere<S0>,
            S0: Reserviere<A0> + Serialize + for<'de> Deserialize<'de>,
            $(
                $type: Serialisiere<$serialisiert>,
                $serialisiert: Reserviere<$type> + Serialize + for<'de> Deserialize<'de>,
            )+
        {
            fn serialisiere(&self) -> (S0, $($serialisiert),+) {
                let (a0, $($name),+) = self;
                let s0 = a0.serialisiere();
                $(
                    let $name = $name.serialisiere();
                )+
                (s0, $($name),+)
            }
            fn anschlüsse(self) -> Anschlüsse {
                let (a0, $($name),+) = self;
                let mut acc = a0.anschlüsse();
                $(
                    acc.anhängen($name.anschlüsse());
                )+
                acc
            }
        }
        impl<A0, S0, $($type, $serialisiert),+> Reserviere<(A0, $($type),+)> for (S0, $($serialisiert),+)
        where
            A0: Serialisiere<S0>,
            S0: Reserviere<A0>,
            $(
                $type: Serialisiere<$serialisiert>,
                $serialisiert: Reserviere<$type>,
            )+
        {
            #[allow(unused_parens)]
            type Arg = (<S0 as Reserviere<A0>>::Arg, $(<$serialisiert as Reserviere<$type>>::Arg),+);
            fn reserviere(
                self,
                lager: &mut anschluss::Lager,
                anschlüsse: Anschlüsse,
                arg: Self::Arg,
            ) -> Ergebnis<(A0, $($type),+)> {
                let (a0, $($name),+) = self;
                let (arg_0, $($arg_name),+) = arg;
                let reserviert = a0.reserviere(lager, anschlüsse, arg_0);
                reserviert.reserviere_ebenfalls_mit(
                    lager,
                    ($($name),+),
                    ($($arg_name),+),
                    #[allow(unused_parens)]
                    |a0, ($($name),+)| (a0, $($name),+),
                    |_| None,
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
