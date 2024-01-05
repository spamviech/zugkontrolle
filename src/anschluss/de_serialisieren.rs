//! Traits zum serialisieren und reservieren der benötigten [Anschlüsse](crate::anschluss::Anschluss).

use either::Either;
use log::error;
use nonempty::NonEmpty;

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
    /// Erzeuge eine leere [Anschlüsse]-Struktur.
    #[must_use]
    pub const fn neu() -> Anschlüsse {
        Anschlüsse {
            pwm_pins: Vec::new(),
            output_anschlüsse: Vec::new(),
            input_anschlüsse: Vec::new(),
        }
    }

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

pub trait Serialisiere<S>: Sized {
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
    pub fn konvertiere<T>(self, formatter: impl FnOnce(R) -> T) -> Ergebnis<T> {
        use Ergebnis::{Fehler, FehlerMitErsatzwert, Wert};
        match self {
            Wert { anschluss, anschlüsse } => {
                Wert { anschluss: formatter(anschluss), anschlüsse }
            },
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                FehlerMitErsatzwert { anschluss: formatter(anschluss), fehler, anschlüsse }
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
    /// Extra Move-Argument zum reservieren der Anschlüsse.
    type MoveArg;
    /// Extra Referenz-Argument zum reservieren der Anschlüsse.
    type RefArg;
    /// Extra veränderliche-Referenz-Argument zum reservieren der Anschlüsse.
    type MutRefArg;

    /// Reserviere die benötigten [Anschlüsse](crate::anschluss::Anschluss),
    /// potentiell unter Verwendung bereits reservierter Anschlüsse,
    /// um den gewünschten Typ zu erzeugen.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<R>;
}

impl<T> Ergebnis<T> {
    /// Reserviere weitere Anschlüsse, ausgehend von dem positiven Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs.
    pub fn reserviere_ebenfalls<S: Reserviere<R>, R>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        move_arg: <S as Reserviere<R>>::MoveArg,
        ref_arg: &<S as Reserviere<R>>::RefArg,
        mut_ref_arg: &mut <S as Reserviere<R>>::MutRefArg,
    ) -> Ergebnis<(T, R)> {
        self.reserviere_ebenfalls_mit(
            lager,
            serialisiert,
            move_arg,
            ref_arg,
            mut_ref_arg,
            // t: T, r:R; mehr Information ist nicht vorhanden und auch nicht notwendig
            #[allow(clippy::min_ident_chars)]
            |t, r| (t, r),
            |_| None,
        )
    }

    // Argumente werden alle benötigt und können nicht sinnvoll zusammengefasst werden.
    #[allow(clippy::too_many_arguments)]
    /// Reserviere weitere Anschlüsse, ausgehend von dem Ergebnis eines vorherigen
    /// [reserviere](Reserviere::reserviere)-Aufrufs und kombiniere beide Ergebnisse mit der
    /// übergebenen Funktion.
    /// Wenn mindestens ein Wert nicht vorhanden ist ([`Ergebnis::Fehler`]) wird stattdessen
    /// mit `fehlerbehandlung` versucht ein Ersatzergebnis zu erzeugen.
    pub fn reserviere_ebenfalls_mit<S: Reserviere<R>, R, U>(
        self,
        lager: &mut anschluss::Lager,
        serialisiert: S,
        move_arg: <S as Reserviere<R>>::MoveArg,
        ref_arg: &<S as Reserviere<R>>::RefArg,
        mut_ref_arg: &mut <S as Reserviere<R>>::MutRefArg,
        kombiniere: impl FnOnce(T, R) -> U,
        fehlerbehandlung: impl FnOnce(Either<Option<T>, R>) -> Option<U>,
    ) -> Ergebnis<U> {
        use Ergebnis::{Fehler, FehlerMitErsatzwert, Wert};
        // t: T
        #[allow(clippy::min_ident_chars)]
        let (t, fehler_t, anschlüsse) = match self {
            Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                (Some(anschluss), Some(fehler), anschlüsse)
            },
            Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
        };
        // r: R
        #[allow(clippy::min_ident_chars)]
        let (r, fehler_r, anschlüsse) =
            match serialisiert.reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg) {
                // false positive, anschlüsse transformiert durch den Funktionsaufruf
                #[allow(clippy::shadow_unrelated)]
                Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
                #[allow(clippy::shadow_unrelated)]
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    (Some(anschluss), Some(fehler), anschlüsse)
                },
                #[allow(clippy::shadow_unrelated)]
                Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
            };
        // t:T, r: R
        #[allow(clippy::min_ident_chars)]
        let anschluss_kombiniert = match (t, r) {
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
        match (anschluss_kombiniert, fehler_kombiniert) {
            (None, None) => unreachable!("Wert und Fehler können nicht gleichzeitig None sein!"),
            (None, Some(fehler)) => Fehler { fehler, anschlüsse },
            (Some(anschluss), None) => Wert { anschluss, anschlüsse },
            (Some(anschluss), Some(fehler)) => {
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse }
            },
        }
    }
}

impl<S, R> Serialisiere<Option<S>> for Option<R>
where
    S: Reserviere<R>,
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> Option<S> {
        self.as_ref().map(Serialisiere::serialisiere)
    }

    fn anschlüsse(self) -> Anschlüsse {
        if let Some(reserviert) = self {
            reserviert.anschlüsse()
        } else {
            Anschlüsse::default()
        }
    }
}

impl<S, R> Reserviere<Option<R>> for Option<S>
where
    S: Reserviere<R>,
{
    type MoveArg = <S as Reserviere<R>>::MoveArg;
    type RefArg = <S as Reserviere<R>>::RefArg;
    type MutRefArg = <S as Reserviere<R>>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Option<R>> {
        use Ergebnis::{Fehler, FehlerMitErsatzwert, Wert};
        if let Some(serialisiert) = self {
            match serialisiert.reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg) {
                // false positive, anschlüsse transformiert durch den Funktionsaufruf
                #[allow(clippy::shadow_unrelated)]
                Wert { anschluss, anschlüsse } => Wert { anschluss: Some(anschluss), anschlüsse },
                #[allow(clippy::shadow_unrelated)]
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    FehlerMitErsatzwert { anschluss: Some(anschluss), fehler, anschlüsse }
                },
                #[allow(clippy::shadow_unrelated)]
                Fehler { fehler, anschlüsse } => {
                    FehlerMitErsatzwert { anschluss: None, fehler, anschlüsse }
                },
            }
        } else {
            Wert { anschluss: None, anschlüsse }
        }
    }
}

impl<S, R> Serialisiere<Vec<S>> for Vec<R>
where
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> Vec<S> {
        self.iter().map(Serialisiere::serialisiere).collect()
    }

    fn anschlüsse(self) -> Anschlüsse {
        let mut anschlüsse = Anschlüsse::default();
        for reserviert in self {
            anschlüsse.anhängen(reserviert.anschlüsse());
        }
        anschlüsse
    }
}

impl<S, R> Reserviere<Vec<R>> for Vec<S>
where
    S: Reserviere<R>,
    <S as Reserviere<R>>::MoveArg: Clone,
{
    type MoveArg = <S as Reserviere<R>>::MoveArg;
    type RefArg = <S as Reserviere<R>>::RefArg;
    type MutRefArg = <S as Reserviere<R>>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Vec<R>> {
        let len = self.len();
        self.into_iter().fold(
            Ergebnis::Wert { anschluss: Vec::with_capacity(len), anschlüsse },
            |acc, serialisiert| {
                acc.reserviere_ebenfalls_mit(
                    lager,
                    serialisiert,
                    move_arg.clone(),
                    ref_arg,
                    mut_ref_arg,
                    |mut vec, reserviert| {
                        vec.push(reserviert);
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

impl<S, R> Serialisiere<NonEmpty<S>> for NonEmpty<R>
where
    R: Serialisiere<S>,
{
    fn serialisiere(&self) -> NonEmpty<S> {
        let head = self.head.serialisiere();
        let tail = self.tail.serialisiere();
        NonEmpty { head, tail }
    }

    fn anschlüsse(self) -> Anschlüsse {
        let mut anschlüsse = Anschlüsse::default();
        for reserviert in self {
            anschlüsse.anhängen(reserviert.anschlüsse());
        }
        anschlüsse
    }
}

impl<S, R> Reserviere<NonEmpty<R>> for NonEmpty<S>
where
    S: Reserviere<R>,
    <S as Reserviere<R>>::MoveArg: Clone,
{
    type MoveArg = <S as Reserviere<R>>::MoveArg;
    type RefArg = <S as Reserviere<R>>::RefArg;
    type MutRefArg = <S as Reserviere<R>>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<NonEmpty<R>> {
        let first = self.head.reserviere(lager, anschlüsse, move_arg.clone(), ref_arg, mut_ref_arg);
        first.reserviere_ebenfalls_mit(
            lager,
            self.tail,
            move_arg,
            ref_arg,
            mut_ref_arg,
            |head, tail| NonEmpty { head, tail },
            |either| match either {
                Either::Left(None) => None,
                Either::Left(Some(element)) => Some(NonEmpty::singleton(element)),
                Either::Right(tail) => NonEmpty::from_vec(tail),
            },
        )
    }
}

/// Erzeuge den Argument-Typ für reservieren eines tuples.
macro_rules! tuple_arg_type {
    ($arg: ident: $type: ident - $serialisiert: ident $(,)?) => {
        <$serialisiert as Reserviere<$type>>::$arg
    };
    ($arg: ident: $head_type: ident - $head_serialisiert: ident, $($tail_type: ident - $tail_serialisiert: ident),+ $(,)?) => {
        (<$head_serialisiert as Reserviere<$head_type>>::$arg, tuple_arg_type!($arg: $($tail_type - $tail_serialisiert),+))
    };
}

/// Implementiere [`Serialisiere`] und [`Reserviere`] für ein Tupel.
macro_rules! impl_serialisiere_tuple {
    ($($name: ident : $type: ident - $serialisiert: ident),+) => {
        #[allow(clippy::min_ident_chars)]
        impl<A0, S0, $($type, $serialisiert),+> Serialisiere<(S0, $($serialisiert),+)> for (A0, $($type),+)
        where
            A0: Serialisiere<S0>,
            S0: Reserviere<A0> + ,
            $(
                $type: Serialisiere<$serialisiert>,
                $serialisiert: Reserviere<$type> + ,
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

        #[allow(clippy::min_ident_chars)]
        impl<A0, S0, $($type, $serialisiert),+> Reserviere<(A0, $($type),+)> for (S0, $($serialisiert),+)
        where
            A0: Serialisiere<S0>,
            S0: Reserviere<A0> + ,
            $(
                $type: Serialisiere<$serialisiert>,
                $serialisiert: Reserviere<$type> + ,
            )+
        {
            #[allow(unused_parens)]
            type MoveArg = tuple_arg_type!(MoveArg: A0 - S0, $($type - $serialisiert),+);
            #[allow(unused_parens)]
            type RefArg = tuple_arg_type!(RefArg: A0 - S0, $($type - $serialisiert),+);
            #[allow(unused_parens)]
            type MutRefArg = tuple_arg_type!(MutRefArg: A0 - S0, $($type - $serialisiert),+);
            fn reserviere(
                self,
                lager: &mut anschluss::Lager,
                anschlüsse: Anschlüsse,
                move_arg: Self::MoveArg,
                ref_arg: &Self::RefArg,
                mut_ref_arg: &mut Self::MutRefArg,
            ) -> Ergebnis<(A0, $($type),+)> {
                let (a0, $($name),+) = self;
                let (move_arg_0, move_tail_tuple) = move_arg;
                let (ref_arg_0, ref_tail_tuple) = ref_arg;
                let (mut_ref_arg_0, mut_ref_tail_tuple) = mut_ref_arg;
                let reserviert = a0.reserviere(lager, anschlüsse, move_arg_0, ref_arg_0, mut_ref_arg_0);
                reserviert.reserviere_ebenfalls_mit(
                    lager,
                    ($($name),+),
                    move_tail_tuple,
                    ref_tail_tuple,
                    mut_ref_tail_tuple,
                    #[allow(unused_parens)]
                    |a0, ($($name),+)| (a0, $($name),+),
                    |_| None,
                )
            }
        }
    };
}

impl_serialisiere_tuple! {a: A-SA}
impl_serialisiere_tuple! {a: A-SA, b: B-SB}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD, e: E-SE}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD, e: E-SE, f: F-SF}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD, e: E-SE, f: F-SF, g: G-SG}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD, e: E-SE, f: F-SF, g: G-SG, h: H-SH}
impl_serialisiere_tuple! {a: A-SA, b: B-SB, c: C-SC, d: D-SD, e: E-SE, f: F-SF, g: G-SG, h: H-SH, i: I-SI}
