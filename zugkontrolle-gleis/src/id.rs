//! Ids zur Identifikation der Gleise.

// Erlaubt id::Repräsentation, only way to export macros from a module
#![allow(clippy::pub_use)]

use std::hash::Hash;

use zugkontrolle_anschluss::de_serialisieren::Serialisiere;
use zugkontrolle_id::GleisId;
use zugkontrolle_typen::Zeichnen;

use crate::steuerung::aktualisieren::MitSteuerung;

// soll direkt importiert werden
#[allow(clippy::module_name_repetitions)]
/// Id für die Definition eines Gleises.
pub type DefinitionId<T> = GleisId<<T as MitSteuerung>::SelfUnit>;

#[macro_export]
/// Helper-Macro für [`erzeuge_any_enum`]: ersetzte `[]` durch `$ty`.
macro_rules! ersetzte_eckige_klammern {
    ($ty: ty, [$($acc: tt)*], [] $($tail: tt)*) => {
        $crate::ersetzte_eckige_klammern! {$ty, [$($acc)* $ty], $($tail)*}
    };
    ($ty: ty, [$($acc: tt)*], $head: tt $($tail: tt)*) => {
        $crate::ersetzte_eckige_klammern! {$ty, [$($acc)* $head], $($tail)*}
    };
    ($ty: ty, [$($acc: tt)*], ) => {
        $($acc)*
    };
}

#[macro_export]
/// Erzeuge ein `enum` mit einer Variante für jede Gleis-Art.
macro_rules! erzeuge_any_enum {
    ($(($vis: vis))? $name: ident$(<$($lt: lifetime),*>)?, $doc: literal, [$($derives: ident),*], $( ($($path: tt)*) ),+ $(,)?) => {
        #[doc = $doc]
        #[derive(zugkontrolle_macros::From, $($derives),*)]
        #[allow(unused_qualifications)]
        $($vis)? enum $name$(<$($lt),*>)? {
            /// Variante für eine [`Gerade`](crate::gleis::gerade::Gerade).
            Gerade($( $crate::ersetzte_eckige_klammern!{$crate::gerade::Gerade, [], $($path)*} ),+),
            /// Variante für eine [`Kurve`](crate::gleis::kurve::Kurve).
            Kurve($( $crate::ersetzte_eckige_klammern!{$crate::kurve::Kurve, [], $($path)*} ),+),
            /// Variante für eine [`Weiche`](crate::gleis::weiche::gerade::Weiche).
            Weiche($( $crate::ersetzte_eckige_klammern!{$crate::weiche::gerade::Weiche, [], $($path)*} ),+),
            /// Variante für eine [`DreiwegeWeiche`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
            DreiwegeWeiche($( $crate::ersetzte_eckige_klammern!{$crate::weiche::dreiwege::DreiwegeWeiche, [], $($path)*} ),+),
            /// Variante für eine [`KurvenWeiche`](crate::gleis::weiche::kurve::KurvenWeiche).
            KurvenWeiche($( $crate::ersetzte_eckige_klammern!{$crate::weiche::kurve::KurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [`SKurvenWeiche`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
            SKurvenWeiche($( $crate::ersetzte_eckige_klammern!{$crate::weiche::s_kurve::SKurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [`Kreuzung`](crate::gleis::kreuzung::Kreuzung).
            Kreuzung($( $crate::ersetzte_eckige_klammern!{$crate::kreuzung::Kreuzung, [], $($path)*} ),+),
        }
    };
}
pub use crate::erzeuge_any_enum;

#[macro_export]
/// Helper-Macro für [`mit_any_id`]: Erzeuge die passende Referenz von `$wert`,
/// ausgehend von einem (optionalen) `ref` oder `mut` suffix.
macro_rules! als_ref {
    (mut $wert: expr) => {
        &mut $wert
    };
    (ref $wert: expr) => {
        &$wert
    };
    ($wert: expr) => {
        &$wert
    };
}

#[macro_export]
// Soll unqualifiziert verwendet werden
#[allow(clippy::module_name_repetitions)]
/// Erzeuge ein `match`-statement und führe das `$macro!`/die `$funktion`
/// mit den als `$ident` gematchten Varianten-Feldern als Argumente aus.
macro_rules! mit_any_id {
    (
        { $($($mut: tt)? $collection: expr),* },
        [$id: ty => $($ident: ident),+] $any_id: expr
        => $macro: ident ! ( $($extra_arg: expr),* $(,)? )
    ) => {{
        use $id::{Gerade, Kurve, Weiche, DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Kreuzung};
        match $any_id {
            Gerade( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.geraden) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kurve( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.kurven) , )* $($ident),+ $(, $extra_arg)*)
            }
            Weiche( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            DreiwegeWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.dreiwege_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            KurvenWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.kurven_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            SKurvenWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.s_kurven_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kreuzung( $($ident),+ ) => {
                $macro! ( $( $crate::als_ref!($($mut)? $collection.kreuzungen) , )* $($ident),+ $(, $extra_arg)*)
            }
        }
    }};
    (
        { $($($mut:tt)? $collection: expr),* },
        [$id: ty => $($ident: ident),+] $any_id: expr
        => $function: ident ( $($extra_arg: expr),* $(,)? )
    ) => {{
        use $id::{Gerade, Kurve, Weiche, DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Kreuzung};
        match $any_id {
            Gerade( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kurve( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Weiche( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            DreiwegeWeiche( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            KurvenWeiche( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            SKurvenWeiche( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kreuzung( $($ident),+ ) => {
                $function ( $( $crate::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
        }
    }};
}
// Soll unqualifiziert verwendet werden
#[allow(clippy::module_name_repetitions)]
pub use mit_any_id;

erzeuge_any_enum! {
    (pub) AnyId,
    "Id für ein beliebiges Gleis.",
    [Debug, Clone, PartialEq, Eq, Hash],
    (GleisId<[]>),
}

erzeuge_any_enum! {
    (pub) AnyDefinitionId,
    "Id für die Definition eines beliebiges Gleises.",
    [Debug, Clone, PartialEq, Eq, Hash],
    (DefinitionId<[]>),
}

erzeuge_any_enum! {
    (pub) AnyGleisDefinitionId,
    "Id für ein beliebiges Gleis und seine Definition.",
    [Debug, Clone, PartialEq, Eq],
    (GleisId<[]>),
    (DefinitionId<[]>),
}

erzeuge_any_enum! {
    (pub) AnyIdSteuerung,
    "Id für ein beliebiges Gleis und seine Steuerung.",
    [Debug, Clone],
    (GleisId<[]>),
    (<[] as MitSteuerung>::Steuerung),
}

impl AnyIdSteuerung {
    /// Erhalte die [`Id`](AnyId) eines Gleises.
    #[must_use]
    pub fn id(&self) -> AnyId {
        /// Hilfs-Macro zur Verwendung mit [`mit_any_id!`].
        macro_rules! id_aux {
            ($id: expr, $steuerung: expr) => {
                AnyId::from($id.clone())
            };
        }
        mit_any_id!(
            {},
            [AnyIdSteuerung => id, _steuerung] self
            =>id_aux!()
        )
    }
}

erzeuge_any_enum! {
    (pub) AnyDefinitionIdSteuerung,
    "Id für die Definition eines beliebigen Gleises und seine Steuerung.",
    [Debug, Clone],
    (DefinitionId<[]>),
    (<[] as MitSteuerung>::Steuerung),
}

impl AnyIdSteuerung {
    /// Serialisiere die Steuerung des Gleises.
    #[must_use]
    pub fn serialisiere(&self) -> AnyIdSteuerungSerialisiert {
        /// Hilfs-Macro zur Verwendung mit [`mit_any_id!`].
        macro_rules! serialisiere_aux {
            ($id: expr, $steuerung: expr) => {
                AnyIdSteuerungSerialisiert::from((
                    $id.clone(),
                    $steuerung.as_ref().map(|steuerung| Serialisiere::serialisiere(steuerung)),
                ))
            };
        }
        mit_any_id!(
            {},
            [AnyIdSteuerung => id, steuerung] self
            => serialisiere_aux!()
        )
    }
}

erzeuge_any_enum! {
    (pub) AnyIdSteuerungSerialisiert,
    "Id für ein beliebiges Gleis und seine serialisierte Steuerung.",
    [Debug, Clone],
    (GleisId<[]>),
    (<[] as MitSteuerung>::Serialisiert),
}

impl AnyIdSteuerungSerialisiert {
    /// Erhalte die [`Id`](AnyId) eines Gleises.
    #[must_use]
    pub fn id(&self) -> AnyId {
        /// Hilfs-Macro zur Verwendung mit [`mit_any_id!`].
        macro_rules! id_aux {
            ($id: expr, $steuerung: expr) => {
                AnyId::from($id.clone())
            };
        }
        mit_any_id!(
            {},
            [AnyIdSteuerungSerialisiert => id, _steuerung] self
            =>id_aux!()
        )
    }
}

erzeuge_any_enum! {
    (pub) AnyIdVerbindung,
    "Id für ein beliebiges Gleis und der Name einer seiner Verbindungen.",
    [Debug, Clone],
    (GleisId<[]>),
    (<[] as Zeichnen<()>>::VerbindungName),
}

erzeuge_any_enum! {
    (pub) AnyDefinitionIdSteuerungVerbindung,
    "Id für die Definition eines beliebigen Gleises, seine Steuerung und der Name einer seiner Verbindungen.",
    [Debug, Clone],
    (DefinitionId<[]>),
    (<[] as MitSteuerung>::Steuerung),
    (<[] as Zeichnen<()>>::VerbindungName),
}
