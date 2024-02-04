//! Ids zur Identifikation der Gleise.

// Erlaubt id::Repräsentation
#![allow(clippy::pub_use)]

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    gleis::gleise::{
        id::eindeutig::{Id, KeineIdVerfügbar},
        steuerung::MitSteuerung,
    },
    typen::Zeichnen,
};

pub mod eindeutig;

#[cfg(test)]
mod test;

pub use eindeutig::Repräsentation;

// soll direkt importiert werden
#[allow(clippy::module_name_repetitions)]
/// Id für ein Gleis.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct GleisId<T: 'static>(Arc<Id<T>>);

// soll direkt importiert werden
#[allow(clippy::module_name_repetitions)]
/// Id für die Definition eines Gleises.
pub type DefinitionId<T> = GleisId<<T as MitSteuerung>::SelfUnit>;

impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for GleisId<T> {}

impl<T> PartialOrd for GleisId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for GleisId<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for GleisId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> GleisId<T> {
    /// Erzeuge eine neue [`GleisId`] für den entsprechenden Typ.
    ///
    /// ## Errors
    ///
    /// Wenn für `T` keine neue [`GleisId`] erzeugt werden kann.
    pub fn neu() -> Result<GleisId<T>, KeineIdVerfügbar> {
        Id::neu().map(|id| GleisId(Arc::new(id)))
    }

    /// Erhalte eine eindeutige Zahl für die [`GleisId`].
    ///
    /// Die selbe [`GleisId`], sowie alle ihre Kopien, werde bei jedem Aufruf die selbe Zahl zurückgeben.
    /// Zwei gleichzeitig existierende [`GleisIds`](GleisId) werden unterschiedliche Zahlen zurückgeben.
    ///
    /// Sobald die letzte Kopie einer [`GleisId`] gedroppt wird kann es sein,
    /// dass eine andere [`GleisId`] die selbe Zahl zurückgibt.
    #[must_use]
    pub fn repräsentation(&self) -> Repräsentation {
        self.0.repräsentation()
    }
}

/// Helper-Macro für [`erzeuge_any_enum`]: ersetzte `[]` durch `$ty`.
macro_rules! ersetzte_eckige_klammern {
    ($ty: ty, [$($acc: tt)*], [] $($tail: tt)*) => {
        $crate::gleis::gleise::id::ersetzte_eckige_klammern! {$ty, [$($acc)* $ty], $($tail)*}
    };
    ($ty: ty, [$($acc: tt)*], $head: tt $($tail: tt)*) => {
        $crate::gleis::gleise::id::ersetzte_eckige_klammern! {$ty, [$($acc)* $head], $($tail)*}
    };
    ($ty: ty, [$($acc: tt)*], ) => {
        $($acc)*
    };
}
pub(in crate::gleis::gleise) use ersetzte_eckige_klammern;

/// Erzeuge ein `enum` mit einer Variante für jede Gleis-Art.
macro_rules! erzeuge_any_enum {
    ($(($vis: vis))? $name: ident$(<$($lt: lifetime),*>)?, $doc: literal, [$($derives: ident),*], $( ($($path: tt)*) ),+ $(,)?) => {
        #[doc = $doc]
        #[derive(zugkontrolle_macros::From, $($derives),*)]
        #[allow(unused_qualifications)]
        $($vis)? enum $name$(<$($lt),*>)? {
            /// Variante für eine [`Gerade`](crate::gleis::gerade::Gerade).
            Gerade($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::gerade::Gerade, [], $($path)*} ),+),
            /// Variante für eine [`Kurve`](crate::gleis::kurve::Kurve).
            Kurve($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::kurve::Kurve, [], $($path)*} ),+),
            /// Variante für eine [`Weiche`](crate::gleis::weiche::gerade::Weiche).
            Weiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::gerade::Weiche, [], $($path)*} ),+),
            /// Variante für eine [`DreiwegeWeiche`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
            DreiwegeWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::dreiwege::DreiwegeWeiche, [], $($path)*} ),+),
            /// Variante für eine [`KurvenWeiche`](crate::gleis::weiche::kurve::KurvenWeiche).
            KurvenWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::kurve::KurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [`SKurvenWeiche`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
            SKurvenWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::s_kurve::SKurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [`Kreuzung`](crate::gleis::kreuzung::Kreuzung).
            Kreuzung($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::kreuzung::Kreuzung, [], $($path)*} ),+),
        }
    };
}
pub(in crate::gleis::gleise) use erzeuge_any_enum;

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
pub(crate) use als_ref;

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
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.geraden) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kurve( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.kurven) , )* $($ident),+ $(, $extra_arg)*)
            }
            Weiche( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            DreiwegeWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.dreiwege_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            KurvenWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.kurven_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            SKurvenWeiche( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.s_kurven_weichen) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kreuzung( $($ident),+ ) => {
                $macro! ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection.kreuzungen) , )* $($ident),+ $(, $extra_arg)*)
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
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kurve( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Weiche( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            DreiwegeWeiche( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            KurvenWeiche( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            SKurvenWeiche( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
            Kreuzung( $($ident),+ ) => {
                $function ( $( $crate::gleis::gleise::id::als_ref!($($mut)? $collection) , )* $($ident),+ $(, $extra_arg)*)
            }
        }
    }};
}
pub(crate) use mit_any_id;

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
