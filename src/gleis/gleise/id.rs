//! Ids zur Identifikation der Gleise.

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

/// Id für ein Gleis.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct GleisId2<T: 'static>(Arc<Id<T>>);

/// Id für die Definition eines Gleises.
pub type DefinitionId2<T> = GleisId2<<T as MitSteuerung>::SelfUnit>;

impl<T> PartialEq for GleisId2<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for GleisId2<T> {}

impl<T> PartialOrd for GleisId2<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for GleisId2<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for GleisId2<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> GleisId2<T> {
    /// Erzeuge eine neue [GleisId] für den entsprechenden Typ.
    pub fn neu() -> Result<GleisId2<T>, KeineIdVerfügbar> {
        Id::neu().map(|id| GleisId2(Arc::new(id)))
    }

    /// Erhalte eine eindeutige Zahl für die [GleisId].
    ///
    /// Die selbe [GleisId], sowie alle ihre Kopien, werde bei jedem Aufruf die selbe Zahl zurückgeben.
    /// Zwei gleichzeitig existierende [GleisIds](GleisId) werden unterschiedliche Zahlen zurückgeben.
    ///
    /// Sobald die letzte Kopie einer [GleisId] gedroppt wird kann es sein,
    /// dass eine andere [GleisId] die selbe Zahl zurückgibt.
    pub fn repräsentation(&self) -> Repräsentation {
        self.0.repräsentation()
    }
}

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

macro_rules! erzeuge_any_enum {
    ($(($vis: vis))? $name: ident$(<$($lt: lifetime),*>)?, $doc: literal, [$($derives: ident),*], $( ($($path: tt)*) ),+ $(,)?) => {
        #[doc = $doc]
        #[derive(zugkontrolle_macros::From, $($derives),*)]
        #[allow(unused_qualifications)]
        $($vis)? enum $name$(<$($lt),*>)? {
            /// Variante für eine [Gerade](crate::gleis::gerade::Gerade).
            Gerade($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::gerade::Gerade, [], $($path)*} ),+),
            /// Variante für eine [Kurve](crate::gleis::kurve::Kurve).
            Kurve($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::kurve::Kurve, [], $($path)*} ),+),
            /// Variante für eine [Weiche](crate::gleis::weiche::gerade::Weiche).
            Weiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::gerade::Weiche, [], $($path)*} ),+),
            /// Variante für eine [DreiwegeWeiche](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
            DreiwegeWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::dreiwege::DreiwegeWeiche, [], $($path)*} ),+),
            /// Variante für eine [KurvenWeiche](crate::gleis::weiche::kurve::KurvenWeiche).
            KurvenWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::kurve::KurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [SKurvenWeiche](crate::gleis::weiche::s_kurve::SKurvenWeiche).
            SKurvenWeiche($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::weiche::s_kurve::SKurvenWeiche, [], $($path)*} ),+),
            /// Variante für eine [Kreuzung](crate::gleis::kreuzung::Kreuzung).
            Kreuzung($( $crate::gleis::gleise::id::ersetzte_eckige_klammern!{crate::gleis::kreuzung::Kreuzung, [], $($path)*} ),+),
        }
    };
}
pub(in crate::gleis::gleise) use erzeuge_any_enum;

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

macro_rules! mit_any_id2 {
    (
        { $($($mut: tt)? $collection: expr),* },
        [$id: ty => $($ident: ident),+] $any_id: expr
        => $macro: ident ! ( $($extra_arg: expr),* $(,)? )
    ) => {{
        use $id::*;
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
        use $id::*;
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
pub(crate) use mit_any_id2;

erzeuge_any_enum! {
    (pub) AnyId2,
    "Id für ein beliebiges Gleis.",
    [Debug, Clone, PartialEq, Eq, Hash],
    (GleisId2<[]>),
}

erzeuge_any_enum! {
    (pub) AnyDefinitionId2,
    "Id für die Definition eines beliebiges Gleises.",
    [Debug, Clone, PartialEq, Eq, Hash],
    (DefinitionId2<[]>),
}

erzeuge_any_enum! {
    (pub) AnyGleisDefinitionId2,
    "Id für ein beliebiges Gleis und seine Definition.",
    [Debug, Clone, PartialEq, Eq],
    (GleisId2<[]>),
    (DefinitionId2<[]>),
}

erzeuge_any_enum! {
    (pub) AnyIdSteuerung2,
    "Id für ein beliebiges Gleis und seine Steuerung.",
    [Debug, Clone],
    (GleisId2<[]>),
    (<[] as MitSteuerung>::Steuerung),
}

impl AnyIdSteuerung2 {
    /// Erhalte die [Id](AnyId) eines Gleises.
    pub fn id(&self) -> AnyId2 {
        macro_rules! id_aux {
            ($id: expr, $steuerung: expr) => {
                AnyId2::from($id.clone())
            };
        }
        mit_any_id2!(
            {},
            [AnyIdSteuerung2 => id, _steuerung] self
            =>id_aux!()
        )
    }
}

erzeuge_any_enum! {
    (pub) AnyDefinitionIdSteuerung2,
    "Id für die Definition eines beliebigen Gleises und seine Steuerung.",
    [Debug, Clone],
    (DefinitionId2<[]>),
    (<[] as MitSteuerung>::Steuerung),
}

impl AnyIdSteuerung2 {
    /// Serialisiere die Steuerung des Gleises.
    pub fn serialisiere(&self) -> AnyIdSteuerungSerialisiert2 {
        macro_rules! serialisiere_aux {
            ($id: expr, $steuerung: expr) => {
                AnyIdSteuerungSerialisiert2::from((
                    $id.clone(),
                    $steuerung.as_ref().map(|steuerung| Serialisiere::serialisiere(steuerung)),
                ))
            };
        }
        mit_any_id2!(
            {},
            [AnyIdSteuerung2 => id, steuerung] self
            => serialisiere_aux!()
        )
    }
}

erzeuge_any_enum! {
    (pub) AnyIdSteuerungSerialisiert2,
    "Id für ein beliebiges Gleis und seine serialisierte Steuerung.",
    [Debug, Clone],
    (GleisId2<[]>),
    (<[] as MitSteuerung>::Serialisiert),
}

erzeuge_any_enum! {
    (pub) AnyIdVerbindung2,
    "Id für ein beliebiges Gleis und der Name einer seiner Verbindungen.",
    [Debug, Clone],
    (GleisId2<[]>),
    (<[] as Zeichnen<()>>::VerbindungName),
}

erzeuge_any_enum! {
    (pub) AnyDefinitionIdSteuerungVerbindung2,
    "Id für die Definition eines beliebigen Gleises, seine Steuerung und der Name einer seiner Verbindungen.",
    [Debug, Clone],
    (DefinitionId2<[]>),
    (<[] as MitSteuerung>::Steuerung),
    (<[] as Zeichnen<()>>::VerbindungName),
}
