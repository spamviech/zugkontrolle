//! Ids zum Speichern der Gleise

use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use log::*;
use serde::{Deserialize, Serialize};

use crate::application::gleis::{
    gerade::Gerade,
    kreuzung::Kreuzung,
    kurve::Kurve,
    weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
};

/// If GleisIdLock<Z>::read contains a Some, the GleisId<Z> is guaranteed to be valid.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct GleisIdLock<T>(Arc<RwLock<Option<GleisId<T>>>>);

impl<T> GleisIdLock<T> {
    pub(super) fn new(gleis_id: u64) -> Self {
        GleisIdLock(Arc::new(RwLock::new(Some(GleisId::new(gleis_id)))))
    }

    pub fn read(&self) -> RwLockReadGuard<Option<GleisId<T>>> {
        self.0.read().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }

    pub(super) fn write(&self) -> RwLockWriteGuard<Option<GleisId<T>>> {
        self.0.write().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Any;

/// Identifier for a Gleis.  Will probably change between restarts.
///
/// The API will only provide &GleisIdLock<Z>.
#[derive(zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct GleisId<T>(u64, PhantomData<*const T>);
impl<T> GleisId<T> {
    pub fn new(gleis_id: u64) -> Self {
        GleisId(gleis_id, PhantomData)
    }

    pub(crate) fn as_any(&self) -> GleisId<Any> {
        GleisId::new(self.0)
    }

    // defined a method so it stays private
    fn clone(&self) -> Self {
        GleisId(self.0, self.1)
    }
}

// explicit implementation needed due to phantom type
// derived instead required corresponding Trait implemented on phantom type
impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Eq for GleisId<T> {}
impl<T> Hash for GleisId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(zugkontrolle_derive::Debug)]
pub(crate) enum AnyId<Z> {
    Gerade(GleisId<Gerade<Z>>, GleisIdLock<Gerade<Z>>),
    Kurve(GleisId<Kurve<Z>>, GleisIdLock<Kurve<Z>>),
    Weiche(GleisId<Weiche<Z>>, GleisIdLock<Weiche<Z>>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche<Z>>, GleisIdLock<DreiwegeWeiche<Z>>),
    KurvenWeiche(GleisId<KurvenWeiche<Z>>, GleisIdLock<KurvenWeiche<Z>>),
    SKurvenWeiche(GleisId<SKurvenWeiche<Z>>, GleisIdLock<SKurvenWeiche<Z>>),
    Kreuzung(GleisId<Kreuzung<Z>>, GleisIdLock<Kreuzung<Z>>),
}
macro_rules! with_any_id {
    ($any_id: expr , $function: expr$(, $objekt:expr$(, $extra_arg:expr)*)?) => {
        match $any_id {
            AnyId::Gerade(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Kurve(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Weiche(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::DreiwegeWeiche(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::KurvenWeiche(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::SKurvenWeiche(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Kreuzung(gleis_id, _gleis_id_lock) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
        }
    };
}
macro_rules! with_any_id_and_lock {
    ($any_id: expr , $function: expr$(, $objekt:expr$(, $extra_arg:expr)*)?) => {
        match $any_id {
            AnyId::Gerade(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::Kurve(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::Weiche(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::DreiwegeWeiche(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::KurvenWeiche(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::SKurvenWeiche(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
            AnyId::Kreuzung(gleis_id, gleis_id_lock) => {
                $function($($objekt,)? gleis_id, gleis_id_lock $($(, $extra_arg)*)?)
            }
        }
    };
}
impl<Z> AnyId<Z> {
    pub(crate) fn id_as_any(&self) -> GleisId<Any> {
        with_any_id!(self, GleisId::as_any)
    }

    pub(super) fn from_refs<T>(gleis_id: &GleisId<T>, gleis_id_lock: &GleisIdLock<T>) -> Self
    where
        (GleisId<T>, GleisIdLock<T>): Into<Self>,
    {
        (gleis_id.clone(), gleis_id_lock.clone()).into()
    }

    pub(super) fn clone(&self) -> Self {
        with_any_id_and_lock!(self, Self::from_refs)
    }
}

macro_rules! impl_any_id_from {
    ($type:ident) => {
        impl<Z> From<(GleisId<$type<Z>>, GleisIdLock<$type<Z>>)> for AnyId<Z> {
            fn from((gleis_id, gleis_id_lock): (GleisId<$type<Z>>, GleisIdLock<$type<Z>>)) -> Self {
                AnyId::$type(gleis_id, gleis_id_lock)
            }
        }
    };
}
impl_any_id_from! {Gerade}
impl_any_id_from! {Kurve}
impl_any_id_from! {Weiche}
impl_any_id_from! {DreiwegeWeiche}
impl_any_id_from! {KurvenWeiche}
impl_any_id_from! {SKurvenWeiche}
impl_any_id_from! {Kreuzung}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, beschreibung: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", beschreibung, poisoned);
    poisoned.into_inner()
}
