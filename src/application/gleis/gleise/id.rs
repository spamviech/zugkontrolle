//! Ids zur Identifikation der Gleise.

use std::marker::PhantomData;

use crate::{
    application::{
        gleis::{
            gerade::Gerade,
            kreuzung::Kreuzung,
            kurve::Kurve,
            weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
        },
        typen::{vektor::Vektor, Zeichnen},
    },
    steuerung::streckenabschnitt,
    zugtyp::Zugtyp,
};

/// Id für ein Gleis. Kann sich beim Programm-Neustart ändern.
#[derive(zugkontrolle_derive::Debug)]
pub struct GleisId<T> {
    position: Rectangle<Vektor>,
    streckenabschnitt: Option<streckenabschnitt::Name>,
    phantom: PhantomData<fn() -> T>,
}
impl<T> GleisId<T> {
    // Als Methode definiert, damit es privat bleibt.
    pub(in crate::application) fn clone(&self) -> Self {
        GleisId {
            position: self.position.clone(),
            streckenabschnitt: self.streckenabschnitt.clone(),
            phantom: self.phantom,
        }
    }
}

// Explizite Implementierung wegen Phantomtyp benötigt.
impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position && self.streckenabschnitt == other.streckenabschnitt
    }
}

// FIXME remove
// impl<T> Hash for GleisId<T> {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.0.hash(state)
//     }
// }

#[derive(zugkontrolle_derive::Debug)]
pub enum AnyId<Z> {
    Gerade(GleisId<Gerade<Z>>),
    Kurve(GleisId<Kurve<Z>>),
    Weiche(GleisId<Weiche<Z>>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche<Z>>),
    KurvenWeiche(GleisId<KurvenWeiche<Z>>),
    SKurvenWeiche(GleisId<SKurvenWeiche<Z>>),
    Kreuzung(GleisId<Kreuzung<Z>>),
}

impl<Z> PartialEq for AnyId<Z> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AnyId::Gerade(l0), AnyId::Gerade(r0)) => l0 == r0,
            (AnyId::Kurve(l0), AnyId::Kurve(r0)) => l0 == r0,
            (AnyId::Weiche(l0), AnyId::Weiche(r0)) => l0 == r0,
            (AnyId::DreiwegeWeiche(l0), AnyId::DreiwegeWeiche(r0)) => l0 == r0,
            (AnyId::KurvenWeiche(l0), AnyId::KurvenWeiche(r0)) => l0 == r0,
            (AnyId::SKurvenWeiche(l0), AnyId::SKurvenWeiche(r0)) => l0 == r0,
            (AnyId::Kreuzung(l0), AnyId::Kreuzung(r0)) => l0 == r0,
            _ => false,
        }
    }
}

macro_rules! with_any_id {
    ($any_id: expr , $function: expr$(, $objekt:expr$(, $extra_arg:expr)*)?) => {
        match $any_id {
            AnyId::Gerade(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Kurve(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Weiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::DreiwegeWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::KurvenWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::SKurvenWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            AnyId::Kreuzung(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
        }
    };
}
use rstar::primitives::Rectangle;
pub(crate) use with_any_id;

impl<Z: Zugtyp> AnyId<Z> {
    pub(super) fn from_ref<T: Zeichnen>(gleis_id: &GleisId<T>) -> Self
    where
        GleisId<T>: Into<Self>,
    {
        gleis_id.clone().into()
    }

    pub(in crate::application) fn clone(&self) -> Self {
        with_any_id!(self, Self::from_ref)
    }
}

macro_rules! impl_any_id_from {
    ($type:ident) => {
        impl<Z: Zugtyp> From<GleisId<$type<Z>>> for AnyId<Z> {
            fn from(gleis_id: GleisId<$type<Z>>) -> Self {
                AnyId::$type(gleis_id)
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
