//! Ids zur Identifikation der Gleise.

use std::marker::PhantomData;

use rstar::primitives::Rectangle;

use crate::{
    application::{
        gleis::{
            gerade::Gerade,
            kreuzung::Kreuzung,
            kurve::Kurve,
            weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
        },
        typen::vektor::Vektor,
    },
    steuerung::streckenabschnitt,
};

/// Id für ein Gleis. Kann sich beim Programm-Neustart ändern.
#[derive(zugkontrolle_derive::Debug)]
pub struct GleisId<T> {
    pub(in crate::application::gleis::gleise) rectangle: Rectangle<Vektor>,
    pub(in crate::application::gleis::gleise) streckenabschnitt: Option<streckenabschnitt::Name>,
    pub(in crate::application::gleis::gleise) phantom: PhantomData<fn() -> T>,
}
impl<T> GleisId<T> {
    // Als Methode definiert, damit es privat bleibt.
    pub(in crate::application) fn clone(&self) -> Self {
        GleisId {
            rectangle: self.rectangle.clone(),
            streckenabschnitt: self.streckenabschnitt.clone(),
            phantom: self.phantom,
        }
    }
}

// Explizite Implementierung wegen Phantomtyp benötigt.
impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        (self.rectangle == other.rectangle) && (self.streckenabschnitt == other.streckenabschnitt)
    }
}

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

macro_rules! mit_any_id {
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
pub(crate) use mit_any_id;

impl<Z> AnyId<Z> {
    fn aus_ref<T>(gleis_id: &GleisId<T>) -> Self
    where
        GleisId<T>: Into<Self>,
    {
        gleis_id.clone().into()
    }

    pub(in crate::application) fn clone(&self) -> AnyId<Z> {
        mit_any_id!(self, Self::aus_ref)
    }
}

macro_rules! impl_any_id_from {
    ($type: ident) => {
        impl<Z> From<GleisId<$type<Z>>> for AnyId<Z> {
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

#[derive(zugkontrolle_derive::Debug)]
pub(in crate::application::gleis::gleise) struct GleisIdRef<'t, T> {
    pub(in crate::application::gleis::gleise) rectangle: &'t Rectangle<Vektor>,
    pub(in crate::application::gleis::gleise) streckenabschnitt:
        &'t Option<streckenabschnitt::Name>,
    pub(in crate::application::gleis::gleise) phantom: PhantomData<fn() -> T>,
}

impl<'t, T> PartialEq<GleisId<T>> for GleisIdRef<'t, T> {
    fn eq(&self, other: &GleisId<T>) -> bool {
        (self.rectangle == &other.rectangle) && (self.streckenabschnitt == &other.streckenabschnitt)
    }
}

#[derive(zugkontrolle_derive::Debug)]
pub(in crate::application::gleis::gleise) enum AnyIdRef<'t, Z> {
    Gerade(GleisIdRef<'t, Gerade<Z>>),
    Kurve(GleisIdRef<'t, Kurve<Z>>),
    Weiche(GleisIdRef<'t, Weiche<Z>>),
    DreiwegeWeiche(GleisIdRef<'t, DreiwegeWeiche<Z>>),
    KurvenWeiche(GleisIdRef<'t, KurvenWeiche<Z>>),
    SKurvenWeiche(GleisIdRef<'t, SKurvenWeiche<Z>>),
    Kreuzung(GleisIdRef<'t, Kreuzung<Z>>),
}

impl<'t, Z> PartialEq<AnyId<Z>> for AnyIdRef<'t, Z> {
    fn eq(&self, other: &AnyId<Z>) -> bool {
        match (self, other) {
            (AnyIdRef::Gerade(l0), AnyId::Gerade(r0)) => l0 == r0,
            (AnyIdRef::Kurve(l0), AnyId::Kurve(r0)) => l0 == r0,
            (AnyIdRef::Weiche(l0), AnyId::Weiche(r0)) => l0 == r0,
            (AnyIdRef::DreiwegeWeiche(l0), AnyId::DreiwegeWeiche(r0)) => l0 == r0,
            (AnyIdRef::KurvenWeiche(l0), AnyId::KurvenWeiche(r0)) => l0 == r0,
            (AnyIdRef::SKurvenWeiche(l0), AnyId::SKurvenWeiche(r0)) => l0 == r0,
            (AnyIdRef::Kreuzung(l0), AnyId::Kreuzung(r0)) => l0 == r0,
            _ => false,
        }
    }
}

macro_rules! impl_any_id_ref_from {
    ($type: ident) => {
        impl<'t, Z> From<GleisIdRef<'t, $type<Z>>> for AnyIdRef<'t, Z> {
            fn from(gleis_id: GleisIdRef<'t, $type<Z>>) -> Self {
                AnyIdRef::$type(gleis_id)
            }
        }
    };
}
impl_any_id_ref_from! {Gerade}
impl_any_id_ref_from! {Kurve}
impl_any_id_ref_from! {Weiche}
impl_any_id_ref_from! {DreiwegeWeiche}
impl_any_id_ref_from! {KurvenWeiche}
impl_any_id_ref_from! {SKurvenWeiche}
impl_any_id_ref_from! {Kreuzung}
