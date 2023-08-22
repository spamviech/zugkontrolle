//! Ids zur Identifikation der Gleise.

use std::marker::PhantomData;

use rstar::primitives::Rectangle;

use crate::{
    gleis::{
        gerade::Gerade,
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{geschwindigkeit, streckenabschnitt},
    typen::vektor::Vektor,
};

pub mod eindeutig;

/// Id für einen Streckenabschnitt.
#[derive(Debug, PartialEq, Eq)]
pub struct StreckenabschnittId {
    pub(crate) geschwindigkeit: Option<geschwindigkeit::Name>,
    pub(crate) name: streckenabschnitt::Name,
}

impl StreckenabschnittId {
    // Als Methode definiert, damit es privat bleibt.
    pub(crate) fn klonen(&self) -> Self {
        Self { geschwindigkeit: self.geschwindigkeit.clone(), name: self.name.clone() }
    }

    pub(crate) fn als_ref<'t>(&'t self) -> StreckenabschnittIdRef<'t> {
        StreckenabschnittIdRef { geschwindigkeit: self.geschwindigkeit.as_ref(), name: &self.name }
    }
}

/// Id für ein Gleis.
#[derive(zugkontrolle_macros::Debug)]
pub struct GleisId<T> {
    pub(in crate::gleis::gleise) rectangle: Rectangle<Vektor>,
    pub(in crate::gleis::gleise) streckenabschnitt: Option<StreckenabschnittId>,
    pub(in crate::gleis::gleise) phantom: PhantomData<fn() -> T>,
}

impl<T> GleisId<T> {
    // Als Methode definiert, damit es privat bleibt.
    pub(crate) fn klonen(&self) -> Self {
        GleisId {
            rectangle: self.rectangle.clone(),
            streckenabschnitt: self.streckenabschnitt.as_ref().map(StreckenabschnittId::klonen),
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

/// Id für ein beliebiges Gleis.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum AnyId {
    /// Eine [Gerade].
    Gerade(GleisId<Gerade>),
    /// Eine [Kurve].
    Kurve(GleisId<Kurve>),
    /// Eine [Weiche].
    Weiche(GleisId<Weiche>),
    /// Eine [DreiwegeWeiche].
    DreiwegeWeiche(GleisId<DreiwegeWeiche>),
    /// Eine [KurvenWeiche].
    KurvenWeiche(GleisId<KurvenWeiche>),
    /// Eine [SKurvenWeiche].
    SKurvenWeiche(GleisId<SKurvenWeiche>),
    /// Eine [Kreuzung].
    Kreuzung(GleisId<Kreuzung>),
}

impl PartialEq for AnyId {
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
            crate::gleis::gleise::id::AnyId::Gerade(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::Kurve(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::Weiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::DreiwegeWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::KurvenWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::SKurvenWeiche(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            crate::gleis::gleise::id::AnyId::Kreuzung(gleis_id) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
        }
    };
}
pub(crate) use mit_any_id;

// completely remove any notion of ID?
#[allow(single_use_lifetimes)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct StreckenabschnittIdRef<'t> {
    pub(crate) geschwindigkeit: Option<&'t geschwindigkeit::Name>,
    pub(crate) name: &'t streckenabschnitt::Name,
}

impl PartialEq<StreckenabschnittId> for StreckenabschnittIdRef<'_> {
    fn eq(&self, other: &StreckenabschnittId) -> bool {
        (self.geschwindigkeit == other.geschwindigkeit.as_ref()) && (*self.name == other.name)
    }
}

impl StreckenabschnittIdRef<'_> {
    /// Klone die Referenzen um eine neue Id zu erzeugen.
    pub(in crate::gleis::gleise) fn als_id(self) -> StreckenabschnittId {
        StreckenabschnittId {
            geschwindigkeit: self.geschwindigkeit.cloned(),
            name: self.name.clone(),
        }
    }
}

// completely remove any notion of ID?
#[derive(zugkontrolle_macros::Debug)]
pub(crate) struct GleisIdRef<'t, T> {
    pub(in crate::gleis::gleise) rectangle: &'t Rectangle<Vektor>,
    pub(in crate::gleis::gleise) streckenabschnitt: Option<StreckenabschnittIdRef<'t>>,
    pub(in crate::gleis::gleise) phantom: PhantomData<fn() -> T>,
}

impl<'s, T> PartialEq<GleisIdRef<'s, T>> for GleisIdRef<'_, T> {
    fn eq(&self, other: &GleisIdRef<'s, T>) -> bool {
        (self.rectangle == other.rectangle) && (self.streckenabschnitt == other.streckenabschnitt)
    }
}

impl<T> PartialEq<GleisId<T>> for GleisIdRef<'_, T> {
    fn eq(&self, other: &GleisId<T>) -> bool {
        (self.rectangle == &other.rectangle)
            && self.streckenabschnitt
                == other.streckenabschnitt.as_ref().map(StreckenabschnittId::als_ref)
    }
}

impl<T> GleisIdRef<'_, T> {
    pub(in crate::gleis::gleise) fn als_id(self) -> GleisId<T> {
        GleisId {
            rectangle: *self.rectangle,
            streckenabschnitt: self.streckenabschnitt.map(|id_ref| id_ref.als_id()),
            phantom: self.phantom,
        }
    }
}

impl<T> GleisId<T> {
    pub(in crate::gleis::gleise) fn als_ref(&self) -> GleisIdRef<'_, T> {
        GleisIdRef {
            rectangle: &self.rectangle,
            streckenabschnitt: self.streckenabschnitt.as_ref().map(StreckenabschnittId::als_ref),
            phantom: self.phantom,
        }
    }
}

#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::From)]
pub(in crate::gleis::gleise) enum AnyIdRef<'t> {
    Gerade(GleisIdRef<'t, Gerade>),
    Kurve(GleisIdRef<'t, Kurve>),
    Weiche(GleisIdRef<'t, Weiche>),
    DreiwegeWeiche(GleisIdRef<'t, DreiwegeWeiche>),
    KurvenWeiche(GleisIdRef<'t, KurvenWeiche>),
    SKurvenWeiche(GleisIdRef<'t, SKurvenWeiche>),
    Kreuzung(GleisIdRef<'t, Kreuzung>),
}

impl<'t> PartialEq<AnyIdRef<'t>> for AnyIdRef<'_> {
    fn eq(&self, other: &AnyIdRef<'t>) -> bool {
        match (self, other) {
            (AnyIdRef::Gerade(l0), AnyIdRef::Gerade(r0)) => l0 == r0,
            (AnyIdRef::Kurve(l0), AnyIdRef::Kurve(r0)) => l0 == r0,
            (AnyIdRef::Weiche(l0), AnyIdRef::Weiche(r0)) => l0 == r0,
            (AnyIdRef::DreiwegeWeiche(l0), AnyIdRef::DreiwegeWeiche(r0)) => l0 == r0,
            (AnyIdRef::KurvenWeiche(l0), AnyIdRef::KurvenWeiche(r0)) => l0 == r0,
            (AnyIdRef::SKurvenWeiche(l0), AnyIdRef::SKurvenWeiche(r0)) => l0 == r0,
            (AnyIdRef::Kreuzung(l0), AnyIdRef::Kreuzung(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl PartialEq<AnyId> for AnyIdRef<'_> {
    fn eq(&self, other: &AnyId) -> bool {
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

impl<'t> PartialEq<AnyIdRef<'t>> for AnyId {
    fn eq(&self, other: &AnyIdRef<'t>) -> bool {
        other.eq(self)
    }
}

impl AnyIdRef<'_> {
    /// Klone die Referenzen um eine neue Id zu erzeugen.
    pub(in crate::gleis::gleise) fn als_id(self) -> AnyId {
        match self {
            AnyIdRef::Gerade(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::Kurve(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::Weiche(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::DreiwegeWeiche(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::KurvenWeiche(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::SKurvenWeiche(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
            AnyIdRef::Kreuzung(gleis_id_ref) => AnyId::from(gleis_id_ref.als_id()),
        }
    }
}
