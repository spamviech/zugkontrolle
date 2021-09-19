//! Ids zur Identifikation der Gleise.

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
#[derive(Debug)]
pub struct GleisId<T: Zeichnen> {
    position: Rectangle<Vektor>,
    streckenabschnitt: Option<streckenabschnitt::Name>,
    verbindungen: T::Verbindungen,
}
impl<T> GleisId<T>
where
    T: Zeichnen,
    T::Verbindungen: Clone,
{
    // Als Methode definiert, damit es privat bleibt.
    pub(in crate::application) fn clone(&self) -> Self {
        GleisId {
            position: self.position.clone(),
            streckenabschnitt: self.streckenabschnitt.clone(),
            verbindungen: self.verbindungen.clone(),
        }
    }
}

impl<T: Zeichnen> GleisId<T> {
    /// Alle Verbindungen des Assoziierten Gleises.
    pub fn verbindungen(&self) -> &T::Verbindungen {
        &self.verbindungen
    }
}

// FIXME remove
// // Explizite Implementierung wegen Phantomtyp benötigt.
// // Die automatisch erzeugte Instanz (derive) würden den jeweiligen Trait für den Phantomtyp benötigen.
// impl<T> PartialEq for GleisId<T> {
//     fn eq(&self, other: &Self) -> bool {
//         self.0 == other.0
//     }
// }
// impl<T> Eq for GleisId<T> {}
// impl<T> PartialOrd for GleisId<T> {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         self.0.partial_cmp(&other.0)
//     }
// }
// impl<T> Ord for GleisId<T> {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.0.cmp(&other.0)
//     }
// }
// impl<T> Hash for GleisId<T> {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.0.hash(state)
//     }
// }

#[derive(zugkontrolle_derive::Debug)]
pub enum AnyId<Z: Zugtyp> {
    Gerade(GleisId<Gerade<Z>>),
    Kurve(GleisId<Kurve<Z>>),
    Weiche(GleisId<Weiche<Z>>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche<Z>>),
    KurvenWeiche(GleisId<KurvenWeiche<Z>>),
    SKurvenWeiche(GleisId<SKurvenWeiche<Z>>),
    Kreuzung(GleisId<Kreuzung<Z>>),
}

// FIXME remove
// // Explizite Implementierung wegen Phantomtyp benötigt.
// // Die automatisch erzeugte Instanz (derive) würden den jeweiligen Trait für den Phantomtyp benötigen.
// impl<Z> PartialEq for AnyId<Z> {
//     fn eq(&self, other: &Self) -> bool {
//         use AnyId::*;
//         match (self, other) {
//             (Gerade(id0), Gerade(id1)) => id0 == id1,
//             (Kurve(id0), Kurve(id1)) => id0 == id1,
//             (Weiche(id0), Weiche(id1)) => id0 == id1,
//             (DreiwegeWeiche(id0), DreiwegeWeiche(id1)) => id0 == id1,
//             (KurvenWeiche(id0), KurvenWeiche(id1)) => id0 == id1,
//             (SKurvenWeiche(id0), SKurvenWeiche(id1)) => id0 == id1,
//             (Kreuzung(id0), Kreuzung(id1)) => id0 == id1,
//             _ => false,
//         }
//     }
// }
// impl<Z> Eq for AnyId<Z> {}
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
