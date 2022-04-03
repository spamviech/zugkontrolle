//! Alle Eigenschaften und bekannte Gleise für einen [Zugtyp].

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::GeradeUnit,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
            s_kurve::SKurvenWeicheUnit,
        },
    },
    typen::mm::Spurweite,
};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct Zugtyp<Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Die Leiter-Art des Zugtyps.
    pub leiter: PhantomData<fn() -> Leiter>,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [Geraden](crate::gleis::gerade::Gerade).
    pub geraden: Vec<GeradeUnit>,
    /// Alle unterstützten [Kurven](crate::gleis::kurve::Kurve).
    pub kurven: Vec<KurveUnit>,
    /// Alle unterstützten [Weichen](crate::gleis::weiche::gerade::Weiche).
    pub weichen: Vec<WeicheUnit>,
    /// Alle unterstützten [Dreiwege-Weichen](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    /// Alle unterstützten [Kurven-Weichen](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    /// Alle unterstützten [S-Kurven-Weichen](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    /// Alle unterstützten [Kreuzungen](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: Vec<KreuzungUnit>,
}

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZugtypSerialisiert {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
    pub leiter: String,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [Geraden](crate::gleis::gerade::Gerade).
    pub geraden: Vec<GeradeUnit>,
    /// Alle unterstützten [Kurven](crate::gleis::kurve::Kurve).
    pub kurven: Vec<KurveUnit>,
    /// Alle unterstützten [Weichen](crate::gleis::weiche::gerade::Weiche).
    pub weichen: Vec<WeicheUnit>,
    /// Alle unterstützten [Dreiwege-Weichen](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    /// Alle unterstützten [Kurven-Weichen](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    /// Alle unterstützten [S-Kurven-Weichen](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    /// Alle unterstützten [Kreuzungen](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: Vec<KreuzungUnit>,
}

/// Ein unterstützter Leiter, aktuell:
/// - [Mittelleiter](crate::steuerung::geschwindigkeit::Mittelleiter)
/// - [Zweileiter](crate::steuerung::geschwindigkeit::Zweileiter).
pub trait BekannterLeiter: Sized {
    /// Der Name des Leiters.
    const NAME: &'static str;

    /// Erzeuge einen Zugtyp mit der entsprechenden Leiter-Art, ausgehend von seinem Namen.
    fn bekannter_zugtyp(name: &str) -> Option<Zugtyp<Self>>;
}

/// Der Leiter stimmt nicht mit dem Namen überein.
#[derive(Debug, Clone)]
pub struct FalscherLeiter(pub String);

impl<Leiter: BekannterLeiter> TryFrom<ZugtypSerialisiert> for Zugtyp<Leiter> {
    type Error = FalscherLeiter;

    fn try_from(serialisiert: ZugtypSerialisiert) -> Result<Self, Self::Error> {
        let ZugtypSerialisiert {
            name,
            leiter,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = serialisiert;
        let gesucht = Leiter::NAME.to_owned();
        if leiter != gesucht {
            return Err(FalscherLeiter(leiter));
        }
        Ok(Zugtyp {
            name,
            leiter: PhantomData,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        })
    }
}

impl<Leiter: BekannterLeiter> From<Zugtyp<Leiter>> for ZugtypSerialisiert {
    fn from(zugtyp: Zugtyp<Leiter>) -> Self {
        let Zugtyp {
            name,
            leiter: _,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = zugtyp;
        let leiter = Leiter::NAME.to_owned();
        ZugtypSerialisiert {
            name,
            leiter,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}
