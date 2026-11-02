//! Serialisierte Strukturen von Version 6.X.

use std::{collections::HashMap, time::Duration};

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::{
    gerade::GeradeUnit,
    kreuzung::KreuzungUnit,
    kurve::KurveUnit,
    steuerung::{
        geschwindigkeit::GeschwindigkeitSerialisiert,
        plan::PlanSerialisiert,
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    weiche::{
        dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
        s_kurve::SKurvenWeicheUnit,
    },
};
use zugkontrolle_typen::mm::Spurweite;
use zugkontrolle_util::eingeschränkt::{NichtNegativ, NullBisEins};

use crate::daten::v4::{GleiseDatenSerialisiert, geschwindigkeit, plan};

/// Streckenabschnitte und ihre Namen.
pub(in crate::daten) type StreckenabschnittMapSerialisiert = HashMap<
    streckenabschnitt::Name,
    (StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>),
>;
/// Geschwindigkeiten und ihre Namen.
pub(in crate::daten) type GeschwindigkeitMapSerialisiert =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert>;

/// Die serialisierbare Darstellung des aktuellen Zustandes, wie er in Version 4 verwendet wird.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct ZustandSerialisiert {
    /// Der serialisierbare Zugtyp.
    pub zugtyp: ZugtypSerialisiert,
    /// Die serialisierbaren Geschwindigkeiten.
    pub geschwindigkeiten: GeschwindigkeitMapSerialisiert,
    /// Die serialisierbaren Streckenabschnitte.
    pub streckenabschnitte: StreckenabschnittMapSerialisiert,
    /// Die serialisierbaren Gleise.
    pub gleise: GleiseDatenSerialisiert,
    /// Die serialisierbaren Pläne.
    pub pläne: HashMap<plan::Name, PlanSerialisiert>,
}

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
pub struct ZugtypSerialisiert {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
    pub leiter: String,
    /// [`Spurweite`].
    pub spurweite: Spurweite,
    /// Alle unterstützten [`Geraden`](crate::gleis::gerade::Gerade).
    pub geraden: HashMap<u32, GeradeUnit>,
    /// Alle unterstützten [`Kurven`](crate::gleis::kurve::Kurve).
    pub kurven: HashMap<u32, KurveUnit>,
    /// Alle unterstützten [`Weichen`](crate::gleis::weiche::gerade::Weiche).
    pub weichen: HashMap<u32, WeicheUnit>,
    /// Alle unterstützten [`Dreiwege-Weichen`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: HashMap<u32, DreiwegeWeicheUnit>,
    /// Alle unterstützten [`Kurven-Weichen`](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: HashMap<u32, KurvenWeicheUnit>,
    /// Alle unterstützten [`S-Kurven-Weichen`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: HashMap<u32, SKurvenWeicheUnit>,
    /// Alle unterstützten [`Kreuzungen`](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: HashMap<u32, KreuzungUnit>,
    /// Frequenz in Herz für den Pwm-Antrieb.
    pub pwm_frequenz: NichtNegativ,
    /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
    pub verhältnis_fahrspannung_überspannung: NullBisEins,
    /// Zeit zum Anhalten vor dem Umdrehen.
    pub stopp_zeit: Duration,
    /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
    pub umdrehen_zeit: Duration,
    /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
    pub schalten_zeit: Duration,
}
