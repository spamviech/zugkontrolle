//! Serialisierbare Darstellung eines [`Zugtyps`](crate::zugtyp::Zugtyp) in Version 3.

use std::{collections::HashMap, fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::steuerung::geschwindigkeit::Leiter;
use zugkontrolle_id::eindeutig::KeineIdVerfügbar;
use zugkontrolle_typen::mm::Spurweite;
use zugkontrolle_util::{eingeschränkt::NichtNegativ, enumerate_checked::EnumerateCheckedExt};

use crate::gleise::daten::{
    v3::{
        gerade::GeradeUnit,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
            s_kurve::SKurvenWeicheUnit,
        },
    },
    v4,
};

// Folge Konvention TypName -> TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[serde(bound(
    serialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize",
    deserialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>",
))]
pub struct ZugtypSerialisiert<L: Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
    pub leiter: String,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [`Geraden`](crate::gleis::gerade::Gerade).
    pub geraden: Vec<GeradeUnit>,
    /// Alle unterstützten [`Kurven`](crate::gleis::kurve::Kurve).
    pub kurven: Vec<KurveUnit>,
    /// Alle unterstützten [`Weichen`](crate::gleis::weiche::gerade::Weiche).
    pub weichen: Vec<WeicheUnit>,
    /// Alle unterstützten [`Dreiwege-Weichen`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    /// Alle unterstützten [`Kurven-Weichen`](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    /// Alle unterstützten [`S-Kurven-Weichen`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    /// Alle unterstützten [`Kreuzungen`](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: Vec<KreuzungUnit>,
    /// Frequenz in Herz für den Pwm-Antrieb.
    pub pwm_frequenz: NichtNegativ,
    /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
    pub verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    /// Zeit zum Anhalten vor dem Umdrehen.
    pub stopp_zeit: Duration,
    /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
    pub umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
    pub schalten_zeit: Duration,
}

impl<L: Leiter> ZugtypSerialisiert<L> {
    /// Konvertiere in die serialisierbare Darstellung eines [`Zugtyp`],
    /// wie er in Version 4 verwendet wird.
    pub(crate) fn v4(self, fehler: &mut Vec<KeineIdVerfügbar>) -> v4::ZugtypSerialisiert<L> {
        /// Erstelle die Maps mit einer eindeutigen [`id::Repräsentation`] als Schlüssel.
        macro_rules! erstelle_maps {
            ($($gleis_art: ident : $typ: ident),* $(,)?) => {{
                let ZugtypSerialisiert {
                    name,
                    leiter,
                    spurweite,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    schalten_zeit,
                    $($gleis_art),*
                } = self;
                $(
                    let $gleis_art = $gleis_art.into_iter().enumerate_checked().fold(
                        HashMap::new(),
                        |mut elemente, (id, element)| {
                            if let Some(id) = id {
                                let _ = elemente.insert(id, element.into());
                            } else {
                                fehler.push(KeineIdVerfügbar::für::<$typ>());
                            }
                            elemente
                        }
                    );
                )*
                v4::ZugtypSerialisiert {
                    name,
                    leiter,
                    spurweite,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    schalten_zeit,
                    $($gleis_art),*
                }
            }};
        }

        erstelle_maps!(
            geraden : GeradeUnit,
            kurven : KurveUnit,
            weichen : WeicheUnit,
            dreiwege_weichen : DreiwegeWeicheUnit,
            kurven_weichen : KurvenWeicheUnit,
            s_kurven_weichen : SKurvenWeicheUnit,
            kreuzungen : KreuzungUnit,
        )
    }
}

impl<L: Leiter> v4::ZugtypSerialisiert<L> {
    /// Konvertiere in die serialisierbare Darstellung eines [`Zugtyp`],
    /// wie er in Version 3 verwendet wird.
    pub(crate) fn v3(self) -> ZugtypSerialisiert<L> {
        /// Konvertiere die id-maps in [`Vecs`](Vec).
        macro_rules! erstelle_vecs {
            ($($gleis_art: ident : $typ: ident),* $(,)?) => {{
                let v4::ZugtypSerialisiert {
                    name,
                    leiter,
                    spurweite,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    schalten_zeit,
                    $($gleis_art),*
                } = self;
                $(
                    let $gleis_art = $gleis_art.into_values().map(Into::into).collect();
                )*
                ZugtypSerialisiert {
                    name,
                    leiter,
                    spurweite,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    schalten_zeit,
                    $($gleis_art),*
                }
            }};
        }

        erstelle_vecs!(
            geraden : GeradeUnit,
            kurven : KurveUnit,
            weichen : WeicheUnit,
            dreiwege_weichen : DreiwegeWeicheUnit,
            kurven_weichen : KurvenWeicheUnit,
            s_kurven_weichen : SKurvenWeicheUnit,
            kreuzungen : KreuzungUnit,
        )
    }
}
