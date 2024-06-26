//! Alle Eigenschaften und bekannte Gleise für einen [`Zugtyp`].

// only way to export macros from a module
#![allow(clippy::pub_use)]

use std::{collections::HashMap, fmt::Debug, marker::PhantomData, time::Duration};

use zugkontrolle_typen::mm::Spurweite;
use zugkontrolle_util::eingeschränkt::NichtNegativ;

use crate::{
    gerade::Gerade,
    id::DefinitionId,
    kreuzung::Kreuzung,
    kurve::Kurve,
    steuerung::{aktualisieren::MitSteuerung, geschwindigkeit::Leiter},
    weiche::{
        dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
    },
};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Die Definitionen für den Typ `T`.
pub type DefinitionMap<T> = HashMap<DefinitionId<T>, <T as MitSteuerung>::SelfUnit>;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct Zugtyp<L: Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Die Leiter-Art des Zugtyps.
    pub leiter: PhantomData<fn() -> L>,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [`Geraden`](crate::gleis::gerade::Gerade).
    pub geraden: DefinitionMap<Gerade>,
    /// Alle unterstützten [`Kurven`](crate::gleis::kurve::Kurve).
    pub kurven: DefinitionMap<Kurve>,
    /// Alle unterstützten [`Weichen`](crate::gleis::weiche::gerade::Weiche).
    pub weichen: DefinitionMap<Weiche>,
    /// Alle unterstützten [`Dreiwege-Weichen`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: DefinitionMap<DreiwegeWeiche>,
    /// Alle unterstützten [`Kurven-Weichen`](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: DefinitionMap<KurvenWeiche>,
    /// Alle unterstützten [`S-Kurven-Weichen`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: DefinitionMap<SKurvenWeiche>,
    /// Alle unterstützten [`Kreuzungen`](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: DefinitionMap<Kreuzung>,
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

#[macro_export]
/// Erzeuge Maps mit [`DefinitionId`] für die Definitionen.
macro_rules! erzeuge_zugtyp_maps {
    ($id_maps: expr => $($gleise: ident : $typ: ty),* $(,)?) => {
        $(
        #[allow(unused_qualifications)]
        let ($gleise, ids) = $gleise
            .into_iter()
            .fold(
                Ok((HashMap::new(), HashMap::new())),
                |acc, (gespeicherte_id, definition)| -> Result<_, ZugtypDeserialisierenFehler> {
                    if let Ok((mut gleise, mut ids)) = acc {
                        let id = $crate::id::DefinitionId::<$typ>::neu()?;
                        // gespeicherte_id ist eindeutig, da es der Schlüssel einer HashMap war
                        let _ = ids.insert(gespeicherte_id, id.clone());
                        // id ist eindeutig, da es von GleisId::neu garantiert wird
                        let _ = gleise.insert(id, definition);
                        Ok((gleise, ids))
                    } else {
                        acc
                    }
                }
            )?;
        $id_maps.$gleise = ids;
        )*
    };
    ($($gleise: ident : $typ: ty | $expect_msg: literal),* $(,)? : $error: ty) => {$(
        #[allow(unused_qualifications)]
        let $gleise = $gleise
            .into_iter()
            .map(|definition| Ok(($crate::id::DefinitionId::<$typ>::neu()?, definition)) )
            .collect::< Result<$crate::zugtyp::DefinitionMap<$typ>, $error>>().expect($expect_msg);
    )*};
}
pub use crate::erzeuge_zugtyp_maps;
