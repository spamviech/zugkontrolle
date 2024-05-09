//! Sammlung der verwendeten Open-Source Lizenzen für die gesamte Zugkontrolle-Anwendung.
//!
//! Es ist ein eigenes crate, da Änderungen im crate mit der Definition zu langen
//! compile-Zeiten führen.
//! Durch die Isolation in ein eigenes crate wird das hoffentlich minimiert.

use std::collections::BTreeMap;

use once_cell::sync::Lazy;

use zugkontrolle_util::unicase_ord::UniCaseOrd;

/// Eine Map von Namen auf eine Funktion, die den Lizenztext erzeugt.
///
/// Die Namen werden mit [`UniCaseOrd`] geordnet.
pub type LizenzenMap = BTreeMap<UniCaseOrd<String>, &'static str>;

/// Alle Lizenzen für die aktuelle target-Platform.
pub static TARGET_LIZENZEN: Lazy<LizenzenMap> = Lazy::new(verwendete_lizenzen);

/// Alle Lizenzen für die aktuelle target-Platform.
static TARGET_NAME_VERSION_LIZENZ: &[(&str, &str, &str)] =
    zugkontrolle_lizenzen_macros::target_crate_lizenzen!();

/// Die Lizenzen der verwendeter Open-Source Bibliotheken für das aktuelle target.
#[must_use]
fn verwendete_lizenzen() -> LizenzenMap {
    TARGET_NAME_VERSION_LIZENZ
        .iter()
        .map(|(name, version, lizenz)| (UniCaseOrd::neu(format!("{name}-{version}")), *lizenz))
        .collect()
}
