//! Serialisierbare Darstellung einer [`Gerade`] in Version 3.

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::{gerade as v4, steuerung::kontakt::KontaktSerialisiert};
use zugkontrolle_typen::skalar::Skalar;

// Folge Konvention TypName -> TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Definition einer Gerade.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct GeradeSerialisiert<Anschluss = Option<KontaktSerialisiert>> {
    /// Die Länge der Gerade auf dem [`Canvas`](iced::widget::canvas::Canvas).
    pub länge: Skalar,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [`Kontakt`] an der Schiene.
    pub kontakt: Anschluss,
}

// Folge Konvention TypName -> TypNameUnit
#[allow(clippy::module_name_repetitions)]
/// Eine Variante ohne Anschlüsse.
pub type GeradeUnit = GeradeSerialisiert<()>;

impl<A> From<GeradeSerialisiert<A>> for v4::GeradeUnit {
    fn from(gerade: GeradeSerialisiert<A>) -> Self {
        let GeradeSerialisiert { länge, beschreibung, kontakt: _ } = gerade;
        v4::GeradeUnit { länge, beschreibung, kontakt: () }
    }
}

impl From<v4::GeradeUnit> for GeradeUnit {
    fn from(gerade: v4::GeradeUnit) -> Self {
        let v4::GeradeUnit { länge, beschreibung, kontakt } = gerade;
        GeradeSerialisiert { länge, beschreibung, kontakt }
    }
}
