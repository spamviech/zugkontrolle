//! Serialisierbare Darstellung einer [Gerade] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{steuerung::kontakt::KontaktSerialisiert, typen::skalar::Skalar};

/// Definition einer Gerade.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GeradeSerialisiert {
    /// Die Länge der Gerade auf dem [Canvas](iced::widget::canvas::Canvas).
    pub länge: Skalar,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [Kontakt] an der Schiene.
    pub kontakt: Option<KontaktSerialisiert>,
}
