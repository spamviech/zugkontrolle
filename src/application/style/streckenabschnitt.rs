//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use crate::{
    application::style::{button::Button, container::Container},
    typen::farbe::Farbe,
};

/// Farbe des aktuellen Streckenabschnittes.
pub fn anzeige_farbe(farbe: Farbe) -> Container {
    Container::Hintergrund { farbe: farbe.into() }
}

/// Kein aktueller Streckenabschnitt.
pub fn anzeige_deaktiviert() -> Container {
    Container::hintergrund_grau(0.5)
}

/// Hintergrund der [Auswahl](crate::application::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
pub fn auswahl_container(farbe: Farbe) -> Container {
    Container::Hintergrund { farbe: farbe.into() }
}

/// Hintergrund der [Auswahl](crate::application::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
pub fn auswahl_button(farbe: Farbe) -> Button {
    Button::Hintergrund { farbe: farbe.into() }
}
