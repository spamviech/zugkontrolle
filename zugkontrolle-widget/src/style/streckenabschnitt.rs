//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use zugkontrolle_typen::farbe::Farbe;

use crate::style::{button::Button, container::Container};

/// Farbe des aktuellen Streckenabschnittes.
#[must_use]
pub fn anzeige_farbe(farbe: Farbe) -> Container {
    Container::Hintergrund { farbe: farbe.into() }
}

/// Kein aktueller Streckenabschnitt.
#[must_use]
pub fn anzeige_deaktiviert() -> Container {
    Container::hintergrund_grau(0.5)
}

/// Hintergrund der [`Auswahl`](crate::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
#[must_use]
pub fn auswahl_container(farbe: Farbe) -> Container {
    Container::Hintergrund { farbe: farbe.into() }
}

/// Hintergrund der [`Auswahl`](crate::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
#[must_use]
pub fn auswahl_button(farbe: Farbe) -> Button {
    Button::Hintergrund { farbe: farbe.into() }
}
