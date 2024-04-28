//! Icon für die Anwendung.

use std::include_bytes;

use iced::window::icon::{self, Icon};
use log::error;

/// Die Bild-Daten für das Programm-[`Icon`].
static DATA: &[u8] = include_bytes!("../../icon/zugkontrolle.data");
/// Die Breite der [Bild-Daten](DATA) in Pixeln.
static WIDTH: u32 = 32;
/// Die Höhe der [Bild-Daten](DATA) in Pixeln.
static HEIGHT: u32 = 32;

/// Das Icon der Anwendung.
#[must_use]
pub fn icon() -> Option<Icon> {
    match icon::from_rgba(Vec::from(DATA), WIDTH, HEIGHT) {
        Ok(icon) => Some(icon),
        Err(fehler) => {
            error!("Fehler beim Konvertieren des Application Icons: {:?}", fehler);
            None
        },
    }
}
