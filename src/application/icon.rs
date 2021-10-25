//! Icon for the Application

use std::include_bytes;

use iced::window::Icon;
use log::error;

static DATA: &[u8] = include_bytes!("../../Icon/Zugkontrolle.data");
static WIDTH: u32 = 32;
static HEIGHT: u32 = 32;

pub fn icon() -> Option<Icon> {
    match Icon::from_rgba(Vec::from(DATA), WIDTH, HEIGHT) {
        Ok(icon) => Some(icon),
        Err(fehler) => {
            error!("Fehler beim Konvertieren des Application Icons: {:?}", fehler);
            None
        }
    }
}
