//! Schriftarten der Anwendung (Adobe-Source unter OFL Lizenz, Bootstrap Icons unter MIT Lizenz).

use std::include_bytes;

use iced::{
    alignment::{Horizontal, Vertical},
    widget::canvas::Text,
    Font, Point,
};
use iced_aw::graphics::icons::ICON_FONT_BYTES;

/// Schriftart ohne zusätzliche Eigenschaften.
pub static REGULAR: Font = Font::with_name("Source Serif 4");

/// Die Bytes für die Schriftart [REGULAR], damit sie von iced geladen werden kann.
pub static REGULAR_BYTES: &[u8] =
    include_bytes!("../../fonts/source-serif/TTF/SourceSerif4-Regular.ttf");

/// Die Bytes für die Schriftart [Bootstrap], damit sie von iced geladen werden kann.
pub static BOOTSTRAP_BYTES: &[u8] = include_bytes!("../../fonts/bootstrap-icons.otf");

/// Schriftart ohne zusätzliche Eigenschaften.
pub static BOOTSTRAP: Font = Font::with_name("bootstrap-icons");

/// Bytes von Schriftarten, die von iced geladen werden müssen ([iced::font::load]).
///
/// Werden sie nicht geladen kann es zu Darstellungsfehlern kommen.
pub static BENÖTIGTE_FONT_BYTES: &[&[u8]] = &[REGULAR_BYTES, ICON_FONT_BYTES, BOOTSTRAP_BYTES];

/// Die Standard-Schriftart, Größe und Ausrichtung für Text auf einem Canvas.
pub fn standard_text() -> Text {
    Text {
        font: REGULAR,
        size: 16.,
        position: Point::ORIGIN,
        horizontal_alignment: Horizontal::Center,
        vertical_alignment: Vertical::Center,
        ..Text::default()
    }
}
