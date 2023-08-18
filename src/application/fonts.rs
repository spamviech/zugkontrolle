//! Schriftarten der Anwendung (Adobe-Source unter OFL Lizenz).

use std::include_bytes;

use iced::{
    alignment::{Horizontal, Vertical},
    widget::canvas::Text,
    Font, Point,
};

/// Schriftart ohne zusätzliche Eigenschaften.
pub static REGULAR: Font = Font::with_name("Source Serif 4");
/// Die Bytes für die Schriftart [REGULAR], damit sie von iced geladen werden kann.
pub static REGULAR_BYTES: &[u8] =
    include_bytes!("../../fonts/source-serif/TTF/SourceSerif4-Regular.ttf");

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
