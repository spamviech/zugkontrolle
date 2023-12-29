//! Schriftarten der Anwendung (Adobe-Source unter OFL Lizenz).

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

/// Schriftart ohne zusätzliche Eigenschaften.
pub static EMOJI: Font = Font::with_name("Noto Color Emoji");

/// Die Bytes für die Schriftart [REGULAR], damit sie von iced geladen werden kann.
pub static EMOJI_BYTES: &[u8] = include_bytes!("../../fonts/noto-emoji/fonts/NotoColorEmoji.ttf");

/// Bytes von Schriftarten, die von iced geladen werden müssen ([iced::font::load]).
///
/// Werden sie nicht geladen kann es zu Darstellungsfehlern kommen.
pub static BENÖTIGTE_FONT_BYTES: &[&[u8]] = &[REGULAR_BYTES, EMOJI_BYTES, ICON_FONT_BYTES];

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
