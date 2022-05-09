//! Schriftarten der Anwendung (Adobe-Source unter OFL Lizenz).

use std::{include_bytes, include_str};

/// Schriftart ohne zusätzliche Eigenschaften.
pub static REGULAR: &[u8] = include_bytes!("../../fonts/source-serif/TTF/SourceSerif4-Regular.ttf");

/// OFL-Lizenz der Adobe-Source Schriftarten.
pub static LICENSE: &str = include_str!("../../fonts/source-serif/LICENSE.md");
