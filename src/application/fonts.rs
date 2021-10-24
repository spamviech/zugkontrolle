//! Schriftarten der Anwendung (Adobe-Source unter OFL Lizenz)

use std::include_bytes;

/// Schriftart ohne zusätzliche Eigenschaften
pub static REGULAR: &[u8] = include_bytes!("../../fonts/SourceSerif4-Regular.ttf");
