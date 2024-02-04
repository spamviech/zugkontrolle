//! Ein leerer Datentyp ohne Konstruktoren.

use serde::{Deserialize, Serialize};

/// Ein leerer Datentyp ohne Konstruktoren.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Void {}

impl Void {
    /// Ein leeres `match` statement.

    pub fn unreachable(self) -> ! {
        match self {}
    }
}
