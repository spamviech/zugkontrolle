//! Ein leerer Datentyp ohne Konstruktoren.

use serde::{Deserialize, Serialize};

/// Ein leerer Datentyp ohne Konstruktoren.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Void {}

impl Void {
    /// Ein leeres `match` statement.
    #[inline(always)]
    pub fn unreachable(self) -> ! {
        match self {}
    }
}
