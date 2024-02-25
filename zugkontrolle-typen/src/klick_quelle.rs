//! Der Auslöser eines Klicks.

use iced_core::touch::Finger;

/// Der Auslöser eines Klicks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KlickQuelle {
    /// Der Klick wurde mit der Maus ausgelöst.
    Maus,
    /// Der Klick wurde mit einem Finger ausgelöst.
    Touch(Finger),
}
