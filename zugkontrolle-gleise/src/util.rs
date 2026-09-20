//! Mehrfach verwendete Utility Funktionen.

use iced::{Rectangle, mouse::Cursor};

use zugkontrolle_typen::{canvas::Position, skalar::Skalar, vektor::Vektor};

/// Position des Cursors auf einem canvas mit `bounds`.
pub(crate) fn berechne_canvas_position(
    bounds: &Rectangle,
    cursor: &Cursor,
    pivot: &Position,
    skalieren: Skalar,
) -> Option<Vektor> {
    // `position_in` gibt nur in-bounds einen `Some`-Wert zurück.
    // `position` hat diese Einschränkung nicht,
    // dafür muss die Position explizit abgezogen werden.
    cursor.position().map(|pos| {
        let relative_position = Vektor { x: Skalar(pos.x - bounds.x), y: Skalar(pos.y - bounds.y) };
        #[allow(
            clippy::arithmetic_side_effects,
            reason = "Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt."
        )]
        {
            pivot.punkt + (relative_position / skalieren).rotiert(&(-pivot.winkel))
        }
    })
}
