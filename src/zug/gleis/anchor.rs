//! anchor points to mark connection points of a rail

use std::collections::HashMap;

use rstar::Point;

use super::types::*;

/// Sammlung aller AnchorPoint für ein Gleis
/// FIXME create own map type with non-Option lookups
/// Creation only when all are present
/// Traits tbd.
pub type AnchorPointMap<AnchorName> = HashMap<AnchorName, AnchorPoint>;

// /// Bezeichner für einen AnchorPoint
// #[derive(Debug, Hash, PartialEq, Eq)]
// pub struct AnchorName(pub String);

/// Ein AnchorPoint repräsentiert Anschlüsse eines Gleises.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorPoint {
    pub position: AnchorPosition,
    pub direction: AnchorDirection,
}

/// Anschluss-Position wenn startend bei (0,0) auf dem Canvas gezeichnet wird.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorPosition {
    pub x: CanvasX,
    pub y: CanvasY,
}
// copy+paste from example implementation for IntegerPoint
impl Point for AnchorPosition {
    type Scalar = f64;

    const DIMENSIONS: usize = 2;

    fn generate(generator: impl Fn(usize) -> Self::Scalar) -> Self {
        AnchorPosition { x: CanvasX(generator(0)), y: CanvasY(generator(1)) }
    }

    fn nth(&self, index: usize) -> Self::Scalar {
        match index {
            0 => self.x.0,
            1 => self.y.0,
            _ => unreachable!(),
        }
    }

    fn nth_mut(&mut self, index: usize) -> &mut Self::Scalar {
        match index {
            0 => &mut self.x.0,
            1 => &mut self.y.0,
            _ => unreachable!(),
        }
    }
}

/// Anschluss-Richtung (ausgehend)
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorDirection {
    pub dx: CanvasX,
    pub dy: CanvasY,
}

/*
FIXME requires min_const_generics, might come to stable soon
https://github.com/rust-lang/rust/pull/79135
The version target is 1.50 (2020-12-31 => beta, 2021-02-11 => stable) 1.51 (2021-02-11 => beta,
2021-03-25 => stable).

enums are probably better anyway

/// Hilfsfunktion zur automatischen AnchorName-Erzeugung.
pub(crate) fn with_anchor_name<const N: usize>(
    description: &str,
    anchor_points: [AnchorPoint; N],
) -> AnchorPointMap<String> {
    let mut anchor_point_map = HashMap::new();
    for i in 0 .. N {
        let anchor_point = anchor_points[i];
        anchor_point_map.insert(AnchorName(format!("{}{}", description, i)), anchor_point);
    }
    anchor_point_map
}
*/
