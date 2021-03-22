//! anchor points to mark connection points of a rail

use std::collections::HashMap;

use rstar::Point;

use super::types::*;

pub type AnchorPointMap = HashMap<AnchorName, AnchorPoint>;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct AnchorName(pub String);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorPoint {
    pub position: AnchorPosition,
    pub direction: AnchorDirection,
}

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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorDirection {
    pub dx: CanvasX,
    pub dy: CanvasY,
}

pub(crate) fn with_anchor_name<const N: usize>(
    description: &str,
    anchor_points: [AnchorPoint; N],
) -> AnchorPointMap {
    let mut anchor_point_map = HashMap::new();
    for i in 0 .. N {
        let anchor_point = anchor_points[i];
        anchor_point_map.insert(AnchorName(format!("{}{}", description, i)), anchor_point);
    }
    anchor_point_map
}
