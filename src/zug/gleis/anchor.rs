//! anchor points to mark connection points of a rail

use nonempty::NonEmpty;
use rstar::{primitives::PointWithData, Point, RTree};
use std::collections::HashMap;
use std::marker::PhantomData;

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
    pub x: f64,
    pub y: f64,
}
// copy+paste from example implementation for IntegerPoint
impl Point for AnchorPosition {
    type Scalar = f64;
    const DIMENSIONS: usize = 2;

    fn generate(generator: impl Fn(usize) -> Self::Scalar) -> Self {
        AnchorPosition { x: generator(0), y: generator(1) }
    }

    fn nth(&self, index: usize) -> Self::Scalar {
        match index {
            0 => self.x,
            1 => self.y,
            _ => unreachable!(),
        }
    }

    fn nth_mut(&mut self, index: usize) -> &mut Self::Scalar {
        match index {
            0 => &mut self.x,
            1 => &mut self.y,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnchorDirection {
    pub x: f64,
    pub y: f64,
}

/// R-Tree of all anchor points, specifying the corresponding widget
pub type AnchorPointRTree<Z> = RTree<PointWithData<NonEmpty<GleisId<Z>>, AnchorPosition>>;

/// Identifier for corresponding widget
/// TODO: use Mutex instead?
pub struct GleisId<Z>(u64, PhantomData<Z>);
