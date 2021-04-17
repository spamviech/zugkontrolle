//! anchor points to mark connection points of a rail

use rstar;

use crate::gleis::types::*;

/// Ein /Anchor/ repräsentiert Anschlüsse eines Gleises.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Anchor {
    pub position: canvas::Point,
    pub direction: canvas::Vector,
}
// copy+paste from example implementation for IntegerPoint
impl rstar::Point for canvas::Point {
    type Scalar = f32;

    const DIMENSIONS: usize = 2;

    fn generate(generator: impl Fn(usize) -> Self::Scalar) -> Self {
        canvas::Point { x: canvas::X(generator(0)), y: canvas::Y(generator(1)) }
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
