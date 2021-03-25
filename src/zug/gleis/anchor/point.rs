//! anchor points to mark connection points of a rail

use rstar;

use crate::zug::gleis::types::*;

/// Ein Point repräsentiert Anschlüsse eines Gleises.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Point {
    pub position: Position,
    pub direction: Direction,
}

/// Anschluss-Position wenn startend bei (0,0) auf dem Canvas gezeichnet wird.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub x: CanvasX,
    pub y: CanvasY,
}
// copy+paste from example implementation for IntegerPoint
impl rstar::Point for Position {
    type Scalar = f64;

    const DIMENSIONS: usize = 2;

    fn generate(generator: impl Fn(usize) -> Self::Scalar) -> Self {
        Position { x: CanvasX(generator(0)), y: CanvasY(generator(1)) }
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
pub struct Direction {
    pub dx: CanvasX,
    pub dy: CanvasY,
}
