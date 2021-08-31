//! newtypes für einen iced::canvas::Frame und iced::canvas::Cache

use iced;
// re-exports
pub use iced::{
    canvas::{Fill, FillRule, Stroke, Text},
    Color, HorizontalAlignment, VerticalAlignment,
};
use serde::{Deserialize, Serialize};

use super::skalar::Skalar;
use super::vektor::Vektor;
use super::winkel::Winkel;

pub mod pfad;
pub use pfad::{Bogen, Pfad, Transformation};

/// newtype auf /iced::canvas::Frame/-Reference, dessen Methoden meine Typen verwenden.
///
/// Alle Koordinaten werden so transformiert, dass /pivot.punkt/ auf (0,0) vom /iced::Frame/ liegt.
/// Anschließend werden die Koordinaten um /pivot.winkel/ gedreht.
/// Danach werden alle Koordinaten mit dem /skalieren/-Faktor multipliziert.
pub struct Frame<'t>(&'t mut iced::canvas::Frame);
impl<'t> Frame<'t> {
    pub fn neu(frame: &'t mut iced::canvas::Frame) -> Self {
        Frame(frame)
    }

    /// Draws the stroke of the given Path on the Frame with the provided style.
    pub fn stroke(&mut self, Pfad { pfad, transformationen }: &Pfad, stroke: impl Into<Stroke>) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation)
            }
            frame.0.stroke(pfad, stroke)
        })
    }

    /// Draws the given Path on the Frame by filling it with the provided style.
    pub fn fill(&mut self, Pfad { pfad, transformationen }: &Pfad, fill: impl Into<Fill>) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation)
            }
            frame.0.fill(pfad, fill)
        })
    }

    /// Draws the characters of the given Text on the Frame, filling them with the given color.
    ///
    /// **Warning:** problems regarding transformation/rotation/scaling from `iced::canvas::Frame`
    /// apply here as well!
    #[inline(always)]
    pub fn fill_text(&mut self, text: impl Into<Text>) {
        self.0.fill_text(text)
    }

    /// Stores the current transform of the Frame and executes the given drawing operations,
    /// restoring the transform afterwards.
    ///
    /// This method is useful to compose transforms and perform drawing operations in different
    /// coordinate systems.
    #[inline(always)]
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        self.0.with_save(|frame| action(&mut Frame(frame)))
    }

    /// Wende die übergebene Transformation auf den Frame an.
    ///
    /// **ACHTUNG**: Durch die Art wie es in /iced/ implementiert ist wird die /transformation/
    /// **vor** allen bisherigen ausgeführt.
    ///
    /// Links zum Implementierung verfolgen:
    /// <https://github.com/hecrj/iced/blob/master/graphics/src/widget/canvas/frame.rs#L234>
    /// <https://docs.rs/lyon/0.17.5/lyon/math/type.Transform.html>
    /// <https://docs.rs/euclid/0.22.3/euclid/struct.Transform2D.html#method.pre_rotate>
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translation(Vektor { x, y }) => {
                self.0.translate(iced::Vector { x: x.0, y: y.0 })
            }
            Transformation::Rotation(winkel) => self.0.rotate(winkel.0),
            Transformation::Skalieren(scale) => self.0.scale(scale.0),
        }
    }
}

#[derive(Debug)]
pub struct Cache(iced::canvas::Cache);
impl Cache {
    pub fn neu() -> Self {
        Cache(iced::canvas::Cache::new())
    }

    /// Leere den Cache, so dass er neu gezeichnet wird.
    #[inline(always)]
    pub fn leeren(&mut self) {
        self.0.clear()
    }

    pub(crate) fn zeichnen_skaliert_von_pivot(
        &self,
        bounds: iced::Size<f32>,
        pivot: &Position,
        skalieren: &Skalar,
        draw_fn: impl Fn(&mut Frame),
    ) -> iced::canvas::Geometry {
        self.0.draw(bounds, |frame| {
            let mut boxed_frame = Frame(frame);
            boxed_frame.with_save(|f| {
                // pivot transformationen
                f.transformation(&Transformation::Skalieren(*skalieren));
                f.transformation(&Transformation::Rotation(pivot.winkel));
                f.transformation(&Transformation::Translation(-pivot.punkt));
                // zeichne auf Frame
                draw_fn(f)
            })
        })
    }

    /// Zeichne die `Geometry` über die übergebenen Closure und speichere sie im `Cache`.
    pub fn zeichnen(
        &self,
        bounds: iced::Size<f32>,
        draw_fn: impl Fn(&mut Frame),
    ) -> iced::canvas::Geometry {
        self.zeichnen_skaliert_von_pivot(
            bounds,
            &Position { punkt: Vektor::null_vektor(), winkel: Winkel(0.) },
            &Skalar::multiplikativ_neutral(),
            draw_fn,
        )
    }
}

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub punkt: Vektor,
    pub winkel: Winkel,
}
impl Position {
    /// Vektor nachdem das Objekt an die Position bewegt und um den Winkel gedreht wurde.
    pub fn transformation(&self, anchor: Vektor) -> Vektor {
        self.punkt + anchor.rotiert(self.winkel)
    }

    /// Vektor nachdem er um den Winkel gedreht wurde.
    pub fn rotation(&self, richtung: Vektor) -> Vektor {
        richtung.rotiert(self.winkel)
    }
}
