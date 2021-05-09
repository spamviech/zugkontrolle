//! newtypes für einen cairo::Context

use iced;
// re-exports
pub use iced::{
    canvas::{Fill, FillRule, Stroke, Text},
    Color,
    HorizontalAlignment,
    VerticalAlignment,
};
use serde::{Deserialize, Serialize};

use super::winkel::Winkel;

pub mod pfad;
pub use pfad::{Bogen, Pfad, Transformation};

pub mod vektor;
pub use vektor::Vektor;
pub mod skalar;
pub use skalar::Skalar;

/// newtype auf /iced::canvas::Frame/-Reference, dessen Methoden meine Typen verwenden.
///
/// Alle Koordinaten werden so transformiert, dass /pivot.punkt/ auf (0,0) vom /iced::Frame/ liegt.
/// Anschließend werden die Koordinaten um /pivot.winkel/ gedreht.
/// Danach werden alle Koordinaten mit dem /skalieren/-Faktor multipliziert.
pub struct Frame<'t> {
    frame: &'t mut iced::canvas::Frame,
    pivot: &'t Position,
    skalieren: &'t Skalar,
}
impl<'t> Frame<'t> {
    pub fn with_new_frame(
        size: iced::Size<f32>,
        draw_fn: impl Fn(&mut Frame),
    ) -> iced::canvas::Geometry {
        let mut frame = iced::canvas::Frame::new(size);
        let mut boxed_frame = Frame {
            frame: &mut frame,
            pivot: &Position { punkt: Vektor::null_vektor(), winkel: Winkel(0.) },
            skalieren: &Skalar(0.),
        };
        boxed_frame.with_save(|f| {
            f.pivot_transformationen();
            draw_fn(f)
        });
        frame.into_geometry()
    }

    fn pivot_transformationen(&mut self) {
        self.transformation(&Transformation::Translation(self.pivot.punkt));
        self.transformation(&Transformation::Rotation(self.pivot.winkel));
        self.transformation(&Transformation::Translation(
            Skalar(-2.) * self.pivot.punkt.rotiere(-self.pivot.winkel),
        ));
        self.transformation(&Transformation::Skalieren(
            Skalar::multiplikativ_neutral() / self.skalieren,
        ));
    }

    /// Draws the stroke of the given Path on the Frame with the provided style.
    pub fn stroke(&mut self, Pfad { pfad, transformationen }: &Pfad, stroke: impl Into<Stroke>) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation)
            }
            frame.frame.stroke(pfad, stroke)
        })
    }

    /// Draws the given Path on the Frame by filling it with the provided style.
    pub fn fill(&mut self, Pfad { pfad, transformationen }: &Pfad, fill: impl Into<Fill>) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation)
            }
            frame.frame.fill(pfad, fill)
        })
    }

    /// Draws the characters of the given Text on the Frame, filling them with the given color.
    ///
    /// **Warning:** problems regarding transformation/rotation/scaling from /iced::canvas::Frame/
    /// apply here as well!
    pub fn fill_text(&mut self, text: impl Into<Text>) {
        // TODO respect pivot point
        self.frame.fill_text(text)
    }

    /// Stores the current transform of the Frame and executes the given drawing operations,
    /// restoring the transform afterwards.
    ///
    /// This method is useful to compose transforms and perform drawing operations in different
    /// coordinate systems.
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        let Frame { frame, pivot, skalieren } = self;
        frame.with_save(|frame| action(&mut Frame { frame, pivot, skalieren }))
    }

    /// Wende die übergebene Transformation auf den Frame an.
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translation(vektor) => {
                self.frame.translate(Vektor::zu_iced(*vektor, self.pivot, self.skalieren))
            },
            Transformation::Rotation(winkel) => self.frame.rotate(winkel.0),
            Transformation::Skalieren(scale) => self.frame.scale(scale.0),
        }
    }
}

#[derive(Debug)]
pub struct Cache(iced::canvas::Cache);
impl Cache {
    pub fn new() -> Self {
        Cache(iced::canvas::Cache::new())
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn draw(
        &self,
        bounds: iced::Size<f32>,
        pivot: &Position,
        skalieren: &Skalar,
        draw_fn: impl Fn(&mut Frame),
    ) -> iced::canvas::Geometry {
        self.0.draw(bounds, |frame| {
            let mut boxed_frame = Frame { frame, pivot, skalieren };
            boxed_frame.with_save(|f| {
                f.pivot_transformationen();
                draw_fn(f)
            })
        })
    }

    pub fn draw_unskaliert(
        &self,
        bounds: iced::Size<f32>,
        draw_fn: impl Fn(&mut Frame),
    ) -> iced::canvas::Geometry {
        self.draw(
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
        self.punkt + anchor.rotiere(self.winkel)
    }

    /// Vektor nachdem er um den Winkel gedreht wurde.
    pub fn rotation(&self, richtung: Vektor) -> Vektor {
        richtung.rotiere(self.winkel)
    }
}
