//! Widget zur Anpassung des Pivot Punktes

use iced::canvas::Program;

use crate::application::typen::*;

#[derive(Debug, Clone)]
pub enum Bewegung {
    Oben,
    Unten,
    Links,
    Rechts,
}
impl Bewegung {
    fn vektor(self) -> Vektor {
        match self {
            Bewegung::Oben => -vektor::EY,
            Bewegung::Unten => vektor::EY,
            Bewegung::Links => -vektor::EX,
            Bewegung::Rechts => vektor::EX,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Nachricht {
    StarteBewegung(Bewegung),
    BeendeBewegung,
    Zurücksetzen,
}

pub struct Bewegen {
    canvas: Cache,
}

impl Bewegen {
    pub fn neu() -> Self {
        Bewegen { canvas: Cache::new() }
    }
}

impl Program<Nachricht> for Bewegen {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let size = bounds.size();
        let width = Skalar(size.width);
        let height = Skalar(size.height);
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        // Startpunkte
        let links = Vektor { x: Skalar(0.), y: half_height };
        let rechts = Vektor { x: width, y: half_height };
        let oben = Vektor { x: half_width, y: Skalar(0.) };
        let unten = Vektor { x: half_width, y: height };
        let zentrum = Vektor { x: half_width, y: half_height };
        // relative Bewegung
        let diagonal_runter = Skalar(0.3) * Vektor { x: half_width, y: half_height };
        let mut diagonal_hoch = diagonal_runter;
        diagonal_hoch.y = -diagonal_hoch.y;
        // Inkreis-Radius r = 2A/u
        // https://de.wikipedia.org/wiki/Inkreis
        let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
        // erzeuge Pfad
        let mut erbauer = pfad::Erbauer::neu();
        // links
        erbauer.move_to(links);
        erbauer.line_to(links + diagonal_hoch);
        erbauer.move_to(links);
        erbauer.line_to(links + diagonal_runter);
        // rechts
        erbauer.move_to(rechts);
        erbauer.line_to(rechts - diagonal_hoch);
        erbauer.move_to(rechts);
        erbauer.line_to(rechts - diagonal_runter);
        // oben
        erbauer.move_to(oben);
        erbauer.line_to(oben - diagonal_hoch);
        erbauer.move_to(oben);
        erbauer.line_to(oben + diagonal_runter);
        // unten
        erbauer.move_to(unten);
        erbauer.line_to(unten + diagonal_hoch);
        erbauer.move_to(unten);
        erbauer.line_to(unten - diagonal_runter);
        // zurücksetzen
        erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU });
        let pfad = erbauer.baue();
        vec![self.canvas.draw(size, |frame| frame.stroke(&pfad, canvas::Stroke::default()))]
    }

    fn update(
        &mut self,
        _event: iced::canvas::Event,
        _bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht>) {
        // unimplemented!("update")
        (iced::canvas::event::Status::Ignored, None)
    }
}
