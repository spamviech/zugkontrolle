//! Widget zur Anpassung des Pivot Punktes

use iced::{
    canvas::{event, Cursor, Event, Geometry, Program},
    mouse, Rectangle,
};

use crate::application::typen::*;

#[derive(Debug, Clone, Copy)]
pub enum Bewegung {
    Oben,
    Unten,
    Links,
    Rechts,
    ObenLinks,
    ObenRechts,
    UntenLinks,
    UntenRechts,
}
impl Bewegung {
    pub fn vektor(self, radius: Skalar) -> Vektor {
        Vektor::polar_koordinaten(
            radius,
            match self {
                Bewegung::Rechts => 0.,
                Bewegung::UntenRechts => 0.25,
                Bewegung::Unten => 0.5,
                Bewegung::UntenLinks => 0.75,
                Bewegung::Links => 1.,
                Bewegung::ObenLinks => 1.25,
                Bewegung::Oben => 1.5,
                Bewegung::ObenRechts => 1.75,
            } * winkel::PI,
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    StarteBewegung(Bewegung),
    BeendeBewegung,
    Zurücksetzen,
}

#[derive(Debug)]
pub struct Bewegen {
    canvas: Cache,
    bewegung: bool,
}

impl Bewegen {
    pub fn neu() -> Self {
        Bewegen { canvas: Cache::neu(), bewegung: false }
    }
}

impl Program<Nachricht> for Bewegen {
    fn draw(&self, bounds: Rectangle, _cursor: Cursor) -> Vec<Geometry> {
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
        vec![self.canvas.zeichnen(size, |frame| frame.stroke(&pfad, canvas::Stroke::default()))]
    }

    fn update(
        &mut self,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        let mut nachricht = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(&bounds) {
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
                    // Grenzen
                    let links_grenze = links.x + diagonal_runter.x;
                    let rechts_grenze = rechts.x - diagonal_runter.x;
                    let oben_grenze = oben.y + diagonal_runter.y;
                    let unten_grenze = unten.y - diagonal_runter.y;
                    // Inkreis-Radius r = 2A/u
                    // https://de.wikipedia.org/wiki/Inkreis
                    let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
                    let klick_radius =
                        (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
                    if position.x < links_grenze.0 {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(if position.y < oben_grenze.0 {
                            Bewegung::ObenLinks
                        } else if position.y > unten_grenze.0 {
                            Bewegung::UntenLinks
                        } else {
                            Bewegung::Links
                        }))
                    } else if position.x >= rechts_grenze.0 {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(if position.y < oben_grenze.0 {
                            Bewegung::ObenRechts
                        } else if position.y >= unten_grenze.0 {
                            Bewegung::UntenRechts
                        } else {
                            Bewegung::Rechts
                        }))
                    } else if position.y < oben_grenze.0 {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Oben))
                    } else if position.y >= unten_grenze.0 {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Unten))
                    } else if klick_radius < radius {
                        nachricht = Some(Nachricht::Zurücksetzen)
                    }
                }
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) if self.bewegung => {
                // beende nur gestartete Bewegungen
                self.bewegung = false;
                nachricht = Some(Nachricht::BeendeBewegung)
            },
            _ => {},
        }

        (event::Status::Ignored, nachricht)
    }
}
