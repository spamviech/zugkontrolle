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
    bewegung: bool,
}

impl Bewegen {
    pub fn neu() -> Self {
        Bewegen { canvas: Cache::new(), bewegung: false }
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
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht>) {
        let mut nachricht = None;
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) => {
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
                    if position.x < links_grenze.0
                        && position.y >= oben_grenze.0
                        && position.y < unten_grenze.0
                    {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Links))
                    } else if position.x >= rechts_grenze.0
                        && position.y >= oben_grenze.0
                        && position.y < unten_grenze.0
                    {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Rechts))
                    } else if position.y < oben_grenze.0
                        && position.x >= links_grenze.0
                        && position.x < rechts_grenze.0
                    {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Oben))
                    } else if position.y >= unten_grenze.0
                        && position.x >= links_grenze.0
                        && position.x < rechts_grenze.0
                    {
                        self.bewegung = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Unten))
                    } else if klick_radius < radius {
                        nachricht = Some(Nachricht::Zurücksetzen)
                    }
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) if self.bewegung => {
                // beende nur gestartete Bewegungen
                self.bewegung = false;
                nachricht = Some(Nachricht::BeendeBewegung)
            }
            _ => {}
        }

        (iced::canvas::event::Status::Ignored, nachricht)
    }
}