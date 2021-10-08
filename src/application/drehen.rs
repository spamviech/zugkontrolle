//! Widget zum Einstellen des Anzeigewinkels

use iced::canvas::Program;

use crate::application::typen::*;

#[derive(Debug)]
pub struct Drehen {
    canvas: Cache,
    winkel: Winkel,
    grabbed: bool,
}

impl Drehen {
    pub fn neu() -> Self {
        Drehen { canvas: Cache::neu(), winkel: winkel::ZERO, grabbed: false }
    }
}

impl Program<Winkel> for Drehen {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let size = bounds.size();
        vec![self.canvas.zeichnen(size, |frame| {
            let min_width_height = Skalar(size.width.min(size.height));
            let half_min_width_height = min_width_height.halbiert();
            let kreis_zentrum = Vektor { x: half_min_width_height, y: half_min_width_height };
            let kreis_radius = Skalar(0.8) * half_min_width_height;
            let kreis_pfad = pfad::Erbauer::neu()
                .arc_chain(Bogen {
                    zentrum: kreis_zentrum,
                    radius: kreis_radius,
                    anfang: winkel::ZERO,
                    ende: winkel::TAU,
                })
                .baue();
            frame.stroke(
                &kreis_pfad,
                iced::canvas::Stroke { color: iced::Color::BLACK, width: 1., ..Default::default() },
            );
            let knopf_zentrum =
                kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, self.winkel);
            let knopf_radius = half_min_width_height - kreis_radius;
            let knopf_pfad = pfad::Erbauer::neu()
                .arc_chain(Bogen {
                    zentrum: knopf_zentrum,
                    radius: knopf_radius,
                    anfang: winkel::ZERO,
                    ende: winkel::TAU,
                })
                .baue();
            let knopf_grau = if self.grabbed {
                0.5
            } else if cursor.position_in(&bounds).map_or(false, |position| {
                (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - knopf_zentrum).länge()
                    < knopf_radius
            }) {
                0.7
            } else {
                0.8
            };
            frame.fill(
                &knopf_pfad,
                iced::canvas::Fill {
                    color: iced::Color::from_rgb(knopf_grau, knopf_grau, knopf_grau),
                    rule: iced::canvas::FillRule::EvenOdd,
                },
            );
        })]
    }

    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Winkel>) {
        let mut status = iced::canvas::event::Status::Ignored;
        let mut winkel = None;
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) => {
                if let Some(position) = cursor.position_in(&bounds) {
                    let relative_position = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
                    let size = bounds.size();
                    let min_width_height = Skalar(size.width.min(size.height));
                    let half_min_width_height = min_width_height.halbiert();
                    let kreis_zentrum =
                        Vektor { x: half_min_width_height, y: half_min_width_height };
                    let kreis_radius = Skalar(0.8) * half_min_width_height;
                    let knopf_zentrum =
                        kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, self.winkel);
                    let knopf_radius = half_min_width_height - kreis_radius;
                    if (relative_position - knopf_zentrum).länge() < knopf_radius {
                        self.grabbed = true
                    }
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) if self.grabbed => {
                self.canvas.leeren();
                self.grabbed = false;
                status = iced::canvas::event::Status::Captured;
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position }) => {
                if self.grabbed {
                    self.canvas.leeren();
                    let relative_position = Vektor {
                        x: Skalar(position.x - bounds.x),
                        y: Skalar(position.y - bounds.y),
                    };
                    let size = bounds.size();
                    let min_width_height = Skalar(size.width.min(size.height));
                    let half_min_width_height = min_width_height.halbiert();
                    let kreis_zentrum =
                        Vektor { x: half_min_width_height, y: half_min_width_height };
                    let position_von_zentrum = relative_position - kreis_zentrum;
                    let acos = Winkel::acos(
                        position_von_zentrum.einheitsvektor().skalarprodukt(&vektor::EX),
                    );
                    self.winkel = if position_von_zentrum.y > Skalar(0.) { acos } else { -acos };
                    winkel = Some(self.winkel);
                } else if cursor.is_over(&bounds) {
                    self.canvas.leeren()
                }
            }
            _ => {}
        }
        (status, winkel)
    }
}
