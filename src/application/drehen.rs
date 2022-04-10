//! Widget zum Einstellen des Anzeigewinkels.

use iced::{
    canvas::{event, Cursor, Event, Fill, FillRule, Geometry, Program, Stroke},
    mouse, Color, Rectangle,
};

use crate::typen::{
    canvas::{
        pfad::{self, Bogen},
        Cache,
    },
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Trigonometrie, Winkel},
};

/// Ein Widget zum Einstellen des Anzeigewinkels, dargestellt über einen
/// [Canvas](crate::touch_canvas::Canvas).
#[derive(Debug)]
pub struct Drehen {
    canvas: Cache,
    winkel: Winkel,
    grabbed: bool,
}

impl Drehen {
    /// Erstelle ein neues [Drehen]-Widget.
    pub fn neu() -> Self {
        Drehen { canvas: Cache::neu(), winkel: winkel::ZERO, grabbed: false }
    }
}

impl Program<Winkel> for Drehen {
    fn draw(&self, bounds: Rectangle, cursor: Cursor) -> Vec<Geometry> {
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
                Stroke { color: Color::BLACK, width: 1., ..Default::default() },
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
                Fill {
                    color: Color::from_rgb(knopf_grau, knopf_grau, knopf_grau),
                    rule: FillRule::EvenOdd,
                },
            );
        })]
    }

    fn update(
        &mut self,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Winkel>) {
        let mut status = event::Status::Ignored;
        let mut winkel = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
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
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) if self.grabbed => {
                self.canvas.leeren();
                self.grabbed = false;
                status = event::Status::Captured;
            },
            Event::Mouse(mouse::Event::CursorMoved { position }) => {
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
                        position_von_zentrum.einheitsvektor().skalarprodukt(&Vektor::EX),
                    );
                    self.winkel = if position_von_zentrum.y > Skalar(0.) { acos } else { -acos };
                    winkel = Some(self.winkel);
                } else if cursor.is_over(&bounds) {
                    self.canvas.leeren()
                }
            },
            _ => {},
        }
        (status, winkel)
    }
}
