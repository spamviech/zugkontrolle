//! Widget zum Einstellen des Anzeigewinkels.

use iced::{
    mouse::{self, Cursor},
    widget::canvas::{
        event,
        fill::{self, Fill},
        stroke::{self, Stroke},
        Event, Geometry, Program,
    },
    Rectangle, Renderer,
};

use crate::{
    application::style::thema::Thema,
    typen::{
        canvas::{
            pfad::{self, Bogen},
            Cache,
        },
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
    },
};

/// Ein Widget zum Einstellen des Anzeigewinkels, dargestellt über einen
/// [`Canvas`](iced::widget::canvas::Canvas).
#[derive(Debug)]
pub struct Drehen(Cache);

impl Drehen {
    /// Erstelle ein neues [`Drehen`]-Widget.
    pub fn neu() -> Self {
        Drehen(Cache::neu())
    }
}

/// Zustand für ein [`Drehen`]-Widget.
#[derive(Debug, Clone, Copy)]
pub struct Zustand {
    winkel: Winkel,
    grabbed: bool,
}

impl Default for Zustand {
    fn default() -> Self {
        Self { winkel: winkel::ZERO, grabbed: false }
    }
}

impl Program<Winkel, Renderer<Thema>> for Drehen {
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer<Thema>,
        thema: &Thema,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Vec<Geometry> {
        let size = bounds.size();
        vec![self.0.zeichnen(renderer, size, |frame| {
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
                Stroke {
                    style: stroke::Style::Solid(thema.strich().into()),
                    width: 1.,
                    ..Default::default()
                },
            );
            let knopf_zentrum =
                kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, state.winkel);
            let knopf_radius = half_min_width_height - kreis_radius;
            let knopf_pfad = pfad::Erbauer::neu()
                .arc_chain(Bogen {
                    zentrum: knopf_zentrum,
                    radius: knopf_radius,
                    anfang: winkel::ZERO,
                    ende: winkel::TAU,
                })
                .baue();
            let hintergrund = thema.hintergrund(
                state.grabbed,
                cursor.position_in(bounds).map_or(false, |position| {
                    let v_r =
                        Vektor { x: Skalar(position.x), y: Skalar(position.y) } - knopf_zentrum;
                    v_r.länge() < knopf_radius
                }),
            );
            frame.fill(
                &knopf_pfad,
                Fill { style: fill::Style::Solid(hintergrund.into()), rule: fill::Rule::EvenOdd },
            );
        })]
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Winkel>) {
        let mut status = event::Status::Ignored;
        let mut winkel = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    let relative_position = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
                    let size = bounds.size();
                    let min_width_height = Skalar(size.width.min(size.height));
                    let half_min_width_height = min_width_height.halbiert();
                    let kreis_zentrum =
                        Vektor { x: half_min_width_height, y: half_min_width_height };
                    let kreis_radius = Skalar(0.8) * half_min_width_height;
                    let knopf_zentrum =
                        kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, state.winkel);
                    let knopf_radius = half_min_width_height - kreis_radius;
                    if (relative_position - knopf_zentrum).länge() < knopf_radius {
                        state.grabbed = true
                    }
                }
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) if state.grabbed => {
                self.0.leeren();
                state.grabbed = false;
                status = event::Status::Captured;
            },
            Event::Mouse(mouse::Event::CursorMoved { position }) => {
                if state.grabbed {
                    self.0.leeren();
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
                    state.winkel = if position_von_zentrum.y > Skalar(0.) { acos } else { -acos };
                    winkel = Some(state.winkel);
                } else if cursor.is_over(bounds) {
                    self.0.leeren()
                }
            },
            _ => {},
        }
        (status, winkel)
    }
}
