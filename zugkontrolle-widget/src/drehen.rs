//! Widget zum Einstellen des Anzeigewinkels.

use iced::{
    Point, Rectangle, Renderer,
    mouse::{self, Cursor},
    touch,
    widget::canvas::{
        Event, Geometry, Program,
        fill::{self, Fill},
        stroke::{self, Stroke},
    },
};

use iced_widget::Action;
use zugkontrolle_gleise::knopf::Thema as _;
use zugkontrolle_typen::{
    canvas::{
        Cache,
        pfad::{self, Bogen},
    },
    klick_quelle::KlickQuelle,
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Winkel},
};
use zugkontrolle_util::event_status::EventStatus;

use crate::style::thema::Thema;

/// Ein Widget zum Einstellen des Anzeigewinkels, dargestellt über einen
/// [`Canvas`](iced::widget::canvas::Canvas).
#[derive(Debug)]
pub struct Drehen(Cache);

impl Drehen {
    /// Erstelle ein neues [`Drehen`]-Widget.
    #[must_use]
    pub fn neu() -> Self {
        Drehen(Cache::neu())
    }

    /// Erzwinge ein neuzeichnen des Canvas.
    pub fn erzwinge_neuzeichnen(&mut self) {
        self.0.leeren();
    }
}

/// Zustand für ein [`Drehen`]-Widget.
#[derive(Debug, Clone, Copy)]
pub struct Zustand {
    /// Der aktuelle Dreh-Winkel.
    winkel: Winkel,
    /// Wird der Winkel aktuell angepasst?
    grabbed: Option<KlickQuelle>,
}

impl Default for Zustand {
    fn default() -> Self {
        Self { winkel: winkel::ZERO, grabbed: None }
    }
}

impl Program<Winkel, Thema, Renderer> for Drehen {
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer,
        thema: &Thema,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Vec<Geometry> {
        let size = bounds.size();
        vec![self.0.zeichnen(renderer, thema, size, |frame| {
            let min_width_height = Skalar(size.width.min(size.height));
            let half_min_width_height = min_width_height.halbiert();
            let kreis_zentrum = Vektor { x: half_min_width_height, y: half_min_width_height };
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
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
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let knopf_zentrum =
                kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, state.winkel);
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
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
                state.grabbed.is_some(),
                cursor.position_in(bounds).is_some_and(|position| {
                    #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
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
        event: &Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Option<Action<Winkel>> {
        /// Reagiere auf einen Maus- oder Touch-Klick.
        fn pressed(
            state: &mut Zustand,
            bounds: Rectangle,
            position: Point,
            klick_quelle: KlickQuelle,
        ) -> EventStatus {
            let relative_position = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
            let size = bounds.size();
            let min_width_height = Skalar(size.width.min(size.height));
            let half_min_width_height = min_width_height.halbiert();
            let kreis_zentrum = Vektor { x: half_min_width_height, y: half_min_width_height };
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let kreis_radius = Skalar(0.8) * half_min_width_height;
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let knopf_zentrum =
                kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, state.winkel);
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let knopf_radius = half_min_width_height - kreis_radius;
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            if (relative_position - knopf_zentrum).länge() < knopf_radius {
                state.grabbed = Some(klick_quelle);
                EventStatus::Captured
            } else {
                EventStatus::Ignored
            }
        }
        /// Reagiere auf einen Maus- oder Touch-Bewegung.
        fn moved(
            state: &mut Zustand,
            cache: &Cache,
            bounds: Rectangle,
            position: Point,
            klick_quelle: KlickQuelle,
        ) -> Option<Winkel> {
            if state.grabbed == Some(klick_quelle) {
                cache.leeren();
                let relative_position =
                    Vektor { x: Skalar(position.x - bounds.x), y: Skalar(position.y - bounds.y) };
                let size = bounds.size();
                let min_width_height = Skalar(size.width.min(size.height));
                let half_min_width_height = min_width_height.halbiert();
                let kreis_zentrum = Vektor { x: half_min_width_height, y: half_min_width_height };
                #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
                let position_von_zentrum = relative_position - kreis_zentrum;
                let acos =
                    Winkel::acos(position_von_zentrum.einheitsvektor().skalarprodukt(&Vektor::EX));
                state.winkel = if position_von_zentrum.y > Skalar(0.) {
                    acos
                } else {
                    #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
                    {
                        -acos
                    }
                };
                Some(state.winkel)
            } else if (klick_quelle == KlickQuelle::Maus)
                && Cursor::Available(position).is_over(bounds)
            {
                cache.leeren();
                None
            } else {
                // Ignoriere Move-Events ohne grab außerhalb des Zeichenbereichs.
                // Es gibt nur einen Unterschied, ob der Knopf aktuell grabbed ist,
                // der Mauszeiger über dem Knopf ist, oder nicht.
                None
            }
        }
        let mut status = EventStatus::Ignored;
        let mut winkel = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    status = pressed(state, bounds, position, KlickQuelle::Maus);
                }
            },
            Event::Touch(touch::Event::FingerPressed { id, position }) => {
                status = pressed(state, bounds, *position, KlickQuelle::Touch(*id));
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
                if state.grabbed == Some(KlickQuelle::Maus) =>
            {
                self.0.leeren();
                state.grabbed = None;
                status = EventStatus::Captured;
            },
            Event::Touch(
                touch::Event::FingerLifted { id, position: _ }
                | touch::Event::FingerLost { id, position: _ },
            ) if state.grabbed == Some(KlickQuelle::Touch(*id)) => {
                self.0.leeren();
                state.grabbed = None;
                status = EventStatus::Captured;
            },
            Event::Mouse(mouse::Event::CursorMoved { position }) => {
                winkel = moved(state, &self.0, bounds, *position, KlickQuelle::Maus);
            },
            Event::Touch(touch::Event::FingerMoved { id, position }) => {
                winkel = moved(state, &self.0, bounds, *position, KlickQuelle::Touch(*id));
            },
            Event::Mouse(_)
            | Event::Touch(_)
            | Event::Keyboard(_)
            | Event::Window(_)
            | Event::InputMethod(_) => {},
        }
        let mut action = if let Some(winkel) = winkel {
            Action::publish(winkel)
        } else {
            Action::request_redraw()
        };
        if status == EventStatus::Captured {
            action = action.and_capture();
        }
        Some(action)
    }

    fn mouse_interaction(
        &self,
        state: &Self::State,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> mouse::Interaction {
        if state.grabbed.is_some() {
            mouse::Interaction::Grabbing
        } else {
            let size = bounds.size();
            let min_width_height = Skalar(size.width.min(size.height));
            let half_min_width_height = min_width_height.halbiert();
            let kreis_zentrum = Vektor { x: half_min_width_height, y: half_min_width_height };
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let kreis_radius = Skalar(0.8) * half_min_width_height;
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let knopf_zentrum =
                kreis_zentrum + Vektor::polar_koordinaten(kreis_radius, state.winkel);
            #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
            let knopf_radius = half_min_width_height - kreis_radius;
            let cursor_über_knopf = cursor.position_in(bounds).is_some_and(|position| {
                #[allow(clippy::arithmetic_side_effects, reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.")]
                let v_r = Vektor { x: Skalar(position.x), y: Skalar(position.y) } - knopf_zentrum;
                v_r.länge() < knopf_radius
            });
            if cursor_über_knopf {
                mouse::Interaction::Grab
            } else {
                mouse::Interaction::default()
            }
        }
    }
}
