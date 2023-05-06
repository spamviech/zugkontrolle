//! Knopf mit dem jeweiligen Gleis.

use iced::{
    alignment::{Horizontal, Vertical},
    mouse,
    widget::{
        canvas::{event, Canvas, Cursor, Event, Geometry, Program},
        container::{self, Container},
    },
    Element, Length, Point, Rectangle,
};
use iced_graphics::{Backend, Renderer};

use crate::{
    gleis::gleise::draw::bewege_an_position,
    typen::{
        canvas::{
            fill::{self, Fill, FillRule},
            pfad::{Pfad, Transformation},
            stroke::{self, Stroke},
            Cache, Color, Text,
        },
        mm::Spurweite,
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        Zeichnen,
    },
};

const STROKE_WIDTH: Skalar = Skalar(1.5);
const BORDER_WIDTH: u16 = 1;
const PADDING: u16 = 2;
const DOUBLE_PADDING: Skalar = Skalar((2 * (BORDER_WIDTH + PADDING)) as f32);
const GREY_IN_BOUNDS_VALUE: f32 = 0.7;
const GREY_IN_BOUNDS: Color =
    Color::from_rgb(GREY_IN_BOUNDS_VALUE, GREY_IN_BOUNDS_VALUE, GREY_IN_BOUNDS_VALUE);
const GREY_OUT_OF_BOUNDS_VALUE: f32 = 0.9;
const GREY_OUT_OF_BOUNDS: Color =
    Color::from_rgb(GREY_OUT_OF_BOUNDS_VALUE, GREY_OUT_OF_BOUNDS_VALUE, GREY_OUT_OF_BOUNDS_VALUE);

/// Ein Knopf, der ein Gleis anzeigt.
#[derive(Debug)]
pub struct Knopf<T> {
    gleis: T,
    spurweite: Spurweite,
}

impl<T> Knopf<T> {
    /// Erstelle einen neuen [Knopf].
    pub fn neu(gleis: T, spurweite: Spurweite) -> Self {
        Knopf { gleis, spurweite }
    }
}

impl<T: Zeichnen> Knopf<T> {
    /// Die Dimensionen des [Knopfes](Knopf).
    pub fn rechteck(&self) -> Rechteck {
        self.gleis
            .rechteck(self.spurweite)
            .verschiebe_chain(&Vektor { x: DOUBLE_PADDING, y: DOUBLE_PADDING })
    }

    /// Erstelle ein [Widget](iced_native::Element), dass den [Knopf] anzeigt.
    pub fn als_iced_widget<'t, Nachricht, B, Theme>(
        &'t self,
        breite: Option<f32>,
    ) -> impl Into<Element<'t, Nachricht, Renderer<B, Theme>>>
    where
        Nachricht: 'static,
        T: KnopfNachricht<Nachricht>,
        B: 't + Backend,
        Theme: 't + container::StyleSheet,
    {
        let größe = self.gleis.rechteck(self.spurweite).größe();
        let standard_breite = (STROKE_WIDTH + größe.x).0;
        let höhe = (DOUBLE_PADDING + STROKE_WIDTH + größe.y).0;
        // account for lines right at the edge
        let canvas = Canvas::new(self)
            .width(Length::Fixed(breite.unwrap_or(standard_breite)))
            .height(Length::Fixed(höhe));
        Container::new(canvas).width(Length::Fill).height(Length::Shrink)
    }
}

/// Zustand für ein [Knopf]-Widget.
#[derive(Debug, Default)]
pub struct Zustand {
    canvas: Cache,
    in_bounds: bool,
}

impl<T: Zeichnen + KnopfNachricht<Nachricht>, Nachricht, Theme> Program<Nachricht, Theme>
    for Knopf<T>
{
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        theme: &Theme,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        vec![state.canvas.zeichnen(bounds.size(), |frame| {
            let bounds_vector = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
            let border_path = Pfad::rechteck(bounds_vector, Vec::new());
            frame.fill(
                &border_path,
                Fill {
                    style: fill::Style::Solid(if state.in_bounds {
                        GREY_IN_BOUNDS
                    } else {
                        GREY_OUT_OF_BOUNDS
                    }),
                    rule: FillRule::EvenOdd,
                },
            );
            frame.stroke(
                &border_path,
                Stroke {
                    style: stroke::Style::Solid(Color::BLACK),
                    width: BORDER_WIDTH as f32,
                    ..Default::default()
                },
            );
            let spurweite = self.spurweite;
            let rechteck = self.gleis.rechteck(spurweite);
            let rechteck_position = rechteck.position();
            frame.transformation(&Transformation::Translation(-rechteck_position));
            let größe = rechteck.größe();
            let maximale_breite = bounds_vector.x - DOUBLE_PADDING;
            if maximale_breite > größe.x {
                // horizontal zentrieren
                frame.transformation(&Transformation::Translation(
                    Skalar(0.5) * (bounds_vector - größe),
                ));
            } else {
                // skaliere zu vorhandener Breite
                frame.transformation(&Transformation::Skalieren(maximale_breite / größe.x));
                frame.transformation(&Transformation::Translation(
                    Skalar(0.5) * Vektor { x: DOUBLE_PADDING, y: DOUBLE_PADDING },
                ));
            }
            for path in self.gleis.zeichne(spurweite) {
                frame.with_save(|frame| {
                    frame.stroke(
                        &path,
                        Stroke {
                            style: stroke::Style::Solid(Color::BLACK),
                            width: STROKE_WIDTH.0,
                            ..Default::default()
                        },
                    );
                });
            }
            if let (relative_position, Some(content), _unit_name) =
                self.gleis.beschreibung_und_name(spurweite)
            {
                frame.with_save(|frame| {
                    bewege_an_position(frame, &relative_position);
                    frame.fill_text(Text {
                        content: content.clone(),
                        position: Point::ORIGIN,
                        color: Color::BLACK,
                        horizontal_alignment: Horizontal::Center,
                        vertical_alignment: Vertical::Center,
                        ..Default::default()
                    });
                })
            }
        })]
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        let in_bounds = cursor.is_over(&bounds);
        if state.in_bounds != in_bounds {
            state.canvas.leeren();
            state.in_bounds = in_bounds;
        }
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) if state.in_bounds => {
                let Point { x, y } = cursor.position_in(&bounds).unwrap_or(Point { x: 0., y: 0. });
                (
                    event::Status::Captured,
                    Some(self.gleis.nachricht(Vektor { x: Skalar(x), y: Skalar(y) })),
                )
            },
            _ => (event::Status::Ignored, None),
        }
    }
}

/// Alle Funktionen eines [Knopfes](Knopf) werden unterstützt.
pub trait KnopfNachricht<Nachricht> {
    /// Erzeuge eine Nachricht ausgehend der relativen Position wo der [Knopf] gedrückt wurde.
    fn nachricht(&self, klick_position: Vektor) -> Nachricht;
}
