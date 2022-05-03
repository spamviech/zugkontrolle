//! Knopf mit dem jeweiligen Gleis.

use iced::{
    alignment::{Horizontal, Vertical},
    canvas::{event, Cursor, Event, Geometry, Program},
    mouse, Canvas, Container, Element, Length, Point, Rectangle,
};
use num_traits::NumCast;

use crate::{
    gleis::gleise::draw::bewege_an_position,
    typen::{
        canvas::{
            pfad::{Pfad, Transformation},
            Cache, Color, Fill, FillRule, Stroke, Text,
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
    canvas: Cache,
    in_bounds: bool,
}

impl<T> Knopf<T> {
    /// Erstelle einen neuen [Knopf].
    pub fn neu(gleis: T, spurweite: Spurweite) -> Self {
        Knopf { gleis, spurweite, canvas: Cache::neu(), in_bounds: false }
    }
}

impl<T: Zeichnen> Knopf<T> {
    /// Die Dimensionen des [Knopfes](Knopf).
    pub fn rechteck(&self) -> Rechteck {
        self.gleis
            .rechteck(self.spurweite)
            .verschiebe_chain(&Vektor { x: DOUBLE_PADDING, y: DOUBLE_PADDING })
    }

    /// Erstelle ein [Widget](iced_native::Element), dass den [Button] anzeigt.
    pub fn als_iced_widget<'t, Nachricht>(
        &'t mut self,
        breite: Option<u16>,
    ) -> impl Into<Element<'t, Nachricht>>
    where
        Nachricht: 'static,
        T: KnopfNachricht<Nachricht>,
    {
        let größe = self.gleis.rechteck(self.spurweite).größe();
        let standard_breite = NumCast::from((STROKE_WIDTH + größe.x).0.ceil()).unwrap_or(u16::MAX);
        let höhe =
            NumCast::from((DOUBLE_PADDING + STROKE_WIDTH + größe.y).0.ceil()).unwrap_or(u16::MAX);
        // account for lines right at the edge
        Container::new(
            Canvas::new(self)
                .width(Length::Units(breite.unwrap_or(standard_breite)))
                .height(Length::Units(höhe)),
        )
        .width(Length::Fill)
        .height(Length::Shrink)
    }
}

impl<T: Zeichnen + KnopfNachricht<Nachricht>, Nachricht> Program<Nachricht> for Knopf<T> {
    fn draw(&self, bounds: Rectangle, _cursor: Cursor) -> Vec<Geometry> {
        vec![self.canvas.zeichnen(bounds.size(), |frame| {
            let bounds_vector = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
            let border_path = Pfad::rechteck(bounds_vector, Vec::new());
            frame.fill(
                &border_path,
                Fill {
                    color: if self.in_bounds { GREY_IN_BOUNDS } else { GREY_OUT_OF_BOUNDS },
                    rule: FillRule::EvenOdd,
                },
            );
            frame.stroke(
                &border_path,
                Stroke { color: Color::BLACK, width: BORDER_WIDTH as f32, ..Default::default() },
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
                        Stroke { color: Color::BLACK, width: STROKE_WIDTH.0, ..Default::default() },
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
        &mut self,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        let in_bounds = cursor.is_over(&bounds);
        if self.in_bounds != in_bounds {
            self.canvas.leeren();
            self.in_bounds = in_bounds;
        }
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) if self.in_bounds => {
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
