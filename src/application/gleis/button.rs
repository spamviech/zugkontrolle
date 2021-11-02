//! Knopf mit dem jeweiligen Gleis

use num_traits::NumCast;

use crate::application::{gleis::gleise::draw::move_to_position, typen::*};

const STROKE_WIDTH: Skalar = Skalar(1.5);
const BORDER_WIDTH: u16 = 1;
const PADDING: u16 = 2;
const DOUBLE_PADDING: Skalar = Skalar((2 * (BORDER_WIDTH + PADDING)) as f32);
const GREY_IN_BOUNDS_VALUE: f32 = 0.7;
const GREY_IN_BOUNDS: canvas::Color =
    canvas::Color::from_rgb(GREY_IN_BOUNDS_VALUE, GREY_IN_BOUNDS_VALUE, GREY_IN_BOUNDS_VALUE);
const GREY_OUT_OF_BOUNDS_VALUE: f32 = 0.9;
const GREY_OUT_OF_BOUNDS: canvas::Color = canvas::Color::from_rgb(
    GREY_OUT_OF_BOUNDS_VALUE,
    GREY_OUT_OF_BOUNDS_VALUE,
    GREY_OUT_OF_BOUNDS_VALUE,
);

/// Ein Knopf, der ein Gleis anzeigt
#[derive(Debug)]
pub struct Button<T> {
    gleis: T,
    canvas: Cache,
    in_bounds: bool,
}

impl<T> Button<T> {
    pub fn neu(gleis: T) -> Self {
        Button { gleis, canvas: Cache::neu(), in_bounds: false }
    }
}

impl<T: Zeichnen> Button<T> {
    pub fn rechteck(&self) -> Rechteck {
        self.gleis.rechteck().verschiebe_chain(&Vektor { x: DOUBLE_PADDING, y: DOUBLE_PADDING })
    }

    pub fn als_iced_widget<'t, Nachricht>(
        &'t mut self,
        breite: Option<u16>,
    ) -> iced::Container<'t, Nachricht>
    where
        Nachricht: 'static,
        T: ButtonNachricht<Nachricht>,
    {
        let größe = self.gleis.rechteck().größe();
        let standard_breite = NumCast::from((STROKE_WIDTH + größe.x).0.ceil()).unwrap_or(u16::MAX);
        let höhe =
            NumCast::from((DOUBLE_PADDING + STROKE_WIDTH + größe.y).0.ceil()).unwrap_or(u16::MAX);
        // account for lines right at the edge
        iced::Container::new(
            iced::Canvas::new(self)
                .width(iced::Length::Units(breite.unwrap_or(standard_breite)))
                .height(iced::Length::Units(höhe)),
        )
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
    }
}

impl<T: Zeichnen + ButtonNachricht<Nachricht>, Nachricht> iced::canvas::Program<Nachricht>
    for Button<T>
{
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        vec![self.canvas.zeichnen(bounds.size(), |frame| {
            let bounds_vector = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
            let border_path = Pfad::rechteck(bounds_vector, Vec::new());
            frame.fill(
                &border_path,
                canvas::Fill {
                    color: if self.in_bounds { GREY_IN_BOUNDS } else { GREY_OUT_OF_BOUNDS },
                    rule: canvas::FillRule::EvenOdd,
                },
            );
            frame.stroke(
                &border_path,
                canvas::Stroke {
                    color: canvas::Color::BLACK,
                    width: BORDER_WIDTH as f32,
                    ..Default::default()
                },
            );
            let rechteck = self.gleis.rechteck();
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
            for path in self.gleis.zeichne() {
                frame.with_save(|frame| {
                    frame.stroke(
                        &path,
                        canvas::Stroke {
                            color: canvas::Color::BLACK,
                            width: STROKE_WIDTH.0,
                            ..Default::default()
                        },
                    );
                });
            }
            if let (relative_position, Some(content), _unit_name) =
                self.gleis.beschreibung_und_name()
            {
                frame.with_save(|frame| {
                    move_to_position(frame, &relative_position);
                    frame.fill_text(canvas::Text {
                        content: content.clone(),
                        position: iced::Point::ORIGIN,
                        color: canvas::Color::BLACK,
                        horizontal_alignment: canvas::HorizontalAlignment::Center,
                        vertical_alignment: canvas::VerticalAlignment::Center,
                        ..Default::default()
                    });
                })
            }
        })]
    }

    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht>) {
        let in_bounds = cursor.is_over(&bounds);
        if self.in_bounds != in_bounds {
            self.canvas.leeren();
            self.in_bounds = in_bounds;
        }
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) if self.in_bounds => {
                let iced::Point { x, y } =
                    cursor.position_in(&bounds).unwrap_or(iced::Point { x: 0., y: 0. });
                (
                    iced::canvas::event::Status::Captured,
                    Some(self.gleis.nachricht(Vektor { x: Skalar(x), y: Skalar(y) })),
                )
            }
            _ => (iced::canvas::event::Status::Ignored, None),
        }
    }
}

pub trait ButtonNachricht<Nachricht> {
    fn nachricht(&self, klick_position: Vektor) -> Nachricht;
}
