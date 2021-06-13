//! Knopf mit dem jeweiligen Gleis

use super::gleise::move_to_position;
use crate::application::typen::*;

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
    canvas: canvas::Cache,
    in_bounds: bool,
}
impl<T> Button<T> {
    pub fn new(gleis: T) -> Self {
        Button { gleis, canvas: canvas::Cache::new(), in_bounds: false }
    }
}
impl<T: Zeichnen> Button<T> {
    pub fn size(&self) -> Vektor {
        // include padding
        self.gleis.size() + Vektor { x: DOUBLE_PADDING, y: DOUBLE_PADDING }
    }

    pub fn to_iced<Message>(&mut self, width: Option<u16>) -> iced::Container<Message>
    where
        Message: 'static + Clone,
        T: ButtonMessage<Message>,
    {
        let size = self.gleis.size();
        // account for lines right at the edge
        iced::Container::new(
            iced::Canvas::new(self)
                .width(iced::Length::Units(
                    width.unwrap_or((STROKE_WIDTH + size.x).0.ceil() as u16),
                ))
                .height(iced::Length::Units(
                    (DOUBLE_PADDING + STROKE_WIDTH + size.y).0.ceil() as u16
                )),
        )
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
    }
}

impl<T: Zeichnen + ButtonMessage<Message>, Message> iced::canvas::Program<Message> for Button<T> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        vec![self.canvas.draw(bounds.size(), |frame| {
            let bounds_vector = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
            let border_path = Pfad::rechteck(bounds_vector, Vec::new());
            frame.fill(&border_path, canvas::Fill {
                color: if self.in_bounds { GREY_IN_BOUNDS } else { GREY_OUT_OF_BOUNDS },
                rule: canvas::FillRule::EvenOdd,
            });
            frame.stroke(&border_path, canvas::Stroke {
                color: canvas::Color::BLACK,
                width: BORDER_WIDTH as f32,
                ..Default::default()
            });
            let size = self.gleis.size();
            if bounds_vector.x > size.x {
                // horizontal zentrieren
                frame.transformation(&Transformation::Translation(
                    Skalar(0.5) * (bounds_vector - size),
                ));
            } else {
                // skaliere zu vorhandener Breite
                frame.transformation(&Transformation::Skalieren(bounds_vector.x / size.x))
            }
            for path in self.gleis.zeichne() {
                frame.with_save(|frame| {
                    frame.stroke(&path, canvas::Stroke {
                        color: canvas::Color::BLACK,
                        width: STROKE_WIDTH.0,
                        ..Default::default()
                    });
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
    ) -> (iced::canvas::event::Status, Option<Message>) {
        let in_bounds = cursor.is_over(&bounds);
        if self.in_bounds != in_bounds {
            self.canvas.clear();
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
                    Some(self.gleis.to_message(Vektor { x: Skalar(x), y: Skalar(y) })),
                )
            },
            _ => (iced::canvas::event::Status::Ignored, None),
        }
    }
}

pub trait ButtonMessage<Message> {
    fn to_message(&self, grab_location: Vektor) -> Message;
}
