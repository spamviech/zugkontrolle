//! Knopf mit dem jeweiligen Gleis

use super::gleise::move_to_position;
use super::style::background;
use super::typen::*;

const STROKE_WIDTH: Skalar = Skalar(1.5);
const BORDER_WIDTH: u16 = 1;
const PADDING: u16 = 2;
const DOUBLE_PADDING: Skalar = Skalar((2 * (BORDER_WIDTH + PADDING)) as f32);

/// Ein Knopf, der ein Gleis anzeigt
#[derive(Debug)]
pub struct Button<T> {
    gleis: T,
    canvas: canvas::Cache,
}
impl<T> Button<T> {
    pub fn new(gleis: T) -> Self {
        Button { gleis, canvas: canvas::Cache::new() }
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
        iced::Container::new(
            iced::Container::new(
                // account for lines right at the edge
                iced::Canvas::new(self)
                    .width(iced::Length::Units(
                        width.unwrap_or((STROKE_WIDTH + size.x).0.ceil() as u16),
                    ))
                    .height(iced::Length::Units((STROKE_WIDTH + size.y).0.ceil() as u16)),
            )
            .width(iced::Length::Shrink)
            .height(iced::Length::Shrink)
            .padding(PADDING)
            .style(background::Grey(0.9)),
        )
        .width(iced::Length::Shrink)
        .height(iced::Length::Shrink)
        .padding(BORDER_WIDTH)
        .style(background::BLACK)
    }
}

impl<T: Zeichnen + ButtonMessage<Message>, Message> iced::canvas::Program<Message> for Button<T> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        vec![self.canvas.draw(bounds.size(), |frame| {
            let bounds_width = Skalar(bounds.width);
            let width = self.gleis.size().x;
            if bounds_width > width {
                // horizontal zentrieren
                let half_extra_width = Skalar(0.5) * bounds_width - width;
                frame.transformation(&Transformation::Translation(Vektor {
                    x: half_extra_width,
                    y: Skalar(0.),
                }));
            } else {
                // skaliere zu vorhandener Breite
                frame.transformation(&Transformation::Skalieren(bounds_width / width))
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
            if let Some((relative_position, content)) = self.gleis.beschreibung() {
                frame.with_save(|frame| {
                    move_to_position(frame, &relative_position);
                    frame.fill_text(canvas::Text {
                        content: content.to_string(),
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
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) if cursor.is_over(&bounds) => {
                (iced::canvas::event::Status::Captured, Some(self.gleis.to_message()))
            },
            _ => (iced::canvas::event::Status::Ignored, None),
        }
    }
}

pub trait ButtonMessage<Message> {
    fn to_message(&self) -> Message;
}
