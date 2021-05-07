//! Knopf mit dem jeweiligen Gleis

use super::gleise::move_to_position;
use super::types::*;

/// Ein Knopf, der ein Gleis anzeigt
#[derive(Debug)]
pub struct Button<T> {
    canvas: ButtonCanvas<T>,
    state: iced::button::State,
}
impl<T> Button<T> {
    pub fn new(gleis: T) -> Self {
        Button {
            canvas: ButtonCanvas { gleis, canvas: canvas::Cache::new() },
            state: iced::button::State::new(),
        }
    }
}
impl<T: Zeichnen> Button<T> {
    pub fn size(&self) -> canvas::Size {
        self.canvas.gleis.size()
    }

    pub fn to_iced<Message>(&mut self) -> iced::Button<Message>
    where
        Message: 'static + Clone,
        T: ButtonMessage<Message>,
    {
        let Button { canvas, state } = self;
        let size = canvas.gleis.size();
        let message = canvas.gleis.to_message();
        iced::Button::new(
            state,
            iced::Canvas::new(canvas)
                .width(iced::Length::Units((canvas::X(0.) + size.width).0.ceil() as u16))
                .height(iced::Length::Units((canvas::Y(0.) + size.height).0.ceil() as u16)),
        )
        .padding(2)
        .on_press(message)
    }
}

#[derive(Debug)]
struct ButtonCanvas<T> {
    gleis: T,
    canvas: canvas::Cache,
}
impl<T: Zeichnen, Message> iced::canvas::Program<Message> for ButtonCanvas<T> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        // TODO adjust to size
        vec![self.canvas.draw(
            canvas::Size::new(
                canvas::X(bounds.width).to_abstand(),
                canvas::Y(bounds.height).to_abstand(),
            ),
            |frame| {
                for path in self.gleis.zeichne() {
                    frame.with_save(|frame| {
                        frame.stroke(
                            &path,
                            canvas::Stroke {
                                color: canvas::Color::BLACK,
                                width: 1.5,
                                ..Default::default()
                            },
                        );
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
            },
        )]
    }
}

pub trait ButtonMessage<Message> {
    fn to_message(&self) -> Message;
}
