//! Knopf mit dem jeweiligen Gleis

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
    pub fn to_button<Message>(&mut self) -> iced::Button<Message>
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
                .width(iced::Length::Units((canvas::X(0.) + size.width).0 as u16))
                .height(iced::Length::Units((canvas::Y(0.) + size.height).0 as u16)),
        )
        .width(iced::Length::Shrink)
        .height(iced::Length::Shrink)
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
            },
        )]
    }
}

pub trait ButtonMessage<Message> {
    fn to_message(&self) -> Message;
}
