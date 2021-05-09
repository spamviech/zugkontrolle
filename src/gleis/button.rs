//! Knopf mit dem jeweiligen Gleis

use super::gleise::move_to_position;
use super::typen::*;

const STROKE_WIDTH: Skalar = Skalar(1.5);

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
const PADDING: u16 = 3;
impl<T: Zeichnen> Button<T> {
    pub fn size(&self) -> Vektor {
        // include padding
        let double_padding = Skalar(2. * PADDING as f32);
        self.canvas.gleis.size() + Vektor { x: double_padding, y: double_padding }
    }

    pub fn to_iced<Message>(&mut self, width: Option<u16>) -> iced::Button<Message>
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
                // account for lines right at the edge
                .width(iced::Length::Units(
                    width.unwrap_or((STROKE_WIDTH + size.x).0.ceil() as u16)
                ))
                .height(iced::Length::Units(
                    (STROKE_WIDTH + size.y).0.ceil() as u16
                )),
        )
        .padding(PADDING)
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
        // TODO adjust(scale) to size
        let half_extra_width = Skalar(0.5) * (Skalar(bounds.width) - self.gleis.size().x);
        vec![self.canvas.draw_unskaliert(bounds.size(), |frame| {
            frame.transformation(&Transformation::Translation(Vektor {
                x: half_extra_width,
                y: Skalar(0.),
            }));
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
}

pub trait ButtonMessage<Message> {
    fn to_message(&self) -> Message;
}
