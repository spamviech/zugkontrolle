//! Style-Strukturen for a iced::Scrollable

#[derive(Debug, Clone, Copy)]
pub struct Collection {
    width: u16,
}

impl Collection {
    pub fn new(width: u16) -> Self {
        Collection { width }
    }

    pub fn width(&self) -> u16 {
        self.width
    }

    fn scrollbar(&self, grey_value: f32) -> iced::scrollable::Scrollbar {
        let scroller_color = iced::Color::from_rgb(grey_value, grey_value, grey_value);
        iced::scrollable::Scrollbar {
            background: None,
            border_radius: 0.,
            border_width: 0.,
            border_color: iced::Color::BLACK,
            scroller: iced::scrollable::Scroller {
                color: scroller_color,
                border_radius: 0.25 * (self.width as f32),
                border_width: 0.,
                border_color: scroller_color,
            },
        }
    }
}

impl iced::scrollable::StyleSheet for Collection {
    fn active(&self) -> iced::scrollable::Scrollbar {
        self.scrollbar(0.7)
    }

    fn hovered(&self) -> iced::scrollable::Scrollbar {
        self.scrollbar(0.6)
    }

    fn dragging(&self) -> iced::scrollable::Scrollbar {
        self.scrollbar(0.5)
    }
}
