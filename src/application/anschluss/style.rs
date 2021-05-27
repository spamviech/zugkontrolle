//! Style-Strukturen zur Auswahl eines Anschlusses.

pub(crate) struct TabBar;
impl TabBar {
    fn style(&self, tab_label_background: iced::Color) -> iced_aw::tab_bar::Style {
        iced_aw::tab_bar::Style {
            background: Some(iced::Background::Color(iced::Color::WHITE)),
            border_color: Some(iced::Color::BLACK),
            border_width: 0.,
            tab_label_background: iced::Background::Color(tab_label_background),
            tab_label_border_color: iced::Color::BLACK,
            tab_label_border_width: 1.,
            icon_color: iced::Color::BLACK,
            text_color: iced::Color::BLACK,
        }
    }
}
impl iced_aw::tab_bar::StyleSheet for TabBar {
    fn active(&self, is_active: bool) -> iced_aw::tab_bar::Style {
        let grey_value: f32;
        if is_active {
            grey_value = 0.8;
        } else {
            grey_value = 0.9;
        }
        self.style(iced::Color::from_rgb(grey_value, grey_value, grey_value))
    }

    fn hovered(&self, _is_active: bool) -> iced_aw::tab_bar::Style {
        let grey_value = 0.7;
        self.style(iced::Color::from_rgb(grey_value, grey_value, grey_value))
    }
}
