//! Style Strukturen für die Hintergrund-Farbe eines iced::Container

pub const WHITE: Background = Background::Grey(1.);
pub const BLACK: Background = Background::Grey(0.);
pub const RED: Background = Background::Red(0.7);
pub const GREEN: Background = Background::Green(0.7);
pub const BLUE: Background = Background::Blue(0.7);
pub const DEFAULT: Background = Background::Default;

pub enum Background {
    Grey(f32),
    Red(f32),
    Green(f32),
    Blue(f32),
    Default,
}

impl Background {
    fn color(&self) -> Option<iced::Color> {
        match self {
            Background::Grey(grey) => Some(iced::Color::from_rgb(*grey, *grey, *grey)),
            Background::Red(red) => Some(iced::Color::from_rgb(*red, 0., 0.)),
            Background::Green(green) => Some(iced::Color::from_rgb(0., *green, 0.)),
            Background::Blue(blue) => Some(iced::Color::from_rgb(0., 0., *blue)),
            Background::Default => None,
        }
    }
}

impl iced::container::StyleSheet for Background {
    fn style(&self) -> iced::container::Style {
        let mut style = iced::container::Style::default();
        if let Some(color) = self.color() {
            style.background = Some(iced::Background::Color(color))
        }
        style
    }
}
impl iced::button::StyleSheet for Background {
    fn active(&self) -> iced::button::Style {
        let mut style = iced::button::Style::default();
        if let Some(color) = self.color() {
            style.background = Some(iced::Background::Color(color))
        }
        style
    }
}
