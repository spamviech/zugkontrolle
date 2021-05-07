//! Style-structs for the background color of iced::Container

pub struct White;
impl iced::container::StyleSheet for White {
    fn style(&self) -> iced::container::Style {
        iced::container::Style {
            background: Some(iced::Background::Color(iced::Color::WHITE)),
            ..Default::default()
        }
    }
}
pub struct Black;
impl iced::container::StyleSheet for Black {
    fn style(&self) -> iced::container::Style {
        iced::container::Style {
            background: Some(iced::Background::Color(iced::Color::BLACK)),
            ..Default::default()
        }
    }
}
