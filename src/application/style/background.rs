//! Style Strukturen für die Hintergrund-Farbe eines [iced::Container] oder [iced::Button].

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]


use iced::{button, container, Color};

pub const WHITE: Background = Background::Grey(1.);
pub const BLACK: Background = Background::Grey(0.);
pub const RED: Background = Background::Red(0.7);
pub const GREEN: Background = Background::Green(0.7);
pub const BLUE: Background = Background::Blue(0.7);
pub const DEFAULT: Background = Background::Grey(0.85);

#[derive(Debug, Clone, Copy)]
pub enum Background {
    Grey(f32),
    Red(f32),
    Green(f32),
    Blue(f32),
    GreyTransparent { grey: f32, alpha: f32 },
}

impl Background {
    /// Erzeuge die entsprechende Hintergrundfarbe.
    pub fn color(&self) -> Color {
        match self {
            Background::Grey(grey) => Color::from_rgb(*grey, *grey, *grey),
            Background::Red(red) => Color::from_rgb(*red, 0., 0.),
            Background::Green(green) => Color::from_rgb(0., *green, 0.),
            Background::Blue(blue) => Color::from_rgb(0., 0., *blue),
            Background::GreyTransparent { grey, alpha } => {
                Color::from_rgba(*grey, *grey, *grey, *alpha)
            },
        }
    }
}

impl container::StyleSheet for Background {
    fn style(&self) -> container::Style {
        container::Style {
            background: Some(iced::Background::Color(self.color())),
            ..Default::default()
        }
    }
}
impl button::StyleSheet for Background {
    fn active(&self) -> button::Style {
        button::Style {
            background: Some(iced::Background::Color(self.color())),
            ..Default::default()
        }
    }
}
