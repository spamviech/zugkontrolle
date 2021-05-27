//! Auswahl eines Anschlusses.

use iced::{Element, Length, Text};
use iced_aw::{TabLabel, Tabs};

mod style;

#[derive(Debug)]
pub struct Auswahl {
    active_tab: usize,
}

pub enum Message {
    TabSelected(usize),
}

impl Auswahl {
    pub fn view(&mut self) -> Element<Message> {
        let tabs = vec![
            (
                TabLabel::Text("Pin".to_string()),
                // TODO
                Text::new("TODO").into(),
            ),
            (
                TabLabel::Text("Pcf8574-Port".to_string()),
                // TODO
                Text::new("TODO").into(),
            ),
        ];
        Tabs::with_tabs(self.active_tab, tabs, Message::TabSelected)
            .tab_bar_style(style::TabBar)
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }
}
