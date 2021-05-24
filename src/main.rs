//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use iced::{Application, Clipboard, Command, Element, Length, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::application::{
    self,
    gleis::{
        anchor,
        gleise::{Gleis, GleisIdLock, Gleise, GleiseMap},
        typen::*,
        *,
    },
    Zugkontrolle,
};
use zugkontrolle::zugtyp::{
    lego::{self, Lego},
    märklin::{self, Märklin},
};

struct AppendGleise<'t, Z> {
    gleise: &'t mut Gleise<Z>,
    y: Skalar,
}
impl<'t, Z> AppendGleise<'t, Z> {
    fn new(gleise: &'t mut Gleise<Z>) -> AppendGleise<'t, Z> {
        AppendGleise { gleise, y: Skalar(5.) }
    }
}

impl<'t, Z: Zugtyp + Eq + Debug> AppendGleise<'t, Z> {
    fn append<T>(&mut self, definition: T) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let size: Vektor = definition.size();
        let punkt = Vektor { x: Skalar(150.) - size.x.halbiert(), y: self.y };
        let height: Skalar = size.y;
        let res = self.gleise.add(Gleis {
            definition,
            position: Position { punkt, winkel: Winkel(0.) },
            streckenabschnitt: None,
        });
        self.y += height + Skalar(25.);
        res
    }
}

mod style {
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
}

#[derive(Debug, Clone)]
enum Message {
    TabSelected(usize),
    Märklin(application::Message<Märklin>),
    Lego(application::Message<Lego>),
}

struct App {
    pub active_tab: usize,
    pub zugkontrolle_märklin: Zugkontrolle<Märklin>,
    pub zugkontrolle_lego: Zugkontrolle<Lego>,
}
impl Application for App {
    type Executor = iced::executor::Default;
    type Flags = (Gleise<Märklin>, Gleise<Lego>);
    type Message = Message;

    fn new((gleise_märklin, gleise_lego): Self::Flags) -> (Self, Command<Self::Message>) {
        (
            App {
                active_tab: 0,
                zugkontrolle_märklin: Zugkontrolle::new(gleise_märklin).0,
                zugkontrolle_lego: Zugkontrolle::new(gleise_lego).0,
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        self.zugkontrolle_märklin.title()
    }

    fn update(
        &mut self,
        message: Self::Message,
        clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        match message {
            Message::TabSelected(selected) => {
                self.active_tab = selected;
            },
            Message::Märklin(message) => {
                self.zugkontrolle_märklin.update(message, clipboard);
            },
            Message::Lego(message) => {
                self.zugkontrolle_lego.update(message, clipboard);
            },
        }

        Command::none()
    }

    fn view(&mut self) -> Element<Self::Message> {
        let tabs = vec![
            (
                iced_aw::TabLabel::Text("Märklin".to_string()),
                self.zugkontrolle_märklin.view().map(Message::Märklin).into(),
            ),
            (
                iced_aw::TabLabel::Text("Lego".to_string()),
                self.zugkontrolle_lego.view().map(Message::Lego).into(),
            ),
        ];
        iced_aw::Tabs::with_tabs(self.active_tab, tabs, Message::TabSelected)
            .tab_bar_style(style::TabBar)
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }
}

fn main() -> iced::Result {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    /*
    use std::collections::HashMap;
    use zugkontrolle::zugtyp::{deserialize, value};
    let zugtypen: HashMap<String, value::Zugtyp> = deserialize::Zugtyp::load_all_from_dir("zugtyp")
        .into_iter()
        .map(|(k, v)| (k, v.into()))
        .collect();
    println!("{:#?}", zugtypen);
    */

    // Märklin-Gleise
    let mut gleise_märklin: Gleise<Märklin> = Gleise::new();
    let mut append_märklin = AppendGleise::new(&mut gleise_märklin);
    append_märklin.append(märklin::gerade_5106());
    append_märklin.append(märklin::kurve_5100());
    append_märklin.append(märklin::weiche_5202_links());
    append_märklin.append(märklin::dreiwege_weiche_5214());
    append_märklin.append(märklin::kurven_weiche_5140_links());
    append_märklin.append(märklin::kreuzung_5207());

    // Lego-Gleise
    let mut gleise_lego: Gleise<Lego> = Gleise::new();
    let mut append_lego = AppendGleise::new(&mut gleise_lego);
    let (gerade_lock, _gerade_anchor_points) = append_lego.append(lego::GERADE);
    let (kurve_lock, _kurve_anchor_points) = append_lego.append(lego::KURVE);
    let (_weiche_lock, weiche_anchor_points) = append_lego.append(lego::WEICHE_LINKS);
    let (kreuzung0_lock, _kreuzung0_anchor_points) = append_lego.append(lego::KREUZUNG);
    let (_kreuzung1_lock, kreuzung1_anchor_points) = append_lego.append(lego::KREUZUNG);
    // relocate
    if let Some(gleis_id) = &*gerade_lock.read() {
        gleise_lego.relocate(gleis_id, Position {
            punkt: Vektor { x: Skalar(250.), y: Skalar(10.) },
            winkel: WinkelGradmaß::neu(90.).into(),
        });
    }
    // attach
    gleise_lego.add_attach(
        lego::GERADE,
        None,
        gerade::AnchorName::Ende,
        weiche_anchor_points.gerade,
    );
    gleise_lego.add_attach(
        lego::GERADE,
        None,
        gerade::AnchorName::Ende,
        kreuzung1_anchor_points.ende_1,
    );
    // relocate-attach
    if let Some(gleis_id) = &*kurve_lock.read() {
        gleise_lego.relocate_attach(
            gleis_id,
            kurve::AnchorName::Anfang,
            weiche_anchor_points.kurve,
        );
    }
    // remove
    let kreuzung0_lock_clone = kreuzung0_lock.clone();
    gleise_lego.remove(kreuzung0_lock);
    // assert!(kreuzung0_lock.read().is_none());
    assert!(kreuzung0_lock_clone.read().is_none());

    App::run(Settings {
        window: iced::window::Settings {
            size: (1024, 768),
            icon: Some(icon()),
            ..Default::default()
        },
        ..Settings::with_flags((gleise_märklin, gleise_lego))
    })
}
