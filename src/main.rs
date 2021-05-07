//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use iced::{Application, Clipboard, Command, Container, Element, Length, Settings};
use log::*;
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::gleise::{Gleis, GleisIdLock, Gleise, GleiseMap};
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::{self, *};
use zugkontrolle::zugtyp::{
    lego::{self, Lego},
    maerklin::{self, Maerklin},
};

struct AppendGleise<'t, Z> {
    gleise: &'t mut Gleise<Z>,
    y: canvas::Y,
}
impl<'t, Z> AppendGleise<'t, Z> {
    fn new(gleise: &'t mut Gleise<Z>) -> AppendGleise<'t, Z> {
        AppendGleise { gleise, y: canvas::Y(5.) }
    }
}

impl<'t, Z: Zugtyp + Eq + Debug> AppendGleise<'t, Z> {
    fn append<T>(&mut self, definition: T) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let size: canvas::Size = definition.size();
        let x: canvas::X = canvas::X(150.) - 0.5 * size.width;
        let height: canvas::Abstand<canvas::Y> = size.height;
        let res = self.gleise.add(Gleis {
            definition,
            position: canvas::Position {
                point: canvas::Point { x, y: self.y },
                winkel: Angle::new(0.),
            },
        });
        self.y += height + canvas::Y(25.).to_abstand();
        res
    }
}

mod background {
    pub(crate) struct White;
    impl iced::container::StyleSheet for White {
        fn style(&self) -> iced::container::Style {
            iced::container::Style {
                background: Some(iced::Background::Color(iced::Color::WHITE)),
                ..Default::default()
            }
        }
    }
    pub(crate) struct Black;
    impl iced::container::StyleSheet for Black {
        fn style(&self) -> iced::container::Style {
            iced::container::Style {
                background: Some(iced::Background::Color(iced::Color::BLACK)),
                ..Default::default()
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Message {
    ResizePane(iced::pane_grid::ResizeEvent),
    Maerklin(gleis::Message<Maerklin>),
    Lego(gleis::Message<Lego>),
}

enum AnyZugkontrolle {
    Maerklin(Zugkontrolle<Maerklin>),
    Lego(Zugkontrolle<Lego>),
}
struct App {
    pub pane_state: iced::pane_grid::State<AnyZugkontrolle>,
    pub pane_maerklin: iced::pane_grid::Pane,
    pub pane_lego: iced::pane_grid::Pane,
}
impl Application for App {
    type Executor = iced::executor::Default;
    type Message = Message;
    type Flags = (Gleise<Maerklin>, Gleise<Lego>);

    fn new((gleise_maerklin, gleise_lego): Self::Flags) -> (Self, Command<Self::Message>) {
        let (mut pane_state, pane_maerklin) = iced::pane_grid::State::new(
            AnyZugkontrolle::Maerklin(Zugkontrolle::new(gleise_maerklin).0),
        );
        let (pane_lego, _pane_split) = pane_state
            .split(
                iced::pane_grid::Axis::Vertical,
                &pane_maerklin,
                AnyZugkontrolle::Lego(Zugkontrolle::new(gleise_lego).0),
            )
            .expect("Failed to split pane!");
        (App { pane_state, pane_maerklin, pane_lego }, Command::none())
    }

    fn title(&self) -> String {
        "Zugkontrolle".to_string()
    }

    fn update(
        &mut self,
        message: Self::Message,
        clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        match message {
            Message::ResizePane(iced::pane_grid::ResizeEvent { split, ratio }) => {
                self.pane_state.resize(&split, ratio)
            }
            Message::Maerklin(message) => {
                if let Some(AnyZugkontrolle::Maerklin(zugkontrolle)) =
                    self.pane_state.get_mut(&self.pane_maerklin)
                {
                    zugkontrolle.update(message, clipboard);
                } else {
                    error!("Märklin-Pane nicht gefunden!")
                }
            }
            Message::Lego(message) => {
                if let Some(AnyZugkontrolle::Lego(zugkontrolle)) =
                    self.pane_state.get_mut(&self.pane_lego)
                {
                    zugkontrolle.update(message, clipboard);
                } else {
                    error!("Lego-Pane nicht gefunden!")
                }
            } // _ => println!("{:?}", message),
        }

        Command::none()
    }

    fn view(&mut self) -> Element<Self::Message> {
        let paned_grid = iced::PaneGrid::new(&mut self.pane_state, |_pane, gleise| match gleise {
            AnyZugkontrolle::Maerklin(zugkontrolle) => {
                zugkontrolle.view().map(|message| Message::Maerklin(message)).into()
            }
            AnyZugkontrolle::Lego(zugkontrolle) => {
                zugkontrolle.view().map(|message| Message::Lego(message)).into()
            }
        })
        .spacing(1)
        .on_resize(1, Message::ResizePane);
        Container::new(
            Container::new(paned_grid)
                .width(Length::Fill)
                .height(Length::Fill)
                .style(background::Black)
                .padding(1),
        )
        .width(Length::Fill)
        .height(Length::Fill)
        .style(background::White)
        .padding(2)
        .into()
    }
}

fn main() -> iced::Result {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Off)
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
    let mut gleise_maerklin: Gleise<Maerklin> = Gleise::new();
    let mut append_maerklin = AppendGleise::new(&mut gleise_maerklin);
    append_maerklin.append(maerklin::gerade_5106());
    append_maerklin.append(maerklin::kurve_5100());
    append_maerklin.append(maerklin::weiche_5202_links());
    append_maerklin.append(maerklin::dreiwege_weiche_5214());
    append_maerklin.append(maerklin::kurven_weiche_5140_links());
    append_maerklin.append(maerklin::kreuzung_5207());

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
        gleise_lego.relocate(
            gleis_id,
            canvas::Position {
                point: canvas::Point { x: canvas::X(250.), y: canvas::Y(10.) },
                winkel: AngleDegrees::new(90.).into(),
            },
        );
    }
    // attach
    gleise_lego.add_attach(lego::GERADE, gerade::AnchorName::Ende, weiche_anchor_points.gerade);
    gleise_lego.add_attach(lego::GERADE, gerade::AnchorName::Ende, kreuzung1_anchor_points.ende_1);
    // relocate-attach
    if let Some(gleis_id) = &*kurve_lock.read() {
        gleise_lego.relocate_attach(gleis_id, kurve::AnchorName::Ende, weiche_anchor_points.kurve);
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
        ..Settings::with_flags((gleise_maerklin, gleise_lego))
    })
}
