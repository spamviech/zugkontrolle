//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use iced::{Application, Clipboard, Command, Container, Element, Length, Row, Settings};
use log::*;
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::button::{Button, ButtonMessage};
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::widget::{Gleis, GleisIdLock, Gleise, GleiseMap};
use zugkontrolle::gleis::{
    anchor,
    gerade::{self, Gerade},
    kreuzung::Kreuzung,
    kurve::{self, Kurve},
    weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche},
};
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
    Gerade(Gerade<Maerklin>),
    Kurve(Kurve<Maerklin>),
    Weiche(Weiche<Maerklin>),
    DreiwegeWeiche(DreiwegeWeiche<Maerklin>),
    KurvenWeiche(KurvenWeiche<Maerklin>),
    SKurvenWeiche(SKurvenWeiche<Maerklin>),
    Kreuzung(Kreuzung<Maerklin>),
}
macro_rules! impl_button_message {
    ($type:ident) => {
        impl ButtonMessage<Message> for $type<Maerklin> {
            fn to_message(&self) -> Message {
                Message::$type(self.clone())
            }
        }
    };
}
impl_button_message! {Gerade}
impl_button_message! {Kurve}
impl_button_message! {Weiche}
impl_button_message! {DreiwegeWeiche}
impl_button_message! {KurvenWeiche}
impl_button_message! {SKurvenWeiche}
impl_button_message! {Kreuzung}

enum AnyGleise {
    Maerklin {
        gleise: Gleise<Maerklin>,
        scrollable_state: iced::scrollable::State,
        geraden: Vec<Button<Gerade<Maerklin>>>,
        kurven: Vec<Button<Kurve<Maerklin>>>,
        weichen: Vec<Button<Weiche<Maerklin>>>,
        dreiwege_weichen: Vec<Button<DreiwegeWeiche<Maerklin>>>,
        kurven_weichen: Vec<Button<KurvenWeiche<Maerklin>>>,
        s_kurven_weichen: Vec<Button<SKurvenWeiche<Maerklin>>>,
        kreuzungen: Vec<Button<Kreuzung<Maerklin>>>,
    },
    Lego(Gleise<Lego>),
}
struct Zugkontrolle {
    pub pane_state: iced::pane_grid::State<AnyGleise>,
    pub pane_maerklin: iced::pane_grid::Pane,
    pub pane_lego: iced::pane_grid::Pane,
}
impl Application for Zugkontrolle {
    type Executor = iced::executor::Default;
    type Message = Message;
    type Flags = (Gleise<Maerklin>, Gleise<Lego>);

    fn new((gleise_maerklin, gleise_lego): Self::Flags) -> (Self, Command<Self::Message>) {
        let (mut pane_state, pane_maerklin) = iced::pane_grid::State::new(AnyGleise::Maerklin {
            gleise: gleise_maerklin,
            scrollable_state: iced::scrollable::State::new(),
            geraden: Maerklin::geraden().into_iter().map(Button::new).collect(),
            kurven: Maerklin::kurven().into_iter().map(Button::new).collect(),
            weichen: Maerklin::weichen().into_iter().map(Button::new).collect(),
            dreiwege_weichen: Maerklin::dreiwege_weichen().into_iter().map(Button::new).collect(),
            kurven_weichen: Maerklin::kurven_weichen().into_iter().map(Button::new).collect(),
            s_kurven_weichen: Maerklin::s_kurven_weichen().into_iter().map(Button::new).collect(),
            kreuzungen: Maerklin::kreuzungen().into_iter().map(Button::new).collect(),
        });
        let (pane_lego, _pane_split) = pane_state
            .split(iced::pane_grid::Axis::Vertical, &pane_maerklin, AnyGleise::Lego(gleise_lego))
            .expect("Failed to split pane!");
        (Zugkontrolle { pane_state, pane_maerklin, pane_lego }, Command::none())
    }

    fn title(&self) -> String {
        "Zugkontrolle".to_string()
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        macro_rules! add_gleis {
            ($gleis: expr) => {
                if let Some(AnyGleise::Maerklin { gleise, .. }) =
                    self.pane_state.get_mut(&self.pane_maerklin)
                {
                    if let Some(y) = gleise.last_mouse_y() {
                        gleise.add(Gleis {
                            definition: $gleis,
                            position: canvas::Position {
                                point: canvas::Point { x: canvas::X(0.), y },
                                winkel: Angle::new(0.),
                            },
                        });
                    } else {
                        warn!("last_mouse_y liefert None-Wert")
                    }
                } else {
                    error!("Märklin-Pane nicht gefunden!")
                }
            };
        }
        match message {
            Message::ResizePane(iced::pane_grid::ResizeEvent { split, ratio }) => {
                self.pane_state.resize(&split, ratio)
            }
            Message::Gerade(gerade) => add_gleis!(gerade),
            Message::Kurve(kurve) => add_gleis!(kurve),
            Message::Weiche(weiche) => add_gleis!(weiche),
            Message::DreiwegeWeiche(dreiwege_weiche) => add_gleis!(dreiwege_weiche),
            Message::KurvenWeiche(kurven_weiche) => add_gleis!(kurven_weiche),
            Message::SKurvenWeiche(s_kurven_weiche) => add_gleis!(s_kurven_weiche),
            Message::Kreuzung(kreuzung) => add_gleis!(kreuzung),
            // _ => println!("{:?}", message),
        }

        Command::none()
    }

    fn view(&mut self) -> Element<Self::Message> {
        let paned_grid = iced::PaneGrid::new(&mut self.pane_state, |_pane, gleise| match gleise {
            AnyGleise::Maerklin {
                gleise,
                scrollable_state,
                geraden,
                kurven,
                weichen,
                dreiwege_weichen,
                kurven_weichen,
                s_kurven_weichen,
                kreuzungen,
            } => {
                let mut scrollable = iced::Scrollable::new(scrollable_state);
                let mut max_width = 0;
                macro_rules! add_buttons {
                    ($($vec: expr),*) => {
                        $(
                        for button in $vec {
                            // include padding
                            max_width = max_width.max((canvas::X(4.) + button.size().width).0.ceil() as u16);
                            scrollable = scrollable.push(
                                button.to_button().width(Length::Fill).height(Length::Shrink)
                            );
                        }
                    )*
                    }
                }
                add_buttons!(
                    geraden,
                    kurven,
                    weichen,
                    dreiwege_weichen,
                    kurven_weichen,
                    s_kurven_weichen,
                    kreuzungen
                );
                Container::new(
                    Row::new()
                        .push(scrollable.width(Length::Units(max_width)).height(Length::Fill))
                        .push(iced::Canvas::new(gleise).width(Length::Fill).height(Length::Fill)),
                )
                .width(Length::Fill)
                .height(Length::Fill)
                .style(background::White)
                .into()
            }
            AnyGleise::Lego(gleise_lego) => Container::new(
                iced::Canvas::new(gleise_lego).width(Length::Fill).height(Length::Fill),
            )
            .width(Length::Fill)
            .height(Length::Fill)
            .style(background::White)
            .into(),
        })
        .spacing(1)
        .on_resize(0, Message::ResizePane);
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
        .padding(10)
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

    Zugkontrolle::run(Settings {
        window: iced::window::Settings {
            size: (900, 700),
            icon: Some(icon()),
            ..Default::default()
        },
        ..Settings::with_flags((gleise_maerklin, gleise_lego))
    })
}

fn icon() -> iced::window::Icon {
    // originally created using the /image/ crate, printing out the resulting ImageBuffer::into_raw()
    // let buf = match image::open("Icon/Zugkontrolle.png").expect("failed to load image") {
    //     image::DynamicImage::ImageRgba8(buf) => buf,
    //     _ => unimplemented!(),
    // };
    // let (width, height) = buf.dimensions();
    // let data = buf.into_raw();
    // println!("{}x{}\n{:?}", width, height, data);
    let width = 32;
    let height = 32;
    let data = vec![
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67, 255, 67, 67, 67,
        255, 67, 67, 67, 255, 67, 67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67,
        67, 255, 67, 67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67,
        255, 67, 67, 67, 255, 67, 67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67,
        67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 67,
        67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67,
        67, 255, 255, 255, 255, 254, 255, 255, 255, 254, 255, 255, 255, 254, 255, 255, 255, 254,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67,
        255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 254, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254, 255, 255, 255, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 254, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254,
        255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 254, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254, 255, 255, 255, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254, 255,
        255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 67, 67, 67, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67,
        255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 67, 67, 67, 255, 67, 67, 67, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 67, 67, 67, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0,
        0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0,
        255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0,
        0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255,
        0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    ];

    iced::window::Icon::from_rgba(data, width, height).expect("failed to create Icon")
}
