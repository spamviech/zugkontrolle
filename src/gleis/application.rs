//! iced::Application für die Gleis-Anzeige

use log::*;
use version::version;

use super::style::*;
use super::*;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnyGleis<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    Weiche(Weiche<Z>),
    DreiwegeWeiche(DreiwegeWeiche<Z>),
    KurvenWeiche(KurvenWeiche<Z>),
    SKurvenWeiche(SKurvenWeiche<Z>),
    Kreuzung(Kreuzung<Z>),
}
macro_rules! impl_any_gleis_from {
    ($type:ident) => {
        impl<Z> From<$type<Z>> for AnyGleis<Z> {
            fn from(gleis: $type<Z>) -> AnyGleis<Z> {
                AnyGleis::$type(gleis)
            }
        }
    };
}
impl_any_gleis_from! {Gerade}
impl_any_gleis_from! {Kurve}
impl_any_gleis_from! {Weiche}
impl_any_gleis_from! {DreiwegeWeiche}
impl_any_gleis_from! {KurvenWeiche}
impl_any_gleis_from! {SKurvenWeiche}
impl_any_gleis_from! {Kreuzung}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Modus {
    Bauen,
    Fahren,
}
impl Modus {
    fn make_radio<Z: 'static>(self, aktueller_modus: Option<Self>) -> iced::Radio<Message<Z>> {
        iced::Radio::new(self, self, aktueller_modus, Message::Modus)
    }
}
impl std::fmt::Display for Modus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Modus::Bauen => "Bauen",
            Modus::Fahren => "Fahren",
        };
        write!(f, "{}", display)
    }
}
impl From<Modus> for String {
    fn from(modus: Modus) -> Self {
        format!("{}", modus)
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    Gleis { gleis: AnyGleis<Z>, grab_height: Skalar },
    Modus(Modus),
}

impl<T: Clone + Into<AnyGleis<Z>>, Z> ButtonMessage<Message<Z>> for T {
    fn to_message(&self, grab_location: Vektor) -> Message<Z> {
        Message::Gleis { gleis: self.clone().into(), grab_height: grab_location.y }
    }
}

pub struct Zugkontrolle<Z> {
    // TODO Frage bei Gleise<Z> nach aktuellem Modus
    modus: Modus,
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<Gerade<Z>>>,
    kurven: Vec<Button<Kurve<Z>>>,
    weichen: Vec<Button<Weiche<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeiche<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeiche<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeiche<Z>>>,
    kreuzungen: Vec<Button<Kreuzung<Z>>>,
}
impl<Z: 'static + Zugtyp + Send> iced::Application for Zugkontrolle<Z> {
    type Executor = iced::executor::Default;
    type Flags = Gleise<Z>;
    type Message = Message<Z>;

    fn new(gleise: Self::Flags) -> (Self, iced::Command<Self::Message>) {
        (
            Zugkontrolle {
                modus: Modus::Bauen,
                gleise,
                scrollable_state: iced::scrollable::State::new(),
                geraden: Z::geraden().into_iter().map(Button::new).collect(),
                kurven: Z::kurven().into_iter().map(Button::new).collect(),
                weichen: Z::weichen().into_iter().map(Button::new).collect(),
                dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::new).collect(),
                kurven_weichen: Z::kurven_weichen().into_iter().map(Button::new).collect(),
                s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::new).collect(),
                kreuzungen: Z::kreuzungen().into_iter().map(Button::new).collect(),
            },
            iced::Command::none(),
        )
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", version!())
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut iced::Clipboard,
    ) -> iced::Command<Self::Message> {
        match message {
            Message::Gleis { gleis, grab_height } => {
                macro_rules! add_grabbed_at_mouse {
                    ($gleis:expr) => {{
                        self.gleise
                            .add_grabbed_at_mouse($gleis, Vektor { x: Skalar(0.), y: grab_height });
                    }};
                }
                match gleis {
                    AnyGleis::Gerade(gerade) => add_grabbed_at_mouse!(gerade),
                    AnyGleis::Kurve(kurve) => add_grabbed_at_mouse!(kurve),
                    AnyGleis::Weiche(weiche) => add_grabbed_at_mouse!(weiche),
                    AnyGleis::DreiwegeWeiche(dreiwege_weiche) => {
                        add_grabbed_at_mouse!(dreiwege_weiche)
                    },
                    AnyGleis::KurvenWeiche(kurven_weiche) => add_grabbed_at_mouse!(kurven_weiche),
                    AnyGleis::SKurvenWeiche(s_kurven_weiche) => {
                        add_grabbed_at_mouse!(s_kurven_weiche)
                    },
                    AnyGleis::Kreuzung(kreuzung) => add_grabbed_at_mouse!(kreuzung),
                }
            },
            Message::Modus(modus) => {
                // TODO informiere stattdessen gleise
                self.modus = modus;
                debug!("TODO Modus-Wechsel: {:?}", modus)
            },
        }

        iced::Command::none()
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        let Zugkontrolle {
            modus,
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = self;

        let mut scrollable = iced::Scrollable::new(scrollable_state);
        let mut max_width = 0;
        macro_rules! add_buttons {
                    ($($vec: expr),*) => {
                        $(
                        for button in $vec.iter() {
                            max_width = max_width.max( button.size().x.0.ceil() as u16);
                        }
                    )*
                    $(
                        for button in $vec {
                            scrollable = scrollable.push(
                                button.to_iced(Some(max_width))
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
        let scrollable_style = scrollable::Collection::new(10);
        let scroller_width = scrollable_style.width();
        // TODO frage stattdessen gleise
        let aktueller_modus = Some(*modus);
        iced::Column::new()
            .push(
                iced::Row::new()
                    .push(Modus::Bauen.make_radio(aktueller_modus))
                    .push(Modus::Fahren.make_radio(aktueller_modus))
                    .padding(5)
                    .spacing(5),
            )
            .push(iced::Rule::horizontal(1).style(rule::SEPARATOR))
            .push(
                iced::Row::new()
                    .push(
                        iced::Container::new(
                            scrollable
                                .scroller_width(scroller_width)
                                .width(iced::Length::Fill)
                                .height(iced::Length::Fill)
                                .style(scrollable_style),
                        )
                        .width(iced::Length::Units(max_width + scroller_width))
                        .height(iced::Length::Fill),
                    )
                    .push(iced::Rule::vertical(1).style(rule::SEPARATOR))
                    .push(
                        iced::Container::new(
                            iced::Canvas::new(gleise)
                                .width(iced::Length::Fill)
                                .height(iced::Length::Fill),
                        )
                        .width(iced::Length::Fill)
                        .height(iced::Length::Fill),
                    ),
            )
            .into()
    }
}
