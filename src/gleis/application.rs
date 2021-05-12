//! iced::Application für die Gleis-Anzeige

use std::convert::identity;

use version::version;

use super::gleise::*;
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

impl Modus {
    fn make_radio(self, aktueller_modus: Self) -> iced::Radio<Modus> {
        iced::Radio::new(self, self, Some(aktueller_modus), identity)
    }
}

#[derive(Debug, Clone)]
pub enum Bewegen {
    Oben,
    Unten,
    Links,
    Rechts,
}
impl Bewegen {
    fn bewegen(self) -> Vektor {
        match self {
            Bewegen::Oben => Vektor { x: Skalar(0.), y: Skalar(1.) },
            Bewegen::Unten => Vektor { x: Skalar(0.), y: Skalar(-1.) },
            Bewegen::Links => Vektor { x: Skalar(1.), y: Skalar(0.) },
            Bewegen::Rechts => Vektor { x: Skalar(-1.), y: Skalar(0.) },
        }
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    Gleis { gleis: AnyGleis<Z>, grab_height: Skalar },
    Modus(Modus),
    Bewegen(Bewegen),
    Drehen(Winkel),
    Skalieren(Skalar),
}

impl<T: Clone + Into<AnyGleis<Z>>, Z> ButtonMessage<Message<Z>> for T {
    fn to_message(&self, grab_location: Vektor) -> Message<Z> {
        Message::Gleis { gleis: self.clone().into(), grab_height: grab_location.y }
    }
}

trait MitTeilNachricht<'t, Msg: 'static>: Into<iced::Element<'t, Msg>> {
    fn mit_teil_nachricht<Z: 'static>(
        self,
        konstruktor: impl Fn(Msg) -> Message<Z> + 'static,
    ) -> iced::Element<'t, Message<Z>> {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<iced::Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

pub struct Zugkontrolle<Z> {
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<Gerade<Z>>>,
    kurven: Vec<Button<Kurve<Z>>>,
    weichen: Vec<Button<Weiche<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeiche<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeiche<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeiche<Z>>>,
    kreuzungen: Vec<Button<Kreuzung<Z>>>,
    // TODO use a good-looking solution instead of simple buttons
    oben: iced::button::State,
    unten: iced::button::State,
    links: iced::button::State,
    rechts: iced::button::State,
    clockwise: iced::button::State,
    counter_clockwise: iced::button::State,
    größer: iced::button::State,
    kleiner: iced::button::State,
}
impl<Z: 'static + Zugtyp + Send> iced::Application for Zugkontrolle<Z> {
    type Executor = iced::executor::Default;
    type Flags = Gleise<Z>;
    type Message = Message<Z>;

    fn new(gleise: Self::Flags) -> (Self, iced::Command<Self::Message>) {
        (
            Zugkontrolle {
                gleise,
                scrollable_state: iced::scrollable::State::new(),
                geraden: Z::geraden().into_iter().map(Button::new).collect(),
                kurven: Z::kurven().into_iter().map(Button::new).collect(),
                weichen: Z::weichen().into_iter().map(Button::new).collect(),
                dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::new).collect(),
                kurven_weichen: Z::kurven_weichen().into_iter().map(Button::new).collect(),
                s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::new).collect(),
                kreuzungen: Z::kreuzungen().into_iter().map(Button::new).collect(),
                oben: iced::button::State::new(),
                unten: iced::button::State::new(),
                links: iced::button::State::new(),
                rechts: iced::button::State::new(),
                clockwise: iced::button::State::new(),
                counter_clockwise: iced::button::State::new(),
                größer: iced::button::State::new(),
                kleiner: iced::button::State::new(),
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
            Message::Modus(modus) => self.gleise.moduswechsel(modus),
            Message::Bewegen(bewegen) => {
                self.gleise.bewege_pivot(
                    self.gleise.skalierfaktor()
                        * bewegen.bewegen().rotiere(self.gleise.pivot().winkel),
                );
            },
            Message::Drehen(winkel) => self.gleise.drehen(winkel),
            Message::Skalieren(skalieren) => self.gleise.skalieren(skalieren),
        }

        iced::Command::none()
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        let Zugkontrolle {
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            oben,
            unten,
            links,
            rechts,
            clockwise,
            counter_clockwise,
            größer,
            kleiner,
        } = self;

        let mut scrollable = iced::Scrollable::new(scrollable_state);
        let mut max_width = None;
        let aktueller_modus = gleise.modus();
        match aktueller_modus {
            Modus::Bauen => {
                macro_rules! add_buttons {
                    ($($vec: expr),*) => {
                        max_width = Vec::new().into_iter()
                            $(.chain($vec.iter().map(|button| button.size().x.0.ceil() as u16)))*
                            .max();
                        $(
                            for button in $vec {
                                scrollable = scrollable.push(button.to_iced(max_width));
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
            },
            Modus::Fahren => {
                // TODO Geschwindigkeiten?, Wegstrecken?, Pläne?
            },
        }
        let scrollable_style = scrollable::Collection::new(10);
        let scroller_width = scrollable_style.width();
        let modus_radios = iced::Column::new()
            .push(Modus::Bauen.make_radio(aktueller_modus))
            .push(Modus::Fahren.make_radio(aktueller_modus));
        let move_buttons = iced::Column::new()
            .push(iced::Button::new(oben, iced::Text::new("^")).on_press(Bewegen::Oben))
            .push(
                iced::Row::new()
                    .push(iced::Button::new(links, iced::Text::new("<")).on_press(Bewegen::Links))
                    .push(
                        iced::Button::new(rechts, iced::Text::new(">")).on_press(Bewegen::Rechts),
                    ),
            )
            .push(iced::Button::new(unten, iced::Text::new("v")).on_press(Bewegen::Unten))
            .align_items(iced::Align::Center);
        // unicode-support nicht vollständig in iced, daher ascii-basierter text für den Moment
        let drehen_buttons = iced::Column::new()
            .push(
                iced::Button::new(counter_clockwise, iced::Text::new("ccw" /* "↺" */))
                    .on_press(Winkel(-0.25)),
            )
            .push(
                iced::Button::new(clockwise, iced::Text::new("cw" /* "↻" */))
                    .on_press(Winkel(0.25)),
            );
        let skalieren_buttons = iced::Column::new()
            .push(iced::Button::new(größer, iced::Text::new("+")).on_press(Skalar(1.5)))
            .push(iced::Button::new(kleiner, iced::Text::new("-")).on_press(Skalar(0.75)));
        // TODO Save/Load/Move?/Rotate?
        // Bauen(Streckenabschnitt?/Geschwindigkeit?/Löschen?)
        // Fahren(Streckenabschnitt-Anzeige?
        iced::Column::new()
            .push(
                iced::Row::new()
                    .push(modus_radios.mit_teil_nachricht(Message::Modus))
                    .push(move_buttons.mit_teil_nachricht(Message::Bewegen))
                    .push(drehen_buttons.mit_teil_nachricht(Message::Drehen))
                    .push(skalieren_buttons.mit_teil_nachricht(Message::Skalieren))
                    .padding(5)
                    .spacing(5)
                    .width(iced::Length::Fill)
                    .height(iced::Length::Shrink),
            )
            .push(iced::Rule::horizontal(1).style(rule::SEPARATOR))
            .push(
                max_width
                    .map_or(iced::Row::new(), |width| {
                        iced::Row::new()
                            .push(
                                iced::Container::new(
                                    scrollable
                                        .scroller_width(scroller_width)
                                        .width(iced::Length::Fill)
                                        .height(iced::Length::Fill)
                                        .style(scrollable_style),
                                )
                                .width(iced::Length::Units(width + scroller_width))
                                .height(iced::Length::Fill),
                            )
                            .push(iced::Rule::vertical(1).style(rule::SEPARATOR))
                    })
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
