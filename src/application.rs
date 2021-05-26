//! iced::Application für die Gleis-Anzeige

use std::convert::identity;
use std::fmt::Debug;

use log::*;
use serde::{Deserialize, Serialize};
use version::version;

mod touch_canvas;

#[macro_use]
pub mod gleis;
use gleis::{gleise::*, *};

pub mod style;
use style::*;

pub mod streckenabschnitt;

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
            Bewegen::Oben => vektor::EY,
            Bewegen::Unten => -vektor::EY,
            Bewegen::Links => vektor::EX,
            Bewegen::Rechts => -vektor::EX,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Drehen {
    Rechtsdrehend, // clockwise
    Linksdrehend,  // counterclockwise
}
impl Drehen {
    fn drehen(self) -> Winkel {
        match self {
            Drehen::Rechtsdrehend => winkel::TAU / 72.,
            Drehen::Linksdrehend => -winkel::TAU / 72.,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Skalieren {
    Größer,
    Kleiner,
}
impl Skalieren {
    fn skalieren(self) -> Skalar {
        Skalar(match self {
            Skalieren::Größer => 4. / 3.,
            Skalieren::Kleiner => 3. / 4.,
        })
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    Gleis { gleis: AnyGleis<Z>, grab_height: Skalar },
    Modus(Modus),
    Bewegen(Bewegen),
    Drehen(Drehen),
    Skalieren(Skalieren),
    ZeigeAuswahlStreckenabschnitt,
    SchließeAuswahlStreckenabschnitt,
    WähleStreckenabschnitt(Option<(streckenabschnitt::Name, iced::Color)>),
    // TODO HinzufügenStreckenabschnitt
    LöscheStreckenabschnitt(streckenabschnitt::Name),
    SetzeStreckenabschnitt(AnyIdLock<Z>),
    Speichern,
    Laden,
    Pfad(String),
}
impl<Z> From<gleise::Message<Z>> for Message<Z> {
    fn from(message: gleise::Message<Z>) -> Self {
        match message {
            gleise::Message::SetzeStreckenabschnitt(any_id_lock) => {
                Message::SetzeStreckenabschnitt(any_id_lock)
            },
        }
    }
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
    streckenabschnitt_aktuell: streckenabschnitt::Anzeige,
    streckenabschnitt_auswahl: streckenabschnitt::Auswahl,
    // TODO use a good-looking solution instead of simple buttons
    oben: iced::button::State,
    unten: iced::button::State,
    links: iced::button::State,
    rechts: iced::button::State,
    clockwise: iced::button::State,
    counter_clockwise: iced::button::State,
    größer: iced::button::State,
    kleiner: iced::button::State,
    speichern: iced::button::State,
    laden: iced::button::State,
    pfad: iced::text_input::State,
    aktueller_pfad: String,
    // TODO Streckenabschnitt
}

impl<Z> iced::Application for Zugkontrolle<Z>
where
    Z: 'static + Zugtyp + Debug + PartialEq + Serialize + for<'de> Deserialize<'de> + Send + Sync,
{
    type Executor = iced::executor::Default;
    type Flags = Gleise<Z>;
    type Message = Message<Z>;

    fn new(gleise: Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let streckenabschnitt_auswahl =
            streckenabschnitt::Auswahl::neu(gleise.streckenabschnitte());
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
                streckenabschnitt_aktuell: streckenabschnitt::Anzeige {
                    aktuell: None,
                    auswählen: iced::button::State::new(),
                },
                streckenabschnitt_auswahl,
                oben: iced::button::State::new(),
                unten: iced::button::State::new(),
                links: iced::button::State::new(),
                rechts: iced::button::State::new(),
                clockwise: iced::button::State::new(),
                counter_clockwise: iced::button::State::new(),
                größer: iced::button::State::new(),
                kleiner: iced::button::State::new(),
                speichern: iced::button::State::new(),
                laden: iced::button::State::new(),
                pfad: iced::text_input::State::new(),
                aktueller_pfad: format!("{}.zug", Z::NAME),
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
                let streckenabschnitt = self
                    .streckenabschnitt_aktuell
                    .aktuell
                    .as_ref()
                    .map(|(name, _farbe)| name.clone());
                macro_rules! add_grabbed_at_mouse {
                    ($gleis:expr) => {{
                        self.gleise.add_grabbed_at_mouse(
                            $gleis,
                            Vektor { x: Skalar(0.), y: grab_height },
                            streckenabschnitt,
                        );
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
                    bewegen.bewegen().rotiert(-self.gleise.pivot().winkel)
                        / self.gleise.skalierfaktor(),
                );
            },
            Message::Drehen(drehen) => self.gleise.drehen(drehen.drehen()),
            Message::Skalieren(skalieren) => self.gleise.skalieren(skalieren.skalieren()),
            Message::ZeigeAuswahlStreckenabschnitt => {
                self.streckenabschnitt_auswahl.0.show(true);
            },
            Message::SchließeAuswahlStreckenabschnitt => {
                self.streckenabschnitt_auswahl.0.show(false);
            },
            Message::WähleStreckenabschnitt(aktuell) => {
                self.streckenabschnitt_aktuell.aktuell = aktuell;
                self.streckenabschnitt_auswahl.0.show(false);
            },
            Message::LöscheStreckenabschnitt(name) => {
                if self
                    .streckenabschnitt_aktuell
                    .aktuell
                    .as_ref()
                    .map_or(false, |(aktuell_name, _farbe)| aktuell_name == &name)
                {
                    self.streckenabschnitt_aktuell.aktuell = None;
                }
                self.streckenabschnitt_auswahl.0.inner_mut().entferne(&name);
                self.gleise.entferne_streckenabschnitt(name);
                self.gleise.erzwinge_neuzeichnen()
            },
            Message::SetzeStreckenabschnitt(any_id_lock) => {
                with_any_id_lock!(
                    any_id_lock,
                    Gleise::setze_streckenabschnitt_unit,
                    &mut self.gleise,
                    self.streckenabschnitt_aktuell
                        .aktuell
                        .as_ref()
                        .map(|(name, _farbe)| name.clone())
                );
            },
            Message::Speichern => {
                if let Err(err) = self.gleise.speichern(&self.aktueller_pfad) {
                    // TODO show a message box with the error message
                    error!("Error while saving to {}: {:?}", self.aktueller_pfad, err)
                }
            },
            Message::Laden => {
                if let Err(err) = self.gleise.laden(&self.aktueller_pfad) {
                    // TODO show a message box with the error message
                    error!("Error while loading from {}: {:?}", self.aktueller_pfad, err)
                } else {
                    self.streckenabschnitt_aktuell.aktuell = None;
                    self.streckenabschnitt_auswahl
                        .0
                        .inner_mut()
                        .update(self.gleise.streckenabschnitte());
                }
            },
            Message::Pfad(pfad) => self.aktueller_pfad = pfad,
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
            streckenabschnitt_aktuell,
            streckenabschnitt_auswahl,
            oben,
            unten,
            links,
            rechts,
            clockwise,
            counter_clockwise,
            größer,
            kleiner,
            speichern,
            laden,
            pfad,
            aktueller_pfad,
        } = self;
        let aktueller_modus = gleise.modus();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell,
            oben,
            unten,
            links,
            rechts,
            clockwise,
            counter_clockwise,
            größer,
            kleiner,
            speichern,
            laden,
            pfad,
            aktueller_pfad,
        );
        let row_with_scrollable = row_with_scrollable(
            aktueller_modus,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );

        let column = iced::Column::new()
            .push(top_row)
            .push(iced::Rule::horizontal(1).style(rule::SEPARATOR))
            .push(
                row_with_scrollable.push(
                    iced::Container::new(
                        iced::Element::from(
                            touch_canvas::Canvas::new(gleise)
                                .width(iced::Length::Fill)
                                .height(iced::Length::Fill),
                        )
                        // TODO sinnvolle Map-Funktion
                        .map(Into::into),
                    )
                    .width(iced::Length::Fill)
                    .height(iced::Length::Fill),
                ),
            );

        streckenabschnitt_auswahl.view(column).into()
    }
}

fn top_row<'t, Z: 'static>(
    aktueller_modus: Modus,
    streckenabschnitt: &'t mut streckenabschnitt::Anzeige,
    oben: &'t mut iced::button::State,
    unten: &'t mut iced::button::State,
    links: &'t mut iced::button::State,
    rechts: &'t mut iced::button::State,
    clockwise: &'t mut iced::button::State,
    counter_clockwise: &'t mut iced::button::State,
    größer: &'t mut iced::button::State,
    kleiner: &'t mut iced::button::State,
    speichern: &'t mut iced::button::State,
    laden: &'t mut iced::button::State,
    pfad: &'t mut iced::text_input::State,
    aktueller_pfad: &'t str,
) -> iced::Row<'t, Message<Z>> {
    let modus_radios = iced::Column::new()
        .push(Modus::Bauen.make_radio(aktueller_modus))
        .push(Modus::Fahren.make_radio(aktueller_modus));
    let oben_unten_buttons = iced::Column::new()
        .push(iced::Button::new(oben, iced::Text::new("^")).on_press(Bewegen::Oben))
        .push(iced::Button::new(unten, iced::Text::new("v")).on_press(Bewegen::Unten))
        .align_items(iced::Align::Center);
    let move_buttons = iced::Row::new()
        .push(iced::Button::new(links, iced::Text::new("<")).on_press(Bewegen::Links))
        .push(oben_unten_buttons)
        .push(iced::Button::new(rechts, iced::Text::new(">")).on_press(Bewegen::Rechts))
        .align_items(iced::Align::Center);
    // unicode-support nicht vollständig in iced, daher ascii-basierter text für den Moment
    let drehen_buttons = iced::Column::new()
        .push(
            iced::Button::new(counter_clockwise, iced::Text::new("ccw" /* "↺" */))
                .on_press(Drehen::Linksdrehend),
        )
        .push(
            iced::Button::new(clockwise, iced::Text::new("cw" /* "↻" */))
                .on_press(Drehen::Rechtsdrehend),
        )
        .align_items(iced::Align::Center);
    let skalieren_buttons = iced::Column::new()
        .push(iced::Button::new(größer, iced::Text::new("+")).on_press(Skalieren::Größer))
        .push(iced::Button::new(kleiner, iced::Text::new("-")).on_press(Skalieren::Kleiner))
        .align_items(iced::Align::Center);
    let speichern_laden = iced::Row::new()
        .push(
            iced::Column::new()
                .push(
                    iced::Button::new(speichern, iced::Text::new("speichern"))
                        .on_press(Message::Speichern),
                )
                .push(iced::Button::new(laden, iced::Text::new("laden")).on_press(Message::Laden))
                .align_items(iced::Align::End),
        )
        .push(
            iced::TextInput::new(pfad, "pfad", aktueller_pfad, Message::Pfad)
                .width(iced::Length::Units(250))
                .padding(1),
        )
        .spacing(5)
        .align_items(iced::Align::Center)
        .width(iced::Length::Shrink);
    iced::Row::new()
        .push(modus_radios.mit_teil_nachricht(Message::Modus))
        .push(move_buttons.mit_teil_nachricht(Message::Bewegen))
        .push(drehen_buttons.mit_teil_nachricht(Message::Drehen))
        .push(skalieren_buttons.mit_teil_nachricht(Message::Skalieren))
        .push(streckenabschnitt.view(Message::ZeigeAuswahlStreckenabschnitt))
        .push(iced::Space::new(iced::Length::Fill, iced::Length::Shrink))
        .push(speichern_laden)
        .padding(5)
        .spacing(5)
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
}

fn row_with_scrollable<'t, Z: 'static + Zugtyp>(
    aktueller_modus: Modus,
    scrollable_state: &'t mut iced::scrollable::State,
    geraden: &'t mut Vec<Button<Gerade<Z>>>,
    kurven: &'t mut Vec<Button<Kurve<Z>>>,
    weichen: &'t mut Vec<Button<Weiche<Z>>>,
    dreiwege_weichen: &'t mut Vec<Button<DreiwegeWeiche<Z>>>,
    kurven_weichen: &'t mut Vec<Button<KurvenWeiche<Z>>>,
    s_kurven_weichen: &'t mut Vec<Button<SKurvenWeiche<Z>>>,
    kreuzungen: &'t mut Vec<Button<Kreuzung<Z>>>,
) -> iced::Row<'t, Message<Z>> {
    // TODO Save/Load/Move?/Rotate?
    // Bauen(Streckenabschnitt?/Geschwindigkeit?/Löschen?)
    // Fahren(Streckenabschnitt-Anzeige?
    let mut scrollable = iced::Scrollable::new(scrollable_state);
    let mut max_width = None;
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
    let row = iced::Row::new();
    match max_width {
        Some(width) => row
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
            .push(iced::Rule::vertical(1).style(rule::SEPARATOR)),
        None => row,
    }
}
