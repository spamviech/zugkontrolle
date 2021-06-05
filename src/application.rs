//! iced::Application für die Gleis-Anzeige

use std::collections::BTreeMap;
use std::convert::identity;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};
use version::version;

use crate::anschluss::anschlüsse::Anschlüsse;
use crate::steuerung::{geschwindigkeit, streckenabschnitt::Streckenabschnitt};

mod touch_canvas;

pub mod gleis;
use gleis::{
    gleise::{id::with_any_id_lock, *},
    kreuzung::*,
    weiche::*,
    *,
};

pub mod typen;
pub use typen::*;

pub mod style;
use style::*;

pub mod streckenabschnitt;

pub mod anschluss;

pub(crate) mod macros;

pub mod farbwahl;

pub mod icon;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnyGleis<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    WeicheUnit(WeicheUnit<Z>),
    DreiwegeWeicheUnit(DreiwegeWeicheUnit<Z>),
    KurvenWeicheUnit(KurvenWeicheUnit<Z>),
    SKurvenWeicheUnit(SKurvenWeicheUnit<Z>),
    KreuzungUnit(KreuzungUnit<Z>),
}
macro_rules! impl_any_gleis_from {
    ($type:ident) => {
        impl<Z> From<$type<Z>> for AnyGleis<Z> {
            fn from(gleis: $type<Z>) -> AnyGleis<Z> {
                AnyGleis::$type(gleis.into())
            }
        }
    };
}
impl_any_gleis_from! {Gerade}
impl_any_gleis_from! {Kurve}
impl_any_gleis_from! {WeicheUnit}
impl_any_gleis_from! {DreiwegeWeicheUnit}
impl_any_gleis_from! {KurvenWeicheUnit}
impl_any_gleis_from! {SKurvenWeicheUnit}
impl_any_gleis_from! {KreuzungUnit}

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
    SchließeModal,
    SchließeMessageBox,
    ZeigeAuswahlStreckenabschnitt,
    WähleStreckenabschnitt(Option<(streckenabschnitt::Name, iced::Color)>),
    HinzufügenStreckenabschnitt(streckenabschnitt::Name, iced::Color, anschluss::OutputAnschluss),
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

#[derive(Debug)]
enum Modal {
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
}

#[derive(Debug)]
struct MessageBox {
    titel: String,
    nachricht: String,
    button_state: iced::button::State,
}

pub struct Zugkontrolle<Z: Zugtyp> {
    anschlüsse: Anschlüsse,
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<Gerade<Z>>>,
    kurven: Vec<Button<Kurve<Z>>>,
    weichen: Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
    modal_state: iced_aw::modal::State<Modal>,
    streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus,
    message_box: iced_aw::modal::State<MessageBox>,
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
    // TODO Geschwindigkeit, Wegstrecke, Plan
}

impl<Z: Zugtyp> Zugkontrolle<Z> {
    fn zeige_message_box(&mut self, titel_arg: String, nachricht_arg: String) {
        let MessageBox { titel, nachricht, .. } = self.message_box.inner_mut();
        *titel = titel_arg;
        *nachricht = nachricht_arg;
        self.message_box.show(true)
    }

    /// Icon für das Fenster.
    pub fn icon() -> iced::window::Icon {
        icon::icon()
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: 'static + Zugtyp + Debug + PartialEq + for<'de> Deserialize<'de>,
{
    fn laden(&mut self) {
        if let Err(err) = self.gleise.laden(&mut self.anschlüsse, &self.aktueller_pfad) {
            self.zeige_message_box(
                format!("Fehler beim Laden von {}", self.aktueller_pfad),
                format!("{:?}", err),
            )
        } else {
            self.streckenabschnitt_aktuell.aktuell = None;
        }
    }
}

impl<Z> iced::Application for Zugkontrolle<Z>
where
    Z: 'static + Zugtyp + Debug + PartialEq + Serialize + for<'de> Deserialize<'de> + Send + Sync,
    Z::Leiter: Debug,
{
    type Executor = iced::executor::Default;
    type Flags = (Anschlüsse, Option<String>);
    type Message = Message<Z>;

    fn new((anschlüsse, pfad_arg): Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let gleise = Gleise::neu();
        let auswahl_status = streckenabschnitt::AuswahlStatus::neu(gleise.streckenabschnitte());
        let mut zugkontrolle = Zugkontrolle {
            anschlüsse,
            gleise,
            scrollable_state: iced::scrollable::State::new(),
            geraden: Z::geraden().into_iter().map(Button::new).collect(),
            kurven: Z::kurven().into_iter().map(Button::new).collect(),
            weichen: Z::weichen().into_iter().map(Button::new).collect(),
            dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::new).collect(),
            kurven_weichen: Z::kurven_weichen().into_iter().map(Button::new).collect(),
            s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::new).collect(),
            kreuzungen: Z::kreuzungen().into_iter().map(Button::new).collect(),
            geschwindigkeiten: BTreeMap::new(),
            modal_state: iced_aw::modal::State::new(Modal::Streckenabschnitt(auswahl_status)),
            streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus::neu(),
            message_box: iced_aw::modal::State::new(MessageBox {
                titel: "Nicht initialisiert".to_string(),
                nachricht: "Diese Nachricht sollte nicht sichtbar sein!".to_string(),
                button_state: iced::button::State::new(),
            }),
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
        };
        if let Some(pfad) = pfad_arg {
            zugkontrolle.aktueller_pfad = pfad;
            zugkontrolle.laden()
        }
        (zugkontrolle, iced::Command::none())
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
                    AnyGleis::WeicheUnit(weiche) => {
                        let WeicheUnit {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            orientierung,
                            beschreibung,
                            steuerung: (),
                        } = weiche;
                        add_grabbed_at_mouse!(Weiche {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            orientierung,
                            beschreibung,
                            steuerung: None
                        })
                    },
                    AnyGleis::DreiwegeWeicheUnit(dreiwege_weiche) => {
                        let DreiwegeWeicheUnit {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            beschreibung,
                            steuerung: (),
                        } = dreiwege_weiche;
                        add_grabbed_at_mouse!(DreiwegeWeiche {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            beschreibung,
                            steuerung: None
                        })
                    },
                    AnyGleis::KurvenWeicheUnit(kurven_weiche) => {
                        let KurvenWeicheUnit {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            orientierung,
                            beschreibung,
                            steuerung: (),
                        } = kurven_weiche;
                        add_grabbed_at_mouse!(KurvenWeiche {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            orientierung,
                            beschreibung,
                            steuerung: None
                        })
                    },
                    AnyGleis::SKurvenWeicheUnit(s_kurven_weiche) => {
                        let SKurvenWeiche {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            radius_reverse,
                            winkel_reverse,
                            orientierung,
                            beschreibung,
                            steuerung: (),
                        } = s_kurven_weiche;
                        add_grabbed_at_mouse!(SKurvenWeiche {
                            zugtyp,
                            länge,
                            radius,
                            winkel,
                            radius_reverse,
                            winkel_reverse,
                            orientierung,
                            beschreibung,
                            steuerung: None
                        })
                    },
                    AnyGleis::KreuzungUnit(kreuzung) => {
                        let Kreuzung {
                            zugtyp,
                            länge,
                            radius,
                            variante,
                            beschreibung,
                            steuerung: (),
                        } = kreuzung;
                        add_grabbed_at_mouse!(Kreuzung {
                            zugtyp,
                            länge,
                            radius,
                            variante,
                            beschreibung,
                            steuerung: None
                        })
                    },
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
            Message::SchließeModal => {
                self.modal_state.show(false);
            },
            Message::ZeigeAuswahlStreckenabschnitt => {
                *self.modal_state.inner_mut() = Modal::Streckenabschnitt(
                    streckenabschnitt::AuswahlStatus::neu(self.gleise.streckenabschnitte()),
                );
                self.modal_state.show(true);
            },
            Message::WähleStreckenabschnitt(aktuell) => {
                self.streckenabschnitt_aktuell.aktuell = aktuell;
            },
            Message::HinzufügenStreckenabschnitt(name, farbe, anschluss_definition) => {
                let anschluss = match anschluss_definition {
                    anschluss::OutputAnschluss::Pin { pin, polarität } => self
                        .anschlüsse
                        .reserviere_pin(pin)
                        .map_err(crate::anschluss::Error::from)
                        .map(crate::anschluss::Pin::into_output)
                        .map(|pin| crate::anschluss::OutputAnschluss::Pin { pin, polarität }),
                    anschluss::OutputAnschluss::Pcf8574Port {
                        a0,
                        a1,
                        a2,
                        variante,
                        port,
                        polarität,
                    } => self
                        .anschlüsse
                        .reserviere_pcf8574_port(a0, a1, a2, variante, port)
                        .map_err(crate::anschluss::Error::from)
                        .and_then(|port| port.into_output().map_err(Into::into))
                        .map(|port| crate::anschluss::OutputAnschluss::Pcf8574Port {
                            port,
                            polarität,
                        }),
                };
                match anschluss {
                    Ok(anschluss) => {
                        self.streckenabschnitt_aktuell.aktuell = Some((name.clone(), farbe));
                        let streckenabschnitt = Streckenabschnitt { farbe, anschluss };
                        match self.modal_state.inner_mut() {
                            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => {
                                streckenabschnitt_auswahl.hinzufügen(&name, &streckenabschnitt);
                            },
                        }
                        let name_string = name.0.clone();
                        if let Some(ersetzt) =
                            self.gleise.neuer_streckenabschnitt(name, streckenabschnitt)
                        {
                            self.zeige_message_box(
                                "Hinzufügen Streckenabschnitt".to_string(),
                                format!(
                                    "Vorherigen Streckenabschnitt {} ersetzt: {:?}",
                                    name_string, ersetzt
                                ),
                            )
                        }
                    },
                    Err(error) => self.zeige_message_box(
                        "Hinzufügen Streckenabschnitt".to_string(),
                        format!("Fehler beim Hinzufügen: {:?}", error),
                    ),
                }
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
                match self.modal_state.inner_mut() {
                    Modal::Streckenabschnitt(streckenabschnitt_auswahl) => {
                        streckenabschnitt_auswahl.entferne(&name);
                    },
                }
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
            Message::SchließeMessageBox => self.message_box.show(false),
            Message::Speichern => {
                if let Err(err) = self.gleise.speichern(&self.aktueller_pfad) {
                    self.zeige_message_box(
                        format!("Fehler beim Speichern in {}", self.aktueller_pfad),
                        format!("{:?}", err),
                    )
                }
            },
            Message::Laden => self.laden(),
            Message::Pfad(pfad) => self.aktueller_pfad = pfad,
        }

        iced::Command::none()
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        let Zugkontrolle {
            anschlüsse: _,
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            modal_state,
            streckenabschnitt_aktuell,
            message_box,
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
            geschwindigkeiten,
        );

        let column: iced::Element<Self::Message> = iced::Column::new()
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
                        .map(Into::into),
                    )
                    .width(iced::Length::Fill)
                    .height(iced::Length::Fill),
                ),
            )
            .into();

        let modal = iced_aw::Modal::new(modal_state, column, |modal| match modal {
            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => iced::Element::from(
                streckenabschnitt::Auswahl::neu(streckenabschnitt_auswahl),
            )
            .map(|message| {
                use streckenabschnitt::AuswahlNachricht::*;
                match message {
                    Schließe => Message::SchließeModal,
                    Wähle(wahl) => Message::WähleStreckenabschnitt(wahl),
                    Hinzufügen(name, farbe, anschluss) => {
                        Message::HinzufügenStreckenabschnitt(name, farbe, anschluss)
                    },
                    Lösche(name) => Message::LöscheStreckenabschnitt(name),
                }
            }),
        })
        .on_esc(Message::SchließeModal);

        iced_aw::Modal::new(message_box, modal, |MessageBox { titel, nachricht, button_state }| {
            iced::Element::from(
                iced_aw::Card::new(
                    iced::Text::new(&*titel),
                    iced::Column::new().push(iced::Text::new(&*nachricht)).push(
                        iced::Button::new(button_state, iced::Text::new("Ok"))
                            .on_press(Message::SchließeMessageBox),
                    ),
                )
                .width(iced::Length::Shrink),
            )
        })
        .on_esc(Message::SchließeMessageBox)
        .into()
    }
}

fn top_row<'t, Z: 'static>(
    aktueller_modus: Modus,
    streckenabschnitt: &'t mut streckenabschnitt::AnzeigeStatus,
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
        .push(iced::Element::from(streckenabschnitt::Anzeige::neu(streckenabschnitt)).map(
            |streckenabschnitt::AnzeigeNachricht::Auswählen| Message::ZeigeAuswahlStreckenabschnitt,
        ))
        .push(iced::Space::new(iced::Length::Fill, iced::Length::Shrink))
        .push(speichern_laden)
        .padding(5)
        .spacing(5)
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
}

fn row_with_scrollable<'t, Z>(
    aktueller_modus: Modus,
    scrollable_state: &'t mut iced::scrollable::State,
    geraden: &'t mut Vec<Button<Gerade<Z>>>,
    kurven: &'t mut Vec<Button<Kurve<Z>>>,
    weichen: &'t mut Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: &'t mut Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: &'t mut Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: &'t mut Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: &'t mut Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: &'t mut geschwindigkeit::Map<Z::Leiter>,
) -> iced::Row<'t, Message<Z>>
where
    Z: 'static + Zugtyp,
    Z::Leiter: Debug,
{
    // TODO Save/Load/Move?/Rotate?
    // Bauen(Streckenabschnitt?/Geschwindigkeit?/Löschen?)
    // Fahren(Streckenabschnitt-Anzeige?
    let mut scrollable = iced::Scrollable::new(scrollable_state);
    let scrollable_style = scrollable::Collection::new(10);
    let scroller_width = scrollable_style.width();
    let mut width = iced::Length::Shrink;
    match aktueller_modus {
        Modus::Bauen => {
            let mut max_width = None;
            macro_rules! add_buttons {
                ($($vec: expr),*) => {
                    max_width = max_width.max(Vec::new().into_iter()
                        $(.chain($vec.iter().map(|button| button.size().x.0.ceil() as u16)))*
                        .max());
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
            if let Some(max) = max_width {
                width = iced::Length::Units(max + scroller_width);
            }
        },
        Modus::Fahren => {
            scrollable = scrollable.push(iced::Text::new("Geschwindigkeiten"));
            for (name, geschwindigkeit) in geschwindigkeiten {
                scrollable = scrollable.push(
                    iced::Row::new()
                        .push(iced::Text::new(&name.0))
                        .push(iced::Text::new(&format!("{:?}", geschwindigkeit))),
                );
            }
            // TODO Geschwindigkeiten?, Wegstrecken?, Pläne?, Separator dazwischen?
        },
    }
    iced::Row::new()
        .push(
            iced::Container::new(
                scrollable
                    .scroller_width(scroller_width)
                    .width(iced::Length::Shrink)
                    .height(iced::Length::Fill)
                    .style(scrollable_style),
            )
            .width(width)
            .height(iced::Length::Fill),
        )
        .push(iced::Rule::vertical(1).style(rule::SEPARATOR))
}
