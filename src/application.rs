//! iced::Application für die Gleis-Anzeige

use std::{
    collections::HashMap,
    convert::identity,
    fmt::Debug,
    sync::Arc,
    time::{Duration, Instant},
};

use serde::{Deserialize, Serialize};
use version::version;

use self::{
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    geschwindigkeit::LeiterAnzeige,
    gleis::{gleise::*, *},
    sleep::Sleep,
    style::*,
    typen::*,
};
use crate::{
    anschluss::{anschlüsse::Anschlüsse, speichern_laden::ToSave, OutputSave},
    args::Args,
    farbe::Farbe,
    steuerung::{
        self,
        geschwindigkeit::{Geschwindigkeit, Leiter},
    },
};

pub mod anschluss;
pub mod bewegen;
pub mod drehen;
pub mod farbwahl;
pub mod geschwindigkeit;
pub mod gleis;
pub mod icon;
pub(crate) mod macros;
pub mod sleep;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod touch_canvas;
pub mod typen;
pub mod update;
pub mod view;
pub mod weiche;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnyGleis<Z> {
    GeradeUnit(GeradeUnit<Z>),
    KurveUnit(KurveUnit<Z>),
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
impl_any_gleis_from! {GeradeUnit}
impl_any_gleis_from! {KurveUnit}
impl_any_gleis_from! {WeicheUnit}
impl_any_gleis_from! {DreiwegeWeicheUnit}
impl_any_gleis_from! {KurvenWeicheUnit}
impl_any_gleis_from! {SKurvenWeicheUnit}
impl_any_gleis_from! {KreuzungUnit}

impl Modus {
    fn make_radio(self, aktueller_modus: Self) -> iced::Radio<Modus> {
        iced::Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnschlüsseAnpassen<Z> {
    Weiche(
        GleisId<Weiche<Z>>,
        Option<
            steuerung::Weiche<
                gleis::weiche::gerade::Richtung,
                gleis::weiche::gerade::RichtungAnschlüsseSave,
            >,
        >,
    ),
    DreiwegeWeiche(
        GleisId<DreiwegeWeiche<Z>>,
        Option<
            steuerung::Weiche<
                gleis::weiche::dreiwege::Richtung,
                gleis::weiche::dreiwege::RichtungAnschlüsseSave,
            >,
        >,
    ),
    KurvenWeiche(
        GleisId<KurvenWeiche<Z>>,
        Option<
            steuerung::Weiche<
                gleis::weiche::kurve::Richtung,
                gleis::weiche::kurve::RichtungAnschlüsseSave,
            >,
        >,
    ),
    SKurvenWeiche(
        GleisId<SKurvenWeiche<Z>>,
        Option<
            steuerung::Weiche<
                gleis::weiche::gerade::Richtung,
                gleis::weiche::gerade::RichtungAnschlüsseSave,
            >,
        >,
    ),
    Kreuzung(
        GleisId<Kreuzung<Z>>,
        Option<
            steuerung::Weiche<
                gleis::weiche::gerade::Richtung,
                gleis::weiche::gerade::RichtungAnschlüsseSave,
            >,
        >,
    ),
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    Gleis {
        gleis: AnyGleis<Z>,
        grab_height: Skalar,
    },
    Modus(Modus),
    Bewegen(bewegen::Nachricht),
    BewegungAusführen,
    Position(Vektor),
    Winkel(Winkel),
    Skalieren(Skalar),
    SchließeModal,
    SchließeMessageBox,
    ZeigeAuswahlStreckenabschnitt,
    WähleStreckenabschnitt(Option<(streckenabschnitt::Name, Farbe)>),
    HinzufügenStreckenabschnitt(streckenabschnitt::Name, Farbe, OutputSave),
    LöscheStreckenabschnitt(streckenabschnitt::Name),
    SetzeStreckenabschnitt(AnyId<Z>),
    StreckenabschnittFestlegen(bool),
    Speichern(String),
    EntferneSpeichernFarbe(Instant),
    Laden(String),
    GeschwindigkeitAuswahl {
        name: geschwindigkeit::Name,
        nachricht: <Z::Leiter as LeiterAnzeige>::Message,
    },
    ZeigeAuswahlGeschwindigkeit,
    HinzufügenGeschwindigkeit(
        geschwindigkeit::Name,
        Geschwindigkeit<<<Z as Zugtyp>::Leiter as ToSave>::Save>,
    ),
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    ZeigeAnschlüsseAnpassen(AnyId<Z>),
    AnschlüsseAnpassen(AnschlüsseAnpassen<Z>),
    FahrenAktion(AnyId<Z>),
}

impl<Z> From<gleise::Message<Z>> for Message<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    fn from(message: gleise::Message<Z>) -> Self {
        match message {
            gleise::Message::SetzeStreckenabschnitt(any_id) => {
                Message::SetzeStreckenabschnitt(any_id)
            }
            gleise::Message::AnschlüsseAnpassen(any_id) => {
                Message::ZeigeAnschlüsseAnpassen(any_id)
            }
            gleise::Message::FahrenAktion(any_id) => Message::FahrenAktion(any_id),
        }
    }
}

impl<T, Z> ButtonMessage<Message<Z>> for T
where
    T: Clone + Into<AnyGleis<Z>>,
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    fn to_message(&self, grab_location: Vektor) -> Message<Z> {
        Message::Gleis { gleis: self.clone().into(), grab_height: grab_location.y }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<Z> Message<Z>
where
    Z: 'static + Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone + Send,
{
    fn as_command(self) -> iced::Command<Message<Z>> {
        iced::Command::perform(async_identity(self), identity)
    }
}

pub enum Modal<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
    Geschwindigkeit(geschwindigkeit::AuswahlStatus),
    Weiche(
        weiche::Status<
            gleis::weiche::gerade::RichtungAnschlüsseSave,
            gleis::weiche::gerade::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                Option<
                    steuerung::Weiche<
                        gleis::weiche::gerade::Richtung,
                        gleis::weiche::gerade::RichtungAnschlüsseSave,
                    >,
                >,
            ) -> Message<Z>,
        >,
    ),
    DreiwegeWeiche(
        weiche::Status<
            gleis::weiche::dreiwege::RichtungAnschlüsseSave,
            gleis::weiche::dreiwege::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                Option<
                    steuerung::Weiche<
                        gleis::weiche::dreiwege::Richtung,
                        gleis::weiche::dreiwege::RichtungAnschlüsseSave,
                    >,
                >,
            ) -> Message<Z>,
        >,
    ),
    KurvenWeiche(
        weiche::Status<
            gleis::weiche::kurve::RichtungAnschlüsseSave,
            gleis::weiche::kurve::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                Option<
                    steuerung::Weiche<
                        gleis::weiche::kurve::Richtung,
                        gleis::weiche::kurve::RichtungAnschlüsseSave,
                    >,
                >,
            ) -> Message<Z>,
        >,
    ),
}

#[derive(Debug)]
struct MessageBox {
    titel: String,
    nachricht: String,
    button_state: iced::button::State,
}

pub struct Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    anschlüsse: Anschlüsse,
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<GeradeUnit<Z>>>,
    kurven: Vec<Button<KurveUnit<Z>>>,
    weichen: Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
    modal_state: iced_aw::modal::State<Modal<Z>>,
    streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus,
    streckenabschnitt_aktuell_festlegen: bool,
    geschwindigkeit_button_state: iced::button::State,
    message_box: iced_aw::modal::State<MessageBox>,
    bewegen: Bewegen,
    drehen: Drehen,
    zoom: iced::slider::State,
    speichern_laden: speichern_laden::Status,
    speichern_gefärbt: Option<Instant>,
    bewegung: Option<(Instant, Bewegung)>,
    // TODO Wegstrecke, Plan
}

impl<Z> iced::Application for Zugkontrolle<Z>
where
    Z: 'static + Zugtyp + Debug + PartialEq + Serialize + for<'de> Deserialize<'de> + Send + Sync,
    Z::Leiter: Debug,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone + Send,
    <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Fahrtrichtung: Debug,
    <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Message: Unpin,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Unpin,
    Geschwindigkeit<<Z as Zugtyp>::Leiter>: Leiter,
{
    type Executor = iced::executor::Default;
    type Flags = (Anschlüsse, Args);
    type Message = Message<Z>;

    fn new((anschlüsse, args): Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let Args { pfad, modus, zoom, x, y, winkel, .. } = args;
        let mut messages = Vec::new();
        if let Some(modus) = modus {
            messages.push(Message::Modus(modus));
        }
        let gleise = Gleise::neu();
        let auswahl_status = streckenabschnitt::AuswahlStatus::neu(gleise.streckenabschnitte());
        let aktueller_pfad: String;
        if let Some(pfad) = pfad {
            messages.push(Message::Laden(pfad.clone()));
            aktueller_pfad = pfad;
        } else {
            aktueller_pfad = format!("{}.zug", Z::NAME);
        };
        if let Some(zoom) = zoom {
            messages.push(Message::Skalieren(Skalar(zoom)))
        }
        if x.is_some() || y.is_some() {
            messages.push(Message::Position(Vektor {
                x: Skalar(x.unwrap_or(0.)),
                y: Skalar(y.unwrap_or(0.)),
            }))
        }
        if let Some(winkel) = winkel {
            messages.push(Message::Winkel(Winkel(winkel)))
        }
        let zugkontrolle = Zugkontrolle {
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
            geschwindigkeiten: HashMap::new(),
            modal_state: iced_aw::modal::State::new(Modal::Streckenabschnitt(auswahl_status)),
            streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus::neu(),
            streckenabschnitt_aktuell_festlegen: false,
            geschwindigkeit_button_state: iced::button::State::new(),
            message_box: iced_aw::modal::State::new(MessageBox {
                titel: "Nicht initialisiert".to_string(),
                nachricht: "Diese Nachricht sollte nicht sichtbar sein!".to_string(),
                button_state: iced::button::State::new(),
            }),
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            zoom: iced::slider::State::new(),
            speichern_laden: speichern_laden::Status::neu(aktueller_pfad),
            speichern_gefärbt: None,
            bewegung: None,
        };
        let command = if messages.is_empty() {
            iced::Command::none()
        } else {
            iced::Command::batch(messages.into_iter().map(Message::as_command))
        };
        (zugkontrolle, command)
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", version!())
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut iced::Clipboard,
    ) -> iced::Command<Self::Message> {
        let mut command = iced::Command::none();

        match message {
            Message::Gleis { gleis, grab_height } => self.gleis_hinzufügen(gleis, grab_height),
            Message::Modus(modus) => self.gleise.moduswechsel(modus),
            Message::Bewegen(bewegen::Nachricht::StarteBewegung(bewegung)) => {
                self.bewegung_starten(bewegung)
            }
            Message::Bewegen(bewegen::Nachricht::BeendeBewegung) => self.bewegung_beenden(),
            Message::Bewegen(bewegen::Nachricht::Zurücksetzen) => self.bewegung_zurücksetzen(),
            Message::BewegungAusführen => self.bewegung_ausführen(),
            Message::Position(position) => self.gleise.setze_pivot(position),
            Message::Winkel(winkel) => self.gleise.winkel(winkel),
            Message::Skalieren(skalieren) => self.gleise.setze_skalierfaktor(skalieren),
            Message::SchließeModal => self.schließe_modal(),
            Message::ZeigeAuswahlStreckenabschnitt => self.zeige_auswahl_streckenabschnitt(),
            Message::WähleStreckenabschnitt(aktuell) => self.streckenabschnitt_wählen(aktuell),
            Message::HinzufügenStreckenabschnitt(name, farbe, anschluss_definition) => {
                self.streckenabschnitt_hinzufügen(name, farbe, anschluss_definition)
            }
            Message::LöscheStreckenabschnitt(name) => self.streckenabschnitt_löschen(name),
            Message::SetzeStreckenabschnitt(any_id) => self.gleis_setzte_streckenabschnitt(any_id),
            Message::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_festlegen(festlegen)
            }
            Message::SchließeMessageBox => self.schließe_message_box(),
            Message::Speichern(pfad) => self.speichern(pfad),
            Message::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit)
            }
            Message::Laden(pfad) => self.laden(pfad),
            Message::GeschwindigkeitAuswahl { name, nachricht } => {
                if let Some(cmd) = self.geschwindigkeit_auswahl_nachricht(name, nachricht) {
                    command = cmd
                }
            }
            Message::ZeigeAuswahlGeschwindigkeit => self.zeige_auswahl_geschwindigkeit(),
            Message::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save)
            }
            Message::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(name),
            Message::ZeigeAnschlüsseAnpassen(any_id) => self.zeige_anschlüsse_anpassen(any_id),
            Message::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                if let Some(message) = self.anschlüsse_anpassen(anschlüsse_anpassen) {
                    command = message.as_command()
                }
            }
            Message::FahrenAktion(any_id) => self.fahren_aktion(any_id),
        }

        command
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        let mut subscriptions = Vec::new();
        if let Some(speicher_zeit) = self.speichern_gefärbt {
            subscriptions.push(iced::Subscription::from_recipe(Sleep::neu(
                speicher_zeit,
                Duration::from_secs(2),
                Message::EntferneSpeichernFarbe(speicher_zeit),
            )))
        }
        if let Some((instant, _bewegung)) = self.bewegung {
            subscriptions.push(iced::Subscription::from_recipe(Sleep::neu(
                instant,
                Duration::from_millis(20),
                Message::BewegungAusführen,
            )))
        }
        if subscriptions.is_empty() {
            iced::Subscription::none()
        } else {
            iced::Subscription::batch(subscriptions)
        }
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        self.view()
    }
}
