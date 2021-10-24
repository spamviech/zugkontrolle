//! iced::Application für die Gleis-Anzeige

use std::{
    convert::identity,
    fmt::Debug,
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    time::Instant,
};

use serde::{Deserialize, Serialize};
use version::version;

use self::{
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    empfänger::Empfänger,
    geschwindigkeit::LeiterAnzeige,
    gleis::{gleise::*, *},
    style::*,
    typen::*,
};
use crate::{
    anschluss::{anschlüsse::Anschlüsse, de_serialisieren::Serialisiere, OutputSerialisiert},
    args::Args,
    farbe::Farbe,
    steuerung::{self, geschwindigkeit::GeschwindigkeitSerialisiert},
};

pub mod anschluss;
pub mod bewegen;
pub mod drehen;
#[path = "application/empfänger.rs"]
pub mod empfänger;
pub mod farbwahl;
pub mod fonts;
pub mod geschwindigkeit;
pub mod gleis;
pub mod icon;
pub(crate) mod macros;
pub mod modal;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod touch_canvas;
pub mod typen;
pub mod update;
pub mod view;
pub mod weiche;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnyGleisUnit<Z> {
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
        impl<Z> From<$type<Z>> for AnyGleisUnit<Z> {
            fn from(gleis: $type<Z>) -> AnyGleisUnit<Z> {
                AnyGleisUnit::$type(gleis.into())
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
    fn erstelle_radio(self, aktueller_modus: Self) -> iced::Radio<Modus> {
        iced::Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

#[derive(zugkontrolle_derive::Debug)]
pub enum AnschlüsseAnpassen<Z> {
    Weiche(GleisId<Weiche<Z>>, Option<WeicheSerialisiert>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche<Z>>, Option<DreiwegeWeicheSerialisiert>),
    KurvenWeiche(GleisId<KurvenWeiche<Z>>, Option<KurvenWeicheSerialisiert>),
    SKurvenWeiche(GleisId<SKurvenWeiche<Z>>, Option<WeicheSerialisiert>),
    Kreuzung(GleisId<Kreuzung<Z>>, Option<WeicheSerialisiert>),
}

#[derive(zugkontrolle_derive::Debug)]
pub enum ZustandZurücksetzen<Z: Zugtyp> {
    Weiche(GleisId<Weiche<Z>>, gleis::weiche::gerade::Richtung, gleis::weiche::gerade::Richtung),
    DreiwegeWeiche(
        GleisId<DreiwegeWeiche<Z>>,
        gleis::weiche::dreiwege::Richtung,
        gleis::weiche::dreiwege::Richtung,
    ),
    KurvenWeiche(
        GleisId<KurvenWeiche<Z>>,
        gleis::weiche::kurve::Richtung,
        gleis::weiche::kurve::Richtung,
    ),
    SKurvenWeiche(
        GleisId<SKurvenWeiche<Z>>,
        gleis::weiche::s_kurve::Richtung,
        gleis::weiche::s_kurve::Richtung,
    ),
    Kreuzung(GleisId<Kreuzung<Z>>, gleis::kreuzung::Richtung, gleis::kreuzung::Richtung),
    GeschwindigkeitAnzeige(
        geschwindigkeit::Name,
        <Z::Leiter as LeiterAnzeige>::ZustandZurücksetzen,
    ),
}

/// Klonbare Nachricht, für Verwendung z.B. mit Button.
#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
enum NachrichtClone<Z: Zugtyp> {
    Gleis {
        gleis: AnyGleisUnit<Z>,
        grab_height: Skalar,
    },
    Skalieren(Skalar),
    SchließeMessageBox,
    GeschwindigkeitAnzeige {
        name: geschwindigkeit::Name,
        nachricht: <Z::Leiter as LeiterAnzeige>::Nachricht,
    },
    ZeigeAuswahlGeschwindigkeit,
}

impl<Z: Zugtyp> From<NachrichtClone<Z>> for Nachricht<Z> {
    fn from(nachricht_clone: NachrichtClone<Z>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { gleis, grab_height } => Nachricht::Gleis { gleis, grab_height },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::SchließeMessageBox => Nachricht::SchließeMessageBox,
            NachrichtClone::GeschwindigkeitAnzeige { name, nachricht } => {
                Nachricht::GeschwindigkeitAnzeige { name, nachricht }
            }
            NachrichtClone::ZeigeAuswahlGeschwindigkeit => Nachricht::ZeigeAuswahlGeschwindigkeit,
        }
    }
}

pub enum Nachricht<Z: Zugtyp> {
    Gleis {
        gleis: AnyGleisUnit<Z>,
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
    WähleStreckenabschnitt(Option<(StreckenabschnittId, Farbe)>),
    HinzufügenStreckenabschnitt(
        Option<geschwindigkeit::Name>,
        streckenabschnitt::Name,
        Farbe,
        OutputSerialisiert,
    ),
    LöscheStreckenabschnitt(StreckenabschnittId),
    SetzeStreckenabschnitt(AnyId<Z>),
    StreckenabschnittFestlegen(bool),
    Speichern(String),
    EntferneSpeichernFarbe(Instant),
    Laden(String),
    GeschwindigkeitAnzeige {
        name: geschwindigkeit::Name,
        nachricht: <Z::Leiter as LeiterAnzeige>::Nachricht,
    },
    ZeigeAuswahlGeschwindigkeit,
    HinzufügenGeschwindigkeit(geschwindigkeit::Name, GeschwindigkeitSerialisiert<Z::Leiter>),
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    ZeigeAnschlüsseAnpassen(AnyId<Z>),
    AnschlüsseAnpassen(AnschlüsseAnpassen<Z>),
    FahrenAktion(AnyId<Z>),
    AsyncFehler {
        titel: String,
        nachricht: String,
        zustand_zurücksetzen: ZustandZurücksetzen<Z>,
    },
}

impl<Z> Debug for Nachricht<Z>
where
    Z: Zugtyp,
    <Z::Leiter as Serialisiere>::Serialisiert: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Gleis { gleis, grab_height } => f
                .debug_struct("Gleis")
                .field("gleis", gleis)
                .field("grab_height", grab_height)
                .finish(),
            Self::Modus(arg0) => f.debug_tuple("Modus").field(arg0).finish(),
            Self::Bewegen(arg0) => f.debug_tuple("Bewegen").field(arg0).finish(),
            Self::BewegungAusführen => write!(f, "BewegungAusführen"),
            Self::Position(arg0) => f.debug_tuple("Position").field(arg0).finish(),
            Self::Winkel(arg0) => f.debug_tuple("Winkel").field(arg0).finish(),
            Self::Skalieren(arg0) => f.debug_tuple("Skalieren").field(arg0).finish(),
            Self::SchließeModal => write!(f, "SchließeModal"),
            Self::SchließeMessageBox => write!(f, "SchließeMessageBox"),
            Self::ZeigeAuswahlStreckenabschnitt => write!(f, "ZeigeAuswahlStreckenabschnitt"),
            Self::WähleStreckenabschnitt(arg0) => {
                f.debug_tuple("WähleStreckenabschnitt").field(arg0).finish()
            }
            Self::HinzufügenStreckenabschnitt(arg0, arg1, arg2, arg3) => f
                .debug_tuple("HinzufügenStreckenabschnitt")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .field(arg3)
                .finish(),
            Self::LöscheStreckenabschnitt(arg0) => {
                f.debug_tuple("LöscheStreckenabschnitt").field(arg0).finish()
            }
            Self::SetzeStreckenabschnitt(arg0) => {
                f.debug_tuple("SetzeStreckenabschnitt").field(arg0).finish()
            }
            Self::StreckenabschnittFestlegen(arg0) => {
                f.debug_tuple("StreckenabschnittFestlegen").field(arg0).finish()
            }
            Self::Speichern(arg0) => f.debug_tuple("Speichern").field(arg0).finish(),
            Self::EntferneSpeichernFarbe(arg0) => {
                f.debug_tuple("EntferneSpeichernFarbe").field(arg0).finish()
            }
            Self::Laden(arg0) => f.debug_tuple("Laden").field(arg0).finish(),
            Self::GeschwindigkeitAnzeige { name, nachricht } => f
                .debug_struct("GeschwindigkeitAnzeige")
                .field("name", name)
                .field("nachricht", nachricht)
                .finish(),
            Self::ZeigeAuswahlGeschwindigkeit => write!(f, "ZeigeAuswahlGeschwindigkeit"),
            Self::HinzufügenGeschwindigkeit(arg0, arg1) => {
                f.debug_tuple("HinzufügenGeschwindigkeit").field(arg0).field(arg1).finish()
            }
            Self::LöscheGeschwindigkeit(arg0) => {
                f.debug_tuple("LöscheGeschwindigkeit").field(arg0).finish()
            }
            Self::ZeigeAnschlüsseAnpassen(arg0) => {
                f.debug_tuple("ZeigeAnschlüsseAnpassen").field(arg0).finish()
            }
            Self::AnschlüsseAnpassen(arg0) => {
                f.debug_tuple("AnschlüsseAnpassen").field(arg0).finish()
            }
            Self::FahrenAktion(arg0) => f.debug_tuple("FahrenAktion").field(arg0).finish(),
            Self::AsyncFehler { titel, nachricht, zustand_zurücksetzen } => f
                .debug_struct("AsyncFehler")
                .field("titel", titel)
                .field("nachricht", nachricht)
                .field("zustand_zurücksetzen", zustand_zurücksetzen)
                .finish(),
        }
    }
}

impl<Z: Zugtyp> From<gleise::Nachricht<Z>> for Nachricht<Z> {
    fn from(message: gleise::Nachricht<Z>) -> Self {
        match message {
            gleise::Nachricht::SetzeStreckenabschnitt(any_id) => {
                Nachricht::SetzeStreckenabschnitt(any_id)
            }
            gleise::Nachricht::AnschlüsseAnpassen(any_id) => {
                Nachricht::ZeigeAnschlüsseAnpassen(any_id)
            }
            gleise::Nachricht::FahrenAktion(any_id) => Nachricht::FahrenAktion(any_id),
        }
    }
}

impl<T, Z> ButtonNachricht<NachrichtClone<Z>> for T
where
    T: Clone + Into<AnyGleisUnit<Z>>,
    Z: Zugtyp,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<Z> {
        NachrichtClone::Gleis { gleis: self.clone().into(), grab_height: klick_position.y }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<Z> Nachricht<Z>
where
    Z: Zugtyp + 'static,
    <Z::Leiter as Serialisiere>::Serialisiert: Debug + Send,
{
    fn als_command(self) -> iced::Command<Nachricht<Z>> {
        iced::Command::perform(async_identity(self), identity)
    }
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type WeicheStatus = weiche::Status<
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
    gleis::weiche::gerade::RichtungAnschlüsseAuswahlStatus,
>;
type WeicheSerialisiert = steuerung::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type DreiwegeWeicheStatus = weiche::Status<
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
    gleis::weiche::dreiwege::RichtungAnschlüsseAuswahlStatus,
>;
type DreiwegeWeicheSerialisiert = steuerung::WeicheSerialisiert<
    gleis::weiche::dreiwege::Richtung,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type KurvenWeicheStatus = weiche::Status<
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
    gleis::weiche::kurve::RichtungAnschlüsseAuswahlStatus,
>;
type KurvenWeicheSerialisiert = steuerung::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;
type ErstelleAnschlussNachricht<T, Z> = Arc<dyn Fn(Option<T>) -> Nachricht<Z>>;

pub enum AuswahlStatus<Z: Zugtyp> {
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
    Geschwindigkeit(geschwindigkeit::AuswahlStatus),
    Weiche(WeicheStatus, ErstelleAnschlussNachricht<WeicheSerialisiert, Z>),
    DreiwegeWeiche(DreiwegeWeicheStatus, ErstelleAnschlussNachricht<DreiwegeWeicheSerialisiert, Z>),
    KurvenWeiche(KurvenWeicheStatus, ErstelleAnschlussNachricht<KurvenWeicheSerialisiert, Z>),
}

impl<Z: Zugtyp> Debug for AuswahlStatus<Z> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Streckenabschnitt(arg0) => {
                f.debug_tuple("Streckenabschnitt").field(arg0).finish()
            }
            Self::Geschwindigkeit(arg0) => f.debug_tuple("Geschwindigkeit").field(arg0).finish(),
            Self::Weiche(arg0, _arg1) => {
                f.debug_tuple("Weiche").field(arg0).field(&"<function>".to_string()).finish()
            }
            Self::DreiwegeWeiche(arg0, _arg1) => f
                .debug_tuple("DreiwegeWeiche")
                .field(arg0)
                .field(&"<function>".to_string())
                .finish(),
            Self::KurvenWeiche(arg0, _arg1) => {
                f.debug_tuple("KurvenWeiche").field(arg0).field(&"<function>".to_string()).finish()
            }
        }
    }
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
    geraden: Vec<Button<GeradeUnit<Z>>>,
    kurven: Vec<Button<KurveUnit<Z>>>,
    weichen: Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
    modal_status: modal::Status<AuswahlStatus<Z>>,
    streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus,
    streckenabschnitt_aktuell_festlegen: bool,
    geschwindigkeit_button_state: iced::button::State,
    message_box: modal::Status<MessageBox>,
    bewegen: Bewegen,
    drehen: Drehen,
    zoom: iced::slider::State,
    speichern_laden: speichern_laden::Status,
    speichern_gefärbt: Option<Instant>,
    bewegung: Option<Bewegung>,
    sender: Sender<Nachricht<Z>>,
    empfänger: Empfänger<Nachricht<Z>>,
    // TODO Plan
}

impl<Z> Debug for Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Zugkontrolle")
            .field("anschlüsse", &self.anschlüsse)
            .field("gleise", &self.gleise)
            .field("scrollable_state", &self.scrollable_state)
            .field("geraden", &self.geraden)
            .field("kurven", &self.kurven)
            .field("weichen", &self.weichen)
            .field("dreiwege_weichen", &self.dreiwege_weichen)
            .field("kurven_weichen", &self.kurven_weichen)
            .field("s_kurven_weichen", &self.s_kurven_weichen)
            .field("kreuzungen", &self.kreuzungen)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .field("modal_state", &self.modal_status)
            .field("streckenabschnitt_aktuell", &self.streckenabschnitt_aktuell)
            .field("streckenabschnitt_aktuell_festlegen", &self.streckenabschnitt_aktuell_festlegen)
            .field("geschwindigkeit_button_state", &self.geschwindigkeit_button_state)
            .field("message_box", &self.message_box)
            .field("bewegen", &self.bewegen)
            .field("drehen", &self.drehen)
            .field("zoom", &self.zoom)
            .field("speichern_laden", &self.speichern_laden)
            .field("speichern_gefärbt", &self.speichern_gefärbt)
            .field("bewegung", &self.bewegung)
            .field("sender", &self.sender)
            .field("empfänger", &self.empfänger)
            .finish()
    }
}

impl<Z> iced::Application for Zugkontrolle<Z>
where
    Z: Zugtyp + Serialize + for<'de> Deserialize<'de> + 'static,
    <Z::Leiter as Serialisiere>::Serialisiert: Debug + Clone + Unpin + Send,
{
    type Executor = iced::executor::Default;
    type Flags = (Anschlüsse, Args);
    type Message = Nachricht<Z>;

    fn new((anschlüsse, args): Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let Args { pfad, modus, zoom, x, y, winkel, .. } = args;
        let mut messages = Vec::new();
        if let Some(modus) = modus {
            messages.push(Nachricht::Modus(modus));
        }
        let gleise = Gleise::neu();
        let aktueller_pfad: String;
        if let Some(pfad) = pfad {
            messages.push(Nachricht::Laden(pfad.clone()));
            aktueller_pfad = pfad;
        } else {
            aktueller_pfad = format!("{}.zug", Z::NAME);
        };
        if let Some(zoom) = zoom {
            messages.push(Nachricht::Skalieren(Skalar(zoom)))
        }
        if x.is_some() || y.is_some() {
            messages.push(Nachricht::Position(Vektor {
                x: Skalar(x.unwrap_or(0.)),
                y: Skalar(y.unwrap_or(0.)),
            }))
        }
        if let Some(winkel) = winkel {
            messages.push(Nachricht::Winkel(Winkel(winkel)))
        }
        let (sender, receiver) = channel();
        let zugkontrolle = Zugkontrolle {
            anschlüsse,
            gleise,
            scrollable_state: iced::scrollable::State::new(),
            geraden: Z::geraden().into_iter().map(Button::neu).collect(),
            kurven: Z::kurven().into_iter().map(Button::neu).collect(),
            weichen: Z::weichen().into_iter().map(Button::neu).collect(),
            dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::neu).collect(),
            kurven_weichen: Z::kurven_weichen().into_iter().map(Button::neu).collect(),
            s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::neu).collect(),
            kreuzungen: Z::kreuzungen().into_iter().map(Button::neu).collect(),
            geschwindigkeiten: geschwindigkeit::Map::new(),
            modal_status: modal::Status::neu(),
            streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus::neu(),
            streckenabschnitt_aktuell_festlegen: false,
            geschwindigkeit_button_state: iced::button::State::new(),
            message_box: modal::Status::neu(),
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            zoom: iced::slider::State::new(),
            speichern_laden: speichern_laden::Status::neu(aktueller_pfad),
            speichern_gefärbt: None,
            bewegung: None,
            sender,
            empfänger: Empfänger::neu(receiver),
        };
        let command = if messages.is_empty() {
            iced::Command::none()
        } else {
            iced::Command::batch(messages.into_iter().map(Nachricht::als_command))
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
            Nachricht::Gleis { gleis, grab_height } => self.gleis_hinzufügen(gleis, grab_height),
            Nachricht::Modus(modus) => self.gleise.moduswechsel(modus),
            Nachricht::Bewegen(bewegen::Nachricht::StarteBewegung(bewegung)) => {
                command = self.bewegung_starten(bewegung)
            }
            Nachricht::Bewegen(bewegen::Nachricht::BeendeBewegung) => self.bewegung_beenden(),
            Nachricht::Bewegen(bewegen::Nachricht::Zurücksetzen) => self.bewegung_zurücksetzen(),
            Nachricht::BewegungAusführen => {
                if let Some(cmd) = self.bewegung_ausführen() {
                    command = cmd
                }
            }
            Nachricht::Position(position) => self.gleise.setze_pivot(position),
            Nachricht::Winkel(winkel) => self.gleise.winkel(winkel),
            Nachricht::Skalieren(skalieren) => self.gleise.setze_skalierfaktor(skalieren),
            Nachricht::SchließeModal => self.schließe_modal(),
            Nachricht::ZeigeAuswahlStreckenabschnitt => self.zeige_auswahl_streckenabschnitt(),
            Nachricht::WähleStreckenabschnitt(aktuell) => self.streckenabschnitt_wählen(aktuell),
            Nachricht::HinzufügenStreckenabschnitt(
                geschwindigkeit,
                name,
                farbe,
                anschluss_definition,
            ) => self.streckenabschnitt_hinzufügen(
                geschwindigkeit.as_ref(),
                name,
                farbe,
                anschluss_definition,
            ),
            Nachricht::LöscheStreckenabschnitt(streckenabschnitt_id) => {
                self.streckenabschnitt_löschen(streckenabschnitt_id)
            }
            Nachricht::SetzeStreckenabschnitt(any_id) => {
                self.gleis_setzte_streckenabschnitt(any_id)
            }
            Nachricht::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_festlegen(festlegen)
            }
            Nachricht::SchließeMessageBox => self.schließe_message_box(),
            Nachricht::Speichern(pfad) => {
                if let Some(cmd) = self.speichern(pfad) {
                    command = cmd
                }
            }
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit)
            }
            Nachricht::Laden(pfad) => self.laden(pfad),
            Nachricht::GeschwindigkeitAnzeige { name, nachricht } => {
                if let Some(cmd) = self.geschwindigkeit_anzeige_nachricht(name, nachricht) {
                    command = cmd
                }
            }
            Nachricht::ZeigeAuswahlGeschwindigkeit => self.zeige_auswahl_geschwindigkeit(),
            Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save)
            }
            Nachricht::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(name),
            Nachricht::ZeigeAnschlüsseAnpassen(any_id) => self.zeige_anschlüsse_anpassen(any_id),
            Nachricht::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                if let Some(message) = self.anschlüsse_anpassen(anschlüsse_anpassen) {
                    command = message.als_command()
                }
            }
            Nachricht::FahrenAktion(any_id) => self.fahren_aktion(any_id),
            Nachricht::AsyncFehler { titel, nachricht, zustand_zurücksetzen } => {
                if let Some(cmd) = self.async_fehler(titel, nachricht, zustand_zurücksetzen) {
                    command = cmd
                }
            }
        }

        command
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        iced::Subscription::from_recipe(self.empfänger.clone())
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        self.view()
    }
}
