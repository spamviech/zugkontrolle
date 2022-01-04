//! iced::Application für die Gleis-Anzeige

use std::{
    convert::identity,
    fmt::{Debug, Display},
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    time::Instant,
};

use flexi_logger::{Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger};
use version::version;

use self::{
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    empfänger::Empfänger,
    geschwindigkeit::LeiterAnzeige,
    gleis::{
        gleise::{daten::de_serialisieren::BekannterLeiter, *},
        *,
    },
    icon::icon,
    style::*,
    typen::*,
};
use crate::{
    anschluss::{de_serialisieren::Serialisiere, OutputSerialisiert},
    args::{self, Args},
    farbe::Farbe,
    steuerung::{
        self,
        geschwindigkeit::{GeschwindigkeitSerialisiert, Mittelleiter, Zweileiter},
    },
    zugtyp::Zugtyp,
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

#[derive(Debug, Clone)]
pub enum AnyGleisUnit {
    GeradeUnit(GeradeUnit),
    KurveUnit(KurveUnit),
    WeicheUnit(WeicheUnit),
    DreiwegeWeicheUnit(DreiwegeWeicheUnit),
    KurvenWeicheUnit(KurvenWeicheUnit),
    SKurvenWeicheUnit(SKurvenWeicheUnit),
    KreuzungUnit(KreuzungUnit),
}
macro_rules! impl_any_gleis_from {
    ($type:ident) => {
        impl From<$type> for AnyGleisUnit {
            fn from(gleis: $type) -> AnyGleisUnit {
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

#[derive(Debug)]
pub enum AnschlüsseAnpassen {
    Weiche(GleisId<Weiche>, Option<WeicheSerialisiert>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<DreiwegeWeicheSerialisiert>),
    KurvenWeiche(GleisId<KurvenWeiche>, Option<KurvenWeicheSerialisiert>),
    SKurvenWeiche(GleisId<SKurvenWeiche>, Option<WeicheSerialisiert>),
    Kreuzung(GleisId<Kreuzung>, Option<WeicheSerialisiert>),
}

#[derive(zugkontrolle_derive::Debug)]
pub enum ZustandZurücksetzen<Leiter: LeiterAnzeige> {
    Weiche(GleisId<Weiche>, gleis::weiche::gerade::Richtung, gleis::weiche::gerade::Richtung),
    DreiwegeWeiche(
        GleisId<DreiwegeWeiche>,
        gleis::weiche::dreiwege::Richtung,
        gleis::weiche::dreiwege::Richtung,
    ),
    KurvenWeiche(
        GleisId<KurvenWeiche>,
        gleis::weiche::kurve::Richtung,
        gleis::weiche::kurve::Richtung,
    ),
    SKurvenWeiche(
        GleisId<SKurvenWeiche>,
        gleis::weiche::s_kurve::Richtung,
        gleis::weiche::s_kurve::Richtung,
    ),
    Kreuzung(GleisId<Kreuzung>, gleis::kreuzung::Richtung, gleis::kreuzung::Richtung),
    GeschwindigkeitAnzeige(geschwindigkeit::Name, <Leiter as LeiterAnzeige>::ZustandZurücksetzen),
}

/// Klonbare Nachricht, für Verwendung z.B. mit Button.
#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
enum NachrichtClone<Leiter: LeiterAnzeige> {
    Gleis {
        gleis: AnyGleisUnit,
        grab_height: Skalar,
    },
    Skalieren(Skalar),
    SchließeMessageBox,
    GeschwindigkeitAnzeige {
        name: geschwindigkeit::Name,
        nachricht: <Leiter as LeiterAnzeige>::Nachricht,
    },
    ZeigeAuswahlGeschwindigkeit,
}

impl<Leiter: LeiterAnzeige> From<NachrichtClone<Leiter>> for Nachricht<Leiter> {
    fn from(nachricht_clone: NachrichtClone<Leiter>) -> Self {
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

#[derive(zugkontrolle_derive::Debug)]
#[zugkontrolle_debug(Leiter: Serialisiere, <Leiter as Serialisiere>::Serialisiert: Debug)]
pub enum Nachricht<Leiter: LeiterAnzeige> {
    Gleis {
        gleis: AnyGleisUnit,
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
    SetzeStreckenabschnitt(AnyId),
    StreckenabschnittFestlegen(bool),
    Speichern(String),
    EntferneSpeichernFarbe(Instant),
    Laden(String),
    GeschwindigkeitAnzeige {
        name: geschwindigkeit::Name,
        nachricht: <Leiter as LeiterAnzeige>::Nachricht,
    },
    ZeigeAuswahlGeschwindigkeit,
    HinzufügenGeschwindigkeit(geschwindigkeit::Name, GeschwindigkeitSerialisiert<Leiter>),
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    ZeigeAnschlüsseAnpassen(AnyId),
    AnschlüsseAnpassen(AnschlüsseAnpassen),
    FahrenAktion(AnyId),
    AsyncFehler {
        titel: String,
        nachricht: String,
        zustand_zurücksetzen: ZustandZurücksetzen<Leiter>,
    },
}

impl<Leiter: LeiterAnzeige> From<gleise::Nachricht> for Nachricht<Leiter> {
    fn from(message: gleise::Nachricht) -> Self {
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

impl<T, Leiter: LeiterAnzeige> ButtonNachricht<NachrichtClone<Leiter>> for T
where
    T: Clone + Into<AnyGleisUnit>,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<Leiter> {
        NachrichtClone::Gleis { gleis: self.clone().into(), grab_height: klick_position.y }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<Leiter> Nachricht<Leiter>
where
    Leiter: 'static + LeiterAnzeige + Serialisiere,
    Leiter::Serialisiert: Debug + Send,
{
    fn als_command(self) -> iced::Command<Nachricht<Leiter>> {
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
type ErstelleAnschlussNachricht<T, Leiter> = Arc<dyn Fn(Option<T>) -> Nachricht<Leiter>>;

pub enum AuswahlStatus<Leiter: LeiterAnzeige> {
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
    Geschwindigkeit(geschwindigkeit::AuswahlStatus),
    Weiche(WeicheStatus, ErstelleAnschlussNachricht<WeicheSerialisiert, Leiter>),
    DreiwegeWeiche(
        DreiwegeWeicheStatus,
        ErstelleAnschlussNachricht<DreiwegeWeicheSerialisiert, Leiter>,
    ),
    KurvenWeiche(KurvenWeicheStatus, ErstelleAnschlussNachricht<KurvenWeicheSerialisiert, Leiter>),
}

impl<Leiter: LeiterAnzeige> Debug for AuswahlStatus<Leiter> {
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

#[allow(missing_debug_implementations, missing_copy_implementations)]
pub enum App {}

#[derive(Debug)]
pub enum Fehler {
    Iced(iced::Error),
    FlexiLogger(FlexiLoggerError),
    Anschluss(crate::anschluss::InitFehler),
}

impl From<iced::Error> for Fehler {
    fn from(fehler: iced::Error) -> Self {
        Fehler::Iced(fehler)
    }
}
impl From<FlexiLoggerError> for Fehler {
    fn from(fehler: FlexiLoggerError) -> Self {
        Fehler::FlexiLogger(fehler)
    }
}
impl From<crate::anschluss::InitFehler> for Fehler {
    fn from(fehler: crate::anschluss::InitFehler) -> Self {
        Fehler::Anschluss(fehler)
    }
}

impl App {
    pub fn run(args: Args) -> Result<(), Fehler> {
        let Args { i2c_settings, zugtyp, verbose, log_to_file, .. } = args;
        let lager = crate::anschluss::Lager::neu(i2c_settings)?;

        fn erstelle_settings<Leiter>(
            args: Args,
            lager: crate::anschluss::Lager,
            zugtyp: Zugtyp<Leiter>,
        ) -> iced::Settings<(Args, crate::anschluss::Lager, Zugtyp<Leiter>)> {
            iced::Settings {
                window: iced::window::Settings {
                    size: (1024, 768),
                    icon: icon(),
                    ..iced::window::Settings::default()
                },
                default_font: Some(&fonts::REGULAR),
                ..iced::Settings::with_flags((args, lager, zugtyp))
            }
        }

        let log_level = if verbose { log::LevelFilter::Debug } else { log::LevelFilter::Warn };
        let mut log_spec_builder = LogSpecBuilder::new();
        let _ = log_spec_builder.default(log::LevelFilter::Error).module("zugkontrolle", log_level);
        let log_spec = log_spec_builder.finalize();
        let logger_base = Logger::with(log_spec);
        let logger = if log_to_file {
            logger_base
                .log_to_file(FileSpec::default().directory("log"))
                .duplicate_to_stderr(Duplicate::All)
        } else {
            logger_base.log_to_stderr()
        };
        let logger_handle = logger.start()?;

        match zugtyp {
            args::Zugtyp::Märklin => <Zugkontrolle<Mittelleiter> as iced::Application>::run(
                erstelle_settings(args, lager, Zugtyp::märklin()),
            )?,
            args::Zugtyp::Lego => <Zugkontrolle<Zweileiter> as iced::Application>::run(
                erstelle_settings(args, lager, Zugtyp::lego()),
            )?,
        }

        // explizit drop aufrufen, damit logger_handle auf jeden Fall lang genau in scope bleibt.
        drop(logger_handle);

        Ok(())
    }
}

#[derive(Debug)]
pub struct Zugkontrolle<Leiter: LeiterAnzeige> {
    gleise: Gleise<Leiter>,
    lager: crate::anschluss::Lager,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<GeradeUnit>>,
    kurven: Vec<Button<KurveUnit>>,
    weichen: Vec<Button<WeicheUnit>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit>>,
    kreuzungen: Vec<Button<KreuzungUnit>>,
    geschwindigkeiten: geschwindigkeit::Map<Leiter>,
    modal_status: modal::Status<AuswahlStatus<Leiter>>,
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
    sender: Sender<Nachricht<Leiter>>,
    empfänger: Empfänger<Nachricht<Leiter>>,
    // TODO Plan
}

#[allow(single_use_lifetimes)]
impl<Leiter> iced::Application for Zugkontrolle<Leiter>
where
    Leiter: 'static + LeiterAnzeige + BekannterLeiter + Serialisiere + Display,
    Leiter::Serialisiert: Debug + Clone + Unpin + Send,
{
    type Executor = iced::executor::Default;
    type Flags = (Args, crate::anschluss::Lager, Zugtyp<Leiter>);
    type Message = Nachricht<Leiter>;

    fn new((args, lager, zugtyp): Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let Args { pfad, modus, zoom, x, y, winkel, .. } = args;

        let command: iced::Command<Self::Message>;
        let aktueller_pfad: String;
        if let Some(pfad) = pfad {
            command = Nachricht::Laden(pfad.clone()).als_command();
            aktueller_pfad = pfad.clone();
        } else {
            command = iced::Command::none();
            aktueller_pfad = zugtyp.name.clone();
        };

        macro_rules! erstelle_button {
            () => {
                |gleis| Button::neu(gleis.clone(), zugtyp.spurweite)
            };
        }
        let geraden = zugtyp.geraden.iter().map(erstelle_button!()).collect();
        let kurven = zugtyp.kurven.iter().map(erstelle_button!()).collect();
        let weichen = zugtyp.weichen.iter().map(erstelle_button!()).collect();
        let dreiwege_weichen = zugtyp.dreiwege_weichen.iter().map(erstelle_button!()).collect();
        let kurven_weichen = zugtyp.kurven_weichen.iter().map(erstelle_button!()).collect();
        let s_kurven_weichen = zugtyp.s_kurven_weichen.iter().map(erstelle_button!()).collect();
        let kreuzungen = zugtyp.kreuzungen.iter().map(erstelle_button!()).collect();

        let gleise = Gleise::neu(
            zugtyp,
            modus,
            Position { punkt: Vektor { x: Skalar(x), y: Skalar(y) }, winkel: Winkel(winkel) },
            Skalar(zoom),
        );

        let (sender, receiver) = channel();
        let zugkontrolle = Zugkontrolle {
            gleise,
            lager,
            scrollable_state: iced::scrollable::State::new(),
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
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

    fn view(&mut self) -> iced::Element<'_, Self::Message> {
        self.view()
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        iced::Subscription::from_recipe(self.empfänger.clone())
    }
}
