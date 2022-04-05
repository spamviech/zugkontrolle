//! [Application] für die Gleis-Anzeige.

use std::{
    convert::identity,
    fmt::{Debug, Display},
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    time::Instant,
};

use flexi_logger::{Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger, LoggerHandle};
use iced::{Application, Clipboard, Command, Element, Radio, Settings, Subscription};
use kommandozeilen_argumente::crate_version;
use log::LevelFilter;

use self::{
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    empfänger::Empfänger,
    geschwindigkeit::LeiterAnzeige,
    icon::icon,
};
use crate::{
    anschluss::{de_serialisieren::Serialisiere, Lager, OutputSerialisiert},
    argumente::{Argumente, ZugtypArgument},
    gleis::{
        self,
        button::{Button, ButtonNachricht},
        gerade::GeradeUnit,
        gleise::{
            self,
            id::{AnyId, GleisId, StreckenabschnittId},
            Gleise, Modus,
        },
        kreuzung::{Kreuzung, KreuzungUnit},
        kurve::KurveUnit,
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        self,
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert},
    },
    typen::{canvas::Position, farbe::Farbe, skalar::Skalar, vektor::Vektor, winkel::Winkel},
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
pub mod icon;
pub(crate) mod macros;
pub mod modal;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod touch_canvas;
pub mod update;
pub mod view;
pub mod weiche;

/// Ein beliebiges Gleis ohne Anschlüsse.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AnyGleisUnit {
    /// Eine [Gerade](gleis::Gerade).
    GeradeUnit(GeradeUnit),
    /// Eine [Kurve](gleis::Kurve).
    KurveUnit(KurveUnit),
    /// Eine [Weiche].
    WeicheUnit(WeicheUnit),
    /// Eine [DreiwegeWeiche].
    DreiwegeWeicheUnit(DreiwegeWeicheUnit),
    /// Eine [KurvenWeiche].
    KurvenWeicheUnit(KurvenWeicheUnit),
    /// Eine [SKurvenWeiche].
    SKurvenWeicheUnit(SKurvenWeicheUnit),
    /// Eine [Kreuzung].
    KreuzungUnit(KreuzungUnit),
}

impl Modus {
    fn erstelle_radio(self, aktueller_modus: Self) -> Radio<Modus> {
        Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

/// Anschlüsse für ein Gleis anpassen.
#[derive(Debug)]
pub enum AnschlüsseAnpassen {
    /// Anschlüsse einer [Weiche] anpassen.
    Weiche(GleisId<Weiche>, Option<WeicheSerialisiert>),
    /// Anschlüsse einer [DreiwegeWeiche] anpassen.
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<DreiwegeWeicheSerialisiert>),
    /// Anschlüsse einer [KurvenWeiche] anpassen.
    KurvenWeiche(GleisId<KurvenWeiche>, Option<KurvenWeicheSerialisiert>),
    /// Anschlüsse einer [SKurvenWeiche] anpassen.
    SKurvenWeiche(GleisId<SKurvenWeiche>, Option<WeicheSerialisiert>),
    /// Anschlüsse einer [Kreuzung] anpassen.
    Kreuzung(GleisId<Kreuzung>, Option<WeicheSerialisiert>),
}

/// Zustand auf Stand vor einer Aktion zurücksetzen.
#[derive(zugkontrolle_macros::Debug)]
pub enum ZustandZurücksetzen<Leiter: LeiterAnzeige> {
    /// Richtung einer [Weiche] zurücksetzen.
    Weiche(GleisId<Weiche>, gleis::weiche::gerade::Richtung, gleis::weiche::gerade::Richtung),
    /// Richtung einer [DreiwegeWeiche] zurücksetzen.
    DreiwegeWeiche(
        GleisId<DreiwegeWeiche>,
        gleis::weiche::dreiwege::Richtung,
        gleis::weiche::dreiwege::Richtung,
    ),
    /// Richtung einer [KurvenWeiche] zurücksetzen.
    KurvenWeiche(
        GleisId<KurvenWeiche>,
        gleis::weiche::kurve::Richtung,
        gleis::weiche::kurve::Richtung,
    ),
    /// Richtung einer [SKurvenWeiche] zurücksetzen.
    SKurvenWeiche(
        GleisId<SKurvenWeiche>,
        gleis::weiche::s_kurve::Richtung,
        gleis::weiche::s_kurve::Richtung,
    ),
    /// Richtung einer [Kreuzung] zurücksetzen.
    Kreuzung(GleisId<Kreuzung>, gleis::kreuzung::Richtung, gleis::kreuzung::Richtung),
    /// Einstellung einer [Geschwindigkeit](steuerung::Geschwindigkeit) zurücksetzen.
    GeschwindigkeitAnzeige(geschwindigkeit::Name, <Leiter as LeiterAnzeige>::ZustandZurücksetzen),
}

/// Klonbare Nachricht, für Verwendung z.B. mit Button.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
enum NachrichtClone<Leiter: LeiterAnzeige> {
    Gleis {
        gleis: AnyGleisUnit,
        klick_höhe: Skalar,
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
            NachrichtClone::Gleis { gleis, klick_höhe } => Nachricht::Gleis { gleis, klick_höhe },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::SchließeMessageBox => Nachricht::SchließeMessageBox,
            NachrichtClone::GeschwindigkeitAnzeige { name, nachricht } => {
                Nachricht::GeschwindigkeitAnzeige { name, nachricht }
            },
            NachrichtClone::ZeigeAuswahlGeschwindigkeit => Nachricht::ZeigeAuswahlGeschwindigkeit,
        }
    }
}

/// Eine Nachricht, die beim [Ausführen](ausführen) der Anwendung auftreten kann.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(Leiter: Serialisiere, <Leiter as Serialisiere>::Serialisiert: Debug)]
pub enum Nachricht<Leiter: LeiterAnzeige> {
    /// Ein neues Gleis hinzufügen.
    Gleis {
        /// Das neue Gleis.
        gleis: AnyGleisUnit,
        /// Auf welcher Höhe wurde es ins Bild gezogen.
        klick_höhe: Skalar,
    },
    /// Wechsle den aktuellen Modus.
    Modus(Modus),
    /// Eine Nachricht des Widget zum Bewegen des angezeigten Bereichs.
    Bewegen(bewegen::Nachricht),
    /// Tick für Bewegen des angezeigten Bereichs.
    BewegungAusführen,
    /// Ändere die linke obere Ecke des angezeigten Bereichs.
    Position(Vektor),
    /// Ändere den Winkel des angezeigten Bereichs.
    Winkel(Winkel),
    /// Ändere den Skalierung-Faktor der Anzeige.
    Skalieren(Skalar),
    /// Schließe das [Auswahl](AuswahlStatus)-Fenster.
    SchließeAuswahl,
    /// Schließe die [MessageBox].
    SchließeMessageBox,
    /// Zeige die Auswahl für [Streckenabschnitte](steuerung::Streckenabschnitt).
    ZeigeAuswahlStreckenabschnitt,
    /// Wähle den aktuellen [Streckenabschnitt](steuerung::Streckenabschnitt).
    WähleStreckenabschnitt(Option<(StreckenabschnittId, Farbe)>),
    /// Hinzufügen eines neuen [Streckenabschnittes](steuerung::Streckenabschnitt).
    HinzufügenStreckenabschnitt(
        /// Der Name der assoziierten [Geschwindigkeit](steuerung::Geschwindigkeit).
        Option<geschwindigkeit::Name>,
        /// Der Name des neuen [Streckenabschnittes](steuerung::Streckenabschnitt).
        streckenabschnitt::Name,
        /// Die Farbe, mit der Gleise eingefärbt werden sollen.
        Farbe,
        /// Der verwendete [OutputAnschluss](crate::anschluss::OutputAnschluss).
        OutputSerialisiert,
    ),
    /// Lösche einen [Streckenabschnitt](steuerung::Streckenabschnitt).
    LöscheStreckenabschnitt(StreckenabschnittId),
    /// Setze den [Streckenabschnitt](steuerung::Streckenabschnitt) des spezifizierten Gleises,
    /// sofern es über [StreckenabschnittFestlegen](Nachricht::StreckenabschnittFestlegen)
    /// aktiviert wurde.
    SetzeStreckenabschnitt(AnyId),
    /// Einstellen, ob bei Klick auf ein Gleis der [Streckenabschnitt](steuerung::Streckenabschnitt)
    /// auf den aktuellen gesetzt werden soll
    /// (beeinflusst Reaktion auf [SetzeStreckenabschnitt](Nachricht::SetzeStreckenabschnitt)).
    StreckenabschnittFestlegen(bool),
    /// Speichern im übergebenen Pfad.
    Speichern(String),
    /// Setze die Farbe des Speichern-Knopfes zurück,
    /// sofern die Zeit mit der letzten Speichern-Zeit übereinstimmt.
    EntferneSpeichernFarbe(Instant),
    /// Laden aus dem übergebenen Pfad.
    Laden(String),
    /// Eine Nachricht der [Geschwindigkeit-Anzeige](geschwindigkeit::AnzeigeStatus).
    GeschwindigkeitAnzeige {
        /// Der Name der Geschwindigkeit.
        name: geschwindigkeit::Name,
        /// Die zugehörige Nachricht.
        nachricht: <Leiter as LeiterAnzeige>::Nachricht,
    },
    /// Zeige die Auswahl für [Geschwindigkeiten](steuerung::Geschwindigkeit).
    ZeigeAuswahlGeschwindigkeit,
    /// Hinzufügen einer neuen [Geschwindigkeit](steuerung::Geschwindigkeit).
    HinzufügenGeschwindigkeit(geschwindigkeit::Name, GeschwindigkeitSerialisiert<Leiter>),
    /// Löschen einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    /// Zeige die Auswahl zum Anpassen der Anschlüsse eines Gleises.
    ZeigeAnschlüsseAnpassen(AnyId),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnschlüsseAnpassen),
    /// Führe die Aktion für das Gleis im Fahren-Modus durch
    /// (Streckenabschnitt umstellen, Weiche stellen).
    FahrenAktion(AnyId),
    /// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
    AsyncFehler {
        /// Der Titel der Fehlermeldung.
        titel: String,
        /// Die Nachricht der Fehlermeldung.
        nachricht: String,
        /// Zustand auf Stand vor der Aktion zurücksetzen.
        zustand_zurücksetzen: ZustandZurücksetzen<Leiter>,
    },
}

impl<Leiter: LeiterAnzeige> From<gleise::Nachricht> for Nachricht<Leiter> {
    fn from(message: gleise::Nachricht) -> Self {
        match message {
            gleise::Nachricht::SetzeStreckenabschnitt(any_id) => {
                Nachricht::SetzeStreckenabschnitt(any_id)
            },
            gleise::Nachricht::AnschlüsseAnpassen(any_id) => {
                Nachricht::ZeigeAnschlüsseAnpassen(any_id)
            },
            gleise::Nachricht::FahrenAktion(any_id) => Nachricht::FahrenAktion(any_id),
        }
    }
}

impl<T, Leiter: LeiterAnzeige> ButtonNachricht<NachrichtClone<Leiter>> for T
where
    T: Clone + Into<AnyGleisUnit>,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<Leiter> {
        NachrichtClone::Gleis { gleis: self.clone().into(), klick_höhe: klick_position.y }
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
    fn als_command(self) -> Command<Nachricht<Leiter>> {
        Command::perform(async_identity(self), identity)
    }
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type WeicheStatus = weiche::Status<
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
    gleis::weiche::gerade::RichtungAnschlüsseAuswahlStatus,
>;
type WeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type DreiwegeWeicheStatus = weiche::Status<
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
    gleis::weiche::dreiwege::RichtungAnschlüsseAuswahlStatus,
>;
type DreiwegeWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::Richtung,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type KurvenWeicheStatus = weiche::Status<
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
    gleis::weiche::kurve::RichtungAnschlüsseAuswahlStatus,
>;
type KurvenWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;
type ErstelleAnschlussNachricht<T, Leiter> = Arc<dyn Fn(Option<T>) -> Nachricht<Leiter>>;

/// Zustand des Auswahl-Fensters.
pub enum AuswahlStatus<Leiter: LeiterAnzeige> {
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::Streckenabschnitt).
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    Geschwindigkeit(geschwindigkeit::AuswahlStatus),
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche], [Kreuzung], oder [SKurvenWeiche].
    Weiche(WeicheStatus, ErstelleAnschlussNachricht<WeicheSerialisiert, Leiter>),
    /// Hinzufügen/Verändern der Anschlüsse einer [DreiwegeWeiche].
    DreiwegeWeiche(
        DreiwegeWeicheStatus,
        ErstelleAnschlussNachricht<DreiwegeWeicheSerialisiert, Leiter>,
    ),
    /// Hinzufügen/Verändern der Anschlüsse einer [KurvenWeiche].
    KurvenWeiche(KurvenWeicheStatus, ErstelleAnschlussNachricht<KurvenWeicheSerialisiert, Leiter>),
}

impl<Leiter: LeiterAnzeige> Debug for AuswahlStatus<Leiter> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Streckenabschnitt(arg0) => {
                f.debug_tuple("Streckenabschnitt").field(arg0).finish()
            },
            Self::Geschwindigkeit(arg0) => f.debug_tuple("Geschwindigkeit").field(arg0).finish(),
            Self::Weiche(arg0, _arg1) => {
                f.debug_tuple("Weiche").field(arg0).field(&"<function>".to_string()).finish()
            },
            Self::DreiwegeWeiche(arg0, _arg1) => f
                .debug_tuple("DreiwegeWeiche")
                .field(arg0)
                .field(&"<function>".to_string())
                .finish(),
            Self::KurvenWeiche(arg0, _arg1) => {
                f.debug_tuple("KurvenWeiche").field(arg0).field(&"<function>".to_string()).finish()
            },
        }
    }
}

#[derive(Debug)]
struct MessageBox {
    titel: String,
    nachricht: String,
    button_state: iced::button::State,
}

/// Bei der [Ausführung](ausführen) potentiell auftretende Fehler.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum Fehler {
    /// Ein Fehler beim anzeigen des GUIs.
    Iced(iced::Error),
    /// Ein Fehler beim starten des Loggers.
    FlexiLogger(FlexiLoggerError),
    /// Ein Fehler beim Initialisieren der Pins und I2C-Busse.
    Anschluss(crate::anschluss::InitFehler),
}

/// Parse die Kommandozeilen-Argumente und führe die Anwendung aus.
#[inline(always)]
pub fn ausführen_aus_env() -> Result<(), Fehler> {
    let args = Argumente::parse_aus_env();
    ausführen(args)
}

/// Parse die übergebenen Kommandozeilen-Argumente und führe die Anwendung aus.
pub fn ausführen(argumente: Argumente) -> Result<(), Fehler> {
    let Argumente { i2c_settings, zugtyp, verbose, log_datei, .. } = argumente;
    let lager = crate::anschluss::Lager::neu(i2c_settings)?;

    fn start_logger(verbose: bool, log_datei: bool) -> Result<LoggerHandle, FlexiLoggerError> {
        let log_level = if verbose { LevelFilter::Debug } else { LevelFilter::Warn };
        let mut log_spec_builder = LogSpecBuilder::new();
        let _ = log_spec_builder.default(LevelFilter::Error).module("zugkontrolle", log_level);
        let log_spec = log_spec_builder.finalize();
        let logger_base = Logger::with(log_spec);
        let logger = if log_datei {
            logger_base
                .log_to_file(FileSpec::default().directory("log"))
                .duplicate_to_stderr(Duplicate::All)
        } else {
            logger_base.log_to_stderr()
        };
        logger.start()
    }
    let logger_handle = start_logger(verbose, log_datei)?;

    fn erstelle_settings<Leiter>(
        argumente: Argumente,
        lager: Lager,
        zugtyp: Zugtyp<Leiter>,
    ) -> Settings<(Argumente, Lager, Zugtyp<Leiter>)> {
        Settings {
            window: iced::window::Settings {
                size: (1024, 768),
                icon: icon(),
                ..iced::window::Settings::default()
            },
            default_font: Some(&fonts::REGULAR),
            ..Settings::with_flags((argumente, lager, zugtyp))
        }
    }
    match zugtyp {
        ZugtypArgument::Märklin => {
            Zugkontrolle::run(erstelle_settings(argumente, lager, Zugtyp::märklin()))
        },
        ZugtypArgument::Lego => {
            Zugkontrolle::run(erstelle_settings(argumente, lager, Zugtyp::lego()))
        },
    }?;

    // explizit drop aufrufen, damit logger_handle auf jeden Fall lang genau in scope bleibt.
    drop(logger_handle);

    Ok(())
}

/// Die Anwendung inklusive des GUI.
#[derive(Debug)]
pub struct Zugkontrolle<Leiter: LeiterAnzeige> {
    gleise: Gleise<Leiter>,
    lager: Lager,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<GeradeUnit>>,
    kurven: Vec<Button<KurveUnit>>,
    weichen: Vec<Button<WeicheUnit>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit>>,
    kreuzungen: Vec<Button<KreuzungUnit>>,
    geschwindigkeiten: geschwindigkeit::Map<Leiter>,
    auswahl: modal::Status<AuswahlStatus<Leiter>>,
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
impl<Leiter> Application for Zugkontrolle<Leiter>
where
    Leiter: 'static + LeiterAnzeige + BekannterLeiter + Serialisiere + Display,
    Leiter::Serialisiert: Debug + Clone + Unpin + Send,
{
    type Executor = iced::executor::Default;
    type Flags = (Argumente, Lager, Zugtyp<Leiter>);
    type Message = Nachricht<Leiter>;

    fn new((argumente, lager, zugtyp): Self::Flags) -> (Self, Command<Self::Message>) {
        let Argumente { pfad, modus, zoom, x, y, winkel, .. } = argumente;

        let command: Command<Self::Message>;
        let aktueller_pfad: String;
        if let Some(pfad) = pfad {
            command = Nachricht::Laden(pfad.clone()).als_command();
            aktueller_pfad = pfad.clone();
        } else {
            command = Command::none();
            aktueller_pfad = {
                let mut pfad = zugtyp.name.clone();
                pfad.push_str(".zug");
                pfad
            };
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

        let gleise = Gleise::neu(zugtyp, modus, Position { punkt: Vektor { x, y }, winkel }, zoom);

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
            auswahl: modal::Status::neu(),
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
        format!("Zugkontrolle {}", crate_version!())
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        let mut command = Command::none();

        match message {
            Nachricht::Gleis { gleis, klick_höhe } => self.gleis_hinzufügen(gleis, klick_höhe),
            Nachricht::Modus(modus) => self.gleise.moduswechsel(modus),
            Nachricht::Bewegen(bewegen::Nachricht::StarteBewegung(bewegung)) => {
                command = self.bewegung_starten(bewegung)
            },
            Nachricht::Bewegen(bewegen::Nachricht::BeendeBewegung) => self.bewegung_beenden(),
            Nachricht::Bewegen(bewegen::Nachricht::Zurücksetzen) => self.bewegung_zurücksetzen(),
            Nachricht::BewegungAusführen => {
                if let Some(cmd) = self.bewegung_ausführen() {
                    command = cmd
                }
            },
            Nachricht::Position(position) => self.gleise.setze_pivot(position),
            Nachricht::Winkel(winkel) => self.gleise.winkel(winkel),
            Nachricht::Skalieren(skalieren) => self.gleise.setze_skalierfaktor(skalieren),
            Nachricht::SchließeAuswahl => self.schließe_auswahl(),
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
            },
            Nachricht::SetzeStreckenabschnitt(any_id) => {
                self.gleis_setzte_streckenabschnitt(any_id)
            },
            Nachricht::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_festlegen(festlegen)
            },
            Nachricht::SchließeMessageBox => self.schließe_message_box(),
            Nachricht::Speichern(pfad) => {
                if let Some(cmd) = self.speichern(pfad) {
                    command = cmd
                }
            },
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit)
            },
            Nachricht::Laden(pfad) => self.laden(pfad),
            Nachricht::GeschwindigkeitAnzeige { name, nachricht } => {
                if let Some(cmd) = self.geschwindigkeit_anzeige_nachricht(name, nachricht) {
                    command = cmd
                }
            },
            Nachricht::ZeigeAuswahlGeschwindigkeit => self.zeige_auswahl_geschwindigkeit(),
            Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save)
            },
            Nachricht::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(name),
            Nachricht::ZeigeAnschlüsseAnpassen(any_id) => self.zeige_anschlüsse_anpassen(any_id),
            Nachricht::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                if let Some(message) = self.anschlüsse_anpassen(anschlüsse_anpassen) {
                    command = message.als_command()
                }
            },
            Nachricht::FahrenAktion(any_id) => self.fahren_aktion(any_id),
            Nachricht::AsyncFehler { titel, nachricht, zustand_zurücksetzen } => {
                if let Some(cmd) = self.async_fehler(titel, nachricht, zustand_zurücksetzen) {
                    command = cmd
                }
            },
        }

        command
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
        self.view()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::from_recipe(self.empfänger.clone())
    }
}
