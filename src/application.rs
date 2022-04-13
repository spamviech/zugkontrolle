//! [Application] für die Gleis-Anzeige.

use std::{
    convert::identity,
    fmt::{Debug, Display},
    hash::Hash,
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
use serde::{Deserialize, Serialize};

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
        gerade::GeradeUnit,
        gleise::{
            self,
            daten::v2,
            id::{AnyId, GleisId, StreckenabschnittId},
            Gleise, Modus, SteuerungDreiwegeWeiche, SteuerungGeradeWeiche, SteuerungKreuzung,
            SteuerungKurvenWeiche, SteuerungSKurveWeiche,
        },
        knopf::{Knopf, KnopfNachricht},
        kreuzung::{self, Kreuzung, KreuzungUnit},
        kurve::KurveUnit,
        weiche::{
            dreiwege::{self, DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{self, Weiche, WeicheUnit},
            kurve::{self, KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{self, SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        self,
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::{AktionGeschwindigkeit, AktionSchalten, AktionStreckenabschnitt, AsyncFehler},
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

/// Klonbare Nachricht, für Verwendung z.B. mit [Button](iced::Button).
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Debug, <L as Leiter>::Fahrtrichtung: Clone)]
enum NachrichtClone<L: LeiterAnzeige> {
    Gleis { gleis: AnyGleisUnit, klick_höhe: Skalar },
    Skalieren(Skalar),
    SchließeMessageBox,
    AktionGeschwindigkeit(geschwindigkeit::Name, AktionGeschwindigkeit<L>),
    ZeigeAuswahlGeschwindigkeit,
}

impl<Leiter: LeiterAnzeige> From<NachrichtClone<Leiter>> for Nachricht<Leiter> {
    fn from(nachricht_clone: NachrichtClone<Leiter>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { gleis, klick_höhe } => Nachricht::Gleis { gleis, klick_höhe },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::SchließeMessageBox => Nachricht::SchließeMessageBox,
            NachrichtClone::AktionGeschwindigkeit(name, aktion) => {
                Nachricht::AktionGeschwindigkeit(name, aktion)
            },
            NachrichtClone::ZeigeAuswahlGeschwindigkeit => Nachricht::ZeigeAuswahlGeschwindigkeit,
        }
    }
}

/// Eine Nachricht, die beim [Ausführen](ausführen) der Anwendung auftreten kann.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug + Serialisiere)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_debug(<L as Serialisiere>::Serialisiert: Debug)]
pub enum Nachricht<L>
where
    L: Leiter + Serialisiere,
{
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
    /// Schließe das [Auswahl](AuswahlZustand)-Fenster.
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
    /// Eine Aktion einer [Geschwindigkeit](steuerung::Geschwindigkeit) im [Fahren](Modus::Fahren)-Modus.
    AktionGeschwindigkeit(geschwindigkeit::Name, AktionGeschwindigkeit<L>),
    /// Aktualisiere den Zustand einer [Geschwindigkeit-Anzeige](geschwindigkeit::Anzeige).
    GeschwindigkeitAktualisieren(geschwindigkeit::Name),
    /// Zeige die Auswahl für [Geschwindigkeiten](steuerung::Geschwindigkeit).
    ZeigeAuswahlGeschwindigkeit,
    /// Hinzufügen einer neuen [Geschwindigkeit](steuerung::Geschwindigkeit).
    HinzufügenGeschwindigkeit(geschwindigkeit::Name, GeschwindigkeitSerialisiert<L>),
    /// Löschen einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    /// Zeige die Auswahl zum Anpassen der Anschlüsse eines Gleises.
    ZeigeAnschlüsseAnpassen(AnyId),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnschlüsseAnpassen),
    /// Ein Gleis mit [Streckenabschnitt](crate::steuerung::Streckenabschnitt) ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    GeradeWeicheSchalten(GleisId<Weiche>, AktionSchalten<SteuerungGeradeWeiche, gerade::Richtung>),
    /// Ein [KurvenWeiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    KurvenWeicheSchalten(
        GleisId<KurvenWeiche>,
        AktionSchalten<SteuerungKurvenWeiche, kurve::Richtung>,
    ),
    /// Ein [DreiwegeWeiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    DreiwegeWeicheSchalten(
        GleisId<DreiwegeWeiche>,
        AktionSchalten<SteuerungDreiwegeWeiche, dreiwege::Richtung>,
    ),
    /// Ein [SKurvenWeiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    SKurvenWeicheSchalten(
        GleisId<SKurvenWeiche>,
        AktionSchalten<SteuerungSKurveWeiche, s_kurve::Richtung>,
    ),
    /// Ein [Kreuzung] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    KreuzungSchalten(GleisId<Kreuzung>, AktionSchalten<SteuerungKreuzung, kreuzung::Richtung>),
    /// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
    AsyncFehler {
        /// Der Titel der Fehlermeldung.
        titel: String,
        /// Die Nachricht der Fehlermeldung.
        nachricht: String,
    },
}

impl<Leiter: LeiterAnzeige> From<gleise::Nachricht> for Nachricht<Leiter> {
    fn from(nachricht: gleise::Nachricht) -> Self {
        match nachricht {
            gleise::Nachricht::SetzeStreckenabschnitt(any_id) => {
                Nachricht::SetzeStreckenabschnitt(any_id)
            },
            gleise::Nachricht::AnschlüsseAnpassen(any_id) => {
                Nachricht::ZeigeAnschlüsseAnpassen(any_id)
            },
            gleise::Nachricht::StreckenabschnittUmschalten(aktion) => {
                Nachricht::StreckenabschnittUmschalten(aktion)
            },
            gleise::Nachricht::GeradeWeicheSchalten(id, aktion) => {
                Nachricht::GeradeWeicheSchalten(id, aktion)
            },
            gleise::Nachricht::KurvenWeicheSchalten(id, aktion) => {
                Nachricht::KurvenWeicheSchalten(id, aktion)
            },
            gleise::Nachricht::DreiwegeWeicheSchalten(id, aktion) => {
                Nachricht::DreiwegeWeicheSchalten(id, aktion)
            },
            gleise::Nachricht::SKurvenWeicheSchalten(id, aktion) => {
                Nachricht::SKurvenWeicheSchalten(id, aktion)
            },
            gleise::Nachricht::KreuzungSchalten(id, aktion) => {
                Nachricht::KreuzungSchalten(id, aktion)
            },
        }
    }
}

impl<Leiter: LeiterAnzeige> From<AsyncFehler> for Nachricht<Leiter> {
    fn from(fehler: AsyncFehler) -> Self {
        let AsyncFehler { titel, nachricht } = fehler;
        Nachricht::AsyncFehler { titel, nachricht }
    }
}

impl<T, Leiter> KnopfNachricht<NachrichtClone<Leiter>> for T
where
    T: Clone + Into<AnyGleisUnit>,
    Leiter: LeiterAnzeige,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<Leiter> {
        NachrichtClone::Gleis { gleis: self.clone().into(), klick_höhe: klick_position.y }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<L> Nachricht<L>
where
    L: 'static + LeiterAnzeige + Serialisiere + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    <L as Serialisiere>::Serialisiert: Debug + Send,
{
    fn als_command(self) -> Command<Nachricht<L>> {
        Command::perform(async_identity(self), identity)
    }
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type WeicheZustand = weiche::Zustand<
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
    gleis::weiche::gerade::RichtungAnschlüsseAuswahlZustand,
>;
type WeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type DreiwegeWeicheZustand = weiche::Zustand<
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
    gleis::weiche::dreiwege::RichtungAnschlüsseAuswahlZustand,
>;
type DreiwegeWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::Richtung,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type KurvenWeicheZustand = weiche::Zustand<
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
    gleis::weiche::kurve::RichtungAnschlüsseAuswahlZustand,
>;
type KurvenWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;
type ErstelleAnschlussNachricht<T, Leiter> = Arc<dyn Fn(Option<T>) -> Nachricht<Leiter>>;

/// Zustand des Auswahl-Fensters.
pub enum AuswahlZustand<Leiter: LeiterAnzeige> {
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::Streckenabschnitt).
    Streckenabschnitt(streckenabschnitt::AuswahlZustand),
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    Geschwindigkeit(geschwindigkeit::AuswahlZustand),
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche], [Kreuzung], oder [SKurvenWeiche].
    Weiche(WeicheZustand, ErstelleAnschlussNachricht<WeicheSerialisiert, Leiter>),
    /// Hinzufügen/Verändern der Anschlüsse einer [DreiwegeWeiche].
    DreiwegeWeiche(
        DreiwegeWeicheZustand,
        ErstelleAnschlussNachricht<DreiwegeWeicheSerialisiert, Leiter>,
    ),
    /// Hinzufügen/Verändern der Anschlüsse einer [KurvenWeiche].
    KurvenWeiche(KurvenWeicheZustand, ErstelleAnschlussNachricht<KurvenWeicheSerialisiert, Leiter>),
}

impl<Leiter: LeiterAnzeige> Debug for AuswahlZustand<Leiter> {
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
    button_zustand: iced::button::State,
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

    fn erstelle_settings<L: Leiter>(
        argumente: Argumente,
        lager: Lager,
        zugtyp: Zugtyp<L>,
    ) -> Settings<(Argumente, Lager, Zugtyp<L>)> {
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
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub struct Zugkontrolle<L: LeiterAnzeige> {
    gleise: Gleise<L>,
    lager: Lager,
    scrollable_zustand: iced::scrollable::State,
    scrollable_style: style::sammlung::Sammlung,
    geraden: Vec<Knopf<GeradeUnit>>,
    kurven: Vec<Knopf<KurveUnit>>,
    weichen: Vec<Knopf<WeicheUnit>>,
    dreiwege_weichen: Vec<Knopf<DreiwegeWeicheUnit>>,
    kurven_weichen: Vec<Knopf<KurvenWeicheUnit>>,
    s_kurven_weichen: Vec<Knopf<SKurvenWeicheUnit>>,
    kreuzungen: Vec<Knopf<KreuzungUnit>>,
    geschwindigkeiten: geschwindigkeit::Map<L>,
    auswahl: modal::Zustand<AuswahlZustand<L>>,
    streckenabschnitt_aktuell: streckenabschnitt::AnzeigeZustand,
    streckenabschnitt_aktuell_festlegen: bool,
    geschwindigkeit_button_zustand: iced::button::State,
    message_box: modal::Zustand<MessageBox>,
    bewegen: Bewegen,
    drehen: Drehen,
    zoom: iced::slider::State,
    speichern_laden: speichern_laden::Zustand,
    speichern_gefärbt: Option<Instant>,
    bewegung: Option<Bewegung>,
    sender: Sender<Nachricht<L>>,
    empfänger: Empfänger<Nachricht<L>>,
    // TODO Plan
}

#[allow(single_use_lifetimes)]
impl<L> Application for Zugkontrolle<L>
where
    L: 'static
        + Debug
        + Display
        + LeiterAnzeige
        + BekannterLeiter
        + Serialisiere
        + v2::Kompatibel
        + Send,
    <L as Serialisiere>::Serialisiert: Debug + Clone + Unpin + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::UmdrehenZeit: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::Fahrtrichtung:
        Debug + Clone + Serialize + for<'de> Deserialize<'de> + Unpin + Send,
    <L as Serialisiere>::Serialisiert: Eq + Hash,
{
    type Executor = iced::executor::Default;
    type Flags = (Argumente, Lager, Zugtyp<L>);
    type Message = Nachricht<L>;

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

        macro_rules! erstelle_knopf {
            () => {
                |gleis| Knopf::neu(gleis.clone(), zugtyp.spurweite)
            };
        }
        let geraden = zugtyp.geraden.iter().map(erstelle_knopf!()).collect();
        let kurven = zugtyp.kurven.iter().map(erstelle_knopf!()).collect();
        let weichen = zugtyp.weichen.iter().map(erstelle_knopf!()).collect();
        let dreiwege_weichen = zugtyp.dreiwege_weichen.iter().map(erstelle_knopf!()).collect();
        let kurven_weichen = zugtyp.kurven_weichen.iter().map(erstelle_knopf!()).collect();
        let s_kurven_weichen = zugtyp.s_kurven_weichen.iter().map(erstelle_knopf!()).collect();
        let kreuzungen = zugtyp.kreuzungen.iter().map(erstelle_knopf!()).collect();

        let gleise = Gleise::neu(zugtyp, modus, Position { punkt: Vektor { x, y }, winkel }, zoom);

        let (sender, receiver) = channel();
        let zugkontrolle = Zugkontrolle {
            gleise,
            lager,
            scrollable_zustand: iced::scrollable::State::new(),
            scrollable_style: style::sammlung::Sammlung::neu(10),
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten: geschwindigkeit::Map::new(),
            auswahl: modal::Zustand::neu(),
            streckenabschnitt_aktuell: streckenabschnitt::AnzeigeZustand::neu(),
            streckenabschnitt_aktuell_festlegen: false,
            geschwindigkeit_button_zustand: iced::button::State::new(),
            message_box: modal::Zustand::neu(),
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            zoom: iced::slider::State::new(),
            speichern_laden: speichern_laden::Zustand::neu(aktueller_pfad),
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
            Nachricht::AktionGeschwindigkeit(name, aktion) => {
                // TODO Umdrehen zeigt Geschwindigkeit(0) erst nach vollständigem ausführen an
                self.async_aktion_ausführen(
                    aktion,
                    Some(Nachricht::GeschwindigkeitAktualisieren(name)),
                )
            },
            Nachricht::GeschwindigkeitAktualisieren(name) => {
                // Ignoriere entfernte Geschwindigkeiten,
                // da es nur um ein aktualisieren als Reaktion auf eine asynchrone Aktion geht.
                if let Some(anzeige_zustand) = self.geschwindigkeiten.get_mut(&name) {
                    anzeige_zustand.flip()
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
            Nachricht::GeradeWeicheSchalten(id, aktion) => {
                self.async_aktion_ausführen(aktion, None)
            },
            Nachricht::KurvenWeicheSchalten(id, aktion) => {
                self.async_aktion_ausführen(aktion, None)
            },
            Nachricht::DreiwegeWeicheSchalten(id, aktion) => {
                self.async_aktion_ausführen(aktion, None)
            },
            Nachricht::SKurvenWeicheSchalten(id, aktion) => {
                self.async_aktion_ausführen(aktion, None)
            },
            Nachricht::KreuzungSchalten(id, aktion) => self.async_aktion_ausführen(aktion, None),
            Nachricht::StreckenabschnittUmschalten(aktion) => self.aktion_ausführen(aktion),
            Nachricht::AsyncFehler { titel, nachricht } => self.async_fehler(titel, nachricht),
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
