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
use iced::{
    application::Application, widget::Radio, Command, Element, Renderer, Settings, Subscription,
};
use kommandozeilen_argumente::crate_version;
use log::LevelFilter;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{Reserviere, Serialisiere},
        Lager, OutputSerialisiert,
    },
    application::{
        bewegen::{Bewegen, Bewegung},
        drehen::Drehen,
        empfänger::Empfänger,
        geschwindigkeit::LeiterAnzeige,
        icon::icon,
        style::thema::Thema,
    },
    argumente::{Argumente, ZugtypArgument},
    gleis::{
        self,
        gerade::GeradeUnit,
        gleise::{
            self,
            daten::v2::BekannterZugtyp,
            id::{AnyId, GleisId, StreckenabschnittId},
            AnschlüsseAnpassen, GleisSteuerung, Gleise, Modus,
        },
        knopf::{Knopf, KnopfNachricht},
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
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::{AktionGeschwindigkeit, AktionStreckenabschnitt, AnyAktionSchalten, AsyncNachricht},
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
pub mod lizenzen;
pub mod map_mit_zustand;
pub mod modal;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
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
    fn erstelle_radio(self, aktueller_modus: Self) -> Radio<Modus, Renderer<Thema>> {
        Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

/// Klonbare Nachricht, für Verwendung z.B. mit [Button](iced::Button).
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Debug, <L as Leiter>::Fahrtrichtung: Clone)]
enum NachrichtClone<L: Leiter> {
    Gleis { gleis: AnyGleisUnit, klick_höhe: Skalar },
    Skalieren(Skalar),
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
}

impl<L: Leiter, S> From<NachrichtClone<L>> for Nachricht<L, S> {
    fn from(nachricht_clone: NachrichtClone<L>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { gleis, klick_höhe } => Nachricht::Gleis { gleis, klick_höhe },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::AktionGeschwindigkeit(aktion) => {
                Nachricht::AktionGeschwindigkeit(aktion)
            },
        }
    }
}

/// Eine Nachricht, die beim [Ausführen](ausführen) der Anwendung auftreten kann.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[non_exhaustive]
pub enum Nachricht<L: Leiter, S> {
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
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
    /// Hinzufügen einer neuen [Geschwindigkeit](steuerung::Geschwindigkeit).
    HinzufügenGeschwindigkeit(geschwindigkeit::Name, GeschwindigkeitSerialisiert<S>),
    /// Löschen einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnschlüsseAnpassen),
    /// Ein Gleis mit [Streckenabschnitt](crate::steuerung::Streckenabschnitt) ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche](steuerung::Weiche) wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Zeige Lizenzen der verwendeten Open Source Libraries an.
    AsyncAktualisieren,
    /// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
    AsyncFehler {
        /// Der Titel der Fehlermeldung.
        titel: String,
        /// Die Nachricht der Fehlermeldung.
        nachricht: String,
    },
}

impl<L: Leiter, S> From<gleise::Nachricht> for modal::Nachricht<AuswahlZustand, Nachricht<L, S>> {
    fn from(nachricht: gleise::Nachricht) -> Self {
        match nachricht {
            gleise::Nachricht::SetzeStreckenabschnitt(any_id) => {
                modal::Nachricht::Underlay(Nachricht::SetzeStreckenabschnitt(any_id))
            },
            gleise::Nachricht::StreckenabschnittUmschalten(aktion) => {
                modal::Nachricht::Underlay(Nachricht::StreckenabschnittUmschalten(aktion))
            },
            gleise::Nachricht::WeicheSchalten(aktion) => {
                modal::Nachricht::Underlay(Nachricht::WeicheSchalten(aktion))
            },
            gleise::Nachricht::AnschlüsseAnpassen(gleis_steuerung) => match gleis_steuerung {
                GleisSteuerung::Gerade((id, startwert)) => {
                    todo!("AuswahlZustand::Gerade({id:?}, {startwert:?}")
                },
                GleisSteuerung::Kurve((id, startwert)) => {
                    todo!("AuswahlZustand::Kurve({id:?}, {startwert:?}")
                },
                GleisSteuerung::Weiche((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::Gerade(id)),
                ),
                GleisSteuerung::KurvenWeiche((id, startwert)) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::KurvenWeiche(startwert, id))
                },
                GleisSteuerung::DreiwegeWeiche((id, startwert)) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::DreiwegeWeiche(startwert, id))
                },
                GleisSteuerung::SKurvenWeiche((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::SKurve(id)),
                ),
                GleisSteuerung::Kreuzung((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::Kreuzung(id)),
                ),
            },
        }
    }
}

impl<L: Leiter, S> From<AsyncNachricht> for Nachricht<L, S> {
    fn from(fehler: AsyncNachricht) -> Self {
        match fehler {
            AsyncNachricht::Aktualisieren => Nachricht::AsyncAktualisieren,
            AsyncNachricht::Fehler { titel, nachricht } => {
                Nachricht::AsyncFehler { titel, nachricht }
            },
        }
    }
}

impl<L: Leiter, S> From<streckenabschnitt::AnzeigeNachricht> for Nachricht<L, S> {
    fn from(nachricht: streckenabschnitt::AnzeigeNachricht) -> Self {
        match nachricht {
            streckenabschnitt::AnzeigeNachricht::Festlegen(festlegen) => {
                Nachricht::StreckenabschnittFestlegen(festlegen)
            },
        }
    }
}

impl<T, L> KnopfNachricht<NachrichtClone<L>> for T
where
    T: Clone + Into<AnyGleisUnit>,
    L: Leiter,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<L> {
        NachrichtClone::Gleis { gleis: self.clone().into(), klick_höhe: klick_position.y }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<L, S> Nachricht<L, S>
where
    L: 'static + Leiter + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    fn als_command(self) -> Command<Nachricht<L, S>> {
        Command::perform(async_identity(self), identity)
    }
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type WeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

/// Die Id einer Weiche mit [gleis::weiche::gerade::Richtung].
#[derive(Debug, PartialEq)]
pub enum WeichenId {
    /// Die Id einer [Weiche].
    Gerade(GleisId<Weiche>),
    /// Die Id einer [SKurvenWeiche].
    SKurve(GleisId<SKurvenWeiche>),
    /// Die Id einer [Kreuzung].
    Kreuzung(GleisId<Kreuzung>),
}

impl WeichenId {
    fn klonen(&self) -> Self {
        match self {
            WeichenId::Gerade(id) => WeichenId::Gerade(id.klonen()),
            WeichenId::SKurve(id) => WeichenId::SKurve(id.klonen()),
            WeichenId::Kreuzung(id) => WeichenId::Kreuzung(id.klonen()),
        }
    }
}

type DreiwegeWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type KurvenWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

/// Zustand des Auswahl-Fensters.
#[derive(Debug, PartialEq)]
pub enum AuswahlZustand {
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::Streckenabschnitt).
    Streckenabschnitt,
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    Geschwindigkeit,
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche], [Kreuzung], oder [SKurvenWeiche].
    Weiche(Option<WeicheSerialisiert>, WeichenId),
    /// Hinzufügen/Verändern der Anschlüsse einer [DreiwegeWeiche].
    DreiwegeWeiche(Option<DreiwegeWeicheSerialisiert>, GleisId<DreiwegeWeiche>),
    /// Hinzufügen/Verändern der Anschlüsse einer [KurvenWeiche].
    KurvenWeiche(Option<KurvenWeicheSerialisiert>, GleisId<KurvenWeiche>),
    /// Anzeige der verwendeten Open-Source Lizenzen.
    ZeigeLizenzen,
}

impl Clone for AuswahlZustand {
    fn clone(&self) -> Self {
        match self {
            AuswahlZustand::Streckenabschnitt => AuswahlZustand::Streckenabschnitt,
            AuswahlZustand::Geschwindigkeit => AuswahlZustand::Geschwindigkeit,
            AuswahlZustand::Weiche(startwert, id) => {
                AuswahlZustand::Weiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::DreiwegeWeiche(startwert, id) => {
                AuswahlZustand::DreiwegeWeiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::KurvenWeiche(startwert, id) => {
                AuswahlZustand::KurvenWeiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::ZeigeLizenzen => AuswahlZustand::ZeigeLizenzen,
        }
    }
}

/// Anzeige einer Meldung.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageBox {
    titel: String,
    nachricht: String,
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
                size: (800, 480),
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
#[zugkontrolle_debug(S: Debug)]
pub struct Zugkontrolle<L: Leiter, S> {
    gleise: Gleise<L>,
    lager: Lager,
    scrollable_style: style::sammlung::Sammlung,
    geraden: Vec<Knopf<GeradeUnit>>,
    kurven: Vec<Knopf<KurveUnit>>,
    weichen: Vec<Knopf<WeicheUnit>>,
    dreiwege_weichen: Vec<Knopf<DreiwegeWeicheUnit>>,
    kurven_weichen: Vec<Knopf<KurvenWeicheUnit>>,
    s_kurven_weichen: Vec<Knopf<SKurvenWeicheUnit>>,
    kreuzungen: Vec<Knopf<KreuzungUnit>>,
    streckenabschnitt_aktuell: Option<(StreckenabschnittId, Farbe)>,
    streckenabschnitt_aktuell_festlegen: bool,
    bewegen: Bewegen,
    drehen: Drehen,
    initialer_pfad: String,
    speichern_gefärbt: Option<Instant>,
    bewegung: Option<Bewegung>,
    message_box: Option<MessageBox>,
    sender: Sender<Nachricht<L, S>>,
    empfänger: Empfänger<Nachricht<L, S>>,
    // TODO Plan
}

impl<L, S> Application for Zugkontrolle<L, S>
where
    L: 'static
        + Debug
        + Display
        + for<'l> LeiterAnzeige<'l, S, Renderer<Thema>>
        + Serialisiere<S>
        + BekannterLeiter
        + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::UmdrehenZeit: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::Fahrtrichtung:
        Debug + Clone + Serialize + for<'de> Deserialize<'de> + Unpin + Send,
    S: 'static + Debug + Clone + Eq + Hash + Unpin + Send,
    S: Reserviere<L, Arg = ()> + Serialize + for<'de> Deserialize<'de>,
    // zusätzlicher Constraint für v2-Kompatibilität
    L: BekannterZugtyp,
    S: From<<L as BekannterZugtyp>::V2>,
    for<'de> <L as BekannterZugtyp>::V2: Deserialize<'de>,
{
    type Executor = iced::executor::Default;
    type Flags = (Argumente, Lager, Zugtyp<L>);
    type Message = Nachricht<L, S>;
    type Theme = Thema;

    fn new((argumente, lager, zugtyp): Self::Flags) -> (Self, Command<Self::Message>) {
        let Argumente { pfad, modus, zoom, x, y, winkel, .. } = argumente;

        let command: Command<Self::Message>;
        let initialer_pfad: String;
        if let Some(pfad) = pfad {
            command = Nachricht::Laden(pfad.clone()).als_command();
            initialer_pfad = pfad.clone();
        } else {
            command = Command::none();
            initialer_pfad = {
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
            scrollable_style: style::sammlung::Sammlung::neu(10.),
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            streckenabschnitt_aktuell: None,
            streckenabschnitt_aktuell_festlegen: false,
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            initialer_pfad,
            speichern_gefärbt: None,
            bewegung: None,
            message_box: None,
            sender,
            empfänger: Empfänger::neu(receiver),
        };

        (zugkontrolle, command)
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", crate_version!())
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
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
            Nachricht::Speichern(pfad) => {
                if let Some(cmd) = self.speichern(pfad) {
                    command = cmd
                }
            },
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit)
            },
            Nachricht::Laden(pfad) => self.laden(pfad),
            Nachricht::AktionGeschwindigkeit(aktion) => {
                self.async_aktion_ausführen(aktion, Some(Nachricht::AsyncAktualisieren))
            },
            Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save)
            },
            Nachricht::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(name),
            Nachricht::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                self.anschlüsse_anpassen(anschlüsse_anpassen)
            },
            Nachricht::StreckenabschnittUmschalten(aktion) => self.aktion_ausführen(aktion),
            Nachricht::WeicheSchalten(aktion) => self.async_aktion_ausführen(aktion, None),
            Nachricht::AsyncAktualisieren => {},
            Nachricht::AsyncFehler { titel, nachricht } => self.async_fehler(titel, nachricht),
        }

        command
    }

    fn view(&self) -> Element<'_, Self::Message, Renderer<Self::Theme>> {
        Zugkontrolle::view(self)
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::from_recipe(self.empfänger.clone())
    }
}
