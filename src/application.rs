//! [Application] für die Gleis-Anzeige.

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter,
    sync::mpsc::{channel, Sender},
    time::Instant,
};

use flexi_logger::{Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger, LoggerHandle};
use iced::{application::Application, Command, Element, Renderer, Settings, Subscription};
use kommandozeilen_argumente::crate_version;
use log::LevelFilter;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{Reserviere, Serialisiere},
        Lager,
    },
    application::{
        auswahl::AuswahlZustand,
        bewegen::{Bewegen, Bewegung},
        drehen::Drehen,
        empfänger::Empfänger,
        fonts::BENÖTIGTE_FONT_BYTES,
        geschwindigkeit::LeiterAnzeige,
        icon::icon,
        modal::Overlay,
        nachricht::Nachricht,
        style::thema::Thema,
    },
    argumente::{Argumente, I2cSettings, ZugtypArgument},
    gleis::gleise::{daten::v2::BekannterZugtyp, Gleise},
    steuerung::{
        geschwindigkeit::{BekannterLeiter, Leiter},
        streckenabschnitt::Name as StreckenabschnittName,
    },
    typen::{canvas::Position, farbe::Farbe, vektor::Vektor},
    zugtyp::Zugtyp,
};

pub mod anschluss;
pub mod auswahl;
pub mod bewegen;
pub mod bootstrap;
pub mod drehen;
#[path = "application/empfänger.rs"]
pub mod empfänger;
pub mod farbwahl;
pub mod flat_map;
pub mod fonts;
pub mod geschwindigkeit;
pub mod icon;
pub mod kontakt;
pub mod lizenzen;
pub mod map_mit_zustand;
pub mod modal;
pub mod nachricht;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod update;
pub mod view;
pub mod weiche;

/// Anzeige einer Meldung.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageBox {
    titel: String,
    nachricht: String,
}

/// Die Anwendung inklusive des GUI.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_debug(S: Debug)]
pub struct Zugkontrolle<L: Leiter, S> {
    gleise: Gleise<L, Nachricht<L, S>>,
    lager: Lager,
    scrollable_style: style::sammlung::Sammlung,
    i2c_settings: I2cSettings,
    streckenabschnitt_aktuell: Option<(StreckenabschnittName, Farbe)>,
    streckenabschnitt_aktuell_festlegen: bool,
    bewegen: Bewegen,
    drehen: Drehen,
    initialer_pfad: String,
    speichern_gefärbt: Option<(bool, Instant)>,
    bewegung: Option<Bewegung>,
    auswahl_zustand: Overlay<AuswahlZustand<S>>,
    message_box: Overlay<MessageBox>,
    sender: Sender<Nachricht<L, S>>,
    empfänger: Empfänger<Nachricht<L, S>>,
    // TODO Plan
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

type Flags<L> = (Argumente, Lager, &'static Zugtyp<L>, &'static [&'static [u8]]);

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
        zugtyp: &'static Zugtyp<L>,
    ) -> Settings<Flags<L>> {
        Settings {
            window: iced::window::Settings {
                size: (800, 480),
                icon: icon(),
                ..iced::window::Settings::default()
            },
            default_font: fonts::REGULAR,
            ..Settings::with_flags((argumente, lager, zugtyp, BENÖTIGTE_FONT_BYTES))
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
    S: Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>
        + Serialize
        + for<'de> Deserialize<'de>,
    // zusätzlicher Constraint für v2-Kompatibilität
    L: BekannterZugtyp,
    S: From<<L as BekannterZugtyp>::V2>,
    for<'de> <L as BekannterZugtyp>::V2: Deserialize<'de>,
{
    type Executor = iced::executor::Default;
    type Flags = Flags<L>;
    type Message = Nachricht<L, S>;
    type Theme = Thema;

    fn new(
        (argumente, lager, zugtyp2, schriftarten): Self::Flags,
    ) -> (Self, Command<Self::Message>) {
        let Argumente { pfad, modus, zoom, x, y, winkel, i2c_settings, .. } = argumente;

        let lade_schriftarten = schriftarten.iter().map(|&schriftart| {
            iced::font::load(schriftart).map(|ergebnis| match ergebnis {
                Ok(()) => Nachricht::AsyncAktualisieren { gleise_neuzeichnen: true },
                Err(fehler) => Nachricht::AsyncFehler {
                    titel: String::from("Schriftart laden"),
                    nachricht: format!("{fehler:?}"),
                },
            })
        });
        let lade_zustand: Command<Self::Message>;
        let initialer_pfad: String;
        if let Some(pfad) = pfad {
            lade_zustand = Nachricht::Laden(pfad.clone()).als_command();
            initialer_pfad = pfad.clone();
        } else {
            lade_zustand = Command::none();
            initialer_pfad = {
                let mut pfad = zugtyp2.name.clone();
                pfad.push_str(".zug");
                pfad
            };
        };

        let (sender, receiver) = channel();

        let gleise = Gleise::neu(
            zugtyp2.clone(),
            modus,
            Position { punkt: Vektor { x, y }, winkel },
            zoom,
            sender.clone(),
        );

        let zugkontrolle = Zugkontrolle {
            gleise,
            lager,
            scrollable_style: style::sammlung::Sammlung::neu(10.),
            i2c_settings,
            streckenabschnitt_aktuell: None,
            streckenabschnitt_aktuell_festlegen: false,
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            initialer_pfad,
            speichern_gefärbt: None,
            bewegung: None,
            auswahl_zustand: Overlay::neu(None),
            message_box: Overlay::neu(None),
            sender,
            empfänger: Empfänger::neu(receiver, ()),
        };

        (zugkontrolle, Command::batch(lade_schriftarten.chain(iter::once(lade_zustand))))
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", crate_version!())
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        let mut command = Command::none();

        match message {
            Nachricht::Gleis { definition_steuerung, klick_quelle, klick_höhe } => {
                self.gleis_hinzufügen(definition_steuerung, klick_quelle, klick_höhe)
            },
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
                geschwindigkeit,
                name,
                farbe,
                anschluss_definition,
            ),
            Nachricht::LöscheStreckenabschnitt(streckenabschnitt_name) => {
                self.streckenabschnitt_löschen(&streckenabschnitt_name)
            },
            Nachricht::SetzeStreckenabschnitt(any_id) => {
                self.gleis_setzte_streckenabschnitt(any_id)
            },
            Nachricht::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_festlegen(festlegen)
            },
            Nachricht::Speichern(pfad) => {
                command = self.speichern(pfad);
            },
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit)
            },
            Nachricht::Laden(pfad) => self.laden(pfad),
            Nachricht::AktionGeschwindigkeit(aktion) => self.async_aktion_ausführen(
                aktion,
                Some(Nachricht::AsyncAktualisieren { gleise_neuzeichnen: false }),
            ),
            Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save)
            },
            Nachricht::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(&name),
            Nachricht::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                self.anschlüsse_anpassen(anschlüsse_anpassen)
            },
            Nachricht::StreckenabschnittUmschalten(aktion) => self.aktion_ausführen(aktion),
            Nachricht::WeicheSchalten(aktion) => self.async_aktion_ausführen(aktion, None),
            Nachricht::GleiseZustandAktualisieren(nachricht) => {
                self.gleise_zustand_aktualisieren(nachricht)
            },
            Nachricht::AsyncAktualisieren { gleise_neuzeichnen } => {
                if gleise_neuzeichnen {
                    self.gleise_neuzeichnen()
                }
            },
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
