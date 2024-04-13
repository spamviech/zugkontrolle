//! [`Application`] für die Gleis-Anzeige.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]
// Erlaube mehr rekursive Aufrufe von Macros.
#![recursion_limit = "256"]

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::mpsc::{channel, Sender},
    time::Instant,
};

use flexi_logger::FlexiLoggerError;
use iced::{application::Application, executor, Command, Element, Renderer, Subscription};
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    de_serialisieren::{Reserviere, Serialisiere},
    InitFehler, Lager,
};
use zugkontrolle_argumente::{Argumente, I2cSettings};
use zugkontrolle_gleis::steuerung::{
    geschwindigkeit::{BekannterLeiter, Leiter},
    streckenabschnitt::Name as StreckenabschnittName,
};
use zugkontrolle_gleis::zugtyp::Zugtyp;
use zugkontrolle_gleise::{daten::v2::geschwindigkeit::BekannterZugtyp, Gleise};
use zugkontrolle_typen::{canvas::Position, farbe::Farbe, vektor::Vektor};

use crate::{
    auswahl::AuswahlZustand,
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    empfänger::Empfänger,
    geschwindigkeit::LeiterAnzeige,
    nachricht::Nachricht,
    style::thema::Thema,
};

pub mod anschluss;
pub mod auswahl;
pub mod bewegen;
pub mod bootstrap;
pub mod drehen;
#[path = "empfänger.rs"]
pub mod empfänger;
pub mod farbwahl;
pub mod flat_map;
pub mod fonts;
pub mod geschwindigkeit;
pub mod icon;
pub mod kontakt;
pub mod lizenzen;
pub mod map_mit_zustand;
pub mod map_operation;
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
    /// Titel der MessageBox.
    titel: String,
    /// Nachricht der MessageBox.
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
    /// Alle Gleise, Streckenabschnitte, und Geschwindigkeiten.
    gleise: Gleise<L, Nachricht<L, S>>,
    /// Noch verfügbare Anschlüsse.
    lager: Lager,
    /// Der Stil für verwendete [`Scrollable-Widgets`](iced::widget::Scrollable)
    scrollable_style: style::sammlung::Sammlung,
    /// Aktivierte [`I2C-Busse`](crate::anschluss::pcf8574::I2cBus).
    i2c_settings: I2cSettings,
    /// Der aktuell aktivierte Streckenabschnitt.
    streckenabschnitt_aktuell: Option<(StreckenabschnittName, Farbe)>,
    /// Führt das anklicken eines Gleises zum setzen des [`Streckenabschnittes`](crate::steuerung::streckenabschnitt::Streckenabschnitt)?
    streckenabschnitt_aktuell_festlegen: bool,
    /// Zustand für Anzeigen des Widgets für die Positions-Steuerung des [`Gleise`]-canvas.
    bewegen: Bewegen,
    /// Zustand für Anzeigen des Widgets für die Rotation-Steuerung des [`Gleise`]-canvas.
    drehen: Drehen,
    /// Das aktuelle Anzeige-[`Thema`].
    thema: Thema,
    /// Der initiale Pfad des SpeichernLaden-Widgets.
    initialer_pfad: String,
    /// Zeigt der Speichern-Knopf aktuell an, dass er gedrückt wurde.
    speichern_gefärbt: Option<(bool, Instant)>,
    /// Wie wird der [`Gleise`]-Canvas aktuell bewegt.
    bewegung: Option<Bewegung>,
    /// Der aktuelle Zustand des [`Auswahl-Widgets`](AuswahlZustand).
    auswahl_zustand: Option<AuswahlZustand<S>>,
    /// Der aktuelle Zustand des [`Mitteilungs-Widgets`](MessageBox).
    message_box: Option<MessageBox>,
    /// Sender für asynchrone [Nachrichten](Nachricht), die über einen [`Kanal`](channel) gesendet werden.
    sender: Sender<Nachricht<L, S>>,
    /// Empfänger für asynchrone [Nachrichten](Nachricht), die über einen [`Kanal`](channel) gesendet werden.
    empfänger: Empfänger<Nachricht<L, S>>,
}

/// Bei der [`Ausführung`](ausführen) potentiell auftretende Fehler.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum Fehler {
    /// Ein Fehler beim anzeigen des GUIs.
    Iced(iced::Error),
    /// Ein Fehler beim starten des Loggers.
    FlexiLogger(FlexiLoggerError),
    /// Ein Fehler beim Initialisieren der Pins und I2C-Busse.
    Anschluss(InitFehler),
}

/// Flags für den [`Application`]-Trait.
pub type Flags<L> = (Argumente, Lager, &'static Zugtyp<L>);

impl<L, S> Application for Zugkontrolle<L, S>
where
    L: 'static
        + Debug
        + Display
        + for<'l> LeiterAnzeige<'l, S, Thema, Renderer>
        + Serialisiere<S>
        + BekannterLeiter
        + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::UmdrehenZeit: Serialize + for<'de> Deserialize<'de> + Send,
    <L as Leiter>::Fahrtrichtung:
        Debug + Clone + Serialize + for<'de> Deserialize<'de> + Unpin + Send,
    S: 'static + Debug + Display + Clone + Default + Eq + Hash + Unpin + Send,
    S: Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>
        + Serialize
        + for<'de> Deserialize<'de>,
    // zusätzlicher Constraint für v2-Kompatibilität
    L: BekannterZugtyp,
    S: From<<L as BekannterZugtyp>::V2>,
    for<'de> <L as BekannterZugtyp>::V2: Deserialize<'de>,
{
    type Executor = executor::Default;
    type Flags = Flags<L>;
    type Message = Nachricht<L, S>;
    type Theme = Thema;

    fn new((argumente, lager, zugtyp): Self::Flags) -> (Self, Command<Self::Message>) {
        let Argumente { pfad, modus, thema, zoom, x, y, winkel, i2c_settings, .. } = argumente;

        let lade_zustand: Command<Self::Message>;
        let initialer_pfad: String;
        if let Some(pfad) = pfad {
            lade_zustand = Nachricht::Laden(pfad.clone()).als_command();
            initialer_pfad = pfad.clone();
        } else {
            lade_zustand = Command::none();
            initialer_pfad = {
                let mut standard_pfad = zugtyp.name.clone();
                standard_pfad.push_str(".zug");
                standard_pfad
            };
        };

        let (sender, receiver) = channel();

        let gleise = Gleise::neu(
            zugtyp.clone(),
            modus.into(),
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
            thema: thema.into(),
            initialer_pfad,
            speichern_gefärbt: None,
            bewegung: None,
            auswahl_zustand: None,
            message_box: None,
            sender,
            empfänger: Empfänger::neu(receiver, ()),
        };

        (zugkontrolle, lade_zustand)
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", env!("zugkontrolle_version"))
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        let mut command = Command::none();

        match message {
            Nachricht::Gleis { definition_steuerung, klick_quelle, klick_höhe } => {
                self.gleis_hinzufügen(definition_steuerung, klick_quelle, klick_höhe);
            },
            Nachricht::Modus(modus) => self.gleise.moduswechsel(modus),
            Nachricht::Bewegen(bewegen::Nachricht::StarteBewegung(bewegung)) => {
                command = self.bewegung_starten(bewegung);
            },
            Nachricht::Bewegen(bewegen::Nachricht::BeendeBewegung) => self.bewegung_beenden(),
            Nachricht::Bewegen(bewegen::Nachricht::Zurücksetzen) => self.bewegung_zurücksetzen(),
            Nachricht::BewegungAusführen => {
                if let Some(cmd) = self.bewegung_ausführen() {
                    command = cmd;
                }
            },
            Nachricht::Position(position) => self.gleise.setze_pivot(position),
            Nachricht::Winkel(winkel) => self.gleise.setze_winkel(winkel),
            Nachricht::Skalieren(skalieren) => self.gleise.setze_skalierfaktor(skalieren),
            Nachricht::WähleStreckenabschnitt(aktuell) => self.streckenabschnitt_wählen(aktuell),
            Nachricht::HinzufügenStreckenabschnitt(
                geschwindigkeit,
                name,
                farbe,
                anschluss_definition,
            ) => self.streckenabschnitt_hinzufügen(
                geschwindigkeit,
                &name,
                farbe,
                anschluss_definition,
            ),
            Nachricht::LöscheStreckenabschnitt(streckenabschnitt_name) => {
                self.streckenabschnitt_löschen(&streckenabschnitt_name);
            },
            Nachricht::SetzeStreckenabschnitt(any_id) => {
                self.gleis_setzte_streckenabschnitt(any_id);
            },
            Nachricht::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_festlegen(festlegen);
            },
            Nachricht::Speichern(pfad) => {
                command = self.speichern(&pfad);
            },
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit);
            },
            Nachricht::Laden(pfad) => self.laden(&pfad),
            Nachricht::AktionGeschwindigkeit(aktion) => self.async_aktion_ausführen(
                aktion,
                Some(Nachricht::AsyncAktualisieren { gleise_neuzeichnen: false }),
            ),
            Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                self.geschwindigkeit_hinzufügen(name, geschwindigkeit_save);
            },
            Nachricht::LöscheGeschwindigkeit(name) => self.geschwindigkeit_entfernen(&name),
            Nachricht::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                self.anschlüsse_anpassen(anschlüsse_anpassen);
            },
            Nachricht::StreckenabschnittUmschalten(aktion) => self.aktion_ausführen(aktion),
            Nachricht::WeicheSchalten(aktion) => self.async_aktion_ausführen(aktion, None),
            Nachricht::GleiseZustandAktualisieren(nachricht) => {
                self.gleise_zustand_aktualisieren(nachricht);
            },
            Nachricht::Thema(thema) => self.setze_thema(thema),
            Nachricht::AuswahlFenster(auswahl_zustand) => {
                self.aktualisiere_auswahlzustand(auswahl_zustand);
            },
            Nachricht::MessageBox(message_box) => self.aktualisiere_message_box(message_box),
            Nachricht::AsyncAktualisieren { gleise_neuzeichnen } => {
                if gleise_neuzeichnen {
                    self.gleise_neuzeichnen();
                }
            },
        }

        command
    }

    fn view(&self) -> Element<'_, Self::Message, Thema, Renderer> {
        self.view_impl()
    }

    fn theme(&self) -> Self::Theme {
        self.thema
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::from_recipe(self.empfänger.clone())
    }
}
