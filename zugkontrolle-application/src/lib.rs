//! [`Application`] für die Gleis-Anzeige.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use std::{
    convert::identity,
    fmt::{Debug, Display},
    hash::Hash,
    sync::{
        Arc,
        mpsc::{Sender, channel},
    },
    time::Instant,
};

use flexi_logger::FlexiLoggerError;
use iced::{Element, Renderer, Settings, Size, Subscription, Task, window};
use iced_futures::subscription;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    InitFehler, Lager,
    de_serialisieren::{Reserviere, Serialisiere},
};
use zugkontrolle_argumente::{Argumente, I2cSettings};
use zugkontrolle_gleis::steuerung::{
    geschwindigkeit::{BekannterLeiter, Leiter},
    streckenabschnitt::Name as StreckenabschnittName,
};
use zugkontrolle_gleis::zugtyp::Zugtyp;
use zugkontrolle_gleise::{Gleise, daten::v2::geschwindigkeit::BekannterZugtyp};
use zugkontrolle_typen::{canvas::Position, farbe::Farbe, vektor::Vektor};
use zugkontrolle_widget::{
    auswahl::AuswahlZustand,
    bewegen::{self, Bewegen, Bewegung},
    drehen::Drehen,
    fonts,
    geschwindigkeit::LeiterAnzeige,
    speichern_laden,
    style::{self, thema::Thema},
};

use crate::{empfänger::Empfänger, icon::icon, nachricht::Nachricht};

#[path = "empfänger.rs"]
pub mod empfänger;
pub mod icon;
pub mod nachricht;
pub mod update;
pub mod view;

/// Anzeige einer Meldung.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageBox {
    /// Titel der [`MessageBox`].
    titel: String,
    /// Nachricht der [`MessageBox`].
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
    lager: Arc<RwLock<Lager>>,
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
    aktueller_pfad: String,
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

impl<L, S> Zugkontrolle<L, S>
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
    pub fn application(
        argumente: Argumente,
        lager: Arc<RwLock<Lager>>,
        zugtyp: &Zugtyp<L>,
    ) -> iced::Application<impl iced::Program<State = Self, Message = Nachricht<L, S>, Theme = Thema>>
    {
        iced::application(
            move || Zugkontrolle::new(argumente.clone(), lager.clone(), zugtyp.clone()),
            Zugkontrolle::update,
            Zugkontrolle::view,
        )
        .theme(Zugkontrolle::theme)
        .subscription(Zugkontrolle::subscription)
        .settings(Settings {
            default_font: fonts::REGULAR,
            fonts: fonts::benötigte_font_bytes(),
            ..Settings::default()
        })
        .centered()
        .window(window::Settings {
            size: Size { width: 800., height: 480. },
            icon: icon(),
            ..window::Settings::default()
        })
    }

    pub fn new(
        argumente: Argumente,
        lager: Arc<RwLock<Lager>>,
        zugtyp: Zugtyp<L>,
    ) -> (Self, Task<Nachricht<L, S>>) {
        let Argumente { pfad, modus, thema, zoom, x, y, winkel, i2c_settings, .. } = argumente;

        let lade_zustand: Task<Nachricht<L, S>>;
        let initialer_pfad: String;
        if let Some(pfad) = pfad {
            lade_zustand = Nachricht::Laden(pfad.clone()).als_task();
            initialer_pfad = pfad.clone();
        } else {
            lade_zustand = Task::none();
            initialer_pfad = {
                let mut standard_pfad = zugtyp.name.clone();
                standard_pfad.push_str(".zug");
                standard_pfad
            };
        }

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
            aktueller_pfad: initialer_pfad,
            speichern_gefärbt: None,
            bewegung: None,
            auswahl_zustand: None,
            message_box: None,
            sender,
            empfänger: Empfänger::neu(receiver, ()),
        };

        (zugkontrolle, lade_zustand)
    }

    pub fn title(&self) -> String {
        format!("Zugkontrolle {}", env!("zugkontrolle_version"))
    }

    pub fn update(&mut self, message: Nachricht<L, S>) -> Task<Nachricht<L, S>> {
        let mut command = Task::none();

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
            Nachricht::ZeigeDateiDialog(zeige_datei_dialog) => {
                command =
                    Task::perform(zeige_datei_dialog.0, identity).map(
                        |nachricht| match nachricht {
                            speichern_laden::Nachricht::Speichern(file_handle) => {
                                Nachricht::Speichern(
                                    file_handle.path().to_str().unwrap_or_default().to_owned(),
                                )
                            },
                            speichern_laden::Nachricht::Laden(file_handle) => Nachricht::Laden(
                                file_handle.path().to_str().unwrap_or_default().to_owned(),
                            ),
                            speichern_laden::Nachricht::Abgebrochen => {
                                Nachricht::AsyncAktualisieren { gleise_neuzeichnen: false }
                            },
                        },
                    );
            },
            Nachricht::Speichern(pfad) => {
                command = self.speichern(pfad);
            },
            Nachricht::EntferneSpeichernFarbe(nachricht_zeit) => {
                self.entferne_speichern_farbe(nachricht_zeit);
            },
            Nachricht::Laden(pfad) => self.laden(pfad),
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

    pub fn view(&self) -> Element<'_, Nachricht<L, S>, Thema, Renderer> {
        self.view_impl()
    }

    pub fn theme(&self) -> Thema {
        self.thema
    }

    pub fn subscription(&self) -> Subscription<Nachricht<L, S>> {
        subscription::from_recipe(self.empfänger.clone())
    }
}
