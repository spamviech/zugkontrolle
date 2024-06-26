//! Methoden für die [update](iced::Application::update)-Methode des [`iced::Application`]-Traits.

use std::{
    convert::identity,
    fmt::{Debug, Display, Write},
    hash::Hash,
    thread,
    time::{Duration, Instant},
};

use async_io::Timer;
use iced::{Command, Renderer};
use log::error;
use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    polarität::Fließend,
    OutputSerialisiert,
};
use zugkontrolle_gleis::{
    id::{AnyDefinitionIdSteuerung, AnyId, AnyIdSteuerungSerialisiert},
    steuerung::{
        geschwindigkeit::{self, BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::{Ausführen, Einstellungen},
        streckenabschnitt::{self, Streckenabschnitt},
    },
};
use zugkontrolle_gleise::{
    self,
    daten::{v2::geschwindigkeit::BekannterZugtyp, SteuerungAktualisierenFehler},
    nachricht::ZustandAktualisieren,
};
use zugkontrolle_typen::{farbe::Farbe, klick_quelle::KlickQuelle, skalar::Skalar, vektor::Vektor};
use zugkontrolle_widget::{
    auswahl::AuswahlZustand, bewegen::Bewegung, geschwindigkeit::LeiterAnzeige, style::thema::Thema,
};

use crate::{MessageBox, Nachricht, Zugkontrolle};

impl<L, S> Nachricht<L, S>
where
    L: 'static + Leiter + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Gebe die Nachricht zurück, nachdem mindestens `dauer` vergangen ist.
    async fn nach_sleep(self, dauer: Duration) -> Self {
        let _ = Timer::after(dauer).await;
        self
    }

    /// Erzeuge ein [`Command`], dass die Nachricht nach erst weitergibt, wenn mindestens `dauer` vergangen ist.
    fn als_sleep_command(self, dauer: Duration) -> Command<Nachricht<L, S>> {
        Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Thema, Renderer>, S> Zugkontrolle<L, S> {
    /// Aktualisiere die angezeigte [`MessageBox`].
    ///
    /// Normalerweise für eine Fehlermeldung verwendet.
    pub fn aktualisiere_message_box(&mut self, message_box: Option<MessageBox>) {
        self.message_box = message_box;
    }

    /// Aktualisiere das angezeigte [Auswahl-Fenster](AuswahlZustand), z.B. zum anpassen der Anschlüsse eines Gleises.
    pub fn aktualisiere_auswahlzustand(&mut self, auswahl_zustand: Option<AuswahlZustand<S>>) {
        self.auswahl_zustand = auswahl_zustand;
    }

    /// Führe eine Aktion aus.
    pub fn aktion_ausführen<Aktion: Ausführen<L> + Debug>(&mut self, mut aktion: Aktion)
    where
        <Aktion as Ausführen<L>>::Fehler: Debug,
    {
        let einstellungen = Einstellungen::from(self.gleise.zugtyp());
        if let Err(fehler) = aktion.ausführen(einstellungen) {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: format!("{aktion:?}"),
                nachricht: format!("{fehler:?}"),
            }));
        }
    }

    /// Führe eine Aktion asynchron aus, ohne auf das Ergebnis zu warten.
    pub fn async_aktion_ausführen<Aktion: Ausführen<L> + Debug + Send>(
        &mut self,
        mut aktion: Aktion,
        aktualisieren: Option<Nachricht<L, S>>,
    ) where
        L: 'static + Send,
        <L as Leiter>::Fahrtrichtung: Send,
        S: 'static + Send,
    {
        let join_handle =
            aktion.async_ausführen(Einstellungen::from(self.gleise.zugtyp()), self.sender.clone());
        if let Some(aktualisieren) = aktualisieren {
            let sender = self.sender.clone();
            let _join_handle = thread::spawn(move || {
                // Warte darauf, dass die Aktion beendet wurde
                let _panic = join_handle.join();
                // Initialisiere ein Update des Widgets.
                let _ = sender.send(aktualisieren);
            });
        }
    }

    /// Wähle den aktuellen [`Streckenabschnitt`].
    pub fn streckenabschnitt_wählen(
        &mut self,
        streckenabschnitt: Option<(streckenabschnitt::Name, Farbe)>,
    ) {
        self.streckenabschnitt_aktuell = streckenabschnitt;
    }

    /// Füge einen neuen [`Streckenabschnitt`] hinzu.
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        geschwindigkeit: Option<geschwindigkeit::Name>,
        name: &streckenabschnitt::Name,
        farbe: Farbe,
        anschluss_definition: OutputSerialisiert,
    ) {
        use Ergebnis::{Fehler, Wert, WertMitWarnungen};
        self.aktualisiere_auswahlzustand(Some(AuswahlZustand::Streckenabschnitt(Some((
            name.clone(),
            Streckenabschnitt::neu_serialisiert(farbe, anschluss_definition.clone()),
            geschwindigkeit.clone(),
        )))));
        // Implementierung über streckenabschnitt_mut (anstelle streckenabschnitt_entfernen)
        // vermeidet (unmöglichen) Fehlerfall mit doppeltem Namen beim hinzufügen.
        match self.gleise.streckenabschnitt_mut(name) {
            Ok(streckenabschnitt)
                if streckenabschnitt.lock_anschluss().serialisiere() == anschluss_definition =>
            {
                streckenabschnitt.setze_farbe(farbe);
                let titel = format!("Streckenabschnitt {} anpassen", name.0);
                let fehlermeldung = if let Err(fehler) = streckenabschnitt.strom(Fließend::Gesperrt)
                {
                    format!("{fehler:?}")
                } else {
                    format!("Streckenabschnitt {} angepasst.", name.0)
                };
                self.aktualisiere_message_box(Some(MessageBox { titel, nachricht: fehlermeldung }));
                return;
            },
            _fehler => {},
        };
        // Streckenabschnitt hat nur einen Anschluss.
        // Nachdem dieser unterschiedlich ist, kann der aktuelle Anschluss ignoriert werden.
        let (anschluss, fehler) = match anschluss_definition.reserviere(
            &mut self.lager,
            Anschlüsse::default(),
            (),
            &(),
            &mut (),
        ) {
            Wert { anschluss, .. } => (Some(anschluss), None),
            WertMitWarnungen { anschluss, fehler, .. } => (Some(anschluss), Some(fehler)),
            Fehler { fehler, .. } => (None, Some(fehler)),
        };

        // false-positive: fehler related über map
        #[allow(clippy::shadow_unrelated)]
        let mut fehlermeldung = fehler.map(|fehler| {
            (format!("Hinzufügen Streckenabschnitt {}", name.0), format!("{fehler:?}"))
        });

        if let Some(anschluss) = anschluss {
            {
                self.streckenabschnitt_aktuell = Some((name.clone(), farbe));
                let streckenabschnitt = Streckenabschnitt::neu(farbe, anschluss);

                if let Some(ersetzt) = self.gleise.streckenabschnitt_hinzufügen(
                    name.clone(),
                    streckenabschnitt,
                    geschwindigkeit,
                ) {
                    let bisherige_nachricht = if let Some((_titel, mut nachricht)) = fehlermeldung {
                        nachricht.push('\n');
                        nachricht
                    } else {
                        String::new()
                    };
                    fehlermeldung = Some((
                        format!("Streckenabschnitt {name:?} anpassen"),
                        format!("{bisherige_nachricht}Streckenabschnitt {name:?} angepasst: {ersetzt:?}"),
                    ));
                }
            }
        }

        if let Some((titel, nachricht)) = fehlermeldung {
            self.aktualisiere_message_box(Some(MessageBox { titel, nachricht }));
        }
    }

    /// Lösche einen [`Streckenabschnitt`].
    pub fn streckenabschnitt_löschen(&mut self, name: &streckenabschnitt::Name) {
        if self
            .streckenabschnitt_aktuell
            .as_ref()
            .map_or(false, |(aktuell_name, _farbe)| aktuell_name == name)
        {
            self.streckenabschnitt_aktuell = None;
        }

        match self.gleise.streckenabschnitt_entfernen(name) {
            Ok(_) => {},
            Err(name) => error!(
                "Streckenabschnitt {} sollte entfernt werden, aber wurde nicht gefunden!",
                name.0
            ),
        }
    }

    /// Ändere den [`Streckenabschnitt`] für ein Gleis zum aktuellen Streckenabschnitt,
    /// falls es nicht mit [`streckenabschnitt_festlegen`](Zugkontrolle::streckenabschnitt_festlegen)
    /// deaktiviert wurde.
    pub fn gleis_setzte_streckenabschnitt(&mut self, any_id: AnyId) {
        if self.streckenabschnitt_aktuell_festlegen {
            if let Err(fehler) = self.gleise.setze_streckenabschnitt(
                any_id,
                self.streckenabschnitt_aktuell
                    .as_ref()
                    .map(|(streckenabschnitt_name, _farbe)| streckenabschnitt_name.clone()),
            ) {
                self.aktualisiere_message_box(Some(MessageBox {titel:                    String::from("Gleis entfernt"),nachricht:                    format!("Versuch den Streckenabschnitt für ein entferntes Gleis zu setzen: {fehler:?}"),}));
            }
        }
    }

    /// Einstellen ob anklicken eines Gleises dessen [`Streckenabschnitt`] zum
    /// aktuellen Streckenabschnitt ändern soll.
    pub fn streckenabschnitt_festlegen(&mut self, festlegen: bool) {
        self.streckenabschnitt_aktuell_festlegen = festlegen;
    }

    /// Setze die Farbe des Speichern-Knopfes zurück.
    pub fn entferne_speichern_farbe(&mut self, nachricht_zeit: Instant) {
        if let Some((_gefärbt, färbe_zeit)) = self.speichern_gefärbt {
            if nachricht_zeit == färbe_zeit {
                self.speichern_gefärbt = None;
            }
        }
    }

    /// Beende die Bewegung des Pivot-Punktes.
    pub fn bewegung_beenden(&mut self) {
        self.bewegung = None;
    }

    /// Setze den Pivot-Punkt auf den (0,0) zurück.
    pub fn bewegung_zurücksetzen(&mut self) {
        self.gleise.setze_pivot(Vektor::null_vektor());
    }

    /// Erzwinge ein neuzeichnen des Canvas.
    pub fn gleise_neuzeichnen(&mut self) {
        self.gleise.erzwinge_neuzeichnen();
    }

    /// Setze das aktuelle Anzeige-[`Thema`].
    pub fn setze_thema(&mut self, thema: Thema) {
        self.thema = thema;
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Thema, Renderer> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Füge ein neues Gleis an der gewünschten Höhe hinzu.
    pub fn gleis_hinzufügen(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung,
        klick_quelle: KlickQuelle,
        klick_höhe: Skalar,
    ) {
        let streckenabschnitt = self
            .streckenabschnitt_aktuell
            .as_ref()
            .map(|(streckenabschnitt_name, _farbe)| streckenabschnitt_name.clone());
        let winkel = self.gleise.winkel();
        if let Err(fehler) = self.gleise.hinzufügen_gehalten_bei_maus(
            definition_steuerung,
            klick_quelle,
            Vektor { x: Skalar(0.), y: klick_höhe }.rotiert(&winkel),
            streckenabschnitt,
            false,
        ) {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: String::from("Fehler beim Gleis hinzufügen!"),
                nachricht: format!("{fehler:?}"),
            }));
        }
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen(&mut self, anschlüsse_anpassen: AnyIdSteuerungSerialisiert) {
        use SteuerungAktualisierenFehler::{GleisNichtGefunden, Reservieren};
        let mut fehlermeldung = None;
        let mut auswahlzustand_verstecken = false;
        match self.gleise.anschlüsse_anpassen(&mut self.lager, anschlüsse_anpassen) {
            Ok(()) => {
                auswahlzustand_verstecken = true;
            },
            Err(aktualisieren_fehler) => match *aktualisieren_fehler {
                Reservieren { fehler: NonEmpty { head, tail }, wiederherstellen_fehler } => {
                    let titel = "Anschlüsse anpassen!".to_owned();
                    let mut nachricht = format!("{head}\n");
                    for fehler in tail {
                        writeln!(nachricht, "- {fehler}")
                            .expect("write! auf einem String fehlgeschlagen!");
                    }
                    if let Some((w_fehler, anschlüsse)) = wiederherstellen_fehler {
                        writeln!(nachricht, "\nFehler beim wiederherstellen der Anschlüsse:")
                            .expect("write! auf einem String fehlgeschlagen!");
                        for fehler in w_fehler {
                            writeln!(nachricht, "- {fehler}")
                                .expect("write! auf einem String fehlgeschlagen!");
                        }
                        write!(nachricht, "{anschlüsse}")
                            .expect("write! auf einem String fehlgeschlagen!");
                    }
                    fehlermeldung = Some((titel, nachricht));
                },
                GleisNichtGefunden(id) => {
                    fehlermeldung = Some((
                        "Gleis entfernt!".to_owned(),
                        format!("Anschlüsse anpassen für ein entferntes Gleis: {id:?}"),
                    ));
                },
                SteuerungAktualisierenFehler::ReservierenWarnung(NonEmpty { head, tail }) => {
                    auswahlzustand_verstecken = true;
                    let titel = "Anschlüsse anpassen!".to_owned();
                    let mut nachricht = format!("{head}\n");
                    for fehler in tail {
                        writeln!(nachricht, "- {fehler}")
                            .expect("write! auf einem String fehlgeschlagen!");
                    }
                    fehlermeldung = Some((titel, nachricht));
                },
            },
        }
        if let Some((titel, nachricht)) = fehlermeldung {
            self.aktualisiere_message_box(Some(MessageBox { titel, nachricht }));
        }
        if auswahlzustand_verstecken {
            self.aktualisiere_auswahlzustand(None);
        }
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Thema, Renderer> + Display, S> Zugkontrolle<L, S> {
    /// Entferne eine [`Geschwindigkeit`](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    pub fn geschwindigkeit_entfernen(&mut self, name: &geschwindigkeit::Name) {
        if let Err(fehler) = self.gleise.geschwindigkeit_entfernen(name) {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: String::from("Geschwindigkeit entfernen"),
                nachricht: format!("{fehler:?}"),
            }));
        }
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: Display + LeiterAnzeige<'t, S, Thema, Renderer> + Serialisiere<S>,
    S: Display + Clone + Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>,
{
    /// Füge eine  [`Geschwindigkeit`](crate::steuerung::geschwindigkeit::Geschwindigkeit) hinzu.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: GeschwindigkeitSerialisiert<S>,
    ) {
        use Ergebnis::{Fehler, Wert, WertMitWarnungen};
        self.aktualisiere_auswahlzustand(Some(AuswahlZustand::Geschwindigkeit(Some((
            name.clone(),
            geschwindigkeit.clone(),
        )))));
        let (alt_serialisiert, anschlüsse) =
            if let Ok(alte_geschwindigkeit) = self.gleise.geschwindigkeit_entfernen(&name) {
                let serialisiert = alte_geschwindigkeit.serialisiere();
                let anschlüsse = alte_geschwindigkeit.anschlüsse();
                (Some(serialisiert), anschlüsse)
            } else {
                (None, Anschlüsse::default())
            };
        // anschlüsse, geschwindigkeit related über `reserviere`
        #[allow(clippy::shadow_unrelated)]
        let (fehler, anschlüsse) =
            match geschwindigkeit.reserviere(&mut self.lager, anschlüsse, (), &(), &mut ()) {
                Wert { anschluss: geschwindigkeit, .. } => {
                    if let Some(serialisiert) = alt_serialisiert {
                        self.aktualisiere_message_box(Some(MessageBox {
                            titel: format!("Geschwindigkeit {} anpassen", name.0),
                            nachricht: format!(
                                "Geschwindigkeit {} angepasst: {}",
                                name.0, serialisiert
                            ),
                        }));
                    };
                    if let Some(bisher) =
                        self.gleise.geschwindigkeit_hinzufügen(name, geschwindigkeit)
                    {
                        error!("Geschwindigkeit {bisher} beim wiederherstellen überschrieben!");
                    }
                    return;
                },
                WertMitWarnungen { anschluss, fehler, mut anschlüsse } => {
                    anschlüsse.anhängen(anschluss.anschlüsse());
                    (fehler, anschlüsse)
                },
                Fehler { fehler, anschlüsse } => (fehler, anschlüsse),
            };

        let mut fehlermeldung = format!("Fehler beim Hinzufügen: {fehler:?}");
        if let Some(serialisiert) = alt_serialisiert {
            let serialisiert_clone = serialisiert.clone();
            let (ursprüngliche_geschwindigkeit, fehler_wiederherstellen) =
                match serialisiert.reserviere(&mut self.lager, anschlüsse, (), &(), &mut ()) {
                    Wert { anschluss, .. } => (Some(anschluss), None),
                    WertMitWarnungen { anschluss, fehler: fehler_wiederherstellen, .. } => {
                        (Some(anschluss), Some(fehler_wiederherstellen))
                    },
                    Fehler { fehler: fehler_wiederherstellen, .. } => {
                        (None, Some(fehler_wiederherstellen))
                    },
                };
            if let Some(ursprüngliche_geschwindigkeit) = ursprüngliche_geschwindigkeit {
                // Modal/AnzeigeZustand-Map muss nicht angepasst werden,
                // nachdem nur wiederhergestellt wird
                let _ = self
                    .gleise
                    .geschwindigkeit_hinzufügen(name.clone(), ursprüngliche_geschwindigkeit);
            }
            if let Some(w_fehler_liste) = fehler_wiederherstellen {
                writeln!(fehlermeldung, "\nFehler beim Wiederherstellen:")
                    .expect("write! auf einem String fehlgeschlagen!");
                for w_fehler in w_fehler_liste {
                    writeln!(fehlermeldung, "- {w_fehler}")
                        .expect("write! auf einem String fehlgeschlagen!");
                }
                write!(fehlermeldung, "Geschwindigkeit {serialisiert_clone} entfernt.")
                    .expect("write! auf einem String fehlgeschlagen!");
            }
        }

        self.aktualisiere_message_box(Some(MessageBox {
            titel: format!("Hinzufügen Geschwindigkeit {}", name.0),
            nachricht: fehlermeldung,
        }));
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Thema, Renderer> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Beginne eine kontinuierliche Bewegung des Pivot-Punktes.
    pub fn bewegung_starten(&mut self, bewegung: Bewegung) -> Command<Nachricht<L, S>> {
        self.bewegung = Some(bewegung);
        Nachricht::BewegungAusführen.als_sleep_command(Duration::from_millis(20))
    }

    /// Tick für eine Bewegung des Pivot-Punktes.
    pub fn bewegung_ausführen(&mut self) -> Option<Command<Nachricht<L, S>>> {
        if let Some(bewegung) = self.bewegung {
            self.bewegung = Some(bewegung);
            self.gleise.bewege_pivot(
                // Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt
                #[allow(clippy::arithmetic_side_effects)]
                bewegung
                    .vektor(Skalar(1.) / self.gleise.skalierfaktor())
                    .rotiert(&(-self.gleise.pivot().winkel)),
            );
            Some(Nachricht::BewegungAusführen.als_sleep_command(Duration::from_millis(20)))
        } else {
            None
        }
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Thema, Renderer>, S> Zugkontrolle<L, S> {
    /// Aktualisiere den Zustand des [Gleise]-Typs, ausgehend von Nachrichten aus seiner [`update`](Gleise::update)-Methode.
    pub fn gleise_zustand_aktualisieren(&mut self, nachricht: ZustandAktualisieren) {
        if let Err(fehler) = self.gleise.zustand_aktualisieren(nachricht) {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: String::from("Interner Applikations-Fehler."),
                nachricht: format!("{fehler:?}"),
            }));
        }
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Thema, Renderer> + BekannterLeiter + Serialisiere<S> + Send,
    S: 'static + Serialize + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
    <L as Leiter>::UmdrehenZeit: Serialize,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + Send,
{
    /// Speicher den aktuellen Zustand in einer Datei.
    pub fn speichern(&mut self, pfad: String) -> Command<Nachricht<L, S>> {
        let ergebnis = self.gleise.speichern(&pfad);
        let speicher_zeit = Instant::now();
        self.speichern_gefärbt = Some((ergebnis.is_ok(), speicher_zeit));
        if let Err(fehler) = ergebnis {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: format!("Fehler beim Speichern in '{pfad}'."),
                nachricht: format!("{fehler:?}"),
            }));
        }
        self.aktueller_pfad = pfad;
        Nachricht::EntferneSpeichernFarbe(speicher_zeit).als_sleep_command(Duration::from_secs(2))
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Thema, Renderer> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Lade einen neuen Zustand aus einer Datei.
    pub fn laden(&mut self, pfad: String)
    where
        L: BekannterLeiter + Serialisiere<S>,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
        <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
        S: Debug
            + Clone
            + Eq
            + Hash
            + Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>
            + for<'de> Deserialize<'de>,
        // zusätzliche Constraints für v2-Kompatibilität
        L: BekannterZugtyp,
        S: From<<L as BekannterZugtyp>::V2>,
        <L as BekannterZugtyp>::V2: for<'de> Deserialize<'de>,
    {
        let lade_ergebnis = self.gleise.laden(&mut self.lager, &pfad);
        self.streckenabschnitt_aktuell = None;
        if let Err(fehler) = lade_ergebnis {
            self.aktualisiere_message_box(Some(MessageBox {
                titel: format!("Fehler beim Laden von '{pfad}'."),
                nachricht: format!("{fehler:?}"),
            }));
        }
        self.aktueller_pfad = pfad;
    }
}
