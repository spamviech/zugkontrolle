//! Methoden für die [update](iced::Application::update)-Methode des [iced::Application]-Traits.

use std::{
    convert::identity,
    fmt::{Debug, Display},
    hash::Hash,
    thread::{self, sleep},
    time::{Duration, Instant},
};

use iced::{Command, Renderer};
use log::error;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        polarität::Fließend,
        OutputSerialisiert,
    },
    application::{
        bewegen::Bewegung,
        geschwindigkeit::{self, LeiterAnzeige},
        streckenabschnitt,
        style::thema::Thema,
        AnschlüsseAnpassen, AnyGleisUnit, MessageBox, Nachricht, Zugkontrolle,
    },
    gleis::gleise::{
        self,
        daten::{v2::BekannterZugtyp, StreckenabschnittMap},
        id::{mit_any_id, AnyId, StreckenabschnittId, StreckenabschnittIdRef},
        AnschlüsseAnpassenFehler, Gleise,
    },
    steuerung::{
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::{Ausführen, Einstellungen},
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{farbe::Farbe, skalar::Skalar, vektor::Vektor},
};

impl<L, S> Nachricht<L, S>
where
    L: 'static + Leiter + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    async fn nach_sleep(self, dauer: Duration) -> Self {
        sleep(dauer);
        self
    }

    fn als_sleep_command(self, dauer: Duration) -> Command<Nachricht<L, S>> {
        Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Renderer<Thema>>, S> Zugkontrolle<L, S> {
    /// Zeige eine neue [MessageBox] mit Titel und Nachricht.
    ///
    /// Normalerweise für eine Fehlermeldung verwendet.
    pub fn zeige_message_box(&mut self, titel: String, nachricht: String) {
        self.message_box = Some(MessageBox { titel, nachricht });
    }

    /// Führe eine Aktion aus.
    pub fn aktion_ausführen<Aktion: Ausführen<L> + Debug>(&mut self, mut aktion: Aktion)
    where
        <Aktion as Ausführen<L>>::Fehler: Debug,
    {
        let einstellungen = Einstellungen::from(&*self.gleise.zugtyp());
        if let Err(fehler) = aktion.ausführen(einstellungen) {
            self.zeige_message_box(format!("{aktion:?}"), format!("{fehler:?}"))
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
        let join_handle = aktion
            .async_ausführen(Einstellungen::from(&*self.gleise.zugtyp()), self.sender.clone());
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

    /// Füge ein neues Gleis an der gewünschten Höhe hinzu.
    pub fn gleis_hinzufügen(&mut self, gleis: AnyGleisUnit, klick_höhe: Skalar) {
        let streckenabschnitt = self
            .streckenabschnitt_aktuell
            .as_ref()
            .map(|(streckenabschnitt_id, _farbe)| streckenabschnitt_id.klonen());
        macro_rules! hinzufügen_gehalten_bei_maus {
            ($gleis: expr) => {
                if let Err(fehler) = self.gleise.hinzufügen_gehalten_bei_maus(
                    $gleis.mit_none(),
                    Vektor { x: Skalar(0.), y: klick_höhe },
                    streckenabschnitt,
                    false,
                ) {
                    error!("Aktueller Streckenabschnitt entfernt: {:?}", fehler);
                    self.streckenabschnitt_aktuell = None;
                    let _ = self.gleise.hinzufügen_gehalten_bei_maus(
                        $gleis.mit_none(),
                        Vektor { x: Skalar(0.), y: klick_höhe },
                        None,
                        false,
                    );
                }
            };
        }
        match gleis {
            AnyGleisUnit::GeradeUnit(gerade) => hinzufügen_gehalten_bei_maus!(gerade),
            AnyGleisUnit::KurveUnit(kurve) => hinzufügen_gehalten_bei_maus!(kurve),
            AnyGleisUnit::WeicheUnit(weiche) => hinzufügen_gehalten_bei_maus!(weiche),
            AnyGleisUnit::DreiwegeWeicheUnit(dreiwege_weiche) => {
                hinzufügen_gehalten_bei_maus!(dreiwege_weiche)
            },
            AnyGleisUnit::KurvenWeicheUnit(kurven_weiche) => {
                hinzufügen_gehalten_bei_maus!(kurven_weiche)
            },
            AnyGleisUnit::SKurvenWeicheUnit(s_kurven_weiche) => {
                hinzufügen_gehalten_bei_maus!(s_kurven_weiche)
            },
            AnyGleisUnit::KreuzungUnit(kreuzung) => hinzufügen_gehalten_bei_maus!(kreuzung),
        }
    }

    /// Wähle den aktuellen [Streckenabschnitt].
    #[inline(always)]
    pub fn streckenabschnitt_wählen(
        &mut self,
        streckenabschnitt: Option<(StreckenabschnittId, Farbe)>,
    ) {
        self.streckenabschnitt_aktuell = streckenabschnitt
    }

    /// Füge einen neuen [Streckenabschnitt] hinzu.
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        farbe: Farbe,
        anschluss_definition: OutputSerialisiert,
    ) {
        let id_ref = StreckenabschnittIdRef { geschwindigkeit, name: &name };
        let message_opt = match self.gleise.streckenabschnitt_mut(&StreckenabschnittId {
            geschwindigkeit: geschwindigkeit.cloned(),
            name: name.clone(),
        }) {
            Ok(streckenabschnitt)
                if streckenabschnitt.lock_anschluss().serialisiere() == anschluss_definition =>
            {
                streckenabschnitt.farbe = farbe;
                let fehlermeldung = if let Err(fehler) = streckenabschnitt.strom(Fließend::Gesperrt)
                {
                    format!("{:?}", fehler)
                } else {
                    format!("Streckenabschnitt {:?} angepasst.", id_ref)
                };
                Some((format!("Streckenabschnitt {:?} anpassen", id_ref), fehlermeldung))
            },
            _fehler => None,
        };
        if let Some((titel, nachricht)) = message_opt {
            self.zeige_message_box(titel, nachricht);
            return;
        }
        // Streckenabschnitt hat nur einen Anschluss,
        // nachdem dieser unterschiedlich ist kann der aktuelle Anschluss ignoriert werden.
        // Implementierung über streckenabschnitt_mut (anstelle streckenabschnitt_entfernen)
        // vermeidet (unmöglichen) Fehlerfall mit nicht gefundener Geschwindigkeit
        // beim hinzufügen.
        use Ergebnis::*;
        let (streckenabschnitt, fehler) =
            match anschluss_definition.reserviere(&mut self.lager, Anschlüsse::default(), ()) {
                Wert { anschluss, .. } => (Some(anschluss), None),
                FehlerMitErsatzwert { anschluss, fehler, .. } => (Some(anschluss), Some(fehler)),
                Fehler { fehler, .. } => (None, Some(fehler)),
            };

        let mut fehlermeldung = fehler.map(|fehler| {
            (format!("Hinzufügen Streckenabschnitt {:?}", id_ref), format!("{:?}", fehler))
        });

        if let Some(streckenabschnitt) = streckenabschnitt {
            {
                self.streckenabschnitt_aktuell = Some((
                    StreckenabschnittId {
                        geschwindigkeit: geschwindigkeit.cloned(),
                        name: name.clone(),
                    },
                    farbe,
                ));
                let streckenabschnitt = Streckenabschnitt::neu(farbe, streckenabschnitt);

                if let Ok((id, Some(ersetzt))) = self.gleise.streckenabschnitt_hinzufügen(
                    geschwindigkeit,
                    name,
                    streckenabschnitt,
                ) {
                    let bisherige_nachricht = if let Some((_titel, mut nachricht)) = fehlermeldung {
                        nachricht.push('\n');
                        nachricht
                    } else {
                        String::new()
                    };
                    fehlermeldung = Some((
                        format!("Streckenabschnitt {:?} anpassen", id),
                        format!(
                            "{}Streckenabschnitt {:?} angepasst: {:?}",
                            bisherige_nachricht, id, ersetzt
                        ),
                    ));
                }
            }
        }

        if let Some((titel, nachricht)) = fehlermeldung {
            self.zeige_message_box(titel, nachricht)
        }
    }

    /// Lösche einen [Streckenabschnitt].
    pub fn streckenabschnitt_löschen(&mut self, streckenabschnitt_id: StreckenabschnittId) {
        if self
            .streckenabschnitt_aktuell
            .as_ref()
            .map_or(false, |(aktuell_id, _farbe)| *aktuell_id == streckenabschnitt_id)
        {
            self.streckenabschnitt_aktuell = None
        }

        let nicht_gefunden_nachricht = format!(
            "Streckenabschnitt {:?} sollte entfernt werden, aber wurde nicht gefunden!",
            streckenabschnitt_id
        );
        match self.gleise.streckenabschnitt_entfernen(streckenabschnitt_id) {
            Ok(None) => error!("{nicht_gefunden_nachricht}"),
            Ok(Some(_)) => {},
            Err(fehler) => self.zeige_message_box(
                "Fehler bei Streckenabschnitt löschen!".to_string(),
                format!("{:?}", fehler),
            ),
        }
    }

    /// Ändere den [Streckenabschnitt] für ein Gleis zum aktuellen Streckenabschnitt,
    /// falls es nicht mit [streckenabschnitt_festlegen](Zugkontrolle::streckenabschnitt_festlegen)
    /// deaktiviert wurde.
    pub fn gleis_setzte_streckenabschnitt(&mut self, mut any_id: AnyId) {
        if self.streckenabschnitt_aktuell_festlegen {
            if let Err(fehler) = mit_any_id!(
                &mut any_id,
                Gleise::setze_streckenabschnitt,
                &mut self.gleise,
                self.streckenabschnitt_aktuell
                    .as_ref()
                    .map(|(streckenabschnitt_id, _farbe)| streckenabschnitt_id.klonen())
            ) {
                self.zeige_message_box(
                    "Gleis entfernt".to_string(),
                    format!(
                        "Versuch den Streckenabschnitt für ein entferntes Gleis zu setzen: {:?}",
                        fehler
                    ),
                )
            }
        }
    }

    /// Einstellen ob anklicken eines Gleises dessen [Streckenabschnitt] zum
    /// aktuellen Streckenabschnitt ändern soll.
    #[inline(always)]
    pub fn streckenabschnitt_festlegen(&mut self, festlegen: bool) {
        self.streckenabschnitt_aktuell_festlegen = festlegen
    }

    /// Setze die Farbe des Speichern-Knopfes zurück.
    pub fn entferne_speichern_farbe(&mut self, nachricht_zeit: Instant) {
        if let Some((_gefärbt, färbe_zeit)) = self.speichern_gefärbt {
            if nachricht_zeit == färbe_zeit {
                self.speichern_gefärbt = None;
            }
        }
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen(&mut self, anschlüsse_anpassen: AnschlüsseAnpassen) {
        use AnschlüsseAnpassenFehler::*;
        let mut fehlermeldung = None;
        match self.gleise.anschlüsse_anpassen(&mut self.lager, anschlüsse_anpassen) {
            Ok(()) => {},
            Err(Deserialisieren { fehler, wiederherstellen_fehler }) => {
                let titel = "Anschlüsse anpassen!".to_owned();
                let mut nachricht = format!("{fehler:?}");
                if let Some((w_fehler, anschlüsse)) = wiederherstellen_fehler {
                    nachricht.push_str(&format!(
                        "\nFehler beim wiederherstellen der Anschlüsse: {w_fehler:?}\n{anschlüsse}"
                    ));
                }
                fehlermeldung = Some((titel, nachricht));
            },
            Err(GleisEntfernt(fehler)) => {
                fehlermeldung = Some((
                    "Gleis entfernt!".to_owned(),
                    format!("Anschlüsse anpassen für ein entferntes Gleis: {fehler:?}"),
                ));
            },
        }
        if let Some((titel, nachricht)) = fehlermeldung {
            self.zeige_message_box(titel, nachricht);
        }
    }

    /// Beende die Bewegung des Pivot-Punktes.
    #[inline(always)]
    pub fn bewegung_beenden(&mut self) {
        self.bewegung = None
    }

    /// Setze den Pivot-Punkt auf den (0,0) zurück.
    #[inline(always)]
    pub fn bewegung_zurücksetzen(&mut self) {
        self.gleise.setze_pivot(Vektor::null_vektor())
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Renderer<Thema>> + Display, S> Zugkontrolle<L, S> {
    /// Entferne eine [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    pub fn geschwindigkeit_entfernen(&mut self, name: geschwindigkeit::Name) {
        if let Err(fehler) = self.gleise.geschwindigkeit_entfernen(name) {
            self.zeige_message_box("Geschwindigkeit entfernen".to_string(), format!("{:?}", fehler))
        }
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: LeiterAnzeige<'t, S, Renderer<Thema>> + Serialisiere<S> + Display,
    S: Debug + Clone + Reserviere<L, Arg = ()>,
{
    /// Füge eine  [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit) hinzu.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit_save: GeschwindigkeitSerialisiert<S>,
    ) {
        let Zugkontrolle { gleise, .. } = self;
        let (alt_serialisiert_und_map, anschlüsse) =
            if let Some((geschwindigkeit, streckenabschnitt_map)) =
                gleise.geschwindigkeit_mit_streckenabschnitten_entfernen(&name)
            {
                let serialisiert = geschwindigkeit.serialisiere();
                let anschlüsse = geschwindigkeit.anschlüsse();
                (Some((serialisiert, streckenabschnitt_map)), anschlüsse)
            } else {
                (None, Anschlüsse::default())
            };
        use Ergebnis::*;
        let (fehler, anschlüsse) =
            match geschwindigkeit_save.reserviere(&mut self.lager, anschlüsse, ()) {
                Wert { anschluss: geschwindigkeit, .. } => {
                    let streckenabschnitt_map = if let Some((serialisiert, streckenabschnitt_map)) =
                        alt_serialisiert_und_map
                    {
                        self.zeige_message_box(
                            format!("Geschwindigkeit {} anpassen", name.0),
                            format!("Geschwindigkeit {} angepasst: {:?}", name.0, serialisiert),
                        );
                        streckenabschnitt_map
                    } else {
                        StreckenabschnittMap::new()
                    };
                    let _ = self.gleise.geschwindigkeit_mit_streckenabschnitten_hinzufügen(
                        name,
                        geschwindigkeit,
                        streckenabschnitt_map,
                    );
                    return;
                },
                FehlerMitErsatzwert { anschluss, fehler, mut anschlüsse } => {
                    anschlüsse.anhängen(anschluss.anschlüsse());
                    (fehler, anschlüsse)
                },
                Fehler { fehler, anschlüsse } => (fehler, anschlüsse),
            };

        let mut fehlermeldung = format!("Fehler beim Hinzufügen: {:?}", fehler);
        if let Some((serialisiert, streckenabschnitt_map)) = alt_serialisiert_und_map {
            let serialisiert_clone = serialisiert.clone();
            let (geschwindigkeit, fehler) =
                match serialisiert.reserviere(&mut self.lager, anschlüsse, ()) {
                    Wert { anschluss, .. } => (Some(anschluss), None),
                    FehlerMitErsatzwert { anschluss, fehler, .. } => {
                        (Some(anschluss), Some(fehler))
                    },
                    Fehler { fehler, .. } => (None, Some(fehler)),
                };
            if let Some(geschwindigkeit) = geschwindigkeit {
                // Modal/AnzeigeZustand-Map muss nicht angepasst werden,
                // nachdem nur wiederhergestellt wird
                let _ = gleise.geschwindigkeit_mit_streckenabschnitten_hinzufügen(
                    name.clone(),
                    geschwindigkeit,
                    streckenabschnitt_map,
                );
            }
            if let Some(fehler) = fehler {
                fehlermeldung.push_str(&format!(
                    "\nFehler beim Wiederherstellen: {:?}\nGeschwindigkeit {:?} entfernt.",
                    fehler, serialisiert_clone
                ));
            }
        }

        self.zeige_message_box(format!("Hinzufügen Geschwindigkeit {}", name.0), fehlermeldung);
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Renderer<Thema>>, S> Zugkontrolle<L, S> {
    /// Behandle einen Fehler, der bei einer asynchronen Aktion aufgetreten ist.
    #[inline(always)]
    pub fn async_fehler(&mut self, titel: String, nachricht: String) {
        self.zeige_message_box(titel, nachricht);
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Renderer<Thema>> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Beginne eine kontinuierliche Bewegung des Pivot-Punktes.
    #[inline(always)]
    pub fn bewegung_starten(&mut self, bewegung: Bewegung) -> Command<Nachricht<L, S>> {
        self.bewegung = Some(bewegung);
        Nachricht::BewegungAusführen.als_sleep_command(Duration::from_millis(20))
    }

    /// Tick für eine Bewegung des Pivot-Punktes.
    pub fn bewegung_ausführen(&mut self) -> Option<Command<Nachricht<L, S>>> {
        if let Some(bewegung) = self.bewegung {
            self.bewegung = Some(bewegung);
            self.gleise.bewege_pivot(
                bewegung
                    .vektor(Skalar(1.) / self.gleise.skalierfaktor())
                    .rotiert(-self.gleise.pivot().winkel),
            );
            Some(Nachricht::BewegungAusführen.als_sleep_command(Duration::from_millis(20)))
        } else {
            None
        }
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Renderer<Thema>>, S> Zugkontrolle<L, S> {
    /// Aktualisiere den Zustand des [Gleise]-Typs, ausgehend von Nachrichten aus seiner [update](Gleise::update)-Methode.
    pub fn gleise_zustand_aktualisieren(
        &mut self,
        nachricht: gleise::nachricht::ZustandAktualisieren,
    ) {
        if let Err(fehler) = self.gleise.zustand_aktualisieren(nachricht) {
            self.zeige_message_box(
                String::from("Interner Applikations-Fehler."),
                format!("{fehler:?}"),
            )
        }
    }
}

impl<'t, L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<'t, S, Renderer<Thema>> + BekannterLeiter + Serialisiere<S> + Send,
    S: 'static + Serialize + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
    <L as Leiter>::UmdrehenZeit: Serialize,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + Send,
{
    /// Speicher den aktuellen Zustand in einer Datei.
    pub fn speichern(&mut self, pfad: String) -> Command<Nachricht<L, S>> {
        let ergebnis = self.gleise.speichern(&pfad);
        let speicher_zeit = Instant::now();
        self.speichern_gefärbt = Some((ergebnis.is_ok(), speicher_zeit.clone()));
        if let Err(fehler) = ergebnis {
            self.zeige_message_box(
                format!("Fehler beim Speichern in '{pfad}'."),
                format!("{fehler:?}"),
            );
        }
        Nachricht::EntferneSpeichernFarbe(speicher_zeit).als_sleep_command(Duration::from_secs(2))
    }
}

impl<'t, L: LeiterAnzeige<'t, S, Renderer<Thema>>, S> Zugkontrolle<L, S> {
    /// Lade einen neuen Zustand aus einer Datei.
    #[allow(single_use_lifetimes)]
    pub fn laden(&mut self, pfad: String)
    where
        L: BekannterLeiter + Serialisiere<S>,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
        <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
        S: Debug + Clone + Eq + Hash + Reserviere<L, Arg = ()> + for<'de> Deserialize<'de>,
        // zusätzliche Constraints für v2-Kompatibilität
        L: BekannterZugtyp,
        S: From<<L as BekannterZugtyp>::V2>,
        <L as BekannterZugtyp>::V2: for<'de> Deserialize<'de>,
    {
        let lade_ergebnis = self.gleise.laden(&mut self.lager, &pfad);
        self.streckenabschnitt_aktuell = None;
        if let Err(fehler) = lade_ergebnis {
            self.zeige_message_box(
                format!("Fehler beim Laden von '{pfad}'."),
                format!("{fehler:?}"),
            )
        }
    }
}
