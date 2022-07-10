//! Methoden für die [update](iced::Application::update)-Methode des [iced::Application]-Traits.

use std::{
    convert::identity,
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
    thread::sleep,
    time::{Duration, Instant},
};

use iced::{button, scrollable, Command};
use log::{debug, error};
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
        lizenzen, streckenabschnitt, weiche, AnschlüsseAnpassen, AnyGleisUnit, AuswahlZustand,
        MessageBox, Nachricht, Zugkontrolle,
    },
    gleis::gleise::{
        daten::{v2, DatenAuswahl, StreckenabschnittMap},
        id::{mit_any_id, AnyId, GleisId, StreckenabschnittId, StreckenabschnittIdRef},
        steuerung::MitSteuerung,
        AnschlüsseAnpassenFehler, Gleise,
    },
    steuerung::{
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::Ausführen,
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{farbe::Farbe, skalar::Skalar, vektor::Vektor},
    zugtyp::Zugtyp,
};

impl<L, S> Nachricht<L, S>
where
    L: 'static + LeiterAnzeige<S> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: Send,
{
    async fn nach_sleep(self, dauer: Duration) -> Self {
        sleep(dauer);
        self
    }

    fn als_sleep_command(self, dauer: Duration) -> Command<Nachricht<L, S>> {
        Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<L: LeiterAnzeige<S>, S> Zugkontrolle<L, S> {
    /// Zeige eine neue [MessageBox] mit Titel und Nachricht.
    ///
    /// Normalerweise für eine Fehlermeldung verwendet.
    pub fn zeige_message_box(&mut self, titel: String, nachricht: String) {
        self.message_box.zeige_modal(MessageBox {
            titel,
            nachricht,
            button_zustand: button::State::new(),
            scrollable_zustand: scrollable::State::new(),
        })
    }

    /// Schließe die [MessageBox].
    #[inline(always)]
    pub fn schließe_message_box(&mut self) {
        self.message_box.verstecke_modal()
    }

    /// Führe eine Aktion aus.
    pub fn aktion_ausführen<Aktion: Ausführen<L> + Debug>(&mut self, mut aktion: Aktion)
    where
        <Aktion as Ausführen<L>>::Fehler: Debug,
    {
        if let Err(fehler) = aktion.ausführen(self.gleise.zugtyp().into()) {
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
        S: Send,
    {
        let join_handle = aktion.async_ausführen(self.gleise.zugtyp().into(), self.sender.clone());
        if let Some(aktualisieren) = aktualisieren {
            let sender = self.sender.clone();
            let _join_handle = std::thread::spawn(move || {
                // Warte darauf, dass die Aktion beendet wurde
                let _panic = join_handle.join();
                // Initialisiere ein Update des Widgets.
                let _ = sender.send(aktualisieren);
            });
        }
    }

    #[allow(single_use_lifetimes)]
    fn zeige_anschlüsse_anpassen_aux<T, W, WS, Zustand>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        erzeuge_modal_zustand: impl Fn(Option<WS>) -> Zustand,
        erzeuge_modal: impl Fn(
            Zustand,
            Arc<dyn Fn(Option<WS>) -> Nachricht<L, S>>,
        ) -> AuswahlZustand<L, S>,
        als_nachricht: impl 'static + Fn(GleisId<T>, Option<WS>) -> AnschlüsseAnpassen,
    ) where
        T: 'static + for<'t> MitSteuerung<'t, Steuerung = Option<W>> + DatenAuswahl,
        W: Serialisiere<WS>,
    {
        let steuerung_res = self.gleise.erhalte_steuerung(&id);
        if let Ok(steuerung) = steuerung_res {
            let steuerung_save = steuerung.opt_as_ref().map(|steuerung| steuerung.serialisiere());
            self.auswahl.zeige_modal(erzeuge_modal(
                erzeuge_modal_zustand(steuerung_save),
                Arc::new(move |steuerung| {
                    Nachricht::AnschlüsseAnpassen(als_nachricht(id.klonen(), steuerung))
                }),
            ))
        } else {
            drop(steuerung_res);
            self.zeige_message_box(
                "Gleis entfernt!".to_owned(),
                format!("Anschlüsse {} anpassen für entferntes Gleis!", gleis_art),
            )
        }
    }

    /// Füge ein neues Gleis an der gewünschten Höhe hinzu.
    pub fn gleis_hinzufügen(&mut self, gleis: AnyGleisUnit, klick_höhe: Skalar) {
        let streckenabschnitt = self
            .streckenabschnitt_aktuell
            .aktuell()
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
                    self.streckenabschnitt_aktuell.entferne_aktuell();
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

    /// Schließe das Auswahl-Fenster.
    #[inline(always)]
    pub fn schließe_auswahl(&mut self) {
        self.auswahl.verstecke_modal()
    }

    /// Zeige das Auswahl-Fenster zum Anpassen der Anschlüsse für einen [Streckenabschnitt].
    pub fn zeige_auswahl_streckenabschnitt(&mut self) {
        self.auswahl.zeige_modal(AuswahlZustand::Streckenabschnitt(
            streckenabschnitt::AuswahlZustand::neu(&self.gleise),
        ))
    }

    /// Wähle den aktuellen [Streckenabschnitt].
    #[inline(always)]
    pub fn streckenabschnitt_wählen(
        &mut self,
        streckenabschnitt: Option<(StreckenabschnittId, Farbe)>,
    ) {
        *self.streckenabschnitt_aktuell.aktuell_mut() = streckenabschnitt
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
        match self.gleise.streckenabschnitt_mut(&StreckenabschnittId {
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
                self.zeige_message_box(
                    format!("Streckenabschnitt {:?} anpassen", id_ref),
                    fehlermeldung,
                );
                return;
            },
            _fehler => {},
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
                self.streckenabschnitt_aktuell.setze_aktuell(
                    StreckenabschnittId {
                        geschwindigkeit: geschwindigkeit.cloned(),
                        name: name.clone(),
                    },
                    farbe,
                );
                let streckenabschnitt = Streckenabschnitt::neu(farbe, streckenabschnitt);
                match self.auswahl.overlay_mut() {
                    Some(AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                        streckenabschnitt_auswahl.hinzufügen(&name, &streckenabschnitt);
                    },
                    modal => {
                        error!("Falscher Modal-State bei HinzufügenStreckenabschnitt: {:?}", modal);
                        self.auswahl.zeige_modal(AuswahlZustand::Streckenabschnitt(
                            streckenabschnitt::AuswahlZustand::neu(&self.gleise),
                        ))
                    },
                }
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
            .aktuell()
            .as_ref()
            .map_or(false, |(aktuell_id, _farbe)| *aktuell_id == streckenabschnitt_id)
        {
            self.streckenabschnitt_aktuell.entferne_aktuell()
        }

        match self.auswahl.overlay_mut() {
            Some(AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                streckenabschnitt_auswahl.entfernen(&streckenabschnitt_id.name);
            },
            modal => {
                error!("Falscher Modal-State bei LöscheStreckenabschnitt: {:?}", modal);
                self.auswahl.zeige_modal(AuswahlZustand::Streckenabschnitt(
                    streckenabschnitt::AuswahlZustand::neu(&self.gleise),
                ))
            },
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
                    .aktuell()
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
        if let Some(färbe_zeit) = self.speichern_gefärbt {
            if nachricht_zeit == färbe_zeit {
                self.speichern_laden.färbe_speichern(false);
                self.speichern_gefärbt = None;
            }
        }
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen(
        &mut self,
        anschlüsse_anpassen: AnschlüsseAnpassen,
    ) -> Option<Nachricht<L, S>> {
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
            None
        } else {
            Some(Nachricht::SchließeAuswahl)
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

    /// Zeige die Lizenzen der verwendeter Open-Source Bibliotheken.
    pub fn zeige_lizenzen(&mut self) {
        self.auswahl.zeige_modal(AuswahlZustand::ZeigeLizenzen(
            lizenzen::Zustand::neu_mit_verwendeten_lizenzen(),
        ))
    }
}

impl<L: LeiterAnzeige<S> + Display, S> Zugkontrolle<L, S> {
    /// Zeige das Auswahl-Fenster zum Einstellen einer
    /// [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    #[inline(always)]
    pub fn zeige_auswahl_geschwindigkeit(&mut self) {
        self.auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
            geschwindigkeit::AuswahlZustand::neu(self.gleise.geschwindigkeiten()),
        ))
    }

    /// Entferne eine [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    pub fn geschwindigkeit_entfernen(&mut self, name: geschwindigkeit::Name) {
        let name_clone = name.clone();
        if let Err(fehler) = self.gleise.geschwindigkeit_entfernen(name) {
            self.zeige_message_box("Geschwindigkeit entfernen".to_string(), format!("{:?}", fehler))
        }

        let _ = self.geschwindigkeiten.remove(&name_clone);

        match self.auswahl.overlay_mut() {
            Some(AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                geschwindigkeit_auswahl.entfernen(&name_clone);
            },
            modal => {
                error!("Falscher Modal-State bei LöscheGeschwindigkeit: {:?}", modal);
                self.auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
                    geschwindigkeit::AuswahlZustand::neu(self.gleise.geschwindigkeiten()),
                ))
            },
        }
    }
}

impl<L, S> Zugkontrolle<L, S>
where
    L: LeiterAnzeige<S> + Display,
    S: Debug + Clone + Reserviere<L, Arg = ()>,
{
    /// Füge eine  [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit) hinzu.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit_save: GeschwindigkeitSerialisiert<S>,
    ) {
        let Zugkontrolle { gleise, auswahl, geschwindigkeiten, .. } = self;
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
                    match auswahl.overlay_mut() {
                        Some(AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                            geschwindigkeit_auswahl.hinzufügen(&name, &geschwindigkeit)
                        },
                        modal => {
                            error!(
                                "Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}",
                                modal
                            );
                            self.auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
                                geschwindigkeit::AuswahlZustand::neu(
                                    self.gleise.geschwindigkeiten(),
                                ),
                            ))
                        },
                    }
                    let Zugtyp {
                        pwm_frequenz,
                        verhältnis_fahrspannung_überspannung,
                        stopp_zeit,
                        umdrehen_zeit,
                        ..
                    } = self.gleise.zugtyp();
                    let _ = geschwindigkeiten.insert(
                        name.clone(),
                        <Leiter as LeiterAnzeige>::anzeige_zustand_neu(
                            name.clone(),
                            *pwm_frequenz,
                            verhältnis_fahrspannung_überspannung.clone(),
                            *stopp_zeit,
                            umdrehen_zeit.clone(),
                        ),
                    );
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
                match auswahl.overlay_mut() {
                    Some(AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                        geschwindigkeit_auswahl.entfernen(&name)
                    },
                    modal => {
                        error!("Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}", modal);
                        auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
                            geschwindigkeit::AuswahlZustand::neu(gleise.geschwindigkeiten()),
                        ))
                    },
                }
                let _ = geschwindigkeiten.remove(&name);
                fehlermeldung.push_str(&format!(
                    "\nFehler beim Wiederherstellen: {:?}\nGeschwindigkeit {:?} entfernt.",
                    fehler, serialisiert_clone
                ));
            }
        }

        self.zeige_message_box(format!("Hinzufügen Geschwindigkeit {}", name.0), fehlermeldung);
    }
}

impl<L: LeiterAnzeige<S>, S> Zugkontrolle<L, S> {
    /// Behandle einen Fehler, der bei einer asynchronen Aktion aufgetreten ist.
    #[inline(always)]
    pub fn async_fehler(&mut self, titel: String, nachricht: String) {
        self.zeige_message_box(titel, nachricht);
    }
}

impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<S> + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: Send,
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

impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + Debug + LeiterAnzeige<S> + Send,
    <L as Leiter>::Fahrtrichtung: Debug + Send,
    S: Send,
{
    /// Zeige das Auswahl-Fenster zum Anpassen der Anschlüsse für ein Gleis.
    pub fn zeige_anschlüsse_anpassen(&mut self, any_id: AnyId) {
        match any_id {
            AnyId::Gerade(id) => {
                debug!("Anschlüsse für Gerade {:?} anpassen.", id)
            },
            AnyId::Kurve(id) => {
                debug!("Anschlüsse für Kurve {:?} anpassen.", id)
            },
            AnyId::Weiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "Weiche",
                id,
                weiche::Zustand::neu,
                AuswahlZustand::Weiche,
                AnschlüsseAnpassen::Weiche,
            ),
            AnyId::DreiwegeWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "DreiwegeWeiche",
                id,
                weiche::Zustand::neu,
                AuswahlZustand::DreiwegeWeiche,
                AnschlüsseAnpassen::DreiwegeWeiche,
            ),
            AnyId::KurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "KurvenWeiche",
                id,
                weiche::Zustand::neu,
                AuswahlZustand::KurvenWeiche,
                AnschlüsseAnpassen::KurvenWeiche,
            ),
            AnyId::SKurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "SKurvenWeiche",
                id,
                weiche::Zustand::neu,
                AuswahlZustand::Weiche,
                AnschlüsseAnpassen::SKurvenWeiche,
            ),
            AnyId::Kreuzung(id) => self.zeige_anschlüsse_anpassen_aux(
                "Kreuzung",
                id,
                weiche::Zustand::neu,
                AuswahlZustand::Weiche,
                AnschlüsseAnpassen::Kreuzung,
            ),
        }
    }
}

#[allow(single_use_lifetimes)]
impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + LeiterAnzeige<S> + BekannterLeiter + Send,
    S: Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
    <L as Leiter>::UmdrehenZeit: Serialize,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de> + Send,
{
    /// Speicher den aktuellen Zustand in einer Datei.
    pub fn speichern(&mut self, pfad: String) -> Option<Command<Nachricht<L, S>>> {
        let ergebnis = self.gleise.speichern(&pfad);
        match ergebnis {
            Ok(()) => {
                self.speichern_laden.färbe_speichern(true);
                let speicher_zeit = Instant::now();
                self.speichern_gefärbt = Some(speicher_zeit.clone());
                let command = Nachricht::EntferneSpeichernFarbe(speicher_zeit)
                    .als_sleep_command(Duration::from_secs(2));
                Some(command)
            },
            Err(err) => {
                self.zeige_message_box(
                    format!("Fehler beim Speichern in {}", pfad),
                    format!("{:?}", err),
                );
                None
            },
        }
    }
}

#[allow(single_use_lifetimes)]
impl<L: LeiterAnzeige<S> + BekannterLeiter, S> Zugkontrolle<L, S>
where
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
    <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
    <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
    S: Debug + Clone + Eq + Hash + Reserviere<L, Arg = ()>,
{
    /// Lade einen neuen Zustand aus einer Datei.
    pub fn laden(&mut self, pfad: String) {
        let lade_ergebnis = self.gleise.laden(&mut self.lager, &pfad);
        let Zugtyp {
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            ..
        } = self.gleise.zugtyp();
        self.geschwindigkeiten = self
            .gleise
            .geschwindigkeiten()
            .map(|(name, _geschwindigkeit)| {
                (
                    name.clone(),
                    L::anzeige_zustand_neu(
                        name.clone(),
                        *pwm_frequenz,
                        verhältnis_fahrspannung_überspannung.clone(),
                        *stopp_zeit,
                        umdrehen_zeit.clone(),
                    ),
                )
            })
            .collect();
        self.streckenabschnitt_aktuell.entferne_aktuell();
        if let Err(fehler) = lade_ergebnis {
            self.zeige_message_box(
                format!("Fehler beim Laden von {}", pfad),
                format!("{:?}", fehler),
            )
        }
    }
}
