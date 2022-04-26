//! Methoden für die [update](iced::Application::update)-Methode des [iced::Application]-Traits.

use std::{
    convert::identity,
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
    thread::sleep,
    time::{Duration, Instant},
};

use iced::{button, Command};
use log::{debug, error};
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        polarität::Fließend,
        OutputSerialisiert,
    },
    application::{
        bewegen::Bewegung,
        geschwindigkeit::{self, LeiterAnzeige},
        steuerung::{MitSteuerung, Steuerung},
        streckenabschnitt, weiche, AnschlüsseAnpassen, AnyGleisUnit, AuswahlZustand, MessageBox,
        Nachricht, Zugkontrolle,
    },
    gleis::gleise::{
        daten::{v2, DatenAuswahl, StreckenabschnittMap},
        id::{mit_any_id, AnyId, GleisId, StreckenabschnittId, StreckenabschnittIdRef},
        Gleise,
    },
    steuerung::{
        self,
        geschwindigkeit::{BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        plan::Ausführen,
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{farbe::Farbe, skalar::Skalar, vektor::Vektor},
    zugtyp::Zugtyp,
};

impl<L> Nachricht<L>
where
    L: 'static + LeiterAnzeige + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    <L as Serialisiere>::Serialisiert: Send,
{
    async fn nach_sleep(self, dauer: Duration) -> Self {
        sleep(dauer);
        self
    }

    fn als_sleep_command(self, dauer: Duration) -> Command<Nachricht<L>> {
        Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<L: LeiterAnzeige> Zugkontrolle<L> {
    /// Zeige eine neue [MessageBox] mit Titel und Nachricht.
    ///
    /// Normalerweise für eine Fehlermeldung verwendet.
    pub fn zeige_message_box(&mut self, titel: String, nachricht: String) {
        self.message_box.zeige_modal(MessageBox {
            titel,
            nachricht,
            button_zustand: button::State::new(),
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
        aktualisieren: Option<Nachricht<L>>,
    ) where
        L: 'static + Send,
        <L as Leiter>::Fahrtrichtung: Send,
        <L as Serialisiere>::Serialisiert: Send,
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
    fn gleis_anschlüsse_anpassen<T, W>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        anschlüsse_serialisiert: Option<<W as Serialisiere>::Serialisiert>,
    ) -> Option<Nachricht<L>>
    where
        T: for<'t> MitSteuerung<'t, Steuerung = Option<W>> + DatenAuswahl,
        W: Serialisiere,
        <W as Serialisiere>::Serialisiert: Debug + Clone,
    {
        let mut message = None;

        let mut error_message = None;
        if let Ok(mut steuerung) = self.gleise.erhalte_steuerung_mut(&id) {
            if let Some(anschlüsse_serialisiert) = anschlüsse_serialisiert {
                let (steuerung_save, (pwm_pins, output_anschlüsse, input_anschlüsse)) =
                    if let Some(s) = steuerung.take() {
                        (Some(s.serialisiere()), s.anschlüsse())
                    } else {
                        (None, (Vec::new(), Vec::new(), Vec::new()))
                    };
                match anschlüsse_serialisiert.reserviere(
                    &mut self.lager,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                ) {
                    Ok(Reserviert { anschluss, .. }) => {
                        let _ = steuerung.insert(anschluss);
                        message = Some(Nachricht::SchließeAuswahl)
                    },
                    Err(de_serialisieren::Fehler {
                        fehler,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    }) => {
                        let mut fehlermeldung = format!("{:?}", fehler);
                        if let Some(steuerung_save) = steuerung_save {
                            let save_clone = steuerung_save.clone();
                            match steuerung_save.reserviere(
                                &mut self.lager,
                                pwm_pins,
                                output_anschlüsse,
                                input_anschlüsse,
                            ) {
                                Ok(Reserviert { anschluss, .. }) => {
                                    let _ = steuerung.insert(anschluss);
                                },
                                Err(error) => {
                                    fehlermeldung.push_str(&format!(
                                    "\nFehler beim Wiederherstellen: {:?}\nSteuerung {:?} entfernt.",
                                    error, save_clone
                                ));
                                },
                            }
                        }
                        let _ = error_message
                            .insert(("Anschlüsse anpassen".to_string(), fehlermeldung));
                    },
                }
            } else {
                let _ = steuerung.take();
                message = Some(Nachricht::SchließeAuswahl);
            }
        } else {
            let _ = error_message.insert((
                "Gleis entfernt!".to_string(),
                format!("Anschlüsse {} anpassen für entferntes Gleis!", gleis_art),
            ));
        }
        if let Some((titel, nachricht)) = error_message {
            self.zeige_message_box(titel, nachricht)
        }

        message
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
                )
            },
            _fehler => {
                // Streckenabschnitt hat nur einen Anschluss,
                // nachdem dieser unterschiedlich ist kann der aktuelle Anschluss ignoriert werden.
                // Implementierung über streckenabschnitt_mut (anstelle streckenabschnitt_entfernen)
                // vermeidet (unmöglichen) Fehlerfall mit nicht gefundener Geschwindigkeit
                // beim hinzufügen.
                match anschluss_definition.reserviere(
                    &mut self.lager,
                    Vec::new(),
                    Vec::new(),
                    Vec::new(),
                ) {
                    Ok(Reserviert { anschluss, .. }) => {
                        self.streckenabschnitt_aktuell.setze_aktuell(
                            StreckenabschnittId {
                                geschwindigkeit: geschwindigkeit.cloned(),
                                name: name.clone(),
                            },
                            farbe,
                        );
                        let streckenabschnitt = Streckenabschnitt::neu(farbe, anschluss);
                        match self.auswahl.overlay_mut() {
                            Some(AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                                streckenabschnitt_auswahl.hinzufügen(&name, &streckenabschnitt);
                            },
                            modal => {
                                error!(
                                    "Falscher Modal-State bei HinzufügenStreckenabschnitt: {:?}",
                                    modal
                                );
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
                            self.zeige_message_box(
                                format!("Streckenabschnitt {:?} anpassen", id),
                                format!("Streckenabschnitt {:?} angepasst: {:?}", id, ersetzt),
                            )
                        }
                    },
                    Err(fehler) => self.zeige_message_box(
                        format!("Hinzufügen Streckenabschnitt {:?}", id_ref),
                        format!("Fehler beim Hinzufügen: {:?}", fehler),
                    ),
                }
            },
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

        let nicht_gefunden_nachricht = format!(
            "Streckenabschnitt {:?} sollte entfernt werden, aber wurde nicht gefunden!",
            streckenabschnitt_id
        );
        let name_clone = streckenabschnitt_id.name.clone();
        match self.gleise.streckenabschnitt_entfernen(streckenabschnitt_id) {
            Ok(None) => error!("{}", nicht_gefunden_nachricht),
            Ok(Some(_)) => {},
            Err(fehler) => self.zeige_message_box(
                "Fehler bei Streckenabschnitt löschen!".to_string(),
                format!("{:?}", fehler),
            ),
        }

        match self.auswahl.overlay_mut() {
            Some(AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                streckenabschnitt_auswahl.entfernen(&name_clone);
            },
            modal => {
                error!("Falscher Modal-State bei LöscheStreckenabschnitt: {:?}", modal);
                self.auswahl.zeige_modal(AuswahlZustand::Streckenabschnitt(
                    streckenabschnitt::AuswahlZustand::neu(&self.gleise),
                ))
            },
        }
    }

    /// Ändere den [Streckenabschnitt] für das gehaltene Gleis zum aktuellen Streckenabschnitt,
    /// falls es nicht mit [streckenabschnitt_festlegen](Zugkontrolle::streckenabschnitt_festlegen)
    /// deaktiviert wurde.
    pub fn setzte_streckenabschnitt_gehalten(&mut self) {
        if self.streckenabschnitt_aktuell_festlegen {
            let aktuell =
                self.streckenabschnitt_aktuell.aktuell().as_ref().map(|(id, _farbe)| id.klonen());
            self.gleise.setzte_streckenabschnitt_gehalten(aktuell)
        } else {
            self.gleise.gehalten_hinzufügen()
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
    ) -> Option<Nachricht<L>> {
        todo!()
        // match anschlüsse_anpassen {
        //     AnschlüsseAnpassen::Weiche(id, anschlüsse_serialisiert) => {
        //         self.gleis_anschlüsse_anpassen("Weiche", id, anschlüsse_serialisiert)
        //     },
        //     AnschlüsseAnpassen::DreiwegeWeiche(id, anschlüsse_serialisiert) => {
        //         self.gleis_anschlüsse_anpassen("DreiwegeWeiche", id, anschlüsse_serialisiert)
        //     },
        //     AnschlüsseAnpassen::KurvenWeiche(id, anschlüsse_serialisiert) => {
        //         self.gleis_anschlüsse_anpassen("KurvenWeiche", id, anschlüsse_serialisiert)
        //     },
        //     AnschlüsseAnpassen::SKurvenWeiche(id, anschlüsse_serialisiert) => {
        //         self.gleis_anschlüsse_anpassen("SKurvenWeiche", id, anschlüsse_serialisiert)
        //     },
        //     AnschlüsseAnpassen::Kreuzung(id, anschlüsse_serialisiert) => {
        //         self.gleis_anschlüsse_anpassen("Kreuzung", id, anschlüsse_serialisiert)
        //     },
        // }
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

impl<Leiter: LeiterAnzeige + Display> Zugkontrolle<Leiter> {
    /// Zeige das Auswahl-Fenster zum Einstellen einer
    /// [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
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

impl<Leiter> Zugkontrolle<Leiter>
where
    Leiter: LeiterAnzeige + Display,
    <Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    /// Füge eine  [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit) hinzu.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit_save: GeschwindigkeitSerialisiert<Leiter>,
    ) {
        let Zugkontrolle { gleise, auswahl, geschwindigkeiten, .. } = self;
        let (alt_serialisiert_und_map, (pwm_pins, output_anschlüsse, input_anschlüsse)) =
            if let Some((geschwindigkeit, streckenabschnitt_map)) =
                gleise.geschwindigkeit_mit_streckenabschnitten_entfernen(&name)
            {
                let serialisiert = geschwindigkeit.serialisiere();
                let anschlüsse = geschwindigkeit.anschlüsse();
                (Some((serialisiert, streckenabschnitt_map)), anschlüsse)
            } else {
                (None, (Vec::new(), Vec::new(), Vec::new()))
            };
        match geschwindigkeit_save.reserviere(
            &mut self.lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        ) {
            Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                match auswahl.overlay_mut() {
                    Some(AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                        geschwindigkeit_auswahl.hinzufügen(&name, &geschwindigkeit)
                    },
                    modal => {
                        error!("Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}", modal);
                        self.auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
                            geschwindigkeit::AuswahlZustand::neu(self.gleise.geschwindigkeiten()),
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
                let streckenabschnitt_map =
                    if let Some((serialisiert, streckenabschnitt_map)) = alt_serialisiert_und_map {
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
            },
            Err(de_serialisieren::Fehler {
                fehler,
                pwm_pins,
                output_anschlüsse,
                input_anschlüsse,
            }) => {
                let mut fehlermeldung = format!("Fehler beim Hinzufügen: {:?}", fehler);
                if let Some((serialisiert, streckenabschnitt_map)) = alt_serialisiert_und_map {
                    let serialisiert_clone = serialisiert.clone();
                    match serialisiert.reserviere(
                        &mut self.lager,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    ) {
                        Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                            // Modal/AnzeigeZustand-Map muss nicht angepasst werden,
                            // nachdem nur wiederhergestellt wird
                            let _ = gleise.geschwindigkeit_mit_streckenabschnitten_hinzufügen(
                                name.clone(),
                                geschwindigkeit,
                                streckenabschnitt_map,
                            );
                        },
                        Err(de_serialisieren::Fehler { fehler, .. }) => {
                            match auswahl.overlay_mut() {
                                Some(AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                                    geschwindigkeit_auswahl.entfernen(&name)
                                },
                                modal => {
                                    error!(
                                        "Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}",
                                        modal
                                    );
                                    auswahl.zeige_modal(AuswahlZustand::Geschwindigkeit(
                                        geschwindigkeit::AuswahlZustand::neu(
                                            gleise.geschwindigkeiten(),
                                        ),
                                    ))
                                },
                            }
                            let _ = geschwindigkeiten.remove(&name);
                            fehlermeldung.push_str(&format!(
                                "\nFehler beim Wiederherstellen: {:?}\nGeschwindigkeit {:?} entfernt.",
                                fehler, serialisiert_clone
                            ));
                        },
                    }
                }
                self.zeige_message_box("Hinzufügen Geschwindigkeit".to_string(), fehlermeldung);
            },
        }
    }
}

impl<L: LeiterAnzeige> Zugkontrolle<L> {
    /// Behandle einen Fehler, der bei einer asynchronen Aktion aufgetreten ist.
    #[inline(always)]
    pub fn async_fehler(&mut self, titel: String, nachricht: String) {
        self.zeige_message_box(titel, nachricht);
    }
}

impl<L> Zugkontrolle<L>
where
    L: 'static + LeiterAnzeige + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    <L as Serialisiere>::Serialisiert: Send,
{
    /// Beginne eine kontinuierliche Bewegung des Pivot-Punktes.
    #[inline(always)]
    pub fn bewegung_starten(&mut self, bewegung: Bewegung) -> Command<Nachricht<L>> {
        self.bewegung = Some(bewegung);
        Nachricht::BewegungAusführen.als_sleep_command(Duration::from_millis(20))
    }

    /// Tick für eine Bewegung des Pivot-Punktes.
    pub fn bewegung_ausführen(&mut self) -> Option<Command<Nachricht<L>>> {
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

impl<L: LeiterAnzeige> Zugkontrolle<L> {
    /// Zeige das Auswahl-Fenster zum Anpassen der Anschlüsse einer Weiche.
    #[allow(single_use_lifetimes)]
    pub fn zeige_weiche_anpassen<
        Richtung,
        Anschlüsse,
        AnschlüsseSerialisiert,
        AnschlüsseAuswahlZustand,
    >(
        &mut self,
        weiche: Steuerung<Arc<Mutex<Option<steuerung::Weiche<Richtung, Anschlüsse>>>>>,
        erzeuge_modal: impl Fn(
            weiche::Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>,
            Steuerung<Arc<Mutex<Option<steuerung::Weiche<Richtung, Anschlüsse>>>>>,
        ) -> AuswahlZustand,
    ) where
        Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
        Anschlüsse: Serialisiere<Serialisiert = AnschlüsseSerialisiert>,
        AnschlüsseSerialisiert: Clone + Default,
        AnschlüsseAuswahlZustand: From<AnschlüsseSerialisiert>,
    {
        let serialisiert = weiche.as_ref().lock().serialisiere();
        self.auswahl.zeige_modal(erzeuge_modal(weiche::Zustand::neu(serialisiert), weiche))
    }

    #[allow(single_use_lifetimes)]
    fn zeige_anschlüsse_anpassen_aux<T, W, Zustand>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        erzeuge_modal_zustand: impl Fn(Option<<W as Serialisiere>::Serialisiert>) -> Zustand,
        erzeuge_modal: impl Fn(
            Zustand,
            Arc<dyn Fn(Option<<W as Serialisiere>::Serialisiert>) -> Nachricht<L>>,
        ) -> AuswahlZustand,
        als_nachricht: impl 'static
            + Fn(GleisId<T>, Option<<W as Serialisiere>::Serialisiert>) -> AnschlüsseAnpassen,
    ) where
        T: 'static + for<'t> MitSteuerung<'t, Steuerung = Option<W>> + DatenAuswahl,
        W: Serialisiere,
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

    /// Zeige das Auswahl-Fenster zum Anpassen der Anschlüsse für ein Gleis.
    pub fn zeige_anschlüsse_anpassen(&mut self, any_id: AnyId)
    where
        L: 'static + Debug + Send,
        <L as Leiter>::Fahrtrichtung: Debug + Send,
        <L as Serialisiere>::Serialisiert: Send,
    {
        todo!()
        // match any_id {
        //     AnyId::Gerade(id) => {
        //         debug!("Anschlüsse für Gerade {:?} anpassen.", id)
        //     },
        //     AnyId::Kurve(id) => {
        //         debug!("Anschlüsse für Kurve {:?} anpassen.", id)
        //     },
        //     AnyId::Weiche(id) => self.zeige_anschlüsse_anpassen_aux(
        //         "Weiche",
        //         id,
        //         weiche::Zustand::neu,
        //         AuswahlZustand::Weiche,
        //         AnschlüsseAnpassen::Weiche,
        //     ),
        //     AnyId::DreiwegeWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
        //         "DreiwegeWeiche",
        //         id,
        //         weiche::Zustand::neu,
        //         AuswahlZustand::DreiwegeWeiche,
        //         AnschlüsseAnpassen::DreiwegeWeiche,
        //     ),
        //     AnyId::KurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
        //         "KurvenWeiche",
        //         id,
        //         weiche::Zustand::neu,
        //         AuswahlZustand::KurvenWeiche,
        //         AnschlüsseAnpassen::KurvenWeiche,
        //     ),
        //     AnyId::SKurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
        //         "SKurvenWeiche",
        //         id,
        //         weiche::Zustand::neu,
        //         AuswahlZustand::Weiche,
        //         AnschlüsseAnpassen::SKurvenWeiche,
        //     ),
        //     AnyId::Kreuzung(id) => self.zeige_anschlüsse_anpassen_aux(
        //         "Kreuzung",
        //         id,
        //         weiche::Zustand::neu,
        //         AuswahlZustand::Weiche,
        //         AnschlüsseAnpassen::Kreuzung,
        //     ),
        // }
    }
}

#[allow(single_use_lifetimes)]
impl<L> Zugkontrolle<L>
where
    L: 'static + LeiterAnzeige + BekannterLeiter + Send,
    <L as Serialisiere>::Serialisiert: Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
    <L as Leiter>::UmdrehenZeit: Serialize,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de> + Send,
{
    /// Speicher den aktuellen Zustand in einer Datei.
    pub fn speichern(&mut self, pfad: String) -> Option<Command<Nachricht<L>>> {
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
impl<L: LeiterAnzeige + BekannterLeiter + v2::Kompatibel> Zugkontrolle<L>
where
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
    <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
    <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
    <L as Serialisiere>::Serialisiert: Debug + Clone + Eq + Hash,
{
    /// Lade einen neuen Zustand aus einer Datei.
    pub fn laden(&mut self, pfad: String) {
        match self.gleise.laden(&mut self.lager, &pfad) {
            Ok(()) => {
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
            },
            Err(fehler) => self.zeige_message_box(
                format!("Fehler beim Laden von {}", pfad),
                format!("{:?}", fehler),
            ),
        }
    }
}
