//! Methoden für die update-Methode des iced::Application-Traits

use std::{
    convert::identity,
    fmt::Debug,
    sync::Arc,
    thread::sleep,
    time::{Duration, Instant},
};

use log::{debug, error};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        Fließend, OutputAnschluss, OutputSerialisiert,
    },
    application::{
        bewegen::Bewegung,
        geschwindigkeit::{self, LeiterAnzeige},
        gleis,
        gleis::gleise::{
            daten::{DatenAuswahl, StreckenabschnittMap},
            id::{mit_any_id, AnyId, GleisId, StreckenabschnittId, StreckenabschnittIdRef},
            steuerung::Steuerung,
            GleisIdFehler, Gleise,
        },
        steuerung, streckenabschnitt,
        typen::*,
        weiche, AnschlüsseAnpassen, AnyGleisUnit, AuswahlStatus, MessageBox, Nachricht,
        Zugkontrolle, ZustandZurücksetzen,
    },
    farbe::Farbe,
    lookup::Lookup,
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter},
        streckenabschnitt::Streckenabschnitt,
    },
    zugtyp::Zugtyp,
};

impl<Z> Nachricht<Z>
where
    Z: Zugtyp + 'static,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Send,
{
    async fn nach_sleep(self, dauer: Duration) -> Self {
        sleep(dauer);
        self
    }

    fn as_sleep_command(self, dauer: Duration) -> iced::Command<Nachricht<Z>> {
        iced::Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<Z: Zugtyp> Zugkontrolle<Z> {
    pub fn zeige_message_box(&mut self, titel: String, nachricht: String) {
        self.message_box.zeige_modal(MessageBox {
            titel,
            nachricht,
            button_state: iced::button::State::new(),
        })
    }

    #[inline(always)]
    pub fn schließe_message_box(&mut self) {
        self.message_box.verstecke_modal()
    }

    fn zeige_anschlüsse_anpassen_aux<T: 'static, W: Serialisiere, Status>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<Steuerung<'t, W>, GleisIdFehler>,
        erzeuge_modal_status: impl Fn(Option<<W as Serialisiere>::Serialisiert>) -> Status,
        erzeuge_modal: impl Fn(
            Status,
            Arc<dyn Fn(Option<<W as Serialisiere>::Serialisiert>) -> Nachricht<Z>>,
        ) -> AuswahlStatus<Z>,
        als_nachricht: impl Fn(GleisId<T>, Option<<W as Serialisiere>::Serialisiert>) -> AnschlüsseAnpassen<Z>
            + 'static,
    ) {
        let steuerung_res = gleise_steuerung(&mut self.gleise, &id);
        if let Ok(steuerung) = steuerung_res {
            let steuerung_save = steuerung.as_ref().map(|steuerung| steuerung.serialisiere());
            self.modal_status.zeige_modal(erzeuge_modal(
                erzeuge_modal_status(steuerung_save),
                Arc::new(move |steuerung| {
                    Nachricht::AnschlüsseAnpassen(als_nachricht(id.klonen(), steuerung))
                }),
            ))
        } else {
            drop(steuerung_res);
            self.zeige_message_box(
                "Gleis entfernt!".to_string(),
                format!("Anschlüsse {} anpassen für entferntes Gleis!", gleis_art),
            )
        }
    }

    fn gleis_anschlüsse_anpassen<T, W>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        anschlüsse_save: Option<<W as Serialisiere>::Serialisiert>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<Steuerung<'t, W>, GleisIdFehler>,
    ) -> Option<Nachricht<Z>>
    where
        W: Serialisiere,
        <W as Serialisiere>::Serialisiert: Debug + Clone,
    {
        let mut message = None;

        let mut error_message = None;
        if let Ok(mut steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(anschlüsse_save) = anschlüsse_save {
                let (steuerung_save, (pwm_pins, output_anschlüsse, input_anschlüsse)) =
                    if let Some(s) = steuerung.take() {
                        (Some(s.serialisiere()), s.anschlüsse())
                    } else {
                        (None, (Vec::new(), Vec::new(), Vec::new()))
                    };
                match anschlüsse_save.reserviere(
                    &mut self.anschlüsse,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                ) {
                    Ok(Reserviert { anschluss, .. }) => {
                        steuerung.insert(anschluss);
                        message = Some(Nachricht::SchließeModal)
                    }
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
                                &mut self.anschlüsse,
                                pwm_pins,
                                output_anschlüsse,
                                input_anschlüsse,
                            ) {
                                Ok(Reserviert { anschluss, .. }) => {
                                    steuerung.insert(anschluss);
                                }
                                Err(error) => {
                                    fehlermeldung.push_str(&format!(
                                    "\nFehler beim Wiederherstellen: {:?}\nSteuerung {:?} entfernt.",
                                    error, save_clone
                                ));
                                }
                            }
                        }
                        let _ = error_message
                            .insert(("Anschlüsse anpassen".to_string(), fehlermeldung));
                    }
                }
            } else {
                steuerung.take();
                message = Some(Nachricht::SchließeModal);
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

    #[zugkontrolle_derive::erstelle_daten_methoden]
    pub(crate) fn streckenabschnitt_umschalten<T: DatenAuswahl<Z>>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
    ) {
        if let Ok(steuerung) = self.gleise.streckenabschnitt_für_id(id) {
            if let Some((streckenabschnitt, fließend)) = steuerung {
                let fließend_neu = !*fließend;
                if let Err(fehler) = streckenabschnitt.strom(fließend_neu) {
                    self.zeige_message_box(
                        "Streckenabschnitt umschalten".to_string(),
                        format!("{:?}", fehler),
                    )
                } else {
                    *fließend = fließend_neu
                }
            } else {
                self.zeige_message_box(
                    "Kein Streckenabschnitt!".to_string(),
                    format!("{} hat keinen Streckenabschnitt!", gleis_art),
                )
            }
        } else {
            self.zeige_message_box(
                "Gleis entfernt!".to_string(),
                format!("FahrenAktion für entfernte {}!", gleis_art),
            )
        }
    }

    pub fn gleis_hinzufügen(&mut self, gleis: AnyGleisUnit<Z>, klick_höhe: Skalar) {
        let streckenabschnitt = self
            .streckenabschnitt_aktuell
            .aktuell
            .as_ref()
            .map(|(streckenabschnitt_id, _farbe)| streckenabschnitt_id.klonen());
        macro_rules! hinzufügen_gehalten_bei_maus {
            ($gleis: expr) => {
                if let Err(fehler) = self.gleise.hinzufügen_gehalten_bei_maus(
                    $gleis.to_option(),
                    Vektor { x: Skalar(0.), y: klick_höhe },
                    streckenabschnitt,
                ) {
                    error!("Aktueller Streckenabschnitt entfernt: {:?}", fehler);
                    self.streckenabschnitt_aktuell.aktuell = None;
                    let _ = self.gleise.hinzufügen_gehalten_bei_maus(
                        $gleis.to_option(),
                        Vektor { x: Skalar(0.), y: klick_höhe },
                        None,
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
            }
            AnyGleisUnit::KurvenWeicheUnit(kurven_weiche) => {
                hinzufügen_gehalten_bei_maus!(kurven_weiche)
            }
            AnyGleisUnit::SKurvenWeicheUnit(s_kurven_weiche) => {
                hinzufügen_gehalten_bei_maus!(s_kurven_weiche)
            }
            AnyGleisUnit::KreuzungUnit(kreuzung) => hinzufügen_gehalten_bei_maus!(kreuzung),
        }
    }

    #[inline(always)]
    pub fn schließe_modal(&mut self) {
        self.modal_status.verstecke_modal()
    }

    pub fn zeige_auswahl_streckenabschnitt(&mut self) {
        self.modal_status.zeige_modal(AuswahlStatus::Streckenabschnitt(
            streckenabschnitt::AuswahlStatus::neu(&self.gleise),
        ))
    }

    #[inline(always)]
    pub fn streckenabschnitt_wählen(
        &mut self,
        streckenabschnitt: Option<(StreckenabschnittId, Farbe)>,
    ) {
        self.streckenabschnitt_aktuell.aktuell = streckenabschnitt
    }

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
            Ok((streckenabschnitt, fließend))
                if streckenabschnitt.lock_anschluss().serialisiere() == anschluss_definition =>
            {
                streckenabschnitt.farbe = farbe;
                *fließend = Fließend::Gesperrt;
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
            }
            _fehler => {
                // Streckenabschnitt hat nur einen Anschluss,
                // nachdem dieser unterschiedlich ist kann der aktuelle Anschluss ignoriert werden.
                // Implementierung über streckenabschnitt_mut (anstelle streckenabschnitt_entfernen)
                // vermeidet (unmöglichen) Fehlerfall mit nicht gefundener Geschwindigkeit
                // beim hinzufügen.
                match anschluss_definition.reserviere(
                    &mut self.anschlüsse,
                    Vec::new(),
                    Vec::new(),
                    Vec::new(),
                ) {
                    Ok(Reserviert { anschluss, .. }) => {
                        self.streckenabschnitt_aktuell.aktuell = Some((
                            StreckenabschnittId {
                                geschwindigkeit: geschwindigkeit.cloned(),
                                name: name.clone(),
                            },
                            farbe,
                        ));
                        let streckenabschnitt = Streckenabschnitt::neu(farbe, anschluss);
                        match self.modal_status.overlay_mut() {
                            Some(AuswahlStatus::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                                streckenabschnitt_auswahl.hinzufügen(&name, &streckenabschnitt);
                            }
                            modal => {
                                error!(
                                    "Falscher Modal-State bei HinzufügenStreckenabschnitt: {:?}",
                                    modal
                                );
                                self.modal_status.zeige_modal(AuswahlStatus::Streckenabschnitt(
                                    streckenabschnitt::AuswahlStatus::neu(&self.gleise),
                                ))
                            }
                        }
                        let id = StreckenabschnittId {
                            geschwindigkeit: geschwindigkeit.cloned(),
                            name: name.clone(),
                        };
                        if let Ok(ersetzt) = self.gleise.streckenabschnitt_hinzufügen(
                            geschwindigkeit,
                            name,
                            streckenabschnitt,
                        ) {
                            self.zeige_message_box(
                                format!("Streckenabschnitt {:?} anpassen", id),
                                format!("Streckenabschnitt {:?} angepasst: {:?}", id, ersetzt),
                            )
                        }
                    }
                    Err(fehler) => self.zeige_message_box(
                        format!("Hinzufügen Streckenabschnitt {:?}", id_ref),
                        format!("Fehler beim Hinzufügen: {:?}", fehler),
                    ),
                }
            }
        }
    }

    pub fn streckenabschnitt_löschen(&mut self, streckenabschnitt_id: StreckenabschnittId) {
        if self
            .streckenabschnitt_aktuell
            .aktuell
            .as_ref()
            .map_or(false, |(aktuell_id, _farbe)| *aktuell_id == streckenabschnitt_id)
        {
            self.streckenabschnitt_aktuell.aktuell = None;
        }

        let nicht_gefunden_nachricht = format!(
            "Streckenabschnitt {:?} sollte entfernt werden, aber wurde nicht gefunden!",
            streckenabschnitt_id
        );
        let name_clone = streckenabschnitt_id.name.clone();
        match self.gleise.streckenabschnitt_entfernen(streckenabschnitt_id) {
            Ok(None) => error!("{}", nicht_gefunden_nachricht),
            Ok(Some(_)) => {}
            Err(fehler) => self.zeige_message_box(
                "Fehler bei Streckenabschnitt löschen!".to_string(),
                format!("{:?}", fehler),
            ),
        }

        match self.modal_status.overlay_mut() {
            Some(AuswahlStatus::Streckenabschnitt(streckenabschnitt_auswahl)) => {
                streckenabschnitt_auswahl.entferne(&name_clone);
            }
            modal => {
                error!("Falscher Modal-State bei LöscheStreckenabschnitt: {:?}", modal);
                self.modal_status.zeige_modal(AuswahlStatus::Streckenabschnitt(
                    streckenabschnitt::AuswahlStatus::neu(&self.gleise),
                ))
            }
        }
    }

    pub fn gleis_setzte_streckenabschnitt(&mut self, mut any_id: AnyId<Z>) {
        if self.streckenabschnitt_aktuell_festlegen {
            if let Err(fehler) = mit_any_id!(
                &mut any_id,
                Gleise::setze_streckenabschnitt,
                &mut self.gleise,
                self.streckenabschnitt_aktuell
                    .aktuell
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

    #[inline(always)]
    pub fn streckenabschnitt_festlegen(&mut self, festlegen: bool) {
        self.streckenabschnitt_aktuell_festlegen = festlegen
    }

    pub fn entferne_speichern_farbe(&mut self, nachricht_zeit: Instant) {
        if let Some(färbe_zeit) = self.speichern_gefärbt {
            if nachricht_zeit == färbe_zeit {
                self.speichern_laden.färbe_speichern(false);
                self.speichern_gefärbt = None;
            }
        }
    }

    pub fn zeige_auswahl_geschwindigkeit(&mut self) {
        self.modal_status.zeige_modal(AuswahlStatus::Geschwindigkeit(
            geschwindigkeit::AuswahlStatus::neu(self.gleise.geschwindigkeiten()),
        ))
    }

    pub fn geschwindigkeit_entfernen(&mut self, name: geschwindigkeit::Name) {
        if let Err(fehler) = self.gleise.geschwindigkeit_entfernen(name) {
            self.zeige_message_box("Geschwindigkeit entfernen".to_string(), format!("{:?}", fehler))
        }
    }

    pub fn anschlüsse_anpassen(
        &mut self,
        anschlüsse_anpassen: AnschlüsseAnpassen<Z>,
    ) -> Option<Nachricht<Z>> {
        match anschlüsse_anpassen {
            AnschlüsseAnpassen::Weiche(id, anschlüsse_save) => self.gleis_anschlüsse_anpassen(
                "Weiche",
                id,
                anschlüsse_save,
                Gleise::steuerung_weiche,
            ),
            AnschlüsseAnpassen::DreiwegeWeiche(id, anschlüsse_save) => self
                .gleis_anschlüsse_anpassen(
                    "DreiwegeWeiche",
                    id,
                    anschlüsse_save,
                    Gleise::steuerung_dreiwege_weiche,
                ),
            AnschlüsseAnpassen::KurvenWeiche(id, anschlüsse_save) => self
                .gleis_anschlüsse_anpassen(
                    "KurvenWeiche",
                    id,
                    anschlüsse_save,
                    Gleise::steuerung_kurven_weiche,
                ),
            AnschlüsseAnpassen::SKurvenWeiche(id, anschlüsse_save) => self
                .gleis_anschlüsse_anpassen(
                    "SKurvenWeiche",
                    id,
                    anschlüsse_save,
                    Gleise::steuerung_s_kurven_weiche,
                ),
            AnschlüsseAnpassen::Kreuzung(id, anschlüsse_save) => self.gleis_anschlüsse_anpassen(
                "Kreuzung",
                id,
                anschlüsse_save,
                Gleise::steuerung_kreuzung,
            ),
        }
    }

    #[inline(always)]
    pub fn bewegung_beenden(&mut self) {
        self.bewegung = None
    }

    #[inline(always)]
    pub fn bewegung_zurücksetzen(&mut self) {
        self.gleise.setze_pivot(Vektor::null_vektor())
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit_save: GeschwindigkeitSerialisiert<Z::Leiter>,
    ) {
        let Zugkontrolle { gleise, anschlüsse, modal_status, geschwindigkeiten, .. } = self;
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
            anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        ) {
            Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                match modal_status.overlay_mut() {
                    Some(AuswahlStatus::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                        geschwindigkeit_auswahl.hinzufügen(&name, &geschwindigkeit)
                    }
                    modal => {
                        error!("Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}", modal);
                        self.modal_status.zeige_modal(AuswahlStatus::Geschwindigkeit(
                            geschwindigkeit::AuswahlStatus::neu(self.gleise.geschwindigkeiten()),
                        ))
                    }
                }
                geschwindigkeiten.insert(
                    name.clone(),
                    <Z::Leiter as LeiterAnzeige>::anzeige_status_neu(name.clone()),
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
                self.gleise.geschwindigkeit_mit_streckenabschnitten_hinzufügen(
                    name,
                    geschwindigkeit,
                    streckenabschnitt_map,
                );
            }
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
                        anschlüsse,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    ) {
                        Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                            // Modal/AnzeigeStatus-Map muss nicht angepasst werden,
                            // nachdem nur wiederhergestellt wird
                            gleise.geschwindigkeit_mit_streckenabschnitten_hinzufügen(
                                name.clone(),
                                geschwindigkeit,
                                streckenabschnitt_map,
                            );
                        }
                        Err(de_serialisieren::Fehler { fehler, .. }) => {
                            match modal_status.overlay_mut() {
                                Some(AuswahlStatus::Geschwindigkeit(geschwindigkeit_auswahl)) => {
                                    geschwindigkeit_auswahl.entfernen(&name)
                                }
                                modal => {
                                    error!(
                                        "Falscher Modal-State bei HinzufügenGeschwindigkeit: {:?}",
                                        modal
                                    );
                                    modal_status.zeige_modal(AuswahlStatus::Geschwindigkeit(
                                        geschwindigkeit::AuswahlStatus::neu(
                                            gleise.geschwindigkeiten(),
                                        ),
                                    ))
                                }
                            }
                            geschwindigkeiten.remove(&name);
                            fehlermeldung.push_str(&format!(
                                "\nFehler beim Wiederherstellen: {:?}\nGeschwindigkeit {:?} entfernt.",
                                fehler, serialisiert_clone
                            ));
                        }
                    }
                }
                self.zeige_message_box("Hinzufügen Geschwindigkeit".to_string(), fehlermeldung);
            }
        }
    }
}

impl<Z: Zugtyp> Zugkontrolle<Z> {
    fn weiche_zurücksetzen<T, Richtung, Anschlüsse>(
        &mut self,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<
            Steuerung<'t, steuerung::Weiche<Richtung, Anschlüsse>>,
            GleisIdFehler,
        >,
        aktuelle_richtung: Richtung,
        letzte_richtung: Richtung,
    ) {
        // Entferntes Gleis wird ignoriert, da es nur um eine Reaktion auf einen Fehler geht
        if let Ok(mut steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(weiche) = steuerung.as_mut() {
                weiche.aktuelle_richtung = aktuelle_richtung;
                weiche.letzte_richtung = letzte_richtung;
            }
        }
    }
}

impl<Z: Zugtyp + 'static> Zugkontrolle<Z> {
    fn geschwindigkeit_anzeige_zurücksetzen(
        &mut self,
        name: geschwindigkeit::Name,
        zustand_zurücksetzen: <Z::Leiter as LeiterAnzeige>::ZustandZurücksetzen,
    ) -> Option<iced::Command<Nachricht<Z>>> {
        let Zugkontrolle { gleise, geschwindigkeiten, .. } = self;
        // Entfernte Geschwindigkeit wird ignoriert,
        // da es nur um eine Reaktion auf einen Fehler geht
        macro_rules! unwrap_or_return {
            ($value:expr) => {
                if let Some(value) = $value {
                    value
                } else {
                    return None;
                }
            };
        }
        let geschwindigkeit = unwrap_or_return!(gleise.geschwindigkeit_mut(&name));
        let anzeige_status = unwrap_or_return!(geschwindigkeiten.get_mut(&name));
        let cmd = <Z::Leiter as LeiterAnzeige>::zustand_zurücksetzen(
            geschwindigkeit,
            anzeige_status,
            zustand_zurücksetzen,
        );
        Some(cmd.map(move |nachricht| Nachricht::GeschwindigkeitAnzeige {
            name: name.clone(),
            nachricht,
        }))
    }

    pub fn async_fehler(
        &mut self,
        titel: String,
        nachricht: String,
        zustand_zurücksetzen: ZustandZurücksetzen<Z>,
    ) -> Option<iced::Command<Nachricht<Z>>> {
        let mut command = None;
        match zustand_zurücksetzen {
            ZustandZurücksetzen::Weiche(id, aktuelle_richtung, letzte_richtung) => self
                .weiche_zurücksetzen(
                    id,
                    Gleise::steuerung_weiche,
                    aktuelle_richtung,
                    letzte_richtung,
                ),
            ZustandZurücksetzen::DreiwegeWeiche(id, aktuelle_richtung, letzte_richtung) => self
                .weiche_zurücksetzen(
                    id,
                    Gleise::steuerung_dreiwege_weiche,
                    aktuelle_richtung,
                    letzte_richtung,
                ),
            ZustandZurücksetzen::KurvenWeiche(id, aktuelle_richtung, letzte_richtung) => self
                .weiche_zurücksetzen(
                    id,
                    Gleise::steuerung_kurven_weiche,
                    aktuelle_richtung,
                    letzte_richtung,
                ),
            ZustandZurücksetzen::SKurvenWeiche(id, aktuelle_richtung, letzte_richtung) => self
                .weiche_zurücksetzen(
                    id,
                    Gleise::steuerung_s_kurven_weiche,
                    aktuelle_richtung,
                    letzte_richtung,
                ),
            ZustandZurücksetzen::Kreuzung(id, aktuelle_richtung, letzte_richtung) => self
                .weiche_zurücksetzen(
                    id,
                    Gleise::steuerung_kreuzung,
                    aktuelle_richtung,
                    letzte_richtung,
                ),
            ZustandZurücksetzen::GeschwindigkeitAnzeige(name, zustand_zurücksetzen) => {
                command = self.geschwindigkeit_anzeige_zurücksetzen(name, zustand_zurücksetzen)
            }
        }
        self.zeige_message_box(titel, nachricht);
        command
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + 'static,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Send,
{
    fn weiche_stellen<T, Richtung, Anschlüsse>(
        &mut self,
        gleis_art: &'static str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<
            Steuerung<'t, steuerung::Weiche<Richtung, Anschlüsse>>,
            GleisIdFehler,
        >,
        nächste_richtung: impl FnOnce(&mut steuerung::Weiche<Richtung, Anschlüsse>) -> Richtung
            + Send
            + 'static,
        zustand_zurücksetzen: impl FnOnce(GleisId<T>, Richtung, Richtung) -> ZustandZurücksetzen<Z>
            + Send
            + 'static,
    ) where
        T: 'static,
        Richtung: Clone + Send + 'static,
        Anschlüsse: Lookup<Richtung, OutputAnschluss> + Send + 'static,
    {
        let mut error_message = None;
        if let Ok(mut steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(mut weiche) = steuerung.as_mut() {
                let richtung = nächste_richtung(&mut weiche);
                let aktuelle_richtung = weiche.aktuelle_richtung.clone();
                let letzte_richtung = weiche.letzte_richtung.clone();
                weiche.async_schalten(richtung, self.sender.clone(), move |fehler| {
                    Nachricht::AsyncFehler {
                        titel: format!("{} schalten", gleis_art),
                        nachricht: format!("{:?}", fehler),
                        zustand_zurücksetzen: zustand_zurücksetzen(
                            id,
                            aktuelle_richtung,
                            letzte_richtung,
                        ),
                    }
                })
            } else {
                let _ = error_message.insert((
                    "Keine Richtungs-Anschlüsse!".to_string(),
                    format!("{} hat keine Anschlüsse!", gleis_art),
                ));
            }
        } else {
            let _ = error_message.insert((
                "Gleis entfernt!".to_string(),
                format!("FahrenAktion für entfernte {}!", gleis_art),
            ));
        }
        if let Some((titel, nachricht)) = error_message {
            self.zeige_message_box(titel, nachricht)
        }
    }

    pub fn fahren_aktion(&mut self, any_id: AnyId<Z>) {
        match any_id {
            AnyId::Gerade(id) => self.streckenabschnitt_umschalten("Gerade", id),
            AnyId::Kurve(id) => self.streckenabschnitt_umschalten("Kurve", id),
            AnyId::Weiche(id) => self.weiche_stellen(
                "Weiche",
                id,
                Gleise::steuerung_weiche,
                |steuerung::Weiche { aktuelle_richtung, .. }| {
                    use gleis::weiche::gerade::Richtung;
                    if aktuelle_richtung == &Richtung::Gerade {
                        Richtung::Kurve
                    } else {
                        Richtung::Gerade
                    }
                },
                ZustandZurücksetzen::Weiche,
            ),
            AnyId::DreiwegeWeiche(id) => self.weiche_stellen(
                "DreiwegeWeiche",
                id,
                Gleise::steuerung_dreiwege_weiche,
                |steuerung::Weiche { aktuelle_richtung, letzte_richtung, .. }| {
                    use gleis::weiche::dreiwege::Richtung;
                    if aktuelle_richtung == &Richtung::Gerade {
                        match letzte_richtung {
                            Richtung::Links => Richtung::Rechts,
                            Richtung::Rechts => Richtung::Links,
                            Richtung::Gerade => {
                                error!("Invalider Zustand bei DreiwegeWeiche! Schalte auf Gerade.");
                                *aktuelle_richtung = Richtung::Links;
                                Richtung::Gerade
                            }
                        }
                    } else {
                        Richtung::Gerade
                    }
                },
                ZustandZurücksetzen::DreiwegeWeiche,
            ),
            AnyId::KurvenWeiche(id) => self.weiche_stellen(
                "KurvenWeiche",
                id,
                Gleise::steuerung_kurven_weiche,
                |steuerung::Weiche { aktuelle_richtung, .. }| {
                    use gleis::weiche::kurve::Richtung;
                    if aktuelle_richtung == &Richtung::Außen {
                        Richtung::Innen
                    } else {
                        Richtung::Außen
                    }
                },
                ZustandZurücksetzen::KurvenWeiche,
            ),
            AnyId::SKurvenWeiche(id) => self.weiche_stellen(
                "SKurvenWeiche",
                id,
                Gleise::steuerung_s_kurven_weiche,
                |steuerung::Weiche { aktuelle_richtung, .. }| {
                    use gleis::weiche::gerade::Richtung;
                    if aktuelle_richtung == &Richtung::Gerade {
                        Richtung::Kurve
                    } else {
                        Richtung::Gerade
                    }
                },
                ZustandZurücksetzen::SKurvenWeiche,
            ),
            AnyId::Kreuzung(id) => self.weiche_stellen(
                "Kreuzung",
                id,
                Gleise::steuerung_kreuzung,
                |steuerung::Weiche { aktuelle_richtung, .. }| {
                    use gleis::weiche::gerade::Richtung;
                    if aktuelle_richtung == &Richtung::Gerade {
                        Richtung::Kurve
                    } else {
                        Richtung::Gerade
                    }
                },
                ZustandZurücksetzen::Kreuzung,
            ),
        }
    }

    #[inline(always)]
    pub fn bewegung_starten(&mut self, bewegung: Bewegung) -> iced::Command<Nachricht<Z>> {
        self.bewegung = Some(bewegung);
        Nachricht::BewegungAusführen.as_sleep_command(Duration::from_millis(20))
    }

    pub fn bewegung_ausführen(&mut self) -> Option<iced::Command<Nachricht<Z>>> {
        if let Some(bewegung) = self.bewegung {
            self.bewegung = Some(bewegung);
            self.gleise.bewege_pivot(
                bewegung
                    .vektor(Skalar(1.) / self.gleise.skalierfaktor())
                    .rotiert(-self.gleise.pivot().winkel),
            );
            Some(Nachricht::BewegungAusführen.as_sleep_command(Duration::from_millis(20)))
        } else {
            None
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + 'static,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Send,
{
    pub fn geschwindigkeit_anzeige_nachricht(
        &mut self,
        name: geschwindigkeit::Name,
        nachricht: <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Nachricht,
    ) -> Option<iced::Command<Nachricht<Z>>> {
        let Zugkontrolle { gleise, geschwindigkeiten, .. } = self;
        macro_rules! unwrap_or_error_return {
            ($value:expr) => {
                if let Some(value) = $value {
                    value
                } else {
                    error!(
                        "Update-Nachricht für gelöschte Geschwindigkeit {}: {:?}",
                        name.0, nachricht
                    );
                    return None;
                }
            };
        }
        let geschwindigkeit = unwrap_or_error_return!(gleise.geschwindigkeit_mut(&name));
        let anzeige_status = unwrap_or_error_return!(geschwindigkeiten.get_mut(&name));
        let name_clone = name.clone();
        let update_result = <Z::Leiter as LeiterAnzeige>::anzeige_update(
            geschwindigkeit,
            anzeige_status,
            nachricht,
            self.sender.clone(),
            move |titel, fehler, zustand_zurücksetzen| Nachricht::AsyncFehler {
                titel,
                nachricht: format!("{:?}", fehler),
                zustand_zurücksetzen: ZustandZurücksetzen::GeschwindigkeitAnzeige(
                    name_clone,
                    zustand_zurücksetzen,
                ),
            },
        );
        match update_result {
            Ok(cmd) => {
                let name_clone = name.clone();
                Some(cmd.map(move |nachricht| Nachricht::GeschwindigkeitAnzeige {
                    name: name_clone.clone(),
                    nachricht,
                }))
            }
            Err(error) => {
                self.zeige_message_box(
                    format!("Fehler Geschwindigkeit {}", name.0),
                    format!("{:?}", error),
                );
                None
            }
        }
    }

    pub fn zeige_anschlüsse_anpassen(&mut self, any_id: AnyId<Z>) {
        match any_id {
            AnyId::Gerade(id) => {
                debug!("Anschlüsse für Gerade {:?} anpassen.", id)
            }
            AnyId::Kurve(id) => {
                debug!("Anschlüsse für Kurve {:?} anpassen.", id)
            }
            AnyId::Weiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "Weiche",
                id,
                Gleise::steuerung_weiche,
                weiche::Status::neu,
                AuswahlStatus::Weiche,
                AnschlüsseAnpassen::Weiche,
            ),
            AnyId::DreiwegeWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "DreiwegeWeiche",
                id,
                Gleise::steuerung_dreiwege_weiche,
                weiche::Status::neu,
                AuswahlStatus::DreiwegeWeiche,
                AnschlüsseAnpassen::DreiwegeWeiche,
            ),
            AnyId::KurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "KurvenWeiche",
                id,
                Gleise::steuerung_kurven_weiche,
                weiche::Status::neu,
                AuswahlStatus::KurvenWeiche,
                AnschlüsseAnpassen::KurvenWeiche,
            ),
            AnyId::SKurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "SKurvenWeiche",
                id,
                Gleise::steuerung_s_kurven_weiche,
                weiche::Status::neu,
                AuswahlStatus::Weiche,
                AnschlüsseAnpassen::SKurvenWeiche,
            ),
            AnyId::Kreuzung(id) => self.zeige_anschlüsse_anpassen_aux(
                "Kreuzung",
                id,
                Gleise::steuerung_kreuzung,
                weiche::Status::neu,
                AuswahlStatus::Weiche,
                AnschlüsseAnpassen::Kreuzung,
            ),
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + Serialize + 'static,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Send,
{
    pub fn speichern(&mut self, pfad: String) -> Option<iced::Command<Nachricht<Z>>> {
        let ergebnis = self.gleise.speichern(&pfad);
        match ergebnis {
            Ok(()) => {
                self.speichern_laden.färbe_speichern(true);
                let speicher_zeit = Instant::now();
                self.speichern_gefärbt = Some(speicher_zeit.clone());
                let command = Nachricht::EntferneSpeichernFarbe(speicher_zeit)
                    .as_sleep_command(Duration::from_secs(2));
                Some(command)
            }
            Err(err) => {
                self.zeige_message_box(
                    format!("Fehler beim Speichern in {}", pfad),
                    format!("{:?}", err),
                );
                None
            }
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + for<'de> Deserialize<'de>,
    Geschwindigkeit<Z::Leiter>: Leiter,
{
    #[inline(always)]
    pub fn laden(&mut self, pfad: String) {
        match self.gleise.laden(&mut self.anschlüsse, &pfad) {
            Ok(()) => {
                self.geschwindigkeiten = self
                    .gleise
                    .geschwindigkeiten()
                    .map(|(name, _geschwindigkeit)| {
                        (name.clone(), Z::Leiter::anzeige_status_neu(name.clone()))
                    })
                    .collect();
                self.streckenabschnitt_aktuell.aktuell = None;
            }
            Err(err) => self
                .zeige_message_box(format!("Fehler beim Laden von {}", pfad), format!("{:?}", err)),
        }
    }
}
