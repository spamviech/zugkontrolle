//! Methoden für die update-Methode des iced::Application-Traits

use std::{
    convert::identity,
    fmt::Debug,
    sync::{Arc, PoisonError},
    thread::{self, sleep},
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
            id::{with_any_id, AnyId, GleisId},
            maps::MapSelector,
            steuerung::Steuerung,
            GleisEntferntFehler, Gleise,
        },
        steuerung, streckenabschnitt,
        typen::*,
        weiche, AnschlüsseAnpassen, AnyGleis, Message, MessageBox, Modal, Zugkontrolle,
        ZustandZurücksetzen,
    },
    farbe::Farbe,
    lookup::Lookup,
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, Leiter},
        streckenabschnitt::Streckenabschnitt,
    },
    zugtyp::Zugtyp,
};

impl<Z> Message<Z>
where
    Z: 'static + Zugtyp,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone + Send,
{
    async fn nach_sleep(self, dauer: Duration) -> Self {
        sleep(dauer);
        self
    }

    fn as_sleep_command(self, dauer: Duration) -> iced::Command<Message<Z>> {
        iced::Command::perform(self.nach_sleep(dauer), identity)
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    pub fn zeige_message_box(&mut self, titel_arg: String, nachricht_arg: String) {
        let MessageBox { titel, nachricht, .. } = self.message_box.inner_mut();
        *titel = titel_arg;
        *nachricht = nachricht_arg;
        self.message_box.show(true)
    }

    #[inline(always)]
    pub fn schließe_message_box(&mut self) {
        self.message_box.show(false)
    }

    fn zeige_anschlüsse_anpassen_aux<T: 'static, W: Serialisiere, Status>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<Steuerung<'t, W>, GleisEntferntFehler>,
        erzeuge_modal_status: impl Fn(Option<<W as Serialisiere>::Serialisiert>) -> Status,
        erzeuge_modal: impl Fn(
            Status,
            Arc<dyn Fn(Option<<W as Serialisiere>::Serialisiert>) -> Message<Z>>,
        ) -> Modal<Z>,
        als_nachricht: impl Fn(GleisId<T>, Option<<W as Serialisiere>::Serialisiert>) -> AnschlüsseAnpassen<Z>
            + 'static,
    ) {
        let steuerung_res = gleise_steuerung(&mut self.gleise, &id);
        if let Ok(steuerung) = steuerung_res {
            let steuerung_save = steuerung.as_ref().map(|steuerung| steuerung.serialisiere());
            *self.modal_state.inner_mut() = erzeuge_modal(
                erzeuge_modal_status(steuerung_save),
                Arc::new(move |steuerung| {
                    Message::AnschlüsseAnpassen(als_nachricht(id.clone(), steuerung))
                }),
            );
            self.modal_state.show(true)
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
        ) -> Result<Steuerung<'t, W>, GleisEntferntFehler>,
    ) -> Option<Message<Z>>
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
                        message = Some(Message::SchließeModal)
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
                        error_message.insert(("Anschlüsse anpassen".to_string(), fehlermeldung));
                    }
                }
            } else {
                steuerung.take();
                message = Some(Message::SchließeModal);
            }
        } else {
            error_message.insert((
                "Gleis entfernt!".to_string(),
                format!("Anschlüsse {} anpassen für entferntes Gleis!", gleis_art),
            ));
        }
        if let Some((titel, nachricht)) = error_message {
            self.zeige_message_box(titel, nachricht)
        }

        message
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    pub(crate) fn streckenabschnitt_umschalten<T: MapSelector<Z>>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
    ) {
        if let Ok(steuerung) = self.gleise.streckenabschnitt_für_id(id) {
            if let Some((streckenabschnitt, fließend)) = steuerung {
                let fließend_neu = !*fließend;
                if let Err(error) = streckenabschnitt.strom(fließend_neu) {
                    self.zeige_message_box(
                        "Streckenabschnitt umschalten".to_string(),
                        format!("{:?}", error),
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

    pub fn gleis_hinzufügen(&mut self, gleis: AnyGleis<Z>, grab_height: Skalar) {
        let streckenabschnitt =
            self.streckenabschnitt_aktuell.aktuell.as_ref().map(|(name, _farbe)| name.clone());
        macro_rules! add_grabbed_at_mouse {
            ($gleis:expr) => {{
                self.gleise.add_grabbed_at_mouse(
                    $gleis.to_option(),
                    Vektor { x: Skalar(0.), y: grab_height },
                    streckenabschnitt,
                );
            }};
        }
        match gleis {
            AnyGleis::GeradeUnit(gerade) => add_grabbed_at_mouse!(gerade),
            AnyGleis::KurveUnit(kurve) => add_grabbed_at_mouse!(kurve),
            AnyGleis::WeicheUnit(weiche) => add_grabbed_at_mouse!(weiche),
            AnyGleis::DreiwegeWeicheUnit(dreiwege_weiche) => {
                add_grabbed_at_mouse!(dreiwege_weiche)
            }
            AnyGleis::KurvenWeicheUnit(kurven_weiche) => {
                add_grabbed_at_mouse!(kurven_weiche)
            }
            AnyGleis::SKurvenWeicheUnit(s_kurven_weiche) => {
                add_grabbed_at_mouse!(s_kurven_weiche)
            }
            AnyGleis::KreuzungUnit(kreuzung) => add_grabbed_at_mouse!(kreuzung),
        }
    }

    #[inline(always)]
    pub fn schließe_modal(&mut self) {
        self.modal_state.show(false)
    }

    pub fn zeige_auswahl_streckenabschnitt(&mut self) {
        *self.modal_state.inner_mut() = Modal::Streckenabschnitt(
            streckenabschnitt::AuswahlStatus::neu(self.gleise.streckenabschnitte()),
        );
        self.modal_state.show(true);
    }

    #[inline(always)]
    pub fn streckenabschnitt_wählen(
        &mut self,
        streckenabschnitt: Option<(streckenabschnitt::Name, Farbe)>,
    ) {
        self.streckenabschnitt_aktuell.aktuell = streckenabschnitt
    }

    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        name: streckenabschnitt::Name,
        farbe: Farbe,
        anschluss_definition: OutputSerialisiert,
    ) {
        match self.gleise.streckenabschnitt_mut(&name) {
            Some((streckenabschnitt, fließend))
                if streckenabschnitt.anschluss.serialisiere() == anschluss_definition =>
            {
                streckenabschnitt.farbe = farbe;
                *fließend = Fließend::Gesperrt;
                let fehlermeldung = if let Err(err) = streckenabschnitt.strom(Fließend::Gesperrt) {
                    format!("{:?}", err)
                } else {
                    format!("Streckenabschnitt {} angepasst.", name.0)
                };
                self.zeige_message_box(
                    format!("Streckenabschnitt {} anpassen", name.0),
                    fehlermeldung,
                )
            }
            _ => {
                match anschluss_definition.reserviere(
                    &mut self.anschlüsse,
                    Vec::new(),
                    Vec::new(),
                    Vec::new(),
                ) {
                    Ok(Reserviert { anschluss, .. }) => {
                        self.streckenabschnitt_aktuell.aktuell = Some((name.clone(), farbe));
                        let streckenabschnitt = Streckenabschnitt { farbe, anschluss };
                        match self.modal_state.inner_mut() {
                            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => {
                                streckenabschnitt_auswahl.hinzufügen(&name, &streckenabschnitt);
                            }
                            modal => {
                                error!("Falscher Modal-State bei HinzufügenStreckenabschnitt!");
                                *modal = Modal::Streckenabschnitt(
                                    streckenabschnitt::AuswahlStatus::neu(
                                        self.gleise.streckenabschnitte(),
                                    ),
                                );
                            }
                        }
                        let name_string = name.0.clone();
                        if let Some(ersetzt) =
                            self.gleise.neuer_streckenabschnitt(name, streckenabschnitt)
                        {
                            self.zeige_message_box(
                                format!("Streckenabschnitt {} anpassen", name_string),
                                format!(
                                    "Streckenabschnitt {} angepasst: {:?}",
                                    name_string, ersetzt
                                ),
                            )
                        }
                    }
                    Err(error) => self.zeige_message_box(
                        "Hinzufügen Streckenabschnitt".to_string(),
                        format!("Fehler beim Hinzufügen: {:?}", error),
                    ),
                }
            }
        }
    }

    pub fn streckenabschnitt_löschen(&mut self, name: streckenabschnitt::Name) {
        if self
            .streckenabschnitt_aktuell
            .aktuell
            .as_ref()
            .map_or(false, |(aktuell_name, _farbe)| aktuell_name == &name)
        {
            self.streckenabschnitt_aktuell.aktuell = None;
        }

        match self.modal_state.inner_mut() {
            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => {
                streckenabschnitt_auswahl.entferne(&name);
            }
            modal => {
                error!("Falscher Modal-State bei LöscheStreckenabschnitt!");
                *modal = Modal::Streckenabschnitt(streckenabschnitt::AuswahlStatus::neu(
                    self.gleise
                        .streckenabschnitte()
                        .filter(|(name_iter, _streckenabschnitt)| name_iter != &&name),
                ));
            }
        }
        self.gleise.entferne_streckenabschnitt(name);
    }

    pub fn gleis_setzte_streckenabschnitt(&mut self, any_id: AnyId<Z>) {
        if self.streckenabschnitt_aktuell_festlegen {
            if let Err(GleisEntferntFehler) = with_any_id!(
                &any_id,
                Gleise::setze_streckenabschnitt_unit,
                &mut self.gleise,
                self.streckenabschnitt_aktuell.aktuell.as_ref().map(|(name, _farbe)| name.clone())
            ) {
                self.zeige_message_box(
                    "Gleis entfernt".to_string(),
                    "Versuch den Streckenabschnitt für ein entferntes Gleis zu setzen!".to_string(),
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
        *self.modal_state.inner_mut() = Modal::Geschwindigkeit(
            geschwindigkeit::AuswahlStatus::neu(self.geschwindigkeiten.iter()),
        );
        self.modal_state.show(true);
    }

    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit_save: Geschwindigkeit<
            <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert,
        >,
    ) {
        let (alt_save, (pwm_pins, output_anschlüsse, input_anschlüsse)) = if let Some((
            geschwindigkeit,
            _anzeige_status,
        )) =
            self.geschwindigkeiten.remove(&name)
        {
            (Some(geschwindigkeit.serialisiere()), geschwindigkeit.anschlüsse())
        } else {
            (None, (Vec::new(), Vec::new(), Vec::new()))
        };
        match geschwindigkeit_save.reserviere(
            &mut self.anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        ) {
            Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                match self.modal_state.inner_mut() {
                    Modal::Geschwindigkeit(geschwindigkeit_auswahl) => {
                        geschwindigkeit_auswahl.hinzufügen(&name, &geschwindigkeit)
                    }
                    modal => {
                        error!("Falscher Modal-State bei HinzufügenGeschwindigkeit!");
                        *modal = Modal::Geschwindigkeit(geschwindigkeit::AuswahlStatus::neu(
                            self.geschwindigkeiten.iter(),
                        ));
                    }
                }
                self.geschwindigkeiten.insert(
                    name.clone(),
                    (
                        geschwindigkeit,
                        <<Z as Zugtyp>::Leiter as LeiterAnzeige>::anzeige_status_neu(),
                    ),
                );
                if let Some(ersetzt) = alt_save {
                    self.zeige_message_box(
                        format!("Geschwindigkeit {} anpassen", name.0),
                        format!("Geschwindigkeit {} angepasst: {:?}", name.0, ersetzt),
                    )
                }
            }
            Err(de_serialisieren::Fehler {
                fehler,
                pwm_pins,
                output_anschlüsse,
                input_anschlüsse,
            }) => {
                let mut fehlermeldung = format!("Fehler beim Hinzufügen: {:?}", fehler);
                if let Some(save) = alt_save {
                    let save_clone = save.clone();
                    match save.reserviere(
                        &mut self.anschlüsse,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    ) {
                        Ok(Reserviert { anschluss: geschwindigkeit, .. }) => {
                            // Modal muss nicht angepasst werden,
                            // nachdem nur wiederhergestellt wird
                            self.geschwindigkeiten.insert(
                                name.clone(),
                                (
                                    geschwindigkeit,
                                    <<Z as Zugtyp>::Leiter as LeiterAnzeige>::anzeige_status_neu(),
                                ),
                            );
                        }
                        Err(de_serialisieren::Fehler { fehler, .. }) => {
                            match self.modal_state.inner_mut() {
                                Modal::Geschwindigkeit(geschwindigkeit_auswahl) => {
                                    geschwindigkeit_auswahl.entfernen(&name)
                                }
                                modal => {
                                    error!("Falscher Modal-State bei HinzufügenGeschwindigkeit!");
                                    *modal = Modal::Geschwindigkeit(
                                        geschwindigkeit::AuswahlStatus::neu(
                                            self.geschwindigkeiten.iter(),
                                        ),
                                    );
                                }
                            }
                            fehlermeldung.push_str(&format!(
                            "\nFehler beim Wiederherstellen: {:?}\nGeschwindigkeit {:?} entfernt.",
                            fehler, save_clone
                        ));
                        }
                    }
                }
                self.zeige_message_box("Hinzufügen Geschwindigkeit".to_string(), fehlermeldung);
            }
        }
    }

    pub fn geschwindigkeit_entfernen(&mut self, name: geschwindigkeit::Name) {
        self.geschwindigkeiten.remove(&name);
        match self.modal_state.inner_mut() {
            Modal::Geschwindigkeit(geschwindigkeit_auswahl) => {
                geschwindigkeit_auswahl.entfernen(&name);
            }
            modal => {
                error!("Falscher Modal-State bei LöscheGeschwindigkeit!");
                *modal = Modal::Geschwindigkeit(geschwindigkeit::AuswahlStatus::neu(
                    self.geschwindigkeiten.iter(),
                ));
            }
        }
    }

    pub fn anschlüsse_anpassen(
        &mut self,
        anschlüsse_anpassen: AnschlüsseAnpassen<Z>,
    ) -> Option<Message<Z>> {
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

    fn zustand_zurücksetzen_aux<T, W>(
        &mut self,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<Steuerung<'t, W>, GleisEntferntFehler>,
        anpassen: impl for<'t> FnOnce(&'t mut W),
    ) {
        // Entferntes Gleis wird ignoriert, da es nur um eine Reaktion auf einen Fehler geht
        if let Ok(mut steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(w) = steuerung.as_mut() {
                anpassen(w)
            }
        }
    }

    pub fn async_fehler(
        &mut self,
        titel: String,
        nachricht: String,
        zustand_zurücksetzen: ZustandZurücksetzen<Z>,
    ) {
        fn heile_poison<T>(poison_error: PoisonError<T>, art: &str, name: &String) -> T {
            error!("Anschlüsse-Mutex für {} {} poisoned!", art, name);
            poison_error.into_inner()
        }
        match zustand_zurücksetzen {
            ZustandZurücksetzen::Weiche(id, aktuelle_richtung) => self.zustand_zurücksetzen_aux(
                id,
                Gleise::steuerung_weiche,
                |steuerung::BenannteWeiche { name, weiche: mutex }| {
                    let mut weiche = &mut *mutex.lock().unwrap_or_else(|poison_error| {
                        heile_poison(poison_error, "Weiche", &name.0)
                    });
                    weiche.aktuelle_richtung = aktuelle_richtung;
                },
            ),
            ZustandZurücksetzen::DreiwegeWeiche(id, aktuelle_richtung, letzte_richtung) => self
                .zustand_zurücksetzen_aux(
                    id,
                    Gleise::steuerung_dreiwege_weiche,
                    |steuerung::BenannteWeiche { name, weiche: mutex }| {
                        let mut weiche = &mut *mutex.lock().unwrap_or_else(|poison_error| {
                            heile_poison(poison_error, "Weiche", &name.0)
                        });
                        weiche.aktuelle_richtung = aktuelle_richtung;
                        weiche.letzte_richtung = letzte_richtung
                    },
                ),
            ZustandZurücksetzen::KurvenWeiche(id, aktuelle_richtung) => self
                .zustand_zurücksetzen_aux(
                    id,
                    Gleise::steuerung_kurven_weiche,
                    |steuerung::BenannteWeiche { name, weiche: mutex }| {
                        let mut weiche = &mut *mutex.lock().unwrap_or_else(|poison_error| {
                            heile_poison(poison_error, "Weiche", &name.0)
                        });
                        weiche.aktuelle_richtung = aktuelle_richtung;
                    },
                ),
            ZustandZurücksetzen::SKurvenWeiche(id, aktuelle_richtung) => self
                .zustand_zurücksetzen_aux(
                    id,
                    Gleise::steuerung_s_kurven_weiche,
                    |steuerung::BenannteWeiche { name, weiche: mutex }| {
                        let mut weiche = &mut *mutex.lock().unwrap_or_else(|poison_error| {
                            heile_poison(poison_error, "Weiche", &name.0)
                        });
                        weiche.aktuelle_richtung = aktuelle_richtung;
                    },
                ),
            ZustandZurücksetzen::Kreuzung(id, aktuelle_richtung) => self.zustand_zurücksetzen_aux(
                id,
                Gleise::steuerung_kreuzung,
                |steuerung::BenannteWeiche { name, weiche: mutex }| {
                    let mut weiche = &mut *mutex.lock().unwrap_or_else(|poison_error| {
                        heile_poison(poison_error, "Weiche", &name.0)
                    });
                    weiche.aktuelle_richtung = aktuelle_richtung;
                },
            ),
        }
        self.zeige_message_box(titel, nachricht)
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + 'static,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone + Send,
{
    fn weiche_stellen<T, Richtung, Anschlüsse>(
        &mut self,
        gleis_art: &'static str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<
            Steuerung<'t, steuerung::BenannteWeiche<Richtung, Anschlüsse>>,
            GleisEntferntFehler,
        >,
        nächste_richtung: impl Fn(&mut steuerung::Weiche<Richtung, Anschlüsse>) -> Richtung
            + Send
            + 'static,
        erzeuge_fehler_nachricht: impl Fn(
                String,
                String,
                steuerung::BenannteWeicheSerialisiert<Richtung, Anschlüsse::Serialisiert>,
            ) -> Message<Z>
            + Send
            + 'static,
    ) where
        Richtung: Clone + Serialize + for<'de> Deserialize<'de> + Send + 'static,
        Anschlüsse: Lookup<Richtung, OutputAnschluss> + Serialisiere + Send + 'static,
        <Anschlüsse as Serialisiere>::Serialisiert: Send + 'static,
    {
        let mut error_message = None;
        if let Ok(mut steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(steuerung::BenannteWeiche { name, weiche: mutex }) = steuerung.as_mut() {
                let mutex_clone = mutex.clone();
                let name_clone = name.clone();
                let sender_clone = self.sender.clone();
                thread::spawn(move || {
                    let mut weiche = mutex_clone.lock().unwrap_or_else(|poison_error| {
                        error!("Weiche-Mutex von {} poisoned!", name_clone.0);
                        poison_error.into_inner()
                    });
                    let richtung = nächste_richtung(&mut weiche);
                    if let Err(fehler) = weiche.schalten(&richtung) {
                        let serialisiert = weiche.serialisiere();
                        let send_result = sender_clone.send(erzeuge_fehler_nachricht(
                            format!("{} schalten", gleis_art),
                            format!("{:?}", fehler),
                            steuerung::BenannteWeicheSerialisiert {
                                name: name_clone.clone(),
                                weiche: serialisiert,
                            },
                        ));
                        if let Err(fehler) = send_result {
                            error!("Message-Channel geschlossen: {:?}", fehler)
                        }
                    }
                });
            } else {
                error_message.insert((
                    "Keine Richtungs-Anschlüsse!".to_string(),
                    format!("{} hat keine Anschlüsse!", gleis_art),
                ));
            }
        } else {
            error_message.insert((
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
                |_, _, _| todo!(),
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
                                error!("Invalider Zustand bei Dreiwegeweiche! Schalte auf Gerade.");
                                *aktuelle_richtung = Richtung::Links;
                                Richtung::Gerade
                            }
                        }
                    } else {
                        Richtung::Gerade
                    }
                },
                |_, _, _| todo!(),
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
                |_, _, _| todo!(),
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
                |_, _, _| todo!(),
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
                |_, _, _| todo!(),
            ),
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + 'static,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone + Send,
{
    #[inline(always)]
    pub fn bewegung_starten(&mut self, bewegung: Bewegung) -> iced::Command<Message<Z>> {
        self.bewegung = Some(bewegung);
        Message::BewegungAusführen.as_sleep_command(Duration::from_millis(20))
    }

    pub fn bewegung_ausführen(&mut self) -> Option<iced::Command<Message<Z>>> {
        if let Some(bewegung) = self.bewegung {
            self.bewegung = Some(bewegung);
            self.gleise.bewege_pivot(
                bewegung
                    .vektor(Skalar(1.) / self.gleise.skalierfaktor())
                    .rotiert(-self.gleise.pivot().winkel),
            );
            Some(Message::BewegungAusführen.as_sleep_command(Duration::from_millis(20)))
        } else {
            None
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + 'static,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    pub fn geschwindigkeit_anzeige_nachricht(
        &mut self,
        name: geschwindigkeit::Name,
        nachricht: <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Message,
    ) -> Option<iced::Command<Message<Z>>> {
        let mut command = None;
        if let Some((geschwindigkeit, anzeige_status)) = self.geschwindigkeiten.get_mut(&name) {
            match <Z::Leiter as LeiterAnzeige>::anzeige_update(
                geschwindigkeit,
                anzeige_status,
                nachricht,
            ) {
                Ok(cmd) => {
                    let name_clone = name.clone();
                    command = Some(cmd.map(move |nachricht| Message::GeschwindigkeitAnzeige {
                        name: name_clone.clone(),
                        nachricht,
                    }))
                }
                Err(error) => self.zeige_message_box(
                    format!("Fehler Geschwindigkeit {}", name.0),
                    format!("{:?}", error),
                ),
            }
        } else {
            error!("Update-Nachricht für gelöschte Geschwindigkeit {}: {:?}", name.0, nachricht)
        }
        command
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
                Modal::Weiche,
                AnschlüsseAnpassen::Weiche,
            ),
            AnyId::DreiwegeWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "DreiwegeWeiche",
                id,
                Gleise::steuerung_dreiwege_weiche,
                weiche::Status::neu,
                Modal::DreiwegeWeiche,
                AnschlüsseAnpassen::DreiwegeWeiche,
            ),
            AnyId::KurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "KurvenWeiche",
                id,
                Gleise::steuerung_kurven_weiche,
                weiche::Status::neu,
                Modal::KurvenWeiche,
                AnschlüsseAnpassen::KurvenWeiche,
            ),
            AnyId::SKurvenWeiche(id) => self.zeige_anschlüsse_anpassen_aux(
                "SKurvenWeiche",
                id,
                Gleise::steuerung_s_kurven_weiche,
                weiche::Status::neu,
                Modal::Weiche,
                AnschlüsseAnpassen::SKurvenWeiche,
            ),
            AnyId::Kreuzung(id) => self.zeige_anschlüsse_anpassen_aux(
                "Kreuzung",
                id,
                Gleise::steuerung_kreuzung,
                weiche::Status::neu,
                Modal::Weiche,
                AnschlüsseAnpassen::Kreuzung,
            ),
        }
    }
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + Serialize + 'static,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone + Send,
{
    pub fn speichern(&mut self, pfad: String) -> Option<iced::Command<Message<Z>>> {
        let ergebnis = self.gleise.speichern(
            &pfad,
            self.geschwindigkeiten
                .iter()
                .map(|(name, (geschwindigkeit, _anzeige_status))| {
                    (name.clone(), geschwindigkeit.serialisiere())
                })
                .collect(),
        );
        match ergebnis {
            Ok(()) => {
                self.speichern_laden.färbe_speichern(true);
                let speicher_zeit = Instant::now();
                self.speichern_gefärbt = Some(speicher_zeit.clone());
                let command = Message::EntferneSpeichernFarbe(speicher_zeit)
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
    Z: Zugtyp + PartialEq + Debug + for<'de> Deserialize<'de>,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
    Geschwindigkeit<<Z as Zugtyp>::Leiter>: Leiter,
{
    #[inline(always)]
    pub fn laden(&mut self, pfad: String) {
        match self.gleise.laden(
            &mut self.anschlüsse,
            self.geschwindigkeiten
                .drain()
                .map(|(_name, (geschwindigkeit, _anzeige_status))| geschwindigkeit),
            &pfad,
        ) {
            Ok(geschwindigkeiten) => {
                self.geschwindigkeiten = geschwindigkeiten
                    .into_iter()
                    .map(|(name, geschwindigkeit)| {
                        (name, (geschwindigkeit, Z::Leiter::anzeige_status_neu()))
                    })
                    .collect();
                self.streckenabschnitt_aktuell.aktuell = None;
            }
            Err(err) => self
                .zeige_message_box(format!("Fehler beim Laden von {}", pfad), format!("{:?}", err)),
        }
    }
}
