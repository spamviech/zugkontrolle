//! iced::Application für die Gleis-Anzeige

use std::{
    collections::HashMap,
    convert::identity,
    fmt::Debug,
    sync::Arc,
    time::{Duration, Instant},
};

use log::{debug, error};
use serde::{Deserialize, Serialize};
use version::version;

use self::{
    bewegen::{Bewegen, Bewegung},
    drehen::Drehen,
    geschwindigkeit::LeiterAnzeige,
    gleis::{
        gleise::{id::with_any_id, *},
        *,
    },
    sleep::Sleep,
    streckenabschnitt::Streckenabschnitt,
    style::*,
    typen::*,
};
use crate::{
    anschluss::{
        anschlüsse::Anschlüsse,
        speichern::{self, Reserviere, Reserviert, ToSave},
        Fließend, OutputAnschluss, OutputSave,
    },
    args::Args,
    farbe::Farbe,
    lookup::Lookup,
    steuerung::{
        self,
        geschwindigkeit::{Geschwindigkeit, Leiter},
    },
};

pub mod anschluss;
pub mod bewegen;
pub mod drehen;
pub mod farbwahl;
pub mod geschwindigkeit;
pub mod gleis;
pub mod icon;
pub(crate) mod macros;
pub mod sleep;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod touch_canvas;
pub mod typen;
pub mod weiche;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnyGleis<Z> {
    GeradeUnit(GeradeUnit<Z>),
    KurveUnit(KurveUnit<Z>),
    WeicheUnit(WeicheUnit<Z>),
    DreiwegeWeicheUnit(DreiwegeWeicheUnit<Z>),
    KurvenWeicheUnit(KurvenWeicheUnit<Z>),
    SKurvenWeicheUnit(SKurvenWeicheUnit<Z>),
    KreuzungUnit(KreuzungUnit<Z>),
}
macro_rules! impl_any_gleis_from {
    ($type:ident) => {
        impl<Z> From<$type<Z>> for AnyGleis<Z> {
            fn from(gleis: $type<Z>) -> AnyGleis<Z> {
                AnyGleis::$type(gleis.into())
            }
        }
    };
}
impl_any_gleis_from! {GeradeUnit}
impl_any_gleis_from! {KurveUnit}
impl_any_gleis_from! {WeicheUnit}
impl_any_gleis_from! {DreiwegeWeicheUnit}
impl_any_gleis_from! {KurvenWeicheUnit}
impl_any_gleis_from! {SKurvenWeicheUnit}
impl_any_gleis_from! {KreuzungUnit}

impl Modus {
    fn make_radio(self, aktueller_modus: Self) -> iced::Radio<Modus> {
        iced::Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AnschlüsseAnpassen<Z> {
    Weiche(
        GleisId<Weiche<Z>>,
        steuerung::Weiche<
            gleis::weiche::gerade::Richtung,
            gleis::weiche::gerade::RichtungAnschlüsseSave,
        >,
    ),
    DreiwegeWeiche(
        GleisId<DreiwegeWeiche<Z>>,
        steuerung::Weiche<
            gleis::weiche::dreiwege::Richtung,
            gleis::weiche::dreiwege::RichtungAnschlüsseSave,
        >,
    ),
    KurvenWeiche(
        GleisId<KurvenWeiche<Z>>,
        steuerung::Weiche<
            gleis::weiche::kurve::Richtung,
            gleis::weiche::kurve::RichtungAnschlüsseSave,
        >,
    ),
    SKurvenWeiche(
        GleisId<SKurvenWeiche<Z>>,
        steuerung::Weiche<
            gleis::weiche::gerade::Richtung,
            gleis::weiche::gerade::RichtungAnschlüsseSave,
        >,
    ),
    Kreuzung(
        GleisId<Kreuzung<Z>>,
        steuerung::Weiche<
            gleis::weiche::gerade::Richtung,
            gleis::weiche::gerade::RichtungAnschlüsseSave,
        >,
    ),
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    Gleis {
        gleis: AnyGleis<Z>,
        grab_height: Skalar,
    },
    Modus(Modus),
    Bewegen(bewegen::Nachricht),
    BewegungAusführen,
    Position(Vektor),
    Winkel(Winkel),
    Skalieren(Skalar),
    SchließeModal,
    SchließeMessageBox,
    ZeigeAuswahlStreckenabschnitt,
    WähleStreckenabschnitt(Option<(streckenabschnitt::Name, Farbe)>),
    HinzufügenStreckenabschnitt(streckenabschnitt::Name, Farbe, OutputSave),
    LöscheStreckenabschnitt(streckenabschnitt::Name),
    SetzeStreckenabschnitt(AnyId<Z>),
    StreckenabschnittFestlegen(bool),
    Speichern(String),
    EntferneSpeichernFarbe(Instant),
    Laden(String),
    GeschwindigkeitAnzeige {
        name: geschwindigkeit::Name,
        nachricht: <Z::Leiter as LeiterAnzeige>::Message,
    },
    ZeigeAuswahlGeschwindigkeit,
    HinzufügenGeschwindigkeit(
        geschwindigkeit::Name,
        Geschwindigkeit<<<Z as Zugtyp>::Leiter as ToSave>::Save>,
    ),
    LöscheGeschwindigkeit(geschwindigkeit::Name),
    ZeigeAnschlüsseAnpassen(AnyId<Z>),
    AnschlüsseAnpassen(AnschlüsseAnpassen<Z>),
    FahrenAktion(AnyId<Z>),
}

impl<Z> From<gleise::Message<Z>> for Message<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    fn from(message: gleise::Message<Z>) -> Self {
        match message {
            gleise::Message::SetzeStreckenabschnitt(any_id) => {
                Message::SetzeStreckenabschnitt(any_id)
            }
            gleise::Message::AnschlüsseAnpassen(any_id) => {
                Message::ZeigeAnschlüsseAnpassen(any_id)
            }
            gleise::Message::FahrenAktion(any_id) => Message::FahrenAktion(any_id),
        }
    }
}

impl<T, Z> ButtonMessage<Message<Z>> for T
where
    T: Clone + Into<AnyGleis<Z>>,
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    fn to_message(&self, grab_location: Vektor) -> Message<Z> {
        Message::Gleis { gleis: self.clone().into(), grab_height: grab_location.y }
    }
}

trait MitTeilNachricht<'t, Msg: 'static>: Into<iced::Element<'t, Msg>> {
    fn mit_teil_nachricht<Z>(
        self,
        konstruktor: impl Fn(Msg) -> Message<Z> + 'static,
    ) -> iced::Element<'t, Message<Z>>
    where
        Z: 'static + Zugtyp,
        <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
    {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<iced::Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<Z> Message<Z>
where
    Z: 'static + Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone + Send,
{
    fn as_command(self) -> iced::Command<Message<Z>> {
        iced::Command::perform(async_identity(self), identity)
    }
}

enum Modal<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    Streckenabschnitt(streckenabschnitt::AuswahlStatus),
    Geschwindigkeit(geschwindigkeit::AuswahlStatus),
    Weiche(
        weiche::Status<
            gleis::weiche::gerade::RichtungAnschlüsseSave,
            gleis::weiche::gerade::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                steuerung::Weiche<
                    gleis::weiche::gerade::Richtung,
                    gleis::weiche::gerade::RichtungAnschlüsseSave,
                >,
            ) -> Message<Z>,
        >,
    ),
    DreiwegeWeiche(
        weiche::Status<
            gleis::weiche::dreiwege::RichtungAnschlüsseSave,
            gleis::weiche::dreiwege::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                steuerung::Weiche<
                    gleis::weiche::dreiwege::Richtung,
                    gleis::weiche::dreiwege::RichtungAnschlüsseSave,
                >,
            ) -> Message<Z>,
        >,
    ),
    KurvenWeiche(
        weiche::Status<
            gleis::weiche::kurve::RichtungAnschlüsseSave,
            gleis::weiche::kurve::RichtungAnschlüsseAuswahlStatus,
        >,
        Arc<
            dyn Fn(
                steuerung::Weiche<
                    gleis::weiche::kurve::Richtung,
                    gleis::weiche::kurve::RichtungAnschlüsseSave,
                >,
            ) -> Message<Z>,
        >,
    ),
}

#[derive(Debug)]
struct MessageBox {
    titel: String,
    nachricht: String,
    button_state: iced::button::State,
}

pub struct Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    anschlüsse: Anschlüsse,
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<GeradeUnit<Z>>>,
    kurven: Vec<Button<KurveUnit<Z>>>,
    weichen: Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: geschwindigkeit::Map<Z::Leiter>,
    modal_state: iced_aw::modal::State<Modal<Z>>,
    streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus,
    streckenabschnitt_aktuell_festlegen: bool,
    geschwindigkeit_button_state: iced::button::State,
    message_box: iced_aw::modal::State<MessageBox>,
    bewegen: Bewegen,
    drehen: Drehen,
    zoom: iced::slider::State,
    speichern_laden: speichern_laden::Status,
    speichern_gefärbt: Option<Instant>,
    bewegung: Option<(Instant, Bewegung)>,
    // TODO Wegstrecke, Plan
}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    fn zeige_message_box(&mut self, titel_arg: String, nachricht_arg: String) {
        let MessageBox { titel, nachricht, .. } = self.message_box.inner_mut();
        *titel = titel_arg;
        *nachricht = nachricht_arg;
        self.message_box.show(true)
    }

    /// Icon für das Fenster.
    pub fn icon() -> iced::window::Icon {
        icon::icon()
    }

    fn zeige_anschlüsse_anpassen<T: 'static, W: ToSave, Status>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<&'t mut Option<W>, GleisEntferntError>,
        erzeuge_modal_status: impl Fn(Option<<W as ToSave>::Save>) -> Status,
        erzeuge_modal: impl Fn(Status, Arc<dyn Fn(<W as ToSave>::Save) -> Message<Z>>) -> Modal<Z>,
        als_nachricht: impl Fn(GleisId<T>, <W as ToSave>::Save) -> AnschlüsseAnpassen<Z> + 'static,
    ) {
        if let Ok(steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            let steuerung_save = steuerung.as_ref().map(|steuerung| steuerung.to_save());
            *self.modal_state.inner_mut() = erzeuge_modal(
                erzeuge_modal_status(steuerung_save),
                Arc::new(move |steuerung| {
                    Message::AnschlüsseAnpassen(als_nachricht(id.clone(), steuerung))
                }),
            );
            self.modal_state.show(true)
        } else {
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
        anschlüsse_save: <W as ToSave>::Save,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<&'t mut Option<W>, GleisEntferntError>,
    ) -> Option<Message<Z>>
    where
        W: ToSave,
        <W as ToSave>::Save: Debug + Clone,
    {
        let mut message = None;

        if let Ok(steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            // TODO korrigieren, bei Fehler wiederherstellen
            let (steuerung_save, (pwm_pins, output_anschlüsse, input_anschlüsse)) =
                if let Some(s) = steuerung.take() {
                    (Some(s.to_save()), s.anschlüsse())
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
                    *steuerung = Some(anschluss);
                    self.gleise.erzwinge_neuzeichnen();
                    message = Some(Message::SchließeModal)
                }
                Err(speichern::Error {
                    fehler, pwm_pins, output_anschlüsse, input_anschlüsse
                }) => {
                    let mut fehlermeldung = format!("{:?}", fehler);
                    if let Some(save) = steuerung_save {
                        let save_clone = save.clone();
                        match save.reserviere(
                            &mut self.anschlüsse,
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse,
                        ) {
                            Ok(Reserviert { anschluss, .. }) => *steuerung = Some(anschluss),
                            Err(error) => {
                                fehlermeldung.push_str(&format!(
                                    "\nFehler beim Wiederherstellen: {:?}\nSteuerung {:?} entfernt.",
                                    error, save_clone
                                ));
                            }
                        }
                    }
                    self.zeige_message_box("Anschlüsse anpassen".to_string(), fehlermeldung)
                }
            }
        } else {
            self.zeige_message_box(
                "Gleis entfernt!".to_string(),
                format!("Anschlüsse {} anpassen für entferntes Gleis!", gleis_art),
            )
        }

        message
    }

    fn streckenabschnitt_umschalten<T: GleiseMap<Z>>(&mut self, gleis_art: &str, id: GleisId<T>) {
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

    fn weiche_stellen<T, Richtung, Anschlüsse>(
        &mut self,
        gleis_art: &str,
        id: GleisId<T>,
        gleise_steuerung: impl for<'t> Fn(
            &'t mut Gleise<Z>,
            &GleisId<T>,
        ) -> Result<
            &'t mut Option<steuerung::Weiche<Richtung, Anschlüsse>>,
            GleisEntferntError,
        >,
        nächste_richtung: impl Fn(&Richtung, &Richtung) -> Richtung,
    ) where
        Richtung: Clone,
        Anschlüsse: Lookup<Richtung, OutputAnschluss>,
    {
        if let Ok(steuerung) = gleise_steuerung(&mut self.gleise, &id) {
            if let Some(weiche) = steuerung {
                let richtung =
                    nächste_richtung(&weiche.aktuelle_richtung, &weiche.letzte_richtung);
                if let Err(error) = weiche.schalten(&richtung) {
                    self.zeige_message_box(
                        format!("{} schalten", gleis_art),
                        format!("{:?}", error),
                    )
                }
            } else {
                self.zeige_message_box(
                    "Keine Richtungs-Anschlüsse!".to_string(),
                    format!("{} hat keine Anschlüsse!", gleis_art),
                )
            }
        } else {
            self.zeige_message_box(
                "Gleis entfernt!".to_string(),
                format!("FahrenAktion für entfernte {}!", gleis_art),
            )
        }
    }
}

impl<Z> iced::Application for Zugkontrolle<Z>
where
    Z: 'static + Zugtyp + Debug + PartialEq + Serialize + for<'de> Deserialize<'de> + Send + Sync,
    Z::Leiter: Debug,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone + Send,
    <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Fahrtrichtung: Debug,
    <<Z as Zugtyp>::Leiter as LeiterAnzeige>::Message: Unpin,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Unpin,
    Geschwindigkeit<<Z as Zugtyp>::Leiter>: Leiter,
{
    type Executor = iced::executor::Default;
    type Flags = (Anschlüsse, Args);
    type Message = Message<Z>;

    fn new((anschlüsse, args): Self::Flags) -> (Self, iced::Command<Self::Message>) {
        let Args { pfad, modus, zoom, x, y, winkel, .. } = args;
        let mut messages = Vec::new();
        if let Some(modus) = modus {
            messages.push(Message::Modus(modus));
        }
        let gleise = Gleise::neu();
        let auswahl_status = streckenabschnitt::AuswahlStatus::neu(gleise.streckenabschnitte());
        let aktueller_pfad: String;
        if let Some(pfad) = pfad {
            messages.push(Message::Laden(pfad.clone()));
            aktueller_pfad = pfad;
        } else {
            aktueller_pfad = format!("{}.zug", Z::NAME);
        };
        if let Some(zoom) = zoom {
            messages.push(Message::Skalieren(Skalar(zoom)))
        }
        if x.is_some() || y.is_some() {
            messages.push(Message::Position(Vektor {
                x: Skalar(x.unwrap_or(0.)),
                y: Skalar(y.unwrap_or(0.)),
            }))
        }
        if let Some(winkel) = winkel {
            messages.push(Message::Winkel(Winkel(winkel)))
        }
        let zugkontrolle = Zugkontrolle {
            anschlüsse,
            gleise,
            scrollable_state: iced::scrollable::State::new(),
            geraden: Z::geraden().into_iter().map(Button::new).collect(),
            kurven: Z::kurven().into_iter().map(Button::new).collect(),
            weichen: Z::weichen().into_iter().map(Button::new).collect(),
            dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::new).collect(),
            kurven_weichen: Z::kurven_weichen().into_iter().map(Button::new).collect(),
            s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::new).collect(),
            kreuzungen: Z::kreuzungen().into_iter().map(Button::new).collect(),
            geschwindigkeiten: HashMap::new(),
            modal_state: iced_aw::modal::State::new(Modal::Streckenabschnitt(auswahl_status)),
            streckenabschnitt_aktuell: streckenabschnitt::AnzeigeStatus::neu(),
            streckenabschnitt_aktuell_festlegen: false,
            geschwindigkeit_button_state: iced::button::State::new(),
            message_box: iced_aw::modal::State::new(MessageBox {
                titel: "Nicht initialisiert".to_string(),
                nachricht: "Diese Nachricht sollte nicht sichtbar sein!".to_string(),
                button_state: iced::button::State::new(),
            }),
            bewegen: Bewegen::neu(),
            drehen: Drehen::neu(),
            zoom: iced::slider::State::new(),
            speichern_laden: speichern_laden::Status::neu(aktueller_pfad),
            speichern_gefärbt: None,
            bewegung: None,
        };
        let command = if messages.is_empty() {
            iced::Command::none()
        } else {
            iced::Command::batch(messages.into_iter().map(Message::as_command))
        };
        (zugkontrolle, command)
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", version!())
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut iced::Clipboard,
    ) -> iced::Command<Self::Message> {
        let mut command = iced::Command::none();

        match message {
            Message::Gleis { gleis, grab_height } => {
                let streckenabschnitt = self
                    .streckenabschnitt_aktuell
                    .aktuell
                    .as_ref()
                    .map(|(name, _farbe)| name.clone());
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
            Message::Modus(modus) => self.gleise.moduswechsel(modus),
            Message::Bewegen(bewegen::Nachricht::StarteBewegung(bewegung)) => {
                self.bewegung = Some((Instant::now(), bewegung))
            }
            Message::Bewegen(bewegen::Nachricht::BeendeBewegung) => self.bewegung = None,
            Message::Bewegen(bewegen::Nachricht::Zurücksetzen) => {
                self.gleise.setze_pivot(Vektor::null_vektor())
            }
            Message::BewegungAusführen => {
                if let Some((_letzte_zeit, bewegung)) = self.bewegung {
                    self.bewegung = Some((Instant::now(), bewegung));
                    self.gleise.bewege_pivot(
                        bewegung
                            .vektor(Skalar(1.) / self.gleise.skalierfaktor())
                            .rotiert(-self.gleise.pivot().winkel),
                    )
                }
            }
            Message::Position(position) => self.gleise.setze_pivot(position),
            Message::Winkel(winkel) => self.gleise.winkel(winkel),
            Message::Skalieren(skalieren) => self.gleise.setze_skalierfaktor(skalieren),
            Message::SchließeModal => {
                self.modal_state.show(false);
            }
            Message::ZeigeAuswahlStreckenabschnitt => {
                *self.modal_state.inner_mut() = Modal::Streckenabschnitt(
                    streckenabschnitt::AuswahlStatus::neu(self.gleise.streckenabschnitte()),
                );
                self.modal_state.show(true);
            }
            Message::WähleStreckenabschnitt(aktuell) => {
                self.streckenabschnitt_aktuell.aktuell = aktuell;
            }
            Message::HinzufügenStreckenabschnitt(name, farbe, anschluss_definition) => {
                match self.gleise.streckenabschnitt_mut(&name) {
                    Some((streckenabschnitt, fließend))
                        if streckenabschnitt.anschluss.to_save() == anschluss_definition =>
                    {
                        streckenabschnitt.farbe = farbe;
                        *fließend = Fließend::Gesperrt;
                        let fehlermeldung =
                            if let Err(err) = streckenabschnitt.strom(Fließend::Gesperrt) {
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
                                self.streckenabschnitt_aktuell.aktuell =
                                    Some((name.clone(), farbe));
                                let streckenabschnitt = Streckenabschnitt { farbe, anschluss };
                                match self.modal_state.inner_mut() {
                                    Modal::Streckenabschnitt(streckenabschnitt_auswahl) => {
                                        streckenabschnitt_auswahl
                                            .hinzufügen(&name, &streckenabschnitt);
                                    }
                                    modal => {
                                        error!(
                                            "Falscher Modal-State bei HinzufügenStreckenabschnitt!"
                                        );
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
                };
            }
            Message::LöscheStreckenabschnitt(name) => {
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
                            self.gleise.streckenabschnitte(),
                        ));
                    }
                }
                self.gleise.entferne_streckenabschnitt(name);
                self.gleise.erzwinge_neuzeichnen()
            }
            Message::SetzeStreckenabschnitt(any_id) => {
                if self.streckenabschnitt_aktuell_festlegen {
                    if let Err(GleisEntferntError) = with_any_id!(
                        &any_id,
                        Gleise::setze_streckenabschnitt_unit,
                        &mut self.gleise,
                        self.streckenabschnitt_aktuell
                            .aktuell
                            .as_ref()
                            .map(|(name, _farbe)| name.clone())
                    ) {
                        self.zeige_message_box(
                            "Gleis entfernt".to_string(),
                            "Versuch den Streckenabschnitt für ein entferntes Gleis zu setzen!"
                                .to_string(),
                        )
                    }
                }
            }
            Message::StreckenabschnittFestlegen(festlegen) => {
                self.streckenabschnitt_aktuell_festlegen = festlegen
            }
            Message::SchließeMessageBox => self.message_box.show(false),
            Message::Speichern(pfad) => {
                match self.gleise.speichern(
                    &pfad,
                    self.geschwindigkeiten
                        .iter()
                        .map(|(name, (geschwindigkeit, _anzeige_status))| {
                            (name.clone(), geschwindigkeit.to_save())
                        })
                        .collect(),
                ) {
                    Ok(()) => {
                        self.speichern_laden.färbe_speichern(true);
                        self.speichern_gefärbt = Some(Instant::now());
                    }
                    Err(err) => self.zeige_message_box(
                        format!("Fehler beim Speichern in {}", pfad),
                        format!("{:?}", err),
                    ),
                }
            }
            Message::EntferneSpeichernFarbe(nachricht_zeit) => {
                if let Some(färbe_zeit) = self.speichern_gefärbt {
                    if nachricht_zeit == färbe_zeit {
                        self.speichern_laden.färbe_speichern(false);
                        self.speichern_gefärbt = None;
                    }
                }
            }
            Message::Laden(pfad) => match self.gleise.laden(
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
                Err(err) => self.zeige_message_box(
                    format!("Fehler beim Laden von {}", pfad),
                    format!("{:?}", err),
                ),
            },
            Message::GeschwindigkeitAnzeige { name, nachricht } => {
                if let Some((geschwindigkeit, anzeige_status)) =
                    self.geschwindigkeiten.get_mut(&name)
                {
                    match <Z::Leiter as LeiterAnzeige>::anzeige_update(
                        geschwindigkeit,
                        anzeige_status,
                        nachricht,
                    ) {
                        Ok(cmd) => {
                            let name_clone = name.clone();
                            command = cmd.map(move |nachricht| Message::GeschwindigkeitAnzeige {
                                name: name_clone.clone(),
                                nachricht,
                            })
                        }
                        Err(error) => self.zeige_message_box(
                            format!("Fehler Geschwindigkeit {}", name.0),
                            format!("{:?}", error),
                        ),
                    }
                } else {
                    error!(
                        "Update-Nachricht für gelöschte Geschwindigkeit {}: {:?}",
                        name.0, nachricht
                    )
                }
            }
            Message::ZeigeAuswahlGeschwindigkeit => {
                *self.modal_state.inner_mut() = Modal::Geschwindigkeit(
                    geschwindigkeit::AuswahlStatus::neu(self.geschwindigkeiten.iter()),
                );
                self.modal_state.show(true);
            }
            Message::HinzufügenGeschwindigkeit(name, geschwindigkeit_save) => {
                let (alt_save, (pwm_pins, output_anschlüsse, input_anschlüsse)) =
                    if let Some((geschwindigkeit, _anzeige_status)) =
                        self.geschwindigkeiten.remove(&name)
                    {
                        (Some(geschwindigkeit.to_save()), geschwindigkeit.anschlüsse())
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
                                *modal =
                                    Modal::Geschwindigkeit(geschwindigkeit::AuswahlStatus::neu(
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
                    Err(speichern::Error {
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
                                Err(speichern::Error { fehler, .. }) => {
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
                        self.zeige_message_box(
                            "Hinzufügen Geschwindigkeit".to_string(),
                            fehlermeldung,
                        );
                    }
                }
            }
            Message::LöscheGeschwindigkeit(name) => {
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
            Message::ZeigeAnschlüsseAnpassen(any_id) => match any_id {
                AnyId::Gerade(id) => {
                    debug!("Anschlüsse für Gerade {:?} anpassen.", id)
                }
                AnyId::Kurve(id) => {
                    debug!("Anschlüsse für Kurve {:?} anpassen.", id)
                }
                AnyId::Weiche(id) => self.zeige_anschlüsse_anpassen(
                    "Weiche",
                    id,
                    Gleise::steuerung_weiche,
                    weiche::Status::neu,
                    Modal::Weiche,
                    AnschlüsseAnpassen::Weiche,
                ),
                AnyId::DreiwegeWeiche(id) => self.zeige_anschlüsse_anpassen(
                    "DreiwegeWeiche",
                    id,
                    Gleise::steuerung_dreiwege_weiche,
                    weiche::Status::neu,
                    Modal::DreiwegeWeiche,
                    AnschlüsseAnpassen::DreiwegeWeiche,
                ),
                AnyId::KurvenWeiche(id) => self.zeige_anschlüsse_anpassen(
                    "KurvenWeiche",
                    id,
                    Gleise::steuerung_kurven_weiche,
                    weiche::Status::neu,
                    Modal::KurvenWeiche,
                    AnschlüsseAnpassen::KurvenWeiche,
                ),
                AnyId::SKurvenWeiche(id) => self.zeige_anschlüsse_anpassen(
                    "SKurvenWeiche",
                    id,
                    Gleise::steuerung_s_kurven_weiche,
                    weiche::Status::neu,
                    Modal::Weiche,
                    AnschlüsseAnpassen::SKurvenWeiche,
                ),
                AnyId::Kreuzung(id) => self.zeige_anschlüsse_anpassen(
                    "Kreuzung",
                    id,
                    Gleise::steuerung_kreuzung,
                    weiche::Status::neu,
                    Modal::Weiche,
                    AnschlüsseAnpassen::Kreuzung,
                ),
            },
            Message::AnschlüsseAnpassen(anschlüsse_anpassen) => {
                if let Some(message) = match anschlüsse_anpassen {
                    AnschlüsseAnpassen::Weiche(id, anschlüsse_save) => self
                        .gleis_anschlüsse_anpassen(
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
                    AnschlüsseAnpassen::Kreuzung(id, anschlüsse_save) => self
                        .gleis_anschlüsse_anpassen(
                            "Kreuzung",
                            id,
                            anschlüsse_save,
                            Gleise::steuerung_kreuzung,
                        ),
                } {
                    command = message.as_command()
                }
            }
            Message::FahrenAktion(any_id) => match any_id {
                // TODO in Methode auslagern
                AnyId::Gerade(id) => self.streckenabschnitt_umschalten("Gerade", id),
                AnyId::Kurve(id) => self.streckenabschnitt_umschalten("Kurve", id),
                AnyId::Weiche(id) => self.weiche_stellen(
                    "Weiche",
                    id,
                    Gleise::steuerung_weiche,
                    |aktuelle_richtung, _letzte_richtung| {
                        use gleis::weiche::gerade::Richtung;
                        if aktuelle_richtung == &Richtung::Gerade {
                            Richtung::Kurve
                        } else {
                            Richtung::Gerade
                        }
                    },
                ),
                AnyId::DreiwegeWeiche(id) => self.weiche_stellen(
                    "DreiwegeWeiche",
                    id,
                    Gleise::steuerung_dreiwege_weiche,
                    |aktuelle_richtung, letzte_richtung| {
                        use gleis::weiche::dreiwege::Richtung;
                        if aktuelle_richtung == &Richtung::Gerade {
                            if letzte_richtung == &Richtung::Links {
                                Richtung::Rechts
                            } else {
                                Richtung::Links
                            }
                        } else {
                            Richtung::Gerade
                        }
                    },
                ),
                AnyId::KurvenWeiche(id) => self.weiche_stellen(
                    "KurvenWeiche",
                    id,
                    Gleise::steuerung_kurven_weiche,
                    |aktuelle_richtung, _letzte_richtung| {
                        use gleis::weiche::kurve::Richtung;
                        if aktuelle_richtung == &Richtung::Außen {
                            Richtung::Innen
                        } else {
                            Richtung::Außen
                        }
                    },
                ),
                AnyId::SKurvenWeiche(id) => self.weiche_stellen(
                    "SKurvenWeiche",
                    id,
                    Gleise::steuerung_s_kurven_weiche,
                    |aktuelle_richtung, _letzte_richtung| {
                        use gleis::weiche::gerade::Richtung;
                        if aktuelle_richtung == &Richtung::Gerade {
                            Richtung::Kurve
                        } else {
                            Richtung::Gerade
                        }
                    },
                ),
                AnyId::Kreuzung(id) => self.weiche_stellen(
                    "Kreuzung",
                    id,
                    Gleise::steuerung_kreuzung,
                    |aktuelle_richtung, _letzte_richtung| {
                        use gleis::weiche::gerade::Richtung;
                        if aktuelle_richtung == &Richtung::Gerade {
                            Richtung::Kurve
                        } else {
                            Richtung::Gerade
                        }
                    },
                ),
            },
        }

        command
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        let mut subscriptions = Vec::new();
        if let Some(speicher_zeit) = self.speichern_gefärbt {
            subscriptions.push(iced::Subscription::from_recipe(Sleep::neu(
                speicher_zeit,
                Duration::from_secs(2),
                Message::EntferneSpeichernFarbe(speicher_zeit),
            )))
        }
        if let Some((instant, _bewegung)) = self.bewegung {
            subscriptions.push(iced::Subscription::from_recipe(Sleep::neu(
                instant,
                Duration::from_millis(20),
                Message::BewegungAusführen,
            )))
        }
        if subscriptions.is_empty() {
            iced::Subscription::none()
        } else {
            iced::Subscription::batch(subscriptions)
        }
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        let Zugkontrolle {
            anschlüsse: _,
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            modal_state,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_state,
            message_box,
            bewegen,
            drehen,
            zoom,
            speichern_laden,
            speichern_gefärbt: _,
            bewegung: _,
        } = self;
        let aktueller_modus = gleise.modus();
        let aktueller_zoom = gleise.skalierfaktor();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_state,
            bewegen,
            drehen,
            zoom,
            aktueller_zoom,
            speichern_laden,
        );
        let row_with_scrollable = row_with_scrollable(
            aktueller_modus,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
        );

        let column: iced::Element<Self::Message> = iced::Column::new()
            .push(top_row)
            .push(iced::Rule::horizontal(1).style(rule::SEPARATOR))
            .push(
                row_with_scrollable.push(
                    iced::Container::new(
                        iced::Element::from(
                            touch_canvas::Canvas::new(gleise)
                                .width(iced::Length::Fill)
                                .height(iced::Length::Fill),
                        )
                        .map(Into::into),
                    )
                    .width(iced::Length::Fill)
                    .height(iced::Length::Fill),
                ),
            )
            .into();

        let modal = iced_aw::Modal::new(modal_state, column, |modal| match modal {
            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => iced::Element::from(
                streckenabschnitt::Auswahl::neu(streckenabschnitt_auswahl),
            )
            .map(|message| {
                use streckenabschnitt::AuswahlNachricht::*;
                match message {
                    Schließe => Message::SchließeModal,
                    Wähle(wahl) => Message::WähleStreckenabschnitt(wahl),
                    Hinzufügen(name, farbe, anschluss) => {
                        Message::HinzufügenStreckenabschnitt(name, farbe, anschluss)
                    }
                    Lösche(name) => Message::LöscheStreckenabschnitt(name),
                }
            }),
            Modal::Geschwindigkeit(geschwindigkeit_auswahl) => iced::Element::from(
                <<Z as Zugtyp>::Leiter as LeiterAnzeige>::auswahl_neu(geschwindigkeit_auswahl),
            )
            .map(|message| {
                use geschwindigkeit::AuswahlNachricht::*;
                match message {
                    Schließen => Message::SchließeModal,
                    Hinzufügen(name, geschwindigkeit) => {
                        Message::HinzufügenGeschwindigkeit(name, geschwindigkeit)
                    }
                    Löschen(name) => Message::LöscheGeschwindigkeit(name),
                }
            }),
            Modal::Weiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Message::SchließeModal,
                    }
                })
            }
            Modal::DreiwegeWeiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Message::SchließeModal,
                    }
                })
            }
            Modal::KurvenWeiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Message::SchließeModal,
                    }
                })
            }
        })
        .on_esc(Message::SchließeModal);

        iced_aw::Modal::new(message_box, modal, |MessageBox { titel, nachricht, button_state }| {
            iced::Element::from(
                iced_aw::Card::new(
                    iced::Text::new(&*titel),
                    iced::Column::new().push(iced::Text::new(&*nachricht)).push(
                        iced::Button::new(button_state, iced::Text::new("Ok"))
                            .on_press(Message::SchließeMessageBox),
                    ),
                )
                .width(iced::Length::Shrink),
            )
        })
        .on_esc(Message::SchließeMessageBox)
        .into()
    }
}

fn top_row<'t, Z>(
    aktueller_modus: Modus,
    streckenabschnitt: &'t mut streckenabschnitt::AnzeigeStatus,
    streckenabschnitt_festlegen: &'t mut bool,
    geschwindigkeit_button_state: &'t mut iced::button::State,
    bewegen: &'t mut Bewegen,
    drehen: &'t mut Drehen,
    zoom: &'t mut iced::slider::State,
    aktueller_zoom: Skalar,
    speichern_laden: &'t mut speichern_laden::Status,
) -> iced::Row<'t, Message<Z>>
where
    Z: 'static + Zugtyp,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    let modus_radios = iced::Column::new()
        .push(Modus::Bauen.make_radio(aktueller_modus))
        .push(Modus::Fahren.make_radio(aktueller_modus));
    let bewegen = touch_canvas::Canvas::new(bewegen)
        .width(iced::Length::Units(50))
        .height(iced::Length::Units(50));
    let drehen = touch_canvas::Canvas::new(drehen)
        .width(iced::Length::Units(50))
        .height(iced::Length::Units(50));
    let skalieren_slider = iced::Column::new()
        .push(iced::Text::new(format!("Zoom {:.2}", aktueller_zoom.0)))
        .push(
            iced::Slider::new(zoom, -2.5..=1.5, aktueller_zoom.0.ln(), |exponent| {
                Message::Skalieren(Skalar(exponent.exp()))
            })
            .step(0.01)
            .width(iced::Length::Units(100)),
        )
        .align_items(iced::Align::Center);
    let speichern_laden = speichern_laden::SpeichernLaden::neu(speichern_laden);
    let mut row = iced::Row::new()
        .push(modus_radios.mit_teil_nachricht(Message::Modus))
        .push(bewegen.mit_teil_nachricht(Message::Bewegen))
        .push(drehen.mit_teil_nachricht(Message::Winkel))
        .push(skalieren_slider);

    // Streckenabschnitte und Geschwindigkeiten können nur im Bauen-Modus geändert werden
    if let Modus::Bauen { .. } = aktueller_modus {
        row = row
            .push(
                iced::Element::from(streckenabschnitt::Anzeige::neu(
                    streckenabschnitt,
                    *streckenabschnitt_festlegen,
                ))
                .map(|message| match message {
                    streckenabschnitt::AnzeigeNachricht::Auswählen => {
                        Message::ZeigeAuswahlStreckenabschnitt
                    }
                    streckenabschnitt::AnzeigeNachricht::Festlegen(festlegen) => {
                        Message::StreckenabschnittFestlegen(festlegen)
                    }
                }),
            )
            .push(
                iced::Button::new(
                    geschwindigkeit_button_state,
                    iced::Text::new("Geschwindigkeiten"),
                )
                .on_press(Message::ZeigeAuswahlGeschwindigkeit),
            );
    }

    row.push(iced::Space::new(iced::Length::Fill, iced::Length::Shrink))
        .push(iced::Element::from(speichern_laden).map(|message| match message {
            speichern_laden::Nachricht::Speichern(pfad) => Message::Speichern(pfad),
            speichern_laden::Nachricht::Laden(pfad) => Message::Laden(pfad),
        }))
        .padding(5)
        .spacing(5)
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
}

fn row_with_scrollable<'t, Z>(
    aktueller_modus: Modus,
    scrollable_state: &'t mut iced::scrollable::State,
    geraden: &'t mut Vec<Button<GeradeUnit<Z>>>,
    kurven: &'t mut Vec<Button<KurveUnit<Z>>>,
    weichen: &'t mut Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: &'t mut Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: &'t mut Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: &'t mut Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: &'t mut Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: &'t mut geschwindigkeit::Map<Z::Leiter>,
) -> iced::Row<'t, Message<Z>>
where
    Z: 'static + Zugtyp,
    Z::Leiter: Debug + LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    // TODO Save/Load/Move?/Rotate?
    // Bauen(Streckenabschnitt?/Geschwindigkeit?/Löschen?)
    // Fahren(Streckenabschnitt-Anzeige?
    let mut scrollable = iced::Scrollable::new(scrollable_state);
    let scrollable_style = scrollable::Collection::new(10);
    let scroller_width = scrollable_style.width();
    let mut width = iced::Length::Shrink;
    match aktueller_modus {
        Modus::Bauen => {
            let mut max_width = None;
            macro_rules! add_buttons {
                ($($vec: expr),*) => {
                    max_width = max_width.max(Vec::new().into_iter()
                        $(.chain($vec.iter().map(|button| button.size().x.0.ceil() as u16)))*
                        .max());
                    $(
                        for button in $vec {
                            scrollable = scrollable.push(button.to_iced(max_width));
                        }
                    )*
                }
            }
            add_buttons!(
                geraden,
                kurven,
                weichen,
                dreiwege_weichen,
                kurven_weichen,
                s_kurven_weichen,
                kreuzungen
            );
            if let Some(max) = max_width {
                width = iced::Length::Units(max + scroller_width);
            }
        }
        Modus::Fahren => {
            scrollable = scrollable.push(iced::Text::new("Geschwindigkeiten")).spacing(1);
            for (name, (geschwindigkeit, anzeige_status)) in geschwindigkeiten {
                let name_clone = name.clone();
                scrollable = scrollable.push(
                    iced::Element::from(Z::Leiter::anzeige_neu(
                        name,
                        geschwindigkeit,
                        anzeige_status,
                    ))
                    .map(move |nachricht| Message::GeschwindigkeitAnzeige {
                        name: name_clone.clone(),
                        nachricht,
                    }),
                );
            }
            // TODO Geschwindigkeiten?, Wegstrecken?, Pläne?, Separator dazwischen?
        }
    }
    iced::Row::new()
        .push(
            iced::Container::new(
                scrollable
                    .scroller_width(scroller_width)
                    .width(iced::Length::Shrink)
                    .height(iced::Length::Fill)
                    .style(scrollable_style),
            )
            .width(width)
            .height(iced::Length::Fill),
        )
        .push(iced::Rule::vertical(1).style(rule::SEPARATOR))
}
