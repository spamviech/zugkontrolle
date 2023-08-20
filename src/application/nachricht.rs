//! [Nachricht] und zugehörige Hilfsgrößen für das [Ausführen](crate::application::ausführen) der [Anwendung](iced::Application).

use std::{convert::identity, fmt::Debug, time::Instant};

use iced::Command;

use crate::{
    anschluss::OutputSerialisiert,
    application::{
        auswahl::AuswahlZustand, bewegen, geschwindigkeit, lizenzen, modal, streckenabschnitt,
        weiche,
    },
    gleis::{
        self,
        gerade::GeradeUnit,
        gleise::{
            self,
            id::{AnyId, GleisId, StreckenabschnittId},
            nachricht::{GleisSteuerung, Nachricht as GleiseNachricht},
            AnschlüsseAnpassen, Modus,
        },
        knopf::KnopfNachricht,
        kreuzung::{Kreuzung, KreuzungUnit},
        kurve::KurveUnit,
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        self,
        geschwindigkeit::{GeschwindigkeitSerialisiert, Leiter, Name as GeschwindigkeitName},
        plan::{AktionGeschwindigkeit, AktionStreckenabschnitt, AnyAktionSchalten, AsyncNachricht},
        streckenabschnitt::Name as StreckenabschnittName,
    },
    typen::{farbe::Farbe, skalar::Skalar, vektor::Vektor, winkel::Winkel},
};

/// Ein beliebiges Gleis ohne Anschlüsse.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AnyGleisUnit {
    /// Eine [Gerade](gleis::Gerade).
    GeradeUnit(GeradeUnit),
    /// Eine [Kurve](gleis::Kurve).
    KurveUnit(KurveUnit),
    /// Eine [Weiche].
    WeicheUnit(WeicheUnit),
    /// Eine [DreiwegeWeiche].
    DreiwegeWeicheUnit(DreiwegeWeicheUnit),
    /// Eine [KurvenWeiche].
    KurvenWeicheUnit(KurvenWeicheUnit),
    /// Eine [SKurvenWeiche].
    SKurvenWeicheUnit(SKurvenWeicheUnit),
    /// Eine [Kreuzung].
    KreuzungUnit(KreuzungUnit),
}

/// Die Id einer Weiche mit [gleis::weiche::gerade::Richtung].
#[derive(Debug, PartialEq)]
pub enum WeichenId {
    /// Die Id einer [Weiche].
    Gerade(GleisId<Weiche>),
    /// Die Id einer [SKurvenWeiche].
    SKurve(GleisId<SKurvenWeiche>),
    /// Die Id einer [Kreuzung].
    Kreuzung(GleisId<Kreuzung>),
}

impl WeichenId {
    pub(in crate::application) fn klonen(&self) -> Self {
        match self {
            WeichenId::Gerade(id) => WeichenId::Gerade(id.klonen()),
            WeichenId::SKurve(id) => WeichenId::SKurve(id.klonen()),
            WeichenId::Kreuzung(id) => WeichenId::Kreuzung(id.klonen()),
        }
    }
}

/// Klonbare Nachricht, für Verwendung z.B. mit [Button](iced::widget::Button).
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Debug, <L as Leiter>::Fahrtrichtung: Clone)]
pub(in crate::application) enum NachrichtClone<L: Leiter> {
    Gleis { gleis: AnyGleisUnit, klick_höhe: Skalar },
    Skalieren(Skalar),
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
}

impl<L: Leiter, S> From<NachrichtClone<L>> for Nachricht<L, S> {
    fn from(nachricht_clone: NachrichtClone<L>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { gleis, klick_höhe } => Nachricht::Gleis { gleis, klick_höhe },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::AktionGeschwindigkeit(aktion) => {
                Nachricht::AktionGeschwindigkeit(aktion)
            },
        }
    }
}

impl<T, L> KnopfNachricht<NachrichtClone<L>> for T
where
    T: Clone + Into<AnyGleisUnit>,
    L: Leiter,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<L> {
        NachrichtClone::Gleis { gleis: self.clone().into(), klick_höhe: klick_position.y }
    }
}

/// Eine Nachricht, die beim [Ausführen](crate::application::ausführen) der [Anwendung](iced::Application) auftreten kann.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[non_exhaustive]
pub enum Nachricht<L: Leiter, S> {
    /// Ein neues Gleis hinzufügen.
    Gleis {
        /// Das neue Gleis.
        gleis: AnyGleisUnit,
        /// Auf welcher Höhe wurde es ins Bild gezogen.
        klick_höhe: Skalar,
    },
    /// Wechsle den aktuellen Modus.
    Modus(Modus),
    /// Eine Nachricht des Widget zum Bewegen des angezeigten Bereichs.
    Bewegen(bewegen::Nachricht),
    /// Tick für Bewegen des angezeigten Bereichs.
    BewegungAusführen,
    /// Ändere die linke obere Ecke des angezeigten Bereichs.
    Position(Vektor),
    /// Ändere den Winkel des angezeigten Bereichs.
    Winkel(Winkel),
    /// Ändere den Skalierung-Faktor der Anzeige.
    Skalieren(Skalar),
    /// Wähle den aktuellen [Streckenabschnitt](steuerung::Streckenabschnitt).
    WähleStreckenabschnitt(Option<(StreckenabschnittId, Farbe)>),
    /// Hinzufügen eines neuen [Streckenabschnittes](steuerung::Streckenabschnitt).
    HinzufügenStreckenabschnitt(
        /// Der Name der assoziierten [Geschwindigkeit](steuerung::Geschwindigkeit).
        Option<GeschwindigkeitName>,
        /// Der Name des neuen [Streckenabschnittes](steuerung::Streckenabschnitt).
        StreckenabschnittName,
        /// Die Farbe, mit der Gleise eingefärbt werden sollen.
        Farbe,
        /// Der verwendete [OutputAnschluss](crate::anschluss::OutputAnschluss).
        OutputSerialisiert,
    ),
    /// Lösche einen [Streckenabschnitt](steuerung::Streckenabschnitt).
    LöscheStreckenabschnitt(StreckenabschnittId),
    /// Setze den [Streckenabschnitt](steuerung::Streckenabschnitt) des spezifizierten Gleises,
    /// sofern es über [StreckenabschnittFestlegen](Nachricht::StreckenabschnittFestlegen)
    /// aktiviert wurde.
    SetzeStreckenabschnitt(AnyId),
    /// Einstellen, ob bei Klick auf ein Gleis der [Streckenabschnitt](steuerung::Streckenabschnitt)
    /// auf den aktuellen gesetzt werden soll
    /// (beeinflusst Reaktion auf [SetzeStreckenabschnitt](Nachricht::SetzeStreckenabschnitt)).
    StreckenabschnittFestlegen(bool),
    /// Speichern im übergebenen Pfad.
    Speichern(String),
    /// Setze die Farbe des Speichern-Knopfes zurück,
    /// sofern die Zeit mit der letzten Speichern-Zeit übereinstimmt.
    EntferneSpeichernFarbe(Instant),
    /// Laden aus dem übergebenen Pfad.
    Laden(String),
    /// Eine Aktion einer [Geschwindigkeit](steuerung::Geschwindigkeit) im [Fahren](Modus::Fahren)-Modus.
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
    /// Hinzufügen einer neuen [Geschwindigkeit](steuerung::Geschwindigkeit).
    HinzufügenGeschwindigkeit(GeschwindigkeitName, GeschwindigkeitSerialisiert<S>),
    /// Löschen einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    LöscheGeschwindigkeit(GeschwindigkeitName),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnschlüsseAnpassen),
    /// Ein Gleis mit [Streckenabschnitt](crate::steuerung::Streckenabschnitt) ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche](steuerung::Weiche) wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Eine GUI-Nachricht für Änderungen des Zustandes des [Gleise]-Typs.
    ///
    /// Notwendig, weil die [update](iced::widget::canvas::Program::update)-Methode keinen `&mut self`-Zugriff erlaubt
    /// und auf den Zustand auch von außerhalb der GUI-Funktionen zugegriffen werden soll
    /// ([State](iced::widget::canvas::Program::State) dadurch nicht möglich).
    GleiseZustandAktualisieren(gleise::nachricht::ZustandAktualisieren),
    /// Dummy-Nachricht, damit die [view](Application::view)-Methode erneut aufgerufen wird.
    ///
    /// Signalisiert eine Anzeige-relevante Änderung, die nicht durch das GUI ausgelöst wurde.
    AsyncAktualisieren,
    /// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
    AsyncFehler {
        /// Der Titel der Fehlermeldung.
        titel: String,
        /// Die Nachricht der Fehlermeldung.
        nachricht: String,
    },
}

impl<L: Leiter, S> From<GleiseNachricht> for modal::Nachricht<AuswahlZustand, Nachricht<L, S>> {
    fn from(nachricht: GleiseNachricht) -> Self {
        match nachricht {
            GleiseNachricht::SetzeStreckenabschnitt(any_id) => {
                modal::Nachricht::Underlay(Nachricht::SetzeStreckenabschnitt(any_id))
            },
            GleiseNachricht::StreckenabschnittUmschalten(aktion) => {
                modal::Nachricht::Underlay(Nachricht::StreckenabschnittUmschalten(aktion))
            },
            GleiseNachricht::WeicheSchalten(aktion) => {
                modal::Nachricht::Underlay(Nachricht::WeicheSchalten(aktion))
            },
            GleiseNachricht::AnschlüsseAnpassen(gleis_steuerung) => match gleis_steuerung {
                GleisSteuerung::Gerade((id, startwert)) => {
                    todo!("AuswahlZustand::Gerade({id:?}, {startwert:?}")
                },
                GleisSteuerung::Kurve((id, startwert)) => {
                    todo!("AuswahlZustand::Kurve({id:?}, {startwert:?}")
                },
                GleisSteuerung::Weiche((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::Gerade(id)),
                ),
                GleisSteuerung::KurvenWeiche((id, startwert)) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::KurvenWeiche(startwert, id))
                },
                GleisSteuerung::DreiwegeWeiche((id, startwert)) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::DreiwegeWeiche(startwert, id))
                },
                GleisSteuerung::SKurvenWeiche((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::SKurve(id)),
                ),
                GleisSteuerung::Kreuzung((id, startwert)) => modal::Nachricht::ZeigeOverlay(
                    AuswahlZustand::Weiche(startwert, WeichenId::Kreuzung(id)),
                ),
            },
            GleiseNachricht::ZustandAktualisieren(nachricht) => {
                modal::Nachricht::Underlay(Nachricht::GleiseZustandAktualisieren(nachricht))
            },
        }
    }
}

impl<L: Leiter, S> From<AsyncNachricht> for Nachricht<L, S> {
    fn from(fehler: AsyncNachricht) -> Self {
        match fehler {
            AsyncNachricht::Aktualisieren => Nachricht::AsyncAktualisieren,
            AsyncNachricht::Fehler { titel, nachricht } => {
                Nachricht::AsyncFehler { titel, nachricht }
            },
        }
    }
}

impl<L: Leiter, S> From<streckenabschnitt::AnzeigeNachricht> for Nachricht<L, S> {
    fn from(nachricht: streckenabschnitt::AnzeigeNachricht) -> Self {
        match nachricht {
            streckenabschnitt::AnzeigeNachricht::Festlegen(festlegen) => {
                Nachricht::StreckenabschnittFestlegen(festlegen)
            },
        }
    }
}

impl<L: Leiter, S> From<steuerung::kontakt::Aktualisieren> for Nachricht<L, S> {
    fn from(_value: steuerung::kontakt::Aktualisieren) -> Self {
        Nachricht::AsyncAktualisieren
    }
}

impl<L: Leiter, S> From<streckenabschnitt::AuswahlNachricht>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from(nachricht: streckenabschnitt::AuswahlNachricht) -> Self {
        use streckenabschnitt::AuswahlNachricht::*;
        match nachricht {
            Schließe => modal::Nachricht::VersteckeOverlay,
            Wähle(wahl) => modal::Nachricht::Underlay(Nachricht::WähleStreckenabschnitt(wahl)),
            Hinzufügen(geschwindigkeit, name, farbe, output) => modal::Nachricht::Underlay(
                Nachricht::HinzufügenStreckenabschnitt(geschwindigkeit, name, farbe, output),
            ),
            Lösche(name) => modal::Nachricht::Underlay(Nachricht::LöscheStreckenabschnitt(name)),
        }
    }
}

impl<L: Leiter, S> From<geschwindigkeit::AuswahlNachricht<S>>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from(nachricht: geschwindigkeit::AuswahlNachricht<S>) -> Self {
        use geschwindigkeit::AuswahlNachricht::*;
        match nachricht {
            Schließen => modal::Nachricht::VersteckeOverlay,
            Hinzufügen(name, geschwindigkeit) => modal::Nachricht::Underlay(
                Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit),
            ),
            Löschen(name) => modal::Nachricht::Underlay(Nachricht::LöscheGeschwindigkeit(name)),
        }
    }
}

/// AuswahlNachricht für die Steuerung einer [Weiche], [Kreuzung] und [SKurvenWeiche].
type WeicheNachricht = weiche::Nachricht<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

impl<L: Leiter, S> From<(WeicheNachricht, WeichenId)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, weichen_id): (WeicheNachricht, WeichenId)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => {
                modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(match weichen_id {
                    WeichenId::Gerade(id) => AnschlüsseAnpassen::Weiche(id, steuerung),
                    WeichenId::SKurve(id) => AnschlüsseAnpassen::SKurvenWeiche(id, steuerung),
                    WeichenId::Kreuzung(id) => AnschlüsseAnpassen::Kreuzung(id, steuerung),
                }))
            },
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

/// AuswahlNachricht für die Steuerung einer [DreiwegeWeiche].
type DreiwegeWeicheNachricht = weiche::Nachricht<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

impl<L: Leiter, S> From<(DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnschlüsseAnpassen::DreiwegeWeiche(gleis_id, steuerung),
            )),
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

/// AuswahlNachricht für die Steuerung einer [KurvenWeiche].
type KurvenWeicheNachricht = weiche::Nachricht<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

impl<L: Leiter, S> From<(KurvenWeicheNachricht, GleisId<KurvenWeiche>)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (KurvenWeicheNachricht, GleisId<KurvenWeiche>)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnschlüsseAnpassen::KurvenWeiche(gleis_id, steuerung),
            )),
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<lizenzen::Nachricht> for modal::Nachricht<AuswahlZustand, Nachricht<L, S>> {
    fn from(nachricht: lizenzen::Nachricht) -> Self {
        use lizenzen::Nachricht::*;
        match nachricht {
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

async fn async_identity<T>(t: T) -> T {
    t
}

impl<L, S> Nachricht<L, S>
where
    L: 'static + Leiter + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    pub(in crate::application) fn als_command(self) -> Command<Nachricht<L, S>> {
        Command::perform(async_identity(self), identity)
    }
}
