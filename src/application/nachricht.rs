//! [Nachricht] und zugehörige Hilfsgrößen für das [Ausführen](crate::application::ausführen) der [Anwendung](iced::Application).

use std::{convert::identity, fmt::Debug, time::Instant};

use iced::Command;

use crate::{
    anschluss::OutputSerialisiert,
    application::{
        auswahl::{
            AuswahlZustand, DreiwegeWeicheNachricht, KontaktId, KurvenWeicheNachricht,
            WeicheNachricht, WeichenId,
        },
        bewegen, geschwindigkeit, kontakt, lizenzen, modal, streckenabschnitt, weiche,
    },
    gleis::{
        gerade::GeradeUnit,
        gleise::{
            self,
            id::{
                mit_any_id2, AnyDefinitionId2, AnyDefinitionIdSteuerung2, AnyId2, AnyIdSteuerung2,
                AnyIdSteuerungSerialisiert2, GleisId, GleisId2, StreckenabschnittId,
            },
            nachricht::{GleisSteuerung, Nachricht as GleiseNachricht},
            Modus,
        },
        knopf::KnopfNachricht,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::WeicheUnit,
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::SKurvenWeicheUnit,
        },
    },
    steuerung::{
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

/// Klonbare Nachricht, für Verwendung z.B. mit [Button](iced::widget::Button).
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Debug, <L as Leiter>::Fahrtrichtung: Clone)]
pub(in crate::application) enum NachrichtClone<L: Leiter> {
    Gleis { definition_steuerung: AnyDefinitionIdSteuerung2, klick_höhe: Skalar },
    Skalieren(Skalar),
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
}

impl<L: Leiter, S> From<NachrichtClone<L>> for Nachricht<L, S> {
    fn from(nachricht_clone: NachrichtClone<L>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { definition_steuerung, klick_höhe } => {
                Nachricht::Gleis { definition_steuerung, klick_höhe }
            },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::AktionGeschwindigkeit(aktion) => {
                Nachricht::AktionGeschwindigkeit(aktion)
            },
        }
    }
}

impl<T, L> KnopfNachricht<NachrichtClone<L>> for T
where
    T: Clone + Into<AnyDefinitionId2>,
    L: Leiter,
{
    fn nachricht(&self, klick_position: Vektor) -> NachrichtClone<L> {
        macro_rules! erhalte_nachricht {
            ($id: expr) => {
                AnyDefinitionIdSteuerung2::from(($id, None))
            };
        }
        let any_id = self.clone().into();
        NachrichtClone::Gleis {
            definition_steuerung: mit_any_id2!({}, [AnyDefinitionId2 => id] any_id => erhalte_nachricht!()),
            klick_höhe: klick_position.y,
        }
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
        definition_steuerung: AnyDefinitionIdSteuerung2,
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
    /// Wähle den aktuellen [Streckenabschnitt](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    WähleStreckenabschnitt(Option<(StreckenabschnittName, Farbe)>),
    /// Hinzufügen eines neuen [Streckenabschnittes](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    HinzufügenStreckenabschnitt(
        /// Der Name der assoziierten [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
        Option<GeschwindigkeitName>,
        /// Der Name des neuen [Streckenabschnittes](crate::steuerung::streckenabschnitt::Streckenabschnitt).
        StreckenabschnittName,
        /// Die Farbe, mit der Gleise eingefärbt werden sollen.
        Farbe,
        /// Der verwendete [OutputAnschluss](crate::anschluss::OutputAnschluss).
        OutputSerialisiert,
    ),
    /// Lösche einen [Streckenabschnitt](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    LöscheStreckenabschnitt(StreckenabschnittName),
    /// Setze den [Streckenabschnitt](crate::steuerung::streckenabschnitt::Streckenabschnitt) des spezifizierten Gleises,
    /// sofern es über [StreckenabschnittFestlegen](Nachricht::StreckenabschnittFestlegen)
    /// aktiviert wurde.
    SetzeStreckenabschnitt(AnyId2),
    /// Einstellen, ob bei Klick auf ein Gleis der [Streckenabschnitt](crate::steuerung::streckenabschnitt::Streckenabschnitt)
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
    /// Eine Aktion einer [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit) im [Fahren](Modus::Fahren)-Modus.
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
    /// Hinzufügen einer neuen [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    HinzufügenGeschwindigkeit(GeschwindigkeitName, GeschwindigkeitSerialisiert<S>),
    /// Löschen einer [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    LöscheGeschwindigkeit(GeschwindigkeitName),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnyIdSteuerungSerialisiert2),
    /// Ein Gleis mit [Streckenabschnitt](crate::steuerung::Streckenabschnitt) ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche](steuerung::Weiche) wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Eine GUI-Nachricht für Änderungen des Zustandes des [Gleise](crate::gleis::gleise::Gleise)-Typs.
    ///
    /// Notwendig, weil die [update](iced::widget::canvas::Program::update)-Methode keinen `&mut self`-Zugriff erlaubt
    /// und auf den Zustand auch von außerhalb der GUI-Funktionen zugegriffen werden soll
    /// ([State](iced::widget::canvas::Program::State) dadurch nicht möglich).
    GleiseZustandAktualisieren(gleise::nachricht::ZustandAktualisieren),
    /// Dummy-Nachricht, damit die [view](Application::view)-Methode erneut aufgerufen wird.
    ///
    /// Signalisiert eine Anzeige-relevante Änderung, die nicht durch das GUI ausgelöst wurde.
    AsyncAktualisieren {
        /// Soll das Canvas der [Gleise](crate::gleis::gleise::Gleise)-Struktur neu gezeichnet werden.
        gleise_neuzeichnen: bool,
    },
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
                AnyIdSteuerungSerialisiert2::Gerade(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Kontakt(
                        startwert,
                        KontaktId::Gerade(id),
                    ))
                },
                AnyIdSteuerungSerialisiert2::Kurve(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Kontakt(
                        startwert,
                        KontaktId::Kurve(id),
                    ))
                },
                AnyIdSteuerungSerialisiert2::Weiche(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        startwert,
                        WeichenId::Gerade(id),
                    ))
                },
                AnyIdSteuerungSerialisiert2::KurvenWeiche(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::KurvenWeiche(startwert, id))
                },
                AnyIdSteuerungSerialisiert2::DreiwegeWeiche(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::DreiwegeWeiche(startwert, id))
                },
                AnyIdSteuerungSerialisiert2::SKurvenWeiche(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        startwert,
                        WeichenId::SKurve(id),
                    ))
                },
                AnyIdSteuerungSerialisiert2::Kreuzung(id, startwert) => {
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        startwert,
                        WeichenId::Kreuzung(id),
                    ))
                },
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
            AsyncNachricht::Aktualisieren => {
                Nachricht::AsyncAktualisieren { gleise_neuzeichnen: true }
            },
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

impl<L: Leiter, S> From<gleise::steuerung::Aktualisieren> for Nachricht<L, S> {
    fn from(_value: gleise::steuerung::Aktualisieren) -> Self {
        Nachricht::AsyncAktualisieren { gleise_neuzeichnen: true }
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
impl<L: Leiter, S> From<(kontakt::Nachricht, KontaktId)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, weichen_id): (kontakt::Nachricht, KontaktId)) -> Self {
        use kontakt::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => {
                modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(match weichen_id {
                    KontaktId::Gerade(id) => AnyIdSteuerungSerialisiert2::Gerade(id, steuerung),
                    KontaktId::Kurve(id) => AnyIdSteuerungSerialisiert2::Kurve(id, steuerung),
                }))
            },
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(WeicheNachricht, WeichenId)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, weichen_id): (WeicheNachricht, WeichenId)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => {
                modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(match weichen_id {
                    WeichenId::Gerade(id) => AnyIdSteuerungSerialisiert2::Weiche(id, steuerung),
                    WeichenId::SKurve(id) => {
                        AnyIdSteuerungSerialisiert2::SKurvenWeiche(id, steuerung)
                    },
                    WeichenId::Kreuzung(id) => AnyIdSteuerungSerialisiert2::Kreuzung(id, steuerung),
                }))
            },
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(DreiwegeWeicheNachricht, GleisId2<DreiwegeWeiche>)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (DreiwegeWeicheNachricht, GleisId2<DreiwegeWeiche>)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnyIdSteuerungSerialisiert2::DreiwegeWeiche(gleis_id, steuerung),
            )),
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(KurvenWeicheNachricht, GleisId2<KurvenWeiche>)>
    for modal::Nachricht<AuswahlZustand, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (KurvenWeicheNachricht, GleisId2<KurvenWeiche>)) -> Self {
        use weiche::Nachricht::*;
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnyIdSteuerungSerialisiert2::KurvenWeiche(gleis_id, steuerung),
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
