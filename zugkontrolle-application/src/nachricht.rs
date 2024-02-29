//! [Nachricht] und zugehörige Hilfsgrößen für das [Ausführen](crate::ausführen) der [`Anwendung`](iced::Application).

use std::{convert::identity, fmt::Debug, time::Instant};

use iced::Command;

use zugkontrolle_anschluss::OutputSerialisiert;
use zugkontrolle_gleis::{
    gerade::GeradeUnit,
    id::{
        mit_any_id, AnyDefinitionId, AnyDefinitionIdSteuerung, AnyId, AnyIdSteuerungSerialisiert,
    },
    kreuzung::KreuzungUnit,
    kurve::KurveUnit,
    steuerung::{
        aktualisieren::Aktualisieren,
        geschwindigkeit::{GeschwindigkeitSerialisiert, Leiter, Name as GeschwindigkeitName},
        plan::{AktionGeschwindigkeit, AktionStreckenabschnitt, AnyAktionSchalten, AsyncNachricht},
        streckenabschnitt::Name as StreckenabschnittName,
    },
    weiche::{
        dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
        gerade::WeicheUnit,
        kurve::{KurvenWeiche, KurvenWeicheUnit},
        s_kurve::SKurvenWeicheUnit,
    },
};
use zugkontrolle_gleise::{
    self, knopf,
    nachricht::{Nachricht as GleiseNachricht, ZustandAktualisieren},
    Modus,
};
use zugkontrolle_id::GleisId;
use zugkontrolle_typen::{
    farbe::Farbe, klick_quelle::KlickQuelle, skalar::Skalar, vektor::Vektor, winkel::Winkel,
};

use crate::{
    auswahl::{
        AuswahlZustand, DreiwegeWeicheNachricht, KontaktId, KurvenWeicheNachricht, WeicheNachricht,
        WeichenId,
    },
    bewegen, geschwindigkeit, kontakt, lizenzen, modal, streckenabschnitt,
    style::Thema,
    weiche,
};

/// Ein beliebiges Gleis ohne Anschlüsse.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AnyGleisUnit {
    /// Eine [`Gerade`](gleis::Gerade).
    GeradeUnit(GeradeUnit),
    /// Eine [`Kurve`](gleis::Kurve).
    KurveUnit(KurveUnit),
    /// Eine [`Weiche`].
    WeicheUnit(WeicheUnit),
    /// Eine [`DreiwegeWeiche`].
    DreiwegeWeicheUnit(DreiwegeWeicheUnit),
    /// Eine [`KurvenWeiche`].
    KurvenWeicheUnit(KurvenWeicheUnit),
    /// Eine [`SKurvenWeiche`].
    SKurvenWeicheUnit(SKurvenWeicheUnit),
    /// Eine [`Kreuzung`].
    KreuzungUnit(KreuzungUnit),
}

/// Klonbare Nachricht, für Verwendung z.B. mit [`Button`](iced::widget::Button).
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Debug, <L as Leiter>::Fahrtrichtung: Clone)]
pub(crate) enum NachrichtClone<L: Leiter> {
    /// Ein neues Gleis hinzufügen.
    Gleis {
        /// Das neue Gleis.
        definition_steuerung: AnyDefinitionIdSteuerung,
        /// Wie wurde das Gleis angeklickt.
        klick_quelle: KlickQuelle,
        /// Auf welcher Höhe wurde es ins Bild gezogen.
        klick_höhe: Skalar,
    },
    /// Ändere den Skalierung-Faktor der Anzeige.
    Skalieren(Skalar),
    /// Eine Aktion einer [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit)
    /// im [`Fahren`](Modus::Fahren)-Modus.
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
    /// Wechsle den aktuellen Modus.
    Modus(Modus),
    /// Ändere das aktuelle Anzeige-[`Thema`].
    Thema(Thema),
}

impl<L: Leiter, S> From<NachrichtClone<L>> for Nachricht<L, S> {
    fn from(nachricht_clone: NachrichtClone<L>) -> Self {
        match nachricht_clone {
            NachrichtClone::Gleis { definition_steuerung, klick_quelle, klick_höhe } => {
                Nachricht::Gleis { definition_steuerung, klick_quelle, klick_höhe }
            },
            NachrichtClone::Skalieren(skalieren) => Nachricht::Skalieren(skalieren),
            NachrichtClone::AktionGeschwindigkeit(aktion) => {
                Nachricht::AktionGeschwindigkeit(aktion)
            },
            NachrichtClone::Modus(modus) => Nachricht::Modus(modus),
            NachrichtClone::Thema(thema) => Nachricht::Thema(thema),
        }
    }
}

impl<T, L> knopf::Nachricht<T> for NachrichtClone<L>
where
    T: Clone + Into<AnyDefinitionId>,
    L: Leiter,
{
    fn nachricht(id: &T, klick_quelle: KlickQuelle, klick_position: Vektor) -> NachrichtClone<L> {
        /// Hilfs-Makro für die Verwendung mit [`mit_any_id`].
        macro_rules! erhalte_nachricht {
            ($id: expr) => {
                AnyDefinitionIdSteuerung::from(($id, None))
            };
        }
        let any_id = id.clone().into();
        NachrichtClone::Gleis {
            definition_steuerung: mit_any_id!({}, [AnyDefinitionId => id] any_id => erhalte_nachricht!()),
            klick_quelle,
            klick_höhe: klick_position.y,
        }
    }
}

/// Eine Nachricht, die beim [Ausführen](crate::ausführen) der [`Anwendung`](iced::Application) auftreten kann.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[non_exhaustive]
pub enum Nachricht<L: Leiter, S> {
    /// Ein neues Gleis hinzufügen.
    Gleis {
        /// Das neue Gleis.
        definition_steuerung: AnyDefinitionIdSteuerung,
        /// Wie wurde das Gleis angeklickt.
        klick_quelle: KlickQuelle,
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
    /// Wähle den aktuellen [`Streckenabschnitt`](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    WähleStreckenabschnitt(Option<(StreckenabschnittName, Farbe)>),
    /// Hinzufügen eines neuen [`Streckenabschnittes`](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    HinzufügenStreckenabschnitt(
        /// Der Name der assoziierten [`Geschwindigkeit`](crate::steuerung::geschwindigkeit::Geschwindigkeit).
        Option<GeschwindigkeitName>,
        /// Der Name des neuen [`Streckenabschnittes`](crate::steuerung::streckenabschnitt::Streckenabschnitt).
        StreckenabschnittName,
        /// Die Farbe, mit der Gleise eingefärbt werden sollen.
        Farbe,
        /// Der verwendete [`OutputAnschluss`](crate::anschluss::OutputAnschluss).
        OutputSerialisiert,
    ),
    /// Lösche einen [`Streckenabschnitt`](crate::steuerung::streckenabschnitt::Streckenabschnitt).
    LöscheStreckenabschnitt(StreckenabschnittName),
    /// Setze den [`Streckenabschnitt`](crate::steuerung::streckenabschnitt::Streckenabschnitt) des spezifizierten Gleises,
    /// sofern es über [`StreckenabschnittFestlegen`](Nachricht::StreckenabschnittFestlegen)
    /// aktiviert wurde.
    SetzeStreckenabschnitt(AnyId),
    /// Einstellen, ob bei Klick auf ein Gleis der [`Streckenabschnitt`](crate::steuerung::streckenabschnitt::Streckenabschnitt)
    /// auf den aktuellen gesetzt werden soll
    /// (beeinflusst Reaktion auf [`SetzeStreckenabschnitt`](Nachricht::SetzeStreckenabschnitt)).
    StreckenabschnittFestlegen(bool),
    /// Speichern im übergebenen Pfad.
    Speichern(String),
    /// Setze die Farbe des Speichern-Knopfes zurück,
    /// sofern die Zeit mit der letzten Speichern-Zeit übereinstimmt.
    EntferneSpeichernFarbe(Instant),
    /// Laden aus dem übergebenen Pfad.
    Laden(String),
    /// Eine Aktion einer [Geschwindigkeit](crate::steuerung::geschwindigkeit::Geschwindigkeit) im [`Fahren`](Modus::Fahren)-Modus.
    AktionGeschwindigkeit(AktionGeschwindigkeit<L>),
    /// Hinzufügen einer neuen [`Geschwindigkeit`](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    HinzufügenGeschwindigkeit(GeschwindigkeitName, GeschwindigkeitSerialisiert<S>),
    /// Löschen einer [`Geschwindigkeit`](crate::steuerung::geschwindigkeit::Geschwindigkeit).
    LöscheGeschwindigkeit(GeschwindigkeitName),
    /// Anpassen der Anschlüsse eines Gleises.
    AnschlüsseAnpassen(AnyIdSteuerungSerialisiert),
    /// Ein Gleis mit [`Streckenabschnitt`](crate::steuerung::Streckenabschnitt) ohne spezielle Aktion
    /// wurde im [`Fahren`](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche](steuerung::Weiche) wurde im [`Fahren`](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Eine GUI-Nachricht für Änderungen des Zustandes des [`Gleise`](crate::gleise::Gleise)-Typs.
    ///
    /// Notwendig, weil die [`update`](iced::widget::canvas::Program::update)-Methode keinen `&mut self`-Zugriff erlaubt
    /// und auf den Zustand auch von außerhalb der GUI-Funktionen zugegriffen werden soll
    /// ([`State`](iced::widget::canvas::Program::State) dadurch nicht möglich).
    GleiseZustandAktualisieren(ZustandAktualisieren),
    /// Ändere das aktuelle Anzeige-[`Thema`].
    Thema(Thema),
    /// Dummy-Nachricht, damit die [`view`](Application::view)-Methode erneut aufgerufen wird.
    ///
    /// Signalisiert eine Anzeige-relevante Änderung, die nicht durch das GUI ausgelöst wurde.
    AsyncAktualisieren {
        /// Soll das Canvas der [`Gleise`](crate::gleise::Gleise)-Struktur neu gezeichnet werden.
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

impl<L: Leiter, S> From<GleiseNachricht> for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>> {
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
                AnyIdSteuerungSerialisiert::Gerade(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Kontakt(
                        KontaktId::Gerade(id),
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::Kurve(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Kontakt(
                        KontaktId::Kurve(id),
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::Weiche(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        WeichenId::Gerade(id),
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::KurvenWeiche(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::KurvenWeiche(
                        id,
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::DreiwegeWeiche(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::DreiwegeWeiche(
                        id,
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::SKurvenWeiche(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        WeichenId::SKurve(id),
                        startwert,
                        hat_steuerung,
                    ))
                },
                AnyIdSteuerungSerialisiert::Kreuzung(id, startwert) => {
                    let hat_steuerung = startwert.is_some();
                    modal::Nachricht::ZeigeOverlay(AuswahlZustand::Weiche(
                        WeichenId::Kreuzung(id),
                        startwert,
                        hat_steuerung,
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

impl<L: Leiter, S> From<Aktualisieren> for Nachricht<L, S> {
    fn from(_value: Aktualisieren) -> Self {
        Nachricht::AsyncAktualisieren { gleise_neuzeichnen: true }
    }
}

impl<L: Leiter, S> From<streckenabschnitt::AuswahlNachricht>
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from(nachricht: streckenabschnitt::AuswahlNachricht) -> Self {
        use streckenabschnitt::AuswahlNachricht::{Hinzufügen, Lösche, Schließe, Wähle};
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
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from(nachricht: geschwindigkeit::AuswahlNachricht<S>) -> Self {
        use geschwindigkeit::AuswahlNachricht::{Hinzufügen, Löschen, Schließen};
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
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from((nachricht, weichen_id): (kontakt::Nachricht, KontaktId)) -> Self {
        use kontakt::Nachricht::{Festlegen, Schließen};
        match nachricht {
            Festlegen(steuerung) => {
                modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(match weichen_id {
                    KontaktId::Gerade(id) => AnyIdSteuerungSerialisiert::Gerade(id, steuerung),
                    KontaktId::Kurve(id) => AnyIdSteuerungSerialisiert::Kurve(id, steuerung),
                }))
            },
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(WeicheNachricht, WeichenId)>
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from((nachricht, weichen_id): (WeicheNachricht, WeichenId)) -> Self {
        use weiche::Nachricht::{Festlegen, Schließen};
        match nachricht {
            Festlegen(steuerung) => {
                modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(match weichen_id {
                    WeichenId::Gerade(id) => AnyIdSteuerungSerialisiert::Weiche(id, steuerung),
                    WeichenId::SKurve(id) => {
                        AnyIdSteuerungSerialisiert::SKurvenWeiche(id, steuerung)
                    },
                    WeichenId::Kreuzung(id) => AnyIdSteuerungSerialisiert::Kreuzung(id, steuerung),
                }))
            },
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)>
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)) -> Self {
        use weiche::Nachricht::{Festlegen, Schließen};
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnyIdSteuerungSerialisiert::DreiwegeWeiche(gleis_id, steuerung),
            )),
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<(KurvenWeicheNachricht, GleisId<KurvenWeiche>)>
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from((nachricht, gleis_id): (KurvenWeicheNachricht, GleisId<KurvenWeiche>)) -> Self {
        use weiche::Nachricht::{Festlegen, Schließen};
        match nachricht {
            Festlegen(steuerung) => modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                AnyIdSteuerungSerialisiert::KurvenWeiche(gleis_id, steuerung),
            )),
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L: Leiter, S> From<lizenzen::Nachricht>
    for modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>
{
    fn from(nachricht: lizenzen::Nachricht) -> Self {
        use lizenzen::Nachricht::Schließen;
        match nachricht {
            Schließen => modal::Nachricht::VersteckeOverlay,
        }
    }
}

impl<L, S> Nachricht<L, S>
where
    L: 'static + Leiter + Send,
    <L as Leiter>::Fahrtrichtung: Send,
    S: 'static + Send,
{
    /// Konvertiere eine Nachricht in ein [`Command`].
    pub(crate) fn als_command(self) -> Command<Nachricht<L, S>> {
        Command::perform(async { identity(self) }, identity)
    }
}
