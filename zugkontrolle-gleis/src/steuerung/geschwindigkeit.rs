//! Einstellen der Geschwindigkeit.

use std::{
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    marker::PhantomData,
    ops::DerefMut as _,
    sync::{Arc, mpsc::Sender},
    thread::{JoinHandle, sleep},
    time::Duration,
};

use log::{debug, error};
use nonempty::NonEmpty;
use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    Lager, OutputAnschluss, OutputSerialisiert,
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    pin::pwm,
    polarität::{Fließend, Polarität},
};
use zugkontrolle_util::{
    eingeschränkt::{NichtNegativ, NullBisEins},
    void::Void,
};

use crate::steuerung::plan::async_ausführen;

/// Name einer [`Geschwindigkeit`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, formatter)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LeiterVariante {
    Mittelleiter,
    Zweileiter,
}

impl LeiterVariante {
    const MITTELLEITER: &str = "Mittelleiter";
    const ZWEILEITER: &str = "Zweileiter";
}

impl Display for LeiterVariante {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mittelleiter => fmt.write_str(LeiterVariante::MITTELLEITER),
            Self::Zweileiter => fmt.write_str(LeiterVariante::ZWEILEITER),
        }
    }
}

impl<'t> TryFrom<&'t str> for LeiterVariante {
    type Error = &'t str;

    fn try_from(value: &'t str) -> Result<Self, Self::Error> {
        match value {
            LeiterVariante::MITTELLEITER => Ok(LeiterVariante::Mittelleiter),
            LeiterVariante::ZWEILEITER => Ok(LeiterVariante::Zweileiter),
            _ => Err(value),
        }
    }
}

impl TryFrom<String> for LeiterVariante {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            LeiterVariante::MITTELLEITER => Ok(LeiterVariante::Mittelleiter),
            LeiterVariante::ZWEILEITER => Ok(LeiterVariante::Zweileiter),
            _ => Err(value),
        }
    }
}

#[derive(Debug)]
pub enum Leiter {
    Mittelleiter(Mittelleiter),
    Zweileiter(Zweileiter),
}

impl Display for Leiter {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mittelleiter(mittelleiter) => <Mittelleiter as Display>::fmt(mittelleiter, fmt),
            Self::Zweileiter(zweileiter) => <Zweileiter as Display>::fmt(zweileiter, fmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LeiterSerialisiert {
    Mittelleiter(MittelleiterSerialisiert),
    Zweileiter(ZweileiterSerialisiert),
}

impl Display for LeiterSerialisiert {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LeiterSerialisiert::Mittelleiter(mittelleiter_serialisiert) => {
                <MittelleiterSerialisiert as Display>::fmt(mittelleiter_serialisiert, formatter)
            },
            LeiterSerialisiert::Zweileiter(zweileiter_serialisiert) => {
                <ZweileiterSerialisiert as Display>::fmt(zweileiter_serialisiert, formatter)
            },
        }
    }
}

impl Serialisiere<LeiterSerialisiert> for Leiter {
    fn serialisiere(&self) -> LeiterSerialisiert {
        match self {
            Leiter::Mittelleiter(mittelleiter) => {
                LeiterSerialisiert::Mittelleiter(mittelleiter.serialisiere())
            },
            Leiter::Zweileiter(zweileiter) => {
                LeiterSerialisiert::Zweileiter(zweileiter.serialisiere())
            },
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.anschlüsse(),
            Leiter::Zweileiter(zweileiter) => zweileiter.anschlüsse(),
        }
    }
}

impl Reserviere<Leiter> for LeiterSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Leiter> {
        match self {
            LeiterSerialisiert::Mittelleiter(mittelleiter_serialisiert) => {
                mittelleiter_serialisiert
                    .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
                    .konvertiere(Leiter::Mittelleiter)
            },
            LeiterSerialisiert::Zweileiter(zweileiter_serialisiert) => zweileiter_serialisiert
                .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
                .konvertiere(Leiter::Zweileiter),
        }
    }
}

/// Drehe alle Züge auf einem aktiven [`Mittelleiter`]-Gleis um.
macro_rules! umdrehen_mittelleiter {
    (
        $self: ident $(=> $method: ident)*,
        $pwm_frequenz: expr,
        $verhältnis_fahrspannung_überspannung: expr,
        $stopp_zeit: expr,
        $umdrehen_zeit: expr
        $(, $aktualisieren: expr)?
    ) => {{
        $self $(.$method())* .geschwindigkeit(0, $pwm_frequenz, $verhältnis_fahrspannung_überspannung)?;
        $(
            if let Some(aktualisieren) = $aktualisieren {
                aktualisieren()
            }
        )?
        sleep($stopp_zeit);
        match $self $(.$method())* {
            Mittelleiter::Pwm { pin, letzter_wert: _, polarität } => {
                pin.aktiviere_mit_konfiguration(pwm::Konfiguration {
                    polarität: *polarität,
                    zeit: pwm::Zeit { frequenz: $pwm_frequenz, betriebszyklus: NullBisEins::MAX },
                })?;
                sleep($umdrehen_zeit);
                pin.deaktiviere()?
            },
            Mittelleiter::KonstanteSpannung { umdrehen, geschwindigkeit: _, letzter_wert: _ } => {
                umdrehen.einstellen(Fließend::Fließend)?;
                sleep($umdrehen_zeit);
                umdrehen.einstellen(Fließend::Gesperrt)?
            },
        }
        Ok(())
    }};
}

/// Ändere die Fahrtrichtung auf einem [`Zweileiter`]-Gleis.
macro_rules! fahrtrichtung_zweileiter {
    (
        $self: ident $(=> $method: ident)*,
        $pwm_frequenz: expr,
        $stopp_zeit: expr,
        $methode: ident ( $($args: expr),* )
        $(, $aktualisieren: expr)?
    ) => {{
        $self $(.$method())* .geschwindigkeit(0, $pwm_frequenz, PhantomData)?;
        $(
            if let Some(aktualisieren) = $aktualisieren {
                aktualisieren()
            }
        )?
        sleep($stopp_zeit);
        match $self $(.$method())* {
            Zweileiter::Pwm { fahrtrichtung, geschwindigkeit: _, letzter_wert: _, polarität: _ } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, geschwindigkeit: _, letzter_wert: _ } => fahrtrichtung,
        }.$methode( $($args),* )?;
        Ok(())
    }};
}

/// Ein [`Leiter`] ermöglicht ein Einstellen der Geschwindigkeit und Umdrehen der Fahrtrichtung.
impl Leiter {
    /// Der Name des Leiters.
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            Leiter::Mittelleiter(_mittelleiter) => "Mittelleiter",
            Leiter::Zweileiter(_zweileiter) => "Zweileiter",
        }
    }

    /// Wie viele Anschlüsse zum einstellen der Geschwindigkeit wurden konfiguriert?
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.ks_länge(),
            Leiter::Zweileiter(zweileiter) => zweileiter.ks_länge(),
        }
    }

    /// Anpassen der Fahrgeschwindigkeit.
    ///
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-[`u8::MAX`]
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste).
    ///
    /// ## Errors
    ///
    /// Einstellen der Geschwindigkeit ist fehlgeschlagen.
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
    ) -> Result<(), Fehler> {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.geschwindigkeit(
                wert,
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
            ),
            Leiter::Zweileiter(zweileiter) => {
                zweileiter.geschwindigkeit(wert, pwm_frequenz, PhantomData)
            },
        }
    }

    /// Die aktuell eingestellte Fahrgeschwindigkeit.
    fn aktuelle_geschwindigkeit(&self) -> u8 {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.aktuelle_geschwindigkeit(),
            Leiter::Zweileiter(zweileiter) => zweileiter.aktuelle_geschwindigkeit(),
        }
    }

    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim umdrehen der Fahrtrichtung.
    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.umdrehen(
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
                stopp_zeit,
                umdrehen_zeit,
            ),
            Leiter::Zweileiter(zweileiter) => {
                zweileiter.umdrehen(pwm_frequenz, PhantomData, stopp_zeit, PhantomData)
            },
        }
    }

    fn mittelleiter(&mut self) -> &mut Mittelleiter {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter,
            Leiter::Zweileiter(_zweileiter) => panic!(),
        }
    }

    fn zweileiter(&mut self) -> &mut Zweileiter {
        match self {
            Leiter::Mittelleiter(_mittelleiter) => panic!(),
            Leiter::Zweileiter(zweileiter) => zweileiter,
        }
    }

    /// Umdrehen der aktuellen Fahrtrichtung in einem anderen Thread.
    ///
    /// Der Mutex sollte immer nur so schnell wie möglich wieder freigegeben werden.
    ///
    /// ## Errors
    ///
    /// Fehler beim umdrehen der Fahrtrichtung.
    fn async_umdrehen_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let guard = mutex.lock();
        match &*guard {
            Leiter::Mittelleiter(_mittelleiter) => {
                drop(guard);
                umdrehen_mittelleiter!(
                    mutex=>lock=>deref_mut=>mittelleiter,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    aktualisieren
                )
            },
            Leiter::Zweileiter(_zweileiter) => {
                drop(guard);
                fahrtrichtung_zweileiter!(
                    mutex=>lock=>deref_mut=>zweileiter,
                    pwm_frequenz,
                    stopp_zeit,
                    umschalten(),
                    aktualisieren
                )
            },
        }
    }

    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim einstellen der Fahrtrichtung.
    fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.fahrtrichtung(
                neue_fahrtrichtung,
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
                stopp_zeit,
                umdrehen_zeit,
            ),
            Leiter::Zweileiter(zweileiter) => zweileiter.fahrtrichtung(
                neue_fahrtrichtung,
                pwm_frequenz,
                PhantomData,
                stopp_zeit,
                PhantomData,
            ),
        }
    }

    /// Einstellen der aktuellen Fahrtrichtung in einem anderen Thread.
    ///
    /// Der Mutex sollte immer nur so schnell wie möglich wieder freigegeben werden.
    ///
    /// ## Errors
    ///
    /// Fehler beim einstellen der Fahrtrichtung.
    fn async_fahrtrichtung_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let guard = mutex.lock();
        match &*guard {
            Leiter::Mittelleiter(_mittelleiter) => {
                drop(guard);
                umdrehen_mittelleiter!(
                    mutex=>lock=>deref_mut=>mittelleiter,
                    pwm_frequenz,
                    verhältnis_fahrspannung_überspannung,
                    stopp_zeit,
                    umdrehen_zeit,
                    aktualisieren
                )
            },
            Leiter::Zweileiter(_zweileiter) => {
                drop(guard);
                fahrtrichtung_zweileiter!(
                    mutex=>lock=>deref_mut=>zweileiter,
                    pwm_frequenz,
                    stopp_zeit,
                    einstellen(neue_fahrtrichtung.into()),
                    aktualisieren
                )
            },
        }
    }

    /// Die aktuell eingestellte Fahrtrichtung, falls bekannt.
    fn aktuelle_fahrtrichtung(&self) -> Option<Fahrtrichtung> {
        match self {
            Leiter::Mittelleiter(mittelleiter) => mittelleiter.aktuelle_fahrtrichtung(),
            Leiter::Zweileiter(zweileiter) => zweileiter.aktuelle_fahrtrichtung(),
        }
    }
}

/// Einstellen der Fahrgeschwindigkeit und Fahrtrichtung.
#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Geschwindigkeit {
    /// Die Anschlüsse zur Steuerung abhängig von der Leiter-Art.
    leiter: Arc<Mutex<Leiter>>,
}

impl Display for Geschwindigkeit {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&*self.lock_leiter(), formatter)
    }
}

impl Geschwindigkeit {
    /// Erstelle eine neue [`Geschwindigkeit`].
    #[must_use]
    pub fn neu(leiter: Leiter) -> Self {
        Geschwindigkeit { leiter: Arc::new(Mutex::new(leiter)) }
    }

    #[must_use]
    pub fn leiter_variante(&self) -> LeiterVariante {
        match &*self.lock_leiter() {
            Leiter::Mittelleiter(_mittelleiter) => LeiterVariante::Mittelleiter,
            Leiter::Zweileiter(_zweileiter) => LeiterVariante::Zweileiter,
        }
    }

    /// Helper-Methode um Zugriff auf die Anschlüsse zu bekommen.
    ///
    /// Blockiert solange, bis der Zugriff erhalten wurde.
    pub(crate) fn lock_leiter(&self) -> MutexGuard<'_, Leiter> {
        self.leiter.lock()
    }

    /// Wie viele Anschlüsse zum einstellen der Geschwindigkeit wurden konfiguriert?
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        self.lock_leiter().ks_länge()
    }
}

/// Verwende die `erzeuge_aktualisieren_nachricht`-closure und erzeuge eine neue closure,
/// bei der das Ergebnis über den `sender`geschickt wird.
macro_rules! sende_aktualisieren_nachricht {
    ($sender: expr, $erzeuge_aktualisieren_nachricht: expr) => {{
        let sender_clone = $sender.clone();
        let erzeuge_aktualisieren_nachricht_clone = $erzeuge_aktualisieren_nachricht.clone();
        erzeuge_aktualisieren_nachricht_clone.map(|erzeuge_nachricht| {
            move || {
                if let Err(fehler) = sender_clone.send(erzeuge_nachricht()) {
                    debug!("Kein Empfänger für Aktualisieren-Nachricht bei Umdrehen einer Geschwindigkeit: {fehler:?}");
                }
            }
        })
    }}
}

/// Notwendige Informationen zum asynchronen Ausführen einer Aktion.
#[derive(Debug, Clone)]
pub struct AsyncAktion<Nachricht, AktualisierenFn, FehlerFn> {
    /// Sender zum schicken der Nachricht.
    pub sender: Sender<Nachricht>,
    /// Erzeuge eine Nachricht, um auf eine Änderung zu reagieren.
    ///
    /// Bei [`None`] wird keine Nachricht erzeugt.
    pub erzeuge_aktualisieren_nachricht: Option<AktualisierenFn>,
    /// Erzeuge eine Nachricht, um auf einen Fehler zu reagieren.
    pub erzeuge_fehler_nachricht: FehlerFn,
}

impl Geschwindigkeit {
    /// Anpassen der Fahrgeschwindigkeit.
    ///
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-[`u8::MAX`]
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste).
    ///
    /// ## Errors
    ///
    /// [`Leiter::geschwindigkeit`].
    pub fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
    ) -> Result<(), Fehler> {
        self.lock_leiter().geschwindigkeit(wert, pwm_frequenz, verhältnis_fahrspannung_überspannung)
    }

    /// Die aktuell eingestellte Fahrgeschwindigkeit.
    #[must_use]
    pub fn aktuelle_geschwindigkeit(&self) -> u8 {
        self.lock_leiter().aktuelle_geschwindigkeit()
    }

    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// [`Leiter::umdrehen`].
    pub fn umdrehen_allgemein(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        self.lock_leiter().umdrehen(
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    /// Erstelle einen neuen Thread zum Umdrehen der aktuellen Fahrtrichtung.
    pub fn async_umdrehen_allgemein<Nachricht: Send + 'static>(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
        async_aktion: AsyncAktion<
            Nachricht,
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
            impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
        >,
    ) -> JoinHandle<()> {
        let AsyncAktion { sender, erzeuge_aktualisieren_nachricht, erzeuge_fehler_nachricht } =
            async_aktion;
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_umdrehen_allgemein_aux = Leiter::async_umdrehen_allgemein_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |leiter, fehler| erzeuge_fehler_nachricht(Geschwindigkeit { leiter }, fehler),
            "einer Geschwindigkeit",
            async_umdrehen_allgemein_aux(
                self.leiter,
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
                stopp_zeit,
                umdrehen_zeit,
                sende_aktualisieren_nachricht,
            )
        )
    }

    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// [`Leiter::fahrtrichtung`].
    pub fn fahrtrichtung_allgemein(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        self.lock_leiter().fahrtrichtung(
            neue_fahrtrichtung,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    /// Erstelle einen neuen Thread zum einstellen der aktuellen Fahrtrichtung.
    pub fn async_fahrtrichtung_allgemein<Nachricht: Send + 'static>(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
        async_aktion: AsyncAktion<
            Nachricht,
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
            impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
        >,
    ) -> JoinHandle<()> {
        let AsyncAktion { sender, erzeuge_aktualisieren_nachricht, erzeuge_fehler_nachricht } =
            async_aktion;
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_fahrtrichtung_allgemein_aux = Leiter::async_fahrtrichtung_allgemein_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |leiter, fehler| erzeuge_fehler_nachricht(Geschwindigkeit { leiter }, fehler),
            "einer Geschwindigkeit",
            async_fahrtrichtung_allgemein_aux(
                self.leiter,
                neue_fahrtrichtung,
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
                stopp_zeit,
                umdrehen_zeit,
                sende_aktualisieren_nachricht,
            )
        )
    }

    /// Die aktuell eingestellte Fahrtrichtung, falls bekannt.
    #[must_use]
    pub fn aktuelle_fahrtrichtung(&self) -> Option<Fahrtrichtung> {
        self.lock_leiter().aktuelle_fahrtrichtung()
    }
}

/// FIXME ist das LeiterSerialisiert-enum ein breaking change für serialisierte Darstellung?
#[expect(
    clippy::module_name_repetitions,
    reason = "Folgt der allgemeinen Konvention TypName -> TypNameSerialisiert"
)]
/// Serialisierbare Repräsentation einer [`Geschwindigkeit`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GeschwindigkeitSerialisiert {
    /// Der Leiter der Geschwindigkeit.
    pub leiter: LeiterSerialisiert,
}

impl Display for GeschwindigkeitSerialisiert {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.leiter, formatter)
    }
}

impl Serialisiere<GeschwindigkeitSerialisiert> for Geschwindigkeit {
    fn serialisiere(&self) -> GeschwindigkeitSerialisiert {
        GeschwindigkeitSerialisiert { leiter: self.lock_leiter().serialisiere() }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match Arc::try_unwrap(self.leiter) {
            Ok(mutex) => mutex.into_inner().anschlüsse(),
            Err(_arc) => {
                // while-Schleife (mit thread::yield bei Err) bis nur noch eine Arc-Referenz besteht
                // (Ok wird zurückgegeben) wäre möglich, kann aber zur nicht-Terminierung führen
                // Gebe stattdessen keine Anschlüsse zurück
                Anschlüsse::default()
            },
        }
    }
}

impl Reserviere<Geschwindigkeit> for GeschwindigkeitSerialisiert {
    type MoveArg = <LeiterSerialisiert as Reserviere<Leiter>>::MoveArg;
    type RefArg = <LeiterSerialisiert as Reserviere<Leiter>>::RefArg;
    type MutRefArg = <LeiterSerialisiert as Reserviere<Leiter>>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Geschwindigkeit> {
        self.leiter
            .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
            .konvertiere(Geschwindigkeit::neu)
    }
}

/// Stelle eine durch einen [`pwm::Pin`] gesteuerte [`Geschwindigkeit`] ein.
fn geschwindigkeit_pwm(
    pin: &mut pwm::Pin,
    letzter_wert: &mut u8,
    wert: u8,
    frequenz: NichtNegativ,
    faktor: NullBisEins,
    polarität: Polarität,
) -> Result<(), pwm::Fehler> {
    // 0 <= u8 / u8::MAX <= 1
    let verhältnis = NullBisEins::neu_unchecked(f64::from(wert) / f64::from(u8::MAX));
    #[expect(clippy::arithmetic_side_effects, reason = "saturating Mult-Implementierung.")]
    let betriebszyklus = faktor * verhältnis;
    pin.aktiviere_mit_konfiguration(pwm::Konfiguration {
        polarität,
        zeit: pwm::Zeit { frequenz, betriebszyklus },
    })?;
    *letzter_wert = wert;
    Ok(())
}

/// Stelle eine durch mehrere [Anschlüsse](crate::anschluss::Anschluss) gesteuerte [`Geschwindigkeit`] ein.
fn geschwindigkeit_ks(
    geschwindigkeit: &mut NonEmpty<OutputAnschluss>,
    letzter_wert: &mut u8,
    wert: u8,
) -> Result<(), Fehler> {
    let wert_usize = usize::from(wert);
    let length = geschwindigkeit.len();
    if wert_usize > length {
        return Err(Fehler::ZuWenigAnschlüsse { geschwindigkeit: wert, vorhanden: length });
    }
    // aktuellen Anschluss ausstellen
    if *letzter_wert == 0 {
        // Geschwindigkeit war aus, es muss also kein Anschluss ausgeschaltet werden
    } else if let Some(anschluss) = geschwindigkeit.get_mut(usize::from(
        #[expect(clippy::arithmetic_side_effects, reason = "*letzter_wert > 0")]
        {
            *letzter_wert - 1
        },
    )) {
        anschluss.einstellen(Fließend::Gesperrt)?;
    } else {
        error!(
            "Letzter Wert ist {letzter_wert}, Geschwindigkeit hat aber nur {length} Anschlüsse!"
        );
    }
    // neuen anstellen
    if wert_usize > 0 {
        #[expect(clippy::arithmetic_side_effects, reason = "*wert_usize > 0")]
        let anschluss_index = wert_usize - 1;
        if let Some(anschluss) = geschwindigkeit.get_mut(anschluss_index) {
            anschluss.einstellen(Fließend::Fließend)?;
        } else {
            error!(
                "{wert_usize} <= {length}, aber geschwindigkeit.get_mut({anschluss_index}) == None"
            );
            return Err(Fehler::ZuWenigAnschlüsse { geschwindigkeit: wert, vorhanden: length });
        }
    }
    *letzter_wert = wert;
    Ok(())
}

/// Antrieb über einen Mittelleiter.
#[derive(Debug)]
pub enum Mittelleiter {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        pin: pwm::Pin,
        /// Der letzte eingestellte Wert.
        letzter_wert: u8,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputAnschluss>,
        /// Der letzte eingestellte Wert.
        letzter_wert: u8,
        /// Der Anschluss mit Überspannung zum Umdrehen der Fahrtrichtung.
        umdrehen: OutputAnschluss,
    },
}

impl Display for Mittelleiter {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Mittelleiter::Pwm { pin, letzter_wert: _, polarität } => {
                write!(formatter, "Pwm({}, {polarität})", pin.pin())
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                formatter.write_str("KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        formatter.write_str(", ")?;
                    }
                    write!(formatter, "{anschluss}")?;
                }
                write!(formatter, "-{umdrehen})")
            },
        }
    }
}

/// Serialisierbare Repräsentation eines [`Mittelleiters`](Mittelleiter).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MittelleiterSerialisiert {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        pin: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputSerialisiert>,
        /// Der Anschluss mit Überspannung zum Umdrehen der Fahrtrichtung.
        umdrehen: OutputSerialisiert,
    },
}

impl Default for MittelleiterSerialisiert {
    fn default() -> Self {
        MittelleiterSerialisiert::Pwm {
            pin: pwm::Serialisiert::default(),
            polarität: Polarität::default(),
        }
    }
}

impl Display for MittelleiterSerialisiert {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MittelleiterSerialisiert::Pwm { pin, polarität } => {
                write!(formatter, "Pwm({}, {polarität})", pin.0)
            },
            MittelleiterSerialisiert::KonstanteSpannung { geschwindigkeit, umdrehen } => {
                formatter.write_str("KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        formatter.write_str(", ")?;
                    }
                    write!(formatter, "{anschluss}")?;
                }
                write!(formatter, "-{umdrehen})")
            },
        }
    }
}

impl Serialisiere<MittelleiterSerialisiert> for Mittelleiter {
    fn serialisiere(&self) -> MittelleiterSerialisiert {
        match self {
            Mittelleiter::Pwm { pin, letzter_wert: _, polarität } => {
                MittelleiterSerialisiert::Pwm { pin: pin.serialisiere(), polarität: *polarität }
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                MittelleiterSerialisiert::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                    umdrehen: umdrehen.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match self {
            Mittelleiter::Pwm { pin, letzter_wert: _, polarität: _ } => pin.anschlüsse(),
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                let mut anschlüsse = geschwindigkeit.anschlüsse();
                anschlüsse.anhängen(umdrehen.anschlüsse());
                anschlüsse
            },
        }
    }
}

impl Reserviere<Mittelleiter> for MittelleiterSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Mittelleiter> {
        match self {
            MittelleiterSerialisiert::Pwm { pin: serialisiert, polarität } => serialisiert
                .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
                .konvertiere(|pin| Mittelleiter::Pwm { pin, letzter_wert: 0, polarität }),
            MittelleiterSerialisiert::KonstanteSpannung { geschwindigkeit, umdrehen } => {
                let geschwindigkeit =
                    geschwindigkeit.reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg);
                geschwindigkeit.reserviere_ebenfalls_mit(
                    lager,
                    umdrehen,
                    move_arg,
                    ref_arg,
                    mut_ref_arg,
                    |geschwindigkeit, umdrehen| Mittelleiter::KonstanteSpannung {
                        geschwindigkeit,
                        letzter_wert: 0,
                        umdrehen,
                    },
                    |_| None,
                )
            },
        }
    }
}

impl Mittelleiter {
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
    ) -> Result<(), Fehler> {
        match self {
            Mittelleiter::Pwm { pin, letzter_wert, polarität } => geschwindigkeit_pwm(
                pin,
                letzter_wert,
                wert,
                pwm_frequenz,
                verhältnis_fahrspannung_überspannung,
                *polarität,
            )
            .map_err(Fehler::from),
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, umdrehen: _ } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    fn aktuelle_geschwindigkeit(&self) -> u8 {
        match self {
            Mittelleiter::Pwm { letzter_wert, pin: _, polarität: _ }
            | Mittelleiter::KonstanteSpannung { letzter_wert, geschwindigkeit: _, umdrehen: _ } => {
                *letzter_wert
            },
        }
    }

    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        umdrehen_mittelleiter!(
            self,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit
        )
    }

    fn async_umdrehen_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: NullBisEins,
        stopp_zeit: Duration,
        umdrehen_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        umdrehen_mittelleiter!(
            mutex=>lock=>deref_mut,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            aktualisieren
        )
    }

    fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        _pwm_frequenz: NichtNegativ,
        _verhältnis_fahrspannung_überspannung: NullBisEins,
        _stopp_zeit: Duration,
        _umdrehen_zeit: Duration,
    ) -> Result<(), Fehler> {
        // neue_fahrtrichtung.unreachable()
        todo!()
    }

    fn async_fahrtrichtung_allgemein_aux(
        _mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: Fahrtrichtung,
        _pwm_frequenz: NichtNegativ,
        _verhältnis_fahrspannung_überspannung: NullBisEins,
        _stopp_zeit: Duration,
        _umdrehen_zeit: Duration,
        _aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        // neue_fahrtrichtung.unreachable()
        todo!()
    }

    fn aktuelle_fahrtrichtung(&self) -> Option<Fahrtrichtung> {
        None
    }

    /// Wie viele Anschlüsse zum einstellen der Geschwindigkeit wurden konfiguriert?
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        match self {
            Mittelleiter::Pwm { pin: _, letzter_wert: _, polarität: _ } => None,
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen: _ } => {
                Some(geschwindigkeit.len())
            },
        }
    }
}

/// Antrieb über Spannungsunterschied zwischen linker und rechter Schiene.
#[derive(Debug)]
pub enum Zweileiter {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        geschwindigkeit: pwm::Pin,
        /// Der letzte eingestellte Wert.
        letzter_wert: u8,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputAnschluss,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputAnschluss>,
        /// Der letzte eingestellte Wert.
        letzter_wert: u8,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputAnschluss,
    },
}

impl Display for Zweileiter {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Zweileiter::Pwm { geschwindigkeit, letzter_wert: _, polarität, fahrtrichtung } => {
                write!(formatter, "Pwm({}, {polarität}-{fahrtrichtung})", geschwindigkeit.pin())
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                formatter.write_str("KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        formatter.write_str(", ")?;
                    }
                    write!(formatter, "{anschluss}")?;
                }
                write!(formatter, "-{fahrtrichtung})")
            },
        }
    }
}

impl Zweileiter {
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        PhantomData: PhantomData<NullBisEins>,
    ) -> Result<(), Fehler> {
        match self {
            Zweileiter::Pwm { geschwindigkeit, letzter_wert, polarität, fahrtrichtung: _ } => {
                geschwindigkeit_pwm(
                    geschwindigkeit,
                    letzter_wert,
                    wert,
                    pwm_frequenz,
                    NullBisEins::MAX,
                    *polarität,
                )
                .map_err(Fehler::from)
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, fahrtrichtung: _ } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    fn aktuelle_geschwindigkeit(&self) -> u8 {
        match self {
            Zweileiter::Pwm {
                letzter_wert,
                geschwindigkeit: _,
                polarität: _,
                fahrtrichtung: _,
            }
            | Zweileiter::KonstanteSpannung {
                letzter_wert,
                geschwindigkeit: _,
                fahrtrichtung: _,
            } => *letzter_wert,
        }
    }

    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        PhantomData: PhantomData<NullBisEins>,
        stopp_zeit: Duration,
        PhantomData: PhantomData<Duration>,
    ) -> Result<(), Fehler> {
        self.umdrehen_impl(pwm_frequenz, stopp_zeit)
    }

    fn async_umdrehen_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        pwm_frequenz: NichtNegativ,
        PhantomData: PhantomData<NullBisEins>,
        stopp_zeit: Duration,
        PhantomData: PhantomData<Duration>,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        Self::async_umdrehen_aux(mutex, pwm_frequenz, stopp_zeit, aktualisieren)
    }

    fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        PhantomData: PhantomData<NullBisEins>,
        stopp_zeit: Duration,
        PhantomData: PhantomData<Duration>,
    ) -> Result<(), Fehler> {
        self.fahrtrichtung_impl(neue_fahrtrichtung, pwm_frequenz, stopp_zeit)
    }

    fn async_fahrtrichtung_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        PhantomData: PhantomData<NullBisEins>,
        stopp_zeit: Duration,
        PhantomData: PhantomData<Duration>,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        Self::async_fahrtrichtung_aux(
            mutex,
            neue_fahrtrichtung,
            pwm_frequenz,
            stopp_zeit,
            aktualisieren,
        )
    }

    fn aktuelle_fahrtrichtung(&self) -> Option<Fahrtrichtung> {
        let anschluss = match self {
            Zweileiter::Pwm {
                fahrtrichtung,
                geschwindigkeit: _,
                letzter_wert: _,
                polarität: _,
            }
            | Zweileiter::KonstanteSpannung {
                fahrtrichtung,
                geschwindigkeit: _,
                letzter_wert: _,
            } => fahrtrichtung,
        };
        Some(anschluss.fließend().into())
    }
}

impl Zweileiter {
    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Ändern der Polarität der Geschwindigkeits-Spannung.
    fn umdrehen_impl(
        &mut self,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
    ) -> Result<(), Fehler> {
        fahrtrichtung_zweileiter!(self, pwm_frequenz, stopp_zeit, umschalten())
    }

    /// Helper-Methode für das Ändern der aktuellen Fahrtrichtung in einem asynchronem Kontext.
    ///
    /// ## Errors
    ///
    /// Fehler beim Ändern der Polarität der Geschwindigkeits-Spannung.
    fn async_umdrehen_aux(
        mutex: &Arc<Mutex<Self>>,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        fahrtrichtung_zweileiter!(
            mutex=>lock=>deref_mut,
            pwm_frequenz,
            stopp_zeit,
            umschalten(),
            aktualisieren
        )
    }

    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Einstellen der Polarität der Geschwindigkeits-Spannung.
    fn fahrtrichtung_impl(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
    ) -> Result<(), Fehler> {
        fahrtrichtung_zweileiter!(
            self,
            pwm_frequenz,
            stopp_zeit,
            einstellen(neue_fahrtrichtung.into())
        )
    }

    /// Helper-Methode für das Einstellen der aktuellen Fahrtrichtung in einem asynchronem Kontext.
    ///
    /// ## Errors
    ///
    /// Fehler beim Einstellen der Polarität der Geschwindigkeits-Spannung.
    fn async_fahrtrichtung_aux(
        mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        fahrtrichtung_zweileiter!(
            mutex=>lock=>deref_mut,
            pwm_frequenz,
            stopp_zeit,
            einstellen(neue_fahrtrichtung.into()),
            aktualisieren
        )
    }

    /// Wie viele Anschlüsse zum einstellen der Geschwindigkeit wurden konfiguriert?
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        match self {
            Zweileiter::Pwm {
                geschwindigkeit: _,
                letzter_wert: _,
                polarität: _,
                fahrtrichtung: _,
            } => None,
            Zweileiter::KonstanteSpannung {
                geschwindigkeit,
                letzter_wert: _,
                fahrtrichtung: _,
            } => Some(geschwindigkeit.len()),
        }
    }
}

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ZweileiterSerialisiert {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        geschwindigkeit: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputSerialisiert>,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
}

impl Default for ZweileiterSerialisiert {
    fn default() -> Self {
        ZweileiterSerialisiert::Pwm {
            geschwindigkeit: pwm::Serialisiert::default(),
            polarität: Polarität::default(),
            fahrtrichtung: OutputSerialisiert::default(),
        }
    }
}

impl Display for ZweileiterSerialisiert {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ZweileiterSerialisiert::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                write!(formatter, "Pwm({}, {polarität}-{fahrtrichtung})", geschwindigkeit.0)
            },
            ZweileiterSerialisiert::KonstanteSpannung { geschwindigkeit, fahrtrichtung } => {
                formatter.write_str("KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        formatter.write_str(", ")?;
                    }
                    write!(formatter, "{anschluss}")?;
                }
                write!(formatter, "-{fahrtrichtung})")
            },
        }
    }
}

impl Serialisiere<ZweileiterSerialisiert> for Zweileiter {
    fn serialisiere(&self) -> ZweileiterSerialisiert {
        match self {
            Zweileiter::Pwm { geschwindigkeit, letzter_wert: _, polarität, fahrtrichtung } => {
                ZweileiterSerialisiert::Pwm {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                    polarität: *polarität,
                    fahrtrichtung: fahrtrichtung.serialisiere(),
                }
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                ZweileiterSerialisiert::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                    fahrtrichtung: fahrtrichtung.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match self {
            Zweileiter::Pwm { geschwindigkeit, letzter_wert: _, polarität: _, fahrtrichtung } => {
                let mut anschlüsse = geschwindigkeit.anschlüsse();
                anschlüsse.anhängen(fahrtrichtung.anschlüsse());
                anschlüsse
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                let mut anschlüsse = geschwindigkeit.anschlüsse();
                anschlüsse.anhängen(fahrtrichtung.anschlüsse());
                anschlüsse
            },
        }
    }
}

impl Reserviere<Zweileiter> for ZweileiterSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Zweileiter> {
        match self {
            ZweileiterSerialisiert::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                let geschwindigkeit =
                    geschwindigkeit.reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg);
                geschwindigkeit.reserviere_ebenfalls_mit(
                    lager,
                    fahrtrichtung,
                    move_arg,
                    ref_arg,
                    mut_ref_arg,
                    |geschwindigkeit, fahrtrichtung| Zweileiter::Pwm {
                        geschwindigkeit,
                        letzter_wert: 0,
                        polarität,
                        fahrtrichtung,
                    },
                    |_| None,
                )
            },
            ZweileiterSerialisiert::KonstanteSpannung { geschwindigkeit, fahrtrichtung } => {
                let geschwindigkeit =
                    geschwindigkeit.reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg);
                geschwindigkeit.reserviere_ebenfalls_mit(
                    lager,
                    fahrtrichtung,
                    move_arg,
                    ref_arg,
                    mut_ref_arg,
                    |geschwindigkeit, fahrtrichtung| Zweileiter::KonstanteSpannung {
                        geschwindigkeit,
                        letzter_wert: 0,
                        fahrtrichtung,
                    },
                    |_| None,
                )
            },
        }
    }
}

/// Die Fahrtrichtung für Züge auf den zugehörigen Gleisen.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Fahrtrichtung {
    /// Fahren mit Triebwagen vorne.
    Vorwärts,
    /// Fahren mit Triebwagen hinten.
    Rückwärts,
}

impl From<Fahrtrichtung> for Fließend {
    fn from(fahrtrichtung: Fahrtrichtung) -> Self {
        match fahrtrichtung {
            Fahrtrichtung::Vorwärts => Fließend::Fließend,
            Fahrtrichtung::Rückwärts => Fließend::Gesperrt,
        }
    }
}

impl From<Fließend> for Fahrtrichtung {
    fn from(fließend: Fließend) -> Self {
        match fließend {
            Fließend::Fließend => Fahrtrichtung::Vorwärts,
            Fließend::Gesperrt => Fahrtrichtung::Rückwärts,
        }
    }
}

impl Display for Fahrtrichtung {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{}",
            match self {
                Fahrtrichtung::Vorwärts => "Vorwärts",
                Fahrtrichtung::Rückwärts => "Rückwärts",
            }
        )
    }
}

/// Mögliche Fehler beim einstellen der Geschwindigkeit oder Fahrtrichtung.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum Fehler {
    /// Fehler bei Interaktion mit einem [`Anschluss`](OutputAnschluss).
    Anschluss(zugkontrolle_anschluss::Fehler),
    /// Fehler bei Interaktion mit einem [`Pwm-Pin`](pwm::Pin).
    Pwm(pwm::Fehler),
    /// Zu wenige Anschlüsse für die gewünschte Geschwindigkeit.
    ZuWenigAnschlüsse {
        /// Die gewünschte Geschwindigkeit.
        geschwindigkeit: u8,
        /// Die Anzahl konfigurierter Anschlüsse.
        vorhanden: usize,
    },
}
