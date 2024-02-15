//! Einstellen der Geschwindigkeit.

use std::{
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    marker::PhantomData,
    ops::DerefMut,
    sync::{mpsc::Sender, Arc},
    thread::{sleep, JoinHandle},
    time::Duration,
    usize,
};

use log::{debug, error};
use nonempty::NonEmpty;
use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    pin::pwm,
    polarität::{Fließend, Polarität},
    Lager, OutputAnschluss, OutputSerialisiert,
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

/// Ein [`Leiter`] ermöglicht ein Einstellen der Geschwindigkeit und Umdrehen der Fahrtrichtung.
pub trait Leiter {
    /// Wie lange ist die Überspannung beim Umdrehen [`Fließend`](Fließend::Fließend).
    type UmdrehenZeit: Clone;

    /// Was ist das Verhältnis von Fahrspannung zur Überspannung zum Umdrehen.
    type VerhältnisFahrspannungÜberspannung: Clone;

    /// Einstellmöglichkeiten der aktuellen Fahrtrichtung.
    type Fahrtrichtung;

    /// Anpassen der Fahrgeschwindigkeit.
    ///
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-[`u8::MAX`]
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste)
    ///
    /// ## Errors
    ///
    /// Einstellen der Geschwindigkeit ist fehlgeschlagen.
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
    ) -> Result<(), Fehler>;

    /// Die aktuell eingestellte Fahrgeschwindigkeit.
    fn aktuelle_geschwindigkeit(&self) -> u8;

    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim umdrehen der Fahrtrichtung.
    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: Self::UmdrehenZeit,
    ) -> Result<(), Fehler>;

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
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler>;

    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim einstellen der Fahrtrichtung.
    fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Self::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: Self::UmdrehenZeit,
    ) -> Result<(), Fehler>;

    /// Einstellen der aktuellen Fahrtrichtung in einem anderen Thread.
    ///
    /// Der Mutex sollte immer nur so schnell wie möglich wieder freigegeben werden.
    ///
    /// ## Errors
    ///
    /// Fehler beim einstellen der Fahrtrichtung.
    fn async_fahrtrichtung_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: <Self as Leiter>::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler>;

    /// Die aktuell eingestellte Fahrtrichtung, falls bekannt.
    fn aktuelle_fahrtrichtung(&self) -> Option<Self::Fahrtrichtung>;
}

/// Ein unterstützter Leiter, mit bekannten Namen.
pub trait BekannterLeiter: Leiter + Sized {
    /// Der Name des Leiters.
    const NAME: &'static str;
}

/// Einstellen der Fahrgeschwindigkeit und Fahrtrichtung.
#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Geschwindigkeit<Leiter> {
    /// Die Anschlüsse zur Steuerung abhängig von der Leiter-Art.
    leiter: Arc<Mutex<Leiter>>,
}

impl<Leiter: Display> Display for Geschwindigkeit<Leiter> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&*self.lock_leiter(), formatter)
    }
}

impl<Leiter> Geschwindigkeit<Leiter> {
    /// Erstelle eine neue [`Geschwindigkeit`].
    pub fn neu(leiter: Leiter) -> Self {
        Geschwindigkeit { leiter: Arc::new(Mutex::new(leiter)) }
    }

    /// Helper-Methode um Zugriff auf die Anschlüsse zu bekommen.
    ///
    /// Blockiert solange, bis der Zugriff erhalten wurde.
    pub(crate) fn lock_leiter(&self) -> MutexGuard<'_, Leiter> {
        self.leiter.lock()
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

impl<L: Leiter> Geschwindigkeit<L> {
    /// Anpassen der Fahrgeschwindigkeit.
    ///
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-[`u8::MAX`]
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste)
    ///
    /// ## Errors
    ///
    /// [`Leiter::geschwindigkeit`]
    pub fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: L::VerhältnisFahrspannungÜberspannung,
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
    /// [`Leiter::umdrehen`]
    pub fn umdrehen_allgemein(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        self.lock_leiter().umdrehen(
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    // TODO Beheben würde eine Typ-Anpassung erfordern, z.B. über ein Zugtyp-Argument.
    #[allow(clippy::too_many_arguments)]
    /// Erstelle einen neuen Thread zum Umdrehen der aktuellen Fahrtrichtung.
    pub fn async_umdrehen_allgemein<Nachricht: Send + 'static>(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()>
    where
        L: 'static + Send,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: Send,
        <L as Leiter>::UmdrehenZeit: Send,
    {
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_umdrehen_allgemein_aux = <L as Leiter>::async_umdrehen_allgemein_aux;
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
    /// [`Leiter::fahrtrichtung`]
    pub fn fahrtrichtung_allgemein(
        &mut self,
        neue_fahrtrichtung: <L as Leiter>::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        self.lock_leiter().fahrtrichtung(
            neue_fahrtrichtung,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    // TODO Beheben würde eine Typ-Anpassung erfordern, z.B. über ein Zugtyp-Argument.
    #[allow(clippy::too_many_arguments)]
    /// Erstelle einen neuen Thread zum einstellen der aktuellen Fahrtrichtung.
    pub fn async_fahrtrichtung_allgemein<Nachricht: Send + 'static>(
        &mut self,
        neue_fahrtrichtung: <L as Leiter>::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()>
    where
        L: 'static + Send,
        <L as Leiter>::Fahrtrichtung: Send,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: Send,
        <L as Leiter>::UmdrehenZeit: Send,
    {
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_fahrtrichtung_allgemein_aux = <L as Leiter>::async_fahrtrichtung_allgemein_aux;
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
    pub fn aktuelle_fahrtrichtung(&self) -> Option<<L as Leiter>::Fahrtrichtung> {
        self.lock_leiter().aktuelle_fahrtrichtung()
    }
}

// Folgt der allgemeinen Konvention TypName -> TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Serialisierbare Repräsentation einer [`Geschwindigkeit`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GeschwindigkeitSerialisiert<LeiterSerialisiert> {
    /// Der Leiter der Geschwindigkeit.
    pub leiter: LeiterSerialisiert,
}

impl<LeiterSerialisiert: Display> Display for GeschwindigkeitSerialisiert<LeiterSerialisiert> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.leiter, formatter)
    }
}

impl<T: Serialisiere<S>, S> Serialisiere<GeschwindigkeitSerialisiert<S>> for Geschwindigkeit<T> {
    fn serialisiere(&self) -> GeschwindigkeitSerialisiert<S> {
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

impl<T, S> Reserviere<Geschwindigkeit<T>> for GeschwindigkeitSerialisiert<S>
where
    S: Reserviere<T>,
{
    type MoveArg = <S as Reserviere<T>>::MoveArg;
    type RefArg = <S as Reserviere<T>>::RefArg;
    type MutRefArg = <S as Reserviere<T>>::MutRefArg;

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Geschwindigkeit<T>> {
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
    // saturating Mult-Implementierung.
    #[allow(clippy::arithmetic_side_effects)]
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
        // *letzter_wert > 0
        #[allow(clippy::arithmetic_side_effects)]
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
        // *wert_usize > 0
        #[allow(clippy::arithmetic_side_effects)]
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
#[allow(variant_size_differences)]
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
                    // Selber wert zu einem späteren Zeitpunkt
                    #[allow(clippy::shadow_unrelated)]
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

impl BekannterLeiter for Mittelleiter {
    const NAME: &'static str = "Mittelleiter";
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
            Mittelleiter::KonstanteSpannung { umdrehen, .. } => {
                umdrehen.einstellen(Fließend::Fließend)?;
                sleep($umdrehen_zeit);
                umdrehen.einstellen(Fließend::Gesperrt)?
            },
        }
        Ok(())
    }};
}

impl Leiter for Mittelleiter {
    type UmdrehenZeit = Duration;
    type VerhältnisFahrspannungÜberspannung = NullBisEins;
    type Fahrtrichtung = Void;

    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
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
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    fn aktuelle_geschwindigkeit(&self) -> u8 {
        match self {
            Mittelleiter::Pwm { letzter_wert, .. }
            | Mittelleiter::KonstanteSpannung { letzter_wert, .. } => *letzter_wert,
        }
    }

    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: Self::UmdrehenZeit,
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
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
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
        neue_fahrtrichtung: Self::Fahrtrichtung,
        _pwm_frequenz: NichtNegativ,
        _verhältnis_fahrspannung_überspannung: Self::VerhältnisFahrspannungÜberspannung,
        _stopp_zeit: Duration,
        _umdrehen_zeit: Self::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        neue_fahrtrichtung.unreachable()
    }

    fn async_fahrtrichtung_allgemein_aux(
        _mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: <Self as Leiter>::Fahrtrichtung,
        _pwm_frequenz: NichtNegativ,
        _verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        _stopp_zeit: Duration,
        _umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
        _aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        neue_fahrtrichtung.unreachable()
    }
    fn aktuelle_fahrtrichtung(&self) -> Option<Self::Fahrtrichtung> {
        None
    }
}

impl Geschwindigkeit<Mittelleiter> {
    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim erzeugen/ausstellen der "Überspannung" zum umdrehen.
    pub fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Mittelleiter as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Mittelleiter as Leiter>::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        self.umdrehen_allgemein(
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    /// TODO Behandeln würde eine Typ-Änderung benötigen, z.B. ein Zugtyp-Argument.
    #[allow(clippy::too_many_arguments)]
    /// Erstelle einen neuen Thread zum Umdrehen der aktuellen Fahrtrichtung.
    pub fn async_umdrehen<Nachricht: Send + 'static>(
        &mut self,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Mittelleiter as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Mittelleiter as Leiter>::UmdrehenZeit,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()> {
        self.async_umdrehen_allgemein(
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            sender,
            erzeuge_aktualisieren_nachricht,
            erzeuge_fehler_nachricht,
        )
    }

    /// Wie viele Anschlüsse zum einstellen der Geschwindigkeit wurden konfiguriert?
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        match &*self.lock_leiter() {
            Mittelleiter::Pwm { .. } => None,
            Mittelleiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.len()),
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

impl BekannterLeiter for Zweileiter {
    const NAME: &'static str = "Zweileiter";
}

impl Leiter for Zweileiter {
    type UmdrehenZeit = PhantomData<Duration>;
    type VerhältnisFahrspannungÜberspannung = PhantomData<NullBisEins>;
    type Fahrtrichtung = Fahrtrichtung;

    fn geschwindigkeit(
        &mut self,
        wert: u8,
        pwm_frequenz: NichtNegativ,
        PhantomData: Self::VerhältnisFahrspannungÜberspannung,
    ) -> Result<(), Fehler> {
        match self {
            Zweileiter::Pwm { geschwindigkeit, letzter_wert, polarität, .. } => {
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
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    fn aktuelle_geschwindigkeit(&self) -> u8 {
        match self {
            Zweileiter::Pwm { letzter_wert, .. }
            | Zweileiter::KonstanteSpannung { letzter_wert, .. } => *letzter_wert,
        }
    }

    fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        PhantomData: Self::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        PhantomData: Self::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        self.umdrehen(pwm_frequenz, stopp_zeit)
    }

    fn async_umdrehen_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        pwm_frequenz: NichtNegativ,
        PhantomData: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        PhantomData: <Self as Leiter>::UmdrehenZeit,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        Self::async_umdrehen_aux(mutex, pwm_frequenz, stopp_zeit, aktualisieren)
    }

    fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Self::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        PhantomData: Self::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        PhantomData: Self::UmdrehenZeit,
    ) -> Result<(), Fehler> {
        self.fahrtrichtung(neue_fahrtrichtung, pwm_frequenz, stopp_zeit)
    }

    fn async_fahrtrichtung_allgemein_aux(
        mutex: &Arc<Mutex<Self>>,
        neue_fahrtrichtung: <Self as Leiter>::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        PhantomData: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        PhantomData: <Self as Leiter>::UmdrehenZeit,
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

    fn aktuelle_fahrtrichtung(&self) -> Option<Self::Fahrtrichtung> {
        let anschluss = match self {
            Zweileiter::Pwm { fahrtrichtung, .. }
            | Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        };
        Some(anschluss.fließend().into())
    }
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
            Zweileiter::Pwm { fahrtrichtung, .. } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        }.$methode( $($args),* )?;
        Ok(())
    }};
}

impl Zweileiter {
    // TODO Änderung bedeutet API-Änderung, kann entfernt werden
    #[allow(clippy::same_name_method)]
    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Ändern der Polarität der Geschwindigkeits-Spannung.
    pub fn umdrehen(
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

    // TODO Änderung bedeutet API-Änderung, kann entfernt werden
    #[allow(clippy::same_name_method)]
    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Einstellen der Polarität der Geschwindigkeits-Spannung.
    pub fn fahrtrichtung(
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
}

impl Geschwindigkeit<Zweileiter> {
    /// Umdrehen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Ändern der Polarität der Geschwindigkeits-Spannung.
    pub fn umdrehen(
        &mut self,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
    ) -> Result<(), Fehler> {
        self.lock_leiter().umdrehen(pwm_frequenz, stopp_zeit)
    }

    /// Erstelle einen neuen Thread zum Umdrehen der aktuellen Fahrtrichtung.
    pub fn async_umdrehen<Nachricht: Send + 'static>(
        &mut self,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()> {
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_umdrehen_aux = Zweileiter::async_umdrehen_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |leiter, fehler| erzeuge_fehler_nachricht(Geschwindigkeit { leiter }, fehler),
            "einer Geschwindigkeit",
            async_umdrehen_aux(
                self.leiter,
                pwm_frequenz,
                stopp_zeit,
                sende_aktualisieren_nachricht
            )
        )
    }

    /// Einstellen der aktuellen Fahrtrichtung.
    ///
    /// ## Errors
    ///
    /// Fehler beim Einstellen der Polarität der Geschwindigkeits-Spannung.
    pub fn fahrtrichtung(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
    ) -> Result<(), Fehler> {
        self.lock_leiter().fahrtrichtung(neue_fahrtrichtung, pwm_frequenz, stopp_zeit)
    }

    /// Erstelle einen neuen Thread zum einstellen der aktuellen Fahrtrichtung.
    pub fn async_fahrtrichtung<Nachricht: Send + 'static>(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        stopp_zeit: Duration,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Self, Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()> {
        let sende_aktualisieren_nachricht =
            sende_aktualisieren_nachricht!(sender, erzeuge_aktualisieren_nachricht);
        let async_fahrtrichtung_aux = Zweileiter::async_fahrtrichtung_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |leiter, fehler| erzeuge_fehler_nachricht(Geschwindigkeit { leiter }, fehler),
            "einer Geschwindigkeit",
            async_fahrtrichtung_aux(
                self.leiter,
                neue_fahrtrichtung,
                pwm_frequenz,
                stopp_zeit,
                sende_aktualisieren_nachricht,
            )
        )
    }

    /// Anzahl der konfigurierten [`Anschlüsse`](crate::anschluss::Anschluss) zum einstellen der Geschwindigkeit.
    #[must_use]
    pub fn ks_länge(&self) -> Option<usize> {
        match &*self.lock_leiter() {
            Zweileiter::Pwm { .. } => None,
            Zweileiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.len()),
        }
    }
}

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[allow(variant_size_differences)]
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
                    // Gleiche Werte zu einem späteren Zeitpunkt.
                    #[allow(clippy::shadow_unrelated)]
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
                    // Gleiche Werte zu einem späteren Zeitpunkt.
                    #[allow(clippy::shadow_unrelated)]
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
#[allow(variant_size_differences)]
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
