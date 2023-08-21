//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    sync::mpsc::{RecvError, Sender},
    thread::{self, JoinHandle},
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self, de_serialisieren::Serialisiere, polarität::Fließend, OutputAnschluss,
        OutputSerialisiert,
    },
    gleis::{
        gleise::{self, steuerung::Steuerung},
        weiche,
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter},
        kontakt::{Kontakt, KontaktSerialisiert},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
        weiche::{Weiche, WeicheSerialisiert, WeicheSteuerung},
    },
    util::{eingeschränkt::NichtNegativ, nachschlagen::Nachschlagen},
    zugtyp::Zugtyp,
};

/// Name eines [Plans](Plan).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

/// Bei einer asynchronen Aktion aufgetretene Nachricht.
#[derive(Debug)]
pub enum AsyncNachricht {
    /// Aktualisiere das GUI, damit Zustands-Änderungen korrekt dargestellt werden.
    Aktualisieren,
    /// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
    Fehler {
        /// Der Titel der Fehlermeldung.
        titel: String,
        /// Die Nachricht der Fehlermeldung.
        nachricht: String,
    },
}

/// Einstellungen, die das [Ausführen] von Aktionen beeinflussen.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_clone(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Clone)]
#[zugkontrolle_clone(<L as Leiter>::UmdrehenZeit: Clone)]
pub struct Einstellungen<L: Leiter> {
    /// Frequenz in Herz für den Pwm-Antrieb.
    pub pwm_frequenz: NichtNegativ,
    /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
    pub verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    /// Zeit zum Anhalten vor dem Umdrehen.
    pub stopp_zeit: Duration,
    /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
    pub umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
    pub schalten_zeit: Duration,
}

impl<L: Leiter> From<Zugtyp<L>> for Einstellungen<L> {
    fn from(zugtyp: Zugtyp<L>) -> Self {
        let Zugtyp {
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
            ..
        } = zugtyp;
        Einstellungen {
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        }
    }
}

impl<L: Leiter> From<&Zugtyp<L>> for Einstellungen<L> {
    fn from(zugtyp: &Zugtyp<L>) -> Self {
        let Zugtyp {
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
            ..
        } = zugtyp;
        Einstellungen {
            pwm_frequenz: *pwm_frequenz,
            verhältnis_fahrspannung_überspannung: verhältnis_fahrspannung_überspannung.clone(),
            stopp_zeit: *stopp_zeit,
            umdrehen_zeit: umdrehen_zeit.clone(),
            schalten_zeit: *schalten_zeit,
        }
    }
}

/// Etwas ausführbares.
pub trait Ausführen<L: Leiter> {
    /// Mögliche Fehler, die beim ausführen auftreten können.
    type Fehler;

    /// Ausführen im aktuellen Thread. Kann den aktuellen Thread blockieren.
    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler>;

    /// Erstelle einen neuen Thread aus und führe es dort aus.
    /// Wenn ein Fehler auftritt wird dieser über den Channel gesendet.
    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()>;
}

pub(crate) fn erzeuge_aktualisieren_nachricht<Nachricht: From<AsyncNachricht>>() -> Nachricht {
    AsyncNachricht::Aktualisieren.into()
}

macro_rules! async_ausführen {
    (
        $sender: expr,
        $erzeuge_aktualisieren_nachricht: expr,
        $erzeuge_fehler_nachricht: expr,
        $aktion_beschreibung: expr,
        $funktion: ident ($self:expr $(=> $as_mut: ident)? $(, $($args: tt)*)?)
    ) => {{
        let mut clone = $self.clone();
        std::thread::spawn(move || {
            let sende_nachricht = |nachricht|
            if let Err(fehler) = $sender.send(nachricht) {
                log::error!(
                    "Kein Empfänger wartet auf die Fehlermeldung {}: {fehler}",
                    $aktion_beschreibung,
                )
            };
            #[allow(unused_mut)]
            if let Err(fehler) = $funktion(&mut clone $(.$as_mut())? $(, $($args)*)?) {
                sende_nachricht($erzeuge_fehler_nachricht(clone, fehler))
            } else if let Some(mut erzeuge_nachricht) = $erzeuge_aktualisieren_nachricht {
                sende_nachricht(erzeuge_nachricht())
            };
        })
    }}
}
pub(crate) use async_ausführen;

macro_rules! impl_ausführen_simple {
    ($type: ty, $fehler: ty, $aktion_beschreibung: expr) => {
        impl<L: Leiter> Ausführen<L> for $type {
            type Fehler = $fehler;

            fn ausführen(&mut self, _einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler> {
                self.ausführen()
            }

            fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
                &mut self,
                _einstellungen: Einstellungen<L>,
                sender: Sender<Nachricht>,
            ) -> JoinHandle<()> {
                let erzeuge_fehler_nachricht = |clone, fehler| {
                    AsyncNachricht::Fehler {
                        titel: format!("{clone:?}"),
                        nachricht: format!("{fehler:?}"),
                    }
                    .into()
                };
                let ausführen = Self::ausführen;
                async_ausführen!(
                    sender,
                    Some(erzeuge_aktualisieren_nachricht),
                    erzeuge_fehler_nachricht,
                    $aktion_beschreibung,
                    ausführen(self)
                )
            }
        }
    };
}

/// Ein Fahrplan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanEnum<Aktion> {
    /// Alle [Aktionen](Aktion) des Plans.
    pub aktionen: Vec<Aktion>,
    /// Werden die Aktionen nach ausführen der letzten wiederholt?
    pub endlosschleife: bool,
}

/// Ein Fahrplan.
pub type Plan<L> = PlanEnum<Aktion<L>>;

/// Fehler beim Ausführen eines Plans.
#[derive(Debug)]
pub struct PlanFehler {
    /// Der aufgetretene Fehler.
    pub fehler: AktionFehler,
    /// Der Index des aufgetretenen Fehlers.
    pub aktion: usize,
}

impl<L: Leiter> Ausführen<L> for Plan<L>
where
    L: 'static + Leiter + Send + Debug,
    <L as Leiter>::Fahrtrichtung: Debug + Clone + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Send,
    <L as Leiter>::UmdrehenZeit: Send,
{
    type Fehler = PlanFehler;

    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler>
    where
        L: Leiter,
    {
        let Plan { aktionen, endlosschleife } = self;
        let mut erstes = true;
        while erstes || *endlosschleife {
            erstes = false;
            for (i, aktion) in aktionen.iter_mut().enumerate() {
                aktion
                    .ausführen(einstellungen.clone())
                    .map_err(|fehler| PlanFehler { fehler, aktion: i })?;
            }
        }
        Ok(())
    }

    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()> {
        let erzeuge_fehler_nachricht = |clone, fehler| {
            AsyncNachricht::Fehler { titel: format!("{clone:?}"), nachricht: format!("{fehler:?}") }
                .into()
        };
        let ausführen = Self::ausführen;
        async_ausführen!(
            sender,
            Some(erzeuge_aktualisieren_nachricht),
            erzeuge_fehler_nachricht,
            "eines Plans",
            ausführen(self, einstellungen)
        )
    }
}

/// Serialisierbare Repräsentation eines Fahrplans.
pub type PlanSerialisiert<L, S> = PlanEnum<AktionSerialisiert<L, S>>;

#[allow(single_use_lifetimes)]
impl<L: Leiter> Plan<L> {
    /// Serialisiere einen [Plan]
    pub fn serialisiere<S>(&self) -> PlanSerialisiert<L, S>
    where
        L: Serialisiere<S>,
        <L as Leiter>::Fahrtrichtung: Clone,
        S:,
    {
        let Plan { aktionen, endlosschleife } = self;
        PlanSerialisiert {
            aktionen: aktionen.into_iter().map(Aktion::serialisiere).collect(),
            endlosschleife: *endlosschleife,
        }
    }
}

/// Serialisierbare Repräsentation der nicht bekannten Anschlüsse.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum UnbekannteAnschlüsse<S> {
    /// Anschlüsse eine [Geschwindigkeit].
    Geschwindigkeit(UnbekannteGeschwindigkeit<S>),
    /// Anschlüsse eines [Streckenabschnittes](Streckenabschnitt).
    Streckenabschnitte(UnbekannterStreckenabschnitt),
    /// Anschlüsse einer [Weiche].
    Weiche(AnyUnbekannteWeiche),
    /// Anschlüsse eines [Kontaktes](Kontakt).
    Kontakt(UnbekannterKontakt),
}

impl<L: Leiter, S: Eq + Hash> PlanSerialisiert<L, S> {
    /// Deserialisiere einen [Plan].
    pub fn deserialisiere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<S>, Geschwindigkeit<L>>,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
        sender: &Sender<Nachricht>,
    ) -> Result<Plan<L>, UnbekannteAnschlüsse<S>> {
        let PlanSerialisiert { aktionen: aktionen_serialisiert, endlosschleife } = self;
        let mut aktionen = Vec::new();
        for aktion in aktionen_serialisiert {
            let aktion = aktion.deserialisiere(
                geschwindigkeiten,
                streckenabschnitte,
                gerade_weichen,
                kurven_weichen,
                dreiwege_weichen,
                kontakte,
                sender.clone(),
            )?;
            aktionen.push(aktion);
        }
        Ok(Plan { aktionen, endlosschleife })
    }
}

/// Eine Aktionen in einem Fahrplan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionEnum<Geschwindigkeit, Streckenabschnitt, Schalten, Warten> {
    /// Eine [AktionGeschwindigkeit].
    Geschwindigkeit(Geschwindigkeit),
    /// Eine [AktionStreckenabschnitt].
    Streckenabschnitt(Streckenabschnitt),
    /// Eine [AnyAktionSchalten].
    Schalten(Schalten),
    /// Eine [AktionWarten].
    Warten(Warten),
    /// Ausführen eines [Plans](Plan).
    Ausführen(PlanEnum<Self>),
}

/// Eine Aktionen in einem Fahrplan.
pub type Aktion<L> =
    AktionEnum<AktionGeschwindigkeit<L>, AktionStreckenabschnitt, AnyAktionSchalten, AktionWarten>;

/// Ein Fehler der beim Ausführen einer [Aktion] auftreten kann.
#[derive(Debug)]
pub enum AktionFehler {
    /// Fehler beim Ausführen einer [AktionGeschwindigkeit].
    Geschwindigkeit(geschwindigkeit::Fehler),
    /// Fehler beim Ausführen einer [AktionStreckenabschnitt].
    Streckenabschnitt(anschluss::Fehler),
    /// Fehler beim Ausführen einer [AktionSchalten].
    Schalten(anschluss::Fehler),
    /// Fehler beim Ausführen einer [AktionWarten].
    Warten(RecvError),
    /// Fehler beim Ausführen eines [Plans](Plan).
    Ausführen(Box<PlanFehler>),
}

impl<L: Leiter> Ausführen<L> for Aktion<L>
where
    L: 'static + Leiter + Send + Debug,
    <L as Leiter>::Fahrtrichtung: Debug + Clone + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Send,
    <L as Leiter>::UmdrehenZeit: Send,
{
    type Fehler = AktionFehler;

    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler> {
        match self {
            Aktion::Geschwindigkeit(aktion) => {
                aktion.ausführen(einstellungen).map_err(AktionFehler::Geschwindigkeit)
            },
            Aktion::Streckenabschnitt(aktion) => {
                aktion.ausführen().map_err(AktionFehler::Streckenabschnitt)
            },
            Aktion::Schalten(aktion) => {
                aktion.ausführen(einstellungen).map_err(AktionFehler::Schalten)
            },
            Aktion::Warten(aktion) => aktion.ausführen().map_err(AktionFehler::Warten),
            Aktion::Ausführen(plan) => plan
                .ausführen(einstellungen)
                .map_err(|fehler| AktionFehler::Ausführen(Box::new(fehler))),
        }
    }

    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()> {
        match self {
            Aktion::Geschwindigkeit(aktion) => aktion.async_ausführen(einstellungen, sender),
            Aktion::Streckenabschnitt(aktion) => aktion.async_ausführen(einstellungen, sender),
            Aktion::Schalten(aktion) => aktion.async_ausführen(einstellungen, sender),
            Aktion::Warten(aktion) => aktion.async_ausführen(einstellungen, sender),
            Aktion::Ausführen(plan) => plan.async_ausführen(einstellungen, sender),
        }
    }
}

/// Serialisierbare Repräsentation einer Aktion in einem Fahrplan.
pub type AktionSerialisiert<L, S> = AktionEnum<
    AktionGeschwindigkeitSerialisiert<L, S>,
    AktionStreckenabschnittSerialisiert,
    AnyAktionSchaltenSerialisiert,
    AktionWartenSerialisiert,
>;

impl<L: Leiter> Aktion<L> {
    /// Serialisiere eine [Aktion].
    #[allow(single_use_lifetimes)]
    pub fn serialisiere<S>(&self) -> AktionSerialisiert<L, S>
    where
        L: Serialisiere<S>,
        <L as Leiter>::Fahrtrichtung: Clone,
        S:,
    {
        match self {
            Aktion::Geschwindigkeit(aktion) => {
                AktionSerialisiert::Geschwindigkeit(aktion.serialisiere())
            },
            Aktion::Streckenabschnitt(aktion) => {
                AktionSerialisiert::Streckenabschnitt(aktion.serialisiere())
            },
            Aktion::Schalten(aktion) => AktionSerialisiert::Schalten(aktion.serialisiere()),
            Aktion::Warten(aktion) => AktionSerialisiert::Warten(aktion.serialisiere()),
            Aktion::Ausführen(plan) => AktionSerialisiert::Ausführen(plan.serialisiere()),
        }
    }
}

impl<L: Leiter, S: Eq + Hash> AktionSerialisiert<L, S> {
    /// Deserialisiere eine [Aktion] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<S>, Geschwindigkeit<L>>,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
        sender: Sender<Nachricht>,
    ) -> Result<Aktion<L>, UnbekannteAnschlüsse<S>> {
        let reserviert = match self {
            AktionSerialisiert::Geschwindigkeit(aktion) => {
                Aktion::Geschwindigkeit(aktion.deserialisiere(geschwindigkeiten)?)
            },
            AktionSerialisiert::Streckenabschnitt(aktion) => {
                Aktion::Streckenabschnitt(aktion.deserialisiere(streckenabschnitte, sender)?)
            },
            AktionSerialisiert::Schalten(aktion) => Aktion::Schalten(aktion.deserialisiere(
                gerade_weichen,
                kurven_weichen,
                dreiwege_weichen,
                sender,
            )?),
            AktionSerialisiert::Warten(aktion) => {
                Aktion::Warten(aktion.deserialisiere(kontakte, sender)?)
            },
            AktionSerialisiert::Ausführen(plan) => Aktion::Ausführen(plan.deserialisiere(
                geschwindigkeiten,
                streckenabschnitte,
                gerade_weichen,
                kurven_weichen,
                dreiwege_weichen,
                kontakte,
                &sender,
            )?),
        };
        Ok(reserviert)
    }
}

macro_rules! erstelle_aktion_geschwindigkeit {
    (
        $docstring: expr,
        $type: ident < $($par: ident $(: $con0: ident $(+ $con: ident)*)?),* >,
        $geschwindigkeit: ty,
        $fahrtrichtung: ty;
        Debug => $($debug: ty),*;
        Clone => $($clone: ty),*;
        $(
            [$serialize: ident, $deserialize: ident]
            $ser: expr;
            $de: expr;
        )?
    ) => {
        #[doc = $docstring]
        #[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone $(, $serialize, $deserialize)?)]
        $(
            #[zugkontrolle_debug($debug: Debug)]
        )*
        $(
            #[zugkontrolle_clone($clone: Clone)]
        )*
        $(#[serde(bound(serialize = $ser, deserialize = $de))])?
        pub enum $type < $($par $(: $con0 $(+ $con)*)? ),* > {
            /// Einstellen der Fahrgeschwindigkeit.
            Geschwindigkeit {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
                /// Die neue Geschwindigkeit.
                wert: u8,
            },
            /// Umdrehen der Fahrtrichtung.
            Umdrehen {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
            },
            /// Einstellen der Fahrtrichtung.
            Fahrtrichtung {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
                /// Die neue Fahrtrichtung.
                fahrtrichtung: $fahrtrichtung,
            },
        }
    };
}

erstelle_aktion_geschwindigkeit! {
    "Eine Aktion mit einer [Geschwindigkeit].",
    AktionGeschwindigkeit<L: Leiter>,
    Geschwindigkeit<L>,
    <L as Leiter>::Fahrtrichtung;
    Debug => L, <L as Leiter>::Fahrtrichtung;
    Clone => <L as Leiter>::Fahrtrichtung;
}

impl<L> Ausführen<L> for AktionGeschwindigkeit<L>
where
    L: 'static + Leiter + Send + Debug,
    <L as Leiter>::Fahrtrichtung: Debug + Clone + Send,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Send,
    <L as Leiter>::UmdrehenZeit: Send,
{
    type Fehler = geschwindigkeit::Fehler;

    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler> {
        match self {
            AktionGeschwindigkeit::Geschwindigkeit { geschwindigkeit, wert } => geschwindigkeit
                .geschwindigkeit(
                    *wert,
                    einstellungen.pwm_frequenz,
                    einstellungen.verhältnis_fahrspannung_überspannung,
                ),
            AktionGeschwindigkeit::Umdrehen { geschwindigkeit } => geschwindigkeit
                .umdrehen_allgemein(
                    einstellungen.pwm_frequenz,
                    einstellungen.verhältnis_fahrspannung_überspannung,
                    einstellungen.stopp_zeit,
                    einstellungen.umdrehen_zeit,
                ),
            AktionGeschwindigkeit::Fahrtrichtung { geschwindigkeit, fahrtrichtung } => {
                geschwindigkeit.fahrtrichtung_allgemein(
                    fahrtrichtung.clone(),
                    einstellungen.pwm_frequenz,
                    einstellungen.verhältnis_fahrspannung_überspannung,
                    einstellungen.stopp_zeit,
                    einstellungen.umdrehen_zeit,
                )
            },
        }
    }

    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()> {
        let erzeuge_aktualisieren_nachricht = || AsyncNachricht::Aktualisieren.into();
        let titel = format!("{self:?}");
        let erzeuge_fehler_nachricht =
            |fehler| AsyncNachricht::Fehler { titel, nachricht: format!("{fehler:?}") }.into();
        match self {
            AktionGeschwindigkeit::Geschwindigkeit { geschwindigkeit, wert } => {
                let wert = *wert;
                let pwm_frequenz = einstellungen.pwm_frequenz;
                let verhältnis_fahrspannung_überspannung =
                    einstellungen.verhältnis_fahrspannung_überspannung.clone();
                let ausführen = Geschwindigkeit::geschwindigkeit;
                async_ausführen!(
                    sender,
                    Some(erzeuge_aktualisieren_nachricht),
                    |_clone, fehler| erzeuge_fehler_nachricht(fehler),
                    "einer Geschwindigkeit-Aktion",
                    ausführen(
                        geschwindigkeit,
                        wert,
                        pwm_frequenz,
                        verhältnis_fahrspannung_überspannung,
                    )
                )
            },
            AktionGeschwindigkeit::Umdrehen { geschwindigkeit } => geschwindigkeit
                .async_umdrehen_allgemein(
                    einstellungen.pwm_frequenz,
                    einstellungen.verhältnis_fahrspannung_überspannung,
                    einstellungen.stopp_zeit,
                    einstellungen.umdrehen_zeit,
                    sender,
                    Some(erzeuge_aktualisieren_nachricht),
                    |_clone, fehler| erzeuge_fehler_nachricht(fehler),
                ),
            AktionGeschwindigkeit::Fahrtrichtung { geschwindigkeit, fahrtrichtung } => {
                geschwindigkeit.async_fahrtrichtung_allgemein(
                    fahrtrichtung.clone(),
                    einstellungen.pwm_frequenz,
                    einstellungen.verhältnis_fahrspannung_überspannung,
                    einstellungen.stopp_zeit,
                    einstellungen.umdrehen_zeit,
                    sender,
                    Some(erzeuge_aktualisieren_nachricht),
                    |_clone, fehler| erzeuge_fehler_nachricht(fehler),
                )
            },
        }
    }
}

erstelle_aktion_geschwindigkeit! {
    "Serialisierbare Repräsentation für eine Aktion mit einer [Geschwindigkeit].",
    AktionGeschwindigkeitSerialisiert<L: Leiter, S>,
    GeschwindigkeitSerialisiert<S>,
    <L as Leiter>::Fahrtrichtung;
    Debug => S, <L as Leiter>::Fahrtrichtung;
    Clone => S, <L as Leiter>::Fahrtrichtung;
    [Serialize, Deserialize]
    "S: Serialize, <L as Leiter>::Fahrtrichtung: Serialize";
    "S: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>";
}

#[allow(single_use_lifetimes)]
impl<L: Leiter> AktionGeschwindigkeit<L> {
    /// Serialisiere eine Aktion mit einer [Geschwindigkeit].
    fn serialisiere<S>(&self) -> AktionGeschwindigkeitSerialisiert<L, S>
    where
        L: Serialisiere<S>,
        <L as Leiter>::Fahrtrichtung: Clone,
        S:,
    {
        match self {
            AktionGeschwindigkeit::Geschwindigkeit { geschwindigkeit, wert } => {
                AktionGeschwindigkeitSerialisiert::Geschwindigkeit {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                    wert: *wert,
                }
            },
            AktionGeschwindigkeit::Umdrehen { geschwindigkeit } => {
                AktionGeschwindigkeitSerialisiert::Umdrehen {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                }
            },
            AktionGeschwindigkeit::Fahrtrichtung { geschwindigkeit, fahrtrichtung } => {
                AktionGeschwindigkeitSerialisiert::Fahrtrichtung {
                    geschwindigkeit: geschwindigkeit.serialisiere(),
                    fahrtrichtung: fahrtrichtung.clone(),
                }
            },
        }
    }
}

/// Eine nicht bekannte [Geschwindigkeit] soll verwendet werden.
#[derive(Debug, Clone)]
pub struct UnbekannteGeschwindigkeit<LeiterSerialisiert>(
    pub GeschwindigkeitSerialisiert<LeiterSerialisiert>,
);

impl<L: Leiter, S: Eq + Hash> AktionGeschwindigkeitSerialisiert<L, S> {
    /// Deserialisiere eine Aktion mit einer [Geschwindigkeit] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<S>, Geschwindigkeit<L>>,
    ) -> Result<AktionGeschwindigkeit<L>, UnbekannteGeschwindigkeit<S>> {
        let aktion = match self {
            AktionGeschwindigkeitSerialisiert::Geschwindigkeit { geschwindigkeit, wert } => {
                let geschwindigkeit = geschwindigkeiten
                    .get(&geschwindigkeit)
                    .ok_or(UnbekannteGeschwindigkeit(geschwindigkeit))?
                    .clone();
                AktionGeschwindigkeit::Geschwindigkeit { geschwindigkeit, wert }
            },
            AktionGeschwindigkeitSerialisiert::Umdrehen { geschwindigkeit } => {
                let geschwindigkeit = geschwindigkeiten
                    .get(&geschwindigkeit)
                    .ok_or(UnbekannteGeschwindigkeit(geschwindigkeit))?
                    .clone();
                AktionGeschwindigkeit::Umdrehen { geschwindigkeit }
            },
            AktionGeschwindigkeitSerialisiert::Fahrtrichtung { geschwindigkeit, fahrtrichtung } => {
                let geschwindigkeit = geschwindigkeiten
                    .get(&geschwindigkeit)
                    .ok_or(UnbekannteGeschwindigkeit(geschwindigkeit))?
                    .clone();
                AktionGeschwindigkeit::Fahrtrichtung { geschwindigkeit, fahrtrichtung }
            },
        };
        Ok(aktion)
    }
}

/// Eine Aktion mit einem [Streckenabschnitt].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionStreckenabschnitt<S = Steuerung<Streckenabschnitt>> {
    /// Strom auf einem Streckenabschnitt einstellen.
    Strom {
        /// Die Anschlüsse zur Steuerung des Streckenabschnittes.
        streckenabschnitt: S,
        /// Wird der Strom an- oder abgestellt.
        fließend: Fließend,
    },
}

impl AktionStreckenabschnitt {
    fn ausführen(&mut self) -> Result<(), anschluss::Fehler> {
        let AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } = self;
        streckenabschnitt.as_mut().strom(*fließend)
    }
}

impl_ausführen_simple! {
    AktionStreckenabschnitt,
    anschluss::Fehler,
    "einer Streckenabschnitt-Aktion"
}

/// Serialisierbare Repräsentation einer Aktion mit einem [Streckenabschnitt].
pub type AktionStreckenabschnittSerialisiert =
    AktionStreckenabschnitt<StreckenabschnittSerialisiert>;

impl AktionStreckenabschnitt {
    /// Serialisiere eine Aktion mit einem [Streckenabschnitt].
    pub fn serialisiere(&self) -> AktionStreckenabschnittSerialisiert {
        match self {
            AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } => {
                AktionStreckenabschnittSerialisiert::Strom {
                    streckenabschnitt: streckenabschnitt.as_ref().serialisiere(),
                    fließend: *fließend,
                }
            },
        }
    }
}

/// Ein nicht bekannter [Streckenabschnitt] soll verwendet werden.
#[derive(Debug, Clone)]
pub struct UnbekannterStreckenabschnitt(pub StreckenabschnittSerialisiert);

impl AktionStreckenabschnittSerialisiert {
    /// Deserialisiere eine Aktion mit einem [Streckenabschnitt] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
        sender: Sender<Nachricht>,
    ) -> Result<AktionStreckenabschnitt, UnbekannterStreckenabschnitt> {
        let aktion = match self {
            AktionStreckenabschnittSerialisiert::Strom { streckenabschnitt, fließend } => {
                let streckenabschnitt = streckenabschnitte
                    .get(streckenabschnitt.anschluss_ref())
                    .ok_or(UnbekannterStreckenabschnitt(streckenabschnitt))?
                    .clone();
                AktionStreckenabschnitt::Strom {
                    streckenabschnitt: Steuerung::neu(streckenabschnitt, (sender, Nachricht::from)),
                    fließend,
                }
            },
        };
        Ok(aktion)
    }
}

pub(crate) type GeradeWeiche = Weiche<weiche::gerade::Richtung, weiche::gerade::RichtungAnschlüsse>;
pub(crate) type GeradeWeicheSerialisiert =
    WeicheSerialisiert<weiche::gerade::Richtung, weiche::gerade::RichtungAnschlüsseSerialisiert>;
pub(crate) type KurvenWeiche = Weiche<weiche::kurve::Richtung, weiche::kurve::RichtungAnschlüsse>;
pub(crate) type KurvenWeicheSerialisiert =
    WeicheSerialisiert<weiche::kurve::Richtung, weiche::kurve::RichtungAnschlüsseSerialisiert>;
pub(crate) type DreiwegeWeiche =
    Weiche<weiche::dreiwege::RichtungInformation, weiche::dreiwege::RichtungAnschlüsse>;
pub(crate) type DreiwegeWeicheSerialisiert = WeicheSerialisiert<
    weiche::dreiwege::RichtungInformation,
    weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

/// Eine Aktion mit einer [Weiche].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnyAktionSchalten<
    Gerade = Steuerung<GeradeWeiche>,
    Kurve = Steuerung<KurvenWeiche>,
    Dreiwege = Steuerung<DreiwegeWeiche>,
> {
    /// Schalten einer [Weiche](weiche::gerade::Weiche), [SKurvenWeiche](weiche::s_kurve::SKurvenWeiche)
    /// oder [Kreuzung](crate::gleis::kreuzung::Kreuzung).
    SchalteGerade(AktionSchalten<Gerade, weiche::gerade::Richtung>),
    /// Schalten einer [KurvenWeiche](weiche::kurve::KurvenWeiche).
    SchalteKurve(AktionSchalten<Kurve, weiche::kurve::Richtung>),
    /// Schalten einer [DreiwegeWeiche](weiche::dreiwege::DreiwegeWeiche).
    SchalteDreiwege(AktionSchalten<Dreiwege, weiche::dreiwege::Richtung>),
}

impl<L: Leiter> Ausführen<L> for AnyAktionSchalten {
    type Fehler = anschluss::Fehler;

    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler> {
        match self {
            AnyAktionSchalten::SchalteGerade(aktion) => aktion.ausführen(einstellungen)?,
            AnyAktionSchalten::SchalteKurve(aktion) => aktion.ausführen(einstellungen)?,
            AnyAktionSchalten::SchalteDreiwege(aktion) => aktion.ausführen(einstellungen)?,
        }
        Ok(())
    }

    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()> {
        match self {
            AnyAktionSchalten::SchalteGerade(aktion) => {
                aktion.async_ausführen(einstellungen, sender)
            },
            AnyAktionSchalten::SchalteKurve(aktion) => {
                aktion.async_ausführen(einstellungen, sender)
            },
            AnyAktionSchalten::SchalteDreiwege(aktion) => {
                aktion.async_ausführen(einstellungen, sender)
            },
        }
    }
}

/// Serialisierbare Repräsentation für eine Aktion mit einer [Weiche].
pub type AnyAktionSchaltenSerialisiert = AnyAktionSchalten<
    GeradeWeicheSerialisiert,
    KurvenWeicheSerialisiert,
    DreiwegeWeicheSerialisiert,
>;

impl AnyAktionSchalten {
    /// Serialisiere eine Aktion mit einer [Weiche].
    pub fn serialisiere(&self) -> AnyAktionSchaltenSerialisiert {
        match self {
            AnyAktionSchalten::SchalteGerade(aktion) => {
                AnyAktionSchaltenSerialisiert::SchalteGerade(aktion.serialisiere())
            },
            AnyAktionSchalten::SchalteKurve(aktion) => {
                AnyAktionSchaltenSerialisiert::SchalteKurve(aktion.serialisiere())
            },
            AnyAktionSchalten::SchalteDreiwege(aktion) => {
                AnyAktionSchaltenSerialisiert::SchalteDreiwege(aktion.serialisiere())
            },
        }
    }
}

/// Eine nicht bekannten [Weiche] soll verwendet werden.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AnyUnbekannteWeiche {
    /// Anschlüsse einer [Weiche](weiche::gerade::Weiche),
    /// [SKurvenWeiche](weiche::s_kurve::SKurvenWeiche)
    /// oder [Kreuzung](crate::gleis::kreuzung::Kreuzung).
    Gerade(UnbekannteWeiche<GeradeWeicheSerialisiert>),
    /// Anschlüsse einer [KurvenWeiche](weiche::kurve::KurvenWeiche).
    Kurve(UnbekannteWeiche<KurvenWeicheSerialisiert>),
    /// Anschlüsse einer [DreiwegeWeiche](weiche::dreiwege::DreiwegeWeiche).
    Dreiwege(UnbekannteWeiche<DreiwegeWeicheSerialisiert>),
}

impl AnyAktionSchaltenSerialisiert {
    /// Deserialisiere eine Aktion mit einer [Weiche] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
        sender: Sender<Nachricht>,
    ) -> Result<AnyAktionSchalten, AnyUnbekannteWeiche> {
        let aktion = match self {
            AnyAktionSchaltenSerialisiert::SchalteGerade(aktion) => {
                AnyAktionSchalten::SchalteGerade(aktion.deserialisiere(gerade_weichen, sender)?)
            },
            AnyAktionSchaltenSerialisiert::SchalteKurve(aktion) => {
                AnyAktionSchalten::SchalteKurve(aktion.deserialisiere(kurven_weichen, sender)?)
            },
            AnyAktionSchaltenSerialisiert::SchalteDreiwege(aktion) => {
                AnyAktionSchalten::SchalteDreiwege(aktion.deserialisiere(dreiwege_weichen, sender)?)
            },
        };
        Ok(aktion)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
/// Schalten einer [Weiche].
pub struct AktionSchalten<Weiche, Richtung> {
    /// Die Anschlüsse zum Schalten der Weiche.
    pub weiche: Weiche,
    /// Die neue Richtung.
    pub richtung: Richtung,
}

impl<L, Anschlüsse, T, Richtung> Ausführen<L>
    for AktionSchalten<Steuerung<Weiche<T, Anschlüsse>>, Richtung>
where
    L: Leiter,
    T: 'static + Debug + Clone + WeicheSteuerung<Richtung> + Send,
    Richtung: 'static + Debug + Clone + Send,
    Anschlüsse: 'static + Debug + Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    type Fehler = anschluss::Fehler;

    fn ausführen(&mut self, einstellungen: Einstellungen<L>) -> Result<(), Self::Fehler> {
        let AktionSchalten { weiche, richtung } = self;
        weiche.as_mut().schalten(richtung.clone(), einstellungen.schalten_zeit)
    }

    fn async_ausführen<Nachricht: 'static + From<AsyncNachricht> + Send>(
        &mut self,
        einstellungen: Einstellungen<L>,
        sender: Sender<Nachricht>,
    ) -> JoinHandle<()> {
        let erzeuge_aktualisieren_nachricht = || AsyncNachricht::Aktualisieren.into();
        let titel = format!("{self:?}");
        let erzeuge_fehler_nachricht =
            |fehler| AsyncNachricht::Fehler { titel, nachricht: format!("{fehler:?}") }.into();
        let AktionSchalten { weiche, richtung } = self;
        weiche.as_mut().async_schalten(
            richtung.clone(),
            einstellungen.schalten_zeit,
            sender,
            Some(erzeuge_aktualisieren_nachricht),
            erzeuge_fehler_nachricht,
        )
    }
}

impl<Weiche, Richtung: Clone> AktionSchalten<Steuerung<Weiche>, Richtung> {
    /// Serialisiere eine Aktion mit einer [Weiche].
    #[allow(single_use_lifetimes)]
    pub fn serialisiere<WeicheSerialisiert>(&self) -> AktionSchalten<WeicheSerialisiert, Richtung>
    where
        Weiche: Serialisiere<WeicheSerialisiert>,
        WeicheSerialisiert:,
    {
        let AktionSchalten { weiche, richtung } = self;
        AktionSchalten { weiche: weiche.as_ref().serialisiere(), richtung: richtung.clone() }
    }
}

/// Eine nicht bekannten [Weiche] soll verwendet werden.
#[derive(Debug, Clone)]
pub struct UnbekannteWeiche<S>(pub S);

impl<S, Richtung> AktionSchalten<S, Richtung> {
    /// Deserialisiere eine Aktion mit einer [Weiche] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere<Weiche, Nachricht>(
        self,
        bekannte_weichen: &HashMap<S, Weiche>,
        sender: Sender<Nachricht>,
    ) -> Result<AktionSchalten<Steuerung<Weiche>, Richtung>, UnbekannteWeiche<S>>
    where
        S: Eq + Hash,
        Weiche: Clone,
        Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let AktionSchalten { weiche, richtung } = self;
        let weiche = bekannte_weichen.get(&weiche).ok_or(UnbekannteWeiche(weiche))?.clone();
        Ok(AktionSchalten { weiche: Steuerung::neu(weiche, (sender, Nachricht::from)), richtung })
    }
}

/// Eine Warte-Aktion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWarten<K = Steuerung<Kontakt>> {
    /// Warte auf das Auslösen eines [Kontaktes](Kontakt).
    WartenAuf {
        /// Die Anschlüsse des Kontaktes.
        kontakt: K,
    },
    /// Warte für eine festgelegte Zeit.
    /// Es kann vorkommen, dass etwas länger gewartet wird, siehe [std::thread::sleep].
    WartenFür {
        /// Die Wartezeit.
        zeit: Duration,
    },
}

impl AktionWarten {
    fn ausführen(&mut self) -> Result<(), RecvError> {
        match self {
            AktionWarten::WartenAuf { kontakt } => {
                let _ = kontakt.as_mut().warte_auf_trigger()?;
            },
            AktionWarten::WartenFür { zeit } => thread::sleep(*zeit),
        }
        Ok(())
    }
}

impl_ausführen_simple! {AktionWarten, RecvError, "einer Warte-Aktion"}

/// Serialisierbare Repräsentation einer Warte-Aktion.
pub type AktionWartenSerialisiert = AktionWarten<KontaktSerialisiert>;

impl AktionWarten {
    /// Serialisiere eine Warte-Aktion.
    pub fn serialisiere(&self) -> AktionWartenSerialisiert {
        match self {
            AktionWarten::WartenAuf { kontakt } => {
                AktionWartenSerialisiert::WartenAuf { kontakt: kontakt.as_ref().serialisiere() }
            },
            AktionWarten::WartenFür { zeit } => {
                AktionWartenSerialisiert::WartenFür { zeit: *zeit }
            },
        }
    }
}

/// Ein nicht bekannter [Kontakt] soll verwendet werden.
#[derive(Debug, Clone)]
pub struct UnbekannterKontakt(pub KontaktSerialisiert);

impl AktionWartenSerialisiert {
    /// Deserialisiere eine Warte-Aktion mithilfe bekannter [Kontakte](Kontakt).
    pub fn deserialisiere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
        sender: Sender<Nachricht>,
    ) -> Result<AktionWarten, UnbekannterKontakt> {
        let aktion = match self {
            AktionWartenSerialisiert::WartenAuf { kontakt } => {
                let kontakt = kontakte.get(&kontakt).ok_or(UnbekannterKontakt(kontakt))?.clone();
                AktionWarten::WartenAuf {
                    kontakt: Steuerung::neu(kontakt, (sender, Nachricht::from)),
                }
            },
            AktionWartenSerialisiert::WartenFür { zeit } => AktionWarten::WartenFür { zeit },
        };
        Ok(aktion)
    }
}
