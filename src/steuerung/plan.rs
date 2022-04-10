//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use std::{
    collections::HashMap, fmt::Debug, hash::Hash, sync::mpsc::Sender, thread, time::Duration,
};

use log::{debug, error};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::Serialisiere, polarität::Fließend, Fehler, OutputSerialisiert
    },
    application::{geschwindigkeit::LeiterAnzeige, ZustandZurücksetzen},
    gleis::weiche,
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter},
        kontakt::{Kontakt, KontaktSerialisiert},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
        weiche::{Weiche, WeicheSerialisiert},
    },
};

/// Name eines [Plans](Plan).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

#[derive(Debug)]
/// Behandle einen bei einer asynchronen Aktion aufgetretenen Fehler.
pub struct AsyncFehler<Leiter: LeiterAnzeige> {
    /// Der Titel der Fehlermeldung.
    pub titel: String,
    /// Die Nachricht der Fehlermeldung.
    pub nachricht: String,
    /// Zustand auf Stand vor der Aktion zurücksetzen.
    pub zustand_zurücksetzen: Option<ZustandZurücksetzen<Leiter>>,
}

/// Etwas ausführbares.
pub trait Ausführen {
    /// Ausführen im aktuellen Thread. Kann den aktuellen Thread blockieren.
    fn ausführen(&mut self) -> Result<(), Fehler>;

    /// Erstelle einen neuen Thread aus und führe es dort aus.
    /// Wenn ein Fehler auftritt wird dieser über den Channel gesendet.
    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    );
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

impl<L: Leiter> Ausführen for Plan<L> {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        let Plan { aktionen, endlosschleife } = self;
        while *endlosschleife {
            for aktion in aktionen.iter_mut() {
                aktion.ausführen()?;
            }
        }
        Ok(())
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        todo!()
    }
}

/// Serialisierbare Repräsentation eines Fahrplans.
pub type PlanSerialisiert<L> = PlanEnum<AktionSerialisiert<L>>;

#[allow(single_use_lifetimes)]
impl<L> Plan<L>
where
    L: Leiter + Serialisiere,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de>,
{
    /// Serialisiere einen [Plan]
    pub fn serialisiere(&self) -> PlanSerialisiert<L> {
        let Plan { aktionen, endlosschleife } = self;
        PlanSerialisiert {
            aktionen: aktionen.into_iter().map(Aktion::serialisiere).collect(),
            endlosschleife: *endlosschleife,
        }
    }
}

/// Serialisierbare Repräsentation der nicht bekannten Anschlüsse.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, zugkontrolle_macros::From)]
#[zugkontrolle_debug(<L as Serialisiere>::Serialisiert: Debug)]
#[zugkontrolle_clone(<L as Serialisiere>::Serialisiert: Clone)]
pub enum UnbekannteAnschlüsse<L: Serialisiere> {
    /// Anschlüsse eine [Geschwindigkeit].
    Geschwindigkeit(UnbekannteGeschwindigkeit<L>),
    /// Anschlüsse eines [Streckenabschnittes](Streckenabschnitt).
    Streckenabschnitte(UnbekannterStreckenabschnitt),
    /// Anschlüsse einer [Weiche].
    Weiche(UnbekannteWeiche),
    /// Anschlüsse eines [Kontaktes](Kontakt).
    Kontakt(UnbekannterKontakt),
}

impl<L: Leiter + Serialisiere> PlanSerialisiert<L> {
    /// Deserialisiere einen [Plan].
    pub fn deserialisiere(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<L>, Geschwindigkeit<L>>,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
    ) -> Result<Plan<L>, UnbekannteAnschlüsse<L>>
    where
        <L as Serialisiere>::Serialisiert: PartialEq + Eq + Hash,
    {
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
    /// Eine [AktionSchalten].
    Schalten(Schalten),
    /// Eine [AktionWarten].
    Warten(Warten),
    /// Ausführen eines [Plans](Plan).
    Ausführen(Name),
}

/// Eine Aktionen in einem Fahrplan.
pub type Aktion<L> =
    AktionEnum<AktionGeschwindigkeit<L>, AktionStreckenabschnitt, AktionSchalten, AktionWarten>;

impl<L: Leiter> Ausführen for Aktion<L> {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        match self {
            AktionEnum::Geschwindigkeit(aktion) => todo!(),
            AktionEnum::Streckenabschnitt(aktion) => todo!(),
            AktionEnum::Schalten(aktion) => todo!(),
            AktionEnum::Warten(aktion) => todo!(),
            AktionEnum::Ausführen(aktion) => todo!(),
        }
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        match self {
            AktionEnum::Geschwindigkeit(aktion) => todo!(),
            AktionEnum::Streckenabschnitt(aktion) => todo!(),
            AktionEnum::Schalten(aktion) => todo!(),
            AktionEnum::Warten(aktion) => todo!(),
            AktionEnum::Ausführen(aktion) => todo!(),
        }
    }
}

/// Serialisierbare Repräsentation einer Aktion in einem Fahrplan.
pub type AktionSerialisiert<L> = AktionEnum<
    AktionGeschwindigkeitSerialisiert<L>,
    AktionStreckenabschnittSerialisiert,
    AktionSchaltenSerialisiert,
    AktionWartenSerialisiert,
>;

#[allow(single_use_lifetimes)]
impl<L> Aktion<L>
where
    L: Leiter + Serialisiere,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de>,
{
    /// Serialisiere eine [Aktion].
    pub fn serialisiere(&self) -> AktionSerialisiert<L> {
        match self {
            Aktion::Geschwindigkeit(aktion) => {
                AktionSerialisiert::Geschwindigkeit(aktion.serialisiere())
            },
            Aktion::Streckenabschnitt(aktion) => {
                AktionSerialisiert::Streckenabschnitt(aktion.serialisiere())
            },
            Aktion::Schalten(aktion) => AktionSerialisiert::Schalten(aktion.serialisiere()),
            Aktion::Warten(aktion) => AktionSerialisiert::Warten(aktion.serialisiere()),
            Aktion::Ausführen(plan) => AktionSerialisiert::Ausführen(plan.clone()),
        }
    }
}

impl<L: Leiter + Serialisiere> AktionSerialisiert<L> {
    /// Deserialisiere eine [Aktion] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<L>, Geschwindigkeit<L>>,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
    ) -> Result<Aktion<L>, UnbekannteAnschlüsse<L>>
    where
        <L as Serialisiere>::Serialisiert: PartialEq + Eq + Hash,
    {
        let reserviert = match self {
            AktionSerialisiert::Geschwindigkeit(aktion) => {
                Aktion::Geschwindigkeit(aktion.deserialisiere(geschwindigkeiten)?)
            },
            AktionSerialisiert::Streckenabschnitt(aktion) => {
                Aktion::Streckenabschnitt(aktion.deserialisiere(streckenabschnitte)?)
            },
            AktionSerialisiert::Schalten(aktion) => Aktion::Schalten(aktion.deserialisiere(
                gerade_weichen,
                kurven_weichen,
                dreiwege_weichen,
            )?),
            AktionSerialisiert::Warten(aktion) => Aktion::Warten(aktion.deserialisiere(kontakte)?),
            AktionSerialisiert::Ausführen(plan) => Aktion::Ausführen(plan),
        };
        Ok(reserviert)
    }
}

/// Eine Aktion mit einer [Geschwindigkeit].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionGeschwindigkeitEnum<Geschwindigkeit, Fahrtrichtung> {
    /// Einstellen der Fahrgeschwindigkeit.
    Geschwindigkeit {
        /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
        leiter: Geschwindigkeit,
        /// Die neue Geschwindigkeit.
        wert: u8,
    },
    /// Umdrehen der Fahrtrichtung.
    Umdrehen {
        /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
        leiter: Geschwindigkeit,
    },
    /// Einstellen der Fahrtrichtung.
    Fahrtrichtung {
        /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
        leiter: Geschwindigkeit,
        /// Die neue Fahrtrichtung.
        fahrtrichtung: Fahrtrichtung,
    },
}

/// Eine Aktion mit einer [Geschwindigkeit].
pub type AktionGeschwindigkeit<L> =
    AktionGeschwindigkeitEnum<Geschwindigkeit<L>, <L as Leiter>::Fahrtrichtung>;

impl<L: Leiter> Ausführen for AktionGeschwindigkeit<L> {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        match self {
            AktionGeschwindigkeitEnum::Geschwindigkeit { leiter, wert } => todo!(),
            AktionGeschwindigkeitEnum::Umdrehen { leiter } => todo!(),
            AktionGeschwindigkeitEnum::Fahrtrichtung { leiter, fahrtrichtung } => todo!(),
        }
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        match self {
            AktionGeschwindigkeitEnum::Geschwindigkeit { leiter, wert } => todo!(),
            AktionGeschwindigkeitEnum::Umdrehen { leiter } => todo!(),
            AktionGeschwindigkeitEnum::Fahrtrichtung { leiter, fahrtrichtung } => todo!(),
        }
    }
}

/// Serialisierbare Repräsentation für eine Aktion mit einer [Geschwindigkeit].
pub type AktionGeschwindigkeitSerialisiert<L> =
    AktionGeschwindigkeitEnum<GeschwindigkeitSerialisiert<L>, <L as Leiter>::Fahrtrichtung>;

#[allow(single_use_lifetimes)]
impl<L> AktionGeschwindigkeit<L>
where
    L: Leiter + Serialisiere,
    <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de>,
{
    /// Serialisiere eine Aktion mit einer [Geschwindigkeit].
    fn serialisiere(&self) -> AktionGeschwindigkeitSerialisiert<L> {
        match self {
            AktionGeschwindigkeit::Geschwindigkeit { leiter, wert } => {
                AktionGeschwindigkeitSerialisiert::Geschwindigkeit {
                    leiter: leiter.serialisiere(),
                    wert: *wert,
                }
            },
            AktionGeschwindigkeit::Umdrehen { leiter } => {
                AktionGeschwindigkeitSerialisiert::Umdrehen { leiter: leiter.serialisiere() }
            },
            AktionGeschwindigkeit::Fahrtrichtung { leiter, fahrtrichtung } => {
                AktionGeschwindigkeitSerialisiert::Fahrtrichtung {
                    leiter: leiter.serialisiere(),
                    fahrtrichtung: fahrtrichtung.clone(),
                }
            },
        }
    }
}

/// Eine nicht bekannte [Geschwindigkeit] soll verwendet werden.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<L as Serialisiere>::Serialisiert: Debug)]
#[zugkontrolle_clone(<L as Serialisiere>::Serialisiert: Clone)]
pub struct UnbekannteGeschwindigkeit<L: Serialisiere>(pub GeschwindigkeitSerialisiert<L>);

impl<L: Leiter + Serialisiere> AktionGeschwindigkeitSerialisiert<L>
where
    <L as Serialisiere>::Serialisiert: PartialEq + Eq + Hash,
{
    /// Deserialisiere eine Aktion mit einer [Geschwindigkeit] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere(
        self,
        geschwindigkeiten: &HashMap<GeschwindigkeitSerialisiert<L>, Geschwindigkeit<L>>,
    ) -> Result<AktionGeschwindigkeit<L>, UnbekannteGeschwindigkeit<L>> {
        let aktion = match self {
            AktionGeschwindigkeitSerialisiert::Geschwindigkeit { leiter, wert } => {
                let leiter = geschwindigkeiten
                    .get(&leiter)
                    .ok_or(UnbekannteGeschwindigkeit(leiter))?
                    .clone();
                AktionGeschwindigkeit::Geschwindigkeit { leiter, wert }
            },
            AktionGeschwindigkeitSerialisiert::Umdrehen { leiter } => {
                let leiter = geschwindigkeiten
                    .get(&leiter)
                    .ok_or(UnbekannteGeschwindigkeit(leiter))?
                    .clone();
                AktionGeschwindigkeit::Umdrehen { leiter }
            },
            AktionGeschwindigkeitSerialisiert::Fahrtrichtung { leiter, fahrtrichtung } => {
                let leiter = geschwindigkeiten
                    .get(&leiter)
                    .ok_or(UnbekannteGeschwindigkeit(leiter))?
                    .clone();
                AktionGeschwindigkeit::Fahrtrichtung { leiter, fahrtrichtung }
            },
        };
        Ok(aktion)
    }
}

/// Eine Aktion mit einem [Streckenabschnitt].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionStreckenabschnitt<S = Streckenabschnitt> {
    /// Strom auf einem Streckenabschnitt einstellen.
    Strom {
        /// Die Anschlüsse zur Steuerung des Streckenabschnittes.
        streckenabschnitt: S,
        /// Wird der Strom an- oder abgestellt.
        fließend: Fließend,
    },
}

impl Ausführen for AktionStreckenabschnitt {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        let AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } = self;
        streckenabschnitt.strom(*fließend)
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        let AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } = self;
        todo!()
    }
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
                    streckenabschnitt: streckenabschnitt.serialisiere(),
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
    pub fn deserialisiere(
        self,
        streckenabschnitte: &HashMap<OutputSerialisiert, Streckenabschnitt>,
    ) -> Result<AktionStreckenabschnitt, UnbekannterStreckenabschnitt> {
        let aktion = match self {
            AktionStreckenabschnittSerialisiert::Strom { streckenabschnitt, fließend } => {
                let streckenabschnitt = streckenabschnitte
                    .get(streckenabschnitt.anschluss_ref())
                    .ok_or(UnbekannterStreckenabschnitt(streckenabschnitt))?
                    .clone();
                AktionStreckenabschnitt::Strom { streckenabschnitt, fließend }
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
    Weiche<weiche::dreiwege::Richtung, weiche::dreiwege::RichtungAnschlüsse>;
pub(crate) type DreiwegeWeicheSerialisiert = WeicheSerialisiert<
    weiche::dreiwege::Richtung,
    weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

/// Eine Aktion mit einer [Weiche].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionSchalten<Gerade = GeradeWeiche, Kurve = KurvenWeiche, Dreiwege = DreiwegeWeiche> {
    /// Schalten einer [Weiche](weiche::gerade::Weiche), [SKurvenWeiche](weiche::s_kurve::SKurvenWeiche)
    /// oder [Kreuzung](crate::gleis::kreuzung::Kreuzung).
    SchalteGerade {
        /// Die Anschlüsse zum Schalten der Weiche.
        weiche: Gerade,
        /// Die neue Richtung.
        richtung: weiche::gerade::Richtung,
    },
    /// Schalten einer [KurvenWeiche](weiche::kurve::KurvenWeiche).
    SchalteKurve {
        /// Die Anschlüsse zum Schalten der Weiche.
        weiche: Kurve,
        /// Die neue Richtung.
        richtung: weiche::gerade::Richtung,
    },
    /// Schalten einer [DreiwegeWeiche](weiche::dreiwege::DreiwegeWeiche).
    SchalteDreiwege {
        /// Die Anschlüsse zum Schalten der Weiche.
        weiche: Dreiwege,
        /// Die neue Richtung.
        richtung: weiche::gerade::Richtung,
    },
}

impl Ausführen for AktionSchalten {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        match self {
            AktionSchalten::SchalteGerade { weiche, richtung } => todo!(),
            AktionSchalten::SchalteKurve { weiche, richtung } => todo!(),
            AktionSchalten::SchalteDreiwege { weiche, richtung } => todo!(),
        }
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        match self {
            AktionSchalten::SchalteGerade { weiche, richtung } => todo!(),
            AktionSchalten::SchalteKurve { weiche, richtung } => todo!(),
            AktionSchalten::SchalteDreiwege { weiche, richtung } => todo!(),
        }
    }
}

/// Serialisierbare Repräsentation für eine Aktion mit einer [Weiche].
pub type AktionSchaltenSerialisiert =
    AktionSchalten<GeradeWeicheSerialisiert, KurvenWeicheSerialisiert, DreiwegeWeicheSerialisiert>;

impl AktionSchalten {
    /// Serialisiere eine Aktion mit einer [Weiche].
    pub fn serialisiere(&self) -> AktionSchaltenSerialisiert {
        match self {
            AktionSchalten::SchalteGerade { weiche, richtung } => {
                AktionSchaltenSerialisiert::SchalteGerade {
                    weiche: weiche.serialisiere(),
                    richtung: *richtung,
                }
            },
            AktionSchalten::SchalteKurve { weiche, richtung } => {
                AktionSchaltenSerialisiert::SchalteKurve {
                    weiche: weiche.serialisiere(),
                    richtung: *richtung,
                }
            },
            AktionSchalten::SchalteDreiwege { weiche, richtung } => {
                AktionSchaltenSerialisiert::SchalteDreiwege {
                    weiche: weiche.serialisiere(),
                    richtung: *richtung,
                }
            },
        }
    }
}

/// Eine nicht bekannten [Weiche] soll verwendet werden.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum UnbekannteWeiche {
    /// Anschlüsse einer [Weiche](weiche::gerade::Weiche),
    /// [SKurvenWeiche](weiche::s_kurve::SKurvenWeiche)
    /// oder [Kreuzung](crate::gleis::kreuzung::Kreuzung).
    Gerade(GeradeWeicheSerialisiert),
    /// Anschlüsse einer [KurvenWeiche](weiche::kurve::KurvenWeiche).
    Kurve(KurvenWeicheSerialisiert),
    /// Anschlüsse einer [DreiwegeWeiche](weiche::dreiwege::DreiwegeWeiche).
    Dreiwege(DreiwegeWeicheSerialisiert),
}

impl AktionSchaltenSerialisiert {
    /// Deserialisiere eine Aktion mit einer [Weiche] mithilfe bekannter Anschlüsse.
    pub fn deserialisiere(
        self,
        gerade_weichen: &HashMap<GeradeWeicheSerialisiert, GeradeWeiche>,
        kurven_weichen: &HashMap<KurvenWeicheSerialisiert, KurvenWeiche>,
        dreiwege_weichen: &HashMap<DreiwegeWeicheSerialisiert, DreiwegeWeiche>,
    ) -> Result<AktionSchalten, UnbekannteWeiche> {
        let aktion = match self {
            AktionSchaltenSerialisiert::SchalteGerade { weiche, richtung } => {
                let weiche = gerade_weichen.get(&weiche).ok_or(weiche)?.clone();
                AktionSchalten::SchalteGerade { weiche, richtung }
            },
            AktionSchaltenSerialisiert::SchalteKurve { weiche, richtung } => {
                let weiche = kurven_weichen.get(&weiche).ok_or(weiche)?.clone();
                AktionSchalten::SchalteKurve { weiche, richtung }
            },
            AktionSchaltenSerialisiert::SchalteDreiwege { weiche, richtung } => {
                let weiche = dreiwege_weichen.get(&weiche).ok_or(weiche)?.clone();
                AktionSchalten::SchalteDreiwege { weiche, richtung }
            },
        };
        Ok(aktion)
    }
}

/// Eine Warte-Aktion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWarten<K = Kontakt> {
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

impl Ausführen for AktionWarten {
    fn ausführen(&mut self) -> Result<(), Fehler> {
        match self {
            AktionWarten::WartenAuf { kontakt } => {
                let _ = kontakt.warte_auf_trigger()?;
            },
            AktionWarten::WartenFür { zeit } => thread::sleep(*zeit),
        }
        Ok(())
    }

    fn async_ausführen<
        Leiter: LeiterAnzeige,
        Nachricht: 'static + From<AsyncFehler<Leiter>> + Send,
    >(
        &mut self,
        erfolg: Option<Sender<()>>,
        fehler: Sender<Nachricht>,
    ) {
        let mut clone = self.clone();
        let _ = thread::spawn(move || match clone.ausführen() {
            Ok(unit) => {
                if let Some(erfolg) = erfolg {
                    if let Err(fehler) = erfolg.send(unit) {
                        error!(
                            "Kein Empfänger wartet auf die Erfolgsmeldung einer Warte-Aktion: {fehler}"
                        )
                    }
                } else {
                    error!("Kein Empfänger für die Erfolgsmeldung einer Warte-Aktion.")
                }
            },
            Err(fehlermeldung) => {
                if let Err(fehler) = fehler.send(
                    AsyncFehler {
                        titel: format!("{clone:?}"),
                        nachricht: format!("{fehlermeldung:?}"),
                        zustand_zurücksetzen: None,
                    }
                    .into(),
                ) {
                    error!(
                        "Kein Empfänger wartet auf die Fehlermeldung einer Warte-Aktion: {fehler}"
                    )
                }
            },
        });
    }
}

/// Serialisierbare Repräsentation einer Warte-Aktion.
pub type AktionWartenSerialisiert = AktionWarten<KontaktSerialisiert>;

impl AktionWarten {
    /// Serialisiere eine Warte-Aktion.
    pub fn serialisiere(&self) -> AktionWartenSerialisiert {
        match self {
            AktionWarten::WartenAuf { kontakt } => {
                AktionWartenSerialisiert::WartenAuf { kontakt: kontakt.serialisiere() }
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
    pub fn deserialisiere(
        self,
        kontakte: &HashMap<KontaktSerialisiert, Kontakt>,
    ) -> Result<AktionWarten, UnbekannterKontakt> {
        let aktion = match self {
            AktionWartenSerialisiert::WartenAuf { kontakt } => {
                let kontakt = kontakte.get(&kontakt).ok_or(UnbekannterKontakt(kontakt))?.clone();
                AktionWarten::WartenAuf { kontakt }
            },
            AktionWartenSerialisiert::WartenFür { zeit } => AktionWarten::WartenFür { zeit },
        };
        Ok(aktion)
    }
}
