//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use std::{fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        polarität::Fließend,
        trigger::Trigger,
        InputAnschluss, Lager, OutputAnschluss,
    },
    gleis::weiche,
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter},
        kontakt::{Kontakt, KontaktSerialisiert},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
        weiche::{Weiche, WeicheSerialisiert},
    },
};

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

/// Serialisierbare Repräsentation eines Fahrplans.
pub type PlanSerialisiert<L> = PlanEnum<AktionSerialisiert<L>>;

#[allow(single_use_lifetimes)]
impl<L> Serialisiere for Plan<L>
where
    L: Leiter + Serialisiere,
    for<'de> <L as Leiter>::Fahrtrichtung: Clone + Serialize + Deserialize<'de>,
{
    type Serialisiert = PlanSerialisiert<L>;

    fn serialisiere(&self) -> Self::Serialisiert {
        let Plan { aktionen, endlosschleife } = self;
        PlanSerialisiert {
            aktionen: aktionen.into_iter().map(Aktion::serialisiere).collect(),
            endlosschleife: *endlosschleife,
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        for aktion in self.aktionen {
            let (pwm, output, input) = aktion.anschlüsse();
            pwm_pins.extend(pwm);
            output_anschlüsse.extend(output);
            input_anschlüsse.extend(input);
        }
        (pwm_pins, output_anschlüsse, input_anschlüsse)
    }
}

impl<L: Leiter + Serialisiere> Reserviere<Plan<L>> for PlanSerialisiert<L> {
    fn reserviere(
        self,
        lager: &mut Lager,
        mut pwm_pins: Vec<pwm::Pin>,
        mut output_anschlüsse: Vec<OutputAnschluss>,
        mut input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Plan<L>> {
        let PlanSerialisiert { aktionen: aktionen_serialisiert, endlosschleife } = self;
        let mut aktionen = Vec::new();
        for aktion in aktionen_serialisiert {
            let Reserviert {
                anschluss,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            } = aktion.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?;
            aktionen.push(anschluss);
            pwm_pins = pwm_nicht_benötigt;
            output_anschlüsse = output_nicht_benötigt;
            input_anschlüsse = input_nicht_benötigt;
        }
        Ok(Reserviert {
            anschluss: Plan { aktionen, endlosschleife },
            pwm_nicht_benötigt: pwm_pins,
            output_nicht_benötigt: output_anschlüsse,
            input_nicht_benötigt: input_anschlüsse,
        })
    }
}

// TODO reservieren schlägt fehl, da die Anschlüsse bereits für die Anschlüsse selbst reserviert sind.
// Clone möglich, da als Arc<Mutex<_>> zu runtime verwendet
// Lager umschreiben, damit es immer einer Kopie des Arc<Mutex<_>> behält?
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
    Ausführen(PlanEnum<AktionEnum<Geschwindigkeit, Streckenabschnitt, Schalten, Warten>>),
}

/// Eine Aktionen in einem Fahrplan.
pub type Aktion<L> =
    AktionEnum<AktionGeschwindigkeit<L>, AktionStreckenabschnitt, AktionSchalten, AktionWarten>;

/// Serialisierbare Repräsentation einer Aktion in einem Fahrplan.
pub type AktionSerialisiert<L> = AktionEnum<
    AktionGeschwindigkeitSerialisiert<L>,
    AktionStreckenabschnittSerialisiert,
    AktionSchaltenSerialisiert,
    AktionWartenSerialisiert,
>;

#[allow(single_use_lifetimes)]
impl<L> Serialisiere for Aktion<L>
where
    L: Leiter + Serialisiere,
    for<'de> <L as Leiter>::Fahrtrichtung: Clone + Serialize + Deserialize<'de>,
{
    type Serialisiert = AktionSerialisiert<L>;

    fn serialisiere(&self) -> Self::Serialisiert {
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

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            Aktion::Geschwindigkeit(aktion) => aktion.anschlüsse(),
            Aktion::Streckenabschnitt(aktion) => aktion.anschlüsse(),
            Aktion::Schalten(aktion) => aktion.anschlüsse(),
            Aktion::Warten(aktion) => aktion.anschlüsse(),
            Aktion::Ausführen(plan) => plan.anschlüsse(),
        }
    }
}

impl<L: Leiter + Serialisiere> Reserviere<Aktion<L>> for AktionSerialisiert<L> {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Aktion<L>> {
        let reserviert = match self {
            AktionSerialisiert::Geschwindigkeit(aktion) => aktion
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(Aktion::Geschwindigkeit),
            AktionSerialisiert::Streckenabschnitt(aktion) => aktion
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(Aktion::Streckenabschnitt),
            AktionSerialisiert::Schalten(aktion) => aktion
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(Aktion::Schalten),
            AktionSerialisiert::Warten(aktion) => aktion
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(Aktion::Warten),
            AktionSerialisiert::Ausführen(plan) => plan
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(Aktion::Ausführen),
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

/// Serialisierbare Repräsentation für eine Aktion mit einer [Geschwindigkeit].
pub type AktionGeschwindigkeitSerialisiert<L> =
    AktionGeschwindigkeitEnum<GeschwindigkeitSerialisiert<L>, <L as Leiter>::Fahrtrichtung>;

#[allow(single_use_lifetimes)]
impl<L> Serialisiere for AktionGeschwindigkeit<L>
where
    L: Leiter + Serialisiere,
    for<'de> <L as Leiter>::Fahrtrichtung: Clone + Serialize + Deserialize<'de>,
{
    type Serialisiert = AktionGeschwindigkeitSerialisiert<L>;

    fn serialisiere(&self) -> Self::Serialisiert {
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

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            AktionGeschwindigkeit::Geschwindigkeit { leiter, wert: _ } => leiter.anschlüsse(),
            AktionGeschwindigkeit::Umdrehen { leiter } => leiter.anschlüsse(),
            AktionGeschwindigkeit::Fahrtrichtung { leiter, fahrtrichtung: _ } => {
                leiter.anschlüsse()
            },
        }
    }
}

impl<L: Leiter + Serialisiere> Reserviere<AktionGeschwindigkeit<L>>
    for AktionGeschwindigkeitSerialisiert<L>
{
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<AktionGeschwindigkeit<L>> {
        let reserviert = match self {
            AktionGeschwindigkeitSerialisiert::Geschwindigkeit { leiter, wert } => leiter
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|leiter| AktionGeschwindigkeit::Geschwindigkeit { leiter, wert }),
            AktionGeschwindigkeitSerialisiert::Umdrehen { leiter } => leiter
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|leiter| AktionGeschwindigkeit::Umdrehen { leiter }),
            AktionGeschwindigkeitSerialisiert::Fahrtrichtung { leiter, fahrtrichtung } => leiter
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|leiter| AktionGeschwindigkeit::Fahrtrichtung {
                    leiter,
                    fahrtrichtung,
                }),
        };
        Ok(reserviert)
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

/// Serialisierbare Repräsentation einer Aktion mit einem [Streckenabschnitt].
pub type AktionStreckenabschnittSerialisiert =
    AktionStreckenabschnitt<StreckenabschnittSerialisiert>;

impl Serialisiere for AktionStreckenabschnitt {
    type Serialisiert = AktionStreckenabschnittSerialisiert;

    fn serialisiere(&self) -> Self::Serialisiert {
        match self {
            AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } => {
                AktionStreckenabschnittSerialisiert::Strom {
                    streckenabschnitt: streckenabschnitt.serialisiere(),
                    fließend: *fließend,
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            AktionStreckenabschnitt::Strom { streckenabschnitt, fließend: _ } => {
                streckenabschnitt.anschlüsse()
            },
        }
    }
}

impl Reserviere<AktionStreckenabschnitt> for AktionStreckenabschnittSerialisiert {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<AktionStreckenabschnitt> {
        let reserviert = match self {
            AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } => streckenabschnitt
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|streckenabschnitt| AktionStreckenabschnitt::Strom {
                    streckenabschnitt,
                    fließend,
                }),
        };
        Ok(reserviert)
    }
}

type GeradeWeiche = Weiche<weiche::gerade::Richtung, weiche::gerade::RichtungAnschlüsse>;
type GeradeWeicheSerialisiert =
    WeicheSerialisiert<weiche::gerade::Richtung, weiche::gerade::RichtungAnschlüsseSerialisiert>;
type KurvenWeiche = Weiche<weiche::kurve::Richtung, weiche::kurve::RichtungAnschlüsse>;
type KurvenWeicheSerialisiert =
    WeicheSerialisiert<weiche::kurve::Richtung, weiche::kurve::RichtungAnschlüsseSerialisiert>;
type DreiwegeWeiche = Weiche<weiche::dreiwege::Richtung, weiche::dreiwege::RichtungAnschlüsse>;
type DreiwegeWeicheSerialisiert = WeicheSerialisiert<
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

/// Serialisierbare Repräsentation für eine Aktion mit einer [Weiche].
pub type AktionSchaltenSerialisiert =
    AktionSchalten<GeradeWeicheSerialisiert, KurvenWeicheSerialisiert, DreiwegeWeicheSerialisiert>;

impl Serialisiere for AktionSchalten {
    type Serialisiert = AktionSchaltenSerialisiert;

    fn serialisiere(&self) -> Self::Serialisiert {
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

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            AktionSchalten::SchalteGerade { weiche, richtung: _ } => weiche.anschlüsse(),
            AktionSchalten::SchalteKurve { weiche, richtung: _ } => weiche.anschlüsse(),
            AktionSchalten::SchalteDreiwege { weiche, richtung: _ } => weiche.anschlüsse(),
        }
    }
}

impl Reserviere<AktionSchalten> for AktionSchaltenSerialisiert {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<AktionSchalten> {
        let reserviert = match self {
            AktionSchaltenSerialisiert::SchalteGerade { weiche, richtung } => weiche
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|weiche| AktionSchalten::SchalteGerade { weiche, richtung }),
            AktionSchaltenSerialisiert::SchalteKurve { weiche, richtung } => weiche
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|weiche| AktionSchalten::SchalteKurve { weiche, richtung }),
            AktionSchaltenSerialisiert::SchalteDreiwege { weiche, richtung } => weiche
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|weiche| AktionSchalten::SchalteDreiwege { weiche, richtung }),
        };
        Ok(reserviert)
    }
}
/// Eine Warte-Aktion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWarten<K = Kontakt> {
    /// Warte auf das Auslösen eines [Kontaktes](Kontakt).
    WartenAuf {
        /// Die Anschlüsse des Kontaktes.
        kontakt: K,
        /// Die Bedingung zum Auslösen des Kontaktes.
        trigger: Trigger,
    },
    /// Warte für eine festgelegte Zeit.
    /// Es kann vorkommen, dass etwas länger gewartet wird, siehe [std::thread::sleep].
    WartenFür {
        /// Die Wartezeit.
        zeit: Duration,
    },
}

/// Serialisierbare Repräsentation einer Warte-Aktion.
pub type AktionWartenSerialisiert = AktionWarten<KontaktSerialisiert>;

impl Serialisiere for AktionWarten {
    type Serialisiert = AktionWartenSerialisiert;

    fn serialisiere(&self) -> Self::Serialisiert {
        match self {
            AktionWarten::WartenAuf { kontakt, trigger } => AktionWartenSerialisiert::WartenAuf {
                kontakt: kontakt.serialisiere(),
                trigger: *trigger,
            },
            AktionWarten::WartenFür { zeit } => {
                AktionWartenSerialisiert::WartenFür { zeit: *zeit }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            AktionWarten::WartenAuf { kontakt, trigger: _ } => kontakt.anschlüsse(),
            AktionWarten::WartenFür { zeit: _ } => (Vec::new(), Vec::new(), Vec::new()),
        }
    }
}

impl Reserviere<AktionWarten> for AktionWartenSerialisiert {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<AktionWarten> {
        let reserviert = match self {
            AktionWarten::WartenAuf { kontakt, trigger } => kontakt
                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                .konvertiere(|kontakt| AktionWarten::WartenAuf { kontakt, trigger }),
            AktionWarten::WartenFür { zeit } => Reserviert {
                anschluss: AktionWarten::WartenFür { zeit },
                pwm_nicht_benötigt: pwm_pins,
                output_nicht_benötigt: output_anschlüsse,
                input_nicht_benötigt: input_anschlüsse,
            },
        };
        Ok(reserviert)
    }
}
