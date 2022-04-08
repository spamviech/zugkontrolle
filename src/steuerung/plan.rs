//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

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
    pub aktionen: Vec<Aktion>,
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

/// Eine Aktionen in einem Fahrplan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionEnum<AktionGeschwindigkeit, AktionStreckenabschnitt, AktionSchalten, AktionWarten> {
    Geschwindigkeit(AktionGeschwindigkeit),
    Streckenabschnitt(AktionStreckenabschnitt),
    Schalten(AktionSchalten),
    Warten(AktionWarten),
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
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            Aktion::Geschwindigkeit(aktion) => aktion.anschlüsse(),
            Aktion::Streckenabschnitt(aktion) => aktion.anschlüsse(),
            Aktion::Schalten(aktion) => aktion.anschlüsse(),
            Aktion::Warten(aktion) => aktion.anschlüsse(),
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
        };
        Ok(reserviert)
    }
}

/// Eine Aktion mit einer [Geschwindigkeit].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionGeschwindigkeitEnum<Geschwindigkeit, Fahrtrichtung> {
    Geschwindigkeit { leiter: Geschwindigkeit, wert: u8 },
    Umdrehen { leiter: Geschwindigkeit },
    Fahrtrichtung { leiter: Geschwindigkeit, fahrtrichtung: Fahrtrichtung },
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
    Streckenabschnitt { streckenabschnitt: S, fließend: Fließend },
}

/// Serialisierbare Repräsentation einer Aktion mit einem [Streckenabschnitt].
pub type AktionStreckenabschnittSerialisiert =
    AktionStreckenabschnitt<StreckenabschnittSerialisiert>;

impl Serialisiere for AktionStreckenabschnitt {
    type Serialisiert = AktionStreckenabschnittSerialisiert;

    fn serialisiere(&self) -> Self::Serialisiert {
        match self {
            AktionStreckenabschnitt::Streckenabschnitt { streckenabschnitt, fließend } => {
                AktionStreckenabschnittSerialisiert::Streckenabschnitt {
                    streckenabschnitt: streckenabschnitt.serialisiere(),
                    fließend: *fließend,
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            AktionStreckenabschnitt::Streckenabschnitt { streckenabschnitt, fließend: _ } => {
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
            AktionStreckenabschnitt::Streckenabschnitt { streckenabschnitt, fließend } => {
                streckenabschnitt
                    .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                    .konvertiere(|streckenabschnitt| AktionStreckenabschnitt::Streckenabschnitt {
                        streckenabschnitt,
                        fließend,
                    })
            },
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
    SchalteGerade { weiche: Gerade, richtung: weiche::gerade::Richtung },
    SchalteKurve { weiche: Kurve, richtung: weiche::gerade::Richtung },
    SchalteDreiwege { weiche: Dreiwege, richtung: weiche::gerade::Richtung },
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
    WartenAuf { kontakt: K, trigger: Trigger },
    WartenFür { zeit: Duration },
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
