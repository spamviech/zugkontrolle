//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden.

use std::collections::HashMap;

use serde::Deserialize;

use crate::{
    anschluss::{
        self,
        de_serialisieren::Serialisiere,
        level::Level,
        pcf8574::{self, I2cBus},
        pin::pwm,
        polarität::Polarität,
        trigger::Trigger,
    },
    eingeschränkt::kleiner_8,
    gleis::{
        gerade,
        gleise::daten as aktuell,
        kreuzung, kurve,
        weiche::{dreiwege, gerade as gerade_weiche, kurve as kurven_weiche, s_kurve},
    },
    steuerung::{
        geschwindigkeit::{self, BekannterLeiter, Mittelleiter, Zweileiter},
        kontakt, plan, streckenabschnitt, weiche,
    },
    typen::{canvas::Position, farbe::Farbe, skalar::Skalar, winkel::Winkel},
    void::Void,
};

/// Beschreibung eines [anschluss::pcf85747::Pcf8574].
#[derive(Deserialize)]
struct Pcf8574Beschreibung {
    /// Anliegendes [Level] an das `A0` Adress-Bit.
    a0: Level,
    /// Anliegendes [Level] an das `A1` Adress-Bit.
    a1: Level,
    /// Anliegendes [Level] an das `A2` Adress-Bit.
    a2: Level,
    /// Variante des [anschluss::pcf85747::Pcf8574], beeinflusst die I2C-Adresse.
    variante: pcf8574::Variante,
}

impl From<Pcf8574Beschreibung> for pcf8574::Beschreibung {
    fn from(Pcf8574Beschreibung { a0, a1, a2, variante }: Pcf8574Beschreibung) -> Self {
        pcf8574::Beschreibung { i2c_bus: I2cBus::I2c0_1, a0, a1, a2, variante }
    }
}

/// Serialisierbare Informationen eines [OutputAnschluss]es.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
enum OutputSerialisiert {
    /// Ein [Pin](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [Pcf8574-Port](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
}

impl From<OutputSerialisiert> for anschluss::OutputSerialisiert {
    fn from(input: OutputSerialisiert) -> Self {
        match input {
            OutputSerialisiert::Pin { pin, polarität } => {
                anschluss::OutputSerialisiert::Pin { pin, polarität }
            },
            OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                anschluss::OutputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    polarität,
                }
            },
        }
    }
}

/// Serialisierbare Informationen eines [InputAnschlusses](anschluss::InputAnschluss).
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
enum InputSerialisiert {
    /// Ein [Pin](input::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
    },
    /// Ein [Pcf8574-Port](pcf8574::InputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Der konfigurierte Interrupt-Pin des Pcf8574.
        interrupt: Option<u8>,
    },
}

impl From<InputSerialisiert> for anschluss::InputSerialisiert {
    fn from(input: InputSerialisiert) -> Self {
        match input {
            InputSerialisiert::Pin { pin } => anschluss::InputSerialisiert::Pin { pin },
            InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt } => {
                anschluss::InputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    interrupt,
                }
            },
        }
    }
}

/// Serialisierte Variante eines [Kontaktes](Kontakt).
#[derive(Deserialize)]
struct KontaktSerialisiert {
    /// Der Name des Kontaktes.
    name: kontakt::Name,
    /// Der Anschluss des Kontaktes.
    anschluss: InputSerialisiert,
    /// Wann wird der Kontakt ausgelöst.
    trigger: Trigger,
}

impl From<KontaktSerialisiert> for kontakt::KontaktSerialisiert {
    fn from(input: KontaktSerialisiert) -> Self {
        let KontaktSerialisiert { name, anschluss, trigger } = input;
        kontakt::KontaktSerialisiert { name, anschluss: anschluss.into(), trigger }
    }
}

/// Serialisierbare Repräsentation einer [Gerade](gerade::Gerade).
#[derive(Deserialize)]
struct GeradeSerialisiert {
    länge: Skalar,
    beschreibung: Option<String>,
    kontakt: Option<KontaktSerialisiert>,
}

impl From<GeradeSerialisiert> for gerade::GeradeSerialisiert {
    fn from(input: GeradeSerialisiert) -> Self {
        let GeradeSerialisiert { länge, beschreibung, kontakt } = input;
        gerade::GeradeSerialisiert { länge, beschreibung, kontakt: kontakt.map(Into::into) }
    }
}

/// Serialisierbare Repräsentation einer [Kurve](kurve::Kurve).
#[derive(Deserialize)]
struct KurveSerialisiert {
    radius: Skalar,
    winkel: Winkel,
    beschreibung: Option<String>,
    kontakt: Option<KontaktSerialisiert>,
}

impl From<KurveSerialisiert> for kurve::KurveSerialisiert {
    fn from(input: KurveSerialisiert) -> Self {
        let KurveSerialisiert { radius, winkel, beschreibung, kontakt } = input;
        kurve::KurveSerialisiert { radius, winkel, beschreibung, kontakt: kontakt.map(Into::into) }
    }
}

#[derive(Deserialize)]
struct WeicheSteuerungSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    name: weiche::Name,
    /// Die aktuelle Richtung der Weiche.
    aktuelle_richtung: Richtung,
    /// Die Richtung vor der aktuellen Richtung.
    letzte_richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    anschlüsse: Anschlüsse,
}

// FIXME Instanzen für Richtung erstellen
struct AktuellUndBisher<R> {
    aktuelle_richtung: R,
    letzte_richtung: R,
}

impl<R1, A1> WeicheSteuerungSerialisiert<R1, A1> {
    fn konvertiere<R2: From<AktuellUndBisher<R1>>, A2: From<A1>>(
        self,
    ) -> weiche::WeicheSerialisiert<R2, A2> {
        let WeicheSteuerungSerialisiert { name, aktuelle_richtung, letzte_richtung, anschlüsse } =
            self;
        weiche::WeicheSerialisiert::neu(
            name,
            AktuellUndBisher { aktuelle_richtung, letzte_richtung }.into(),
            anschlüsse.into(),
        )
    }
}

/// Serialisierbare Repräsentation der [Anschlüsse](gerade_weiche::RichtungAnschlüsse)
/// einer [Weiche](gerade_weiche::Weiche).
#[derive(Deserialize)]
struct WeicheAnschlüsseSerialisiert {
    gerade: OutputSerialisiert,
    kurve: OutputSerialisiert,
}

impl From<WeicheAnschlüsseSerialisiert> for gerade_weiche::RichtungAnschlüsseSerialisiert {
    fn from(input: WeicheAnschlüsseSerialisiert) -> Self {
        let WeicheAnschlüsseSerialisiert { gerade, kurve } = input;
        gerade_weiche::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            kurve: kurve.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [Weiche](gerade_weiche::Weiche).
#[derive(Deserialize)]
struct WeicheSerialisiert {
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    orientierung: gerade_weiche::Orientierung,
    beschreibung: Option<String>,
    steuerung:
        Option<WeicheSteuerungSerialisiert<gerade_weiche::Richtung, WeicheAnschlüsseSerialisiert>>,
}

impl From<WeicheSerialisiert> for gerade_weiche::WeicheSerialisiert {
    fn from(input: WeicheSerialisiert) -> Self {
        let WeicheSerialisiert { länge, radius, winkel, orientierung, beschreibung, steuerung } =
            input;
        gerade_weiche::WeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation der [Anschlüsse](kurven_weiche::RichtungAnschlüsse)
/// einer [KurvenWeiche](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
struct KurvenWeicheAnschlüsseSerialisiert {
    innen: OutputSerialisiert,
    außen: OutputSerialisiert,
}

impl From<KurvenWeicheAnschlüsseSerialisiert> for kurven_weiche::RichtungAnschlüsseSerialisiert {
    fn from(input: KurvenWeicheAnschlüsseSerialisiert) -> Self {
        let KurvenWeicheAnschlüsseSerialisiert { innen, außen } = input;
        kurven_weiche::RichtungAnschlüsseSerialisiert {
            innen: innen.into(), außen: außen.into()
        }
    }
}

/// Serialisierbare Repräsentation einer [KurvenWeiche](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
struct KurvenWeicheSerialisiert {
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    orientierung: gerade_weiche::Orientierung,
    beschreibung: Option<String>,
    steuerung: Option<
        WeicheSteuerungSerialisiert<kurven_weiche::Richtung, KurvenWeicheAnschlüsseSerialisiert>,
    >,
}

impl From<KurvenWeicheSerialisiert> for kurven_weiche::KurvenWeicheSerialisiert {
    fn from(input: KurvenWeicheSerialisiert) -> Self {
        let KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung,
        } = input;
        kurven_weiche::KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation der [Anschlüsse](dreiwege::RichtungAnschlüsse)
/// einer [DreiwegeWeiche](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
struct DreiwegeAnschlüsseSerialisiert {
    gerade: OutputSerialisiert,
    links: OutputSerialisiert,
    rechts: OutputSerialisiert,
}

impl From<DreiwegeAnschlüsseSerialisiert> for dreiwege::RichtungAnschlüsseSerialisiert {
    fn from(input: DreiwegeAnschlüsseSerialisiert) -> Self {
        let DreiwegeAnschlüsseSerialisiert { gerade, links, rechts } = input;
        dreiwege::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            links: links.into(),
            rechts: rechts.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [DreiwegeWeiche](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
struct DreiwegeWeicheSerialisiert {
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    beschreibung: Option<String>,
    steuerung:
        Option<WeicheSteuerungSerialisiert<dreiwege::Richtung, DreiwegeAnschlüsseSerialisiert>>,
}

impl From<DreiwegeWeicheSerialisiert> for dreiwege::DreiwegeWeicheSerialisiert {
    fn from(input: DreiwegeWeicheSerialisiert) -> Self {
        let DreiwegeWeicheSerialisiert { länge, radius, winkel, beschreibung, steuerung } = input;
        dreiwege::DreiwegeWeicheSerialisiert {
            länge,
            radius,
            winkel,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation einer [SKurvenWeiche](s_kurve::SKurvenWeiche).
#[derive(Deserialize)]
struct SKurvenWeicheSerialisiert {
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    radius_reverse: Skalar,
    winkel_reverse: Winkel,
    orientierung: gerade_weiche::Orientierung,
    beschreibung: Option<String>,
    steuerung:
        Option<WeicheSteuerungSerialisiert<gerade_weiche::Richtung, WeicheAnschlüsseSerialisiert>>,
}

impl From<SKurvenWeicheSerialisiert> for s_kurve::SKurvenWeicheSerialisiert {
    fn from(input: SKurvenWeicheSerialisiert) -> Self {
        let SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_reverse,
            winkel_reverse,
            orientierung,
            beschreibung,
            steuerung,
        } = input;
        s_kurve::SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_reverse,
            winkel_reverse,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation einer [Kreuzung](kreuzung::Kreuzung).
#[derive(Deserialize)]
struct KreuzungSerialisiert {
    länge: Skalar,
    radius: Skalar,
    variante: kreuzung::Variante,
    beschreibung: Option<String>,
    steuerung:
        Option<WeicheSteuerungSerialisiert<gerade_weiche::Richtung, WeicheAnschlüsseSerialisiert>>,
}

impl From<KreuzungSerialisiert> for kreuzung::KreuzungSerialisiert {
    fn from(input: KreuzungSerialisiert) -> Self {
        let KreuzungSerialisiert { länge, radius, variante, beschreibung, steuerung } = input;
        kreuzung::KreuzungSerialisiert {
            länge,
            radius,
            variante,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation der Steuerung der Stromzufuhr.
#[derive(Deserialize)]
struct StreckenabschnittSerialisiert {
    /// Die Farbe des Streckenabschnittes.
    farbe: Farbe,
    /// Die Anschlüsse des Streckenabschnittes.
    anschluss: OutputSerialisiert,
}

impl From<StreckenabschnittSerialisiert> for streckenabschnitt::StreckenabschnittSerialisiert {
    fn from(input: StreckenabschnittSerialisiert) -> Self {
        let StreckenabschnittSerialisiert { farbe, anschluss } = input;
        streckenabschnitt::StreckenabschnittSerialisiert::neu_serialisiert(farbe, anschluss.into())
    }
}

type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

/// Erlaube Deserialisieren einer geänderten Repräsentation des Leiters.
#[allow(single_use_lifetimes)]
pub trait Kompatibel: Serialisiere {
    /// Alte Serialisierbare Repräsentation des Leiters.
    type Kompatibel: Into<Self::Serialisiert> + for<'de> Deserialize<'de>;
}

#[derive(Deserialize)]
struct NonEmpty<T> {
    head: T,
    tail: Vec<T>,
}

/// Serialisierbare Repräsentation eines [Mittelleiters](Mittelleiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct MittelleiterSerialisiert(MittelleiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [Mittelleiters](Mittelleiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
enum MittelleiterSerialisiertEnum {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [Pwm-Pin](pwm::Pin).
        pin: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputSerialisiert>,
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Der Anschluss mit Überspannung zum Umdrehen der Fahrtrichtung.
        umdrehen: OutputSerialisiert,
    },
}

impl From<MittelleiterSerialisiert> for geschwindigkeit::MittelleiterSerialisiert {
    fn from(input: MittelleiterSerialisiert) -> Self {
        match input.0 {
            MittelleiterSerialisiertEnum::Pwm { pin, polarität } => {
                geschwindigkeit::MittelleiterSerialisiert::Pwm { pin, polarität }
            },
            MittelleiterSerialisiertEnum::KonstanteSpannung {
                geschwindigkeit: NonEmpty { head, tail },
                letzter_wert: _,
                umdrehen,
            } => geschwindigkeit::MittelleiterSerialisiert::KonstanteSpannung {
                geschwindigkeit: nonempty::NonEmpty {
                    head: head.into(),
                    tail: tail.into_iter().map(Into::into).collect(),
                },
                umdrehen: umdrehen.into(),
            },
        }
    }
}

impl Kompatibel for Mittelleiter {
    type Kompatibel = MittelleiterSerialisiert;
}

/// Serialisierbare Repräsentation eines [Zweileiters](Zweileiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct ZweileiterSerialisiert(ZweileiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [Zweileiters](Zweileiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
enum ZweileiterSerialisiertEnum {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [Pwm-Pin](pwm::Pin).
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
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
}

impl From<ZweileiterSerialisiert> for geschwindigkeit::ZweileiterSerialisiert {
    fn from(input: ZweileiterSerialisiert) -> Self {
        match input.0 {
            ZweileiterSerialisiertEnum::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                geschwindigkeit::ZweileiterSerialisiert::Pwm {
                    geschwindigkeit,
                    polarität,
                    fahrtrichtung: fahrtrichtung.into(),
                }
            },
            ZweileiterSerialisiertEnum::KonstanteSpannung {
                geschwindigkeit: NonEmpty { head, tail },
                letzter_wert: _,
                fahrtrichtung,
            } => geschwindigkeit::ZweileiterSerialisiert::KonstanteSpannung {
                geschwindigkeit: nonempty::NonEmpty {
                    head: head.into(),
                    tail: tail.into_iter().map(Into::into).collect(),
                },
                fahrtrichtung: fahrtrichtung.into(),
            },
        }
    }
}

impl Kompatibel for Zweileiter {
    type Kompatibel = ZweileiterSerialisiert;
}

/// Serialisierbare Repräsentation einer [Geschwindigkeit](geschwindigkeit::Geschwindigkeit).
#[derive(Deserialize)]
#[serde(bound = "")]
struct GeschwindigkeitSerialisiert<Leiter: Kompatibel> {
    /// Der Leiter der Geschwindigkeit.
    leiter: Leiter::Kompatibel,
}

impl<Leiter: Kompatibel> From<GeschwindigkeitSerialisiert<Leiter>>
    for geschwindigkeit::GeschwindigkeitSerialisiert<Leiter>
{
    fn from(input: GeschwindigkeitSerialisiert<Leiter>) -> Self {
        geschwindigkeit::GeschwindigkeitSerialisiert { leiter: input.leiter.into() }
    }
}

type GeschwindigkeitMapSerialisiert<Leiter> =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert<Leiter>>;

/// Darstellung eines [Gleises](aktuell::Gleis) bei Version 2.
#[derive(Deserialize)]
struct Gleis<T> {
    definition: T,
    position: Position,
    streckenabschnitt: Option<streckenabschnitt::Name>,
}

#[derive(Deserialize)]
#[serde(bound = "Leiter: Kompatibel")]
pub(crate) struct GleiseVecs<Leiter: Kompatibel> {
    /// Der Name des gespeicherten Zugtyps.
    name: String,
    geraden: Vec<Gleis<GeradeSerialisiert>>,
    kurven: Vec<Gleis<KurveSerialisiert>>,
    weichen: Vec<Gleis<WeicheSerialisiert>>,
    dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
    streckenabschnitte: StreckenabschnittMapSerialisiert,
    geschwindigkeiten: GeschwindigkeitMapSerialisiert<Leiter>,
    pläne: HashMap<plan::Name, Void>,
}

impl<Leiter: Serialisiere + BekannterLeiter + Kompatibel> TryFrom<GleiseVecs<Leiter>>
    for aktuell::de_serialisieren::ZustandSerialisiert<Leiter>
{
    type Error = anschluss::Fehler;

    fn try_from(v2: GleiseVecs<Leiter>) -> Result<Self, Self::Error> {
        let leiter = Leiter::NAME;
        let zugtyp = match Leiter::bekannter_zugtyp(&v2.name) {
            Some(zugtyp) => zugtyp,
            None => return Err(anschluss::Fehler::UnbekannterZugtyp { zugtyp: v2.name, leiter }),
        };

        let mut ohne_streckenabschnitt = aktuell::de_serialisieren::GleiseDatenSerialisiert::neu();
        let mut streckenabschnitte: HashMap<_, _> = v2
            .streckenabschnitte
            .into_iter()
            .map(|(name, streckenabschnitt)| {
                (
                    name,
                    (
                        streckenabschnitt.into(),
                        aktuell::de_serialisieren::GleiseDatenSerialisiert::neu(),
                    ),
                )
            })
            .collect();
        macro_rules! verteile_gleise {
            ($($gleis: ident),*) => {
                $(for Gleis { definition, position, streckenabschnitt } in v2.$gleis.into_iter() {
                    let daten = if let Some((_streckenabschnitt, daten)) =
                        streckenabschnitt.and_then(|name| streckenabschnitte.get_mut(&name))
                    {
                        daten
                    } else {
                        &mut ohne_streckenabschnitt
                    };
                    daten.$gleis.push(aktuell::Gleis {definition: definition.into(), position})
                })*
            };
        }
        verteile_gleise! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
        let geschwindigkeiten = v2
            .geschwindigkeiten
            .into_iter()
            .map(|(name, geschwindigkeit)| {
                (
                    name,
                    (
                        geschwindigkeit.into(),
                        aktuell::de_serialisieren::StreckenabschnittMapSerialisiert::new(),
                    ),
                )
            })
            .collect();
        Ok(aktuell::de_serialisieren::ZustandSerialisiert {
            zugtyp: zugtyp.into(),
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne.into_iter().map(|(_name, void)| void.unreachable()).collect(),
        })
    }
}
