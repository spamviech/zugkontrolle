//! Serialisierte Strukturen von Version 2.X, die mit Version 3.0.0 geändert wurden.

use std::collections::HashMap;

use serde::Deserialize;

use zugkontrolle_anschluss::{
    level::Level,
    pcf8574::{self, I2cBus},
    pin::pwm,
    polarität::Polarität,
    trigger::Trigger,
    InputSerialisiert as V4_InputSerialisiert, OutputSerialisiert as V4_OutputSerialisiert,
};
use zugkontrolle_gleis::{
    steuerung::{
        geschwindigkeit::{self, BekannterLeiter, Mittelleiter, Zweileiter},
        kontakt, plan, streckenabschnitt, weiche,
    },
    zugtyp::Zugtyp,
};
use zugkontrolle_typen::{canvas::Position, farbe::Farbe, skalar::Skalar, winkel::Winkel};
use zugkontrolle_util::{eingeschränkt::kleiner_8, void::Void};

use crate::daten::{
    de_serialisieren::LadenFehler,
    v3::{self, kreuzung, weiche::orientierung::Orientierung},
    v4::ZugtypSerialisiert,
};

/// Beschreibung eines [`anschluss::pcf85747::Pcf8574`].
#[derive(Deserialize)]
struct Pcf8574Beschreibung {
    /// Anliegendes [`Level`] an das `A0` Adress-Bit.
    a0: Level,
    /// Anliegendes [`Level`] an das `A1` Adress-Bit.
    a1: Level,
    /// Anliegendes [`Level`] an das `A2` Adress-Bit.
    a2: Level,
    /// Variante des [`anschluss::pcf85747::Pcf8574`], beeinflusst die I2C-Adresse.
    variante: pcf8574::Variante,
}

impl From<Pcf8574Beschreibung> for pcf8574::Beschreibung {
    fn from(Pcf8574Beschreibung { a0, a1, a2, variante }: Pcf8574Beschreibung) -> Self {
        pcf8574::Beschreibung { i2c_bus: I2cBus::I2c0_1, a0, a1, a2, variante }
    }
}

/// Serialisierbare Informationen eines [`OutputAnschluss`]es.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
enum OutputSerialisiert {
    /// Ein [`Pin`](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
}

impl From<OutputSerialisiert> for zugkontrolle_anschluss::OutputSerialisiert {
    fn from(input: OutputSerialisiert) -> Self {
        match input {
            OutputSerialisiert::Pin { pin, polarität } => {
                V4_OutputSerialisiert::Pin { pin, polarität }
            },
            OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                V4_OutputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    polarität,
                }
            },
        }
    }
}

/// Serialisierbare Informationen eines [`InputAnschlusses`](anschluss::InputAnschluss).
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Deserialize)]
enum InputSerialisiert {
    /// Ein [`Pin`](input::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::InputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: Pcf8574Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Der konfigurierte Interrupt-Pin des Pcf8574.
        interrupt: Option<u8>,
    },
}

impl From<InputSerialisiert> for zugkontrolle_anschluss::InputSerialisiert {
    fn from(input: InputSerialisiert) -> Self {
        match input {
            InputSerialisiert::Pin { pin } => V4_InputSerialisiert::Pin { pin },
            InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt } => {
                V4_InputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung.into(),
                    port,
                    interrupt,
                }
            },
        }
    }
}

/// Serialisierte Variante eines [`Kontaktes`](Kontakt).
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

/// Serialisierbare Repräsentation einer [`Gerade`](gerade::Gerade).
#[derive(Deserialize)]
struct GeradeSerialisiert {
    /// Die Länge der Geraden.
    länge: Skalar,
    /// Die Beschreibung der Geraden.
    beschreibung: Option<String>,
    /// Der mit der Geraden assoziierte Kontakt.
    kontakt: Option<KontaktSerialisiert>,
}

impl From<GeradeSerialisiert> for v3::gerade::GeradeSerialisiert {
    fn from(input: GeradeSerialisiert) -> Self {
        let GeradeSerialisiert { länge, beschreibung, kontakt } = input;
        v3::gerade::GeradeSerialisiert { länge, beschreibung, kontakt: kontakt.map(Into::into) }
    }
}

/// Serialisierbare Repräsentation einer [`Kurve`](kurve::Kurve).
#[derive(Deserialize)]
struct KurveSerialisiert {
    /// Der Radius der Kurve.
    radius: Skalar,
    /// Der Winkel, wie lange die Kurve geht.
    winkel: Winkel,
    /// Die Beschreibung der Kurve.
    beschreibung: Option<String>,
    /// Der Kontakt assoziiert mit der Kurve.
    kontakt: Option<KontaktSerialisiert>,
}

impl From<KurveSerialisiert> for v3::kurve::KurveSerialisiert {
    fn from(input: KurveSerialisiert) -> Self {
        let KurveSerialisiert { radius, winkel, beschreibung, kontakt } = input;
        v3::kurve::KurveSerialisiert {
            radius,
            winkel,
            beschreibung,
            kontakt: kontakt.map(Into::into),
        }
    }
}

/// Die serialisierbare Darstellung der Steuerung einer Weiche.
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

/// Die aktuelle und vorherige Richtung.
struct AktuellUndBisher<R> {
    #[allow(clippy::missing_docs_in_private_items)]
    aktuelle_richtung: R,
    #[allow(clippy::missing_docs_in_private_items)]
    letzte_richtung: R,
}

/// Hilfs-Type für alternative [`From`]-Implementierungen.
struct Wrapper<T>(T);

impl<R> From<AktuellUndBisher<R>> for Wrapper<R> {
    fn from(input: AktuellUndBisher<R>) -> Self {
        Wrapper(input.aktuelle_richtung)
    }
}

impl From<AktuellUndBisher<v3::weiche::dreiwege::Richtung>>
    for Wrapper<v3::weiche::dreiwege::RichtungInformation>
{
    fn from(input: AktuellUndBisher<v3::weiche::dreiwege::Richtung>) -> Self {
        let AktuellUndBisher { aktuelle_richtung, letzte_richtung } = input;
        Wrapper(v3::weiche::dreiwege::RichtungInformation { aktuelle_richtung, letzte_richtung })
    }
}

impl<R1, A1> WeicheSteuerungSerialisiert<R1, A1> {
    /// Konvertiere in die Darstellung, wie sie in Version 3 verwendet wird.
    fn konvertiere<R2, A2>(self) -> v3::weiche::steuerung::WeicheSerialisiert<R2, A2>
    where
        Wrapper<R2>: From<AktuellUndBisher<R1>>,
        A2: From<A1>,
    {
        let WeicheSteuerungSerialisiert { name, aktuelle_richtung, letzte_richtung, anschlüsse } =
            self;
        v3::weiche::steuerung::WeicheSerialisiert::neu(
            v3::weiche::steuerung::Name(name.0),
            Wrapper::from(AktuellUndBisher { aktuelle_richtung, letzte_richtung }).0,
            anschlüsse.into(),
        )
    }
}

/// Serialisierbare Repräsentation der [`Anschlüsse`](gerade_weiche::RichtungAnschlüsse)
/// einer [`Weiche`](gerade_weiche::Weiche).
#[derive(Deserialize)]
struct WeicheAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die Gerade.
    gerade: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die Kurve.
    kurve: OutputSerialisiert,
}

impl From<WeicheAnschlüsseSerialisiert> for v3::weiche::gerade::RichtungAnschlüsseSerialisiert {
    fn from(input: WeicheAnschlüsseSerialisiert) -> Self {
        let WeicheAnschlüsseSerialisiert { gerade, kurve } = input;
        v3::weiche::gerade::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            kurve: kurve.into(),
        }
    }
}

// FIXME richtung, orientierung, variante in v2 definieren -> use in v3
// FIXME Name in v2 definieren

/// Serialisierbare Repräsentation einer [`Weiche`](v3::weiche::gerade::Weiche).
#[derive(Deserialize)]
struct WeicheSerialisiert {
    /// Die Länge der Geraden der Weiche.
    länge: Skalar,
    /// Der Radius der Kurve einer Weiche.
    radius: Skalar,
    /// Der Winkel der Kurve einer Weiche.
    winkel: Winkel,
    /// Die Orientierung der Kurve einer Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<WeicheSerialisiert> for v3::weiche::gerade::WeicheSerialisiert {
    fn from(input: WeicheSerialisiert) -> Self {
        let WeicheSerialisiert { länge, radius, winkel, orientierung, beschreibung, steuerung } =
            input;
        v3::weiche::gerade::WeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation der [`Anschlüsse`](kurven_weiche::RichtungAnschlüsse)
/// einer [`KurvenWeiche`](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
struct KurvenWeicheAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die innere Kurve.
    innen: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die äußere Kurve.
    außen: OutputSerialisiert,
}

impl From<KurvenWeicheAnschlüsseSerialisiert>
    for v3::weiche::kurve::RichtungAnschlüsseSerialisiert
{
    fn from(input: KurvenWeicheAnschlüsseSerialisiert) -> Self {
        let KurvenWeicheAnschlüsseSerialisiert { innen, außen } = input;
        v3::weiche::kurve::RichtungAnschlüsseSerialisiert {
            innen: innen.into(),
            außen: außen.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [`KurvenWeiche`](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
struct KurvenWeicheSerialisiert {
    /// Die Länge der Geraden, bevor die äußere Kurve beginnt.
    länge: Skalar,
    /// Der Radius der Kurven.
    radius: Skalar,
    /// Der Winkel der Kurven.
    winkel: Winkel,
    /// Die Orientierung der Kurven der Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<
            v3::weiche::kurve::Richtung,
            KurvenWeicheAnschlüsseSerialisiert,
        >,
    >,
}

impl From<KurvenWeicheSerialisiert> for v3::weiche::kurve::KurvenWeicheSerialisiert {
    fn from(input: KurvenWeicheSerialisiert) -> Self {
        let KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung,
        } = input;
        v3::weiche::kurve::KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation der [`Anschlüsse`](dreiwege::RichtungAnschlüsse)
/// einer [`DreiwegeWeiche`](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
struct DreiwegeAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die Gerade.
    gerade: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die linke Kurve.
    links: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die rechte Kurve.
    rechts: OutputSerialisiert,
}

impl From<DreiwegeAnschlüsseSerialisiert>
    for v3::weiche::dreiwege::RichtungAnschlüsseSerialisiert
{
    fn from(input: DreiwegeAnschlüsseSerialisiert) -> Self {
        let DreiwegeAnschlüsseSerialisiert { gerade, links, rechts } = input;
        v3::weiche::dreiwege::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            links: links.into(),
            rechts: rechts.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [`DreiwegeWeiche`](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
struct DreiwegeWeicheSerialisiert {
    /// Die Länge der Gerade der Weiche.
    länge: Skalar,
    /// Der Radius der Kurven der Weiche.
    radius: Skalar,
    /// Der Winkel der Kurven der Weiche
    winkel: Winkel,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::dreiwege::Richtung, DreiwegeAnschlüsseSerialisiert>,
    >,
}

impl From<DreiwegeWeicheSerialisiert> for v3::weiche::dreiwege::DreiwegeWeicheSerialisiert {
    fn from(input: DreiwegeWeicheSerialisiert) -> Self {
        let DreiwegeWeicheSerialisiert { länge, radius, winkel, beschreibung, steuerung } = input;
        v3::weiche::dreiwege::DreiwegeWeicheSerialisiert {
            länge,
            radius,
            winkel,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation einer [`SKurvenWeiche`](s_kurve::SKurvenWeiche).
#[derive(Deserialize)]
struct SKurvenWeicheSerialisiert {
    /// Die Länge der Gerade der Weiche.
    länge: Skalar,
    /// Der Radius der nach außen gehenden Kurve der Weiche.
    radius: Skalar,
    /// Der Winkel der nach außen gehenden Kurve der Weiche.
    winkel: Winkel,
    /// Der Radius der nach innen gehenden Kurve der Weiche.
    radius_reverse: Skalar,
    /// Der Winkel der nach innen gehenden Kurve der Weiche.
    winkel_reverse: Winkel,
    /// Die Orientierung der Kurve der Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<SKurvenWeicheSerialisiert> for v3::weiche::s_kurve::SKurvenWeicheSerialisiert {
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
        v3::weiche::s_kurve::SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen: radius_reverse,
            winkel_kurve_nach_innen: winkel_reverse,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}

/// Serialisierbare Repräsentation einer [`Kreuzung`](kreuzung::Kreuzung).
#[derive(Deserialize)]
struct KreuzungSerialisiert {
    /// Die Länge der Geraden der Kreuzung.
    länge: Skalar,
    /// Der Radius der Kurven der Kreuzung.
    radius: Skalar,
    /// Sind die Kurven Teil der Kreuzung.
    variante: kreuzung::Variante,
    /// Die Beschreibung der Kreuzung.
    beschreibung: Option<String>,
    /// Die Steuerung der Kreuzung.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<KreuzungSerialisiert> for v3::kreuzung::KreuzungSerialisiert {
    fn from(input: KreuzungSerialisiert) -> Self {
        let KreuzungSerialisiert { länge, radius, variante, beschreibung, steuerung } = input;
        v3::kreuzung::KreuzungSerialisiert {
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

/// Streckenabschnitte mit ihrem Namen.
type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, StreckenabschnittSerialisiert>;

/// Ein unterstützten Leiter, mit über Namen identifizierten Zugtypen. Aktuell:
/// - [`Mittelleiter`] mit "Märklin".
/// - [`Zweileiter`] mit "Lego".
pub trait BekannterZugtyp: BekannterLeiter {
    /// Serialisierbare Repräsentation in v2.*
    type V2;

    /// Erzeuge einen Zugtyp mit der entsprechenden Leiter-Art, ausgehend von seinem Namen.
    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>>;
}

/// Nicht-leerer Vektor.
///
/// Explizit definiert, anstelle [`nonempty::NonEmpty`],
/// damit die [`Deserialize`]-Implementierung übereinstimmt.
#[derive(Deserialize)]
struct NonEmpty<T> {
    /// Das erste Element.
    head: T,
    /// Alle folgenden Elemente.
    tail: Vec<T>,
}

/// Serialisierbare Repräsentation eines [`Mittelleiters`](Mittelleiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct MittelleiterSerialisiert(MittelleiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [`Mittelleiters`](Mittelleiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
enum MittelleiterSerialisiertEnum {
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
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Der Anschluss mit Überspannung zum Umdrehen der Fahrtrichtung.
        umdrehen: OutputSerialisiert,
    },
}

impl BekannterZugtyp for Mittelleiter {
    type V2 = MittelleiterSerialisiert;

    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>> {
        (name == "Märklin").then(|| ZugtypSerialisiert::from(Zugtyp::märklin()).v3())
    }
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

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct ZweileiterSerialisiert(ZweileiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
enum ZweileiterSerialisiertEnum {
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
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
}

impl BekannterZugtyp for Zweileiter {
    type V2 = ZweileiterSerialisiert;

    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>> {
        (name == "Lego").then(|| ZugtypSerialisiert::from(Zugtyp::lego()).v3())
    }
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

/// Serialisierbare Repräsentation einer [`Geschwindigkeit`](geschwindigkeit::Geschwindigkeit).
#[derive(Deserialize)]
struct GeschwindigkeitSerialisiert<LeiterV2> {
    /// Der Leiter der Geschwindigkeit.
    leiter: LeiterV2,
}

impl<LeiterV2, LeiterSerialisiert> From<GeschwindigkeitSerialisiert<LeiterV2>>
    for geschwindigkeit::GeschwindigkeitSerialisiert<LeiterSerialisiert>
where
    LeiterV2: Into<LeiterSerialisiert>,
{
    fn from(input: GeschwindigkeitSerialisiert<LeiterV2>) -> Self {
        geschwindigkeit::GeschwindigkeitSerialisiert { leiter: input.leiter.into() }
    }
}

/// Geschwindigkeiten mit ihrem Namen.
type GeschwindigkeitMapSerialisiert<LeiterV2> =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert<LeiterV2>>;

/// Darstellung eines [`Gleises`](aktuell::Gleis) bei Version 2.
#[derive(Deserialize)]
struct Gleis<T> {
    /// Die Definition des Gleises.
    definition: T,
    /// Die Position des Gleises.
    position: Position,
    /// Der Name des assoziierten Streckenabschnittes.
    streckenabschnitt: Option<streckenabschnitt::Name>,
}

/// Der serialisierbare Zustand, wie er in Version 2 verwendet wurde.
#[derive(Deserialize)]
pub(crate) struct GleiseVecs<LeiterV2> {
    /// Der Name des gespeicherten Zugtyps.
    name: String,
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: Vec<Gleis<GeradeSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: Vec<Gleis<KurveSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: Vec<Gleis<WeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
    #[allow(clippy::missing_docs_in_private_items)]
    streckenabschnitte: StreckenabschnittMapSerialisiert,
    #[allow(clippy::missing_docs_in_private_items)]
    geschwindigkeiten: GeschwindigkeitMapSerialisiert<LeiterV2>,
    /// Die Pläne. In Version 2 wurden keine Pläne unterstützt.
    #[allow(clippy::zero_sized_map_values)]
    pläne: HashMap<plan::Name, Void>,
}

impl<L: 'static + BekannterZugtyp, S: From<<L as BekannterZugtyp>::V2>>
    TryFrom<GleiseVecs<<L as BekannterZugtyp>::V2>> for v3::ZustandSerialisiert<L, S>
{
    type Error = LadenFehler<S>;

    fn try_from(v2: GleiseVecs<<L as BekannterZugtyp>::V2>) -> Result<Self, Self::Error> {
        let Some(zugtyp) = L::bekannter_zugtyp(&v2.name) else {
            return Err(LadenFehler::UnbekannterZugtyp { zugtyp: v2.name, leiter: L::NAME });
        };

        let mut ohne_streckenabschnitt = v3::GleiseDatenSerialisiert::neu();
        let mut streckenabschnitte: HashMap<_, _> = v2
            .streckenabschnitte
            .into_iter()
            .map(|(name, streckenabschnitt)| {
                (name, (streckenabschnitt.into(), v3::GleiseDatenSerialisiert::neu()))
            })
            .collect();
        /// Verteile die Gleise in den `GleiseDaten` passend zum assoziiertem Streckenabschnitt.
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
                    daten.$gleis.push(v3::Gleis {definition: definition.into(), position})
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
                (name, (geschwindigkeit.into(), v3::StreckenabschnittMapSerialisiert::new()))
            })
            .collect();
        Ok(v3::ZustandSerialisiert {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit: streckenabschnitte,
            geschwindigkeiten,
            pläne: v2.pläne.into_values().map(|void| void.unreachable()).collect(),
        })
    }
}
