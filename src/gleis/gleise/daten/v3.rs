//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{collections::HashMap, fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::de_serialisieren::Anschlüsse,
    gleis::{
        gerade::{GeradeSerialisiert, GeradeUnit},
        kreuzung::{KreuzungSerialisiert, KreuzungUnit},
        kurve::{KurveSerialisiert, KurveUnit},
        weiche::{
            dreiwege::{DreiwegeWeicheSerialisiert, DreiwegeWeicheUnit},
            gerade::{WeicheSerialisiert, WeicheUnit},
            kurve::{KurvenWeicheSerialisiert, KurvenWeicheUnit},
            s_kurve::{SKurvenWeicheSerialisiert, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert, Leiter},
        kontakt::KontaktSerialisiert,
        plan::{self, PlanSerialisiert},
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    typen::{canvas::Position, mm::Spurweite, skalar::Skalar, winkel::Winkel},
    util::eingeschränkt::NichtNegativ,
};

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [Anschlüsse](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [Canvas](iced::widget::canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct GleiseDatenSerialisiert {
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
}

impl From<GleiseDatenSerialisiert>
    for crate::gleis::gleise::daten::de_serialisieren::GleiseDatenSerialisiert
{
    fn from(value: GleiseDatenSerialisiert) -> Self {
        todo!()
    }
}

pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
pub(in crate::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<LeiterSerialisiert> =
    HashMap<
        geschwindigkeit::Name,
        (GeschwindigkeitSerialisiert<LeiterSerialisiert>, StreckenabschnittMapSerialisiert),
    >;

#[derive(zugkontrolle_macros::Debug, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[serde(bound(
    serialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize, <L as Leiter>::Fahrtrichtung: Serialize, S: Serialize",
    deserialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>, S: Deserialize<'de>",
))]
pub(in crate::gleis::gleise) struct ZustandSerialisiert<L: Leiter, S> {
    pub(crate) zugtyp: ZugtypSerialisiert<L>,
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<S>,
    pub(crate) pläne: HashMap<plan::Name, PlanSerialisiert<L, S>>,
}

impl<L: Leiter, S> From<ZustandSerialisiert<L, S>>
    for crate::gleis::gleise::daten::de_serialisieren::ZustandSerialisiert<L, S>
{
    fn from(value: ZustandSerialisiert<L, S>) -> Self {
        todo!()
    }
}

// FIXME in der selben Datei definiert + use * zum einfacheren Sammeln
use self::zugtyp::*;
pub mod zugtyp {
    use super::*;

    /// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
    #[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
    #[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
    #[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
    #[serde(bound(
        serialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize",
        deserialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>",
    ))]
    pub struct ZugtypSerialisiert<L: Leiter> {
        /// Der Name des Zugtyps.
        pub name: String,
        /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
        pub leiter: String,
        /// Spurweite
        pub spurweite: Spurweite,
        /// Alle unterstützten [Geraden](crate::gleis::gerade::Gerade).
        pub geraden: Vec<GeradeUnit>,
        /// Alle unterstützten [Kurven](crate::gleis::kurve::Kurve).
        pub kurven: Vec<KurveUnit>,
        /// Alle unterstützten [Weichen](crate::gleis::weiche::gerade::Weiche).
        pub weichen: Vec<WeicheUnit>,
        /// Alle unterstützten [Dreiwege-Weichen](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
        pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
        /// Alle unterstützten [Kurven-Weichen](crate::gleis::weiche::kurve::KurvenWeiche).
        pub kurven_weichen: Vec<KurvenWeicheUnit>,
        /// Alle unterstützten [S-Kurven-Weichen](crate::gleis::weiche::s_kurve::SKurvenWeiche).
        pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
        /// Alle unterstützten [Kreuzungen](crate::gleis::kreuzung::Kreuzung).
        pub kreuzungen: Vec<KreuzungUnit>,
        /// Frequenz in Herz für den Pwm-Antrieb.
        pub pwm_frequenz: NichtNegativ,
        /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
        pub verhältnis_fahrspannung_überspannung:
            <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        /// Zeit zum Anhalten vor dem Umdrehen.
        pub stopp_zeit: Duration,
        /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
        pub umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
        /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
        pub schalten_zeit: Duration,
    }
}

// FIXME in der selben Datei definiert + use * zum einfacheren Sammeln
use self::gleis::*;
pub mod gleis {
    use super::*;

    use self::gerade::*;
    pub mod gerade {
        use super::*;

        /// Definition einer Gerade.
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub struct GeradeSerialisiert {
            /// Die Länge der Gerade auf dem [Canvas](iced::widget::canvas::Canvas).
            pub länge: Skalar,
            /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Der Anschluss für einen [Kontakt] an der Schiene.
            pub kontakt: Option<KontaktSerialisiert>,
        }
    }

    use self::kreuzung::*;
    pub mod kreuzung {
        use super::*;

        use super::weiche::gerade::{Richtung, RichtungAnschlüsseSerialisiert};

        /// Definition einer Kreuzung.
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub struct KreuzungSerialisiert {
            /// Die Länge der Geraden.
            pub länge: Skalar,
            /// Der Kurvenradius; legt automatisch den Winkel fest.
            pub radius: Skalar,
            /// Werden die Kurven gezeichnet, oder nur die Geraden.
            pub variante: Variante,
            /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Die Anschlüsse zum Schalten der Kreuzung.
            pub steuerung: AnschlüsseSerialisiert,
        }

        /// Werden die Kurven gezeichnet, oder nur die Geraden.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        pub enum Variante {
            /// Zeichne die Kurven und die Geraden.
            MitKurve,
            /// Zeichne nur die Geraden.
            OhneKurve,
        }

        type AnschlüsseSerialisiert =
            super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;
    }

    use self::kurve::*;
    pub mod kurve {
        use super::*;

        /// Definition einer Kurve.
        ///
        /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub struct KurveSerialisiert {
            /// Der Radius auf dem Canvas.
            pub radius: Skalar,
            /// Der Winkel der Kurve.
            pub winkel: Winkel,
            /// Eine allgemeine Beschreibung der Kurve, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Der Anschluss für einen [Kontakt] an der Schiene.
            pub kontakt: Option<KontaktSerialisiert>,
        }
    }

    use self::weiche::*;
    pub mod weiche {
        use super::*;

        use self::dreiwege::*;
        pub mod dreiwege {
            use super::*;

            type AnschlüsseSerialisiert = super::steuerung::WeicheSerialisiert<
                RichtungInformation,
                RichtungAnschlüsseSerialisiert,
            >;

            /// Die aktuelle und letzte [Richtung] einer [DreiwegeWeiche].
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            pub struct RichtungInformation {
                /// Die aktuelle [Richtung] der [DreiwegeWeiche].
                pub aktuelle_richtung: Richtung,
                /// Die [Richtung] vor der aktuellen [Richtung].
                pub letzte_richtung: Richtung,
            }

            /// Definition einer Dreiwege-Weiche.
            ///
            /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct DreiwegeWeicheSerialisiert {
                /// Die Länge der Gerade.
                pub länge: Skalar,
                /// Der Radius der Kurven.
                pub radius: Skalar,
                /// Der Winkel der Kurven.
                pub winkel: Winkel,
                /// Eine allgemeine Beschreibung der DreiwegeWeiche, z.B. die Produktnummer.
                pub beschreibung: Option<String>,
                /// Die Anschlüsse zum Schalten der DreiwegeWeiche.
                pub steuerung: AnschlüsseSerialisiert,
            }

            #[doc = r" Mögliche Richtungen zum Schalten."]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            pub enum Richtung {
                #[allow(missing_docs)]
                Gerade,
                #[allow(missing_docs)]
                Links,
                #[allow(missing_docs)]
                Rechts,
            }
            #[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
            #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
            pub struct RichtungAnschlüsseSerialisiert {
                #[doc = "[Richtung::Gerade]"]
                pub gerade: crate::anschluss::OutputSerialisiert,
                #[doc = "[Richtung::Links]"]
                pub links: crate::anschluss::OutputSerialisiert,
                #[doc = "[Richtung::Rechts]"]
                pub rechts: crate::anschluss::OutputSerialisiert,
            }
        }

        use self::gerade::*;
        pub mod gerade {
            use super::*;

            use super::orientierung::Orientierung;

            type AnschlüsseSerialisiert =
                super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

            /// Definition einer Weiche.
            ///
            /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct Weiche<Anschlüsse = Option<self::Anschlüsse>> {
                /// Die Länge der Geraden.
                pub länge: Skalar,
                /// Der Radius der Kurve.
                pub radius: Skalar,
                /// Der Winkel der Kurve.
                pub winkel: Winkel,
                /// Die Orientierung der Weiche.
                pub orientierung: Orientierung,
                /// Eine allgemeine Beschreibung der Weiche, z.B. die Produktnummer.
                pub beschreibung: Option<String>,
                /// Die Anschlüsse zum Schalten der Weiche.
                pub steuerung: Anschlüsse,
            }

            #[doc = r" Eine Variante ohne Anschlüsse."]
            pub type WeicheSerialisiert = Weiche<Option<AnschlüsseSerialisiert>>;
            #[doc = r" Eine serialisierbare Repräsentation."]
            pub type WeicheUnit = Weiche<()>;

            #[doc = r" Mögliche Richtungen zum Schalten."]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            pub enum Richtung {
                #[allow(missing_docs)]
                Gerade,
                #[allow(missing_docs)]
                Kurve,
            }
            #[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
            #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
            pub struct RichtungAnschlüsseSerialisiert {
                #[doc = "[Richtung::Gerade]"]
                pub gerade: crate::anschluss::OutputSerialisiert,
                #[doc = "[Richtung::Kurve]"]
                pub kurve: crate::anschluss::OutputSerialisiert,
            }
        }

        use self::kurve::*;
        pub mod kurve {
            use super::*;

            use super::orientierung::Orientierung;

            type AnschlüsseSerialisiert = super::steuerung::WeicheSerialisiert<
                super::gerade::Richtung,
                super::gerade::RichtungAnschlüsseSerialisiert,
            >;

            /// Definition einer Kurven-Weiche.
            ///
            /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct KurvenWeicheSerialisiert {
                /// Die Länge der Geraden vor der äußeren Kurve.
                pub länge: Skalar,
                /// Der Radius der Kurven.
                pub radius: Skalar,
                /// Der Winkel der Kurven.
                pub winkel: Winkel,
                /// Die Orientierung der KurvenWeiche.
                pub orientierung: Orientierung,
                /// Eine allgemeine Beschreibung der KurvenWeiche, z.B. die Produktnummer.
                pub beschreibung: Option<String>,
                /// Die Anschlüsse zum Schalten der KurvenWeiche.
                pub steuerung: AnschlüsseSerialisiert,
            }
        }

        use self::s_kurve::*;
        pub mod s_kurve {
            use super::*;

            type AnschlüsseSerialisiert = super::steuerung::WeicheSerialisiert<
                super::gerade::Richtung,
                super::gerade::RichtungAnschlüsseSerialisiert,
            >;

            /// Definition einer Weiche mit S-Kurve.
            ///
            /// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct SKurvenWeicheSerialisiert {
                /// Die Länge der Geraden.
                pub länge: Skalar,
                /// Der Radius der Kurve nach außen.
                pub radius: Skalar,
                /// Der Winkel der Kurve nach außen.
                pub winkel: Winkel,
                /// Der Radius der Kurve nach innen.
                pub radius_kurve_nach_innen: Skalar,
                /// Der Winkel der Kurve nach innen.
                pub winkel_kurve_nach_innen: Winkel,
                /// Die Orientierung der SKurvenWeiche.
                pub orientierung: Orientierung,
                /// Eine allgemeine Beschreibung der SKurvenWeiche, z.B. die Produktnummer.
                pub beschreibung: Option<String>,
                /// Die Anschlüsse zum Schalten der SKurvenWeiche.
                pub steuerung: AnschlüsseSerialisiert,
            }
        }

        use self::orientierung::*;
        pub mod orientierung {
            use super::*;

            /// Die Orientierung einer [Weiche], in welche Richtung geht die Kurve.
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
            pub enum Orientierung {
                /// Die Kurve geht nach links.
                Links,
                /// Die Kurve geht nach rechts.
                Rechts,
            }
        }
    }

    use self::steuerung::*;
    pub mod steuerung {
        use super::*;

        /// Serialisierbare Repräsentation der Steuerung einer [Weiche].
        #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
        pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
            /// Der Name der Weiche.
            pub name: Name,
            /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
            pub richtung: Richtung,
            /// Die Anschlüsse der Weiche.
            pub anschlüsse: Anschlüsse,
        }

        /// Name einer [Weiche](WeicheSerialisiert).
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        pub struct Name(pub String);
    }
}
