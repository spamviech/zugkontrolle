//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    io,
    marker::PhantomData,
    sync::{mpsc::Sender, Arc},
    time::Duration,
};

use bincode::config::{
    DefaultOptions, FixintEncoding, Options, RejectTrailing, WithOtherIntEncoding,
    WithOtherTrailing,
};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree, RTreeObject, SelectionFunction, AABB,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        OutputSerialisiert,
    },
    gleis::{
        gerade::{Gerade, GeradeSerialisiert, GeradeUnit},
        gleise::{
            self,
            daten::v2::{self, BekannterZugtyp},
            id::{
                eindeutig::KeineIdVerfügbar, AnyId, AnyIdRef, GleisId, GleisIdRef,
                StreckenabschnittId, StreckenabschnittIdRef,
            },
            steuerung::{MitSteuerung, SomeAktualisierenSender},
            Fehler, GeschwindigkeitEntferntFehler, GleisIdFehler, Gleise,
            StreckenabschnittIdFehler,
        },
        kreuzung::{Kreuzung, KreuzungSerialisiert, KreuzungUnit},
        kurve::{Kurve, KurveSerialisiert, KurveUnit},
        verbindung::{self, Verbindung},
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheSerialisiert, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheSerialisiert, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheSerialisiert, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheSerialisiert, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        geschwindigkeit::{
            self, BekannterLeiter, Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter,
        },
        kontakt::{Kontakt, KontaktSerialisiert},
        plan::{self, Plan, PlanSerialisiert, UnbekannteAnschlüsse},
        streckenabschnitt::{self, Streckenabschnitt, StreckenabschnittSerialisiert},
    },
    typen::{
        canvas::Position, mm::Spurweite, rechteck::Rechteck, skalar::Skalar, vektor::Vektor,
        winkel::Winkel, Zeichnen,
    },
    util::{eingeschränkt::NichtNegativ, nachschlagen::Nachschlagen},
    zugtyp::ZugtypDeserialisierenFehler,
};

/// Definition und Position eines Gleises.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    /// Wie sieht da Gleis aus, welche [Anschlüsse](anschluss::Anschluss) hat es.
    pub definition: T,
    /// Wo auf dem [Canvas](iced::widget::canvas::Canvas) wird das Gleis gezeichnet.
    pub position: Position,
}

#[allow(single_use_lifetimes)]
impl<T, S> Serialisiere<Gleis<S>> for Gleis<T>
where
    T: Serialisiere<S>,
    S:,
{
    fn serialisiere(&self) -> Gleis<S> {
        Gleis { definition: self.definition.serialisiere(), position: self.position.clone() }
    }

    fn anschlüsse(self) -> Anschlüsse {
        self.definition.anschlüsse()
    }
}

impl<R, T: Reserviere<R>> Reserviere<Gleis<R>> for Gleis<T> {
    type Arg = <T as Reserviere<R>>::Arg;

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: Self::Arg,
    ) -> Ergebnis<Gleis<R>> {
        let Gleis { definition, position } = self;
        definition
            .reserviere(lager, anschlüsse, arg)
            .konvertiere(|definition| Gleis { definition, position })
    }
}

pub(crate) type StreckenabschnittMap =
    HashMap<streckenabschnitt::Name, (Streckenabschnitt, GleiseDaten)>;
type GeschwindigkeitMap<Leiter> =
    HashMap<geschwindigkeit::Name, (Geschwindigkeit<Leiter>, StreckenabschnittMap)>;

/// Alle [Gleise](Gleis), [Geschwindigkeiten](Geschwindigkeit) und [Streckenabschnitte](Streckenabschnitt),
/// sowie der verwendete [Zugtyp].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub(in crate::gleis::gleise) struct Zustand<L: Leiter> {
    pub(in crate::gleis::gleise) zugtyp: Zugtyp<L>,
    pub(in crate::gleis::gleise) ohne_streckenabschnitt: GleiseDaten,
    pub(in crate::gleis::gleise) ohne_geschwindigkeit: StreckenabschnittMap,
    pub(in crate::gleis::gleise) geschwindigkeiten: GeschwindigkeitMap<L>,
    pub(in crate::gleis::gleise) pläne: HashMap<plan::Name, Plan<L>>,
}

pub(in crate::gleis::gleise) type RStern<T> = RTree<GeomWithData<Rectangle<Vektor>, Gleis<T>>>;

#[derive(Debug)]
pub(crate) struct GleiseDaten {
    pub(in crate::gleis::gleise) geraden: RStern<Gerade>,
    pub(in crate::gleis::gleise) kurven: RStern<Kurve>,
    pub(in crate::gleis::gleise) weichen: RStern<Weiche>,
    pub(in crate::gleis::gleise) dreiwege_weichen: RStern<DreiwegeWeiche>,
    pub(in crate::gleis::gleise) kurven_weichen: RStern<KurvenWeiche>,
    pub(in crate::gleis::gleise) s_kurven_weichen: RStern<SKurvenWeiche>,
    pub(in crate::gleis::gleise) kreuzungen: RStern<Kreuzung>,
}

impl GleiseDaten {
    /// Füge alle Gleise von `neu` zu `self` hinzu.
    fn verschmelze(&mut self, mut neu: GleiseDaten) {
        macro_rules! kombinierte_rstern {
                ($($rstern:ident),* $(,)?) => {$(
                    for geom_with_data in neu.$rstern.drain_with_selection_function(SelectAll) {
                        self.$rstern.insert(geom_with_data);
                    }
                )*};
            }
        kombinierte_rstern! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}

const ÜBERLAPPENDE_VERBINDUNG_GENAUIGKEIT: Skalar = Skalar(5.);

/// SelectionFunction, die jedes Element akzeptiert.
/// Haupt-Nutzen ist das vollständiges Leeren eines RTree (siehe [GleiseDaten::verschmelze]).
struct SelectAll;

impl<T: RTreeObject> SelectionFunction<T> for SelectAll {
    fn should_unpack_parent(&self, _envelope: &T::Envelope) -> bool {
        true
    }
}

/// SelectionFunction, die einen bestimmten Envelope sucht.
pub(in crate::gleis::gleise) struct SelectEnvelope(pub(in crate::gleis::gleise) AABB<Vektor>);

impl<T> SelectionFunction<T> for SelectEnvelope
where
    T: RTreeObject<Envelope = AABB<Vektor>>,
{
    fn should_unpack_parent(&self, envelope: &AABB<Vektor>) -> bool {
        let self_upper = self.0.upper();
        let self_lower = self.0.lower();
        let upper = envelope.upper();
        let lower = envelope.lower();
        // der gesuchte Envelope muss komplett in den parent passen
        lower.x <= self_lower.x
            && lower.y <= self_lower.y
            && upper.x >= self_upper.x
            && upper.y >= self_upper.y
    }

    fn should_unpack_leaf(&self, leaf: &T) -> bool {
        self.0 == leaf.envelope()
    }
}

/// Trait um eine Referenz auf die Map für den jeweiligen Typ zu bekommen.
/// Kein schönes API, daher nur crate-public.
pub(crate) trait DatenAuswahl: Sized {
    fn rstern(gleise: &GleiseDaten) -> &RStern<Self>;
    fn rstern_mut(gleise: &mut GleiseDaten) -> &mut RStern<Self>;
}
impl GleiseDaten {
    #[inline(always)]
    pub(in crate::gleis::gleise) fn rstern<T: DatenAuswahl>(&self) -> &RStern<T> {
        T::rstern(self)
    }
    #[inline(always)]
    pub(in crate::gleis::gleise) fn rstern_mut<T: DatenAuswahl>(&mut self) -> &mut RStern<T> {
        T::rstern_mut(self)
    }
}
impl DatenAuswahl for Gerade {
    fn rstern(GleiseDaten { geraden, .. }: &GleiseDaten) -> &RStern<Self> {
        geraden
    }
    fn rstern_mut(GleiseDaten { geraden, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        geraden
    }
}
impl DatenAuswahl for Kurve {
    fn rstern(GleiseDaten { kurven, .. }: &GleiseDaten) -> &RStern<Self> {
        kurven
    }
    fn rstern_mut(GleiseDaten { kurven, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kurven
    }
}
impl DatenAuswahl for Weiche {
    fn rstern(GleiseDaten { weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        weichen
    }
    fn rstern_mut(GleiseDaten { weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        weichen
    }
}
impl DatenAuswahl for DreiwegeWeiche {
    fn rstern(GleiseDaten { dreiwege_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        dreiwege_weichen
    }
    fn rstern_mut(GleiseDaten { dreiwege_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        dreiwege_weichen
    }
}
impl DatenAuswahl for KurvenWeiche {
    fn rstern(GleiseDaten { kurven_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        kurven_weichen
    }
    fn rstern_mut(GleiseDaten { kurven_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kurven_weichen
    }
}
impl DatenAuswahl for SKurvenWeiche {
    fn rstern(GleiseDaten { s_kurven_weichen, .. }: &GleiseDaten) -> &RStern<Self> {
        s_kurven_weichen
    }
    fn rstern_mut(GleiseDaten { s_kurven_weichen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        s_kurven_weichen
    }
}
impl DatenAuswahl for Kreuzung {
    fn rstern(GleiseDaten { kreuzungen, .. }: &GleiseDaten) -> &RStern<Self> {
        kreuzungen
    }
    fn rstern_mut(GleiseDaten { kreuzungen, .. }: &mut GleiseDaten) -> &mut RStern<Self> {
        kreuzungen
    }
}

// Im Gegensatz zu [DefaultOptions] verwendet [die Standard-Funktion](bincode::deserialize) fixint-encoding.
// https://docs.rs/bincode/latest/bincode/config/index.html#options-struct-vs-bincode-functions
static BINCODE_OPTIONS: Lazy<
    WithOtherTrailing<WithOtherIntEncoding<DefaultOptions, FixintEncoding>, RejectTrailing>,
> = Lazy::new(|| DefaultOptions::new().with_fixint_encoding().reject_trailing_bytes());

/// Fehler der beim [Laden](Gleise::laden) auftreten kann.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum LadenFehler<S> {
    /// Ein IO-Fehler.
    IO(io::Error),
    /// Fehler beim reservieren eines [Anschlusses](anschluss::Anschluss).
    Anschluss(anschluss::Fehler),
    /// Fehler beim Deserialisieren (laden) gespeicherter Daten.
    BincodeDeserialisieren {
        /// Fehler beim Deserialisieren nach aktuellem Speicherformat.
        aktuell: bincode::Error,
        /// Fehler beim Deserialisieren nach Version-2 Speicherformat.
        v2: bincode::Error,
    },
    /// Unbekannte Anschlüsse sollen in einem [Plan](plan::Plan) verwendet werden.
    UnbekannteAnschlüsse {
        /// Der Name des Plans.
        plan: plan::Name,
        /// Die unbekannten Anschlüsse.
        anschlüsse: UnbekannteAnschlüsse<S>,
    },
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

impl GleiseDatenSerialisiert {
    pub(crate) fn neu() -> Self {
        GleiseDatenSerialisiert {
            geraden: Vec::new(),
            kurven: Vec::new(),
            weichen: Vec::new(),
            dreiwege_weichen: Vec::new(),
            kurven_weichen: Vec::new(),
            s_kurven_weichen: Vec::new(),
            kreuzungen: Vec::new(),
        }
    }
}

impl GleiseDaten {
    /// Erzeuge eine Serialisierbare Repräsentation
    fn serialisiere(&self) -> GleiseDatenSerialisiert {
        macro_rules! rstern_to_vecs {
            ($($rstern:ident),* $(,)?) => {
                GleiseDatenSerialisiert {
                    $($rstern: self.$rstern.iter().map(
                        |GeomWithData {data, ..}| {
                            Gleis {
                                position: data.position.clone(),
                                definition: data.definition.serialisiere(),
                            }
                        })
                        .collect()
                    ),*
                }
            };
        }
        rstern_to_vecs! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}

#[allow(single_use_lifetimes)]
fn reserviere_anschlüsse<T, Ts, S, Ss, L>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    serialisiert: Vec<Gleis<Ts>>,
    anschlüsse: Anschlüsse,
    steuerung: impl Fn(&T) -> &Option<S>,
    map: &mut HashMap<Ss, S>,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    arg: &<Ts as Reserviere<T>>::Arg,
) -> (Vec<GeomWithData<Rectangle<Vektor>, Gleis<T>>>, Anschlüsse)
where
    // T: MitSteuerung,
    // <T as MitSteuerung>::SelfUnit: Zeichnen<T>,
    T: Zeichnen<()>,
    Ts: Reserviere<T>,
    <Ts as Reserviere<T>>::Arg: Clone,
    S: Clone + Serialisiere<Ss>,
    Ss: Eq + Hash,
{
    use Ergebnis::*;
    serialisiert.into_iter().fold((Vec::new(), anschlüsse), |acc, gleis_serialisiert| {
        let mut gleise = acc.0;
        let (gleis, anschlüsse) = match gleis_serialisiert.reserviere(lager, acc.1, arg.clone()) {
            Wert { anschluss, anschlüsse } => (anschluss, anschlüsse),
            FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
                (anschluss, anschlüsse)
            },
            Fehler { fehler, anschlüsse } => {
                laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
                return (gleise, anschlüsse);
            },
        };
        // Bekannte Steuerung sichern
        if let Some(steuerung) = steuerung(&gleis.definition) {
            let serialisiert = steuerung.serialisiere();
            let _ = map.insert(serialisiert, steuerung.clone());
        }
        // Gleis mit BoundingBox speichern
        let rectangle =
            Rectangle::from(gleis.definition.rechteck_an_position(&(), spurweite, &gleis.position));
        gleise.push(GeomWithData::new(rectangle, gleis));
        (gleise, anschlüsse)
    })
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere<S, Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        spurweite: Spurweite,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
        kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
        dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
        kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
        fehler: &mut Vec<LadenFehler<S>>,
        sender: &Sender<Nachricht>,
    ) -> (GleiseDaten, Anschlüsse) {
        macro_rules! reserviere_anschlüsse {
            ($($rstern: ident: $ty: ty: $steuerung: ident: $map: ident: $arg: expr),* $(,)?) => {
                $(
                    let ($rstern, anschlüsse) =
                        reserviere_anschlüsse(
                            spurweite,
                            lager,
                            self.$rstern,
                            anschlüsse,
                            |gleis: &$ty| &gleis.$steuerung,
                            $map,
                            fehler,
                            $arg,
                        );
                )*
                (
                    GleiseDaten {
                        $($rstern: RTree::bulk_load($rstern)),*
                    },
                    anschlüsse,
                )
            };
        }
        let aktualisieren_sender = SomeAktualisierenSender::from((sender.clone(), Nachricht::from));
        reserviere_anschlüsse! {
            geraden: Gerade: kontakt: kontakte: &aktualisieren_sender,
            kurven: Kurve: kontakt: kontakte: &aktualisieren_sender,
            weichen: Weiche: steuerung: gerade_weichen: &aktualisieren_sender,
            dreiwege_weichen: DreiwegeWeiche: steuerung: dreiwege_weichen: &aktualisieren_sender,
            kurven_weichen: KurvenWeiche: steuerung: kurven_weichen: &aktualisieren_sender,
            s_kurven_weichen: SKurvenWeiche: steuerung: gerade_weichen: &aktualisieren_sender,
            kreuzungen: Kreuzung: steuerung: gerade_weichen: &aktualisieren_sender,
        }
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

impl<S> From<ZugtypDeserialisierenFehler> for LadenFehler<S> {
    fn from(fehler: ZugtypDeserialisierenFehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

fn reserviere_streckenabschnitt_map<
    S,
    Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    streckenabschnitt_map: StreckenabschnittMapSerialisiert,
    anschlüsse: Anschlüsse,
    streckenabschnitte: &mut HashMap<OutputSerialisiert, Streckenabschnitt>,
    gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
    kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
    dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
    kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
    laden_fehler: &mut Vec<LadenFehler<S>>,
    sender: &Sender<Nachricht>,
) -> (StreckenabschnittMap, Anschlüsse, Option<GleiseDaten>) {
    streckenabschnitt_map.into_iter().fold(
        (HashMap::new(), anschlüsse, None),
        |(mut map, anschlüsse, mut fehler_daten), (name, (streckenabschnitt, daten))| {
            use Ergebnis::*;
            let leiter_serialisiert = streckenabschnitt.anschluss_ref().clone();
            let (streckenabschnitt, fehler, anschlüsse) =
                match streckenabschnitt.reserviere(lager, anschlüsse, ()) {
                    Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
                    FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                        (Some(anschluss), Some(fehler), anschlüsse)
                    },
                    Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
                };

            let (daten, anschlüsse) = daten.reserviere(
                spurweite,
                lager,
                anschlüsse,
                gerade_weichen,
                kurven_weichen,
                dreiwege_weichen,
                kontakte,
                laden_fehler,
                sender,
            );

            if let Some(streckenabschnitt) = streckenabschnitt {
                let _ = streckenabschnitte.insert(leiter_serialisiert, streckenabschnitt.clone());
                let _ = map.insert(name, (streckenabschnitt, daten));
            } else {
                if let Some(fehler_daten) = fehler_daten.as_mut() {
                    fehler_daten.verschmelze(daten)
                } else {
                    fehler_daten = Some(daten);
                }
            }

            if let Some(fehler) = fehler {
                laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
            }

            (map, anschlüsse, fehler_daten)
        },
    )
}

#[allow(single_use_lifetimes)]
fn reserviere_geschwindigkeit_map<L, S, Nachricht>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    geschwindigkeiten_map: GeschwindigkeitMapSerialisiert<S>,
    anschlüsse: Anschlüsse,
    ohne_streckenabschnitt: &mut GleiseDaten,
    geschwindigkeiten: &mut HashMap<GeschwindigkeitSerialisiert<S>, Geschwindigkeit<L>>,
    streckenabschnitte: &mut HashMap<OutputSerialisiert, Streckenabschnitt>,
    gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
    kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
    dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
    kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
    laden_fehler: &mut Vec<LadenFehler<S>>,
    sender: &Sender<Nachricht>,
) -> (GeschwindigkeitMap<L>, Anschlüsse, Option<StreckenabschnittMap>)
where
    L: Serialisiere<S>,
    S: Clone + Eq + Hash + Reserviere<L, Arg = ()>,
    Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
{
    geschwindigkeiten_map.into_iter().fold(
        (HashMap::new(), anschlüsse, None),
        |acc, (name, (geschwindigkeit, streckenabschnitt_map))| {
            use Ergebnis::*;
            let (mut map, anschlüsse, mut fehler_streckenabschnitte) = acc;
            let geschwindigkeit_serialisiert = geschwindigkeit.clone();
            let (geschwindigkeit, fehler, anschlüsse) =
                match geschwindigkeit.reserviere(lager, anschlüsse, ()) {
                    Wert { anschluss, anschlüsse } => (Some(anschluss), None, anschlüsse),
                    FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                        (Some(anschluss), Some(fehler), anschlüsse)
                    },
                    Fehler { fehler, anschlüsse } => (None, Some(fehler), anschlüsse),
                };

            let (streckenabschnitt_map, anschlüsse, fehler_daten) =
                reserviere_streckenabschnitt_map(
                    spurweite,
                    lager,
                    streckenabschnitt_map,
                    anschlüsse,
                    streckenabschnitte,
                    gerade_weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    kontakte,
                    laden_fehler,
                    sender,
                );

            if let Some(fehler_daten) = fehler_daten {
                ohne_streckenabschnitt.verschmelze(fehler_daten);
            }

            if let Some(geschwindigkeit) = geschwindigkeit {
                let _ =
                    geschwindigkeiten.insert(geschwindigkeit_serialisiert, geschwindigkeit.clone());
                let _ = map.insert(name, (geschwindigkeit, streckenabschnitt_map));
            } else {
                if let Some(fehler_streckenabschnitte) = fehler_streckenabschnitte.as_mut() {
                    fehler_streckenabschnitte.extend(streckenabschnitt_map)
                } else {
                    fehler_streckenabschnitte = Some(streckenabschnitt_map)
                }
            };

            if let Some(fehler) = fehler {
                laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
            }

            (map, anschlüsse, fehler_streckenabschnitte)
        },
    )
}

#[allow(single_use_lifetimes)]
impl<L, S> ZustandSerialisiert<L, S>
where
    L: Serialisiere<S> + BekannterLeiter,
    S: Clone + Eq + Hash + Reserviere<L, Arg = ()>,
{
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        sender: &Sender<Nachricht>,
    ) -> Result<(Zustand<L>, Vec<LadenFehler<S>>), ZugtypDeserialisierenFehler> {
        let mut bekannte_geschwindigkeiten = HashMap::new();
        let mut bekannte_streckenabschnitte = HashMap::new();
        let mut bekannte_gerade_weichen = HashMap::new();
        let mut bekannte_kurven_weichen = HashMap::new();
        let mut bekannte_dreiwege_weichen = HashMap::new();
        let mut bekannte_kontakte = HashMap::new();

        let ZustandSerialisiert {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne: pläne_serialisiert,
        } = self;

        // let zugtyp = Zugtyp::try_from(zugtyp)?;
        let zugtyp: Zugtyp<L> = todo!();
        let _ = ();

        let mut fehler = Vec::new();

        let (mut ohne_streckenabschnitt, anschlüsse) = ohne_streckenabschnitt.reserviere(
            zugtyp.spurweite,
            lager,
            anschlüsse,
            &mut bekannte_gerade_weichen,
            &mut bekannte_kurven_weichen,
            &mut bekannte_dreiwege_weichen,
            &mut bekannte_kontakte,
            &mut fehler,
            sender,
        );

        let (mut ohne_geschwindigkeit, anschlüsse, fehler_daten) = reserviere_streckenabschnitt_map(
            zugtyp.spurweite,
            lager,
            ohne_geschwindigkeit,
            anschlüsse,
            &mut bekannte_streckenabschnitte,
            &mut bekannte_gerade_weichen,
            &mut bekannte_kurven_weichen,
            &mut bekannte_dreiwege_weichen,
            &mut bekannte_kontakte,
            &mut fehler,
            sender,
        );
        if let Some(fehler_daten) = fehler_daten {
            ohne_streckenabschnitt.verschmelze(fehler_daten);
        }

        let (geschwindigkeiten, _anschlüsse, fehler_streckenabschnitte) =
            reserviere_geschwindigkeit_map(
                zugtyp.spurweite,
                lager,
                geschwindigkeiten,
                anschlüsse,
                &mut ohne_streckenabschnitt,
                &mut bekannte_geschwindigkeiten,
                &mut bekannte_streckenabschnitte,
                &mut bekannte_gerade_weichen,
                &mut bekannte_kurven_weichen,
                &mut bekannte_dreiwege_weichen,
                &mut bekannte_kontakte,
                &mut fehler,
                sender,
            );
        if let Some(fehler_streckenabschnitte) = fehler_streckenabschnitte {
            ohne_geschwindigkeit.extend(fehler_streckenabschnitte);
        }

        let mut pläne = HashMap::new();
        for (name, plan_serialisiert) in pläne_serialisiert {
            let plan = match plan_serialisiert.deserialisiere(
                &bekannte_geschwindigkeiten,
                &bekannte_streckenabschnitte,
                &bekannte_gerade_weichen,
                &bekannte_kurven_weichen,
                &bekannte_dreiwege_weichen,
                &bekannte_kontakte,
                sender,
            ) {
                Ok(plan) => plan,
                Err(anschlüsse) => {
                    fehler.push(LadenFehler::UnbekannteAnschlüsse { plan: name, anschlüsse });
                    continue;
                },
            };
            let _ = pläne.insert(name, plan);
        }

        Ok((
            Zustand {
                zugtyp,
                ohne_streckenabschnitt,
                ohne_geschwindigkeit,
                geschwindigkeiten,
                pläne,
            },
            fehler,
        ))
    }
}

// FIXME in der selben Datei definiert + use * zum einfacheren Sammeln
use self::zugtyp::*;
pub mod zugtyp {
    use super::*;

    /// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
    #[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
    #[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
    #[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
    pub struct Zugtyp<L: Leiter> {
        /// Der Name des Zugtyps.
        pub name: String,
        /// Die Leiter-Art des Zugtyps.
        pub leiter: PhantomData<fn() -> L>,
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
        pub struct Gerade<Anschluss = Option<Kontakt>> {
            /// Die Länge der Gerade auf dem [Canvas](iced::widget::canvas::Canvas).
            pub länge: Skalar,
            /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Der Anschluss für einen [Kontakt] an der Schiene.
            pub kontakt: Anschluss,
        }

        #[doc = r" Eine Variante ohne Anschlüsse."]
        pub type GeradeSerialisiert = Gerade<Option<KontaktSerialisiert>>;
        #[doc = r" Eine serialisierbare Repräsentation."]
        pub type GeradeUnit = Gerade<()>;
        impl crate::anschluss::de_serialisieren::Serialisiere<GeradeSerialisiert> for Gerade {
            fn serialisiere(&self) -> GeradeSerialisiert {
                let Gerade { länge, beschreibung, kontakt } = self;
                GeradeSerialisiert {
                    länge: länge.clone(),
                    beschreibung: beschreibung.clone(),
                    kontakt: kontakt.as_ref().map(|steuerung| steuerung.serialisiere()),
                }
            }
            fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                if let Some(steuerung) = self.kontakt {
                    anschlüsse.anhängen(steuerung.anschlüsse());
                }
                anschlüsse
            }
        }
        impl crate::anschluss::de_serialisieren::Reserviere<Gerade> for GeradeSerialisiert {
            #[allow(unused_qualifications)]
            type Arg =
                <Option<KontaktSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                    Option<Kontakt>,
                >>::Arg;
            fn reserviere(
                self,
                lager: &mut crate::anschluss::Lager,
                anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                arg: Self::Arg,
            ) -> crate::anschluss::de_serialisieren::Ergebnis<Gerade> {
                let Gerade { länge, beschreibung, kontakt } = self;
                (kontakt).reserviere(lager, anschlüsse, arg).konvertiere(|(kontakt)| Gerade {
                    länge,
                    beschreibung,
                    kontakt,
                })
            }
        }
    }

    use self::kreuzung::*;
    pub mod kreuzung {
        use super::*;

        use super::weiche::gerade::{
            Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert
        };

        /// Definition einer Kreuzung.
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub struct Kreuzung<Anschlüsse = Option<self::Anschlüsse>> {
            /// Die Länge der Geraden.
            pub länge: Skalar,
            /// Der Kurvenradius; legt automatisch den Winkel fest.
            pub radius: Skalar,
            /// Werden die Kurven gezeichnet, oder nur die Geraden.
            pub variante: Variante,
            /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Die Anschlüsse zum Schalten der Kreuzung.
            pub steuerung: Anschlüsse,
        }

        /// Werden die Kurven gezeichnet, oder nur die Geraden.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        pub enum Variante {
            /// Zeichne die Kurven und die Geraden.
            MitKurve,
            /// Zeichne nur die Geraden.
            OhneKurve,
        }

        type Anschlüsse = super::steuerung::Weiche<Richtung, RichtungAnschlüsse>;
        type AnschlüsseSerialisiert =
            super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

        #[doc = r" Eine Variante ohne Anschlüsse."]
        pub type KreuzungSerialisiert = Kreuzung<Option<AnschlüsseSerialisiert>>;
        #[doc = r" Eine serialisierbare Repräsentation."]
        pub type KreuzungUnit = Kreuzung<()>;
        impl crate::anschluss::de_serialisieren::Serialisiere<KreuzungSerialisiert> for Kreuzung {
            fn serialisiere(&self) -> KreuzungSerialisiert {
                let Kreuzung { länge, radius, variante, beschreibung, steuerung } = self;
                KreuzungSerialisiert {
                    länge: länge.clone(),
                    radius: radius.clone(),
                    variante: variante.clone(),
                    beschreibung: beschreibung.clone(),
                    steuerung: steuerung
                        .as_ref()
                        .map(|steuerung| todo!("steuerung.serialisiere()")),
                }
            }
            fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                if let Some(steuerung) = self.steuerung {
                    anschlüsse.anhängen(todo!("steuerung.anschlüsse()"));
                }
                anschlüsse
            }
        }
        impl crate::anschluss::de_serialisieren::Reserviere<Kreuzung> for KreuzungSerialisiert {
            #[allow(unused_qualifications)]
            type Arg =
            <Option<AnschlüsseSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                Option<self::Anschlüsse>,
            >>::Arg;
            fn reserviere(
                self,
                lager: &mut crate::anschluss::Lager,
                anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                arg: Self::Arg,
            ) -> crate::anschluss::de_serialisieren::Ergebnis<Kreuzung> {
                let Kreuzung { länge, radius, variante, beschreibung, steuerung } = self;
                (steuerung).reserviere(lager, anschlüsse, arg).konvertiere(|(steuerung)| Kreuzung {
                    länge,
                    radius,
                    variante,
                    beschreibung,
                    steuerung,
                })
            }
        }
    }

    use self::kurve::*;
    pub mod kurve {
        use super::*;

        /// Definition einer Kurve.
        ///
        /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
        #[derive(Clone, Debug, Serialize, Deserialize)]
        pub struct Kurve<Anschluss = Option<Kontakt>> {
            /// Der Radius auf dem Canvas.
            pub radius: Skalar,
            /// Der Winkel der Kurve.
            pub winkel: Winkel,
            /// Eine allgemeine Beschreibung der Kurve, z.B. die Produktnummer.
            pub beschreibung: Option<String>,
            /// Der Anschluss für einen [Kontakt] an der Schiene.
            pub kontakt: Anschluss,
        }

        #[doc = r" Eine Variante ohne Anschlüsse."]
        pub type KurveSerialisiert = Kurve<Option<KontaktSerialisiert>>;
        #[doc = r" Eine serialisierbare Repräsentation."]
        pub type KurveUnit = Kurve<()>;
        impl crate::anschluss::de_serialisieren::Serialisiere<KurveSerialisiert> for Kurve {
            fn serialisiere(&self) -> KurveSerialisiert {
                let Kurve { radius, winkel, beschreibung, kontakt } = self;
                KurveSerialisiert {
                    radius: radius.clone(),
                    winkel: winkel.clone(),
                    beschreibung: beschreibung.clone(),
                    kontakt: kontakt.as_ref().map(|steuerung| steuerung.serialisiere()),
                }
            }
            fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                if let Some(steuerung) = self.kontakt {
                    anschlüsse.anhängen(steuerung.anschlüsse());
                }
                anschlüsse
            }
        }
        impl crate::anschluss::de_serialisieren::Reserviere<Kurve> for KurveSerialisiert {
            #[allow(unused_qualifications)]
            type Arg =
                <Option<KontaktSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                    Option<Kontakt>,
                >>::Arg;
            fn reserviere(
                self,
                lager: &mut crate::anschluss::Lager,
                anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                arg: Self::Arg,
            ) -> crate::anschluss::de_serialisieren::Ergebnis<Kurve> {
                let Kurve { radius, winkel, beschreibung, kontakt } = self;
                (kontakt).reserviere(lager, anschlüsse, arg).konvertiere(|(kontakt)| Kurve {
                    radius,
                    winkel,
                    beschreibung,
                    kontakt,
                })
            }
        }
    }

    use self::weiche::*;
    pub mod weiche {
        use super::*;

        use self::dreiwege::*;
        pub mod dreiwege {
            use super::*;

            type Anschlüsse = super::steuerung::Weiche<RichtungInformation, RichtungAnschlüsse>;
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

            impl Default for RichtungInformation {
                fn default() -> Self {
                    RichtungInformation {
                        aktuelle_richtung: Richtung::Gerade,
                        letzte_richtung: Richtung::Rechts,
                    }
                }
            }

            /// Definition einer Dreiwege-Weiche.
            ///
            /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct DreiwegeWeiche<Anschlüsse = Option<self::Anschlüsse>> {
                /// Die Länge der Gerade.
                pub länge: Skalar,
                /// Der Radius der Kurven.
                pub radius: Skalar,
                /// Der Winkel der Kurven.
                pub winkel: Winkel,
                /// Eine allgemeine Beschreibung der DreiwegeWeiche, z.B. die Produktnummer.
                pub beschreibung: Option<String>,
                /// Die Anschlüsse zum Schalten der DreiwegeWeiche.
                pub steuerung: Anschlüsse,
            }

            #[doc = r" Eine Variante ohne Anschlüsse."]
            pub type DreiwegeWeicheSerialisiert = DreiwegeWeiche<Option<AnschlüsseSerialisiert>>;
            #[doc = r" Eine serialisierbare Repräsentation."]
            pub type DreiwegeWeicheUnit = DreiwegeWeiche<()>;
            impl crate::anschluss::de_serialisieren::Serialisiere<DreiwegeWeicheSerialisiert>
                for DreiwegeWeiche
            {
                fn serialisiere(&self) -> DreiwegeWeicheSerialisiert {
                    let DreiwegeWeiche { länge, radius, winkel, beschreibung, steuerung } = self;
                    DreiwegeWeicheSerialisiert {
                        länge: länge.clone(),
                        radius: radius.clone(),
                        winkel: winkel.clone(),
                        beschreibung: beschreibung.clone(),
                        steuerung: steuerung
                            .as_ref()
                            .map(|steuerung| todo!("steuerung.serialisiere()")),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    if let Some(steuerung) = self.steuerung {
                        anschlüsse.anhängen(todo!("steuerung.anschlüsse()"));
                    }
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<DreiwegeWeiche> for DreiwegeWeicheSerialisiert {
                #[allow(unused_qualifications)]
                type Arg =
            <Option<AnschlüsseSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                Option<self::Anschlüsse>,
            >>::Arg;
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    arg: Self::Arg,
                ) -> crate::anschluss::de_serialisieren::Ergebnis<DreiwegeWeiche> {
                    let DreiwegeWeiche { länge, radius, winkel, beschreibung, steuerung } = self;
                    (steuerung).reserviere(lager, anschlüsse, arg).konvertiere(|(steuerung)| {
                        DreiwegeWeiche { länge, radius, winkel, beschreibung, steuerung }
                    })
                }
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
            impl
                crate::util::nachschlagen::Nachschlagen<
                    Richtung,
                    crate::anschluss::OutputSerialisiert,
                > for RichtungAnschlüsseSerialisiert
            {
                fn erhalte(&self, key: &Richtung) -> &crate::anschluss::OutputSerialisiert {
                    match key {
                        Richtung::Gerade => &self.gerade,
                        Richtung::Links => &self.links,
                        Richtung::Rechts => &self.rechts,
                    }
                }
                fn erhalte_mut(
                    &mut self,
                    key: &Richtung,
                ) -> &mut crate::anschluss::OutputSerialisiert {
                    match key {
                        Richtung::Gerade => &mut self.gerade,
                        Richtung::Links => &mut self.links,
                        Richtung::Rechts => &mut self.rechts,
                    }
                }
                fn für_alle<F: FnMut(Richtung, &crate::anschluss::OutputSerialisiert)>(
                    &self,
                    mut action: F,
                ) {
                    action(Richtung::Gerade, &self.gerade);
                    action(Richtung::Links, &self.links);
                    action(Richtung::Rechts, &self.rechts)
                }
                fn zuordnen<
                    F: Fn(
                        &crate::anschluss::OutputSerialisiert,
                    ) -> crate::anschluss::OutputSerialisiert,
                >(
                    &self,
                    mut action: F,
                ) -> Self {
                    RichtungAnschlüsseSerialisiert {
                        gerade: action(&self.gerade),
                        links: action(&self.links),
                        rechts: action(&self.rechts),
                    }
                }
                fn referenzen<'t>(
                    &'t self,
                ) -> Vec<(Richtung, &'t crate::anschluss::OutputSerialisiert)> {
                    let RichtungAnschlüsseSerialisiert { gerade, links, rechts } = self;
                    vec![
                        (Richtung::Gerade, gerade),
                        (Richtung::Links, links),
                        (Richtung::Rechts, rechts),
                    ]
                }
                fn referenzen_mut<'t>(
                    &'t mut self,
                ) -> Vec<(Richtung, &'t mut crate::anschluss::OutputSerialisiert)> {
                    let RichtungAnschlüsseSerialisiert { gerade, links, rechts } = self;
                    vec![
                        (Richtung::Gerade, gerade),
                        (Richtung::Links, links),
                        (Richtung::Rechts, rechts),
                    ]
                }
            }
            #[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
            #[derive(Debug)]
            pub struct RichtungAnschlüsse {
                #[doc = "[Richtung::Gerade]"]
                pub gerade: crate::anschluss::OutputAnschluss,
                #[doc = "[Richtung::Links]"]
                pub links: crate::anschluss::OutputAnschluss,
                #[doc = "[Richtung::Rechts]"]
                pub rechts: crate::anschluss::OutputAnschluss,
            }
            impl
                crate::util::nachschlagen::Nachschlagen<Richtung, crate::anschluss::OutputAnschluss>
                for RichtungAnschlüsse
            {
                fn erhalte(&self, key: &Richtung) -> &crate::anschluss::OutputAnschluss {
                    match key {
                        Richtung::Gerade => &self.gerade,
                        Richtung::Links => &self.links,
                        Richtung::Rechts => &self.rechts,
                    }
                }
                fn erhalte_mut(
                    &mut self,
                    key: &Richtung,
                ) -> &mut crate::anschluss::OutputAnschluss {
                    match key {
                        Richtung::Gerade => &mut self.gerade,
                        Richtung::Links => &mut self.links,
                        Richtung::Rechts => &mut self.rechts,
                    }
                }
                fn für_alle<F: FnMut(Richtung, &crate::anschluss::OutputAnschluss)>(
                    &self,
                    mut action: F,
                ) {
                    action(Richtung::Gerade, &self.gerade);
                    action(Richtung::Links, &self.links);
                    action(Richtung::Rechts, &self.rechts)
                }
                fn zuordnen<
                    F: Fn(&crate::anschluss::OutputAnschluss) -> crate::anschluss::OutputAnschluss,
                >(
                    &self,
                    mut action: F,
                ) -> Self {
                    RichtungAnschlüsse {
                        gerade: action(&self.gerade),
                        links: action(&self.links),
                        rechts: action(&self.rechts),
                    }
                }
                fn referenzen<'t>(
                    &'t self,
                ) -> Vec<(Richtung, &'t crate::anschluss::OutputAnschluss)> {
                    let RichtungAnschlüsse { gerade, links, rechts } = self;
                    vec![
                        (Richtung::Gerade, gerade),
                        (Richtung::Links, links),
                        (Richtung::Rechts, rechts),
                    ]
                }
                fn referenzen_mut<'t>(
                    &'t mut self,
                ) -> Vec<(Richtung, &'t mut crate::anschluss::OutputAnschluss)> {
                    let RichtungAnschlüsse { gerade, links, rechts } = self;
                    vec![
                        (Richtung::Gerade, gerade),
                        (Richtung::Links, links),
                        (Richtung::Rechts, rechts),
                    ]
                }
            }
            impl Default for Richtung {
                fn default() -> Self {
                    Richtung::Gerade
                }
            }
            impl std::fmt::Display for Richtung {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(
                        f,
                        "{}",
                        match self {
                            Richtung::Gerade => "Gerade",
                            Richtung::Links => "Links",
                            Richtung::Rechts => "Rechts",
                        }
                    )
                }
            }
            #[allow(unused_qualifications)]
            impl crate::steuerung::weiche::MitRichtung<Richtung> for Richtung {
                fn aktuelle_richtung(&self) -> Option<Richtung> {
                    Some(*self)
                }
            }
            impl crate::anschluss::de_serialisieren::Serialisiere<RichtungAnschlüsseSerialisiert>
                for RichtungAnschlüsse
            {
                fn serialisiere(&self) -> RichtungAnschlüsseSerialisiert {
                    let RichtungAnschlüsse { gerade, links, rechts } = self;
                    RichtungAnschlüsseSerialisiert {
                        gerade: gerade.serialisiere(),
                        links: links.serialisiere(),
                        rechts: rechts.serialisiere(),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    anschlüsse.anhängen(self.gerade.anschlüsse());
                    anschlüsse.anhängen(self.links.anschlüsse());
                    anschlüsse.anhängen(self.rechts.anschlüsse());
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<RichtungAnschlüsse>
                for RichtungAnschlüsseSerialisiert
            {
                type Arg = ();
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    _arg: (),
                ) -> crate::anschluss::de_serialisieren::Ergebnis<RichtungAnschlüsse>
                {
                    let RichtungAnschlüsseSerialisiert { gerade, links, rechts } = self;
                    #[allow(unused_parens)]
                    (gerade, links, rechts).reserviere(lager, anschlüsse, ((), (), ())).konvertiere(
                        |(gerade, links, rechts)| RichtungAnschlüsse { gerade, links, rechts },
                    )
                }
            }
            impl Default for RichtungAnschlüsseSerialisiert {
                fn default() -> Self {
                    RichtungAnschlüsseSerialisiert {
                        gerade: crate::anschluss::OutputSerialisiert::Pin {
                            pin: 0,
                            polarität: crate::anschluss::polarität::Polarität::Normal,
                        },
                        links: crate::anschluss::OutputSerialisiert::Pin {
                            pin: 0,
                            polarität: crate::anschluss::polarität::Polarität::Normal,
                        },
                        rechts: crate::anschluss::OutputSerialisiert::Pin {
                            pin: 0,
                            polarität: crate::anschluss::polarität::Polarität::Normal,
                        },
                    }
                }
            }
        }

        use self::gerade::*;
        pub mod gerade {
            use super::*;

            use super::orientierung::Orientierung;

            type AnschlüsseSerialisiert =
                super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;
            type Anschlüsse = super::steuerung::Weiche<Richtung, RichtungAnschlüsse>;

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
            impl crate::anschluss::de_serialisieren::Serialisiere<WeicheSerialisiert> for Weiche {
                fn serialisiere(&self) -> WeicheSerialisiert {
                    let Weiche { länge, radius, winkel, orientierung, beschreibung, steuerung } =
                        self;
                    WeicheSerialisiert {
                        länge: länge.clone(),
                        radius: radius.clone(),
                        winkel: winkel.clone(),
                        orientierung: orientierung.clone(),
                        beschreibung: beschreibung.clone(),
                        steuerung: steuerung
                            .as_ref()
                            .map(|steuerung| todo!("steuerung.serialisiere()")),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    if let Some(steuerung) = self.steuerung {
                        anschlüsse.anhängen(todo!("steuerung.anschlüsse()"));
                    }
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<Weiche> for WeicheSerialisiert {
                #[allow(unused_qualifications)]
                type Arg =
            <Option<AnschlüsseSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                Option<self::Anschlüsse>,
            >>::Arg;
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    arg: Self::Arg,
                ) -> crate::anschluss::de_serialisieren::Ergebnis<Weiche> {
                    let Weiche { länge, radius, winkel, orientierung, beschreibung, steuerung } =
                        self;
                    (steuerung).reserviere(lager, anschlüsse, arg).konvertiere(|(steuerung)| {
                        Weiche { länge, radius, winkel, orientierung, beschreibung, steuerung }
                    })
                }
            }

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
            impl
                crate::util::nachschlagen::Nachschlagen<
                    Richtung,
                    crate::anschluss::OutputSerialisiert,
                > for RichtungAnschlüsseSerialisiert
            {
                fn erhalte(&self, key: &Richtung) -> &crate::anschluss::OutputSerialisiert {
                    match key {
                        Richtung::Gerade => &self.gerade,
                        Richtung::Kurve => &self.kurve,
                    }
                }
                fn erhalte_mut(
                    &mut self,
                    key: &Richtung,
                ) -> &mut crate::anschluss::OutputSerialisiert {
                    match key {
                        Richtung::Gerade => &mut self.gerade,
                        Richtung::Kurve => &mut self.kurve,
                    }
                }
                fn für_alle<F: FnMut(Richtung, &crate::anschluss::OutputSerialisiert)>(
                    &self,
                    mut action: F,
                ) {
                    action(Richtung::Gerade, &self.gerade);
                    action(Richtung::Kurve, &self.kurve)
                }
                fn zuordnen<
                    F: Fn(
                        &crate::anschluss::OutputSerialisiert,
                    ) -> crate::anschluss::OutputSerialisiert,
                >(
                    &self,
                    mut action: F,
                ) -> Self {
                    RichtungAnschlüsseSerialisiert {
                        gerade: action(&self.gerade),
                        kurve: action(&self.kurve),
                    }
                }
                fn referenzen<'t>(
                    &'t self,
                ) -> Vec<(Richtung, &'t crate::anschluss::OutputSerialisiert)> {
                    let RichtungAnschlüsseSerialisiert { gerade, kurve } = self;
                    vec![(Richtung::Gerade, gerade), (Richtung::Kurve, kurve)]
                }
                fn referenzen_mut<'t>(
                    &'t mut self,
                ) -> Vec<(Richtung, &'t mut crate::anschluss::OutputSerialisiert)> {
                    let RichtungAnschlüsseSerialisiert { gerade, kurve } = self;
                    vec![(Richtung::Gerade, gerade), (Richtung::Kurve, kurve)]
                }
            }
            #[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
            #[derive(Debug)]
            pub struct RichtungAnschlüsse {
                #[doc = "[Richtung::Gerade]"]
                pub gerade: crate::anschluss::OutputAnschluss,
                #[doc = "[Richtung::Kurve]"]
                pub kurve: crate::anschluss::OutputAnschluss,
            }
            impl
                crate::util::nachschlagen::Nachschlagen<Richtung, crate::anschluss::OutputAnschluss>
                for RichtungAnschlüsse
            {
                fn erhalte(&self, key: &Richtung) -> &crate::anschluss::OutputAnschluss {
                    match key {
                        Richtung::Gerade => &self.gerade,
                        Richtung::Kurve => &self.kurve,
                    }
                }
                fn erhalte_mut(
                    &mut self,
                    key: &Richtung,
                ) -> &mut crate::anschluss::OutputAnschluss {
                    match key {
                        Richtung::Gerade => &mut self.gerade,
                        Richtung::Kurve => &mut self.kurve,
                    }
                }
                fn für_alle<F: FnMut(Richtung, &crate::anschluss::OutputAnschluss)>(
                    &self,
                    mut action: F,
                ) {
                    action(Richtung::Gerade, &self.gerade);
                    action(Richtung::Kurve, &self.kurve)
                }
                fn zuordnen<
                    F: Fn(&crate::anschluss::OutputAnschluss) -> crate::anschluss::OutputAnschluss,
                >(
                    &self,
                    mut action: F,
                ) -> Self {
                    RichtungAnschlüsse { gerade: action(&self.gerade), kurve: action(&self.kurve) }
                }
                fn referenzen<'t>(
                    &'t self,
                ) -> Vec<(Richtung, &'t crate::anschluss::OutputAnschluss)> {
                    let RichtungAnschlüsse { gerade, kurve } = self;
                    vec![(Richtung::Gerade, gerade), (Richtung::Kurve, kurve)]
                }
                fn referenzen_mut<'t>(
                    &'t mut self,
                ) -> Vec<(Richtung, &'t mut crate::anschluss::OutputAnschluss)> {
                    let RichtungAnschlüsse { gerade, kurve } = self;
                    vec![(Richtung::Gerade, gerade), (Richtung::Kurve, kurve)]
                }
            }
            impl Default for Richtung {
                fn default() -> Self {
                    Richtung::Gerade
                }
            }
            impl std::fmt::Display for Richtung {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(
                        f,
                        "{}",
                        match self {
                            Richtung::Gerade => "Gerade",
                            Richtung::Kurve => "Kurve",
                        }
                    )
                }
            }
            #[allow(unused_qualifications)]
            impl crate::steuerung::weiche::MitRichtung<Richtung> for Richtung {
                fn aktuelle_richtung(&self) -> Option<Richtung> {
                    Some(*self)
                }
            }
            impl crate::anschluss::de_serialisieren::Serialisiere<RichtungAnschlüsseSerialisiert>
                for RichtungAnschlüsse
            {
                fn serialisiere(&self) -> RichtungAnschlüsseSerialisiert {
                    let RichtungAnschlüsse { gerade, kurve } = self;
                    RichtungAnschlüsseSerialisiert {
                        gerade: gerade.serialisiere(),
                        kurve: kurve.serialisiere(),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    anschlüsse.anhängen(self.gerade.anschlüsse());
                    anschlüsse.anhängen(self.kurve.anschlüsse());
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<RichtungAnschlüsse>
                for RichtungAnschlüsseSerialisiert
            {
                type Arg = ();
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    _arg: (),
                ) -> crate::anschluss::de_serialisieren::Ergebnis<RichtungAnschlüsse>
                {
                    let RichtungAnschlüsseSerialisiert { gerade, kurve } = self;
                    #[allow(unused_parens)]
                    (gerade, kurve)
                        .reserviere(lager, anschlüsse, ((), ()))
                        .konvertiere(|(gerade, kurve)| RichtungAnschlüsse { gerade, kurve })
                }
            }
            impl Default for RichtungAnschlüsseSerialisiert {
                fn default() -> Self {
                    RichtungAnschlüsseSerialisiert {
                        gerade: crate::anschluss::OutputSerialisiert::Pin {
                            pin: 0,
                            polarität: crate::anschluss::polarität::Polarität::Normal,
                        },
                        kurve: crate::anschluss::OutputSerialisiert::Pin {
                            pin: 0,
                            polarität: crate::anschluss::polarität::Polarität::Normal,
                        },
                    }
                }
            }
        }

        use self::kurve::*;
        pub mod kurve {
            use super::*;

            use super::orientierung::Orientierung;

            type AnschlüsseSerialisiert =
                super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;
            type Anschlüsse = super::steuerung::Weiche<Richtung, RichtungAnschlüsse>;

            /// Definition einer Kurven-Weiche.
            ///
            /// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct KurvenWeiche<Anschlüsse = Option<self::Anschlüsse>> {
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
                pub steuerung: Anschlüsse,
            }

            #[doc = r" Eine Variante ohne Anschlüsse."]
            pub type KurvenWeicheSerialisiert = KurvenWeiche<Option<AnschlüsseSerialisiert>>;
            #[doc = r" Eine serialisierbare Repräsentation."]
            pub type KurvenWeicheUnit = KurvenWeiche<()>;
            impl crate::anschluss::de_serialisieren::Serialisiere<KurvenWeicheSerialisiert> for KurvenWeiche {
                fn serialisiere(&self) -> KurvenWeicheSerialisiert {
                    let KurvenWeiche {
                        länge,
                        radius,
                        winkel,
                        orientierung,
                        beschreibung,
                        steuerung,
                    } = self;
                    KurvenWeicheSerialisiert {
                        länge: länge.clone(),
                        radius: radius.clone(),
                        winkel: winkel.clone(),
                        orientierung: orientierung.clone(),
                        beschreibung: beschreibung.clone(),
                        steuerung: steuerung
                            .as_ref()
                            .map(|steuerung| todo!("steuerung.serialisiere()")),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    if let Some(steuerung) = self.steuerung {
                        anschlüsse.anhängen(todo!("steuerung.anschlüsse()"));
                    }
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<KurvenWeiche> for KurvenWeicheSerialisiert {
                #[allow(unused_qualifications)]
                type Arg =
                    <Option<AnschlüsseSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                        Option<self::Anschlüsse>,
                    >>::Arg;
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    arg: Self::Arg,
                ) -> crate::anschluss::de_serialisieren::Ergebnis<KurvenWeiche> {
                    let KurvenWeiche {
                        länge,
                        radius,
                        winkel,
                        orientierung,
                        beschreibung,
                        steuerung,
                    } = self;
                    (steuerung).reserviere(lager, anschlüsse, arg).konvertiere(|(steuerung)| {
                        KurvenWeiche {
                            länge,
                            radius,
                            winkel,
                            orientierung,
                            beschreibung,
                            steuerung,
                        }
                    })
                }
            }
        }

        use self::s_kurve::*;
        pub mod s_kurve {
            use super::*;

            type AnschlüsseSerialisiert =
                super::steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;
            type Anschlüsse = super::steuerung::Weiche<Richtung, RichtungAnschlüsse>;

            /// Definition einer Weiche mit S-Kurve.
            ///
            /// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
            #[derive(Clone, Debug, Serialize, Deserialize)]
            pub struct SKurvenWeiche<Anschlüsse = Option<self::Anschlüsse>> {
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
                pub steuerung: Anschlüsse,
            }

            #[doc = r" Eine Variante ohne Anschlüsse."]
            pub type SKurvenWeicheSerialisiert = SKurvenWeiche<Option<AnschlüsseSerialisiert>>;
            #[doc = r" Eine serialisierbare Repräsentation."]
            pub type SKurvenWeicheUnit = SKurvenWeiche<()>;
            impl crate::anschluss::de_serialisieren::Serialisiere<SKurvenWeicheSerialisiert> for SKurvenWeiche {
                fn serialisiere(&self) -> SKurvenWeicheSerialisiert {
                    let SKurvenWeiche {
                        länge,
                        radius,
                        winkel,
                        radius_kurve_nach_innen,
                        winkel_kurve_nach_innen,
                        orientierung,
                        beschreibung,
                        steuerung,
                    } = self;
                    SKurvenWeicheSerialisiert {
                        länge: länge.clone(),
                        radius: radius.clone(),
                        winkel: winkel.clone(),
                        radius_kurve_nach_innen: radius_kurve_nach_innen.clone(),
                        winkel_kurve_nach_innen: winkel_kurve_nach_innen.clone(),
                        orientierung: orientierung.clone(),
                        beschreibung: beschreibung.clone(),
                        steuerung: steuerung
                            .as_ref()
                            .map(|steuerung| todo!("steuerung.serialisiere()")),
                    }
                }
                fn anschlüsse(self) -> crate::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = crate::anschluss::de_serialisieren::Anschlüsse::default();
                    if let Some(steuerung) = self.steuerung {
                        anschlüsse.anhängen(todo!("(steuerung.anschlüsse()"));
                    }
                    anschlüsse
                }
            }
            impl crate::anschluss::de_serialisieren::Reserviere<SKurvenWeiche> for SKurvenWeicheSerialisiert {
                #[allow(unused_qualifications)]
                type Arg =
                    <Option<AnschlüsseSerialisiert> as crate::anschluss::de_serialisieren::Reserviere<
                        Option<self::Anschlüsse>,
                    >>::Arg;
                fn reserviere(
                    self,
                    lager: &mut crate::anschluss::Lager,
                    anschlüsse: crate::anschluss::de_serialisieren::Anschlüsse,
                    arg: Self::Arg,
                ) -> crate::anschluss::de_serialisieren::Ergebnis<SKurvenWeiche> {
                    let SKurvenWeiche {
                        länge,
                        radius,
                        winkel,
                        radius_kurve_nach_innen,
                        winkel_kurve_nach_innen,
                        orientierung,
                        beschreibung,
                        steuerung,
                    } = self;
                    (steuerung).reserviere(lager, anschlüsse, arg).konvertiere(|(steuerung)| {
                        SKurvenWeiche {
                            länge,
                            radius,
                            winkel,
                            radius_kurve_nach_innen,
                            winkel_kurve_nach_innen,
                            orientierung,
                            beschreibung,
                            steuerung,
                        }
                    })
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

        /// Name einer [Weiche].
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        pub struct Name(pub String);

        // FIXME remove
        // inklusive Kreuzung
        /// [Name], aktuelle Richtung und Anschlüsse einer Weiche.
        #[derive(Debug, zugkontrolle_macros::Clone)]
        pub struct Weiche<Richtung, Anschlüsse> {
            /// Der Name der Weiche.
            pub name: Name,
            /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
            richtung: Arc<Mutex<Steuerung<Richtung>>>,
            /// Die Anschlüsse der Weiche.
            anschlüsse: Arc<Mutex<Anschlüsse>>,
        }

        impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
            /// Erstelle eine neue [Weichen-Steuerung](Weiche).
            pub fn neu(
                name: Name,
                richtung: Richtung,
                anschlüsse: Anschlüsse,
                sender: impl Into<SomeAktualisierenSender>,
            ) -> Self {
                Weiche {
                    name,
                    richtung: Arc::new(Mutex::new(Steuerung::neu(richtung, sender))),
                    anschlüsse: Arc::new(Mutex::new(anschlüsse)),
                }
            }
        }

        /// Steuerung eines Gleises.
        /// Bei [AsMut]-Zugriff wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
        #[derive(Clone)]
        pub struct Steuerung<T> {
            steuerung: T,
            sender: SomeAktualisierenSender,
        }

        impl<T> Steuerung<T> {
            /// Erstelle eine neue [Steuerung].
            pub fn neu(steuerung: T, sender: impl Into<SomeAktualisierenSender>) -> Self {
                Steuerung { steuerung, sender: sender.into() }
            }
        }

        // Explizite Implementierung, um einen stack-overflow zu vermeiden.
        impl<T: Debug> Debug for Steuerung<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Steuerung")
                    .field("steuerung", &self.steuerung)
                    .field("canvas", &"<Cache>")
                    .finish()
            }
        }

        #[allow(single_use_lifetimes)]
        impl<Richtung, R, S> Reserviere<Weiche<Richtung, R>> for WeicheSerialisiert<Richtung, S>
        where
            S: Reserviere<R, Arg = ()>,
        {
            type Arg = SomeAktualisierenSender;

            fn reserviere(
                self,
                lager: &mut anschluss::Lager,
                bekannte_anschlüsse: Anschlüsse,
                sender: SomeAktualisierenSender,
            ) -> crate::anschluss::de_serialisieren::Ergebnis<Weiche<Richtung, R>> {
                let WeicheSerialisiert { name, richtung, anschlüsse } = self;
                anschlüsse
                    .reserviere(lager, bekannte_anschlüsse, ())
                    .konvertiere(|anschlüsse| Weiche::neu(name, richtung, anschlüsse, sender))
            }
        }
    }
}
