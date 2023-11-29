//! Serialisierte Strukturen von Version 3.X, die mit Version 4.0.0 geändert wurden.

use std::{
    collections::HashMap, fmt::Debug, hash::Hash, io, marker::PhantomData, sync::mpsc::Sender,
    time::Duration,
};

use bincode::config::{
    DefaultOptions, FixintEncoding, Options, RejectTrailing, WithOtherIntEncoding,
    WithOtherTrailing,
};
use once_cell::sync::Lazy;
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
        Zeichnen,
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
