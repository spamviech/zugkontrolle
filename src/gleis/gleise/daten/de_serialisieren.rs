//! speichern und laden Methode für [Gleise].

use std::{
    collections::HashMap,
    fmt::Debug,
    fs,
    hash::Hash,
    io::{self, Read},
    sync::Arc,
};

use nonempty::NonEmpty;
use parking_lot::Mutex;
use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        OutputSerialisiert,
    },
    gleis::{
        gerade::{Gerade, GeradeSerialisiert},
        gleise::{
            daten::{
                v2, DatenAuswahl, GeschwindigkeitMap, Gleis, GleiseDaten, SelectAll,
                StreckenabschnittMap, Zustand,
            },
            Fehler, Gleise,
        },
        kreuzung::{Kreuzung, KreuzungSerialisiert},
        kurve::{Kurve, KurveSerialisiert},
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheSerialisiert},
            gerade::{Weiche, WeicheSerialisiert},
            kurve::{KurvenWeiche, KurvenWeicheSerialisiert},
            s_kurve::{SKurvenWeiche, SKurvenWeicheSerialisiert},
        },
    },
    steuerung::{
        geschwindigkeit::{
            self, BekannterLeiter, Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter,
        },
        kontakt::{Kontakt, KontaktSerialisiert},
        plan::{self, PlanSerialisiert, UnbekannteAnschlüsse},
        streckenabschnitt::{self, Streckenabschnitt, StreckenabschnittSerialisiert},
    },
    typen::{canvas::Cache, mm::Spurweite, vektor::Vektor, Zeichnen},
    zugtyp::{FalscherLeiter, Zugtyp, ZugtypSerialisiert},
};

/// Fehler der beim [Laden](Gleise::laden) auftreten kann.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::From)]
#[zugkontrolle_debug(<L as Serialisiere>::Serialisiert: Debug)]
pub enum LadenFehler<L: Serialisiere> {
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
        anschlüsse: UnbekannteAnschlüsse<L>,
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

fn reserviere_anschlüsse<T, S, L>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    serialisiert: Vec<Gleis<<T as Serialisiere>::Serialisiert>>,
    anschlüsse: Anschlüsse,
    steuerung: impl Fn(&T) -> &Option<S>,
    map: &mut HashMap<S::Serialisiert, S>,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    arg: &<<T as Serialisiere>::Serialisiert as Reserviere<T>>::Arg,
) -> (Vec<GeomWithData<Rectangle<Vektor>, Gleis<T>>>, Anschlüsse)
where
    T: Zeichnen + Serialisiere,
    <<T as Serialisiere>::Serialisiert as Reserviere<T>>::Arg: Clone,
    S: Clone + Serialisiere,
    <S as Serialisiere>::Serialisiert: Eq + Hash,
    L: Serialisiere,
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
            Rectangle::from(gleis.definition.rechteck_an_position(spurweite, &gleis.position));
        gleise.push(GeomWithData::new(rectangle, gleis));
        (gleise, anschlüsse)
    })
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere<L: Serialisiere>(
        self,
        spurweite: Spurweite,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
        kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
        dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
        kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
        fehler: &mut Vec<LadenFehler<L>>,
        canvas: &Arc<Mutex<Cache>>,
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
        reserviere_anschlüsse! {
            geraden: Gerade: kontakt: kontakte: &(),
            kurven: Kurve: kontakt: kontakte: &(),
            weichen: Weiche: steuerung: gerade_weichen: canvas,
            dreiwege_weichen: DreiwegeWeiche: steuerung: dreiwege_weichen: canvas,
            kurven_weichen: KurvenWeiche: steuerung: kurven_weichen: canvas,
            s_kurven_weichen: SKurvenWeiche: steuerung: gerade_weichen: canvas,
            kreuzungen: Kreuzung: steuerung: gerade_weichen: canvas,
        }
    }
}

pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
pub(in crate::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<Leiter> = HashMap<
    geschwindigkeit::Name,
    (GeschwindigkeitSerialisiert<Leiter>, StreckenabschnittMapSerialisiert),
>;

#[derive(zugkontrolle_macros::Debug, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Serialisiere>::Serialisiert: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[serde(bound(
    serialize = "L: Serialisiere + Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize, <L as Leiter>::Fahrtrichtung: Serialize",
    deserialize = "L: Serialisiere + Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>",
))]
pub(in crate::gleis::gleise) struct ZustandSerialisiert<L>
where
    L: Leiter + Serialisiere,
{
    pub(crate) zugtyp: ZugtypSerialisiert<L>,
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<L>,
    pub(crate) pläne: HashMap<plan::Name, PlanSerialisiert<L>>,
}

impl<L: Serialisiere + BekannterLeiter> Zustand<L> {
    /// Erzeuge eine Serialisierbare Repräsentation.
    #[allow(single_use_lifetimes)]
    pub(in crate::gleis::gleise) fn serialisiere(&self) -> ZustandSerialisiert<L>
    where
        <L as Leiter>::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de>,
    {
        let serialisiere_streckenabschnitt_map = |map: &StreckenabschnittMap| {
            map.iter()
                .map(|(name, (streckenabschnitt, daten))| {
                    (name.clone(), (streckenabschnitt.serialisiere(), daten.serialisiere()))
                })
                .collect()
        };

        ZustandSerialisiert {
            zugtyp: self.zugtyp.clone().into(),
            ohne_streckenabschnitt: self.ohne_streckenabschnitt.serialisiere(),
            ohne_geschwindigkeit: serialisiere_streckenabschnitt_map(&self.ohne_geschwindigkeit),
            geschwindigkeiten: self
                .geschwindigkeiten
                .iter()
                .map(|(name, (geschwindigkeit, streckenabschnitt_map))| {
                    (
                        name.clone(),
                        (
                            geschwindigkeit.serialisiere(),
                            serialisiere_streckenabschnitt_map(streckenabschnitt_map),
                        ),
                    )
                })
                .collect(),
            pläne: self
                .pläne
                .iter()
                .map(|(name, plan)| (name.clone(), plan.serialisiere()))
                .collect(),
        }
    }

    fn anschlüsse_ausgeben(&mut self) -> Anschlüsse {
        let mut anschlüsse = Anschlüsse::default();
        fn collect_gleis_anschlüsse<T: DatenAuswahl + Serialisiere>(
            daten: &mut GleiseDaten,
            anschlüsse: &mut Anschlüsse,
        ) {
            while let Some(geom_with_data) =
                daten.rstern_mut::<T>().remove_with_selection_function(SelectAll)
            {
                anschlüsse.anhängen(geom_with_data.data.definition.anschlüsse())
            }
        }
        fn collect_daten_anschlüsse(daten: &mut GleiseDaten, anschlüsse: &mut Anschlüsse) {
            macro_rules! collect_gleis_anschlüsse {
                ($($typ: ident),* $(,)?) => {$(
                    collect_gleis_anschlüsse::<$typ>(daten, anschlüsse)
                );*}
            }
            collect_gleis_anschlüsse! {
                Gerade,
                Kurve,
                Weiche,
                DreiwegeWeiche,
                KurvenWeiche,
                SKurvenWeiche,
                Kreuzung
            }
        }
        collect_daten_anschlüsse(&mut self.ohne_streckenabschnitt, &mut anschlüsse);
        fn collect_streckenabschnitt_map_anschlüsse(
            streckenabschnitt_map: &mut StreckenabschnittMap,
            anschlüsse: &mut Anschlüsse,
        ) {
            for (_name, (streckenabschnitt, mut daten)) in streckenabschnitt_map.drain() {
                anschlüsse.anhängen(streckenabschnitt.anschlüsse());
                collect_daten_anschlüsse(&mut daten, anschlüsse);
            }
        }
        collect_streckenabschnitt_map_anschlüsse(&mut self.ohne_geschwindigkeit, &mut anschlüsse);
        for (_name, (geschwindigkeit, mut streckenabschnitt_map)) in self.geschwindigkeiten.drain()
        {
            anschlüsse.anhängen(geschwindigkeit.anschlüsse());
            collect_streckenabschnitt_map_anschlüsse(&mut streckenabschnitt_map, &mut anschlüsse);
        }
        anschlüsse
    }
}

impl<L: Serialisiere> From<FalscherLeiter> for LadenFehler<L> {
    fn from(fehler: FalscherLeiter) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

fn reserviere_streckenabschnitt_map<L: Serialisiere>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    streckenabschnitt_map: StreckenabschnittMapSerialisiert,
    anschlüsse: Anschlüsse,
    streckenabschnitte: &mut HashMap<OutputSerialisiert, Streckenabschnitt>,
    gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
    kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
    dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
    kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    canvas: &Arc<Mutex<Cache>>,
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
                canvas,
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

fn reserviere_geschwindigkeit_map<L>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    geschwindigkeiten_map: GeschwindigkeitMapSerialisiert<L>,
    anschlüsse: Anschlüsse,
    ohne_streckenabschnitt: &mut GleiseDaten,
    geschwindigkeiten: &mut HashMap<GeschwindigkeitSerialisiert<L>, Geschwindigkeit<L>>,
    streckenabschnitte: &mut HashMap<OutputSerialisiert, Streckenabschnitt>,
    gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
    kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
    dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
    kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    canvas: &Arc<Mutex<Cache>>,
) -> (GeschwindigkeitMap<L>, Anschlüsse, Option<StreckenabschnittMap>)
where
    L: Serialisiere,
    <L as Serialisiere>::Serialisiert: Clone + Eq + Hash + Reserviere<L, Arg = ()>,
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
                    canvas,
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

impl<L: Serialisiere + BekannterLeiter> ZustandSerialisiert<L>
where
    <L as Serialisiere>::Serialisiert: Clone + Eq + Hash + Reserviere<L, Arg = ()>,
{
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        canvas: &Arc<Mutex<Cache>>,
    ) -> Result<(Zustand<L>, Vec<LadenFehler<L>>), FalscherLeiter> {
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

        let zugtyp = Zugtyp::try_from(zugtyp)?;

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
            canvas,
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
            canvas,
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
                canvas,
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
                &canvas,
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

impl<L: Serialisiere + BekannterLeiter> Gleise<L> {
    /// Speicher alle Gleise, [Streckenabschnitte](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [Zugtyp]
    /// in einer Datei.
    #[allow(single_use_lifetimes)]
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> Result<(), Fehler>
    where
        L::VerhältnisFahrspannungÜberspannung: Serialize,
        L::UmdrehenZeit: Serialize,
        L::Fahrtrichtung: Clone + Serialize + for<'de> Deserialize<'de>,
    {
        let serialisiert = self.zustand.serialisiere();
        let file = fs::File::create(pfad)?;
        bincode::serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }

    /// Lade Gleise, [Streckenabschnitte](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [Zugtyp]
    /// aus einer Datei.
    #[allow(single_use_lifetimes)]
    pub fn laden(
        &mut self,
        lager: &mut anschluss::Lager,
        pfad: impl AsRef<std::path::Path>,
    ) -> Result<(), NonEmpty<LadenFehler<L>>>
    where
        L: v2::Kompatibel,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
        <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
        <L as Serialisiere>::Serialisiert: Clone + Eq + Hash + Reserviere<L, Arg = ()>,
    {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.canvas.lock().leeren();
        let anschlüsse = self.zustand.anschlüsse_ausgeben();

        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // lese & parse Datei
        let mut file = fs::File::open(pfad).map_err(|fehler| NonEmpty::singleton(fehler.into()))?;
        let mut content = Vec::new();
        let _ =
            file.read_to_end(&mut content).map_err(|fehler| NonEmpty::singleton(fehler.into()))?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<L> = bincode::deserialize(slice)
            .or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map_err(|v2| LadenFehler::BincodeDeserialisieren { aktuell, v2 })
                    .and_then(|v2: v2::GleiseVecs<L>| v2.try_into().map_err(LadenFehler::from))
            })
            .map_err(|fehler| NonEmpty::singleton(fehler.into()))?;

        // reserviere Anschlüsse
        let (zustand, fehler) = zustand_serialisiert
            .reserviere(lager, anschlüsse, &self.canvas)
            .map_err(|fehler| NonEmpty::singleton(LadenFehler::from(fehler)))?;
        self.zustand = zustand;
        if let Some(non_empty) = NonEmpty::from_vec(fehler) {
            Err(non_empty)
        } else {
            Ok(())
        }
    }
}
