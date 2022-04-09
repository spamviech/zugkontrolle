//! speichern und laden Methode für [Gleise].

use std::{
    collections::HashMap,
    fmt::Debug,
    fs,
    hash::Hash,
    io::{self, Read},
};

use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        polarität::Fließend,
        InputAnschluss, OutputAnschluss, OutputSerialisiert,
    },
    gleis::{
        gerade::{Gerade, GeradeSerialisiert},
        gleise::{
            daten::{
                v2, DatenAuswahl, Gleis, GleiseDaten, SelectAll, StreckenabschnittMap, Zustand,
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
        geschwindigkeit::{self, BekannterLeiter, GeschwindigkeitSerialisiert, Leiter},
        kontakt::{Kontakt, KontaktSerialisiert},
        plan::{self, PlanSerialisiert, UnbekannteAnschlüsse},
        streckenabschnitt::{self, Streckenabschnitt, StreckenabschnittSerialisiert},
    },
    typen::{mm::Spurweite, vektor::Vektor, Zeichnen},
    zugtyp::{FalscherLeiter, Zugtyp, ZugtypSerialisiert},
};

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
                .map(|(name, (streckenabschnitt, _fließend, daten))| {
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

    fn anschlüsse_ausgeben(
        &mut self,
    ) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        fn collect_anschlüsse<S: Serialisiere>(
            struktur: S,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            let (pwm, output, input) = struktur.anschlüsse();
            pwm_pins.extend(pwm.into_iter());
            output_anschlüsse.extend(output.into_iter());
            input_anschlüsse.extend(input.into_iter());
        }
        fn collect_gleis_anschlüsse<T: DatenAuswahl + Serialisiere>(
            daten: &mut GleiseDaten,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            while let Some(geom_with_data) =
                daten.rstern_mut::<T>().remove_with_selection_function(SelectAll)
            {
                collect_anschlüsse(
                    geom_with_data.data.definition,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                )
            }
        }
        fn collect_daten_anschlüsse(
            daten: &mut GleiseDaten,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            macro_rules! collect_gleis_anschlüsse {
                ($($typ: ident),* $(,)?) => {$(
                    collect_gleis_anschlüsse::<$typ>(
                        daten,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    )
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
        collect_daten_anschlüsse(
            &mut self.ohne_streckenabschnitt,
            &mut pwm_pins,
            &mut output_anschlüsse,
            &mut input_anschlüsse,
        );
        fn collect_streckenabschnitt_map_anschlüsse(
            streckenabschnitt_map: &mut StreckenabschnittMap,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            for (_name, (streckenabschnitt, _fließend, mut daten)) in streckenabschnitt_map.drain()
            {
                collect_anschlüsse(
                    streckenabschnitt,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                );
                collect_daten_anschlüsse(
                    &mut daten,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                );
            }
        }
        collect_streckenabschnitt_map_anschlüsse(
            &mut self.ohne_geschwindigkeit,
            &mut pwm_pins,
            &mut output_anschlüsse,
            &mut input_anschlüsse,
        );
        for (_name, (geschwindigkeit, mut streckenabschnitt_map)) in self.geschwindigkeiten.drain()
        {
            collect_anschlüsse(
                geschwindigkeit,
                &mut pwm_pins,
                &mut output_anschlüsse,
                &mut input_anschlüsse,
            );
            collect_streckenabschnitt_map_anschlüsse(
                &mut streckenabschnitt_map,
                &mut pwm_pins,
                &mut output_anschlüsse,
                &mut input_anschlüsse,
            );
        }
        (pwm_pins, output_anschlüsse, input_anschlüsse)
    }
}

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
    UnbekannteAnschlüsse(UnbekannteAnschlüsse<L>),
}

impl<L: Serialisiere> From<FalscherLeiter> for LadenFehler<L> {
    fn from(fehler: FalscherLeiter) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<L: Serialisiere + BekannterLeiter> ZustandSerialisiert<L>
where
    <L as Serialisiere>::Serialisiert: Clone + Eq + Hash,
{
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Zustand<L>, LadenFehler<L>> {
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

        let Reserviert {
            anschluss: ohne_streckenabschnitt,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = ohne_streckenabschnitt.reserviere(
            zugtyp.spurweite,
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
            &mut bekannte_gerade_weichen,
            &mut bekannte_kurven_weichen,
            &mut bekannte_dreiwege_weichen,
            &mut bekannte_kontakte,
        )?;

        fn reserviere_streckenabschnitt_map(
            spurweite: Spurweite,
            lager: &mut anschluss::Lager,
            streckenabschnitt_map: StreckenabschnittMapSerialisiert,
            pwm_pins: Vec<pwm::Pin>,
            output_anschlüsse: Vec<OutputAnschluss>,
            input_anschlüsse: Vec<InputAnschluss>,
            streckenabschnitte: &mut HashMap<OutputSerialisiert, Streckenabschnitt>,
            gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
            kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
            dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
            kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
        ) -> Result<Reserviert<StreckenabschnittMap>, anschluss::Fehler> {
            streckenabschnitt_map.into_iter().fold(
                Ok(Reserviert {
                    anschluss: HashMap::new(),
                    pwm_nicht_benötigt: pwm_pins,
                    output_nicht_benötigt: output_anschlüsse,
                    input_nicht_benötigt: input_anschlüsse,
                }),
                |acc: Result<_, anschluss::Fehler>, (name, (streckenabschnitt, daten))| {
                    let Reserviert {
                        anschluss: mut map,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = acc?;
                    let leiter_serialisiert = streckenabschnitt.anschluss_ref().clone();
                    let Reserviert {
                        anschluss: streckenabschnitt,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = streckenabschnitt
                        .reserviere(
                            lager,
                            pwm_nicht_benötigt,
                            output_nicht_benötigt,
                            input_nicht_benötigt,
                        )
                        .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
                    let _ =
                        streckenabschnitte.insert(leiter_serialisiert, streckenabschnitt.clone());
                    let Reserviert {
                        anschluss: daten,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = daten.reserviere(
                        spurweite,
                        lager,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                        gerade_weichen,
                        kurven_weichen,
                        dreiwege_weichen,
                        kontakte,
                    )?;
                    let _ = map.insert(name, (streckenabschnitt, Fließend::Gesperrt, daten));
                    Ok(Reserviert {
                        anschluss: map,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    })
                },
            )
        }
        let Reserviert {
            anschluss: ohne_geschwindigkeit,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = reserviere_streckenabschnitt_map(
            zugtyp.spurweite,
            lager,
            ohne_geschwindigkeit,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
            &mut bekannte_streckenabschnitte,
            &mut bekannte_gerade_weichen,
            &mut bekannte_kurven_weichen,
            &mut bekannte_dreiwege_weichen,
            &mut bekannte_kontakte,
        )?;

        let Reserviert {
            anschluss: geschwindigkeiten,
            pwm_nicht_benötigt: _,
            output_nicht_benötigt: _,
            input_nicht_benötigt: _,
        } = geschwindigkeiten.into_iter().fold(
            Ok(Reserviert {
                anschluss: HashMap::new(),
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            }),
            |acc: Result<_, anschluss::Fehler>,
             (name, (geschwindigkeit, streckenabschnitt_map))| {
                let Reserviert {
                    anschluss: mut map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = acc?;
                let geschwindigkeit_serialisiert = geschwindigkeit.clone();
                let Reserviert {
                    anschluss: geschwindigkeit,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit
                    .reserviere(
                        lager,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    )
                    .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
                let _ = bekannte_geschwindigkeiten
                    .insert(geschwindigkeit_serialisiert, geschwindigkeit.clone());
                let Reserviert {
                    anschluss: streckenabschnitt_map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = reserviere_streckenabschnitt_map(
                    zugtyp.spurweite,
                    lager,
                    streckenabschnitt_map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                    &mut bekannte_streckenabschnitte,
                    &mut bekannte_gerade_weichen,
                    &mut bekannte_kurven_weichen,
                    &mut bekannte_dreiwege_weichen,
                    &mut bekannte_kontakte,
                )?;
                let _ = map.insert(name, (geschwindigkeit, streckenabschnitt_map));
                Ok(Reserviert {
                    anschluss: map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                })
            },
        )?;

        let mut pläne = HashMap::new();
        for (name, plan_serialisiert) in pläne_serialisiert {
            let plan = plan_serialisiert.deserialisiere(
                &bekannte_geschwindigkeiten,
                &bekannte_streckenabschnitte,
                &bekannte_gerade_weichen,
                &bekannte_kurven_weichen,
                &bekannte_dreiwege_weichen,
                &bekannte_kontakte,
            )?;
            let _ = pläne.insert(name, plan);
        }

        Ok(Zustand {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne,
        })
    }
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

fn reserviere_anschlüsse<T: Zeichnen + Serialisiere, S: Clone + Serialisiere>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    source: Vec<Gleis<<T as Serialisiere>::Serialisiert>>,
    pwm_pins: Vec<pwm::Pin>,
    output_anschlüsse: Vec<OutputAnschluss>,
    input_anschlüsse: Vec<InputAnschluss>,
    steuerung: impl Fn(&T) -> &Option<S>,
    map: &mut HashMap<S::Serialisiert, S>,
) -> Result<
    (
        Vec<GeomWithData<Rectangle<Vektor>, Gleis<T>>>,
        Vec<pwm::Pin>,
        Vec<OutputAnschluss>,
        Vec<InputAnschluss>,
    ),
    anschluss::Fehler,
>
where
    <S as Serialisiere>::Serialisiert: Eq + Hash,
{
    source.into_iter().fold(
        Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        |acc_res: Result<_, anschluss::Fehler>, gleis_serialisiert| {
            let mut acc = acc_res?;
            let Reserviert {
                anschluss: gleis,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            } = gleis_serialisiert
                .reserviere(lager, acc.1, acc.2, acc.3)
                .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
            // Bekannte Steuerung sichern
            if let Some(steuerung) = steuerung(&gleis.definition) {
                let serialisiert = steuerung.serialisiere();
                let _ = map.insert(serialisiert, steuerung.clone());
            }
            // Gleis mit BoundingBox speichern
            let rectangle =
                Rectangle::from(gleis.definition.rechteck_an_position(spurweite, &gleis.position));
            acc.0.push(GeomWithData::new(rectangle, gleis));
            Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        },
    )
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        spurweite: Spurweite,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        gerade_weichen: &mut HashMap<plan::GeradeWeicheSerialisiert, plan::GeradeWeiche>,
        kurven_weichen: &mut HashMap<plan::KurvenWeicheSerialisiert, plan::KurvenWeiche>,
        dreiwege_weichen: &mut HashMap<plan::DreiwegeWeicheSerialisiert, plan::DreiwegeWeiche>,
        kontakte: &mut HashMap<KontaktSerialisiert, Kontakt>,
    ) -> Result<Reserviert<GleiseDaten>, anschluss::Fehler> {
        macro_rules! reserviere_anschlüsse {
            ($($rstern: ident: $ty: ty: $steuerung: ident: $map: expr),* $(,)?) => {
                $(
                    let ($rstern, pwm_pins, output_anschlüsse, input_anschlüsse) =
                        reserviere_anschlüsse(
                            spurweite,
                            lager,
                            self.$rstern,
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse,
                            |gleis: &$ty| &gleis.$steuerung,
                            $map,
                        )?;
                )*
                Ok(Reserviert {
                    anschluss: GleiseDaten {
                        $($rstern: RTree::bulk_load($rstern)),*
                    },
                    pwm_nicht_benötigt: pwm_pins,
                    output_nicht_benötigt: output_anschlüsse,
                    input_nicht_benötigt: input_anschlüsse,
                })
            };
        }
        reserviere_anschlüsse! {
            geraden: Gerade: kontakt: kontakte,
            kurven: Kurve: kontakt: kontakte,
            weichen: Weiche: steuerung: gerade_weichen,
            dreiwege_weichen: DreiwegeWeiche: steuerung: dreiwege_weichen,
            kurven_weichen: KurvenWeiche: steuerung: kurven_weichen,
            s_kurven_weichen: SKurvenWeiche: steuerung: gerade_weichen,
            kreuzungen: Kreuzung: steuerung: gerade_weichen,
        }
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
    ) -> Result<(), LadenFehler<L>>
    where
        L: v2::Kompatibel,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
        <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
        <L as Serialisiere>::Serialisiert: Clone + Eq + Hash,
    {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.canvas.leeren();
        let (pwm_pins, output_anschlüsse, input_anschlüsse) = self.zustand.anschlüsse_ausgeben();

        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // lese & parse Datei
        let mut file = fs::File::open(pfad)?;
        let mut content = Vec::new();
        let _ = file.read_to_end(&mut content)?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<L> =
            bincode::deserialize(slice).or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map_err(|v2| LadenFehler::BincodeDeserialisieren { aktuell, v2 })
                    .and_then(|v2: v2::GleiseVecs<L>| v2.try_into().map_err(LadenFehler::from))
            })?;

        // reserviere Anschlüsse
        self.zustand = zustand_serialisiert.reserviere(
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(())
    }
}
