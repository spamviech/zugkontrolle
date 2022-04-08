//! speichern und laden Methode für [Gleise].

use std::{collections::HashMap, fmt::Debug, io::Read};

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
        InputAnschluss, OutputAnschluss,
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
        plan::{Plan, PlanSerialisiert},
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    typen::{mm::Spurweite, vektor::Vektor, Zeichnen},
    zugtyp::{Zugtyp, ZugtypSerialisiert},
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
    pub(crate) pläne: Vec<PlanSerialisiert<L>>,
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
            pläne: self.pläne.iter().map(Plan::serialisiere).collect(),
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

impl<L: Serialisiere + BekannterLeiter> ZustandSerialisiert<L> {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Zustand<L>, anschluss::Fehler> {
        let ZustandSerialisiert {
            zugtyp,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne,
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
        )?;

        fn reserviere_streckenabschnitt_map(
            spurweite: Spurweite,
            lager: &mut anschluss::Lager,
            streckenabschnitt_map: StreckenabschnittMapSerialisiert,
            pwm_pins: Vec<pwm::Pin>,
            output_anschlüsse: Vec<OutputAnschluss>,
            input_anschlüsse: Vec<InputAnschluss>,
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
        )?;

        let Reserviert {
            anschluss: geschwindigkeiten,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
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

        let Reserviert {
            anschluss: pläne,
            pwm_nicht_benötigt: _,
            output_nicht_benötigt: _,
            input_nicht_benötigt: _,
        } = pläne.into_iter().fold(
            Ok(Reserviert {
                anschluss: Vec::new(),
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            }),
            |acc: Result<_, anschluss::Fehler>, plan_serialisiert| {
                let Reserviert {
                    anschluss: mut pläne,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = acc?;
                let Reserviert {
                    anschluss: plan,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = plan_serialisiert
                    .reserviere(
                        lager,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    )
                    .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
                pläne.push(plan);
                Ok(Reserviert {
                    anschluss: pläne,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                })
            },
        )?;

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

fn reserviere_anschlüsse<T: Zeichnen + Serialisiere>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    source: Vec<Gleis<<T as Serialisiere>::Serialisiert>>,
    pwm_pins: Vec<pwm::Pin>,
    output_anschlüsse: Vec<OutputAnschluss>,
    input_anschlüsse: Vec<InputAnschluss>,
) -> Result<
    (
        Vec<GeomWithData<Rectangle<Vektor>, Gleis<T>>>,
        Vec<pwm::Pin>,
        Vec<OutputAnschluss>,
        Vec<InputAnschluss>,
    ),
    anschluss::Fehler,
> {
    source.into_iter().fold(
        Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        |acc_res: Result<_, anschluss::Fehler>, gleis_save| {
            let mut acc = acc_res?;
            let Reserviert {
                anschluss: gleis,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            } = gleis_save
                .reserviere(lager, acc.1, acc.2, acc.3)
                .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
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
    ) -> Result<Reserviert<GleiseDaten>, anschluss::Fehler> {
        macro_rules! reserviere_anschlüsse {
            ($($rstern: ident),* $(,)?) => {
                $(
                    let ($rstern, pwm_pins, output_anschlüsse, input_anschlüsse) =
                        reserviere_anschlüsse(
                            spurweite,
                            lager,
                            self.$rstern,
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse
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
        let file = std::fs::File::create(pfad)?;
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
    ) -> Result<(), Fehler>
    where
        L: v2::Kompatibel,
        L::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        L::UmdrehenZeit: for<'de> Deserialize<'de>,
        L::Fahrtrichtung: for<'de> Deserialize<'de>,
    {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.canvas.leeren();
        let (pwm_pins, output_anschlüsse, input_anschlüsse) = self.zustand.anschlüsse_ausgeben();

        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // lese & parse Datei
        let mut file = std::fs::File::open(pfad)?;
        let mut content = Vec::new();
        let _ = file.read_to_end(&mut content)?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<L> =
            bincode::deserialize(slice).or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map_err(|v2| Fehler::BincodeDeserialisieren { aktuell, v2 })
                    .and_then(|v2: v2::GleiseVecs<L>| v2.try_into().map_err(Fehler::from))
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
