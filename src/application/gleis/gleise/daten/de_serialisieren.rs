//! speichern und laden Methode für Gleise

use std::io::Read;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        InputAnschluss, OutputAnschluss,
    },
    application::gleis::gleise::{daten::*, Fehler, Gleise},
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert},
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
};

pub(in crate::application::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
pub(in crate::application::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<Leiter> =
    HashMap<
        geschwindigkeit::Name,
        (GeschwindigkeitSerialisiert<Leiter>, StreckenabschnittMapSerialisiert),
    >;
#[derive(Debug, Serialize, Deserialize)]
pub struct ZustandSerialisiert<Leiter> {
    pub(crate) zugtyp: Zugtyp<Leiter>,
    pub(crate) leiter: String,
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<Leiter>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Leiter> Zustand<Leiter> {
    /// Erzeuge eine serealisierbare Repräsentation.
    pub fn serialisiere(&self) -> ZustandSerialisiert<Leiter> {
        let serialisiere_streckenabschnitt_map = |map: &StreckenabschnittMap| {
            map.iter()
                .map(|(name, (streckenabschnitt, _fließend, daten))| {
                    (name.clone(), (streckenabschnitt.serialisiere(), daten.serialisiere()))
                })
                .collect()
        };
        ZustandSerialisiert {
            zugtyp: todo!("name"),
            leiter: todo!("leiter"),
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
            // TODO wirkliche Konvertierung, sobald Plan implementiert ist
            pläne: Vec::new(),
        }
    }

    /// Erhalten alle verwendeten Anschlüsse.
    pub fn anschlüsse(mut self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
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

impl<Leiter> ZustandSerialisiert<Leiter> {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Zustand<Leiter>, anschluss::Fehler> {
        let ZustandSerialisiert {
            zugtyp: _,
            leiter,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            // TODO wirkliche Konvertierung, sobald Plan implementiert ist
            pläne: _,
        } = self;
        let Reserviert {
            anschluss: ohne_streckenabschnitt,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = ohne_streckenabschnitt.reserviere(
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        fn reserviere_streckenabschnitt_map(
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
            lager,
            ohne_geschwindigkeit,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
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
        Ok(Zustand { ohne_streckenabschnitt, ohne_geschwindigkeit, geschwindigkeiten })
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
    /// Erzeuge eine serealisierbare Repräsentation
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
            let rectangle = Rectangle::from(gleis.definition.rechteck_an_position(&gleis.position));
            acc.0.push(GeomWithData::new(rectangle, gleis));
            Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        },
    )
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
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

impl<Leiter> Gleise<Leiter> {
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> Result<(), Fehler> {
        let serialisiert = self.zustand.serialisiere();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }
}

impl<Leiter> Gleise<Leiter> {
    pub fn laden(
        &mut self,
        lager: &mut anschluss::Lager,
        pfad: impl AsRef<std::path::Path>,
    ) -> Result<(), Fehler> {
        // aktuellen Zustand zurücksetzen
        self.canvas.leeren();
        let zustand = std::mem::replace(&mut self.zustand, Zustand::neu());
        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // sammle bisherige Anschlüsse
        let (pwm_pins, output_anschlüsse, input_anschlüsse) = zustand.anschlüsse();

        // lese & parse Datei
        let mut file = std::fs::File::open(pfad)?;
        let mut content = Vec::new();
        let _ = file.read_to_end(&mut content)?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<Leiter> = bincode::deserialize(slice)
            .or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map(v2::GleiseVecs::<Leiter>::into)
                    .map_err(|v2| Fehler::BincodeDeserialisieren { aktuell, v2 })
            })?;
        if zustand_serialisiert.leiter != todo!("name") {
            return Err(Fehler::FalscherLeiter(zustand_serialisiert.leiter));
        }

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
