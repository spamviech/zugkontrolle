//! speichern und laden Methode für Gleise

use std::io::Read;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        Anschlüsse, InputAnschluss, OutputAnschluss,
    },
    application::{
        gleis::gleise::{daten::*, Fehler, Gleise},
        typen::*,
        verbindung::Verbindung,
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
};

// FIXME Streckenabschnitt der Gleise aktuell nicht erwähnt!
// Auf Map<Name, (Streckenabschnitt, DatenSerialisiert)> konvertieren?
#[derive(Serialize, Deserialize)]
pub struct ZustandSerialisiert<Z: Zugtyp> {
    pub zugtyp: String,
    pub ohne_streckenabschnitt: GleiseDatenSerialisiert<Z>,
    pub streckenabschnitte: HashMap<
        streckenabschnitt::Name,
        (StreckenabschnittSerialisiert, GleiseDatenSerialisiert<Z>),
    >,
    pub geschwindigkeiten: geschwindigkeit::MapSerialisiert<Z::Leiter>,
    pub pläne: Vec<Plan>,
}

impl<Z> Debug for ZustandSerialisiert<Z>
where
    Z: Zugtyp,
    <Z::Leiter as Serialisiere>::Serialisiert: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ZustandSerialisiert")
            .field("zugtyp", &self.zugtyp)
            .field("ohne_streckenabschnitt", &self.ohne_streckenabschnitt)
            .field("streckenabschnitte", &self.streckenabschnitte)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .field("pläne", &self.pläne)
            .finish()
    }
}

impl<Z: Zugtyp + Serialize + for<'de> Deserialize<'de>> Zustand<Z> {
    fn serialisiere(&self) -> ZustandSerialisiert<Z> {
        ZustandSerialisiert {
            zugtyp: Z::NAME.to_string(),
            ohne_streckenabschnitt: self.ohne_streckenabschnitt.serialisiere(),
            streckenabschnitte: self
                .streckenabschnitte
                .iter()
                .map(|(name, (streckenabschnitt, _fließend, daten))| {
                    (name.clone(), (streckenabschnitt.serialisiere(), daten.serialisiere()))
                })
                .collect(),
            geschwindigkeiten: self
                .geschwindigkeiten
                .iter()
                .map(|(name, geschwindigkeit)| (name.clone(), geschwindigkeit.serialisiere()))
                .collect(),
            // TODO wirkliche Konvertierung, sobald Plan implementiert ist
            pläne: Vec::new(),
        }
    }

    fn anschlüsse(mut self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        macro_rules! collect_anschlüsse {
            ($struktur: expr) => {
                let (pwm, output, input) = $struktur.anschlüsse();
                pwm_pins.extend(pwm.into_iter());
                output_anschlüsse.extend(output.into_iter());
                input_anschlüsse.extend(input.into_iter());
            };
        }
        macro_rules! collect_gleis_anschlüsse {
            ($daten: expr, $($rstern: ident),*) => {
                $(while let Some(geom_with_data) =
                    $daten.$rstern.remove_with_selection_function(SelectAll)
                {
                    collect_anschlüsse! {
                        geom_with_data.data.definition
                    }
                })*
            };
        }
        macro_rules! collect_all_gleis_anschlüsse {
            ($daten: expr) => {
                collect_gleis_anschlüsse! {
                    $daten,
                    geraden,
                    kurven,
                    weichen,
                    dreiwege_weichen,
                    kurven_weichen,
                    s_kurven_weichen,
                    kreuzungen
                }
            };
        }
        for (_name, geschwindigkeit) in self.geschwindigkeiten.drain() {
            collect_anschlüsse!(geschwindigkeit);
        }
        collect_all_gleis_anschlüsse!(self.ohne_streckenabschnitt);
        for (_name, (streckenabschnitt, _fließend, mut daten)) in self.streckenabschnitte.drain() {
            collect_anschlüsse!(streckenabschnitt);
            collect_all_gleis_anschlüsse!(daten);
        }
        (pwm_pins, output_anschlüsse, input_anschlüsse)
    }
}

impl<Z: Zugtyp + Serialize + for<'de> Deserialize<'de>> ZustandSerialisiert<Z> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Zustand<Z>, anschluss::Fehler> {
        let ZustandSerialisiert {
            zugtyp: _,
            ohne_streckenabschnitt,
            streckenabschnitte,
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
            anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        let Reserviert {
            anschluss: streckenabschnitte,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = streckenabschnitte.into_iter().fold(
            Ok(Reserviert {
                anschluss: HashMap::new(),
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
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
                        anschlüsse,
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
                    anschlüsse,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                )?;
                map.insert(name, (streckenabschnitt, Fließend::Gesperrt, daten));
                Ok(Reserviert {
                    anschluss: map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                })
            },
        )?;
        let Reserviert {
            anschluss: geschwindigkeiten,
            pwm_nicht_benötigt: _,
            output_nicht_benötigt: _,
            input_nicht_benötigt: _,
        } = geschwindigkeiten
            .into_iter()
            .fold(
                Ok(Reserviert {
                    anschluss: HashMap::new(),
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }),
                |acc, (name, geschwindigkeit)| {
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
                    } = geschwindigkeit.reserviere(
                        anschlüsse,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    )?;
                    map.insert(name, geschwindigkeit);
                    Ok(Reserviert {
                        anschluss: map,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    })
                },
            )
            .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
        Ok(Zustand { ohne_streckenabschnitt, streckenabschnitte, geschwindigkeiten })
    }
}

#[derive(zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct GleiseDatenSerialisiert<Z> {
    pub geraden: Vec<Gleis<GeradeSerialisiert<Z>>>,
    pub kurven: Vec<Gleis<KurveSerialisiert<Z>>>,
    pub weichen: Vec<Gleis<WeicheSerialisiert<Z>>>,
    pub dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert<Z>>>,
    pub kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert<Z>>>,
    pub s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert<Z>>>,
    pub kreuzungen: Vec<Gleis<KreuzungSerialisiert<Z>>>,
}

impl<Z> GleiseDatenSerialisiert<Z> {
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

impl<Z: Zugtyp + Serialize + for<'de> Deserialize<'de>> GleiseDaten<Z> {
    fn serialisiere(&self) -> GleiseDatenSerialisiert<Z> {
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

    fn anschlüsse(mut self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        macro_rules! collect_anschlüsse {
            ($struktur: expr) => {
                let (pwm, output, input) = $struktur.anschlüsse();
                pwm_pins.extend(pwm.into_iter());
                output_anschlüsse.extend(output.into_iter());
                input_anschlüsse.extend(input.into_iter());
            };
        }
        macro_rules! collect_gleis_anschlüsse {
            ($($rstern: ident),* $(,)?) => {
                $(while let Some(geom_with_data) =
                    self.$rstern.remove_with_selection_function(SelectAll)
                {
                    collect_anschlüsse! {
                        geom_with_data.data.definition
                    }
                })*
            };
        }
        collect_gleis_anschlüsse! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
        (pwm_pins, output_anschlüsse, input_anschlüsse)
    }
}

fn reserviere_anschlüsse<T: Zeichnen + Serialisiere>(
    anschlüsse: &mut Anschlüsse,
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
                .reserviere(anschlüsse, acc.1, acc.2, acc.3)
                .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
            let rectangle = Rectangle::from(gleis.definition.rechteck_an_position(&gleis.position));
            acc.0.push(GeomWithData::new(rectangle, gleis));
            Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        },
    )
}

impl<Z: Zugtyp> GleiseDatenSerialisiert<Z> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Reserviert<GleiseDaten<Z>>, anschluss::Fehler> {
        macro_rules! reserviere_anschlüsse {
            ($($rstern: ident),* $(,)?) => {
                $(
                    let ($rstern, pwm_pins, output_anschlüsse, input_anschlüsse) =
                        reserviere_anschlüsse(
                            anschlüsse,
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

impl<Z: Zugtyp + Serialize + for<'de> Deserialize<'de>> Gleise<Z> {
    #[must_use]
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> std::result::Result<(), Fehler> {
        let serialisiert = self.zustand.serialisiere();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }
}

impl<Z> Gleise<Z>
where
    Z: Zugtyp + PartialEq + std::fmt::Debug + for<'de> Deserialize<'de>,
    Geschwindigkeit<<Z as Zugtyp>::Leiter>: Leiter,
{
    pub fn laden(
        &mut self,
        anschlüsse: &mut Anschlüsse,
        pfad: impl AsRef<std::path::Path>,
    ) -> Result<(), Fehler> {
        // sammle bisherige Anschlüsse
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        macro_rules! collect_anschlüsse {
            ($struktur: expr) => {
                let (pwm, output, input) = $struktur.anschlüsse();
                pwm_pins.extend(pwm.into_iter());
                output_anschlüsse.extend(output.into_iter());
                input_anschlüsse.extend(input.into_iter());
            };
        }
        macro_rules! collect_gleis_anschlüsse {
            ($daten: expr, $($rstern: ident),*) => {
                $(while let Some(geom_with_data) =
                    $daten.$rstern.remove_with_selection_function(SelectAll)
                {
                    collect_anschlüsse! {
                        geom_with_data.data.definition
                    }
                })*
            };
        }
        macro_rules! collect_all_gleis_anschlüsse {
            ($daten: expr) => {
                collect_gleis_anschlüsse! {
                    $daten,
                    geraden,
                    kurven,
                    weichen,
                    dreiwege_weichen,
                    kurven_weichen,
                    s_kurven_weichen,
                    kreuzungen
                }
            };
        }
        for (_name, geschwindigkeit) in self.zustand.geschwindigkeiten.drain() {
            collect_anschlüsse!(geschwindigkeit);
        }
        collect_all_gleis_anschlüsse!(self.zustand.ohne_streckenabschnitt);
        for (_name, (streckenabschnitt, _fließend, mut daten)) in
            self.zustand.streckenabschnitte.drain()
        {
            collect_anschlüsse!(streckenabschnitt);
            collect_all_gleis_anschlüsse!(daten);
        }

        // aktuellen Zustand zurücksetzen
        self.canvas.leeren();
        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen
        // self.zustand = Zustand::neu();
        // self.anchor_points = verbindung::rstern::RStern::neu();
        // lese & parse Datei
        let mut file = std::fs::File::open(pfad)?;
        let mut content = Vec::new();
        file.read_to_end(&mut content)?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<Z> =
            bincode::deserialize(slice).or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map(v2::GleiseVecs::<Z>::into)
                    .map_err(|v2| Fehler::BincodeDeserialisieren { aktuell, v2 })
            })?;
        if zustand_serialisiert.zugtyp != Z::NAME {
            return Err(Fehler::FalscherZugtyp(zustand_serialisiert.zugtyp));
        }

        // // reserviere Anschlüsse
        // macro_rules! reserviere_anschlüsse {
        //     (
        //         $name:tt::<$type:ident>,
        //         $pwm_pins:tt,
        //         $output_anschlüsse:tt,
        //         $input_anschlüsse:tt$(,)?
        //     ) => {
        //         // collect to Vec to fail on first error
        //         let ($name, $pwm_pins, $output_anschlüsse, $input_anschlüsse) =
        //             reserviere_anschlüsse::<crate::application::gleis::$type<Z>>(
        //                 anschlüsse,
        //                 $name,
        //                 $pwm_pins,
        //                 $output_anschlüsse,
        //                 $input_anschlüsse,
        //             )?;
        //     };
        // }
        // reserviere_anschlüsse! {
        //     geraden::<Gerade>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     kurven::<Kurve>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     weichen::<Weiche>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     dreiwege_weichen::<DreiwegeWeiche>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     kurven_weichen::<KurvenWeiche>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     s_kurven_weichen::<SKurvenWeiche>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // reserviere_anschlüsse! {
        //     kreuzungen::<Kreuzung>,
        //     pwm_pins,
        //     output_anschlüsse,
        //     input_anschlüsse,
        // }
        // let (streckenabschnitte_reserviert, pwm_pins, output_anschlüsse, input_anschlüsse) =
        //     streckenabschnitte.into_iter().fold(
        //         Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        //         |acc_res: Result<_, anschluss::Fehler>, (name, streckenabschnitt)| {
        //             let mut acc = acc_res?;
        //             let Reserviert {
        //                 anschluss: streckenabschnitt,
        //                 pwm_nicht_benötigt,
        //                 output_nicht_benötigt,
        //                 input_nicht_benötigt,
        //             } = streckenabschnitt
        //                 .reserviere(anschlüsse, acc.1, acc.2, acc.3)
        //                 .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
        //             acc.0.push((name, streckenabschnitt));
        //             Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        //         },
        //     )?;
        // let (geschwindigkeiten_reserviert, _pwm_pins, _output_anschlüsse, _input_anschlüsse) =
        //     geschwindigkeiten.into_iter().fold(
        //         Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        //         |acc_res: Result<_, anschluss::Fehler>, (name, geschwindigkeit)| {
        //             let mut acc = acc_res?;
        //             let Reserviert {
        //                 anschluss: geschwindigkeit,
        //                 pwm_nicht_benötigt,
        //                 output_nicht_benötigt,
        //                 input_nicht_benötigt,
        //             } = geschwindigkeit
        //                 .reserviere(anschlüsse, acc.1, acc.2, acc.3)
        //                 .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
        //             acc.0.push((name, geschwindigkeit));
        //             Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        //         },
        //     )?;
        // // füge anschlüsse zu maps hinzu
        // macro_rules! add_gleise {
        //     ($($gleise: ident,)*) => {
        //         $(
        //             for gleis in $gleise {
        //                 self.add(gleis);
        //             }
        //         );*
        //     }
        // }
        // add_gleise!(
        //     geraden,
        //     kurven,
        //     weichen,
        //     dreiwege_weichen,
        //     kurven_weichen,
        //     s_kurven_weichen,
        //     kreuzungen,
        // );
        // for (name, streckenabschnitt) in streckenabschnitte_reserviert {
        //     self.neuer_streckenabschnitt(name, streckenabschnitt);
        // }
        // Ok(geschwindigkeiten_reserviert)
        // FIXME RTree::bulk_load verwenden!
        todo!()
    }
}
