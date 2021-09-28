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
    steuerung::geschwindigkeit::{self, Geschwindigkeit, Leiter},
};

// FIXME Streckenabschnitt der Gleise aktuell nicht erwähnt!
// Auf Map<Name, (Streckenabschnitt, DatenSerialisiert)> konvertieren?
#[derive(Serialize, Deserialize)]
pub(crate) struct Serialisiert<Z: Zugtyp> {
    pub(crate) zugtyp: String,
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert<Z>>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert<Z>>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert<Z>>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert<Z>>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert<Z>>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert<Z>>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert<Z>>>,
    pub(crate) streckenabschnitte: streckenabschnitt::MapSerialisiert,
    pub(crate) geschwindigkeiten: geschwindigkeit::MapSerialisiert<Z::Leiter>,
    pub(crate) pläne: Vec<Plan>,
}

impl<Z> Debug for Serialisiert<Z>
where
    Z: Zugtyp,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Serialisiert")
            .field("zugtyp", &self.zugtyp)
            .field("geraden", &self.geraden)
            .field("kurven", &self.kurven)
            .field("weichen", &self.weichen)
            .field("dreiwege_weichen", &self.dreiwege_weichen)
            .field("kurven_weichen", &self.kurven_weichen)
            .field("s_kurven_weichen", &self.s_kurven_weichen)
            .field("kreuzungen", &self.kreuzungen)
            .field("streckenabschnitte", &self.streckenabschnitte)
            .field("geschwindigkeiten", &self.geschwindigkeiten)
            .field("pläne", &self.pläne)
            .finish()
    }
}

impl<Z: Zugtyp> From<&Zustand<Z>> for Serialisiert<Z> {
    fn from(
        Zustand { ohne_streckenabschnitt, streckenabschnitte, geschwindigkeiten }: &Zustand<Z>,
    ) -> Self {
        // macro_rules! hashmaps_to_vecs {
        //     ($($map:ident),* $(,)?) => {
        //         Serialisiert {
        //             zugtyp: Z::NAME.to_string(),
        //             streckenabschnitte: maps.streckenabschnitte.iter().map(
        //                 |(name, (streckenabschnitt, _fließend))|
        //                     (name.clone(), streckenabschnitt.serialisiere())
        //                 ).collect(),
        //             geschwindigkeiten,
        //             // TODO wirkliche Konvertierung, sobald Plan implementiert ist
        //             pläne: Vec::new(),
        //             $($map: maps.$map.values().map(
        //                 |Gleis {position, definition, streckenabschnitt}|
        //                 Gleis {
        //                     position: position.clone(),
        //                     definition: definition.serialisiere(),
        //                     streckenabschnitt: streckenabschnitt.clone()
        //                 })
        //                 .collect()
        //             ),*
        //         }
        //     };
        // }
        // hashmaps_to_vecs!(
        //     geraden,
        //     kurven,
        //     weichen,
        //     dreiwege_weichen,
        //     kurven_weichen,
        //     s_kurven_weichen,
        //     kreuzungen,
        // )
        todo!()
    }
}

impl<Z: Zugtyp + Serialize> Gleise<Z> {
    #[must_use]
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> std::result::Result<(), Fehler> {
        let serialisiert = Serialisiert::from(&self.zustand);
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }
}

fn reserviere_anschlüsse<T: Serialisiere>(
    anschlüsse: &mut Anschlüsse,
    source: Vec<Gleis<<T as Serialisiere>::Serialisiert>>,
    pwm_pins: Vec<pwm::Pin>,
    output_anschlüsse: Vec<OutputAnschluss>,
    input_anschlüsse: Vec<InputAnschluss>,
) -> Result<
    (Vec<Gleis<T>>, Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>),
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
            acc.0.push(gleis);
            Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        },
    )
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
        let Serialisiert {
            zugtyp,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            streckenabschnitte,
            geschwindigkeiten,
            pläne: _, // TODO verwenden, sobald Plan implementiert ist
        } = bincode::deserialize(slice).or_else(|aktuell| {
            bincode::deserialize(slice)
                .map(v2::GleiseVecs::<Z>::into)
                .map_err(|v2| Fehler::BincodeDeserialisieren { aktuell, v2 })
        })?;
        if zugtyp != Z::NAME {
            return Err(Fehler::FalscherZugtyp(zugtyp));
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
