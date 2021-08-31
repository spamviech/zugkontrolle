//! speichern und laden Methode für Gleise

use log::error;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        pin::pwm,
        speichern_laden::{self, Reserviere, Reserviert, Serialisiere},
        Anschlüsse, Fließend, InputAnschluss, OutputAnschluss,
    },
    application::{
        anchor,
        gleis::gleise::{maps::*, Error, Gleise},
        typen::*,
    },
    steuerung::geschwindigkeit::{self, Geschwindigkeit, Leiter},
};

impl<Z: Zugtyp + Serialize> Gleise<Z> {
    #[must_use]
    pub fn speichern(
        &self,
        pfad: impl AsRef<std::path::Path>,
        geschwindigkeiten: geschwindigkeit::Map<<Z::Leiter as Serialisiere>::Serialisiert>,
    ) -> std::result::Result<(), Error> {
        let Gleise { maps, .. } = self;
        let vecs: GleiseVecs<Z> = (maps, geschwindigkeiten).into();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &vecs)?;
        Ok(())
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
    anschluss::Error,
> {
    source.into_iter().fold(
        Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        |acc_res: Result<_, anschluss::Error>, gleis_save| {
            let mut acc = acc_res?;
            let Reserviert {
                anschluss: gleis,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            } = gleis_save
                .reserviere(anschlüsse, acc.1, acc.2, acc.3)
                .map_err(|speichern_laden::Error { fehler, .. }| fehler)?;
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
    #[must_use]
    pub fn laden(
        &mut self,
        anschlüsse: &mut Anschlüsse,
        bisherige_geschwindigkeiten: impl Iterator<Item = Geschwindigkeit<Z::Leiter>>,
        pfad: impl AsRef<std::path::Path>,
    ) -> std::result::Result<Vec<(geschwindigkeit::Name, Geschwindigkeit<Z::Leiter>)>, Error> {
        // aktuellen Zustand zurücksetzen
        self.canvas.clear();
        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen
        self.maps = GleiseMaps::neu();
        self.anchor_points = anchor::rstar::RTree::new();

        // lese & parse Datei
        let file = std::fs::File::open(pfad)?;
        let GleiseVecs {
            name,
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
        } = bincode::deserialize_from(file)?;
        if name != Z::NAME {
            return Err(Error::FalscherZugtyp(name));
        }

        // sammle bisherige Anschlüsse
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        macro_rules! fold_anschlüsse {
            ($iterator:expr) => {
                for struktur in $iterator {
                    let (pwm, output, input) = struktur.anschlüsse();
                    pwm_pins.extend(pwm.into_iter());
                    output_anschlüsse.extend(output.into_iter());
                    input_anschlüsse.extend(input.into_iter());
                }
            };
        }
        macro_rules! fold_gleis_anschlüsse {
            ($map:ident) => {
                fold_anschlüsse! {
                    self.maps.$map.drain().map(|(_id, Gleis { definition, .. })| definition)
                }
            };
        }
        fold_anschlüsse! {bisherige_geschwindigkeiten.map(|mut geschwindigkeit| {
            if let Err(error) = geschwindigkeit.geschwindigkeit(0){
                error!("Fehler beim Geschwindigkeit ausstellen: {:?}", error)
            }
            geschwindigkeit
        })}
        fold_gleis_anschlüsse! {geraden}
        fold_gleis_anschlüsse! {kurven}
        fold_gleis_anschlüsse! {weichen}
        fold_gleis_anschlüsse! {dreiwege_weichen}
        fold_gleis_anschlüsse! {kurven_weichen}
        fold_gleis_anschlüsse! {s_kurven_weichen}
        fold_gleis_anschlüsse! {kreuzungen}
        fold_anschlüsse! {
            self.maps.streckenabschnitte.drain().map(
                |(_id, (mut streckenabschnitt, _fließend))| {
                    if let Err(error) = streckenabschnitt.strom(Fließend::Gesperrt) {
                        error!("Fehler beim Streckenabschnitt ausstellen: {:?}", error)
                    }
                    streckenabschnitt
                }
            )
        }

        // reserviere Anschlüsse
        macro_rules! reserviere_anschlüsse {
            (
                $name:tt::<$type:ident>,
                $pwm_pins:tt,
                $output_anschlüsse:tt,
                $input_anschlüsse:tt$(,)?
            ) => {
                // collect to Vec to fail on first error
                let ($name, $pwm_pins, $output_anschlüsse, $input_anschlüsse) =
                    reserviere_anschlüsse::<crate::application::gleis::$type<Z>>(
                        anschlüsse,
                        $name,
                        $pwm_pins,
                        $output_anschlüsse,
                        $input_anschlüsse,
                    )?;
            };
        }
        reserviere_anschlüsse! {
            geraden::<Gerade>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            kurven::<Kurve>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            weichen::<Weiche>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            dreiwege_weichen::<DreiwegeWeiche>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            kurven_weichen::<KurvenWeiche>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            s_kurven_weichen::<SKurvenWeiche>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        reserviere_anschlüsse! {
            kreuzungen::<Kreuzung>,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        }
        let (streckenabschnitte_reserviert, pwm_pins, output_anschlüsse, input_anschlüsse) =
            streckenabschnitte.into_iter().fold(
                Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
                |acc_res: Result<_, anschluss::Error>, (name, streckenabschnitt)| {
                    let mut acc = acc_res?;
                    let Reserviert {
                        anschluss: streckenabschnitt,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = streckenabschnitt
                        .reserviere(anschlüsse, acc.1, acc.2, acc.3)
                        .map_err(|speichern_laden::Error { fehler, .. }| fehler)?;
                    acc.0.push((name, streckenabschnitt));
                    Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
                },
            )?;
        let (geschwindigkeiten_reserviert, _pwm_pins, _output_anschlüsse, _input_anschlüsse) =
            geschwindigkeiten.into_iter().fold(
                Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
                |acc_res: Result<_, anschluss::Error>, (name, geschwindigkeit)| {
                    let mut acc = acc_res?;
                    let Reserviert {
                        anschluss: geschwindigkeit,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = geschwindigkeit
                        .reserviere(anschlüsse, acc.1, acc.2, acc.3)
                        .map_err(|speichern_laden::Error { fehler, .. }| fehler)?;
                    acc.0.push((name, geschwindigkeit));
                    Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
                },
            )?;
        // füge anschlüsse zu maps hinzu
        macro_rules! add_gleise {
            ($($gleise: ident,)*) => {
                $(
                    for gleis in $gleise {
                        self.add(gleis);
                    }
                );*
            }
        }
        add_gleise!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );
        for (name, streckenabschnitt) in streckenabschnitte_reserviert {
            self.neuer_streckenabschnitt(name, streckenabschnitt);
        }
        Ok(geschwindigkeiten_reserviert)
    }
}
