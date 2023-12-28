//! speichern und laden Methode für [Gleise].

use std::{
    collections::HashMap,
    fmt::Debug,
    fs,
    hash::Hash,
    io::{self, Read},
    marker::PhantomData,
    sync::mpsc::Sender,
};

use bincode::config::{
    DefaultOptions, FixintEncoding, Options, RejectTrailing, WithOtherIntEncoding,
    WithOtherTrailing,
};
use nonempty::NonEmpty;
use once_cell::sync::Lazy;
use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTree,
};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        pin::input,
        OutputSerialisiert,
    },
    gleis::{
        gerade::{Gerade, GeradeSerialisiert},
        gleise::{
            self,
            daten::{
                v2::{self, BekannterZugtyp},
                v3::{self, kreuzung, Gleis},
                v4::{
                    GeschwindigkeitMapSerialisiert, GleisSerialisiert, GleiseDatenSerialisiert,
                    StreckenabschnittMapSerialisiert, ZugtypSerialisiert2, ZustandSerialisiert,
                },
                GeschwindigkeitMap2, Gleis2, GleisMap2, GleiseDaten2, RStern2, SelectAll,
                StreckenabschnittMap2, Zustand2,
            },
            id::{
                self, eindeutig::KeineIdVerfügbar, AnyDefinitionId2, AnyGleisDefinitionId2,
                DefinitionId2, GleisId2,
            },
            nachricht::GleisSteuerung,
            steuerung::{MitSteuerung, SomeAktualisierenSender},
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
    typen::{canvas::Position, mm::Spurweite, vektor::Vektor, Zeichnen},
    zugtyp::{DefinitionMap2, Zugtyp2},
};

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
        /// Fehler beim Deserialisieren nach Version-3 Speicherformat.
        v3: bincode::Error,
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

impl<S> From<input::Fehler> for LadenFehler<S> {
    fn from(fehler: input::Fehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<anschluss::pcf8574::Fehler> for LadenFehler<S> {
    fn from(fehler: anschluss::pcf8574::Fehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<anschluss::pin::ReservierenFehler> for LadenFehler<S> {
    fn from(fehler: anschluss::pin::ReservierenFehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<ZugtypDeserialisierenFehler> for LadenFehler<S> {
    fn from(fehler: ZugtypDeserialisierenFehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<KeineIdVerfügbar> for LadenFehler<S> {
    fn from(fehler: KeineIdVerfügbar) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl GleiseDaten2 {
    /// Erzeuge eine Serialisierbare Repräsentation
    fn serialisiere(&self) -> GleiseDatenSerialisiert {
        macro_rules! konvertiere_maps {
            ($($map:ident),* $(,)?) => {
                GleiseDatenSerialisiert {
                    $($map: self.$map.iter().map(
                        |(id, (gleis, _rectangle))| (
                            id.repräsentation(),
                            GleisSerialisiert {
                                definition: gleis.definition.repräsentation(),
                                steuerung: gleis.steuerung.serialisiere(),
                                position: gleis.position.clone(),
                                streckenabschnitt: gleis.streckenabschnitt.clone(),
                            }
                        ))
                        .collect()
                    ),*
                }
            };
        }
        konvertiere_maps! {
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

#[must_use]
fn reserviere_anschlüsse<T, Ts, L>(
    lager: &mut anschluss::Lager,
    serialisiert: impl IntoIterator<Item = (id::Repräsentation, GleisSerialisiert<T>)>,
    spurweite: Spurweite,
    definitionen: &DefinitionMap2<T>,
    anschlüsse: Anschlüsse,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    bekannte_ids: &mut HashMap<id::Repräsentation, GleisId2<T>>,
    bekannte_definition_ids: &HashMap<id::Repräsentation, DefinitionId2<T>>,
    arg: &<Ts as Reserviere<<T as MitSteuerung>::Steuerung>>::MoveArg,
) -> (
    GleisMap2<T>,
    Vec<GeomWithData<Rectangle<Vektor>, (AnyGleisDefinitionId2, Position)>>,
    Anschlüsse,
)
where
    T: 'static + MitSteuerung<Serialisiert = Ts>,
    Ts: Reserviere<<T as MitSteuerung>::Steuerung, RefArg = (), MutRefArg = ()>,
    <Ts as Reserviere<<T as MitSteuerung>::Steuerung>>::MoveArg: Clone,
    <T as MitSteuerung>::SelfUnit: Zeichnen<()>,
    AnyDefinitionId2: From<DefinitionId2<T>>,
    AnyGleisDefinitionId2: From<(GleisId2<T>, DefinitionId2<T>)>,
{
    use Ergebnis::*;
    serialisiert.into_iter().fold(
        (GleisMap2::new(), Vec::new(), anschlüsse),
        |(mut gleise, mut rstern_elemente, anschlüsse), (gespeicherte_id, gleis_serialisiert)| {
            let id = match bekannte_ids.get(&gespeicherte_id) {
                Some(id) => id.clone(),
                None => match GleisId2::neu() {
                    Ok(id) => id,
                    Err(fehler) => {
                        laden_fehler.push(fehler.into());
                        return (gleise, rstern_elemente, anschlüsse);
                    },
                },
            };
            let (gleis, anschlüsse) = match gleis_serialisiert.reserviere(
                lager,
                anschlüsse,
                arg.clone(),
                bekannte_definition_ids,
                &mut (),
            ) {
                Wert { anschluss, anschlüsse } => (anschluss, anschlüsse),
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
                    (anschluss, anschlüsse)
                },
                Fehler { fehler, anschlüsse } => {
                    laden_fehler.extend(fehler.into_iter().map(LadenFehler::from));
                    return (gleise, rstern_elemente, anschlüsse);
                },
            };
            let Some(definition) = definitionen.get(&gleis.definition) else {
                laden_fehler.push(LadenFehler::from(anschluss::Fehler::UnbekannteDefinition {
                    id: gleis.definition.into(),
                }));
                return (gleise, rstern_elemente, anschlüsse);
            };
            let rectangle = Rectangle::from(definition.rechteck(&(), spurweite));
            let _ = bekannte_ids.insert(gespeicherte_id, id.clone());
            rstern_elemente.push(GeomWithData::new(
                rectangle,
                (
                    AnyGleisDefinitionId2::from((id.clone(), gleis.definition.clone())),
                    gleis.position.clone(),
                ),
            ));
            let _ = gleise.insert(id, (gleis, rectangle));
            (gleise, rstern_elemente, anschlüsse)
        },
    )
}

/// Mapping von der Zahl aus der serialisierten Darstellung zur [DefinitionId].
#[derive(Debug)]
pub struct DefinitionIdMaps {
    geraden: HashMap<u32, DefinitionId2<Gerade>>,
    kurven: HashMap<u32, DefinitionId2<Kurve>>,
    weichen: HashMap<u32, DefinitionId2<Weiche>>,
    dreiwege_weichen: HashMap<u32, DefinitionId2<DreiwegeWeiche>>,
    kurven_weichen: HashMap<u32, DefinitionId2<KurvenWeiche>>,
    s_kurven_weichen: HashMap<u32, DefinitionId2<SKurvenWeiche>>,
    kreuzungen: HashMap<u32, DefinitionId2<Kreuzung>>,
}

impl DefinitionIdMaps {
    /// Erzeuge eine neue, leere [DefinitionIdMaps].
    pub fn neu() -> DefinitionIdMaps {
        DefinitionIdMaps {
            geraden: HashMap::new(),
            kurven: HashMap::new(),
            weichen: HashMap::new(),
            dreiwege_weichen: HashMap::new(),
            kurven_weichen: HashMap::new(),
            s_kurven_weichen: HashMap::new(),
            kreuzungen: HashMap::new(),
        }
    }
}

/// Mapping von der Zahl aus der serialisierten Darstellung zur [GleisId].
#[derive(Debug)]
pub struct IdMaps {
    geraden: HashMap<u32, GleisId2<Gerade>>,
    kurven: HashMap<u32, GleisId2<Kurve>>,
    weichen: HashMap<u32, GleisId2<Weiche>>,
    dreiwege_weichen: HashMap<u32, GleisId2<DreiwegeWeiche>>,
    kurven_weichen: HashMap<u32, GleisId2<KurvenWeiche>>,
    s_kurven_weichen: HashMap<u32, GleisId2<SKurvenWeiche>>,
    kreuzungen: HashMap<u32, GleisId2<Kreuzung>>,
    definitionen: DefinitionIdMaps,
}

impl IdMaps {
    /// Erzeuge eine neue, leere [IdMaps].
    pub fn neu() -> IdMaps {
        IdMaps {
            geraden: HashMap::new(),
            kurven: HashMap::new(),
            weichen: HashMap::new(),
            dreiwege_weichen: HashMap::new(),
            kurven_weichen: HashMap::new(),
            s_kurven_weichen: HashMap::new(),
            kreuzungen: HashMap::new(),
            definitionen: DefinitionIdMaps::neu(),
        }
    }
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    #[must_use]
    fn reserviere<L, S, Nachricht>(
        self,
        zugtyp: &Zugtyp2<L>,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        bekannte_ids: &mut IdMaps,
        laden_fehler: &mut Vec<LadenFehler<S>>,
        sender: &Sender<Nachricht>,
    ) -> (GleiseDaten2, Anschlüsse)
    where
        L: Leiter,
        Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let GleiseDatenSerialisiert {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = self;
        let aktualisieren_sender = SomeAktualisierenSender::from((sender.clone(), Nachricht::from));

        let mut rstern_elemente = Vec::new();

        macro_rules! reserviere_anschlüsse {
            ($anschlüsse: ident => $($gleise: ident),* $(,)?) => {$(
                let ($gleise, neue_rstern_elemente, $anschlüsse) = reserviere_anschlüsse(
                    lager,
                    $gleise,
                    zugtyp.spurweite,
                    &zugtyp.$gleise,
                    $anschlüsse,
                    laden_fehler,
                    &mut bekannte_ids.$gleise,
                    &bekannte_ids.definitionen.$gleise,
                    &aktualisieren_sender,
                );
                rstern_elemente.extend(neue_rstern_elemente);
            )*};
        }

        reserviere_anschlüsse!(
            anschlüsse =>
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );

        let daten = GleiseDaten2 {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            rstern: RStern2::bulk_load(rstern_elemente),
        };

        (daten, anschlüsse)
    }
}

impl<L: Leiter> Zustand2<L> {
    /// Erzeuge eine Serialisierbare Repräsentation.
    pub(in crate::gleis::gleise) fn serialisiere<S>(&self) -> ZustandSerialisiert<L, S>
    where
        L: Serialisiere<S> + BekannterLeiter,
        <L as Leiter>::Fahrtrichtung: Clone,
    {
        let Zustand2 { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne } = self;
        macro_rules! serialisiere_head_clone_tail {
            ($head: ident $(, $tail: ident)* $(,)?) => {
                ($head.serialisiere() $(, $tail.clone())*)
            };
        }
        macro_rules! serialisiere_maps {
            ($(($($matching: ident),*): $map: ident - $serialize_id: ident),* $(,)?) => {$(
                #[allow(unused_parens)]
                let $map = $map
                    .iter()
                    .map(|(id, ($($matching),*))| (id.$serialize_id(), serialisiere_head_clone_tail!($($matching),*)))
                    .collect();
            )*};
        }
        serialisiere_maps!(
            (geschwindigkeit): geschwindigkeiten - clone,
            (streckenabschnitt, geschwindigkeit): streckenabschnitte - clone,
            (plan): pläne - clone,
        );
        ZustandSerialisiert {
            zugtyp: zugtyp.serialisiere(),
            geschwindigkeiten,
            streckenabschnitte,
            gleise: gleise.serialisiere(),
            pläne,
        }
    }

    fn anschlüsse_ausgeben<S>(&mut self) -> Anschlüsse
    where
        L: Serialisiere<S>,
    {
        let Zustand2 { zugtyp: _, geschwindigkeiten, streckenabschnitte, gleise, pläne: _ } = self;
        let GleiseDaten2 {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            rstern: _,
        } = gleise;
        let mut anschlüsse = Anschlüsse::default();
        macro_rules! head {
            ($head: ident $(, $tail: ident)* $(,)?) => {
                $head
            };
        }
        macro_rules! collect_anschlüsse {
            (($($matching: ident),+) : $map: ident) => {
                #[allow(unused_parens)]
                for (_id, ($($matching),+)) in $map.drain() {
                    anschlüsse.anhängen(head!($($matching),+).anschlüsse());
                }
            };
        }
        collect_anschlüsse!((geschwindigkeit): geschwindigkeiten);
        collect_anschlüsse!((streckenabschnitt, _geschwindigkeit): streckenabschnitte);
        collect_anschlüsse!((gerade, _streckenabschnitt): geraden);
        collect_anschlüsse!((kurve, _streckenabschnitt): kurven);
        collect_anschlüsse!((weiche, _streckenabschnitt): weichen);
        collect_anschlüsse!((dreiwege_weiche, _streckenabschnitt): dreiwege_weichen);
        collect_anschlüsse!((kurven_weiche, _streckenabschnitt): kurven_weichen);
        collect_anschlüsse!((s_kurven_weiche, _streckenabschnitt): s_kurven_weichen);
        collect_anschlüsse!((kreuzung, _streckenabschnitt): kreuzungen);
        anschlüsse
    }
}

/// Der Leiter stimmt nicht mit dem Namen überein.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum ZugtypDeserialisierenFehler {
    FalscherLeiter(String),
    KeineIdVerfügbar(KeineIdVerfügbar),
}

macro_rules! erzeuge_zugtyp_maps2 {
    ($id_maps: expr => $($gleise: ident : $typ: ty),* $(,)?) => {
        $(
        #[allow(unused_qualifications)]
        let ($gleise, ids) = $gleise
            .into_iter()
            .fold(
                Ok((HashMap::new(), HashMap::new())),
                |acc, (gespeicherte_id, definition)| -> Result<_, ZugtypDeserialisierenFehler> {
                    if let Ok((mut gleise, mut ids)) = acc {
                        let id = crate::gleis::gleise::id::DefinitionId2::<$typ>::neu()?;
                        // gespeicherte_id ist eindeutig, da es der Schlüssel einer HashMap war
                        let _ = ids.insert(gespeicherte_id, id.clone());
                        // id ist eindeutig, da es von GleisId::neu garantiert wird
                        let _ = gleise.insert(id, definition);
                        Ok((gleise, ids))
                    } else {
                        acc
                    }
                }
            )?;
        $id_maps.$gleise = ids;
        )*
    };
    ($($gleise: ident : $typ: ty | $expect_msg: literal),* $(,)?) => {$(
        #[allow(unused_qualifications)]
        let $gleise = $gleise
            .into_iter()
            .map(|definition| Ok((crate::gleis::gleise::id::DefinitionId2::<$typ>::neu()?, definition)) )
            .collect::<
                Result<
                    crate::zugtyp::DefinitionMap2<$typ>,
                    crate::gleis::gleise::daten::de_serialisieren::ZugtypDeserialisierenFehler
                >
            >().expect($expect_msg);
    )*};
}
pub(crate) use erzeuge_zugtyp_maps2;

impl<L: BekannterLeiter> ZugtypSerialisiert2<L> {
    #[must_use]
    fn reserviere(self) -> Result<(Zugtyp2<L>, DefinitionIdMaps), ZugtypDeserialisierenFehler> {
        let ZugtypSerialisiert2 {
            name,
            leiter,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        } = self;
        let gesucht = L::NAME.to_owned();
        if leiter != gesucht {
            return Err(ZugtypDeserialisierenFehler::FalscherLeiter(leiter));
        }

        let mut bekannte_ids = DefinitionIdMaps::neu();
        erzeuge_zugtyp_maps2!(
            bekannte_ids =>
            geraden: Gerade,
            kurven: Kurve,
            weichen: Weiche,
            dreiwege_weichen: DreiwegeWeiche,
            kurven_weichen: KurvenWeiche,
            s_kurven_weichen: SKurvenWeiche,
            kreuzungen: Kreuzung,
        );

        let zugtyp = Zugtyp2 {
            name,
            leiter: PhantomData,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        };

        Ok((zugtyp, bekannte_ids))
    }
}

impl<L: BekannterLeiter> Zugtyp2<L> {
    fn serialisiere(&self) -> ZugtypSerialisiert2<L> {
        let Zugtyp2 {
            name,
            leiter: PhantomData,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        } = self;
        let leiter = L::NAME.to_owned();
        macro_rules! erzeuge_zugtyp_maps {
            ($($gleise: ident),* $(,)?) => {$(
                let $gleise = $gleise.into_iter().map(|(id, gleis)| (id.repräsentation(), gleis.clone())).collect();
            )*};
        }
        erzeuge_zugtyp_maps!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );
        ZugtypSerialisiert2 {
            name: name.clone(),
            leiter,
            spurweite: *spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz: *pwm_frequenz,
            verhältnis_fahrspannung_überspannung: verhältnis_fahrspannung_überspannung.clone(),
            stopp_zeit: *stopp_zeit,
            umdrehen_zeit: umdrehen_zeit.clone(),
            schalten_zeit: *schalten_zeit,
        }
    }
}

impl<L, S> ZustandSerialisiert<L, S>
where
    L: BekannterLeiter,
    S: Clone + Eq + Hash + Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>,
{
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere<Nachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send>(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        sender: &Sender<Nachricht>,
    ) -> Result<(Zustand2<L>, Vec<LadenFehler<S>>), ZugtypDeserialisierenFehler> {
        let ZustandSerialisiert { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne } =
            self;

        let (zugtyp, bekannte_definition_ids) = zugtyp.reserviere()?;

        let mut bekannte_ids = IdMaps { definitionen: bekannte_definition_ids, ..IdMaps::neu() };

        let mut laden_fehler: Vec<LadenFehler<S>> = Vec::new();

        macro_rules! reserviere_maps {
            ($anschlüsse: ident => $($elemente: ident $(, $extra_info: ident)?);* $(;)? ) => {$(
                #[allow(unused_parens)]
                let ($elemente, $anschlüsse) = $elemente.into_iter().fold(
                    (HashMap::new(), $anschlüsse),
                    |(mut elemente, anschlüsse), (name, (serialisiert $(, $extra_info)?))| {
                        let ergebnis = serialisiert.reserviere(lager, anschlüsse, (), &(), &mut ());
                        let anschlüsse = match ergebnis {
                            Ergebnis::Wert { anschluss, anschlüsse } => {
                                // Name ist eindeutig, da die serialisierte Struktur ebenfalls eine HashMap war.
                                let _ = elemente.insert(name, (anschluss $(, $extra_info.clone())?));
                                anschlüsse
                            },
                            Ergebnis::FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                                // Name ist eindeutig, da die serialisierte Struktur ebenfalls eine HashMap war.
                                let _ = elemente.insert(name, (anschluss $(, $extra_info.clone())?));
                                laden_fehler.extend(fehler.map(LadenFehler::from));
                                anschlüsse
                            },
                            Ergebnis::Fehler { fehler, anschlüsse } => {
                                // Name ist eindeutig, da die serialisierte Struktur ebenfalls eine HashMap war.
                                laden_fehler.extend(fehler.map(LadenFehler::from));
                                anschlüsse
                            },
                        };
                        (elemente, anschlüsse)
                    },
                );
            )*};
        }

        reserviere_maps!(
            anschlüsse =>
            geschwindigkeiten;
            streckenabschnitte, geschwindigkeit;
        );
        let (gleise, anschlüsse) = gleise.reserviere(
            &zugtyp,
            lager,
            anschlüsse,
            &mut bekannte_ids,
            &mut laden_fehler,
            sender,
        );
        // let (pläne, anschlüsse) = reserviere_map(lager, pläne, anschlüsse, &mut laden_fehler);
        let pläne = todo!("Pläne reservieren/deserialisieren");
        let _ = ();

        let zustand = Zustand2 { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne };
        Ok((zustand, laden_fehler))
    }
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Speicher alle Gleise, [Streckenabschnitte](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [Zugtyp]
    /// in einer Datei.
    #[allow(single_use_lifetimes)]
    pub fn speichern<S>(&self, pfad: impl AsRef<std::path::Path>) -> Result<(), Fehler>
    where
        L: Serialisiere<S> + BekannterLeiter,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
        <L as Leiter>::UmdrehenZeit: Serialize,
        <L as Leiter>::Fahrtrichtung: Clone + Serialize,
        S: Serialize,
    {
        let serialisiert: ZustandSerialisiert<L, S> = self.zustand2.serialisiere();
        let file = fs::File::create(pfad)?;
        BINCODE_OPTIONS.serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }

    /// Lade Gleise, [Streckenabschnitte](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [Zugtyp]
    /// aus einer Datei.
    #[allow(single_use_lifetimes)]
    pub fn laden<S>(
        &mut self,
        lager: &mut anschluss::Lager,
        pfad: impl AsRef<std::path::Path>,
    ) -> Result<(), NonEmpty<LadenFehler<S>>>
    where
        L: BekannterLeiter + Serialisiere<S>,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: for<'de> Deserialize<'de>,
        <L as Leiter>::UmdrehenZeit: for<'de> Deserialize<'de>,
        <L as Leiter>::Fahrtrichtung: for<'de> Deserialize<'de>,
        S: Clone
            + Eq
            + Hash
            + Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>
            + for<'de> Deserialize<'de>,
        // zusätzliche Constraints für v2-Kompatibilität
        L: BekannterZugtyp,
        S: From<<L as BekannterZugtyp>::V2>,
        <L as BekannterZugtyp>::V2: for<'de> Deserialize<'de>,
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.erzwinge_neuzeichnen();
        let anschlüsse = self.zustand2.anschlüsse_ausgeben();

        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // lese & parse Datei
        let mut file = fs::File::open(pfad).map_err(|fehler| NonEmpty::singleton(fehler.into()))?;
        let mut content = Vec::new();
        let _ =
            file.read_to_end(&mut content).map_err(|fehler| NonEmpty::singleton(fehler.into()))?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<L, S> = BINCODE_OPTIONS
            .deserialize(slice)
            .or_else(|aktuell| {
                match BINCODE_OPTIONS.deserialize::<v3::ZustandSerialisiert<L, S>>(slice) {
                    Ok(v3) => todo!("v3.try_into().map_err(LadenFehler::from)"),
                    Err(v3) => {
                        match BINCODE_OPTIONS
                            .deserialize::<v2::GleiseVecs<<L as BekannterZugtyp>::V2>>(slice)
                        {
                            Ok(v2) => todo!("v2.try_into().map_err(LadenFehler::from)"),
                            Err(v2) => Err(LadenFehler::BincodeDeserialisieren { aktuell, v3, v2 }),
                        }
                    },
                }
            })
            .map_err(|fehler| NonEmpty::singleton(fehler.into()))?;

        // reserviere Anschlüsse
        let (zustand, fehler) = zustand_serialisiert
            .reserviere(lager, anschlüsse, &self.sender)
            .map_err(|fehler| NonEmpty::singleton(LadenFehler::from(fehler)))?;
        self.zustand2 = zustand;
        if let Some(non_empty) = NonEmpty::from_vec(fehler) {
            Err(non_empty)
        } else {
            Ok(())
        }
    }
}
