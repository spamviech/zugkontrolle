//! speichern und laden Methode für [`Gleise`].

use std::{
    collections::HashMap, fs, hash::Hash, io, marker::PhantomData, path::Path, sync::mpsc::Sender,
};

use bincode::config::{
    DefaultOptions, FixintEncoding, Options, RejectTrailing, WithOtherIntEncoding,
    WithOtherTrailing,
};
use nonempty::NonEmpty;
use once_cell::sync::Lazy;
use rstar::primitives::{GeomWithData, Rectangle};
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    pcf8574,
    pin::{self, input},
    Lager,
};
use zugkontrolle_gleis::{
    gerade::Gerade,
    id::{AnyDefinitionId, AnyGleisDefinitionId, DefinitionId},
    kreuzung::Kreuzung,
    kurve::Kurve,
    steuerung::{
        aktualisieren::{Aktualisieren, MitSteuerung, SomeAktualisierenSender},
        geschwindigkeit::{BekannterLeiter, Leiter},
        plan::{self, SteuerungMaps, UnbekannteAnschlüsse},
    },
    weiche::{
        dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
    },
    zugtyp::{DefinitionMap, Zugtyp},
};
use zugkontrolle_id::{eindeutig::KeineIdVerfügbar, GleisId};
use zugkontrolle_typen::{mm::Spurweite, Zeichnen};

use crate::{
    daten::{
        v2::{self, geschwindigkeit::BekannterZugtyp},
        v3::{self},
        v4::{GleisSerialisiert, GleiseDatenSerialisiert, ZugtypSerialisiert, ZustandSerialisiert},
        GleisMap, GleiseDaten, RStern, RSternEintrag, Zustand,
    },
    Fehler, Gleise,
};

/// [`bincode`]-Optionen, bei denen trailing bytes abgelehnt werden.
///
/// Im Gegensatz zu [`DefaultOptions`] verwendet [die Standard-Funktion](bincode::deserialize) fixint-encoding.
/// <https://docs.rs/bincode/latest/bincode/config/index.html#options-struct-vs-bincode-functions>
static BINCODE_OPTIONS: Lazy<
    WithOtherTrailing<WithOtherIntEncoding<DefaultOptions, FixintEncoding>, RejectTrailing>,
> = Lazy::new(|| DefaultOptions::new().with_fixint_encoding().reject_trailing_bytes());

/// Fehler der beim [`Laden`](Gleise::laden) auftreten kann.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum LadenFehler<S> {
    /// Ein IO-Fehler.
    IO(io::Error),
    /// Fehler beim reservieren eines [`Anschlusses`](anschluss::Anschluss).
    Anschluss(zugkontrolle_anschluss::Fehler),
    /// Der Leiter stimmt nicht mit dem Namen überein.
    ZugtypDeserialisieren(ZugtypDeserialisierenFehler),
    /// Fehler beim Deserialisieren (laden) gespeicherter Daten.
    BincodeDeserialisieren {
        /// Fehler beim Deserialisieren nach aktuellem Speicherformat.
        aktuell: bincode::Error,
        /// Fehler beim Deserialisieren nach Version-3 Speicherformat.
        v3: bincode::Error,
        /// Fehler beim Deserialisieren nach Version-2 Speicherformat.
        v2: bincode::Error,
    },
    /// Unbekannte Anschlüsse sollen in einem [`Plan`](plan::Plan) verwendet werden.
    UnbekannteAnschlüsse {
        /// Der Name des Plans.
        plan: plan::Name,
        /// Die unbekannten Anschlüsse.
        anschlüsse: UnbekannteAnschlüsse<S>,
    },
    /// Ein Gleis mit unbekannter [`DefinitionId`].
    UnbekannteDefinition {
        /// Die Id ohne zugehörigen Eintrag im [`Zugtyp`].
        id: AnyDefinitionId,
    },
    /// Unbekannter Zugtyp beim Laden von v2-Speicherdaten.
    UnbekannterZugtyp {
        /// Der gespeicherte Zugtyp.
        zugtyp: String,
        /// Der Name des aktuellen Leiters.
        leiter: &'static str,
    },
}

impl<S> From<input::Fehler> for LadenFehler<S> {
    fn from(fehler: input::Fehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<pcf8574::Fehler> for LadenFehler<S> {
    fn from(fehler: pcf8574::Fehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl<S> From<pin::ReservierenFehler> for LadenFehler<S> {
    fn from(fehler: pin::ReservierenFehler) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}
impl<S> From<KeineIdVerfügbar> for LadenFehler<S> {
    fn from(fehler: KeineIdVerfügbar) -> Self {
        LadenFehler::Anschluss(fehler.into())
    }
}

impl GleiseDaten {
    /// Erzeuge eine Serialisierbare Repräsentation
    fn serialisiere(&self) -> GleiseDatenSerialisiert {
        /// Konvertiere die [`GleisMaps`](GleisMap) in ihre serialisierbare Repräsentation.
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

// Alle Argumente benötigt.
#[allow(clippy::too_many_arguments)]
/// Reserviere die Anschlüsse für alle Gleise.
#[must_use]
fn reserviere_anschlüsse<T, S, Ss, L>(
    lager: &mut Lager,
    serialisiert: impl IntoIterator<Item = (zugkontrolle_id::Repräsentation, GleisSerialisiert<T>)>,
    spurweite: Spurweite,
    definitionen: &DefinitionMap<T>,
    anschlüsse: Anschlüsse,
    bekannte_steuerungen: &mut HashMap<Ss, S>,
    laden_fehler: &mut Vec<LadenFehler<L>>,
    bekannte_ids: &mut HashMap<zugkontrolle_id::Repräsentation, GleisId<T>>,
    bekannte_definition_ids: &HashMap<zugkontrolle_id::Repräsentation, DefinitionId<T>>,
    arg: &<Ss as Reserviere<S>>::MoveArg,
) -> (GleisMap<T>, Vec<RSternEintrag>, Anschlüsse)
where
    T: 'static + MitSteuerung<Steuerung = Option<S>, Serialisiert = Option<Ss>>,
    S: Clone + Serialisiere<Ss>,
    Ss: Eq + Hash + Reserviere<S, RefArg = (), MutRefArg = ()>,
    <Ss as Reserviere<S>>::MoveArg: Clone,
    <T as MitSteuerung>::SelfUnit: Zeichnen<()>,
    AnyDefinitionId: From<DefinitionId<T>>,
    AnyGleisDefinitionId: From<(GleisId<T>, DefinitionId<T>)>,
{
    use Ergebnis::{Fehler, FehlerMitErsatzwert, Wert};
    serialisiert.into_iter().fold(
        (GleisMap::new(), Vec::new(), anschlüsse),
        // `anschlüsse` über Argument->Rückgabewert zusammenhängend.
        #[allow(clippy::shadow_unrelated)]
        |(mut gleise, mut rstern_elemente, anschlüsse), (gespeicherte_id, gleis_serialisiert)| {
            let id = match bekannte_ids.get(&gespeicherte_id) {
                Some(id) => id.clone(),
                None => match GleisId::neu() {
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
                laden_fehler
                    .push(LadenFehler::UnbekannteDefinition { id: gleis.definition.into() });
                return (gleise, rstern_elemente, anschlüsse);
            };
            // Bekannte Steuerung sichern
            if let Some(steuerung) = &gleis.steuerung {
                let _ = bekannte_steuerungen.insert(steuerung.serialisiere(), steuerung.clone());
            }
            // Bekannte Id sichern
            let _ = bekannte_ids.insert(gespeicherte_id, id.clone());
            // Rstern-Elemente (rectangle, id, position) sichern
            let rectangle =
                Rectangle::from(definition.rechteck_an_position(&(), spurweite, &gleis.position));
            rstern_elemente.push(GeomWithData::new(
                rectangle,
                (
                    AnyGleisDefinitionId::from((id.clone(), gleis.definition.clone())),
                    gleis.position.clone(),
                ),
            ));
            // Akkumulator aktualisieren
            let _ = gleise.insert(id, (gleis, rectangle));

            (gleise, rstern_elemente, anschlüsse)
        },
    )
}

/// Mapping von der Zahl aus der serialisierten Darstellung zur [`DefinitionId`].
#[derive(Debug)]
pub(crate) struct DefinitionIdMaps {
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: HashMap<u32, DefinitionId<Gerade>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: HashMap<u32, DefinitionId<Kurve>>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: HashMap<u32, DefinitionId<Weiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: HashMap<u32, DefinitionId<DreiwegeWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: HashMap<u32, DefinitionId<KurvenWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: HashMap<u32, DefinitionId<SKurvenWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: HashMap<u32, DefinitionId<Kreuzung>>,
}

impl DefinitionIdMaps {
    /// Erzeuge eine neue, leere [`DefinitionIdMaps`].
    pub(crate) fn neu() -> DefinitionIdMaps {
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

/// Mapping von der Zahl aus der serialisierten Darstellung zur [`GleisId`].
#[derive(Debug)]
pub(crate) struct IdMaps {
    #[allow(clippy::missing_docs_in_private_items)]
    geraden: HashMap<u32, GleisId<Gerade>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven: HashMap<u32, GleisId<Kurve>>,
    #[allow(clippy::missing_docs_in_private_items)]
    weichen: HashMap<u32, GleisId<Weiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    dreiwege_weichen: HashMap<u32, GleisId<DreiwegeWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kurven_weichen: HashMap<u32, GleisId<KurvenWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    s_kurven_weichen: HashMap<u32, GleisId<SKurvenWeiche>>,
    #[allow(clippy::missing_docs_in_private_items)]
    kreuzungen: HashMap<u32, GleisId<Kreuzung>>,
    #[allow(clippy::missing_docs_in_private_items)]
    definitionen: DefinitionIdMaps,
}

impl IdMaps {
    /// Erzeuge eine neue, leere [`IdMaps`].
    pub(crate) fn neu() -> IdMaps {
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
    #[allow(clippy::too_many_arguments)]
    /// Reserviere alle benötigten Anschlüsse.
    #[must_use]
    fn reserviere<L, S, Nachricht>(
        self,
        zugtyp: &Zugtyp<L>,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        bekannte_ids: &mut IdMaps,
        bekannte_steuerungen: &mut SteuerungMaps<L, S>,
        laden_fehler: &mut Vec<LadenFehler<S>>,
        sender: &Sender<Nachricht>,
    ) -> (GleiseDaten, Anschlüsse)
    where
        L: Leiter,
        Nachricht: 'static + From<Aktualisieren> + Send,
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

        /// Reserviere die Anschlüsse der jeweiligen für alle `$gleise` mit dem `$steuerung`-Ident.
        macro_rules! reserviere_anschlüsse {
            ($anschlüsse: ident => $($gleise: ident - $steuerungen: ident),* $(,)?) => {$(
                let ($gleise, neue_rstern_elemente, $anschlüsse) = reserviere_anschlüsse(
                    lager,
                    $gleise,
                    zugtyp.spurweite,
                    &zugtyp.$gleise,
                    $anschlüsse,
                    &mut bekannte_steuerungen.$steuerungen,
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
            geraden - kontakte,
            kurven - kontakte,
            weichen - gerade_weichen,
            dreiwege_weichen - dreiwege_weichen,
            kurven_weichen - kurven_weichen,
            s_kurven_weichen - gerade_weichen,
            kreuzungen - gerade_weichen,
        );

        let daten = GleiseDaten {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            rstern: RStern::bulk_load(rstern_elemente),
        };

        (daten, anschlüsse)
    }
}

impl<L: BekannterLeiter> From<&Zugtyp<L>> for ZugtypSerialisiert<L> {
    /// Erzeuge eine serialisierbare Darstellung eines [`Zugtyp`].
    fn from(zugtyp: &Zugtyp<L>) -> ZugtypSerialisiert<L> {
        let Zugtyp {
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
        } = zugtyp;
        let leiter = L::NAME.to_owned();
        /// Erzeuge eine serialisierbare Darstellung für die [`DefinitionMaps`](DefinitionMap).
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
        ZugtypSerialisiert {
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

/// Der Leiter stimmt nicht mit dem Namen überein.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum ZugtypDeserialisierenFehler {
    /// Der Leiter des Zugtyps stimmt nicht mit dem Kommandozeilen-Argument überein.
    FalscherLeiter(String),
    /// Alle [`Ids`](crate::gleise::id::eindeutig::Id) wurden bereits verwendet.
    /// Es ist aktuell keine eindeutige [`Id`](crate::gleise::id::eindeutig::Id) verfügbar.
    KeineIdVerfügbar(KeineIdVerfügbar),
}

/// Erzeuge Maps mit [`DefinitionId`] für die Definitionen.
macro_rules! erzeuge_zugtyp_maps {
    ($id_maps: expr => $($gleise: ident : $typ: ty),* $(,)?) => {
        $(
        #[allow(unused_qualifications)]
        let ($gleise, ids) = $gleise
            .into_iter()
            .fold(
                Ok((HashMap::new(), HashMap::new())),
                |acc, (gespeicherte_id, definition)| -> Result<_, ZugtypDeserialisierenFehler> {
                    if let Ok((mut gleise, mut ids)) = acc {
                        let id = DefinitionId::<$typ>::neu()?;
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
            .map(|definition| Ok((crate::gleise::id::DefinitionId::<$typ>::neu()?, definition)) )
            .collect::<
                Result<
                    crate::zugtyp::DefinitionMap<$typ>,
                    crate::gleise::daten::de_serialisieren::ZugtypDeserialisierenFehler
                >
            >().expect($expect_msg);
    )*};
}
pub(crate) use erzeuge_zugtyp_maps;

impl<L: BekannterLeiter> ZugtypSerialisiert<L> {
    /// Erzeuge die Laufzeit-Darstellung für einen [`Zugtyp`].
    fn reserviere(self) -> Result<(Zugtyp<L>, DefinitionIdMaps), ZugtypDeserialisierenFehler> {
        let ZugtypSerialisiert {
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
        erzeuge_zugtyp_maps!(
            bekannte_ids =>
            geraden: Gerade,
            kurven: Kurve,
            weichen: Weiche,
            dreiwege_weichen: DreiwegeWeiche,
            kurven_weichen: KurvenWeiche,
            s_kurven_weichen: SKurvenWeiche,
            kreuzungen: Kreuzung,
        );

        let zugtyp = Zugtyp {
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

impl<L: Leiter> Zustand<L> {
    /// Erzeuge eine Serialisierbare Repräsentation.
    pub(crate) fn serialisiere<S>(&self) -> ZustandSerialisiert<L, S>
    where
        L: Serialisiere<S> + BekannterLeiter,
        <L as Leiter>::Fahrtrichtung: Clone,
    {
        let Zustand { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne } = self;
        /// Serialisiere das erste Argument, klone alle folgenden.
        macro_rules! serialisiere_head_clone_tail {
            ($head: ident $(, $tail: ident)* $(,)?) => {
                ($head.serialisiere() $(, $tail.clone())*)
            };
        }
        /// Erzeuge eine serialisierbare Darstellung für die jeweiligen [`HashMaps`](HashMap).
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
            zugtyp: ZugtypSerialisiert::from(zugtyp),
            geschwindigkeiten,
            streckenabschnitte,
            gleise: gleise.serialisiere(),
            pläne,
        }
    }

    /// Gebe alle im [`Zustand`] verwendeten [`Anschlüsse`] zurück.
    fn anschlüsse_ausgeben<S>(&mut self) -> Anschlüsse
    where
        L: Serialisiere<S>,
    {
        let Zustand { zugtyp: _, geschwindigkeiten, streckenabschnitte, gleise, pläne: _ } = self;
        let GleiseDaten {
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
        /// Gebe das erste Argument zurück.
        macro_rules! head {
            ($head: ident $(, $tail: ident)* $(,)?) => {
                $head
            };
        }
        /// Sammle alle Anschlüsse in den [`HashMaps`](HashMap),
        /// mit den entsprechenden `$matching`-pattern für die Werte.
        ///
        /// Auf das erste pattern-argument wird [`Serialisiere::anschlüsse`] aufgerufen.
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

impl<L, S> ZustandSerialisiert<L, S>
where
    L: BekannterLeiter + Serialisiere<S>,
    S: Clone + Eq + Hash + Reserviere<L, MoveArg = (), RefArg = (), MutRefArg = ()>,
{
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere<Nachricht: 'static + From<Aktualisieren> + Send>(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        sender: &Sender<Nachricht>,
    ) -> Result<(Zustand<L>, Vec<LadenFehler<S>>), ZugtypDeserialisierenFehler> {
        let ZustandSerialisiert { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne } =
            self;

        let (zugtyp, bekannte_definition_ids) = zugtyp.reserviere()?;

        let mut bekannte_ids = IdMaps { definitionen: bekannte_definition_ids, ..IdMaps::neu() };
        let mut bekannte_steuerungen = SteuerungMaps::neu();

        let mut laden_fehler: Vec<LadenFehler<S>> = Vec::new();

        /// Reserviere die benötigten Anschlüsse für die übergebenen [`HashMaps`](HashMap).
        macro_rules! reserviere_maps {
            ($anschlüsse: ident => $($elemente: ident $(, $extra_info: ident - $hash_eq_steuerung: ident)?);* $(;)? ) => {$(
                #[allow(unused_parens)]
                let ($elemente, $anschlüsse) = $elemente.into_iter().fold(
                    (HashMap::new(), $anschlüsse),
                    |(mut elemente, anschlüsse), (name, (serialisiert $(, $extra_info)?))| {
                        let ergebnis = serialisiert.reserviere(lager, anschlüsse, (), &(), &mut ());
                        let (anschluss, fehler, anschlüsse) = match ergebnis {
                            Ergebnis::Wert { anschluss, anschlüsse } => {
                                (Some(anschluss), None, anschlüsse)
                            },
                            Ergebnis::FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                                (Some(anschluss), Some(fehler), anschlüsse)
                            },
                            Ergebnis::Fehler { fehler, anschlüsse } => {
                                (None, Some(fehler), anschlüsse)
                            },
                        };
                        if let Some(anschluss) = anschluss {
                            // Speichere bekannte Steuerungen
                            let _ = bekannte_steuerungen.$elemente.insert(
                                anschluss.serialisiere() $(.$hash_eq_steuerung())?,
                                anschluss.clone()
                            );
                            // Name ist eindeutig, da die serialisierte Struktur ebenfalls eine HashMap war.
                            let _ = elemente.insert(name, (anschluss $(, $extra_info.clone())?));
                        }
                        if let Some(fehler) = fehler {
                            laden_fehler.extend(fehler.map(LadenFehler::from));
                        }
                        (elemente, anschlüsse)
                    },
                );
            )*};
        }

        reserviere_maps!(
            anschlüsse =>
            geschwindigkeiten;
            streckenabschnitte, geschwindigkeit - anschluss;
        );
        let (gleise, _anschlüsse) = gleise.reserviere(
            &zugtyp,
            lager,
            anschlüsse,
            &mut bekannte_ids,
            &mut bekannte_steuerungen,
            &mut laden_fehler,
            sender,
        );
        let pläne = pläne.into_iter().fold(HashMap::new(), |mut acc_pläne, (name, plan)| {
            let plan = match plan.deserialisiere(&bekannte_steuerungen, sender) {
                Ok(plan) => plan,
                Err(fehler) => {
                    laden_fehler.push(LadenFehler::UnbekannteAnschlüsse {
                        plan: name,
                        anschlüsse: fehler,
                    });
                    return acc_pläne;
                },
            };
            // Name ist eindeutig, da die serialisierte Struktur ebenfalls eine HashMap war.
            let _ = acc_pläne.insert(name, plan);
            acc_pläne
        });

        let zustand = Zustand { zugtyp, geschwindigkeiten, streckenabschnitte, gleise, pläne };
        Ok((zustand, laden_fehler))
    }
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Speicher alle Gleise, [`Streckenabschnitte`](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [`Zugtyp`]
    /// in einer Datei.
    ///
    /// ## Errors
    ///
    /// Fehler beim Schreiben der Datei.
    pub fn speichern<S>(&self, pfad: impl AsRef<Path>) -> Result<(), Fehler>
    where
        L: Serialisiere<S> + BekannterLeiter,
        <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize,
        <L as Leiter>::UmdrehenZeit: Serialize,
        <L as Leiter>::Fahrtrichtung: Clone + Serialize,
        S: Serialize,
    {
        let serialisiert: ZustandSerialisiert<L, S> = self.zustand.serialisiere();
        let file = fs::File::create(pfad)?;
        BINCODE_OPTIONS.serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }

    /// Lade Gleise, [`Streckenabschnitte`](streckenabschnitt::Streckenabschnitt),
    /// [Geschwindigkeiten](geschwindigkeit::Geschwindigkeit) und den verwendeten [`Zugtyp`]
    /// aus einer Datei.
    ///
    /// ## Errors
    ///
    /// - Fehler beim Lesen der Datei.
    /// - Invalides Speicherformat der Datei.
    /// - Fehler beim [`Reservieren`](Reserviere::reserviere) der benötigen Anschlüsse.
    pub fn laden<S>(
        &mut self,
        lager: &mut Lager,
        pfad: impl AsRef<Path>,
    ) -> Result<(), NonEmpty<LadenFehler<S>>>
    where
        L: 'static + BekannterLeiter + Serialisiere<S>,
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
        AktualisierenNachricht: 'static + From<Aktualisieren> + Send,
    {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.erzwinge_neuzeichnen();
        let anschlüsse = self.zustand.anschlüsse_ausgeben();

        // pivot, skalieren, modus, letzte_maus_position, letzte_canvas_größe nicht anpassen

        // lese & parse Datei
        let content = fs::read(pfad).map_err(|fehler| NonEmpty::singleton(fehler.into()))?;
        let slice = content.as_slice();
        let mut id_fehler = Vec::new();
        let zustand_serialisiert: ZustandSerialisiert<L, S> = BINCODE_OPTIONS
            .deserialize(slice)
            .or_else(|aktuell| {
                match BINCODE_OPTIONS.deserialize::<v3::ZustandSerialisiert<L, S>>(slice) {
                    Ok(v3) => Ok(v3.v4(&mut id_fehler)),
                    Err(v3) => {
                        match BINCODE_OPTIONS
                            .deserialize::<v2::GleiseVecs<<L as BekannterZugtyp>::V2>>(slice)
                        {
                            Ok(v2) => match v3::ZustandSerialisiert::try_from(v2) {
                                Ok(v3_from_v2) => Ok(v3_from_v2.v4(&mut id_fehler)),
                                Err(fehler) => Err(fehler),
                            },
                            Err(v2) => Err(LadenFehler::BincodeDeserialisieren { aktuell, v3, v2 }),
                        }
                    },
                }
            })
            .map_err(NonEmpty::singleton)?;

        // reserviere Anschlüsse
        let (zustand, reservieren_fehler) = zustand_serialisiert
            .reserviere(lager, anschlüsse, &self.sender)
            .map_err(|fehler| NonEmpty::singleton(LadenFehler::from(fehler)))?;
        self.zustand = zustand;
        if let Some(non_empty) = NonEmpty::collect(
            id_fehler.into_iter().map(LadenFehler::from).chain(reservieren_fehler),
        ) {
            Err(non_empty)
        } else {
            Ok(())
        }
    }
}
