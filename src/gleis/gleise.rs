//! Verwalten und Anzeige der Gleis-Definitionen auf einem
//! [Canvas](crate::application::touch_canvas::Canvas).

use std::{
    collections::hash_map::Entry, convert::identity, fmt::Debug, iter, sync::Arc, time::Instant,
};

use iced::{
    mouse,
    widget::canvas::{event, Cursor, Event, Geometry, Program},
    Rectangle, Theme,
};
use log::error;
use nonempty::NonEmpty;
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard,
};

use crate::{
    anschluss,
    gleis::{
        self,
        gerade::Gerade,
        gleise::{
            daten::{GleiseDaten, StreckenabschnittMap, Zustand},
            id::{AnyIdRef, StreckenabschnittIdRef},
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        kontakt::KontaktSerialisiert,
        plan::{AktionStreckenabschnitt, AnyAktionSchalten},
        streckenabschnitt::{self, Streckenabschnitt},
        weiche,
    },
    typen::{
        canvas::{Cache, Position},
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::Winkel,
    },
    zugtyp::{FalscherLeiter, Zugtyp},
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod steuerung;
pub mod update;

pub use self::{
    daten::Gleis,
    id::{AnyId, GleisId, StreckenabschnittId},
};

type IdUndSteuerungSerialisiert<T, S> = (GleisId<T>, S);

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type StWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type StDreiwegeWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type StKurvenWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

// FIXME sind die ganzen Typ-Aliase notwendig? Record-Felder wären vmtl. besser
#[derive(Debug, zugkontrolle_macros::From)]
pub enum GleisSteuerung {
    Gerade(IdUndSteuerungSerialisiert<Gerade, Option<KontaktSerialisiert>>),
    Kurve(IdUndSteuerungSerialisiert<Kurve, Option<KontaktSerialisiert>>),
    Weiche(IdUndSteuerungSerialisiert<Weiche, Option<StWeicheSerialisiert>>),
    KurvenWeiche(IdUndSteuerungSerialisiert<KurvenWeiche, Option<StKurvenWeicheSerialisiert>>),
    DreiwegeWeiche(
        IdUndSteuerungSerialisiert<DreiwegeWeiche, Option<StDreiwegeWeicheSerialisiert>>,
    ),
    SKurvenWeiche(IdUndSteuerungSerialisiert<SKurvenWeiche, Option<StWeicheSerialisiert>>),
    Kreuzung(IdUndSteuerungSerialisiert<Kreuzung, Option<StWeicheSerialisiert>>),
}

impl GleisSteuerung {
    fn id(&self) -> AnyIdRef<'_> {
        match self {
            GleisSteuerung::Gerade((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Kurve((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Weiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::KurvenWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::DreiwegeWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::SKurvenWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Kreuzung((id, _steuerung)) => id.als_ref().into(),
        }
    }

    pub(crate) fn klonen(&self) -> GleisSteuerung {
        match self {
            GleisSteuerung::Gerade((id, steuerung)) => {
                GleisSteuerung::Gerade((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Kurve((id, steuerung)) => {
                GleisSteuerung::Kurve((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Weiche((id, steuerung)) => {
                GleisSteuerung::Weiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::KurvenWeiche((id, steuerung)) => {
                GleisSteuerung::KurvenWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::DreiwegeWeiche((id, steuerung)) => {
                GleisSteuerung::DreiwegeWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::SKurvenWeiche((id, steuerung)) => {
                GleisSteuerung::SKurvenWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Kreuzung((id, steuerung)) => {
                GleisSteuerung::Kreuzung((id.klonen(), steuerung.clone()))
            },
        }
    }
}

macro_rules! mit_any_steuerung_id {
    ($gleis_steuerung: expr , $function: expr$(, $objekt:expr$(, $extra_arg:expr)*)?) => {
        match $gleis_steuerung {
            GleisSteuerung::Gerade((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Kurve((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Weiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::DreiwegeWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::KurvenWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::SKurvenWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Kreuzung((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
        }
    };
}
pub(crate) use mit_any_steuerung_id;

#[derive(Debug)]
struct Gehalten {
    gleis_steuerung: GleisSteuerung,
    halte_position: Vektor,
    winkel: Winkel,
    bewegt: bool,
}

#[zugkontrolle_macros::erstelle_enum(pub, Modus)]
/// Aktueller Modus von [Gleise].
#[derive(Debug)]
enum ModusDaten {
    /// Im Bauen-Modus können Gleise hinzugefügt, bewegt, angepasst und bewegt werden.
    Bauen { gehalten: Option<Gehalten>, last: Instant },
    /// Im Fahren-Modus werden die mit den Gleisen assoziierten Aktionen durchgeführt.
    Fahren,
}

impl ModusDaten {
    fn neu(modus: Modus) -> Self {
        match modus {
            Modus::Bauen => ModusDaten::Bauen { gehalten: None, last: Instant::now() },
            Modus::Fahren => ModusDaten::Fahren,
        }
    }
}

/// Verwalten und Anzeige aller Gleise.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub struct Gleise<L: Leiter> {
    canvas: Arc<Mutex<Cache>>,
    pivot: Position,
    skalieren: Skalar,
    zustand: RwLock<Zustand<L>>,
    last_mouse: RwLock<Vektor>,
    last_size: RwLock<Vektor>,
    modus: RwLock<ModusDaten>,
}

// Helper-funktion zur Verwendung mit RwLockRead/WriteGuard::try_map
fn speicher_fehler_in_variable<T, E, F>(result: Result<T, E>, option: &mut Option<F>) -> Option<T>
where
    E: Into<F>,
{
    match result {
        Ok(t) => Some(t),
        Err(e) => {
            *option = Some(e.into());
            None
        },
    }
}

impl<L: Leiter> Gleise<L> {
    /// Erstelle ein neues, leeres [Gleise]-struct.
    pub fn neu(zugtyp: Zugtyp<L>, modus: Modus, pivot: Position, skalieren: Skalar) -> Self {
        Gleise {
            canvas: Arc::new(Mutex::new(Cache::neu())),
            pivot,
            skalieren,
            zustand: RwLock::new(Zustand::neu(zugtyp)),
            last_mouse: RwLock::new(Vektor::null_vektor()),
            last_size: RwLock::new(Vektor::null_vektor()),
            modus: RwLock::new(ModusDaten::neu(modus)),
        }
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match &*self.modus.read() {
            ModusDaten::Bauen { .. } => Modus::Bauen,
            ModusDaten::Fahren => Modus::Fahren,
        }
    }

    /// Wechsel den aktuellen Modus zu `modus`.
    pub fn moduswechsel(&mut self, modus: Modus) {
        *self.modus.write() = ModusDaten::neu(modus);
    }

    /// Aktuelle Pivot-Punkt und Dreh-Winkel
    pub fn pivot(&self) -> &Position {
        &self.pivot
    }

    /// Bewege aktuellen Pivot-Punkt nach `pivot`.
    pub fn setze_pivot(&mut self, pivot: Vektor) {
        self.pivot.punkt = pivot;
        self.canvas.lock().leeren();
    }

    /// Bewege aktuellen Pivot-Punkt um `bewegung`.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.lock().leeren();
    }

    /// Setze den `winkel` für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.lock().leeren();
    }

    /// Drehe die aktuelle Darstellung um `winkel`.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.lock().leeren();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Setze den aktueller Skalierfaktor zur Darstellung.
    pub fn setze_skalierfaktor(&mut self, skalieren: Skalar) {
        self.skalieren = skalieren;
        self.canvas.lock().leeren();
    }

    /// Multipliziere die aktuelle Darstellung mit `skalieren`.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.lock().leeren();
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    #[inline(always)]
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
    ) -> Result<(StreckenabschnittId, Option<Streckenabschnitt>), StreckenabschnittHinzufügenFehler>
    {
        self.streckenabschnitt_hinzufügen_aux(geschwindigkeit, name, streckenabschnitt)
    }

    /// Füge einen Streckenabschnitt mit angenommenen Fließend-Zustand hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    fn streckenabschnitt_hinzufügen_aux(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        mut streckenabschnitt: Streckenabschnitt,
    ) -> Result<(StreckenabschnittId, Option<Streckenabschnitt>), StreckenabschnittHinzufügenFehler>
    {
        let mut guard = self.zustand.write();
        let streckenabschnitt_map = match guard.streckenabschnitt_map_mut(geschwindigkeit) {
            Ok(streckenabschnitt_map) => streckenabschnitt_map,
            Err(GeschwindigkeitEntferntFehler(name)) => {
                return Err(StreckenabschnittHinzufügenFehler::GeschwindigkeitEntfernt(
                    name,
                    streckenabschnitt,
                ))
            },
        };
        let entry = streckenabschnitt_map.entry(name.clone());
        let bisher = match entry {
            Entry::Occupied(mut occupied) => {
                let bisher = occupied.get_mut();
                std::mem::swap(&mut bisher.0, &mut streckenabschnitt);
                Some(streckenabschnitt)
            },
            Entry::Vacant(vacant) => {
                let _mut_ref = vacant.insert((streckenabschnitt, GleiseDaten::neu()));
                None
            },
        };
        Ok((StreckenabschnittId { geschwindigkeit: geschwindigkeit.cloned(), name }, bisher))
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s>(
        &'s self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<MappedRwLockReadGuard<'s, Streckenabschnitt>, StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let guard = self.zustand.read();
        // try_map kann den fehler nicht direkt übergeben, verwende daher eine temporäre Variable
        let mut geschwindigkeit_entfernt_fehler = None;
        RwLockReadGuard::try_map(guard, |zustand| {
            let streckenabschnitt_map = speicher_fehler_in_variable(
                zustand.streckenabschnitt_map(geschwindigkeit.as_ref()),
                &mut geschwindigkeit_entfernt_fehler,
            )?;
            streckenabschnitt_map.get(name).map(|(streckenabschnitt, _daten)| streckenabschnitt)
        })
        .map_err(|_guard| {
            geschwindigkeit_entfernt_fehler.unwrap_or_else(|| {
                StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt.klonen())
            })
        })
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<MappedRwLockWriteGuard<'s, Streckenabschnitt>, StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let guard = self.zustand.write();
        let mut geschwindigkeit_entfernt_fehler = None;
        RwLockWriteGuard::try_map(guard, |zustand| {
            let streckenabschnitt_map = speicher_fehler_in_variable(
                zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref()),
                &mut geschwindigkeit_entfernt_fehler,
            )?;
            streckenabschnitt_map.get_mut(name).map(|(streckenabschnitt, _daten)| streckenabschnitt)
        })
        .map_err(|_guard| {
            geschwindigkeit_entfernt_fehler.unwrap_or_else(|| {
                StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt.klonen())
            })
        })
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    pub fn streckenabschnitt_entfernen(
        &mut self,
        streckenabschnitt_id: StreckenabschnittId,
    ) -> Result<Option<Streckenabschnitt>, StreckenabschnittBearbeitenFehler> {
        let StreckenabschnittId { geschwindigkeit, name: _ } = &streckenabschnitt_id;
        let mut guard = self.zustand.write();
        let streckenabschnitt_map = guard.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        self.canvas.lock().leeren();
        streckenabschnitt_entfernen(
            streckenabschnitt_map,
            streckenabschnitt_id,
            Some,
            |_streckenabschnitt_id| Ok(None),
        )
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn aus_allen_streckenabschnitten<T, C>(
        &self,
        f: impl for<'s> Fn(StreckenabschnittIdRef<'s>, &'s Streckenabschnitt) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        let guard = self.zustand.read();
        // Notwendig, da sonst f in die flat_map-closure moved wird, wodurch sie nur FnOnce ist.
        // Außerdem darf sie aus irgendeinem Grund nicht als Variable gespeichert werden.
        let g = &f;
        iter::once((None, &guard.ohne_geschwindigkeit))
            .chain(guard.geschwindigkeiten.iter().map(
                |(geschwindigkeit_name, (_geschwindigkeit, streckenabschnitt_map))| {
                    (Some(geschwindigkeit_name), streckenabschnitt_map)
                },
            ))
            .flat_map(|(geschwindigkeit, streckenabschnitt_map): (_, &StreckenabschnittMap)| {
                streckenabschnitt_map.iter().map(move |(name, (streckenabschnitt, _daten))| {
                    g(StreckenabschnittIdRef { geschwindigkeit, name }, streckenabschnitt)
                })
            })
            .collect()
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
    ) -> Option<Geschwindigkeit<L>> {
        match self.zustand.write().geschwindigkeiten.entry(name) {
            Entry::Occupied(mut occupied) => {
                let bisher = std::mem::replace(&mut occupied.get_mut().0, geschwindigkeit);
                Some(bisher)
            },
            Entry::Vacant(vacant) => {
                let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
                None
            },
        }
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit<'s>(
        &'s self,
        name: &geschwindigkeit::Name,
    ) -> Option<MappedRwLockReadGuard<'s, Geschwindigkeit<L>>> {
        let guard = self.zustand.read();
        RwLockReadGuard::try_map(guard, |zustand| {
            zustand
                .geschwindigkeiten
                .get(name)
                .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
        })
        .ok()
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<MappedRwLockWriteGuard<'s, Geschwindigkeit<L>>> {
        let guard = self.zustand.write();
        RwLockWriteGuard::try_map(guard, |zustand| {
            zustand
                .geschwindigkeiten
                .get_mut(name)
                .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
        })
        .ok()
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Schlägt fehl, wenn noch assoziierten Streckenabschnitte vorhanden waren.
    pub fn geschwindigkeit_entfernen(
        &mut self,
        name: geschwindigkeit::Name,
    ) -> Result<Option<Geschwindigkeit<L>>, GeschwindigkeitEntfernenFehler> {
        if let Some((name_entry, (geschwindigkeit, streckenabschnitt_map))) =
            self.zustand.write().geschwindigkeiten.remove_entry(&name)
        {
            if streckenabschnitt_map.is_empty() {
                Ok(Some(geschwindigkeit))
            } else {
                let _ = self
                    .zustand
                    .write()
                    .geschwindigkeiten
                    .insert(name_entry, (geschwindigkeit, streckenabschnitt_map));
                Err(GeschwindigkeitEntfernenFehler::StreckenabschnitteNichtEntfernt(name))
            }
        } else {
            Ok(None)
        }
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub(crate) fn geschwindigkeit_mit_streckenabschnitten_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
        streckenabschnitt_map: StreckenabschnittMap,
    ) -> Option<(Geschwindigkeit<L>, StreckenabschnittMap)> {
        match self.zustand.write().geschwindigkeiten.entry(name) {
            Entry::Occupied(mut occupied) => {
                let bisher =
                    std::mem::replace(occupied.get_mut(), (geschwindigkeit, streckenabschnitt_map));
                Some(bisher)
            },
            Entry::Vacant(vacant) => {
                let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
                None
            },
        }
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Assoziierte Streckenabschnitte werden (mit allen Gleisen) ebenfalls entfernt.
    #[inline(always)]
    pub(crate) fn geschwindigkeit_mit_streckenabschnitten_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<(Geschwindigkeit<L>, StreckenabschnittMap)> {
        self.zustand.write().geschwindigkeiten.remove(name)
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn mit_allen_geschwindigkeiten(
        &self,
        mut f: impl FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>),
    ) {
        let guard = self.zustand.read();
        for (name, (geschwindigkeit, _streckenabschnitt_map)) in guard.geschwindigkeiten.iter() {
            f(name, geschwindigkeit)
        }
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn mit_allen_geschwindigkeiten_mut(
        &self,
        mut f: impl FnMut(&geschwindigkeit::Name, &mut Geschwindigkeit<L>),
    ) {
        let mut guard = self.zustand.write();
        for (name, (geschwindigkeit, _streckenabschnitt_map)) in guard.geschwindigkeiten.iter_mut()
        {
            f(name, geschwindigkeit)
        }
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp<'t>(&'t self) -> MappedRwLockReadGuard<'t, Zugtyp<L>> {
        let guard = self.zustand.read();
        RwLockReadGuard::map(guard, |zustand| &zustand.zugtyp)
    }

    /// Spurweite des verwendeten Zugtyps.
    pub fn spurweite(&self) -> Spurweite {
        self.zustand.read().zugtyp.spurweite
    }
}

impl<L: Debug + Leiter> Gleise<L> {
    /// Assoziiere einen Streckenabschnitt mit einer Geschwindigkeit.
    /// Existiert bei der neuen Geschwindigkeit ein Streckenabschnitt mit identischem Namen
    /// wird dieser überschrieben und zurückgegeben.
    /// Schlägt fehl, wenn die neue Geschwindigkeit identisch zur aktuellen ist.
    /// Schlägt fehl, wenn der Streckenabschnitt oder die neue Geschwindigkeit nicht gefunden wurde.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    pub fn streckenabschnitt_assoziiere_geschwindigkeit(
        &mut self,
        streckenabschnitt_id: &mut StreckenabschnittId,
        geschwindigkeit_neu: Option<&geschwindigkeit::Name>,
    ) -> Result<Option<Streckenabschnitt>, StreckenabschnittBearbeitenFehler> {
        if streckenabschnitt_id.geschwindigkeit.as_ref() == geschwindigkeit_neu {
            return Err(StreckenabschnittBearbeitenFehler::IdentischeGeschwindigkeit(
                geschwindigkeit_neu.cloned(),
            ));
        }
        let geschwindigkeit = streckenabschnitt_id.geschwindigkeit.clone();
        let name = streckenabschnitt_id.name.clone();
        let (geschwindigkeit_name_und_eintrag, streckenabschnitt) =
            if let Some(geschwindigkeit_name) = geschwindigkeit {
                match self.zustand.write().geschwindigkeiten.remove(&geschwindigkeit_name) {
                    Some((geschwindigkeit, mut streckenabschnitt_map)) => {
                        let streckenabschnitt = streckenabschnitt_entfernen(
                            &mut streckenabschnitt_map,
                            streckenabschnitt_id.klonen(),
                            identity,
                            |streckenabschnitt_id| {
                                Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
                                    streckenabschnitt_id,
                                ))
                            },
                        )?;
                        (
                            Some((geschwindigkeit_name, (geschwindigkeit, streckenabschnitt_map))),
                            streckenabschnitt,
                        )
                    },
                    None => {
                        return Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(
                            geschwindigkeit_name,
                        ))
                    },
                }
            } else {
                let streckenabschnitt = streckenabschnitt_entfernen(
                    &mut self.zustand.write().ohne_geschwindigkeit,
                    streckenabschnitt_id.klonen(),
                    identity,
                    |streckenabschnitt_id| {
                        Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
                            streckenabschnitt_id,
                        ))
                    },
                )?;
                (None, streckenabschnitt)
            };
        match self.streckenabschnitt_hinzufügen_aux(
            geschwindigkeit_neu,
            name.clone(),
            streckenabschnitt,
        ) {
            Ok((id_neu, bisher)) => {
                if let Some((geschwindigkeit_name, geschwindigkeit_eintrag)) =
                    geschwindigkeit_name_und_eintrag
                {
                    let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
                    if let Some(geschwindigkeit_eintrag) = self
                        .zustand
                        .write()
                        .geschwindigkeiten
                        .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
                    {
                        error!(
                            "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
                            geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
                        )
                    }
                }
                *streckenabschnitt_id = id_neu;
                self.canvas.lock().leeren();
                Ok(bisher)
            },
            Err(StreckenabschnittHinzufügenFehler::GeschwindigkeitEntfernt(
                geschwindigkeit_neu,
                streckenabschnitt,
            )) => {
                if let Some((geschwindigkeit_name, mut geschwindigkeit_eintrag)) =
                    geschwindigkeit_name_und_eintrag
                {
                    if let Some(streckenabschnitt_eintrag) = geschwindigkeit_eintrag
                        .1
                        .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
                    {
                        error!(
                            "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
                            StreckenabschnittIdRef {
                                geschwindigkeit: Some(&geschwindigkeit_name),
                                name: &name
                            },
                            streckenabschnitt_eintrag
                        )
                    }
                    let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
                    if let Some(geschwindigkeit_eintrag) = self
                        .zustand
                        .write()
                        .geschwindigkeiten
                        .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
                    {
                        error!(
                            "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
                            geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
                        )
                    }
                } else {
                    if let Some(streckenabschnitt_eintrag) = self
                        .zustand
                        .write()
                        .ohne_geschwindigkeit
                        .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
                    {
                        error!(
                            "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
                            StreckenabschnittId { geschwindigkeit: None, name },
                            streckenabschnitt_eintrag
                        )
                    }
                }
                Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(geschwindigkeit_neu))
            },
        }
    }
}

fn streckenabschnitt_entfernen<T>(
    streckenabschnitt_map: &mut StreckenabschnittMap,
    streckenabschnitt_id: StreckenabschnittId,
    gefunden: impl FnOnce(Streckenabschnitt) -> T,
    bereits_entfernt: impl FnOnce(StreckenabschnittId) -> Result<T, StreckenabschnittBearbeitenFehler>,
) -> Result<T, StreckenabschnittBearbeitenFehler> {
    let StreckenabschnittId { geschwindigkeit: _, name } = &streckenabschnitt_id;
    if let Some((name_entry, (streckenabschnitt, daten))) = streckenabschnitt_map.remove_entry(name)
    {
        if daten.ist_leer() {
            Ok(gefunden(streckenabschnitt))
        } else {
            let _ = streckenabschnitt_map.insert(name_entry, (streckenabschnitt, daten));
            Err(StreckenabschnittBearbeitenFehler::GleiseNichtEntfernt(streckenabschnitt_id))
        }
    } else {
        bereits_entfernt(streckenabschnitt_id)
    }
}

// TODO Id aus Nachricht entfernen?
// SetzeStreckenabschnitt(AnyGleis)
//      Entferne Gleis um es neu hinzuzufügen
//      Id auch in Gehalten durch AnyGleis ersetzen
// AnschlüsseAnpassen(Arc<Mutex<AnyGleis>>)
//      Muss in Arc<Mutex<_>> sein, da in Nachricht aus update keine Referenz aus self enthalten kann.
/// Eine GUI-Nachricht als Reaktion auf Interaktion mit dem
/// [Canvas](crate::application::touch_canvas::Canvas).
#[derive(zugkontrolle_macros::Debug)]
#[non_exhaustive]
pub enum Nachricht {
    /// Setze den Streckenabschnitt für ein Gleis.
    SetzeStreckenabschnitt(AnyId),
    /// Ein Gleis mit [Streckenabschnitt] ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Die Anschlüsse für ein Gleis sollen angepasst werden.
    AnschlüsseAnpassen(GleisSteuerung),
}

impl<L: Leiter> Program<Nachricht> for Gleise<L> {
    type State = ();

    #[inline(always)]
    fn draw(
        &self,
        state: &Self::State,
        theme: &Theme,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Vec<Geometry> {
        self.draw(state, bounds, cursor)
    }

    #[inline(always)]
    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        self.update(state, event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        state: &Self::State,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> mouse::Interaction {
        match &*self.modus.read() {
            ModusDaten::Bauen { gehalten: Some(_gehalten), .. } if cursor.is_over(&bounds) => {
                mouse::Interaction::Pointer
            },
            _ => mouse::Interaction::default(),
        }
    }
}

/// Fehler, die bei Interaktion mit den [Gleisen](Gleise) auftreten können.
#[derive(Debug)]
pub enum Fehler {
    /// Ein IO-Fehler.
    IO(std::io::Error),
    /// Fehler beim Serialisieren (speichern) der Gleise.
    BincodeSerialisieren(bincode::Error),
    /// Ein Fehler bei Interaktion mit einem [Anschluss](anschluss::Anschluss).
    Anschluss(anschluss::Fehler),
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt,
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<FalscherLeiter> for Fehler {
    fn from(fehler: FalscherLeiter) -> Self {
        Fehler::Anschluss(fehler.into())
    }
}

impl From<std::io::Error> for Fehler {
    fn from(error: std::io::Error) -> Self {
        Fehler::IO(error)
    }
}

impl From<anschluss::Fehler> for Fehler {
    fn from(error: anschluss::Fehler) -> Self {
        Fehler::Anschluss(error)
    }
}

/// Fehler bei Interaktion mit einem [bestimmten Gleis](AnyId).
#[derive(Debug)]
pub enum GleisIdFehler {
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt,
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<GleisIdFehler> for Fehler {
    fn from(fehler: GleisIdFehler) -> Self {
        match fehler {
            GleisIdFehler::GleisEntfernt => Fehler::GleisEntfernt,
            GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            GleisIdFehler::GeschwindigkeitEntfernt(name) => Fehler::GeschwindigkeitEntfernt(name),
        }
    }
}

/// Das betroffene Gleis wurde entfernt.
#[derive(Debug, Clone, Copy)]
pub struct GleisEntferntFehler;

impl From<GleisEntferntFehler> for Fehler {
    fn from(GleisEntferntFehler: GleisEntferntFehler) -> Self {
        Fehler::GleisEntfernt
    }
}

impl From<GleisEntferntFehler> for GleisIdFehler {
    fn from(GleisEntferntFehler: GleisEntferntFehler) -> Self {
        GleisIdFehler::GleisEntfernt
    }
}

/// Fehler bei Interaktion mit einem [Streckenabschnitt]
#[derive(Debug)]
pub enum StreckenabschnittIdFehler {
    /// Der betroffene Streckenabschnitt wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<StreckenabschnittIdFehler> for Fehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
            StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            StreckenabschnittIdFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                Fehler::GeschwindigkeitEntfernt(geschwindigkeit)
            },
        }
    }
}

impl From<StreckenabschnittIdFehler> for GleisIdFehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
            StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            StreckenabschnittIdFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                GleisIdFehler::GeschwindigkeitEntfernt(geschwindigkeit)
            },
        }
    }
}

/// Die betroffene [Geschwindigkeit] wurde entfernt.
#[derive(Debug)]
pub struct GeschwindigkeitEntferntFehler(pub geschwindigkeit::Name);

impl From<GeschwindigkeitEntferntFehler> for Fehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        Fehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

impl From<GeschwindigkeitEntferntFehler> for GleisIdFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        GleisIdFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittIdFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittIdFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

/// Fehler beim Hinzufügen eines [Streckenabschnittes](Streckenabschnitt).
#[derive(Debug)]
pub enum StreckenabschnittHinzufügenFehler {
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name, Streckenabschnitt),
}

/// Fehler beim Bearbeiten eines [Streckenabschnittes](Streckenabschnitt).
#[derive(Debug)]
pub enum StreckenabschnittBearbeitenFehler {
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
    /// Es gibt noch mit dem [Streckenabschnitt] assoziierte Gleise.
    GleiseNichtEntfernt(StreckenabschnittId),
    /// Es wurde die selbe [Geschwindigkeit] gewählt.
    IdentischeGeschwindigkeit(Option<geschwindigkeit::Name>),
}

impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittBearbeitenFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

/// Ein Fehler beim Entfernen einer [Geschwindigkeit].
#[derive(Debug)]
pub enum GeschwindigkeitEntfernenFehler {
    /// Es gibt noch mit der [Geschwindigkeit] assoziierte [Streckenabschnitte](Streckenabschnitt).
    StreckenabschnitteNichtEntfernt(geschwindigkeit::Name),
}

type WeicheSerialisiert = weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;
type DreiwegeWeicheSerialisiert = weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;
type KurvenWeicheSerialisiert = weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

/// Anschlüsse für ein Gleis anpassen.
#[derive(Debug)]
pub enum AnschlüsseAnpassen {
    /// Anschlüsse einer [Weiche] anpassen.
    Weiche(GleisId<Weiche>, Option<WeicheSerialisiert>),
    /// Anschlüsse einer [DreiwegeWeiche] anpassen.
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<DreiwegeWeicheSerialisiert>),
    /// Anschlüsse einer [KurvenWeiche] anpassen.
    KurvenWeiche(GleisId<KurvenWeiche>, Option<KurvenWeicheSerialisiert>),
    /// Anschlüsse einer [SKurvenWeiche] anpassen.
    SKurvenWeiche(GleisId<SKurvenWeiche>, Option<WeicheSerialisiert>),
    /// Anschlüsse einer [Kreuzung] anpassen.
    Kreuzung(GleisId<Kreuzung>, Option<WeicheSerialisiert>),
}

/// Fehler, die beim Anpassen der Anschlüsse eines Gleises auftreten können.
#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum AnschlüsseAnpassenFehler {
    /// Ein Fehler beim [Reservieren](Reserviere::reserviere) der [Anschlüsse](anschluss::Anschluss).
    Deserialisieren {
        /// Der Fehler beim reservieren der neuen Anschlüsse.
        fehler: NonEmpty<anschluss::Fehler>,
        /// Ein Fehler beim Wiederherstellen der ursprünglichen Anschlüsse,
        /// sowie eine Repräsentation der ursprünglichen Anschlüsse.
        wiederherstellen_fehler: Option<(NonEmpty<anschluss::Fehler>, String)>,
    },
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt(GleisIdFehler),
}
