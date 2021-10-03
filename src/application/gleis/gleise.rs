//! Anzeige der GleisDefinition auf einem Canvas

use std::{collections::hash_map::Entry, fmt::Debug, iter, time::Instant};

use log::error;

pub use self::{
    daten::{Gleis, Zustand},
    id::{AnyId, GleisId, StreckenabschnittId},
};
use self::{
    daten::{GleiseDaten, StreckenabschnittMap},
    id::StreckenabschnittIdRef,
};
use crate::{
    anschluss::{self, Fließend},
    application::{
        typen::*,
        verbindung::{self, Verbindung},
    },
    lookup::Lookup,
    steuerung::{geschwindigkeit, streckenabschnitt, Geschwindigkeit, Streckenabschnitt},
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod steuerung;
pub mod update;

#[derive(zugkontrolle_derive::Debug)]
struct Gehalten<Z> {
    gleis_id: AnyId<Z>,
    halte_position: Vektor,
    bewegt: bool,
}

// Aktueller Modus von `Gleise`
#[zugkontrolle_derive::make_enum(pub, Modus)]
#[derive(zugkontrolle_derive::Debug)]
enum ModusDaten<Z> {
    Bauen { gehalten: Option<Gehalten<Z>>, last: Instant },
    Fahren,
}

/// Anzeige aller Gleise.
pub struct Gleise<Z: Zugtyp> {
    canvas: canvas::Cache,
    pivot: Position,
    skalieren: Skalar,
    zustand: Zustand<Z>,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: ModusDaten<Z>,
}

impl<Z> Debug for Gleise<Z>
where
    Z: Zugtyp,
    <Z as Zugtyp>::Leiter: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gleise")
            .field("canvas", &self.canvas)
            .field("pivot", &self.pivot)
            .field("skalieren", &self.skalieren)
            .field("zustand", &self.zustand)
            .field("last_mouse", &self.last_mouse)
            .field("last_size", &self.last_size)
            .field("modus", &self.modus)
            .finish()
    }
}

impl<Z: Zugtyp> Gleise<Z> {
    pub fn neu() -> Self {
        Gleise {
            canvas: canvas::Cache::neu(),
            pivot: Position { punkt: Vektor { x: Skalar(0.), y: Skalar(0.) }, winkel: Winkel(0.) },
            skalieren: Skalar(1.),
            zustand: Zustand::neu(),
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::Bauen { gehalten: None, last: Instant::now() },
        }
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match &self.modus {
            ModusDaten::Bauen { .. } => Modus::Bauen,
            ModusDaten::Fahren => Modus::Fahren,
        }
    }

    /// Wechsel den aktuellen Modus zu `modus`.
    pub fn moduswechsel(&mut self, modus: Modus) {
        self.modus = match modus {
            Modus::Bauen => ModusDaten::Bauen { gehalten: None, last: Instant::now() },
            Modus::Fahren => ModusDaten::Fahren,
        };
    }

    /// Aktuelle Pivot-Punkt und Dreh-Winkel
    pub fn pivot(&self) -> &Position {
        &self.pivot
    }

    /// Bewege aktuellen Pivot-Punkt nach `pivot`.
    pub fn setze_pivot(&mut self, pivot: Vektor) {
        self.pivot.punkt = pivot;
        self.canvas.leeren();
    }

    /// Bewege aktuellen Pivot-Punkt um `bewegung`.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.leeren();
    }

    /// Setze den `winkel` für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.leeren();
    }

    /// Drehe die aktuelle Darstellung um `winkel`.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.leeren();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Setze den aktueller Skalierfaktor zur Darstellung.
    pub fn setze_skalierfaktor(&mut self, skalieren: Skalar) {
        self.skalieren = skalieren;
        self.canvas.leeren();
    }

    /// Multipliziere die aktuelle Darstellung mit `skalieren`.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.leeren();
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    pub fn neuer_streckenabschnitt(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        mut streckenabschnitt: Streckenabschnitt,
    ) -> Result<Option<(Streckenabschnitt, Fließend)>, GeschwindigkeitEntferntFehler> {
        let streckenabschnitt_map = self.zustand.streckenabschnitt_map_mut(geschwindigkeit)?;
        let entry = streckenabschnitt_map.entry(name);
        Ok(match entry {
            Entry::Occupied(mut occupied) => {
                let value = occupied.get_mut();
                std::mem::swap(&mut value.0, &mut streckenabschnitt);
                let bisherig_fließend = value.1;
                value.1 = Fließend::Gesperrt;
                Some((streckenabschnitt, bisherig_fließend))
            }
            Entry::Vacant(vacant) => {
                vacant.insert((streckenabschnitt, Fließend::Gesperrt, GleiseDaten::neu()));
                None
            }
        })
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s, 't>(
        &'s self,
        streckenabschnitt: StreckenabschnittIdRef<'t>,
    ) -> Result<(&'s Streckenabschnitt, &'s Fließend), StreckenabschnittFehler> {
        let StreckenabschnittIdRef { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map = self.zustand.streckenabschnitt_map(geschwindigkeit)?;
        streckenabschnitt_map
            .get(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
            .ok_or_else(|| {
                StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt.als_id())
            })
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s, 't>(
        &'s mut self,
        streckenabschnitt: StreckenabschnittIdRef<'t>,
    ) -> Result<(&'s mut Streckenabschnitt, &'s mut Fließend), StreckenabschnittFehler> {
        let StreckenabschnittIdRef { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map = self.zustand.streckenabschnitt_map_mut(geschwindigkeit)?;
        streckenabschnitt_map
            .get_mut(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
            .ok_or_else(|| {
                StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt.als_id())
            })
    }

    // FIXME invalidiert alle GleisId für Gleise mit dem Streckenabschnitt
    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    pub fn entferne_streckenabschnitt<'t>(
        &'t mut self,
        streckenabschnitt: StreckenabschnittId,
    ) -> Result<Option<(Streckenabschnitt, Fließend)>, GeschwindigkeitEntferntFehler> {
        self.canvas.leeren();
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        Ok(streckenabschnitt_map.remove(&name).map(|(streckenabschnitt, fließend, daten)| {
            self.zustand.ohne_streckenabschnitt.verschmelze(daten);
            (streckenabschnitt, fließend)
        }))
    }

    // FIXME invalidiert alle GleisId für Gleise mit dem Streckenabschnitt
    /// Assoziiere einen Streckenabschnitt mit einer Geschwindigkeit.
    pub fn assoziiere_streckenabschnitt_geschwindigkeit(
        &mut self,
        streckenabschnitt_id: StreckenabschnittId,
        geschwindigkeit_neu: &Option<geschwindigkeit::Name>,
    ) -> Result<StreckenabschnittId, StreckenabschnittFehler> {
        self.canvas.leeren();
        let StreckenabschnittId { geschwindigkeit, name } = &streckenabschnitt_id;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        let (streckenabschnitt, fließend, daten) = if let Some(streckenabschnitt_mit_daten) =
            streckenabschnitt_map.remove(&name)
        {
            streckenabschnitt_mit_daten
        } else {
            return Err(StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt_id));
        };
        let streckenabschnitt_map_neu =
            match self.zustand.streckenabschnitt_map_mut(geschwindigkeit_neu.as_ref()) {
                Ok(streckenabschnitt_map_neu) => streckenabschnitt_map_neu,
                Err(fehler) => {
                    let streckenabschnitt_map =
                        match self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref()) {
                            Ok(streckenabschnitt_map) => streckenabschnitt_map,
                            Err(fehler) => {
                                error!(
                                "StreckenabschnittMap bei wiederherstellen nicht gefunden: {:?}",
                                fehler
                            );
                                &mut self.zustand.ohne_geschwindigkeit
                            }
                        };
                    streckenabschnitt_map
                        .insert(name.clone(), (streckenabschnitt, fließend, daten));
                    return Err(fehler.into());
                }
            };
        streckenabschnitt_map_neu.insert(name.clone(), (streckenabschnitt, fließend, daten));
        Ok(StreckenabschnittId {
            geschwindigkeit: geschwindigkeit_neu.clone(),
            name: streckenabschnitt_id.name,
        })
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn streckenabschnitte<'t>(
        &'t self,
    ) -> impl Iterator<Item = (StreckenabschnittIdRef<'t>, (&'t Streckenabschnitt, &Fließend))>
    {
        let iter_map =
            |(geschwindigkeit, streckenabschnitt_map): (_, &'t StreckenabschnittMap<Z>)| {
                streckenabschnitt_map.iter().map(
                    move |(name, (streckenabschnitt, fließend, _maps))| {
                        (
                            StreckenabschnittIdRef { geschwindigkeit, name },
                            (streckenabschnitt, fließend),
                        )
                    },
                )
            };
        iter::once((None, &self.zustand.ohne_geschwindigkeit))
            .chain(self.zustand.geschwindigkeiten.iter().map(
                |(geschwindigkeit_name, (_geschwindigkeit, streckenabschnitt_map))| {
                    (Some(geschwindigkeit_name), streckenabschnitt_map)
                },
            ))
            .flat_map(iter_map)
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    pub fn neue_geschwindigkeit(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<Z::Leiter>,
    ) -> Option<Geschwindigkeit<Z::Leiter>> {
        self.zustand
            .geschwindigkeiten
            .insert(name, (geschwindigkeit, StreckenabschnittMap::new()))
            .map(|(geschwindigkeit, streckenabschnitt_map)| {
                self.zustand.ohne_geschwindigkeit.extend(streckenabschnitt_map);
                geschwindigkeit
            })
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit<'s, 't>(
        &'s self,
        name: &'t geschwindigkeit::Name,
    ) -> Option<&'s Geschwindigkeit<Z::Leiter>> {
        self.zustand
            .geschwindigkeiten
            .get(name)
            .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s, 't>(
        &'s mut self,
        name: &'t geschwindigkeit::Name,
    ) -> Option<&'s mut Geschwindigkeit<Z::Leiter>> {
        self.zustand
            .geschwindigkeiten
            .get_mut(name)
            .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    pub fn entferne_geschwindigkeit(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<Geschwindigkeit<Z::Leiter>> {
        self.zustand.geschwindigkeiten.remove(&name).map(
            |(geschwindigkeit, streckenabschnitt_map)| {
                self.zustand.ohne_geschwindigkeit.extend(streckenabschnitt_map);
                geschwindigkeit
            },
        )
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn geschwindigkeiten(
        &self,
    ) -> impl Iterator<Item = (&geschwindigkeit::Name, &Geschwindigkeit<Z::Leiter>)> {
        self.zustand
            .geschwindigkeiten
            .iter()
            .map(|(name, (geschwindigkeit, _streckenabschnitt_map))| (name, geschwindigkeit))
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Nachricht<Z> {
    SetzeStreckenabschnitt(AnyId<Z>),
    AnschlüsseAnpassen(AnyId<Z>),
    FahrenAktion(AnyId<Z>),
}

impl<Z: Zugtyp> iced::canvas::Program<Nachricht<Z>> for Gleise<Z> {
    #[inline(always)]
    fn draw(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        self.draw(bounds, cursor)
    }

    #[inline(always)]
    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht<Z>>) {
        self.update(event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> iced::mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { gehalten: Some(_gehalten), .. } if cursor.is_over(&bounds) => {
                iced::mouse::Interaction::Pointer
            }
            _ => iced::mouse::Interaction::default(),
        }
    }
}

impl Position {
    /// Position damit anchor::Verbindung übereinander mit entgegengesetzter Richtung liegen
    fn attach_position<T>(
        definition: &T,
        anchor_name: &T::VerbindungName,
        target_anchor_point: Verbindung,
    ) -> Self
    where
        T: Zeichnen,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let verbindungen = definition.verbindungen();
        let verbindung = verbindungen.get(anchor_name);
        let winkel: Winkel = winkel::PI - verbindung.richtung + target_anchor_point.richtung;
        Position {
            punkt: Vektor {
                x: target_anchor_point.position.x - verbindung.position.x * winkel.cos()
                    + verbindung.position.y * winkel.sin(),
                y: target_anchor_point.position.y
                    - verbindung.position.x * winkel.sin()
                    - verbindung.position.y * winkel.cos(),
            },
            winkel,
        }
    }
}

#[derive(Debug)]
pub enum Fehler {
    IO(std::io::Error),
    BincodeSerialisieren(bincode::Error),
    BincodeDeserialisieren { aktuell: bincode::Error, v2: bincode::Error },
    FalscherZugtyp(String),
    Anschluss(anschluss::Fehler),
    GleisEntfernt,
    StreckenabschnittEntfernt(StreckenabschnittId),
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
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

#[derive(Debug)]
pub enum GleisIdFehler {
    GleisEntfernt,
    StreckenabschnittEntfernt(StreckenabschnittId),
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}
impl From<GleisIdFehler> for Fehler {
    fn from(fehler: GleisIdFehler) -> Self {
        match fehler {
            GleisIdFehler::GleisEntfernt => Fehler::GleisEntfernt,
            GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            }
            GleisIdFehler::GeschwindigkeitEntfernt(name) => Fehler::GeschwindigkeitEntfernt(name),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum StreckenabschnittFehler {
    StreckenabschnittEntfernt(StreckenabschnittId),
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}
impl From<StreckenabschnittFehler> for Fehler {
    fn from(fehler: StreckenabschnittFehler) -> Self {
        match fehler {
            StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            }
            StreckenabschnittFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                Fehler::GeschwindigkeitEntfernt(geschwindigkeit)
            }
        }
    }
}
impl From<StreckenabschnittFehler> for GleisIdFehler {
    fn from(fehler: StreckenabschnittFehler) -> Self {
        match fehler {
            StreckenabschnittFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt)
            }
            StreckenabschnittFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                GleisIdFehler::GeschwindigkeitEntfernt(geschwindigkeit)
            }
        }
    }
}

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
impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}
