//! Anzeige der GleisDefinition auf einem Canvas

use std::{collections::hash_map::Entry, fmt::Debug, time::Instant};

use self::daten::GleiseDaten;
pub use self::{
    daten::{Gleis, Zustand},
    id::{AnyId, GleisId},
};
use crate::{
    anschluss::{self, Fließend},
    application::{
        typen::*,
        verbindung::{self, Verbindung},
    },
    lookup::Lookup,
    steuerung::{streckenabschnitt, Streckenabschnitt},
};

pub mod daten;
pub mod de_serialisieren;
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
        name: streckenabschnitt::Name,
        mut streckenabschnitt: Streckenabschnitt,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        let entry = self.zustand.streckenabschnitte.entry(name);
        match entry {
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
        }
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt(
        &self,
        name: &streckenabschnitt::Name,
    ) -> Option<(&Streckenabschnitt, &Fließend)> {
        self.zustand
            .streckenabschnitte
            .get(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s, 't>(
        &'s mut self,
        name: &'t streckenabschnitt::Name,
    ) -> Option<(&'s mut Streckenabschnitt, &'s mut Fließend)> {
        self.zustand
            .streckenabschnitte
            .get_mut(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    pub fn entferne_streckenabschnitt(
        &mut self,
        name: streckenabschnitt::Name,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        self.canvas.leeren();
        if let Some((streckenabschnitt, fließend, daten)) =
            self.zustand.streckenabschnitte.remove(&name)
        {
            self.zustand.ohne_streckenabschnitt.verschmelze(daten);
            Some((streckenabschnitt, fließend))
        } else {
            None
        }
    }

    /// Namen und Farbe aller aktuell bekannten Streckenabschnitte.
    pub(crate) fn streckenabschnitte(
        &self,
    ) -> impl Iterator<Item = (&streckenabschnitt::Name, (&Streckenabschnitt, &Fließend))> {
        self.zustand.streckenabschnitte.iter().map(
            |(name, (streckenabschnitt, fließend, _maps))| (name, (streckenabschnitt, fließend)),
        )
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
    StreckenabschnittEntfernt(streckenabschnitt::Name),
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
    StreckenabschnittEntfernt(streckenabschnitt::Name),
}
impl From<GleisIdFehler> for Fehler {
    fn from(fehler: GleisIdFehler) -> Self {
        match fehler {
            GleisIdFehler::GleisEntfernt => Fehler::GleisEntfernt,
            GleisIdFehler::StreckenabschnittEntfernt(name) => {
                Fehler::StreckenabschnittEntfernt(name)
            }
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
pub struct StreckenabschnittEntferntFehler(streckenabschnitt::Name);
impl From<StreckenabschnittEntferntFehler> for Fehler {
    fn from(fehler: StreckenabschnittEntferntFehler) -> Self {
        Fehler::StreckenabschnittEntfernt(fehler.0)
    }
}
impl From<StreckenabschnittEntferntFehler> for GleisIdFehler {
    fn from(fehler: StreckenabschnittEntferntFehler) -> Self {
        GleisIdFehler::StreckenabschnittEntfernt(fehler.0)
    }
}
