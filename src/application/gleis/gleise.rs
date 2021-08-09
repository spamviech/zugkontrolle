//! Anzeige der GleisDefinition auf einem Canvas

use std::{fmt::Debug, time::Instant};

pub use self::{id::*, maps::*};
use crate::{
    anschluss::{self, Fließend},
    application::{anchor, typen::*},
    lookup::Lookup,
    steuerung::{streckenabschnitt, Streckenabschnitt},
};

pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod maps;
pub mod speichern_laden;
pub mod steuerung;
pub mod update;

#[derive(zugkontrolle_derive::Debug)]
struct Grabbed<Z> {
    gleis_id: AnyId<Z>,
    grab_location: Vektor,
    moved: bool,
}

// Aktueller Modus von /Gleise/
#[zugkontrolle_derive::make_enum(pub, Modus)]
#[derive(zugkontrolle_derive::Debug)]
enum ModusDaten<Z> {
    Bauen { grabbed: Option<Grabbed<Z>>, last: Instant },
    Fahren,
}

/// Anzeige aller Gleise.
#[derive(zugkontrolle_derive::Debug)]
pub struct Gleise<Z> {
    canvas: canvas::Cache,
    pivot: Position,
    skalieren: Skalar,
    maps: GleiseMaps<Z>,
    anchor_points: anchor::rstar::RTree<Z>,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: ModusDaten<Z>,
}

impl<Z> Gleise<Z> {
    pub fn neu() -> Self {
        Gleise {
            canvas: canvas::Cache::new(),
            pivot: Position { punkt: Vektor { x: Skalar(0.), y: Skalar(0.) }, winkel: Winkel(0.) },
            skalieren: Skalar(1.),
            maps: GleiseMaps::neu(),
            anchor_points: anchor::rstar::RTree::new(),
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::Bauen { grabbed: None, last: Instant::now() },
        }
    }

    fn next_id<T: Debug + GleiseMap<Z>>(&mut self) -> GleisId<T> {
        T::get_map_mut(&mut self.maps)
            .keys()
            .max()
            .map(GleisId::nachfolger)
            .unwrap_or_else(GleisId::initial)
    }

    fn relocate_grabbed<T: Debug + Zeichnen>(
        &mut self,
        gleis_id: GleisId<T>,
        punkt: Vektor,
    ) -> Result<(), GleisEntferntError>
    where
        Z: Zugtyp,
        T: GleiseMap<Z>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { position, .. } =
            T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
        let position_neu = Position { punkt, winkel: position.winkel };
        self.relocate(&gleis_id, position_neu)?;
        Ok(())
    }

    fn snap_to_anchor<T>(&mut self, gleis_id: GleisId<T>) -> Result<(), GleisEntferntError>
    where
        Z: Zugtyp,
        T: Debug + Zeichnen + GleiseMap<Z>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { definition, position, .. } =
            T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        let mut snap = None;
        anchor_points.for_each(|anchor_name, anchor| {
            if snap.is_none() {
                snap = self
                    .anchor_points
                    .get_other_id_at_point(AnyId::from_ref(&gleis_id), anchor)
                    .map(|snap_anchor| (anchor_name, snap_anchor))
            }
        });
        if let Some((snap_name, snap_anchor)) = snap {
            self.relocate_attach(&gleis_id, &snap_name, snap_anchor)?;
        };
        Ok(())
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match &self.modus {
            ModusDaten::Bauen { .. } => Modus::Bauen,
            ModusDaten::Fahren => Modus::Fahren,
        }
    }

    /// Wechsel den aktuellen Modus zu /modus/.
    pub fn moduswechsel(&mut self, modus: Modus) {
        self.modus = match modus {
            Modus::Bauen => ModusDaten::Bauen { grabbed: None, last: Instant::now() },
            Modus::Fahren => ModusDaten::Fahren,
        };
    }

    /// Aktuelle Pivot-Punkt und Dreh-Winkel
    pub fn pivot(&self) -> &Position {
        &self.pivot
    }

    /// Bewege aktuellen Pivot-Punkt nach /pivot/.
    pub fn setze_pivot(&mut self, pivot: Vektor) {
        self.pivot.punkt = pivot;
        self.canvas.clear();
    }

    /// Bewege aktuellen Pivot-Punkt um /bewegung/.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.clear();
    }

    /// Setze den /winkel/ für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.clear();
    }

    /// Drehe die aktuelle Darstellung um /winkel/.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.clear();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Setze den aktueller Skalierfaktor zur Darstellung.
    pub fn setze_skalierfaktor(&mut self, skalieren: Skalar) {
        self.skalieren = skalieren;
        self.canvas.clear();
    }

    /// Multipliziere die aktuelle Darstellung mit /skalieren/.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.clear();
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    pub fn neuer_streckenabschnitt(
        &mut self,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.insert(name, (streckenabschnitt, Fließend::Gesperrt))
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt(
        &self,
        name: &streckenabschnitt::Name,
    ) -> Option<&(Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.get(name)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s, 't>(
        &'s mut self,
        name: &'t streckenabschnitt::Name,
    ) -> Option<&'s mut (Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.get_mut(name)
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    pub fn entferne_streckenabschnitt(
        &mut self,
        name: streckenabschnitt::Name,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        macro_rules! clean_maps {
            ($($map:ident),*) => {
                $(
                    for Gleis { streckenabschnitt, .. } in self.maps.$map.values_mut() {
                        if streckenabschnitt.as_ref() == Some(&name) {
                            *streckenabschnitt = None;
                        }
                    }
                )*
            };
        }
        clean_maps! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
        let result = self.maps.streckenabschnitte.remove(&name);
        self.canvas.clear();
        result
    }

    /// Namen und Farbe aller aktuell bekannten Streckenabschnitte.
    pub(crate) fn streckenabschnitte(
        &self,
    ) -> impl Iterator<Item = (&streckenabschnitt::Name, &(Streckenabschnitt, Fließend))> {
        self.maps.streckenabschnitte.iter()
    }

    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    /// Der bisherige Wert wird zurückgegeben.
    pub fn setze_streckenabschnitt<T: GleiseMap<Z>>(
        &mut self,
        gleis_id: &GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, GleisEntferntError> {
        let gleis = T::get_map_mut(&mut self.maps).get_mut(gleis_id).ok_or(GleisEntferntError)?;
        Ok(std::mem::replace(&mut gleis.streckenabschnitt, name))
    }

    /// Wie setzte_streckenabschnitt, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline(always)]
    pub(in crate::application) fn setze_streckenabschnitt_unit<T: GleiseMap<Z>>(
        &mut self,
        gleis_id: &GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<(), GleisEntferntError> {
        self.setze_streckenabschnitt(gleis_id, name)?;
        Ok(())
    }
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    SetzeStreckenabschnitt(AnyId<Z>),
    AnschlüsseAnpassen(AnyId<Z>),
    FahrenAktion(AnyId<Z>),
}

impl<Z: Zugtyp> iced::canvas::Program<Message<Z>> for Gleise<Z> {
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
    ) -> (iced::canvas::event::Status, Option<Message<Z>>) {
        self.update(event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> iced::mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { grabbed: Some(_grabbed), .. } if cursor.is_over(&bounds) => {
                iced::mouse::Interaction::Pointer
            }
            _ => iced::mouse::Interaction::default(),
        }
    }
}

impl Position {
    /// Position damit anchor::Anchor übereinander mit entgegengesetzter Richtung liegen
    fn attach_position<T>(
        definition: &T,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> Self
    where
        T: Zeichnen,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let anchor_points = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Winkel = winkel::PI - anchor_point.richtung + target_anchor_point.richtung;
        Position {
            punkt: Vektor {
                x: target_anchor_point.position.x - anchor_point.position.x * winkel.cos()
                    + anchor_point.position.y * winkel.sin(),
                y: target_anchor_point.position.y
                    - anchor_point.position.x * winkel.sin()
                    - anchor_point.position.y * winkel.cos(),
            },
            winkel,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Bincode(bincode::Error),
    FalscherZugtyp(String),
    Anschluss(anschluss::Error),
    GleisEntfernt,
}
impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IO(error)
    }
}
impl From<bincode::Error> for Error {
    fn from(error: bincode::Error) -> Self {
        Error::Bincode(error)
    }
}
impl From<anschluss::Error> for Error {
    fn from(error: anschluss::Error) -> Self {
        Error::Anschluss(error)
    }
}

#[derive(Debug)]
pub struct GleisEntferntError;
impl From<GleisEntferntError> for Error {
    fn from(GleisEntferntError: GleisEntferntError) -> Self {
        Error::GleisEntfernt
    }
}
