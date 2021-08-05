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
pub mod id;
pub mod maps;
pub mod speichern_laden;
pub(in crate::application) mod steuerung;
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
    #[inline]
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
    #[inline]
    fn draw(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        self.draw(bounds, cursor)
    }

    #[inline]
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

impl<Z: Zugtyp> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add<T>(&mut self, gleis: Gleis<T>) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { definition, position, .. } = &gleis;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        let gleis_id = self.next_id();
        // add to anchor_points
        anchor_points.for_each(|_name, anchor| {
            self.anchor_points.insert(AnyId::from_ref(&gleis_id), anchor.clone())
        });
        // add to HashMap
        T::get_map_mut(&mut self.maps).insert(gleis_id.clone(), gleis);
        // trigger redraw
        self.canvas.clear();
        // return value
        (gleis_id, anchor_points)
    }

    /// Add a gleis at the last known mouse position
    /// capped at the last known canvas size.
    pub(crate) fn add_grabbed_at_mouse<T>(
        &mut self,
        definition: T,
        grab_location: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        GleisId<T>: Into<AnyId<Z>>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let mut canvas_position = self.last_mouse;
        let ex = Vektor { x: Skalar(1.), y: Skalar(0.) }.rotiert(-self.pivot.winkel);
        let cp_x = canvas_position.skalarprodukt(&ex);
        if cp_x < Skalar(0.) {
            canvas_position -= cp_x * ex;
        } else if cp_x > self.last_size.x {
            canvas_position -= (cp_x - self.last_size.x) * ex;
        }
        let ey = Vektor { x: Skalar(0.), y: Skalar(1.) }.rotiert(-self.pivot.winkel);
        let cp_y = canvas_position.skalarprodukt(&ey);
        if cp_y < Skalar(0.) {
            canvas_position -= cp_y * ey;
        } else if cp_y > self.last_size.y {
            canvas_position -= (cp_y - self.last_size.y) * ey;
        }
        let result = self.add(Gleis {
            definition,
            position: Position {
                punkt: canvas_position - grab_location,
                winkel: -self.pivot.winkel,
            },
            streckenabschnitt,
        });
        if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
            let gleis_id = result.0.clone().into();
            *grabbed = Some(Grabbed { gleis_id, grab_location, moved: true });
        }
        result
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn add_attach<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        // calculate new position
        let position = Position::attach_position(&definition, anchor_name, target_anchor_point);
        // add new gleis
        self.add(Gleis { definition, position, streckenabschnitt })
    }

    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        position_neu: Position,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { definition, position, .. } =
            T::get_map_mut(&mut self.maps).get_mut(&gleis_id).ok_or(GleisEntferntError)?;
        // calculate absolute position for current AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        // calculate absolute position for new AnchorPoints
        let anchor_points_neu = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position_neu.transformation(anchor_position),
                richtung: position_neu.winkel + richtung,
            },
        );
        // store new position
        *position = position_neu;
        // delete old from anchor_points
        anchor_points.for_each(|_name, anchor| {
            self.anchor_points.remove(AnyId::from_ref(gleis_id), &anchor);
        });
        // add new to anchor_points
        anchor_points_neu.for_each(|_name, anchor| {
            self.anchor_points.insert(AnyId::from_ref(gleis_id), anchor.clone())
        });
        // trigger redraw
        self.canvas.clear();
        // return value
        Ok(anchor_points_neu)
    }

    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub fn relocate_attach<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let position = {
            let Gleis { definition, .. } =
                T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
            Position::attach_position(definition, anchor_name, target_anchor_point)
        };
        // move gleis to new position
        self.relocate(gleis_id, position)
    }

    /// Remove the Gleis associated the the GleisId.
    ///
    /// Only the first remove has an effect.
    /// Regardless, after a remove the associated Gleis is guaranteed to be removed.
    pub fn remove<T>(&mut self, gleis_id: GleisId<T>)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        if let Some(Gleis { definition, position, .. }) =
            T::get_map_mut(&mut self.maps).remove(&gleis_id)
        {
            // delete from anchor_points
            definition.anchor_points().for_each(|_name, anchor| {
                self.anchor_points.remove(
                    AnyId::from_ref(&gleis_id),
                    &anchor::Anchor {
                        position: position.transformation(anchor.position),
                        richtung: position.winkel + anchor.richtung,
                    },
                );
            });
            // trigger redraw
            self.canvas.clear();
        }
    }

    pub(crate) fn streckenabschnitt_für_id<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<&mut (Streckenabschnitt, Fließend)>, GleisEntferntError>
    where
        T: GleiseMap<Z>,
    {
        if let Some(Gleis { streckenabschnitt, .. }) = T::get_map_mut(&mut self.maps).get(&gleis_id)
        {
            Ok(if let Some(name) = streckenabschnitt {
                let name_clone = name.clone();
                drop(name);
                self.streckenabschnitt_mut(&name_clone)
            } else {
                None
            })
        } else {
            Err(GleisEntferntError)
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
