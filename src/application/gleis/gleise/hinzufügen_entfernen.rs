//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::fmt::Debug;

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                id::{AnyId, GleisId},
                maps::{Gleis, MapSelector},
                GleisEntferntError, Gleise, Grabbed, ModusDaten,
            },
            verbindung,
        },
        typen::*,
    },
    lookup::Lookup,
    steuerung::{streckenabschnitt, Streckenabschnitt},
    zugtyp::Zugtyp,
};

impl<Z: Zugtyp> Gleise<Z> {
    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Add a new gleis to its position.
    pub(crate) fn add<T>(&mut self, gleis: Gleis<T>) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + MapSelector<Z>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { definition, position, .. } = &gleis;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&verbindung::Verbindung { position: anchor_position, richtung }| verbindung::Verbindung {
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
        self.maps.get_map_mut().insert(gleis_id.clone(), gleis);
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
        T: Debug + Zeichnen + MapSelector<Z>,
        GleisId<T>: Into<AnyId<Z>>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
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

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub(crate) fn add_attach<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        anchor_name: &T::AnchorName,
        target_anchor_point: verbindung::Verbindung,
    ) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + MapSelector<Z>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        // calculate new position
        let position = Position::attach_position(&definition, anchor_name, target_anchor_point);
        // add new gleis
        self.add(Gleis { definition, position, streckenabschnitt })
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub(crate) fn relocate<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        position_neu: Position,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + MapSelector<Z>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let Gleis { definition, position, .. } =
            self.maps.get_map_mut().get_mut(&gleis_id).ok_or(GleisEntferntError)?;
        // calculate absolute position for current AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&verbindung::Verbindung { position: anchor_position, richtung }| verbindung::Verbindung {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        // calculate absolute position for new AnchorPoints
        let anchor_points_neu = definition.anchor_points().map(
            |&verbindung::Verbindung { position: anchor_position, richtung }| verbindung::Verbindung {
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

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub(crate) fn relocate_attach<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        anchor_name: &T::AnchorName,
        target_anchor_point: verbindung::Verbindung,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + MapSelector<Z>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let position = {
            let Gleis { definition, .. } =
                self.maps.get_map().get(&gleis_id).ok_or(GleisEntferntError)?;
            Position::attach_position(definition, anchor_name, target_anchor_point)
        };
        // move gleis to new position
        self.relocate(gleis_id, position)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Remove the Gleis associated the the GleisId.
    ///
    /// Only the first remove has an effect.
    /// Regardless, after a remove the associated Gleis is guaranteed to be removed.
    pub(crate) fn remove<T>(&mut self, gleis_id: GleisId<T>)
    where
        T: Debug + Zeichnen + MapSelector<Z>,
        T::AnchorPoints: verbindung::Lookup<T::AnchorName>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        if let Some(Gleis { definition, position, .. }) = self.maps.get_map_mut().remove(&gleis_id)
        {
            // delete from anchor_points
            definition.anchor_points().for_each(|_name, anchor| {
                self.anchor_points.remove(
                    AnyId::from_ref(&gleis_id),
                    &verbindung::Verbindung {
                        position: position.transformation(anchor.position),
                        richtung: position.winkel + anchor.richtung,
                    },
                );
            });
            // trigger redraw
            self.canvas.clear();
        }
    }

    pub(crate) fn streckenabschnitt_für_id<T: MapSelector<Z>>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<&mut (Streckenabschnitt, Fließend)>, GleisEntferntError> {
        if let Some(Gleis { streckenabschnitt, .. }) = self.maps.get_map().get(&gleis_id) {
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
