//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::{fmt::Debug, marker::PhantomData};

use rstar::primitives::{GeomWithData, Rectangle};

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                daten::{DatenAuswahl, Gleis},
                id::{AnyId, GleisId},
                GleisEntferntFehler, Gleise, Grabbed, ModusDaten, StreckenabschnittEntferntFehler,
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
    /// Füge ein neues Gleis an der `Position` mit dem gewählten `streckenabschnitt` hinzu.
    pub(crate) fn hinzufügen<T>(
        &mut self,
        definition: T,
        position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<GleisId<T>, StreckenabschnittEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        // Berechne Bounding Box.
        let mut rechteck = definition.rechteck();
        rechteck.verschiebe(&position.punkt);
        rechteck.respektiere_rotation(&position.winkel);
        let rectangle = Rectangle::from(rechteck);
        // Füge zu RStern hinzu.
        let mut daten = if let Some(name) = streckenabschnitt {
            self.zustand
                .streckenabschnitte
                .get_mut(&name)
                .map(|(_streckenabschnitt, _fließend, maps)| maps)
                .ok_or(StreckenabschnittEntferntFehler)?
        } else {
            &mut self.zustand.ohne_streckenabschnitt
        };
        daten
            .rstern_mut()
            .insert(GeomWithData::new(rectangle.clone(), (definition, position.winkel)));
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(GleisId { position: rectangle, streckenabschnitt, phantom: PhantomData })
    }

    /// Füge ein Gleis zur letzten bekannten Maus-Position,
    /// beschränkt durch die zuletzt bekannte Canvas-Größe hinzu.
    pub(crate) fn hinzufügen_grabbed_bei_maus<T>(
        &mut self,
        definition: T,
        grab_position: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<GleisId<T>, StreckenabschnittEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
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
        let result = self.add(
            definition,
            Position { punkt: canvas_position - grab_position, winkel: -self.pivot.winkel },
            streckenabschnitt.clone(),
        )?;
        if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
            let gleis_id = result.0.clone().into();
            *grabbed = Some(Grabbed {
                gleis_id,
                streckenabschnitt,
                grab_location: grab_position,
                moved: true,
            });
        }
        Ok(result)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub(crate) fn add_attach<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        anchor_name: &T::VerbindungName,
        target_anchor_point: verbindung::Verbindung,
    ) -> Result<GleisId<T>, StreckenabschnittEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        // calculate new position
        let position = Position::attach_position(&definition, anchor_name, target_anchor_point);
        // add new gleis
        self.add(definition, position, streckenabschnitt)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub(crate) fn relocate<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        position_neu: Position,
    ) -> Result<(), GleisEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let Gleis { definition, position, .. } = self
            .zustand
            .alle_gleise_maps_mut()
            .fold(None, |acc, (streckenabschnitt, maps)| {
                acc.or_else(move || maps.rstern_mut().get_mut(&gleis_id))
            })
            .ok_or(GleisEntferntFehler)?;
        // calculate absolute position for current Verbindungen
        let anchor_points = definition.anchor_points().map(
            |&verbindung::Verbindung { position: anchor_position, richtung }| {
                verbindung::Verbindung {
                    position: position.transformation(anchor_position),
                    richtung: position.winkel + richtung,
                }
            },
        );
        // calculate absolute position for new Verbindungen
        let anchor_points_neu = definition.anchor_points().map(
            |&verbindung::Verbindung { position: anchor_position, richtung }| {
                verbindung::Verbindung {
                    position: position_neu.transformation(anchor_position),
                    richtung: position_neu.winkel + richtung,
                }
            },
        );
        // store new position
        *position = position_neu;
        // delete old from anchor_points
        anchor_points.for_each(|_name, anchor| {
            self.anchor_points.entfernen(AnyId::from_ref(gleis_id), &anchor);
        });
        // add new to anchor_points
        anchor_points_neu.for_each(|_name, anchor| {
            self.anchor_points.hinzufügen(AnyId::from_ref(gleis_id), anchor.clone())
        });
        // trigger redraw
        self.canvas.leeren();
        // return value
        Ok(anchor_points_neu)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub(crate) fn relocate_attach<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        anchor_name: &T::VerbindungName,
        target_anchor_point: verbindung::Verbindung,
    ) -> Result<(), GleisEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let position = {
            let Gleis { definition, .. } = self
                .zustand
                .alle_gleise_maps_mut()
                .fold(None, |acc, (streckenabschnitt, maps)| {
                    acc.or_else(move || maps.rstern_mut().get_mut(&gleis_id))
                })
                .ok_or(GleisEntferntFehler)?;
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
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        if let Some(Gleis { definition, position, .. }) =
            self.zustand.alle_gleise_maps_mut().fold(None, |acc, (streckenabschnitt, maps)| {
                acc.or_else(|| maps.rstern_mut().remove(&gleis_id))
            })
        {
            // delete from anchor_points
            definition.anchor_points().for_each(|_name, anchor| {
                self.anchor_points.entfernen(
                    AnyId::from_ref(&gleis_id),
                    &verbindung::Verbindung {
                        position: position.transformation(anchor.position),
                        richtung: position.winkel + anchor.richtung,
                    },
                );
            });
            // trigger redraw
            self.canvas.leeren();
        }
    }

    pub(crate) fn streckenabschnitt_für_id<T: DatenAuswahl<Z>>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<(&mut Streckenabschnitt, &mut Fließend)>, GleisEntferntFehler> {
        if self.zustand.ohne_streckenabschnitt.rstern().contains_key(&gleis_id) {
            Ok(None)
        } else {
            for (streckenabschnitt, fließend, maps) in self.zustand.streckenabschnitte.values_mut()
            {
                if maps.rstern().contains_key(&gleis_id) {
                    return Ok(Some((streckenabschnitt, fließend)));
                }
            }
            Err(GleisEntferntFehler)
        }
    }
}
