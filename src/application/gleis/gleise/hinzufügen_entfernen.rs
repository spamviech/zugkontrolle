//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::{fmt::Debug, marker::PhantomData};

use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTreeObject,
};

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                daten::{DatenAuswahl, SelectEnvelope},
                id::{AnyId, GleisId},
                Gehalten, GleisEntferntFehler, GleisIdFehler, Gleise, ModusDaten,
                StreckenabschnittEntferntFehler,
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
        self.zustand
            .daten_mut(&streckenabschnitt)?
            .rstern_mut()
            .insert(GeomWithData::new(rectangle.clone(), (definition, position)));
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(GleisId { rectangle, streckenabschnitt, phantom: PhantomData })
    }

    /// Füge ein Gleis zur letzten bekannten Maus-Position,
    /// beschränkt durch die zuletzt bekannte Canvas-Größe hinzu.
    pub(crate) fn hinzufügen_gehalten_bei_maus<T>(
        &mut self,
        definition: T,
        halte_position: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<GleisId<T>, StreckenabschnittEntferntFehler>
    where
        GleisId<T>: Into<AnyId<Z>>,
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
        let gleis_id = self.hinzufügen(
            definition,
            Position { punkt: canvas_position - halte_position, winkel: -self.pivot.winkel },
            streckenabschnitt.clone(),
        )?;
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            let any_id = gleis_id.clone().into();
            *gehalten = Some(Gehalten { gleis_id: any_id, halte_position, bewegt: true });
        }
        Ok(gleis_id)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(crate) fn hinzufügen_anliegend<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: verbindung::Verbindung,
    ) -> Result<GleisId<T>, StreckenabschnittEntferntFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        // calculate new position
        let position = Position::attach_position(&definition, verbindung_name, ziel_verbindung);
        // add new gleis
        self.hinzufügen(definition, position, streckenabschnitt)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Bewege ein Gleis an die neue position.
    pub(crate) fn bewegen<T>(
        &mut self,
        gleis_id: GleisId<T>,
        position_neu: Position,
    ) -> Result<GleisId<T>, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let streckenabschnitt = gleis_id.streckenabschnitt.clone();
        // Entferne aktuellen Eintrag.
        let (definition, _position) = self.entfernen(gleis_id)?;
        // Füge an neuer Position hinzu.
        self.hinzufügen(definition, position_neu, streckenabschnitt).map_err(GleisIdFehler::from)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(crate) fn bewegen_anliegend<T>(
        &mut self,
        gleis_id: GleisId<T>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: verbindung::Verbindung,
    ) -> Result<GleisId<T>, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let streckenabschnitt = gleis_id.streckenabschnitt.clone();
        // Entferne aktuellen Eintrag.
        let (definition, _position) = self.entfernen(gleis_id)?;
        // Füge Gleis an neuer Position hinzu.
        self.hinzufügen_anliegend(definition, streckenabschnitt, verbindung_name, ziel_verbindung)
            .map_err(GleisIdFehler::from)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<(T, Position), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        // Entferne aktuellen Eintrag.
        let data = self
            .zustand
            .daten_mut(&streckenabschnitt)?
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisEntferntFehler)?
            .data;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        Ok(data)
    }

    pub(crate) fn streckenabschnitt_für_id<T: DatenAuswahl<Z>>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<(&mut Streckenabschnitt, &mut Fließend)>, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom } = gleis_id;
        if let Some(name) = streckenabschnitt {
            let (streckenabschnitt, fließend, daten) = self
                .zustand
                .streckenabschnitte
                .get_mut(&name)
                .ok_or(GleisIdFehler::StreckenabschnittEntfernt(name))?;
            if daten
                .rstern::<T>()
                .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
                .next()
                .is_some()
            {
                Ok(Some((streckenabschnitt, fließend)))
            } else {
                Err(GleisIdFehler::GleisEntfernt)
            }
        } else {
            Ok(None)
        }
    }

    pub(crate) fn bewegen_gehalten<T: Debug + Zeichnen>(
        &mut self,
        gleis_id: GleisId<T>,
        punkt: Vektor,
    ) -> Result<(), GleisIdFehler>
    where
        Z: Zugtyp,
        T: DatenAuswahl<Z>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom } = gleis_id;
        let daten = self.zustand.daten(&streckenabschnitt)?;
        let (_definition, position) = &daten
            .rstern::<T>()
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let position_neu = Position { punkt, winkel: position.winkel };
        self.bewegen(gleis_id, position_neu)?;
        Ok(())
    }

    pub(crate) fn einrasten_an_verbindung<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<(), GleisIdFehler>
    where
        Z: Zugtyp,
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom } = gleis_id;
        let rstern = self.zustand.daten(&streckenabschnitt)?.rstern::<T>();
        let (definition, position) = &rstern
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let verbindungen = definition.verbindungen_an_position(*position);
        let mut snap = None;
        verbindungen.for_each(|verbindung_name, verbindung| {
            snap = snap.or_else(|| {
                // FIXME alle daten/rstern überprüfen, nicht nur den eigenen!
                let kandidaten = rstern.locate_all_at_point(&verbindung.position);
                for kandidat in kandidaten {
                    if snap.is_some() || (kandidat.geom() == &rectangle) {
                        continue;
                    }
                    let kandidat_verbindungen =
                        kandidat.data.0.verbindungen_an_position(kandidat.data.1);
                    kandidat_verbindungen.for_each(|kandidat_name, kandidat_verbindung| {
                        snap = snap.or_else(|| {
                            if (verbindung.position - kandidat_verbindung.position).länge()
                                < Skalar(5.)
                            {
                                Some((kandidat_name, kandidat_verbindung))
                            } else {
                                None
                            }
                        });
                    });
                }
                None
            });
        });
        if let Some((snap_name, snap_verbindung)) = snap {
            self.bewegen_anliegend(gleis_id, &snap_name, *snap_verbindung)?;
        };
        Ok(())
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    pub(crate) fn setze_streckenabschnitt<T>(
        &mut self,
        gleis_id: GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<GleisId<T>, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom } = gleis_id;
        let bisherige_daten = self.zustand.daten_mut(&streckenabschnitt)?;
        let neue_daten = self.zustand.daten_mut(&name)?;
        // Entferne aktuellen Eintrag.
        let geom_with_data = bisherige_daten
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisEntferntFehler)?;
        // Füge eintrag bei neuem Streckenabschnitt hinzu.
        neue_daten.rstern_mut().insert(geom_with_data);
        Ok(GleisId { rectangle, streckenabschnitt: name, phantom })
    }

    /// Wie setzte_streckenabschnitt, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline(always)]
    pub(in crate::application) fn setze_streckenabschnitt_unit<T>(
        &mut self,
        gleis_id: GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        self.setze_streckenabschnitt(gleis_id, name)?;
        Ok(())
    }
}
