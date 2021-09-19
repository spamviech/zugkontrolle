//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::{fmt::Debug, marker::PhantomData};

use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTreeObject, SelectionFunction, AABB,
};

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                daten::DatenAuswahl,
                id::{AnyId, GleisId},
                Gehalten, GleisEntferntFehler, GleisIdFehler, Gleise, GleiseDaten, ModusDaten,
                StreckenabschnittEntferntFehler,
            },
            verbindung,
        },
        typen::*,
    },
    steuerung::{streckenabschnitt, Streckenabschnitt},
    zugtyp::Zugtyp,
};

impl<Z: Zugtyp> Gleise<Z> {
    fn daten(
        &self,
        streckenabschnitt: &Option<streckenabschnitt::Name>,
    ) -> Result<&GleiseDaten<Z>, StreckenabschnittEntferntFehler> {
        Ok(if let Some(name) = streckenabschnitt {
            self.zustand
                .streckenabschnitte
                .get(name)
                .map(|(_streckenabschnitt, _fließend, maps)| maps)
                .ok_or(StreckenabschnittEntferntFehler)?
        } else {
            &self.zustand.ohne_streckenabschnitt
        })
    }
    fn daten_mut(
        &mut self,
        streckenabschnitt: &Option<streckenabschnitt::Name>,
    ) -> Result<&mut GleiseDaten<Z>, StreckenabschnittEntferntFehler> {
        Ok(if let Some(name) = streckenabschnitt {
            self.zustand
                .streckenabschnitte
                .get_mut(name)
                .map(|(_streckenabschnitt, _fließend, maps)| maps)
                .ok_or(StreckenabschnittEntferntFehler)?
        } else {
            &mut self.zustand.ohne_streckenabschnitt
        })
    }

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
        self.daten_mut(&streckenabschnitt)?
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
            Position { punkt: canvas_position - grab_position, winkel: -self.pivot.winkel },
            streckenabschnitt.clone(),
        )?;
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            let any_id = gleis_id.clone().into();
            *gehalten = Some(Gehalten { gleis_id: any_id, grab_position, bewegt: true });
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
        let definition = self.entfernen(gleis_id)?;
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
        let definition = self.entfernen(gleis_id)?;
        // Füge Gleis an neuer Position hinzu.
        self.hinzufügen_anliegend(definition, streckenabschnitt, verbindung_name, ziel_verbindung)
            .map_err(GleisIdFehler::from)
    }

    #[zugkontrolle_derive::erstelle_maps_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T>(&mut self, gleis_id: GleisId<T>) -> Result<T, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let GleisId { position, streckenabschnitt, phantom: _ } = gleis_id;
        let mut daten = self.daten_mut(&streckenabschnitt)?;
        // Entferne aktuellen Eintrag.
        let definition = daten
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(position.envelope()))
            .ok_or(GleisEntferntFehler)?
            .data
            .0;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        Ok(definition)
    }

    pub(crate) fn streckenabschnitt_für_id<T: DatenAuswahl<Z>>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<(&mut Streckenabschnitt, &mut Fließend)>, GleisIdFehler> {
        let GleisId { position, streckenabschnitt, phantom } = gleis_id;
        if let Some(name) = streckenabschnitt {
            let (streckenabschnitt, fließend, daten) = self
                .zustand
                .streckenabschnitte
                .get_mut(&name)
                .ok_or(GleisIdFehler::StreckenabschnittEntfernt)?;
            if daten
                .rstern::<T>()
                .locate_with_selection_function_mut(SelectEnvelope(position.envelope()))
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
}

/// SelectionFunction, die einen bestimmten Envelope sucht.
struct SelectEnvelope(AABB<Vektor>);
impl<T> SelectionFunction<T> for SelectEnvelope
where
    T: RTreeObject<Envelope = AABB<Vektor>>,
{
    fn should_unpack_parent(&self, envelope: &T::Envelope) -> bool {
        let self_upper = self.0.upper();
        let self_lower = self.0.lower();
        let upper = envelope.upper();
        let lower = envelope.lower();
        // der gesuchte Envelope muss komplett in den parent passen
        lower.x <= self_lower.x
            && lower.y <= self_lower.y
            && upper.x >= self_upper.x
            && upper.y >= self_upper.y
    }

    fn should_unpack_leaf(&self, leaf: &T) -> bool {
        self.0 == leaf.envelope()
    }
}
