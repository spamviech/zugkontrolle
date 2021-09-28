//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::{fmt::Debug, marker::PhantomData};

use log::error;
use rstar::{
    primitives::{GeomWithData, Rectangle},
    RTreeObject,
};

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                daten::{DatenAuswahl, Gleis, SelectEnvelope},
                id::{AnyId, AnyIdRef, GleisId, GleisIdRef},
                Gehalten, GleisEntferntFehler, GleisIdFehler, Gleise, ModusDaten,
                StreckenabschnittEntferntFehler,
            },
            verbindung::{self, Verbindung},
        },
        typen::*,
    },
    lookup::Lookup,
    steuerung::{streckenabschnitt, Streckenabschnitt},
    zugtyp::Zugtyp,
};

impl<Z: Zugtyp> Gleise<Z> {
    #[zugkontrolle_derive::erstelle_daten_methoden]
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
            .insert(GeomWithData::new(rectangle.clone(), Gleis { definition, position }));
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

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(crate) fn hinzufügen_anliegend<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
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

    #[zugkontrolle_derive::erstelle_daten_methoden]
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
        let Gleis { definition, position: _ } = self.entfernen(gleis_id)?;
        // Füge an neuer Position hinzu.
        self.hinzufügen(definition, position_neu, streckenabschnitt).map_err(GleisIdFehler::from)
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(crate) fn bewegen_anliegend<T>(
        &mut self,
        gleis_id: GleisId<T>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<GleisId<T>, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let streckenabschnitt = gleis_id.streckenabschnitt.clone();
        // Entferne aktuellen Eintrag.
        let Gleis { definition, position: _ } = self.entfernen(gleis_id)?;
        // Füge Gleis an neuer Position hinzu.
        self.hinzufügen_anliegend(definition, streckenabschnitt, verbindung_name, ziel_verbindung)
            .map_err(GleisIdFehler::from)
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T>(&mut self, gleis_id: GleisId<T>) -> Result<Gleis<T>, GleisIdFehler>
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

    /// Wie `entfernen`, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline(always)]
    pub(in crate::application) fn entfernen_unit<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        self.entfernen(gleis_id)?;
        Ok(())
    }

    /// `Streckenabschnitt` und aktueller `Fließend`-Zustand
    /// für das Gleis mit der übergebenen `AnyId`.
    pub(crate) fn streckenabschnitt_für_id<T: DatenAuswahl<Z>>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<(&mut Streckenabschnitt, &mut Fließend)>, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        if let Some(name) = streckenabschnitt {
            let (streckenabschnitt, fließend, daten) = self
                .zustand
                .streckenabschnitte
                .get_mut(&name)
                .ok_or(GleisIdFehler::StreckenabschnittEntfernt(name))?;
            if daten
                .rstern_mut::<T>()
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

    /// Bewege das gehaltene Gleis an die übergebene Position.
    pub(crate) fn bewegen_gehalten<T: Debug + Zeichnen>(
        &mut self,
        gleis_id: GleisId<T>,
        punkt: Vektor,
    ) -> Result<AnyId<Z>, GleisIdFehler>
    where
        Z: Zugtyp,
        T: DatenAuswahl<Z>,
        GleisId<T>: Into<AnyId<Z>>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = &gleis_id;
        let daten = self.zustand.daten(&streckenabschnitt)?;
        let Gleis { definition: _, position } = &daten
            .rstern::<T>()
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let position_neu = Position { punkt, winkel: position.winkel };
        let gleis_id_neu = self.bewegen(gleis_id, position_neu)?;
        Ok(gleis_id_neu.into())
    }

    /// Lasse das gehaltene Gleis an einer überlappenden `Verbindung` einrasten.
    pub(in crate::application::gleis::gleise) fn einrasten_an_verbindung<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<(), GleisIdFehler>
    where
        Z: Zugtyp,
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        for<'t> AnyIdRef<'t, Z>: From<GleisIdRef<'t, T>>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom } = &gleis_id;
        let any_id = AnyIdRef::from(GleisIdRef {
            rectangle,
            streckenabschnitt: streckenabschnitt.as_ref(),
            phantom: *phantom,
        });
        let rstern = self.zustand.daten(&streckenabschnitt)?.rstern::<T>();
        let Gleis { definition, position } = &rstern
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let verbindungen = definition.verbindungen_an_position(position.clone());
        let mut snap = None;
        verbindungen.for_each(|verbindung_name, verbindung| {
            if snap.is_none() {
                let (mut überlappende, _gehalten) =
                    self.zustand.überlappende_verbindungen(verbindung, &any_id, None);
                snap = überlappende.next().map(|überlappend| (verbindung_name, überlappend));
            }
        });
        if let Some((einrasten_name, einrasten_verbindung)) = snap {
            self.bewegen_anliegend(gleis_id, &einrasten_name, einrasten_verbindung)?;
        };
        Ok(())
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
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
        // Entferne aktuellen Eintrag.
        let geom_with_data = bisherige_daten
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisIdFehler::GleisEntfernt)?;
        // Füge eintrag bei neuem Streckenabschnitt hinzu.
        match self.zustand.daten_mut(&name) {
            Ok(neue_daten) => {
                neue_daten.rstern_mut().insert(geom_with_data);
                Ok(GleisId { rectangle, streckenabschnitt: name, phantom })
            }
            Err(fehler) => {
                match self.zustand.daten_mut(&streckenabschnitt) {
                    Ok(bisherige_daten) => bisherige_daten.rstern_mut().insert(geom_with_data),
                    Err(wiederherstellen_fehler) => error!(
                        "Fehler bei Streckenabschnitt wiederherstellen: {:?}\nGleis entfernt: {:?}",
                        wiederherstellen_fehler, geom_with_data.data
                    ),
                }
                Err(fehler.into())
            }
        }
    }

    /// Wie `setzte_streckenabschnitt`, nur ohne Rückgabewert für Verwendung mit `with_any_id`
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
