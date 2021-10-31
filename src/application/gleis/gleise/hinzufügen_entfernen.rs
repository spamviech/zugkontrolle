//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen

use std::fmt::Debug;

use log::error;
use rstar::RTreeObject;

use crate::{
    anschluss::polarität::Fließend,
    application::{
        gleis::{
            gleise::{
                daten::{DatenAuswahl, Gleis, SelectEnvelope, Zustand},
                id::{mit_any_id, AnyId, AnyIdRef, GleisId, GleisIdRef, StreckenabschnittId},
                Gehalten, GleisIdFehler, Gleise, ModusDaten, StreckenabschnittIdFehler,
            },
            verbindung::{self, Verbindung},
        },
        typen::*,
    },
    steuerung::Streckenabschnitt,
    zugtyp::Zugtyp,
};

impl<Z: Zugtyp> Gleise<Z> {
    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Füge ein neues Gleis an der `Position` mit dem gewählten `streckenabschnitt` hinzu.
    pub(crate) fn hinzufügen<T>(
        &mut self,
        definition: T,
        position: Position,
        streckenabschnitt: Option<StreckenabschnittId>,
        einrasten: bool,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let gleis_id =
            self.zustand.hinzufügen(definition, position, streckenabschnitt, einrasten)?;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(gleis_id)
    }

    /// Füge ein Gleis zur letzten bekannten Maus-Position,
    /// beschränkt durch die zuletzt bekannte Canvas-Größe hinzu.
    pub(crate) fn hinzufügen_gehalten_bei_maus<T>(
        &mut self,
        definition: T,
        halte_position: Vektor,
        streckenabschnitt: Option<StreckenabschnittId>,
        einrasten: bool,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
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
            streckenabschnitt.map(|id| id.klonen()),
            einrasten,
        )?;
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            let any_id = gleis_id.klonen().into();
            *gehalten = Some(Gehalten { gleis_id: any_id, halte_position, bewegt: true });
        }
        Ok(gleis_id)
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(crate) fn hinzufügen_anliegend<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<StreckenabschnittId>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let gleis_id = self.zustand.hinzufügen_anliegend(
            definition,
            streckenabschnitt,
            verbindung_name,
            ziel_verbindung,
        )?;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(gleis_id)
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Bewege ein Gleis an die neue position.
    pub(crate) fn bewegen<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        position_neu: Position,
        einrasten: bool,
    ) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        self.zustand.bewegen(gleis_id, position_neu, einrasten)?;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(())
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(crate) fn bewegen_anliegend<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        self.zustand.bewegen_anliegend(gleis_id, verbindung_name, ziel_verbindung)?;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
        Ok(())
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T>(&mut self, gleis_id: GleisId<T>) -> Result<Gleis<T>, GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let data = self.zustand.entfernen(gleis_id)?;
        // Erzwinge Neuzeichnen
        self.canvas.leeren();
        // Rückgabewert
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
        if let Some(streckenabschnitt_id) = streckenabschnitt {
            let StreckenabschnittId { geschwindigkeit, name } = &streckenabschnitt_id;
            let (streckenabschnitt, fließend, daten) = self
                .zustand
                .streckenabschnitt_map_mut(geschwindigkeit.as_ref())?
                .get_mut(name)
                .ok_or(GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt_id))?;
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
    pub(in crate::application::gleis::gleise) fn gehalten_bewegen(
        &mut self,
        canvas_pos: Vektor,
    ) -> Result<(), GleisIdFehler> {
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            if let Some(Gehalten { gleis_id, halte_position, bewegt }) = gehalten {
                let punkt = canvas_pos - halte_position;
                mit_any_id!(gleis_id, Zustand::bewegen_an_punkt, &mut self.zustand, punkt, false)?;
                *bewegt = true;
                self.canvas.leeren();
            }
        }
        Ok(())
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Lasse das gehaltene Gleis an einer überlappenden `Verbindung` einrasten.
    /// Wenn keine überlappende Verbindung existiert wird das Gleis nicht bewegt (kein Fehler).
    #[inline(always)]
    pub(in crate::application::gleis::gleise) fn einrasten_an_verbindung<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
    ) -> Result<(), GleisIdFehler>
    where
        T: Zeichnen + DatenAuswahl<Z>,
        for<'t> AnyIdRef<'t, Z>: From<GleisIdRef<'t, T>>,
    {
        self.zustand.einrasten_an_verbindung(gleis_id)
    }

    #[zugkontrolle_derive::erstelle_daten_methoden]
    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    pub(crate) fn setze_streckenabschnitt<T>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        streckenabschnitt_neu: Option<StreckenabschnittId>,
    ) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl<Z>,
        T::Verbindungen: verbindung::Lookup<T::VerbindungName>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = &*gleis_id;
        let bisherige_daten = self.zustand.daten_mut(streckenabschnitt)?;
        // Entferne aktuellen Eintrag.
        let geom_with_data = bisherige_daten
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisIdFehler::GleisEntfernt)?;
        // Füge eintrag bei neuem Streckenabschnitt hinzu.
        match self.zustand.daten_mut(&streckenabschnitt_neu) {
            Ok(neue_daten) => {
                neue_daten.rstern_mut().insert(geom_with_data);
                gleis_id.streckenabschnitt = streckenabschnitt_neu;
                Ok(())
            }
            Err(fehler) => {
                let daten = match self.zustand.daten_mut(&streckenabschnitt) {
                    Ok(bisherige_daten) => bisherige_daten,
                    Err(wiederherstellen_fehler) => {
                        error!(
                        "Fehler bei Streckenabschnitt wiederherstellen: {:?}\nStreckenabschnitt für Gleis entfernt: {:?}",
                        wiederherstellen_fehler, geom_with_data.data
                    );
                        &mut self.zustand.ohne_streckenabschnitt
                    }
                };
                daten.rstern_mut().insert(geom_with_data);
                Err(fehler.into())
            }
        }
    }
}
