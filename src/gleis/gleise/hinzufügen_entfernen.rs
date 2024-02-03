//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen.

use log::{error, info};

use crate::{
    anschluss::Lager,
    gleis::{
        gleise::{
            self,
            daten::{
                AnyGleis, BewegenFehler, EntfernenFehler, GleisNichtGefunden, HinzufügenFehler2,
                SetzteStreckenabschnittFehler, SteuerungAktualisierenFehler,
            },
            id::{AnyDefinitionIdSteuerung, AnyId, AnyIdSteuerung, AnyIdSteuerungSerialisiert},
            steuerung::SomeAktualisierenSender,
            Gehalten, Gleise, ModusDaten,
        },
        knopf::KlickQuelle,
    },
    steuerung::{geschwindigkeit::Leiter, streckenabschnitt},
    typen::{canvas::Position, vektor::Vektor},
};

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Füge ein neues Gleis an der [Position] mit dem gewählten
    /// [`Streckenabschnitt`](streckenabschnitt::Streckenabschnitt) hinzu.
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur [`DefinitionId`](crate::gleis::gleise::id::DefinitionId) gehörende
    /// Definition gefunden, oder es war keine [`GleisId`](crate::gleis::gleise::id::GleisId)
    /// verfügbar.
    pub(crate) fn hinzufügen(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung,
        position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId, HinzufügenFehler2> {
        self.zustand.hinzufügen(definition_steuerung, position, streckenabschnitt, einrasten)
    }

    /// Füge ein Gleis zur letzten bekannten Maus-Position,
    /// beschränkt durch die zuletzt bekannte Canvas-Größe hinzu.
    pub(crate) fn hinzufügen_gehalten_bei_maus(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung,
        klick_quelle: KlickQuelle,
        halte_position: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId, HinzufügenFehler2>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let punkt = self.letzte_maus_position - halte_position;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let winkel = -self.pivot.winkel;
        let gleis_id = self.hinzufügen(
            definition_steuerung.clone(),
            Position { punkt, winkel },
            streckenabschnitt,
            einrasten,
        )?;
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            let gleis_steuerung = match (&gleis_id, definition_steuerung) {
                (AnyId::Gerade(id), AnyDefinitionIdSteuerung::Gerade(_definition, steuerung)) => {
                    AnyIdSteuerung::Gerade(id.clone(), steuerung)
                },
                (AnyId::Kurve(id), AnyDefinitionIdSteuerung::Kurve(_definition, steuerung)) => {
                    AnyIdSteuerung::Kurve(id.clone(), steuerung)
                },
                (AnyId::Weiche(id), AnyDefinitionIdSteuerung::Weiche(_definition, steuerung)) => {
                    AnyIdSteuerung::Weiche(id.clone(), steuerung)
                },
                (
                    AnyId::DreiwegeWeiche(id),
                    AnyDefinitionIdSteuerung::DreiwegeWeiche(_definition, steuerung),
                ) => AnyIdSteuerung::DreiwegeWeiche(id.clone(), steuerung),
                (
                    AnyId::KurvenWeiche(id),
                    AnyDefinitionIdSteuerung::KurvenWeiche(_definition, steuerung),
                ) => AnyIdSteuerung::KurvenWeiche(id.clone(), steuerung),
                (
                    AnyId::SKurvenWeiche(id),
                    AnyDefinitionIdSteuerung::SKurvenWeiche(_definition, steuerung),
                ) => AnyIdSteuerung::SKurvenWeiche(id.clone(), steuerung),
                (
                    AnyId::Kreuzung(id),
                    AnyDefinitionIdSteuerung::Kreuzung(_definition, steuerung),
                ) => AnyIdSteuerung::Kreuzung(id.clone(), steuerung),
                wert => unreachable!("Inkompatible GleisId und Steuerung: {wert:?}"),
            };
            let bisher = gehalten.insert(
                klick_quelle,
                Gehalten { gleis_steuerung, halte_position, winkel, bewegt: true },
            );
            if let Some(bisher) = bisher {
                error!("Gehaltenes Gleis für {klick_quelle:?} ersetzt: {bisher:?}");
            } else {
                info!("Neues gehaltenes Gleis für {klick_quelle:?}.");
            }
        }
        Ok(gleis_id)
    }

    /// Entferne das [Gleis] assoziiert mit der [`GleisId`].
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur [`GleisId`](crate::gleis::gleise::id::GleisId) gehörendes Gleis gefunden.
    pub(in crate::gleis::gleise) fn entfernen(
        &mut self,
        gleis_id: impl Into<AnyId>,
    ) -> Result<AnyGleis, EntfernenFehler> {
        self.zustand.entfernen(gleis_id.into())
    }

    /// Bewege das gehaltene Gleis an die übergebene Position.
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur [`GleisId`](crate::gleis::gleise::id::GleisId) gehörendes Gleis gefunden.
    pub(in crate::gleis::gleise) fn gehalten_bewegen(
        &mut self,
        klick_quelle: &KlickQuelle,
        canvas_pos: Vektor,
    ) -> Result<(), BewegenFehler> {
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            if let Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt }) =
                gehalten.get_mut(klick_quelle)
            {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let punkt = canvas_pos - halte_position;
                let id = gleis_steuerung.id();
                self.zustand.bewegen(id, Position { punkt, winkel: *winkel }, true)?;
                *bewegt = true;
                self.erzwinge_neuzeichnen();
            }
        }
        Ok(())
    }

    /// Setzte (oder entferne) den [`Streckenabschnitt`](streckenabschnitt::Streckenabschnitt)
    /// für das [Gleis] assoziiert mit der [`GleisId`].
    ///
    /// Rückgabewert ist der [`Name`](streckenabschnitt::Name) des bisherigen
    /// [`Streckenabschnittes`](streckenabschnitt::Streckenabschnitt) (falls einer gesetzt war).
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur `gleis_id` gehörendes Gleis gefunden.
    pub fn setze_streckenabschnitt(
        &mut self,
        gleis_id: impl Into<AnyId>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, SetzteStreckenabschnittFehler> {
        self.zustand.setze_streckenabschnitt(gleis_id, streckenabschnitt)
    }

    /// Passe die Anschlüsse für ein Gleis an.
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur [`GleisId`](crate::gleis::gleise::id::GleisId) gehörendes Gleis gefunden.
    pub fn anschlüsse_anpassen(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: AnyIdSteuerungSerialisiert,
    ) -> Result<(), SteuerungAktualisierenFehler>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        self.zustand.steuerung_aktualisieren(
            lager,
            gleis_steuerung,
            SomeAktualisierenSender::from((self.sender.clone(), AktualisierenNachricht::from)),
        )
    }

    /// Sind für ein Gleis Anschlüsse definiert?
    ///
    /// ## Errors
    ///
    /// Es wurde kein zur [`GleisId`](crate::gleis::gleise::id::GleisId) gehörendes Gleis gefunden.
    pub fn hat_steuerung(&self, gleis: AnyId) -> Result<bool, GleisNichtGefunden> {
        self.zustand.hat_steuerung(gleis)
    }
}
