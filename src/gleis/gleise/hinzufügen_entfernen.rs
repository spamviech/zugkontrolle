//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen.

use crate::{
    anschluss::Lager,
    gleis::gleise::{
        self,
        daten::{
            AnyGleis2, BewegenFehler2, EntfernenFehler2, HinzufügenFehler2,
            SetzteStreckenabschnittFehler2, SteuerungAktualisierenFehler2,
        },
        id::{AnyDefinitionIdSteuerung, AnyId, AnyIdSteuerung, AnyIdSteuerungSerialisiert},
        steuerung::SomeAktualisierenSender,
        Gehalten, Gleise, ModusDaten,
    },
    steuerung::{geschwindigkeit::Leiter, streckenabschnitt},
    typen::{canvas::Position, vektor::Vektor},
};

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Füge ein neues Gleis an der [Position] mit dem gewählten [Streckenabschnitt](streckenabschnitt::Streckenabschnitt) hinzu.
    pub(crate) fn hinzufügen2(
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
    pub(crate) fn hinzufügen_gehalten_bei_maus2(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung,
        halte_position: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId, HinzufügenFehler2>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let punkt = self.letzte_maus_position - halte_position;
        let winkel = -self.pivot.winkel;
        let gleis_id = self.hinzufügen2(
            definition_steuerung.clone(),
            Position { punkt, winkel },
            streckenabschnitt,
            einrasten,
        )?;
        if let ModusDaten::Bauen { gehalten: gehalten2, .. } = &mut self.modus {
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
            *gehalten2 = Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt: true });
        }
        Ok(gleis_id)
    }

    /// Entferne das [Gleis] assoziiert mit der [GleisId].
    pub(in crate::gleis::gleise) fn entfernen2(
        &mut self,
        gleis_id: impl Into<AnyId>,
    ) -> Result<AnyGleis2, EntfernenFehler2> {
        self.zustand.entfernen(gleis_id.into())
    }

    /// Bewege das gehaltene Gleis an die übergebene Position.
    pub(in crate::gleis::gleise) fn gehalten_bewegen(
        &mut self,
        canvas_pos: Vektor,
    ) -> Result<(), BewegenFehler2> {
        if let ModusDaten::Bauen { gehalten: gehalten2, .. } = &mut self.modus {
            if let Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt }) = gehalten2 {
                let punkt = canvas_pos - halte_position;
                let id = gleis_steuerung.id();
                self.zustand.bewegen(id, Position { punkt, winkel: *winkel }, true)?;
                *bewegt = true;
                self.erzwinge_neuzeichnen();
            }
        }
        Ok(())
    }

    /// Setzte (oder entferne) den [Streckenabschnitt](streckenabschnitt::Streckenabschnitt)
    /// für das [Gleis] assoziiert mit der [GleisId].
    ///
    /// Rückgabewert ist der [Name](streckenabschnitt::Name) des bisherigen
    /// [Streckenabschnittes](streckenabschnitt::Streckenabschnitt) (falls einer gesetzt war).
    pub fn setze_streckenabschnitt2(
        &mut self,
        gleis_id: impl Into<AnyId>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, SetzteStreckenabschnittFehler2> {
        self.zustand.setze_streckenabschnitt(gleis_id, streckenabschnitt)
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen2(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: AnyIdSteuerungSerialisiert,
    ) -> Result<(), SteuerungAktualisierenFehler2>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        self.zustand.steuerung_aktualisieren(
            lager,
            gleis_steuerung,
            SomeAktualisierenSender::from((self.sender.clone(), AktualisierenNachricht::from)),
        )
    }
}
