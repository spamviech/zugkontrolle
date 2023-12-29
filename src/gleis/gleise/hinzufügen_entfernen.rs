//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen.

use crate::{
    anschluss::Lager,
    gleis::gleise::{
        self,
        daten::{
            AnyGleis2, BewegenFehler2, EntfernenFehler2, HinzufügenFehler2,
            SetzteStreckenabschnittFehler2, SteuerungAktualisierenFehler2,
        },
        id::{AnyDefinitionIdSteuerung2, AnyId2, AnyIdSteuerung2, AnyIdSteuerungSerialisiert2},
        steuerung::SomeAktualisierenSender,
        Gehalten2, Gleise, ModusDaten,
    },
    steuerung::{geschwindigkeit::Leiter, streckenabschnitt},
    typen::{canvas::Position, vektor::Vektor},
};

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Füge ein neues Gleis an der [Position] mit dem gewählten [Streckenabschnitt](streckenabschnitt::Streckenabschnitt) hinzu.
    pub(crate) fn hinzufügen2(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung2,
        position: Position,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId2, HinzufügenFehler2> {
        self.zustand2.hinzufügen(definition_steuerung, position, streckenabschnitt, einrasten)
    }

    /// Füge ein Gleis zur letzten bekannten Maus-Position,
    /// beschränkt durch die zuletzt bekannte Canvas-Größe hinzu.
    pub(crate) fn hinzufügen_gehalten_bei_maus2(
        &mut self,
        definition_steuerung: AnyDefinitionIdSteuerung2,
        halte_position: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        einrasten: bool,
    ) -> Result<AnyId2, HinzufügenFehler2>
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
        if let ModusDaten::Bauen { gehalten2, .. } = &mut self.modus {
            let gleis_steuerung = match (&gleis_id, definition_steuerung) {
                (AnyId2::Gerade(id), AnyDefinitionIdSteuerung2::Gerade(_definition, steuerung)) => {
                    AnyIdSteuerung2::Gerade(id.clone(), steuerung)
                },
                (AnyId2::Kurve(id), AnyDefinitionIdSteuerung2::Kurve(_definition, steuerung)) => {
                    AnyIdSteuerung2::Kurve(id.clone(), steuerung)
                },
                (AnyId2::Weiche(id), AnyDefinitionIdSteuerung2::Weiche(_definition, steuerung)) => {
                    AnyIdSteuerung2::Weiche(id.clone(), steuerung)
                },
                (
                    AnyId2::DreiwegeWeiche(id),
                    AnyDefinitionIdSteuerung2::DreiwegeWeiche(_definition, steuerung),
                ) => AnyIdSteuerung2::DreiwegeWeiche(id.clone(), steuerung),
                (
                    AnyId2::KurvenWeiche(id),
                    AnyDefinitionIdSteuerung2::KurvenWeiche(_definition, steuerung),
                ) => AnyIdSteuerung2::KurvenWeiche(id.clone(), steuerung),
                (
                    AnyId2::SKurvenWeiche(id),
                    AnyDefinitionIdSteuerung2::SKurvenWeiche(_definition, steuerung),
                ) => AnyIdSteuerung2::SKurvenWeiche(id.clone(), steuerung),
                (
                    AnyId2::Kreuzung(id),
                    AnyDefinitionIdSteuerung2::Kreuzung(_definition, steuerung),
                ) => AnyIdSteuerung2::Kreuzung(id.clone(), steuerung),
                wert => unreachable!("Inkompatible GleisId und Steuerung: {wert:?}"),
            };
            *gehalten2 = Some(Gehalten2 { gleis_steuerung, halte_position, winkel, bewegt: true });
        }
        Ok(gleis_id)
    }

    /// Entferne das [Gleis] assoziiert mit der [GleisId].
    pub(in crate::gleis::gleise) fn entfernen2(
        &mut self,
        gleis_id: impl Into<AnyId2>,
    ) -> Result<AnyGleis2, EntfernenFehler2> {
        self.zustand2.entfernen(gleis_id.into())
    }

    /// Bewege das gehaltene Gleis an die übergebene Position.
    pub(in crate::gleis::gleise) fn gehalten_bewegen(
        &mut self,
        canvas_pos: Vektor,
    ) -> Result<(), BewegenFehler2> {
        if let ModusDaten::Bauen { gehalten2, .. } = &mut self.modus {
            if let Some(Gehalten2 { gleis_steuerung, halte_position, winkel, bewegt }) = gehalten2 {
                let punkt = canvas_pos - halte_position;
                let id = gleis_steuerung.id();
                self.zustand2.bewegen(id, Position { punkt, winkel: *winkel }, true)?;
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
        gleis_id: impl Into<AnyId2>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, SetzteStreckenabschnittFehler2> {
        self.zustand2.setze_streckenabschnitt(gleis_id, streckenabschnitt)
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen2(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: AnyIdSteuerungSerialisiert2,
    ) -> Result<(), SteuerungAktualisierenFehler2>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        self.zustand2.steuerung_aktualisieren(
            lager,
            gleis_steuerung,
            SomeAktualisierenSender::from((self.sender.clone(), AktualisierenNachricht::from)),
        )
    }
}
