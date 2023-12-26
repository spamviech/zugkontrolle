//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen.

use std::{convert::identity, fmt::Debug};

use crate::{
    anschluss::{
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        Lager,
    },
    gleis::{
        gleise::{
            self,
            daten::{
                AnyGleis2, BewegenFehler2, EntfernenFehler2, HinzufügenFehler2,
                SetzteStreckenabschnittFehler2, SteuerungAktualisierenFehler2,
            },
            id::{
                AnyDefinitionIdSteuerung2, AnyDefinitionIdSteuerungVerbindung2, AnyId2,
                AnyIdSteuerung2, AnyIdSteuerungSerialisiert2, AnyIdVerbindung2, GleisId,
            },
            nachricht::GleisSteuerung,
            steuerung::{MitSteuerung, SomeAktualisierenSender},
            AnschlüsseAnpassenFehler, Gehalten2, Gleise, ModusDaten,
        },
        verbindung::Verbindung,
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

    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung`
    /// mit dem gewählten [Streckenabschnitt](streckenabschnitt::Streckenabschnitt) hinzu.
    pub(in crate::gleis::gleise) fn hinzufügen_anliegend2(
        &mut self,
        definition_steuerung_verbindung: impl Into<AnyDefinitionIdSteuerungVerbindung2>,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        ziel_verbindung: Verbindung,
    ) -> Result<AnyId2, HinzufügenFehler2> {
        self.zustand2.hinzufügen_anliegend(
            definition_steuerung_verbindung.into(),
            streckenabschnitt,
            ziel_verbindung,
        )
    }

    /// Bewege ein [Gleis] an die neue [Position].
    pub(in crate::gleis::gleise) fn bewegen2(
        &mut self,
        gleis_id: impl Into<AnyId2>,
        position: Position,
        einrasten: bool,
    ) -> Result<(), BewegenFehler2> {
        self.zustand2.bewegen(gleis_id.into(), position, einrasten)
    }

    /// Bewege ein [Gleis], so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(in crate::gleis::gleise) fn bewegen_anliegend2(
        &mut self,
        id_verbindung: impl Into<AnyIdVerbindung2>,
        ziel_verbindung: Verbindung,
    ) -> Result<(), BewegenFehler2> {
        self.zustand2.bewegen_anliegend(id_verbindung.into(), ziel_verbindung)
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

    #[allow(single_use_lifetimes)]
    fn gleis_anschlüsse_anpassen<T, W, S>(
        &mut self,
        id: GleisId<T>,
        anschlüsse_serialisiert: Option<S>,
        lager: &mut Lager,
        move_arg: <S as Reserviere<W>>::MoveArg,
    ) -> Result<(), AnschlüsseAnpassenFehler>
    where
        T: for<'t> MitSteuerung<Steuerung = Option<W>>,
        W: Serialisiere<S>,
        S: Debug + Reserviere<W, RefArg = (), MutRefArg = ()>,
        <S as Reserviere<W>>::MoveArg: Clone,
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        use Ergebnis::*;
        self.mit_steuerung_mut(
            &id,
            (self.sender.clone(), AktualisierenNachricht::from),
            |mut steuerung| {
                let anschlüsse_serialisiert =
                    if let Some(anschlüsse_serialisiert) = anschlüsse_serialisiert {
                        anschlüsse_serialisiert
                    } else {
                        let _ = steuerung.take();
                        return Ok(());
                    };
                let (steuerung_serialisiert, anschlüsse) = if let Some(s) = steuerung.take() {
                    (Some(s.serialisiere()), s.anschlüsse())
                } else {
                    (None, Anschlüsse::default())
                };
                let (fehler, anschlüsse) = match anschlüsse_serialisiert.reserviere(
                    lager,
                    anschlüsse,
                    move_arg.clone(),
                    &(),
                    &mut (),
                ) {
                    Wert { anschluss, .. } => {
                        let _ = steuerung.insert(anschluss);
                        return Ok(());
                    },
                    FehlerMitErsatzwert { anschluss, fehler, mut anschlüsse } => {
                        anschlüsse.anhängen(anschluss.anschlüsse());
                        (fehler, anschlüsse)
                    },
                    Fehler { fehler, anschlüsse } => (fehler, anschlüsse),
                };
                let mut wiederherstellen_fehler = None;
                if let Some(steuerung_serialisiert) = steuerung_serialisiert {
                    let serialisiert_string = format!("{steuerung_serialisiert:?}");
                    match steuerung_serialisiert.reserviere(
                        lager,
                        anschlüsse,
                        move_arg,
                        &(),
                        &mut (),
                    ) {
                        Wert { anschluss, .. } => {
                            let _ = steuerung.insert(anschluss);
                        },
                        FehlerMitErsatzwert { anschluss, fehler, .. } => {
                            let _ = steuerung.insert(anschluss);
                            wiederherstellen_fehler = Some((fehler, serialisiert_string));
                        },
                        Fehler { fehler, .. } => {
                            wiederherstellen_fehler = Some((fehler, serialisiert_string))
                        },
                    }
                }
                Err(AnschlüsseAnpassenFehler::Deserialisieren { fehler, wiederherstellen_fehler })
            },
        )
        .map_err(AnschlüsseAnpassenFehler::from)
        .and_then(identity)
        // TODO and_then(identity) is actually flatten, but this hasn't been stabilized yet
        // https://doc.rust-lang.org/src/core/result.rs.html#1739
    }

    /// Passe die Anschlüsse für ein Gleis an.
    pub fn anschlüsse_anpassen(
        &mut self,
        lager: &mut Lager,
        gleis_steuerung: GleisSteuerung,
    ) -> Result<(), AnschlüsseAnpassenFehler>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let aktualisieren_sender =
            SomeAktualisierenSender::from((self.sender.clone(), AktualisierenNachricht::from));
        match gleis_steuerung {
            GleisSteuerung::Gerade(id, anschlüsse_serialisiert) => self.gleis_anschlüsse_anpassen(
                id,
                anschlüsse_serialisiert,
                lager,
                aktualisieren_sender,
            ),
            GleisSteuerung::Kurve(id, anschlüsse_serialisiert) => self.gleis_anschlüsse_anpassen(
                id,
                anschlüsse_serialisiert,
                lager,
                aktualisieren_sender,
            ),
            GleisSteuerung::Weiche(id, anschlüsse_serialisiert) => self.gleis_anschlüsse_anpassen(
                id,
                anschlüsse_serialisiert,
                lager,
                aktualisieren_sender,
            ),
            GleisSteuerung::DreiwegeWeiche(id, anschlüsse_serialisiert) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::KurvenWeiche(id, anschlüsse_serialisiert) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::SKurvenWeiche(id, anschlüsse_serialisiert) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::Kreuzung(id, anschlüsse_serialisiert) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
        }
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
