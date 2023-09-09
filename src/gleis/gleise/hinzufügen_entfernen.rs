//! Methoden zum hinzufügen, verschieben und entfernen von Gleisen.

use std::{convert::identity, fmt::Debug};

use log::error;
use rstar::RTreeObject;

use crate::{
    anschluss::{
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        Lager,
    },
    gleis::{
        gleise::{
            self,
            daten::{
                AnyGleis2, BewegenFehler2, DatenAuswahl, EntfernenFehler2, Gleis,
                HinzufügenFehler2, SelectEnvelope, SetzteStreckenabschnittFehler2, Zustand,
            },
            id::{
                AnyDefinitionId2, AnyDefinitionIdSteuerung2, AnyDefinitionIdSteuerungVerbindung2,
                AnyGleisDefinitionId2, AnyId, AnyId2, AnyIdSteuerung2, AnyIdVerbindung2, GleisId,
                StreckenabschnittId,
            },
            nachricht::{mit_any_steuerung_id, AnyIdSteuerungSerialisiert2, GleisSteuerung},
            steuerung::{MitSteuerung, SomeAktualisierenSender},
            AnschlüsseAnpassenFehler, Gehalten, Gehalten2, GleisIdFehler, Gleise, ModusDaten,
            StreckenabschnittIdFehler,
        },
        verbindung::{self, Verbindung},
    },
    steuerung::{
        geschwindigkeit::{self, Leiter},
        streckenabschnitt,
    },
    typen::{canvas::Position, vektor::Vektor, Zeichnen},
};

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Füge ein neues Gleis an der `Position` mit dem gewählten `streckenabschnitt` hinzu.
    pub(crate) fn hinzufügen<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        definition: T,
        position: Position,
        streckenabschnitt: Option<StreckenabschnittId>,
        einrasten: bool,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let gleis_id =
            self.zustand.hinzufügen(definition, position, streckenabschnitt, einrasten)?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
        Ok(gleis_id)
    }

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
    pub(crate) fn hinzufügen_gehalten_bei_maus<T, R, S>(
        &mut self,
        definition: T,
        halte_position: Vektor,
        streckenabschnitt: Option<StreckenabschnittId>,
        einrasten: bool,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        GleisId<T>: Into<AnyId>,
        T: Debug + Zeichnen<()> + DatenAuswahl + for<'t> MitSteuerung<Steuerung = Option<R>>,
        <T as Zeichnen<()>>::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
        R: Serialisiere<S>,
        (GleisId<T>, Option<S>): Into<GleisSteuerung>,
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let punkt = self.letzte_maus_position - halte_position;
        let winkel = -self.pivot.winkel;
        let serialisiert = definition.steuerung().as_ref().map(Serialisiere::serialisiere);
        let gleis_id = self.hinzufügen(
            definition,
            Position { punkt, winkel },
            streckenabschnitt.map(|id| id.klonen()),
            einrasten,
        )?;
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            let gleis_steuerung = (gleis_id.klonen(), serialisiert).into();
            *gehalten = Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt: true });
        }
        Ok(gleis_id)
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(crate) fn hinzufügen_anliegend<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<StreckenabschnittId>,
        verbindung_name: &<T as Zeichnen<()>>::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<GleisId<T>, StreckenabschnittIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let gleis_id = self.zustand.hinzufügen_anliegend(
            definition,
            streckenabschnitt,
            verbindung_name,
            ziel_verbindung,
        )?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Bewege ein Gleis an die neue position.
    pub(crate) fn bewegen<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        position_neu: Position,
        einrasten: bool,
    ) -> Result<(), GleisIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        self.zustand.bewegen(gleis_id, position_neu, einrasten)?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
        Ok(())
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(crate) fn bewegen_anliegend<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        verbindung_name: &<T as Zeichnen<()>>::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Result<(), GleisIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        self.zustand.bewegen_anliegend(gleis_id, verbindung_name, ziel_verbindung)?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
        Ok(())
    }

    /// Bewege ein [Gleis], so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(in crate::gleis::gleise) fn bewegen_anliegend2(
        &mut self,
        id_verbindung: impl Into<AnyIdVerbindung2>,
        ziel_verbindung: Verbindung,
    ) -> Result<(), BewegenFehler2> {
        self.zustand2.bewegen_anliegend(id_verbindung.into(), ziel_verbindung)
    }

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Gleis<T>, GleisIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let gleis = self.zustand.entfernen(gleis_id)?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
        Ok(gleis)
    }

    /// Entferne das [Gleis] assoziiert mit der [GleisId].
    pub(in crate::gleis::gleise) fn entfernen2(
        &mut self,
        gleis_id: impl Into<AnyId2>,
    ) -> Result<AnyGleis2, EntfernenFehler2> {
        self.zustand2.entfernen(gleis_id.into())
    }

    /// Wie `entfernen`, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline(always)]
    pub(crate) fn entfernen_unit<T>(&mut self, gleis_id: GleisId<T>) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen<()> + DatenAuswahl,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let _gleis = self.zustand.entfernen(gleis_id)?;
        // Erzwinge Neuzeichnen
        self.erzwinge_neuzeichnen();
        // Rückgabewert
        Ok(())
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    pub(crate) fn setze_streckenabschnitt<T: Debug + Zeichnen<()> + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        streckenabschnitt_neu: Option<StreckenabschnittId>,
    ) -> Result<(), GleisIdFehler>
    where
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = &*gleis_id;
        let bisherige_daten = self.zustand.daten_mut(streckenabschnitt)?;
        // Entferne aktuellen Eintrag.
        let geom_with_data = bisherige_daten
            .rstern_mut::<T>()
            .remove_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .ok_or(GleisIdFehler::GleisEntfernt)?;
        // Füge Eintrag bei neuem Streckenabschnitt hinzu.
        match self.zustand.daten_mut(&streckenabschnitt_neu) {
            Ok(neue_daten) => {
                neue_daten.rstern_mut().insert(geom_with_data);
                gleis_id.streckenabschnitt = streckenabschnitt_neu;
                Ok(())
            },
            Err(fehler) => {
                let daten = match self.zustand.daten_mut(&streckenabschnitt) {
                    Ok(bisherige_daten) => bisherige_daten,
                    Err(wiederherstellen_fehler) => {
                        error!(
                        "Fehler bei Streckenabschnitt wiederherstellen: {:?}\nStreckenabschnitt für Gleis entfernt: {:?}",
                        wiederherstellen_fehler, geom_with_data.data
                    );
                        &mut self.zustand.ohne_streckenabschnitt
                    },
                };
                daten.rstern_mut().insert(geom_with_data);
                Err(fehler.into())
            },
        }
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
        arg: <S as Reserviere<W>>::Arg,
    ) -> Result<(), AnschlüsseAnpassenFehler>
    where
        T: for<'t> MitSteuerung<Steuerung = Option<W>> + DatenAuswahl,
        W: Serialisiere<S>,
        S: Debug + Reserviere<W>,
        <S as Reserviere<W>>::Arg: Clone,
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
                let (fehler, anschlüsse) =
                    match anschlüsse_serialisiert.reserviere(lager, anschlüsse, arg.clone()) {
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
                    match steuerung_serialisiert.reserviere(lager, anschlüsse, arg) {
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
    ) -> Result<(), AnschlüsseAnpassenFehler> {
        todo!("anschlüsse_anpassen")
    }
}
