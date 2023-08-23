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
            daten::{DatenAuswahl, Gleis, SelectEnvelope, Zustand},
            id::{AnyId, GleisId, StreckenabschnittId},
            nachricht::{mit_any_steuerung_id, GleisSteuerung},
            steuerung::{MitSteuerung, SomeAktualisierenSender},
            AnschlüsseAnpassenFehler, Gehalten, GleisIdFehler, Gleise, ModusDaten,
            StreckenabschnittIdFehler,
        },
        verbindung::{self, Verbindung},
    },
    steuerung::geschwindigkeit::Leiter,
    typen::{canvas::Position, vektor::Vektor, Zeichnen},
};

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Füge ein neues Gleis an der `Position` mit dem gewählten `streckenabschnitt` hinzu.
    pub(crate) fn hinzufügen<T: Debug + Zeichnen + DatenAuswahl>(
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
        T: Debug + Zeichnen + DatenAuswahl + for<'t> MitSteuerung<Steuerung = Option<R>>,
        <T as Zeichnen>::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Füge ein neues Gleis mit `verbindung_name` anliegend an `ziel_verbindung` hinzu.
    pub(crate) fn hinzufügen_anliegend<T: Debug + Zeichnen + DatenAuswahl>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<StreckenabschnittId>,
        verbindung_name: &<T as Zeichnen>::VerbindungName,
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Bewege ein Gleis an die neue position.
    pub(crate) fn bewegen<T: Debug + Zeichnen + DatenAuswahl>(
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Bewege ein Gleis, so dass `verbindung_name` mit `ziel_verbindung` anliegend ist.
    pub(crate) fn bewegen_anliegend<T: Debug + Zeichnen + DatenAuswahl>(
        &mut self,
        gleis_id: &mut GleisId<T>,
        verbindung_name: &<T as Zeichnen>::VerbindungName,
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

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Entferne das Gleis assoziiert mit der `GleisId`.
    pub(crate) fn entfernen<T: Debug + Zeichnen + DatenAuswahl>(
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

    /// Wie `entfernen`, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline(always)]
    pub(crate) fn entfernen_unit<T>(&mut self, gleis_id: GleisId<T>) -> Result<(), GleisIdFehler>
    where
        T: Debug + Zeichnen + DatenAuswahl,
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
    ) -> Result<(), GleisIdFehler> {
        if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
            if let Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt }) = gehalten {
                let punkt = canvas_pos - halte_position;
                let mut_ref = &mut self.zustand;
                mit_any_steuerung_id!(
                    gleis_steuerung,
                    Zustand::bewegen,
                    mut_ref,
                    Position { punkt, winkel: *winkel },
                    true
                )?;
                *bewegt = true;
                self.erzwinge_neuzeichnen();
            }
        }
        Ok(())
    }

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    pub(crate) fn setze_streckenabschnitt<T: Debug + Zeichnen + DatenAuswahl>(
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
            GleisSteuerung::Gerade((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::Kurve((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::Weiche((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::DreiwegeWeiche((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::KurvenWeiche((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::SKurvenWeiche((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
            GleisSteuerung::Kreuzung((id, anschlüsse_serialisiert)) => self
                .gleis_anschlüsse_anpassen(
                    id,
                    anschlüsse_serialisiert,
                    lager,
                    aktualisieren_sender,
                ),
        }
    }
}
