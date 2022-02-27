//! Steuerungs-Struktur eines Gleises

use std::fmt::Debug;

use rstar::RTreeObject;

use crate::{
    gleis::{
        self,
        gleise::{
            daten::{Gleis, SelectEnvelope},
            id::GleisId,
            GleisIdFehler, Gleise,
        },
    },
    steuerung,
    typen::canvas::Cache,
};

/// Mutable Referenz auf die Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein Neuzeichen des Canvas (Cache) ausgelöst.
#[derive(Debug)]
pub struct Steuerung<'t, T> {
    steuerung: &'t mut Option<T>,
    canvas: &'t mut Cache,
    verändert: bool,
}

impl<T> Drop for Steuerung<'_, T> {
    fn drop(&mut self) {
        if self.verändert {
            self.canvas.leeren()
        }
    }
}

impl<'t, T> Steuerung<'t, T> {
    pub fn neu(steuerung: &'t mut Option<T>, canvas: &'t mut Cache) -> Self {
        Steuerung { steuerung, canvas, verändert: false }
    }
    /// Erhalte den Wert der zugehörigen Option-Referenz und hinterlasse None.
    pub fn take(&mut self) -> Option<T> {
        self.verändert = true;
        self.steuerung.take()
    }

    /// Füge einen Wert in die zugehörige Option-Referenz ein.
    /// Enthält diese bereits einen Wert wird dieser überschrieben.
    pub fn insert(&mut self, steuerung: T) -> &mut T {
        self.verändert = true;
        self.steuerung.insert(steuerung)
    }

    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn as_ref(&self) -> Option<&T> {
        self.steuerung.as_ref()
    }

    /// Erhalte eine mutable Referenz, falls ein Wert vorhanden ist.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        self.verändert = true;
        self.steuerung.as_mut()
    }
}

macro_rules! steuerung_weiche {
    ($name:ident, $type:ty, $map:ident, $richtung:ty, $anschlüsse:ty) => {
        pub fn $name<'t>(
            &'t mut self,
            gleis_id: &GleisId<$type>,
            // streckenabschnitt: &Option<steuerung::streckenabschnitt::Name>,
        ) -> Result<Steuerung<'t, steuerung::weiche::Weiche<$richtung, $anschlüsse>>, GleisIdFehler>
        {
            let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
            let Gleise { zustand, canvas, .. } = self;
            let Gleis { definition, position: _ } = &mut zustand
                .daten_mut(streckenabschnitt)?
                .$map
                .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
                .next()
                .ok_or(GleisIdFehler::GleisEntfernt)?
                .data;
            Ok(Steuerung::neu(&mut definition.steuerung, canvas))
        }
    };
}

impl<Leiter> Gleise<Leiter> {
    steuerung_weiche! {
        steuerung_weiche,
        gleis::weiche::gerade::Weiche,
        weichen,
        gleis::weiche::gerade::Richtung,
        gleis::weiche::gerade::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_dreiwege_weiche,
        gleis::weiche::dreiwege::DreiwegeWeiche,
        dreiwege_weichen,
        gleis::weiche::dreiwege::Richtung,
        gleis::weiche::dreiwege::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_kurven_weiche,
        gleis::weiche::kurve::KurvenWeiche,
        kurven_weichen,
        gleis::weiche::kurve::Richtung,
        gleis::weiche::kurve::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_s_kurven_weiche,
        gleis::weiche::s_kurve::SKurvenWeiche,
        s_kurven_weichen,
        gleis::weiche::s_kurve::Richtung,
        gleis::weiche::s_kurve::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_kreuzung,
        gleis::kreuzung::Kreuzung,
        kreuzungen,
        gleis::kreuzung::Richtung,
        gleis::kreuzung::RichtungAnschlüsse
    }
}
