//! Steuerungs-Struktur eines Gleises, die bei [drop](Drop::drop) ein Neuzeichnen des
//! [Canvas](crate::application::touch_canvas::Canvas) erzwingt.

use std::{fmt::Debug, sync::Arc};

use parking_lot::Mutex;
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
    steuerung::{self, geschwindigkeit::Leiter},
    typen::canvas::Cache,
};

/// Mutable Referenz auf die Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein Neuzeichen des Canvas (Cache) ausgelöst.
#[derive(Debug)]
pub struct Steuerung<T> {
    steuerung: T,
    canvas: Arc<Mutex<Cache>>,
    verändert: bool,
}

impl<T> Drop for Steuerung<T> {
    fn drop(&mut self) {
        if self.verändert {
            self.canvas.lock().leeren()
        }
    }
}

impl<T> AsRef<T> for Steuerung<T> {
    fn as_ref(&self) -> &T {
        &self.steuerung
    }
}

impl<T> AsMut<T> for Steuerung<T> {
    fn as_mut(&mut self) -> &mut T {
        self.verändert = true;
        &mut self.steuerung
    }
}

impl<T> Steuerung<T> {
    /// Erstelle eine neue [Steuerung].
    pub fn neu(steuerung: T, canvas: Arc<Mutex<Cache>>) -> Self {
        Steuerung { steuerung, canvas, verändert: false }
    }
}

impl<T> Steuerung<&'_ mut Option<T>> {
    /// Erhalte den Wert der zugehörigen Option-Referenz und hinterlasse [None].
    pub fn take(&mut self) -> Option<T> {
        self.as_mut().take()
    }

    /// Füge einen Wert in die zugehörige Option-Referenz ein.
    /// Enthält diese bereits einen Wert wird dieser überschrieben.
    pub fn insert(&mut self, steuerung: T) -> &mut T {
        self.as_mut().insert(steuerung)
    }

    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Erhalte eine mutable Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_mut(&mut self) -> Option<&mut T> {
        self.as_mut().as_mut()
    }
}

type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;

// TODO Trait MitSteuerung erstellen
macro_rules! steuerung_weiche {
    ($name:ident, $type:ty, $map:ident, $richtung:ty, $anschlüsse:ty) => {
        /// Erhalte die [Steuerung] für das spezifizierte Gleis.
        pub fn $name<'t>(
            &'t mut self,
            gleis_id: &GleisId<$type>,
        ) -> Result<Steuerung<&'t mut OptionWeiche<$richtung, $anschlüsse>>, GleisIdFehler> {
            let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
            let Gleise { zustand, canvas, .. } = self;
            let Gleis { definition, position: _ } = &mut zustand
                .daten_mut(streckenabschnitt)?
                .$map
                .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
                .next()
                .ok_or(GleisIdFehler::GleisEntfernt)?
                .data;
            Ok(Steuerung::neu(&mut definition.steuerung, canvas.clone()))
        }
    };
}

impl<L: Leiter> Gleise<L> {
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
