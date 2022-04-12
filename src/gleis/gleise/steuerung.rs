//! Steuerungs-Struktur eines Gleises, die bei [drop](Drop::drop) ein Neuzeichnen des
//! [Canvas](crate::application::touch_canvas::Canvas) erzwingt.

use std::{fmt::Debug, sync::Arc};

use parking_lot::Mutex;
use rstar::RTreeObject;

use crate::{
    gleis::{
        self,
        gleise::{
            daten::{DatenAuswahl, Gleis, SelectEnvelope},
            id::GleisId,
            GleisIdFehler, Gleise,
        },
    },
    steuerung::{self, geschwindigkeit::Leiter, kontakt::Kontakt},
    typen::canvas::Cache,
};

/// Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
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

/// Enthält eine Steuerung, die auf dem Canvas angezeigt wird.
pub trait MitSteuerung<'t, S: 't> {
    /// Erzeuge eine [Steuerung]-Struktur, die bei [Veränderung](AsMut::as_mut)
    /// ein [Neuzeichnen des Canvas](Cache::leeren) auslöst.
    fn steuerung(&'t mut self, canvas: Arc<Mutex<Cache>>) -> Steuerung<S>;
}

impl<L: Leiter> Gleise<L> {
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn erhalte_steuerung<'t, S: 't, T: 't + MitSteuerung<'t, S> + DatenAuswahl>(
        &'t mut self,
        gleis_id: &GleisId<T>,
    ) -> Result<Steuerung<S>, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, .. } = self;
        let Gleis { definition, position: _ }: &mut Gleis<T> = &mut zustand
            .daten_mut(streckenabschnitt)?
            .rstern_mut()
            .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        Ok(definition.steuerung(canvas.clone()))
    }
}

type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;

macro_rules! impl_mit_steuerung {
    ($type: ty, $steuerung: ty, $ident: ident) => {
        impl<'t> MitSteuerung<'t, &'t mut $steuerung> for $type {
            #[inline(always)]
            fn steuerung(&'t mut self, canvas: Arc<Mutex<Cache>>) -> Steuerung<&'t mut $steuerung> {
                Steuerung::neu(&mut self.$ident, canvas)
            }
        }
    };
}

macro_rules! impl_mit_steuerung_weiche {
    (gleis $(:: $pfad: ident)*, $type: ident $(,)?) => {
        impl_mit_steuerung! {
            gleis $(:: $pfad)* :: $type,
            OptionWeiche<gleis$(:: $pfad)*::Richtung, gleis$(:: $pfad)*::RichtungAnschlüsse>,
            steuerung
        }
    }
}

impl_mit_steuerung! {gleis::gerade::Gerade, Option<Kontakt>, kontakt}
impl_mit_steuerung! {gleis::kurve::Kurve, Option<Kontakt>, kontakt}
impl_mit_steuerung_weiche! {gleis::weiche::gerade, Weiche}
impl_mit_steuerung_weiche! {gleis::weiche::dreiwege, DreiwegeWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::kurve, KurvenWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::s_kurve, SKurvenWeiche}
impl_mit_steuerung_weiche! {gleis::kreuzung, Kreuzung}
