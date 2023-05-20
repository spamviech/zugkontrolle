//! Steuerungs-Struktur eines Gleises, die bei [drop](Drop::drop) ein Neuzeichnen des
//! [Canvas](iced::widget::canvas::Canvas) erzwingt.

use std::{
    fmt::{self, Debug, Formatter},
    sync::Arc,
};

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
/// Bei [AsMut]-Zugriff wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
#[derive(Clone)]
pub struct Steuerung<T> {
    steuerung: T,
    canvas: Arc<Mutex<Cache>>,
}

// Explizite Implementierung, um einen stack-overflow zu vermeiden.
impl<T: Debug> Debug for Steuerung<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Steuerung")
            .field("steuerung", &self.steuerung)
            .field("canvas", &"<Cache>")
            .finish()
    }
}

impl<T> AsRef<T> for Steuerung<T> {
    fn as_ref(&self) -> &T {
        &self.steuerung
    }
}

impl<T> AsMut<T> for Steuerung<T> {
    fn as_mut(&mut self) -> &mut T {
        self.canvas.lock().leeren();
        &mut self.steuerung
    }
}

impl<T> Steuerung<T> {
    /// Erstelle eine neue [Steuerung].
    pub fn neu(steuerung: T, canvas: Arc<Mutex<Cache>>) -> Self {
        Steuerung { steuerung, canvas }
    }

    /// Erzeuge eine neue [Steuerung], die nur einen Teil der Steuerung überwacht.
    pub fn konvertiere<'t, S>(&'t self, f: impl FnOnce(&'t T) -> S) -> Steuerung<S> {
        let Steuerung { steuerung, canvas } = self;
        Steuerung { steuerung: f(steuerung), canvas: canvas.clone() }
    }

    /// Konsumiere die [Steuerung] und beende die Überwachung.
    #[inline(always)]
    pub fn konsumiere<S>(self, f: impl FnOnce(T) -> S) -> S {
        f(self.steuerung)
    }
}

impl<T> Steuerung<&Option<T>> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(&self) -> Option<Steuerung<&T>> {
        let Steuerung { steuerung, canvas } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas: canvas.clone() })
        } else {
            None
        }
    }
}

impl<T> Steuerung<&mut Option<T>> {
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

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(&mut self) -> Option<Steuerung<&mut T>> {
        let Steuerung { steuerung, canvas } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas: canvas.clone() })
        } else {
            None
        }
    }
}

/// Enthält eine Steuerung, die auf dem Canvas angezeigt wird.
pub trait MitSteuerung<'t> {
    /// Die Steuerung für das Gleis.
    type Steuerung: 't;
    /// Erzeuge eine [Steuerung]-Struktur, ohne die Möglichkeit sie zu verändern.
    fn steuerung(&'t self, canvas: Arc<Mutex<Cache>>) -> Steuerung<&'t Self::Steuerung>;
    /// Erzeuge eine [Steuerung]-Struktur, die bei [Veränderung](AsMut::as_mut)
    /// ein [Neuzeichnen des Canvas](Cache::leeren) auslöst.
    fn steuerung_mut(&'t mut self, canvas: Arc<Mutex<Cache>>)
        -> Steuerung<&'t mut Self::Steuerung>;
}

impl<L: Leiter> Gleise<L> {
    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn mit_steuerung<T: for<'t> MitSteuerung<'t> + DatenAuswahl, V>(
        &self,
        gleis_id: &GleisId<T>,
        f: impl FnOnce(Steuerung<&<T as MitSteuerung<'_>>::Steuerung>) -> V,
    ) -> Result<V, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, .. } = self;
        let guard = zustand.read();
        let Gleis { definition, position: _ }: &Gleis<T> = &guard
            .daten(streckenabschnitt)?
            .rstern()
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let steuerung = definition.steuerung(canvas.clone());
        Ok(f(steuerung))
    }

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn mit_steuerung_mut<T: for<'t> MitSteuerung<'t> + DatenAuswahl, V>(
        &mut self,
        gleis_id: &GleisId<T>,
        f: impl FnOnce(Steuerung<&mut <T as MitSteuerung<'_>>::Steuerung>) -> V,
    ) -> Result<V, GleisIdFehler> {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, .. } = self;
        let mut guard = zustand.write();
        let Gleis { definition, position: _ }: &mut Gleis<T> = &mut guard
            .daten_mut(streckenabschnitt)?
            .rstern_mut()
            .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let steuerung = definition.steuerung_mut(canvas.clone());
        Ok(f(steuerung))
    }
}

macro_rules! impl_mit_steuerung {
    ($type: ty, $steuerung: ty, $ident: ident $(,)?) => {
        impl<'t> MitSteuerung<'t> for $type {
            type Steuerung = $steuerung;
            #[inline(always)]
            fn steuerung(&'t self, canvas: Arc<Mutex<Cache>>) -> Steuerung<&'t Self::Steuerung> {
                Steuerung::neu(&self.$ident, canvas)
            }
            #[inline(always)]
            fn steuerung_mut(
                &'t mut self,
                canvas: Arc<Mutex<Cache>>,
            ) -> Steuerung<&'t mut Self::Steuerung> {
                Steuerung::neu(&mut self.$ident, canvas)
            }
        }
    };
}

type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;

macro_rules! impl_mit_steuerung_weiche {
    (gleis $(:: $pfad: ident)*, $type: ident $(,)?) => {
        impl_mit_steuerung! {
            gleis $(:: $pfad)* :: $type,
            OptionWeiche<gleis$(:: $pfad)*::Richtung, gleis$(:: $pfad)*::RichtungAnschlüsse>,
            steuerung,
        }
    }
}

impl_mit_steuerung! {gleis::gerade::Gerade, Option<Kontakt>, kontakt}
impl_mit_steuerung! {gleis::kurve::Kurve, Option<Kontakt>, kontakt}
impl_mit_steuerung_weiche! {gleis::weiche::gerade, Weiche}
impl_mit_steuerung! {
    gleis::weiche::dreiwege::DreiwegeWeiche,
    OptionWeiche<gleis::weiche::dreiwege::RichtungInformation, gleis::weiche::dreiwege::RichtungAnschlüsse>,
    steuerung,
}
impl_mit_steuerung_weiche! {gleis::weiche::kurve, KurvenWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::s_kurve, SKurvenWeiche}
impl_mit_steuerung_weiche! {gleis::kreuzung, Kreuzung}
