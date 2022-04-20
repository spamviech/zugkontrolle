//! Steuerungs-Struktur eines Gleises, die bei [drop](Drop::drop) ein Neuzeichnen des
//! [Canvas](crate::application::touch_canvas::Canvas) erzwingt.

use std::{
    fmt::{self, Debug, Formatter},
    sync::{mpsc::Sender, Arc},
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

// -> Ändern auf Nachricht schicken
/// Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
#[derive(Clone)]
pub struct Steuerung<T, F> {
    steuerung: T,
    canvas: Arc<Mutex<Cache>>,
    sende_nachricht: F,
}

// Explizite Implementierung, um einen stack-overflow zu vermeiden.
impl<T: Debug, F> Debug for Steuerung<T, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Steuerung")
            .field("steuerung", &self.steuerung)
            .field("canvas", &"<Cache>")
            .field("sende_nachricht", &"<funktion>")
            .finish()
    }
}

impl<T, F> AsRef<T> for Steuerung<T, F> {
    fn as_ref(&self) -> &T {
        &self.steuerung
    }
}

impl<T, F: FnMut()> AsMut<T> for Steuerung<T, F> {
    fn as_mut(&mut self) -> &mut T {
        self.canvas.lock().leeren();
        (self.sende_nachricht)();
        &mut self.steuerung
    }
}

impl<T, F> Steuerung<T, F> {
    /// Erstelle eine neue [Steuerung].
    pub fn neu(steuerung: T, canvas: Arc<Mutex<Cache>>, sende_nachricht: F) -> Self {
        Steuerung { steuerung, canvas, sende_nachricht }
    }

    /// Erzeuge eine neue [Steuerung], die nur einen Teil der Steuerung überwacht.
    pub fn konvertiere<S>(self, f: impl FnOnce(T) -> S) -> Steuerung<S, F> {
        let Steuerung { steuerung, canvas, sende_nachricht } = self;
        Steuerung { steuerung: f(steuerung), canvas, sende_nachricht }
    }
}

impl<T, F> Steuerung<&Option<T>, F> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(self) -> Option<Steuerung<T, F>> {
        let Steuerung { steuerung, canvas, sende_nachricht } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas, sende_nachricht })
        } else {
            None
        }
    }
}

impl<T, F: FnMut()> Steuerung<&mut Option<T>, F> {
    /// Erhalte den Wert der zugehörigen Option-Referenz und hinterlasse [None].
    pub fn take(&mut self) -> Option<T> {
        self.as_mut().take()
    }

    /// Füge einen Wert in die zugehörige Option-Referenz ein.
    /// Enthält diese bereits einen Wert wird dieser überschrieben.
    pub fn insert(&mut self, steuerung: T) -> &mut T {
        self.as_mut().insert(steuerung)
    }

    /// Erhalte eine mutable Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_mut(&mut self) -> Option<&mut T> {
        self.as_mut().as_mut()
    }
}

impl<'t, T, F> Steuerung<&'t mut Option<T>, F> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(self) -> Option<Steuerung<&'t mut T, F>> {
        let Steuerung { steuerung, canvas, sende_nachricht } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas, sende_nachricht })
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
    fn steuerung<F>(
        &'t self,
        canvas: Arc<Mutex<Cache>>,
        sende_nachricht: F,
    ) -> Steuerung<&'t Self::Steuerung, F>;
    /// Erzeuge eine [Steuerung]-Struktur, die bei [Veränderung](AsMut::as_mut)
    /// ein [Neuzeichnen des Canvas](Cache::leeren) auslöst.
    fn steuerung_mut<F>(
        &'t mut self,
        canvas: Arc<Mutex<Cache>>,
        sende_nachricht: F,
    ) -> Steuerung<&'t mut Self::Steuerung, F>;
}

/// Eine asynchrone Aktion hat eine Änderung des Zustands bewirkt.
#[derive(Debug, Clone, Copy)]
pub struct AsyncAktualisieren;

impl<L: Leiter> Gleise<L> {
    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn erhalte_steuerung<
        't,
        T: 't + MitSteuerung<'t> + DatenAuswahl,
        Nachricht: From<AsyncAktualisieren>,
    >(
        &'t self,
        gleis_id: &GleisId<T>,
        sender: Sender<Nachricht>,
    ) -> Result<Steuerung<&'t <T as MitSteuerung<'t>>::Steuerung, impl FnMut()>, GleisIdFehler>
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, .. } = self;
        let Gleis { definition, position: _ }: &Gleis<T> = &zustand
            .daten(streckenabschnitt)?
            .rstern()
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let sende_nachricht = || {
            let _ = sender.send(AsyncAktualisieren.into());
        };
        Ok(definition.steuerung(canvas.clone(), sende_nachricht))
    }

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn erhalte_steuerung_mut<
        't,
        T: 't + MitSteuerung<'t> + DatenAuswahl,
        Nachricht: From<AsyncAktualisieren>,
    >(
        &'t mut self,
        gleis_id: &GleisId<T>,
        sender: Sender<Nachricht>,
    ) -> Result<Steuerung<&'t mut <T as MitSteuerung<'t>>::Steuerung, impl FnMut()>, GleisIdFehler>
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, .. } = self;
        let Gleis { definition, position: _ }: &mut Gleis<T> = &mut zustand
            .daten_mut(streckenabschnitt)?
            .rstern_mut()
            .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        let sende_nachricht = || {
            let _ = sender.send(AsyncAktualisieren.into());
        };
        Ok(definition.steuerung_mut(canvas.clone(), sende_nachricht))
    }
}

type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;

macro_rules! impl_mit_steuerung {
    ($type: ty, $steuerung: ty, $ident: ident) => {
        impl<'t> MitSteuerung<'t> for $type {
            type Steuerung = $steuerung;
            #[inline(always)]
            fn steuerung<F>(
                &'t self,
                canvas: Arc<Mutex<Cache>>,
                sende_nachricht: F,
            ) -> Steuerung<&'t Self::Steuerung, F> {
                Steuerung::neu(&self.$ident, canvas, sende_nachricht)
            }
            #[inline(always)]
            fn steuerung_mut<F>(
                &'t mut self,
                canvas: Arc<Mutex<Cache>>,
                sende_nachricht: F,
            ) -> Steuerung<&'t mut Self::Steuerung, F> {
                Steuerung::neu(&mut self.$ident, canvas, sende_nachricht)
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
