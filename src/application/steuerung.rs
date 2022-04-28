//! Steuerungs-Struktur eines Gleises, die bei [drop](Drop::drop) ein Neuzeichnen des
//! [Canvas](crate::application::touch_canvas::Canvas) erzwingt.

use std::{
    fmt::{self, Debug, Formatter},
    sync::{mpsc::Sender, Arc},
};

use parking_lot::Mutex;

use crate::{
    gleis,
    steuerung::{self, kontakt::Kontakt},
    typen::canvas::Cache,
};

// -> Ändern auf Nachricht schicken
/// Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
#[derive(Clone)]
pub struct Steuerung<T, Nachricht = AsyncAktualisieren> {
    steuerung: T,
    canvas: Option<Arc<Mutex<Cache>>>,
    sender: Sender<Nachricht>,
}

// Explizite Implementierung, um einen stack-overflow zu vermeiden.
impl<T: Debug, F> Debug for Steuerung<T, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Steuerung")
            .field("steuerung", &self.steuerung)
            .field("canvas", &self.canvas.as_ref().map(|_| "<Cache>"))
            .field("sender", &self.sender)
            .finish()
    }
}

impl<T, F> AsRef<T> for Steuerung<T, F> {
    fn as_ref(&self) -> &T {
        &self.steuerung
    }
}

/// Eine asynchrone Aktion hat eine Änderung des Zustands bewirkt.
#[derive(Debug, Clone, Copy)]
pub struct AsyncAktualisieren;

impl<T, Nachricht: From<AsyncAktualisieren>> AsMut<T> for Steuerung<T, Nachricht> {
    fn as_mut(&mut self) -> &mut T {
        if let Some(canvas) = &self.canvas {
            canvas.lock().leeren();
        }
        let _ = self.sender.send(AsyncAktualisieren.into());
        &mut self.steuerung
    }
}

impl<T, Nachricht> Steuerung<T, Nachricht> {
    /// Erstelle eine neue [Steuerung].
    pub fn neu(steuerung: T, canvas: Option<Arc<Mutex<Cache>>>, sender: Sender<Nachricht>) -> Self {
        Steuerung { steuerung, canvas, sender }
    }

    /// Erstelle eine neue [Steuerung] mit einem [Cache], der bei Änderung geleert wird.
    pub fn neu_mit_canvas(
        steuerung: T,
        canvas: Arc<Mutex<Cache>>,
        sender: Sender<Nachricht>,
    ) -> Self {
        Steuerung { steuerung, canvas: Some(canvas), sender }
    }

    /// Erstelle eine neue [Steuerung] ohne [Cache].
    pub fn neu_ohne_canvas(steuerung: T, sender: Sender<Nachricht>) -> Self {
        Steuerung { steuerung, canvas: None, sender }
    }

    /// Erzeuge eine neue [Steuerung], die nur einen Teil der Steuerung überwacht.
    pub fn konvertiere<S>(self, f: impl FnOnce(T) -> S) -> Steuerung<S, Nachricht> {
        let Steuerung { steuerung, canvas, sender } = self;
        Steuerung { steuerung: f(steuerung), canvas, sender }
    }
}

impl<T, Nachricht: From<AsyncAktualisieren>> Steuerung<Option<T>, Nachricht> {
    /// Erhalte eine veränderliche Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_mut(&mut self) -> Option<&mut T> {
        self.as_mut().as_mut()
    }
}

impl<T, Nachricht> Steuerung<Option<T>, Nachricht> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(self) -> Option<Steuerung<T, Nachricht>> {
        let Steuerung { steuerung, canvas, sender } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas, sender })
        } else {
            None
        }
    }
}

impl<'t, T, Nachricht> Steuerung<&'t Option<T>, Nachricht> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(self) -> Option<Steuerung<&'t T, Nachricht>> {
        let Steuerung { steuerung, canvas, sender } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas, sender })
        } else {
            None
        }
    }
}

impl<T, Nachricht: From<AsyncAktualisieren>> Steuerung<&mut Option<T>, Nachricht> {
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

impl<'t, T, Nachricht> Steuerung<&'t mut Option<T>, Nachricht> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }
    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [Some] ist.
    pub fn nur_some(self) -> Option<Steuerung<&'t mut T, Nachricht>> {
        let Steuerung { steuerung, canvas, sender } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, canvas, sender })
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
    fn steuerung<N>(
        &'t self,
        canvas: Option<Arc<Mutex<Cache>>>,
        sender: Sender<N>,
    ) -> Steuerung<&'t Self::Steuerung, N>;
    /// Erzeuge eine [Steuerung]-Struktur, die bei [Veränderung](AsMut::as_mut)
    /// ein [Neuzeichnen des Canvas](Cache::leeren) auslöst.
    fn steuerung_mut<N>(
        &'t mut self,
        canvas: Option<Arc<Mutex<Cache>>>,
        sender: Sender<N>,
    ) -> Steuerung<&'t mut Self::Steuerung, N>;
}

macro_rules! impl_mit_steuerung {
    ($type: ty, $steuerung: ty, $ident: ident) => {
        impl<'t> MitSteuerung<'t> for $type {
            type Steuerung = $steuerung;
            #[inline(always)]
            fn steuerung<N>(
                &'t self,
                canvas: Option<Arc<Mutex<Cache>>>,
                sender: Sender<N>,
            ) -> Steuerung<&'t Self::Steuerung, N> {
                Steuerung::neu(&self.$ident, canvas, sender)
            }
            #[inline(always)]
            fn steuerung_mut<N>(
                &'t mut self,
                canvas: Option<Arc<Mutex<Cache>>>,
                sender: Sender<N>,
            ) -> Steuerung<&'t mut Self::Steuerung, N> {
                Steuerung::neu(&mut self.$ident, canvas, sender)
            }
        }
    };
}

impl_mit_steuerung! {gleis::gerade::Gerade, Arc<Mutex<Option<Kontakt>>>, kontakt}
impl_mit_steuerung! {gleis::kurve::Kurve, Arc<Mutex<Option<Kontakt>>>, kontakt}

type OptionWeiche<Richtung, Anschlüsse> =
    Arc<Mutex<Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>>>;
macro_rules! impl_mit_steuerung_weiche {
    (gleis $(:: $pfad: ident)*, $type: ident $(,)?) => {
        impl_mit_steuerung! {
            gleis $(:: $pfad)* :: $type,
            OptionWeiche<gleis$(:: $pfad)*::Richtung, gleis$(:: $pfad)*::RichtungAnschlüsse>,
            steuerung
        }
    }
}
impl_mit_steuerung_weiche! {gleis::weiche::gerade, Weiche}
impl_mit_steuerung_weiche! {gleis::weiche::dreiwege, DreiwegeWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::kurve, KurvenWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::s_kurve, SKurvenWeiche}
impl_mit_steuerung_weiche! {gleis::kreuzung, Kreuzung}