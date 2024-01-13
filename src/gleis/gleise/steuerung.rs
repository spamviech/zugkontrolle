//! Steuerungs-Struktur eines Gleises, die bei [`drop`](Drop::drop) ein Neuzeichnen des
//! [`Canvas`](iced::widget::canvas::Canvas) erzwingt.

use std::fmt::{self, Debug, Formatter};

use log::error;

use crate::{
    gleis,
    steuerung::{
        self,
        kontakt::{Kontakt, KontaktSerialisiert},
    },
    util::sender_trait::erstelle_sender_trait_existential,
};

/// Es gab eine Änderung, die Anzeige muss aktualisiert werden.
#[derive(Debug, Clone, Copy)]
pub struct Aktualisieren;

erstelle_sender_trait_existential! {
    (pub), AktualisierenSender,
    "Hilfs-Trait um existential types zu ermöglichen (Verstecke T).",
    SomeAktualisierenSender,
    "Ein beliebiger [AktualisierenSender].",
    Aktualisieren,
}

/// Steuerung eines Gleises.
/// Bei [`AsMut`]-Zugriff wird ein [Neuzeichen des Canvas](Cache::leeren) ausgelöst.
#[derive(Clone)]
pub struct Steuerung<T> {
    steuerung: T,
    sender: SomeAktualisierenSender,
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
        if let Err(fehler) = self.sender.send(Aktualisieren) {
            error!("Kanal für Aktualisieren-Nachrichten einer Steuerung getrennt: {fehler:?}");
        }
        &mut self.steuerung
    }
}

impl<T> Steuerung<T> {
    /// Erstelle eine neue [`Steuerung`].
    pub fn neu(steuerung: T, sender: impl Into<SomeAktualisierenSender>) -> Self {
        Steuerung { steuerung, sender: sender.into() }
    }

    /// Erzeuge eine neue [`Steuerung`], die nur einen Teil der Steuerung überwacht.
    pub fn konvertiere<'t, S>(&'t self, f: impl FnOnce(&'t T) -> S) -> Steuerung<S> {
        let Steuerung { steuerung, sender } = self;
        Steuerung { steuerung: f(steuerung), sender: sender.clone() }
    }

    /// Konsumiere die [`Steuerung`] und beende die Überwachung.
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

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [`Some`] ist.
    pub fn nur_some(&self) -> Option<Steuerung<&T>> {
        let Steuerung { steuerung, sender } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, sender: sender.clone() })
        } else {
            None
        }
    }
}

impl<T> Steuerung<&mut Option<T>> {
    /// Erhalte den Wert der zugehörigen Option-Referenz und hinterlasse [`None`].
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

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [`Some`] ist.
    pub fn nur_some(&mut self) -> Option<Steuerung<&mut T>> {
        let Steuerung { steuerung, sender } = self;
        if let Some(steuerung) = steuerung {
            Some(Steuerung { steuerung, sender: sender.clone() })
        } else {
            None
        }
    }
}

/// Enthält eine Steuerung, die auf dem Canvas angezeigt wird.
pub trait MitSteuerung {
    /// Self mit `()` als Steuerung.
    type SelfUnit;
    /// Die Steuerung für das Gleis.
    type Steuerung;
    /// Die serialisierte Steuerung für das Gleis.
    type Serialisiert;

    /// Erzeuge eine Referenz auf die Steuerung, ohne die Möglichkeit sie zu verändern.
    fn steuerung(&self) -> &Self::Steuerung;

    /// Erzeuge eine [Steuerung]-Struktur, die bei [`Veränderung`](AsMut::as_mut)
    /// ein [Neuzeichnen des Canvas](crate::typen::canvas::Cache::leeren) auslöst.
    fn steuerung_mut(
        &mut self,
        sender: impl 'static + AktualisierenSender,
    ) -> Steuerung<&mut Self::Steuerung>;
}

macro_rules! impl_mit_steuerung {
    ($($path: ident)::*, $steuerung: ty, $serialisiert: ty, $ident: ident $(,)?) => {
        impl MitSteuerung for $($path)::* {
            type SelfUnit = $($path)::* <()>;

            type Steuerung = $steuerung;

            type Serialisiert = $serialisiert;

            fn steuerung(& self) -> & Self::Steuerung {
                &self.$ident
            }

            fn steuerung_mut(
                & mut self,
                sender: impl 'static + AktualisierenSender,
            ) -> Steuerung<& mut Self::Steuerung> {
                Steuerung::neu(&mut self.$ident, sender)
            }
        }
    };
}

type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;
type OptionWeicheSerialisiert<Richtung, AnschlüsseSerialisiert> =
    Option<steuerung::weiche::WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>;

macro_rules! impl_mit_steuerung_weiche {
    (gleis $(:: $pfad: ident)*, $type: ident $(,)?) => {
        impl_mit_steuerung! {
            gleis $(:: $pfad)* :: $type,
            OptionWeiche<gleis$(:: $pfad)*::Richtung, gleis$(:: $pfad)*::RichtungAnschlüsse>,
            OptionWeicheSerialisiert<gleis$(:: $pfad)*::Richtung, gleis$(:: $pfad)*::RichtungAnschlüsseSerialisiert>,
            steuerung,
        }
    }
}

impl_mit_steuerung! {gleis::gerade::Gerade, Option<Kontakt>, Option<KontaktSerialisiert>, kontakt}
impl_mit_steuerung! {gleis::kurve::Kurve, Option<Kontakt>, Option<KontaktSerialisiert>, kontakt}
impl_mit_steuerung_weiche! {gleis::weiche::gerade, Weiche}
impl_mit_steuerung! {
    gleis::weiche::dreiwege::DreiwegeWeiche,
    OptionWeiche<gleis::weiche::dreiwege::RichtungInformation, gleis::weiche::dreiwege::RichtungAnschlüsse>,
    OptionWeicheSerialisiert<gleis::weiche::dreiwege::RichtungInformation, gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert>,
    steuerung,
}
impl_mit_steuerung_weiche! {gleis::weiche::kurve, KurvenWeiche}
impl_mit_steuerung_weiche! {gleis::weiche::s_kurve, SKurvenWeiche}
impl_mit_steuerung_weiche! {gleis::kreuzung, Kreuzung}
