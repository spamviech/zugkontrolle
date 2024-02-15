//! Hilfs-Struktur, die bei [`AsMut`]-Zugriff eine [`Aktualisieren`]-Nachricht erzeugt.

use log::error;

use zugkontrolle_util::erstelle_sender_trait_existential;

use crate::{
    gerade, kreuzung, kurve,
    steuerung::{
        self,
        kontakt::{Kontakt, KontaktSerialisiert},
    },
    weiche,
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
/// Bei [`AsMut`]-Zugriff wird eine [`Aktualisieren`]-Nachricht geschickt.
#[derive(Debug, Clone)]
pub struct Steuerung<T> {
    /// Die Steuerung.
    steuerung: T,
    /// Sender um nach Änderungen eine [`Aktualisieren`]-Nachricht zu schicken.
    sender: SomeAktualisierenSender,
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
    pub fn konvertiere<'t, S>(&'t self, funktion: impl FnOnce(&'t T) -> S) -> Steuerung<S> {
        let Steuerung { steuerung, sender } = self;
        Steuerung { steuerung: funktion(steuerung), sender: sender.clone() }
    }

    /// Konsumiere die [`Steuerung`] und beende die Überwachung.
    pub fn konsumiere<S>(self, funktion: impl FnOnce(T) -> S) -> S {
        funktion(self.steuerung)
    }
}

impl<T> Steuerung<&Option<T>> {
    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    #[must_use]
    pub fn opt_as_ref(&self) -> Option<&T> {
        self.as_ref().as_ref()
    }

    /// Betrachte die [Steuerung] nur, wenn der enthaltene Wert [`Some`] ist.
    #[must_use]
    pub fn nur_some(&self) -> Option<Steuerung<&T>> {
        let Steuerung { steuerung, sender } = self;
        // steuerung related über `.as_ref().map()`
        #[allow(clippy::shadow_unrelated)]
        steuerung.as_ref().map(|steuerung| Steuerung { steuerung, sender: sender.clone() })
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
    #[must_use]
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
        // steuerung related über `.as_mut().map()`
        #[allow(clippy::shadow_unrelated)]
        steuerung.as_mut().map(|steuerung| Steuerung { steuerung, sender: sender.clone() })
    }
}

// TODO Wird das alles benötigt? Es werden aktuell nur die Typ-Aliase verwendet.
// TODO Behandeln erfordert Anpassen des public API.
#[allow(clippy::module_name_repetitions)]
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
    /// eine [`Aktualisieren`]-Nachricht erzeugt.
    fn steuerung_mut(
        &mut self,
        sender: impl 'static + AktualisierenSender,
    ) -> Steuerung<&mut Self::Steuerung>;
}

/// Implementiere den [`MitSteuerung`]-trait für den Typ `$($path)::*`.
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

/// Die Steuerung einer Weiche.
type OptionWeiche<Richtung, Anschlüsse> = Option<steuerung::weiche::Weiche<Richtung, Anschlüsse>>;
/// Serialisierbare Darstellung der Steuerung einer Weiche.
type OptionWeicheSerialisiert<Richtung, AnschlüsseSerialisiert> =
    Option<steuerung::weiche::WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>;

/// Spezialisierung von [`impl_mit_steuerung`] für Weichen.
macro_rules! impl_mit_steuerung_weiche {
    ($($pfad: ident)::*, $type: ident $(,)?) => {
        impl_mit_steuerung! {
            $($pfad::)* $type,
            OptionWeiche<$($pfad ::)* Richtung, $($pfad ::)* RichtungAnschlüsse>,
            OptionWeicheSerialisiert<$($pfad ::)* Richtung, $($pfad ::)* RichtungAnschlüsseSerialisiert>,
            steuerung,
        }
    }
}

impl_mit_steuerung! {gerade::Gerade, Option<Kontakt>, Option<KontaktSerialisiert>, kontakt}
impl_mit_steuerung! {kurve::Kurve, Option<Kontakt>, Option<KontaktSerialisiert>, kontakt}
impl_mit_steuerung_weiche! {weiche::gerade, Weiche}
impl_mit_steuerung! {
    weiche::dreiwege::DreiwegeWeiche,
    OptionWeiche<weiche::dreiwege::RichtungInformation, weiche::dreiwege::RichtungAnschlüsse>,
    OptionWeicheSerialisiert<weiche::dreiwege::RichtungInformation, weiche::dreiwege::RichtungAnschlüsseSerialisiert>,
    steuerung,
}
impl_mit_steuerung_weiche! {weiche::kurve, KurvenWeiche}
impl_mit_steuerung_weiche! {weiche::s_kurve, SKurvenWeiche}
impl_mit_steuerung_weiche! {kreuzung, Kreuzung}
