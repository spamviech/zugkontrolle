//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    mem,
    sync::{mpsc::Sender, Arc},
    thread::{sleep, JoinHandle},
    time::Duration,
};

use log::debug;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Anschlüsse, Reserviere, Serialisiere},
        polarität::Fließend,
        Fehler, OutputAnschluss,
    },
    gleis::gleise::steuerung::{SomeAktualisierenSender, Steuerung},
    steuerung::plan::async_ausführen,
    typen::MitName,
    util::nachschlagen::Nachschlagen,
};

/// Name einer [`Weiche`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

impl<R, A> MitName for Option<Weiche<R, A>> {
    fn name(&self) -> Option<&str> {
        self.as_ref().map(|weiche| weiche.name.0.as_str())
    }
}

// TODO Benötigt public API Anpassung, ersetzte durch "Getter"
#[allow(clippy::partial_pub_fields)]
// inklusive Kreuzung
/// [`Name`], aktuelle Richtung und Anschlüsse einer Weiche.
#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Weiche<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [`Weiche`].
    richtung: Arc<Mutex<Steuerung<Richtung>>>,
    /// Die Anschlüsse der Weiche.
    anschlüsse: Arc<Mutex<Anschlüsse>>,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erstelle eine neue [`Weichen-Steuerung`](Weiche).
    pub fn neu(
        name: Name,
        richtung: Richtung,
        anschlüsse: Anschlüsse,
        sender: impl Into<SomeAktualisierenSender>,
    ) -> Self {
        Weiche {
            name,
            richtung: Arc::new(Mutex::new(Steuerung::neu(richtung, sender))),
            anschlüsse: Arc::new(Mutex::new(anschlüsse)),
        }
    }
}

impl<Richtung: Clone, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erhalte die aktuelle Richtung einer [`Weiche`].
    #[must_use]
    pub fn richtung(&self) -> Richtung {
        self.richtung.lock().as_ref().clone()
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erhalte einen Teil der aktuellen Richtung einer [`Weiche`].
    pub fn richtung_ausschnitt<T: Clone>(&self, formatter: impl FnOnce(&Richtung) -> &T) -> T {
        formatter(self.richtung.lock().as_ref()).clone()
    }
}

// TODO Behandeln erfordert Anpassung des public APIs, umbenennen zu Steuerung?
#[allow(clippy::module_name_repetitions)]
/// Notwendige Information zum ermitteln der nächsten Richtung einer Weiche.
pub trait WeicheSteuerung<R>: MitRichtung<R> {
    /// Notwendige Information zum zurücksetzen bei Schalt-Fehler.
    type Zurücksetzen;

    /// Einstellen einer neuen Richtung.
    fn einstellen(&mut self, neue_richtung: R) -> Self::Zurücksetzen;

    /// Zurücksetzen nach einem Schalt-Fehler.
    fn zurücksetzen(&mut self, zurücksetzen: Self::Zurücksetzen);
}

impl<R: MitRichtung<R>> WeicheSteuerung<R> for R {
    type Zurücksetzen = R;

    fn einstellen(&mut self, neue_richtung: R) -> Self::Zurücksetzen {
        mem::replace(self, neue_richtung)
    }

    fn zurücksetzen(&mut self, zurücksetzen: Self::Zurücksetzen) {
        *self = zurücksetzen;
    }
}

impl<T, Anschlüsse> Weiche<T, Anschlüsse> {
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    ///
    /// ## Errors
    ///
    /// Ansteuerung eines [`Anschlusses`](crate::anschluss::Anschluss) schlug fehl.
    pub fn schalten<Richtung>(
        &mut self,
        richtung: Richtung,
        schalten_zeit: Duration,
    ) -> Result<(), Fehler>
    where
        T: WeicheSteuerung<Richtung>,
        Richtung: Clone,
        Anschlüsse: Nachschlagen<Richtung, OutputAnschluss>,
    {
        Self::schalten_aux(
            &self.richtung,
            &self.anschlüsse,
            richtung,
            schalten_zeit,
            None::<fn()>,
        )?;
        Ok(())
    }

    /// Implementierung für [`schalten`](Weiche::schalten).
    fn schalten_aux<Richtung>(
        richtung: &Arc<Mutex<Steuerung<T>>>,
        anschlüsse: &Arc<Mutex<Anschlüsse>>,
        neue_richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler>
    where
        T: WeicheSteuerung<Richtung>,
        Richtung: Clone,
        Anschlüsse: Nachschlagen<Richtung, OutputAnschluss>,
    {
        let mut richtung_guard = richtung.lock();
        let weiche_richtung = richtung_guard.as_mut();
        let richtung_zurücksetzen = weiche_richtung.einstellen(neue_richtung.clone());
        // richtung_guard freigeben, damit Zeichnen nicht blockiert wird.
        drop(richtung_guard);
        if let Some(aktualisieren) = aktualisieren {
            aktualisieren();
        }
        /// Rufe bei einem [`Err`]  [`zurücksetzten`](WeicheSteuerung::zurücksetzen) auf und geben den Fehler zurück.
        macro_rules! bei_fehler_zurücksetzen {
            ($result: expr) => {
                if let Err(fehler) = $result {
                    let mut richtung_guard = richtung.lock();
                    let weiche_richtung = richtung_guard.as_mut();
                    weiche_richtung.zurücksetzen(richtung_zurücksetzen);
                    return Err(fehler);
                }
            };
        }
        // Reserviere die Anschlüsse bis der gesamte Schaltvorgang abgeschlossen ist.
        let mut anschlüsse_guard = anschlüsse.lock();
        bei_fehler_zurücksetzen!(anschlüsse_guard
            .erhalte_mut(&neue_richtung)
            .einstellen(Fließend::Fließend));
        sleep(schalten_zeit);
        bei_fehler_zurücksetzen!(anschlüsse_guard
            .erhalte_mut(&neue_richtung)
            .einstellen(Fließend::Gesperrt));
        Ok(())
    }
}

impl<T, Anschlüsse> Weiche<T, Anschlüsse> {
    /// Schalte eine [`Weiche`] auf die übergebene `Richtung`.
    pub fn async_schalten<Richtung, Nachricht>(
        &mut self,
        richtung: Richtung,
        schalten_zeit: Duration,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()>
    where
        T: 'static + WeicheSteuerung<Richtung> + Send,
        Richtung: 'static + Clone + Send,
        Anschlüsse: 'static + Nachschlagen<Richtung, OutputAnschluss> + Send,
        Nachricht: 'static + Send,
    {
        let name_arc: Arc<str> = Arc::from(self.name.0.as_str());
        let name_arc_clone = Arc::clone(&name_arc);
        let sender_clone = sender.clone();
        let erzeuge_aktualisieren_nachricht_clone = erzeuge_aktualisieren_nachricht.clone();
        let sende_aktualisieren_nachricht =
            erzeuge_aktualisieren_nachricht_clone.map(|erzeuge_nachricht| {
                move || {
                    if let Err(fehler) = sender_clone.send(erzeuge_nachricht()) {
                        debug!(
                            "Kein Empfänger für Aktualisieren-Nachricht bei Schalten der Weiche {name_arc_clone}: {fehler:?}"
                        );
                    }
                }
            });
        // Selber Wert, übergeben als Argument.
        #[allow(clippy::shadow_unrelated)]
        let schalten_aux = |(richtung, anschlüsse): &mut _,
                            neue_richtung,
                            schalten_zeit,
                            sende_aktualisieren_nachricht| {
            Self::schalten_aux(
                richtung,
                anschlüsse,
                neue_richtung,
                schalten_zeit,
                sende_aktualisieren_nachricht,
            )
        };
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |_mutex_clone, fehler| erzeuge_fehler_nachricht(fehler),
            format!("für Schalten der Weiche {name_arc}"),
            schalten_aux(
                (Arc::clone(&self.richtung), Arc::clone(&self.anschlüsse)),
                richtung,
                schalten_zeit,
                sende_aktualisieren_nachricht
            )
        )
    }
}

// Folge Konvention TypName->TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Serialisierbare Repräsentation der Steuerung einer [`Weiche`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [`Weiche`].
    pub richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [`WeicheSerialisiert`].
    pub fn neu(name: Name, richtung: Richtung, anschlüsse: Anschlüsse) -> Self {
        WeicheSerialisiert { name, richtung, anschlüsse }
    }
}

impl<Richtung, T, S> Serialisiere<WeicheSerialisiert<Richtung, S>> for Weiche<Richtung, T>
where
    Richtung: Clone,
    T: Serialisiere<S>,
{
    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, S> {
        WeicheSerialisiert {
            name: self.name.clone(),
            richtung: self.richtung.lock().as_ref().clone(),
            anschlüsse: self.anschlüsse.lock().serialisiere(),
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match Arc::try_unwrap(self.anschlüsse) {
            Ok(mutex) => mutex.into_inner().anschlüsse(),
            Err(_arc) => {
                // while-Schleife (mit thread::yield bei Err) bis nur noch eine Arc-Referenz besteht
                // (Ok wird zurückgegeben) wäre möglich, kann aber zur nicht-Terminierung führen
                // Gebe stattdessen keine Anschlüsse zurück
                Anschlüsse::default()
            },
        }
    }
}

impl<Richtung, R, S> Reserviere<Weiche<Richtung, R>> for WeicheSerialisiert<Richtung, S>
where
    R: Serialisiere<S>,
    S: Reserviere<R, MoveArg = (), RefArg = (), MutRefArg = ()>,
{
    type MoveArg = SomeAktualisierenSender;
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        bekannte_anschlüsse: Anschlüsse,
        sender: SomeAktualisierenSender,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> de_serialisieren::Ergebnis<Weiche<Richtung, R>> {
        let WeicheSerialisiert { name, richtung, anschlüsse } = self;
        // Selber Wert `anschlüsse`, als Ergebnis von `reserviere`.
        #[allow(clippy::shadow_unrelated)]
        anschlüsse
            .reserviere(lager, bekannte_anschlüsse, (), ref_arg, mut_ref_arg)
            .konvertiere(|anschlüsse| Weiche::neu(name, richtung, anschlüsse, sender))
    }
}

/// Trait für Typen mit einer aktuellen Richtung.
pub trait MitRichtung<Richtung> {
    /// Erhalte die aktuelle Richtung.
    fn aktuelle_richtung(&self) -> Option<Richtung>;
}

impl<R> MitRichtung<R> for () {
    fn aktuelle_richtung(&self) -> Option<R> {
        None
    }
}

impl<R, T: MitRichtung<R>> MitRichtung<R> for Option<T> {
    fn aktuelle_richtung(&self) -> Option<R> {
        self.as_ref().and_then(MitRichtung::aktuelle_richtung)
    }
}

impl<R, T: Clone + MitRichtung<R>, A> MitRichtung<R> for Weiche<T, A> {
    fn aktuelle_richtung(&self) -> Option<R> {
        self.richtung().aktuelle_richtung()
    }
}
