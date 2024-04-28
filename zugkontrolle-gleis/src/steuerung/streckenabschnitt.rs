//! Ein [`Streckenabschnitt`] regelt die Stromzufuhr.

use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    polarität::Fließend,
    Fehler, Lager, OutputAnschluss, OutputSerialisiert,
};
use zugkontrolle_typen::farbe::Farbe;

/// Name eines [`Streckenabschnittes`](Streckenabschnitt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, formatter)
    }
}

/// Steuerung der Stromzufuhr.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Streckenabschnitt<Anschluss = Arc<Mutex<OutputAnschluss>>> {
    /// Die Farbe des Streckenabschnittes.
    farbe: Farbe,
    /// Die Anschlüsse des Streckenabschnittes.
    anschluss: Anschluss,
}

impl Streckenabschnitt {
    /// Erstelle einen neuen [`Streckenabschnitt`].
    #[must_use]
    pub fn neu(farbe: Farbe, anschluss: OutputAnschluss) -> Self {
        Streckenabschnitt { farbe, anschluss: Arc::new(Mutex::new(anschluss)) }
    }

    /// Schalte den Strom für einen [`Streckenabschnitt`].
    ///
    /// ## Errors
    ///
    /// Steuern des [`Anschlusses`](crate::anschluss::Anschluss) schlug fehl.
    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        self.lock_anschluss().einstellen(fließend)
    }

    /// Schalte den Strom eines [`Streckenabschnittes`](Streckenabschnitt).
    /// von [Fließend](Fließend::Fließend) auf [`Gesperrt`](Fließend::Gesperrt) und umgekehrt.
    ///
    /// ## Errors
    ///
    /// Steuern des [`Anschlusses`](crate::anschluss::Anschluss) schlug fehl.
    pub fn strom_umschalten(&mut self) -> Result<(), Fehler> {
        self.lock_anschluss().umschalten()
    }

    /// Aktuelle Einstellung eines [`Streckenabschnittes`](Streckenabschnitt).
    #[must_use]
    pub fn fließend(&self) -> Fließend {
        self.lock_anschluss().fließend()
    }

    /// Erhalte Zugriff auf den [`Anschluss`](crate::anschluss::Anschluss) zur Steuerung des Streckenabschnittes.
    ///
    /// Blockiert, bis der Zugriff erhalten wurde.
    pub fn lock_anschluss(&self) -> MutexGuard<'_, OutputAnschluss> {
        self.anschluss.lock()
    }
}

impl<Anschluss> Streckenabschnitt<Anschluss> {
    /// Die Farbe des Streckenabschnittes.
    #[must_use]
    pub fn farbe(&self) -> Farbe {
        self.farbe
    }

    /// Anpassen der Farbe des Streckenabschnittes.
    pub fn setze_farbe(&mut self, farbe: Farbe) {
        self.farbe = farbe;
    }
}

// Befolge Konvention TypName->TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Serialisierbare Repräsentation der Steuerung der Stromzufuhr.
pub type StreckenabschnittSerialisiert = Streckenabschnitt<OutputSerialisiert>;

impl StreckenabschnittSerialisiert {
    /// Erstelle einen neuen [`StreckenabschnittSerialisiert`].
    #[must_use]
    pub fn neu_serialisiert(farbe: Farbe, anschluss: OutputSerialisiert) -> Self {
        Streckenabschnitt { farbe, anschluss }
    }

    /// Der Anschluss des Streckenabschnittes.
    #[must_use]
    pub fn anschluss(self) -> OutputSerialisiert {
        self.anschluss
    }

    /// Eine Referenz des Anschlusses des Streckenabschnittes.
    #[must_use]
    pub fn anschluss_ref(&self) -> &OutputSerialisiert {
        &self.anschluss
    }

    /// Eine veränderliche Referenz des Anschlusses des Streckenabschnittes.
    pub fn anschluss_mut(&mut self) -> &mut OutputSerialisiert {
        &mut self.anschluss
    }
}

impl Serialisiere<StreckenabschnittSerialisiert> for Streckenabschnitt {
    fn serialisiere(&self) -> StreckenabschnittSerialisiert {
        StreckenabschnittSerialisiert {
            farbe: self.farbe,
            anschluss: self.lock_anschluss().serialisiere(),
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        match Arc::try_unwrap(self.anschluss) {
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

impl Reserviere<Streckenabschnitt> for StreckenabschnittSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut Lager,
        anschlüsse: Anschlüsse,
        move_arg: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Streckenabschnitt> {
        let Streckenabschnitt { anschluss: serialisiert, farbe } = self;
        serialisiert
            .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
            .konvertiere(|anschluss| Streckenabschnitt::neu(farbe, anschluss))
    }
}
