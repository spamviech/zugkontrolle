//! Mock-Methoden oder re-export für rppal.

#[cfg(not(feature = "raspi"))]
use std::fmt::{self, Debug, Formatter};

#[cfg(not(feature = "raspi"))]
use parking_lot::{const_mutex, MappedMutexGuard, Mutex, MutexGuard};

#[cfg(feature = "raspi")]
// TODO wird nur für die Fallback-Implementierung benötigt.
use num_traits as _;

pub mod gpio;
pub mod i2c;
pub mod pwm;

#[cfg(not(feature = "raspi"))]
/// Ein Element, oder eine Funktion, die den initialen Wert erzeugt.
///
/// Hilfs-Typ für [`LazyMutex`].
enum ElementOderKonstruktor<T, F = fn() -> T> {
    /// Es ist ein Element enthalten.
    Element(T),
    /// Es wurde noch kein Element erzeugt.
    Konstruktor(F),
}

#[cfg(not(feature = "raspi"))]
impl<T: Debug, F> Debug for ElementOderKonstruktor<T, F> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ElementOderKonstruktor::Element(element) => {
                formatter.debug_tuple("Element").field(element).finish()
            },
            ElementOderKonstruktor::Konstruktor(_) => {
                formatter.debug_tuple("Konstruktor").field(&"<Funktion>").finish()
            },
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F> ElementOderKonstruktor<T, F> {
    /// Erhalte eine veränderliche Referenz auf das Element.
    ///
    /// ## Panics
    ///
    /// Wenn ein [`Konstruktor`](ElementOderKonstruktor::Konstruktor) enthalten war wird [`panic!`] aufgerufen.
    fn erhalte_element_mut_unchecked(&mut self) -> &mut T {
        match self {
            ElementOderKonstruktor::Element(element) => element,
            ElementOderKonstruktor::Konstruktor(_konstruktor) => {
                // Methode ist explizit partiell.
                #[allow(clippy::panic)]
                {
                    panic!(
                        "erhalte_element_mut_unchecked für eine Konstruktor-Variante aufgerufen!"
                    );
                }
            },
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F: FnOnce() -> T> ElementOderKonstruktor<T, F> {
    /// Stellt sicher, dass danach ein Wert enthalten ist.
    /// Wenn davor kein Wert enthalten ist wird der [`Konstruktor`](ElementOderKonstruktor::Konstruktor) ausgeführt,
    /// ansonsten bleibt das [`Element`](ElementOderKonstruktor::Element) unverändert.
    fn initialisiere_wenn_notwendig(&mut self) {
        take_mut::take(self, |element_oder_konstruktor| match element_oder_konstruktor {
            ElementOderKonstruktor::Element(_) => element_oder_konstruktor,
            ElementOderKonstruktor::Konstruktor(konstruktor) => {
                ElementOderKonstruktor::Element(konstruktor())
            },
        });
    }
}

#[cfg(not(feature = "raspi"))]
/// Mutex mit einem Wert, oder eine (potentiell nicht-konstanten) Funktion um den initialen Wert zu erzeugen.
#[derive(Debug)]
struct LazyMutex<T, F = fn() -> T>(Mutex<ElementOderKonstruktor<T, F>>);

#[cfg(not(feature = "raspi"))]
impl<T, F> LazyMutex<T, F> {
    /// Erzeuge einen neuen [`LazyMutex`].
    const fn neu(konstruktor: F) -> LazyMutex<T, F> {
        LazyMutex(const_mutex(ElementOderKonstruktor::Konstruktor(konstruktor)))
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F: FnOnce() -> T> LazyMutex<T, F> {
    /// [`Mutex::lock`]
    fn lock(&self) -> MappedMutexGuard<'_, T> {
        let mut write_guard = self.0.lock();
        // Stelle sicher, dass das Element initialisiert ist (write-Berechtigung wird sowieso benötigt).
        write_guard.initialisiere_wenn_notwendig();
        // Gebe eine WriteGuard auf das Element zurück.
        MutexGuard::map(write_guard, ElementOderKonstruktor::erhalte_element_mut_unchecked)
    }
}
