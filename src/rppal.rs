//! Mock-Methoden oder re-export für rppal.

#[cfg(not(feature = "raspi"))]
use std::fmt::{self, Debug, Formatter};

#[cfg(not(feature = "raspi"))]
use parking_lot::{const_mutex, MappedMutexGuard, Mutex, MutexGuard};

pub mod gpio;
pub mod i2c;
pub mod pwm;

#[cfg(not(feature = "raspi"))]
enum ElementOderKonstruktor<T, F = fn() -> T> {
    Element(T),
    Konstruktor(F),
}

#[cfg(not(feature = "raspi"))]
impl<T: Debug, F> Debug for ElementOderKonstruktor<T, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ElementOderKonstruktor::Element(element) => {
                f.debug_tuple("Element").field(element).finish()
            },
            ElementOderKonstruktor::Konstruktor(_) => {
                f.debug_tuple("Konstruktor").field(&"<Funktion>").finish()
            },
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F> ElementOderKonstruktor<T, F> {
    fn erhalte_element_mut_unchecked(&mut self) -> &mut T {
        match self {
            ElementOderKonstruktor::Element(element) => element,
            ElementOderKonstruktor::Konstruktor(_konstruktor) => {
                panic!("erhalte_element_mut_unchecked für eine Konstruktor-Variante aufgerufen!")
            },
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F: FnOnce() -> T> ElementOderKonstruktor<T, F> {
    fn initialisiere_wenn_notwendig(&mut self) {
        take_mut::take(self, |element_oder_konstruktor| match element_oder_konstruktor {
            ElementOderKonstruktor::Element(_) => element_oder_konstruktor,
            ElementOderKonstruktor::Konstruktor(konstruktor) => {
                ElementOderKonstruktor::Element(konstruktor())
            },
        })
    }
}

#[cfg(not(feature = "raspi"))]
#[derive(Debug)]
struct LazyMutex<T, F = fn() -> T>(Mutex<ElementOderKonstruktor<T, F>>);

#[cfg(not(feature = "raspi"))]
impl<T, F> LazyMutex<T, F> {
    #[inline(always)]
    const fn neu(konstruktor: F) -> LazyMutex<T, F> {
        LazyMutex(const_mutex(ElementOderKonstruktor::Konstruktor(konstruktor)))
    }
}

#[cfg(not(feature = "raspi"))]
impl<T, F: FnOnce() -> T> LazyMutex<T, F> {
    fn lock(&self) -> MappedMutexGuard<'_, T> {
        let mut write_guard = self.0.lock();
        // Stelle sicher, dass das Element initialisiert ist (write-Berechtigung wird sowieso benötigt).
        write_guard.initialisiere_wenn_notwendig();
        // Gebe eine WriteGuard auf das Element zurück.
        MutexGuard::map(write_guard, ElementOderKonstruktor::erhalte_element_mut_unchecked)
    }
}
