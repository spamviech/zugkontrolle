//! Mock-Methoden oder re-export für rppal.

#[cfg(not(raspi))]
use std::fmt::{self, Debug, Formatter};

#[cfg(not(raspi))]
use parking_lot::{
    const_rwlock, MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard,
    RwLockWriteGuard,
};

pub mod gpio;
pub mod i2c;
pub mod pwm;

#[cfg(not(raspi))]
enum ElementOderKonstruktor<T, F = fn() -> T> {
    Element(T),
    Konstruktor(F),
}

#[cfg(not(raspi))]
impl<T: Debug, F> Debug for ElementOderKonstruktor<T, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ElementOderKonstruktor::Element(element) => {
                f.debug_tuple("Element").field(element).finish()
            },
            ElementOderKonstruktor::Konstruktor(_) => {
                f.debug_tuple("Konstruktor").field(&"<funktion>").finish()
            },
        }
    }
}

#[cfg(not(raspi))]
impl<T, F> ElementOderKonstruktor<T, F> {
    fn erhalte_element_unchecked(&self) -> &T {
        match self {
            ElementOderKonstruktor::Element(element) => element,
            ElementOderKonstruktor::Konstruktor(_konstruktor) => {
                panic!("erhalte_element_unchecked für eine Konstruktor-Variante aufgerufen!")
            },
        }
    }

    fn erhalte_element_mut_unchecked(&mut self) -> &mut T {
        match self {
            ElementOderKonstruktor::Element(element) => element,
            ElementOderKonstruktor::Konstruktor(_konstruktor) => {
                panic!("erhalte_element_mut_unchecked für eine Konstruktor-Variante aufgerufen!")
            },
        }
    }
}

#[cfg(not(raspi))]
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

#[cfg(not(raspi))]
#[derive(Debug)]
struct LazyRwLock<T, F = fn() -> T>(RwLock<ElementOderKonstruktor<T, F>>);

#[cfg(not(raspi))]
impl<T, F> LazyRwLock<T, F> {
    #[inline(always)]
    const fn neu(konstruktor: F) -> LazyRwLock<T, F> {
        LazyRwLock(const_rwlock(ElementOderKonstruktor::Konstruktor(konstruktor)))
    }
}

#[cfg(not(raspi))]
impl<T, F: FnOnce() -> T> LazyRwLock<T, F> {
    fn read(&self) -> MappedRwLockReadGuard<'_, T> {
        // Teste, ob der Inhalt bereits initialisiert ist (möglich mit anderen read-Aufrufen).
        let read_guard = self.0.read();
        match &*read_guard {
            ElementOderKonstruktor::Element(_element) => {
                return RwLockReadGuard::map(
                    read_guard,
                    ElementOderKonstruktor::erhalte_element_unchecked,
                )
            },
            ElementOderKonstruktor::Konstruktor(_konstruktor) => {},
        }
        // Initialisiere das Element (write-Berechtigung notwendig).
        self.0.write().initialisiere_wenn_notwendig();
        // Jetzt ist das Element garantiert initialisiert.
        RwLockReadGuard::map(self.0.read(), ElementOderKonstruktor::erhalte_element_unchecked)
    }

    fn write(&self) -> MappedRwLockWriteGuard<'_, T> {
        let mut write_guard = self.0.write();
        // Stelle sicher, dass das Element initialisiert ist (write-Berechtigung wird sowieso benötigt).
        write_guard.initialisiere_wenn_notwendig();
        // Gebe eine WriteGuard auf das Element zurück.
        RwLockWriteGuard::map(write_guard, ElementOderKonstruktor::erhalte_element_mut_unchecked)
    }
}
