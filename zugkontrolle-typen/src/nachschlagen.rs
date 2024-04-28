//! Trait für fehler-freies Nachschlagen von Elementen.

// Erlaube, `zugkontrolle_macros` nicht direkt als dependency anzugeben.
#![allow(clippy::pub_use)]
// Soll unqualifiziert verwendet werden.
#[allow(clippy::module_name_repetitions)]
pub use zugkontrolle_macros::impl_nachschlagen;

/// Trait für fehler-freies Nachschlagen von Elementen.
pub trait Nachschlagen<Name, Element> {
    /// Erhalte eine Referenz für das spezifizierte `Element`.
    fn erhalte(&self, key: &Name) -> &Element;
    /// Erhalte eine mutable Referenz für das spezifizierte `Element`.
    fn erhalte_mut(&mut self, key: &Name) -> &mut Element;
    /// Führe eine Aktion für jedes `Element` aus.
    fn für_alle<F: FnMut(Name, &Element)>(&self, action: F);
    /// Führe eine Funktion für jedes `Element` und ersetze es mit dem Ergebnis.
    #[must_use]
    fn zuordnen<F: Fn(&Element) -> Element>(&self, function: F) -> Self;
    /// Erhalte einen Vec über Referenzen aller `Element`e.
    fn referenzen(&self) -> Vec<(Name, &Element)>;
    /// Erhalte einen [`Vec`] über mutable Referenzen aller `Element`e.
    fn referenzen_mut(&mut self) -> Vec<(Name, &mut Element)>;
}
