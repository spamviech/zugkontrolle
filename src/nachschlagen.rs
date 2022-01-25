//! Trait für fehler-freies Nachschlagen von Elementen.

pub use zugkontrolle_derive::impl_nachschlagen;

/// Trait für fehler-freies Nachschlagen von Elementen.
pub trait Nachschlagen<Name, Element> {
    /// Erhalte eine Referenz für das spezifizierte `Element`.
    fn erhalte(&self, key: &Name) -> &Element;
    /// Erhalte eine mutable Referenz für das spezifizierte `Element`.
    fn erhalte_mut(&mut self, key: &Name) -> &mut Element;
    /// Führe eine Aktion für jedes `Element` aus.
    fn für_alle<F: FnMut(Name, &Element)>(&self, action: F);
    /// Führe eine Funktion für jedes `Element` und ersetze es mit dem Ergebnis.
    fn zuordnen<F: Fn(&Element) -> Element>(&self, function: F) -> Self;
    /// Erhalte einen Vec über Referenzen aller `Element`e.
    fn referenzen<'t>(&'t self) -> Vec<(Name, &'t Element)>;
    /// Erhalte einen [Vec] über mutable Referenzen aller `Element`e.
    fn referenzen_mut<'t>(&'t mut self) -> Vec<(Name, &'t mut Element)>;
}
