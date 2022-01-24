//! Trait für failure-free Lookup.

pub use zugkontrolle_derive::impl_lookup;

pub trait Lookup<Name, Element> {
    /// Erhalte eine Referenz für das spezifizierte `Element`.
    fn get(&self, key: &Name) -> &Element;
    /// Erhalte eine mutable Referenz für das spezifizierte `Element`.
    fn get_mut(&mut self, key: &Name) -> &mut Element;
    /// Führe eine Aktion für jedes `Element` aus.
    fn for_each<F: FnMut(Name, &Element)>(&self, action: F);
    /// Führe eine Funktion für jedes `Element` und ersetze es mit dem Ergebnis.
    fn map<F: Fn(&Element) -> Element>(&self, function: F) -> Self;
    /// Erhalte einen Vec über Referenzen aller `Element`e.
    fn refs<'t>(&'t self) -> Vec<(Name, &'t Element)>;
    /// Erhalte einen [Vec] über mutable Referenzen aller `Element`e.
    fn refs_mut<'t>(&'t mut self) -> Vec<(Name, &'t mut Element)>;
}
