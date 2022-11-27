//! Pfad auf einem Canvas und assoziierte Typen.

use std::{fmt::Debug, marker::PhantomData};

use iced::{
    widget::canvas::{path, Path},
    Point,
};

use crate::typen::{
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Winkel},
};

/// Pfad auf dem Canvas.
///
/// Transformationen werden ausgeführt, bevor der Pfad gezeichnet/gefüllt wird!
#[derive(Debug)]
pub struct Pfad {
    pub(crate) pfad: Path,
    pub(crate) transformationen: Vec<Transformation>,
}

impl Pfad {
    /// Erzeuge ein Rechteck der gegebenen `größe` unter den gegebenen `transformationen`.
    pub fn rechteck(größe: Vektor, transformationen: Vec<Transformation>) -> Self {
        Erbauer::neu()
            .move_to_chain(Vektor::null_vektor())
            .line_to_chain(größe.x * Vektor::EX)
            .line_to_chain(größe)
            .line_to_chain(größe.y * Vektor::EY)
            .close_chain()
            .baue_unter_transformationen(transformationen)
    }
}

/// Unterstützte Transformationen.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Transformation {
    /// Verschiebe alle Koordinaten um den übergebenen Vector.
    Translation(Vektor),
    /// Rotiere alle Koordinaten um den Ursprung (im Uhrzeigersinn).
    Rotation(Winkel),
    /// Skaliere alle Koordinaten (x',y') = (x*scale, y*scale).
    Skalieren(Skalar),
}

/// Variante von [iced::canvas::path::Arc] mit [Invertiert]-Implementierung.
///
/// Beschreibt einen Bogen um `zentrum` mit `radius` von Winkel `anfang` bis `ende`
/// (im Uhrzeigersinn, y-Achse wächst nach Unten)
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Bogen {
    /// Der Zentrum des Bogens.
    pub zentrum: Vektor,
    /// Der Radius des Bogens.
    pub radius: Skalar,
    /// Der Winkel bei dem der Bogen anfängt.
    pub anfang: Winkel,
    /// Der Winkel bei dem der Bogen aufhört.
    pub ende: Winkel,
}

/// Marker-Typ für [Invertiert], X-Achse (Horizontal).
#[derive(Debug, Clone, Copy)]
pub struct X;

/// Marker-Typ für [Invertiert], Y-Achse (Vertikal).
#[derive(Debug, Clone, Copy)]
pub struct Y;

/// Hilf-Struktur um mich vor dummen Fehlern (z.B. doppeltes invertieren) zu bewahren.
#[derive(Debug)]
pub struct Invertiert<T, Achse>(T, PhantomData<*const Achse>);

impl<T, Achse> From<T> for Invertiert<T, Achse> {
    fn from(t: T) -> Self {
        Invertiert(t, PhantomData)
    }
}

impl<P: Into<Vektor>> From<Invertiert<P, X>> for Vektor {
    fn from(invertiert: Invertiert<P, X>) -> Self {
        let mut v = invertiert.0.into();
        v.x = -v.x;
        v
    }
}

impl<P: Into<Vektor>> From<Invertiert<P, Y>> for Vektor {
    fn from(invertiert: Invertiert<P, Y>) -> Self {
        let mut v = invertiert.0.into();
        v.y = -v.y;
        v
    }
}

impl<A: Into<Winkel>> From<Invertiert<A, X>> for Winkel {
    fn from(invertiert: Invertiert<A, X>) -> Self {
        let w = invertiert.0.into();
        winkel::PI - w
    }
}

impl<A: Into<Winkel>> From<Invertiert<A, Y>> for Winkel {
    fn from(invertiert: Invertiert<A, Y>) -> Self {
        let w = invertiert.0.into();
        -w
    }
}

impl<B, Achse> From<Invertiert<B, Achse>> for Bogen
where
    B: Into<Bogen>,
    Vektor: From<Invertiert<Vektor, Achse>>,
    Winkel: From<Invertiert<Winkel, Achse>>,
{
    fn from(invertiert: Invertiert<B, Achse>) -> Self {
        let b = invertiert.0.into();
        let zentrum: Invertiert<Vektor, Achse> = b.zentrum.into();
        let anfang: Invertiert<Winkel, Achse> = b.anfang.into();
        let ende: Invertiert<Winkel, Achse> = b.ende.into();
        Bogen {
            zentrum: zentrum.into(),
            radius: b.radius,
            anfang: anfang.into(),
            ende: ende.into(),
        }
    }
}

/// Newtype auf einem [iced::canvas::path::Builder].
///
/// Implementiert nur Methoden, die ich auch benötige.
/// Evtl. werden später weitere hinzugefügt.
/// Alle Methoden verwenden die hier definierten Typen.
pub struct Erbauer<V, B> {
    builder: path::Builder,
    phantom_data: PhantomData<*const (V, B)>,
}

impl<V, B> Debug for Erbauer<V, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Erbauer")
            .field("builder", &"<Builder>")
            .field("phantom_data", &self.phantom_data)
            .finish()
    }
}

impl Erbauer<Vektor, Bogen> {
    /// Erstelle einen neuen [Erbauer].
    pub fn neu() -> Self {
        Erbauer { builder: path::Builder::new(), phantom_data: PhantomData }
    }

    /// Finalisiere und erzeuge den unveränderlichen Pfad.
    pub fn baue(self) -> Pfad {
        self.baue_unter_transformationen(Vec::new())
    }

    /// Finalisiere und erzeuge den unveränderlichen Pfad
    /// nach Anwendung der übergebenen Transformationen.
    pub fn baue_unter_transformationen(self, transformationen: Vec<Transformation>) -> Pfad {
        Pfad { pfad: self.builder.build(), transformationen }
    }
}

impl<V: Into<Vektor>, B: Into<Bogen>> Erbauer<V, B> {
    /// Beginne einen neuen Unterpfad bei `punkt`.
    #[zugkontrolle_macros::chain]
    pub fn move_to(&mut self, punkt: V) {
        let Vektor { x, y } = punkt.into();
        self.builder.move_to(Point { x: x.0, y: y.0 })
    }

    /// Zeichne einen Linie vom aktuellen Punkt zu `ziel`.
    #[zugkontrolle_macros::chain]
    pub fn line_to(&mut self, ziel: V) {
        let Vektor { x, y } = ziel.into();
        self.builder.line_to(Point { x: x.0, y: y.0 })
    }

    /// Zeichne den beschriebenen Bogen.
    ///
    /// Beginnt einen neuen Unterpfad.
    #[zugkontrolle_macros::chain]
    pub fn arc(&mut self, bogen: B) {
        let Bogen { zentrum: Vektor { x, y }, radius, anfang, ende } = bogen.into();
        self.builder.arc(path::Arc {
            center: Point { x: x.0, y: y.0 },
            radius: radius.0,
            start_angle: anfang.0,
            end_angle: ende.0,
        })
    }

    /*
    // TODO Funktioniert nicht mit with_invert_x,y :(
    //      (wurde es mittlerweile gefixt?)
    // iced-github-Issue öffnen, die verwendete Bibliothek scheint eine Flag zu unterstützen
    /// Strike an arc from `a` to `b` with given radius (clockwise).
    ///
    /// If `move_to` is `true` start a new subgraph before the arc.
    /// Otherwise, strike a direct line from the current point to the start of the arc.
    pub fn arc_to(&mut self, a: Point, b: Point, radius: Radius, new_sub_path: bool) {
        if new_sub_path {
            self.move_to(a.clone())
        }
        self.builder.arc_to(
            self.invert_point_axis(a).into(),
            self.invert_point_axis(b).into(),
            radius.0,
        )
    }
    */

    /// Zeichne eine Linie vom aktuellen Punkt zum start des aktuellen Unterpfades.
    #[zugkontrolle_macros::chain]
    pub fn close(&mut self) {
        self.builder.close()
    }

    /// Alle Methoden der closure verwenden unveränderte Achsen (x',y') = (x,y).
    ///
    /// Convenience-Funktion um nicht permanent no-op closures erstellen zu müssen.
    #[zugkontrolle_macros::chain]
    pub fn with_normal_axis(&mut self, action: impl FnOnce(&mut Erbauer<V, B>)) {
        action(self)
    }

    /// Alle Methoden der closure verwenden eine gespiegelte x-Achse (x',y') = (-x,y).
    #[zugkontrolle_macros::chain]
    pub fn with_invert_x(
        &mut self,
        action: impl FnOnce(&mut Erbauer<Invertiert<V, X>, Invertiert<B, X>>),
    ) {
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: Erbauer<Invertiert<V, X>, Invertiert<B, X>> =
                Erbauer { builder, phantom_data: PhantomData };
            action(&mut inverted_builder);
            inverted_builder.builder
        })
    }

    /// Alle Methoden der closure verwenden eine gespiegelte y-Achse (x',y') = (x,-y).
    #[zugkontrolle_macros::chain]
    pub fn with_invert_y(
        &mut self,
        action: impl FnOnce(&mut Erbauer<Invertiert<V, Y>, Invertiert<B, Y>>),
    ) {
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: Erbauer<Invertiert<V, Y>, Invertiert<B, Y>> =
                Erbauer { builder, phantom_data: PhantomData };
            action(&mut inverted_builder);
            inverted_builder.builder
        })
    }
}
