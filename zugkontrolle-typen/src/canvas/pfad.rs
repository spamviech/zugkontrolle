//! Pfad auf einem Canvas und assoziierte Typen.

use std::{
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
};

use iced_core::{Point, Radians};
use iced_graphics::geometry::{path, Path};

use crate::{
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Winkel},
};

/// Pfad auf dem Canvas.
///
/// Transformationen werden ausgeführt, bevor der Pfad gezeichnet/gefüllt wird!
#[derive(Debug)]
pub struct Pfad {
    /// Der beschriebene Pfad.
    pub(crate) pfad: Path,
    /// Unter welchen Transformationen wird der Pfad beschreiben.
    pub(crate) transformationen: Vec<Transformation>,
}

impl Pfad {
    /// Erzeuge ein Rechteck der gegebenen `größe` unter den gegebenen `transformationen`.
    #[must_use]
    pub fn rechteck(größe: Vektor, transformationen: Vec<Transformation>) -> Self {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
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

/// Variante von [`iced::widget::canvas::path::Arc`] mit [`Invertiert`]-Implementierung.
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

/// Marker-Typ für [`Invertiert`], X-Achse (Horizontal).
#[derive(Debug, Clone, Copy)]
pub struct XAchse;

/// Marker-Typ für [`Invertiert`], Y-Achse (Vertikal).
#[derive(Debug, Clone, Copy)]
pub struct YAchse;

/// Hilfs-Struktur um mich vor dummen Fehlern (z.B. doppeltes invertieren) zu bewahren.
#[derive(Debug)]
pub struct Invertiert<T, Achse>(T, PhantomData<*const Achse>);

impl<T, Achse> From<T> for Invertiert<T, Achse> {
    #[allow(clippy::min_ident_chars)]
    fn from(t: T) -> Self {
        Invertiert(t, PhantomData)
    }
}

impl<P: Into<Vektor>> From<Invertiert<P, XAchse>> for Vektor {
    fn from(invertiert: Invertiert<P, XAchse>) -> Self {
        let mut vektor = invertiert.0.into();
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            vektor.x = -vektor.x;
        }
        vektor
    }
}

impl<P: Into<Vektor>> From<Invertiert<P, YAchse>> for Vektor {
    fn from(invertiert: Invertiert<P, YAchse>) -> Self {
        let mut vektor = invertiert.0.into();
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            vektor.y = -vektor.y;
        }
        vektor
    }
}

impl<A: Into<Winkel>> From<Invertiert<A, XAchse>> for Winkel {
    fn from(invertiert: Invertiert<A, XAchse>) -> Self {
        let w = invertiert.0.into();
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            winkel::PI - w
        }
    }
}

impl<A: Into<Winkel>> From<Invertiert<A, YAchse>> for Winkel {
    fn from(invertiert: Invertiert<A, YAchse>) -> Self {
        let w = invertiert.0.into();
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            -w
        }
    }
}

impl<B, Achse> From<Invertiert<B, Achse>> for Bogen
where
    B: Into<Bogen>,
    Vektor: From<Invertiert<Vektor, Achse>>,
    Winkel: From<Invertiert<Winkel, Achse>>,
{
    fn from(invertiert: Invertiert<B, Achse>) -> Self {
        let bogen = invertiert.0.into();
        let zentrum: Invertiert<Vektor, Achse> = bogen.zentrum.into();
        let anfang: Invertiert<Winkel, Achse> = bogen.anfang.into();
        let ende: Invertiert<Winkel, Achse> = bogen.ende.into();
        Bogen {
            zentrum: zentrum.into(),
            radius: bogen.radius,
            anfang: anfang.into(),
            ende: ende.into(),
        }
    }
}

/// Newtype auf einem [`iced::widget::canvas::path::Builder`].
///
/// Implementiert nur Methoden, die ich auch benötige.
/// Evtl. werden später weitere hinzugefügt.
/// Alle Methoden verwenden die hier definierten Typen.
pub struct Erbauer<V, B> {
    /// Der Builder.
    builder: path::Builder,
    /// [`PhantomData`] um [`Vektor`] und [`Bogen`] unter Berücksichtigung von [`Invertiert`] zu verwenden.
    phantom_data: PhantomData<fn() -> (V, B)>,
}

impl<V, B> Debug for Erbauer<V, B> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Erbauer")
            .field("builder", &"<Builder>")
            .field("phantom_data", &self.phantom_data)
            .finish()
    }
}

impl Erbauer<Vektor, Bogen> {
    /// Erstelle einen neuen [`Erbauer`].
    #[must_use]
    pub fn neu() -> Self {
        Erbauer { builder: path::Builder::new(), phantom_data: PhantomData }
    }

    /// Finalisiere und erzeuge den unveränderlichen Pfad.
    #[must_use]
    pub fn baue(self) -> Pfad {
        self.baue_unter_transformationen(Vec::new())
    }

    /// Finalisiere und erzeuge den unveränderlichen Pfad
    /// nach Anwendung der übergebenen Transformationen.
    #[must_use]
    pub fn baue_unter_transformationen(self, transformationen: Vec<Transformation>) -> Pfad {
        Pfad { pfad: self.builder.build(), transformationen }
    }
}

impl<V: Into<Vektor>, B: Into<Bogen>> Erbauer<V, B> {
    /// Beginne einen neuen Unterpfad bei `punkt`.
    #[zugkontrolle_macros::chain]
    pub fn move_to(&mut self, punkt: V) {
        let Vektor { x, y } = punkt.into();
        self.builder.move_to(Point { x: x.0, y: y.0 });
    }

    /// Zeichne einen Linie vom aktuellen Punkt zu `ziel`.
    #[zugkontrolle_macros::chain]
    pub fn line_to(&mut self, ziel: V) {
        let Vektor { x, y } = ziel.into();
        self.builder.line_to(Point { x: x.0, y: y.0 });
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
            start_angle: Radians(anfang.0),
            end_angle: Radians(ende.0),
        });
    }

    /// Zeichne eine Linie vom aktuellen Punkt zum start des aktuellen Unterpfades.
    #[zugkontrolle_macros::chain]
    pub fn close(&mut self) {
        self.builder.close();
    }

    /// Alle Methoden der closure verwenden unveränderte Achsen (x',y') = (x,y).
    ///
    /// Convenience-Funktion um nicht permanent no-op closures erstellen zu müssen.
    pub fn with_normal_axis<T>(&mut self, action: impl FnOnce(&mut Erbauer<V, B>) -> T) -> T {
        action(self)
    }

    /// Alle Methoden der closure verwenden eine gespiegelte x-Achse `(x',y') = (-x,y)`.
    ///
    /// ## Panics
    ///
    /// Bei einem Programmierfehler, wenn das Ergebnis nicht gesetzt wird.
    pub fn with_invert_x<T>(
        &mut self,
        action: impl FnOnce(&mut Erbauer<Invertiert<V, XAchse>, Invertiert<B, XAchse>>) -> T,
    ) -> T {
        let mut ergebnis = None;
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: Erbauer<Invertiert<V, XAchse>, Invertiert<B, XAchse>> =
                Erbauer { builder, phantom_data: PhantomData };
            ergebnis = Some(action(&mut inverted_builder));
            inverted_builder.builder
        });
        ergebnis.expect("Ergebnis nicht gesetzt!")
    }

    /// Alle Methoden der closure verwenden eine gespiegelte y-Achse `(x',y') = (x,-y)`.
    ///
    /// ## Panics
    ///
    /// Bei einem Programmierfehler, wenn das Ergebnis nicht gesetzt wird.
    pub fn with_invert_y<T>(
        &mut self,
        action: impl FnOnce(&mut Erbauer<Invertiert<V, YAchse>, Invertiert<B, YAchse>>) -> T,
    ) -> T {
        let mut ergebnis = None;
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: Erbauer<Invertiert<V, YAchse>, Invertiert<B, YAchse>> =
                Erbauer { builder, phantom_data: PhantomData };
            ergebnis = Some(action(&mut inverted_builder));
            inverted_builder.builder
        });
        ergebnis.expect("Ergebnis nicht gesetzt!")
    }
}
