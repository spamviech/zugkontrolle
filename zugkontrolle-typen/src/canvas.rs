//! Newtypes für [`iced::widget::canvas::Frame`] und [`iced::widget::canvas::Cache`].

use std::{
    fmt::{self, Debug, Formatter},
    sync::atomic::{AtomicU8, Ordering},
};

use iced_core::Size;
use iced_graphics::geometry::{fill::Fill, stroke::Stroke, Text};
use iced_renderer::{
    geometry::{self, Geometry},
    Renderer,
};
use serde::{Deserialize, Serialize};

use crate::{
    canvas::pfad::{Pfad, Transformation},
    mm::Spurweite,
    nachschlagen::Nachschlagen,
    skalar::Skalar,
    vektor::Vektor,
    verbindung::{self, Verbindung},
    winkel::{self, Winkel},
    Zeichnen,
};

pub mod pfad;

/// Newtype auf [`iced::widget::canvas::Frame`], dessen Methoden meine Typen verwenden.
///
/// Alle Koordinaten werden so transformiert, dass `pivot.punkt` auf (0,0) vom [`Frame`](iced_renderer::Frame) liegt.
/// Anschließend werden die Koordinaten um `pivot.winkel` gedreht.
/// Danach werden alle Koordinaten mit dem `skalieren`-Faktor multipliziert.
pub struct Frame<'t>(&'t mut geometry::Frame);

impl Debug for Frame<'_> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_tuple("Frame").field(&"<Frame>").finish()
    }
}

impl<'t> Frame<'t> {
    /// Erzeuge einen neuen [`Frame`].
    pub fn neu(frame: &'t mut geometry::Frame) -> Self {
        Frame(frame)
    }

    // elided lifetimes in impl-Traits are experimental
    #[allow(single_use_lifetimes)]
    /// Zeichne den gegebenen [Pfad] auf den [Frame] im gewünschten [`Stil`](Stroke).
    pub fn stroke<'s>(
        &mut self,
        Pfad { pfad, transformationen }: &Pfad,
        stroke: impl Into<Stroke<'s>>,
    ) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation);
            }
            frame.0.stroke(pfad, stroke);
        });
    }

    /// Fülle den gegebenen [Pfad] auf den [Frame] im gewünschten [`Stil`](Fill).
    pub fn fill(&mut self, Pfad { pfad, transformationen }: &Pfad, fill: impl Into<Fill>) {
        self.with_save(|frame| {
            for transformation in transformationen {
                frame.transformation(transformation);
            }
            frame.0.fill(pfad, fill);
        });
    }

    /// Zeichne die Buchstaben des [Textes](Text) auf den [`Frame`]
    /// und fülle sie mit der gewünschten Farbe.
    ///
    /// **Warnung:** Probleme bezüglich Transformation/Rotation/Skalierung von [`iced::widget::canvas::Frame`]
    /// treten hier ebenfalls auf!

    pub fn fill_text(&mut self, text: impl Into<Text>) {
        self.0.fill_text(text);
    }

    /// Speichere die aktuelle Transformations-Matrix des [`Frame`] und führe die gegebenen Operation aus.
    /// Anschließend wird die Transformations-Matrix wiederhergestellt.
    ///
    /// Diese Methode ist nützlich um mehrere Transformationen zusammenzufassen und Zeichen-Operationen
    /// in verschiedenen Koordinaten-Systemen durchzuführen.
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        self.0.with_save(|frame| action(&mut Frame(frame)));
    }

    /// Wende die übergebene Transformation auf den Frame an.
    ///
    /// **ACHTUNG**: Durch die Art wie es in `iced` implementiert ist wird die [`Transformation`]
    /// **vor** allen bisherigen ausgeführt.
    ///
    /// Links zum Implementierung verfolgen:
    /// <https://github.com/hecrj/iced/blob/master/graphics/src/widget/canvas/frame.rs#L234>
    /// <https://docs.rs/lyon/0.17.5/lyon/math/type.Transform.html>
    /// <https://docs.rs/euclid/0.22.3/euclid/struct.Transform2D.html#method.pre_rotate>
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translation(Vektor { x, y }) => {
                self.0.translate(iced_core::Vector { x: x.0, y: y.0 });
            },
            Transformation::Rotation(winkel) => self.0.rotate(winkel.0),
            Transformation::Skalieren(scale) => self.0.scale(scale.0),
        }
    }
}

/// Ein einfacher Cache, der die erzeugte [`Geometry`] speichert um Neu-Berechnungen zu vermeiden.
///
/// Ein Cache wird die [`Geometry`] nicht neu berechnen, sofern
/// sich seine Dimensionen nicht verändert haben oder er explizit [`geleert`](Cache::leeren) wurde.
#[derive(Debug, Default)]
pub struct Cache {
    /// Der Cache mit der gespeicherten Geometrie.
    cache: geometry::Cache,
    /// Die [`u8`]-Repräsentation des Themas beim letzten
    /// [`zeichnen_skaliert_von_pivot`](Cache::zeichnen_skaliert_von_pivot)-Aufruf.
    thema: AtomicU8,
}

impl Cache {
    /// Erstelle einen neuen [`Cache`].
    #[must_use]
    pub fn neu() -> Self {
        // Initialer Wert ist nicht relevant.
        let thema = AtomicU8::new(0);
        Cache { cache: geometry::Cache::new(), thema }
    }

    /// Leere den [`Cache`], so dass er neu gezeichnet wird.

    pub fn leeren(&self) {
        self.cache.clear();
    }

    /// Führe die `draw_fn` under Transformationen aus, so dass alles von `pivot` gesehen
    /// und mit `skalieren` vergrößert/verkleinert angezeigt wird.
    pub fn zeichnen_skaliert_von_pivot<Thema>(
        &self,
        renderer: &Renderer,
        thema: &Thema,
        bounds: Size<f32>,
        pivot: &Position,
        skalieren: Skalar,
        draw_fn: impl Fn(&mut Frame<'_>),
    ) -> Geometry
    where
        Thema: Clone + Into<u8> + PartialEq,
        u8: TryInto<Thema>,
    {
        let bisher = self.thema.swap(thema.clone().into(), Ordering::Relaxed).try_into().ok();
        if bisher.as_ref() != Some(thema) {
            self.cache.clear();
        }
        self.cache.draw(renderer, bounds, |frame| {
            let mut boxed_frame = Frame(frame);
            boxed_frame.with_save(|transformierter_frame| {
                // pivot transformationen
                transformierter_frame.transformation(&Transformation::Skalieren(skalieren));
                transformierter_frame.transformation(&Transformation::Rotation(pivot.winkel));
                transformierter_frame.transformation(&Transformation::Translation(
                    // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
                    #[allow(clippy::arithmetic_side_effects)]
                    {
                        -pivot.punkt
                    },
                ));
                // zeichne auf Frame
                draw_fn(transformierter_frame);
            });
        })
    }

    /// Zeichne die [Geometry] über die übergebenen Closure und speichere sie im [`Cache`].
    pub fn zeichnen<Thema>(
        &self,
        renderer: &Renderer,
        thema: &Thema,
        bounds: Size<f32>,
        draw_fn: impl Fn(&mut Frame<'_>),
    ) -> Geometry
    where
        Thema: Clone + Into<u8> + PartialEq,
        u8: TryInto<Thema>,
    {
        self.zeichnen_skaliert_von_pivot(
            renderer,
            thema,
            bounds,
            &Position { punkt: Vektor::null_vektor(), winkel: Winkel(0.) },
            Skalar::multiplikativ_neutral(),
            draw_fn,
        )
    }
}

/// Position eines Gleises/Textes auf dem Canvas.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// Die linke Obere Ecke auf dem Canvas.
    pub punkt: Vektor,
    /// Der Winkel in dem das Gleis/der Text gezeichnet wird.
    pub winkel: Winkel,
}

impl Position {
    /// Vektor nachdem das Objekt an die Position bewegt und um den Winkel gedreht wurde.
    #[must_use]
    pub fn transformation(&self, anchor: Vektor) -> Vektor {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.punkt + anchor.rotiert(&self.winkel)
        }
    }

    /// Vektor nachdem er um den Winkel gedreht wurde.
    #[must_use]
    pub fn rotation(&self, richtung: Vektor) -> Vektor {
        richtung.rotiert(&self.winkel)
    }

    /// Position damit Verbindungen übereinander mit entgegengesetzter Richtung liegen.
    #[must_use]
    pub fn anliegend_position<T, Z>(
        definition: &T,
        z: &Z,
        spurweite: Spurweite,
        verbindung_name: &T::VerbindungName,
        ziel_verbindung: Verbindung,
    ) -> Position
    where
        T: Zeichnen<Z>,
        T::Verbindungen: verbindung::Nachschlagen<T::VerbindungName>,
    {
        let verbindungen = definition.verbindungen(z, spurweite);
        let verbindung = verbindungen.erhalte(verbindung_name);
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        let winkel: Winkel = winkel::PI - verbindung.richtung + ziel_verbindung.richtung;
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        Position {
            punkt: Vektor {
                x: ziel_verbindung.position.x - verbindung.position.x * winkel.cos()
                    + verbindung.position.y * winkel.sin(),
                y: ziel_verbindung.position.y
                    - verbindung.position.x * winkel.sin()
                    - verbindung.position.y * winkel.cos(),
            },
            winkel,
        }
    }
}
