//! Knopf mit dem jeweiligen Gleis.

use iced::{
    mouse::{self, Cursor},
    touch::{self, Finger},
    widget::{
        canvas::{
            event,
            fill::{self, Fill},
            stroke::{self, Stroke},
            Canvas, Event, Geometry, Program, Text,
        },
        container::Container,
    },
    Element, Length, Point, Rectangle, Renderer,
};
use log::debug;

use crate::{
    application::{fonts::standard_text, style::thema::Thema},
    gleis::gleise::{draw::bewege_an_position, id::GleisId},
    typen::{
        canvas::{
            pfad::{Pfad, Transformation},
            Cache,
        },
        mm::Spurweite,
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        Zeichnen,
    },
};

/// Die Breite von Kontur-Linien des Gleises.
const STROKE_WIDTH: Skalar = Skalar(1.5);
/// Die Breite des Randes.
const BORDER_WIDTH: u16 = 1;
/// Das Padding zwischen Gleis und Rand.
const PADDING: u16 = 2;
// notwendig wegen const
#[allow(clippy::as_conversions)]
/// Doppelter Wert von [`PADDING`] und [`BORDER_WIDTH`] als [`Skalar`].
const DOUBLE_PADDING_BORDER_WIDTH: Skalar = Skalar((2 * (BORDER_WIDTH + PADDING)) as f32);

/// Ein Knopf, der ein Gleis anzeigt.
#[derive(Debug)]
pub struct Knopf<'t, T: 'static> {
    /// Das angezeigte Gleis.
    gleis: &'t T,
    /// Die [`DefinitionId`](crate::gleis::gleise::id::DefinitionId) für die [`KnopfNachricht`].
    definition: GleisId<T>,
    /// Die Spurweite des [`Zugtyps`](crate::zugtyp::Zugtyp).
    spurweite: Spurweite,
}

impl<'t, T> Knopf<'t, T> {
    /// Erstelle einen neuen [`Knopf`].
    #[must_use]
    pub fn neu(gleis: &'t T, definition: GleisId<T>, spurweite: Spurweite) -> Self {
        Knopf { gleis, definition, spurweite }
    }
}

impl<'t, T: Zeichnen<()>> Knopf<'t, T> {
    /// Die Dimensionen des [`Knopfes`](Knopf).
    #[must_use]
    pub fn rechteck(&self) -> Rechteck {
        self.gleis.rechteck(&(), self.spurweite).verschiebe_chain(&Vektor {
            x: DOUBLE_PADDING_BORDER_WIDTH,
            y: DOUBLE_PADDING_BORDER_WIDTH,
        })
    }

    /// Erstelle ein [Widget](iced_native::Element), dass den [`Knopf`] anzeigt.
    #[must_use]
    pub fn als_iced_widget<Nachricht>(
        self,
        breite: Option<f32>,
    ) -> impl Into<Element<'t, Nachricht, Renderer<Thema>>>
    where
        Nachricht: 'static,
        GleisId<T>: KnopfNachricht<Nachricht>,
    {
        let größe = self.gleis.rechteck(&(), self.spurweite).größe();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let standard_breite = (STROKE_WIDTH + größe.x).0;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let höhe = (DOUBLE_PADDING_BORDER_WIDTH + STROKE_WIDTH + größe.y).0;
        // account for lines right at the edge
        let canvas = Canvas::new(self)
            .width(Length::Fixed(breite.unwrap_or(standard_breite)))
            .height(Length::Fixed(höhe));
        Container::new(canvas).width(Length::Fill).height(Length::Shrink)
    }
}

/// Zustand für ein [`Knopf`]-Widget.
#[derive(Debug, Default)]
pub struct Zustand {
    /// Der [`Cache`] für die Canvas-Anzeige.
    canvas: Cache,
    /// Ist der Cursor aktuell innerhalb des Knopfes.
    in_bounds: bool,
}

impl<T, Nachricht> Program<Nachricht, Renderer<Thema>> for Knopf<'_, T>
where
    T: Zeichnen<()>,
    GleisId<T>: KnopfNachricht<Nachricht>,
{
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer<Thema>,
        thema: &Thema,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        vec![state.canvas.zeichnen(renderer, bounds.size(), |frame| {
            let bounds_vector = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
            let border_path = Pfad::rechteck(bounds_vector, Vec::new());
            frame.fill(
                &border_path,
                Fill {
                    style: fill::Style::Solid(thema.hintergrund(false, state.in_bounds).into()),
                    rule: fill::Rule::EvenOdd,
                },
            );
            frame.stroke(
                &border_path,
                Stroke {
                    style: stroke::Style::Solid(thema.strich().into()),
                    width: BORDER_WIDTH.into(),
                    ..Default::default()
                },
            );
            let spurweite = self.spurweite;
            let rechteck = self.gleis.rechteck(&(), spurweite);
            let rechteck_position = rechteck.position();
            frame.transformation(&Transformation::Translation(
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                {
                    -rechteck_position
                },
            ));
            let größe = rechteck.größe();
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let maximale_breite = bounds_vector.x - DOUBLE_PADDING_BORDER_WIDTH;
            if maximale_breite > größe.x {
                // horizontal zentrieren
                frame.transformation(&Transformation::Translation(
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    {
                        Skalar(0.5) * (bounds_vector - größe)
                    },
                ));
            } else {
                // skaliere zu vorhandener Breite
                frame.transformation(&Transformation::Skalieren(
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    {
                        maximale_breite / größe.x
                    },
                ));
                frame.transformation(&Transformation::Translation(
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    {
                        Skalar(0.5)
                            * Vektor {
                                x: DOUBLE_PADDING_BORDER_WIDTH,
                                y: DOUBLE_PADDING_BORDER_WIDTH,
                            }
                    },
                ));
            }
            for path in self.gleis.zeichne(&(), spurweite) {
                // frame related über `with_save`
                #[allow(clippy::shadow_unrelated)]
                frame.with_save(|frame| {
                    frame.stroke(
                        &path,
                        Stroke {
                            style: stroke::Style::Solid(thema.strich().into()),
                            width: STROKE_WIDTH.0,
                            ..Default::default()
                        },
                    );
                });
            }
            if let (relative_position, Some(content), _unit_name) =
                self.gleis.beschreibung_und_name(&(), spurweite)
            {
                // frame related über `with_save`
                #[allow(clippy::shadow_unrelated)]
                frame.with_save(|frame| {
                    bewege_an_position(frame, &relative_position);
                    frame.fill_text(Text {
                        content: String::from(content),
                        color: thema.strich().into(),
                        ..standard_text()
                    });
                });
            }
        })]
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        /// Reagiere darauf, dass der [`Knopf`] angeklickt wurde.
        fn pressed<T, Nachricht>(
            definition: &GleisId<T>,
            klick_quelle: KlickQuelle,
            cursor: Cursor,
            bounds: Rectangle,
        ) -> (event::Status, Option<Nachricht>)
        where
            GleisId<T>: KnopfNachricht<Nachricht>,
        {
            if let Some(Point { x, y }) = cursor.position_in(bounds) {
                (
                    event::Status::Captured,
                    Some(definition.nachricht(klick_quelle, Vektor { x: Skalar(x), y: Skalar(y) })),
                )
            } else {
                (event::Status::Ignored, None)
            }
        }
        let in_bounds = cursor.is_over(bounds);
        if state.in_bounds != in_bounds {
            state.in_bounds = in_bounds;
            state.canvas.leeren();
        }
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                debug!("{event:?}");
                pressed(&self.definition, KlickQuelle::Maus, cursor, bounds)
            },
            Event::Touch(touch::Event::FingerPressed { id, position }) => {
                debug!("{event:?}");
                pressed(
                    &self.definition,
                    KlickQuelle::Touch(id),
                    Cursor::Available(position),
                    bounds,
                )
            },
            Event::Mouse(_) | Event::Touch(_) | Event::Keyboard(_) => {
                (event::Status::Ignored, None)
            },
        }
    }
}

// TODO in eigenes Modul verschieben
/// Der Auslöser eines Klicks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KlickQuelle {
    /// Der Klick wurde mit der Maus ausgelöst.
    Maus,
    /// Der Klick wurde mit einem Finger ausgelöst.
    Touch(Finger),
}

// TODO Behandeln erfordert anpassen des public API.
#[allow(clippy::module_name_repetitions)]
/// Alle Funktionen eines [`Knopfes`](Knopf) werden unterstützt.
pub trait KnopfNachricht<Nachricht> {
    /// Erzeuge eine Nachricht ausgehend der relativen Position wo der [`Knopf`] gedrückt wurde.
    fn nachricht(&self, klick_quelle: KlickQuelle, klick_position: Vektor) -> Nachricht;
}
