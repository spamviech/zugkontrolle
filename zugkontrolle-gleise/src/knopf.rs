//! Knopf mit dem jeweiligen Gleis.

use iced::{
    mouse::{self, Cursor},
    touch,
    widget::{
        canvas::{
            event,
            fill::{self, Fill},
            stroke::{self, Stroke},
            Canvas, Event, Geometry, Program, Text,
        },
        container::{self, Container},
    },
    Element, Length, Point, Rectangle, Renderer,
};
use log::debug;

use zugkontrolle_id::GleisId;
use zugkontrolle_typen::{
    canvas::{
        pfad::{Pfad, Transformation},
        Cache,
    },
    farbe::Farbe,
    klick_quelle::KlickQuelle,
    mm::Spurweite,
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    Zeichnen,
};

use crate::draw::bewege_an_position;

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
    /// Die [`DefinitionId`](crate::gleise::id::DefinitionId) für die [`KnopfNachricht`].
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
    pub fn als_iced_widget<Nachricht, Thema>(
        self,
        breite: Option<f32>,
    ) -> impl Into<Element<'t, Nachricht, Renderer<Thema>>>
    where
        Nachricht: 'static,
        Thema: 't + container::StyleSheet,
        Knopf<'t, T>: Program<Nachricht, Renderer<Thema>>,
    {
        let größe = self.gleis.rechteck(&(), self.spurweite).größe();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let standard_breite = (STROKE_WIDTH + größe.x).0;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let höhe = (DOUBLE_PADDING_BORDER_WIDTH + STROKE_WIDTH + größe.y).0;
        // account for lines right at the edge
        let canvas: Canvas<_, Nachricht, Renderer<Thema>> = Canvas::new(self)
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

impl<Gleis, N, T> Program<N, Renderer<T>> for Knopf<'_, Gleis>
where
    Gleis: Zeichnen<()>,
    N: Nachricht<GleisId<Gleis>>,
    T: Clone + Into<u8> + PartialEq + Thema,
    u8: TryInto<T>,
{
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer<T>,
        thema: &T,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        vec![state.canvas.zeichnen(renderer, thema, bounds.size(), |frame| {
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
                        ..thema.standard_text()
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
    ) -> (event::Status, Option<N>) {
        /// Reagiere darauf, dass der [`Knopf`] angeklickt wurde.
        fn pressed<Gleis, N>(
            definition: &GleisId<Gleis>,
            klick_quelle: KlickQuelle,
            cursor: Cursor,
            bounds: Rectangle,
        ) -> (event::Status, Option<N>)
        where
            N: Nachricht<GleisId<Gleis>>,
        {
            if let Some(Point { x, y }) = cursor.position_in(bounds) {
                (
                    event::Status::Captured,
                    Some(<N as Nachricht<GleisId<Gleis>>>::nachricht(
                        definition,
                        klick_quelle,
                        Vektor { x: Skalar(x), y: Skalar(y) },
                    )),
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

/// Alle Funktionen eines [`Knopfes`](Knopf) werden unterstützt.
pub trait Nachricht<Definition> {
    /// Erzeuge eine Nachricht ausgehend der relativen Position wo der [`Knopf`] gedrückt wurde.
    fn nachricht(
        definition: &Definition,
        klick_quelle: KlickQuelle,
        klick_position: Vektor,
    ) -> Self;
}

/// Anforderung für ein Thema, damit es für einen [`Knopf`] unterstützt wird.
pub trait Thema {
    /// Die Standard-Schriftart, Größe und Ausrichtung für Text auf einem Canvas.
    #[must_use]
    fn standard_text(&self) -> Text;
    /// Die Farbe für Hintergrund eines Widgets.
    #[must_use]
    fn hintergrund(&self, aktiv: bool, hovered: bool) -> Farbe;
    /// Die Farbe für generische Striche (z.B. Text).
    #[must_use]
    fn strich(&self) -> Farbe;
}
