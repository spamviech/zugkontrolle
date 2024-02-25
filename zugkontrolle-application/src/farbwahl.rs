//! Widget zur Farbwahl ohne Overlay.

use std::fmt::{self, Debug, Formatter};

use iced_core::{
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    renderer::{Quad, Renderer, Style},
    touch,
    widget::tree::Tree,
    Background, BorderRadius, Clipboard, Color, Element, Length, Rectangle, Shell, Size, Widget,
};

use zugkontrolle_typen::{farbe::Farbe, skalar::Skalar, vektor::Vektor, winkel};

/// Widget zur Farbwahl.
///
/// Im Gegensatz zum `iced_aw::ColorPicker` wird kein `overlay` verwendet, so dass es innerhalb
/// eines [`Modal`](crate::modal::Modal) verwendet werden kann.
pub struct Farbwahl<'a, M> {
    /// Der Durchmesser des Farbkreises.
    durchmesser: u16,
    /// Funktion zum erzeugen der Nachricht als Reaktion auf einen Klick.
    nachricht: &'a dyn Fn(Farbe) -> M,
}

impl<M> Debug for Farbwahl<'_, M> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Farbwahl")
            .field("durchmesser", &self.durchmesser)
            .field("nachricht", &"<closure>")
            .finish()
    }
}

impl<'a, M> Farbwahl<'a, M> {
    /// Erstelle eine neue [`Farbwahl`].
    pub fn neu(nachricht: &'a impl Fn(Farbe) -> M) -> Self {
        Farbwahl { durchmesser: 50, nachricht }
    }

    /// Ändere den Radius der [`Farbwahl`].
    #[must_use]
    pub fn radius(mut self, radius: u16) -> Self {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.durchmesser = 2 * radius;
        }
        self
    }

    /// Ändere den Durchmesser der [`Farbwahl`].
    #[must_use]
    pub fn durchmesser(mut self, durchmesser: u16) -> Self {
        self.durchmesser = durchmesser;
        self
    }

    /// Farbe eines Pixel oder None wenn außerhalb vom Radius.
    fn farbe(&self, vr: Vektor) -> Option<Farbe> {
        let länge = vr.länge();
        let radius = Skalar(0.5 * f32::from(self.durchmesser));
        let halber_radius = radius.halbiert();
        (länge <= radius).then(|| {
            let e_r = Vektor { x: Skalar(1.), y: Skalar(0.) };
            let e_g = {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let winkel_g = winkel::TAU / 3.;
                Vektor { x: winkel_g.cos(), y: winkel_g.sin() }
            };
            let e_b = {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let winkel_b = winkel::TAU * 2. / 3.;
                Vektor { x: winkel_b.cos(), y: winkel_b.sin() }
            };
            // Wie f32: Schlimmstenfalls wird ein Nan-Wert erzeugt.
            #[allow(clippy::arithmetic_side_effects)]
            let skaliert = vr / halber_radius;
            if länge <= halber_radius {
                Farbe {
                    rot: skaliert.skalarprodukt(&e_r).0.max(0.),
                    grün: skaliert.skalarprodukt(&e_g).0.max(0.),
                    blau: skaliert.skalarprodukt(&e_b).0.max(0.),
                }
            } else {
                let einheitsvektor = vr.einheitsvektor();
                // skaliert um schwarzen äußeren Ring zu verhindern
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let reduziert = Skalar(0.8) * skaliert - einheitsvektor;
                let anpassen = |vektor: Vektor, e_vektor: Vektor| {
                    let x = vektor.skalarprodukt(&e_vektor).0;
                    let s_max = vektor.einheitsvektor().skalarprodukt(&e_vektor).0;
                    (s_max - x).abs()
                };
                let rot = anpassen(reduziert, e_r);
                let grün = anpassen(reduziert, e_g);
                let blau = anpassen(reduziert, e_b);
                Farbe { rot, grün, blau }
            }
        })
    }
}

impl<M, R: Renderer> Widget<M, R> for Farbwahl<'_, M> {
    fn width(&self) -> Length {
        Length::Fixed(f32::from(self.durchmesser))
    }

    fn height(&self) -> Length {
        Length::Fixed(f32::from(self.durchmesser))
    }

    fn layout(&self, _renderer: &R, _limits: &layout::Limits) -> layout::Node {
        let durchmesser = f32::from(self.durchmesser);
        layout::Node::new(Size { width: durchmesser, height: durchmesser })
    }

    fn draw(
        &self,
        _state: &Tree,
        renderer: &mut R,
        _theme: &<R as Renderer>::Theme,
        _style: &Style,
        layout: Layout<'_>,
        _cursor_position: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        let bounds = layout.bounds();
        let radius = Skalar(0.5 * f32::from(self.durchmesser));
        let center = Vektor { x: radius, y: radius };
        for x in 0..self.durchmesser {
            for y in 0..self.durchmesser {
                let vektor = Vektor { x: Skalar(f32::from(x)), y: Skalar(f32::from(y)) };
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let vr = vektor - center;
                if let Some(farbe) = self.farbe(vr) {
                    let quad = Quad {
                        bounds: Rectangle {
                            x: bounds.x + vektor.x.0,
                            y: bounds.y + vektor.y.0,
                            width: 1.,
                            height: 1.,
                        },
                        border_radius: BorderRadius::from(0.),
                        border_width: 0.,
                        border_color: Color::default(),
                    };
                    let background = Background::Color(farbe.into());
                    renderer.fill_quad(quad, background);
                }
            }
        }
    }

    fn on_event(
        &mut self,
        _state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        _renderer: &R,
        _clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, M>,
        _viewport: &Rectangle,
    ) -> event::Status {
        let mut status = event::Status::Ignored;
        let bounds = layout.bounds();
        let position = match (event, cursor_position) {
            (
                Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
                mouse::Cursor::Available(position),
            )
            | (Event::Touch(touch::Event::FingerPressed { id: _, position }), _) => Some(position),
            _ => None,
        };
        if let Some(position) = position {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let vr = Vektor { x: Skalar(position.x), y: Skalar(position.y) }
                - Vektor { x: Skalar(bounds.center_x()), y: Skalar(bounds.center_y()) };
            if let Some(farbe) = self.farbe(vr) {
                shell.publish((self.nachricht)(farbe));
                status = event::Status::Captured;
            }
        }
        status
    }
}

impl<'a, M, R: Renderer> From<Farbwahl<'a, M>> for Element<'a, M, R> {
    fn from(farbwahl: Farbwahl<'a, M>) -> Self {
        Element::new(farbwahl)
    }
}
