//! Widget zur Farbwahl ohne Overlay.

use std::fmt::Debug;

use iced_native::{
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    renderer::{Quad, Renderer, Style},
    touch, Background, Clipboard, Color, Element, Length, Point, Rectangle, Shell, Size, Widget,
};

use crate::typen::{
    farbe::Farbe,
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Trigonometrie},
};

/// Widget zur Farbwahl.
///
/// Im Gegensatz zum `iced_aw::ColorPicker` wird kein `overlay` verwendet, so dass es innerhalb
/// eines [Modal](crate::application::modal::Modal) verwendet werden kann.
pub struct Farbwahl<'a, M> {
    durchmesser: u16,
    nachricht: &'a dyn Fn(Farbe) -> M,
}

impl<M> Debug for Farbwahl<'_, M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Farbwahl")
            .field("durchmesser", &self.durchmesser)
            .field("nachricht", &"<closure>")
            .finish()
    }
}

impl<'a, M> Farbwahl<'a, M> {
    /// Erstelle eine neue [Farbwahl].
    pub fn neu(nachricht: &'a impl Fn(Farbe) -> M) -> Self {
        Farbwahl { durchmesser: 50, nachricht }
    }

    /// Ändere den Radius der [Farbwahl].
    pub fn radius(mut self, radius: u16) -> Self {
        self.durchmesser = 2 * radius;
        self
    }

    /// Ändere den Durchmesser der [Farbwahl].
    pub fn durchmesser(mut self, durchmesser: u16) -> Self {
        self.durchmesser = durchmesser;
        self
    }

    /// Farbe eines Pixel oder None wenn außerhalb vom Radius.
    fn farbe(&self, vr: Vektor) -> Option<Farbe> {
        let länge = vr.länge();
        let radius = Skalar(0.5 * self.durchmesser as f32);
        let halber_radius = radius.halbiert();
        if länge <= radius {
            let e_r = Vektor { x: Skalar(1.), y: Skalar(0.) };
            let e_g = {
                let winkel_g = winkel::TAU / 3.;
                Vektor { x: winkel_g.cos(), y: winkel_g.sin() }
            };
            let e_b = {
                let winkel_b = winkel::TAU * 2. / 3.;
                Vektor { x: winkel_b.cos(), y: winkel_b.sin() }
            };
            let skaliert = vr / halber_radius;
            let c = if länge <= halber_radius {
                Farbe {
                    rot: skaliert.skalarprodukt(&e_r).0.max(0.),
                    grün: skaliert.skalarprodukt(&e_g).0.max(0.),
                    blau: skaliert.skalarprodukt(&e_b).0.max(0.),
                }
            } else {
                let e = vr.einheitsvektor();
                // skaliert um schwarzen äußeren Ring zu verhindern
                let reduziert = Skalar(0.8) * skaliert - e;
                let anpassen = |v: Vektor, e: Vektor| {
                    let x = v.skalarprodukt(&e).0;
                    let s_max = v.einheitsvektor().skalarprodukt(&e).0;
                    (s_max - x).abs()
                };
                let rot = anpassen(reduziert, e_r);
                let grün = anpassen(reduziert, e_g);
                let blau = anpassen(reduziert, e_b);
                Farbe { rot, grün, blau }
            };
            Some(c)
        } else {
            None
        }
    }
}

impl<M, R: Renderer> Widget<M, R> for Farbwahl<'_, M> {
    fn width(&self) -> Length {
        Length::Units(self.durchmesser)
    }

    fn height(&self) -> Length {
        Length::Units(self.durchmesser)
    }

    fn layout(&self, _renderer: &R, _limits: &layout::Limits) -> layout::Node {
        let durchmesser = self.durchmesser as f32;
        layout::Node::new(Size { width: durchmesser, height: durchmesser })
    }

    fn draw(
        &self,
        renderer: &mut R,
        _style: &Style,
        layout: Layout<'_>,
        _cursor_position: Point,
        _viewport: &Rectangle,
    ) {
        // let mut primitives = Vec::new();
        let bounds = layout.bounds();
        let radius = Skalar(0.5 * self.durchmesser as f32);
        let center = Vektor { x: radius, y: radius };
        for x in 0..self.durchmesser {
            for y in 0..self.durchmesser {
                let v = Vektor { x: Skalar(f32::from(x)), y: Skalar(f32::from(y)) };
                let vr = v - center;
                if let Some(farbe) = self.farbe(vr) {
                    let quad = Quad {
                        bounds: Rectangle {
                            x: bounds.x + v.x.0,
                            y: bounds.y + v.y.0,
                            width: 1.,
                            height: 1.,
                        },
                        border_radius: 0.,
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
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        _renderer: &R,
        _clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, M>,
    ) -> event::Status {
        let mut status = event::Status::Ignored;
        let bounds = layout.bounds();
        if let Some(position) = match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => Some(cursor_position),
            Event::Touch(touch::Event::FingerPressed { id: _, position }) => Some(position),
            _ => None,
        } {
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
