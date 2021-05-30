//! Widget zur Farbwahl ohne Overlay.

use std::hash::Hash;

use iced_graphics::Primitive;
use iced_native::{
    event,
    layout,
    mouse,
    touch,
    Background,
    Clipboard,
    Color,
    Element,
    Event,
    Hasher,
    Layout,
    Length,
    Point,
    Rectangle,
    Renderer,
    Size,
    Widget,
};

use super::gleis::typen::{
    skalar::Skalar,
    vektor::Vektor,
    winkel::{self, Trigonometrie},
};

/// Widget zur Farbwahl.
/// Im Gegensatz zum `iced_aw::ColorPicker` wird kein `overlay` verwendet, so dass es innerhalb
/// eines `Modal` verwendet werden kann.
pub struct Farbwahl<'a, M> {
    durchmesser: u16,
    nachricht: &'a dyn Fn(Color) -> M,
}
impl<'a, M> Farbwahl<'a, M> {
    pub fn neu(nachricht: &'a impl Fn(Color) -> M) -> Self {
        Farbwahl { durchmesser: 50, nachricht }
    }

    pub fn radius(mut self, radius: u16) -> Self {
        self.durchmesser = 2 * radius;
        self
    }

    pub fn durchmesser(mut self, durchmesser: u16) -> Self {
        self.durchmesser = durchmesser;
        self
    }

    // Farbe eines Pixel oder None wenn außerhalb vom Radius.
    fn farbe(&self, vr: Vektor) -> Option<Color> {
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
                Color::from_rgb(
                    skaliert.skalarprodukt(&e_r).0.max(0.),
                    skaliert.skalarprodukt(&e_g).0.max(0.),
                    skaliert.skalarprodukt(&e_b).0.max(0.),
                )
            } else {
                let e = vr.einheitsvektor();
                // skaliert um schwarzen äußeren Ring zu verhindern
                let reduziert = Skalar(0.8) * skaliert - e;
                let anpassen = |v: Vektor, e: Vektor| {
                    let x = v.skalarprodukt(&e).0;
                    let s_max = v.einheitsvektor().skalarprodukt(&e).0;
                    (s_max - x).abs()
                };
                let r = anpassen(reduziert, e_r);
                let g = anpassen(reduziert, e_g);
                let b = anpassen(reduziert, e_b);
                Color::from_rgb(r, g, b)
            };
            Some(c)
        } else {
            None
        }
    }
}

impl<'a, M, R> Widget<M, R> for Farbwahl<'a, M>
where
    R: Renderer,
    R::Output: From<(Primitive, mouse::Interaction)>,
{
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
        _renderer: &mut R,
        _defaults: &<R as Renderer>::Defaults,
        layout: Layout<'_>,
        _cursor_position: Point,
        _viewport: &Rectangle,
    ) -> <R as Renderer>::Output {
        let mut primitives = Vec::new();
        let bounds = layout.bounds();
        let radius = Skalar(0.5 * self.durchmesser as f32);
        let center = Vektor { x: radius, y: radius };
        for x in 0 .. self.durchmesser {
            for y in 0 .. self.durchmesser {
                let v = Vektor { x: Skalar(x as f32), y: Skalar(y as f32) };
                let vr = v - center;
                if let Some(farbe) = self.farbe(vr) {
                    primitives.push(Primitive::Quad {
                        bounds: Rectangle {
                            x: bounds.x + v.x.0,
                            y: bounds.y + v.y.0,
                            width: 1.,
                            height: 1.,
                        },
                        background: Background::Color(farbe),
                        border_radius: 0.,
                        border_width: 0.,
                        border_color: Color::default(),
                    })
                }
            }
        }
        (Primitive::Group { primitives }, mouse::Interaction::default()).into()
    }

    fn hash_layout(&self, state: &mut Hasher) {
        self.durchmesser.hash(state)
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        _renderer: &R,
        _clipboard: &mut dyn Clipboard,
        messages: &mut Vec<M>,
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
                messages.push((self.nachricht)(farbe));
                status = event::Status::Captured;
            }
        }
        status
    }
}

impl<'a, M, R> From<Farbwahl<'a, M>> for Element<'a, M, R>
where
    R: Renderer,
    R::Output: From<(Primitive, mouse::Interaction)>,
{
    fn from(farbwahl: Farbwahl<'a, M>) -> Self {
        Element::new(farbwahl)
    }
}
