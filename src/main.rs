//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use gio::prelude::*;
use gtk::prelude::*;
use gtk::{Application, ApplicationWindow, DrawingArea};
use simple_logger::SimpleLogger;

use gleis::anchor::{self, Lookup};
use gleis::types::*;
use gleis::weiche;
use gleis::widget::Zeichnen;
use gleis::{lego, maerklin};

pub mod gleis;
pub mod zugtyp;

// include std in doc generated by `cargo doc`
// https://github.com/rust-lang/rfcs/issues/2324#issuecomment-502437904
#[cfg(doc)]
#[doc(inline)]
pub use std;

fn main() {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    let application =
        Application::new(None, Default::default()).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Zugkontrolle");

        let drawing_area = DrawingArea::new();
        drawing_area.set_size_request(800, 600);
        fn test(drawing_area: &DrawingArea, c: &cairo::Context) -> glib::signal::Inhibit {
            let allocation = drawing_area.get_allocation();
            let cairo: &Cairo = &Cairo::new(c);
            // Märklin Gleise
            cairo.with_save_restore(|cairo| {
                cairo.translate(CanvasX(0.25 * (allocation.width as u64) as f64), CanvasY(10.));
                show_gleis(cairo, maerklin::GERADE_5106);
                show_gleis(cairo, maerklin::KURVE_5100);
                show_gleis(cairo, maerklin::weiche_5202(weiche::Richtung::Links));
                show_gleis(cairo, maerklin::DREIWEGE_WEICHE_5214);
                show_gleis(cairo, maerklin::kurven_weiche_5140(weiche::Richtung::Links));
                show_gleis(cairo, maerklin::KREUZUNG_5207);
            });
            // Lego Gleise
            cairo.with_save_restore(|cairo| {
                cairo.translate(CanvasX(0.75 * (allocation.width as u64) as f64), CanvasY(10.));
                show_gleis(cairo, lego::GERADE);
                show_gleis(cairo, lego::KURVE);
                show_gleis(cairo, lego::weiche(weiche::Richtung::Links));
                show_gleis(cairo, lego::KREUZUNG);
            });
            glib::signal::Inhibit(false)
        }
        drawing_area.connect_draw(test);
        window.add(&drawing_area);

        window.show_all();
    });

    application.run(&[]);
}

fn show_gleis<T>(cairo: &Cairo, gleis: T)
where
    T: Zeichnen,
    T::AnchorPoints: anchor::Lookup<T::AnchorName>,
{
    cairo.with_save_restore(|cairo| {
        cairo.translate(CanvasX(-0.5 * (gleis.width() as f64)), CanvasY(0.));
        // zeichne Box umd das Gleis (überprüfen von width, height)
        cairo.with_save_restore(|cairo| {
            cairo.set_source_rgb(0., 1., 0.);
            let left = CanvasX(0.);
            let right = CanvasX(gleis.width() as f64);
            let up = CanvasY(0.);
            let down = CanvasY(gleis.height() as f64);
            cairo.move_to(left, up);
            cairo.line_to(right, up);
            cairo.line_to(right, down);
            cairo.line_to(left, down);
            cairo.line_to(left, up);
            cairo.stroke();
        });
        // zeichne gleis
        cairo.with_save_restore(|cairo| {
            gleis.zeichne(cairo);
            cairo.stroke();
        });
        // zeichne anchor points
        cairo.with_save_restore(|cairo| {
            cairo.set_source_rgb(0., 0., 1.);
            gleis.anchor_points().foreach(
                |anchor::Point {
                     position: anchor::Position { x, y },
                     direction: anchor::Direction { dx, dy },
                 }| {
                    cairo.move_to(*x, *y);
                    cairo.line_to(
                        *x + 5. * CanvasAbstand::from(*dx),
                        *y + 5. * CanvasAbstand::from(*dy),
                    );
                },
            );
            cairo.stroke()
        });
    });
    // verschiebe Context, damit nächstes Gleis unter das aktuelle gezeichnet wird
    let skip_y: CanvasAbstand = CanvasY(10.).into();
    cairo.translate(CanvasX(0.), CanvasY(gleis.height() as f64) + skip_y);
}
