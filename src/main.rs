//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use gio::prelude::*;
use gtk::prelude::*;
use gtk::{Application, ApplicationWindow, DrawingArea};
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::anchor::{self, Lookup};
use zugkontrolle::gleis::definition::Definition;
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::widget::{Gleis, Gleise, Position};
use zugkontrolle::gleis::{lego, maerklin};
use zugkontrolle::zugtyp::{Lego, Maerklin};

fn main() {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    let application =
        Application::new(None, Default::default()).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Zugkontrolle");

        let mut gleise: Gleise<Maerklin> = Gleise::new();
        gleise.set_size_request(CanvasX(800.), CanvasY(600.));

        gleise.add(Gleis {
            definition: maerklin::GERADE_5106.definition(),
            position: Position { x: CanvasX(5.), y: CanvasY(5.), winkel: Angle::new(0.) },
        });
        // drawing_area.set_size_request(800, 600);
        // fn test(drawing_area: &DrawingArea, c: &cairo::Context) -> glib::signal::Inhibit {
        //     let allocation = drawing_area.get_allocation();
        //     let cairo: &Cairo = &Cairo::new(c);
        //     // Märklin Gleise
        //     cairo.with_save_restore(|cairo| {
        //         cairo.translate(CanvasX(0.25 * (allocation.width as u64) as f64), CanvasY(10.));
        //         show_gleis(cairo, maerklin::GERADE_5106);
        //         show_gleis(cairo, maerklin::KURVE_5100);
        //         show_gleis(cairo, maerklin::WEICHE_5202_LINKS);
        //         show_gleis(cairo, maerklin::DREIWEGE_WEICHE_5214);
        //         show_gleis(cairo, maerklin::KURVEN_WEICHE_5140_LINKS);
        //         show_gleis(cairo, maerklin::KREUZUNG_5207);
        //     });
        //     // Lego Gleise
        //     cairo.with_save_restore(|cairo| {
        //         cairo.translate(CanvasX(0.75 * (allocation.width as u64) as f64), CanvasY(10.));
        //         show_gleis(cairo, lego::GERADE);
        //         show_gleis(cairo, lego::KURVE);
        //         show_gleis(cairo, lego::WEICHE_RECHTS);
        //         show_gleis(cairo, lego::KREUZUNG);
        //     });
        //     glib::signal::Inhibit(false)
        // }
        // drawing_area.connect_draw(test);
        // window.add(&drawing_area);

        gleise.add_to_container(&window);

        window.show_all();
    });

    application.run(&[]);
}
