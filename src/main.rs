//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use gio::prelude::*;
use gtk::prelude::*;
use gtk::{
    Application, ApplicationWindow, Orientation, Paned, PanedBuilder, ScrolledWindow,
    ScrolledWindowBuilder,
};
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::definition::GleisDefinition;
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::widget::{Gleis, Gleise, Position};
use zugkontrolle::gleis::{lego, maerklin};
use zugkontrolle::zugtyp::{Lego, Maerklin};

struct AppendGleise<'t, Z> {
    gleise: &'t mut Gleise<Z>,
    y: CanvasY,
}
impl<'t, Z> AppendGleise<'t, Z> {
    fn new(gleise: &'t mut Gleise<Z>) -> AppendGleise<'t, Z> {
        AppendGleise { gleise, y: CanvasY(5.) }
    }
}

impl<'t, Z: Zugtyp + Eq + Debug> AppendGleise<'t, Z> {
    fn append<T: Zeichnen + Into<GleisDefinition<Z>>>(&mut self, definition: T) {
        let x: CanvasX =
            CanvasX(200.) - 0.5 * CanvasAbstand::from(CanvasX(definition.width() as f64));
        let height: CanvasAbstand = CanvasY(definition.height() as f64).into();
        self.gleise.add(Gleis {
            definition: definition.into(),
            position: Position { x, y: self.y, winkel: Angle::new(0.) },
        });
        self.y += height + CanvasAbstand::from(CanvasY(5.));
    }
}

fn main() {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    let application =
        Application::new(None, Default::default()).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Zugkontrolle");

        let paned: Paned =
            PanedBuilder::new().orientation(Orientation::Horizontal).position(400).build();
        window.add(&paned);

        let scrolled_window1: ScrolledWindow = ScrolledWindowBuilder::new()
            .propagate_natural_width(true)
            .propagate_natural_height(true)
            .build();
        let mut gleise_maerklin: Gleise<Maerklin> =
            Gleise::new_with_size(CanvasX(400.), CanvasY(800.));
        gleise_maerklin.add_to_container(&scrolled_window1);
        paned.add1(&scrolled_window1);

        let scrolled_window2: ScrolledWindow = ScrolledWindowBuilder::new()
            .propagate_natural_width(true)
            .propagate_natural_height(true)
            .build();
        let mut gleise_lego: Gleise<Lego> = Gleise::new_with_size(CanvasX(400.), CanvasY(800.));
        gleise_lego.add_to_container(&scrolled_window2);
        paned.add2(&scrolled_window2);

        window.show_all();

        let mut append_maerklin = AppendGleise::new(&mut gleise_maerklin);
        append_maerklin.append(maerklin::GERADE_5106);
        append_maerklin.append(maerklin::KURVE_5100);
        append_maerklin.append(maerklin::WEICHE_5202_LINKS);
        append_maerklin.append(maerklin::DREIWEGE_WEICHE_5214);
        append_maerklin.append(maerklin::KURVEN_WEICHE_5140_LINKS);
        append_maerklin.append(maerklin::KREUZUNG_5207);

        let mut append_lego = AppendGleise::new(&mut gleise_lego);
        append_lego.append(lego::GERADE);
        append_lego.append(lego::KURVE);
        append_lego.append(lego::WEICHE_RECHTS);
        append_lego.append(lego::KREUZUNG);
    });

    application.run(&[]);
}
