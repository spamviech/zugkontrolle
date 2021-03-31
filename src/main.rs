//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use gio::prelude::*;
#[cfg(target_family = "unix")]
use gtk::prelude::*;
#[cfg(target_family = "unix")]
use gtk::{
    Application, ApplicationWindow, Orientation, Paned, PanedBuilder, ScrolledWindow,
    ScrolledWindowBuilder,
};
#[cfg(target_family = "windows")]
use gtk4::prelude::*;
#[cfg(target_family = "windows")]
use gtk4::{
    Application, ApplicationWindow, Orientation, Paned, PanedBuilder, ScrolledWindow,
    ScrolledWindowBuilder,
};
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::anchor;
use zugkontrolle::gleis::definition::GleisDefinition;
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::widget::{GleisIdLock, Gleise, Position};
use zugkontrolle::gleis::{gerade, kurve};
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
    fn append<T>(&mut self, definition: T) -> (GleisIdLock<Z>, T::AnchorPoints)
    where
        T: Zeichnen + Into<GleisDefinition<Z>>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let x: CanvasX =
            CanvasX(200.) - 0.5 * CanvasAbstand::from(CanvasX(definition.width() as f64));
        let height: CanvasAbstand = CanvasY(definition.height() as f64).into();
        let res = self.gleise.add(definition, Position { x, y: self.y, winkel: Angle::new(0.) });
        self.y += height + CanvasAbstand::from(CanvasY(5.));
        res
    }
}

fn main() {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    let application =
        Application::new(None, Default::default()).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        #[cfg(target_family = "unix")]
        window.set_title("Zugkontrolle");
        #[cfg(target_family = "windows")]
        window.set_title(Some("Zugkontrolle"));

        let paned: Paned =
            PanedBuilder::new().orientation(Orientation::Horizontal).position(400).build();
        #[cfg(target_family = "unix")]
        window.add(&paned);
        #[cfg(target_family = "windows")]
        window.set_child(Some(&paned));

        let scrolled_window1: ScrolledWindow = ScrolledWindowBuilder::new()
            .propagate_natural_width(true)
            .propagate_natural_height(true)
            .build();
        let mut gleise_maerklin: Gleise<Maerklin> =
            Gleise::new_with_size(CanvasX(400.), CanvasY(800.));
        #[cfg(target_family = "unix")]
        {
            gleise_maerklin.add_to_container(&scrolled_window1);
            paned.add1(&scrolled_window1);
        }
        #[cfg(target_family = "windows")]
        {
            gleise_maerklin.add_to_scrolled_window(&scrolled_window1);
            paned.set_start_child(&scrolled_window1);
        }

        let scrolled_window2: ScrolledWindow = ScrolledWindowBuilder::new()
            .propagate_natural_width(true)
            .propagate_natural_height(true)
            .build();
        let mut gleise_lego: Gleise<Lego> = Gleise::new_with_size(CanvasX(400.), CanvasY(800.));
        #[cfg(target_family = "unix")]
        {
            gleise_lego.add_to_container(&scrolled_window2);
            paned.add2(&scrolled_window2);
        }
        #[cfg(target_family = "windows")]
        {
            gleise_lego.add_to_scrolled_window(&scrolled_window2);
            paned.set_end_child(&scrolled_window2);
        }

        #[cfg(target_family = "unix")]
        window.show_all();
        #[cfg(target_family = "windows")]
        window.show();

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
        let (_weiche_id_lock, weiche_anchor_points) = append_lego.append(lego::WEICHE_RECHTS);
        append_lego.append(lego::KREUZUNG);
        // try attach
        gleise_lego.attach(lego::GERADE, gerade::AnchorName::Ende, weiche_anchor_points.gerade);
        gleise_lego.attach(lego::KURVE, kurve::AnchorName::Ende, weiche_anchor_points.kurve);
    });

    application.run(&[]);
}
