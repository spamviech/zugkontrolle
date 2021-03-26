use gio::prelude::*;
use gtk::prelude::*;
use gtk::{Application, ApplicationWindow, Button};
use simple_logger::SimpleLogger;

pub mod gleis;
pub mod zugtyp;

#[cfg(feature = "derive")]
pub use zugkontrolle_derive::AnchorLookup;

fn main() {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    let application =
        Application::new(None, Default::default()).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let window = ApplicationWindow::new(app);
        window.set_title("Zugkontrolle");
        window.set_default_size(600, 400);

        let button = Button::with_label("Click me!");
        button.connect_clicked(|b| {
            let next = match b.get_label() {
                Some(g) => match g.as_str() {
                    "Click me!" => "Click me again!",
                    _ => "Click me!",
                },
                None => panic!("Button without label"),
            };
            b.set_label(next);
        });
        window.add(&button);

        window.show_all();
    });

    application.run(&[]);
}
