use gio::prelude::*;
use gtk::prelude::*;
use gtk::{Application, ApplicationWindow, Button};

pub mod zug;

fn main() {
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
