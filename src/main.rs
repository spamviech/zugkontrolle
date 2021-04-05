//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use std::fmt::Debug;

use iced::{executor, Application, Clipboard, Command, Container, Element, Length, Settings};
use simple_logger::SimpleLogger;

use zugkontrolle::gleis::anchor;
use zugkontrolle::gleis::types::*;
use zugkontrolle::gleis::widget::{Gleis, GleisIdLock, Gleise, GleiseMap, Position};
use zugkontrolle::gleis::{gerade, kurve};
use zugkontrolle::gleis::{lego, maerklin};
use zugkontrolle::zugtyp::{Lego, Maerklin};

struct AppendGleise<'t, Z> {
    gleise: &'t mut Gleise<Z>,
    y: canvas::Y,
}
impl<'t, Z> AppendGleise<'t, Z> {
    fn new(gleise: &'t mut Gleise<Z>) -> AppendGleise<'t, Z> {
        AppendGleise { gleise, y: canvas::Y(5.) }
    }
}

impl<'t, Z: Zugtyp + Eq + Debug> AppendGleise<'t, Z> {
    fn append<T>(&mut self, definition: T) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let size: canvas::Size = definition.size();
        let x: canvas::X = canvas::X(200.) - 0.5 * size.width.to_abstand();
        let height: canvas::Abstand = size.height.into();
        let res = self
            .gleise
            .add(Gleis { definition, position: Position { x, y: self.y, winkel: Angle::new(0.) } });
        self.y += height + canvas::Y(25.).to_abstand();
        res
    }
}

struct Zugkontrolle {
    gleise_maerklin: Gleise<Maerklin>,
    gleise_lego: Gleise<Lego>,
}
impl Application for Zugkontrolle {
    type Executor = executor::Default;
    type Message = ();
    type Flags = (Gleise<Maerklin>, Gleise<Lego>);

    fn new((gleise_maerklin, gleise_lego): Self::Flags) -> (Self, Command<Self::Message>) {
        (Zugkontrolle { gleise_maerklin, gleise_lego }, Command::none())
    }

    fn title(&self) -> String {
        "Zugkontrolle".to_string()
    }

    fn update(
        &mut self,
        _message: Self::Message,
        _clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        Command::none()
    }

    fn view(&mut self) -> Element<Self::Message> {
        let canvas_maerklin = Container::new(
            iced::Canvas::new(self.gleise_maerklin).width(Length::Fill).height(Length::Fill),
        )
        .width(Length::Fill)
        .height(Length::Fill);
        let canvas_lego = Container::new(
            iced::Canvas::new(self.gleise_lego).width(Length::Fill).height(Length::Fill),
        )
        .width(Length::Fill)
        .height(Length::Fill);
        let (pane_state, pane_maerklin) = iced::pane_grid::State::new(canvas_maerklin);
        match pane_state.split(iced::pane_grid::Axis::Horizontal, &pane_maerklin, canvas_lego) {
            Some(_) => {}
            None => panic!("Failed to split pane!"),
        }
        let paned_grid = iced::PaneGrid::new(&mut pane_state, |pane, content| {
            iced::pane_grid::Content::new(content)
        });
        Container::new(paned_grid)
            .width(Length::Units(600))
            .height(Length::Units(800))
            .padding(20)
            .center_x()
            .center_y()
            .into()
    }
}

fn main() -> iced::Result {
    SimpleLogger::new().init().expect("failed to initialize error logging");

    // Märklin-Gleise
    let mut gleise_maerklin: Gleise<Maerklin> = Gleise::new();
    let mut append_maerklin = AppendGleise::new(&mut gleise_maerklin);
    append_maerklin.append(maerklin::GERADE_5106);
    append_maerklin.append(maerklin::KURVE_5100);
    append_maerklin.append(maerklin::WEICHE_5202_LINKS);
    append_maerklin.append(maerklin::DREIWEGE_WEICHE_5214);
    append_maerklin.append(maerklin::KURVEN_WEICHE_5140_LINKS);
    append_maerklin.append(maerklin::KREUZUNG_5207);

    // Lego-Gleise
    let mut gleise_lego: Gleise<Lego> = Gleise::new();
    let mut append_lego = AppendGleise::new(&mut gleise_lego);
    let (gerade_lock, _gerade_anchor_points) = append_lego.append(lego::GERADE);
    let (kurve_lock, _kurve_anchor_points) = append_lego.append(lego::KURVE);
    let (_weiche_id_lock, weiche_anchor_points) = append_lego.append(lego::WEICHE_RECHTS);
    let (kreuzung_lock, _kreuzung_anchor_points) = append_lego.append(lego::KREUZUNG);
    append_lego.append(lego::KREUZUNG);
    // relocate
    if let Some(gleis_id) = &*gerade_lock.read() {
        gleise_lego.relocate(
            gleis_id,
            Position {
                x: canvas::X(250.),
                y: canvas::Y(10.),
                winkel: AngleDegrees::new(90.).into(),
            },
        );
    }
    // attach
    gleise_lego.add_attach(lego::GERADE, gerade::AnchorName::Ende, weiche_anchor_points.gerade);
    // relocate-attach
    if let Some(gleis_id) = &*kurve_lock.read() {
        gleise_lego.relocate_attach(gleis_id, kurve::AnchorName::Ende, weiche_anchor_points.kurve);
    }
    // remove
    let kreuzung_lock_clone = kreuzung_lock.clone();
    gleise_lego.remove(kreuzung_lock);
    // assert!(kreuzung_lock.read().is_none());
    assert!(kreuzung_lock_clone.read().is_none());

    Zugkontrolle::run(Settings { flags: (gleise_maerklin, gleise_lego), ..Settings::default() })
}
