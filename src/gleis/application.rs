//! iced::Application für die Gleis-Anzeige

use version::version;

use super::*;

mod background;
mod scrollable;

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    Weiche(Weiche<Z>),
    DreiwegeWeiche(DreiwegeWeiche<Z>),
    KurvenWeiche(KurvenWeiche<Z>),
    SKurvenWeiche(SKurvenWeiche<Z>),
    Kreuzung(Kreuzung<Z>),
}
macro_rules! impl_button_message {
    ($type:ident) => {
        impl<Z> ButtonMessage<Message<Z>> for $type<Z> {
            fn to_message(&self) -> Message<Z> {
                Message::$type(self.clone())
            }
        }
    };
}
impl_button_message! {Gerade}
impl_button_message! {Kurve}
impl_button_message! {Weiche}
impl_button_message! {DreiwegeWeiche}
impl_button_message! {KurvenWeiche}
impl_button_message! {SKurvenWeiche}
impl_button_message! {Kreuzung}

pub struct Zugkontrolle<Z> {
    gleise: Gleise<Z>,
    scrollable_state: iced::scrollable::State,
    geraden: Vec<Button<Gerade<Z>>>,
    kurven: Vec<Button<Kurve<Z>>>,
    weichen: Vec<Button<Weiche<Z>>>,
    dreiwege_weichen: Vec<Button<DreiwegeWeiche<Z>>>,
    kurven_weichen: Vec<Button<KurvenWeiche<Z>>>,
    s_kurven_weichen: Vec<Button<SKurvenWeiche<Z>>>,
    kreuzungen: Vec<Button<Kreuzung<Z>>>,
}
impl<Z: 'static + Zugtyp + Send> iced::Application for Zugkontrolle<Z> {
    type Executor = iced::executor::Default;
    type Flags = Gleise<Z>;
    type Message = Message<Z>;

    fn new(gleise: Self::Flags) -> (Self, iced::Command<Self::Message>) {
        (
            Zugkontrolle {
                gleise,
                scrollable_state: iced::scrollable::State::new(),
                geraden: Z::geraden().into_iter().map(Button::new).collect(),
                kurven: Z::kurven().into_iter().map(Button::new).collect(),
                weichen: Z::weichen().into_iter().map(Button::new).collect(),
                dreiwege_weichen: Z::dreiwege_weichen().into_iter().map(Button::new).collect(),
                kurven_weichen: Z::kurven_weichen().into_iter().map(Button::new).collect(),
                s_kurven_weichen: Z::s_kurven_weichen().into_iter().map(Button::new).collect(),
                kreuzungen: Z::kreuzungen().into_iter().map(Button::new).collect(),
            },
            iced::Command::none(),
        )
    }

    fn title(&self) -> String {
        format!("Zugkontrolle {}", version!())
    }

    fn update(
        &mut self,
        message: Self::Message,
        _clipboard: &mut iced::Clipboard,
    ) -> iced::Command<Self::Message> {
        macro_rules! add_gleis {
            ($gleis:expr) => {{
                self.gleise.add_at_mouse_height($gleis);
            }};
        }
        match message {
            Message::Gerade(gerade) => add_gleis!(gerade),
            Message::Kurve(kurve) => add_gleis!(kurve),
            Message::Weiche(weiche) => add_gleis!(weiche),
            Message::DreiwegeWeiche(dreiwege_weiche) => add_gleis!(dreiwege_weiche),
            Message::KurvenWeiche(kurven_weiche) => add_gleis!(kurven_weiche),
            Message::SKurvenWeiche(s_kurven_weiche) => add_gleis!(s_kurven_weiche),
            Message::Kreuzung(kreuzung) => add_gleis!(kreuzung),
        }

        iced::Command::none()
    }

    fn view(&mut self) -> iced::Element<Self::Message> {
        let Zugkontrolle {
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = self;

        let mut scrollable = iced::Scrollable::new(scrollable_state);
        let mut max_width = 0;
        macro_rules! add_buttons {
                    ($($vec: expr),*) => {
                        $(
                        for button in $vec.iter() {
                            max_width = max_width.max((canvas::X(0.) + button.size().width).0.ceil() as u16);
                        }
                    )*
                    $(
                        for button in $vec {
                            scrollable = scrollable.push(
                                button.to_iced()
                                    .width(iced::Length::Units(max_width))
                                    .height(iced::Length::Shrink)
                            );
                        }
                    )*
                    }
                }
        add_buttons!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        );
        let scrollable_style = scrollable::Collection::new(10);
        let scroller_width = scrollable_style.width();
        iced::Container::new(
            iced::Row::new()
                .push(
                    iced::Container::new(
                        scrollable
                            .scroller_width(scroller_width)
                            .width(iced::Length::Fill)
                            .height(iced::Length::Fill)
                            .style(scrollable_style),
                    )
                    .width(iced::Length::Units(max_width + scroller_width))
                    .height(iced::Length::Fill)
                    .style(background::White),
                )
                .push(
                    iced::Container::new(
                        iced::Canvas::new(gleise)
                            .width(iced::Length::Fill)
                            .height(iced::Length::Fill),
                    )
                    .width(iced::Length::Fill)
                    .height(iced::Length::Fill)
                    .style(background::White),
                )
                .spacing(1),
        )
        .width(iced::Length::Fill)
        .height(iced::Length::Fill)
        .style(background::Black)
        .into()
    }
}