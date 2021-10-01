//! Methoden für die view-Methode des iced::Application-Traits

use std::fmt::Debug;

use log::error;
use num_traits::NumCast;

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::{
        bewegen::Bewegen,
        button::Button,
        drehen::Drehen,
        geschwindigkeit::{self, LeiterAnzeige},
        gleis::{
            gerade::GeradeUnit,
            gleise::Gleise,
            kreuzung::KreuzungUnit,
            kurve::KurveUnit,
            weiche::{
                dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
                s_kurve::SKurvenWeicheUnit,
            },
        },
        scrollable, speichern_laden, streckenabschnitt,
        style::rule,
        touch_canvas,
        typen::*,
        weiche, MessageBox, Modal, Modus, Nachricht, Zugkontrolle,
    },
    zugtyp::Zugtyp,
};

trait MitTeilNachricht<'t, Msg: 'static>: Into<iced::Element<'t, Msg>> {
    fn mit_teil_nachricht<Z>(
        self,
        konstruktor: impl Fn(Msg) -> Nachricht<Z> + 'static,
    ) -> iced::Element<'t, Nachricht<Z>>
    where
        Z: 'static + Zugtyp,
        <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
    {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<iced::Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp + Debug + 'static,
    Z::Leiter: LeiterAnzeige,
    <Z as Zugtyp>::Leiter: Debug,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    pub fn view(&mut self) -> iced::Element<Nachricht<Z>> {
        let Zugkontrolle {
            anschlüsse: _,
            gleise,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            modal_state,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_state,
            message_box,
            bewegen,
            drehen,
            zoom,
            speichern_laden,
            speichern_gefärbt: _,
            bewegung: _,
            sender: _,
            empfänger: _,
        } = self;
        let aktueller_modus = gleise.modus();
        let aktueller_zoom = gleise.skalierfaktor();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_state,
            bewegen,
            drehen,
            zoom,
            aktueller_zoom,
            speichern_laden,
        );
        let row_with_scrollable = row_with_scrollable(
            aktueller_modus,
            scrollable_state,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            gleise,
        );

        let column: iced::Element<Nachricht<Z>> = iced::Column::new()
            .push(top_row)
            .push(iced::Rule::horizontal(1).style(rule::SEPARATOR))
            .push(
                row_with_scrollable.push(
                    iced::Container::new(
                        iced::Element::from(
                            touch_canvas::Canvas::new(gleise)
                                .width(iced::Length::Fill)
                                .height(iced::Length::Fill),
                        )
                        .map(Into::into),
                    )
                    .width(iced::Length::Fill)
                    .height(iced::Length::Fill),
                ),
            )
            .into();

        let modal = iced_aw::Modal::new(modal_state, column, |modal| match modal {
            Modal::Streckenabschnitt(streckenabschnitt_auswahl) => iced::Element::from(
                streckenabschnitt::Auswahl::neu(streckenabschnitt_auswahl),
            )
            .map(|message| {
                use streckenabschnitt::AuswahlNachricht::*;
                match message {
                    Schließe => Nachricht::SchließeModal,
                    Wähle(wahl) => Nachricht::WähleStreckenabschnitt(wahl),
                    Hinzufügen(geschwindigkeit, name, farbe, output) => {
                        Nachricht::HinzufügenStreckenabschnitt(
                            geschwindigkeit,
                            name,
                            farbe,
                            output,
                        )
                    }
                    Lösche(name) => Nachricht::LöscheStreckenabschnitt(name),
                }
            }),
            Modal::Geschwindigkeit(geschwindigkeit_auswahl) => iced::Element::from(
                <<Z as Zugtyp>::Leiter as LeiterAnzeige>::auswahl_neu(geschwindigkeit_auswahl),
            )
            .map(|message| {
                use geschwindigkeit::AuswahlNachricht::*;
                match message {
                    Schließen => Nachricht::SchließeModal,
                    Hinzufügen(name, geschwindigkeit) => {
                        Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit)
                    }
                    Löschen(name) => Nachricht::LöscheGeschwindigkeit(name),
                }
            }),
            Modal::Weiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeModal,
                    }
                })
            }
            Modal::DreiwegeWeiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeModal,
                    }
                })
            }
            Modal::KurvenWeiche(status, als_message) => {
                let als_message_clone = als_message.clone();
                iced::Element::from(weiche::Auswahl::neu(status)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeModal,
                    }
                })
            }
        })
        .on_esc(Nachricht::SchließeModal);

        iced_aw::Modal::new(message_box, modal, |MessageBox { titel, nachricht, button_state }| {
            iced::Element::from(
                iced_aw::Card::new(
                    iced::Text::new(&*titel),
                    iced::Column::new().push(iced::Text::new(&*nachricht)).push(
                        iced::Button::new(button_state, iced::Text::new("Ok"))
                            .on_press(Nachricht::SchließeMessageBox),
                    ),
                )
                .width(iced::Length::Shrink),
            )
        })
        .on_esc(Nachricht::SchließeMessageBox)
        .into()
    }
}

fn top_row<'t, Z>(
    aktueller_modus: Modus,
    streckenabschnitt: &'t mut streckenabschnitt::AnzeigeStatus,
    streckenabschnitt_festlegen: &'t mut bool,
    geschwindigkeit_button_state: &'t mut iced::button::State,
    bewegen: &'t mut Bewegen,
    drehen: &'t mut Drehen,
    zoom: &'t mut iced::slider::State,
    aktueller_zoom: Skalar,
    speichern_laden: &'t mut speichern_laden::Status,
) -> iced::Row<'t, Nachricht<Z>>
where
    Z: 'static + Zugtyp,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    let modus_radios = iced::Column::new()
        .push(Modus::Bauen.make_radio(aktueller_modus))
        .push(Modus::Fahren.make_radio(aktueller_modus));
    let bewegen = touch_canvas::Canvas::new(bewegen)
        .width(iced::Length::Units(50))
        .height(iced::Length::Units(50));
    let drehen = touch_canvas::Canvas::new(drehen)
        .width(iced::Length::Units(50))
        .height(iced::Length::Units(50));
    let skalieren_slider = iced::Column::new()
        .push(iced::Text::new(format!("Zoom {:.2}", aktueller_zoom.0)))
        .push(
            iced::Slider::new(zoom, -2.5..=1.5, aktueller_zoom.0.ln(), |exponent| {
                Nachricht::Skalieren(Skalar(exponent.exp()))
            })
            .step(0.01)
            .width(iced::Length::Units(100)),
        )
        .align_items(iced::Align::Center);
    let speichern_laden = speichern_laden::SpeichernLaden::neu(speichern_laden);
    let mut row = iced::Row::new()
        .push(modus_radios.mit_teil_nachricht(Nachricht::Modus))
        .push(bewegen.mit_teil_nachricht(Nachricht::Bewegen))
        .push(drehen.mit_teil_nachricht(Nachricht::Winkel))
        .push(skalieren_slider);

    // Streckenabschnitte und Geschwindigkeiten können nur im Bauen-Modus geändert werden
    if let Modus::Bauen { .. } = aktueller_modus {
        row = row
            .push(
                iced::Element::from(streckenabschnitt::Anzeige::neu(
                    streckenabschnitt,
                    *streckenabschnitt_festlegen,
                ))
                .map(|message| match message {
                    streckenabschnitt::AnzeigeNachricht::Auswählen => {
                        Nachricht::ZeigeAuswahlStreckenabschnitt
                    }
                    streckenabschnitt::AnzeigeNachricht::Festlegen(festlegen) => {
                        Nachricht::StreckenabschnittFestlegen(festlegen)
                    }
                }),
            )
            .push(
                iced::Button::new(
                    geschwindigkeit_button_state,
                    iced::Text::new("Geschwindigkeiten"),
                )
                .on_press(Nachricht::ZeigeAuswahlGeschwindigkeit),
            );
    }

    row.push(iced::Space::new(iced::Length::Fill, iced::Length::Shrink))
        .push(iced::Element::from(speichern_laden).map(|message| match message {
            speichern_laden::Nachricht::Speichern(pfad) => Nachricht::Speichern(pfad),
            speichern_laden::Nachricht::Laden(pfad) => Nachricht::Laden(pfad),
        }))
        .padding(5)
        .spacing(5)
        .width(iced::Length::Fill)
        .height(iced::Length::Shrink)
}

fn row_with_scrollable<'s, 't, Z>(
    aktueller_modus: Modus,
    scrollable_state: &'t mut iced::scrollable::State,
    geraden: &'t mut Vec<Button<GeradeUnit<Z>>>,
    kurven: &'t mut Vec<Button<KurveUnit<Z>>>,
    weichen: &'t mut Vec<Button<WeicheUnit<Z>>>,
    dreiwege_weichen: &'t mut Vec<Button<DreiwegeWeicheUnit<Z>>>,
    kurven_weichen: &'t mut Vec<Button<KurvenWeicheUnit<Z>>>,
    s_kurven_weichen: &'t mut Vec<Button<SKurvenWeicheUnit<Z>>>,
    kreuzungen: &'t mut Vec<Button<KreuzungUnit<Z>>>,
    geschwindigkeiten: &'t mut geschwindigkeit::Map<Z::Leiter>,
    gleise: &'s Gleise<Z>,
) -> iced::Row<'t, Nachricht<Z>>
where
    Z: 'static + Zugtyp,
    Z::Leiter: Debug + LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as Serialisiere>::Serialisiert: Debug + Clone,
{
    let mut scrollable = iced::Scrollable::new(scrollable_state);
    let scrollable_style = scrollable::Collection::new(10);
    let scroller_width = scrollable_style.width();
    let mut width = iced::Length::Shrink;
    match aktueller_modus {
        Modus::Bauen => {
            let mut max_width = None;
            macro_rules! add_buttons {
                ($($vec: expr),*) => {
                    max_width = max_width.max(Vec::new().into_iter()
                        $(.chain($vec.iter().map(|button| {
                            let größe = button.rechteck().größe();
                            NumCast::from(größe.x.0.ceil()).unwrap_or(u16::MAX)
                        })))*
                        .max());
                    $(
                        for button in $vec {
                            scrollable = scrollable.push(button.als_iced_widget(max_width));
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
            if let Some(max) = max_width {
                width = iced::Length::Units(max + scroller_width);
            }
        }
        Modus::Fahren => {
            scrollable = scrollable.push(iced::Text::new("Geschwindigkeiten")).spacing(1);
            for (name, anzeige_status) in geschwindigkeiten {
                let geschwindigkeit = if let Some(geschwindigkeit) = gleise.geschwindigkeit(name) {
                    geschwindigkeit
                } else {
                    error!("Anzeige für entfernte Geschwindigkeit {}!", name.0);
                    continue;
                };
                let name_clone = name.clone();
                scrollable = scrollable.push(
                    iced::Element::from(Z::Leiter::anzeige_neu(geschwindigkeit, anzeige_status))
                        .map(move |nachricht| Nachricht::GeschwindigkeitAnzeige {
                            name: name_clone.clone(),
                            nachricht,
                        }),
                );
            }
            // TODO Wegstrecken?, Pläne?, Separator dazwischen?
        }
    }
    iced::Row::new()
        .push(
            iced::Container::new(
                scrollable
                    .scroller_width(scroller_width)
                    .width(iced::Length::Shrink)
                    .height(iced::Length::Fill)
                    .style(scrollable_style),
            )
            .width(width)
            .height(iced::Length::Fill),
        )
        .push(iced::Rule::vertical(1).style(rule::SEPARATOR))
}
