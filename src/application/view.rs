//! Methoden für die [view](iced::Application::view)-Methode des [iced::Application]-Traits.

use std::fmt::Debug;

use iced::{
    Alignment, Button, Column, Container, Element, Length, Row, Rule, Scrollable, Slider, Space,
    Text,
};
use log::error;
use num_traits::NumCast;

use crate::{
    application::{
        bewegen::Bewegen,
        drehen::Drehen,
        geschwindigkeit::{self, LeiterAnzeige},
        lizenzen::{self, Lizenzen},
        modal::Modal,
        speichern_laden, streckenabschnitt,
        style::{linie::TRENNLINIE, sammlung::Sammlung},
        touch_canvas, weiche, AnyGleisUnit, AuswahlZustand, MessageBox, Modus, Nachricht,
        NachrichtClone, Zugkontrolle,
    },
    gleis::{
        gerade::GeradeUnit,
        gleise::Gleise,
        knopf::Knopf,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
            s_kurve::SKurvenWeicheUnit,
        },
    },
    steuerung::geschwindigkeit::Leiter,
    typen::{skalar::Skalar, Zeichnen},
};

trait MitTeilNachricht<'t, Msg: 'static>: Into<Element<'t, Msg>> {
    fn mit_teil_nachricht<Leiter: 'static + LeiterAnzeige>(
        self,
        konstruktor: impl Fn(Msg) -> Nachricht<Leiter> + 'static,
    ) -> Element<'t, Nachricht<Leiter>> {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

impl<L> Zugkontrolle<L>
where
    L: 'static + Debug + LeiterAnzeige,
    <L as Leiter>::Fahrtrichtung: Clone,
{
    /// [view](iced::Application::view)-Methode für [Zugkontrolle].
    pub fn view(&mut self) -> Element<'_, Nachricht<L>> {
        let Zugkontrolle {
            gleise,
            scrollable_zustand,
            scrollable_style,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            auswahl,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_zustand,
            message_box,
            bewegen,
            drehen,
            zoom,
            speichern_laden,
            lager: _,
            speichern_gefärbt: _,
            bewegung: _,
            sender: _,
            empfänger: _,
            zeige_lizenzen,
        } = self;
        let aktueller_modus = gleise.modus();
        let aktueller_zoom = gleise.skalierfaktor();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            geschwindigkeit_button_zustand,
            bewegen,
            drehen,
            zoom,
            aktueller_zoom,
            speichern_laden,
            zeige_lizenzen,
        );
        let row_with_scrollable = row_with_scrollable(
            aktueller_modus,
            scrollable_zustand,
            *scrollable_style,
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

        let column: Element<'_, Nachricht<L>> = Column::new()
            .push(top_row)
            .push(Rule::horizontal(1).style(TRENNLINIE))
            .push(
                row_with_scrollable.push(
                    Container::new(
                        Element::from(
                            touch_canvas::Canvas::new(gleise)
                                .width(Length::Fill)
                                .height(Length::Fill),
                        )
                        .map(Into::into),
                    )
                    .width(Length::Fill)
                    .height(Length::Fill),
                ),
            )
            .into();

        let modal = Modal::neu(auswahl, column, |modal| match modal {
            AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl) => Element::from(
                streckenabschnitt::Auswahl::neu(streckenabschnitt_auswahl),
            )
            .map(|message| {
                use streckenabschnitt::AuswahlNachricht::*;
                match message {
                    Schließe => Nachricht::SchließeAuswahl,
                    Wähle(wahl) => Nachricht::WähleStreckenabschnitt(wahl),
                    Hinzufügen(geschwindigkeit, name, farbe, output) => {
                        Nachricht::HinzufügenStreckenabschnitt(
                            geschwindigkeit,
                            name,
                            farbe,
                            output,
                        )
                    },
                    Lösche(name) => Nachricht::LöscheStreckenabschnitt(name),
                }
            }),
            AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl) => Element::from(
                <L as LeiterAnzeige>::auswahl_neu(geschwindigkeit_auswahl),
            )
            .map(|message| {
                use geschwindigkeit::AuswahlNachricht::*;
                match message {
                    Schließen => Nachricht::SchließeAuswahl,
                    Hinzufügen(name, geschwindigkeit) => {
                        Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit)
                    },
                    Löschen(name) => Nachricht::LöscheGeschwindigkeit(name),
                }
            }),
            AuswahlZustand::Weiche(zustand, als_message) => {
                let als_message_clone = als_message.clone();
                Element::from(weiche::Auswahl::neu(zustand)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeAuswahl,
                    }
                })
            },
            AuswahlZustand::DreiwegeWeiche(zustand, als_message) => {
                let als_message_clone = als_message.clone();
                Element::from(weiche::Auswahl::neu(zustand)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeAuswahl,
                    }
                })
            },
            AuswahlZustand::KurvenWeiche(zustand, als_message) => {
                let als_message_clone = als_message.clone();
                Element::from(weiche::Auswahl::neu(zustand)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => als_message_clone(steuerung),
                        Schließen => Nachricht::SchließeAuswahl,
                    }
                })
            },
            AuswahlZustand::ZeigeLizenzen(zustand) => {
                Element::from(Lizenzen::neu(zustand, *scrollable_style))
                    .map(|lizenzen::Nachricht::Schließen| Nachricht::SchließeAuswahl)
            },
        })
        .on_esc(&|| Nachricht::SchließeAuswahl);

        Modal::neu(message_box, modal, |MessageBox { titel, nachricht, button_zustand }| {
            Element::from(
                iced_aw::Card::new(
                    Text::new(&*titel),
                    Column::new().push(Text::new(&*nachricht)).push(
                        iced::Button::new(button_zustand, Text::new("Ok"))
                            .on_press(NachrichtClone::SchließeMessageBox),
                    ),
                )
                .width(Length::Shrink),
            )
            .map(Nachricht::from)
        })
        .on_esc(&|| Nachricht::SchließeMessageBox)
        .into()
    }
}

fn top_row<'t, L>(
    aktueller_modus: Modus,
    streckenabschnitt: &'t mut streckenabschnitt::AnzeigeZustand,
    streckenabschnitt_festlegen: &'t mut bool,
    geschwindigkeit_button_zustand: &'t mut iced::button::State,
    bewegen: &'t mut Bewegen,
    drehen: &'t mut Drehen,
    zoom: &'t mut iced::slider::State,
    aktueller_zoom: Skalar,
    speichern_laden: &'t mut speichern_laden::Zustand,
    zeige_lizenzen: &'t mut iced::button::State,
) -> Row<'t, Nachricht<L>>
where
    L: 'static + Debug + LeiterAnzeige,
    <L as Leiter>::Fahrtrichtung: Clone,
{
    let modus_radios = Column::new()
        .push(Modus::Bauen.erstelle_radio(aktueller_modus))
        .push(Modus::Fahren.erstelle_radio(aktueller_modus));
    let bewegen =
        touch_canvas::Canvas::new(bewegen).width(Length::Units(50)).height(Length::Units(50));
    let drehen =
        touch_canvas::Canvas::new(drehen).width(Length::Units(50)).height(Length::Units(50));
    let skalieren_slider = Column::new()
        .push(Text::new(format!("Zoom {:.2}", aktueller_zoom.0)))
        .push(
            Slider::new(zoom, -2.5..=1.5, aktueller_zoom.0.ln(), |exponent| {
                NachrichtClone::Skalieren(Skalar(exponent.exp()))
            })
            .step(0.01)
            .width(Length::Units(100)),
        )
        .align_items(Alignment::Center);
    let speichern_laden = speichern_laden::SpeichernLaden::neu(speichern_laden);
    let mut row = Row::new()
        .push(modus_radios.mit_teil_nachricht(Nachricht::Modus))
        .push(bewegen.mit_teil_nachricht(Nachricht::Bewegen))
        .push(drehen.mit_teil_nachricht(Nachricht::Winkel))
        .push(Element::new(skalieren_slider).map(Nachricht::from));

    // Streckenabschnitte und Geschwindigkeiten können nur im Bauen-Modus geändert werden
    if let Modus::Bauen { .. } = aktueller_modus {
        row = row
            .push(
                Element::from(streckenabschnitt::Anzeige::neu(
                    streckenabschnitt,
                    *streckenabschnitt_festlegen,
                ))
                .map(|message| match message {
                    streckenabschnitt::AnzeigeNachricht::Auswählen => {
                        Nachricht::ZeigeAuswahlStreckenabschnitt
                    },
                    streckenabschnitt::AnzeigeNachricht::Festlegen(festlegen) => {
                        Nachricht::StreckenabschnittFestlegen(festlegen)
                    },
                }),
            )
            .push(
                Element::new(
                    iced::Button::new(
                        geschwindigkeit_button_zustand,
                        Text::new("Geschwindigkeiten"),
                    )
                    .on_press(NachrichtClone::ZeigeAuswahlGeschwindigkeit),
                )
                .map(Nachricht::from),
            );
    }

    row.push(Space::new(Length::Fill, Length::Shrink))
        .push(Element::from(speichern_laden).map(|message| match message {
            speichern_laden::Nachricht::Speichern(pfad) => Nachricht::Speichern(pfad),
            speichern_laden::Nachricht::Laden(pfad) => Nachricht::Laden(pfad),
        }))
        .push(
            Element::from(
                Button::new(zeige_lizenzen, Text::new("Lizenzen"))
                    .on_press(NachrichtClone::ZeigeLizenzen),
            )
            .map(Nachricht::from),
        )
        .padding(5)
        .spacing(5)
        .width(Length::Fill)
        .height(Length::Shrink)
}

fn row_with_scrollable<'t, Leiter: 'static + LeiterAnzeige>(
    aktueller_modus: Modus,
    scrollable_zustand: &'t mut iced::scrollable::State,
    scrollable_style: Sammlung,
    geraden: &'t mut Vec<Knopf<GeradeUnit>>,
    kurven: &'t mut Vec<Knopf<KurveUnit>>,
    weichen: &'t mut Vec<Knopf<WeicheUnit>>,
    dreiwege_weichen: &'t mut Vec<Knopf<DreiwegeWeicheUnit>>,
    kurven_weichen: &'t mut Vec<Knopf<KurvenWeicheUnit>>,
    s_kurven_weichen: &'t mut Vec<Knopf<SKurvenWeicheUnit>>,
    kreuzungen: &'t mut Vec<Knopf<KreuzungUnit>>,
    geschwindigkeiten: &'t mut geschwindigkeit::Map<Leiter>,
    gleise: &Gleise<Leiter>,
) -> Row<'t, Nachricht<Leiter>> {
    let mut scrollable = Scrollable::new(scrollable_zustand);
    let scroller_width = scrollable_style.breite();
    let mut width = Length::Shrink;
    match aktueller_modus {
        Modus::Bauen => {
            let mut max_breite = None;
            macro_rules! max_breite_berechnen {
                ($($vec: expr),* $(,)?) => {
                    $(
                        for button in $vec.iter() {
                            let größe = button.rechteck().größe();
                            let breite = NumCast::from(größe.x.0.ceil()).unwrap_or(u16::MAX);
                            max_breite = max_breite.max(Some(breite));
                        }
                    )*
                }
            }
            fn knöpfe_hinzufügen<'t, Leiter, T>(
                max_breite: &mut Option<u16>,
                scrollable: &mut Scrollable<'t, NachrichtClone<Leiter>>,
                buttons: &'t mut Vec<Knopf<T>>,
            ) where
                Leiter: 'static + LeiterAnzeige,
                T: Zeichnen + Clone + Into<AnyGleisUnit>,
            {
                take_mut::take(scrollable, |mut scrollable| {
                    for button in buttons {
                        scrollable = scrollable.push(button.als_iced_widget(*max_breite))
                    }
                    scrollable
                })
            }
            macro_rules! knöpfe_hinzufügen {
                ($($vec: expr),* $(,)?) => {
                    max_breite_berechnen!($($vec),*);
                    $(knöpfe_hinzufügen(&mut max_breite, &mut scrollable, $vec);)*
                }
            }
            knöpfe_hinzufügen!(
                geraden,
                kurven,
                weichen,
                dreiwege_weichen,
                kurven_weichen,
                s_kurven_weichen,
                kreuzungen
            );
            if let Some(max) = max_breite {
                width = Length::Units(max + scroller_width);
            }
        },
        Modus::Fahren => {
            scrollable = scrollable.push(Text::new("Geschwindigkeiten")).spacing(1);
            for (name, anzeige_zustand) in geschwindigkeiten {
                let geschwindigkeit = if let Some(geschwindigkeit) = gleise.geschwindigkeit(name) {
                    geschwindigkeit
                } else {
                    error!("Anzeige für entfernte Geschwindigkeit {}!", name.0);
                    continue;
                };
                scrollable = scrollable.push(
                    Element::from(Leiter::anzeige_neu(geschwindigkeit, anzeige_zustand))
                        .map(NachrichtClone::AktionGeschwindigkeit),
                );
            }
            // TODO Wegstrecken?, Pläne?, Separator dazwischen?
        },
    }
    Row::new()
        .push(
            Container::new(
                Element::new(
                    scrollable
                        .scroller_width(scroller_width)
                        .width(Length::Shrink)
                        .height(Length::Fill)
                        .style(scrollable_style),
                )
                .map(Nachricht::from),
            )
            .width(width)
            .height(Length::Fill),
        )
        .push(Rule::vertical(1).style(TRENNLINIE))
}
