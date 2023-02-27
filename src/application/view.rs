//! Methoden für die [view](iced::Application::view)-Methode des [iced::Application]-Traits.

use std::fmt::Debug;

use iced::{
    widget::{Button, Canvas, Column, Container, Row, Rule, Scrollable, Slider, Space, Text},
    Alignment, Element, Length, Point, Renderer,
};
use log::error;

use crate::{
    application::{
        bewegen::Bewegen,
        drehen::Drehen,
        geschwindigkeit::{self, LeiterAnzeige},
        lizenzen::{self, Lizenzen},
        modal::{self, Modal},
        speichern_laden, streckenabschnitt,
        style::{
            linie::{Linie, TRENNLINIE},
            sammlung::Sammlung,
        },
        weiche, AnyGleisUnit, AuswahlZustand, MessageBox, Modus, Nachricht, NachrichtClone,
        Zugkontrolle,
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
    fn mit_teil_nachricht<L: 'static + LeiterAnzeige<S, R>, S: 'static, R>(
        self,
        konstruktor: impl Fn(Msg) -> Nachricht<L, S> + 'static,
    ) -> Element<'t, Nachricht<L, S>> {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + Debug + LeiterAnzeige<S, Renderer>,
    <L as Leiter>::Fahrtrichtung: Clone,
    S: 'static,
{
    /// [view](iced::Application::view)-Methode für [Zugkontrolle].
    pub fn view(&self) -> Element<'_, Nachricht<L, S>> {
        let Zugkontrolle {
            gleise,
            scrollable_style,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            geschwindigkeiten,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            bewegen,
            drehen,
            lager: _,
            speichern_gefärbt: _,
            bewegung: _,
            sender: _,
            empfänger: _,
            initialer_pfad,
        } = self;
        let aktueller_modus = gleise.modus();
        let aktueller_zoom = gleise.skalierfaktor();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell_festlegen,
            bewegen,
            drehen,
            aktueller_zoom,
            initialer_pfad,
        );
        let row_with_scrollable = row_with_scrollable(
            aktueller_modus,
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

        let column = Element::new(
            Column::new().push(top_row).push(Rule::horizontal(1).style(TRENNLINIE)).push(
                row_with_scrollable.push(
                    Container::new(
                        Element::new(Canvas::new(gleise).width(Length::Fill).height(Length::Fill))
                            .map(Nachricht::from),
                    )
                    .width(Length::Fill)
                    .height(Length::Fill),
                ),
            ),
        );

        // let modal = Modal::neu(todo!("{:?}", auswahl), column, |modal| match modal {
        //     AuswahlZustand::Streckenabschnitt(streckenabschnitt_auswahl) => Element::new(
        //         streckenabschnitt::Auswahl::neu(streckenabschnitt_auswahl),
        //     )
        //     .map(|message| {
        //         use streckenabschnitt::AuswahlNachricht::*;
        //         match message {
        //             Schließe => Nachricht::SchließeAuswahl,
        //             Wähle(wahl) => Nachricht::WähleStreckenabschnitt(wahl),
        //             Hinzufügen(geschwindigkeit, name, farbe, output) => {
        //                 Nachricht::HinzufügenStreckenabschnitt(
        //                     geschwindigkeit,
        //                     name,
        //                     farbe,
        //                     output,
        //                 )
        //             },
        //             Lösche(name) => Nachricht::LöscheStreckenabschnitt(name),
        //         }
        //     }),
        //     AuswahlZustand::Geschwindigkeit(geschwindigkeit_auswahl) => {
        //         Element::new(<L as LeiterAnzeige<S>>::auswahl_neu(geschwindigkeit_auswahl, todo!()))
        //             .map(|message| {
        //                 use geschwindigkeit::AuswahlNachricht::*;
        //                 match message {
        //                     Schließen => Nachricht::SchließeAuswahl,
        //                     Hinzufügen(name, geschwindigkeit) => {
        //                         Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit)
        //                     },
        //                     Löschen(name) => Nachricht::LöscheGeschwindigkeit(name),
        //                 }
        //             })
        //     },
        //     AuswahlZustand::Weiche(zustand, als_message) => {
        //         let als_message_clone = als_message.clone();
        //         Element::new(weiche::Auswahl::neu(zustand)).map(move |message| {
        //             use weiche::Nachricht::*;
        //             match message {
        //                 Festlegen(steuerung) => als_message_clone(steuerung),
        //                 Schließen => Nachricht::SchließeAuswahl,
        //             }
        //         })
        //     },
        //     AuswahlZustand::DreiwegeWeiche(zustand, als_message) => {
        //         let als_message_clone = als_message.clone();
        //         Element::new(weiche::Auswahl::neu(zustand)).map(move |message| {
        //             use weiche::Nachricht::*;
        //             match message {
        //                 Festlegen(steuerung) => als_message_clone(steuerung),
        //                 Schließen => Nachricht::SchließeAuswahl,
        //             }
        //         })
        //     },
        //     AuswahlZustand::KurvenWeiche(zustand, als_message) => {
        //         let als_message_clone = als_message.clone();
        //         Element::new(weiche::Auswahl::neu(zustand)).map(move |message| {
        //             use weiche::Nachricht::*;
        //             match message {
        //                 Festlegen(steuerung) => als_message_clone(steuerung),
        //                 Schließen => Nachricht::SchließeAuswahl,
        //             }
        //         })
        //     },
        //     AuswahlZustand::ZeigeLizenzen(zustand) => {
        //         Element::new(Lizenzen::neu(zustand, *scrollable_style))
        //             .map(|lizenzen::Nachricht::Schließen| Nachricht::SchließeAuswahl)
        //     },
        // })
        // .on_esc(&|| Nachricht::SchließeAuswahl);
        let modal: Element<'_, _> = todo!();
        let _ = ();

        Modal::neu(modal, &|message_box| {
            let MessageBox { titel, nachricht } = message_box;
            Element::new(
                iced_aw::Card::new(
                    Text::new(&*titel),
                    Scrollable::new(Text::new(&*nachricht)).height(Length::Fixed(300.)),
                )
                .foot(
                    iced::widget::Button::new(Text::new("Ok"))
                        .on_press(NachrichtClone::SchließeMessageBox),
                )
                .width(Length::Shrink),
            )
            .map(Nachricht::from)
            .map(modal::Nachricht::Underlay)
        })
        .schließe_bei_esc()
        .into()
    }
}

const BEWEGEN_HÖHE: f32 = 50.;
const BEWEGEN_BREITE: f32 = 50.;
const DREHEN_HÖHE: f32 = 50.;
const DREHEN_BREITE: f32 = 50.;
const SKALIEREN_BREITE: f32 = 75.;

fn top_row<'t, L, S, R>(
    aktueller_modus: Modus,
    streckenabschnitt_festlegen: &'t bool,
    bewegen: &'t Bewegen,
    drehen: &'t Drehen,
    aktueller_zoom: Skalar,
    initialer_pfad: &str,
) -> Row<'t, Nachricht<L, S>>
where
    L: 'static + Debug + LeiterAnzeige<S, R>,
    <L as Leiter>::Fahrtrichtung: Clone,
    S: 'static,
{
    let modus_radios = Column::new()
        .push(Modus::Bauen.erstelle_radio(aktueller_modus))
        .push(Modus::Fahren.erstelle_radio(aktueller_modus));
    let bewegen = Canvas::new(bewegen)
        .width(Length::Fixed(BEWEGEN_HÖHE))
        .height(Length::Fixed(BEWEGEN_BREITE));
    let drehen =
        Canvas::new(drehen).width(Length::Fixed(DREHEN_HÖHE)).height(Length::Fixed(DREHEN_BREITE));
    let skalieren_slider = Column::new()
        .push(Text::new(format!("Zoom {:.2}", aktueller_zoom.0)))
        .push(
            Slider::new(-2.5..=1.5, aktueller_zoom.0.ln(), |exponent| {
                NachrichtClone::Skalieren(Skalar(exponent.exp()))
            })
            .step(0.01)
            .width(Length::Fixed(SKALIEREN_BREITE)),
        )
        .align_items(Alignment::Center);
    let speichern_laden = speichern_laden::SpeichernLaden::neu(initialer_pfad);
    let mut row = Row::new()
        .push(modus_radios.mit_teil_nachricht(Nachricht::Modus))
        .push(bewegen.mit_teil_nachricht(Nachricht::Bewegen))
        .push(drehen.mit_teil_nachricht(Nachricht::Winkel))
        .push(Element::new(skalieren_slider).map(Nachricht::from));

    // Streckenabschnitte und Geschwindigkeiten können nur im Bauen-Modus geändert werden
    if let Modus::Bauen { .. } = aktueller_modus {
        let geschwindigkeit = Element::new(
            Button::new(Text::new("Geschwindigkeiten"))
                .on_press(NachrichtClone::ZeigeAuswahlGeschwindigkeit),
        )
        .map(Nachricht::from);
        let streckenabschnitt = Element::new(streckenabschnitt::Anzeige::neu(
            todo!("streckenabschnitt"),
            *streckenabschnitt_festlegen,
        ))
        .map(Nachricht::from);
        row = row.push(Column::new().push(geschwindigkeit).push(streckenabschnitt).spacing(1));
    }

    row.push(Space::new(Length::Fill, Length::Shrink))
        .push(Element::new(speichern_laden).map(|message| match message {
            speichern_laden::Nachricht::Speichern(pfad) => Nachricht::Speichern(pfad),
            speichern_laden::Nachricht::Laden(pfad) => Nachricht::Laden(pfad),
        }))
        .push(
            Element::new(
                Button::new(Text::new("Lizenzen")).on_press(NachrichtClone::ZeigeLizenzen),
            )
            .map(Nachricht::from),
        )
        .padding(5)
        .spacing(5)
        .width(Length::Fill)
        .height(Length::Shrink)
}

fn row_with_scrollable<'t, L: 'static + LeiterAnzeige<S, R>, S: 'static, R>(
    aktueller_modus: Modus,
    scrollable_style: Sammlung,
    geraden: &'t Vec<Knopf<GeradeUnit>>,
    kurven: &'t Vec<Knopf<KurveUnit>>,
    weichen: &'t Vec<Knopf<WeicheUnit>>,
    dreiwege_weichen: &'t Vec<Knopf<DreiwegeWeicheUnit>>,
    kurven_weichen: &'t Vec<Knopf<KurvenWeicheUnit>>,
    s_kurven_weichen: &'t Vec<Knopf<SKurvenWeicheUnit>>,
    kreuzungen: &'t Vec<Knopf<KreuzungUnit>>,
    geschwindigkeiten: &'t geschwindigkeit::Map<L>,
    gleise: &Gleise<L>,
) -> Row<'t, Nachricht<L, S>> {
    let mut scrollable_row = Row::new();
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
                            let breite = Some(größe.x.0);
                            if breite > max_breite {
                                max_breite = breite;
                            }
                        }
                    )*
                }
            }
            fn knöpfe_hinzufügen<'t, L, S, R, T>(
                max_breite: &mut Option<f32>,
                scrollable_row: &mut Row<'t, NachrichtClone<L>>,
                buttons: &'t Vec<Knopf<T>>,
            ) where
                L: 'static + LeiterAnzeige<S, R>,
                T: Zeichnen + Clone + Into<AnyGleisUnit>,
            {
                take_mut::take(scrollable_row, |mut scrollable_row| {
                    for button in buttons {
                        scrollable_row = scrollable_row.push(button.als_iced_widget(*max_breite))
                    }
                    scrollable_row
                })
            }
            macro_rules! knöpfe_hinzufügen {
                ($($vec: expr),* $(,)?) => {
                    max_breite_berechnen!($($vec),*);
                    $(knöpfe_hinzufügen(&mut max_breite, &mut scrollable_row, $vec);)*
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
                width = Length::Fixed(max);
            }
        },
        Modus::Fahren => {
            scrollable_row = scrollable_row.push(Text::new("Geschwindigkeiten")).spacing(1);
            for (name, anzeige_zustand) in geschwindigkeiten {
                let geschwindigkeit = if let Some(geschwindigkeit) = gleise.geschwindigkeit(name) {
                    geschwindigkeit
                } else {
                    error!("Anzeige für entfernte Geschwindigkeit {}!", name.0);
                    continue;
                };
                scrollable_row = scrollable_row.push(
                    Element::new(L::anzeige_neu(todo!("name"), geschwindigkeit))
                        .map(NachrichtClone::AktionGeschwindigkeit),
                );
            }
            // TODO Wegstrecken?, Pläne?, Separator dazwischen?
        },
    }
    let scrollable = Scrollable::new(scrollable_row);
    Row::new()
        .push(
            Container::new(
                Element::new(
                    scrollable
                        .scroller_width(scroller_width)
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
