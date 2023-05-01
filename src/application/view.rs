//! Methoden für die [view](iced::Application::view)-Methode des [iced::Application]-Traits.

use std::fmt::Debug;

use iced::{
    widget::{
        scrollable::{self, Scrollable},
        Button, Canvas, Column, Container, Row, Rule, Slider, Space, Text,
    },
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
    fn mit_teil_nachricht<L: 'static + LeiterAnzeige<'t, S, R>, S: 'static, R>(
        self,
        konstruktor: impl Fn(Msg) -> Nachricht<L, S> + 'static,
    ) -> Element<'t, Nachricht<L, S>> {
        self.into().map(konstruktor)
    }
}

impl<'t, T: Into<Element<'t, Msg>>, Msg: 'static> MitTeilNachricht<'t, Msg> for T {}

impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + Debug + for<'t> LeiterAnzeige<'t, S, Renderer>,
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
        let row_mit_scrollable = row_mit_scrollable(
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
                row_mit_scrollable.push(
                    Container::new(
                        Element::new(Canvas::new(gleise).width(Length::Fill).height(Length::Fill))
                            .map(Nachricht::from),
                    )
                    .width(Length::Fill)
                    .height(Length::Fill),
                ),
            ),
        );

        let zeige_auswahlzustand = |modal: &AuswahlZustand<L, S>| match modal {
            AuswahlZustand::Streckenabschnitt => {
                Element::from(streckenabschnitt::Auswahl::neu(gleise)).map(|message| {
                    use streckenabschnitt::AuswahlNachricht::*;
                    match message {
                        Schließe => modal::Nachricht::VersteckeOverlay,
                        Wähle(wahl) => {
                            modal::Nachricht::Underlay(Nachricht::WähleStreckenabschnitt(wahl))
                        },
                        Hinzufügen(geschwindigkeit, name, farbe, output) => {
                            modal::Nachricht::Underlay(Nachricht::HinzufügenStreckenabschnitt(
                                geschwindigkeit,
                                name,
                                farbe,
                                output,
                            ))
                        },
                        Lösche(name) => {
                            modal::Nachricht::Underlay(Nachricht::LöscheStreckenabschnitt(name))
                        },
                    }
                })
            },
            AuswahlZustand::Geschwindigkeit => Element::from(
                <L as LeiterAnzeige<S, Renderer>>::auswahl_neu(todo!("geschwindigkeiten")),
            )
            .map(|message| {
                use geschwindigkeit::AuswahlNachricht::*;
                match message {
                    Schließen => modal::Nachricht::VersteckeOverlay,
                    Hinzufügen(name, geschwindigkeit) => modal::Nachricht::Underlay(
                        Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit),
                    ),
                    Löschen(name) => {
                        modal::Nachricht::Underlay(Nachricht::LöscheGeschwindigkeit(name))
                    },
                }
            }),
            AuswahlZustand::Weiche(als_message) => {
                let als_message_clone = als_message.clone();
                let weiche: &Option<
                    crate::steuerung::WeicheSerialisiert<
                        crate::gleis::weiche::gerade::Richtung,
                        crate::gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
                    >,
                > = todo!();
                let _ = ();
                Element::from(weiche::Auswahl::neu(weiche)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => {
                            modal::Nachricht::Underlay(als_message_clone(steuerung))
                        },
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
                })
            },
            AuswahlZustand::DreiwegeWeiche(als_message) => {
                let als_message_clone = als_message.clone();
                let dreiwege_weiche: &Option<
                    crate::steuerung::WeicheSerialisiert<
                        crate::gleis::weiche::dreiwege::RichtungInformation,
                        crate::gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
                    >,
                > = todo!();
                let _ = ();
                Element::from(weiche::Auswahl::neu(dreiwege_weiche)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => {
                            modal::Nachricht::Underlay(als_message_clone(steuerung))
                        },
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
                })
            },
            AuswahlZustand::KurvenWeiche(als_message) => {
                let als_message_clone = als_message.clone();
                let kurven_weiche: &Option<
                    crate::steuerung::WeicheSerialisiert<
                        crate::gleis::weiche::kurve::Richtung,
                        crate::gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
                    >,
                > = todo!();
                let _ = ();
                Element::from(weiche::Auswahl::neu(kurven_weiche)).map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => {
                            modal::Nachricht::Underlay(als_message_clone(steuerung))
                        },
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
                })
            },
            AuswahlZustand::ZeigeLizenzen => {
                Element::from(Lizenzen::neu(todo!("lizenzen"), todo!("aktuell"), *scrollable_style))
                    .map(|lizenzen::Nachricht::Schließen| modal::Nachricht::VersteckeOverlay)
            },
        };
        let auswahlzustand: Element<'_, _> =
            Modal::neu(column.map(modal::Nachricht::Underlay), zeige_auswahlzustand)
                .schließe_bei_esc()
                .into();

        let zeige_message_box = |message_box: &MessageBox| {
            let MessageBox { titel, nachricht } = message_box;
            Element::new(
                iced_aw::Card::new(
                    Text::new(titel.clone()),
                    Scrollable::new(Text::new(nachricht.clone())).height(Length::Fixed(300.)),
                )
                .foot(
                    iced::widget::Button::new(Text::new("Ok"))
                        .on_press(NachrichtClone::SchließeMessageBox),
                )
                .width(Length::Shrink),
            )
            .map(Nachricht::from)
            .map(modal::Nachricht::Underlay)
        };
        Modal::neu(auswahlzustand.map(modal::Nachricht::Underlay), zeige_message_box)
            .schließe_bei_esc()
            .into()
    }
}

const BEWEGEN_HÖHE: f32 = 50.;
const BEWEGEN_BREITE: f32 = 50.;
const DREHEN_HÖHE: f32 = 50.;
const DREHEN_BREITE: f32 = 50.;
const SKALIEREN_BREITE: f32 = 75.;

fn top_row<'t, L, S>(
    aktueller_modus: Modus,
    streckenabschnitt_festlegen: &'t bool,
    bewegen: &'t Bewegen,
    drehen: &'t Drehen,
    aktueller_zoom: Skalar,
    initialer_pfad: &'t str,
) -> Row<'t, Nachricht<L, S>>
where
    L: 'static + Debug + LeiterAnzeige<'t, S, Renderer>,
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
        let streckenabschnitt = Element::from(streckenabschnitt::Anzeige::neu(
            todo!("Option<(StreckenabschnittId, Farbe)>"),
            *streckenabschnitt_festlegen,
        ))
        .map(Nachricht::from);
        row = row.push(Column::new().push(geschwindigkeit).push(streckenabschnitt).spacing(1));
    }

    row.push(Space::new(Length::Fill, Length::Shrink))
        .push(Element::from(speichern_laden).map(|message| match message {
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

fn row_mit_scrollable<'t, L: 'static + LeiterAnzeige<'t, S, Renderer>, S: 'static>(
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
                L: 'static + LeiterAnzeige<'t, S, R>,
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
                    Element::from(L::anzeige_neu(todo!("name"), &*geschwindigkeit))
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
                        .vertical_scroll(
                            scrollable::Properties::default().scroller_width(scroller_width),
                        )
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
