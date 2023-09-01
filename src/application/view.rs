//! Methoden für die [view](iced::Application::view)-Methode des [iced::Application]-Traits.

use std::fmt::Debug;

use iced::{
    widget::{
        scrollable::{self, Scrollable},
        Button, Canvas, Column, Container, Row, Rule, Slider, Space, Text,
    },
    Alignment, Element, Length, Renderer,
};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::{
        auswahl::AuswahlZustand,
        bewegen::Bewegen,
        drehen::Drehen,
        flat_map::FlatMap,
        geschwindigkeit::LeiterAnzeige,
        modal::{self, Modal},
        nachricht::{AnyGleisUnit, Nachricht, NachrichtClone},
        speichern_laden, streckenabschnitt,
        style::{linie::TRENNLINIE, sammlung::Sammlung, thema::Thema},
        MessageBox, Zugkontrolle,
    },
    gleis::{
        gerade::{Gerade, GeradeUnit},
        gleise::{
            id::{AnyDefinitionId2, AnyDefinitionIdSteuerung2, DefinitionId2, StreckenabschnittId},
            steuerung::MitSteuerung,
            Gleise, Modus,
        },
        knopf::Knopf,
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::geschwindigkeit::Leiter,
    typen::{farbe::Farbe, mm::Spurweite, skalar::Skalar, Zeichnen},
    zugtyp::DefinitionMap2,
};

trait MitTeilNachricht<'t, Msg, R>: Into<Element<'t, Msg, R>>
where
    Msg: 'static,
    R: 't + iced_core::Renderer,
{
    fn mit_teil_nachricht<L: 'static + LeiterAnzeige<'t, S, R>, S: 'static>(
        self,
        konstruktor: impl Fn(Msg) -> Nachricht<L, S> + 'static,
    ) -> Element<'t, Nachricht<L, S>, R> {
        self.into().map(konstruktor)
    }
}

impl<'t, T, R, Msg> MitTeilNachricht<'t, Msg, R> for T
where
    Msg: 'static,
    T: Into<Element<'t, Msg, R>>,
    R: 't + iced_core::Renderer,
{
}

impl<L, S> Zugkontrolle<L, S>
where
    L: 'static + Debug + Serialisiere<S> + for<'t> LeiterAnzeige<'t, S, Renderer<Thema>> + Send,
    <L as Leiter>::Fahrtrichtung: Clone + Send,
    S: 'static + Clone + Send,
{
    /// [view](iced::Application::view)-Methode für [Zugkontrolle].
    pub fn view(&self) -> Element<'_, Nachricht<L, S>, Renderer<Thema>> {
        let Zugkontrolle {
            gleise,
            scrollable_style,
            i2c_settings,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            bewegen,
            drehen,
            lager,
            speichern_gefärbt,
            bewegung: _,
            message_box,
            sender: _,
            empfänger: _,
            initialer_pfad,
        } = self;
        let aktueller_modus = gleise.modus();
        let aktueller_zoom = gleise.skalierfaktor();

        let top_row = top_row(
            aktueller_modus,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            bewegen,
            drehen,
            aktueller_zoom,
            initialer_pfad,
            speichern_gefärbt.map(|(gefärbt, _färbe_zeit)| gefärbt),
        );
        let row_mit_scrollable = row_mit_scrollable(aktueller_modus, *scrollable_style, gleise);
        let canvas = Element::new(FlatMap::neu(
            Box::new(Canvas::new(gleise).width(Length::Fill).height(Length::Fill)),
            |nachrichten| {
                nachrichten
                    .into_iter()
                    .map(modal::Nachricht::<AuswahlZustand, Nachricht<L, S>>::from)
            },
        ))
        .map(|modal_nachricht| modal_nachricht.underlay_map(modal::Nachricht::Underlay));
        let row_mit_scrollable_und_canvas = row_mit_scrollable
            .push(Container::new(canvas).width(Length::Fill).height(Length::Fill));

        let column = Element::from(
            Column::new()
                .push(Element::from(top_row).map(modal::Nachricht::underlay_from))
                .push(Rule::horizontal(1).style(TRENNLINIE))
                .push(Element::from(row_mit_scrollable_und_canvas)),
        );

        let zeige_auswahlzustand = |modal: &AuswahlZustand| {
            AuswahlZustand::view(modal, gleise, &lager.pcf8574, *scrollable_style, *i2c_settings)
                .map(modal::Nachricht::äußeres_modal)
        };
        let auswahlzustand =
            Element::from(Modal::neu(column, zeige_auswahlzustand).schließe_bei_esc());

        let zeige_message_box = |message_box: &MessageBox| {
            let MessageBox { titel, nachricht } = message_box;
            Element::new(
                iced_aw::Card::new(
                    Text::new(titel.clone()),
                    Scrollable::new(Text::new(nachricht.clone())).height(Length::Fixed(300.)),
                )
                .foot(
                    iced::widget::Button::new(Text::new("Ok"))
                        .on_press(modal::Nachricht::VersteckeOverlay),
                )
                .on_close(modal::Nachricht::VersteckeOverlay)
                .width(Length::Shrink),
            )
            .map(modal::Nachricht::underlay_from::<NachrichtClone<L>>)
        };
        let mut message_box_modal =
            Modal::neu(auswahlzustand, zeige_message_box).schließe_bei_esc();
        if let Some(message_box) = message_box {
            message_box_modal = message_box_modal.initiales_overlay(message_box.clone());
        }
        message_box_modal.into()
    }
}

const BEWEGEN_HÖHE: f32 = 50.;
const BEWEGEN_BREITE: f32 = 50.;
const DREHEN_HÖHE: f32 = 50.;
const DREHEN_BREITE: f32 = 50.;
const SKALIEREN_BREITE: f32 = 75.;

fn top_row<'t, L, S>(
    aktueller_modus: Modus,
    streckenabschnitt_aktuell: &'t Option<(StreckenabschnittId, Farbe)>,
    streckenabschnitt_festlegen: &'t bool,
    bewegen: &'t Bewegen,
    drehen: &'t Drehen,
    aktueller_zoom: Skalar,
    initialer_pfad: &'t str,
    speichern_gefärbt: Option<bool>,
) -> Row<'t, modal::Nachricht<AuswahlZustand, Nachricht<L, S>>, Renderer<Thema>>
where
    L: 'static + Debug + LeiterAnzeige<'t, S, Renderer<Thema>>,
    <L as Leiter>::Fahrtrichtung: Clone,
    S: 'static + Clone,
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
    let speichern_laden = speichern_laden::SpeichernLaden::neu(initialer_pfad, speichern_gefärbt);
    let mut row = Row::new()
        .push(modus_radios.mit_teil_nachricht(Nachricht::Modus).map(modal::Nachricht::Underlay))
        .push(bewegen.mit_teil_nachricht(Nachricht::Bewegen).map(modal::Nachricht::Underlay))
        .push(drehen.mit_teil_nachricht(Nachricht::Winkel).map(modal::Nachricht::Underlay))
        .push(Element::from(skalieren_slider).map(Nachricht::from).map(modal::Nachricht::Underlay));

    // Streckenabschnitte und Geschwindigkeiten können nur im Bauen-Modus geändert werden
    if let Modus::Bauen { .. } = aktueller_modus {
        let geschwindigkeit = Element::new(
            Button::new(Text::new("Geschwindigkeiten"))
                .on_press(modal::Nachricht::ZeigeOverlay(AuswahlZustand::Geschwindigkeit)),
        )
        .map(modal::Nachricht::underlay_from::<NachrichtClone<L>>);
        let streckenabschnitt = Element::from(streckenabschnitt::Anzeige::neu(
            streckenabschnitt_aktuell,
            *streckenabschnitt_festlegen,
            AuswahlZustand::Streckenabschnitt,
        ))
        .map(modal::Nachricht::underlay_from);
        row = row.push(Column::new().push(geschwindigkeit).push(streckenabschnitt).spacing(1));
    }

    row.push(Space::new(Length::Fill, Length::Shrink))
        .push(
            Element::from(speichern_laden)
                .map(|message| match message {
                    speichern_laden::Nachricht::Speichern(pfad) => Nachricht::Speichern(pfad),
                    speichern_laden::Nachricht::Laden(pfad) => Nachricht::Laden(pfad),
                })
                .map(modal::Nachricht::Underlay),
        )
        .push(
            Element::from(
                Button::new(Text::new("Lizenzen"))
                    .on_press(modal::Nachricht::ZeigeOverlay(AuswahlZustand::ZeigeLizenzen)),
            )
            .map(modal::Nachricht::underlay_from::<NachrichtClone<L>>),
        )
        .padding(5)
        .spacing(5)
        .width(Length::Fill)
        .height(Length::Shrink)
}

fn row_mit_scrollable<'t, L: 'static + LeiterAnzeige<'t, S, Renderer<Thema>>, S: 'static>(
    aktueller_modus: Modus,
    scrollable_style: Sammlung,
    gleise: &'t Gleise<L, Nachricht<L, S>>,
) -> Row<
    't,
    modal::Nachricht<AuswahlZustand, modal::Nachricht<MessageBox, Nachricht<L, S>>>,
    Renderer<Thema>,
> {
    let mut scrollable_column: Column<'_, NachrichtClone<_>, Renderer<Thema>> = Column::new();
    let scroller_width = scrollable_style.breite();
    let mut width = Length::Shrink;

    match aktueller_modus {
        Modus::Bauen => {
            let mut max_breite = None;
            macro_rules! max_breite_berechnen {
                ($($map: expr),* $(,)?) => {
                    $(
                        for button in $map.values() {
                            let größe = button.rechteck(&(), gleise.spurweite()).größe();
                            let breite = Some(größe.x.0);
                            if breite > max_breite {
                                max_breite = breite;
                            }
                        }
                    )*
                }
            }
            fn knöpfe_hinzufügen<'t, L, S, R, T>(
                spurweite: Spurweite,
                max_breite: Option<f32>,
                scrollable_column: &mut Column<'t, NachrichtClone<L>, Renderer<Thema>>,
                buttons: &'t DefinitionMap2<T>,
            ) where
                L: 'static + LeiterAnzeige<'t, S, R>,
                T: MitSteuerung,
                DefinitionId2<T>: Into<AnyDefinitionId2>,
                <T as MitSteuerung>::SelfUnit: Zeichnen<()> + Clone,
            {
                take_mut::take(scrollable_column, |mut scrollable_column| {
                    for (id, button) in buttons.iter() {
                        let knopf = Knopf::neu(button.clone(), id.clone(), spurweite);
                        scrollable_column =
                            scrollable_column.push(knopf.als_iced_widget(max_breite))
                    }
                    scrollable_column
                })
            }
            macro_rules! knöpfe_hinzufügen {
                ($($map: expr),* $(,)?) => {
                    max_breite_berechnen!($($map),*);
                    // $(knöpfe_hinzufügen(gleise.spurweite(), max_breite, &mut scrollable_column, $map);)*
                }
            }
            knöpfe_hinzufügen::<L, S, _, Gerade>(
                gleise.spurweite(),
                max_breite,
                &mut scrollable_column,
                &gleise.zugtyp2().geraden,
            );
            knöpfe_hinzufügen!(
                &gleise.zugtyp2().geraden,
                &gleise.zugtyp2().kurven,
                &gleise.zugtyp2().weichen,
                &gleise.zugtyp2().dreiwege_weichen,
                &gleise.zugtyp2().kurven_weichen,
                &gleise.zugtyp2().s_kurven_weichen,
                &gleise.zugtyp2().kreuzungen,
            );
            if let Some(max) = max_breite {
                width = Length::Fixed(max);
            }
        },
        Modus::Fahren => {
            scrollable_column = scrollable_column.push(Text::new("Geschwindigkeiten")).spacing(1);
            gleise.mit_allen_geschwindigkeiten(|name, geschwindigkeit| {
                take_mut::take(&mut scrollable_column, |scrollable_column| {
                    scrollable_column.push(
                        Element::from(L::anzeige_neu(name, &*geschwindigkeit))
                            .map(NachrichtClone::AktionGeschwindigkeit),
                    )
                })
            })
            // TODO Wegstrecken?, Pläne?, Separator dazwischen?
        },
    }
    let scrollable = Scrollable::new(scrollable_column);
    Row::new()
        .push(
            Container::new(
                Element::new(
                    scrollable
                        .direction(scrollable::Direction::Vertical(
                            scrollable::Properties::default().scroller_width(scroller_width),
                        ))
                        .height(Length::Fill)
                        .style(scrollable_style),
                )
                .map(Nachricht::from)
                .map(modal::Nachricht::Underlay)
                .map(modal::Nachricht::Underlay),
            )
            .width(width)
            .height(Length::Fill),
        )
        .push(Rule::vertical(1).style(TRENNLINIE))
}
