//! Methoden für die [view](iced::Application::view)-Methode des [`iced::Application`]-Traits.

use std::{convert::identity, fmt::Debug};

use enum_iterator::all;
use iced::{
    mouse, touch,
    widget::{
        scrollable::{self, Scrollable},
        Button, Canvas, Column, Container, Row, Rule, Slider, Space, Text,
    },
    Alignment, Element, Event, Length, Renderer,
};
use iced_widget::PickList;
use itertools::Itertools;
use log::debug;

use zugkontrolle_anschluss::de_serialisieren::Serialisiere;
use zugkontrolle_gleis::{
    gerade::Gerade,
    id::{AnyDefinitionId, DefinitionId},
    kreuzung::Kreuzung,
    kurve::Kurve,
    steuerung::aktualisieren::MitSteuerung,
    steuerung::{geschwindigkeit::Leiter, streckenabschnitt::Name as StreckenabschnittName},
    weiche::{
        dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
    },
    zugtyp::DefinitionMap,
};
use zugkontrolle_gleise::{
    knopf::{KlickQuelle, Knopf},
    Gleise, Modus,
};
use zugkontrolle_typen::{farbe::Farbe, mm::Spurweite, skalar::Skalar, Zeichnen};

use crate::{
    auswahl::AuswahlZustand,
    bewegen::Bewegen,
    drehen::Drehen,
    flat_map::FlatMap,
    geschwindigkeit::LeiterAnzeige,
    modal::{self, Modal},
    nachricht::{Nachricht, NachrichtClone},
    speichern_laden, streckenabschnitt,
    style::{linie::TRENNLINIE, sammlung::Sammlung, thema::Thema},
    MessageBox, Zugkontrolle,
};

/// Ein Widget, dessen Nachricht sich in einen [`Nachricht`] konvertieren lässt.
trait MitTeilNachricht<'t, Msg, R>: Into<Element<'t, Msg, R>>
where
    Msg: 'static,
    R: 't + iced_core::Renderer,
{
    /// Erzeuge ein [ge`map`tes Element](Element::map).
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
    S: 'static + Clone + PartialEq + Send,
{
    // TODO Behandeln benötigt Anpassung des public API
    #[allow(clippy::same_name_method)]
    /// [view](iced::Application::view)-Methode für [`Zugkontrolle`].
    pub fn view(&self) -> Element<'_, Nachricht<L, S>, Renderer<Thema>> {
        let Zugkontrolle {
            gleise,
            scrollable_style,
            i2c_settings,
            streckenabschnitt_aktuell,
            streckenabschnitt_aktuell_festlegen,
            bewegen,
            drehen,
            lager,
            thema,
            initialer_pfad,
            speichern_gefärbt,
            bewegung: _,
            auswahl_zustand,
            message_box,
            sender: _,
            empfänger: _,
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
            *thema,
            initialer_pfad,
            speichern_gefärbt.map(|(gefärbt, _färbe_zeit)| gefärbt),
        );
        let row_mit_scrollable = row_mit_scrollable(aktueller_modus, *scrollable_style, gleise);
        let canvas = Element::new(FlatMap::neu(
            Box::new(Canvas::new(gleise).width(Length::Fill).height(Length::Fill)),
            |nachrichten| {
                nachrichten
                    .into_iter()
                    .map(modal::Nachricht::<AuswahlZustand<S>, Nachricht<L, S>>::from)
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

        let zeige_auswahlzustand = |modal: &AuswahlZustand<S>| {
            AuswahlZustand::view(modal, gleise, &lager.pcf8574, *scrollable_style, *i2c_settings)
                .map(modal::Nachricht::äußeres_modal)
        };
        let passthrough_event = |event: &Event| {
            let klick_quelle = match event {
                Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                    KlickQuelle::Maus
                },
                Event::Touch(
                    touch::Event::FingerLifted { id, position: _ }
                    | touch::Event::FingerLost { id, position: _ },
                ) => KlickQuelle::Touch(*id),
                Event::Keyboard(_) | Event::Mouse(_) | Event::Window(_) | Event::Touch(_) => {
                    return false
                },
            };
            let gehalten = gleise.hat_gehaltenes_gleis(klick_quelle);
            debug!("{event:?} -> {gehalten}");
            gehalten
        };
        let auswahlzustand = Element::from(
            Modal::neu(column, auswahl_zustand, zeige_auswahlzustand)
                .passthrough_event(passthrough_event)
                .schließe_bei_esc(),
        );

        let zeige_message_box = |MessageBox { titel, nachricht }: &MessageBox| {
            Element::new(
                iced_aw::Card::new(
                    Text::new(titel.clone()),
                    Scrollable::new(Text::new(nachricht.clone())).height(Length::Fixed(300.)),
                )
                .foot(Button::new(Text::new("Ok")).on_press(modal::Nachricht::VersteckeOverlay))
                .on_close(modal::Nachricht::VersteckeOverlay)
                .width(Length::Shrink),
            )
            .map(modal::Nachricht::underlay_from::<NachrichtClone<L>>)
        };
        let message_box_modal = Modal::neu(auswahlzustand, message_box, zeige_message_box)
            .passthrough_event(passthrough_event)
            .schließe_bei_esc();
        message_box_modal.into()
    }
}

/// Die Höhe des [`Bewegen`]-Widgets in Pixeln.
const BEWEGEN_HÖHE: f32 = 50.;
/// Die Breite des [`Bewegen`]-Widgets in Pixeln.
const BEWEGEN_BREITE: f32 = 50.;
/// Die Höhe des [`Drehen`]-Widgets in Pixeln.
const DREHEN_HÖHE: f32 = 50.;
/// Die Breite des [`Drehen`]-Widgets in Pixeln.
const DREHEN_BREITE: f32 = 50.;
/// Die Breite des [`Sliders`](Slider) zum Einstellen der Skalierung in Pixeln.
const SKALIEREN_BREITE: f32 = 75.;

/// [`Nachricht`] mit der Möglichkeit, das [`Auswahl`](AuswahlZustand)-[`Modal`] zu öffnen.
type AuswahlNachricht<L, S> = modal::Nachricht<AuswahlZustand<S>, Nachricht<L, S>>;

// Interne Methode, alle Argumente benötigt.
#[allow(clippy::too_many_arguments)]
/// Erzeuge die Widgets für die Kopfleiste.
fn top_row<'t, L, S>(
    aktueller_modus: Modus,
    streckenabschnitt_aktuell: &'t Option<(StreckenabschnittName, Farbe)>,
    streckenabschnitt_festlegen: &'t bool,
    bewegen: &'t Bewegen,
    drehen: &'t Drehen,
    aktueller_zoom: Skalar,
    aktuelles_thema: Thema,
    initialer_pfad: &'t str,
    speichern_gefärbt: Option<bool>,
) -> Row<'t, AuswahlNachricht<L, S>, Renderer<Thema>>
where
    L: 'static + Debug + LeiterAnzeige<'t, S, Renderer<Thema>>,
    <L as Leiter>::Fahrtrichtung: Clone,
    S: 'static + Clone + PartialEq,
{
    let modus_radios = Column::new().push(PickList::new(
        all::<Modus>().collect_vec(),
        Some(aktueller_modus),
        identity,
    ));
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
                .on_press(modal::Nachricht::ZeigeOverlay(AuswahlZustand::Geschwindigkeit(None))),
        )
        .map(modal::Nachricht::underlay_from::<NachrichtClone<L>>);
        let streckenabschnitt = Element::from(streckenabschnitt::Anzeige::neu(
            streckenabschnitt_aktuell,
            *streckenabschnitt_festlegen,
            AuswahlZustand::Streckenabschnitt(None),
        ))
        .map(modal::Nachricht::underlay_from);
        row = row.push(Column::new().push(geschwindigkeit).push(streckenabschnitt).spacing(1));
    }

    row.push(Space::new(Length::Fill, Length::Shrink))
        .push(
            Element::from(PickList::new(
                all::<Thema>().collect_vec(),
                Some(aktuelles_thema),
                Nachricht::Thema,
            ))
            .map(modal::Nachricht::Underlay),
        )
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

/// [`Nachricht`] mit der Möglichkeit, das [`Auswahl`](AuswahlZustand)-[`Modal`] und die [`MessageBox`] zu öffnen.
type MessageBoxNachricht<L, S> =
    modal::Nachricht<AuswahlZustand<S>, modal::Nachricht<MessageBox, Nachricht<L, S>>>;

/// Erzeuge die Seitenleiste.
fn row_mit_scrollable<'t, L: 'static + LeiterAnzeige<'t, S, Renderer<Thema>>, S: 'static>(
    aktueller_modus: Modus,
    scrollable_style: Sammlung,
    gleise: &'t Gleise<L, Nachricht<L, S>>,
) -> Row<'t, MessageBoxNachricht<L, S>, Renderer<Thema>> {
    let mut scrollable_column: Column<'_, NachrichtClone<_>, Renderer<Thema>> = Column::new();
    let scroller_width = scrollable_style.breite();
    let mut width = Length::Shrink;

    match aktueller_modus {
        Modus::Bauen => {
            /// Füge für jedes Element der [`DefinitionMap`] einen [`Knopf`] zum `scrollable_column` hinzu.
            fn knöpfe_hinzufügen<'t, L, S, R, T>(
                spurweite: Spurweite,
                max_breite: Option<f32>,
                scrollable_column: &mut Column<'t, NachrichtClone<L>, Renderer<Thema>>,
                buttons: &'t DefinitionMap<T>,
            ) where
                L: 'static + LeiterAnzeige<'t, S, R>,
                T: MitSteuerung,
                DefinitionId<T>: Into<AnyDefinitionId>,
                <T as MitSteuerung>::SelfUnit: Zeichnen<()> + Clone,
            {
                // scrollable_column related über take_mut::take
                #[allow(clippy::shadow_unrelated)]
                take_mut::take(scrollable_column, |mut scrollable_column| {
                    for (id, button) in buttons.iter().sorted_by_key(|(_id, gleis)| {
                        let (_position, beschreibung, _name) =
                            gleis.beschreibung_und_name(&(), spurweite);
                        beschreibung
                    }) {
                        let knopf = Knopf::neu(button, id.clone(), spurweite);
                        scrollable_column =
                            scrollable_column.push(knopf.als_iced_widget(max_breite));
                    }
                    scrollable_column
                });
            }
            let mut max_breite = None;
            /// Wrapper um [`knöpfe_hinzufügen`] für mehrere [`DefinitionMap`] mit unterschiedlichen Elementen.
            macro_rules! knöpfe_hinzufügen {
                ($($map: expr => $type: ty),* $(,)?) => {
                    max_breite_berechnen!($($map),*);
                    $(knöpfe_hinzufügen::<L, S, _, $type>(gleise.spurweite(), max_breite, &mut scrollable_column, $map);)*
                }
            }
            /// Berechne die maximale Breite aller Knöpfe.
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
            knöpfe_hinzufügen!(
                &gleise.zugtyp2().geraden => Gerade,
                &gleise.zugtyp2().kurven => Kurve,
                &gleise.zugtyp2().weichen => Weiche,
                &gleise.zugtyp2().dreiwege_weichen => DreiwegeWeiche,
                &gleise.zugtyp2().kurven_weichen => KurvenWeiche,
                &gleise.zugtyp2().s_kurven_weichen => SKurvenWeiche,
                &gleise.zugtyp2().kreuzungen => Kreuzung,
            );
            if let Some(max) = max_breite {
                width = Length::Fixed(max);
            }
        },
        Modus::Fahren => {
            scrollable_column = scrollable_column.push(Text::new("Geschwindigkeiten")).spacing(1);
            gleise.mit_allen_geschwindigkeiten(|name, geschwindigkeit| {
                // scrollable_column related über take_mut::take
                #[allow(clippy::shadow_unrelated)]
                take_mut::take(&mut scrollable_column, |scrollable_column| {
                    scrollable_column.push(
                        Element::from(L::anzeige_neu(name, geschwindigkeit))
                            .map(NachrichtClone::AktionGeschwindigkeit),
                    )
                });
            });
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
