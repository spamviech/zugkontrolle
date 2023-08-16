//! [draw](iced::widget::canvas::Program::draw)-Methode für [Gleise].

use std::marker::PhantomData;

use iced::{
    alignment::{Horizontal, Vertical},
    widget::canvas::{Cursor, Geometry, Program},
    Point,
};
use rstar::primitives::Rectangle;

use crate::{
    anschluss::polarität::Fließend,
    application::style::thema::Thema,
    gleis::{
        gerade::Gerade,
        gleise::{
            daten::{Gleis, RStern},
            id::{AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
            Gehalten, Gleise, ModusDaten, Nachricht,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        verbindung::Verbindung,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    nachschlagen::Nachschlagen,
    steuerung::geschwindigkeit::Leiter,
    typen::{
        canvas::{
            fill::{self, Fill, FillRule},
            pfad::{self, Transformation},
            stroke::{self, Stroke},
            Color, Frame, Position, Text,
        },
        farbe::Farbe,
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        Transparenz, Zeichnen,
    },
};

pub(crate) fn bewege_an_position(frame: &mut Frame<'_>, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

fn fülle_alle_gleise<'t, T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &'t RStern<T>,
    transparent: impl Fn(&'t Rectangle<Vektor>, Fließend) -> Transparenz,
    streckenabschnitt_farbe: &Farbe,
    streckenabschnitt_fließend: &Fließend,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            bewege_an_position(frame, position);
            // einfärben
            for (path, transparenz) in definition.fülle(spurweite) {
                let alpha = transparent(rectangle, *streckenabschnitt_fließend)
                    .kombiniere(transparenz)
                    .alpha();
                frame.with_save(|frame| {
                    let Farbe { rot, grün, blau } = *streckenabschnitt_farbe;
                    let color = Color { r: rot, g: grün, b: blau, a: alpha };
                    frame.fill(
                        &path,
                        Fill { style: fill::Style::Solid(color), rule: FillRule::EvenOdd },
                    );
                });
            }
        })
    }
}

fn zeichne_alle_gleise<'t, T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &'t RStern<T>,
    ist_gehalten: impl Fn(&'t Rectangle<Vektor>) -> bool,
    farbe: Farbe,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            bewege_an_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne(spurweite) {
                frame.with_save(|frame| {
                    let a = Transparenz::true_reduziert(ist_gehalten(rectangle)).alpha();
                    frame.stroke(
                        &path,
                        Stroke {
                            style: stroke::Style::Solid(Color { a, ..Color::from(farbe) }),
                            width: 1.5,
                            ..Stroke::default()
                        },
                    );
                });
            }
        })
    }
}

fn zeichne_alle_anchor_points<'r, 's, 't, T, F>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &'t RStern<T>,
    ist_gehalten_und_andere_verbindung: F,
) where
    't: 'r + 's,
    T: Zeichnen,
    F: Fn(&'r Rectangle<Vektor>, Verbindung) -> GehaltenVerbindung,
{
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            bewege_an_position(frame, position);
            // zeichne anchor points
            definition.verbindungen(spurweite).für_alle(|_name, &verbindung| {
                let verbindung_an_position = Verbindung {
                    position: position.transformation(verbindung.position),
                    richtung: position.winkel + verbindung.richtung,
                };
                frame.with_save(|frame| {
                    let GehaltenVerbindung { gehalten, andere_entgegengesetzt, andere_gehalten } =
                        ist_gehalten_und_andere_verbindung(rectangle, verbindung_an_position);
                    let a = Transparenz::true_reduziert(gehalten).alpha();
                    let g = if andere_entgegengesetzt { 1. } else { 0. };
                    let color = Color { r: 0., g, b: 1. - g, a };
                    let richtung = Vektor::polar_koordinaten(Skalar(5.), verbindung.richtung);
                    let richtung_seite = Skalar(0.5) * richtung.rotiert(winkel::FRAC_PI_2);
                    let verbindung_position = verbindung.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(verbindung_position + richtung_seite);
                    path_builder.line_to(verbindung_position + richtung);
                    path_builder.line_to(verbindung_position - richtung_seite);
                    let path = path_builder.baue();
                    frame.stroke(
                        &path,
                        Stroke {
                            style: stroke::Style::Solid(color),
                            width: 1.5,
                            ..Stroke::default()
                        },
                    );
                    // fill on connect/snap for drag&drop
                    if andere_gehalten {
                        frame.fill(
                            &path,
                            Fill { style: fill::Style::Solid(color), ..Fill::default() },
                        );
                    }
                });
            });
        })
    }
}

fn schreibe_alle_beschreibungen<'t, T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &'t RStern<T>,
    ist_gehalten: impl Fn(&'t Rectangle<Vektor>) -> bool,
    farbe: Farbe,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        let (relative_position, beschreibung, name) = definition.beschreibung_und_name(spurweite);
        if let Some(content) = match (beschreibung, name) {
            (Some(beschreibung), Some(name)) => Some(format!("{} ({})", name, beschreibung)),
            (None, Some(name)) => Some(String::from(name)),
            (Some(beschreibung), None) => Some(String::from(beschreibung)),
            (None, None) => None,
        } {
            let punkt =
                position.punkt + Vektor::from(relative_position.punkt).rotiert(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = Position { punkt, winkel };
            frame.with_save(|frame| {
                bewege_an_position(frame, &absolute_position);
                let a = Transparenz::true_reduziert(ist_gehalten(rectangle)).alpha();
                frame.fill_text(Text {
                    content,
                    position: Point::ORIGIN,
                    color: Color { a, ..Color::from(farbe) },
                    horizontal_alignment: Horizontal::Center,
                    vertical_alignment: Vertical::Center,
                    ..Default::default()
                });
            })
        }
    }
}

fn ist_gehalten_test<'t>(
    gehalten_id: Option<&'t AnyIdRef<'t>>,
) -> impl 't + Fn(AnyIdRef<'t>) -> bool {
    move |parameter_id| gehalten_id.map_or(false, |id| id == &parameter_id)
}

struct GehaltenVerbindung {
    gehalten: bool,
    andere_entgegengesetzt: bool,
    andere_gehalten: bool,
}

impl<L: Leiter> Gleise<L> {
    fn ist_gehalten_und_andere_verbindung<'t, T>(
        &'t self,
        streckenabschnitt: Option<StreckenabschnittIdRef<'t>>,
        gehalten_id: Option<&'t AnyIdRef<'t>>,
    ) -> impl 't + Fn(&'t Rectangle<Vektor>, Verbindung) -> GehaltenVerbindung
    where
        T: Zeichnen,
        AnyIdRef<'t>: From<GleisIdRef<'t, T>>,
    {
        let ist_gehalten = ist_gehalten_test(gehalten_id);
        move |rectangle: &Rectangle<Vektor>, verbindung: Verbindung| {
            let gehalten = ist_gehalten(AnyIdRef::from(GleisIdRef {
                rectangle,
                streckenabschnitt,
                phantom: PhantomData::<fn() -> T>,
            }));
            let any_id = AnyIdRef::from(GleisIdRef {
                rectangle,
                streckenabschnitt,
                phantom: PhantomData::<fn() -> T>,
            });
            let guard = self.zustand.read();
            let (mut überlappende, andere_gehalten) =
                guard.überlappende_verbindungen(&verbindung, Some(&any_id), gehalten_id);
            let ist_entgegengesetzt = |überlappend: &Verbindung| {
                (winkel::PI + verbindung.richtung - überlappend.richtung).normalisiert().abs()
                    < Winkel(0.1)
            };
            let andere_entgegengesetzt = überlappende.find(ist_entgegengesetzt).is_some();
            GehaltenVerbindung { gehalten, andere_entgegengesetzt, andere_gehalten }
        }
    }

    /// [draw](iced::widget::canvas::Program::draw)-Methode für [Gleise].
    pub fn draw(
        &self,
        _state: &<Self as Program<Nachricht, Thema>>::State,
        thema: &Thema,
        bounds: iced::Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let spurweite = self.spurweite();
        let Gleise { canvas, zustand, modus, .. } = self;
        // TODO zeichne keine out-of-bounds Gleise (`locate_in_envelope_intersecting`)
        // bounds müssen an Position angepasst werden:
        // - ignoriere screen-position (verwende nur height+width, i.e. size)
        // - berücksichtige eigene Position (Punkt + Winkel)
        // - berücksichtige Zoom
        // keine Priorität, in den meisten Fällen dürften alle Gleise angezeigt werden
        vec![canvas.lock().zeichnen_skaliert_von_pivot(
            bounds.size(),
            &self.pivot,
            &self.skalieren,
            |frame| {
                // Zeichne Gleise
                let gehalten_id: Option<AnyIdRef<'_>>;
                let modus_bauen: bool;
                let guard = modus.read();
                match &*guard {
                    ModusDaten::Bauen { gehalten: Some(Gehalten { gleis_steuerung, .. }), .. } => {
                        gehalten_id = Some(gleis_steuerung.id());
                        modus_bauen = true;
                    }
                    ModusDaten::Bauen { gehalten: None, .. } => {
                        gehalten_id = None;
                        modus_bauen = true;
                    }
                    ModusDaten::Fahren => {
                        gehalten_id = None;
                        modus_bauen = false;
                    }
                };

                macro_rules! mit_allen_gleisen {
                    ($daten:expr, $funktion:expr, $arg_macro:ident $(, $($extra_args:expr),*)?) => {
                        $funktion(frame, spurweite, &$daten.geraden, $arg_macro!(Gerade)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.kurven, $arg_macro!(Kurve)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.weichen, $arg_macro!(Weiche)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.kurven_weichen, $arg_macro!(KurvenWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.s_kurven_weichen, $arg_macro!(SKurvenWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.dreiwege_weichen, $arg_macro!(DreiwegeWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, spurweite, &$daten.kreuzungen, $arg_macro!(Kreuzung)$(, $($extra_args),*)?);
                    };
                }

                let guard = zustand.read();
                // TODO markiere gehalten als "wird-gelöscht", falls cursor out of bounds ist
                let ist_gehalten = ist_gehalten_test(gehalten_id.as_ref());
                // Hintergrund
                for (streckenabschnitt_opt, daten) in guard.alle_streckenabschnitte_und_daten() {
                    let (streckenabschnitt_id, streckenabschnitt)
                        = if let Some(tuple) = streckenabschnitt_opt
                    {
                        tuple
                    } else {
                        continue
                    };
                    let fließend = streckenabschnitt.fließend();
                    let farbe = streckenabschnitt.farbe;
                    macro_rules! transparenz {
                        ($gleis: ident) => {
                            |rectangle, fließend| {
                                Transparenz::true_reduziert(if modus_bauen {
                                    let any_id_ref =  AnyIdRef::from(GleisIdRef {
                                        rectangle,
                                        streckenabschnitt: Some(streckenabschnitt_id),
                                        phantom: PhantomData::<fn() -> $gleis>
                                    });
                                    ist_gehalten(any_id_ref)
                                } else {
                                    fließend == Fließend::Gesperrt
                                })
                            }
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        fülle_alle_gleise,
                        transparenz,
                        &farbe,
                        &fließend
                    }
                }
                // Kontur
                for (streckenabschnitt, daten) in guard.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten {
                        ($gleis: ident) => {
                            |rectangle| ist_gehalten(AnyIdRef::from(GleisIdRef {
                                rectangle,
                                streckenabschnitt,
                                phantom: PhantomData::<fn() -> $gleis>
                            }))
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        zeichne_alle_gleise,
                        ist_gehalten,
                        thema.strich()
                    }
                }
                // Verbindungen
                for (streckenabschnitt, daten) in guard.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten_und_andere_verbindung {
                        ($gleis: ident) => {
                            self.ist_gehalten_und_andere_verbindung::<$gleis>(
                                streckenabschnitt,
                                gehalten_id.as_ref()
                            )
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        zeichne_alle_anchor_points,
                        ist_gehalten_und_andere_verbindung
                    }
                }
                // Beschreibung
                for (streckenabschnitt, daten) in guard.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten {
                        ($gleis: ident) => {
                            |rectangle| ist_gehalten(AnyIdRef::from(GleisIdRef {
                                rectangle,
                                streckenabschnitt,
                                phantom: PhantomData::<fn() -> $gleis>
                            }))
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        schreibe_alle_beschreibungen,
                        ist_gehalten,
                        thema.strich()
                    }
                }
            },
        )]
    }
}
