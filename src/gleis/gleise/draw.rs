//! [draw](iced::Application::draw)-Methode für [Gleise].

use std::marker::PhantomData;

use iced::{
    canvas::{Cursor, Geometry},
    Point,
};
use rstar::primitives::Rectangle;

use crate::{
    anschluss::polarität::Fließend,
    gleis::{
        gerade::Gerade,
        gleise::{
            daten::{Gleis, RStern, Zustand},
            id::{AnyId, AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
            AnyGleis, Gehalten, Gleise, ModusDaten,
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
            pfad::{self, Transformation},
            Color, Fill, FillRule, Frame, HorizontalAlignment, Position, Stroke, Text,
            VerticalAlignment,
        },
        farbe::Farbe,
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        Transparenz, Zeichnen,
    },
};

use super::daten::mit_any_gleis;

pub(crate) fn bewege_an_position(frame: &mut Frame<'_>, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

fn fülle_gleis<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    gleis: &Gleis<T>,
    transparenz: &Transparenz,
    farbe: &Farbe,
) {
    let Gleis { definition, position } = gleis;
    frame.with_save(|frame| {
        bewege_an_position(frame, position);
        // einfärben
        for (path, pfad_transparenz) in definition.fülle(spurweite) {
            let alpha = transparenz.kombiniere(pfad_transparenz).alpha();
            frame.with_save(|frame| {
                let Farbe { rot, grün, blau } = *farbe;
                let color = Color { r: rot, g: grün, b: blau, a: alpha };
                frame.fill(&path, Fill { color, rule: FillRule::EvenOdd });
            });
        }
    })
}

fn fülle_alle_gleise<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &RStern<T>,
    transparenz: &Transparenz,
    farbe: &Farbe,
) {
    for geom_with_data in rstern.iter() {
        let gleis = &geom_with_data.data;
        fülle_gleis(frame, spurweite, gleis, transparenz, farbe)
    }
}

fn zeichne_gleis<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    gleis: &Gleis<T>,
    transparenz: &Transparenz,
) {
    let Gleis { definition, position } = gleis;
    frame.with_save(|frame| {
        bewege_an_position(frame, position);
        // zeichne Kontur
        for path in definition.zeichne(spurweite) {
            frame.with_save(|frame| {
                let a = transparenz.alpha();
                frame.stroke(
                    &path,
                    Stroke { color: Color { a, ..Color::BLACK }, width: 1.5, ..Default::default() },
                );
            });
        }
    })
}

fn zeichne_alle_gleise<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &RStern<T>,
    transparenz: &Transparenz,
) {
    for geom_with_data in rstern.iter() {
        let gleis = &geom_with_data.data;
        zeichne_gleis(frame, spurweite, gleis, transparenz)
    }
}

fn zeichne_anchor_points<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    gleis: &Gleis<T>,
    ist_gehalten_und_andere_verbindung: impl Fn(Verbindung) -> GehaltenVerbindung,
) {
    let Gleis { definition, position } = gleis;
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
                    ist_gehalten_und_andere_verbindung(verbindung_an_position);
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
                frame.stroke(&path, Stroke { color, width: 1.5, ..Default::default() });
                // fill on connect/snap for drag&drop
                if andere_gehalten {
                    frame.fill(&path, Fill { color, ..Default::default() });
                }
            });
        });
    })
}

fn zeichne_alle_anchor_points<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &RStern<T>,
    ist_gehalten_und_andere_verbindung: impl Fn(Verbindung) -> GehaltenVerbindung,
) {
    for geom_with_data in rstern.iter() {
        let gleis = &geom_with_data.data;
        zeichne_anchor_points(frame, spurweite, gleis, &ist_gehalten_und_andere_verbindung)
    }
}

fn schreibe_beschreibung<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    gleis: &Gleis<T>,
    transparenz: &Transparenz,
) {
    let Gleis { definition, position } = gleis;
    let (relative_position, beschreibung, name) = definition.beschreibung_und_name(spurweite);
    if let Some(content) = match (beschreibung, name) {
        (Some(beschreibung), Some(name)) => Some(format!("{} ({})", name, beschreibung)),
        (None, Some(name)) => Some(name.into_owned()),
        (Some(beschreibung), None) => Some(beschreibung.clone()),
        (None, None) => None,
    } {
        let punkt = position.punkt + Vektor::from(relative_position.punkt).rotiert(position.winkel);
        let winkel = position.winkel + relative_position.winkel;
        let absolute_position = Position { punkt, winkel };
        frame.with_save(|frame| {
            bewege_an_position(frame, &absolute_position);
            let a = transparenz.alpha();
            frame.fill_text(Text {
                content,
                position: Point::ORIGIN,
                color: Color { a, ..Color::BLACK },
                horizontal_alignment: HorizontalAlignment::Center,
                vertical_alignment: VerticalAlignment::Center,
                ..Default::default()
            });
        })
    }
}

fn schreibe_alle_beschreibungen<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    rstern: &RStern<T>,
    transparenz: &Transparenz,
) {
    for geom_with_data in rstern.iter() {
        let gleis = &geom_with_data.data;
        schreibe_beschreibung(frame, spurweite, gleis, transparenz)
    }
}

struct GehaltenVerbindung {
    gehalten: bool,
    andere_entgegengesetzt: bool,
    andere_gehalten: bool,
}

impl<L: Leiter> Gleise<L> {
    /// [draw](iced::Application::draw)-Methode für [Gleise].
    pub fn draw(&self, bounds: iced::Rectangle, _cursor: Cursor) -> Vec<Geometry> {
        let spurweite = self.spurweite();
        let Gleise { canvas, zustand, modus, pivot, skalieren, .. } = self;
        // TODO zeichne keine out-of-bounds Gleise (`locate_in_envelope_intersecting`)
        // bounds müssen an Position angepasst werden:
        // - ignoriere screen-position (verwende nur height+width, i.e. size)
        // - berücksichtige eigene Position (Punkt + Winkel)
        // - berücksichtige Zoom
        // keine Priorität, in den meisten Fällen dürften alle Gleise angezeigt werden
        let draw_frame = |frame: &mut Frame<'_>| {
            // Zeichne Gleise
            let gehalten_gleis: Option<&AnyGleis>;
            let modus_bauen: bool;
            match modus {
                ModusDaten::Bauen { gehalten, .. } => {
                    gehalten_gleis = gehalten.as_ref().map(|Gehalten { gleis, .. }| gleis);
                    modus_bauen = true;
                },
                ModusDaten::Fahren => {
                    gehalten_gleis = None;
                    modus_bauen = false;
                },
            };
            // TODO markiere gehalten als "wird-gelöscht", falls cursor out of bounds ist
            // FIXME gehaltenes Gleis zeichnen

            macro_rules! mit_allen_gleisen {
                ($daten:expr, $funktion:expr, $arg_macro:ident $(, $($extra_args:expr),* $(,)?)?) => {
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.geraden,
                        $arg_macro!(Gerade)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.kurven,
                        $arg_macro!(Kurve)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.weichen,
                        $arg_macro!(Weiche)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.kurven_weichen,
                        $arg_macro!(KurvenWeiche)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.s_kurven_weichen,
                        $arg_macro!(SKurvenWeiche)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.dreiwege_weichen,
                        $arg_macro!(DreiwegeWeiche)
                        $(, $($extra_args),*)?
                    );
                    $funktion(
                        frame,
                        spurweite,
                        &$daten.kreuzungen,
                        $arg_macro!(Kreuzung)
                        $(, $($extra_args),*)?
                    );
                };
            }
            // Hintergrund
            for (streckenabschnitt_opt, daten) in zustand.alle_streckenabschnitte_und_daten() {
                let streckenabschnitt =
                    if let Some((_id, streckenabschnitt)) = streckenabschnitt_opt {
                        streckenabschnitt
                    } else {
                        continue;
                    };
                let fließend = streckenabschnitt.fließend();
                let farbe = streckenabschnitt.farbe;
                let transparenz =
                    Transparenz::true_reduziert(modus_bauen || fließend == Fließend::Gesperrt);
                macro_rules! transparenz {
                    ($typ: ident) => {
                        &transparenz
                    };
                }
                mit_allen_gleisen! {
                    daten,
                    fülle_alle_gleise,
                    transparenz,
                    &farbe,
                }
            }
            // Kontur
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                macro_rules! transparenz {
                    ($gleis: ident) => {
                        &Transparenz::Voll
                    };
                }
                mit_allen_gleisen! {
                    daten,
                    zeichne_alle_gleise,
                    transparenz,
                }
            }
            // Verbindungen
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                macro_rules! ist_gehalten_und_andere_verbindung {
                    ($gleis: ident) => {
                        ist_gehalten_und_andere_verbindung::<L, $gleis>(
                            &self.zustand,
                            gehalten_gleis,
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
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                macro_rules! ist_gehalten {
                    ($gleis: ident) => {
                        &Transparenz::Voll
                    };
                }
                mit_allen_gleisen! {
                    daten,
                    schreibe_alle_beschreibungen,
                    ist_gehalten
                }
            }
        };
        vec![canvas.lock().zeichnen_skaliert_von_pivot(bounds.size(), pivot, skalieren, draw_frame)]
    }
}

fn ist_gehalten_und_andere_verbindung<'t, L: Leiter, T: Zeichnen>(
    zustand: &'t Zustand<L>,
    gehalten: Option<&'t AnyGleis>,
) -> impl 't + Fn(Verbindung) -> GehaltenVerbindung {
    let spurweite = zustand.zugtyp.spurweite;
    move |verbindung: Verbindung| {
        let überlappend_gehalten = gehalten.map_or(Vec::new(), |gehalten| {
            mit_any_gleis!(gehalten, Gleis::überlappende_verbindungen, spurweite, &verbindung)
        });
        let ist_gehalten = überlappend_gehalten.contains(&verbindung);
        let andere_gehalten = !überlappend_gehalten.is_empty();
        let mut überlappende = zustand.überlappende_verbindungen(&verbindung);
        let ist_entgegengesetzt = |überlappend: &Verbindung| {
            (winkel::PI + verbindung.richtung - überlappend.richtung).normalisiert().abs()
                < Winkel(0.1)
        };
        let andere_entgegengesetzt = überlappende.find(ist_entgegengesetzt).is_some();
        GehaltenVerbindung { gehalten: ist_gehalten, andere_entgegengesetzt, andere_gehalten }
    }
}
