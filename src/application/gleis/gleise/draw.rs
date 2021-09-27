//! draw-Methode für Gleise

use std::marker::PhantomData;

use rstar::primitives::Rectangle;

use crate::{
    anschluss::Fließend,
    application::{
        gleis::{
            gerade::Gerade,
            gleise::{
                daten::{Gleis, RStern},
                id::{AnyId, AnyIdRef, GleisIdRef},
                Gehalten, Gleise, ModusDaten,
            },
            kreuzung::Kreuzung,
            kurve::Kurve,
            weiche::{
                dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche,
                s_kurve::SKurvenWeiche,
            },
        },
        typen::*,
        verbindung::Verbindung,
    },
    farbe::Farbe,
    lookup::Lookup,
    steuerung::streckenabschnitt::Streckenabschnitt,
    zugtyp::Zugtyp,
};

pub(crate) fn move_to_position(frame: &mut canvas::Frame, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

fn fülle_alle_gleise<'t, T: Zeichnen>(
    frame: &mut canvas::Frame,
    rstern: &'t RStern<T>,
    transparent: impl Fn(&'t Rectangle<Vektor>, Fließend) -> Transparenz,
    streckenabschnitt_farbe: &Farbe,
    streckenabschnitt_fließend: &Fließend,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // einfärben
            for (path, transparenz) in definition.fülle() {
                let a = transparent(rectangle, *streckenabschnitt_fließend)
                    .kombiniere(transparenz)
                    .alpha();
                frame.with_save(|frame| {
                    let Farbe { r, g, b } = *streckenabschnitt_farbe;
                    let color = iced::Color { r, g, b, a };
                    frame.fill(&path, canvas::Fill { color, rule: canvas::FillRule::EvenOdd });
                });
            }
        })
    }
}

fn zeichne_alle_gleise<'t, T: Zeichnen>(
    frame: &mut canvas::Frame,
    rstern: &'t RStern<T>,
    ist_gehalten: impl Fn(&'t Rectangle<Vektor>) -> bool,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    let a = Transparenz::true_reduziert(ist_gehalten(rectangle)).alpha();
                    frame.stroke(
                        &path,
                        canvas::Stroke {
                            color: canvas::Color { a, ..canvas::Color::BLACK },
                            width: 1.5,
                            ..Default::default()
                        },
                    );
                });
            }
        })
    }
}

fn zeichne_alle_anchor_points<'r, 's, 't: 'r + 's, T: Zeichnen>(
    frame: &mut canvas::Frame,
    rstern: &'t RStern<T>,
    ist_gehalten_und_andere_entgegengesetzt_oder_gehaltene_verbindung: impl Fn(
        &'r Rectangle<Vektor>,
        Verbindung,
    )
        -> (bool, bool, bool),
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.verbindungen().for_each(|_name, &verbindung| {
                let verbindung_an_position = Verbindung {
                    position: position.transformation(verbindung.position),
                    richtung: position.winkel + verbindung.richtung,
                };
                frame.with_save(|frame| {
                    let (gehalten, entgegengesetzt, andere_gehalten) =
                        ist_gehalten_und_andere_entgegengesetzt_oder_gehaltene_verbindung(
                            rectangle,
                            verbindung_an_position,
                        );
                    let a = Transparenz::true_reduziert(gehalten).alpha();
                    let g = if entgegengesetzt { 1. } else { 0. };
                    let color = canvas::Color { r: 0., g, b: 1. - g, a };
                    let richtung = Vektor::polar_koordinaten(Skalar(5.), verbindung.richtung);
                    let richtung_seite = Skalar(0.5) * richtung.rotiert(winkel::FRAC_PI_2);
                    let verbindung_position = verbindung.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(verbindung_position + richtung_seite);
                    path_builder.line_to(verbindung_position + richtung);
                    path_builder.line_to(verbindung_position - richtung_seite);
                    let path = path_builder.baue();
                    frame.stroke(&path, canvas::Stroke { color, width: 1.5, ..Default::default() });
                    // fill on connect/snap for drag&drop
                    if andere_gehalten {
                        frame.fill(&path, canvas::Fill { color, ..Default::default() });
                    }
                });
            });
        })
    }
}

fn schreibe_alle_beschreibungen<'t, T: Zeichnen>(
    frame: &mut canvas::Frame,
    rstern: &'t RStern<T>,
    ist_gehalten: impl Fn(&'t Rectangle<Vektor>) -> bool,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        let (relative_position, beschreibung, name) = definition.beschreibung_und_name();
        if let Some(content) = match (beschreibung, name) {
            (Some(beschreibung), Some(name)) => Some(format!("{} ({})", name, beschreibung)),
            (None, Some(name)) => Some(name.clone()),
            (Some(beschreibung), None) => Some(beschreibung.clone()),
            (None, None) => None,
        } {
            let punkt =
                position.punkt + Vektor::from(relative_position.punkt).rotiert(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = Position { punkt, winkel };
            frame.with_save(|frame| {
                move_to_position(frame, &absolute_position);
                let a = Transparenz::true_reduziert(ist_gehalten(rectangle)).alpha();
                frame.fill_text(canvas::Text {
                    content,
                    position: iced::Point::ORIGIN,
                    color: canvas::Color { a, ..canvas::Color::BLACK },
                    horizontal_alignment: canvas::HorizontalAlignment::Center,
                    vertical_alignment: canvas::VerticalAlignment::Center,
                    ..Default::default()
                });
            })
        }
    }
}

impl<Z: Zugtyp> Gleise<Z> {
    pub fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let Gleise { canvas, zustand, modus, .. } = self;
        // TODO zeichne keine out-of-bounds Gleise (`locate_in_envelope_intersecting`)
        // bounds müssen an Position angepasst werden:
        // - ignoriere screen-position (verwende nur height+width, i.e. size)
        // - berücksichtige eigene Position (Punkt + Winkel)
        // - berücksichtige Zoom
        // keine Priorität, in den meisten Fällen dürften alle Gleise angezeigt werden
        vec![canvas.zeichnen_skaliert_von_pivot(
            bounds.size(),
            &self.pivot,
            &self.skalieren,
            |frame| {
                // Zeichne Gleise
                let gehalten_id: Option<&AnyId<Z>>;
                let modus_bauen: bool;
                match modus {
                    ModusDaten::Bauen { gehalten: Some(Gehalten { gleis_id, .. }), .. } => {
                        gehalten_id = Some(gleis_id);
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
                // TODO markiere gehalten als "wird-gelöscht", falls cursor out of bounds ist
                let ist_gehalten
                    = |parameter_id| gehalten_id.map_or(false, |id| id == &parameter_id);

                macro_rules! mit_allen_gleisen {
                    ($daten:expr, $funktion:expr, $arg_macro:ident $(, $($extra_args:expr),*)?) => {
                        $funktion(frame, &$daten.geraden, $arg_macro!(Gerade)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.kurven, $arg_macro!(Kurve)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.weichen, $arg_macro!(Weiche)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.kurven_weichen, $arg_macro!(KurvenWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.s_kurven_weichen, $arg_macro!(SKurvenWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.dreiwege_weichen, $arg_macro!(DreiwegeWeiche)$(, $($extra_args),*)?);
                        $funktion(frame, &$daten.kreuzungen, $arg_macro!(Kreuzung)$(, $($extra_args),*)?);
                    };
                }
                // Hintergrund
                for (name, (Streckenabschnitt { farbe, .. }, fließend, daten)) in
                    zustand.streckenabschnitte.iter()
                {
                    macro_rules! transparenz {
                        ($gleis: ident) => {
                            |rectangle, fließend| {
                                Transparenz::true_reduziert(if modus_bauen {
                                    ist_gehalten(
                                        AnyIdRef::from(GleisIdRef {
                                        rectangle,
                                        streckenabschnitt: Some(name),
                                        phantom:PhantomData::<fn() -> $gleis<Z>>
                                        })
                                    )
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
                        farbe,
                        fließend
                    }
                }
                // Kontur
                for (streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten {
                        ($gleis: ident) => {
                            |rectangle| ist_gehalten(AnyIdRef::from(GleisIdRef {
                                rectangle,
                                streckenabschnitt,
                                phantom: PhantomData::<fn() -> $gleis<Z>>
                            }))
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        zeichne_alle_gleise,
                        ist_gehalten,
                    }
                }
                // Verbindungen
                for (streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten_und_andere_entgegengesetzt_oder_gehaltene_verbindung {
                        ($gleis: ident) => {
                            |rectangle: &Rectangle<Vektor>, verbindung: Verbindung| {
                                let gehalten = ist_gehalten(AnyIdRef::from(GleisIdRef {
                                    rectangle,
                                    streckenabschnitt,
                                    phantom: PhantomData::<fn() -> $gleis<Z>>
                                }));
                                let any_id = AnyIdRef::from(GleisIdRef {
                                    rectangle,
                                    streckenabschnitt,
                                    phantom: PhantomData::<fn() -> $gleis<Z>>
                                });
                                let (mut überlappende, andere_gehalten) = self.zustand.überlappende_verbindungen(&verbindung, &any_id, gehalten_id);
                                let ist_entgegengesetzt = |überlappend: &Verbindung| {
                                    (winkel::PI + verbindung.richtung - überlappend.richtung)
                                        .normalisiert()
                                        .abs()
                                        < Winkel(0.1)
                                };
                                let entgegengesetzt = überlappende.find(ist_entgegengesetzt).is_some();
                                (gehalten, entgegengesetzt, andere_gehalten)
                            }
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        zeichne_alle_anchor_points,
                        ist_gehalten_und_andere_entgegengesetzt_oder_gehaltene_verbindung
                    }
                }
                // Beschreibung
                for (streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                    macro_rules! ist_gehalten {
                        ($gleis: ident) => {
                            |rectangle| ist_gehalten(AnyIdRef::from(GleisIdRef {
                                rectangle,
                                streckenabschnitt,
                                phantom: PhantomData::<fn() -> $gleis<Z>>
                            }))
                        };
                    }
                    mit_allen_gleisen! {
                        daten,
                        schreibe_alle_beschreibungen,
                        ist_gehalten
                    }
                }
            },
        )]
    }
}
