//! draw-Methode für Gleise

use rstar::primitives::Rectangle;

use crate::{
    anschluss::Fließend,
    application::{
        gleis::gleise::{daten::*, id::*, Gehalten, Gleise, ModusDaten},
        typen::*,
        verbindung,
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
        let (definition, position) = &geom_with_data.data;
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
        let (definition, position) = &geom_with_data.data;
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
    existiert_andere_oder_gehaltene_verbindung: impl Fn(
        &'r Rectangle<Vektor>,
        &'r verbindung::Verbindung,
    ) -> (bool, bool),
    ist_gehalten: impl Fn(&'s Rectangle<Vektor>) -> bool,
) {
    for geom_with_data in rstern.iter() {
        let rectangle = geom_with_data.geom();
        let (definition, position) = &geom_with_data.data;
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.verbindungen().for_each(|_name, &verbindung| {
                let verbindung_an_position = verbindung::Verbindung {
                    position: position.transformation(verbindung.position),
                    richtung: position.winkel + verbindung.richtung,
                };
                frame.with_save(|frame| {
                    let (opposing, grabbed) = existiert_andere_oder_gehaltene_verbindung(
                        rectangle,
                        &verbindung_an_position,
                    );
                    let a = Transparenz::true_reduziert(ist_gehalten(rectangle)).alpha();
                    let g = if opposing { 1. } else { 0. };
                    let color = canvas::Color { r: 0., g, b: 1. - g, a };
                    let direction: Vektor =
                        Vektor::polar_koordinaten(Skalar(5.), verbindung.richtung);
                    let direction_side: Vektor = Skalar(0.5) * direction.rotiert(winkel::FRAC_PI_2);
                    let verbindung_position: Vektor = verbindung.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(verbindung_position + direction_side);
                    path_builder.line_to(verbindung_position + direction);
                    path_builder.line_to(verbindung_position - direction_side);
                    let path = path_builder.baue();
                    frame.stroke(&path, canvas::Stroke { color, width: 1.5, ..Default::default() });
                    // fill on connect/snap for drag&drop
                    if grabbed {
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
        let (definition, position) = &geom_with_data.data;
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
        vec![canvas.zeichnen_skaliert_von_pivot(
            bounds.size(),
            &self.pivot,
            &self.skalieren,
            |frame| {
                // TODO zeichne keine out-of-bounds Gleise
                // Zeichne Gleise
                let gehalten_id: Option<AnyId<Z>>;
                let modus_bauen: bool;
                match modus {
                    ModusDaten::Bauen { gehalten: Some(Gehalten { gleis_id, .. }), .. } => {
                        gehalten_id = Some(gleis_id.clone());
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
                let ist_gehalten = |parameter_id| {
                    // Some(parameter_id) == grabbed_id
                    todo!()
                };
                let existiert_andere_oder_gehaltene_verbindung = |gleis_id, position| {
                    // verbindungen.hat_andere_id_und_grabbed_an_position(
                    //     &gleis_id,
                    //     |id| ist_gehalten(id.clone()),
                    //     &position,
                    // )
                    todo!()
                };

                macro_rules! mit_allen_gleisen {
                    ($maps:expr, $funktion:expr$(, $($extra_args:expr),+)?) => {
                        $funktion(frame, &$maps.geraden$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.kurven$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.weichen$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.kurven_weichen$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.s_kurven_weichen$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.dreiwege_weichen$(, $($extra_args),+)?);
                        $funktion(frame, &$maps.kreuzungen$(, $($extra_args),+)?);
                    };
                }
                // Hintergrund
                for (Streckenabschnitt { farbe, .. }, fließend, maps) in
                    self.zustand.streckenabschnitte.values()
                {
                    mit_allen_gleisen! {
                        maps,
                        fülle_alle_gleise,
                        |rectangle, fließend| Transparenz::true_reduziert(if modus_bauen {
                            ist_gehalten(rectangle)
                        } else {
                            fließend == Fließend::Gesperrt
                        }),
                        farbe,
                        fließend
                    }
                }
                // Kontur
                for (streckenabschnitt, maps) in self.zustand.alle_gleise_maps() {
                    mit_allen_gleisen! {
                        maps,
                        zeichne_alle_gleise,
                        ist_gehalten
                    }
                }
                // Verbindungen
                for (streckenabschnitt, maps) in self.zustand.alle_gleise_maps() {
                    mit_allen_gleisen! {
                        maps,
                        zeichne_alle_anchor_points,
                        existiert_andere_oder_gehaltene_verbindung,
                        &ist_gehalten
                    }
                }
                // Beschreibung
                for (streckenabschnitt, maps) in self.zustand.alle_gleise_maps() {
                    mit_allen_gleisen! {
                        maps,
                        schreibe_alle_beschreibungen,
                        ist_gehalten
                    }
                }
            },
        )]
    }
}
