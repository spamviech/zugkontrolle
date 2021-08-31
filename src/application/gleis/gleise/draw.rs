//! draw-Methode für Gleise

use crate::{
    anschluss::Fließend,
    application::{
        gleis::gleise::{id::*, maps::*, Gleise, Grabbed, ModusDaten},
        typen::*,
        verbindung,
    },
    farbe::Farbe,
    lookup::Lookup,
    steuerung::{streckenabschnitt, Streckenabschnitt},
    zugtyp::Zugtyp,
};

pub(crate) fn move_to_position(frame: &mut canvas::Frame, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

fn fülle_alle_gleise<T, Z>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    transparent: impl Fn(AnyId<Z>, Fließend) -> Transparenz,
    streckenabschnitte: &streckenabschnitt::Map,
) where
    T: Zeichnen,
    GleisId<T>: Into<AnyId<Z>>,
{
    for (gleis_id, Gleis { definition, position, streckenabschnitt }) in map.iter() {
        if let Some((Streckenabschnitt { farbe, .. }, fließend)) =
            streckenabschnitt.as_ref().and_then(|name| streckenabschnitte.get(name))
        {
            frame.with_save(|frame| {
                move_to_position(frame, position);
                // einfärben
                for (path, transparenz) in definition.fülle() {
                    let a = transparent(AnyId::from_ref(gleis_id), *fließend)
                        .kombiniere(transparenz)
                        .alpha();
                    frame.with_save(|frame| {
                        let Farbe { r, g, b } = *farbe;
                        let color = iced::Color { r, g, b, a };
                        frame.fill(&path, canvas::Fill { color, rule: canvas::FillRule::EvenOdd });
                    });
                }
            })
        }
    }
}

fn zeichne_alle_gleise<T, Z>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    is_grabbed: impl Fn(AnyId<Z>) -> bool,
) where
    T: Zeichnen,
    GleisId<T>: Into<AnyId<Z>>,
{
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    let a =
                        Transparenz::true_reduziert(is_grabbed(AnyId::from_ref(gleis_id))).alpha();
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

fn zeichne_alle_anchor_points<T, Z>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    has_other_and_grabbed_id_at_point: impl Fn(AnyId<Z>, verbindung::Anchor) -> (bool, bool),
    is_grabbed: impl Fn(AnyId<Z>) -> bool,
) where
    T: Zeichnen,
    GleisId<T>: Into<AnyId<Z>>,
{
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.anchor_points().for_each(|_name, &anchor| {
                frame.with_save(|frame| {
                    let (opposing, grabbed) = has_other_and_grabbed_id_at_point(
                        AnyId::from_ref(gleis_id),
                        verbindung::Anchor {
                            position: position.transformation(anchor.position),
                            richtung: position.winkel + anchor.richtung,
                        },
                    );
                    let a =
                        Transparenz::true_reduziert(is_grabbed(AnyId::from_ref(gleis_id))).alpha();
                    let g = if opposing { 1. } else { 0. };
                    let color = canvas::Color { r: 0., g, b: 1. - g, a };
                    let direction: Vektor = Vektor::polar_koordinaten(Skalar(5.), anchor.richtung);
                    let direction_side: Vektor = Skalar(0.5) * direction.rotiert(winkel::FRAC_PI_2);
                    let anchor_position: Vektor = anchor.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(anchor_position + direction_side);
                    path_builder.line_to(anchor_position + direction);
                    path_builder.line_to(anchor_position - direction_side);
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

fn schreibe_alle_beschreibungen<T, Z>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    is_grabbed: impl Fn(AnyId<Z>) -> bool,
) where
    T: Zeichnen,
    GleisId<T>: Into<AnyId<Z>>,
{
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
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
                let a = Transparenz::true_reduziert(is_grabbed(AnyId::from_ref(gleis_id))).alpha();
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
        let Gleise {
            canvas,
            maps:
                GleiseMaps {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                    streckenabschnitte,
                },
            anchor_points,
            modus,
            ..
        } = self;
        vec![canvas.draw_skaliert_von_pivot(bounds.size(), &self.pivot, &self.skalieren, |frame| {
            // TODO zeichne keine out-of-bounds Gleise
            // Zeichne Gleise
            let grabbed_id: Option<AnyId<Z>>;
            let modus_bauen: bool;
            match modus {
                ModusDaten::Bauen { grabbed: Some(Grabbed { gleis_id, .. }), .. } => {
                    grabbed_id = Some(gleis_id.clone());
                    modus_bauen = true;
                }
                ModusDaten::Bauen { grabbed: None, .. } => {
                    grabbed_id = None;
                    modus_bauen = true;
                }
                ModusDaten::Fahren => {
                    grabbed_id = None;
                    modus_bauen = false;
                }
            };
            // TODO markiere grabbed als "wird-gelöscht", falls cursor out of bounds ist
            let is_grabbed = |parameter_id| Some(parameter_id) == grabbed_id;
            let has_other_and_grabbed_id_at_point = |gleis_id, position| {
                anchor_points.has_other_and_grabbed_id_at_point(
                    &gleis_id,
                    |id| is_grabbed(id.clone()),
                    &position,
                )
            };

            macro_rules! mit_allen_gleisen {
                ($funktion:expr$(, $($extra_args:expr),+)?) => {
                    $funktion(frame, geraden$(, $($extra_args),+)?);
                    $funktion(frame, kurven$(, $($extra_args),+)?);
                    $funktion(frame, weichen$(, $($extra_args),+)?);
                    $funktion(frame, kurven_weichen$(, $($extra_args),+)?);
                    $funktion(frame, s_kurven_weichen$(, $($extra_args),+)?);
                    $funktion(frame, dreiwege_weichen$(, $($extra_args),+)?);
                    $funktion(frame, kreuzungen$(, $($extra_args),+)?);
                };
            }
            // Hintergrund
            mit_allen_gleisen!(
                fülle_alle_gleise,
                |gleis_id, fließend| Transparenz::true_reduziert(if modus_bauen {
                    is_grabbed(gleis_id)
                } else {
                    fließend == Fließend::Gesperrt
                }),
                streckenabschnitte
            );
            // Kontur
            mit_allen_gleisen!(zeichne_alle_gleise, is_grabbed);
            // AnchorPoints
            mit_allen_gleisen!(
                zeichne_alle_anchor_points,
                has_other_and_grabbed_id_at_point,
                &is_grabbed
            );
            // Beschreibung
            mit_allen_gleisen!(schreibe_alle_beschreibungen, is_grabbed);
        })]
    }
}
