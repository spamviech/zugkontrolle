//! [draw](iced::Application::draw)-Methode für [Gleise].

use iced::{
    canvas::{Cursor, Geometry},
    Point,
};

use crate::{
    anschluss::polarität::Fließend,
    gleis::{
        gleise::{
            daten::{Gleis, RStern, Zustand},
            AnyGleis, Gehalten, Gleise, ModusDaten,
        },
        verbindung::Verbindung,
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

struct AndereVerbindung {
    /// Es existiert eine entgegengesetzte [Verbindung] in der Nähe.
    andere_entgegengesetzt: bool,
    /// Es existiert eine gehaltene [Verbindung] in der Nähe.
    andere_gehalten: bool,
}

enum SelbstGehalten<'t> {
    /// Es geht um die Verbindungen des gehaltenen Gleises.
    Selbst,
    /// Es geht um die Verbindungen eines nicht-gehaltenen Gleises.
    Anderes(Option<(&'t AnyGleis, Option<&'t Farbe>)>),
}

fn finde_andere_verbindung<'t, L: Leiter>(
    zustand: &'t Zustand<L>,
    gehalten: SelbstGehalten<'t>,
) -> impl 't + Fn(Verbindung) -> AndereVerbindung {
    let spurweite = zustand.zugtyp.spurweite;
    move |verbindung: Verbindung| {
        let überlappende: Vec<_> = zustand.überlappende_verbindungen(&verbindung).collect();
        let ist_entgegengesetzt = |überlappend: &&Verbindung| {
            (winkel::PI + verbindung.richtung - überlappend.richtung).normalisiert().abs()
                < Winkel(0.1)
        };
        let andere_entgegengesetzt = überlappende.iter().find(ist_entgegengesetzt).is_some();
        let andere_gehalten = match gehalten {
            SelbstGehalten::Selbst => !überlappende.is_empty(),
            SelbstGehalten::Anderes(gehalten) => {
                let überlappend_gehalten = gehalten.map_or(Vec::new(), |(gleis, _farbe)| {
                    mit_any_gleis!(gleis, Gleis::überlappende_verbindungen, spurweite, &verbindung)
                });
                !überlappend_gehalten.is_empty()
            },
        };
        AndereVerbindung { andere_entgegengesetzt, andere_gehalten }
    }
}

fn zeichne_anchor_points<T: Zeichnen>(
    frame: &mut Frame<'_>,
    spurweite: Spurweite,
    gleis: &Gleis<T>,
    transparenz: &Transparenz,
    ist_gehalten_und_andere_verbindung: impl Fn(Verbindung) -> AndereVerbindung,
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
                let AndereVerbindung { andere_entgegengesetzt, andere_gehalten } =
                    ist_gehalten_und_andere_verbindung(verbindung_an_position);
                let a = transparenz.alpha();
                let g = if andere_entgegengesetzt { 1. } else { 0. };
                let b = 1. - g;
                let color = Color { r: 0., g, b, a };
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
    transparenz: &Transparenz,
    ist_gehalten_und_andere_verbindung: impl Fn(Verbindung) -> AndereVerbindung,
) {
    for geom_with_data in rstern.iter() {
        let gleis = &geom_with_data.data;
        zeichne_anchor_points(
            frame,
            spurweite,
            gleis,
            transparenz,
            &ist_gehalten_und_andere_verbindung,
        )
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
            let gehalten_gleis_und_farbe: Option<(&AnyGleis, Option<&Farbe>)>;
            let modus_bauen: bool;
            match modus {
                ModusDaten::Bauen { gehalten, .. } => {
                    gehalten_gleis_und_farbe =
                        gehalten.as_ref().map(|Gehalten { gleis, streckenabschnitt, .. }| {
                            (gleis, streckenabschnitt.as_ref().map(|(_id, farbe)| farbe))
                        });
                    modus_bauen = true;
                },
                ModusDaten::Fahren => {
                    gehalten_gleis_und_farbe = None;
                    modus_bauen = false;
                },
            };

            macro_rules! mit_allen_gleisen {
                ($daten:expr, $funktion:expr $(, $($extra_args:expr),* $(,)?)?) => {
                    $funktion(frame, spurweite, &$daten.geraden $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.kurven $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.weichen $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.kurven_weichen $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.dreiwege_weichen $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.s_kurven_weichen $(, $($extra_args),*)? );
                    $funktion(frame, spurweite, &$daten.kreuzungen $(, $($extra_args),*)? );
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
                mit_allen_gleisen! {
                    daten,
                    fülle_alle_gleise,
                    &transparenz,
                    &farbe,
                }
            }
            // Kontur
            let transparenz_voll = Transparenz::Voll;
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                mit_allen_gleisen! {
                    daten,
                    zeichne_alle_gleise,
                    &transparenz_voll,
                }
            }
            // Verbindungen
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                mit_allen_gleisen! {
                    daten,
                    zeichne_alle_anchor_points,
                    &transparenz_voll,
                    finde_andere_verbindung(
                        &self.zustand,
                        SelbstGehalten::Anderes(gehalten_gleis_und_farbe)
                    )
                }
            }
            // Beschreibung
            for (_streckenabschnitt, daten) in zustand.alle_streckenabschnitt_daten() {
                mit_allen_gleisen! {
                    daten,
                    schreibe_alle_beschreibungen,
                    &transparenz_voll
                }
            }
            // TODO markiere gehalten als "wird-gelöscht", falls cursor out of bounds ist
            // Gehaltenes Gleis
            if let Some((gleis, farbe)) = gehalten_gleis_und_farbe {
                let transparenz = Transparenz::Reduziert;
                if let Some(farbe) = farbe {
                    mit_any_gleis!(=> frame, spurweite => gleis, fülle_gleis, &transparenz, farbe);
                }
                mit_any_gleis!(=> frame, spurweite => gleis, zeichne_gleis, &transparenz);
                mit_any_gleis!(
                    => frame, spurweite =>
                    gleis,
                    zeichne_anchor_points,
                    &transparenz,
                    finde_andere_verbindung(&self.zustand, SelbstGehalten::Selbst)
                );
                mit_any_gleis!(=> frame, spurweite => gleis, schreibe_beschreibung, &transparenz);
            }
        };
        vec![canvas.lock().zeichnen_skaliert_von_pivot(bounds.size(), pivot, skalieren, draw_frame)]
    }
}
