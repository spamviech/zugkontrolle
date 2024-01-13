//! Widget zum Anpassen des Pivot Punktes.

use iced::{
    mouse::{self, Cursor},
    widget::canvas::{event, Event, Geometry, Program, Stroke, Style},
    Rectangle, Renderer,
};

use crate::{
    application::style::thema::Thema,
    typen::{
        canvas::{
            pfad::{self, Bogen},
            Cache,
        },
        skalar::Skalar,
        vektor::Vektor,
        winkel,
    },
};

/// Mögliche Bewegungen.
#[derive(Debug, Clone, Copy)]
pub enum Bewegung {
    /// Vertikale Bewegung nach oben.
    Oben,
    /// Vertikale Bewegung nach unten.
    Unten,
    /// Horizontale Bewegung nach links.
    Links,
    /// Horizontale Bewegung nach rechts.
    Rechts,
    /// Diagonale Bewegung nach oben und links.
    ObenLinks,
    /// Diagonale Bewegung nach oben und rechts.
    ObenRechts,
    /// Diagonale Bewegung nach unten und links.
    UntenLinks,
    /// Diagonale Bewegung nach unten und rechts.
    UntenRechts,
}

impl Bewegung {
    /// Bewegung als Vektor der gegebenen Länge.
    #[must_use]
    pub fn vektor(self, länge: Skalar) -> Vektor {
        let bogenmaß = match self {
            Bewegung::Rechts => 0.,
            Bewegung::UntenRechts => 0.25,
            Bewegung::Unten => 0.5,
            Bewegung::UntenLinks => 0.75,
            Bewegung::Links => 1.,
            Bewegung::ObenLinks => 1.25,
            Bewegung::Oben => 1.5,
            Bewegung::ObenRechts => 1.75,
        };
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let gradmaß = bogenmaß * winkel::PI;
        Vektor::polar_koordinaten(länge, gradmaß)
    }
}

/// Nachricht des [`Bewegen`]-Widgets.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Beginne eine kontinuierliche Bewegung.
    StarteBewegung(Bewegung),
    /// Beende die kontinuierliche Bewegung.
    BeendeBewegung,
    /// Setze den Pivot-Punkt auf `(0,0)` zurück.
    Zurücksetzen,
}

/// Widget zum Anpassen des Pivot-Punktes.
#[derive(Debug)]
pub struct Bewegen(Cache);

impl Bewegen {
    /// Erstelle ein neues [`Bewegen`]-Widget.
    #[must_use]
    pub fn neu() -> Self {
        Bewegen(Cache::neu())
    }
}

impl Program<Nachricht, Renderer<Thema>> for Bewegen {
    type State = bool;

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer<Thema>,
        thema: &Thema,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let size = bounds.size();
        let width = Skalar(size.width);
        let height = Skalar(size.height);
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        // Startpunkte
        let links = Vektor { x: Skalar(0.), y: half_height };
        let rechts = Vektor { x: width, y: half_height };
        let oben = Vektor { x: half_width, y: Skalar(0.) };
        let unten = Vektor { x: half_width, y: height };
        let zentrum = Vektor { x: half_width, y: half_height };
        // relative Bewegung
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let diagonal_runter = Skalar(0.3) * Vektor { x: half_width, y: half_height };
        let mut diagonal_hoch = diagonal_runter;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            diagonal_hoch.y = -diagonal_hoch.y;
        }
        // Inkreis-Radius r = 2A/u
        // https://de.wikipedia.org/wiki/Inkreis
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
        // erzeuge Pfad
        let mut erbauer = pfad::Erbauer::neu();
        // links
        erbauer.move_to(links);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(links + diagonal_hoch);
        erbauer.move_to(links);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(links + diagonal_runter);
        // rechts
        erbauer.move_to(rechts);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(rechts - diagonal_hoch);
        erbauer.move_to(rechts);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(rechts - diagonal_runter);
        // oben
        erbauer.move_to(oben);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(oben - diagonal_hoch);
        erbauer.move_to(oben);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(oben + diagonal_runter);
        // unten
        erbauer.move_to(unten);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(unten + diagonal_hoch);
        erbauer.move_to(unten);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        erbauer.line_to(unten - diagonal_runter);
        // zurücksetzen
        erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU });
        let pfad = erbauer.baue();
        vec![self.0.zeichnen(renderer, size, |frame| {
            frame.stroke(
                &pfad,
                Stroke { style: Style::Solid(thema.strich().into()), ..Stroke::default() },
            );
        })]
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        let mut nachricht = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    let size = bounds.size();
                    let width = Skalar(size.width);
                    let height = Skalar(size.height);
                    let half_width = width.halbiert();
                    let half_height = height.halbiert();
                    // Startpunkte
                    let links = Vektor { x: Skalar(0.), y: half_height };
                    let rechts = Vektor { x: width, y: half_height };
                    let oben = Vektor { x: half_width, y: Skalar(0.) };
                    let unten = Vektor { x: half_width, y: height };
                    let zentrum = Vektor { x: half_width, y: half_height };
                    // relative Bewegung
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let diagonal_runter = Skalar(0.3) * Vektor { x: half_width, y: half_height };
                    // Grenzen
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let links_grenze = links.x + diagonal_runter.x;
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let rechts_grenze = rechts.x - diagonal_runter.x;
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let oben_grenze = oben.y + diagonal_runter.y;
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let unten_grenze = unten.y - diagonal_runter.y;
                    // Inkreis-Radius r = 2A/u
                    // https://de.wikipedia.org/wiki/Inkreis
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    let klick_radius =
                        (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
                    if position.x < links_grenze.0 {
                        *state = true;
                        let bewegung = if position.y < oben_grenze.0 {
                            Bewegung::ObenLinks
                        } else if position.y > unten_grenze.0 {
                            Bewegung::UntenLinks
                        } else {
                            Bewegung::Links
                        };
                        nachricht = Some(Nachricht::StarteBewegung(bewegung));
                    } else if position.x >= rechts_grenze.0 {
                        *state = true;
                        let bewegung = if position.y < oben_grenze.0 {
                            Bewegung::ObenRechts
                        } else if position.y >= unten_grenze.0 {
                            Bewegung::UntenRechts
                        } else {
                            Bewegung::Rechts
                        };
                        nachricht = Some(Nachricht::StarteBewegung(bewegung));
                    } else if position.y < oben_grenze.0 {
                        *state = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Oben));
                    } else if position.y >= unten_grenze.0 {
                        *state = true;
                        nachricht = Some(Nachricht::StarteBewegung(Bewegung::Unten));
                    } else if klick_radius < radius {
                        nachricht = Some(Nachricht::Zurücksetzen);
                    } else {
                        // Kein aktives Element angeklickt.
                    }
                }
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) if *state => {
                // beende nur gestartete Bewegungen
                *state = false;
                nachricht = Some(Nachricht::BeendeBewegung);
            },
            // TODO touch-Events berücksichtigen
            Event::Mouse(_) | Event::Touch(_) | Event::Keyboard(_) => {},
        }

        (event::Status::Ignored, nachricht)
    }

    fn mouse_interaction(
        &self,
        _state: &Self::State,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> mouse::Interaction {
        let mut interaction = mouse::Interaction::default();
        if let Some(position) = cursor.position_in(bounds) {
            let size = bounds.size();
            let width = Skalar(size.width);
            let height = Skalar(size.height);
            let half_width = width.halbiert();
            let half_height = height.halbiert();
            // Startpunkte
            let links = Vektor { x: Skalar(0.), y: half_height };
            let rechts = Vektor { x: width, y: half_height };
            let oben = Vektor { x: half_width, y: Skalar(0.) };
            let unten = Vektor { x: half_width, y: height };
            let zentrum = Vektor { x: half_width, y: half_height };
            // relative Bewegung
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let diagonal_runter = Skalar(0.3) * Vektor { x: half_width, y: half_height };
            // Grenzen
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let links_grenze = links.x + diagonal_runter.x;
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let rechts_grenze = rechts.x - diagonal_runter.x;
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let oben_grenze = oben.y + diagonal_runter.y;
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let unten_grenze = unten.y - diagonal_runter.y;
            // Inkreis-Radius r = 2A/u
            // https://de.wikipedia.org/wiki/Inkreis
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let klick_radius =
                (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
            if (position.x < links_grenze.0)
                || (position.x >= rechts_grenze.0)
                || (position.y < oben_grenze.0)
                || (position.y >= unten_grenze.0)
                || (klick_radius < radius)
            {
                interaction = mouse::Interaction::Pointer;
            }
        }
        interaction
    }
}
