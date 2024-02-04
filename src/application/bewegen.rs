//! Widget zum Anpassen des Pivot Punktes.

use iced::{
    mouse::{self, Cursor},
    touch,
    widget::canvas::{event, Event, Geometry, Program, Stroke, Style},
    Point, Rectangle, Renderer,
};

use crate::{
    application::style::thema::Thema,
    gleis::knopf::KlickQuelle,
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
    type State = Option<KlickQuelle>;

    // FIXME for now
    #[allow(clippy::too_many_lines)]
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
        let diagonale_länge = (links - oben).länge();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let bein_länge = diagonale_länge / Skalar(3.);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let diagonal_runter =
            bein_länge * Vektor { x: half_width, y: half_height }.einheitsvektor();
        let diagonal_hoch = Vektor {
            x: diagonal_runter.x,
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: -diagonal_runter.y,
        };
        // Zielpunkte
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_links_oben = links + diagonal_hoch;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_links_unten = links + diagonal_runter;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_rechts_oben = rechts - diagonal_runter;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_rechts_unten = rechts - diagonal_hoch;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_oben_links = oben - diagonal_hoch;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_oben_rechts = oben + diagonal_runter;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_unten_links = unten - diagonal_runter;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende_unten_rechts = unten + diagonal_hoch;

        // erzeuge Pfad
        let mut erbauer = pfad::Erbauer::neu();
        // links
        erbauer.move_to(links);
        erbauer.line_to(ende_links_oben);
        erbauer.move_to(links);
        erbauer.line_to(ende_links_unten);
        // rechts
        erbauer.move_to(rechts);
        erbauer.line_to(ende_rechts_oben);
        erbauer.move_to(rechts);
        erbauer.line_to(ende_rechts_unten);
        // oben
        erbauer.move_to(oben);
        erbauer.line_to(ende_oben_links);
        erbauer.move_to(oben);
        erbauer.line_to(ende_oben_rechts);
        // unten
        erbauer.move_to(unten);
        erbauer.line_to(ende_unten_links);
        erbauer.move_to(unten);
        erbauer.line_to(ende_unten_rechts);

        // Diagonale Start-Werte
        let abstand_diagonale = Skalar(
            ((bein_länge.0.powf(2.)) - ((0.5 - (1. / 3.)) * diagonale_länge.0).powf(2.)).sqrt(),
        );
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_nach_oben = oben - links;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_oben = links
            + Skalar(0.5) * links_nach_oben
            + abstand_diagonale * links_nach_oben.rotiert(-winkel::FRAC_PI_2).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_nach_unten = unten - links;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_unten = links
            + Skalar(0.5) * links_nach_unten
            + abstand_diagonale * links_nach_unten.rotiert(winkel::FRAC_PI_2).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_nach_oben = oben - rechts;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_oben = rechts
            + Skalar(0.5) * rechts_nach_oben
            + abstand_diagonale * rechts_nach_oben.rotiert(winkel::FRAC_PI_2).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_nach_unten = unten - rechts;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_unten = rechts
            + Skalar(0.5) * rechts_nach_unten
            + abstand_diagonale * rechts_nach_unten.rotiert(-winkel::FRAC_PI_2).einheitsvektor();

        // links-oben
        erbauer.move_to(links_oben);
        erbauer.line_to(ende_links_oben);
        erbauer.move_to(links_oben);
        erbauer.line_to(ende_oben_links);
        // links-unten
        erbauer.move_to(links_unten);
        erbauer.line_to(ende_links_unten);
        erbauer.move_to(links_unten);
        erbauer.line_to(ende_unten_links);
        // rechts-oben
        erbauer.move_to(rechts_oben);
        erbauer.line_to(ende_rechts_oben);
        erbauer.move_to(rechts_oben);
        erbauer.line_to(ende_oben_rechts);
        // rechts-unten
        erbauer.move_to(rechts_unten);
        erbauer.line_to(ende_rechts_unten);
        erbauer.move_to(rechts_unten);
        erbauer.line_to(ende_unten_rechts);

        // zurücksetzen
        // Inkreis-Radius r = 2A/u
        // https://de.wikipedia.org/wiki/Inkreis
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let radius = Skalar(0.75) * (half_width * half_height) / (width + height);
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
        /// Reagiere auf einen Maus- oder Touch-Klick.
        fn pressed(
            state: &mut Option<KlickQuelle>,
            bounds: Rectangle,
            position: Point,
            klick_quelle: KlickQuelle,
        ) -> Option<Nachricht> {
            if state.is_some() {
                return None;
            }
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
                *state = Some(klick_quelle);
                let bewegung = if position.y < oben_grenze.0 {
                    Bewegung::ObenLinks
                } else if position.y > unten_grenze.0 {
                    Bewegung::UntenLinks
                } else {
                    Bewegung::Links
                };
                Some(Nachricht::StarteBewegung(bewegung))
            } else if position.x >= rechts_grenze.0 {
                *state = Some(klick_quelle);
                let bewegung = if position.y < oben_grenze.0 {
                    Bewegung::ObenRechts
                } else if position.y >= unten_grenze.0 {
                    Bewegung::UntenRechts
                } else {
                    Bewegung::Rechts
                };
                Some(Nachricht::StarteBewegung(bewegung))
            } else if position.y < oben_grenze.0 {
                *state = Some(klick_quelle);
                Some(Nachricht::StarteBewegung(Bewegung::Oben))
            } else if position.y >= unten_grenze.0 {
                *state = Some(klick_quelle);
                Some(Nachricht::StarteBewegung(Bewegung::Unten))
            } else if klick_radius < radius {
                Some(Nachricht::Zurücksetzen)
            } else {
                // Kein aktives Element angeklickt.
                None
            }
        }
        let mut nachricht = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    nachricht = pressed(state, bounds, position, KlickQuelle::Maus);
                }
            },
            Event::Touch(touch::Event::FingerPressed { id, position }) => {
                nachricht = pressed(state, bounds, position, KlickQuelle::Touch(id));
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
                if *state == Some(KlickQuelle::Maus) =>
            {
                // Beende nur mit der Maus gestartete Bewegungen
                *state = None;
                nachricht = Some(Nachricht::BeendeBewegung);
            },
            Event::Touch(
                touch::Event::FingerLifted { id, position: _ }
                | touch::Event::FingerLost { id, position: _ },
            ) if *state == Some(KlickQuelle::Touch(id)) => {
                // Beende nur mit dem selben Finger gestartete Bewegungen
                *state = None;
                nachricht = Some(Nachricht::BeendeBewegung);
            },
            Event::Mouse(_) | Event::Touch(_) | Event::Keyboard(_) => {},
        }

        let status =
            if nachricht.is_some() { event::Status::Captured } else { event::Status::Ignored };

        (status, nachricht)
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
