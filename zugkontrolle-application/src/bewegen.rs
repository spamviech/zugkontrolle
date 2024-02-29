//! Widget zum Anpassen des Pivot Punktes.

use iced::{
    mouse::{self, Cursor},
    touch,
    widget::canvas::{event, Event, Geometry, Program, Stroke, Style},
    Point, Rectangle, Renderer, Size,
};
use itertools::{Itertools, MinMaxResult};

use zugkontrolle_gleise::knopf::Thema as _;
use zugkontrolle_typen::{
    canvas::{
        pfad::{self, Bogen},
        Cache,
    },
    klick_quelle::KlickQuelle,
    skalar::Skalar,
    vektor::Vektor,
    winkel,
};

use crate::style::thema::Thema;

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

    /// Erzwinge ein neuzeichnen des Canvas.
    pub fn erzwinge_neuzeichnen(&mut self) {
        self.0.leeren();
    }
}

/// Wichtige Punkte und Größen für die Darstellung und Interaktion mit dem Widget.
struct WichtigeWerte {
    #[allow(clippy::missing_docs_in_private_items)]
    links: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    rechts: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    oben: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    unten: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    zentrum: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_links_oben: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_links_unten: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_rechts_oben: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_rechts_unten: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_oben_links: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_oben_rechts: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_unten_links: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    ende_unten_rechts: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    links_oben: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    links_unten: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    rechts_oben: Vektor,
    #[allow(clippy::missing_docs_in_private_items)]
    rechts_unten: Vektor,
    /// Der Radius für den Zurücksetzen-Kreis.
    radius: Skalar,
}

impl WichtigeWerte {
    /// Erzeuge alle [`WichtigenPunkte`] innerhalb der gegebenen Bounds.
    #[must_use]
    fn aus_size(size: Size) -> Self {
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
            + abstand_diagonale * links_nach_oben.rotiert(&(-winkel::FRAC_PI_2)).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_nach_unten = unten - links;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_unten = links
            + Skalar(0.5) * links_nach_unten
            + abstand_diagonale * links_nach_unten.rotiert(&winkel::FRAC_PI_2).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_nach_oben = oben - rechts;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_oben = rechts
            + Skalar(0.5) * rechts_nach_oben
            + abstand_diagonale * rechts_nach_oben.rotiert(&winkel::FRAC_PI_2).einheitsvektor();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_nach_unten = unten - rechts;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let rechts_unten = rechts
            + Skalar(0.5) * rechts_nach_unten
            + abstand_diagonale * rechts_nach_unten.rotiert(&(-winkel::FRAC_PI_2)).einheitsvektor();

        // Zurücksetzen
        // Inkreis-Radius r = 2A/u
        // https://de.wikipedia.org/wiki/Inkreis
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let radius = Skalar(0.75) * (half_width * half_height) / (width + height);

        WichtigeWerte {
            links,
            rechts,
            oben,
            unten,
            zentrum,
            ende_links_oben,
            ende_links_unten,
            ende_rechts_oben,
            ende_rechts_unten,
            ende_oben_links,
            ende_oben_rechts,
            ende_unten_links,
            ende_unten_rechts,
            links_oben,
            links_unten,
            rechts_oben,
            rechts_unten,
            radius,
        }
    }
}

// Gibt es bessere Namen für die Ecken eines Dreiecks?
#[allow(clippy::min_ident_chars)]
/// Liegt der `punkt` innerhalb des Dreiecks `a`-`b`-`c`.
///
/// <https://prlbr.de/2014/liegt-der-punkt-im-dreieck/>
/// Ansatz 4
fn punkt_innerhalb_dreieck(punkt: Vektor, a: Vektor, b: Vektor, c: Vektor) -> bool {
    /// Ordnung identisch zum Winkel zwischen `vektor` und positiver x-Achse.
    fn winkel_ordnung(vektor: Vektor) -> Skalar {
        let Vektor { x, y } = vektor;
        let faktor = if y >= Skalar(0.) { Skalar(1.) } else { Skalar(-1.) };
        // Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            faktor * (Skalar(1.) - (x / (x.abs() + y.abs())))
        }
    }
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let schwerpunkt = (a + b + c) / Skalar(3.);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let punkt_rel = punkt - schwerpunkt;
    let punkt_foo = winkel_ordnung(punkt_rel);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let a_rel = a - schwerpunkt;
    let a_foo = winkel_ordnung(a_rel);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let b_rel = b - schwerpunkt;
    let b_foo = winkel_ordnung(b_rel);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let c_rel = c - schwerpunkt;
    let c_foo = winkel_ordnung(c_rel);
    let dreieck_winkel_ordnung_werte = [(a_foo, a), (b_foo, b), (c_foo, c)];
    let winkel_ordnung_key = |(winkel_ordnung, _point): &(Skalar, Vektor)| *winkel_ordnung;
    let größtes_kleiner = dreieck_winkel_ordnung_werte
        .iter()
        .copied()
        .filter(|(winkel_ordnung, _point)| *winkel_ordnung < punkt_foo)
        .minmax_by_key(winkel_ordnung_key)
        .into_option()
        .map(|(_min, (_max_wert, größtes_kleiner))| größtes_kleiner);
    let kleinstes_größer = dreieck_winkel_ordnung_werte
        .iter()
        .copied()
        .filter(|(winkel_ordnung, _point)| *winkel_ordnung >= punkt_foo)
        .minmax_by_key(winkel_ordnung_key)
        .into_option()
        .map(|((_min_wert, kleinstes_größter), _max)| kleinstes_größter);
    let MinMaxResult::MinMax((_kleinster_wert, kleinstes), (_größter_wert, größtes)) =
        dreieck_winkel_ordnung_werte.into_iter().minmax_by_key(winkel_ordnung_key)
    else {
        unreachable!("Iterator hat 3 Elemente!")
    };
    let Vektor { x: x1, y: y1 } = größtes_kleiner.unwrap_or(größtes);
    let Vektor { x: x2, y: y2 } = kleinstes_größer.unwrap_or(kleinstes);
    let Vektor { x: xp, y: yp } = punkt;
    let Vektor { x: xs, y: ys } = schwerpunkt;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let sgn1 = (((yp - y2) * (x1 - x2)) - ((y1 - y2) * (xp - x2))).signum();
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let sgn2 = (((ys - y2) * (x1 - x2)) - ((y1 - y2) * (xs - x2))).signum();
    sgn1 == sgn2
}

/// Hilfs-Funktion für update: Reagiere auf einen Maus- oder Touch-Klick.
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
    let WichtigeWerte {
        links,
        rechts,
        oben,
        unten,
        zentrum,
        ende_links_oben,
        ende_links_unten,
        ende_rechts_oben,
        ende_rechts_unten,
        ende_oben_links,
        ende_oben_rechts,
        ende_unten_links,
        ende_unten_rechts,
        links_oben,
        links_unten,
        rechts_oben,
        rechts_unten,
        radius,
    } = WichtigeWerte::aus_size(size);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let klick_radius = (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
    let punkt = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
    if punkt_innerhalb_dreieck(punkt, links, ende_links_oben, ende_links_unten) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::Links))
    } else if punkt_innerhalb_dreieck(punkt, rechts, ende_rechts_oben, ende_rechts_unten) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::Rechts))
    } else if punkt_innerhalb_dreieck(punkt, oben, ende_oben_links, ende_oben_rechts) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::Oben))
    } else if punkt_innerhalb_dreieck(punkt, unten, ende_unten_links, ende_unten_rechts) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::Unten))
    } else if punkt_innerhalb_dreieck(punkt, links_oben, ende_links_oben, ende_oben_links) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::ObenLinks))
    } else if punkt_innerhalb_dreieck(punkt, links_unten, ende_links_unten, ende_unten_links) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::UntenLinks))
    } else if punkt_innerhalb_dreieck(punkt, rechts_oben, ende_rechts_oben, ende_oben_rechts) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::ObenRechts))
    } else if punkt_innerhalb_dreieck(punkt, rechts_unten, ende_rechts_unten, ende_unten_rechts) {
        *state = Some(klick_quelle);
        Some(Nachricht::StarteBewegung(Bewegung::UntenRechts))
    } else if klick_radius < radius {
        Some(Nachricht::Zurücksetzen)
    } else {
        // Kein aktives Element angeklickt.
        None
    }
}

impl Program<Nachricht, Thema, Renderer> for Bewegen {
    type State = Option<KlickQuelle>;

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        thema: &Thema,
        bounds: Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let size = bounds.size();
        let WichtigeWerte {
            links,
            rechts,
            oben,
            unten,
            zentrum,
            ende_links_oben,
            ende_links_unten,
            ende_rechts_oben,
            ende_rechts_unten,
            ende_oben_links,
            ende_oben_rechts,
            ende_unten_links,
            ende_unten_rechts,
            links_oben,
            links_unten,
            rechts_oben,
            rechts_unten,
            radius,
        } = WichtigeWerte::aus_size(size);

        // erzeuge Pfad
        let mut erbauer = pfad::Erbauer::neu();
        // links
        erbauer.move_to(ende_links_unten);
        erbauer.line_to(links);
        erbauer.line_to(ende_links_oben);
        // links-oben
        erbauer.line_to(links_oben);
        erbauer.line_to(ende_oben_links);
        // oben
        erbauer.line_to(oben);
        erbauer.line_to(ende_oben_rechts);
        // rechts-oben
        erbauer.line_to(rechts_oben);
        erbauer.line_to(ende_rechts_oben);
        // rechts
        erbauer.line_to(rechts);
        erbauer.line_to(ende_rechts_unten);
        // rechts-unten
        erbauer.line_to(rechts_unten);
        erbauer.line_to(ende_unten_rechts);
        // unten
        erbauer.line_to(unten);
        erbauer.line_to(ende_unten_links);
        // links-unten
        erbauer.line_to(links_unten);
        erbauer.close();

        // zurücksetzen
        erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU });

        let pfad = erbauer.baue();
        vec![self.0.zeichnen(renderer, thema, size, |frame| {
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
            let WichtigeWerte {
                links,
                rechts,
                oben,
                unten,
                zentrum,
                ende_links_oben,
                ende_links_unten,
                ende_rechts_oben,
                ende_rechts_unten,
                ende_oben_links,
                ende_oben_rechts,
                ende_unten_links,
                ende_unten_rechts,
                links_oben,
                links_unten,
                rechts_oben,
                rechts_unten,
                radius,
            } = WichtigeWerte::aus_size(size);
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            let klick_radius =
                (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
            let punkt = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
            if punkt_innerhalb_dreieck(punkt, links, ende_links_oben, ende_links_unten)
                || punkt_innerhalb_dreieck(punkt, rechts, ende_rechts_oben, ende_rechts_unten)
                || punkt_innerhalb_dreieck(punkt, oben, ende_oben_links, ende_oben_rechts)
                || punkt_innerhalb_dreieck(punkt, unten, ende_unten_links, ende_unten_rechts)
                || punkt_innerhalb_dreieck(punkt, links_oben, ende_links_oben, ende_oben_links)
                || punkt_innerhalb_dreieck(punkt, links_unten, ende_links_unten, ende_unten_links)
                || punkt_innerhalb_dreieck(punkt, rechts_oben, ende_rechts_oben, ende_oben_rechts)
                || punkt_innerhalb_dreieck(
                    punkt,
                    rechts_unten,
                    ende_rechts_unten,
                    ende_unten_rechts,
                )
                || (klick_radius < radius)
            {
                interaction = mouse::Interaction::Pointer;
            }
        }
        interaction
    }
}
