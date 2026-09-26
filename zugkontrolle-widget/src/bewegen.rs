//! Widget zum Anpassen des Pivot Punktes.

use iced::{
    Point, Rectangle, Renderer, Size,
    mouse::{self, Cursor},
    touch,
    widget::canvas::{Event, Geometry, Program, Stroke, Style},
};
use iced_widget::{
    Action,
    canvas::{Fill, fill::Rule},
};
use itertools::{Itertools, MinMaxResult};

use zugkontrolle_gleise::knopf;
use zugkontrolle_typen::{
    canvas::{
        Cache,
        pfad::{self, Bogen, Pfad},
    },
    klick_quelle::KlickQuelle,
    skalar::Skalar,
    vektor::Vektor,
    winkel,
};

use crate::style::thema::Thema;

/// Mögliche Bewegungen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
        #[expect(
            clippy::arithmetic_side_effects,
            reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
        )]
        let gradmaß = bogenmaß * winkel::PI;
        Vektor::polar_koordinaten(länge, gradmaß)
    }
}

/// Nachricht des [`Bewegen`]-Widgets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[allow(unfulfilled_lint_expectations, reason = "clippy::missing_docs_in_private_items")]
#[expect(clippy::missing_docs_in_private_items, reason = "Namen sind aussagekräftig genug.")]
struct WichtigeWerte {
    links: Vektor,
    rechts: Vektor,
    oben: Vektor,
    unten: Vektor,
    zentrum: Vektor,
    ende_links_oben: Vektor,
    ende_links_unten: Vektor,
    ende_rechts_oben: Vektor,
    ende_rechts_unten: Vektor,
    ende_oben_links: Vektor,
    ende_oben_rechts: Vektor,
    ende_unten_links: Vektor,
    ende_unten_rechts: Vektor,
    links_oben: Vektor,
    links_unten: Vektor,
    rechts_oben: Vektor,
    rechts_unten: Vektor,
    /// Der Radius für den Zurücksetzen-Kreis.
    radius: Skalar,
}

impl WichtigeWerte {
    /// Erzeuge alle [`WichtigenPunkte`] innerhalb der gegebenen Bounds.
    #[must_use]
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    fn aus_size(size: Size) -> Self {
        let padding_x = Skalar(0.05 * size.width);
        let padding_y = Skalar(0.05 * size.height);
        let width = Skalar(size.width);
        let height = Skalar(size.height);
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        // Startpunkte
        let links = Vektor { x: padding_x, y: half_height };
        let rechts = Vektor { x: width - padding_x, y: half_height };
        let oben = Vektor { x: half_width, y: padding_y };
        let unten = Vektor { x: half_width, y: height - padding_y };
        let zentrum = Vektor { x: half_width, y: half_height };
        // relative Bewegung
        let diagonale_länge = (links - oben).länge();
        let bein_länge = diagonale_länge / Skalar(3.);
        let diagonal_runter =
            bein_länge * Vektor { x: half_width, y: half_height }.einheitsvektor();
        let diagonal_hoch = Vektor { x: diagonal_runter.x, y: -diagonal_runter.y };
        // Zielpunkte
        let ende_links_oben = links + diagonal_hoch;
        let ende_links_unten = links + diagonal_runter;
        let ende_rechts_oben = rechts - diagonal_runter;
        let ende_rechts_unten = rechts - diagonal_hoch;
        let ende_oben_links = oben - diagonal_hoch;
        let ende_oben_rechts = oben + diagonal_runter;
        let ende_unten_links = unten - diagonal_runter;
        let ende_unten_rechts = unten + diagonal_hoch;

        // Diagonale Start-Werte
        let abstand_diagonale = Skalar(
            ((bein_länge.0.powf(2.)) - ((0.5 - (1. / 3.)) * diagonale_länge.0).powf(2.)).sqrt(),
        );
        let links_nach_oben = oben - links;
        let links_oben = links
            + Skalar(0.5) * links_nach_oben
            + abstand_diagonale * links_nach_oben.rotiert(&(-winkel::FRAC_PI_2)).einheitsvektor();
        let links_nach_unten = unten - links;
        let links_unten = links
            + Skalar(0.5) * links_nach_unten
            + abstand_diagonale * links_nach_unten.rotiert(&winkel::FRAC_PI_2).einheitsvektor();
        let rechts_nach_oben = oben - rechts;
        let rechts_oben = rechts
            + Skalar(0.5) * rechts_nach_oben
            + abstand_diagonale * rechts_nach_oben.rotiert(&winkel::FRAC_PI_2).einheitsvektor();
        let rechts_nach_unten = unten - rechts;
        let rechts_unten = rechts
            + Skalar(0.5) * rechts_nach_unten
            + abstand_diagonale * rechts_nach_unten.rotiert(&(-winkel::FRAC_PI_2)).einheitsvektor();

        // Zurücksetzen
        // Inkreis-Radius r = 2A/u
        // https://de.wikipedia.org/wiki/Inkreis
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

#[expect(clippy::min_ident_chars, reason = "Gibt es bessere Namen für die Ecken eines Dreiecks?")]
/// Liegt der `punkt` innerhalb des Dreiecks `a`-`b`-`c`.
///
/// <https://prlbr.de/2014/liegt-der-punkt-im-dreieck/>
/// Ansatz 4
fn punkt_innerhalb_dreieck(punkt: Vektor, a: Vektor, b: Vektor, c: Vektor) -> bool {
    /// Ordnung identisch zum Winkel zwischen `vektor` und positiver x-Achse.
    fn winkel_ordnung(vektor: Vektor) -> Skalar {
        let Vektor { x, y } = vektor;
        let faktor = if y >= Skalar(0.) { Skalar(1.) } else { Skalar(-1.) };
        #[expect(
            clippy::arithmetic_side_effects,
            reason = "Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt."
        )]
        {
            faktor * (Skalar(1.) - (x / (x.abs() + y.abs())))
        }
    }
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let schwerpunkt = (a + b + c) / Skalar(3.);
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let punkt_rel = punkt - schwerpunkt;
    let punkt_foo = winkel_ordnung(punkt_rel);
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let a_rel = a - schwerpunkt;
    let a_foo = winkel_ordnung(a_rel);
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let b_rel = b - schwerpunkt;
    let b_foo = winkel_ordnung(b_rel);
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
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
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let sgn1 = (((yp - y2) * (x1 - x2)) - ((y1 - y2) * (xp - x2))).signum();
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let sgn2 = (((ys - y2) * (x1 - x2)) - ((y1 - y2) * (xs - x2))).signum();
    sgn1 == sgn2
}

/// Hilfs-Funktion für update und view: Nachricht wenn an aktueller Position geklickt würde.
fn nachricht_an_position(bounds: Rectangle, position: Point) -> Option<Nachricht> {
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
    #[expect(
        clippy::arithmetic_side_effects,
        reason = "Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen."
    )]
    let klick_radius = (Vektor { x: Skalar(position.x), y: Skalar(position.y) } - zentrum).länge();
    let punkt = Vektor { x: Skalar(position.x), y: Skalar(position.y) };
    if punkt_innerhalb_dreieck(punkt, links, ende_links_oben, ende_links_unten) {
        Some(Nachricht::StarteBewegung(Bewegung::Links))
    } else if punkt_innerhalb_dreieck(punkt, rechts, ende_rechts_oben, ende_rechts_unten) {
        Some(Nachricht::StarteBewegung(Bewegung::Rechts))
    } else if punkt_innerhalb_dreieck(punkt, oben, ende_oben_links, ende_oben_rechts) {
        Some(Nachricht::StarteBewegung(Bewegung::Oben))
    } else if punkt_innerhalb_dreieck(punkt, unten, ende_unten_links, ende_unten_rechts) {
        Some(Nachricht::StarteBewegung(Bewegung::Unten))
    } else if punkt_innerhalb_dreieck(punkt, links_oben, ende_links_oben, ende_oben_links) {
        Some(Nachricht::StarteBewegung(Bewegung::ObenLinks))
    } else if punkt_innerhalb_dreieck(punkt, links_unten, ende_links_unten, ende_unten_links) {
        Some(Nachricht::StarteBewegung(Bewegung::UntenLinks))
    } else if punkt_innerhalb_dreieck(punkt, rechts_oben, ende_rechts_oben, ende_oben_rechts) {
        Some(Nachricht::StarteBewegung(Bewegung::ObenRechts))
    } else if punkt_innerhalb_dreieck(punkt, rechts_unten, ende_rechts_unten, ende_unten_rechts) {
        Some(Nachricht::StarteBewegung(Bewegung::UntenRechts))
    } else if klick_radius < radius {
        Some(Nachricht::Zurücksetzen)
    } else {
        // Kein aktives Element angeklickt.
        None
    }
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
    let nachricht = nachricht_an_position(bounds, position);
    if nachricht.is_some() {
        *state = Some(klick_quelle);
    }
    nachricht
}

/// Erzeuge den Pfad für ein Dreieck.
#[expect(clippy::min_ident_chars, reason = "Gibt es bessere Namen für die Ecken eines Dreiecks?")]
fn dreieck(a: Vektor, b: Vektor, c: Vektor) -> Pfad {
    pfad::Erbauer::neu().move_to_chain(a).line_to_chain(b).line_to_chain(c).baue()
}

/// Aktueller Zustand eines [`Bewegen`] Widgets.
#[derive(Debug, Clone, Copy, Default)]
pub struct Zustand {
    /// Wie wurde die letzte Aktion ausgelöst.
    klick_quelle: Option<KlickQuelle>,
    /// Die Nachricht für einen Klick auf die aktuelle [`Cursor`]-Position.
    /// Wird verwendet um ein Neuzeichnen des Canvas auszulösen.
    maus_nachricht: Option<Nachricht>,
}

impl Program<Nachricht, Thema, Renderer> for Bewegen {
    type State = Zustand;

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer,
        thema: &Thema,
        bounds: Rectangle,
        cursor: Cursor,
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

        let nachricht =
            cursor.position_in(bounds).and_then(|position| nachricht_an_position(bounds, position));
        let mut füll_pfad = None;
        // links
        let links = dreieck(ende_links_unten, links, ende_links_oben);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::Links)) {
            füll_pfad = Some(links.clone());
        }
        // links-oben
        let oben_links = dreieck(ende_links_oben, links_oben, ende_oben_links);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::ObenLinks)) {
            füll_pfad = Some(oben_links.clone());
        }
        // oben
        let oben = dreieck(ende_oben_links, oben, ende_oben_rechts);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::Oben)) {
            füll_pfad = Some(oben.clone());
        }
        // rechts-oben
        let oben_rechts = dreieck(ende_oben_rechts, rechts_oben, ende_rechts_oben);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::ObenRechts)) {
            füll_pfad = Some(oben_rechts.clone());
        }
        // rechts
        let rechts = dreieck(ende_rechts_oben, rechts, ende_rechts_unten);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::Rechts)) {
            füll_pfad = Some(rechts.clone());
        }
        // rechts-unten
        let unten_rechts = dreieck(ende_rechts_unten, rechts_unten, ende_unten_rechts);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::UntenRechts)) {
            füll_pfad = Some(unten_rechts.clone());
        }
        // unten
        let unten = dreieck(ende_unten_rechts, unten, ende_unten_links);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::Unten)) {
            füll_pfad = Some(unten.clone());
        }
        // links-unten
        let unten_links = dreieck(ende_unten_links, links_unten, ende_links_unten);
        if nachricht == Some(Nachricht::StarteBewegung(Bewegung::UntenLinks)) {
            füll_pfad = Some(unten_links.clone());
        }
        // zurücksetzen
        let zurücksetzten = pfad::Erbauer::neu()
            .arc_chain(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU })
            .baue();
        if nachricht == Some(Nachricht::Zurücksetzen) {
            füll_pfad = Some(zurücksetzten.clone());
        }

        let pfade = [
            links,
            oben_links,
            oben,
            oben_rechts,
            rechts,
            unten_rechts,
            unten,
            unten_links,
            zurücksetzten,
        ];
        let strich = <Thema as knopf::Catalog>::strich(thema);
        let von_maus_gehalten = state.klick_quelle == Some(KlickQuelle::Maus);
        let füllen = <Thema as knopf::Catalog>::hintergrund(thema, von_maus_gehalten, true);
        vec![self.0.zeichnen(renderer, thema, size, |frame| {
            for pfad in &pfade {
                frame.stroke(
                    pfad,
                    Stroke { style: Style::Solid(strich.into()), ..Stroke::default() },
                );
            }
            if let Some(füll_pfad) = &füll_pfad {
                frame.fill(
                    füll_pfad,
                    Fill { style: Style::Solid(füllen.into()), rule: Rule::NonZero },
                );
            }
        })]
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: &Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Option<Action<Nachricht>> {
        let Zustand { klick_quelle, maus_nachricht } = state;
        let mut nachricht = None;
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    nachricht = pressed(klick_quelle, bounds, position, KlickQuelle::Maus);
                }
            },
            Event::Touch(touch::Event::FingerPressed { id, position }) => {
                nachricht = pressed(klick_quelle, bounds, *position, KlickQuelle::Touch(*id));
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
                if *klick_quelle == Some(KlickQuelle::Maus) =>
            {
                // Beende nur mit der Maus gestartete Bewegungen
                *klick_quelle = None;
                nachricht = Some(Nachricht::BeendeBewegung);
            },
            Event::Touch(
                touch::Event::FingerLifted { id, position: _ }
                | touch::Event::FingerLost { id, position: _ },
            ) if *klick_quelle == Some(KlickQuelle::Touch(*id)) => {
                // Beende nur mit dem selben Finger gestartete Bewegungen
                *klick_quelle = None;
                nachricht = Some(Nachricht::BeendeBewegung);
            },
            Event::Mouse(_)
            | Event::Touch(_)
            | Event::Keyboard(_)
            | Event::Window(_)
            | Event::InputMethod(_) => {},
        }
        let aktuelle_maus_nachricht = cursor
            .position_in(bounds)
            .and_then(|maus_position| nachricht_an_position(bounds, maus_position));
        if nachricht.is_some() || (maus_nachricht != &aktuelle_maus_nachricht) {
            self.0.leeren();
        }
        *maus_nachricht = aktuelle_maus_nachricht;

        nachricht.map(Action::publish)
    }

    fn mouse_interaction(
        &self,
        _state: &Self::State,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> mouse::Interaction {
        if let Some(position) = cursor.position_in(bounds)
            && nachricht_an_position(bounds, position).is_some()
        {
            mouse::Interaction::Pointer
        } else {
            mouse::Interaction::default()
        }
    }
}
