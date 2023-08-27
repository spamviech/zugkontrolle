//! [draw](iced::widget::canvas::Program::draw)-Methode für [Gleise].

use std::marker::PhantomData;

use iced::{
    mouse::Cursor,
    widget::canvas::{
        fill::{self, Fill},
        stroke::{self, Stroke},
        Geometry, Program, Text,
    },
    Color, Renderer,
};
use nonempty::NonEmpty;
use rstar::primitives::Rectangle;

use crate::{
    anschluss::polarität::Fließend,
    application::{fonts::standard_text, style::thema::Thema},
    gleis::{
        gerade::Gerade,
        gleise::{
            self,
            daten::{Gleis, RStern},
            id::{AnyId2, AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
            steuerung::MitSteuerung,
            Gehalten, Gleise, ModusDaten, Nachricht,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        verbindung::Verbindung,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::geschwindigkeit::Leiter,
    typen::{
        canvas::{
            pfad::{self, Transformation},
            Frame, Position,
        },
        farbe::Farbe,
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        Transparenz, Zeichnen,
    },
    util::nachschlagen::Nachschlagen,
};

pub(crate) fn bewege_an_position(frame: &mut Frame<'_>, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// [draw](iced::widget::canvas::Program::draw)-Methode für [Gleise].
    pub fn draw(
        &self,
        _state: &<Self as Program<NonEmpty<Nachricht>, Renderer<Thema>>>::State,
        renderer: &Renderer<Thema>,
        thema: &Thema,
        bounds: iced::Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry>
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let spurweite = self.spurweite();
        let Gleise { canvas, zustand, modus, .. } = self;
        // TODO zeichne keine out-of-bounds Gleise (`locate_in_envelope_intersecting`)
        // bounds müssen an Position angepasst werden:
        // - ignoriere screen-position (verwende nur height+width, i.e. size)
        // - berücksichtige eigene Position (Punkt + Winkel)
        // - berücksichtige Zoom
        // keine Priorität, in den meisten Fällen dürften alle Gleise angezeigt werden
        vec![canvas.zeichnen_skaliert_von_pivot(
            renderer,
            bounds.size(),
            &self.pivot,
            &self.skalieren,
            |frame| {
                // Zeichne Gleise
                let gehalten_id: Option<AnyId2>;
                let modus_bauen: bool;
                match modus {
                    ModusDaten::Bauen {
                        gehalten: Some(Gehalten { gleis_steuerung, .. }), ..
                    } => {
                        gehalten_id = todo!();
                        // gehalten_id = Some(gleis_steuerung.id());
                        modus_bauen = true;
                    },
                    ModusDaten::Bauen { gehalten: None, .. } => {
                        gehalten_id = None;
                        modus_bauen = true;
                    },
                    ModusDaten::Fahren => {
                        gehalten_id = None;
                        modus_bauen = false;
                    },
                };
                let ist_gehalten = |id| Some(id) == gehalten_id;
                let transparent_hintergrund = |id, fließend| {
                    Transparenz::true_reduziert(if modus_bauen {
                        ist_gehalten(id)
                    } else {
                        fließend == Fließend::Gesperrt
                    })
                };

                self.zustand2.darstellen_aller_gleise(
                    frame,
                    transparent_hintergrund,
                    ist_gehalten,
                    thema.strich(),
                    self.skalieren,
                )
            },
        )]
    }
}
