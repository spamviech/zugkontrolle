//! [draw](iced::widget::canvas::Program::draw)-Methode für [`Gleise`].

use std::collections::HashSet;

use iced::{
    mouse::Cursor,
    widget::canvas::{Geometry, Program},
    Renderer,
};
use nonempty::NonEmpty;

use zugkontrolle_anschluss::polarität::Fließend;
use zugkontrolle_typen::{
    canvas::{pfad::Transformation, Frame, Position},
    Transparenz,
};

use crate::{
    application::style::thema::Thema,
    gleis::gleise::{
        self,
        id::AnyId,
        nachricht::{Gehalten, Nachricht},
        Gleise, ModusDaten,
    },
    steuerung::geschwindigkeit::Leiter,
};

/// Führe die notwendigen [`Transformationen`](Transformation) aus,
/// damit folgende Aktionen relativ zur `position` ausgeführt werden.
pub(crate) fn bewege_an_position(frame: &mut Frame<'_>, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    // TODO Behandeln erfordert Anpassen des public API
    #[allow(clippy::same_name_method)]
    /// [draw](iced::widget::canvas::Program::draw)-Methode für [`Gleise`].
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
        let Gleise { canvas, modus, .. } = self;
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
            self.skalieren,
            |frame| {
                // Zeichne Gleise
                let gehalten_ids: HashSet<AnyId>;
                let modus_bauen: bool;
                match modus {
                    ModusDaten::Bauen { gehalten, .. } => {
                        gehalten_ids = gehalten
                            .iter()
                            .map(|(_klick_quelle, Gehalten { gleis_steuerung, .. })| {
                                gleis_steuerung.id()
                            })
                            .collect();
                        modus_bauen = true;
                    },
                    ModusDaten::Fahren => {
                        gehalten_ids = HashSet::new();
                        modus_bauen = false;
                    },
                };
                let ist_gehalten = |id| gehalten_ids.contains(&id);
                let transparent_hintergrund = |id, fließend| {
                    Transparenz::true_reduziert(if modus_bauen {
                        ist_gehalten(id)
                    } else {
                        fließend == Fließend::Gesperrt
                    })
                };

                self.zustand.darstellen_aller_gleise(
                    frame,
                    transparent_hintergrund,
                    ist_gehalten,
                    thema.strich(),
                    self.skalieren,
                );
            },
        )]
    }
}
