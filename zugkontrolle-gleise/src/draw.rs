//! [draw](iced::widget::canvas::Program::draw)-Methode für [`Gleise`].

use std::collections::HashSet;

use iced::{
    mouse::Cursor,
    widget::canvas::{Geometry, Program},
    Renderer,
};
use nonempty::NonEmpty;

use zugkontrolle_anschluss::polarität::Fließend;
use zugkontrolle_gleis::{
    id::AnyId,
    steuerung::{aktualisieren::Aktualisieren, geschwindigkeit::Leiter},
};
use zugkontrolle_typen::{
    canvas::{pfad::Transformation, Frame, Position},
    Transparenz,
};

use crate::{
    knopf,
    nachricht::{Gehalten, Nachricht},
    Gleise, ModusDaten,
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
    /// [draw](iced::widget::canvas::Program::draw)-Methode für [`Gleise`].
    pub(crate) fn draw_impl<Thema>(
        &self,
        _state: &<Self as Program<NonEmpty<Nachricht>, Thema, Renderer>>::State,
        renderer: &Renderer,
        thema: &Thema,
        bounds: iced::Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry>
    where
        AktualisierenNachricht: 'static + From<Aktualisieren>,
        Thema: Clone + Into<u8> + PartialEq + knopf::Thema,
        u8: TryInto<Thema>,
        Gleise<L, AktualisierenNachricht>: Program<NonEmpty<Nachricht>, Thema, Renderer>,
    {
        let Gleise { canvas, modus, .. } = self;
        vec![canvas.zeichnen_skaliert_von_pivot(
            renderer,
            thema,
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
                    thema,
                );
            },
        )]
    }
}
