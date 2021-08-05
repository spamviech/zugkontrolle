//! Steuerungs-Struktur eines Gleises

use crate::{
    application::{
        gleis::{
            self,
            gleise::{id::GleisId, maps::Gleis, GleisEntferntError, Gleise},
        },
        typen::*,
    },
    steuerung,
};

/// Mutable Referenz auf die Steuerung eines Gleises.
/// Mit dem Drop-Handler wird ein Neuzeichen des Canvas (Cache) ausgelöst.
pub(in crate::application) struct Steuerung<'t, T> {
    steuerung: &'t mut Option<T>,
    canvas: &'t mut Cache,
    verändert: bool,
}
impl<'t, T> Drop for Steuerung<'t, T> {
    fn drop(&mut self) {
        if self.verändert {
            self.canvas.clear()
        }
    }
}
impl<'t, T> Steuerung<'t, T> {
    pub fn neu(steuerung: &'t mut Option<T>, canvas: &'t mut Cache) -> Self {
        Steuerung { steuerung, canvas, verändert: false }
    }
    /// Erhalte den Wert der zugehörigen Option-Referenz und hinterlasse None.
    pub fn take(&mut self) -> Option<T> {
        self.verändert = true;
        self.steuerung.take()
    }

    /// Füge einen Wert in die zugehörige Option-Referenz ein.
    /// Enthält diese bereits einen Wert wird dieser überschrieben.
    pub fn insert(&mut self, steuerung: T) -> &mut T {
        self.verändert = true;
        self.steuerung.insert(steuerung)
    }

    /// Erhalte eine Referenz, falls ein Wert vorhanden ist.
    pub fn as_ref(&self) -> Option<&T> {
        self.steuerung.as_ref()
    }

    /// Erhalte eine mutable Referenz, falls ein Wert vorhanden ist.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        self.verändert = true;
        self.steuerung.as_mut()
    }
}

macro_rules! steuerung_weiche {
    ($name:ident, $type:ty, $map:ident, $richtung:ty, $anschlüsse:ty) => {
        pub(in crate::application) fn $name<'t>(
            &'t mut self,
            gleis_id: &GleisId<$type>,
        ) -> Result<
            Steuerung<'t, steuerung::weiche::Weiche<$richtung, $anschlüsse>>,
            GleisEntferntError,
        > {
            let Gleise { maps, canvas, .. } = self;
            let Gleis { definition, .. } =
                maps.$map.get_mut(&gleis_id).ok_or(GleisEntferntError)?;
            Ok(Steuerung::neu(&mut definition.steuerung, canvas))
        }
    };
}

impl<Z: Zugtyp> Gleise<Z> {
    steuerung_weiche! {
        steuerung_weiche,
        gleis::Weiche<Z>,
        weichen,
        gleis::weiche::gerade::Richtung,
        gleis::weiche::gerade::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_dreiwege_weiche,
        gleis::DreiwegeWeiche<Z>,
        dreiwege_weichen,
        gleis::weiche::dreiwege::Richtung,
        gleis::weiche::dreiwege::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_kurven_weiche,
        gleis::KurvenWeiche<Z>,
        kurven_weichen,
        gleis::weiche::kurve::Richtung,
        gleis::weiche::kurve::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_s_kurven_weiche,
        gleis::SKurvenWeiche<Z>,
        s_kurven_weichen,
        gleis::weiche::gerade::Richtung,
        gleis::weiche::gerade::RichtungAnschlüsse
    }

    steuerung_weiche! {
        steuerung_kreuzung,
        gleis::Kreuzung<Z>,
        kreuzungen,
        gleis::weiche::gerade::Richtung,
        gleis::weiche::gerade::RichtungAnschlüsse
    }
}
