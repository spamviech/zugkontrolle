//! Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [Canvas](iced::widget::canvas::Canvas).

use std::time::Instant;

use crate::{
    gleis::{
        self,
        gerade::Gerade,
        gleise::id::{AnyId, AnyIdRef, GleisId},
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        kontakt::KontaktSerialisiert,
        plan::{AktionStreckenabschnitt, AnyAktionSchalten},
    },
    typen::{vektor::Vektor, winkel::Winkel},
};

pub(in crate::gleis::gleise) type IdUndSteuerungSerialisiert<T, S> = (GleisId<T>, S);

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type StWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type StDreiwegeWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type StKurvenWeicheSerialisiert = crate::steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

// FIXME sind die ganzen Typ-Aliase notwendig? Record-Felder wären vmtl. besser
/// [GleisId] und serialisierte Steuerung eines Gleises.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum GleisSteuerung {
    /// [GleisId] und [KontaktSerialisiert] einer [Gerade].
    Gerade(IdUndSteuerungSerialisiert<Gerade, Option<KontaktSerialisiert>>),
    /// [GleisId] und [KontaktSerialisiert] einer [Kurve].
    Kurve(IdUndSteuerungSerialisiert<Kurve, Option<KontaktSerialisiert>>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [Weiche].
    Weiche(IdUndSteuerungSerialisiert<Weiche, Option<StWeicheSerialisiert>>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [KurvenWeiche].
    KurvenWeiche(IdUndSteuerungSerialisiert<KurvenWeiche, Option<StKurvenWeicheSerialisiert>>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [DreiwegeWeiche].
    DreiwegeWeiche(
        IdUndSteuerungSerialisiert<DreiwegeWeiche, Option<StDreiwegeWeicheSerialisiert>>,
    ),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [SKurvenWeiche].
    SKurvenWeiche(IdUndSteuerungSerialisiert<SKurvenWeiche, Option<StWeicheSerialisiert>>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [Kreuzung].
    Kreuzung(IdUndSteuerungSerialisiert<Kreuzung, Option<StWeicheSerialisiert>>),
}

impl GleisSteuerung {
    pub(in crate::gleis::gleise) fn id(&self) -> AnyIdRef<'_> {
        match self {
            GleisSteuerung::Gerade((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Kurve((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Weiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::KurvenWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::DreiwegeWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::SKurvenWeiche((id, _steuerung)) => id.als_ref().into(),
            GleisSteuerung::Kreuzung((id, _steuerung)) => id.als_ref().into(),
        }
    }

    pub(crate) fn klonen(&self) -> GleisSteuerung {
        match self {
            GleisSteuerung::Gerade((id, steuerung)) => {
                GleisSteuerung::Gerade((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Kurve((id, steuerung)) => {
                GleisSteuerung::Kurve((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Weiche((id, steuerung)) => {
                GleisSteuerung::Weiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::KurvenWeiche((id, steuerung)) => {
                GleisSteuerung::KurvenWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::DreiwegeWeiche((id, steuerung)) => {
                GleisSteuerung::DreiwegeWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::SKurvenWeiche((id, steuerung)) => {
                GleisSteuerung::SKurvenWeiche((id.klonen(), steuerung.clone()))
            },
            GleisSteuerung::Kreuzung((id, steuerung)) => {
                GleisSteuerung::Kreuzung((id.klonen(), steuerung.clone()))
            },
        }
    }
}

macro_rules! mit_any_steuerung_id {
    ($gleis_steuerung: expr , $function: expr$(, $objekt:expr$(, $extra_arg:expr)*)?) => {
        match $gleis_steuerung {
            GleisSteuerung::Gerade((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Kurve((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Weiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::DreiwegeWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::KurvenWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::SKurvenWeiche((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
            GleisSteuerung::Kreuzung((gleis_id, _steuerung)) => {
                $function($($objekt,)? gleis_id $($(, $extra_arg)*)?)
            }
        }
    };
}
pub(crate) use mit_any_steuerung_id;

#[derive(Debug)]
pub(in crate::gleis::gleise) struct Gehalten {
    pub(in crate::gleis::gleise) gleis_steuerung: GleisSteuerung,
    pub(in crate::gleis::gleise) halte_position: Vektor,
    pub(in crate::gleis::gleise) winkel: Winkel,
    pub(in crate::gleis::gleise) bewegt: bool,
}

/// Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [Canvas](iced::widget::canvas::Canvas).
#[derive(zugkontrolle_macros::Debug)]
#[non_exhaustive]
pub enum Nachricht {
    /// Setze den Streckenabschnitt für ein Gleis.
    SetzeStreckenabschnitt(AnyId),
    /// Ein Gleis mit [Streckenabschnitt] ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Die Anschlüsse für ein Gleis sollen angepasst werden.
    AnschlüsseAnpassen(GleisSteuerung),
    /// Eine GUI-Nachricht für Änderungen des Zustandes.
    ///
    /// Notwendig, weil die [update](iced::widget::canvas::Program::update)-Methode keinen `&mut self`-Zugriff erlaubt
    /// und auf den Zustand auch von außerhalb der GUI-Funktionen zugegriffen werden soll
    /// ([State](iced::widget::canvas::Program::State) dadurch nicht möglich).
    ZustandAktualisieren(ZustandAktualisieren),
}

/// Eine GUI-Nachricht für Änderungen interner Attribute.
#[derive(Debug)]
pub struct ZustandAktualisieren(pub(in crate::gleis::gleise) ZustandAktualisierenEnum);

#[derive(Debug)]
pub(in crate::gleis::gleise) enum ZustandAktualisierenEnum {
    /// Aktualisiere die letzte bekannte Maus-Position.
    LetzteMausPosition(Vektor),
    /// Aktualisiere die Zeit des letzten Maus-Klicks.
    LetzterKlick(Instant),
    /// Aktualisiere die letzte bekannte Canvas-Größe.
    LetzteCanvasGröße(Vektor),
    /// Aktualisiere das aktuell gehaltene Gleis.
    GehaltenAktualisieren(Option<Gehalten>),
    /// Bewege ein Gleis an die neue Position.
    GehaltenBewegen(Vektor),
    /// Entferne ein Gleis.
    GleisEntfernen(AnyId),
}

impl From<ZustandAktualisierenEnum> for Nachricht {
    fn from(nachricht: ZustandAktualisierenEnum) -> Self {
        Nachricht::ZustandAktualisieren(ZustandAktualisieren(nachricht))
    }
}
