//! Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [Canvas](iced::widget::canvas::Canvas).

use std::time::Instant;

use crate::{
    gleis::{
        self,
        gerade::Gerade,
        gleise::id::{AnyId2, AnyIdSteuerung2, AnyIdSteuerungSerialisiert2, GleisId},
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

/// [GleisId] und serialisierte Steuerung eines Gleises.
#[derive(Debug, zugkontrolle_macros::From)]
pub(crate) enum GleisSteuerung {
    /// [GleisId] und [KontaktSerialisiert] einer [Gerade].
    Gerade(GleisId<Gerade>, Option<KontaktSerialisiert>),
    /// [GleisId] und [KontaktSerialisiert] einer [Kurve].
    Kurve(GleisId<Kurve>, Option<KontaktSerialisiert>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [Weiche].
    Weiche(GleisId<Weiche>, Option<StWeicheSerialisiert>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [KurvenWeiche].
    KurvenWeiche(GleisId<KurvenWeiche>, Option<StKurvenWeicheSerialisiert>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [DreiwegeWeiche].
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<StDreiwegeWeicheSerialisiert>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [SKurvenWeiche].
    SKurvenWeiche(GleisId<SKurvenWeiche>, Option<StWeicheSerialisiert>),
    /// [GleisId] und [WeicheSerialisiert](crate::steuerung::weiche::WeicheSerialisiert) einer [Kreuzung].
    Kreuzung(GleisId<Kreuzung>, Option<StWeicheSerialisiert>),
}

#[derive(Debug)]
pub(in crate::gleis::gleise) struct Gehalten {
    pub(in crate::gleis::gleise) gleis_steuerung: AnyIdSteuerung2,
    pub(in crate::gleis::gleise) halte_position: Vektor,
    pub(in crate::gleis::gleise) winkel: Winkel,
    pub(in crate::gleis::gleise) bewegt: bool,
}

/// Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [Canvas](iced::widget::canvas::Canvas).
#[derive(zugkontrolle_macros::Debug)]
#[non_exhaustive]
pub enum Nachricht {
    /// Setze den Streckenabschnitt für ein Gleis.
    SetzeStreckenabschnitt(AnyId2),
    /// Ein Gleis mit [Streckenabschnitt] ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Die Anschlüsse für ein Gleis sollen angepasst werden.
    AnschlüsseAnpassen(AnyIdSteuerungSerialisiert2),
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
    GleisEntfernen(AnyId2),
}

impl From<ZustandAktualisierenEnum> for Nachricht {
    fn from(nachricht: ZustandAktualisierenEnum) -> Self {
        Nachricht::ZustandAktualisieren(ZustandAktualisieren(nachricht))
    }
}
