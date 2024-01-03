//! Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [Canvas](iced::widget::canvas::Canvas).

use std::time::Instant;

use crate::{
    gleis::{
        gleise::id::{AnyId, AnyIdSteuerung, AnyIdSteuerungSerialisiert},
        knopf::KlickQuelle,
    },
    steuerung::plan::{AktionStreckenabschnitt, AnyAktionSchalten},
    typen::{vektor::Vektor, winkel::Winkel},
};

#[derive(Debug)]
pub(in crate::gleis::gleise) struct Gehalten {
    pub(in crate::gleis::gleise) gleis_steuerung: AnyIdSteuerung,
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
    AnschlüsseAnpassen(AnyIdSteuerungSerialisiert),
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
    /// Aktualisiere die Zeit und Art des letzten Maus- oder Touch-Klicks.
    LetzterKlick(KlickQuelle, Instant),
    /// Aktualisiere die letzte bekannte Canvas-Größe.
    LetzteCanvasGröße(Vektor),
    /// Aktualisiere das aktuell von der [KlickQuelle] gehaltene Gleis.
    GehaltenAktualisieren(KlickQuelle, Option<Gehalten>),
    /// Bewege ein Gleis an die neue Position.
    GehaltenBewegen(KlickQuelle, Vektor),
    /// Entferne ein Gleis.
    GleisEntfernen(AnyId),
}

impl From<ZustandAktualisierenEnum> for Nachricht {
    fn from(nachricht: ZustandAktualisierenEnum) -> Self {
        Nachricht::ZustandAktualisieren(ZustandAktualisieren(nachricht))
    }
}
