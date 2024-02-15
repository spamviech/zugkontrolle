//! Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [`Canvas`](iced::widget::canvas::Canvas).

use std::time::Instant;

use zugkontrolle_gleis::{
    id::{AnyId, AnyIdSteuerung, AnyIdSteuerungSerialisiert},
    steuerung::plan::{AktionStreckenabschnitt, AnyAktionSchalten},
};
use zugkontrolle_typen::{vektor::Vektor, winkel::Winkel};

use crate::gleise::knopf::KlickQuelle;

/// Ein aktuell gehaltenes Gleis.
#[derive(Debug)]
pub(in crate::gleise) struct Gehalten {
    /// Das [`GleisId`](crate::gleise::id::GleisId) und Steuerung des Gleises.
    pub(in crate::gleise) gleis_steuerung: AnyIdSteuerung,
    /// Die relative Position, wo das gleis Gehalten wird.
    pub(in crate::gleise) halte_position: Vektor,
    /// Der aktuelle Winkel des Gleises auf dem Canvas.
    pub(in crate::gleise) winkel: Winkel,
    /// Wurde das Gleis bewegt.
    pub(in crate::gleise) bewegt: bool,
}

/// Eine GUI-Nachricht als Reaktion auf Interaktion mit dem [`Canvas`](iced::widget::canvas::Canvas).
#[derive(zugkontrolle_macros::Debug)]
#[non_exhaustive]
pub enum Nachricht {
    /// Setze den Streckenabschnitt für ein Gleis.
    SetzeStreckenabschnitt(AnyId),
    /// Ein Gleis mit [`Streckenabschnitt`] ohne spezielle Aktion
    /// wurde im [`Fahren`](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [`Fahren`](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
    /// Die Anschlüsse für ein Gleis sollen angepasst werden.
    AnschlüsseAnpassen(AnyIdSteuerungSerialisiert),
    /// Eine GUI-Nachricht für Änderungen des Zustandes.
    ///
    /// Notwendig, weil die [`update`](iced::widget::canvas::Program::update)-Methode keinen `&mut self`-Zugriff erlaubt
    /// und auf den Zustand auch von außerhalb der GUI-Funktionen zugegriffen werden soll
    /// ([`State`](iced::widget::canvas::Program::State) dadurch nicht möglich).
    ZustandAktualisieren(ZustandAktualisieren),
}

/// Eine GUI-Nachricht für Änderungen interner Attribute.
#[derive(Debug)]
pub struct ZustandAktualisieren(pub(in crate::gleise) ZustandAktualisierenEnum);

/// Interne Nachricht, wie der [`Zustand`](crate::gleise::Zustand) aktualisiert werden soll.
#[derive(Debug)]
pub(in crate::gleise) enum ZustandAktualisierenEnum {
    /// Aktualisiere die letzte bekannte Maus-Position.
    LetzteMausPosition(Vektor),
    /// Aktualisiere die Zeit und Art des letzten Maus- oder Touch-Klicks.
    LetzterKlick(KlickQuelle, Instant),
    /// Aktualisiere die letzte bekannte Canvas-Größe.
    LetzteCanvasGröße(Vektor),
    /// Aktualisiere das aktuell von der [`KlickQuelle`] gehaltene Gleis.
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
