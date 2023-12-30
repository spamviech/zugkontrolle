//! [Zustand](AuswahlZustand) und [anzeige](AuswahlZustand::view) des Auswahl-Fensters.

use std::fmt::Debug;

use iced::{Element, Renderer};

use crate::{
    anschluss::{de_serialisieren::Serialisiere, pcf8574::Lager},
    application::{
        geschwindigkeit::{self, LeiterAnzeige},
        lizenzen,
        lizenzen::Lizenzen,
        modal, streckenabschnitt,
        style::{sammlung::Sammlung, thema::Thema},
        weiche,
    },
    argumente::I2cSettings,
    gleis::{
        self,
        gerade::Gerade,
        gleise::{
            id::{AnyIdSteuerungSerialisiert, GleisId},
            Gleise,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        self, geschwindigkeit::GeschwindigkeitSerialisiert, kontakt::KontaktSerialisiert,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
};

use super::kontakt;

/// Die Id eines Gleises mit einem [Kontakt](crate::steuerung::kontakt::Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, zugkontrolle_macros::From)]
pub enum KontaktId {
    /// Die Id einer [Geraden](Gerade).
    Gerade(GleisId<Gerade>),
    /// Die Id einer [Kurve].
    Kurve(GleisId<Kurve>),
}

/// Die Id einer Weiche mit [gleis::weiche::gerade::Richtung].
#[derive(Debug, Clone, PartialEq, Eq, zugkontrolle_macros::From)]
pub enum WeichenId {
    /// Die Id einer [Weiche].
    Gerade(GleisId<Weiche>),
    /// Die Id einer [SKurvenWeiche].
    SKurve(GleisId<SKurvenWeiche>),
    /// Die Id einer [Kreuzung].
    Kreuzung(GleisId<Kreuzung>),
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
type WeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

type DreiwegeWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

type KurvenWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

/// Zustand des Auswahl-Fensters.
#[derive(Debug, Clone, PartialEq)]
pub enum AuswahlZustand<S> {
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::streckenabschnitt::Streckenabschnitt).
    Streckenabschnitt(Option<(steuerung::streckenabschnitt::Name, StreckenabschnittSerialisiert)>),
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::geschwindigkeit::Geschwindigkeit).
    Geschwindigkeit(Option<(steuerung::geschwindigkeit::Name, GeschwindigkeitSerialisiert<S>)>),
    /// Hinzufügen/Verändern der Anschlüsse einer [Geraden](gleis::gerade::Gerade),
    /// oder [Kurve](gleis::kurve::Kurve).
    Kontakt(KontaktId, Option<KontaktSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche](gleis::weiche::gerade::Weiche),
    /// [Kreuzung](gleis::kreuzung::Kreuzung),
    /// oder [SKurvenWeiche](gleis::weiche::s_kurve::SKurvenWeiche).
    Weiche(WeichenId, Option<WeicheSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [DreiwegeWeiche].
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<DreiwegeWeicheSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [KurvenWeiche].
    KurvenWeiche(GleisId<KurvenWeiche>, Option<KurvenWeicheSerialisiert>, bool),
    /// Anzeige der verwendeten Open-Source Lizenzen.
    ZeigeLizenzen,
}

impl<S> From<(AnyIdSteuerungSerialisiert, bool)> for AuswahlZustand<S> {
    fn from((wert, hat_steuerung): (AnyIdSteuerungSerialisiert, bool)) -> Self {
        use AnyIdSteuerungSerialisiert::*;
        match wert {
            Gerade(id, steuerung) => {
                AuswahlZustand::Kontakt(KontaktId::from(id), steuerung, hat_steuerung)
            },
            Kurve(id, steuerung) => {
                AuswahlZustand::Kontakt(KontaktId::from(id), steuerung, hat_steuerung)
            },
            Weiche(id, steuerung) => {
                AuswahlZustand::Weiche(WeichenId::from(id), steuerung, hat_steuerung)
            },
            DreiwegeWeiche(id, steuerung) => {
                AuswahlZustand::DreiwegeWeiche(id, steuerung, hat_steuerung)
            },
            KurvenWeiche(id, steuerung) => {
                AuswahlZustand::KurvenWeiche(id, steuerung, hat_steuerung)
            },
            SKurvenWeiche(id, steuerung) => {
                AuswahlZustand::Weiche(WeichenId::from(id), steuerung, hat_steuerung)
            },
            Kreuzung(id, steuerung) => {
                AuswahlZustand::Weiche(WeichenId::from(id), steuerung, hat_steuerung)
            },
        }
    }
}

/// AuswahlNachricht für die Steuerung einer [Weiche], [Kreuzung] und [SKurvenWeiche].
pub(in crate::application) type WeicheNachricht = weiche::Nachricht<
    gleis::weiche::gerade::Richtung,
    gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

/// AuswahlNachricht für die Steuerung einer [DreiwegeWeiche].
pub(in crate::application) type DreiwegeWeicheNachricht = weiche::Nachricht<
    gleis::weiche::dreiwege::RichtungInformation,
    gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

/// AuswahlNachricht für die Steuerung einer [KurvenWeiche].
pub(in crate::application) type KurvenWeicheNachricht = weiche::Nachricht<
    gleis::weiche::kurve::Richtung,
    gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

impl<S> AuswahlZustand<S> {
    /// Anzeige des Auswahlfensters
    pub fn view<'t, L, Nachricht: 't, AktualisierenNachricht>(
        &self,
        gleise: &'t Gleise<L, AktualisierenNachricht>,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        i2c_settings: I2cSettings,
    ) -> Element<'t, modal::Nachricht<AuswahlZustand<S>, Nachricht>, Renderer<Thema>>
    where
        L: LeiterAnzeige<'t, S, Renderer<Thema>> + Serialisiere<S>,
        S: 't,
        modal::Nachricht<AuswahlZustand<S>, Nachricht>: From<streckenabschnitt::AuswahlNachricht>
            + From<geschwindigkeit::AuswahlNachricht<S>>
            + From<(kontakt::Nachricht, KontaktId)>
            + From<(WeicheNachricht, WeichenId)>
            + From<(DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)>
            + From<(KurvenWeicheNachricht, GleisId<KurvenWeiche>)>
            + From<lizenzen::Nachricht>,
    {
        match self {
            AuswahlZustand::Streckenabschnitt(startwert) => {
                Element::from(streckenabschnitt::Auswahl::neu(
                    startwert.clone(),
                    gleise,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(|nachricht| modal::Nachricht::from(nachricht))
            },
            AuswahlZustand::Geschwindigkeit(startwert) => {
                let geschwindigkeiten =
                    gleise.aus_allen_geschwindigkeiten(|name, geschwindigkeit| {
                        (name.clone(), geschwindigkeit.serialisiere())
                    });
                Element::from(<L as LeiterAnzeige<S, Renderer<Thema>>>::auswahl_neu(
                    geschwindigkeiten,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(|nachricht| modal::Nachricht::from(nachricht))
            },
            AuswahlZustand::Kontakt(kontakt_id, kontakt, hat_steuerung) => {
                let gleis_art = match &kontakt_id {
                    KontaktId::Gerade(_id) => "Gerade",
                    KontaktId::Kurve(_id) => "Kurve",
                };
                let kontakt_id_clone = kontakt_id.clone();
                Element::from(kontakt::Auswahl::neu(
                    gleis_art,
                    kontakt.clone(),
                    *hat_steuerung,
                    lager,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, kontakt_id_clone.clone())))
            },
            AuswahlZustand::Weiche(weichen_id, weiche, hat_steuerung) => {
                let weichen_art = match &weichen_id {
                    WeichenId::Gerade(_id) => "Weiche",
                    WeichenId::SKurve(_id) => "S-Kurven-Weiche",
                    WeichenId::Kreuzung(_id) => "Kreuzung",
                };
                let weichen_id_clone = weichen_id.clone();
                Element::from(weiche::Auswahl::neu(
                    weichen_art,
                    weiche.clone(),
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, weichen_id_clone.clone())))
            },
            AuswahlZustand::DreiwegeWeiche(id, dreiwege_weiche, hat_steuerung) => {
                let id_clone = id.clone();
                Element::from(weiche::Auswahl::neu(
                    "Dreiwege-Weiche",
                    dreiwege_weiche.clone(),
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, id_clone.clone())))
            },
            AuswahlZustand::KurvenWeiche(id, kurven_weiche, hat_steuerung) => {
                let id_clone = id.clone();
                Element::from(weiche::Auswahl::neu(
                    "Kurven-Weiche",
                    kurven_weiche.clone(),
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, id_clone.clone())))
            },
            AuswahlZustand::ZeigeLizenzen => {
                Element::from(Lizenzen::neu_mit_verwendeten_lizenzen(scrollable_style))
                    .map(|nachricht| modal::Nachricht::from(nachricht))
            },
        }
    }
}
