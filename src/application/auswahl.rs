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
        gleise::{id::GleisId, Gleise},
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{self, kontakt::KontaktSerialisiert},
};

use super::kontakt;

/// Die Id eines Gleises mit einem [Kontakt](crate::steuerung::kontakt::Kontakt).
#[derive(Debug, PartialEq)]
pub enum KontaktId {
    /// Die Id einer [Geraden](Gerade).
    Gerade(GleisId<Gerade>),
    /// Die Id einer [Kurve].
    Kurve(GleisId<Kurve>),
}

impl KontaktId {
    pub(in crate::application) fn klonen(&self) -> Self {
        match self {
            KontaktId::Gerade(id) => KontaktId::Gerade(id.klonen()),
            KontaktId::Kurve(id) => KontaktId::Kurve(id.klonen()),
        }
    }
}

/// Die Id einer Weiche mit [gleis::weiche::gerade::Richtung].
#[derive(Debug, PartialEq)]
pub enum WeichenId {
    /// Die Id einer [Weiche].
    Gerade(GleisId<Weiche>),
    /// Die Id einer [SKurvenWeiche].
    SKurve(GleisId<SKurvenWeiche>),
    /// Die Id einer [Kreuzung].
    Kreuzung(GleisId<Kreuzung>),
}

impl WeichenId {
    pub(in crate::application) fn klonen(&self) -> Self {
        match self {
            WeichenId::Gerade(id) => WeichenId::Gerade(id.klonen()),
            WeichenId::SKurve(id) => WeichenId::SKurve(id.klonen()),
            WeichenId::Kreuzung(id) => WeichenId::Kreuzung(id.klonen()),
        }
    }
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
#[derive(Debug, PartialEq)]
pub enum AuswahlZustand {
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::streckenabschnitt::Streckenabschnitt).
    Streckenabschnitt,
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::geschwindigkeit::Geschwindigkeit).
    Geschwindigkeit,
    /// Hinzufügen/Verändern der Anschlüsse einer [Geraden](gleis::gerade::Gerade),
    /// oder [Kurve](gleis::kurve::Kurve).
    Kontakt(Option<KontaktSerialisiert>, KontaktId),
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche](gleis::weiche::gerade::Weiche),
    /// [Kreuzung](gleis::kreuzung::Kreuzung),
    /// oder [SKurvenWeiche](gleis::weiche::s_kurve::SKurvenWeiche).
    Weiche(Option<WeicheSerialisiert>, WeichenId),
    /// Hinzufügen/Verändern der Anschlüsse einer [DreiwegeWeiche].
    DreiwegeWeiche(Option<DreiwegeWeicheSerialisiert>, GleisId<DreiwegeWeiche>),
    /// Hinzufügen/Verändern der Anschlüsse einer [KurvenWeiche].
    KurvenWeiche(Option<KurvenWeicheSerialisiert>, GleisId<KurvenWeiche>),
    /// Anzeige der verwendeten Open-Source Lizenzen.
    ZeigeLizenzen,
}

impl Clone for AuswahlZustand {
    fn clone(&self) -> Self {
        match self {
            AuswahlZustand::Streckenabschnitt => AuswahlZustand::Streckenabschnitt,
            AuswahlZustand::Geschwindigkeit => AuswahlZustand::Geschwindigkeit,
            AuswahlZustand::Kontakt(startwert, id) => {
                AuswahlZustand::Kontakt(startwert.clone(), id.klonen())
            },
            AuswahlZustand::Weiche(startwert, id) => {
                AuswahlZustand::Weiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::DreiwegeWeiche(startwert, id) => {
                AuswahlZustand::DreiwegeWeiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::KurvenWeiche(startwert, id) => {
                AuswahlZustand::KurvenWeiche(startwert.clone(), id.klonen())
            },
            AuswahlZustand::ZeigeLizenzen => AuswahlZustand::ZeigeLizenzen,
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

impl AuswahlZustand {
    /// Anzeige des Auswahlfensters
    pub fn view<'t, L, S, Nachricht: 't, AktualisierenNachricht>(
        &self,
        gleise: &'t Gleise<L, AktualisierenNachricht>,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        i2c_settings: I2cSettings,
    ) -> Element<'t, modal::Nachricht<AuswahlZustand, Nachricht>, Renderer<Thema>>
    where
        L: LeiterAnzeige<'t, S, Renderer<Thema>> + Serialisiere<S>,
        S: 't,
        modal::Nachricht<AuswahlZustand, Nachricht>: From<streckenabschnitt::AuswahlNachricht>
            + From<geschwindigkeit::AuswahlNachricht<S>>
            + From<(kontakt::Nachricht, KontaktId)>
            + From<(WeicheNachricht, WeichenId)>
            + From<(DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)>
            + From<(KurvenWeicheNachricht, GleisId<KurvenWeiche>)>
            + From<lizenzen::Nachricht>,
    {
        match self {
            AuswahlZustand::Streckenabschnitt => Element::from(streckenabschnitt::Auswahl::neu(
                gleise,
                scrollable_style,
                i2c_settings,
            ))
            .map(|nachricht| modal::Nachricht::from(nachricht)),
            AuswahlZustand::Geschwindigkeit => {
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
            AuswahlZustand::Kontakt(kontakt, kontakt_id) => {
                let gleis_art = match &kontakt_id {
                    KontaktId::Gerade(_id) => "Gerade",
                    KontaktId::Kurve(_id) => "Kurve",
                };
                let kontakt_id_clone = kontakt_id.klonen();
                Element::from(kontakt::Auswahl::neu(
                    gleis_art,
                    kontakt.clone(),
                    lager,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| {
                    modal::Nachricht::from((nachricht, kontakt_id_clone.klonen()))
                })
            },
            AuswahlZustand::Weiche(weiche, weichen_id) => {
                let weichen_art = match &weichen_id {
                    WeichenId::Gerade(_id) => "Weiche",
                    WeichenId::SKurve(_id) => "S-Kurven-Weiche",
                    WeichenId::Kreuzung(_id) => "Kreuzung",
                };
                let weichen_id_clone = weichen_id.klonen();
                Element::from(weiche::Auswahl::neu(
                    weichen_art,
                    weiche.clone(),
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| {
                    modal::Nachricht::from((nachricht, weichen_id_clone.klonen()))
                })
            },
            AuswahlZustand::DreiwegeWeiche(dreiwege_weiche, id) => {
                let id_clone = id.klonen();
                Element::from(weiche::Auswahl::neu(
                    "Dreiwege-Weiche",
                    dreiwege_weiche.clone(),
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, id_clone.klonen())))
            },
            AuswahlZustand::KurvenWeiche(kurven_weiche, id) => {
                let id_clone = id.klonen();
                Element::from(weiche::Auswahl::neu(
                    "Kurven-Weiche",
                    kurven_weiche.clone(),
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| modal::Nachricht::from((nachricht, id_clone.klonen())))
            },
            AuswahlZustand::ZeigeLizenzen => {
                Element::from(Lizenzen::neu_mit_verwendeten_lizenzen(scrollable_style))
                    .map(|nachricht| modal::Nachricht::from(nachricht))
            },
        }
    }
}
