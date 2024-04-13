//! [Zustand](AuswahlZustand) und [`anzeige`](AuswahlZustand::view) des Auswahl-Fensters.

use std::fmt::Debug;

use iced::{Element, Renderer};

use zugkontrolle_anschluss::{de_serialisieren::Serialisiere, pcf8574::Lager};
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_gleis::id::AnyIdSteuerungSerialisiert;
use zugkontrolle_gleis::{
    gerade::Gerade,
    kreuzung::Kreuzung,
    kurve::Kurve,
    steuerung::{
        self, geschwindigkeit::GeschwindigkeitSerialisiert, kontakt::KontaktSerialisiert,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    weiche::{
        dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
    },
};
use zugkontrolle_gleise::Gleise;
use zugkontrolle_id::GleisId;

use crate::{
    geschwindigkeit::{self, LeiterAnzeige},
    lizenzen,
    lizenzen::Lizenzen,
    streckenabschnitt,
    style::{sammlung::Sammlung, thema::Thema},
    weiche,
};

use super::kontakt;

/// Die Id eines Gleises mit einem [`Kontakt`](crate::steuerung::kontakt::Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, zugkontrolle_macros::From)]
pub enum KontaktId {
    /// Die Id einer [`Geraden`](Gerade).
    Gerade(GleisId<Gerade>),
    /// Die Id einer [`Kurve`].
    Kurve(GleisId<Kurve>),
}

/// Die Id einer Weiche mit [`gleis::weiche::gerade::Richtung`].
#[derive(Debug, Clone, PartialEq, Eq, zugkontrolle_macros::From)]
pub enum WeichenId {
    /// Die Id einer [`Weiche`].
    Gerade(GleisId<Weiche>),
    /// Die Id einer [`SKurvenWeiche`].
    SKurve(GleisId<SKurvenWeiche>),
    /// Die Id einer [`Kreuzung`].
    Kreuzung(GleisId<Kreuzung>),
}

// Beinhaltet SKurveWeiche und Kreuzung (identische Richtungen)
#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// Serialisierte Steuerung für eine [`Weiche`], [`SKurvenWeiche`] oder [`Kreuzung`].
type WeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    zugkontrolle_gleis::weiche::gerade::Richtung,
    zugkontrolle_gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;
#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// Serialisierte Steuerung für eine [`DreiwegeWeiche`].
type DreiwegeWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    zugkontrolle_gleis::weiche::dreiwege::RichtungInformation,
    zugkontrolle_gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;
#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// Serialisierte Steuerung für eine [`KurvenWeiche`].
type KurvenWeicheSerialisiert = steuerung::weiche::WeicheSerialisiert<
    zugkontrolle_gleis::weiche::kurve::Richtung,
    zugkontrolle_gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

// Beheben benötigt Änderung des public API.
#[allow(clippy::module_name_repetitions)]
/// Zustand des Auswahl-Fensters.
#[derive(Debug, Clone, PartialEq)]
pub enum AuswahlZustand<S> {
    /// Hinzufügen/Verändern eines [`Streckenabschnittes`](steuerung::streckenabschnitt::Streckenabschnitt).
    Streckenabschnitt(
        Option<(
            steuerung::streckenabschnitt::Name,
            StreckenabschnittSerialisiert,
            Option<steuerung::geschwindigkeit::Name>,
        )>,
    ),
    /// Hinzufügen/Verändern einer [`Geschwindigkeit`](steuerung::geschwindigkeit::Geschwindigkeit).
    Geschwindigkeit(Option<(steuerung::geschwindigkeit::Name, GeschwindigkeitSerialisiert<S>)>),
    /// Hinzufügen/Verändern der Anschlüsse einer [`Geraden`](gleis::gerade::Gerade),
    /// oder [`Kurve`](gleis::kurve::Kurve).
    Kontakt(KontaktId, Option<KontaktSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [`Weiche`](gleis::weiche::gerade::Weiche),
    /// [`Kreuzung`](gleis::kreuzung::Kreuzung),
    /// oder [`SKurvenWeiche`](gleis::weiche::s_kurve::SKurvenWeiche).
    Weiche(WeichenId, Option<WeicheSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [`DreiwegeWeiche`].
    DreiwegeWeiche(GleisId<DreiwegeWeiche>, Option<DreiwegeWeicheSerialisiert>, bool),
    /// Hinzufügen/Verändern der Anschlüsse einer [`KurvenWeiche`].
    KurvenWeiche(GleisId<KurvenWeiche>, Option<KurvenWeicheSerialisiert>, bool),
    /// Anzeige der verwendeten Open-Source Lizenzen.
    ZeigeLizenzen,
}

impl<S> From<(AnyIdSteuerungSerialisiert, bool)> for AuswahlZustand<S> {
    fn from((wert, hat_steuerung): (AnyIdSteuerungSerialisiert, bool)) -> Self {
        use AnyIdSteuerungSerialisiert::{
            DreiwegeWeiche, Gerade, Kreuzung, Kurve, KurvenWeiche, SKurvenWeiche, Weiche,
        };
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

#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// `AuswahlNachricht` für die Steuerung einer [Weiche], [Kreuzung] und [`SKurvenWeiche`].
pub(crate) type WeicheNachricht = weiche::Nachricht<
    zugkontrolle_gleis::weiche::gerade::Richtung,
    zugkontrolle_gleis::weiche::gerade::RichtungAnschlüsseSerialisiert,
>;

#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// `AuswahlNachricht` für die Steuerung einer [`DreiwegeWeiche`].
pub(crate) type DreiwegeWeicheNachricht = weiche::Nachricht<
    zugkontrolle_gleis::weiche::dreiwege::RichtungInformation,
    zugkontrolle_gleis::weiche::dreiwege::RichtungAnschlüsseSerialisiert,
>;

#[allow(clippy::absolute_paths)] // Notwendig, da `weiche` bereits in scope ist.
/// `AuswahlNachricht` für die Steuerung einer [`KurvenWeiche`].
pub(crate) type KurvenWeicheNachricht = weiche::Nachricht<
    zugkontrolle_gleis::weiche::kurve::Richtung,
    zugkontrolle_gleis::weiche::kurve::RichtungAnschlüsseSerialisiert,
>;

impl<S> AuswahlZustand<S> {
    /// Anzeige des Auswahlfensters
    pub fn view<'t, L, Nachricht: 't, AktualisierenNachricht>(
        &self,
        gleise: &'t Gleise<L, AktualisierenNachricht>,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        i2c_settings: I2cSettings,
    ) -> Element<'t, Nachricht, Thema, Renderer>
    where
        L: LeiterAnzeige<'t, S, Thema, Renderer> + Serialisiere<S>,
        S: 'static + Clone + Default,
        Nachricht: From<streckenabschnitt::AuswahlNachricht>
            + From<geschwindigkeit::AuswahlNachricht<S>>
            + From<(kontakt::Nachricht, KontaktId)>
            + From<(WeicheNachricht, WeichenId)>
            + From<(DreiwegeWeicheNachricht, GleisId<DreiwegeWeiche>)>
            + From<(KurvenWeicheNachricht, GleisId<KurvenWeiche>)>
            + From<lizenzen::Nachricht>,
    {
        match self {
            AuswahlZustand::Streckenabschnitt(startwert) => Element::from(
                streckenabschnitt::Auswahl::neu(startwert, gleise, scrollable_style, i2c_settings),
            )
            .map(Nachricht::from),
            AuswahlZustand::Geschwindigkeit(startwert) => {
                let geschwindigkeiten =
                    gleise.aus_allen_geschwindigkeiten(|name, geschwindigkeit| {
                        (name.clone(), geschwindigkeit.serialisiere())
                    });
                Element::from(<L as LeiterAnzeige<S, Thema, Renderer>>::auswahl_neu(
                    startwert,
                    geschwindigkeiten,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(Nachricht::from)
            },
            AuswahlZustand::Kontakt(kontakt_id, kontakt, hat_steuerung) => {
                let gleis_art = match &kontakt_id {
                    KontaktId::Gerade(_id) => "Gerade",
                    KontaktId::Kurve(_id) => "Kurve",
                };
                let kontakt_id_clone = kontakt_id.clone();
                Element::from(kontakt::Auswahl::neu(
                    gleis_art,
                    kontakt,
                    *hat_steuerung,
                    lager,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| Nachricht::from((nachricht, kontakt_id_clone.clone())))
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
                    weiche,
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| Nachricht::from((nachricht, weichen_id_clone.clone())))
            },
            AuswahlZustand::DreiwegeWeiche(id, dreiwege_weiche, hat_steuerung) => {
                let id_clone = id.clone();
                Element::from(weiche::Auswahl::neu(
                    "Dreiwege-Weiche",
                    dreiwege_weiche,
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| Nachricht::from((nachricht, id_clone.clone())))
            },
            AuswahlZustand::KurvenWeiche(id, kurven_weiche, hat_steuerung) => {
                let id_clone = id.clone();
                Element::from(weiche::Auswahl::neu(
                    "Kurven-Weiche",
                    kurven_weiche,
                    *hat_steuerung,
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |nachricht| Nachricht::from((nachricht, id_clone.clone())))
            },
            AuswahlZustand::ZeigeLizenzen => {
                Element::from(Lizenzen::neu_mit_verwendeten_lizenzen(scrollable_style))
                    .map(Nachricht::from)
            },
        }
    }
}
