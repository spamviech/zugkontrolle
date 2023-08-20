//! [Zustand](AuswahlZustand) und [anzeige](AuswahlZustand::view) des Auswahl-Fensters.

use std::fmt::Debug;

use iced::{Element, Renderer};

use crate::{
    anschluss::de_serialisieren::Serialisiere,
    application::{
        geschwindigkeit::{self, LeiterAnzeige},
        lizenzen::{self, Lizenzen},
        modal::{self},
        nachricht::{Nachricht, WeichenId},
        streckenabschnitt,
        style::{sammlung::Sammlung, thema::Thema},
        weiche,
    },
    argumente::I2cSettings,
    gleis::{
        self,
        gleise::{id::GleisId, AnschlüsseAnpassen, Gleise},
        weiche::{dreiwege::DreiwegeWeiche, kurve::KurvenWeiche},
    },
    steuerung,
};

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
    /// Hinzufügen/Verändern eines [Streckenabschnittes](steuerung::Streckenabschnitt).
    Streckenabschnitt,
    /// Hinzufügen/Verändern einer [Geschwindigkeit](steuerung::Geschwindigkeit).
    Geschwindigkeit,
    /// Hinzufügen/Verändern der Anschlüsse einer [Weiche], [Kreuzung], oder [SKurvenWeiche].
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

impl AuswahlZustand {
    /// Anzeige des Auswahlfensters
    pub fn view<'t, L, S, MessageBox>(
        &self,
        gleise: &'t Gleise<L>,
        scrollable_style: Sammlung,
        i2c_settings: I2cSettings,
    ) -> Element<
        't,
        modal::Nachricht<AuswahlZustand, modal::Nachricht<MessageBox, Nachricht<L, S>>>,
        Renderer<Thema>,
    >
    where
        L: LeiterAnzeige<'t, S, Renderer<Thema>> + Serialisiere<S>,
        S: 't,
        MessageBox: 't,
    {
        match self {
            AuswahlZustand::Streckenabschnitt => Element::from(streckenabschnitt::Auswahl::neu(
                gleise,
                scrollable_style,
                i2c_settings,
            ))
            .map(|message| {
                use streckenabschnitt::AuswahlNachricht::*;
                match message {
                    Schließe => modal::Nachricht::VersteckeOverlay,
                    Wähle(wahl) => modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                        Nachricht::WähleStreckenabschnitt(wahl),
                    )),
                    Hinzufügen(geschwindigkeit, name, farbe, output) => {
                        modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                            Nachricht::HinzufügenStreckenabschnitt(
                                geschwindigkeit,
                                name,
                                farbe,
                                output,
                            ),
                        ))
                    },
                    Lösche(name) => modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                        Nachricht::LöscheStreckenabschnitt(name),
                    )),
                }
            }),
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
                .map(|message| {
                    use geschwindigkeit::AuswahlNachricht::*;
                    match message {
                        Schließen => modal::Nachricht::VersteckeOverlay,
                        Hinzufügen(name, geschwindigkeit) => {
                            modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                                Nachricht::HinzufügenGeschwindigkeit(name, geschwindigkeit),
                            ))
                        },
                        Löschen(name) => modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                            Nachricht::LöscheGeschwindigkeit(name),
                        )),
                    }
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
                .map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => {
                            modal::Nachricht::Underlay(modal::Nachricht::Underlay(
                                Nachricht::AnschlüsseAnpassen(match &weichen_id_clone {
                                    WeichenId::Gerade(id) => {
                                        AnschlüsseAnpassen::Weiche(id.klonen(), steuerung)
                                    },
                                    WeichenId::SKurve(id) => {
                                        AnschlüsseAnpassen::SKurvenWeiche(id.klonen(), steuerung)
                                    },
                                    WeichenId::Kreuzung(id) => {
                                        AnschlüsseAnpassen::Kreuzung(id.klonen(), steuerung)
                                    },
                                }),
                            ))
                        },
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
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
                .map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => modal::Nachricht::Underlay(
                            modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                                AnschlüsseAnpassen::DreiwegeWeiche(id_clone.klonen(), steuerung),
                            )),
                        ),
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
                })
            },
            AuswahlZustand::KurvenWeiche(kurven_weiche, id) => {
                let id_clone = id.klonen();
                Element::from(weiche::Auswahl::neu(
                    "Kurven-Weiche",
                    kurven_weiche.clone(),
                    scrollable_style,
                    i2c_settings,
                ))
                .map(move |message| {
                    use weiche::Nachricht::*;
                    match message {
                        Festlegen(steuerung) => modal::Nachricht::Underlay(
                            modal::Nachricht::Underlay(Nachricht::AnschlüsseAnpassen(
                                AnschlüsseAnpassen::KurvenWeiche(id_clone.klonen(), steuerung),
                            )),
                        ),
                        Schließen => modal::Nachricht::VersteckeOverlay,
                    }
                })
            },
            AuswahlZustand::ZeigeLizenzen => {
                Element::from(Lizenzen::neu_mit_verwendeten_lizenzen(scrollable_style))
                    .map(|lizenzen::Nachricht::Schließen| modal::Nachricht::VersteckeOverlay)
            },
        }
    }
}
