//! Serialisierte Strukturen von Version 6.X.

use std::{collections::HashMap, marker::PhantomData, time::Duration};

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::{
    gerade::GeradeUnit,
    kreuzung::KreuzungUnit,
    kurve::KurveUnit,
    steuerung::{
        geschwindigkeit::{GeschwindigkeitSerialisiert, LeiterSerialisiert},
        plan::{self as plan_v6, PlanSerialisiert},
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    weiche::{
        dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
        s_kurve::SKurvenWeicheUnit,
    },
};
use zugkontrolle_typen::mm::Spurweite;
use zugkontrolle_util::eingeschränkt::{NichtNegativ, NullBisEins};

use crate::daten::v4::{self, GleiseDatenSerialisiert, geschwindigkeit, plan};

/// Streckenabschnitte und ihre Namen.
pub(in crate::daten) type StreckenabschnittMapSerialisiert = HashMap<
    streckenabschnitt::Name,
    (StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>),
>;
/// Geschwindigkeiten und ihre Namen.
pub(in crate::daten) type GeschwindigkeitMapSerialisiert =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert>;

/// Die serialisierbare Darstellung des aktuellen Zustandes, wie er in Version 4 verwendet wird.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct ZustandSerialisiert {
    /// Der serialisierbare Zugtyp.
    pub zugtyp: ZugtypSerialisiert,
    /// Die serialisierbaren Geschwindigkeiten.
    pub geschwindigkeiten: GeschwindigkeitMapSerialisiert,
    /// Die serialisierbaren Streckenabschnitte.
    pub streckenabschnitte: StreckenabschnittMapSerialisiert,
    /// Die serialisierbaren Gleise.
    pub gleise: GleiseDatenSerialisiert,
    /// Die serialisierbaren Pläne.
    pub pläne: HashMap<plan::Name, PlanSerialisiert>,
}

impl<L, S> TryFrom<v4::ZustandSerialisiert<L, S>> for ZustandSerialisiert
where
    L: geschwindigkeit::Leiter,
    <L as geschwindigkeit::Leiter>::VerhältnisFahrspannungÜberspannung:
        TypeOrPhantomData<NullBisEins>,
    <L as geschwindigkeit::Leiter>::UmdrehenZeit: TypeOrPhantomData<Duration>,
    S: Into<LeiterSerialisiert>,
    plan::AktionGeschwindigkeitSerialisiert<L, S>: Into<plan_v6::AktionGeschwindigkeitSerialisiert>,
{
    type Error = plan::FehlendeWeichensteuerung;

    fn try_from(value: v4::ZustandSerialisiert<L, S>) -> Result<Self, Self::Error> {
        Ok(ZustandSerialisiert {
            zugtyp: value.zugtyp.into(),
            geschwindigkeiten: from_geschwindigkeiten(value.geschwindigkeiten),
            streckenabschnitte: value.streckenabschnitte,
            gleise: value.gleise,
            pläne: try_from_pläne(value.pläne)?,
        })
    }
}

fn from_geschwindigkeiten<S: Into<LeiterSerialisiert>>(
    geschwindigkeiten: HashMap<
        v4::geschwindigkeit::Name,
        v4::geschwindigkeit::GeschwindigkeitSerialisiert<S>,
    >,
) -> HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert> {
    geschwindigkeiten
        .into_iter()
        .map(|(name, geschwindigkeit)| (name, geschwindigkeit.into()))
        .collect()
}

fn try_from_pläne<L: geschwindigkeit::Leiter, S>(
    pläne: HashMap<plan::Name, plan::PlanSerialisiert<L, S>>,
) -> Result<HashMap<plan::Name, PlanSerialisiert>, plan::FehlendeWeichensteuerung>
where
    plan::AktionGeschwindigkeitSerialisiert<L, S>: Into<plan_v6::AktionGeschwindigkeitSerialisiert>,
{
    pläne
        .into_iter()
        .map(|(name, plan)| {
            let plan = plan.try_into().map_err(|mut fehler: plan::FehlendeWeichensteuerung| {
                fehler.plan = Some(name.clone());
                fehler
            })?;
            Ok((name, plan))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{plan, plan_v6};
    use std::time::Duration;
    use zugkontrolle_anschluss::polarität::Fließend;
    use zugkontrolle_gleis::{
        steuerung::{
            geschwindigkeit::{Mittelleiter, MittelleiterSerialisiert},
            weiche,
        },
        weiche::{dreiwege, gerade, kurve, orientierung::Orientierung},
    };
    use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

    #[test]
    fn aktionen_konvertieren() {
        let strom = plan::AktionStreckenabschnitt::Strom {
            streckenabschnitt: 7_u8,
            fließend: Fließend::Fließend,
        };
        let strom: plan_v6::AktionStreckenabschnitt<u16> = strom.into();
        assert!(matches!(
            strom,
            plan_v6::AktionStreckenabschnitt::Strom {
                streckenabschnitt: 7,
                fließend: Fließend::Fließend
            }
        ));

        let warten: plan::AktionWarten<u8> = plan::AktionWarten::WartenAuf { kontakt: 3 };
        let warten: plan_v6::AktionWarten<u16> = warten.into();
        assert!(matches!(warten, plan_v6::AktionWarten::WartenAuf { kontakt: 3 }));
        let warten: plan::AktionWarten<u8> =
            plan::AktionWarten::WartenFür { zeit: Duration::from_secs(2) };
        let warten: plan_v6::AktionWarten<u16> = warten.into();
        assert!(
            matches!(warten, plan_v6::AktionWarten::WartenFür { zeit } if zeit == Duration::from_secs(2))
        );
    }

    #[test]
    fn weichen_aktionen_mit_und_ohne_steuerung() {
        let gerade_steuerung = weiche::WeicheSerialisiert::neu(
            weiche::Name("Gerade".into()),
            gerade::Richtung::Gerade,
            gerade::RichtungAnschlüsseSerialisiert::default(),
        );
        let kurve_steuerung = weiche::WeicheSerialisiert::neu(
            weiche::Name("Kurve".into()),
            kurve::Richtung::Innen,
            kurve::RichtungAnschlüsseSerialisiert::default(),
        );
        let dreiwege_steuerung = weiche::WeicheSerialisiert::neu(
            weiche::Name("Dreiwege".into()),
            dreiwege::RichtungInformation::default(),
            dreiwege::RichtungAnschlüsseSerialisiert::default(),
        );
        let gerade = gerade::WeicheSerialisiert {
            länge: Skalar(1.),
            radius: Skalar(2.),
            winkel: Winkel(0.5),
            orientierung: Orientierung::Links,
            beschreibung: None,
            steuerung: Some(gerade_steuerung.clone()),
        };
        let kurve = kurve::KurvenWeicheSerialisiert {
            länge: Skalar(1.),
            radius: Skalar(2.),
            winkel: Winkel(0.5),
            orientierung: Orientierung::Rechts,
            beschreibung: None,
            steuerung: Some(kurve_steuerung.clone()),
        };
        let dreiwege = dreiwege::DreiwegeWeicheSerialisiert {
            länge: Skalar(1.),
            radius: Skalar(2.),
            winkel: Winkel(0.5),
            beschreibung: None,
            steuerung: Some(dreiwege_steuerung.clone()),
        };
        let aktionen = [
            plan::AnyAktionSchalten::SchalteGerade(plan_v6::AktionSchalten {
                weiche: gerade.clone(),
                richtung: gerade::Richtung::Kurve,
            }),
            plan::AnyAktionSchalten::SchalteKurve(plan_v6::AktionSchalten {
                weiche: kurve.clone(),
                richtung: kurve::Richtung::Außen,
            }),
            plan::AnyAktionSchalten::SchalteDreiwege(plan_v6::AktionSchalten {
                weiche: dreiwege.clone(),
                richtung: dreiwege::Richtung::Links,
            }),
        ];
        let innerer_plan = plan::PlanSerialisiert::<Mittelleiter, MittelleiterSerialisiert> {
            aktionen: aktionen.iter().cloned().map(plan::AktionEnum::Schalten).collect(),
            endlosschleife: false,
        };
        let äußerer_plan = plan::PlanSerialisiert::<Mittelleiter, MittelleiterSerialisiert> {
            aktionen: vec![plan::AktionEnum::Ausführen(innerer_plan)],
            endlosschleife: true,
        };
        let konvertiert = plan_v6::PlanSerialisiert::try_from(äußerer_plan).expect("gültiger Plan");
        assert!(konvertiert.endlosschleife);
        assert!(
            matches!(konvertiert.aktionen.as_slice(), [plan_v6::AktionEnum::Ausführen(plan)] if !plan.endlosschleife && plan.aktionen.len() == 3)
        );

        let [gerade_aktion, kurve_aktion, dreiwege_aktion] = aktionen;
        assert!(
            matches!(plan_v6::AnyAktionSchaltenSerialisiert::try_from(gerade_aktion), Ok(plan_v6::AnyAktionSchalten::SchalteGerade(aktion)) if aktion.weiche == gerade_steuerung && aktion.richtung == gerade::Richtung::Kurve)
        );
        assert!(
            matches!(plan_v6::AnyAktionSchaltenSerialisiert::try_from(kurve_aktion), Ok(plan_v6::AnyAktionSchalten::SchalteKurve(aktion)) if aktion.weiche == kurve_steuerung && aktion.richtung == kurve::Richtung::Außen)
        );
        assert!(
            matches!(plan_v6::AnyAktionSchaltenSerialisiert::try_from(dreiwege_aktion), Ok(plan_v6::AnyAktionSchalten::SchalteDreiwege(aktion)) if aktion.weiche == dreiwege_steuerung && aktion.richtung == dreiwege::Richtung::Links)
        );

        let fehlende = [
            (
                "Gerade",
                plan::AnyAktionSchalten::SchalteGerade(plan_v6::AktionSchalten {
                    weiche: gerade::WeicheSerialisiert { steuerung: None, ..gerade },
                    richtung: gerade::Richtung::Gerade,
                }),
            ),
            (
                "Kurve",
                plan::AnyAktionSchalten::SchalteKurve(plan_v6::AktionSchalten {
                    weiche: kurve::KurvenWeicheSerialisiert { steuerung: None, ..kurve },
                    richtung: kurve::Richtung::Innen,
                }),
            ),
            (
                "Dreiwege",
                plan::AnyAktionSchalten::SchalteDreiwege(plan_v6::AktionSchalten {
                    weiche: dreiwege::DreiwegeWeicheSerialisiert { steuerung: None, ..dreiwege },
                    richtung: dreiwege::Richtung::Gerade,
                }),
            ),
        ];
        for (weichenart, aktion) in fehlende {
            let ergebnis = plan_v6::AnyAktionSchaltenSerialisiert::try_from(aktion.clone());
            assert!(
                matches!(ergebnis, Err(fehler) if fehler.weichenart == weichenart && fehler.plan.is_none())
            );
            let innerer_plan = plan::PlanSerialisiert::<Mittelleiter, MittelleiterSerialisiert> {
                aktionen: vec![plan::AktionEnum::Schalten(aktion)],
                endlosschleife: false,
            };
            let äußerer_plan = plan::PlanSerialisiert::<Mittelleiter, MittelleiterSerialisiert> {
                aktionen: vec![plan::AktionEnum::Ausführen(innerer_plan)],
                endlosschleife: true,
            };
            let name = plan::Name("Fahrplan".into());
            let ergebnis =
                super::try_from_pläne([(name.clone(), äußerer_plan)].into_iter().collect());
            assert!(
                matches!(ergebnis, Err(fehler) if fehler.weichenart == weichenart && fehler.plan == Some(name))
            );
        }
    }
}

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
pub struct ZugtypSerialisiert {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
    pub leiter: String,
    /// [`Spurweite`].
    pub spurweite: Spurweite,
    /// Alle unterstützten [`Geraden`](crate::gleis::gerade::Gerade).
    pub geraden: HashMap<u32, GeradeUnit>,
    /// Alle unterstützten [`Kurven`](crate::gleis::kurve::Kurve).
    pub kurven: HashMap<u32, KurveUnit>,
    /// Alle unterstützten [`Weichen`](crate::gleis::weiche::gerade::Weiche).
    pub weichen: HashMap<u32, WeicheUnit>,
    /// Alle unterstützten [`Dreiwege-Weichen`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: HashMap<u32, DreiwegeWeicheUnit>,
    /// Alle unterstützten [`Kurven-Weichen`](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: HashMap<u32, KurvenWeicheUnit>,
    /// Alle unterstützten [`S-Kurven-Weichen`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: HashMap<u32, SKurvenWeicheUnit>,
    /// Alle unterstützten [`Kreuzungen`](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: HashMap<u32, KreuzungUnit>,
    /// Frequenz in Herz für den Pwm-Antrieb.
    pub pwm_frequenz: NichtNegativ,
    /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
    pub verhältnis_fahrspannung_überspannung: NullBisEins,
    /// Zeit zum Anhalten vor dem Umdrehen.
    pub stopp_zeit: Duration,
    /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
    pub umdrehen_zeit: Duration,
    /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
    pub schalten_zeit: Duration,
}

/// Helper-Trait to write generic code for a type or [`PhantomData`] of the same type.
trait TypeOrPhantomData<T> {
    /// Return [`Some`] value or [`None`] when receiving [`PhantomData`].
    fn into_option(self) -> Option<T>;
}

impl<T> TypeOrPhantomData<T> for PhantomData<T> {
    fn into_option(self) -> Option<T> {
        None
    }
}

impl<T> TypeOrPhantomData<T> for T {
    fn into_option(self) -> Option<T> {
        Some(self)
    }
}

impl<L> From<v4::ZugtypSerialisiert<L>> for ZugtypSerialisiert
where
    L: geschwindigkeit::Leiter,
    <L as geschwindigkeit::Leiter>::VerhältnisFahrspannungÜberspannung:
        TypeOrPhantomData<NullBisEins>,
    <L as geschwindigkeit::Leiter>::UmdrehenZeit: TypeOrPhantomData<Duration>,
{
    fn from(value: v4::ZugtypSerialisiert<L>) -> Self {
        ZugtypSerialisiert {
            name: value.name,
            leiter: value.leiter,
            spurweite: value.spurweite,
            geraden: value.geraden,
            kurven: value.kurven,
            weichen: value.weichen,
            dreiwege_weichen: value.dreiwege_weichen,
            kurven_weichen: value.kurven_weichen,
            s_kurven_weichen: value.s_kurven_weichen,
            kreuzungen: value.kreuzungen,
            pwm_frequenz: value.pwm_frequenz,
            verhältnis_fahrspannung_überspannung: value
                .verhältnis_fahrspannung_überspannung
                .into_option()
                .unwrap_or(NullBisEins::neu_unchecked(0.)),
            stopp_zeit: value.stopp_zeit,
            umdrehen_zeit: value.umdrehen_zeit.into_option().unwrap_or(Duration::from_secs(0)),
            schalten_zeit: value.schalten_zeit,
        }
    }
}
