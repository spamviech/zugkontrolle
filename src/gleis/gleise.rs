//! Verwalten und Anzeige der Gleis-Definitionen auf einem
//! [Canvas](crate::application::touch_canvas::Canvas).

use std::{
    collections::hash_map::Entry,
    convert::identity,
    fmt::Debug,
    iter,
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    time::Instant,
};

use iced::{
    canvas::{event, Cursor, Event, Geometry, Program},
    mouse, Rectangle,
};
use log::error;
use parking_lot::Mutex;
use rstar::RTreeObject;

pub use self::{
    daten::Gleis,
    id::{AnyId, GleisId, StreckenabschnittId},
};
use self::{
    daten::{AnyGleis, DatenAuswahl, GleiseDaten, SelectEnvelope, StreckenabschnittMap, Zustand},
    id::StreckenabschnittIdRef,
};
use crate::{
    anschluss,
    application::{
        empfänger::Empfänger,
        steuerung::{AsyncAktualisieren, MitSteuerung, Steuerung},
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        kontakt::Kontakt,
        plan::{self, AktionStreckenabschnitt, AnyAktionSchalten},
        streckenabschnitt::{self, Streckenabschnitt},
    },
    typen::{
        canvas::{Cache, Position},
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::Winkel,
    },
    zugtyp::{FalscherLeiter, Zugtyp},
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod update;

#[derive(Debug)]
pub(crate) struct Gehalten {
    /// Das Gleis.
    gleis: AnyGleis,
    /// Der Streckenabschnitt des Gleises.
    streckenabschnitt: Option<StreckenabschnittId>,
    /// Die Position der Maus relativ zum Gleis.
    halte_position: Vektor,
    /// Der ursprüngliche Winkel des Gleises.
    winkel: Winkel,
    /// Wurde das Gleis bewegt.
    bewegt: bool,
}

#[zugkontrolle_macros::make_enum(pub, Modus)]
/// Aktueller Modus von [Gleise].
#[derive(Debug)]
enum ModusDaten {
    /// Im Bauen-Modus können Gleise hinzugefügt, bewegt, angepasst und bewegt werden.
    Bauen {
        /// Das aktuell gehaltene Gleis.
        gehalten: Option<Gehalten>,
        /// Der Zeitpunkt des letzten Klicks.
        last: Instant,
    },
    /// Im Fahren-Modus werden die mit den Gleisen assoziierten Aktionen durchgeführt.
    Fahren,
}

impl ModusDaten {
    fn neu(modus: Modus) -> Self {
        match modus {
            Modus::Bauen => ModusDaten::Bauen { gehalten: None, last: Instant::now() },
            Modus::Fahren => ModusDaten::Fahren,
        }
    }
}

/// Verwalten und Anzeige aller Gleise.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub struct Gleise<L: Leiter> {
    canvas: Arc<Mutex<Cache>>,
    pivot: Position,
    skalieren: Skalar,
    zustand: Zustand<L>,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: ModusDaten,
    sender: Sender<AsyncAktualisieren>,
}

impl<L: Leiter> Gleise<L> {
    /// Erstelle ein neues, leeres [Gleise]-struct.
    pub fn neu(
        zugtyp: Zugtyp<L>,
        modus: Modus,
        pivot: Position,
        skalieren: Skalar,
    ) -> (Self, Empfänger<AsyncAktualisieren>) {
        let (sender, receiver) = channel();
        let gleise = Gleise {
            canvas: Arc::new(Mutex::new(Cache::neu())),
            pivot,
            skalieren,
            zustand: Zustand::neu(zugtyp),
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::neu(modus),
            sender,
        };
        (gleise, Empfänger::neu(receiver))
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match &self.modus {
            ModusDaten::Bauen { .. } => Modus::Bauen,
            ModusDaten::Fahren => Modus::Fahren,
        }
    }

    /// Wechsel den aktuellen Modus zu `modus`.
    pub fn moduswechsel(&mut self, modus: Modus) {
        self.modus = ModusDaten::neu(modus);
    }

    /// Aktuelle Pivot-Punkt und Dreh-Winkel
    pub fn pivot(&self) -> &Position {
        &self.pivot
    }

    /// Bewege aktuellen Pivot-Punkt nach `pivot`.
    pub fn setze_pivot(&mut self, pivot: Vektor) {
        self.pivot.punkt = pivot;
        self.canvas.lock().leeren();
    }

    /// Bewege aktuellen Pivot-Punkt um `bewegung`.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.lock().leeren();
    }

    /// Setze den `winkel` für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.lock().leeren();
    }

    /// Drehe die aktuelle Darstellung um `winkel`.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.lock().leeren();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Setze den aktueller Skalierfaktor zur Darstellung.
    pub fn setze_skalierfaktor(&mut self, skalieren: Skalar) {
        self.skalieren = skalieren;
        self.canvas.lock().leeren();
    }

    /// Multipliziere die aktuelle Darstellung mit `skalieren`.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.lock().leeren();
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    #[inline(always)]
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
    ) -> Result<(StreckenabschnittId, Option<Streckenabschnitt>), StreckenabschnittHinzufügenFehler>
    {
        self.streckenabschnitt_hinzufügen_aux(geschwindigkeit, name, streckenabschnitt)
    }

    /// Füge einen Streckenabschnitt mit angenommenen Fließend-Zustand hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    fn streckenabschnitt_hinzufügen_aux(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        mut streckenabschnitt: Streckenabschnitt,
    ) -> Result<(StreckenabschnittId, Option<Streckenabschnitt>), StreckenabschnittHinzufügenFehler>
    {
        let streckenabschnitt_map = match self.zustand.streckenabschnitt_map_mut(geschwindigkeit) {
            Ok(streckenabschnitt_map) => streckenabschnitt_map,
            Err(GeschwindigkeitEntferntFehler(name)) => {
                return Err(StreckenabschnittHinzufügenFehler::GeschwindigkeitEntfernt(
                    name,
                    streckenabschnitt,
                ))
            },
        };
        let entry = streckenabschnitt_map.entry(name.clone());
        let bisher = match entry {
            Entry::Occupied(mut occupied) => {
                let bisher = occupied.get_mut();
                std::mem::swap(&mut bisher.0, &mut streckenabschnitt);
                Some(streckenabschnitt)
            },
            Entry::Vacant(vacant) => {
                let _mut_ref = vacant.insert((streckenabschnitt, GleiseDaten::neu()));
                None
            },
        };
        Ok((StreckenabschnittId { geschwindigkeit: geschwindigkeit.cloned(), name }, bisher))
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s>(
        &'s self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<&'s Streckenabschnitt, StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map = self.zustand.streckenabschnitt_map(geschwindigkeit.as_ref())?;
        streckenabschnitt_map
            .get(name)
            .map(|(streckenabschnitt, _daten)| streckenabschnitt)
            .ok_or_else(|| {
                StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt.klonen())
            })
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<&'s mut Streckenabschnitt, StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        streckenabschnitt_map
            .get_mut(name)
            .map(|(streckenabschnitt, _daten)| streckenabschnitt)
            .ok_or_else(|| {
                StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt.klonen())
            })
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    pub fn streckenabschnitt_entfernen(
        &mut self,
        streckenabschnitt_id: StreckenabschnittId,
    ) -> Result<Option<Streckenabschnitt>, StreckenabschnittBearbeitenFehler> {
        let StreckenabschnittId { geschwindigkeit, name: _ } = &streckenabschnitt_id;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        self.canvas.lock().leeren();
        streckenabschnitt_entfernen(
            streckenabschnitt_map,
            streckenabschnitt_id,
            Some,
            |_streckenabschnitt_id| Ok(None),
        )
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn streckenabschnitte<'t>(
        &'t self,
    ) -> impl Iterator<Item = (StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)> {
        let iter_map = |(geschwindigkeit, streckenabschnitt_map): (_, &'t StreckenabschnittMap)| {
            streckenabschnitt_map.iter().map(move |(name, (streckenabschnitt, _daten))| {
                (StreckenabschnittIdRef { geschwindigkeit, name }, streckenabschnitt)
            })
        };
        iter::once((None, &self.zustand.ohne_geschwindigkeit))
            .chain(self.zustand.geschwindigkeiten.iter().map(
                |(geschwindigkeit_name, (_geschwindigkeit, streckenabschnitt_map))| {
                    (Some(geschwindigkeit_name), streckenabschnitt_map)
                },
            ))
            .flat_map(iter_map)
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
    ) -> Option<Geschwindigkeit<L>> {
        match self.zustand.geschwindigkeiten.entry(name) {
            Entry::Occupied(mut occupied) => {
                let bisher = std::mem::replace(&mut occupied.get_mut().0, geschwindigkeit);
                Some(bisher)
            },
            Entry::Vacant(vacant) => {
                let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
                None
            },
        }
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit<'s>(
        &'s self,
        name: &geschwindigkeit::Name,
    ) -> Option<&'s Geschwindigkeit<L>> {
        self.zustand
            .geschwindigkeiten
            .get(name)
            .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<&'s mut Geschwindigkeit<L>> {
        self.zustand
            .geschwindigkeiten
            .get_mut(name)
            .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Schlägt fehl, wenn noch assoziierten Streckenabschnitte vorhanden waren.
    pub fn geschwindigkeit_entfernen(
        &mut self,
        name: geschwindigkeit::Name,
    ) -> Result<Option<Geschwindigkeit<L>>, GeschwindigkeitEntfernenFehler> {
        if let Some((name_entry, (geschwindigkeit, streckenabschnitt_map))) =
            self.zustand.geschwindigkeiten.remove_entry(&name)
        {
            if streckenabschnitt_map.is_empty() {
                Ok(Some(geschwindigkeit))
            } else {
                let _ = self
                    .zustand
                    .geschwindigkeiten
                    .insert(name_entry, (geschwindigkeit, streckenabschnitt_map));
                Err(GeschwindigkeitEntfernenFehler::StreckenabschnitteNichtEntfernt(name))
            }
        } else {
            Ok(None)
        }
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub(crate) fn geschwindigkeit_mit_streckenabschnitten_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
        streckenabschnitt_map: StreckenabschnittMap,
    ) -> Option<(Geschwindigkeit<L>, StreckenabschnittMap)> {
        match self.zustand.geschwindigkeiten.entry(name) {
            Entry::Occupied(mut occupied) => {
                let bisher =
                    std::mem::replace(occupied.get_mut(), (geschwindigkeit, streckenabschnitt_map));
                Some(bisher)
            },
            Entry::Vacant(vacant) => {
                let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
                None
            },
        }
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Assoziierte Streckenabschnitte werden (mit allen Gleisen) ebenfalls entfernt.
    #[inline(always)]
    pub(crate) fn geschwindigkeit_mit_streckenabschnitten_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<(Geschwindigkeit<L>, StreckenabschnittMap)> {
        self.zustand.geschwindigkeiten.remove(name)
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn geschwindigkeiten(
        &self,
    ) -> impl Iterator<Item = (&geschwindigkeit::Name, &Geschwindigkeit<L>)> {
        self.zustand
            .geschwindigkeiten
            .iter()
            .map(|(name, (geschwindigkeit, _streckenabschnitt_map))| (name, geschwindigkeit))
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp(&self) -> &Zugtyp<L> {
        &self.zustand.zugtyp
    }

    /// Spurweite des verwendeten Zugtyps.
    pub fn spurweite(&self) -> Spurweite {
        self.zustand.zugtyp.spurweite
    }
}

impl<L: Debug + Leiter> Gleise<L> {
    /// Assoziiere einen Streckenabschnitt mit einer Geschwindigkeit.
    /// Existiert bei der neuen Geschwindigkeit ein Streckenabschnitt mit identischem Namen
    /// wird dieser überschrieben und zurückgegeben.
    /// Schlägt fehl, wenn die neue Geschwindigkeit identisch zur aktuellen ist.
    /// Schlägt fehl, wenn der Streckenabschnitt oder die neue Geschwindigkeit nicht gefunden wurde.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    pub fn streckenabschnitt_assoziiere_geschwindigkeit(
        &mut self,
        streckenabschnitt_id: &mut StreckenabschnittId,
        geschwindigkeit_neu: Option<&geschwindigkeit::Name>,
    ) -> Result<Option<Streckenabschnitt>, StreckenabschnittBearbeitenFehler> {
        if streckenabschnitt_id.geschwindigkeit.as_ref() == geschwindigkeit_neu {
            return Err(StreckenabschnittBearbeitenFehler::IdentischeGeschwindigkeit(
                geschwindigkeit_neu.cloned(),
            ));
        }
        let geschwindigkeit = streckenabschnitt_id.geschwindigkeit.clone();
        let name = streckenabschnitt_id.name.clone();
        let (geschwindigkeit_name_und_eintrag, streckenabschnitt) =
            if let Some(geschwindigkeit_name) = geschwindigkeit {
                match self.zustand.geschwindigkeiten.remove(&geschwindigkeit_name) {
                    Some((geschwindigkeit, mut streckenabschnitt_map)) => {
                        let streckenabschnitt = streckenabschnitt_entfernen(
                            &mut streckenabschnitt_map,
                            streckenabschnitt_id.klonen(),
                            identity,
                            |streckenabschnitt_id| {
                                Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
                                    streckenabschnitt_id,
                                ))
                            },
                        )?;
                        (
                            Some((geschwindigkeit_name, (geschwindigkeit, streckenabschnitt_map))),
                            streckenabschnitt,
                        )
                    },
                    None => {
                        return Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(
                            geschwindigkeit_name,
                        ))
                    },
                }
            } else {
                let streckenabschnitt = streckenabschnitt_entfernen(
                    &mut self.zustand.ohne_geschwindigkeit,
                    streckenabschnitt_id.klonen(),
                    identity,
                    |streckenabschnitt_id| {
                        Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
                            streckenabschnitt_id,
                        ))
                    },
                )?;
                (None, streckenabschnitt)
            };
        match self.streckenabschnitt_hinzufügen_aux(
            geschwindigkeit_neu,
            name.clone(),
            streckenabschnitt,
        ) {
            Ok((id_neu, bisher)) => {
                if let Some((geschwindigkeit_name, geschwindigkeit_eintrag)) =
                    geschwindigkeit_name_und_eintrag
                {
                    let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
                    if let Some(geschwindigkeit_eintrag) = self
                        .zustand
                        .geschwindigkeiten
                        .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
                    {
                        error!(
                            "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
                            geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
                        )
                    }
                }
                *streckenabschnitt_id = id_neu;
                self.canvas.lock().leeren();
                Ok(bisher)
            },
            Err(StreckenabschnittHinzufügenFehler::GeschwindigkeitEntfernt(
                geschwindigkeit_neu,
                streckenabschnitt,
            )) => {
                if let Some((geschwindigkeit_name, mut geschwindigkeit_eintrag)) =
                    geschwindigkeit_name_und_eintrag
                {
                    if let Some(streckenabschnitt_eintrag) = geschwindigkeit_eintrag
                        .1
                        .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
                    {
                        error!(
                            "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
                            StreckenabschnittIdRef {
                                geschwindigkeit: Some(&geschwindigkeit_name),
                                name: &name
                            },
                            streckenabschnitt_eintrag
                        )
                    }
                    let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
                    if let Some(geschwindigkeit_eintrag) = self
                        .zustand
                        .geschwindigkeiten
                        .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
                    {
                        error!(
                            "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
                            geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
                        )
                    }
                } else {
                    if let Some(streckenabschnitt_eintrag) = self
                        .zustand
                        .ohne_geschwindigkeit
                        .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
                    {
                        error!(
                            "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
                            StreckenabschnittId { geschwindigkeit: None, name },
                            streckenabschnitt_eintrag
                        )
                    }
                }
                Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(geschwindigkeit_neu))
            },
        }
    }
}

fn streckenabschnitt_entfernen<T>(
    streckenabschnitt_map: &mut StreckenabschnittMap,
    streckenabschnitt_id: StreckenabschnittId,
    gefunden: impl FnOnce(Streckenabschnitt) -> T,
    bereits_entfernt: impl FnOnce(StreckenabschnittId) -> Result<T, StreckenabschnittBearbeitenFehler>,
) -> Result<T, StreckenabschnittBearbeitenFehler> {
    let StreckenabschnittId { geschwindigkeit: _, name } = &streckenabschnitt_id;
    if let Some((name_entry, (streckenabschnitt, daten))) = streckenabschnitt_map.remove_entry(name)
    {
        if daten.ist_leer() {
            Ok(gefunden(streckenabschnitt))
        } else {
            let _ = streckenabschnitt_map.insert(name_entry, (streckenabschnitt, daten));
            Err(StreckenabschnittBearbeitenFehler::GleiseNichtEntfernt(streckenabschnitt_id))
        }
    } else {
        bereits_entfernt(streckenabschnitt_id)
    }
}

impl<L: Leiter> Gleise<L> {
    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn erhalte_steuerung<'t, T: 't + MitSteuerung<'t> + DatenAuswahl>(
        &'t self,
        gleis_id: &GleisId<T>,
    ) -> Result<Steuerung<&'t <T as MitSteuerung<'t>>::Steuerung, AsyncAktualisieren>, GleisIdFehler>
    {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, sender, .. } = self;
        let Gleis { definition, position: _ }: &Gleis<T> = &zustand
            .daten(streckenabschnitt)?
            .rstern()
            .locate_with_selection_function(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        Ok(definition.steuerung(Some(canvas.clone()), sender.clone()))
    }

    #[zugkontrolle_macros::erstelle_daten_methoden]
    /// Erhalte die [Steuerung] für das spezifizierte Gleis.
    pub(crate) fn erhalte_steuerung_mut<'t, T: 't + MitSteuerung<'t> + DatenAuswahl>(
        &'t mut self,
        gleis_id: &GleisId<T>,
    ) -> Result<
        Steuerung<&'t mut <T as MitSteuerung<'t>>::Steuerung, AsyncAktualisieren>,
        GleisIdFehler,
    > {
        let GleisId { rectangle, streckenabschnitt, phantom: _ } = gleis_id;
        let Gleise { zustand, canvas, sender, .. } = self;
        let Gleis { definition, position: _ }: &mut Gleis<T> = &mut zustand
            .daten_mut(streckenabschnitt)?
            .rstern_mut()
            .locate_with_selection_function_mut(SelectEnvelope(rectangle.envelope()))
            .next()
            .ok_or(GleisIdFehler::GleisEntfernt)?
            .data;
        Ok(definition.steuerung_mut(Some(canvas.clone()), sender.clone()))
    }
}

// TODO Id aus Nachricht entfernen?
// SetzeStreckenabschnitt(AnyGleis)
//      Entferne Gleis um es neu hinzuzufügen
//      Id auch in Gehalten durch AnyGleis ersetzen
// AnschlüsseAnpassen(Arc<Mutex<AnyGleis>>)
//      Muss in Arc<Mutex<_>> sein, da in Nachricht aus update keine Referenz aus self enthalten kann.
/// Eine GUI-Nachricht als Reaktion auf Interaktion mit dem
/// [Canvas](crate::application::touch_canvas::Canvas).
#[derive(zugkontrolle_macros::Debug)]
#[non_exhaustive]
pub enum Nachricht {
    /// Setze den Streckenabschnitt für das gehaltene Gleis.
    SetzeStreckenabschnittGehalten,
    /// Öffne das Fenster zum Anpassen eines Kontaktes ([Gerade], [Kurve]).
    KontaktAnpassen(Steuerung<Arc<Mutex<Option<Kontakt>>>, AsyncAktualisieren>),
    /// Öffne das Fenster zum Anpassen der Anschlüsse einer Weiche
    /// ([Weiche], [SKurvenWeiche], [Kreuzung]).
    GeradeWeicheAnpassen(Steuerung<Arc<Mutex<Option<plan::GeradeWeiche>>>, AsyncAktualisieren>),
    /// Öffne das Fenster zum Anpassen der Anschlüsse einer Weiche ([KurvenWeiche]).
    KurvenWeicheAnpassen(Steuerung<Arc<Mutex<Option<plan::KurvenWeiche>>>, AsyncAktualisieren>),
    /// Öffne das Fenster zum Anpassen der Anschlüsse einer Weiche ([KurvenWeiche]).
    DreiwegeWeicheAnpassen(Steuerung<Arc<Mutex<Option<plan::DreiwegeWeiche>>>, AsyncAktualisieren>),
    /// Ein Gleis mit [Streckenabschnitt] ohne spezielle Aktion
    /// wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    StreckenabschnittUmschalten(AktionStreckenabschnitt),
    /// Ein [Weiche] wurde im [Fahren](Modus::Fahren)-Modus angeklickt.
    WeicheSchalten(AnyAktionSchalten),
}

impl<L: Leiter> Program<Nachricht> for Gleise<L> {
    #[inline(always)]
    fn draw(&self, bounds: Rectangle, cursor: Cursor) -> Vec<Geometry> {
        self.draw(bounds, cursor)
    }

    #[inline(always)]
    fn update(
        &mut self,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        self.update(event, bounds, cursor)
    }

    fn mouse_interaction(&self, bounds: Rectangle, cursor: Cursor) -> mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { gehalten: Some(_gehalten), .. } if cursor.is_over(&bounds) => {
                mouse::Interaction::Pointer
            },
            _ => mouse::Interaction::default(),
        }
    }
}

/// Fehler, die bei Interaktion mit den [Gleisen](Gleise) auftreten können.
#[derive(Debug)]
pub enum Fehler {
    /// Ein IO-Fehler.
    IO(std::io::Error),
    /// Fehler beim Serialisieren (speichern) der Gleise.
    BincodeSerialisieren(bincode::Error),
    /// Ein Fehler bei Interaktion mit einem [Anschluss](anschluss::Anschluss).
    Anschluss(anschluss::Fehler),
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt,
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<FalscherLeiter> for Fehler {
    fn from(fehler: FalscherLeiter) -> Self {
        Fehler::Anschluss(fehler.into())
    }
}

impl From<std::io::Error> for Fehler {
    fn from(error: std::io::Error) -> Self {
        Fehler::IO(error)
    }
}

impl From<anschluss::Fehler> for Fehler {
    fn from(error: anschluss::Fehler) -> Self {
        Fehler::Anschluss(error)
    }
}

/// Fehler bei Interaktion mit einem [bestimmten Gleis](AnyId).
#[derive(Debug)]
pub enum GleisIdFehler {
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt,
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<GleisIdFehler> for Fehler {
    fn from(fehler: GleisIdFehler) -> Self {
        match fehler {
            GleisIdFehler::GleisEntfernt => Fehler::GleisEntfernt,
            GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            GleisIdFehler::GeschwindigkeitEntfernt(name) => Fehler::GeschwindigkeitEntfernt(name),
        }
    }
}

/// Das betroffene Gleis wurde entfernt.
#[derive(Debug, Clone, Copy)]
pub struct GleisEntferntFehler;

impl From<GleisEntferntFehler> for Fehler {
    fn from(GleisEntferntFehler: GleisEntferntFehler) -> Self {
        Fehler::GleisEntfernt
    }
}

impl From<GleisEntferntFehler> for GleisIdFehler {
    fn from(GleisEntferntFehler: GleisEntferntFehler) -> Self {
        GleisIdFehler::GleisEntfernt
    }
}

/// Fehler bei Interaktion mit einem [Streckenabschnitt]
#[derive(Debug)]
pub enum StreckenabschnittIdFehler {
    /// Der betroffene Streckenabschnitt wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<StreckenabschnittIdFehler> for Fehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
            StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                Fehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            StreckenabschnittIdFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                Fehler::GeschwindigkeitEntfernt(geschwindigkeit)
            },
        }
    }
}

impl From<StreckenabschnittIdFehler> for GleisIdFehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
            StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt) => {
                GleisIdFehler::StreckenabschnittEntfernt(streckenabschnitt)
            },
            StreckenabschnittIdFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                GleisIdFehler::GeschwindigkeitEntfernt(geschwindigkeit)
            },
        }
    }
}

/// Die betroffene [Geschwindigkeit] wurde entfernt.
#[derive(Debug)]
pub struct GeschwindigkeitEntferntFehler(pub geschwindigkeit::Name);

impl From<GeschwindigkeitEntferntFehler> for Fehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        Fehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

impl From<GeschwindigkeitEntferntFehler> for GleisIdFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        GleisIdFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittIdFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittIdFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

/// Fehler beim Hinzufügen eines [Streckenabschnittes](Streckenabschnitt).
#[derive(Debug)]
pub enum StreckenabschnittHinzufügenFehler {
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name, Streckenabschnitt),
}

/// Fehler beim Bearbeiten eines [Streckenabschnittes](Streckenabschnitt).
#[derive(Debug)]
pub enum StreckenabschnittBearbeitenFehler {
    /// Der betroffene [Streckenabschnitt] wurde entfernt.
    StreckenabschnittEntfernt(StreckenabschnittId),
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
    /// Es gibt noch mit dem [Streckenabschnitt] assoziierte Gleise.
    GleiseNichtEntfernt(StreckenabschnittId),
    /// Es wurde die selbe [Geschwindigkeit] gewählt.
    IdentischeGeschwindigkeit(Option<geschwindigkeit::Name>),
}

impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittBearbeitenFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

/// Ein Fehler beim Entfernen einer [Geschwindigkeit].
#[derive(Debug)]
pub enum GeschwindigkeitEntfernenFehler {
    /// Es gibt noch mit der [Geschwindigkeit] assoziierte [Streckenabschnitte](Streckenabschnitt).
    StreckenabschnitteNichtEntfernt(geschwindigkeit::Name),
}
