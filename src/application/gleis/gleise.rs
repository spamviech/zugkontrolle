//! Anzeige der GleisDefinition auf einem Canvas

use std::{collections::hash_map::Entry, convert::identity, fmt::Debug, iter, time::Instant};

use log::error;

pub use self::{
    daten::{Gleis, Zustand},
    id::{AnyId, GleisId, StreckenabschnittId},
};
use self::{
    daten::{GleiseDaten, StreckenabschnittMap},
    id::StreckenabschnittIdRef,
};
use crate::{
    anschluss::{self, Fließend},
    application::typen::*,
    steuerung::{geschwindigkeit, streckenabschnitt, Geschwindigkeit, Streckenabschnitt},
    zugtyp::Zugtyp,
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod steuerung;
pub mod update;

#[derive(Debug)]
struct Gehalten {
    gleis_id: AnyId,
    halte_position: Vektor,
    winkel: Winkel,
    bewegt: bool,
}

// Aktueller Modus von `Gleise`
#[zugkontrolle_derive::make_enum(pub, Modus)]
#[derive(Debug)]
enum ModusDaten {
    Bauen { gehalten: Option<Gehalten>, last: Instant },
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

/// Anzeige aller Gleise.
#[derive(Debug)]
pub struct Gleise<Leiter> {
    canvas: Cache,
    pivot: Position,
    skalieren: Skalar,
    zustand: Zustand<Leiter>,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: ModusDaten,
}

impl<Leiter> Gleise<Leiter> {
    pub fn neu(zugtyp: Zugtyp<Leiter>, modus: Modus, pivot: Position, skalieren: Skalar) -> Self {
        Gleise {
            canvas: canvas::Cache::neu(),
            pivot,
            skalieren,
            zustand: Zustand::neu(zugtyp),
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::neu(modus),
        }
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
        self.canvas.leeren();
    }

    /// Bewege aktuellen Pivot-Punkt um `bewegung`.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.leeren();
    }

    /// Setze den `winkel` für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.leeren();
    }

    /// Drehe die aktuelle Darstellung um `winkel`.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.leeren();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Setze den aktueller Skalierfaktor zur Darstellung.
    pub fn setze_skalierfaktor(&mut self, skalieren: Skalar) {
        self.skalieren = skalieren;
        self.canvas.leeren();
    }

    /// Multipliziere die aktuelle Darstellung mit `skalieren`.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.leeren();
    }

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    #[inline(always)]
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
    ) -> Result<
        (StreckenabschnittId, Option<(Streckenabschnitt, Fließend)>),
        StreckenabschnittHinzufügenFehler,
    > {
        self.streckenabschnitt_hinzufügen_aux(
            geschwindigkeit,
            name,
            streckenabschnitt,
            Fließend::Gesperrt,
        )
    }

    /// Füge einen Streckenabschnitt mit angenommenen Fließend-Zustand hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    fn streckenabschnitt_hinzufügen_aux(
        &mut self,
        geschwindigkeit: Option<&geschwindigkeit::Name>,
        name: streckenabschnitt::Name,
        mut streckenabschnitt: Streckenabschnitt,
        fließend: Fließend,
    ) -> Result<
        (StreckenabschnittId, Option<(Streckenabschnitt, Fließend)>),
        StreckenabschnittHinzufügenFehler,
    > {
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
                let value = occupied.get_mut();
                std::mem::swap(&mut value.0, &mut streckenabschnitt);
                let bisherig_fließend = value.1;
                value.1 = fließend;
                Some((streckenabschnitt, bisherig_fließend))
            },
            Entry::Vacant(vacant) => {
                let _ = vacant.insert((streckenabschnitt, Fließend::Gesperrt, GleiseDaten::neu()));
                None
            },
        };
        Ok((StreckenabschnittId { geschwindigkeit: geschwindigkeit.cloned(), name }, bisher))
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s>(
        &'s self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<(&'s Streckenabschnitt, &'s Fließend), StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map = self.zustand.streckenabschnitt_map(geschwindigkeit.as_ref())?;
        streckenabschnitt_map
            .get(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
            .ok_or_else(|| {
                StreckenabschnittIdFehler::StreckenabschnittEntfernt(streckenabschnitt.klonen())
            })
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        streckenabschnitt: &StreckenabschnittId,
    ) -> Result<(&'s mut Streckenabschnitt, &'s mut Fließend), StreckenabschnittIdFehler> {
        let StreckenabschnittId { geschwindigkeit, name } = streckenabschnitt;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        streckenabschnitt_map
            .get_mut(name)
            .map(|(streckenabschnitt, fließend, _maps)| (streckenabschnitt, fließend))
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
    ) -> Result<Option<(Streckenabschnitt, Fließend)>, StreckenabschnittBearbeitenFehler> {
        let StreckenabschnittId { geschwindigkeit, name: _ } = &streckenabschnitt_id;
        let streckenabschnitt_map =
            self.zustand.streckenabschnitt_map_mut(geschwindigkeit.as_ref())?;
        self.canvas.leeren();
        streckenabschnitt_entfernen(
            streckenabschnitt_map,
            streckenabschnitt_id,
            Some,
            |_streckenabschnitt_id| Ok(None),
        )
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(in crate::application) fn streckenabschnitte<'t>(
        &'t self,
    ) -> impl Iterator<Item = (StreckenabschnittIdRef<'t>, (&'t Streckenabschnitt, &Fließend))>
    {
        let iter_map = |(geschwindigkeit, streckenabschnitt_map): (_, &'t StreckenabschnittMap)| {
            streckenabschnitt_map.iter().map(move |(name, (streckenabschnitt, fließend, _maps))| {
                (StreckenabschnittIdRef { geschwindigkeit, name }, (streckenabschnitt, fließend))
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
        geschwindigkeit: Geschwindigkeit<Leiter>,
    ) -> Option<Geschwindigkeit<Leiter>> {
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
    ) -> Option<&'s Geschwindigkeit<Leiter>> {
        self.zustand
            .geschwindigkeiten
            .get(name)
            .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<&'s mut Geschwindigkeit<Leiter>> {
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
    ) -> Result<Option<Geschwindigkeit<Leiter>>, GeschwindigkeitEntfernenFehler> {
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
    pub(in crate::application) fn geschwindigkeit_mit_streckenabschnitten_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<Leiter>,
        streckenabschnitt_map: StreckenabschnittMap,
    ) -> Option<(Geschwindigkeit<Leiter>, StreckenabschnittMap)> {
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
    pub(in crate::application) fn geschwindigkeit_mit_streckenabschnitten_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<(Geschwindigkeit<Leiter>, StreckenabschnittMap)> {
        self.zustand.geschwindigkeiten.remove(name)
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn geschwindigkeiten(
        &self,
    ) -> impl Iterator<Item = (&geschwindigkeit::Name, &Geschwindigkeit<Leiter>)> {
        self.zustand
            .geschwindigkeiten
            .iter()
            .map(|(name, (geschwindigkeit, _streckenabschnitt_map))| (name, geschwindigkeit))
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp(&self) -> &Zugtyp<Leiter> {
        &self.zustand.zugtyp
    }

    /// Spurweite des verwendeten Zugtyps.
    pub fn spurweite(&self) -> Spurweite {
        self.zustand.zugtyp.spurweite
    }
}

impl<Leiter: Debug> Gleise<Leiter> {
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
    ) -> Result<Option<(Streckenabschnitt, Fließend)>, StreckenabschnittBearbeitenFehler> {
        if streckenabschnitt_id.geschwindigkeit.as_ref() == geschwindigkeit_neu {
            return Err(StreckenabschnittBearbeitenFehler::IdentischeGeschwindigkeit(
                geschwindigkeit_neu.cloned(),
            ));
        }
        let geschwindigkeit = streckenabschnitt_id.geschwindigkeit.clone();
        let name = streckenabschnitt_id.name.clone();
        let (geschwindigkeit_name_und_eintrag, streckenabschnitt, fließend) =
            if let Some(geschwindigkeit_name) = geschwindigkeit {
                match self.zustand.geschwindigkeiten.remove(&geschwindigkeit_name) {
                    Some((geschwindigkeit, mut streckenabschnitt_map)) => {
                        let (streckenabschnitt, fließend) = streckenabschnitt_entfernen(
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
                            fließend,
                        )
                    },
                    None => {
                        return Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(
                            geschwindigkeit_name,
                        ))
                    },
                }
            } else {
                let (streckenabschnitt, fließend) = streckenabschnitt_entfernen(
                    &mut self.zustand.ohne_geschwindigkeit,
                    streckenabschnitt_id.klonen(),
                    identity,
                    |streckenabschnitt_id| {
                        Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
                            streckenabschnitt_id,
                        ))
                    },
                )?;
                (None, streckenabschnitt, fließend)
            };
        match self.streckenabschnitt_hinzufügen_aux(
            geschwindigkeit_neu,
            name.clone(),
            streckenabschnitt,
            fließend,
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
                self.canvas.leeren();
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
                        .insert(name.clone(), (streckenabschnitt, fließend, GleiseDaten::neu()))
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
                        .insert(name.clone(), (streckenabschnitt, fließend, GleiseDaten::neu()))
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
    gefunden: impl FnOnce((Streckenabschnitt, Fließend)) -> T,
    bereits_entfernt: impl FnOnce(StreckenabschnittId) -> Result<T, StreckenabschnittBearbeitenFehler>,
) -> Result<T, StreckenabschnittBearbeitenFehler> {
    let StreckenabschnittId { geschwindigkeit: _, name } = &streckenabschnitt_id;
    if let Some((name_entry, (streckenabschnitt, fließend, daten))) =
        streckenabschnitt_map.remove_entry(name)
    {
        if daten.ist_leer() {
            Ok(gefunden((streckenabschnitt, fließend)))
        } else {
            let _ = streckenabschnitt_map.insert(name_entry, (streckenabschnitt, fließend, daten));
            Err(StreckenabschnittBearbeitenFehler::GleiseNichtEntfernt(streckenabschnitt_id))
        }
    } else {
        bereits_entfernt(streckenabschnitt_id)
    }
}

#[derive(zugkontrolle_derive::Debug)]
pub enum Nachricht {
    SetzeStreckenabschnitt(AnyId),
    AnschlüsseAnpassen(AnyId),
    FahrenAktion(AnyId),
}

impl<Leiter> iced::canvas::Program<Nachricht> for Gleise<Leiter> {
    #[inline(always)]
    fn draw(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        self.draw(bounds, cursor)
    }

    #[inline(always)]
    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht>) {
        self.update(event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> iced::mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { gehalten: Some(_gehalten), .. } if cursor.is_over(&bounds) => {
                iced::mouse::Interaction::Pointer
            },
            _ => iced::mouse::Interaction::default(),
        }
    }
}

#[derive(Debug)]
pub enum Fehler {
    IO(std::io::Error),
    BincodeSerialisieren(bincode::Error),
    BincodeDeserialisieren { aktuell: bincode::Error, v2: bincode::Error },
    FalscherLeiter(String),
    Anschluss(anschluss::Fehler),
    GleisEntfernt,
    StreckenabschnittEntfernt(StreckenabschnittId),
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
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

#[derive(Debug)]
pub enum GleisIdFehler {
    GleisEntfernt,
    StreckenabschnittEntfernt(StreckenabschnittId),
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

#[derive(Debug)]
pub enum StreckenabschnittIdFehler {
    StreckenabschnittEntfernt(StreckenabschnittId),
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

#[derive(Debug)]
pub enum StreckenabschnittHinzufügenFehler {
    GeschwindigkeitEntfernt(geschwindigkeit::Name, Streckenabschnitt),
}

#[derive(Debug)]
pub enum StreckenabschnittBearbeitenFehler {
    StreckenabschnittEntfernt(StreckenabschnittId),
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
    GleiseNichtEntfernt(StreckenabschnittId),
    IdentischeGeschwindigkeit(Option<geschwindigkeit::Name>),
}
impl From<GeschwindigkeitEntferntFehler> for StreckenabschnittBearbeitenFehler {
    fn from(fehler: GeschwindigkeitEntferntFehler) -> Self {
        StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(fehler.0)
    }
}

#[derive(Debug)]
pub enum GeschwindigkeitEntfernenFehler {
    StreckenabschnitteNichtEntfernt(geschwindigkeit::Name),
}
