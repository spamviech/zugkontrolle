//! Verwalten und Anzeige der Gleis-Definitionen auf einem
//! [Canvas](iced::widget::canvas::Canvas).

use std::{
    collections::hash_map::Entry, convert::identity, fmt::Debug, iter, sync::mpsc::Sender,
    time::Instant,
};

use iced::{
    mouse::{self, Cursor},
    widget::{
        canvas::{event, Event, Geometry, Program},
        Radio,
    },
    Rectangle, Renderer,
};
use log::error;
use nonempty::NonEmpty;

use crate::{
    anschluss,
    application::style::thema::Thema,
    gleis::gleise::{
        daten::{
            GeschwindigkeitEntferntFehler2, GleiseDaten, StreckenabschnittEntferntFehler2,
            StreckenabschnittMap, Zustand, Zustand2,
        },
        id::{StreckenabschnittId, StreckenabschnittIdRef},
        nachricht::{Gehalten, Gehalten2, Nachricht},
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        streckenabschnitt::{self, Streckenabschnitt},
    },
    typen::{
        canvas::{Cache, Position},
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::Winkel,
    },
    zugtyp::{FalscherLeiter, Zugtyp, Zugtyp2},
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod nachricht;
pub mod steuerung;
pub mod update;

#[zugkontrolle_macros::erstelle_enum(pub, Modus)]
/// Aktueller Modus von [Gleise].
#[derive(Debug)]
enum ModusDaten {
    /// Im Bauen-Modus können Gleise hinzugefügt, bewegt, angepasst und bewegt werden.
    Bauen { gehalten: Option<Gehalten>, gehalten2: Option<Gehalten2>, letzter_klick: Instant },
    /// Im Fahren-Modus werden die mit den Gleisen assoziierten Aktionen durchgeführt.
    Fahren,
}

impl ModusDaten {
    fn neu(modus: Modus) -> Self {
        match modus {
            Modus::Bauen => {
                ModusDaten::Bauen { gehalten: None, gehalten2: None, letzter_klick: Instant::now() }
            },
            Modus::Fahren => ModusDaten::Fahren,
        }
    }
}

impl Modus {
    pub(crate) fn erstelle_radio(self, aktueller_modus: Self) -> Radio<Modus, Renderer<Thema>> {
        Radio::new(self, self, Some(aktueller_modus), identity).spacing(0)
    }
}

/// Verwalten und Anzeige aller Gleise.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub struct Gleise<L: Leiter, AktualisierenNachricht> {
    canvas: Cache,
    pivot: Position,
    skalieren: Skalar,
    zustand2: Zustand2<L>,
    letzte_maus_position: Vektor,
    letzte_canvas_größe: Vektor,
    modus: ModusDaten,
    sender: Sender<AktualisierenNachricht>,
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Erstelle ein neues, leeres [Gleise]-struct.
    pub fn neu(
        zugtyp2: Zugtyp2<L>,
        modus: Modus,
        pivot: Position,
        skalieren: Skalar,
        sender: Sender<AktualisierenNachricht>,
    ) -> Self {
        Gleise {
            canvas: Cache::neu(),
            pivot,
            skalieren,
            zustand2: Zustand2::neu(zugtyp2),
            letzte_maus_position: Vektor::null_vektor(),
            letzte_canvas_größe: Vektor::null_vektor(),
            modus: ModusDaten::neu(modus),
            sender,
        }
    }

    /// Erzwinge ein neuzeichnen des Canvas.
    pub fn erzwinge_neuzeichnen(&mut self) {
        self.canvas.leeren();
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match self.modus {
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
    pub fn streckenabschnitt_hinzufügen(
        &mut self,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
        geschwindigkeit: Option<geschwindigkeit::Name>,
    ) -> Option<(Streckenabschnitt, Option<geschwindigkeit::Name>)> {
        self.zustand2.streckenabschnitt_hinzufügen(name, streckenabschnitt, geschwindigkeit)
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s>(
        &'s self,
        streckenabschnitt: &streckenabschnitt::Name,
    ) -> Result<&'s Streckenabschnitt, StreckenabschnittEntferntFehler2> {
        self.zustand2
            .streckenabschnitt(streckenabschnitt)
            .map(|(streckenabschnitt, _steuerung)| streckenabschnitt)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        streckenabschnitt: &streckenabschnitt::Name,
    ) -> Result<&'s mut Streckenabschnitt, StreckenabschnittEntferntFehler2> {
        self.zustand2
            .streckenabschnitt_mut(streckenabschnitt)
            .map(|(streckenabschnitt, _steuerung)| streckenabschnitt)
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    pub fn streckenabschnitt_entfernen(
        &mut self,
        name: &streckenabschnitt::Name,
    ) -> Result<Streckenabschnitt, StreckenabschnittEntferntFehler2> {
        self.zustand2
            .streckenabschnitt_entfernen(name)
            .map(|(streckenabschnitt, _geschwindigkeit)| streckenabschnitt)
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn aus_allen_streckenabschnitten<T, C>(
        &self,
        f: impl for<'s> Fn(StreckenabschnittIdRef<'s>, &'s Streckenabschnitt) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        // // Notwendig, da sonst f in die flat_map-closure moved wird, wodurch sie nur FnOnce ist.
        // // Außerdem darf sie aus irgendeinem Grund nicht als Variable gespeichert werden.
        // let g = &f;
        // iter::once((None, &self.zustand.ohne_geschwindigkeit))
        //     .chain(self.zustand.geschwindigkeiten.iter().map(
        //         |(geschwindigkeit_name, (_geschwindigkeit, streckenabschnitt_map))| {
        //             (Some(geschwindigkeit_name), streckenabschnitt_map)
        //         },
        //     ))
        //     .flat_map(|(geschwindigkeit, streckenabschnitt_map): (_, &StreckenabschnittMap)| {
        //         streckenabschnitt_map.iter().map(move |(name, (streckenabschnitt, _daten))| {
        //             g(StreckenabschnittIdRef { geschwindigkeit, name }, streckenabschnitt)
        //         })
        //     })
        //     .collect()
        todo!()
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
    ) -> Option<Geschwindigkeit<L>> {
        // match self.zustand.geschwindigkeiten.entry(name) {
        //     Entry::Occupied(mut occupied) => {
        //         let bisher = std::mem::replace(&mut occupied.get_mut().0, geschwindigkeit);
        //         Some(bisher)
        //     },
        //     Entry::Vacant(vacant) => {
        //         let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
        //         None
        //     },
        // }
        todo!()
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit<'s>(
        &'s self,
        name: &geschwindigkeit::Name,
    ) -> Option<&'s Geschwindigkeit<L>> {
        // self.zustand
        //     .geschwindigkeiten
        //     .get(name)
        //     .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
        todo!()
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<&'s mut Geschwindigkeit<L>> {
        // self.zustand
        //     .geschwindigkeiten
        //     .get_mut(name)
        //     .map(|(geschwindigkeit, _streckenabschnitt_map)| geschwindigkeit)
        todo!()
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Schlägt fehl, wenn noch assoziierten Streckenabschnitte vorhanden waren.
    pub fn geschwindigkeit_entfernen(
        &mut self,
        name: geschwindigkeit::Name,
    ) -> Result<Option<Geschwindigkeit<L>>, GeschwindigkeitEntfernenFehler> {
        // if let Some((name_entry, (geschwindigkeit, streckenabschnitt_map))) =
        //     self.zustand.geschwindigkeiten.remove_entry(&name)
        // {
        //     if streckenabschnitt_map.is_empty() {
        //         Ok(Some(geschwindigkeit))
        //     } else {
        //         let _ = self
        //             .zustand
        //             .geschwindigkeiten
        //             .insert(name_entry, (geschwindigkeit, streckenabschnitt_map));
        //         Err(GeschwindigkeitEntfernenFehler::StreckenabschnitteNichtEntfernt(name))
        //     }
        // } else {
        //     Ok(None)
        // }
        todo!()
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
        // match self.zustand.geschwindigkeiten.entry(name) {
        //     Entry::Occupied(mut occupied) => {
        //         let bisher =
        //             std::mem::replace(occupied.get_mut(), (geschwindigkeit, streckenabschnitt_map));
        //         Some(bisher)
        //     },
        //     Entry::Vacant(vacant) => {
        //         let _ = vacant.insert((geschwindigkeit, StreckenabschnittMap::new()));
        //         None
        //     },
        // }
        todo!()
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Assoziierte Streckenabschnitte werden (mit allen Gleisen) ebenfalls entfernt.
    #[inline(always)]
    pub(crate) fn geschwindigkeit_mit_streckenabschnitten_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Option<(Geschwindigkeit<L>, StreckenabschnittMap)> {
        // self.zustand.geschwindigkeiten.remove(name)
        todo!()
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn mit_allen_geschwindigkeiten(
        &self,
        mut f: impl FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>),
    ) {
        // for (name, (geschwindigkeit, _streckenabschnitt_map)) in
        //     self.zustand.geschwindigkeiten.iter()
        // {
        //     f(name, geschwindigkeit)
        // }
        todo!()
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn aus_allen_geschwindigkeiten<T, C>(
        &self,
        mut f: impl for<'s> FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        // self.zustand
        //     .geschwindigkeiten
        //     .iter()
        //     .map(|(name, (geschwindigkeit, _streckenabschnitt_map))| f(name, geschwindigkeit))
        //     .collect()
        todo!()
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp<'t>(&'t self) -> &'t Zugtyp<L> {
        // &self.zustand.zugtyp
        todo!()
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp2<'t>(&'t self) -> &'t Zugtyp2<L> {
        self.zustand2.zugtyp()
    }

    /// Spurweite des verwendeten Zugtyps.
    pub fn spurweite(&self) -> Spurweite {
        self.zugtyp2().spurweite
    }
}

impl<L: Debug + Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
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
        // if streckenabschnitt_id.geschwindigkeit.as_ref() == geschwindigkeit_neu {
        //     return Err(StreckenabschnittBearbeitenFehler::IdentischeGeschwindigkeit(
        //         geschwindigkeit_neu.cloned(),
        //     ));
        // }
        // let geschwindigkeit = streckenabschnitt_id.geschwindigkeit.clone();
        // let name = streckenabschnitt_id.name.clone();
        // let (geschwindigkeit_name_und_eintrag, streckenabschnitt) =
        //     if let Some(geschwindigkeit_name) = geschwindigkeit {
        //         match self.zustand.geschwindigkeiten.remove(&geschwindigkeit_name) {
        //             Some((geschwindigkeit, mut streckenabschnitt_map)) => {
        //                 let streckenabschnitt = streckenabschnitt_entfernen(
        //                     &mut streckenabschnitt_map,
        //                     streckenabschnitt_id.klonen(),
        //                     identity,
        //                     |streckenabschnitt_id| {
        //                         Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
        //                             streckenabschnitt_id,
        //                         ))
        //                     },
        //                 )?;
        //                 (
        //                     Some((geschwindigkeit_name, (geschwindigkeit, streckenabschnitt_map))),
        //                     streckenabschnitt,
        //                 )
        //             },
        //             None => {
        //                 return Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(
        //                     geschwindigkeit_name,
        //                 ))
        //             },
        //         }
        //     } else {
        //         let streckenabschnitt = streckenabschnitt_entfernen(
        //             &mut self.zustand.ohne_geschwindigkeit,
        //             streckenabschnitt_id.klonen(),
        //             identity,
        //             |streckenabschnitt_id| {
        //                 Err(StreckenabschnittBearbeitenFehler::StreckenabschnittEntfernt(
        //                     streckenabschnitt_id,
        //                 ))
        //             },
        //         )?;
        //         (None, streckenabschnitt)
        //     };
        // match self.streckenabschnitt_hinzufügen(
        //     geschwindigkeit_neu,
        //     name.clone(),
        //     streckenabschnitt,
        // ) {
        //     Ok((id_neu, bisher)) => {
        //         if let Some((geschwindigkeit_name, geschwindigkeit_eintrag)) =
        //             geschwindigkeit_name_und_eintrag
        //         {
        //             let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
        //             if let Some(geschwindigkeit_eintrag) = self
        //                 .zustand
        //                 .geschwindigkeiten
        //                 .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
        //             {
        //                 error!(
        //                     "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
        //                     geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
        //                 )
        //             }
        //         }
        //         *streckenabschnitt_id = id_neu;
        //         self.canvas.leeren();
        //         Ok(bisher)
        //     },
        //     Err(StreckenabschnittHinzufügenFehler::GeschwindigkeitEntfernt(
        //         geschwindigkeit_neu,
        //         streckenabschnitt,
        //     )) => {
        //         if let Some((geschwindigkeit_name, mut geschwindigkeit_eintrag)) =
        //             geschwindigkeit_name_und_eintrag
        //         {
        //             if let Some(streckenabschnitt_eintrag) = geschwindigkeit_eintrag
        //                 .1
        //                 .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
        //             {
        //                 error!(
        //                     "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
        //                     StreckenabschnittIdRef {
        //                         geschwindigkeit: Some(&geschwindigkeit_name),
        //                         name: &name
        //                     },
        //                     streckenabschnitt_eintrag
        //                 )
        //             }
        //             let geschwindigkeit_name_clone = geschwindigkeit_name.clone();
        //             if let Some(geschwindigkeit_eintrag) = self
        //                 .zustand
        //                 .geschwindigkeiten
        //                 .insert(geschwindigkeit_name, geschwindigkeit_eintrag)
        //             {
        //                 error!(
        //                     "Entfernte Geschwindigkeit {} war weiterhin vorhanden: {:?}",
        //                     geschwindigkeit_name_clone.0, geschwindigkeit_eintrag
        //                 )
        //             }
        //         } else {
        //             if let Some(streckenabschnitt_eintrag) = self
        //                 .zustand
        //                 .ohne_geschwindigkeit
        //                 .insert(name.clone(), (streckenabschnitt, GleiseDaten::neu()))
        //             {
        //                 error!(
        //                     "Entfernter Streckenabschnitt {:?} war weiterhin vorhanden: {:?}",
        //                     StreckenabschnittId { geschwindigkeit: None, name },
        //                     streckenabschnitt_eintrag
        //                 )
        //             }
        //         }
        //         Err(StreckenabschnittBearbeitenFehler::GeschwindigkeitEntfernt(geschwindigkeit_neu))
        //     },
        // }
        todo!()
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

impl<L, AktualisierenNachricht> Program<NonEmpty<Nachricht>, Renderer<Thema>>
    for Gleise<L, AktualisierenNachricht>
where
    L: Leiter,
    AktualisierenNachricht: 'static + From<steuerung::Aktualisieren> + Send,
{
    type State = ();

    #[inline(always)]
    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer<Thema>,
        thema: &Thema,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> Vec<Geometry> {
        Gleise::draw(self, state, renderer, thema, bounds, cursor)
    }

    #[inline(always)]
    fn update(
        &self,
        state: &mut Self::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<NonEmpty<Nachricht>>) {
        Gleise::update(self, state, event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        _state: &Self::State,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { gehalten: Some(_gehalten), .. } if cursor.is_over(bounds) => {
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

/// Fehler, die beim Anpassen der Anschlüsse eines Gleises auftreten können.
#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum AnschlüsseAnpassenFehler {
    /// Ein Fehler beim [Reservieren](crate::anschluss::Reserviere::reserviere) der [Anschlüsse](anschluss::Anschluss).
    Deserialisieren {
        /// Der Fehler beim reservieren der neuen Anschlüsse.
        fehler: NonEmpty<anschluss::Fehler>,
        /// Ein Fehler beim Wiederherstellen der ursprünglichen Anschlüsse,
        /// sowie eine Repräsentation der ursprünglichen Anschlüsse.
        wiederherstellen_fehler: Option<(NonEmpty<anschluss::Fehler>, String)>,
    },
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt(GleisIdFehler),
}
