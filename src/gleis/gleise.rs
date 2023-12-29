//! Verwalten und Anzeige der Gleis-Definitionen auf einem
//! [Canvas](iced::widget::canvas::Canvas).

use std::{convert::identity, fmt::Debug, sync::mpsc::Sender, time::Instant};

use iced::{
    mouse::{self, Cursor},
    widget::{
        canvas::{event, Event, Geometry, Program},
        Radio,
    },
    Rectangle, Renderer,
};
use nonempty::NonEmpty;

use crate::{
    anschluss,
    application::style::thema::Thema,
    gleis::gleise::{
        daten::{
            de_serialisieren::ZugtypDeserialisierenFehler, GeschwindigkeitEntferntFehler2,
            StreckenabschnittEntferntFehler2, Zustand,
        },
        nachricht::{Gehalten, Nachricht},
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
    zugtyp::Zugtyp,
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
    Bauen { gehalten: Option<Gehalten>, letzter_klick: Instant },
    /// Im Fahren-Modus werden die mit den Gleisen assoziierten Aktionen durchgeführt.
    Fahren,
}

impl ModusDaten {
    fn neu(modus: Modus) -> Self {
        match modus {
            Modus::Bauen => ModusDaten::Bauen { gehalten: None, letzter_klick: Instant::now() },
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
    zustand: Zustand<L>,
    letzte_maus_position: Vektor,
    letzte_canvas_größe: Vektor,
    modus: ModusDaten,
    sender: Sender<AktualisierenNachricht>,
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Erstelle ein neues, leeres [Gleise]-struct.
    pub fn neu(
        zugtyp2: Zugtyp<L>,
        modus: Modus,
        pivot: Position,
        skalieren: Skalar,
        sender: Sender<AktualisierenNachricht>,
    ) -> Self {
        Gleise {
            canvas: Cache::neu(),
            pivot,
            skalieren,
            zustand: Zustand::neu(zugtyp2),
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
        self.zustand.streckenabschnitt_hinzufügen(name, streckenabschnitt, geschwindigkeit)
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt<'s>(
        &'s self,
        streckenabschnitt: &streckenabschnitt::Name,
    ) -> Result<&'s Streckenabschnitt, StreckenabschnittEntferntFehler2> {
        self.zustand
            .streckenabschnitt(streckenabschnitt)
            .map(|(streckenabschnitt, _steuerung)| streckenabschnitt)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        streckenabschnitt: &streckenabschnitt::Name,
    ) -> Result<&'s mut Streckenabschnitt, StreckenabschnittEntferntFehler2> {
        self.zustand
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
        self.zustand
            .streckenabschnitt_entfernen(name)
            .map(|(streckenabschnitt, _geschwindigkeit)| streckenabschnitt)
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn aus_allen_streckenabschnitten<T, C>(
        &self,
        f: impl for<'s> Fn(&'s streckenabschnitt::Name, &'s Streckenabschnitt) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        self.zustand
            .streckenabschnitte()
            .iter()
            .map(|(name, (streckenabschnitt, _geschwindigkeit))| f(name, streckenabschnitt))
            .collect()
    }

    /// Füge eine Geschwindigkeit hinzu.
    /// Eine vorher gespeicherte Geschwindigkeit mit identischem Namen wird zurückgegeben.
    /// Assoziierte Streckenabschnitte (und Gleise) werden nicht verändert.
    pub fn geschwindigkeit_hinzufügen(
        &mut self,
        name: geschwindigkeit::Name,
        geschwindigkeit: Geschwindigkeit<L>,
    ) -> Option<Geschwindigkeit<L>> {
        self.zustand.geschwindigkeit_hinzufügen(name, geschwindigkeit)
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit<'s>(
        &'s self,
        name: &geschwindigkeit::Name,
    ) -> Result<&'s Geschwindigkeit<L>, GeschwindigkeitEntferntFehler2> {
        self.zustand.geschwindigkeit(name)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<&'s mut Geschwindigkeit<L>, GeschwindigkeitEntferntFehler2> {
        self.zustand.geschwindigkeit_mut(name)
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Schlägt fehl, wenn noch assoziierten Streckenabschnitte vorhanden waren.
    pub fn geschwindigkeit_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<Geschwindigkeit<L>, GeschwindigkeitEntferntFehler2> {
        self.zustand.geschwindigkeit_entfernen(name)
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn mit_allen_geschwindigkeiten(
        &self,
        mut f: impl FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>),
    ) {
        self.zustand
            .geschwindigkeiten()
            .iter()
            .for_each(|(name, geschwindigkeit)| f(name, geschwindigkeit))
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn aus_allen_geschwindigkeiten<T, C>(
        &self,
        mut f: impl for<'s> FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        self.zustand
            .geschwindigkeiten()
            .iter()
            .map(|(name, geschwindigkeit)| f(name, geschwindigkeit))
            .collect()
    }

    /// Verwendeter Zugtyp.
    pub fn zugtyp2<'t>(&'t self) -> &'t Zugtyp<L> {
        self.zustand.zugtyp()
    }

    /// Spurweite des verwendeten Zugtyps.
    pub fn spurweite(&self) -> Spurweite {
        self.zugtyp2().spurweite
    }
}

impl<L: Debug + Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Assoziiere einen Streckenabschnitt mit einer Geschwindigkeit.
    /// Schlägt fehl, wenn der Streckenabschnitt nicht gefunden wurde.
    pub fn streckenabschnitt_assoziiere_geschwindigkeit(
        &mut self,
        name: &streckenabschnitt::Name,
        mut geschwindigkeit: Option<geschwindigkeit::Name>,
    ) -> Result<Option<geschwindigkeit::Name>, StreckenabschnittEntferntFehler2> {
        let (_streckenabschnitt, bisherige_geschwindigkeit) =
            self.zustand.streckenabschnitt_mut(name)?;
        std::mem::swap(bisherige_geschwindigkeit, &mut geschwindigkeit);
        Ok(geschwindigkeit)
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
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<ZugtypDeserialisierenFehler> for Fehler {
    fn from(fehler: ZugtypDeserialisierenFehler) -> Self {
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
pub(crate) enum GleisIdFehler {
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt,
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<GleisIdFehler> for Fehler {
    fn from(fehler: GleisIdFehler) -> Self {
        match fehler {
            GleisIdFehler::GleisEntfernt => Fehler::GleisEntfernt,
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
pub(crate) enum StreckenabschnittIdFehler {
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
}

impl From<StreckenabschnittIdFehler> for Fehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
            StreckenabschnittIdFehler::GeschwindigkeitEntfernt(geschwindigkeit) => {
                Fehler::GeschwindigkeitEntfernt(geschwindigkeit)
            },
        }
    }
}

impl From<StreckenabschnittIdFehler> for GleisIdFehler {
    fn from(fehler: StreckenabschnittIdFehler) -> Self {
        match fehler {
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
pub(crate) enum StreckenabschnittBearbeitenFehler {
    /// Die betroffene [Geschwindigkeit] wurde entfernt.
    GeschwindigkeitEntfernt(geschwindigkeit::Name),
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
pub(crate) enum AnschlüsseAnpassenFehler {
    /// Ein Fehler beim [Reservieren](crate::anschluss::Reserviere::reserviere) der [Anschlüsse](anschluss::Anschluss).
    Deserialisieren {
        /// Der Fehler beim reservieren der neuen Anschlüsse.
        #[allow(dead_code)]
        fehler: NonEmpty<anschluss::Fehler>,
        /// Ein Fehler beim Wiederherstellen der ursprünglichen Anschlüsse,
        /// sowie eine Repräsentation der ursprünglichen Anschlüsse.
        #[allow(dead_code)]
        wiederherstellen_fehler: Option<(NonEmpty<anschluss::Fehler>, String)>,
    },
    /// Das betroffene Gleis wurde entfernt.
    GleisEntfernt(GleisIdFehler),
}
