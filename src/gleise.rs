//! Verwalten und Anzeige der Gleis-Definitionen auf einem
//! [`Canvas`](iced::widget::canvas::Canvas).

use std::{collections::HashMap, fmt::Debug, io, mem, sync::mpsc::Sender, time::Instant};

use iced::{
    mouse::{self, Cursor},
    widget::canvas::{event, Event, Geometry, Program},
    Rectangle, Renderer,
};
use nonempty::NonEmpty;

use zugkontrolle_argumente::ModusArgument;
use zugkontrolle_typen::{
    canvas::{Cache, Position},
    mm::Spurweite,
    skalar::Skalar,
    vektor::Vektor,
    winkel::Winkel,
};

use crate::{
    application::style::thema::Thema,
    gleise::{
        daten::{GeschwindigkeitEntferntFehler, StreckenabschnittEntferntFehler, Zustand},
        knopf::KlickQuelle,
        nachricht::{Gehalten, Nachricht},
    },
    steuerung::{
        geschwindigkeit::{self, Geschwindigkeit, Leiter},
        streckenabschnitt::{self, Streckenabschnitt},
    },
    zugtyp::Zugtyp,
};

pub mod daten;
pub mod draw;
#[path = "gleise/hinzufügen_entfernen.rs"]
pub mod hinzufügen_entfernen;
pub mod id;
pub mod knopf;
pub mod nachricht;
pub mod steuerung;
pub mod update;

#[zugkontrolle_macros::erstelle_enum(pub, Modus)]
/// Aktueller Modus von [`Gleise`].
#[derive(Debug)]
enum ModusDaten {
    /// Im Bauen-Modus können Gleise hinzugefügt, bewegt, angepasst und bewegt werden.
    Bauen {
        /// Alle aktuell gehaltenen Gleise.
        gehalten: HashMap<KlickQuelle, Gehalten>,
        /// Der letzte verarbeitete Klick. Wird zum Erkennen von Doppel-Klicks verwendet.
        letzter_klick: Option<(KlickQuelle, Instant)>,
    },
    /// Im Fahren-Modus werden die mit den Gleisen assoziierten Aktionen durchgeführt.
    Fahren,
}

impl ModusDaten {
    /// Erzeuge neue [`ModusDaten`] für den gewünschten [`Modus`].
    #[must_use]
    fn neu(modus: Modus) -> Self {
        match modus {
            Modus::Bauen => ModusDaten::Bauen { gehalten: HashMap::new(), letzter_klick: None },
            Modus::Fahren => ModusDaten::Fahren,
        }
    }
}

impl From<ModusArgument> for Modus {
    fn from(value: ModusArgument) -> Self {
        match value {
            ModusArgument::Bauen => Modus::Bauen,
            ModusArgument::Fahren => Modus::Fahren,
        }
    }
}

/// Verwalten und Anzeige aller Gleise.
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
pub struct Gleise<L: Leiter, AktualisierenNachricht> {
    /// Der [`Cache`] für die Canvas-Anzeige aller Gleise.
    canvas: Cache,
    /// Der Pivot-Punkt wird links oben auf dem Canvas angezeigt.
    pivot: Position,
    /// Wie groß werden die Gleise angezeigt.
    skalieren: Skalar,
    /// Alle Gleise, Streckenabschnitte, Geschwindigkeiten, Pläne, sowie der verwendete Zugtyp.
    zustand: Zustand<L>,
    /// Die letzte bekannte Position des Mauszeigers.
    letzte_maus_position: Vektor,
    /// Die letzte bekannte Größe des Canvas.
    letzte_canvas_größe: Vektor,
    /// Der aktuelle Modus und zugehörige Zustandsdaten.
    modus: ModusDaten,
    /// Sender zum schicken einer `AktualisierenNachricht`.
    /// Zwinge `iced` dazu, die `draw`-Methode erneut aufzurufen,
    /// z.B. nach asynchronen Zustandsänderungen.
    sender: Sender<AktualisierenNachricht>,
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Erstelle ein neues, leeres [`Gleise`]-struct.
    #[must_use]
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

    /// Gibt es ein zur [`KlickQuelle`] gehörendes gehaltenes Gleis.
    pub fn hat_gehaltenes_gleis(&self, klick_quelle: KlickQuelle) -> bool {
        match &self.modus {
            ModusDaten::Bauen { gehalten, .. } => gehalten.contains_key(&klick_quelle),
            ModusDaten::Fahren => false,
        }
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
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.pivot.punkt += bewegung;
        }
        self.canvas.leeren();
    }

    /// Setze den `winkel` für die aktuelle Darstellung.
    pub fn winkel(&mut self, winkel: Winkel) {
        self.pivot.winkel = winkel;
        self.canvas.leeren();
    }

    /// Drehe die aktuelle Darstellung um `winkel`.
    pub fn drehen(&mut self, winkel: Winkel) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.pivot.winkel += winkel;
        }
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
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.skalieren *= skalieren;
        }
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
    ///
    /// ## Errors
    ///
    /// Kein Streckenabschnitt mit `name` gefunden.
    pub fn streckenabschnitt<'s>(
        &'s self,
        name: &streckenabschnitt::Name,
    ) -> Result<&'s Streckenabschnitt, StreckenabschnittEntferntFehler> {
        self.zustand
            .streckenabschnitt(name)
            .map(|(streckenabschnitt, _steuerung)| streckenabschnitt)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    ///
    /// ## Errors
    ///
    /// Kein Streckenabschnitt mit `name` gefunden.
    pub fn streckenabschnitt_mut<'s>(
        &'s mut self,
        name: &streckenabschnitt::Name,
    ) -> Result<&'s mut Streckenabschnitt, StreckenabschnittEntferntFehler> {
        self.zustand
            .streckenabschnitt_mut(name)
            .map(|(streckenabschnitt, _steuerung)| streckenabschnitt)
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    /// Schlägt fehl, wenn noch Gleise den Streckenabschnitt zugeordnet waren.
    ///
    /// ## Errors
    ///
    /// Kein Streckenabschnitt mit `name` gefunden.
    pub fn streckenabschnitt_entfernen(
        &mut self,
        name: &streckenabschnitt::Name,
    ) -> Result<Streckenabschnitt, StreckenabschnittEntferntFehler> {
        self.zustand
            .streckenabschnitt_entfernen(name)
            .map(|(streckenabschnitt, _geschwindigkeit)| streckenabschnitt)
    }

    /// Alle aktuell bekannten Streckenabschnitte.
    pub(crate) fn aus_allen_streckenabschnitten<T, C>(
        &self,
        funktion: impl for<'s> Fn(&'s streckenabschnitt::Name, &'s Streckenabschnitt) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        self.zustand
            .streckenabschnitte()
            .iter()
            .map(|(name, (streckenabschnitt, _geschwindigkeit))| funktion(name, streckenabschnitt))
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
    ///
    /// ## Errors
    ///
    /// Keine Geschwindigkeit mit `name` gefunden.
    pub fn geschwindigkeit<'s>(
        &'s self,
        name: &geschwindigkeit::Name,
    ) -> Result<&'s Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        self.zustand.geschwindigkeit(name)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    ///
    /// ## Errors
    ///
    /// Keine Geschwindigkeit mit `name` gefunden.
    pub fn geschwindigkeit_mut<'s>(
        &'s mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<&'s mut Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        self.zustand.geschwindigkeit_mut(name)
    }

    /// Entferne eine Geschwindigkeit.
    /// Falls sie vorhanden war wird sie zurückgegeben.
    /// Schlägt fehl, wenn noch assoziierten Streckenabschnitte vorhanden waren.
    ///
    /// ## Errors
    ///
    /// Keine Geschwindigkeit mit `name` gefunden.
    pub fn geschwindigkeit_entfernen(
        &mut self,
        name: &geschwindigkeit::Name,
    ) -> Result<Geschwindigkeit<L>, GeschwindigkeitEntferntFehler> {
        self.zustand.geschwindigkeit_entfernen(name)
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn mit_allen_geschwindigkeiten(
        &self,
        mut funktion: impl FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>),
    ) {
        self.zustand
            .geschwindigkeiten()
            .iter()
            .for_each(|(name, geschwindigkeit)| funktion(name, geschwindigkeit));
    }

    /// Alle aktuell bekannten Geschwindigkeiten.
    pub(crate) fn aus_allen_geschwindigkeiten<T, C>(
        &self,
        mut funktion: impl for<'s> FnMut(&geschwindigkeit::Name, &Geschwindigkeit<L>) -> T,
    ) -> C
    where
        C: FromIterator<T>,
    {
        self.zustand
            .geschwindigkeiten()
            .iter()
            .map(|(name, geschwindigkeit)| funktion(name, geschwindigkeit))
            .collect()
    }

    // FIXME 2-suffix entfernen
    /// Verwendeter Zugtyp.
    pub fn zugtyp2(&self) -> &Zugtyp<L> {
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
    ///
    /// ## Errors
    ///
    /// Kein Streckenabschnitt mit `name` gefunden.
    pub fn streckenabschnitt_assoziiere_geschwindigkeit(
        &mut self,
        name: &streckenabschnitt::Name,
        mut geschwindigkeit: Option<geschwindigkeit::Name>,
    ) -> Result<Option<geschwindigkeit::Name>, StreckenabschnittEntferntFehler> {
        let (_streckenabschnitt, bisherige_geschwindigkeit) =
            self.zustand.streckenabschnitt_mut(name)?;
        mem::swap(bisherige_geschwindigkeit, &mut geschwindigkeit);
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
            ModusDaten::Bauen { gehalten, .. }
                if gehalten.contains_key(&KlickQuelle::Maus) && cursor.is_over(bounds) =>
            {
                mouse::Interaction::Pointer
            },
            ModusDaten::Bauen { .. } | ModusDaten::Fahren => mouse::Interaction::default(),
        }
    }
}

/// Fehler, die bei Interaktion mit den [`Gleisen`](Gleise) auftreten können.
#[derive(Debug)]
pub enum Fehler {
    /// Ein IO-Fehler.
    IO(io::Error),
    /// Fehler beim Serialisieren (speichern) der Gleise.
    BincodeSerialisieren(bincode::Error),
    /// Ein Fehler bei Interaktion mit einem [`Anschluss`](anschluss::Anschluss).
    Anschluss(zugkontrolle_anschluss::Fehler),
}

impl From<io::Error> for Fehler {
    fn from(error: io::Error) -> Self {
        Fehler::IO(error)
    }
}

impl From<zugkontrolle_anschluss::Fehler> for Fehler {
    fn from(error: zugkontrolle_anschluss::Fehler) -> Self {
        Fehler::Anschluss(error)
    }
}
