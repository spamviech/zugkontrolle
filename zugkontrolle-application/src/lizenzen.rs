//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    ops::DerefMut,
};

use iced_core::{
    event, text as text_core,
    widget::text::{self, Text},
    Element, Length, Renderer,
};
use iced_widget::{
    button::{self, Button},
    container::{self, Container},
    rule::{self, Rule},
    scrollable::{self, Scrollable},
    Column, Row, Space,
};
use nonempty::NonEmpty;
use once_cell::sync::Lazy;

use zugkontrolle_util::unicase_ord::UniCaseOrd;

use crate::{
    lizenzen::texte::OflCopyright,
    map_mit_zustand::MapMitZustand,
    style::{
        self,
        linie::{Linie, TRENNLINIE},
    },
};

pub mod texte;

use texte::{
    apache_2_0, apache_2_0_eingerückt, apache_2_0_standard_eingerückt, bsd_0, bsd_2, bsd_3,
    bsl_1_0, cc_0, isc, mit, mit_missing_note, mit_ohne_copyright, mit_ohne_copyright_x11, ofl_1_1,
    servo_fontconfig_sys, zlib, ApacheCopyright, ApacheEinrückung, BSD3Copyright, BSD3Darstellung,
    BSD3Zeilenumbruch, ISCZeilenumbruch, MITCopyright, MITEinrückung, MITEnde, MITInfix, MITPräfix,
    MITZeilenumbruch,
};

/// Interne Nachricht zur Interaktion mit einem [`Lizenzen`]-Widget.
#[derive(Debug, Clone)]
enum InterneNachricht {
    /// Zeige den übergebenen Lizenz-Text an.
    Aktuell(UniCaseOrd<String>, fn() -> Cow<'static, str>),
    /// Schließe das Dialog-Fenster.
    Schließen,
}

/// Nachricht, die von einem [`Lizenzen`]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [`Lizenzen`]-Anzeige.
    Schließen,
}

/// Zustand eines [`Lizenzen`]-Widgets.
#[derive(Debug, PartialEq, Eq)]
struct Zustand {
    /// Die aktuell gezeigte Lizenz.
    aktuell: Option<(UniCaseOrd<String>, Cow<'static, str>)>,
}

/// Eine Map von Namen auf eine Funktion, die den Lizenztext erzeugt.
///
/// Die Namen werden mit [`UniCaseOrd`] geordnet.
type LizenzenMap = BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>>;

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [`Lizenzen`]-Widgets.
    fn neu(lizenzen: &LizenzenMap) -> Self {
        let aktuell = lizenzen
            .iter()
            .next()
            .map(|(name, erzeuge_lizenztext)| (name.clone(), erzeuge_lizenztext()));
        Zustand { aktuell }
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
#[derive(Debug)]
pub struct Lizenzen<'a, R>(MapMitZustand<'a, Zustand, InterneNachricht, Nachricht, R>);

/// Der [`Abstand`](Space) zwischen Widgets in Pixel.
const PADDING: f32 = 5.;
/// Die Breite der [`Trennlinie`](Rule) zwischen der Auswahl-Liste und dem aktuell gezeigten Lizenztext.
const TRENNLINIE_BREITE: u16 = 1;

impl<'a, R> Lizenzen<'a, R>
where
    R: 'a + text_core::Renderer,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + rule::StyleSheet
        + text::StyleSheet,
    <<R as Renderer>::Theme as rule::StyleSheet>::Style: From<Linie>,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    /// Erstelle ein neues [`Lizenzen`]-Widget mit den verwendeten Lizenzen.
    pub fn neu_mit_verwendeten_lizenzen<ScrollableStyle>(scrollable_style: ScrollableStyle) -> Self
    where
        ScrollableStyle: 'a + Clone,
        <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<ScrollableStyle>,
    {
        Self::neu(&TARGET_LIZENZEN, scrollable_style)
    }

    /// Erstelle ein neues [`Lizenzen`]-Widget.
    pub fn neu<ScrollableStyle>(
        lizenzen: &'a LizenzenMap,
        scrollable_style: ScrollableStyle,
    ) -> Self
    where
        ScrollableStyle: 'a + Clone,
        <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<ScrollableStyle>,
    {
        let erzeuge_zustand = || Zustand::neu(lizenzen);
        let erzeuge_element = move |zustand: &Zustand| -> Element<'a, InterneNachricht, R> {
            Self::erzeuge_element(zustand, lizenzen, scrollable_style.clone())
        };
        let mapper = |interne_nachricht,
                      zustand: &mut dyn DerefMut<Target = Zustand>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Aktuell(name, erzeuge_lizenz_text) => {
                    zustand.aktuell = Some((name, erzeuge_lizenz_text()));
                    Vec::new()
                },
                InterneNachricht::Schließen => vec![Nachricht::Schließen],
            }
        };
        Lizenzen(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Lizenzen`]-Widget.
    fn erzeuge_element<ScrollableStyle>(
        zustand: &Zustand,
        lizenzen: &'a LizenzenMap,
        scrollable_style: ScrollableStyle,
    ) -> Element<'a, InterneNachricht, R>
    where
        <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<ScrollableStyle>,
    {
        let Zustand { aktuell } = zustand;
        let mut buttons = Column::new().width(Length::Shrink).height(Length::Shrink);
        let (aktuell_name, aktuell_text) = if let Some((name, text)) = aktuell {
            (Some(name.clone()), Some(text.clone().into_owned()))
        } else {
            (None, None)
        };
        for (name, erzeuge_lizenz_text) in lizenzen {
            buttons = buttons.push({
                let button = Button::new(Text::new(name.as_ref()));
                if Some(name) == aktuell_name.as_ref() {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name.clone(), *erzeuge_lizenz_text))
                }
            });
        }
        let buttons = Scrollable::new(buttons).style(scrollable_style);
        let column = Column::new()
            .push(Button::new(Text::new("Schließen")).on_press(InterneNachricht::Schließen))
            .push(Space::with_height(Length::Fixed(PADDING)))
            .push(buttons)
            .width(Length::Shrink)
            .height(Length::Fill);
        let mut column_aktuell = Column::new().width(Length::Fill).height(Length::Shrink);
        if let Some(aktuell_text) = aktuell_text {
            let text_mit_horizontalem_padding = Row::new()
                .push(Space::with_width(Length::Fixed(PADDING)))
                .push(Text::new(aktuell_text).width(Length::Fill).height(Length::Shrink))
                .push(Space::with_width(Length::Fixed(PADDING)))
                .width(Length::Fill)
                .height(Length::Shrink);
            column_aktuell = column_aktuell
                .push(Space::with_height(Length::Fixed(PADDING)))
                .push(text_mit_horizontalem_padding)
                .push(Space::with_height(Length::Fixed(PADDING)));
        }
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(TRENNLINIE_BREITE).style(TRENNLINIE))
                .push(Scrollable::new(column_aktuell)),
        )
        .style(style::container::WEIß);
        container.into()
    }
}

impl<'a, R> From<Lizenzen<'a, R>> for Element<'a, Nachricht, R>
where
    R: 'a + text_core::Renderer,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + rule::StyleSheet
        + text::StyleSheet,
    <<R as Renderer>::Theme as rule::StyleSheet>::Style: From<Linie>,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen.0)
    }
}

/// Crates für das aktuelle target, ausgehend von `cargo --filter-platform <target> metadata`.
fn target_crates_und_schriftarten() -> HashMap<&'static str, NonEmpty<&'static str>> {
    let mut crates_und_schriftarten: HashMap<&'static str, NonEmpty<&'static str>> = HashMap::new();
    // Schriftarten
    let _ = crates_und_schriftarten.insert("SourceSerif4-Regular", NonEmpty::singleton("4.005"));
    let _ = crates_und_schriftarten.insert("Bootstrap Icons", NonEmpty::singleton("v1.11.2"));
    // crates
    for (name, version) in zugkontrolle_macros::target_crates!() {
        use std::collections::hash_map::Entry;
        match crates_und_schriftarten.entry(name) {
            Entry::Occupied(mut occupied) => occupied.get_mut().push(version),
            Entry::Vacant(vacant) => {
                let _ = vacant.insert(NonEmpty::singleton(version));
            },
        }
    }
    crates_und_schriftarten
}

/// Alle Lizenzen für die aktuelle target-Platform.
static TARGET_LIZENZEN: Lazy<LizenzenMap> =
    Lazy::new(|| verwendete_lizenzen(target_crates_und_schriftarten()));

/// Lizenzen für eine dependency, potentiell Unterschiedlich je nach Version.
#[derive(Debug, Clone)]
struct Lizenz {
    /// Die allgemeine Lizenz für alle Versionen.
    /// Fallback, falls in `version_spezifisch` kein Eintrag für die Version vorhanden ist.
    lizenz: fn() -> Cow<'static, str>,
    /// Spezielle Lizenztexte für bestimmte Versionen.
    version_spezifisch: HashMap<&'static str, fn() -> Cow<'static, str>>,
}

impl From<fn() -> Cow<'static, str>> for Lizenz {
    fn from(value: fn() -> Cow<'static, str>) -> Self {
        Lizenz::neu(value)
    }
}

impl Lizenz {
    /// Erzeuge eine neue [`Lizenz`] mit dem selben Text für alle Versionen.
    fn neu(lizenz: fn() -> Cow<'static, str>) -> Self {
        Lizenz { lizenz, version_spezifisch: HashMap::new() }
    }

    /// Erhalte eine Funktion um den Lizenztext für die gewünschte Version zu erzeugen.
    #[must_use]
    fn lizenz_für_version(&self, version: &str) -> fn() -> Cow<'static, str> {
        *self.version_spezifisch.get(version).unwrap_or(&self.lizenz)
    }
}

/// Source Serif Schriftart
fn source_lizenz() -> Cow<'static, str> {
    let extra_notice = " All Rights Reserved. Source is a trademark of Adobe in the United States and/or other countries.";
    let copyright = OflCopyright {
        copyright_c: false,
        jahr: "2014-2021",
        voller_name: "Adobe (http://www.adobe.com/),",
        font_name: "'Source'",
        punkt_nach_font_name: true,
        extra_notice,
    };
    ofl_1_1(Some(copyright), false, true, false, false)
}

/// Bootstrap Icon Schriftart
fn bootstrap_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2019-2023", "The Bootstrap Authors")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Lato Schriftart
fn lato_lizenz() -> Cow<'static, str> {
    let copyright = OflCopyright {
        copyright_c: true,
        jahr: "2010-2014",
        voller_name: "by tyPoland Lukasz Dziedzic (team@latofonts.com)",
        font_name: "\"Lato\"",
        punkt_nach_font_name: false,
        extra_notice: "",
    };
    ofl_1_1(Some(copyright), false, false, true, true)
}

/// MIT-Lizenz für "The Rust Project Developers" mit dem entsprechenden `jahr`.
fn mit_rust_project_developers_lizenz<'t>(jahr: &str) -> Cow<'t, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, jahr, "The Rust Project Developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz für "The Rust Project Developers" mit dem Jahr 2010.
fn mit_rust_project_developers_lizenz_2010() -> Cow<'static, str> {
    mit_rust_project_developers_lizenz("2010")
}

/// MIT-Lizenz für "The Rust Project Developers" mit dem Jahr 2014.
fn mit_rust_project_developers_lizenz_2014() -> Cow<'static, str> {
    mit_rust_project_developers_lizenz("2014")
}

/// MIT-Lizenz für "The Rust Project Developers" mit dem Jahr 2015.
fn mit_rust_project_developers_lizenz_2015() -> Cow<'static, str> {
    mit_rust_project_developers_lizenz("2015")
}

/// MIT-Lizenz für "The Rust Project Developers" mit dem Jahr 2016.
fn mit_rust_project_developers_lizenz_2016() -> Cow<'static, str> {
    mit_rust_project_developers_lizenz("2016")
}

/// MIT-Lizenz für "Jorge Aparicio" mit dem entsprechenden `jahr`.
fn mit_lizenz_aparicio<'t>(jahr: &str) -> Cow<'t, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, jahr, "Jorge Aparicio")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz für die "Mozilla Foundation".
fn mozilla_foundation_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2012-2013", "Mozilla Foundation")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz, die ich zuerst beim ab_glyph-crate gesehen habe.
fn ab_glyph_lizenz(appendix: bool, ende_neue_zeile: usize) -> Cow<'static, str> {
    apache_2_0_eingerückt(
        false,
        &ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
        appendix,
        ende_neue_zeile,
    )
}

/// MIT-Lizenz, die ich zuerst beim arrayvec-crate gesehen habe.
fn arrayvec_lizenz(year: &str) -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "Ulrik Sverdrup \"bluss\"", year)],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim bytemuck-crate gesehen habe.
fn bytemuck_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2019", "Daniel \"Lokathor\" Gee.")],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        true,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz, die ich zuerst beim clipboard-crate gesehen habe.
fn clipboard_apache_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright::braces(),
        &ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
        1,
    )
}

/// MIT-Lizenz, die ich zuerst beim crossbeam-crate gesehen habe.
fn crossbeam_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2019", "The Crossbeam Project Developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim darling-crate gesehen habe.
fn darling_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2017", "Ted Driggs")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim foreign-types-crate gesehen habe.
fn foreign_types_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "The foreign-types Developers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim futures-crate gesehen habe.
fn futures_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![
            MITCopyright::neu(true, "2016", "Alex Crichton"),
            MITCopyright::neu(true, "2017", "The Tokio Authors"),
        ],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim glutin-crate gesehen habe.
fn glutin_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "The glutin contributors" },
        &ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
        1,
    )
}

/// MIT-Lizenz, die ich zuerst beim glyph-brush-crate gesehen habe.
fn glyph_brush_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(
        false,
        &ApacheCopyright { brackets: "{}", jahr: "2017", voller_name: "Alex Butler" },
        true,
        1,
    )
}

/// MIT-Lizenz, die ich zuerst beim iced-crate gesehen habe.
fn iced_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(false, "2019", "Héctor Ramón, Iced contributors")],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz mit Autor "spamviech".
fn spamviech_lizenz(jahr: &str) -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, jahr, "spamviech")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim kommandozeilen_argumente-crate gesehen habe.
fn kommandozeilen_argumente_lizenz() -> Cow<'static, str> {
    spamviech_lizenz("2022")
}

/// MIT-Lizenz, die ich zuerst beim associated_list-crate gesehen habe.
fn associated_list_lizenz() -> Cow<'static, str> {
    spamviech_lizenz("2024")
}

/// MIT-Lizenz, die ich zuerst beim lyon-crate gesehen habe.
fn lyon_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2013", "Nicolas Silva")],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim memchr-crate gesehen habe.
fn memchr_lizenz(jahr: &'static str) -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, jahr, "Andrew Gallant")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim memmap2-crate gesehen habe.
fn memmap2_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![
            MITCopyright::neu(true, "2020", "Yevhenii Reizner"),
            MITCopyright::neu(true, "2015", "Dan Burkert"),
        ],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim ndk-crate gesehen habe.
fn ndk_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        Vec::new(),
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim nix-crate gesehen habe.
fn nix_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2015", "Carl Lerche + nix-rust Authors")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim nonempty-crate gesehen habe.
fn nonempty_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "Alexis Sellier")],
        None,
        MITZeilenumbruch::NonEmpty,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim rand-crate gesehen habe.
fn rand_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![
            MITCopyright::neu(false, "2018", "Developers of the Rand project"),
            MITCopyright::neu(true, "2014", "The Rust Project Developers"),
        ],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim raw-window-handle-crate gesehen habe.
fn raw_window_handle_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2019", "Osspial")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim serde-crate gesehen habe.
fn serde_lizenz() -> Cow<'static, str> {
    mit(
        None,
        Vec::new(),
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim smithy-client-toolkit-crate gesehen habe.
fn smithay_client_toolkit_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "Victor Berger")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz, die ich zuerst beim time-crate gesehen habe.
fn time_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2022", "Jacob Pratt et al.")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim vcell-crate gesehen habe.
fn vcell_lizenz() -> Cow<'static, str> {
    mit_lizenz_aparicio("2017")
}

/// MIT-Lizenz, die ich zuerst beim vswhom-crate gesehen habe.
fn vswwhom_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2019", "nabijaczleweli")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz für "Alex Crichton" und dem jahr "2014".
fn crichton_2014_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2014", "Alex Crichton")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim wayland-crate gesehen habe.
fn wayland_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "Victor Berger")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz, die ich zuerst beim wayland-scanner-crate gesehen habe.
fn wayland_scanner_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "Elinor Berger")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim winapi-crate gesehen habe.
fn winapi_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015-2018", "The winapi-rs Developers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim windows-sys-crate gesehen habe.
fn widows_sys_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, None, "Microsoft Corporation.")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::leerzeichen_4(),
        false,
        MITEnde { punkt: false, neue_zeile: 1 },
    )
}

/// MIT-Lizenz, die ich zuerst beim tiny-skia-crate gesehen habe.
fn tiny_skia_lizenz() -> Cow<'static, str> {
    bsd_3(
        vec![
            BSD3Copyright::neu("2011", false, "Google Inc.", false),
            BSD3Copyright::neu("2020", false, "Yevhenii Reizner", false),
        ],
        BSD3Zeilenumbruch::TinySkia,
        &BSD3Darstellung {
            punkte: |_| Cow::Borrowed("*"),
            einrückung_punkte: "  ",
            einrückung_text: "    ",
            author: "copyright holder",
            copyright_holder: "OWNER",
        },
    )
}

/// MIT-Lizenz, die ich zuerst beim phf-crate gesehen habe.
fn phf_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014-2022", "Steven Fackler, Yuki Okushi")],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim palette-crate gesehen habe.
fn palette_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2015", "Erik Hedvall")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz, die ich zuerst beim freetype-crate gesehen habe.
fn freetype_lizenz(ende: MITEnde) -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014", "PistonDevelopers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        ende,
    )
}

/// MIT-Lizenz, die ich zuerst beim x11rb-crate gesehen habe.
fn x11rb_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(false, "2019", "x11rb Contributers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim unicode-ccc-crate gesehen habe.
fn unicode_ccc_bidi_mirroring_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Reizner Evgeniy"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim toml-crate gesehen habe.
fn toml_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: None,
            voller_name: Some("Individual contributors"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz, die ich zuerst beim ttf-parser-crate gesehen habe.
fn ttf_parser_lizenz(jahr: &'static str, ende_neue_zeilen: u8) -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, jahr, "Yevhenii Reizner")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde { punkt: true, neue_zeile: ende_neue_zeilen },
    )
}

/// MIT-Lizenz, die ich zuerst beim dyn-clonable-crate gesehen habe.
fn dyn_clonable_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2022"),
            voller_name: Some("Jacob Brown <kardeiz@gmail.com>"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz, die ich zuerst beim object-crate gesehen habe.
fn gimli_developers_lizenz(großes_g: bool, jahr: &'static str) -> Cow<'static, str> {
    let name = if großes_g { "The Gimli Developers" } else { "The gimli Developers" };
    mit(
        None,
        vec![MITCopyright { c_in_klammern: true, jahr: Some(jahr), voller_name: Some(name) }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `android_glue`-crates.
fn android_glue_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright::braces(),
        &ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
        1,
    )
}

/// MIT-Lizenz des `ansi_term`-crates.
fn ansi_term_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014", "Benjamin Sago")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `atomic-polyfill`-crates.
fn atomic_polyfill_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2020", "Dario Nieuwenhuis")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `atty`-crates.
fn atty_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015-2019", "Doug Tangren")],
        None,
        MITZeilenumbruch::Redox,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `autocfg`-crates.
fn autocfg_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "Josh Stone")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `bincode`-crates.
fn bincode_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014", "Ty Overby")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `bitfield`-crates.
fn bitfield_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Loïc Damien")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `bit_field`-crates.
fn bit_field_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2016", "Philipp Oppermann")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `bumpalo`-crates.
fn bumpalo_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "Nick Fitzgerald")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `calloop`-crates.
fn calloop_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "Victor Berger")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `cfg_aliases`-crates.
fn cfg_aliases_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Katharos Technology")],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `clipboard_x11`-crates.
fn clipboard_x11_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(
            true,
            "2019",
            "quininer@live.com, Héctor Ramón, window_clipboard_x11 contributors",
        )],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `core-video-sys`-crates.
fn core_video_sys_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2018", "寧靜")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `cortex-m`-crates.
fn cortex_m_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2016", "Jorge Aparicio")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `difference`-crates.
fn difference_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2015", "Johann Hofmann")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `downcast-rs`-crates.
fn downcast_rs_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2020", "Ashish Myles and contributors")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `either`-crates.
fn either_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", None)],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `embed-resource`-crates.
fn embed_resource_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2017", "nabijaczleweli")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `flexi_logger`-crates.
fn flexi_logger_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "The AUTHORS")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `float_next_after`-crates.
fn float_next_after_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Scripta Qumranica Electronica")],
        MITInfix("Created by Bronson Brown-deVost", 2),
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `fnv`-crates.
fn fnv_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Contributors")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `gethostname`-crates.
fn gethostname_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright::standard(),
        &ApacheEinrückung {
            titel: "                              ",
            version: "                        ",
            url: "                     ",
            header: "",
            text: "   ",
            sub_text: "       ",
            finale_url: "\t",
        },
        true,
        1,
    )
}

/// MIT-Lizenz des `getrandom`-crates.
fn getrandom_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![
            MITCopyright::neu(true, "2018-2024", "The rust-random Project Developers"),
            MITCopyright::neu(true, "2014", "The Rust Project Developers"),
        ],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `glow`-crates.
fn glow_lizenz() -> Cow<'static, str> {
    mit(
        None,
        Vec::new(),
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `iced_aw`-crates.
fn iced_aw_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Kaiden42")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `ident_case`-crates.
fn ident_case_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        Vec::new(),
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT- und Apache-Lizenz des `chrono`-crates.
fn chrono_lizenz() -> Cow<'static, str> {
    let präfix = "Rust-chrono is dual-licensed under The MIT License [1] and
Apache 2.0 License [2]. Copyright (c) 2014--2017, Kang Seonghoon and
contributors.

Nota Bene: This is same as the Rust Project's own license.


[1]: <http://opensource.org/licenses/MIT>, which is reproduced below:
";
    let mit = mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014,", "Kang Seonghoon.")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    );
    let infix =
        "\n\n[2]: <http://www.apache.org/licenses/LICENSE-2.0>, which is reproduced below:\n";
    let apache = apache_2_0(
        false,
        &ApacheCopyright::standard(),
        &ApacheEinrückung {
            titel: "                              ",
            version: "                        ",
            url: "                     ",
            header: "",
            text: "   ",
            sub_text: "       ",
            finale_url: "\t",
        },
        true,
        0,
    );
    Cow::Owned(format!("{präfix}\n~~~~\n{mit}\n~~~~\n{infix}\n~~~~\n{apache}\n~~~~\n\n"))
}

/// BSD3-Lizenz des `instant`-crates.
fn instant_lizenz() -> Cow<'static, str> {
    bsd_3(
        vec![BSD3Copyright::neu("2019", true, "Sébastien Crozet", true)],
        BSD3Zeilenumbruch::Instant,
        &BSD3Darstellung {
            punkte: |i| Cow::Owned(format!("{i}.")),
            einrückung_punkte: "",
            einrückung_text: "   ",
            author: "author",
            copyright_holder: "HOLDER",
        },
    )
}

/// MIT-Lizenz des `itertools`-crates.
fn itertools_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", None)],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `jni-sys`-crates.
fn jni_sys_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "The rust-jni-sys Developers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `malloc_buf`-crates.
fn malloc_buf_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Steven Sheldon")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `memoffset`-crates.
fn memoffset_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Gilad Naaman")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `nom`-crates.
fn nom_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2014-2019", "Geoffroy Couprie")],
        None,
        MITZeilenumbruch::Redox,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `mio`-crates.
fn mio_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2014", "Carl Lerche and other MIO contributors")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `num_cpus`-crates.
fn num_cpus_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", None)],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `num_threads`-crates.
fn num_threads_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2021", "Jacob Pratt")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `objc`-crates.
fn objc_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, None, "Steven Sheldon")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `ordered-float`-crates.
fn ordered_float_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "Jonathan Reem")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `owned_ttf_parser`-crates.
fn owned_ttf_parser_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(
        false,
        &ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
        false,
        0,
    )
}

/// MIT-Lizenz des `percent-encoding`-crates.
fn percent_encoding_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2013-2022", "The rust-url developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `pin-utils`-crates.
fn pin_utils_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "The pin-utils authors")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `ppv-lite86`-crates.
fn ppv_lite86_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "The CryptoCorrosion Contributors")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `redox_syscall`-crates.
fn redox_syscall_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Redox OS Developers")],
        MITInfix("MIT License", 2),
        MITZeilenumbruch::Redox,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `riscv-target`-crates.
fn riscv_target_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2020", "Ilya Epifanov")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `rppal`-crates.
fn rppal_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017-2024", "Rene van der Meer")],
        None,
        MITZeilenumbruch::RPPal,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `rstar`-crates.
fn rstar_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "The rstar project developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `ryu`-crates.
fn ryu_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright::standard(),
        &ApacheEinrückung {
            titel: "                              ",
            version: "                        ",
            url: "                     ",
            header: "",
            text: "   ",
            sub_text: "       ",
            finale_url: "       ",
        },
        false,
        0,
    )
}

/// MIT-Lizenz des `scopeguard`-crates.
fn scopeguard_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(
            true,
            "2016-2019",
            r#"Ulrik Sverdrup "bluss" and scopeguard developers"#,
        )],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `semver-parser`-crates.
fn semver_parser_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2016", "Steve Klabnik")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `shared_library`-crates.
fn shared_library_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Pierre Krieger")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `slab`-crates.
fn slab_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "Carl Lerche")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `smallvec`-crates.
fn smallvec_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "The Servo Project Developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `smithay-clipboard`-crates.
fn smithay_clipboard_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2018", "Lucas Timmins & Victor Berger")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `spin`-crates.
fn spin_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014", "Mathijs van de Nes")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `stable_deref_trait`-crates.
fn stable_deref_trait_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "Robert Grosse")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `static_assertions`-crates.
fn static_assertions_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2017", "Nikolai Vazquez")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `strsim`-crates.
fn strsim_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![
            MITCopyright::neu(true, "2015", "Danny Guo"),
            MITCopyright::neu(true, "2016", "Titus Wormer <tituswormer@gmail.com>"),
            MITCopyright::neu(true, "2018", "Akash Kurdekar"),
        ],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `take_mut`-crates.
fn take_mut_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2016", "Sgeo")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `tinyvec_macros`-crates.
fn tinyvec_macros_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Soveu")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `twox-hash`-crates.
fn twox_hash_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2015", "Jake Goulding")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `unicase`-crates.
fn unicase_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2014-2017", "Sean McArthur")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `version_check`-crates.
fn version_check_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 1),
        vec![MITCopyright::neu(true, "2017-2018", "Sergio Benitez")],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `void`-crates.
fn void_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "The rust-void Developers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `wasm-timer`-crates.
fn wasm_timer_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![
            MITCopyright::neu(false, "2019", "Pierre Krieger"),
            MITCopyright::neu(true, "2019", "Tokio Contributors"),
        ],
        None,
        MITZeilenumbruch::WasmTimer,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `window_clipboard`-crates.
fn window_clipboard_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(false, "2019", "Héctor Ramón, window_clipboard contributors")],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `winit`-crates.
fn winit_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright::braces(),
        &ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
        0,
    )
}

/// MIT-Lizenz des `winreg`-crates.
fn winreg_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "Igor Shaula")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `xcursor`-crates.
fn xcursor_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2020", "Samuele Esposito")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `xml-rs`-crates.
fn xml_rs_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2014", "Vladimir Matveev")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `crc32fast`-crates.
fn crc32fast_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2018", "Sam Rijs, Alex Crichton and contributors")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `crossfont`-crates.
fn crossfont_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        &ApacheCopyright { brackets: "[]", jahr: "2020", voller_name: "The Alacritty Project" },
        &ApacheEinrückung {
            titel: "                              ",
            version: "                        ",
            url: "                     ",
            header: "",
            text: "   ",
            sub_text: "       ",
            finale_url: "   ",
        },
        true,
        1,
    )
}

/// MIT-Lizenz des `foreign-types-macros`-crates.
fn foreign_types_macros_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2017", "The foreign-types Developers")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `hashbrown`-crates.
fn hashbrown_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2016", "Amanieu d'Antras")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `iana-time-zone`-crates.
fn iana_time_zone_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2020", "Andrew D. Straw")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `indexmap`-crates.
fn indexmap_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2016--2017", None)],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `miniz_oxide`-crates.
fn miniz_oxide_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2017", "Frommi")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `nu-ansi-term`-crates.
fn nu_ansi_term_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![
            MITCopyright::neu(true, "2014", "Benjamin Sago"),
            MITCopyright::neu(true, "2021-2022", "The Nushell Project Developers"),
        ],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `overload`-crates.
fn overload_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2019", "Daniel Augusto Rizzi Salvadori")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `png`-crates.
fn png_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2015", "nwin")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `safe_arch`-crates.
fn safe_arch_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2023", r#"Daniel "Lokathor" Gee."#)],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        true,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `sctk-adwaita`-crates.
fn sctk_adwaita_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2022", "Bartłomiej Maryńczak")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `simd-adler32`-crates.
fn simd_adler32_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "[2021]", "[Marvin Countryman]")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT- or Apache-Lizenz des `siphasher`-crates.
fn siphasher_lizenz() -> Cow<'static, str> {
    Cow::Borrowed(
        "Copyright 2012-2016 The Rust Project Developers.
Copyright 2016-2023 Frank Denis.

Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
option.
",
    )
}

/// MIT-Lizenz des `windows-targets`-crates.
fn windows_targets_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, None, "Microsoft Corporation.")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::leerzeichen_4(),
        false,
        MITEnde { punkt: false, neue_zeile: 1 },
    )
}

/// MIT-Lizenz des `equivalent`-crates.
fn equivalent_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2016--2023", None)],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `fdeflate`-crates.
fn fdeflate_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        Vec::new(),
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `zerocopy`-crates.
fn zerocopy_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: false,
            jahr: Some("2023"),
            voller_name: Some("The Fuchsia Authors"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `zeno`-crates.
fn zeno_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Chad Brokaw"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `yazi`-crates.
fn yazi_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Chad Brokaw"),
        }],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `wayland-backend`-crates.
fn wayland_backend_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2015"),
            voller_name: Some("Elinor Berger"),
        }],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `unicode-script`-crates.
fn unicode_script_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2019"),
            voller_name: Some("Manish Goregaokar"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `sys-locale`-crates.
fn sys_locale_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2021"),
            voller_name: Some("1Password"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `swash`-crates.
fn swash_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Chad Brokaw"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `svg_fmt`-crates.
fn svg_fmt_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2019"),
            voller_name: Some("Nicolas Silva"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `softbuffer`-crates.
fn softbuffer_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: false,
            jahr: Some("2022"),
            voller_name: Some("Kirill Chibisov"),
        }],
        None,
        MITZeilenumbruch::Softbuffer,
        MITEinrückung::keine(),
        false,
        MITEnde::zwei_neue_zeilen(),
    )
}

/// MIT-Lizenz des `wayland-csd-frame`-crates.
fn wayland_csd_frame_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2023"),
            voller_name: Some("Kirill Chibisov"),
        }],
        None,
        MITZeilenumbruch::NonEmpty,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `calloop-wayland-source`-crates.
fn calloop_wayland_source_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2023"),
            voller_name: Some("Kirill Chibisov"),
        }],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `rustybuzz`-crates.
fn rustybuzz_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![
            MITCopyright {
                c_in_klammern: true,
                jahr: None,
                voller_name: Some("HarfBuzz developers"),
            },
            MITCopyright {
                c_in_klammern: true,
                jahr: Some("2020"),
                voller_name: Some("Evgeniy Reizner"),
            },
        ],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `renderdoc-sys`-crates.
fn renderdoc_sys_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2022"),
            voller_name: Some("Eyal Kalderon"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `rangemap`-crates.
fn rangemap_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: false,
            jahr: Some("2019"),
            voller_name: Some("Jeffrey Parsons"),
        }],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `quick-xml`-crates.
fn quick_xml_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2016"),
            voller_name: Some("Johann Tuffe"),
        }],
        None,
        MITZeilenumbruch::QuickXml,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des profiling-lizenz.
fn profiling_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Philip Degarmo and other contributors"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `naga`-crates.
fn naga_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("[yyyy]"),
            voller_name: Some("[name of copyright owner]"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `lru`-crates.
fn lru_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2016"),
            voller_name: Some("Jerome Froelich"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `kurbo`-crates.
fn kurbo_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2018"),
            voller_name: Some("Raph Levien"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `hassle-rs`-crates.
fn hassle_rs_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2018"),
            voller_name: Some("Jasper Bekkers"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `guillotiere`-crates.
fn guillotiere_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2019"),
            voller_name: Some("Nicolas Silva"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `gpu-allocator`-crates.
fn gpu_allocator_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2021"),
            voller_name: Some("Traverse Research B.V."),
        }],
        None,
        MITZeilenumbruch::Keine,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `fontdb`-crates.
fn fontdb_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Yevhenii Reizner"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `fast-srgb8`-crates.
fn fast_srgb8_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2021"),
            voller_name: Some("Thom Chiovoloni"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::ohne_neue_zeile(),
    )
}

/// MIT-Lizenz des `etagere`-crates.
fn etagere_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("Nicolas Silva"),
        }],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `errno`-crates.
fn errno_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2014"),
            voller_name: Some("Chris Wong"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `cosmic-text`-crates.
fn cosmic_text_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2022"),
            voller_name: Some("System76"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `com-rs`-crates.
fn com_rs_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2016"),
            voller_name: Some("Lee Jeffery"),
        }],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `codespan-reporting`-crates.
fn codespan_reporting_lizenz() -> Cow<'static, str> {
    apache_2_0(false, &ApacheCopyright::standard(), &ApacheEinrückung::eingerückt(), true, 1)
}

/// MIT-Lizenz des `ash`-crates.
fn ash_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright { c_in_klammern: true, jahr: Some("2016"), voller_name: Some("ASH") }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `aliasable`-crates.
fn aliasable_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2020"),
            voller_name: Some("James Dyson <avitex@wfxlabs.com>"),
        }],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `ahash`-crates.
fn ahash_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2018"),
            voller_name: Some("Tom Kaitchuck"),
        }],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// Apache-Lizenz des `unicode-linebreak`-crates.
fn unicode_linebreak_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(false, &ApacheCopyright::standard(), true, 1)
}

/// Apache-Lizenz des `unicode-general-category`-crates.
fn unicode_general_category_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(false, &ApacheCopyright::braces(), true, 1)
}

/// BSD-Lizenz des `arrayref`-crates.
fn arrayref_lizenz() -> Cow<'static, str> {
    bsd_2("2015", "David Roundy <roundyd@physics.oregonstate.edu>")
}

/// ISC-Lizenz des `libloading`-crates.
fn libloading_lizenz() -> Cow<'static, str> {
    isc(true, "2015", "Simonas Kazlauskas", ISCZeilenumbruch::Libloading)
}

/// MIT-Lizenz des `xkeysym`-crates.
fn xkeysym_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some("2022-2023"),
            voller_name: Some("John Nunley"),
        }],
        None,
        MITZeilenumbruch::NonEmpty,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `tracing`-crates.
fn tracing_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "Tokio Contributors")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `read-fonts`-crates.
fn read_fonts_lizenz() -> Cow<'static, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, "2019", "Colin Rothfels")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `enum-iterator`-crates.
fn enum_iterator_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2023", "Stephane Raux")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

/// MIT-Lizenz des `yansi`-crates.
fn yansi_lizenz(jahr: &str) -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 1),
        vec![MITCopyright {
            c_in_klammern: true,
            jahr: Some(jahr),
            voller_name: Some("Sergio Benitez"),
        }],
        None,
        MITZeilenumbruch::Iced,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

// Mindestens eine Zeile pro dependency notwendig!
#[allow(clippy::too_many_lines)]
/// Die Lizenzen aller in `Cargo.lock` erwähnten Open-Source Bibliotheken.
fn cargo_lock_lizenzen() -> HashMap<&'static str, Lizenz> {
    HashMap::from([
        ("SourceSerif4-Regular", Lizenz::neu(source_lizenz)),
        ("Bootstrap Icons", Lizenz::neu(bootstrap_lizenz)),
        // War über iced_graphics mit feature "font-fallback" eingebunden (dependency von iced_glow)
        ("Lato", Lizenz::neu(lato_lizenz)),
        ("ab_glyph", Lizenz::neu(|| ab_glyph_lizenz(false, 0))),
        ("ab_glyph_rasterizer", Lizenz::neu(|| ab_glyph_lizenz(true, 1))),
        ("aho-corasick", Lizenz::neu(|| memchr_lizenz("2015"))),
        ("android_glue", Lizenz::neu(android_glue_lizenz)),
        ("ansi_term", Lizenz::neu(ansi_term_lizenz)),
        ("approx", Lizenz::neu(apache_2_0_standard_eingerückt)),
        (
            "arrayvec",
            Lizenz {
                lizenz: || arrayvec_lizenz("2015-2023"),
                version_spezifisch: {
                    // force coercion from closure to fn()
                    let array: [(_, fn() -> _); 1] = [("0.5.2", || arrayvec_lizenz("2015-2017"))];
                    HashMap::from(array)
                },
            },
        ),
        ("atomic-polyfill", Lizenz::neu(atomic_polyfill_lizenz)),
        ("atty", Lizenz::neu(atty_lizenz)),
        ("autocfg", Lizenz::neu(autocfg_lizenz)),
        ("bare-metal", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        ("bare-metal", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        ("bincode", Lizenz::neu(bincode_lizenz)),
        ("bitfield", Lizenz::neu(bitfield_lizenz)),
        ("bitflags", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("bit_field", Lizenz::neu(bit_field_lizenz)),
        ("bumpalo", Lizenz::neu(bumpalo_lizenz)),
        ("bytemuck", Lizenz::neu(bytemuck_lizenz)),
        ("bytemuck_derive", Lizenz::neu(bytemuck_lizenz)),
        ("byteorder", Lizenz::neu(|| memchr_lizenz("2015"))),
        ("calloop", Lizenz::neu(calloop_lizenz)),
        ("camino", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cargo_metadata", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cargo-platform", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cc", Lizenz::neu(crichton_2014_lizenz)),
        ("cfg-if", Lizenz::neu(crichton_2014_lizenz)),
        ("cfg-if", Lizenz::neu(crichton_2014_lizenz)),
        ("cfg_aliases", Lizenz::neu(cfg_aliases_lizenz)),
        ("cgl", Lizenz::neu(mozilla_foundation_lizenz)),
        ("clipboard-win", Lizenz::neu(bsl_1_0)),
        ("clipboard_macos", Lizenz::neu(clipboard_apache_lizenz)),
        ("clipboard_wayland", Lizenz::neu(clipboard_apache_lizenz)),
        ("clipboard_x11", Lizenz::neu(clipboard_x11_lizenz)),
        ("cocoa", Lizenz::neu(mozilla_foundation_lizenz)),
        ("cocoa-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation-sys", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation-sys", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics-types", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-video-sys", Lizenz::neu(core_video_sys_lizenz)),
        ("cortex-m", Lizenz::neu(cortex_m_lizenz)),
        ("critical-section", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("crossbeam-channel", Lizenz::neu(crossbeam_lizenz)),
        ("crossbeam-deque", Lizenz::neu(crossbeam_lizenz)),
        ("crossbeam-epoch", Lizenz::neu(crossbeam_lizenz)),
        ("crossbeam-utils", Lizenz::neu(crossbeam_lizenz)),
        ("cty", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        ("current_platform", Lizenz::neu(|| mit_ohne_copyright(MITZeilenumbruch::Keine))),
        ("darling", Lizenz::neu(darling_lizenz)),
        ("darling_core", Lizenz::neu(darling_lizenz)),
        ("darling_macro", Lizenz::neu(darling_lizenz)),
        ("difference", Lizenz::neu(difference_lizenz)),
        ("dlib", Lizenz::neu(wayland_lizenz)),
        ("downcast-rs", Lizenz::neu(downcast_rs_lizenz)),
        ("either", Lizenz::neu(either_lizenz)),
        ("embed-resource", Lizenz::neu(embed_resource_lizenz)),
        ("embedded-hal", Lizenz::neu(|| mit_lizenz_aparicio("2017-2018"))),
        ("error-code", Lizenz::neu(bsl_1_0)),
        ("euclid", Lizenz::neu(mozilla_foundation_lizenz)),
        ("flexi_logger", Lizenz::neu(flexi_logger_lizenz)),
        ("float_next_after", Lizenz::neu(float_next_after_lizenz)),
        ("fnv", Lizenz::neu(fnv_lizenz)),
        ("foreign-types", Lizenz::neu(foreign_types_lizenz)),
        ("foreign-types-shared", Lizenz::neu(foreign_types_lizenz)),
        ("futures", Lizenz::neu(futures_lizenz)),
        ("futures-channel", Lizenz::neu(futures_lizenz)),
        ("futures-core", Lizenz::neu(futures_lizenz)),
        ("futures-executor", Lizenz::neu(futures_lizenz)),
        ("futures-io", Lizenz::neu(futures_lizenz)),
        ("futures-macro", Lizenz::neu(futures_lizenz)),
        ("futures-sink", Lizenz::neu(futures_lizenz)),
        ("futures-task", Lizenz::neu(futures_lizenz)),
        ("futures-util", Lizenz::neu(futures_lizenz)),
        ("gethostname", Lizenz::neu(gethostname_lizenz)),
        ("getrandom", Lizenz::neu(getrandom_lizenz)),
        ("glam", Lizenz::neu(mit_ohne_copyright_x11)),
        ("glob", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("glow", Lizenz::neu(glow_lizenz)),
        ("glutin", Lizenz::neu(glutin_lizenz)),
        ("glutin_egl_sys", Lizenz::neu(glutin_lizenz)),
        ("glutin_emscripten_sys", Lizenz::neu(glutin_lizenz)),
        ("glutin_gles2_sys", Lizenz::neu(glutin_lizenz)),
        ("glutin_glx_sys", Lizenz::neu(glutin_lizenz)),
        ("glutin_wgl_sys", Lizenz::neu(glutin_lizenz)),
        ("glyph_brush", Lizenz::neu(glyph_brush_lizenz)),
        ("glyph_brush_draw_cache", Lizenz::neu(glyph_brush_lizenz)),
        ("glyph_brush_layout", Lizenz::neu(glyph_brush_lizenz)),
        ("gl_generator", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("hash32", Lizenz::neu(|| mit_lizenz_aparicio("2018"))),
        ("heapless", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        ("heck", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("hermit-abi", Lizenz::neu(mit_ohne_copyright_x11)),
        ("iced", Lizenz::neu(iced_lizenz)),
        ("iced_core", Lizenz::neu(iced_lizenz)),
        ("iced_futures", Lizenz::neu(iced_lizenz)),
        ("iced_glow", Lizenz::neu(iced_lizenz)),
        ("iced_glutin", Lizenz::neu(iced_lizenz)),
        ("iced_graphics", Lizenz::neu(iced_lizenz)),
        ("iced_native", Lizenz::neu(iced_lizenz)),
        ("iced_renderer", Lizenz::neu(iced_lizenz)),
        ("iced_runtime", Lizenz::neu(iced_lizenz)),
        ("iced_style", Lizenz::neu(iced_lizenz)),
        ("iced_tiny_skia", Lizenz::neu(iced_lizenz)),
        ("iced_wgpu", Lizenz::neu(iced_lizenz)),
        ("iced_widget", Lizenz::neu(iced_lizenz)),
        ("iced_winit", Lizenz::neu(iced_lizenz)),
        ("iced_aw", Lizenz::neu(iced_aw_lizenz)),
        ("ident_case", Lizenz::neu(ident_case_lizenz)),
        ("instant", Lizenz::neu(instant_lizenz)),
        ("itertools", Lizenz::neu(itertools_lizenz)),
        ("itoa", Lizenz::neu(mit_ohne_copyright_x11)),
        ("jni-sys", Lizenz::neu(jni_sys_lizenz)),
        ("js-sys", Lizenz::neu(crichton_2014_lizenz)),
        ("khronos_api", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("kommandozeilen_argumente", Lizenz::neu(kommandozeilen_argumente_lizenz)),
        ("kommandozeilen_argumente_derive", Lizenz::neu(kommandozeilen_argumente_lizenz)),
        ("lazy_static", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        ("libc", Lizenz::neu(|| mit_rust_project_developers_lizenz("2014-2020"))),
        ("libloading", Lizenz::neu(libloading_lizenz)),
        ("libm", Lizenz::neu(|| mit_lizenz_aparicio("2018"))),
        ("linked-hash-map", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("lock_api", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("log", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("lyon", Lizenz::neu(lyon_lizenz)),
        ("lyon_algorithms", Lizenz::neu(lyon_lizenz)),
        ("lyon_geom", Lizenz::neu(lyon_lizenz)),
        ("lyon_path", Lizenz::neu(lyon_lizenz)),
        ("lyon_tessellation", Lizenz::neu(lyon_lizenz)),
        ("malloc_buf", Lizenz::neu(malloc_buf_lizenz)),
        ("memchr", Lizenz::neu(|| memchr_lizenz("2015"))),
        ("memmap2", Lizenz::neu(memmap2_lizenz)),
        ("memmap2", Lizenz::neu(memmap2_lizenz)),
        ("memoffset", Lizenz::neu(memoffset_lizenz)),
        ("minimal-lexical", Lizenz::neu(mit_ohne_copyright_x11)),
        ("mio", Lizenz::neu(mio_lizenz)),
        ("nb", Lizenz::neu(vcell_lizenz)),
        ("nb", Lizenz::neu(vcell_lizenz)),
        ("ndk", Lizenz::neu(ndk_lizenz)),
        ("ndk-context", Lizenz::neu(ndk_lizenz)),
        ("ndk-glue", Lizenz::neu(ndk_lizenz)),
        ("ndk-macro", Lizenz::neu(ndk_lizenz)),
        ("ndk-sys", Lizenz::neu(ndk_lizenz)),
        ("nix", Lizenz::neu(nix_lizenz)),
        ("nix", Lizenz::neu(nix_lizenz)),
        ("nom", Lizenz::neu(nom_lizenz)),
        ("nonempty", Lizenz::neu(nonempty_lizenz)),
        ("num-traits", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("num_cpus", Lizenz::neu(num_cpus_lizenz)),
        ("num_enum", Lizenz::neu(mit_ohne_copyright_x11)),
        ("num_enum_derive", Lizenz::neu(mit_ohne_copyright_x11)),
        ("num_threads", Lizenz::neu(num_threads_lizenz)),
        ("objc", Lizenz::neu(objc_lizenz)),
        ("once_cell", Lizenz::neu(mit_ohne_copyright_x11)),
        ("ordered-float", Lizenz::neu(ordered_float_lizenz)),
        ("osmesa-sys", Lizenz::neu(cc_0)),
        ("owned_ttf_parser", Lizenz::neu(owned_ttf_parser_lizenz)),
        ("parking_lot", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot_core", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot_core", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("percent-encoding", Lizenz::neu(percent_encoding_lizenz)),
        ("pin-project-lite", Lizenz::neu(mit_ohne_copyright_x11)),
        ("pin-utils", Lizenz::neu(pin_utils_lizenz)),
        ("pkg-config", Lizenz::neu(crichton_2014_lizenz)),
        ("ppv-lite86", Lizenz::neu(ppv_lite86_lizenz)),
        ("proc-macro-crate", Lizenz::neu(mit_ohne_copyright_x11)),
        ("proc-macro2", Lizenz::neu(mit_ohne_copyright_x11)),
        ("proc-macro2-diagnostics", Lizenz::neu(|| yansi_lizenz("2016-2020"))),
        ("quote", Lizenz::neu(mit_ohne_copyright_x11)),
        ("rand", Lizenz::neu(rand_lizenz)),
        ("rand_chacha", Lizenz::neu(rand_lizenz)),
        ("rand_core", Lizenz::neu(rand_lizenz)),
        ("raw-window-handle", Lizenz::neu(raw_window_handle_lizenz)),
        ("raw-window-handle", Lizenz::neu(raw_window_handle_lizenz)),
        ("rayon", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        ("rayon-core", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        ("redox_syscall", Lizenz::neu(redox_syscall_lizenz)),
        ("regex", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("regex-syntax", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        (
            "riscv",
            Lizenz::neu(|| isc(false, "2019-2020", "[RISC-V team][team]", ISCZeilenumbruch::Riscv)),
        ),
        ("riscv-target", Lizenz::neu(riscv_target_lizenz)),
        ("rppal", Lizenz::neu(rppal_lizenz)),
        ("rstar", Lizenz::neu(rstar_lizenz)),
        ("rustc-hash", Lizenz::neu(mit_ohne_copyright_x11)),
        ("rustc_version", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("rustc_version", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("rustversion", Lizenz::neu(mit_ohne_copyright_x11)),
        ("ryu", Lizenz::neu(ryu_lizenz)),
        ("scoped-tls", Lizenz::neu(crichton_2014_lizenz)),
        ("scopeguard", Lizenz::neu(scopeguard_lizenz)),
        ("semver", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("semver", Lizenz::neu(mit_ohne_copyright_x11)),
        ("semver-parser", Lizenz::neu(semver_parser_lizenz)),
        ("serde", Lizenz::neu(serde_lizenz)),
        ("serde_derive", Lizenz::neu(serde_lizenz)),
        ("serde_json", Lizenz::neu(serde_lizenz)),
        ("shared_library", Lizenz::neu(shared_library_lizenz)),
        ("slab", Lizenz::neu(slab_lizenz)),
        ("slotmap", Lizenz::neu(|| zlib("2021", "Orson Peters <orsonpeters@gmail.com>"))),
        ("smallvec", Lizenz::neu(smallvec_lizenz)),
        ("smithay-client-toolkit", Lizenz::neu(smithay_client_toolkit_lizenz)),
        ("smithay-client-toolkit", Lizenz::neu(smithay_client_toolkit_lizenz)),
        ("smithay-clipboard", Lizenz::neu(smithay_clipboard_lizenz)),
        ("spin", Lizenz::neu(spin_lizenz)),
        ("stable_deref_trait", Lizenz::neu(stable_deref_trait_lizenz)),
        ("static_assertions", Lizenz::neu(static_assertions_lizenz)),
        ("str-buf", Lizenz::neu(bsl_1_0)),
        ("strsim", Lizenz::neu(strsim_lizenz)),
        ("syn", Lizenz::neu(mit_ohne_copyright_x11)),
        ("take_mut", Lizenz::neu(take_mut_lizenz)),
        ("thiserror", Lizenz::neu(mit_ohne_copyright_x11)),
        ("thiserror-impl", Lizenz::neu(mit_ohne_copyright_x11)),
        ("time", Lizenz::neu(time_lizenz)),
        ("time-macros", Lizenz::neu(time_lizenz)),
        ("tinyvec", Lizenz::neu(|| mit_ohne_copyright(MITZeilenumbruch::Keine))),
        ("tinyvec_macros", Lizenz::neu(tinyvec_macros_lizenz)),
        ("ttf-parser", Lizenz::neu(|| ttf_parser_lizenz("2018", 2))),
        ("twox-hash", Lizenz::neu(twox_hash_lizenz)),
        ("unicase", Lizenz::neu(unicase_lizenz)),
        ("unicode-ident", Lizenz::neu(mit_ohne_copyright_x11)),
        ("unicode-normalization", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("unicode-segmentation", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("vcell", Lizenz::neu(vcell_lizenz)),
        ("version_check", Lizenz::neu(version_check_lizenz)),
        ("void", Lizenz::neu(void_lizenz)),
        ("volatile-register", Lizenz::neu(|| mit_lizenz_aparicio("2016"))),
        ("vswhom", Lizenz::neu(vswwhom_lizenz)),
        ("vswhom-sys", Lizenz::neu(vswwhom_lizenz)),
        ("wasi", Lizenz::neu(mit_ohne_copyright_x11)),
        ("wasm-bindgen", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-bindgen-backend", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-bindgen-futures", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-bindgen-macro", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-bindgen-macro-support", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-bindgen-shared", Lizenz::neu(crichton_2014_lizenz)),
        ("wasm-timer", Lizenz::neu(wasm_timer_lizenz)),
        (
            "wayland-client",
            Lizenz {
                lizenz: wayland_scanner_lizenz,
                version_spezifisch: {
                    // for some reason, there is a compile-error when used directly
                    let array: [(_, fn() -> _); 2] =
                        [("0.29.5", wayland_lizenz), ("0.30.2", wayland_lizenz)];
                    HashMap::from(array)
                },
            },
        ),
        ("wayland-commons", Lizenz::neu(wayland_lizenz)),
        (
            "wayland-cursor",
            Lizenz {
                lizenz: wayland_scanner_lizenz,
                version_spezifisch: {
                    // for some reason, there is a compile-error when used directly
                    let array: [(_, fn() -> _); 1] = [("0.29.5", wayland_lizenz)];
                    HashMap::from(array)
                },
            },
        ),
        ("wayland-egl", Lizenz::neu(wayland_lizenz)),
        (
            "wayland-protocols",
            Lizenz {
                lizenz: wayland_scanner_lizenz,
                version_spezifisch: {
                    // for some reason, there is a compile-error when used directly
                    let array: [(_, fn() -> _); 1] = [("0.29.5", wayland_lizenz)];
                    HashMap::from(array)
                },
            },
        ),
        ("wayland-protocols-wlr", Lizenz::neu(wayland_scanner_lizenz)),
        (
            "wayland-scanner",
            Lizenz {
                lizenz: wayland_scanner_lizenz,
                version_spezifisch: {
                    // for some reason, there is a compile-error when used directly
                    let array: [(_, fn() -> _); 2] =
                        [("0.29.5", wayland_lizenz), ("0.30.1", wayland_lizenz)];
                    HashMap::from(array)
                },
            },
        ),
        ("wayland-sys", Lizenz::neu(wayland_lizenz)),
        ("wayland-csd-frame", Lizenz::neu(wayland_csd_frame_lizenz)),
        ("web-sys", Lizenz::neu(crichton_2014_lizenz)),
        ("winapi", Lizenz::neu(winapi_lizenz)),
        ("winapi-i686-pc-windows-gnu", Lizenz::neu(winapi_lizenz)),
        ("winapi-wsapoll", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("winapi-x86_64-pc-windows-gnu", Lizenz::neu(winapi_lizenz)),
        ("winapi-util", Lizenz::neu(|| memchr_lizenz("2017"))),
        ("windows", Lizenz::neu(widows_sys_lizenz)),
        ("windows-sys", Lizenz::neu(widows_sys_lizenz)),
        ("windows_aarch64_msvc", Lizenz::neu(widows_sys_lizenz)),
        ("windows_i686_gnu", Lizenz::neu(widows_sys_lizenz)),
        ("windows_i686_msvc", Lizenz::neu(widows_sys_lizenz)),
        ("windows_x86_64_gnu", Lizenz::neu(widows_sys_lizenz)),
        ("windows_x86_64_msvc", Lizenz::neu(widows_sys_lizenz)),
        ("window_clipboard", Lizenz::neu(window_clipboard_lizenz)),
        ("winit", Lizenz::neu(winit_lizenz)),
        ("winreg", Lizenz::neu(winreg_lizenz)),
        ("x11-dl", Lizenz::neu(mit_ohne_copyright_x11)),
        ("x11rb", Lizenz::neu(x11rb_lizenz)),
        ("x11rb-protocol", Lizenz::neu(x11rb_lizenz)),
        ("xcursor", Lizenz::neu(xcursor_lizenz)),
        ("xi-unicode", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("xml-rs", Lizenz::neu(xml_rs_lizenz)),
        ("adler", Lizenz::neu(mit_ohne_copyright_x11)),
        ("arrayref", Lizenz::neu(arrayref_lizenz)),
        ("chrono", Lizenz::neu(chrono_lizenz)),
        ("cmake", Lizenz::neu(crichton_2014_lizenz)),
        ("crc32fast", Lizenz::neu(crc32fast_lizenz)),
        ("crossfont", Lizenz::neu(crossfont_lizenz)),
        ("find-crate", Lizenz::neu(mit_ohne_copyright_x11)),
        ("flate2", Lizenz::neu(crichton_2014_lizenz)),
        ("foreign-types-macros", Lizenz::neu(foreign_types_macros_lizenz)),
        ("freetype-rs", Lizenz::neu(|| freetype_lizenz(MITEnde::ohne_neue_zeile()))),
        ("freetype-sys", Lizenz::neu(|| freetype_lizenz(MITEnde::zwei_neue_zeilen()))),
        ("hashbrown", Lizenz::neu(hashbrown_lizenz)),
        ("iana-time-zone", Lizenz::neu(iana_time_zone_lizenz)),
        ("indexmap", Lizenz::neu(indexmap_lizenz)),
        ("io-lifetimes", Lizenz::neu(mit_ohne_copyright_x11)),
        ("is-terminal", Lizenz::neu(mit_ohne_copyright_x11)),
        ("linux-raw-sys", Lizenz::neu(mit_ohne_copyright_x11)),
        ("miniz_oxide", Lizenz::neu(miniz_oxide_lizenz)),
        ("nu-ansi-term", Lizenz::neu(nu_ansi_term_lizenz)),
        ("num-integer", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("overload", Lizenz::neu(overload_lizenz)),
        ("palette", Lizenz::neu(palette_lizenz)),
        ("palette_derive", Lizenz::neu(palette_lizenz)),
        ("palettef_shared", Lizenz::neu(palette_lizenz)),
        ("phf", Lizenz::neu(phf_lizenz)),
        ("phf_generator", Lizenz::neu(phf_lizenz)),
        ("phf_macros", Lizenz::neu(phf_lizenz)),
        ("phf_shared", Lizenz::neu(phf_lizenz)),
        ("png", Lizenz::neu(png_lizenz)),
        ("rustix", Lizenz::neu(mit_ohne_copyright_x11)),
        ("safe_arch", Lizenz::neu(safe_arch_lizenz)),
        ("sctk-adwaita", Lizenz::neu(sctk_adwaita_lizenz)),
        ("serde_spanned", Lizenz::neu(toml_lizenz)),
        ("servo-fontconfig", Lizenz::neu(mozilla_foundation_lizenz)),
        ("servo-fontconfig-sys", Lizenz::neu(servo_fontconfig_sys)),
        ("simd-adler32", Lizenz::neu(simd_adler32_lizenz)),
        ("siphasher", Lizenz::neu(siphasher_lizenz)),
        ("tiny-skia", Lizenz::neu(tiny_skia_lizenz)),
        ("tiny-skia-path", Lizenz::neu(tiny_skia_lizenz)),
        ("toml_datetime", Lizenz::neu(toml_lizenz)),
        ("toml", Lizenz::neu(toml_lizenz)),
        ("toml_edit", Lizenz::neu(toml_lizenz)),
        ("vec_map", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("windows-targets", Lizenz::neu(windows_targets_lizenz)),
        ("winnow", Lizenz::neu(|| mit_ohne_copyright(MITZeilenumbruch::Redox))),
        ("equivalent", Lizenz::neu(equivalent_lizenz)),
        ("fdeflate", Lizenz::neu(fdeflate_lizenz)),
        ("widestring", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("zerocopy", Lizenz::neu(zerocopy_lizenz)),
        ("zeno", Lizenz::neu(zeno_lizenz)),
        ("yazi", Lizenz::neu(yazi_lizenz)),
        ("wgpu", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("wgpu-core", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("wgpu-hal", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("wgpu-types", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("wayland-backend", Lizenz::neu(wayland_backend_lizenz)),
        ("unicode-xid", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("unicode-width", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("unicode-script", Lizenz::neu(unicode_script_lizenz)),
        ("unicode-linebreak", Lizenz::neu(unicode_linebreak_lizenz)),
        ("unicode-general-category", Lizenz::neu(unicode_general_category_lizenz)),
        ("unicode-ccc", Lizenz::neu(unicode_ccc_bidi_mirroring_lizenz)),
        ("unicode-bidi-mirroring", Lizenz::neu(unicode_ccc_bidi_mirroring_lizenz)),
        ("unicode-bidi", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("termcolor", Lizenz::neu(|| memchr_lizenz("2015"))),
        ("sys-locale", Lizenz::neu(sys_locale_lizenz)),
        ("swash", Lizenz::neu(swash_lizenz)),
        ("svg_fmt", Lizenz::neu(svg_fmt_lizenz)),
        ("strict-num", Lizenz::neu(|| ttf_parser_lizenz("2022", 1))),
        ("spirv", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("softbuffer", Lizenz::neu(softbuffer_lizenz)),
        ("rustybuzz", Lizenz::neu(rustybuzz_lizenz)),
        ("rustc-demangle", Lizenz::neu(crichton_2014_lizenz)),
        ("renderdoc-sys", Lizenz::neu(renderdoc_sys_lizenz)),
        ("rangemap", Lizenz::neu(rangemap_lizenz)),
        ("range-alloc", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("quick-xml", Lizenz::neu(quick_xml_lizenz)),
        ("profiling", Lizenz::neu(profiling_lizenz)),
        ("object", Lizenz::neu(|| gimli_developers_lizenz(true, "2015"))),
        ("naga", Lizenz::neu(naga_lizenz)),
        ("lru", Lizenz::neu(lru_lizenz)),
        ("kurbo", Lizenz::neu(kurbo_lizenz)),
        ("khronos-egl", Lizenz::neu(mit_ohne_copyright_x11)),
        ("jobserver", Lizenz::neu(crichton_2014_lizenz)),
        ("hexf-parse", Lizenz::neu(cc_0)),
        ("hassle-rs", Lizenz::neu(hassle_rs_lizenz)),
        ("half", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("guillotiere", Lizenz::neu(guillotiere_lizenz)),
        ("gpu-descriptor", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("gpu-descriptor-types", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("gpu-allocator", Lizenz::neu(gpu_allocator_lizenz)),
        ("gpu-alloc", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("gpu-alloc-types", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("glyphon", Lizenz::neu(mit_ohne_copyright_x11)),
        ("gimli", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("fontdb", Lizenz::neu(fontdb_lizenz)),
        ("fast-srgb8", Lizenz::neu(fast_srgb8_lizenz)),
        ("etagere", Lizenz::neu(etagere_lizenz)),
        ("errno", Lizenz::neu(errno_lizenz)),
        ("enum-iterator-derive", Lizenz::neu(|| bsd_0("2018-2022", "Stephane Raux"))),
        ("enum-iterator", Lizenz::neu(|| bsd_0("2018-2022", "Stephane Raux"))),
        ("dyn-clone", Lizenz::neu(mit_ohne_copyright_x11)),
        ("dyn-clonable", Lizenz::neu(dyn_clonable_lizenz)),
        ("dyn-clonable-impl", Lizenz::neu(dyn_clonable_lizenz)),
        ("cosmic-text", Lizenz::neu(cosmic_text_lizenz)),
        ("com-rs", Lizenz::neu(com_rs_lizenz)),
        ("d3d12", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("codespan-reporting", Lizenz::neu(codespan_reporting_lizenz)),
        ("bit-vec", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("bit-set", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("backtrace", Lizenz::neu(crichton_2014_lizenz)),
        ("ash", Lizenz::neu(ash_lizenz)),
        ("allocator-api2", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("aliasable", Lizenz::neu(aliasable_lizenz)),
        ("ahash", Lizenz::neu(ahash_lizenz)),
        ("addr2line", Lizenz::neu(|| gimli_developers_lizenz(false, "2016-2018"))),
        ("xkeysym", Lizenz::neu(xkeysym_lizenz)),
        ("associated_list", Lizenz::neu(associated_list_lizenz)),
        ("tracing-core", Lizenz::neu(tracing_lizenz)),
        ("tracing", Lizenz::neu(tracing_lizenz)),
        ("read-fonts", Lizenz::neu(read_fonts_lizenz)),
        ("font-types", Lizenz::neu(read_fonts_lizenz)),
        ("polling", Lizenz::neu(mit_ohne_copyright_x11)),
        ("parking", Lizenz::neu(mit_ohne_copyright_x11)),
        ("futures-lite", Lizenz::neu(mit_ohne_copyright_x11)),
        ("event-listener-strategy", Lizenz::neu(mit_ohne_copyright_x11)),
        ("event-listener", Lizenz::neu(mit_ohne_copyright_x11)),
        ("enum-iterator-derive", Lizenz::neu(enum_iterator_lizenz)),
        ("enum-iterator", Lizenz::neu(enum_iterator_lizenz)),
        ("cursor-icon", Lizenz::neu(wayland_csd_frame_lizenz)),
        ("concurrent-queue", Lizenz::neu(mit_ohne_copyright_x11)),
        ("calloop-wayland-source", Lizenz::neu(calloop_wayland_source_lizenz)),
        ("async-lock", Lizenz::neu(mit_ohne_copyright_x11)),
        ("async-io", Lizenz::neu(mit_ohne_copyright_x11)),
        ("yansi", Lizenz::neu(|| yansi_lizenz("2017"))),
        ("int-enum", Lizenz::neu(mit_ohne_copyright_x11)),
    ])
}

/// Die Lizenzen der verwendeter Open-Source Bibliotheken für das übergebene target.
fn verwendete_lizenzen_impl<K: Ord>(
    target_crates: HashMap<&'static str, NonEmpty<&'static str>>,
    mut erzeuge_key: impl FnMut(&'static str, &'static str) -> K,
) -> BTreeMap<K, fn() -> Cow<'static, str>> {
    let alle_lizenzen = cargo_lock_lizenzen();
    let fallback_lizenz = Lizenz::neu(mit_missing_note);
    target_crates
        .into_iter()
        .flat_map(|(name, versionen)| {
            if name.starts_with("zugkontrolle") {
                return Vec::new();
            }
            let lizenz = alle_lizenzen.get(name).unwrap_or(&fallback_lizenz);
            versionen
                .into_iter()
                .map(|version| {
                    let key = erzeuge_key(name, version);
                    let text = lizenz.lizenz_für_version(version);
                    (key, text)
                })
                .collect()
        })
        .collect()
}

/// Die Lizenzen der verwendeter Open-Source Bibliotheken für das übergebene target.
#[must_use]
fn verwendete_lizenzen(
    target_crates: HashMap<&'static str, NonEmpty<&'static str>>,
) -> LizenzenMap {
    verwendete_lizenzen_impl(target_crates, |name, version| {
        UniCaseOrd::neu(format!("{name}-{version}"))
    })
}

#[cfg(test)]
mod test;
