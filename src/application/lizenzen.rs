//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    ops::DerefMut,
};

use iced_native::{
    event,
    widget::{
        button::{self, Button},
        container::{self, Container},
        rule::{self, Rule},
        scrollable::{self, Scrollable},
        text::{self, Text},
        Column, Row, Space,
    },
    Element, Length, Renderer,
};
use nonempty::NonEmpty;
use once_cell::sync::Lazy;

use crate::{
    application::{
        map_mit_zustand::MapMitZustand,
        style::{
            self,
            linie::{Linie, TRENNLINIE},
        },
    },
    unicase_ord::UniCaseOrd,
};

pub mod texte;

use texte::{
    apache_2_0, apache_2_0_eingerückt, apache_2_0_standard_eingerückt, bsd_3, bsl_1_0, cc_0, isc,
    mit, mit_missing_note, mit_ohne_copyright, mit_ohne_copyright_x11, ofl_1_1, zlib,
    ApacheCopyright, ApacheEinrückung, ISCZeilenumbruch, MITCopyright, MITEinrückung, MITEnde,
    MITInfix, MITPräfix, MITZeilenumbruch,
};

#[derive(Debug, Clone)]
enum InterneNachricht {
    Aktuell(UniCaseOrd<String>, fn() -> Cow<'static, str>),
    Schließen,
}

/// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [Lizenzen]-Anzeige.
    Schließen,
}

/// Zustand eines [Lizenzen]-Widgets.
#[derive(Debug, PartialEq, Eq)]
struct Zustand {
    aktuell: Option<(UniCaseOrd<String>, Cow<'static, str>)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    fn neu(lizenzen: &BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>>) -> Self {
        let aktuell = lizenzen.iter().next().map(|(name, f)| (name.clone(), f()));
        Zustand { aktuell }
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
#[derive(Debug)]
pub struct Lizenzen<'a, R>(MapMitZustand<'a, Zustand, InterneNachricht, Nachricht, R>);

const PADDING: f32 = 5.;
const TRENNLINIE_BREITE: u16 = 1;

impl<'a, R> Lizenzen<'a, R>
where
    R: 'a + iced_native::text::Renderer,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + rule::StyleSheet
        + text::StyleSheet,
    <<R as Renderer>::Theme as rule::StyleSheet>::Style: From<Linie>,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    /// Erstelle ein neues [Lizenzen]-Widget mit den verwendeten Lizenzen.
    pub fn neu_mit_verwendeten_lizenzen<ScrollableStyle>(scrollable_style: ScrollableStyle) -> Self
    where
        ScrollableStyle: 'a + Clone,
        <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<ScrollableStyle>,
    {
        Self::neu(&*TARGET_LIZENZEN, scrollable_style)
    }

    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu<ScrollableStyle>(
        lizenzen: &'a BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>>,
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
                InterneNachricht::Aktuell(name, f) => {
                    zustand.aktuell = Some((name, f()));
                    Vec::new()
                },
                InterneNachricht::Schließen => vec![Nachricht::Schließen],
            }
        };
        Lizenzen(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    fn erzeuge_element<ScrollableStyle>(
        zustand: &Zustand,
        lizenzen: &'a BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>>,
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
        for (name, f) in lizenzen {
            buttons = buttons.push({
                let button = Button::new(Text::new(name.as_ref()));
                if Some(name) == aktuell_name.as_ref() {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name.clone(), *f))
                }
            });
        }
        let buttons = Scrollable::new(buttons).style(scrollable_style);
        // FIXME Schließen-Knopf nach den Buttons wird nicht angezeigt :(
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
                .push(Space::with_height(Length::Fixed(PADDING)))
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
    R: 'a + iced_native::text::Renderer,
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

fn ab_glyph_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(
        false,
        ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
        true,
    )
}

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

fn clipboard_apache_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        ApacheCopyright::braces(),
        ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
    )
}

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

fn glutin_lizenz() -> Cow<'static, str> {
    apache_2_0(
        false,
        ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "The glutin contributors" },
        ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
        true,
    )
}

fn glyph_brush_lizenz() -> Cow<'static, str> {
    apache_2_0_eingerückt(
        false,
        ApacheCopyright { brackets: "{}", jahr: "2017", voller_name: "Alex Butler" },
        true,
    )
}

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

fn kommandozeilen_argumente_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("MIT License", 2),
        vec![MITCopyright::neu(true, "2022", "spamviech")],
        None,
        MITZeilenumbruch::Standard,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

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

fn memchr_lizenz() -> Cow<'static, str> {
    mit(
        MITPräfix("The MIT License (MIT)", 2),
        vec![MITCopyright::neu(true, "2015", "Andrew Gallant")],
        None,
        MITZeilenumbruch::Winreg,
        MITEinrückung::keine(),
        false,
        MITEnde::standard(),
    )
}

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

fn vcell_lizenz() -> Cow<'static, str> {
    mit_lizenz_aparicio("2017")
}

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

fn wasm_bindgen_lizenz() -> Cow<'static, str> {
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

/// Crates für das aktuelle target, ausgehend von `cargo --filter-platform <target> metadata`.
fn target_crates() -> HashMap<&'static str, NonEmpty<&'static str>> {
    let mut crates: HashMap<&'static str, NonEmpty<&'static str>> = HashMap::new();
    for (name, version) in zugkontrolle_macros::target_crates!() {
        use std::collections::hash_map::Entry;
        match crates.entry(name) {
            Entry::Occupied(mut o) => o.get_mut().push(version),
            Entry::Vacant(v) => {
                let _ = v.insert(NonEmpty::singleton(version));
            },
        }
    }
    crates
}

static TARGET_LIZENZEN: Lazy<BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>>> =
    Lazy::new(|| verwendete_lizenzen(target_crates()));

struct Lizenz {
    lizenz: fn() -> Cow<'static, str>,
    version_spezifisch: HashMap<&'static str, fn() -> Cow<'static, str>>,
}

impl From<fn() -> Cow<'static, str>> for Lizenz {
    fn from(value: fn() -> Cow<'static, str>) -> Self {
        Lizenz::neu(value)
    }
}

impl Lizenz {
    fn neu(lizenz: fn() -> Cow<'static, str>) -> Self {
        Lizenz { lizenz, version_spezifisch: HashMap::new() }
    }

    fn lizenz_für_version(&self, version: &str) -> fn() -> Cow<'static, str> {
        *self.version_spezifisch.get(version).unwrap_or(&self.lizenz)
    }
}

// TODO Fehlende Lizenztexte suchen/Issues öffnen.
/// Die Lizenzen aller in `Cargo.lock` erwähnten Open-Source Bibliotheken.
fn cargo_lock_lizenzen() -> Vec<(&'static str, Lizenz)> {
    let mit_rust_project_developers_lizenz_2010 = || mit_rust_project_developers_lizenz("2010");
    let mit_rust_project_developers_lizenz_2014 = || mit_rust_project_developers_lizenz("2014");
    let mit_rust_project_developers_lizenz_2015 = || mit_rust_project_developers_lizenz("2015");
    let mit_rust_project_developers_lizenz_2016 = || mit_rust_project_developers_lizenz("2016");
    vec![
        ("block", Lizenz::neu(mit_missing_note)),           // TODO
        ("dispatch", Lizenz::neu(mit_missing_note)),        // TODO
        ("glow_glyph", Lizenz::neu(mit_missing_note)),      // TODO
        ("objc-foundation", Lizenz::neu(mit_missing_note)), // TODO
        ("objc_id", Lizenz::neu(mit_missing_note)),         // TODO
        ("sid", Lizenz::neu(mit_missing_note)),             // TODO
        (
            "SourceSerif4-Regular",
            Lizenz::neu(|| {
                let extra_notice = " All Rights Reserved. Source is a trademark of Adobe in the United States and/or other countries.";
                ofl_1_1(
                    false,
                    "2014-2021",
                    "Adobe (http://www.adobe.com/),",
                    "'Source'",
                    true,
                    extra_notice,
                    true,
                    false,
                    false,
                )
            }),
        ),
        // Über iced_graphics mit feature "font-fallback" eingebunden (dependency von iced_glow)
        (
            "Lato",
            Lizenz::neu(|| {
                ofl_1_1(
                    true,
                    "2010-2014",
                    "by tyPoland Lukasz Dziedzic (team@latofonts.com)",
                    "\"Lato\"",
                    false,
                    "",
                    false,
                    true,
                    true,
                )
            }),
        ),
        ("ab_glyph", Lizenz::neu(ab_glyph_lizenz)),
        ("ab_glyph_rasterizer", Lizenz::neu(ab_glyph_lizenz)),
        ("aho-corasick", Lizenz::neu(memchr_lizenz)),
        (
            "android_glue",
            Lizenz::neu(|| {
                apache_2_0(
                    false,
                    ApacheCopyright::braces(),
                    ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
                    true,
                )
            }),
        ),
        (
            "ansi_term",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2014", "Benjamin Sago")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("approx", Lizenz::neu(apache_2_0_standard_eingerückt)),
        (
            "arrayvec",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "Ulrik Sverdrup \"bluss\"", "2015-2017")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "atomic-polyfill",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2020", "Dario Nieuwenhuis")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "atty",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015-2019", "Doug Tangren")],
                    None,
                    MITZeilenumbruch::Redox,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "autocfg",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "Josh Stone")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("bare-metal", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        ("bare-metal", Lizenz::neu(|| mit_lizenz_aparicio("2017"))),
        (
            "bincode",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2014", "Ty Overby")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "bitfield",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Loïc Damien")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("bitflags", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        (
            "bit_field",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2016", "Philipp Oppermann")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "bumpalo",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2019", "Nick Fitzgerald")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("bytemuck", Lizenz::neu(bytemuck_lizenz)),
        ("bytemuck_derive", Lizenz::neu(bytemuck_lizenz)),
        ("byteorder", Lizenz::neu(memchr_lizenz)),
        (
            "calloop",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "Victor Berger")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("camino", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cargo_metadata", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cargo-platform", Lizenz::neu(mit_ohne_copyright_x11)),
        ("cc", Lizenz::neu(wasm_bindgen_lizenz)),
        ("cfg-if", Lizenz::neu(wasm_bindgen_lizenz)),
        ("cfg-if", Lizenz::neu(wasm_bindgen_lizenz)),
        (
            "cfg_aliases",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Katharos Technology")],
                    None,
                    MITZeilenumbruch::Keine,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("cgl", Lizenz::neu(mozilla_foundation_lizenz)),
        ("clipboard-win", Lizenz::neu(bsl_1_0)),
        ("clipboard_macos", Lizenz::neu(clipboard_apache_lizenz)),
        ("clipboard_wayland", Lizenz::neu(clipboard_apache_lizenz)),
        (
            "clipboard_x11",
            Lizenz::neu(|| {
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
            }),
        ),
        ("cocoa", Lizenz::neu(mozilla_foundation_lizenz)),
        ("cocoa-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation-sys", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-foundation-sys", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics", Lizenz::neu(mozilla_foundation_lizenz)),
        ("core-graphics-types", Lizenz::neu(mozilla_foundation_lizenz)),
        (
            "core-video-sys",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2018", "寧靜")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "cortex-m",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2016", "Jorge Aparicio")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
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
        (
            "difference",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2015", "Johann Hofmann")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::zwei_neue_zeilen(),
                )
            }),
        ),
        ("dlib", Lizenz::neu(wayland_lizenz)),
        (
            "downcast-rs",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2020", "Ashish Myles and contributors")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "either",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", None)],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "embed-resource",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2017", "nabijaczleweli")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("embedded-hal", Lizenz::neu(|| mit_lizenz_aparicio("2017-2018"))),
        ("error-code", Lizenz::neu(bsl_1_0)),
        ("euclid", Lizenz::neu(mozilla_foundation_lizenz)),
        (
            "flexi_logger",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "The AUTHORS")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "float_next_after",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Scripta Qumranica Electronica")],
                    MITInfix("Created by Bronson Brown-deVost", 2),
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "fnv",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Contributors")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
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
        (
            "gethostname",
            Lizenz::neu(|| {
                apache_2_0(
                    false,
                    ApacheCopyright::standard(),
                    ApacheEinrückung {
                        titel: "                              ",
                        version: "                        ",
                        url: "                     ",
                        header: "",
                        text: "   ",
                        sub_text: "       ",
                        finale_url: "\t",
                    },
                    true,
                )
            }),
        ),
        (
            "getrandom",
            Lizenz::neu(|| {
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
            }),
        ),
        ("glam", Lizenz::neu(mit_ohne_copyright_x11)),
        ("glob", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        (
            "glow",
            Lizenz::neu(|| {
                mit(
                    None,
                    Vec::new(),
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::ohne_neue_zeile(),
                )
            }),
        ),
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
        (
            "iced_aw",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Kaiden42")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("iced_core", Lizenz::neu(iced_lizenz)),
        ("iced_futures", Lizenz::neu(iced_lizenz)),
        ("iced_glow", Lizenz::neu(iced_lizenz)),
        ("iced_glutin", Lizenz::neu(iced_lizenz)),
        ("iced_graphics", Lizenz::neu(iced_lizenz)),
        ("iced_native", Lizenz::neu(iced_lizenz)),
        ("iced_style", Lizenz::neu(iced_lizenz)),
        ("iced_winit", Lizenz::neu(iced_lizenz)),
        (
            "ident_case",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    Vec::new(),
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("instant", Lizenz::neu(|| bsd_3("2019", "Sébastien Crozet"))),
        (
            "itertools",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", None)],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("itoa", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "jni-sys",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", "The rust-jni-sys Developers")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("js-sys", Lizenz::neu(wasm_bindgen_lizenz)),
        ("khronos_api", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("kommandozeilen_argumente", Lizenz::neu(kommandozeilen_argumente_lizenz)),
        ("kommandozeilen_argumente_derive", Lizenz::neu(kommandozeilen_argumente_lizenz)),
        ("lazy_static", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        ("libc", Lizenz::neu(|| mit_rust_project_developers_lizenz("2014-2020"))),
        (
            "libloading",
            Lizenz::neu(|| isc(true, "2015", "Simonas Kazlauskas", ISCZeilenumbruch::Libloading)),
        ),
        ("libm", Lizenz::neu(|| mit_lizenz_aparicio("2018"))),
        ("linked-hash-map", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("lock_api", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("log", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("lyon", Lizenz::neu(lyon_lizenz)),
        ("lyon_algorithms", Lizenz::neu(lyon_lizenz)),
        ("lyon_geom", Lizenz::neu(lyon_lizenz)),
        ("lyon_path", Lizenz::neu(lyon_lizenz)),
        ("lyon_tessellation", Lizenz::neu(lyon_lizenz)),
        (
            "malloc_buf",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Steven Sheldon")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("memchr", Lizenz::neu(memchr_lizenz)),
        ("memmap2", Lizenz::neu(memmap2_lizenz)),
        ("memmap2", Lizenz::neu(memmap2_lizenz)),
        (
            "memoffset",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Gilad Naaman")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::ohne_neue_zeile(),
                )
            }),
        ),
        ("minimal-lexical", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "mio",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2014", "Carl Lerche and other MIO contributors")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("nb", Lizenz::neu(vcell_lizenz)),
        ("nb", Lizenz::neu(vcell_lizenz)),
        ("ndk", Lizenz::neu(ndk_lizenz)),
        ("ndk-context", Lizenz::neu(ndk_lizenz)),
        ("ndk-glue", Lizenz::neu(ndk_lizenz)),
        ("ndk-macro", Lizenz::neu(ndk_lizenz)),
        ("ndk-sys", Lizenz::neu(ndk_lizenz)),
        ("nix", Lizenz::neu(nix_lizenz)),
        ("nix", Lizenz::neu(nix_lizenz)),
        (
            "nom",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2014-2019", "Geoffroy Couprie")],
                    None,
                    MITZeilenumbruch::Redox,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("nonempty", Lizenz::neu(nonempty_lizenz)),
        ("num-traits", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        (
            "num_cpus",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", None)],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::zwei_neue_zeilen(),
                )
            }),
        ),
        ("num_enum", Lizenz::neu(mit_ohne_copyright_x11)),
        ("num_enum_derive", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "num_threads",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2021", "Jacob Pratt")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "objc",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, None, "Steven Sheldon")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("once_cell", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "ordered-float",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", "Jonathan Reem")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("osmesa-sys", Lizenz::neu(cc_0)),
        (
            "owned_ttf_parser",
            Lizenz::neu(|| {
                apache_2_0_eingerückt(
                    false,
                    ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
                    true,
                )
            }),
        ),
        ("parking_lot", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot_core", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("parking_lot_core", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        (
            "percent-encoding",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2013-2016", "The rust-url developers")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("pin-project-lite", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "pin-utils",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "The pin-utils authors")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("pkg-config", Lizenz::neu(wasm_bindgen_lizenz)),
        (
            "ppv-lite86",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2019", "The CryptoCorrosion Contributors")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("proc-macro-crate", Lizenz::neu(mit_ohne_copyright_x11)),
        ("proc-macro2", Lizenz::neu(wasm_bindgen_lizenz)),
        ("quote", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("rand", Lizenz::neu(rand_lizenz)),
        ("rand_chacha", Lizenz::neu(rand_lizenz)),
        ("rand_core", Lizenz::neu(rand_lizenz)),
        ("raw-window-handle", Lizenz::neu(raw_window_handle_lizenz)),
        ("raw-window-handle", Lizenz::neu(raw_window_handle_lizenz)),
        ("rayon", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        ("rayon-core", Lizenz::neu(mit_rust_project_developers_lizenz_2010)),
        (
            "redox_syscall",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Redox OS Developers")],
                    MITInfix("MIT License", 2),
                    MITZeilenumbruch::Redox,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("regex", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("regex-syntax", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        (
            "riscv",
            Lizenz::neu(|| isc(false, "2019-2020", "[RISC-V team][team]", ISCZeilenumbruch::Riscv)),
        ),
        (
            "riscv-target",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2020", "Ilya Epifanov")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "rppal",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017-2021", "Rene van der Meer")],
                    None,
                    MITZeilenumbruch::RPPal,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "rstar",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "The rstar project developers")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("rustc-hash", Lizenz::neu(mit_ohne_copyright_x11)),
        ("rustc_version", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("rustc_version", Lizenz::neu(mit_rust_project_developers_lizenz_2016)),
        ("rustversion", Lizenz::neu(mit_ohne_copyright_x11)),
        ("ryu", Lizenz::neu(|| apache_2_0_eingerückt(false, ApacheCopyright::standard(), true))),
        ("scoped-tls", Lizenz::neu(wasm_bindgen_lizenz)),
        (
            "scopeguard",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(
                        true,
                        "2016-2019",
                        "Ulrik Sverdrup \"bluss\" and scopeguard developers",
                    )],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("semver", Lizenz::neu(mit_rust_project_developers_lizenz_2014)),
        ("semver", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "semver-parser",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2016", "Steve Klabnik")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("serde", Lizenz::neu(serde_lizenz)),
        ("serde_derive", Lizenz::neu(serde_lizenz)),
        ("serde_json", Lizenz::neu(serde_lizenz)),
        (
            "shared_library",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Pierre Krieger")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::ohne_neue_zeile(),
                )
            }),
        ),
        (
            "slab",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2019", "Carl Lerche")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("slotmap", Lizenz::neu(|| zlib("2021", "Orson Peters <orsonpeters@gmail.com>"))),
        (
            "smallvec",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "The Servo Project Developers")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("smithay-client-toolkit", Lizenz::neu(smithay_client_toolkit_lizenz)),
        ("smithay-client-toolkit", Lizenz::neu(smithay_client_toolkit_lizenz)),
        (
            "smithay-clipboard",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "Lucas Timmins & Victor Berger")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "spin",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2014", "Mathijs van de Nes")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::ohne_neue_zeile(),
                )
            }),
        ),
        (
            "stable_deref_trait",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2017", "Robert Grosse")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::ohne_neue_zeile(),
                )
            }),
        ),
        (
            "static_assertions",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2017", "Nikolai Vazquez")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("str-buf", Lizenz::neu(bsl_1_0)),
        (
            "strsim",
            Lizenz::neu(|| {
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
            }),
        ),
        ("syn", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "take_mut",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2016", "Sgeo")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::zwei_neue_zeilen(),
                )
            }),
        ),
        ("thiserror", Lizenz::neu(mit_ohne_copyright_x11)),
        ("thiserror-impl", Lizenz::neu(mit_ohne_copyright_x11)),
        ("time", Lizenz::neu(time_lizenz)),
        ("time-macros", Lizenz::neu(time_lizenz)),
        ("tinyvec", Lizenz::neu(|| mit_ohne_copyright(MITZeilenumbruch::Keine))),
        (
            "tinyvec_macros",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Soveu")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("toml", Lizenz::neu(wasm_bindgen_lizenz)),
        (
            "ttf-parser",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2018", "Yevhenii Reizner")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::zwei_neue_zeilen(),
                )
            }),
        ),
        (
            "twox-hash",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2015", "Jake Goulding")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "unicase",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2014-2017", "Sean McArthur")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::zwei_neue_zeilen(),
                )
            }),
        ),
        ("unicode-ident", Lizenz::neu(mit_ohne_copyright_x11)),
        ("unicode-normalization", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("unicode-segmentation", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        ("vcell", Lizenz::neu(vcell_lizenz)),
        (
            "version_check",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 1),
                    vec![MITCopyright::neu(true, "2017-2018", "Sergio Benitez")],
                    None,
                    MITZeilenumbruch::Iced,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "void",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", "The rust-void Developers")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("volatile-register", Lizenz::neu(|| mit_lizenz_aparicio("2016"))),
        ("vswhom", Lizenz::neu(vswwhom_lizenz)),
        ("vswhom-sys", Lizenz::neu(vswwhom_lizenz)),
        ("wasi", Lizenz::neu(mit_ohne_copyright_x11)),
        ("wasm-bindgen", Lizenz::neu(wasm_bindgen_lizenz)),
        ("wasm-bindgen-backend", Lizenz::neu(wasm_bindgen_lizenz)),
        ("wasm-bindgen-futures", Lizenz::neu(wasm_bindgen_lizenz)),
        ("wasm-bindgen-macro", Lizenz::neu(wasm_bindgen_lizenz)),
        ("wasm-bindgen-macro-support", Lizenz::neu(wasm_bindgen_lizenz)),
        ("wasm-bindgen-shared", Lizenz::neu(wasm_bindgen_lizenz)),
        (
            "wasm-timer",
            Lizenz::neu(|| {
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
            }),
        ),
        ("wayland-client", Lizenz::neu(wayland_lizenz)),
        ("wayland-commons", Lizenz::neu(wayland_lizenz)),
        ("wayland-cursor", Lizenz::neu(wayland_lizenz)),
        ("wayland-egl", Lizenz::neu(wayland_lizenz)),
        ("wayland-protocols", Lizenz::neu(wayland_lizenz)),
        ("wayland-scanner", Lizenz::neu(wayland_lizenz)),
        ("wayland-sys", Lizenz::neu(wayland_lizenz)),
        ("web-sys", Lizenz::neu(wasm_bindgen_lizenz)),
        ("winapi", Lizenz::neu(winapi_lizenz)),
        ("winapi-i686-pc-windows-gnu", Lizenz::neu(winapi_lizenz)),
        ("winapi-wsapoll", Lizenz::neu(apache_2_0_standard_eingerückt)),
        ("winapi-x86_64-pc-windows-gnu", Lizenz::neu(winapi_lizenz)),
        ("windows-sys", Lizenz::neu(widows_sys_lizenz)),
        ("windows_aarch64_msvc", Lizenz::neu(widows_sys_lizenz)),
        ("windows_i686_gnu", Lizenz::neu(widows_sys_lizenz)),
        ("windows_i686_msvc", Lizenz::neu(widows_sys_lizenz)),
        ("windows_x86_64_gnu", Lizenz::neu(widows_sys_lizenz)),
        ("windows_x86_64_msvc", Lizenz::neu(widows_sys_lizenz)),
        (
            "window_clipboard",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(
                        false,
                        "2019",
                        "Héctor Ramón, window_clipboard contributors",
                    )],
                    None,
                    MITZeilenumbruch::Iced,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "winit",
            Lizenz::neu(|| {
                apache_2_0(
                    false,
                    ApacheCopyright::braces(),
                    ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
                    false,
                )
            }),
        ),
        (
            "winreg",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(true, "2015", "Igor Shaula")],
                    None,
                    MITZeilenumbruch::Winreg,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("x11-dl", Lizenz::neu(mit_ohne_copyright_x11)),
        (
            "x11rb",
            Lizenz::neu(|| {
                mit(
                    None,
                    vec![MITCopyright::neu(false, "2019", "x11rb Contributers")],
                    None,
                    MITZeilenumbruch::X11,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        (
            "xcursor",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, "2020", "Samuele Esposito")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("xi-unicode", Lizenz::neu(apache_2_0_standard_eingerückt)),
        (
            "xml-rs",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("The MIT License (MIT)", 2),
                    vec![MITCopyright::neu(true, "2014", "Vladimir Matveev")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::keine(),
                    false,
                    MITEnde::standard(),
                )
            }),
        ),
        ("adler", Lizenz::neu(mit_missing_note)), // TODO new
        ("arrayref", Lizenz::neu(mit_missing_note)), // TODO new
        ("chrono", Lizenz::neu(mit_missing_note)), // TODO new
        ("cmake", Lizenz::neu(mit_missing_note)), // TODO new
        ("crc32fast", Lizenz::neu(mit_missing_note)), // TODO new
        ("crossfont", Lizenz::neu(mit_missing_note)), // TODO new
        ("expat-sys", Lizenz::neu(mit_missing_note)), // TODO new
        ("fdeflate", Lizenz::neu(mit_missing_note)), // TODO new
        ("find-crate", Lizenz::neu(mit_missing_note)), // TODO new
        ("flate2", Lizenz::neu(mit_missing_note)), // TODO new
        ("foreign-types-macros", Lizenz::neu(mit_missing_note)), // TODO new
        ("freetype-rs", Lizenz::neu(mit_missing_note)), // TODO new
        ("freetype-sys", Lizenz::neu(mit_missing_note)), // TODO new
        ("hashbrown", Lizenz::neu(mit_missing_note)), // TODO new
        ("iana-time-zone", Lizenz::neu(mit_missing_note)), // TODO new
        ("indexmap", Lizenz::neu(mit_missing_note)), // TODO new
        ("io-lifetimes", Lizenz::neu(mit_missing_note)), // TODO new
        ("is-terminal", Lizenz::neu(mit_missing_note)), // TODO new
        ("linux-raw-sys", Lizenz::neu(mit_missing_note)), // TODO new
        ("miniz_oxide", Lizenz::neu(mit_missing_note)), // TODO new
        ("nu-ansi-term", Lizenz::neu(mit_missing_note)), // TODO new
        ("num-integer", Lizenz::neu(mit_missing_note)), // TODO new
        ("overload", Lizenz::neu(mit_missing_note)), // TODO new
        ("palette", Lizenz::neu(mit_missing_note)), // TODO new
        ("palette_derive", Lizenz::neu(mit_missing_note)), // TODO new
        ("palettef_shared", Lizenz::neu(mit_missing_note)), // TODO new
        ("phf", Lizenz::neu(mit_missing_note)),   // TODO new
        ("phf_generator", Lizenz::neu(mit_missing_note)), // TODO new
        ("phf_macros", Lizenz::neu(mit_missing_note)), // TODO new
        ("phf_shared", Lizenz::neu(mit_missing_note)), // TODO new
        ("png", Lizenz::neu(mit_missing_note)),   // TODO new
        ("rustix", Lizenz::neu(mit_missing_note)), // TODO new
        ("safe_arch", Lizenz::neu(mit_missing_note)), // TODO new
        ("sctk-adwaita", Lizenz::neu(mit_missing_note)), // TODO new
        ("serde_spanned", Lizenz::neu(mit_missing_note)), // TODO new
        ("servo-fontconfig", Lizenz::neu(mit_missing_note)), // TODO new
        ("servo-fontconfig-sys", Lizenz::neu(mit_missing_note)), // TODO new
        ("simd-adler32", Lizenz::neu(mit_missing_note)), // TODO new
        ("siphasher", Lizenz::neu(mit_missing_note)), // TODO new
        ("tiny-skia", Lizenz::neu(mit_missing_note)), // TODO new
        ("tiny-skia-path", Lizenz::neu(mit_missing_note)), // TODO new
        ("toml_datetime", Lizenz::neu(mit_missing_note)), // TODO new
        ("toml_edit", Lizenz::neu(mit_missing_note)), // TODO new
        ("vec_map", Lizenz::neu(mit_rust_project_developers_lizenz_2015)),
        (
            "windows-targets",
            Lizenz::neu(|| {
                mit(
                    MITPräfix("MIT License", 2),
                    vec![MITCopyright::neu(true, None, "Microsoft Corporation.")],
                    None,
                    MITZeilenumbruch::Standard,
                    MITEinrückung::leerzeichen_4(),
                    false,
                    MITEnde { punkt: false, neue_zeile: 1 },
                )
            }),
        ),
        ("winnow", Lizenz::neu(|| mit_ohne_copyright(MITZeilenumbruch::Redox))),
    ]
}

fn verwendete_lizenzen_impl<K: Ord>(
    target_crates: HashMap<&'static str, NonEmpty<&'static str>>,
    mut erzeuge_key: impl FnMut(&'static str, &'static str) -> K,
) -> BTreeMap<K, fn() -> Cow<'static, str>> {
    let alle_lizenzen = cargo_lock_lizenzen();
    alle_lizenzen
        .into_iter()
        .flat_map(|(name, lizenz)| {
            target_crates.get(name).map_or_else(Vec::new, |versions| {
                versions
                    .into_iter()
                    .map(|version| (erzeuge_key(name, version), lizenz.lizenz_für_version(version)))
                    .collect()
            })
        })
        .collect()
}

/// Die Lizenzen der verwendeter Open-Source Bibliotheken für das übergebene target.
pub fn verwendete_lizenzen(
    target_crates: HashMap<&'static str, NonEmpty<&'static str>>,
) -> BTreeMap<UniCaseOrd<String>, fn() -> Cow<'static, str>> {
    verwendete_lizenzen_impl(target_crates, |name, version| {
        UniCaseOrd::neu(format!("{name}-{version}"))
    })
}

#[cfg(test)]
mod test;
