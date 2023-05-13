//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashSet},
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
    Aktuell(UniCaseOrd<&'static str>, fn() -> Cow<'static, str>),
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
    aktuell: Option<(UniCaseOrd<&'static str>, Cow<'static, str>)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    fn neu(
        aktuell_name: Option<UniCaseOrd<&'static str>>,
        lizenzen: &BTreeMap<UniCaseOrd<&'static str>, fn() -> Cow<'static, str>>,
    ) -> Self {
        let aktuell = aktuell_name
            .and_then(|name| lizenzen.get(&name).map(|f| (name, f())))
            .or_else(|| lizenzen.iter().next().map(|(name, f)| (*name, f())));
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
        Self::neu(&*TARGET_LIZENZEN, &None, scrollable_style)
    }

    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu<ScrollableStyle>(
        lizenzen: &'a BTreeMap<UniCaseOrd<&'static str>, fn() -> Cow<'static, str>>,
        aktuell: &'a Option<UniCaseOrd<&'static str>>,
        scrollable_style: ScrollableStyle,
    ) -> Self
    where
        ScrollableStyle: 'a + Clone,
        <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<ScrollableStyle>,
    {
        let erzeuge_zustand = || Zustand::neu(*aktuell, lizenzen);
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
        lizenzen: &'a BTreeMap<UniCaseOrd<&'static str>, fn() -> Cow<'static, str>>,
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
                if Some(*name) == aktuell_name {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(*name, *f))
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

#[inline(always)]
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

#[inline(always)]
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

/// Crates für das aktuelle target, ausgehend von `cargo --filter-platform <target> metadata`.
fn target_crates() -> HashSet<&'static str> {
    zugkontrolle_macros::target_crates!()
}

static TARGET_LIZENZEN: Lazy<BTreeMap<UniCaseOrd<&'static str>, fn() -> Cow<'static, str>>> =
    Lazy::new(|| verwendete_lizenzen(target_crates()));

// TODO Fehlende Lizenztexte suchen/Issues öffnen.
/// Die Lizenzen aller in `Cargo.lock` erwähnten Open-Source Bibliotheken.
const fn cargo_lock_lizenzen() -> [(&'static str, fn() -> Cow<'static, str>); 268] {
    let mit_rust_project_developers_lizenz_2010 = || mit_rust_project_developers_lizenz("2010");
    let mit_rust_project_developers_lizenz_2014 = || mit_rust_project_developers_lizenz("2014");
    let mit_rust_project_developers_lizenz_2015 = || mit_rust_project_developers_lizenz("2015");
    let mit_rust_project_developers_lizenz_2016 = || mit_rust_project_developers_lizenz("2016");
    let mozilla_foundation_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2012-2013", "Mozilla Foundation")],
            None,
            MITZeilenumbruch::X11,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let ab_glyph_lizenz = || {
        apache_2_0_eingerückt(
            false,
            ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
            true,
        )
    };
    let bytemuck_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            vec![MITCopyright::neu(true, "2019", "Daniel \"Lokathor\" Gee.")],
            None,
            MITZeilenumbruch::Keine,
            MITEinrückung::keine(),
            true,
            MITEnde::standard(),
        )
    };
    let clipboard_apache_lizenz = || {
        apache_2_0(
            false,
            ApacheCopyright::braces(),
            ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
            true,
        )
    };
    let crossbeam_lizenz = || {
        mit(
            MITPräfix("The MIT License (MIT)", 2),
            vec![MITCopyright::neu(true, "2019", "The Crossbeam Project Developers")],
            None,
            MITZeilenumbruch::X11,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let darling_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            vec![MITCopyright::neu(true, "2017", "Ted Driggs")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let foreign_types_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2017", "The foreign-types Developers")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let futures_lizenz = || {
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
    };
    let glutin_lizenz = || {
        apache_2_0(
            false,
            ApacheCopyright {
                brackets: "{}",
                jahr: "2020",
                voller_name: "The glutin contributors",
            },
            ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
            true,
        )
    };
    let glyph_brush_lizenz = || {
        apache_2_0_eingerückt(
            false,
            ApacheCopyright { brackets: "{}", jahr: "2017", voller_name: "Alex Butler" },
            true,
        )
    };
    let iced_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(false, "2019", "Héctor Ramón, Iced contributors")],
            None,
            MITZeilenumbruch::Iced,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let kommandozeilen_argumente_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            vec![MITCopyright::neu(true, "2022", "spamviech")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let lyon_lizenz = || {
        mit(
            MITPräfix("The MIT License (MIT)", 2),
            vec![MITCopyright::neu(true, "2013", "Nicolas Silva")],
            None,
            MITZeilenumbruch::Iced,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let memchr_lizenz = || {
        mit(
            MITPräfix("The MIT License (MIT)", 2),
            vec![MITCopyright::neu(true, "2015", "Andrew Gallant")],
            None,
            MITZeilenumbruch::Winreg,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let memmap2_lizenz = || {
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
    };
    let ndk_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            Vec::new(),
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let nix_lizenz = || {
        mit(
            MITPräfix("The MIT License (MIT)", 2),
            vec![MITCopyright::neu(true, "2015", "Carl Lerche + nix-rust Authors")],
            None,
            MITZeilenumbruch::Winreg,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let nonempty_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2019", "Alexis Sellier")],
            None,
            MITZeilenumbruch::NonEmpty,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let rand_lizenz = || {
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
    };
    let raw_window_handle_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            vec![MITCopyright::neu(true, "2019", "Osspial")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let serde_lizenz = || {
        mit(
            None,
            Vec::new(),
            None,
            MITZeilenumbruch::X11,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let smithay_client_toolkit_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2018", "Victor Berger")],
            None,
            MITZeilenumbruch::Winreg,
            MITEinrückung::keine(),
            false,
            MITEnde::ohne_neue_zeile(),
        )
    };
    let time_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2022", "Jacob Pratt et al.")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let vcell_lizenz = || mit_lizenz_aparicio("2017");
    let vswwhom_lizenz = || {
        mit(
            MITPräfix("The MIT License (MIT)", 2),
            vec![MITCopyright::neu(true, "2019", "nabijaczleweli")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::zwei_neue_zeilen(),
        )
    };
    let wasm_bindgen_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2014", "Alex Crichton")],
            None,
            MITZeilenumbruch::X11,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let wayland_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2015", "Victor Berger")],
            None,
            MITZeilenumbruch::Winreg,
            MITEinrückung::keine(),
            false,
            MITEnde::ohne_neue_zeile(),
        )
    };
    let winapi_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2015-2018", "The winapi-rs Developers")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
            false,
            MITEnde::standard(),
        )
    };
    let widows_sys_lizenz = || {
        mit(
            MITPräfix("MIT License", 2),
            vec![MITCopyright::neu(true, None, "Microsoft Corporation.")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::leerzeichen_4(),
            false,
            MITEnde { punkt: false, neue_zeile: 1 },
        )
    };
    [
        ("block-0.1.6", mit_missing_note),           // TODO
        ("dispatch-0.2.0", mit_missing_note),        // TODO
        ("glow_glyph-0.5.1", mit_missing_note),      // TODO
        ("objc-foundation-0.1.1", mit_missing_note), // TODO
        ("objc_id-0.1.1", mit_missing_note),         // TODO
        ("sid-0.6.1", mit_missing_note),             // TODO
        ("SourceSerif4-Regular", || {
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
        // Über iced_graphics mit feature "font-fallback" eingebunden (dependency von iced_glow)
        ("Lato", || {
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
        ("ab_glyph-0.2.15", ab_glyph_lizenz),
        ("ab_glyph_rasterizer-0.1.5", ab_glyph_lizenz),
        ("aho-corasick-0.7.18", memchr_lizenz),
        ("android_glue-0.2.3", || {
            apache_2_0(
                false,
                ApacheCopyright::braces(),
                ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
                true,
            )
        }),
        ("ansi_term-0.12.1", || {
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
        ("approx-0.5.1", apache_2_0_standard_eingerückt),
        ("arrayvec-0.5.2", || {
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
        ("atomic-polyfill-0.1.8", || {
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
        ("atty-0.2.14", || {
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
        ("autocfg-1.1.0", || {
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
        ("bare-metal-0.2.5", || mit_lizenz_aparicio("2017")),
        ("bare-metal-1.0.0", || mit_lizenz_aparicio("2017")),
        ("bincode-1.3.3", || {
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
        ("bitfield-0.13.2", || {
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
        ("bitflags-1.3.2", mit_rust_project_developers_lizenz_2014),
        ("bit_field-0.10.1", || {
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
        ("bumpalo-3.10.0", || {
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
        ("bytemuck-1.9.1", bytemuck_lizenz),
        ("bytemuck_derive-1.1.0", bytemuck_lizenz),
        ("byteorder-1.4.3", memchr_lizenz),
        ("calloop-0.9.3", || {
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
        ("camino-1.0.9", mit_ohne_copyright_x11),
        ("cargo_metadata-0.14.2", mit_ohne_copyright_x11),
        ("cargo-platform-0.1.2", mit_ohne_copyright_x11),
        ("cc-1.0.73", wasm_bindgen_lizenz),
        ("cfg-if-0.1.10", wasm_bindgen_lizenz),
        ("cfg-if-1.0.0", wasm_bindgen_lizenz),
        ("cfg_aliases-0.1.1", || {
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
        ("cgl-0.3.2", mozilla_foundation_lizenz),
        ("clipboard-win-4.4.1", bsl_1_0),
        ("clipboard_macos-0.1.0", clipboard_apache_lizenz),
        ("clipboard_wayland-0.2.0", clipboard_apache_lizenz),
        ("clipboard_x11-0.4.0", || {
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
        ("cocoa-0.24.0", mozilla_foundation_lizenz),
        ("cocoa-foundation-0.1.0", mozilla_foundation_lizenz),
        ("core-foundation-0.7.0", mozilla_foundation_lizenz),
        ("core-foundation-0.9.3", mozilla_foundation_lizenz),
        ("core-foundation-sys-0.7.0", mozilla_foundation_lizenz),
        ("core-foundation-sys-0.8.3", mozilla_foundation_lizenz),
        ("core-graphics-0.19.2", mozilla_foundation_lizenz),
        ("core-graphics-0.22.3", mozilla_foundation_lizenz),
        ("core-graphics-types-0.1.1", mozilla_foundation_lizenz),
        ("core-video-sys-0.1.4", || {
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
        ("cortex-m-0.7.5", || {
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
        ("critical-section-0.2.7", apache_2_0_standard_eingerückt),
        ("crossbeam-channel-0.5.5", crossbeam_lizenz),
        ("crossbeam-deque-0.8.1", crossbeam_lizenz),
        ("crossbeam-epoch-0.9.9", crossbeam_lizenz),
        ("crossbeam-utils-0.8.9", crossbeam_lizenz),
        ("cty-0.2.2", || mit_lizenz_aparicio("2017")),
        ("current_platform-0.2.0", || mit_ohne_copyright(MITZeilenumbruch::Keine)),
        ("darling-0.13.4", darling_lizenz),
        ("darling_core-0.13.4", darling_lizenz),
        ("darling_macro-0.13.4", darling_lizenz),
        ("difference-2.0.0", || {
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
        ("dlib-0.5.0", wayland_lizenz),
        ("downcast-rs-1.2.0", || {
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
        ("either-1.6.1", || {
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
        ("embed-resource-1.7.2", || {
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
        ("embedded-hal-0.2.7", || mit_lizenz_aparicio("2017-2018")),
        ("error-code-2.3.1", bsl_1_0),
        ("euclid-0.22.7", mozilla_foundation_lizenz),
        ("flexi_logger-0.22.5", || {
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
        ("float_next_after-0.1.5", || {
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
        ("fnv-1.0.7", || {
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
        ("foreign-types-0.3.2", foreign_types_lizenz),
        ("foreign-types-shared-0.1.1", foreign_types_lizenz),
        ("futures-0.3.21", futures_lizenz),
        ("futures-channel-0.3.21", futures_lizenz),
        ("futures-core-0.3.21", futures_lizenz),
        ("futures-executor-0.3.21", futures_lizenz),
        ("futures-io-0.3.21", futures_lizenz),
        ("futures-macro-0.3.21", futures_lizenz),
        ("futures-sink-0.3.21", futures_lizenz),
        ("futures-task-0.3.21", futures_lizenz),
        ("futures-util-0.3.21", futures_lizenz),
        ("gethostname-0.2.3", || {
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
        ("getrandom-0.2.7", || {
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
        ("glam-0.10.2", mit_ohne_copyright_x11),
        ("glob-0.3.0", mit_rust_project_developers_lizenz_2014),
        ("glow-0.11.2", || {
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
        ("glutin-0.28.0", glutin_lizenz),
        ("glutin_egl_sys-0.1.5", glutin_lizenz),
        ("glutin_emscripten_sys-0.1.1", glutin_lizenz),
        ("glutin_gles2_sys-0.1.5", glutin_lizenz),
        ("glutin_glx_sys-0.1.7", glutin_lizenz),
        ("glutin_wgl_sys-0.1.5", glutin_lizenz),
        ("glyph_brush-0.7.4", glyph_brush_lizenz),
        ("glyph_brush_draw_cache-0.1.5", glyph_brush_lizenz),
        ("glyph_brush_layout-0.2.3", glyph_brush_lizenz),
        ("gl_generator-0.14.0", apache_2_0_standard_eingerückt),
        ("hash32-0.2.1", || mit_lizenz_aparicio("2018")),
        ("heapless-0.7.14", || mit_lizenz_aparicio("2017")),
        ("heck-0.4.0", mit_rust_project_developers_lizenz_2015),
        ("hermit-abi-0.1.19", mit_ohne_copyright_x11),
        ("iced-0.4.2", iced_lizenz),
        ("iced_aw-0.2.0", || {
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
        ("iced_core-0.5.0", iced_lizenz),
        ("iced_futures-0.4.1", iced_lizenz),
        ("iced_glow-0.3.0", iced_lizenz),
        ("iced_glutin-0.3.0", iced_lizenz),
        ("iced_graphics-0.3.0", iced_lizenz),
        ("iced_native-0.5.1", iced_lizenz),
        ("iced_style-0.4.0", iced_lizenz),
        ("iced_winit-0.4.0", iced_lizenz),
        ("ident_case-1.0.1", || {
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
        ("instant-0.1.12", || bsd_3("2019", "Sébastien Crozet")),
        ("itertools-0.10.3", || {
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
        ("itoa-1.0.2", mit_ohne_copyright_x11),
        ("jni-sys-0.3.0", || {
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
        ("js-sys-0.3.58", wasm_bindgen_lizenz),
        ("khronos_api-3.1.0", apache_2_0_standard_eingerückt),
        ("kommandozeilen_argumente-0.2.1", kommandozeilen_argumente_lizenz),
        ("kommandozeilen_argumente_derive-0.2.0", kommandozeilen_argumente_lizenz),
        ("lazy_static-1.4.0", mit_rust_project_developers_lizenz_2010),
        ("libc-0.2.126", || mit_rust_project_developers_lizenz("2014-2020")),
        ("libloading-0.7.3", || {
            isc(true, "2015", "Simonas Kazlauskas", ISCZeilenumbruch::Libloading)
        }),
        ("libm-0.2.2", || mit_lizenz_aparicio("2018")),
        ("linked-hash-map-0.5.4", mit_rust_project_developers_lizenz_2015),
        ("lock_api-0.4.7", mit_rust_project_developers_lizenz_2016),
        ("log-0.4.17", mit_rust_project_developers_lizenz_2014),
        ("lyon-0.17.10", lyon_lizenz),
        ("lyon_algorithms-0.17.7", lyon_lizenz),
        ("lyon_geom-0.17.7", lyon_lizenz),
        ("lyon_path-0.17.7", lyon_lizenz),
        ("lyon_tessellation-0.17.10", lyon_lizenz),
        ("malloc_buf-0.0.6", || {
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
        ("memchr-2.5.0", memchr_lizenz),
        ("memmap2-0.3.1", memmap2_lizenz),
        ("memmap2-0.5.4", memmap2_lizenz),
        ("memoffset-0.6.5", || {
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
        ("minimal-lexical-0.2.1", mit_ohne_copyright_x11),
        ("mio-0.8.4", || {
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
        ("nb-0.1.3", vcell_lizenz),
        ("nb-1.0.0", vcell_lizenz),
        ("ndk-0.5.0", ndk_lizenz),
        ("ndk-context-0.1.1", ndk_lizenz),
        ("ndk-glue-0.5.2", ndk_lizenz),
        ("ndk-macro-0.3.0", ndk_lizenz),
        ("ndk-sys-0.2.2", ndk_lizenz),
        ("nix-0.22.3", nix_lizenz),
        ("nix-0.24.1", nix_lizenz),
        ("nom-7.1.1", || {
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
        ("nonempty-0.8.0", nonempty_lizenz),
        ("num-traits-0.2.15", mit_rust_project_developers_lizenz_2014),
        ("num_cpus-1.13.1", || {
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
        ("num_enum-0.5.7", mit_ohne_copyright_x11),
        ("num_enum_derive-0.5.7", mit_ohne_copyright_x11),
        ("num_threads-0.1.6", || {
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
        ("objc-0.2.7", || {
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
        ("once_cell-1.12.0", mit_ohne_copyright_x11),
        ("ordered-float-3.0.0", || {
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
        ("osmesa-sys-0.1.2", cc_0),
        ("owned_ttf_parser-0.15.0", || {
            apache_2_0_eingerückt(
                false,
                ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
                true,
            )
        }),
        ("parking_lot-0.11.2", mit_rust_project_developers_lizenz_2016),
        ("parking_lot-0.12.1", mit_rust_project_developers_lizenz_2016),
        ("parking_lot_core-0.8.5", mit_rust_project_developers_lizenz_2016),
        ("parking_lot_core-0.9.3", mit_rust_project_developers_lizenz_2016),
        ("percent-encoding-2.1.0", || {
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
        ("pin-project-lite-0.2.9", mit_ohne_copyright_x11),
        ("pin-utils-0.1.0", || {
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
        ("pkg-config-0.3.25", wasm_bindgen_lizenz),
        ("ppv-lite86-0.2.16", || {
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
        ("proc-macro-crate-1.1.3", mit_ohne_copyright_x11),
        ("proc-macro2-1.0.40", wasm_bindgen_lizenz),
        ("quote-1.0.20", mit_rust_project_developers_lizenz_2016),
        ("rand-0.8.5", rand_lizenz),
        ("rand_chacha-0.3.1", rand_lizenz),
        ("rand_core-0.6.3", rand_lizenz),
        ("raw-window-handle-0.3.4", raw_window_handle_lizenz),
        ("raw-window-handle-0.4.3", raw_window_handle_lizenz),
        ("rayon-1.5.3", mit_rust_project_developers_lizenz_2010),
        ("rayon-core-1.9.3", mit_rust_project_developers_lizenz_2010),
        ("redox_syscall-0.2.13", || {
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
        ("regex-1.5.6", mit_rust_project_developers_lizenz_2014),
        ("regex-syntax-0.6.26", mit_rust_project_developers_lizenz_2014),
        ("riscv-0.7.0", || isc(false, "2019-2020", "[RISC-V team][team]", ISCZeilenumbruch::Riscv)),
        ("riscv-target-0.1.2", || {
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
        ("rppal-0.13.1", || {
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
        ("rstar-0.9.3", || {
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
        ("rustc-hash-1.1.0", mit_ohne_copyright_x11),
        ("rustc_version-0.2.3", mit_rust_project_developers_lizenz_2016),
        ("rustc_version-0.4.0", mit_rust_project_developers_lizenz_2016),
        ("rustversion-1.0.7", mit_ohne_copyright_x11),
        ("ryu-1.0.10", || apache_2_0_eingerückt(false, ApacheCopyright::standard(), true)),
        ("scoped-tls-1.0.0", wasm_bindgen_lizenz),
        ("scopeguard-1.1.0", || {
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
        ("semver-0.9.0", mit_rust_project_developers_lizenz_2014),
        ("semver-1.0.10", mit_ohne_copyright_x11),
        ("semver-parser-0.7.0", || {
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
        ("serde-1.0.137", serde_lizenz),
        ("serde_derive-1.0.137", serde_lizenz),
        ("serde_json-1.0.81", serde_lizenz),
        ("shared_library-0.1.9", || {
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
        ("slab-0.4.6", || {
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
        ("slotmap-1.0.6", || zlib("2021", "Orson Peters <orsonpeters@gmail.com>")),
        ("smallvec-1.8.0", || {
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
        ("smithay-client-toolkit-0.15.4", smithay_client_toolkit_lizenz),
        ("smithay-client-toolkit-0.16.0", smithay_client_toolkit_lizenz),
        ("smithay-clipboard-0.6.6", || {
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
        ("spin-0.9.3", || {
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
        ("stable_deref_trait-1.2.0", || {
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
        ("static_assertions-1.1.0", || {
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
        ("str-buf-1.0.6", bsl_1_0),
        ("strsim-0.10.0", || {
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
        ("syn-1.0.98", mit_ohne_copyright_x11),
        ("take_mut-0.2.2", || {
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
        ("thiserror-1.0.31", mit_ohne_copyright_x11),
        ("thiserror-impl-1.0.31", mit_ohne_copyright_x11),
        ("time-0.3.10", time_lizenz),
        ("time-macros-0.2.4", time_lizenz),
        ("tinyvec-1.6.0", || mit_ohne_copyright(MITZeilenumbruch::Keine)),
        ("tinyvec_macros-0.1.0", || {
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
        ("toml-0.5.9", wasm_bindgen_lizenz),
        ("ttf-parser-0.15.2", || {
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
        ("twox-hash-1.6.3", || {
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
        ("unicase-2.6.0", || {
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
        ("unicode-ident-1.0.1", mit_ohne_copyright_x11),
        ("unicode-normalization-0.1.19", mit_rust_project_developers_lizenz_2015),
        ("unicode-segmentation-1.9.0", mit_rust_project_developers_lizenz_2015),
        ("vcell-0.1.3", vcell_lizenz),
        ("version_check-0.9.4", || {
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
        ("void-1.0.2", || {
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
        ("volatile-register-0.2.1", || mit_lizenz_aparicio("2016")),
        ("vswhom-0.1.0", vswwhom_lizenz),
        ("vswhom-sys-0.1.1", vswwhom_lizenz),
        ("wasi-0.11.0+wasi-snapshot-preview1", mit_ohne_copyright_x11),
        ("wasm-bindgen-0.2.81", wasm_bindgen_lizenz),
        ("wasm-bindgen-backend-0.2.81", wasm_bindgen_lizenz),
        ("wasm-bindgen-futures-0.4.31", wasm_bindgen_lizenz),
        ("wasm-bindgen-macro-0.2.81", wasm_bindgen_lizenz),
        ("wasm-bindgen-macro-support-0.2.81", wasm_bindgen_lizenz),
        ("wasm-bindgen-shared-0.2.81", wasm_bindgen_lizenz),
        ("wasm-timer-0.2.5", || {
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
        ("wayland-client-0.29.4", wayland_lizenz),
        ("wayland-commons-0.29.4", wayland_lizenz),
        ("wayland-cursor-0.29.4", wayland_lizenz),
        ("wayland-egl-0.29.4", wayland_lizenz),
        ("wayland-protocols-0.29.4", wayland_lizenz),
        ("wayland-scanner-0.29.4", wayland_lizenz),
        ("wayland-sys-0.29.4", wayland_lizenz),
        ("web-sys-0.3.58", wasm_bindgen_lizenz),
        ("winapi-0.3.9", winapi_lizenz),
        ("winapi-i686-pc-windows-gnu-0.4.0", winapi_lizenz),
        ("winapi-wsapoll-0.1.1", apache_2_0_standard_eingerückt),
        ("winapi-x86_64-pc-windows-gnu-0.4.0", winapi_lizenz),
        ("windows-sys-0.36.1", widows_sys_lizenz),
        ("windows_aarch64_msvc-0.36.1", widows_sys_lizenz),
        ("windows_i686_gnu-0.36.1", widows_sys_lizenz),
        ("windows_i686_msvc-0.36.1", widows_sys_lizenz),
        ("windows_x86_64_gnu-0.36.1", widows_sys_lizenz),
        ("windows_x86_64_msvc-0.36.1", widows_sys_lizenz),
        ("window_clipboard-0.2.3", || {
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
        ("winit-0.26.1", || {
            apache_2_0(
                false,
                ApacheCopyright::braces(),
                ApacheEinrückung { titel: "", ..ApacheEinrückung::eingerückt() },
                false,
            )
        }),
        ("winreg-0.10.1", || {
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
        ("x11-dl-2.19.1", mit_ohne_copyright_x11),
        ("x11rb-0.9.0", || {
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
        ("xcursor-0.3.4", || {
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
        ("xi-unicode-0.3.0", apache_2_0_standard_eingerückt),
        ("xml-rs-0.8.4", || {
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
    ]
}

/// Die Lizenzen der verwendeter Open-Source Bibliotheken für das übergebene target.
pub fn verwendete_lizenzen(
    target_crates: HashSet<&'static str>,
) -> BTreeMap<UniCaseOrd<&'static str>, fn() -> Cow<'static, str>> {
    let alle_lizenzen = cargo_lock_lizenzen();
    alle_lizenzen
        .into_iter()
        .filter(|(name, _f)| target_crates.contains(*name))
        .map(|(name, f)| (UniCaseOrd::neu(name), f))
        .collect()
}

#[cfg(test)]
mod test;
