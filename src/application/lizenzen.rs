//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_native::{
    event::{self, Event},
    text,
    widget::{
        button::{self, Button},
        scrollable::{self, Scrollable},
        Column, Container, Row, Rule, Space, Text,
    },
    Clipboard, Element, Layout, Length, Point, Renderer, Shell, Widget,
};

use crate::application::{
    macros::reexport_no_event_methods,
    style::{hintergrund, linie::TRENNLINIE},
};

pub mod texte;

//FIXME entferne unbenutzte imports
#[allow(unused_imports)]
use texte::{
    apache_2_0, apache_2_0_eingerückt, apache_2_0_nicht_eingerückt, apache_2_0_standard_eingerückt,
    apache_2_0_standard_nicht_eingerückt, bsd_3, bsl_1_0, cc_0, isc, mit, mit_ohne_copyright,
    mit_ohne_copyright_x11, mit_plain, mpl_2_0, ofl_1_1, zlib, ApacheCopyright, ApacheEinrückung,
    ISCZeilenumbruch, MITCopyright, MITEinrückung, MITEnde, MITInfix, MITPräfix, MITZeilenumbruch,
};

#[derive(Debug, Clone)]
enum InterneNachricht {
    Aktuell(&'static str, fn() -> Cow<'static, str>),
    Schließen,
}

/// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [Lizenzen]-Anzeige.
    Schließen,
}

/// Zustand eines [Lizenzen]-Widgets.
#[derive(Debug)]
pub struct Zustand {
    lizenzen_und_button_states: BTreeMap<&'static str, (button::State, fn() -> Cow<'static, str>)>,
    scrollable_buttons: scrollable::State,
    scrollable_text: scrollable::State,
    scrollable_text_zurücksetzen: bool,
    schließen: button::State,
    aktuell: Option<(&'static str, Cow<'static, str>)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    pub fn neu(
        lizenzen: impl IntoIterator<Item = (&'static str, fn() -> Cow<'static, str>)>,
    ) -> Self {
        let mut aktuell = None;
        let lizenzen_und_button_states = lizenzen
            .into_iter()
            .map(|(name, f)| {
                if aktuell.is_none() {
                    aktuell = Some((name, f()));
                }
                (name, (button::State::new(), f))
            })
            .collect();
        Zustand {
            lizenzen_und_button_states,
            scrollable_buttons: scrollable::State::new(),
            scrollable_text: scrollable::State::new(),
            scrollable_text_zurücksetzen: false,
            schließen: button::State::new(),
            aktuell,
        }
    }

    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    #[inline(always)]
    pub fn neu_mit_verwendeten_lizenzen() -> Self {
        Self::neu(verwendete_lizenzen())
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
pub struct Lizenzen<'a, R> {
    container: Container<'a, InterneNachricht, R>,
    aktuell: &'a mut Option<(&'static str, Cow<'static, str>)>,
    scrollable_text_zurücksetzen: &'a mut bool,
}

impl<R> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen").field("row", &"<Row>").finish()
    }
}

const PADDING: u16 = 5;
const TRENNLINIE_BREITE: u16 = 1;

impl<'a, R: 'a + text::Renderer> Lizenzen<'a, R> {
    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu(
        zustand: &'a mut Zustand,
        scrollable_style: impl Into<Box<dyn scrollable::StyleSheet + 'a>>,
    ) -> Self {
        let Zustand {
            lizenzen_und_button_states,
            scrollable_buttons,
            scrollable_text,
            scrollable_text_zurücksetzen,
            schließen,
            aktuell,
        } = zustand;
        let mut buttons = Scrollable::new(scrollable_buttons)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        let (aktuell_name, aktuell_text) =
            if let Some((name, text)) = aktuell { (Some(*name), Some(text)) } else { (None, None) };
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            buttons = buttons.push({
                let button = Button::new(button_state, Text::new(name));
                if Some(name) == aktuell_name {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name, *f))
                }
            });
        }
        let column = Column::new()
            .push(buttons)
            .push(Space::with_height(Length::Units(PADDING)))
            .push(
                Button::new(schließen, Text::new("Schließen"))
                    .on_press(InterneNachricht::Schließen),
            )
            .width(Length::Shrink)
            .height(Length::Fill);
        if *scrollable_text_zurücksetzen {
            *scrollable_text = scrollable::State::new();
            *scrollable_text_zurücksetzen = false;
        }
        let mut scrollable_aktuell =
            Scrollable::new(scrollable_text).width(Length::Fill).height(Length::Fill);
        if let Some(aktuell_text) = aktuell_text {
            let text_mit_horizontalem_padding = Row::new()
                .push(Space::with_width(Length::Units(PADDING)))
                .push(Text::new(aktuell_text.as_ref()).width(Length::Fill).height(Length::Shrink))
                .push(Space::with_width(Length::Units(PADDING)))
                .width(Length::Fill)
                .height(Length::Shrink);
            scrollable_aktuell = scrollable_aktuell
                .push(Space::with_height(Length::Units(PADDING)))
                .push(text_mit_horizontalem_padding)
                .push(Space::with_height(Length::Units(PADDING)))
        }
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(TRENNLINIE_BREITE).style(TRENNLINIE))
                .push(scrollable_aktuell),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, aktuell, scrollable_text_zurücksetzen }
    }
}

impl<'a, R: Renderer> Widget<Nachricht, R> for Lizenzen<'a, R> {
    reexport_no_event_methods! {Container<'a, InterneNachricht, R>, container, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let mut interne_shell = Shell::new(&mut interne_nachrichten);
        let event_status = self.container.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_shell,
        );
        if interne_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            interne_shell.revalidate_layout(|| shell.invalidate_layout())
        }
        for interne_nachricht in interne_nachrichten {
            match interne_nachricht {
                InterneNachricht::Aktuell(name, f) => {
                    *self.aktuell = Some((name, f()));
                    *self.scrollable_text_zurücksetzen = true;
                },
                InterneNachricht::Schließen => shell.publish(Nachricht::Schließen),
            }
        }
        event_status
    }
}

impl<'a, R: 'a + Renderer> From<Lizenzen<'a, R>> for Element<'a, Nachricht, R> {
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}

#[inline(always)]
fn rust_project_developers_lizenz<'t>(jahr: &str) -> Cow<'t, str> {
    mit(
        None,
        vec![MITCopyright::neu(true, jahr, "The Rust Project Developers")],
        None,
        MITZeilenumbruch::X11,
        MITEinrückung::keine(),
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
        MITEnde::standard(),
    )
}

// TODO Lizenzen anpassen, so dass passende_Lizenzen nicht mehr fehlschlägt.
// TODO abweichende Dateinamen in passende_Lizenzen eintragen.
/// Die Lizenzen der verwendeter Open-Source Bibliotheken.
pub fn verwendete_lizenzen() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
    let rust_project_developers_lizenz_2010 = || rust_project_developers_lizenz("2010");
    let rust_project_developers_lizenz_2014 = || rust_project_developers_lizenz("2014");
    let rust_project_developers_lizenz_2015 = || rust_project_developers_lizenz("2015");
    let rust_project_developers_lizenz_2016 = || rust_project_developers_lizenz("2016");
    let ab_lizenz = || {
        apache_2_0_nicht_eingerückt(
            false,
            ApacheCopyright { brackets: "[]", jahr: "2020", voller_name: "Alex Butler" },
            true,
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
            MITEnde::standard(),
        )
    };
    let time_lizenz = || {
        mit(
            None,
            vec![MITCopyright::neu(true, "2022", "Jacob Pratt et al.")],
            None,
            MITZeilenumbruch::Standard,
            MITEinrückung::keine(),
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
            MITEnde { punkt: false, neue_zeile: 1 },
        )
    };
    vec![
        ("SourceSerif4-Regular", || {
            ofl_1_1("2014-2021",
            "Adobe (http://www.adobe.com/),",
            "'Source'",true,
            " All Rights Reserved. Source is a trademark of Adobe in the United States and/or other countries.",
            true,
            false)
        }),
        // Über iced_graphics mit feature "font-fallback" eingebunden (dependency von iced_glow)
        ("Lato", || {
            ofl_1_1(
                "2010-2014",
                "by tyPoland Lukasz Dziedzic (team@latofonts.com)",
                "\"Lato\"",
                false,
                "",
                false,
                true,
            )
        }),
        ("ab_glyph-0.2.15", ab_lizenz),
        ("ab_glyph_rasterizer-0.1.5", ab_lizenz),
        ("aho-corasick-0.7.18", || {
            mit(
                MITPräfix("The MIT License (MIT)", 2),
                vec![MITCopyright::neu(true, "2015", "Andrew Gallant")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::ohne_neue_zeile(),
            )
        }),
        ("android_glue-0.2.3", mit_plain),
        ("ansi_term-0.12.1", mit_plain),
        ("approx-0.5.1", apache_2_0_standard_nicht_eingerückt),
        ("arrayvec-0.5.2", mit_plain),
        ("atomic-polyfill-0.1.8", mit_plain),
        ("atty-0.2.14", mit_plain),
        ("autocfg-1.1.0", mit_plain),
        ("bare-metal-0.2.5", mit_plain),
        ("bare-metal-1.0.0", mit_plain),
        ("bincode-1.3.3", mit_plain),
        ("bitfield-0.13.2", mit_plain),
        ("bitflags-1.3.2", mit_plain),
        ("bit_field-0.10.1", mit_plain),
        ("block-0.1.6", mit_plain),
        ("bumpalo-2.6.0", mit_plain),
        ("bumpalo-3.9.1", mit_plain),
        ("bytemuck-1.9.1", mit_plain),
        ("bytemuck_derive-1.1.0", mit_plain),
        ("byteorder-1.4.3", mit_plain),
        ("calloop-0.9.3", mit_plain),
        ("cc-1.0.73", mit_plain),
        ("cfg-if-0.1.10", mit_plain),
        ("cfg-if-1.0.0", mit_plain),
        ("cfg_aliases-0.1.1", mit_plain),
        ("cgl-0.3.2", mit_plain),
        ("clipboard-win-4.4.1", bsl_1_0),
        ("clipboard_macos-0.1.0", apache_2_0_standard_nicht_eingerückt),
        ("clipboard_wayland-0.2.0", apache_2_0_standard_nicht_eingerückt),
        ("clipboard_x11-0.4.0", mit_plain),
        ("cocoa-0.24.0", mit_plain),
        ("cocoa-foundation-0.1.0", mit_plain),
        ("core-foundation-0.7.0", mit_plain),
        ("core-foundation-0.9.3", mit_plain),
        ("core-foundation-sys-0.7.0", mit_plain),
        ("core-foundation-sys-0.8.3", mit_plain),
        ("core-graphics-0.19.2", mit_plain),
        ("core-graphics-0.22.3", mit_plain),
        ("core-graphics-types-0.1.1", mit_plain),
        ("core-video-sys-0.1.4", mit_plain),
        ("cortex-m-0.7.4", mit_plain),
        ("critical-section-0.2.7", mit_plain),
        ("crossbeam-channel-0.5.4", mit_plain),
        ("crossbeam-deque-0.8.1", mit_plain),
        ("crossbeam-epoch-0.9.8", mit_plain),
        ("crossbeam-utils-0.8.8", mit_plain),
        ("cty-0.2.2", mit_plain),
        ("darling-0.13.4", mit_plain),
        ("darling_core-0.13.4", mit_plain),
        ("darling_macro-0.13.4", mit_plain),
        ("difference-2.0.0", mit_plain),
        ("dispatch-0.2.0", mit_plain),
        ("dlib-0.5.0", mit_plain),
        ("dodrio-0.2.0", mpl_2_0),
        ("downcast-rs-1.2.0", mit_plain),
        ("either-1.6.1", mit_plain),
        ("embed-resource-1.7.2", mit_plain),
        ("embedded-hal-0.2.7", mit_plain),
        ("error-code-2.3.1", bsl_1_0),
        ("euclid-0.22.7", mit_plain),
        ("flexi_logger-0.22.3", mit_plain),
        ("float_next_after-0.1.5", mit_plain),
        ("fnv-1.0.7", mit_plain),
        ("foreign-types-0.3.2", mit_plain),
        ("foreign-types-shared-0.1.1", mit_plain),
        ("form_urlencoded-1.0.1", mit_plain),
        ("futures-0.3.21", mit_plain),
        ("futures-channel-0.3.21", mit_plain),
        ("futures-core-0.3.21", mit_plain),
        ("futures-executor-0.3.21", mit_plain),
        ("futures-io-0.3.21", mit_plain),
        ("futures-macro-0.3.21", mit_plain),
        ("futures-sink-0.3.21", mit_plain),
        ("futures-task-0.3.21", mit_plain),
        ("futures-util-0.3.21", mit_plain),
        ("fxhash-0.2.1", mit_plain), // TODO
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
        ("getrandom-0.2.6", || {
            mit(
                None,
                vec![
                    MITCopyright::neu(false, "2018", "Developers of the Rand project"),
                    MITCopyright::neu(true, "2014", "The Rust Project Developers"),
                ],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("glam-0.10.2", mit_ohne_copyright_x11),
        ("glob-0.3.0", rust_project_developers_lizenz_2014),
        ("glow-0.11.2", || {
            mit(
                None,
                Vec::new(),
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::ohne_neue_zeile(),
            )
        }),
        ("glow_glyph-0.5.1", mit_plain), // TODO
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
        ("heapless-0.7.13", || mit_lizenz_aparicio("2017")),
        ("heck-0.4.0", rust_project_developers_lizenz_2015),
        ("hermit-abi-0.1.19", mit_ohne_copyright_x11),
        ("iced-0.4.2", iced_lizenz),
        ("iced_aw-0.1.0", || {
            mit(
                MITPräfix("MIT License", 2),
                vec![MITCopyright::neu(true, "2020", "Kaiden42")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("iced_core-0.4.0", iced_lizenz),
        ("iced_core-0.5.0", iced_lizenz),
        ("iced_futures-0.3.0", iced_lizenz),
        ("iced_futures-0.4.0", iced_lizenz),
        ("iced_glow-0.3.0", iced_lizenz),
        ("iced_glutin-0.3.0", iced_lizenz),
        ("iced_graphics-0.3.0", iced_lizenz),
        ("iced_native-0.5.0", iced_lizenz),
        ("iced_style-0.3.0", iced_lizenz),
        ("iced_style-0.4.0", iced_lizenz),
        ("iced_web-0.4.0", iced_lizenz),
        ("iced_winit-0.4.0", iced_lizenz),
        ("ident_case-1.0.1", || {
            mit(
                MITPräfix("MIT License", 2),
                Vec::new(),
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("idna-0.2.3", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2013-2016", "The rust-url developers")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("js-sys-0.3.57", wasm_bindgen_lizenz),
        ("khronos_api-3.1.0", apache_2_0_standard_eingerückt),
        ("kommandozeilen_argumente-0.2.0", kommandozeilen_argumente_lizenz),
        ("kommandozeilen_argumente_derive-0.2.0", kommandozeilen_argumente_lizenz),
        ("lazy_static-1.4.0", rust_project_developers_lizenz_2010),
        ("libc-0.2.126", || rust_project_developers_lizenz("2014-2020")),
        ("libloading-0.7.3", || {
            isc(true, "2015", "Simonas Kazlauskas", ISCZeilenumbruch::Libloading)
        }),
        ("libm-0.2.2", || mit_lizenz_aparicio("2018")),
        ("linked-hash-map-0.5.4", rust_project_developers_lizenz_2015),
        ("lock_api-0.4.7", rust_project_developers_lizenz_2016),
        ("log-0.4.17", rust_project_developers_lizenz_2014),
        ("longest-increasing-subsequence-0.1.0", wasm_bindgen_lizenz),
        ("lyon-0.17.10", lyon_lizenz),
        ("lyon_algorithms-0.17.7", lyon_lizenz),
        ("lyon_geom-0.17.6", lyon_lizenz),
        ("lyon_path-0.17.7", lyon_lizenz),
        ("lyon_tessellation-0.17.10", lyon_lizenz),
        ("malloc_buf-0.0.6", || {
            mit(
                MITPräfix("MIT License", 2),
                vec![MITCopyright::neu(true, "2020", "Steven Sheldon")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("matches-0.1.9", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2014-2016", "Simon Sapin")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("memchr-2.5.0", || {
            mit(
                MITPräfix("The MIT License (MIT)", 2),
                vec![MITCopyright::neu(true, "2015", "Andrew Gallant")],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("memmap2-0.3.1", || {
            mit(
                None,
                vec![
                    MITCopyright::neu(true, "2020", "Yevhenii Reizner"),
                    MITCopyright::neu(true, "2015", "Dan Burkert"),
                ],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("memoffset-0.6.5", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2017", "Gilad Naaman")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::ohne_neue_zeile(),
            )
        }),
        ("minimal-lexical-0.2.1", mit_ohne_copyright_x11),
        ("mio-0.8.3", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2014", "Carl Lerche and other MIO contributors")],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
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
        ("nom-7.1.1", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2014-2019", "Geoffroy Couprie")],
                None,
                MITZeilenumbruch::Redox,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("nonempty-0.7.0", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2019", "Alexis Sellier")],
                None,
                MITZeilenumbruch::NonEmpty,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("num-traits-0.2.15", rust_project_developers_lizenz_2014),
        ("num_cpus-1.13.1", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2015", None)],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("objc-foundation-0.1.1", mit_plain), // TODO
        ("objc_id-0.1.1", mit_plain),         // TODO
        ("once_cell-1.11.0", mit_ohne_copyright_x11),
        ("ordered-float-3.0.0", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2015", "Jonathan Reem")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("osmesa-sys-0.1.2", cc_0), // TODO
        ("owned_ttf_parser-0.15.0", || {
            apache_2_0_eingerückt(
                false,
                ApacheCopyright { brackets: "{}", jahr: "2020", voller_name: "Alex Butler" },
                true,
            )
        }),
        ("parking_lot-0.11.2", rust_project_developers_lizenz_2016),
        ("parking_lot-0.12.0", rust_project_developers_lizenz_2016),
        ("parking_lot_core-0.8.5", rust_project_developers_lizenz_2016),
        ("parking_lot_core-0.9.3", rust_project_developers_lizenz_2016),
        ("percent-encoding-2.1.0", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2013-2016", "The rust-url developers")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("proc-macro-crate-1.1.3", mit_ohne_copyright_x11),
        ("proc-macro2-1.0.39", wasm_bindgen_lizenz),
        ("quote-1.0.18", rust_project_developers_lizenz_2016),
        ("rand-0.8.5", rand_lizenz),
        ("rand_chacha-0.3.1", rand_lizenz),
        ("rand_core-0.6.3", rand_lizenz),
        ("raw-window-handle-0.3.4", raw_window_handle_lizenz),
        ("raw-window-handle-0.4.3", raw_window_handle_lizenz),
        ("rayon-1.5.3", rust_project_developers_lizenz_2010),
        ("rayon-core-1.9.3", rust_project_developers_lizenz_2010),
        ("redox_syscall-0.2.13", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2017", "Redox OS Developers")],
                MITInfix("MIT License", 2),
                MITZeilenumbruch::Redox,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("regex-1.5.6", rust_project_developers_lizenz_2014),
        ("regex-syntax-0.6.26", rust_project_developers_lizenz_2014),
        ("riscv-0.7.0", || isc(false, "2019-2020", "[RISC-V team][team]", ISCZeilenumbruch::Riscv)),
        ("riscv-target-0.1.2", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2020", "Ilya Epifanov")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("rustc-hash-1.1.0", mit_ohne_copyright_x11),
        ("rustc_version-0.2.3", rust_project_developers_lizenz_2016),
        ("rustc_version-0.4.0", rust_project_developers_lizenz_2016),
        ("rustversion-1.0.6", mit_ohne_copyright_x11),
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
                MITEnde::standard(),
            )
        }),
        ("semver-0.9.0", rust_project_developers_lizenz_2014),
        ("semver-1.0.9", mit_ohne_copyright_x11),
        ("semver-parser-0.7.0", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2016", "Steve Klabnik")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("serde-1.0.137", serde_lizenz),
        ("serde_derive-1.0.137", serde_lizenz),
        ("shared_library-0.1.9", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2017", "Pierre Krieger")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::ohne_neue_zeile(),
            )
        }),
        ("sid-0.6.1", mit_plain), // TODO
        ("slab-0.4.6", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2019", "Carl Lerche")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("smithay-client-toolkit-0.15.4", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2018", "Victor Berger")],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
                MITEnde::ohne_neue_zeile(),
            )
        }),
        ("smithay-clipboard-0.6.5", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2018", "Lucas Timmins & Victor Berger")],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("str-buf-1.0.5", bsl_1_0),
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
                MITEnde::standard(),
            )
        }),
        ("syn-1.0.95", mit_ohne_copyright_x11),
        ("take_mut-0.2.2", || {
            mit(
                MITPräfix("The MIT License (MIT)", 2),
                vec![MITCopyright::neu(true, "2016", "Sgeo")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::zwei_neue_zeilen(),
            )
        }),
        ("thiserror-1.0.31", mit_ohne_copyright_x11),
        ("thiserror-impl-1.0.31", mit_ohne_copyright_x11),
        ("time-0.3.9", time_lizenz),
        ("time-macros-0.2.4", time_lizenz),
        ("tinyvec-1.6.0", || mit_ohne_copyright(MITZeilenumbruch::Keine)),
        ("tinyvec_macros-0.1.0", || {
            mit(
                MITPräfix("MIT License", 2),
                vec![MITCopyright::neu(true, "2020", "Soveu")],
                None,
                MITZeilenumbruch::Standard,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("toml-0.5.9", wasm_bindgen_lizenz),
        ("ttf-parser-0.15.0", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2018", "Yevhenii Reizner")],
                None,
                MITZeilenumbruch::Winreg,
                MITEinrückung::keine(),
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
                MITEnde::zwei_neue_zeilen(),
            )
        }),
        ("unicode-bidi-0.3.8", rust_project_developers_lizenz_2015),
        ("unicode-ident-1.0.0", mit_ohne_copyright_x11),
        ("unicode-normalization-0.1.19", rust_project_developers_lizenz_2015),
        ("unicode-segmentation-1.9.0", rust_project_developers_lizenz_2015),
        ("url-2.2.2", || {
            mit(
                None,
                vec![MITCopyright::neu(true, "2013-2016", "The rust-url developers")],
                None,
                MITZeilenumbruch::X11,
                MITEinrückung::keine(),
                MITEnde::standard(),
            )
        }),
        ("vcell-0.1.3", vcell_lizenz),
        ("version_check-0.9.4", || {
            mit(
                MITPräfix("The MIT License (MIT)", 1),
                vec![MITCopyright::neu(true, "2017-2018", "Sergio Benitez")],
                None,
                MITZeilenumbruch::Iced,
                MITEinrückung::keine(),
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
                MITEnde::standard(),
            )
        }),
        ("volatile-register-0.2.1", || mit_lizenz_aparicio("2016")),
        ("vswhom-0.1.0", vswwhom_lizenz),
        ("vswhom-sys-0.1.1", vswwhom_lizenz),
        ("wasi-0.10.2+wasi-snapshot-preview1", mit_ohne_copyright_x11),
        ("wasi-0.11.0+wasi-snapshot-preview1", mit_ohne_copyright_x11),
        ("wasm-bindgen-0.2.80", wasm_bindgen_lizenz),
        ("wasm-bindgen-backend-0.2.80", wasm_bindgen_lizenz),
        ("wasm-bindgen-futures-0.4.30", wasm_bindgen_lizenz),
        ("wasm-bindgen-macro-0.2.80", wasm_bindgen_lizenz),
        ("wasm-bindgen-macro-support-0.2.80", wasm_bindgen_lizenz),
        ("wasm-bindgen-shared-0.2.80", wasm_bindgen_lizenz),
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
        ("web-sys-0.3.57", wasm_bindgen_lizenz),
        ("winapi-0.3.9", winapi_lizenz),
        ("winapi-i686-pc-windows-gnu-0.4.0", winapi_lizenz),
        ("winapi-wsapoll-0.1.1", mit_plain), // TODO
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
                MITEnde::standard(),
            )
        }),
        ("winit-0.26.1", || {
            apache_2_0(
                false,
                ApacheCopyright {
                    brackets: "{}",
                    jahr: "{yyyy}",
                    voller_name: "{name of copyright owner}",
                },
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
                MITEnde::standard(),
            )
        }),
    ]
}

#[cfg(test)]
mod test;
