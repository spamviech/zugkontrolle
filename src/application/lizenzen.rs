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
// FIXME entferne unbenutzte imports
#[allow(unused_imports)]
use texte::{
    apache_2_0, apache_2_0_eingerückt, apache_2_0_nicht_eingerückt, apache_2_0_standard_eingerückt,
    apache_2_0_standard_nicht_eingerückt, bsd_3, bsl_1_0, cc_0, isc, mit, mit_plain, mpl_2_0,
    ofl_1_1, zlib, ApacheCopyright, ApacheEinrückung, MITCopyright, MITZeilenumbruch,
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

// TODO Lizenzen anpassen, so dass passende_Lizenzen nicht mehr fehlschlägt.
// TODO abweichende Dateinamen in passende_Lizenzen eintragen.
/// Die Lizenzen der verwendeter Open-Source Bibliotheken.
pub fn verwendete_lizenzen() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
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
                r#""Lato""#,
                false,
                "",
                false,
                true,
            )
        }),
        ("ab_glyph-0.2.15", || {
            apache_2_0_nicht_eingerückt(
                false,
                ApacheCopyright { brackets: "[]", jahr: "2020", voller_name: "Alex Butler" },
                true,
            )
        }),
        ("ab_glyph_rasterizer-0.1.5", || {
            apache_2_0_nicht_eingerückt(
                false,
                ApacheCopyright { brackets: "[]", jahr: "2020", voller_name: "Alex Butler" },
                true,
            )
        }),
        ("aho-corasick-0.7.18", || {
            mit(
                "The MIT License (MIT)\n\n",
                Some(MITCopyright::neu(true, "2015", "Andrew Gallant")),
                MITZeilenumbruch::Standard,
                false,
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
        ("clipboard_x11-0.3.1", mit_plain),
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
        ("fxhash-0.2.1", mit_plain),
        ("gethostname-0.2.3", apache_2_0_standard_nicht_eingerückt),
        ("getrandom-0.2.6", mit_plain),
        ("glam-0.10.2", mit_plain),
        ("glob-0.3.0", mit_plain),
        ("glow-0.11.2", mit_plain),
        ("glow_glyph-0.5.0", mit_plain),
        ("glutin-0.28.0", apache_2_0_standard_nicht_eingerückt),
        ("glutin_egl_sys-0.1.5", apache_2_0_standard_nicht_eingerückt),
        ("glutin_emscripten_sys-0.1.1", apache_2_0_standard_nicht_eingerückt),
        ("glutin_gles2_sys-0.1.5", apache_2_0_standard_nicht_eingerückt),
        ("glutin_glx_sys-0.1.7", apache_2_0_standard_nicht_eingerückt),
        ("glutin_wgl_sys-0.1.5", apache_2_0_standard_nicht_eingerückt),
        ("glyph_brush-0.7.4", apache_2_0_standard_nicht_eingerückt),
        ("glyph_brush_draw_cache-0.1.5", apache_2_0_standard_nicht_eingerückt),
        ("glyph_brush_layout-0.2.3", apache_2_0_standard_nicht_eingerückt),
        ("gl_generator-0.14.0", apache_2_0_standard_nicht_eingerückt),
        ("hash32-0.2.1", mit_plain),
        ("heapless-0.7.10", mit_plain),
        ("heck-0.4.0", mit_plain),
        ("hermit-abi-0.1.19", mit_plain),
        ("iced-0.4.2", mit_plain),
        ("iced_aw-0.1.0", mit_plain),
        ("iced_core-0.4.0", mit_plain),
        ("iced_core-0.5.0", mit_plain),
        ("iced_futures-0.3.0", mit_plain),
        ("iced_futures-0.4.0", mit_plain),
        ("iced_glow-0.3.0", mit_plain),
        ("iced_glutin-0.3.0", mit_plain),
        ("iced_graphics-0.3.0", mit_plain),
        ("iced_native-0.5.0", mit_plain),
        ("iced_style-0.3.0", mit_plain),
        ("iced_style-0.4.0", mit_plain),
        ("iced_web-0.4.0", mit_plain),
        ("iced_winit-0.4.0", mit_plain),
        ("ident_case-1.0.1", mit_plain),
        ("idna-0.2.3", mit_plain),
        ("instant-0.1.12", || bsd_3("2019", "Sébastien Crozet")),
        ("itertools-0.10.3", mit_plain),
        ("itoa-1.0.1", mit_plain),
        ("jni-sys-0.3.0", mit_plain),
        ("js-sys-0.3.57", mit_plain),
        ("khronos_api-3.1.0", apache_2_0_standard_nicht_eingerückt),
        ("kommandozeilen_argumente-0.2.0", mit_plain),
        ("kommandozeilen_argumente_derive-0.2.0", mit_plain),
        ("lazy_static-1.4.0", mit_plain),
        ("libc-0.2.125", mit_plain),
        ("libloading-0.7.3", || isc("2015", "Simonas Kazlauskas")),
        ("libm-0.2.2", mit_plain),
        ("linked-hash-map-0.5.4", mit_plain),
        ("lock_api-0.4.7", mit_plain),
        ("log-0.4.17", mit_plain),
        ("longest-increasing-subsequence-0.1.0", mit_plain),
        ("lyon-0.17.10", mit_plain),
        ("lyon_algorithms-0.17.7", mit_plain),
        ("lyon_geom-0.17.6", mit_plain),
        ("lyon_path-0.17.7", mit_plain),
        ("lyon_tessellation-0.17.10", mit_plain),
        ("malloc_buf-0.0.6", mit_plain),
        ("matches-0.1.9", mit_plain),
        ("memchr-2.5.0", mit_plain),
        ("memmap2-0.3.1", mit_plain),
        ("memoffset-0.6.5", mit_plain),
        ("minimal-lexical-0.2.1", mit_plain),
        ("mio-0.8.2", mit_plain),
        ("miow-0.3.7", mit_plain),
        ("nb-0.1.3", mit_plain),
        ("nb-1.0.0", mit_plain),
        ("newline-converter-0.2.0", mit_plain),
        ("ndk-0.5.0", mit_plain),
        ("ndk-context-0.1.1", mit_plain),
        ("ndk-glue-0.5.2", mit_plain),
        ("ndk-macro-0.3.0", mit_plain),
        ("ndk-sys-0.2.2", mit_plain),
        ("nix-0.20.0", mit_plain),
        ("nix-0.22.3", mit_plain),
        ("nom-7.1.1", mit_plain),
        ("nonempty-0.7.0", mit_plain),
        ("ntapi-0.3.7", mit_plain),
        ("num-traits-0.2.15", mit_plain),
        ("num_cpus-1.13.1", mit_plain),
        ("num_enum-0.5.7", mit_plain),
        ("num_enum_derive-0.5.7", mit_plain),
        ("num_threads-0.1.6", mit_plain),
        ("objc-0.2.7", mit_plain),
        ("objc-foundation-0.1.1", mit_plain),
        ("objc_id-0.1.1", mit_plain),
        ("once_cell-1.10.0", mit_plain),
        ("ordered-float-3.0.0", mit_plain),
        ("osmesa-sys-0.1.2", cc_0),
        ("owned_ttf_parser-0.15.0", apache_2_0_standard_nicht_eingerückt),
        ("parking_lot-0.11.2", mit_plain),
        ("parking_lot-0.12.0", mit_plain),
        ("parking_lot_core-0.8.5", mit_plain),
        ("parking_lot_core-0.9.3", mit_plain),
        ("percent-encoding-2.1.0", mit_plain),
        ("pin-project-lite-0.2.9", mit_plain),
        ("pin-utils-0.1.0", mit_plain),
        ("pkg-config-0.3.25", mit_plain),
        ("ppv-lite86-0.2.16", mit_plain),
        ("proc-macro-crate-1.1.3", mit_plain),
        ("proc-macro2-1.0.38", mit_plain),
        ("quote-1.0.18", mit_plain),
        ("rand-0.8.5", mit_plain),
        ("rand_chacha-0.3.1", mit_plain),
        ("rand_core-0.6.3", mit_plain),
        ("raw-window-handle-0.3.4", mit_plain),
        ("raw-window-handle-0.4.3", mit_plain),
        ("rayon-1.5.2", mit_plain),
        ("rayon-core-1.9.2", mit_plain),
        ("redox_syscall-0.2.13", mit_plain),
        ("regex-1.5.5", mit_plain),
        ("regex-syntax-0.6.25", mit_plain),
        ("riscv-0.7.0", || isc("2019-2020", "[RISC-V team][team]")),
        ("riscv-target-0.1.2", || {
            mit(
                "MIT License (MIT)\n\n",
                Some(MITCopyright::neu(true, "2020", "Ilya Epifanov")),
                MITZeilenumbruch::Standard,
                false,
            )
        }),
        ("rppal-0.13.1", mit_plain),
        ("rstar-0.9.3", mit_plain),
        ("rustc-hash-1.1.0", mit_plain),
        ("rustc_version-0.2.3", mit_plain),
        ("rustc_version-0.4.0", mit_plain),
        ("rustversion-1.0.6", mit_plain),
        ("scoped-tls-1.0.0", mit_plain),
        ("scopeguard-1.1.0", mit_plain),
        ("semver-0.9.0", mit_plain),
        ("semver-1.0.9", mit_plain),
        ("semver-parser-0.7.0", mit_plain),
        ("serde-1.0.137", mit_plain),
        ("serde_derive-1.0.137", mit_plain),
        ("shared_library-0.1.9", mit_plain),
        ("sid-0.6.1", mit_plain),
        ("slab-0.4.6", mit_plain),
        ("slotmap-1.0.6", || zlib("2021", "Orson Peters <orsonpeters@gmail.com>")),
        ("smallvec-1.8.0", mit_plain),
        ("smithay-client-toolkit-0.15.4", mit_plain),
        ("smithay-clipboard-0.6.5", mit_plain),
        ("spin-0.9.3", mit_plain),
        ("stable_deref_trait-1.2.0", mit_plain),
        ("static_assertions-1.1.0", mit_plain),
        ("str-buf-1.0.5", bsl_1_0),
        ("strsim-0.10.0", mit_plain),
        ("syn-1.0.92", mit_plain),
        ("take_mut-0.2.2", mit_plain),
        ("thiserror-1.0.31", mit_plain),
        ("thiserror-impl-1.0.31", mit_plain),
        ("time-0.3.9", mit_plain),
        ("time-macros-0.2.4", mit_plain),
        ("tinyvec-1.6.0", mit_plain),
        ("tinyvec_macros-0.1.0", mit_plain),
        ("toml-0.5.9", mit_plain),
        ("ttf-parser-0.15.0", mit_plain),
        ("twox-hash-1.6.3", mit_plain),
        ("unicase-2.6.0", mit_plain),
        ("unicode-bidi-0.3.8", mit_plain),
        ("unicode-normalization-0.1.19", mit_plain),
        ("unicode-segmentation-1.9.0", mit_plain),
        ("unicode-xid-0.2.3", mit_plain),
        ("url-2.2.2", mit_plain),
        ("vcell-0.1.3", mit_plain),
        ("version_check-0.9.4", mit_plain),
        ("void-1.0.2", mit_plain),
        ("volatile-register-0.2.1", mit_plain),
        ("vswhom-0.1.0", mit_plain),
        ("vswhom-sys-0.1.1", mit_plain),
        ("wasi-0.10.2+wasi-snapshot-preview1", mit_plain),
        ("wasi-0.11.0+wasi-snapshot-preview1", mit_plain),
        ("wasm-bindgen-0.2.80", mit_plain),
        ("wasm-bindgen-backend-0.2.80", mit_plain),
        ("wasm-bindgen-futures-0.4.30", mit_plain),
        ("wasm-bindgen-macro-0.2.80", mit_plain),
        ("wasm-bindgen-macro-support-0.2.80", mit_plain),
        ("wasm-bindgen-shared-0.2.80", mit_plain),
        ("wasm-timer-0.2.5", mit_plain),
        ("wayland-client-0.29.4", mit_plain),
        ("wayland-commons-0.29.4", mit_plain),
        ("wayland-cursor-0.29.4", mit_plain),
        ("wayland-egl-0.29.4", mit_plain),
        ("wayland-protocols-0.29.4", mit_plain),
        ("wayland-scanner-0.29.4", mit_plain),
        ("wayland-sys-0.29.4", mit_plain),
        ("web-sys-0.3.57", mit_plain),
        ("winapi-0.3.9", mit_plain),
        ("winapi-i686-pc-windows-gnu-0.4.0", mit_plain),
        ("winapi-wsapoll-0.1.1", mit_plain),
        ("winapi-x86_64-pc-windows-gnu-0.4.0", mit_plain),
        ("windows-sys-0.36.1", mit_plain),
        ("windows_aarch64_msvc-0.36.1", mit_plain),
        ("windows_i686_gnu-0.36.1", mit_plain),
        ("windows_i686_msvc-0.36.1", mit_plain),
        ("windows_x86_64_gnu-0.36.1", mit_plain),
        ("windows_x86_64_msvc-0.36.1", mit_plain),
        ("window_clipboard-0.2.2", mit_plain),
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
                "",
                Some(MITCopyright::neu(true, "2015", "Igor Shaula")),
                MITZeilenumbruch::Winreg,
                true,
            )
        }),
        ("x11-dl-2.19.1", || mit("", None, MITZeilenumbruch::X11, true)),
        ("x11rb-0.8.1", || {
            mit(
                "",
                Some(MITCopyright::neu(false, "2019", "x11rb Contributers")),
                MITZeilenumbruch::X11,
                true,
            )
        }),
        ("xcursor-0.3.4", || {
            mit(
                "MIT License\n\n",
                Some(MITCopyright::neu(true, "2020", "Samuele Esposito")),
                MITZeilenumbruch::Standard,
                true,
            )
        }),
        ("xi-unicode-0.3.0", apache_2_0_standard_eingerückt),
        ("xml-rs-0.8.4", || {
            mit(
                "The MIT License (MIT)\n\n",
                Some(MITCopyright::neu(true, "2014", "Vladimir Matveev")),
                MITZeilenumbruch::Standard,
                true,
            )
        }),
    ]
}

#[cfg(test)]
mod test;
