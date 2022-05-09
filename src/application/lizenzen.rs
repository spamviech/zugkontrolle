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
    fonts,
    macros::reexport_no_event_methods,
    style::{hintergrund, linie::TRENNLINIE},
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
        Self::neu(verwendete_lizenzen_mock())
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
                .push(Rule::vertical(1).style(TRENNLINIE))
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

// BSD-3-Clause (1): instant
// BSL-1.0 (3): clipboard-win, error-code, str-buf
// CC0-1.0 (1): osmesa-sys
// ISC (3): libloading, riscv, riscv-target
// MIT AND OFL-1.1 (1): iced_glow
// MPL-2.0 (1): dodrio
// Zlib (1): slotmap

/// Die Lizenzen der verwendeter Open-Source Bibliotheken.
pub fn verwendete_lizenzen() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
    vec![
        ("SourceSerif4-Regular", || Cow::Borrowed(fonts::LICENSE)),
        ("ab_glyph-0.2.15", || apache_2_0("2020", "Alex Butler")),
        ("ab_glyph_rasterizer-0.1.5", || apache_2_0("2020", "Alex Butler")),
        ("aho-corasick-0.7.18", || mit("The ", "2015", "Andrew Gallant")),
        ("android_glue-0.2.3", mit_plain),
        ("ansi_term-0.12.1", mit_plain),
        ("approx-0.5.1", apache_2_0_plain),
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
        ("clipboard-win-4.4.1", todo!()),
        ("clipboard_macos-0.1.0", apache_2_0_plain),
        ("clipboard_wayland-0.2.0", apache_2_0_plain),
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
        ("dodrio-0.2.0", todo!()),
        ("downcast-rs-1.2.0", mit_plain),
        ("either-1.6.1", mit_plain),
        ("embed-resource-1.7.2", mit_plain),
        ("embedded-hal-0.2.7", mit_plain),
        ("error-code-2.3.1", todo!()),
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
        ("gethostname-0.2.3", apache_2_0_plain),
        ("getrandom-0.2.6", mit_plain),
        ("glam-0.10.2", mit_plain),
        ("glob-0.3.0", mit_plain),
        ("glow-0.11.2", mit_plain),
        ("glow_glyph-0.5.0", mit_plain),
        ("glutin-0.28.0", apache_2_0_plain),
        ("glutin_egl_sys-0.1.5", apache_2_0_plain),
        ("glutin_emscripten_sys-0.1.1", apache_2_0_plain),
        ("glutin_gles2_sys-0.1.5", apache_2_0_plain),
        ("glutin_glx_sys-0.1.7", apache_2_0_plain),
        ("glutin_wgl_sys-0.1.5", apache_2_0_plain),
        ("glyph_brush-0.7.4", apache_2_0_plain),
        ("glyph_brush_draw_cache-0.1.5", apache_2_0_plain),
        ("glyph_brush_layout-0.2.3", apache_2_0_plain),
        ("gl_generator-0.14.0", apache_2_0_plain),
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
        ("iced_glow-0.3.0", todo!()),
        ("iced_glutin-0.3.0", mit_plain),
        ("iced_graphics-0.3.0", mit_plain),
        ("iced_native-0.5.0", mit_plain),
        ("iced_style-0.3.0", mit_plain),
        ("iced_style-0.4.0", mit_plain),
        ("iced_web-0.4.0", mit_plain),
        ("iced_winit-0.4.0", mit_plain),
        ("ident_case-1.0.1", mit_plain),
        ("idna-0.2.3", mit_plain),
        ("instant-0.1.12", todo!()),
        ("itertools-0.10.3", mit_plain),
        ("itoa-1.0.1", mit_plain),
        ("jni-sys-0.3.0", mit_plain),
        ("js-sys-0.3.57", mit_plain),
        ("khronos_api-3.1.0", apache_2_0_plain),
        ("kommandozeilen_argumente-0.2.0", mit_plain),
        ("kommandozeilen_argumente_derive-0.2.0", mit_plain),
        ("lazy_static-1.4.0", mit_plain),
        ("libc-0.2.125", mit_plain),
        ("libloading-0.7.3", todo!()),
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
        ("osmesa-sys-0.1.2", todo!()),
        ("owned_ttf_parser-0.15.0", apache_2_0_plain),
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
        ("riscv-0.7.0", todo!()),
        ("riscv-target-0.1.2", todo!()),
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
        ("slotmap-1.0.6", todo!()),
        ("smallvec-1.8.0", mit_plain),
        ("smithay-client-toolkit-0.15.4", mit_plain),
        ("smithay-clipboard-0.6.5", mit_plain),
        ("spin-0.9.3", mit_plain),
        ("stable_deref_trait-1.2.0", mit_plain),
        ("static_assertions-1.1.0", mit_plain),
        ("str-buf-1.0.5", todo!()),
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
        ("winit-0.26.1", apache_2_0_plain),
        ("winreg-0.10.1", mit_plain),
        ("x11-dl-2.19.1", mit_plain),
        ("x11rb-0.8.1", mit_plain),
        ("xcursor-0.3.4", mit_plain),
        ("xi-unicode-0.3.0", apache_2_0_plain),
        ("xml-rs-0.8.4", mit_plain),
    ]
}

fn verwendete_lizenzen_mock() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
    // FIXME verwende echte Lizenzen
    let f: fn() -> Cow<'static, str> = || {
        Cow::Borrowed("Some long license text.\n\nTherefore, it needs multiple lines!\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.\n\n\n\n\n\n\n\n\n\n\nSome text in the middle.\n\n\n\n\n\n\nAnother midway text.\n\n\n\n\n\n\n\nYet another debug line.\n\n\n\nHello from the deep.\n\n\n\n\nA final last line after a lot of vertical space.")
    };
    let g: fn() -> Cow<'static, str> = || {
        Cow::Borrowed("Ein andere Lizenz.\nAußerdem gibt es dabei sehr lange Texte, die ausreichen sollten um neben expliziten neuen Zeilen auch automatische Zeilenumbrüche überprüfen zu können.\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.")
    };
    // TODO
    vec![
        ("test", f),
        ("alternativ", g),
        ("mit", || mit("", "YYYY", "Full Name")),
        ("apache-2.0", apache_2_0_plain),
    ]
}

// TODO Test schreiben ob, die angezeigte Lizenz mit der wirklichen übereinstimmt

#[inline(always)]
fn mit_plain() -> Cow<'static, str> {
    mit("", "[year]", "[full_name]")
}

fn mit(prefix_the: &str, year: &str, full_name: &str) -> Cow<'static, str> {
    Cow::Owned(format!(
        r#"{prefix_the}MIT License

Copyright (c) {year} {full_name}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE."#
    ))
}

#[inline(always)]
fn apache_2_0_plain() -> Cow<'static, str> {
    apache_2_0("[yyyy]", "[name of copyright owner]")
}

fn apache_2_0(year: &str, full_name: &str) -> Cow<'static, str> {
    Cow::Owned(format!(
        r#"        Apache License
        Version 2.0, January 2004
     http://www.apache.org/licenses/

TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

1. Definitions.

"License" shall mean the terms and conditions for use, reproduction,
and distribution as defined by Sections 1 through 9 of this document.

"Licensor" shall mean the copyright owner or entity authorized by
the copyright owner that is granting the License.

"Legal Entity" shall mean the union of the acting entity and all
other entities that control, are controlled by, or are under common
control with that entity. For the purposes of this definition,
"control" means (i) the power, direct or indirect, to cause the
direction or management of such entity, whether by contract or
otherwise, or (ii) ownership of fifty percent (50%) or more of the
outstanding shares, or (iii) beneficial ownership of such entity.

"You" (or "Your") shall mean an individual or Legal Entity
exercising permissions granted by this License.

"Source" form shall mean the preferred form for making modifications,
including but not limited to software source code, documentation
source, and configuration files.

"Object" form shall mean any form resulting from mechanical
transformation or translation of a Source form, including but
not limited to compiled object code, generated documentation,
and conversions to other media types.

"Work" shall mean the work of authorship, whether in Source or
Object form, made available under the License, as indicated by a
copyright notice that is included in or attached to the work
(an example is provided in the Appendix below).

"Derivative Works" shall mean any work, whether in Source or Object
form, that is based on (or derived from) the Work and for which the
editorial revisions, annotations, elaborations, or other modifications
represent, as a whole, an original work of authorship. For the purposes
of this License, Derivative Works shall not include works that remain
separable from, or merely link (or bind by name) to the interfaces of,
the Work and Derivative Works thereof.

"Contribution" shall mean any work of authorship, including
the original version of the Work and any modifications or additions
to that Work or Derivative Works thereof, that is intentionally
submitted to Licensor for inclusion in the Work by the copyright owner
or by an individual or Legal Entity authorized to submit on behalf of
the copyright owner. For the purposes of this definition, "submitted"
means any form of electronic, verbal, or written communication sent
to the Licensor or its representatives, including but not limited to
communication on electronic mailing lists, source code control systems,
and issue tracking systems that are managed by, or on behalf of, the
Licensor for the purpose of discussing and improving the Work, but
excluding communication that is conspicuously marked or otherwise
designated in writing by the copyright owner as "Not a Contribution."

"Contributor" shall mean Licensor and any individual or Legal Entity
on behalf of whom a Contribution has been received by Licensor and
subsequently incorporated within the Work.

2. Grant of Copyright License. Subject to the terms and conditions of
this License, each Contributor hereby grants to You a perpetual,
worldwide, non-exclusive, no-charge, royalty-free, irrevocable
copyright license to reproduce, prepare Derivative Works of,
publicly display, publicly perform, sublicense, and distribute the
Work and such Derivative Works in Source or Object form.

3. Grant of Patent License. Subject to the terms and conditions of
this License, each Contributor hereby grants to You a perpetual,
worldwide, non-exclusive, no-charge, royalty-free, irrevocable
(except as stated in this section) patent license to make, have made,
use, offer to sell, sell, import, and otherwise transfer the Work,
where such license applies only to those patent claims licensable
by such Contributor that are necessarily infringed by their
Contribution(s) alone or by combination of their Contribution(s)
with the Work to which such Contribution(s) was submitted. If You
institute patent litigation against any entity (including a
cross-claim or counterclaim in a lawsuit) alleging that the Work
or a Contribution incorporated within the Work constitutes direct
or contributory patent infringement, then any patent licenses
granted to You under this License for that Work shall terminate
as of the date such litigation is filed.

4. Redistribution. You may reproduce and distribute copies of the
Work or Derivative Works thereof in any medium, with or without
modifications, and in Source or Object form, provided that You
meet the following conditions:

(a) You must give any other recipients of the Work or
Derivative Works a copy of this License; and

(b) You must cause any modified files to carry prominent notices
stating that You changed the files; and

(c) You must retain, in the Source form of any Derivative Works
that You distribute, all copyright, patent, trademark, and
attribution notices from the Source form of the Work,
excluding those notices that do not pertain to any part of
the Derivative Works; and

(d) If the Work includes a "NOTICE" text file as part of its
distribution, then any Derivative Works that You distribute must
include a readable copy of the attribution notices contained
within such NOTICE file, excluding those notices that do not
pertain to any part of the Derivative Works, in at least one
of the following places: within a NOTICE text file distributed
as part of the Derivative Works; within the Source form or
documentation, if provided along with the Derivative Works; or,
within a display generated by the Derivative Works, if and
wherever such third-party notices normally appear. The contents
of the NOTICE file are for informational purposes only and
do not modify the License. You may add Your own attribution
notices within Derivative Works that You distribute, alongside
or as an addendum to the NOTICE text from the Work, provided
that such additional attribution notices cannot be construed
as modifying the License.

You may add Your own copyright statement to Your modifications and
may provide additional or different license terms and conditions
for use, reproduction, or distribution of Your modifications, or
for any such Derivative Works as a whole, provided Your use,
reproduction, and distribution of the Work otherwise complies with
the conditions stated in this License.

5. Submission of Contributions. Unless You explicitly state otherwise,
any Contribution intentionally submitted for inclusion in the Work
by You to the Licensor shall be under the terms and conditions of
this License, without any additional terms or conditions.
Notwithstanding the above, nothing herein shall supersede or modify
the terms of any separate license agreement you may have executed
with Licensor regarding such Contributions.

6. Trademarks. This License does not grant permission to use the trade
names, trademarks, service marks, or product names of the Licensor,
except as required for reasonable and customary use in describing the
origin of the Work and reproducing the content of the NOTICE file.

7. Disclaimer of Warranty. Unless required by applicable law or
agreed to in writing, Licensor provides the Work (and each
Contributor provides its Contributions) on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, including, without limitation, any warranties or conditions
of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
PARTICULAR PURPOSE. You are solely responsible for determining the
appropriateness of using or redistributing the Work and assume any
risks associated with Your exercise of permissions under this License.

8. Limitation of Liability. In no event and under no legal theory,
whether in tort (including negligence), contract, or otherwise,
unless required by applicable law (such as deliberate and grossly
negligent acts) or agreed to in writing, shall any Contributor be
liable to You for damages, including any direct, indirect, special,
incidental, or consequential damages of any character arising as a
result of this License or out of the use or inability to use the
Work (including but not limited to damages for loss of goodwill,
work stoppage, computer failure or malfunction, or any and all
other commercial damages or losses), even if such Contributor
has been advised of the possibility of such damages.

9. Accepting Warranty or Additional Liability. While redistributing
the Work or Derivative Works thereof, You may choose to offer,
and charge a fee for, acceptance of support, warranty, indemnity,
or other liability obligations and/or rights consistent with this
License. However, in accepting such obligations, You may act only
on Your own behalf and on Your sole responsibility, not on behalf
of any other Contributor, and only if You agree to indemnify,
defend, and hold each Contributor harmless for any liability
incurred by, or claims asserted against, such Contributor by reason
of your accepting any such warranty or additional liability.

END OF TERMS AND CONDITIONS

APPENDIX: How to apply the Apache License to your work.

To apply the Apache License to your work, attach the following
boilerplate notice, with the fields enclosed by brackets "[]"
replaced with your own identifying information. (Don't include
the brackets!)  The text should be enclosed in the appropriate
comment syntax for the file format. We also recommend that a
file or class name and description of purpose be included on the
same "printed page" as the copyright notice for easier
identification within third-party archives.

Copyright {year} {full_name}

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"#,
    ))
}

#[test]
fn passende_lizenzen() -> Result<(), std::collections::BTreeSet<&'static str>> {
    use difference::Changeset;
    use either::Either;

    let lizenzen = verwendete_lizenzen();
    // Lizenz-Dateien, die nicht "LICENSE" heißen.
    let lizenz_dateien = BTreeMap::from([("aho-corasick-0.7.18", "LICENSE-MIT")]);

    let mut unterschiede = BTreeMap::new();
    for (name, f) in lizenzen {
        let datei = lizenz_dateien.get(name).unwrap_or(&"LICENSE");
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}/{datei}");
        match std::fs::read_to_string(lizenz_pfad.clone()) {
            Ok(gespeicherte_lizenz) => {
                let changeset = Changeset::new(&gespeicherte_lizenz, &verwendete_lizenz, "\n");
                if !changeset.diffs.is_empty() {
                    let _ = unterschiede.insert(name, Either::Left(changeset));
                }
            },
            Err(lese_fehler) => {
                let _ = unterschiede.insert(name, Either::Right((lizenz_pfad, lese_fehler)));
            },
        }
    }

    if unterschiede.is_empty() {
        Ok(())
    } else {
        let mut not_first = false;
        for (name, changeset_oder_fehler) in unterschiede.iter() {
            if not_first {
                eprintln!("---------------------------------");
            } else {
                not_first = true;
            }
            eprintln!("{name}");
            match changeset_oder_fehler {
                Either::Left(changeset) => {
                    eprintln!("{changeset}")
                },
                Either::Right((lizenz_pfad, lese_fehler)) => {
                    eprintln!("Fehler beim lesen der gespeicherten Lizenz \"{lizenz_pfad}\":\n{lese_fehler}")
                },
            }
        }
        Err(unterschiede.into_keys().collect())
    }
}
