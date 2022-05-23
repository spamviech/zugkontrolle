//! Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.

use std::{
    collections::{BTreeMap, BTreeSet},
    ffi::OsStr,
    fs,
    str::Split,
};

use difference::{Changeset, Difference};
use either::Either;

use crate::application::lizenzen::{
    texte::{mit_ohne_copyright, MITZeilenumbruch},
    verwendete_lizenzen,
};

fn push_letzte_same(
    string: &mut String,
    letzte_same: &mut Option<Split<'_, &String>>,
    split: &str,
) {
    match letzte_same.take() {
        Some(iter) => {
            let mut ellipsis = false;
            let mut vorletztes = None;
            let mut letztes = None;
            for s in iter {
                ellipsis = vorletztes.is_some();
                vorletztes = letztes;
                letztes = Some(s);
            }
            if let Some(letztes) = letztes {
                if ellipsis {
                    string.push_str("...");
                    string.push_str(split);
                }
                if let Some(vorletztes) = vorletztes {
                    string.push_str(vorletztes);
                    string.push_str(split);
                }
                string.push_str(letztes);
                string.push_str(split);
            }
        },
        None => {},
    }
}

fn push_diff(string: &mut String, diff: &str, farbe_str: &str, split: &str) {
    string.push_str(farbe_str);
    let mut erstes = true;
    for teil_string in diff.split(split) {
        if erstes {
            erstes = false;
        } else {
            string.push_str(split);
        }
        if teil_string.is_empty() {
            string.push_str("<Leerer String>");
        } else if teil_string.trim().is_empty() {
            string.push('"');
            string.push_str(teil_string);
            string.push('"');
        } else {
            string.push_str(teil_string);
        }
    }
    string.push_str("\x1b[0m");
    string.push_str(split);
}

fn changeset_als_string(changeset: &Changeset) -> String {
    let mut string = String::new();
    let mut letzte_same = None;
    let mut letzte_diff = false;
    let split = &changeset.split;
    for diff in &changeset.diffs {
        match diff {
            Difference::Same(x) => {
                let mut iter = x.split(split);
                if letzte_diff {
                    if let Some(nächstes) = iter.next() {
                        string.push_str(nächstes);
                        string.push_str(split);
                    }
                    if let Some(nächstes) = iter.next() {
                        string.push_str(nächstes);
                        string.push_str(split);
                    }
                }
                letzte_same = Some(iter);
                letzte_diff = false;
            },
            Difference::Add(x) => {
                push_letzte_same(&mut string, &mut letzte_same, split);
                letzte_diff = true;
                push_diff(&mut string, x, "\x1b[92m", split);
            },
            Difference::Rem(x) => {
                push_letzte_same(&mut string, &mut letzte_same, split);
                letzte_diff = true;
                push_diff(&mut string, x, "\x1b[91m", split);
            },
        }
    }
    if let Some(iter) = letzte_same {
        if iter.count() > 1 {
            string.push_str("...");
            string.push_str(split);
        }
    }
    string
}

impl MITZeilenumbruch {
    fn alle() -> impl Iterator<Item = Self> {
        use MITZeilenumbruch::*;
        [Standard, Winreg, X11, Iced, WasmTimer, RPPal, Redox, NonEmpty, Keine].into_iter()
    }
}

// regex ist nur eine dev-dependency, damit es heruntergeladen wird.
// Es ist Teil des dependency trees für riscv targets.
use regex as _;

#[test]
/// Test ob alle Lizenzen angezeigt werden.
/// Nimmt vorheriges ausführen von `python fetch_licenses.py` im licenses-Ordner an.
fn alle_lizenzen() -> Result<(), (BTreeSet<String>, usize)> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dev-dependency
    // cargo-lock = "8.0.1"

    let lizenzen: BTreeSet<_> = verwendete_lizenzen().into_iter().map(|(name, _f)| name).collect();
    let mut fehlend = BTreeSet::new();
    for entry_res in fs::read_dir("licenses").expect("In git-repository eingecheckt.") {
        match entry_res {
            Ok(entry) => {
                let pfad = entry.path();
                if pfad.is_dir() {
                    if let Some(pfad_str) = pfad.file_name().and_then(OsStr::to_str) {
                        if !lizenzen.contains(pfad_str) {
                            let _ = fehlend.insert(pfad_str.to_owned());
                        }
                    } else {
                        eprintln!("Pfad konnte nicht nach str konvertiert werden: {pfad:?}");
                    }
                }
            },
            Err(fehler) => {
                eprintln!("{fehler:?}");
            },
        }
    }
    if fehlend.is_empty() {
        Ok(())
    } else {
        let anzahl = fehlend.len();
        Err((fehlend, anzahl))
    }
}

#[test]
/// Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.
fn passende_lizenzen() -> Result<(), (BTreeSet<&'static str>, usize)> {
    let lizenzen = verwendete_lizenzen();
    // Lizenz-Dateien, die nicht "LICENSE" heißen.
    let lizenz_dateien = BTreeMap::from([
        ("Lato", "../iced_graphics-0.3.0/fonts/OFL.txt"),
        ("SourceSerif4-Regular", "../../fonts/source-serif/LICENSE.md"),
        ("aho-corasick-0.7.18", "LICENSE-MIT"),
        ("android_glue-0.2.3", "LICENSE-GITHUB"),
        ("ansi_term-0.12.1", "LICENCE"),
        ("arrayvec-0.5.2", "LICENSE-MIT"),
        ("atomic-polyfill-0.1.8", "LICENSE-MIT"),
        ("autocfg-1.1.0", "LICENSE-MIT"),
        ("bare-metal-0.2.5", "LICENSE-MIT"),
        ("bare-metal-1.0.0", "LICENSE-MIT"),
        ("bincode-1.3.3", "LICENSE.md"),
        ("bit_field-0.10.1", "LICENSE-MIT"),
        ("bitfield-0.13.2", "LICENSE-MIT"),
        ("bitflags-1.3.2", "LICENSE-MIT"),
        ("block-0.1.6", "TODO"), // TODO
        ("bumpalo-2.6.0", "LICENSE-MIT"),
        ("bumpalo-3.9.1", "LICENSE-MIT"),
        ("bytemuck-1.9.1", "LICENSE-MIT"),
        ("bytemuck_derive-1.1.0", "LICENSE-MIT"),
        ("byteorder-1.4.3", "LICENSE-MIT"),
        ("calloop-0.9.3", "LICENSE.txt"),
        ("cc-1.0.73", "LICENSE-MIT"),
        ("cfg-if-0.1.10", "LICENSE-MIT"),
        ("cfg-if-1.0.0", "LICENSE-MIT"),
        ("cgl-0.3.2", "LICENSE-MIT"),
        ("clipboard-win-4.4.1", "LICENSE-GITHUB"),
        ("cocoa-0.24.0", "LICENSE-MIT"),
        ("cocoa-foundation-0.1.0", "LICENSE-MIT-GITHUB"),
        ("core-foundation-0.7.0", "LICENSE-MIT"),
        ("core-foundation-0.9.3", "LICENSE-MIT"),
        ("core-foundation-sys-0.7.0", "LICENSE-MIT"),
        ("core-foundation-sys-0.8.3", "LICENSE-MIT"),
        ("core-graphics-0.19.2", "LICENSE-MIT"),
        ("core-graphics-0.22.3", "LICENSE-MIT"),
        ("core-graphics-types-0.1.1", "LICENSE-MIT-GITHUB"),
        ("cortex-m-0.7.4", "LICENSE-MIT"),
        ("critical-section-0.2.7", "TODO"), // TODO
        ("crossbeam-channel-0.5.4", "LICENSE-MIT"),
        ("crossbeam-deque-0.8.1", "LICENSE-MIT"),
        ("crossbeam-epoch-0.9.8", "LICENSE-MIT"),
        ("crossbeam-utils-0.8.8", "LICENSE-MIT"),
        ("cty-0.2.2", "LICENSE-MIT"),
        ("dispatch-0.2.0", "TODO"), // TODO
        ("dlib-0.5.0", "LICENSE.txt"),
        ("downcast-rs-1.2.0", "LICENSE-MIT"),
        ("either-1.6.1", "LICENSE-MIT"),
        ("embedded-hal-0.2.7", "LICENSE-MIT"),
        ("error-code-2.3.1", "LICENSE-GITHUB"),
        ("euclid-0.22.7", "LICENSE-MIT"),
        ("flexi_logger-0.22.3", "LICENSE-MIT"),
        ("fnv-1.0.7", "LICENSE-MIT"),
        ("foreign-types-0.3.2", "LICENSE-MIT"),
        ("foreign-types-shared-0.1.1", "LICENSE-MIT"),
        ("form_urlencoded-1.0.1", "LICENSE-MIT"),
        ("futures-0.3.21", "LICENSE-MIT"),
        ("futures-channel-0.3.21", "LICENSE-MIT"),
        ("futures-core-0.3.21", "LICENSE-MIT"),
        ("futures-executor-0.3.21", "LICENSE-MIT"),
        ("futures-io-0.3.21", "LICENSE-MIT"),
        ("futures-macro-0.3.21", "LICENSE-MIT"),
        ("futures-sink-0.3.21", "LICENSE-MIT"),
        ("futures-task-0.3.21", "LICENSE-MIT"),
        ("futures-util-0.3.21", "LICENSE-MIT"),
        ("fxhash-0.2.1", "TODO"), // TODO
        ("getrandom-0.2.6", "LICENSE-MIT"),
        ("gl_generator-0.14.0", "LICENSE-GITHUB"),
        ("glam-0.10.2", "LICENSE-MIT"),
        ("glob-0.3.0", "LICENSE-MIT"),
        ("glow-0.11.2", "LICENSE-MIT"),
        ("glow_glyph-0.5.1", "TODO"), // TODO
        ("glutin_emscripten_sys-0.1.1", "LICENSE-GITHUB"),
        ("hash32-0.2.1", "LICENSE-MIT"),
        ("heapless-0.7.13", "LICENSE-MIT"),
        ("heck-0.4.0", "LICENSE-MIT"),
        ("hermit-abi-0.1.19", "LICENSE-MIT"),
        ("iced_aw-0.1.0", "9a99d5b/LICENSE"),
        ("iced_core-0.4.0", "LICENSE-GITHUB"),
        ("iced_core-0.5.0", "LICENSE-GITHUB"),
        ("iced_futures-0.3.0", "LICENSE-GITHUB"),
        ("iced_futures-0.4.0", "LICENSE-GITHUB"),
        ("iced_glow-0.3.0", "LICENSE-GITHUB"),
        ("iced_glutin-0.3.0", "LICENSE-GITHUB"),
        ("iced_graphics-0.3.0", "LICENSE-GITHUB"),
        ("iced_native-0.5.0", "LICENSE-GITHUB"),
        ("iced_style-0.3.0", "LICENSE-GITHUB"),
        ("iced_style-0.4.0", "LICENSE-GITHUB"),
        ("iced_web-0.4.0", "LICENSE-GITHUB"),
        ("iced_winit-0.4.0", "LICENSE-GITHUB"),
        ("idna-0.2.3", "LICENSE-MIT"),
        ("itertools-0.10.3", "LICENSE-MIT"),
        ("itoa-1.0.2", "LICENSE-MIT"),
        ("jni-sys-0.3.0", "LICENSE-MIT"),
        ("js-sys-0.3.57", "LICENSE-MIT"),
        ("khronos_api-3.1.0", "LICENSE-GITHUB"),
        ("lazy_static-1.4.0", "LICENSE-MIT"),
        ("libc-0.2.126", "LICENSE-MIT"),
        ("libm-0.2.2", "LICENSE-MIT"),
        ("linked-hash-map-0.5.4", "LICENSE-MIT"),
        ("lock_api-0.4.7", "LICENSE-MIT"),
        ("log-0.4.17", "LICENSE-MIT"),
        ("longest-increasing-subsequence-0.1.0", "LICENSE-MIT"),
        ("lyon-0.17.10", "LICENSE-MIT-GITHUB"),
        ("lyon_algorithms-0.17.7", "LICENSE-MIT-GITHUB"),
        ("lyon_geom-0.17.6", "LICENSE-MIT-GITHUB"),
        ("lyon_path-0.17.7", "LICENSE-MIT-GITHUB"),
        ("lyon_tessellation-0.17.10", "LICENSE-MIT-GITHUB"),
        ("malloc_buf-0.0.6", "LICENSE-GITHUB"),
        ("memchr-2.5.0", "LICENSE-MIT"),
        ("memmap2-0.3.1", "LICENSE-MIT"),
        ("minimal-lexical-0.2.1", "LICENSE-MIT"),
        ("nb-0.1.3", "LICENSE-MIT"),
        ("nb-1.0.0", "LICENSE-MIT"),
        ("ndk-0.5.0", "LICENSE-MIT-GITHUB"),
        ("ndk-context-0.1.1", "LICENSE-MIT-GITHUB"),
        ("ndk-glue-0.5.2", "LICENSE-MIT-GITHUB"),
        ("ndk-macro-0.3.0", "LICENSE-MIT-GITHUB"),
        ("ndk-sys-0.2.2", "LICENSE-MIT-GITHUB"),
        ("newline-converter-0.2.0", "LICENSE-GITHUB"),
        ("ntapi-0.3.7", "LICENSE-MIT"),
        ("num-traits-0.2.15", "LICENSE-MIT"),
        ("num_cpus-1.13.1", "LICENSE-MIT"),
        ("num_enum-0.5.7", "LICENSE-MIT"),
        ("num_enum_derive-0.5.7", "LICENSE-MIT"),
        ("num_threads-0.1.6", "LICENSE-MIT"),
        ("objc-0.2.7", "LICENSE.txt"),
        ("objc-foundation-0.1.1", "TODO"), // TODO
        ("objc_id-0.1.1", "TODO"),         // TODO
        ("once_cell-1.11.0", "LICENSE-MIT"),
        ("ordered-float-3.0.0", "LICENSE-MIT"),
        ("osmesa-sys-0.1.2", "TODO"), // TODO
        ("parking_lot-0.11.2", "LICENSE-MIT"),
        ("parking_lot-0.12.0", "LICENSE-MIT"),
        ("parking_lot_core-0.8.5", "LICENSE-MIT"),
        ("parking_lot_core-0.9.3", "LICENSE-MIT"),
        ("percent-encoding-2.1.0", "LICENSE-MIT"),
        ("pin-project-lite-0.2.9", "LICENSE-MIT"),
        ("pin-utils-0.1.0", "LICENSE-MIT"),
        ("pkg-config-0.3.25", "LICENSE-MIT"),
        ("ppv-lite86-0.2.16", "LICENSE-MIT"),
        ("proc-macro-crate-1.1.3", "LICENSE-MIT"),
        ("proc-macro2-1.0.39", "LICENSE-MIT"),
        ("quote-1.0.18", "LICENSE-MIT"),
        ("rand-0.8.5", "LICENSE-MIT"),
        ("rand_chacha-0.3.1", "LICENSE-MIT"),
        ("rand_core-0.6.3", "LICENSE-MIT"),
        ("raw-window-handle-0.4.3", "LICENSE-MIT.md"),
        ("rayon-1.5.3", "LICENSE-MIT"),
        ("rayon-core-1.9.3", "LICENSE-MIT"),
        ("regex-1.5.6", "LICENSE-MIT"),
        ("regex-syntax-0.6.26", "LICENSE-MIT"),
        ("riscv-0.7.0", "LICENSE-README.md"),
        ("riscv-target-0.1.2", "LICENSE-MIT"),
        ("rstar-0.9.3", "LICENSE-MIT-GITHUB"),
        ("rustc-hash-1.1.0", "LICENSE-MIT"),
        ("rustc_version-0.2.3", "LICENSE-MIT"),
        ("rustc_version-0.4.0", "LICENSE-MIT"),
        ("rustversion-1.0.6", "LICENSE-MIT"),
        ("scoped-tls-1.0.0", "LICENSE-MIT"),
        ("scopeguard-1.1.0", "LICENSE-MIT"),
        ("semver-0.9.0", "LICENSE-MIT"),
        ("semver-1.0.9", "LICENSE-MIT"),
        ("semver-parser-0.7.0", "LICENSE-MIT"),
        ("serde-1.0.137", "LICENSE-MIT"),
        ("serde_derive-1.0.137", "LICENSE-MIT"),
        ("shared_library-0.1.9", "LICENSE-MIT"),
        ("sid-0.6.1", "TODO"), // TODO
        ("smallvec-1.8.0", "LICENSE-MIT"),
        ("smithay-client-toolkit-0.15.4", "LICENSE.txt"),
        ("stable_deref_trait-1.2.0", "LICENSE-MIT"),
        ("static_assertions-1.1.0", "LICENSE-MIT"),
        ("str-buf-1.0.5", "LICENSE-GITHUB"),
        ("syn-1.0.95", "LICENSE-MIT"),
        ("thiserror-1.0.31", "LICENSE-MIT"),
        ("thiserror-impl-1.0.31", "LICENSE-MIT"),
        ("time-0.3.9", "LICENSE-MIT"),
        ("time-macros-0.2.4", "LICENSE-MIT"),
        ("tinyvec-1.6.0", "LICENSE-MIT.md"),
        ("toml-0.5.9", "LICENSE-MIT"),
        ("ttf-parser-0.15.0", "LICENSE-MIT"),
        ("twox-hash-1.6.3", "LICENSE.txt"),
        ("unicase-2.6.0", "LICENSE-MIT"),
        ("unicode-bidi-0.3.8", "LICENSE-MIT"),
        ("unicode-ident-1.0.0", "LICENSE-MIT"),
        ("unicode-normalization-0.1.19", "LICENSE-MIT"),
        ("unicode-segmentation-1.9.0", "LICENSE-MIT"),
        ("url-2.2.2", "LICENSE-MIT"),
        ("vcell-0.1.3", "LICENSE-MIT"),
        ("version_check-0.9.4", "LICENSE-MIT"),
        ("void-1.0.2", "LICENSE-MIT-GITHUB"),
        ("volatile-register-0.2.1", "LICENSE-MIT"),
        ("wasi-0.10.2+wasi-snapshot-preview1", "LICENSE-MIT"),
        ("wasi-0.11.0+wasi-snapshot-preview1", "LICENSE-MIT"),
        ("wasm-bindgen-0.2.80", "LICENSE-MIT"),
        ("wasm-bindgen-backend-0.2.80", "LICENSE-MIT"),
        ("wasm-bindgen-futures-0.4.30", "LICENSE-MIT"),
        ("wasm-bindgen-macro-0.2.80", "LICENSE-MIT"),
        ("wasm-bindgen-macro-support-0.2.80", "LICENSE-MIT"),
        ("wasm-bindgen-shared-0.2.80", "LICENSE-MIT"),
        ("wayland-client-0.29.4", "LICENSE.txt"),
        ("wayland-commons-0.29.4", "LICENSE.txt"),
        ("wayland-cursor-0.29.4", "LICENSE.txt"),
        ("wayland-egl-0.29.4", "LICENSE.txt"),
        ("wayland-protocols-0.29.4", "LICENSE.txt"),
        ("wayland-scanner-0.29.4", "LICENSE.txt"),
        ("wayland-sys-0.29.4", "LICENSE.txt"),
        ("web-sys-0.3.57", "LICENSE-MIT"),
        ("winapi-0.3.9", "LICENSE-MIT"),
        ("winapi-i686-pc-windows-gnu-0.4.0", "LICENSE-MIT-GITHUB"),
        ("winapi-wsapoll-0.1.1", "TODO"), // TODO
        ("winapi-x86_64-pc-windows-gnu-0.4.0", "LICENSE-MIT-GITHUB"),
        ("windows-sys-0.36.1", "LICENSE-MIT"),
        ("windows_aarch64_msvc-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_aarch64-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_gnu-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_msvc-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_gnu-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_msvc-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("x11-dl-2.19.1", "LICENSE-MIT"),
        ("x11rb-0.9.0", "LICENSE-MIT"),
        ("xi-unicode-0.3.0", "LICENSE-GITHUB"),
    ]);

    let mut unterschiede = BTreeMap::new();
    let is_diff = |diff: &Difference| {
        if let Difference::Same(_) = diff {
            false
        } else {
            true
        }
    };
    for (name, f) in lizenzen {
        let datei = lizenz_dateien.get(name).unwrap_or(&"LICENSE");
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}/{datei}");
        match std::fs::read_to_string(lizenz_pfad.clone()) {
            Ok(gespeicherte_lizenz) => {
                let gespeicherte_lizenz_unix = gespeicherte_lizenz.replace("\r\n", "\n");
                let changeset = Changeset::new(&gespeicherte_lizenz_unix, &verwendete_lizenz, "\n");
                if changeset.diffs.iter().any(is_diff) {
                    let mit_changesets: Vec<_> = MITZeilenumbruch::alle()
                        .map(|zeilenumbrüche| {
                            (
                                zeilenumbrüche,
                                Changeset::new(
                                    &gespeicherte_lizenz_unix,
                                    &mit_ohne_copyright(zeilenumbrüche),
                                    "\n",
                                ),
                            )
                        })
                        .collect();
                    let _ = unterschiede.insert(name, Either::Left((changeset, mit_changesets)));
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
                Either::Left((changeset, mit_changesets)) => {
                    eprintln!("{}", changeset_als_string(changeset));
                    if let Some((zeilenumbrüche, mit_changeset)) =
                        mit_changesets.iter().min_by_key(|(_z, c)| c.distance)
                    {
                        let is_non_whitespace_same = |diff: &Difference| {
                            if let Difference::Same(t) = diff {
                                !t.trim().is_empty()
                            } else {
                                false
                            }
                        };
                        // Zeige nur Changesets mit mindestens einer Übereinstimmung.
                        // (schlage keinen MIT-Zeilenumbruch bei Apache-Lizenz vor)
                        if mit_changeset.diffs.iter().any(is_non_whitespace_same) {
                            eprintln!("\nNächste MIT-Zeilenumbrüche: {zeilenumbrüche:?}");
                            eprintln!("{}", changeset_als_string(mit_changeset));
                        }
                        eprintln!("\n{name}");
                    }
                },
                Either::Right((lizenz_pfad, lese_fehler)) => {
                    eprintln!("Fehler beim lesen der gespeicherten Lizenz \"{lizenz_pfad}\":");
                    eprintln!("{lese_fehler}");
                },
            }
        }
        if not_first {
            eprintln!("---------------------------------");
        }
        let set: BTreeSet<_> = unterschiede.into_keys().collect();
        let anzahl = set.len();
        Err((set, anzahl))
    }
}
