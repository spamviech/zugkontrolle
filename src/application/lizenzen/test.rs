//! Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{self, Display, Formatter, Write},
    str::Split,
};

use difference::{Changeset, Difference};
use either::Either;
use log::error;
use nonempty::NonEmpty;

use crate::{
    application::lizenzen::{
        cargo_lock_lizenzen,
        texte::{mit_ohne_copyright, MITZeilenumbruch},
        verwendete_lizenzen_impl,
    },
    init_test_logging,
    unicase_ord::UniCaseOrd,
};

struct OptionD<'t, T>(&'t str, Option<T>);

impl<T: Display> Display for OptionD<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let OptionD(präfix, Some(t)) = self {
            write!(f, "{präfix}{t}")
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
/// Das verwendete Target wird nicht unterstützt.
struct UnbekanntesTarget<'t>(&'t str);

macro_rules! count_literals {
    ($(,)?) => {0usize};
    ($head: literal $(, $tail: literal)*) => {1usize + count_literals!($($tail),*)};
}

macro_rules! release_targets {
    ($($target: literal),*) => {
        /// Targets, für die bei einem Release binaries bereitgestellt werden.
        static RELEASE_TARGETS: [&'static str; count_literals!($($target),*)] = [$($target),*];

        /// Crates für das übergebene target, ausgehend von `cargo --filter-platform <target> metadata`.
        fn target_crates(target: &str) -> Result<Vec<(&'static str, &'static str)>, UnbekanntesTarget<'_>> {
            match target {
                $($target => Ok(zugkontrolle_macros::verwendete_crates!($target).into()),)*
                _ => Err(UnbekanntesTarget(target)),
            }
        }
    }
}

release_targets! {"x86_64-pc-windows-gnu", "armv7-unknown-linux-gnueabihf"}

fn cargo_lock_crates() -> HashSet<&'static str> {
    cargo_lock_lizenzen().into_iter().map(|(name, _lizenz)| name).collect()
}

#[test]
/// Test ob alle Lizenzen angezeigt werden.
/// Nimmt vorheriges ausführen von `python fetch_licenses.py` im licenses-Ordner an.
fn alle_lizenzen() -> Result<(), (BTreeSet<(&'static str, &'static str)>, usize)> {
    init_test_logging();

    let mut fehlend = BTreeSet::new();
    for target in RELEASE_TARGETS {
        let target_crates = target_crates(target).expect("cargo metadata failed!");
        let lizenzen: HashSet<_> = cargo_lock_crates();
        fehlend.extend(target_crates.into_iter().filter(|(name, _version)| {
            !lizenzen.contains(name) && !name.starts_with("zugkontrolle")
        }));
    }
    if fehlend.is_empty() {
        Ok(())
    } else {
        let anzahl = fehlend.len();
        Err((fehlend, anzahl))
    }
}

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

/// Lizenz-Dateien, die nicht "LICENSE" heißen.
fn lizenz_dateien() -> BTreeMap<UniCaseOrd<&'static str>, &'static str> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dev-dependency
    // cargo-lock = "8.0.1"

    [
        ("block", "TODO"),           // TODO Missing
        ("dispatch", "TODO"),        // TODO Missing
        ("glow_glyph", "TODO"),      // TODO Missing
        ("objc-foundation", "TODO"), // TODO Missing
        ("objc_id", "TODO"),         // TODO Missing
        ("sid", "TODO"),             // TODO Missing
        ("Lato", "../iced_graphics-0.3.0/fonts/OFL.txt"),
        ("SourceSerif4-Regular", "../../fonts/source-serif/LICENSE.md"),
        ("aho-corasick", "LICENSE-MIT"),
        ("android_glue", "LICENSE-GITHUB"),
        ("ansi_term", "LICENCE"),
        ("arrayvec", "LICENSE-MIT"),
        ("atomic-polyfill", "LICENSE-MIT"),
        ("autocfg", "LICENSE-MIT"),
        ("bare-metal", "LICENSE-MIT"),
        ("bare-metal", "LICENSE-MIT"),
        ("bincode", "LICENSE.md"),
        ("bit_field", "LICENSE-MIT"),
        ("bitfield", "LICENSE-MIT"),
        ("bitflags", "LICENSE-MIT"),
        ("bumpalo", "LICENSE-MIT"),
        ("bytemuck", "LICENSE-MIT"),
        ("bytemuck_derive", "../bytemuck-1.9.1/LICENSE-MIT"),
        ("byteorder", "LICENSE-MIT"),
        ("calloop", "LICENSE.txt"),
        ("camino", "LICENSE-MIT"),
        ("cargo_metadata", "LICENSE-MIT"),
        ("cargo-platform", "LICENSE-MIT"),
        ("cc", "LICENSE-MIT"),
        ("cfg-if", "LICENSE-MIT"),
        ("cfg-if", "LICENSE-MIT"),
        ("cgl", "LICENSE-MIT"),
        ("clipboard-win", "LICENSE-GITHUB"),
        ("cocoa", "LICENSE-MIT"),
        ("cocoa-foundation", "LICENSE-MIT-GITHUB"),
        ("core-foundation", "LICENSE-MIT"),
        ("core-foundation", "LICENSE-MIT"),
        ("core-foundation-sys", "LICENSE-MIT"),
        ("core-foundation-sys", "LICENSE-MIT"),
        ("core-graphics", "LICENSE-MIT"),
        ("core-graphics", "LICENSE-MIT"),
        ("core-graphics-types", "LICENSE-MIT-GITHUB"),
        ("cortex-m", "LICENSE-MIT"),
        ("critical-section", "../LICENSE-APACHE-2.0.txt"),
        ("crossbeam-channel", "LICENSE-MIT"),
        ("crossbeam-deque", "LICENSE-MIT"),
        ("crossbeam-epoch", "LICENSE-MIT"),
        ("crossbeam-utils", "LICENSE-MIT"),
        ("cty", "LICENSE-MIT"),
        ("current_platform", "LICENSE-MIT.md"),
        ("dlib", "LICENSE.txt"),
        ("downcast-rs", "LICENSE-MIT"),
        ("either", "LICENSE-MIT"),
        ("embedded-hal", "LICENSE-MIT"),
        ("error-code", "LICENSE-GITHUB"),
        ("euclid", "LICENSE-MIT"),
        ("flexi_logger", "LICENSE-MIT"),
        ("fnv", "LICENSE-MIT"),
        ("foreign-types", "LICENSE-MIT"),
        ("foreign-types-shared", "LICENSE-MIT"),
        ("form_urlencoded", "LICENSE-MIT"),
        ("futures", "LICENSE-MIT"),
        ("futures-channel", "LICENSE-MIT"),
        ("futures-core", "LICENSE-MIT"),
        ("futures-executor", "LICENSE-MIT"),
        ("futures-io", "LICENSE-MIT"),
        ("futures-macro", "LICENSE-MIT"),
        ("futures-sink", "LICENSE-MIT"),
        ("futures-task", "LICENSE-MIT"),
        ("futures-util", "LICENSE-MIT"),
        ("getrandom", "LICENSE-MIT"),
        ("gl_generator", "LICENSE-GITHUB"),
        ("glam", "LICENSE-MIT"),
        ("glob", "LICENSE-MIT"),
        ("glow", "LICENSE-MIT"),
        ("glutin_emscripten_sys", "LICENSE-GITHUB"),
        ("hash32", "LICENSE-MIT"),
        ("heapless", "LICENSE-MIT"),
        ("heck", "LICENSE-MIT"),
        ("hermit-abi", "LICENSE-MIT"),
        ("iced_core", "LICENSE-GITHUB"),
        ("iced_futures", "LICENSE-GITHUB"),
        ("iced_glow", "LICENSE-GITHUB"),
        ("iced_glutin", "LICENSE-GITHUB"),
        ("iced_graphics", "LICENSE-GITHUB"),
        ("iced_native", "LICENSE-GITHUB"),
        ("iced_style", "LICENSE-GITHUB"),
        ("iced_winit", "LICENSE-GITHUB"),
        ("itertools", "LICENSE-MIT"),
        ("itoa", "LICENSE-MIT"),
        ("jni-sys", "LICENSE-MIT"),
        ("js-sys", "LICENSE-MIT"),
        ("khronos_api", "LICENSE-GITHUB"),
        ("lazy_static", "LICENSE-MIT"),
        ("libc", "LICENSE-MIT"),
        ("libm", "LICENSE-MIT"),
        ("linked-hash-map", "LICENSE-MIT"),
        ("lock_api", "LICENSE-MIT"),
        ("log", "LICENSE-MIT"),
        ("lyon", "LICENSE-MIT-GITHUB"),
        ("lyon_algorithms", "LICENSE-MIT-GITHUB"),
        ("lyon_geom", "LICENSE-MIT-GITHUB"),
        ("lyon_path", "LICENSE-MIT-GITHUB"),
        ("lyon_tessellation", "LICENSE-MIT-GITHUB"),
        ("malloc_buf", "LICENSE-GITHUB"),
        ("memchr", "LICENSE-MIT"),
        ("memmap2", "LICENSE-MIT"),
        ("memmap2", "LICENSE-MIT"),
        ("minimal-lexical", "LICENSE-MIT"),
        ("nb", "LICENSE-MIT"),
        ("nb", "LICENSE-MIT"),
        ("ndk", "LICENSE-MIT-GITHUB"),
        ("ndk-context", "LICENSE-MIT-GITHUB"),
        ("ndk-glue", "LICENSE-MIT-GITHUB"),
        ("ndk-macro", "LICENSE-MIT-GITHUB"),
        ("ndk-sys", "LICENSE-MIT-GITHUB"),
        ("newline-converter", "LICENSE-GITHUB"),
        ("ntapi", "LICENSE-MIT"),
        ("num-traits", "LICENSE-MIT"),
        ("num_cpus", "LICENSE-MIT"),
        ("num_enum", "LICENSE-MIT"),
        ("num_enum_derive", "LICENSE-MIT"),
        ("num_threads", "LICENSE-MIT"),
        ("objc", "LICENSE.txt"),
        ("once_cell", "LICENSE-MIT"),
        ("ordered-float", "LICENSE-MIT"),
        ("parking_lot", "LICENSE-MIT"),
        ("parking_lot", "LICENSE-MIT"),
        ("parking_lot_core", "LICENSE-MIT"),
        ("parking_lot_core", "LICENSE-MIT"),
        ("percent-encoding", "LICENSE-MIT"),
        ("pin-project-lite", "LICENSE-MIT"),
        ("pin-utils", "LICENSE-MIT"),
        ("pkg-config", "LICENSE-MIT"),
        ("ppv-lite86", "LICENSE-MIT"),
        ("proc-macro-crate", "LICENSE-MIT"),
        ("proc-macro2", "LICENSE-MIT"),
        ("quote", "LICENSE-MIT"),
        ("rand", "LICENSE-MIT"),
        ("rand_chacha", "LICENSE-MIT"),
        ("rand_core", "LICENSE-MIT"),
        ("raw-window-handle", "LICENSE-MIT.md"),
        ("rayon", "LICENSE-MIT"),
        ("rayon-core", "LICENSE-MIT"),
        ("regex", "LICENSE-MIT"),
        ("regex-syntax", "LICENSE-MIT"),
        ("riscv", "LICENSE-README.md"),
        ("riscv-target", "LICENSE-MIT"),
        ("rstar", "LICENSE-MIT-GITHUB"),
        ("rustc-hash", "LICENSE-MIT"),
        ("rustc_version", "LICENSE-MIT"),
        ("rustc_version", "LICENSE-MIT"),
        ("rustversion", "LICENSE-MIT"),
        ("ryu", "LICENSE-APACHE"),
        ("scoped-tls", "LICENSE-MIT"),
        ("scopeguard", "LICENSE-MIT"),
        ("semver", "LICENSE-MIT"),
        ("semver", "LICENSE-MIT"),
        ("semver-parser", "LICENSE-MIT"),
        ("serde", "LICENSE-MIT"),
        ("serde_derive", "LICENSE-MIT"),
        ("serde_json", "LICENSE-MIT"),
        ("shared_library", "LICENSE-MIT"),
        ("smallvec", "LICENSE-MIT"),
        ("smithay-client-toolkit", "LICENSE.txt"),
        ("smithay-client-toolkit", "LICENSE.txt"),
        ("stable_deref_trait", "LICENSE-MIT"),
        ("static_assertions", "LICENSE-MIT"),
        ("str-buf", "LICENSE-GITHUB"),
        ("syn", "LICENSE-MIT"),
        ("thiserror", "LICENSE-MIT"),
        ("thiserror-impl", "LICENSE-MIT"),
        ("time", "LICENSE-MIT"),
        ("time-macros", "LICENSE-MIT"),
        ("tinyvec", "LICENSE-MIT.md"),
        ("toml", "LICENSE-MIT"),
        ("ttf-parser", "LICENSE-MIT"),
        ("twox-hash", "LICENSE.txt"),
        ("osmesa-sys", "../CC0.txt"),
        ("unicase", "LICENSE-MIT"),
        ("unicode-ident", "LICENSE-MIT"),
        ("unicode-normalization", "LICENSE-MIT"),
        ("unicode-segmentation", "LICENSE-MIT"),
        ("vcell", "LICENSE-MIT"),
        ("version_check", "LICENSE-MIT"),
        ("void", "LICENSE-MIT-GITHUB"),
        ("volatile-register", "LICENSE-MIT"),
        ("wasi", "LICENSE-MIT"),
        ("wasm-bindgen", "LICENSE-MIT"),
        ("wasm-bindgen-backend", "LICENSE-MIT"),
        ("wasm-bindgen-futures", "LICENSE-MIT"),
        ("wasm-bindgen-macro", "LICENSE-MIT"),
        ("wasm-bindgen-macro-support", "LICENSE-MIT"),
        ("wasm-bindgen-shared", "LICENSE-MIT"),
        ("wayland-client", "LICENSE.txt"),
        ("wayland-commons", "LICENSE.txt"),
        ("wayland-cursor", "LICENSE.txt"),
        ("wayland-egl", "LICENSE.txt"),
        ("wayland-protocols", "LICENSE.txt"),
        ("wayland-scanner", "LICENSE.txt"),
        ("wayland-sys", "LICENSE.txt"),
        ("web-sys", "LICENSE-MIT"),
        ("winapi", "LICENSE-MIT"),
        ("winapi-i686-pc-windows-gnu", "LICENSE-MIT-GITHUB"),
        ("winapi-wsapoll", "../LICENSE-APACHE-2.0.txt"),
        ("winapi-x86_64-pc-windows-gnu", "LICENSE-MIT-GITHUB"),
        ("windows-sys", "LICENSE-MIT"),
        ("windows_aarch64_msvc", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_aarch64", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_gnu", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_msvc", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_gnu", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_msvc", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("x11-dl", "LICENSE-MIT"),
        ("x11rb", "LICENSE-MIT"),
        ("xi-unicode", "LICENSE-GITHUB"),
    ]
    .into_iter()
    .map(|(name, pfad)| (UniCaseOrd::neu(name), pfad))
    .collect()
}

#[test]
/// Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.
fn passende_lizenzen() -> Result<(), (BTreeSet<(&'static str, &'static str)>, usize)> {
    init_test_logging();

    let release_target_crates = RELEASE_TARGETS
        .into_iter()
        .flat_map(|target| {
            let mut crates: HashMap<&'static str, NonEmpty<&'static str>> = HashMap::new();
            for (name, version) in target_crates(target).expect("unbekanntes target!") {
                use std::collections::hash_map::Entry;
                match crates.entry(name) {
                    Entry::Occupied(mut o) => o.get_mut().push(version),
                    Entry::Vacant(v) => {
                        let _ = v.insert(NonEmpty::singleton(version));
                    },
                }
            }
            crates
        })
        .collect();
    let lizenzen: BTreeMap<_, _> =
        verwendete_lizenzen_impl(release_target_crates, |name, version| (name, version));
    let lizenz_dateien = lizenz_dateien();

    let mut unterschiede = BTreeMap::new();
    let is_diff = |diff: &Difference| {
        if let Difference::Same(_) = diff {
            false
        } else {
            true
        }
    };
    for ((name, version), f) in lizenzen {
        let datei = lizenz_dateien.get(&UniCaseOrd::neu(name)).unwrap_or(&"LICENSE");
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}-{version}/{datei}");
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
                    let _ = unterschiede
                        .insert((name, version), Either::Left((changeset, mit_changesets)));
                }
            },
            Err(lese_fehler) => {
                let _ =
                    unterschiede.insert((name, version), Either::Right((lizenz_pfad, lese_fehler)));
            },
        }
    }

    if unterschiede.is_empty() {
        Ok(())
    } else {
        for ((name, version), changeset_oder_fehler) in unterschiede.iter() {
            let mut fehlermeldung = format!("{name}-{version}\x1b[0m\n");
            match changeset_oder_fehler {
                Either::Left((changeset, mit_changesets)) => {
                    fehlermeldung.push_str(&changeset_als_string(changeset));
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
                            writeln!(
                                fehlermeldung,
                                "\nNächste MIT-Zeilenumbrüche: {zeilenumbrüche:?}\n{}",
                                changeset_als_string(mit_changeset)
                            )
                            .unwrap();
                        }
                        fehlermeldung.push('\n');
                        fehlermeldung.push_str(name.as_ref());
                        fehlermeldung.push('\n');
                    }
                },
                Either::Right((lizenz_pfad, lese_fehler)) => {
                    writeln!(
                        fehlermeldung,
                        "Fehler beim lesen der gespeicherten Lizenz \"{lizenz_pfad}\":\n{lese_fehler}"
                    )
                    .unwrap();
                },
            }
            error!("{fehlermeldung}")
        }
        let set: BTreeSet<_> = unterschiede.into_keys().collect();
        let anzahl = set.len();
        Err((set, anzahl))
    }
}
