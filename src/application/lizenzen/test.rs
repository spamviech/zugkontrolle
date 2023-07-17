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
fn lizenz_dateien(
) -> BTreeMap<UniCaseOrd<&'static str>, (&'static str, HashMap<&'static str, &'static str>)> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dev-dependency
    // cargo-lock = "8.0.1"

    [
        ("block", ("TODO", HashMap::new())),           // TODO Missing
        ("dispatch", ("TODO", HashMap::new())),        // TODO Missing
        ("glow_glyph", ("TODO", HashMap::new())),      // TODO Missing
        ("objc-foundation", ("TODO", HashMap::new())), // TODO Missing
        ("objc_id", ("TODO", HashMap::new())),         // TODO Missing
        ("sid", ("TODO", HashMap::new())),             // TODO Missing
        ("expat-sys", ("TODO", HashMap::new())),       // TODO Missing
        ("Lato", ("../iced_graphics-0.3.0/fonts/OFL.txt", HashMap::new())),
        ("SourceSerif4-Regular", ("../../fonts/source-serif/LICENSE.md", HashMap::new())),
        ("fdeflate", ("LICENSE-GITHUB", HashMap::new())),
        ("aho-corasick", ("LICENSE-MIT", HashMap::new())),
        ("android_glue", ("LICENSE-GITHUB", HashMap::new())),
        ("ansi_term", ("LICENCE", HashMap::new())),
        ("arrayvec", ("LICENSE-MIT", HashMap::new())),
        ("atomic-polyfill", ("LICENSE-MIT", HashMap::new())),
        ("autocfg", ("LICENSE-MIT", HashMap::new())),
        ("bare-metal", ("LICENSE-MIT", HashMap::new())),
        ("bare-metal", ("LICENSE-MIT", HashMap::new())),
        ("bincode", ("LICENSE.md", HashMap::new())),
        ("bit_field", ("LICENSE-MIT", HashMap::new())),
        ("bitfield", ("LICENSE-MIT", HashMap::new())),
        ("bitflags", ("LICENSE-MIT", HashMap::new())),
        ("bumpalo", ("LICENSE-MIT", HashMap::new())),
        ("bytemuck", ("LICENSE-MIT", HashMap::new())),
        ("bytemuck_derive", ("../bytemuck-1.9.1/LICENSE-MIT", HashMap::new())),
        ("byteorder", ("LICENSE-MIT", HashMap::new())),
        ("calloop", ("LICENSE.txt", HashMap::new())),
        ("camino", ("LICENSE-MIT", HashMap::new())),
        ("cargo_metadata", ("LICENSE-MIT", HashMap::new())),
        ("cargo-platform", ("LICENSE-MIT", HashMap::new())),
        ("cc", ("LICENSE-MIT", HashMap::new())),
        ("cfg-if", ("LICENSE-MIT", HashMap::new())),
        ("cfg-if", ("LICENSE-MIT", HashMap::new())),
        ("cgl", ("LICENSE-MIT", HashMap::new())),
        ("clipboard-win", ("LICENSE-GITHUB", HashMap::new())),
        ("cocoa", ("LICENSE-MIT", HashMap::new())),
        ("cocoa-foundation", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("core-foundation", ("LICENSE-MIT", HashMap::new())),
        ("core-foundation", ("LICENSE-MIT", HashMap::new())),
        ("core-foundation-sys", ("LICENSE-MIT", HashMap::new())),
        ("core-foundation-sys", ("LICENSE-MIT", HashMap::new())),
        ("core-graphics", ("LICENSE-MIT", HashMap::new())),
        ("core-graphics", ("LICENSE-MIT", HashMap::new())),
        ("core-graphics-types", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("cortex-m", ("LICENSE-MIT", HashMap::new())),
        ("critical-section", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("crossbeam-channel", ("LICENSE-MIT", HashMap::new())),
        ("crossbeam-deque", ("LICENSE-MIT", HashMap::new())),
        ("crossbeam-epoch", ("LICENSE-MIT", HashMap::new())),
        ("crossbeam-utils", ("LICENSE-MIT", HashMap::new())),
        ("cty", ("LICENSE-MIT", HashMap::new())),
        ("current_platform", ("LICENSE-MIT.md", HashMap::new())),
        ("dlib", ("LICENSE.txt", HashMap::new())),
        ("downcast-rs", ("LICENSE-MIT", HashMap::new())),
        ("either", ("LICENSE-MIT", HashMap::new())),
        ("embedded-hal", ("LICENSE-MIT", HashMap::new())),
        ("error-code", ("LICENSE-GITHUB", HashMap::new())),
        ("euclid", ("LICENSE-MIT", HashMap::new())),
        ("flexi_logger", ("LICENSE-MIT", HashMap::new())),
        ("fnv", ("LICENSE-MIT", HashMap::new())),
        ("foreign-types", ("LICENSE-MIT", HashMap::new())),
        ("foreign-types-shared", ("LICENSE-MIT", HashMap::new())),
        ("form_urlencoded", ("LICENSE-MIT", HashMap::new())),
        ("futures", ("LICENSE-MIT", HashMap::new())),
        ("futures-channel", ("LICENSE-MIT", HashMap::new())),
        ("futures-core", ("LICENSE-MIT", HashMap::new())),
        ("futures-executor", ("LICENSE-MIT", HashMap::new())),
        ("futures-io", ("LICENSE-MIT", HashMap::new())),
        ("futures-macro", ("LICENSE-MIT", HashMap::new())),
        ("futures-sink", ("LICENSE-MIT", HashMap::new())),
        ("futures-task", ("LICENSE-MIT", HashMap::new())),
        ("futures-util", ("LICENSE-MIT", HashMap::new())),
        ("getrandom", ("LICENSE-MIT", HashMap::new())),
        ("gl_generator", ("LICENSE-GITHUB", HashMap::new())),
        ("glam", ("LICENSE-MIT", HashMap::new())),
        ("glob", ("LICENSE-MIT", HashMap::new())),
        ("glow", ("LICENSE-MIT", HashMap::new())),
        ("glutin_emscripten_sys", ("LICENSE-GITHUB", HashMap::new())),
        ("hash32", ("LICENSE-MIT", HashMap::new())),
        ("heapless", ("LICENSE-MIT", HashMap::new())),
        ("heck", ("LICENSE-MIT", HashMap::new())),
        ("hermit-abi", ("LICENSE-MIT", HashMap::new())),
        ("iced_core", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_futures", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_glow", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_glutin", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_graphics", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_native", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_style", ("LICENSE-GITHUB", HashMap::new())),
        ("iced_winit", ("LICENSE-GITHUB", HashMap::new())),
        ("itertools", ("LICENSE-MIT", HashMap::new())),
        ("itoa", ("LICENSE-MIT", HashMap::new())),
        ("jni-sys", ("LICENSE-MIT", HashMap::new())),
        ("js-sys", ("LICENSE-MIT", HashMap::new())),
        ("khronos_api", ("LICENSE-GITHUB", HashMap::new())),
        ("lazy_static", ("LICENSE-MIT", HashMap::new())),
        ("libc", ("LICENSE-MIT", HashMap::new())),
        ("libm", ("LICENSE-MIT", HashMap::new())),
        ("linked-hash-map", ("LICENSE-MIT", HashMap::new())),
        ("lock_api", ("LICENSE-MIT", HashMap::new())),
        ("log", ("LICENSE-MIT", HashMap::new())),
        ("lyon", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("lyon_algorithms", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("lyon_geom", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("lyon_path", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("lyon_tessellation", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("malloc_buf", ("LICENSE-GITHUB", HashMap::new())),
        ("memchr", ("LICENSE-MIT", HashMap::new())),
        ("memmap2", ("LICENSE-MIT", HashMap::new())),
        ("minimal-lexical", ("LICENSE-MIT", HashMap::new())),
        ("nb", ("LICENSE-MIT", HashMap::new())),
        ("nb", ("LICENSE-MIT", HashMap::new())),
        ("ndk", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("ndk-context", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("ndk-glue", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("ndk-macro", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("ndk-sys", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("newline-converter", ("LICENSE-GITHUB", HashMap::new())),
        ("ntapi", ("LICENSE-MIT", HashMap::new())),
        ("num-traits", ("LICENSE-MIT", HashMap::new())),
        ("num_cpus", ("LICENSE-MIT", HashMap::new())),
        ("num_enum", ("LICENSE-MIT", HashMap::new())),
        ("num_enum_derive", ("LICENSE-MIT", HashMap::new())),
        ("num_threads", ("LICENSE-MIT", HashMap::new())),
        ("objc", ("LICENSE.txt", HashMap::new())),
        ("once_cell", ("LICENSE-MIT", HashMap::new())),
        ("ordered-float", ("LICENSE-MIT", HashMap::new())),
        ("parking_lot", ("LICENSE-MIT", HashMap::new())),
        ("parking_lot", ("LICENSE-MIT", HashMap::new())),
        ("parking_lot_core", ("LICENSE-MIT", HashMap::new())),
        ("parking_lot_core", ("LICENSE-MIT", HashMap::new())),
        ("percent-encoding", ("LICENSE-MIT", HashMap::new())),
        ("pin-project-lite", ("LICENSE-MIT", HashMap::new())),
        ("pin-utils", ("LICENSE-MIT", HashMap::new())),
        ("pkg-config", ("LICENSE-MIT", HashMap::new())),
        ("ppv-lite86", ("LICENSE-MIT", HashMap::new())),
        ("proc-macro-crate", ("LICENSE-MIT", HashMap::new())),
        ("proc-macro2", ("LICENSE-MIT", HashMap::new())),
        ("quote", ("LICENSE-MIT", HashMap::new())),
        ("rand", ("LICENSE-MIT", HashMap::new())),
        ("rand_chacha", ("LICENSE-MIT", HashMap::new())),
        ("rand_core", ("LICENSE-MIT", HashMap::new())),
        ("raw-window-handle", ("LICENSE-MIT.md", HashMap::from([("0.3.4", "LICENSE")]))),
        ("rayon", ("LICENSE-MIT", HashMap::new())),
        ("rayon-core", ("LICENSE-MIT", HashMap::new())),
        ("regex", ("LICENSE-MIT", HashMap::new())),
        ("regex-syntax", ("LICENSE-MIT", HashMap::new())),
        ("riscv", ("LICENSE-README.md", HashMap::new())),
        ("riscv-target", ("LICENSE-MIT", HashMap::new())),
        ("rstar", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("rustc-hash", ("LICENSE-MIT", HashMap::new())),
        ("rustc_version", ("LICENSE-MIT", HashMap::new())),
        ("rustc_version", ("LICENSE-MIT", HashMap::new())),
        ("rustversion", ("LICENSE-MIT", HashMap::new())),
        ("ryu", ("LICENSE-APACHE", HashMap::new())),
        ("scoped-tls", ("LICENSE-MIT", HashMap::new())),
        ("scopeguard", ("LICENSE-MIT", HashMap::new())),
        ("semver", ("LICENSE-MIT", HashMap::new())),
        ("semver", ("LICENSE-MIT", HashMap::new())),
        ("semver-parser", ("LICENSE-MIT", HashMap::new())),
        ("serde", ("LICENSE-MIT", HashMap::new())),
        ("serde_derive", ("LICENSE-MIT", HashMap::new())),
        ("serde_json", ("LICENSE-MIT", HashMap::new())),
        ("shared_library", ("LICENSE-MIT", HashMap::new())),
        ("smallvec", ("LICENSE-MIT", HashMap::new())),
        ("smithay-client-toolkit", ("LICENSE.txt", HashMap::new())),
        ("smithay-client-toolkit", ("LICENSE.txt", HashMap::new())),
        ("stable_deref_trait", ("LICENSE-MIT", HashMap::new())),
        ("static_assertions", ("LICENSE-MIT", HashMap::new())),
        ("str-buf", ("LICENSE-GITHUB", HashMap::new())),
        ("syn", ("LICENSE-MIT", HashMap::new())),
        ("thiserror", ("LICENSE-MIT", HashMap::new())),
        ("thiserror-impl", ("LICENSE-MIT", HashMap::new())),
        ("time", ("LICENSE-MIT", HashMap::new())),
        ("time-macros", ("LICENSE-MIT", HashMap::new())),
        ("tinyvec", ("LICENSE-MIT.md", HashMap::new())),
        ("toml", ("LICENSE-MIT", HashMap::new())),
        ("ttf-parser", ("LICENSE-MIT", HashMap::new())),
        ("twox-hash", ("LICENSE.txt", HashMap::new())),
        ("osmesa-sys", ("../CC0.txt", HashMap::new())),
        ("unicase", ("LICENSE-MIT", HashMap::new())),
        ("unicode-ident", ("LICENSE-MIT", HashMap::new())),
        ("unicode-normalization", ("LICENSE-MIT", HashMap::new())),
        ("unicode-segmentation", ("LICENSE-MIT", HashMap::new())),
        ("vcell", ("LICENSE-MIT", HashMap::new())),
        ("version_check", ("LICENSE-MIT", HashMap::new())),
        ("void", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("volatile-register", ("LICENSE-MIT", HashMap::new())),
        ("wasi", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen-backend", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen-futures", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen-macro", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen-macro-support", ("LICENSE-MIT", HashMap::new())),
        ("wasm-bindgen-shared", ("LICENSE-MIT", HashMap::new())),
        ("wayland-client", ("LICENSE.txt", HashMap::new())),
        ("wayland-commons", ("LICENSE.txt", HashMap::new())),
        ("wayland-cursor", ("LICENSE.txt", HashMap::new())),
        ("wayland-egl", ("LICENSE.txt", HashMap::new())),
        ("wayland-protocols", ("LICENSE.txt", HashMap::new())),
        ("wayland-scanner", ("LICENSE.txt", HashMap::new())),
        ("wayland-sys", ("LICENSE.txt", HashMap::new())),
        ("web-sys", ("LICENSE-MIT", HashMap::new())),
        ("winapi", ("LICENSE-MIT", HashMap::new())),
        ("winapi-i686-pc-windows-gnu", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("winapi-wsapoll", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("winapi-x86_64-pc-windows-gnu", ("LICENSE-MIT-GITHUB", HashMap::new())),
        ("windows-sys", ("LICENSE-MIT", HashMap::new())),
        ("windows_aarch64_msvc", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("windows_i686_aarch64", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("windows_i686_gnu", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("windows_i686_msvc", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("windows_x86_64_gnu", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("windows_x86_64_msvc", ("../windows-sys-0.36.1/LICENSE-MIT", HashMap::new())),
        ("x11-dl", ("LICENSE-MIT", HashMap::new())),
        ("x11rb", ("LICENSE-MIT", HashMap::new())),
        ("xi-unicode", ("LICENSE-GITHUB", HashMap::new())),
        ("adler", ("LICENSE-MIT", HashMap::new())),
        ("bytemuck_derive", ("LICENSE-MIT", HashMap::new())),
        ("chrono", ("LICENSE.txt", HashMap::new())),
        ("cmake", ("LICENSE-MIT", HashMap::new())),
        ("crc32fast", ("LICENSE-MIT", HashMap::new())),
        ("find-crate", ("LICENSE-MIT", HashMap::new())),
        ("flate2", ("LICENSE-MIT", HashMap::new())),
        ("foreign-types-macros", ("LICENSE-MIT", HashMap::new())),
        ("hashbrown", ("LICENSE-MIT", HashMap::new())),
        ("iana-time-zone", ("LICENSE-MIT", HashMap::new())),
        ("indexmap", ("LICENSE-MIT", HashMap::new())),
        ("io-lifetimes", ("LICENSE-MIT", HashMap::new())),
        ("is-terminal", ("LICENSE-MIT", HashMap::new())),
        ("linux-raw-sys", ("LICENSE-MIT", HashMap::new())),
        ("num-integer", ("LICENSE-MIT", HashMap::new())),
        ("nu-ansi-term", ("LICENCE", HashMap::new())),
        ("palette", ("LICENSE-MIT", HashMap::new())),
        ("palette_derive", ("LICENSE-MIT", HashMap::new())),
        ("phf", ("LICENSE-GITHUB", HashMap::from([("0.11.2", "LICENSE")]))),
        ("phf_generator", ("LICENSE-GITHUB", HashMap::from([("0.11.2", "LICENSE")]))),
        ("phf_macros", ("LICENSE-GITHUB", HashMap::new())),
        ("phf_shared", ("LICENSE-GITHUB", HashMap::from([("0.11.2", "LICENSE")]))),
        ("png", ("LICENSE-MIT", HashMap::new())),
        ("rustix", ("LICENSE-MIT", HashMap::new())),
        ("safe_arch", ("LICENSE-MIT-GITHUB..md", HashMap::new())),
        ("serde_spanned", ("LICENSE-MIT", HashMap::new())),
        ("servo-fontconfig", ("LICENSE-MIT", HashMap::new())),
        ("servo-fontconfig-sys", ("COPYING", HashMap::new())),
        ("simd-adler32", ("LICENSE.md", HashMap::new())),
        ("siphasher", ("COPYING", HashMap::new())),
        ("tinyvec_macros", ("LICENSE-MIT.md", HashMap::new())),
        ("toml_datetime", ("LICENSE-MIT", HashMap::new())),
        ("toml_edit", ("LICENSE-MIT", HashMap::new())),
        ("vec_map", ("LICENSE-MIT", HashMap::new())),
        ("windows-targets", ("LICENSE-MIT", HashMap::new())),
        ("winnow", ("LICENSE-MIT", HashMap::new())),
        ("equivalent", ("LICENSE-MIT", HashMap::new())),
        ("fdeflate", ("LICENSE-MIT-GITHUB", HashMap::new())),
    ]
    .into_iter()
    .map(|(name, (pfad, version_spezifisch))| (UniCaseOrd::neu(name), (pfad, version_spezifisch)))
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
    let standard_lizenz_pfad = ("LICENSE", HashMap::new());

    let mut unterschiede = BTreeMap::new();
    let is_diff = |diff: &Difference| {
        if let Difference::Same(_) = diff {
            false
        } else {
            true
        }
    };
    for ((name, version), f) in lizenzen {
        let (pfad, version_spezifisch) =
            lizenz_dateien.get(&UniCaseOrd::neu(name)).unwrap_or(&standard_lizenz_pfad);
        let datei = version_spezifisch.get(version).unwrap_or(pfad);
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
