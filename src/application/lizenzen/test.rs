//! Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{self, Display, Formatter, Write},
    path::Path,
    str::Split,
};

use difference::{Changeset, Difference};
use either::Either;
use enum_iterator::all;
use itertools::Itertools;
use log::error;
use nonempty::NonEmpty;

use crate::{
    application::lizenzen::{
        cargo_lock_lizenzen,
        texte::{mit_ohne_copyright, MITZeilenumbruch},
        verwendete_lizenzen_impl,
    },
    init_test_logging,
    util::unicase_ord::UniCaseOrd,
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
    ($($target: literal),* $(,)?) => {
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

release_targets! {
    "armv7-unknown-linux-gnueabihf",
    "aarch64-unknown-linux-gnu",
    "x86_64-pc-windows-gnu",
    "x86_64-unknown-linux-gnu",
}

fn cargo_lock_crates() -> HashSet<&'static str> {
    cargo_lock_lizenzen().into_iter().map(|(name, _lizenz)| name).collect()
}

#[test]
/// Test ob alle Lizenzen angezeigt werden.
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

/// Lizenz-Dateien, die nicht "LICENSE" heißen.
fn lizenz_dateien(
) -> BTreeMap<UniCaseOrd<&'static str>, (&'static str, HashMap<&'static str, &'static str>)> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dev-dependency
    // cargo-lock = "8.0.1"
    // Nachteil: fetch dauert eine Weile

    [
        ("SourceSerif4-Regular", ("../../fonts/source-serif/LICENSE.md", HashMap::new())),
        ("Bootstrap Icons", ("../../fonts/bootstrap-icons/LICENSE", HashMap::new())),
        ("bytemuck_derive", ("../bytemuck-1.9.1/LICENSE-MIT", HashMap::new())),
        ("windows_aarch64_msvc", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("windows_i686_aarch64", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("windows_i686_gnu", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("windows_i686_msvc", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("windows_x86_64_gnu", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("windows_x86_64_msvc", ("../windows-0.44.0/LICENSE-MIT", HashMap::new())),
        ("widestring", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-core", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-hal", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-types", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wayland-backend", ("LICENSE-GITHUB..txt", HashMap::new())),
        ("range-alloc", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
    ]
    .into_iter()
    .map(|(name, (pfad, version_spezifisch))| (UniCaseOrd::neu(name), (pfad, version_spezifisch)))
    .collect()
}

#[test]
/// Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.
/// Nimmt vorheriges ausführen von `python3 fetch_licenses.py` an.
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
    let standard_lizenz_pfade = ["LICENSE", "LICENSE-MIT", "LICENSE-APACHE", "COPYING"]
        .into_iter()
        .flat_map(|pfad| {
            [
                String::from(pfad),
                format!("{pfad}.txt"),
                format!("{pfad}.md"),
                format!("{pfad}-GITHUB"),
            ]
        })
        .collect_vec();
    let fallback_standard_pfad = String::from("UnbekannterStandardLizenzPfad");

    let mut unterschiede = BTreeMap::new();
    let is_diff = |diff: &Difference| {
        if let Difference::Same(_) = diff {
            false
        } else {
            true
        }
    };
    for ((name, version), f) in lizenzen {
        let ordner_pfad = format!("licenses/{name}-{version}");
        let standard_lizenz_pfad: &str = standard_lizenz_pfade
            .iter()
            .filter(|pfad| Path::new(&format!("{ordner_pfad}/{pfad}")).is_file())
            .next()
            .unwrap_or(&fallback_standard_pfad);
        let standard_lizenz_pfad_mit_map = (standard_lizenz_pfad, HashMap::new());
        let (pfad, version_spezifisch) =
            lizenz_dateien.get(&UniCaseOrd::neu(name)).unwrap_or(&standard_lizenz_pfad_mit_map);
        let datei = version_spezifisch.get(version).unwrap_or(pfad);
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}-{version}/{datei}");
        match std::fs::read_to_string(lizenz_pfad.clone()) {
            Ok(gespeicherte_lizenz) => {
                let gespeicherte_lizenz_unix = gespeicherte_lizenz.replace("\r\n", "\n");
                let changeset = Changeset::new(&gespeicherte_lizenz_unix, &verwendete_lizenz, "\n");
                if changeset.diffs.iter().any(is_diff) {
                    let mit_changesets: Vec<_> = all::<MITZeilenumbruch>()
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
                        // Zeige nur MIT-Changesets mit mindestens einer Übereinstimmung.
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
                        fehlermeldung.push_str(&format!("{name}-{version}"));
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
