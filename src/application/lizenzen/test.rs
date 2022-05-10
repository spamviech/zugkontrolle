//! Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.

use std::{
    collections::{BTreeMap, BTreeSet},
    str::Split,
};

use difference::{Changeset, Difference};
use either::Either;

use crate::application::lizenzen::verwendete_lizenzen;

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
                string.push_str("\x1b[92m");
                string.push_str(x);
                string.push_str("\x1b[0m");
                string.push_str(split);
            },
            Difference::Rem(x) => {
                push_letzte_same(&mut string, &mut letzte_same, split);
                letzte_diff = true;
                string.push_str("\x1b[91m");
                string.push_str(x);
                string.push_str("\x1b[0m");
                string.push_str(split);
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

#[test]
/// Test ob die angezeigten Lizenzen mit den wirklichen Lizenzen übereinstimmen.
fn passende_lizenzen() -> Result<(), BTreeSet<&'static str>> {
    let lizenzen = verwendete_lizenzen();
    // Lizenz-Dateien, die nicht "LICENSE" heißen.
    let lizenz_dateien = BTreeMap::from([
        ("SourceSerif4-Regular", "../../fonts/source-serif/LICENSE.md"),
        ("Lato", "../iced_graphics-0.3.0/fonts/OFL.txt"),
        ("aho-corasick-0.7.18", "LICENSE-MIT"),
        ("riscv-0.7.0", "LICENSE-README"),
        ("riscv-target-0.1.2", "LICENSE-MIT"),
        ("windows-sys-0.36.1", "LICENSE-MIT"),
        ("windows_i686_aarch64-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_gnu-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_i686_msvc-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_gnu-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("windows_x86_64_msvc-0.36.1", "../windows-sys-0.36.1/LICENSE-MIT"),
        ("x11-dl-2.19.1", "LICENSE-MIT"),
        ("x11rb-0.8.1", "LICENSE-MIT"),
        ("xi-unicode-0.3.0", "LICENSE-GITHUB"),
    ]);

    let mut unterschiede = BTreeMap::new();
    for (name, f) in lizenzen {
        let datei = lizenz_dateien.get(name).unwrap_or(&"LICENSE");
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}/{datei}");
        match std::fs::read_to_string(lizenz_pfad.clone()) {
            Ok(gespeicherte_lizenz) => {
                let changeset = Changeset::new(&gespeicherte_lizenz, &verwendete_lizenz, "\n");
                let is_diff = |diff: &Difference| {
                    if let Difference::Same(_) = diff {
                        false
                    } else {
                        true
                    }
                };
                if changeset.diffs.iter().any(is_diff) {
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
                    eprintln!("{}", changeset_als_string(changeset))
                },
                Either::Right((lizenz_pfad, lese_fehler)) => {
                    eprintln!("Fehler beim lesen der gespeicherten Lizenz \"{lizenz_pfad}\":\n{lese_fehler}")
                },
            }
        }
        if not_first {
            eprintln!("---------------------------------");
        }
        Err(unterschiede.into_keys().collect())
    }
}
