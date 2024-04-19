//! Spezialisierte Widgets für die Gleis-Anzeige.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]
// Erlaube mehr rekursive Aufrufe von Macros.
#![recursion_limit = "256"]

pub mod anschluss;
pub mod auswahl;
pub mod bewegen;
pub mod bootstrap;
pub mod drehen;
pub mod farbwahl;
pub mod flat_map;
pub mod fonts;
pub mod geschwindigkeit;
pub mod kontakt;
pub mod lizenzen;
pub mod map_mit_zustand;
pub mod map_operation;
pub mod modal;
pub mod speichern_laden;
pub mod streckenabschnitt;
pub mod style;
pub mod weiche;
