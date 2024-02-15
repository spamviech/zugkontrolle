//! Steuerung einer Model-Eisenbahn über einen Raspberry Pi.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]
// Erlaube mehr rekursive Aufrufe von Macros.
#![recursion_limit = "256"]

pub mod application;
pub mod gleise;
