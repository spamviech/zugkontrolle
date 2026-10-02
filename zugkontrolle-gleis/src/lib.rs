//! Anzeige von Gleisen.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![expect(
    clippy::multiple_crate_versions,
    reason = "Zu viele/große dependencies, um das wirklich zu vermeiden."
)]

pub mod gerade;
pub mod id;
pub mod kreuzung;
pub mod kurve;
pub mod steuerung;
pub mod weiche;
pub mod zugtyp;
