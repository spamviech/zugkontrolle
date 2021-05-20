//! Typen zur Steuerung einer Modelleisenbahn.

pub struct Streckenabschnitt;

// inklusive Kreuzung
pub struct Weiche;

pub struct Kupplung;

// Geschwindigkeit: Pwm / Mehrere Anschlüsse mit konstanter Spannung
// Umdrehen:
//      Märklin: Pwm->Kein Extra-Anschluss, KS->Extra-Anschluss (Überspannung)
//      Lego: Immer Extra-Anschluss (High-Low an Schienen wechseln)
pub struct Geschwindigkeit;

pub struct Kontakt;

pub struct Wegstrecke;
