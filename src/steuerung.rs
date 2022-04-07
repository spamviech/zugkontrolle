//! Steuerung einer Modelleisenbahn.

pub mod streckenabschnitt;
pub use streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert};

pub mod weiche;
pub use weiche::{Weiche, WeicheSerialisiert};

pub mod geschwindigkeit;
pub use geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert};

pub mod kontakt;
pub use kontakt::{Kontakt, KontaktSerialisiert};

pub mod plan;
pub use plan::{
    Aktion, AktionGeschwindigkeit, AktionGeschwindigkeitSerialisiert, AktionSchalten,
    AktionSchaltenSerialisiert, AktionSerialisiert, AktionStreckenabschnitt, AktionWarten, Plan,
};
