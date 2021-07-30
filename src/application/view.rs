//! Methoden für die view-Methode des iced::Application-Traits

use std::fmt::Debug;

use crate::{
    anschluss::speichern::ToSave,
    application::{geschwindigkeit::LeiterAnzeige, Zugkontrolle},
    zugtyp::Zugtyp,
};

impl<Z> Zugkontrolle<Z>
where
    Z: Zugtyp,
    Z::Leiter: LeiterAnzeige,
    <<Z as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
}
