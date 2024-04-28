//! Pfadauswahl mit Speichern und Laden Knopf.

use std::{
    fmt::{self, Debug, Formatter},
    future::Future,
    path::Path,
    pin::Pin,
};

use iced_core::{text as text_core, widget::text, Alignment, Element, Font, Length};
use iced_widget::{
    button::{self, Button},
    text_input::{self},
    Row,
};
use rfd::{AsyncFileDialog, FileHandle};

use crate::{
    bootstrap::{Bootstrap, Icon},
    style,
};

/// Wrapper um eine [`Future`] um eine Dummy [`Debug`]-Implementierung anzugeben.
pub struct ZeigeDateiDialog(pub Pin<Box<dyn Future<Output = Nachricht> + Send>>);

impl Debug for ZeigeDateiDialog {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_tuple("ZeigeDateiDialog").field(&"<Future>").finish()
    }
}

/// Die interne Nachricht zum Identifizieren des gedrückten [`Button`] in einem [`SpeichernLaden`]-Widget.
#[derive(Debug, Clone)]
enum InterneNachricht {
    /// Speichern gewünscht.
    Speichern,
    /// Laden gewünscht.
    Laden,
}

/// Nachricht des [`SpeichernLaden`]-Widgets.
#[derive(Debug)]
pub enum Nachricht {
    /// Speichern im gegebenen Pfad gewünscht.
    Speichern(FileHandle),
    /// Laden aus dem gegebenen Pfad gewünscht.
    Laden(FileHandle),
    /// Der [`AsyncFileDialog`] wurde beendet, ohne einen Pfad zu wählen.
    Abgebrochen,
}

/// Widget mit Pfadauswahl und Knöpfen zum Speichern und Laden.
pub struct SpeichernLaden<'a, Thema, R>(Element<'a, ZeigeDateiDialog, Thema, R>);

impl<Thema: Debug, R: Debug> Debug for SpeichernLaden<'_, Thema, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_tuple("SpeichernLaden").field(&"<Element>").finish()
    }
}

// Message=Future<Nachricht>

impl<'a, Thema, R> SpeichernLaden<'a, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a + button::StyleSheet + text::StyleSheet + text_input::StyleSheet,
    <Thema as button::StyleSheet>::Style: From<style::Button>,
    <R as text_core::Renderer>::Font: From<Font>,
{
    /// Erstelle ein [`SpeichernLaden`]-Widget.
    #[must_use]
    pub fn neu(aktueller_pfad: &'a str, speichern_gefärbt: Option<bool>) -> Self {
        let speichern_ungefärbt =
            Button::new(Icon::neu(Bootstrap::Floppy)).on_press(InterneNachricht::Speichern);
        let speichern_style = match speichern_gefärbt {
            Some(true) => style::button::GRÜN,
            Some(false) => style::button::ROT,
            None => style::Button::Standard,
        };
        let laden = Button::new(Icon::neu(Bootstrap::FileEarmark))
            .style(style::Button::Standard)
            .on_press(InterneNachricht::Laden);
        let row = Row::new()
            .push(speichern_ungefärbt.style(speichern_style))
            .push(laden)
            .align_items(Alignment::End)
            .width(Length::Shrink);
        let pfad = Path::new(aktueller_pfad).canonicalize().unwrap_or_default();
        let mapped = Element::from(row).map(move |button| {
            let async_file_dialog = AsyncFileDialog::new()
                .set_directory(pfad.parent().and_then(Path::to_str).unwrap_or_default())
                .set_file_name(pfad.to_str().unwrap_or_default())
                .add_filter(".zug", &["zug"])
                .add_filter("*", &["*"]);
            let future = async move {
                let (file_handle, erzeuge_nachricht): (_, fn(FileHandle) -> Nachricht) =
                    match button {
                        InterneNachricht::Speichern => (
                            async_file_dialog.set_title("Speichern").save_file().await,
                            Nachricht::Speichern,
                        ),
                        InterneNachricht::Laden => (
                            async_file_dialog.set_title("Laden").pick_file().await,
                            Nachricht::Laden,
                        ),
                    };
                if let Some(file_handle) = file_handle {
                    erzeuge_nachricht(file_handle)
                } else {
                    Nachricht::Abgebrochen
                }
            };
            let boxed_future: Pin<Box<dyn Future<Output = Nachricht> + Send>> = Box::pin(future);
            ZeigeDateiDialog(boxed_future)
        });
        SpeichernLaden(mapped)
    }
}

impl<'a, Thema, R> From<SpeichernLaden<'a, Thema, R>> for Element<'a, ZeigeDateiDialog, Thema, R> {
    fn from(auswahl: SpeichernLaden<'a, Thema, R>) -> Self {
        auswahl.0
    }
}
