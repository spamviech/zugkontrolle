//! Erstelle einen Trait für das senden einer beliebigen Nachricht, sowie den dazugehörigen (klonbaren) Existential-Typ.

/// Erstelle einen Trait für das senden einer beliebigen Nachricht, sowie den dazugehörigen (klonbaren) Existential-Typ.
macro_rules! erstelle_sender_trait_existential {
    ($(($vis: vis),)? $trait: ident, $trait_doc: literal, $existential: ident, $existential_doc: literal, $msg: ty $(,)?) => {
        #[doc = $trait_doc]
        #[dyn_clonable::clonable]
        #[doc = "Sende eine [$msg]-Nachricht."]
        #[allow(unused_qualifications)]
        $($vis)? trait $trait: Clone + Send {
            #[doc = "Sende eine [$msg]-Nachricht.\n\n## Errors\n\nFehler beim Senden der Nachricht."]
            fn send(&self, msg: $msg) -> Result<(), std::sync::mpsc::SendError<$msg>>;

            #[doc = "[Debug]-Ausgabe zur Darstellung eines [$existential].\n\n## Errors\n\nFehler beim erzeugen der Debug-Darstellung."]
            fn debug_fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
        }

        #[allow(unused_qualifications)]
        impl $trait for std::sync::mpsc::Sender<$msg> {
            fn send(&self, msg: $msg) -> Result<(), std::sync::mpsc::SendError<$msg>> {
                std::sync::mpsc::Sender::send(self, msg)
            }

            fn debug_fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                <Self as std::fmt::Debug>::fmt(self, formatter)
            }
        }

        #[allow(unused_qualifications)]
        impl<T: Send, F: Fn($msg) -> T + Clone + Send> $trait for (std::sync::mpsc::Sender<T>, F) {
            fn send(&self, msg: $msg) -> Result<(), std::sync::mpsc::SendError<$msg>> {
                let (sender, funktion) = self;
                std::sync::mpsc::Sender::send(sender, funktion(msg))
                    .map_err(|std::sync::mpsc::SendError(_)| std::sync::mpsc::SendError(msg))
            }

            fn debug_fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.debug_tuple("").field(&self.0).field(&"<closure>").finish()
            }
        }

        #[doc = $existential_doc]
        #[derive(Clone)]
        $($vis)? struct $existential(Box<dyn $trait>);

        #[allow(unused_qualifications)]
        impl std::fmt::Debug for $existential {
            fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("SomeLevelSender(")?;
                self.0.debug_fmt(formatter)?;
                formatter.write_str(")")
            }
        }

        impl<T: 'static + $trait + Send> From<T> for $existential {
            fn from(value: T) -> Self {
                $existential(Box::new(value))
            }
        }

        #[allow(unused_qualifications)]
        impl std::ops::Deref for $existential {
            type Target = dyn $trait;

            fn deref(&self) -> &Self::Target {
                self.0.as_ref()
            }
        }

        #[allow(unused_qualifications)]
        impl std::ops::DerefMut for $existential {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.0.as_mut()
            }
        }
    };
}
pub(crate) use erstelle_sender_trait_existential;
