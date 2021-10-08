//! Auf channel-Nachricht wartende Subscription

use std::{
    hash::{Hash, Hasher},
    pin::Pin,
    sync::{
        mpsc::{Receiver, RecvError},
        Arc, Mutex,
    },
    task::{Context, Poll},
};

use iced_futures::{futures::stream::Stream, subscription::Recipe, BoxStream};
use log::{debug, error};

/// Warte auf eine Nachricht
#[derive(Debug, zugkontrolle_derive::Clone)]
pub struct Empfänger<Nachricht> {
    receiver: Arc<Mutex<Receiver<Nachricht>>>,
}

impl<Nachricht> Empfänger<Nachricht> {
    pub fn neu(receiver: Receiver<Nachricht>) -> Self {
        Empfänger { receiver: Arc::new(Mutex::new(receiver)) }
    }
}

impl<H, Event, Nachricht> Recipe<H, Event> for Empfänger<Nachricht>
where
    H: Hasher,
    Nachricht: Unpin + Send + 'static,
{
    type Output = Nachricht;

    fn hash(&self, state: &mut H) {
        // Add some string to differentiate from other possible subscriptions without hashable state.
        "Empfänger".hash(state);
    }

    fn stream(self: Box<Self>, _input: BoxStream<Event>) -> BoxStream<Self::Output> {
        Box::pin(self)
    }
}

impl<Nachricht: Unpin> Stream for Empfänger<Nachricht> {
    type Item = Nachricht;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let unpinned = Pin::into_inner(self);
        let receiver = match unpinned.receiver.lock() {
            Ok(receiver) => receiver,
            Err(poison_error) => {
                error!("Receiver-Mutex von Empfänger poisoned!");
                poison_error.into_inner()
            }
        };
        match receiver.recv() {
            Ok(nachricht) => Poll::Ready(Some(nachricht)),
            Err(RecvError) => {
                debug!("Channel for Message subscription disconnected!");
                Poll::Ready(None)
            }
        }
    }
}
