//! Auf channel-Nachricht wartende Subscription

use std::{
    hash::{Hash, Hasher},
    pin::Pin,
    sync::{
        mpsc::{Receiver, TryRecvError},
        Arc, Mutex, TryLockError,
    },
    task::{Context, Poll},
};

use iced_futures::{futures::stream::Stream, subscription::Recipe, BoxStream};
use log::error;

/// Warte auf eine Nachricht
#[derive(Debug, Clone)]
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
        // always return a new hash, copied from download_progress-example
        // https://github.com/hecrj/iced/blob/master/examples/download_progress/src/download.rs
        struct Marker;
        std::any::TypeId::of::<Marker>().hash(state);
    }

    fn stream(self: Box<Self>, _input: BoxStream<Event>) -> BoxStream<Self::Output> {
        Box::pin(self)
    }
}

impl<Nachricht: Unpin> Stream for Empfänger<Nachricht> {
    type Item = Nachricht;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let unpinned = Pin::into_inner(self);
        let receiver = match unpinned.receiver.try_lock() {
            Ok(receiver) => receiver,
            Err(TryLockError::WouldBlock) => return Poll::Pending,
            Err(TryLockError::Poisoned(poisoned)) => {
                error!("Mutex containing Channel for Message subscription poisoned!");
                poisoned.into_inner()
            }
        };
        match receiver.try_recv() {
            Ok(nachricht) => Poll::Ready(Some(nachricht)),
            Err(TryRecvError::Empty) => Poll::Pending,
            Err(TryRecvError::Disconnected) => {
                error!("Channel for Message subscription disconnected!");
                Poll::Ready(None)
            }
        }
    }
}
