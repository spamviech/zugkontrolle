//! Auf channel-Nachricht wartende [`Subscription`](iced::Subscription).

use std::{
    any::TypeId,
    hash::Hash,
    pin::Pin,
    sync::{
        mpsc::{Receiver, RecvError},
        Arc,
    },
    task::{Context, Poll},
};

use iced_core::{
    event::{Event, Status},
    Hasher,
};
use iced_futures::{futures::stream::Stream, subscription::Recipe, BoxStream};
use log::debug;
use parking_lot::Mutex;

/// Warte auf eine Nachricht, kann als Ergebnis von [`subscription`](iced::Application::subscription)
/// über [`from_recipe`](iced::Subscription::from_recipe) verwendet werden.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct Empfänger<Nachricht, Id = ()> {
    id: Id,
    receiver: Arc<Mutex<Receiver<Nachricht>>>,
}

impl<Nachricht, Id> Empfänger<Nachricht, Id> {
    /// Erstelle einen neuen [`Empfänger`].
    pub fn neu(receiver: Receiver<Nachricht>, id: Id) -> Self {
        Empfänger { id, receiver: Arc::new(Mutex::new(receiver)) }
    }
}

impl<Nachricht, Id> Recipe for Empfänger<Nachricht, Id>
where
    Nachricht: Unpin + Send + 'static,
    Id: Hash + Unpin + Send + 'static,
{
    type Output = Nachricht;

    fn hash(&self, state: &mut Hasher) {
        TypeId::of::<Id>().hash(state);
        TypeId::of::<Empfänger<Nachricht, Id>>().hash(state);
        self.id.hash(state);
    }

    fn stream(self: Box<Self>, _input: BoxStream<(Event, Status)>) -> BoxStream<Self::Output> {
        Box::pin(self)
    }
}

impl<Nachricht: Unpin, Id: Unpin> Stream for Empfänger<Nachricht, Id> {
    type Item = Nachricht;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let unpinned = Pin::into_inner(self);
        let receiver = unpinned.receiver.lock();
        match receiver.recv() {
            Ok(nachricht) => Poll::Ready(Some(nachricht)),
            Err(RecvError) => {
                debug!("Channel für Nachricht subscription getrennt!");
                Poll::Ready(None)
            },
        }
    }
}
