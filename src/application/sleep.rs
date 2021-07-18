//! Sleep-artige Subscription

use std::{
    hash::{Hash, Hasher},
    pin::Pin,
    task::{Context, Poll},
    thread::sleep,
    time::{Duration, Instant},
};

use iced_futures::{futures::stream::Stream, subscription::Recipe, BoxStream};

/// Sende einmalig eine Nachricht nach der angegebenen Zeit
pub struct Sleep<Nachricht> {
    start: Instant,
    zeit: Duration,
    nachricht: Option<Nachricht>,
}

impl<Nachricht> Sleep<Nachricht> {
    pub fn neu(start: Instant, zeit: Duration, nachricht: Nachricht) -> Self {
        Sleep { start, zeit, nachricht: Some(nachricht) }
    }
}

impl<H, Event, Nachricht> Recipe<H, Event> for Sleep<Nachricht>
where
    H: Hasher,
    Nachricht: 'static + Unpin + Send,
{
    type Output = Nachricht;

    fn hash(&self, state: &mut H) {
        self.start.hash(state);
        self.zeit.hash(state);
    }

    fn stream(self: Box<Self>, _input: BoxStream<Event>) -> BoxStream<Self::Output> {
        Box::pin(self)
    }
}

impl<Nachricht: Unpin> Stream for Sleep<Nachricht> {
    type Item = Nachricht;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let unpinned = Pin::into_inner(self);
        sleep(unpinned.zeit);
        Poll::Ready(unpinned.nachricht.take())
    }
}
