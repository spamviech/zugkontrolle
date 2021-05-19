//! Singleton für Zugriffsrechte auf Anschlüsse.

use std::sync::PoisonError;
use std::sync::{
    mpsc::{channel, Receiver, Sender},
    Arc,
    Mutex,
};
use std::thread;

use cfg_if::cfg_if;
use log::{debug, error};
use num_x::u3;
use once_cell::sync::Lazy;
use paste::paste;

use super::level::Level;
use super::pcf8574::{self, Pcf8574, Port};
use super::pin::Pin;

/// originally taken from: https://www.ecorax.net/macro-bunker-1/
/// adjusted to 4 arguments
macro_rules! matrix {
    ( $inner_macro:ident [$($k:ident),+] $ls:tt $ms:tt $ns:tt $(: $value:ident)?) => {
        matrix! { $inner_macro $($k $ls $ms $ns)+ $(: $value)?}
    };
    ( $inner_macro:ident $($k:ident [$($l:tt),+] $ms:tt $ns:tt)+ $(: $value:ident)?) => {
        matrix! { $inner_macro $($( $k $l $ms $ns )+)+ $(: $value)? }
    };
    ( $inner_macro:ident $($k:ident $l:ident [$($m:tt),+] $ns:tt)+ $(: $value:ident)?) => {
        matrix! { $inner_macro $($( $k $l $m $ns )+)+ $(: $value)? }
    };
    ( $inner_macro:ident $($k:ident $l:ident $m:ident [$($n:tt),+])+ $(: $value:ident)?) => {
         $inner_macro! { $($($k $l $m $n),+),+ $(: $value)? }
    };
}
macro_rules! anschlüsse_data {
    {$($k:ident $l:ident $m:ident $n:ident),*} => {
        paste! {
            #[doc="Singleton für Zugriff auf raspberry pi Anschlüsse."]
            #[derive(Debug)]
            struct AnschlüsseData {
                #[cfg(raspi)]
                gpio: rppal::gpio::Gpio,
                #[cfg(raspi)]
                i2c: Arc<Mutex<rppal::gpio::I2c>>,
                $(
                    [<$k $l $m $n>]: Arc<Mutex<Pcf8574>>,
                    [<$k $l $m $n 0>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 1>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 2>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 3>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 4>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 5>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 6>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                    [<$k $l $m $n 7>]: Option<Port<Pcf8574, pcf8574::Nachricht>>,
                )*
            }
        }
    };
}
macro_rules! level {
    (l) => {
        Level::Low
    };
    (h) => {
        Level::High
    };
}
macro_rules! variante {
    (n) => {
        pcf8574::Variante::Normal
    };
    (a) => {
        pcf8574::Variante::A
    };
}
macro_rules! llln_to_hhha {
    ($inner_macro:ident $(: $value:ident)?) => {
        matrix! {$inner_macro  [l,h] [l,h] [l,h] [n,a] $(: $value)?}
    };
}

llln_to_hhha! { anschlüsse_data }
impl AnschlüsseData {
    /// Gebe den Pcf8574 an Anschlüsse zurück, so dass er von anderen verwendet werden kann.
    ///
    /// Wird vom Drop-handler ausgeführt, hier ist es explizit.
    fn rückgabe(&mut self, port: Port<Pcf8574, pcf8574::Nachricht>) -> Result<(), SyncError> {
        macro_rules! match_pcf8574 {
            {$($k:ident $l:ident $m:ident $n:ident),*} => {
                paste! {
                    match port.adresse() {
                        $(
                            (level!($k), level!($l), level!($m), variante!($n)) => {
                                debug!("rückgabe {:?}{:?}{:?}{:?}{:?}", level!($k), level!($l), level!($m), variante!($n), port.port());
                                match u8::from(port.port()) {
                                    0 => self.[<$k $l $m $n 0>] = Some(port),
                                    1 => self.[<$k $l $m $n 1>] = Some(port),
                                    2 => self.[<$k $l $m $n 2>] = Some(port),
                                    3 => self.[<$k $l $m $n 3>] = Some(port),
                                    4 => self.[<$k $l $m $n 4>] = Some(port),
                                    5 => self.[<$k $l $m $n 5>] = Some(port),
                                    6 => self.[<$k $l $m $n 6>] = Some(port),
                                    7 => self.[<$k $l $m $n 7>] = Some(port),
                                    _ => error!("Port > 7!"),
                                }
                            }
                            //TODO port über macro-expansion
                        ),*
                    }
                    Ok(())
                }
            };
        }
        llln_to_hhha! {match_pcf8574}
    }

    /// Reserviere den spezifizierten Pcf8574 zur exklusiven Nutzung.
    fn reserviere_pcf8574port(
        &mut self,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u3,
    ) -> Option<Port<Pcf8574, pcf8574::Nachricht>> {
        // gebe aktuellen Wert zurück und speichere stattdessen None
        macro_rules! reserviere_pcf8574 {
            {$($k:ident $l:ident $m:ident $n:ident),*} => {
                paste! {
                    match (a0, a1,a2, variante) {
                        $(
                            (level!($k),level!($l),level!($m),variante!($n)) => {
                                debug!("reserviere {:?}{:?}{:?}{:?}{}", level!($k), level!($l), level!($m), variante!($n), 0);
                                match u8::from(port) {
                                    0 => std::mem::replace(&mut self.[<$k $l $m $n 0>], None),
                                    1 => std::mem::replace(&mut self.[<$k $l $m $n 1>], None),
                                    2 => std::mem::replace(&mut self.[<$k $l $m $n 2>], None),
                                    3 => std::mem::replace(&mut self.[<$k $l $m $n 3>], None),
                                    4 => std::mem::replace(&mut self.[<$k $l $m $n 4>], None),
                                    5 => std::mem::replace(&mut self.[<$k $l $m $n 5>], None),
                                    6 => std::mem::replace(&mut self.[<$k $l $m $n 6>], None),
                                    7 => std::mem::replace(&mut self.[<$k $l $m $n 7>], None),
                                    _ => None,
                                }
                            }
                        ),*
                    }
                }
            };
        }
        llln_to_hhha! {reserviere_pcf8574}
    }
}

/// Singleton für Zugriff auf raspberry pi Anschlüsse.
#[derive(Debug)]
pub struct Anschlüsse(Option<AnschlüsseInternal>);
impl Drop for Anschlüsse {
    fn drop(&mut self) {
        let Anschlüsse(option_data) = self;
        if let Ok(mut guard) = ANSCHLÜSSE.lock() {
            let static_anschlüsse = &mut *guard;
            if let Err(Error::Sync(SyncError::InVerwendung)) = static_anschlüsse {
                if let Some(anschlüsse) = std::mem::replace(option_data, None) {
                    *static_anschlüsse = Ok(anschlüsse);
                } else {
                    error!("None-Wert in Anschlüsse während drop!");
                }
            }
        }
    }
}
impl Anschlüsse {
    pub fn neu() -> Result<Anschlüsse, Error> {
        match ANSCHLÜSSE.lock() {
            Ok(mut guard) => {
                let anschlüsse = &mut *guard;
                let data =
                    std::mem::replace(anschlüsse, Err(Error::Sync(SyncError::InVerwendung)))?;
                Ok(Anschlüsse(Some(data)))
            },
            Err(_er) => Err(Error::Sync(SyncError::PoisonError)),
        }
    }

    fn listen_restore_messages(
        sender: Sender<(pcf8574::Nachricht, u3)>,
        receiver: Receiver<(pcf8574::Nachricht, u3)>,
        inner: AnschlüsseInternal,
        #[cfg(raspi)] i2c: Arc<Mutex<i2c::I2C>>,
    ) {
        loop {
            match receiver.recv() {
                Ok((nachricht, port)) => match inner.lock() {
                    Ok(mut guard) => {
                        let anschlüsse = &mut *guard;
                        macro_rules! port_value {
                            ($a0:ident $a1:ident $a2:ident $var:ident $port:expr) => {
                                paste! {
                                    Port::neu(
                                        anschlüsse.[<$a0 $a1 $a2 $var>].clone(),
                                        $port,
                                        (level!{$a0}, level!{$a1}, level!{$a2}, variante!{$var}),
                                        sender.clone()
                                    )
                                }
                            };
                        }
                        macro_rules! match_nachricht {
                            {$($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
                                paste! {
                                    match nachricht {
                                        $(
                                            (level!($k),level!($l),level!($m),variante!($n)) => {
                                                $value! {$k $l $m $n port}
                                            }
                                        )*
                                    }
                                }
                            }
                        }
                        let port = llln_to_hhha! {match_nachricht: port_value};
                        if let Err(err) = anschlüsse.rückgabe(port) {
                            error!("Error bei rückgabe: {:?}", err);
                            break
                        }
                    },
                    Err(err) => {
                        error!("Anschlüsse-static poisoned: {}", err);
                        break
                    },
                },
                Err(err) => {
                    error!("Kanal für Pcf8574-Rückgabe geschlossen: {}", err);
                    break
                },
            }
        }
    }

    fn erstelle_static() -> AnschlüsseStatic {
        macro_rules! make_anschlüsse {
            {$($a0:ident $a1:ident $a2:ident $var:ident),*} => {{
                #[cfg(raspi)]
                let i2c = Arc::new(Mutex::new(rppal::gpio::I2c::new()?));
                paste! {
                    Ok(AnschlüsseData {
                        #[cfg(raspi)]
                        gpio: rppal::gpio::Gpio::new()?,
                        #[cfg(raspi)]
                        i2c: i2c.clone(),
                        $(
                            [<$a0 $a1 $a2 $var>]: Arc::new(Mutex::new(Pcf8574::neu(
                                level!($a0),
                                level!($a1),
                                level!($a2),
                                variante!($var),
                                #[cfg(raspi)]
                                i2c.clone(),
                            ))),
                            [<$a0 $a1 $a2 $var 0>]: None,
                            [<$a0 $a1 $a2 $var 1>]: None,
                            [<$a0 $a1 $a2 $var 2>]: None,
                            [<$a0 $a1 $a2 $var 3>]: None,
                            [<$a0 $a1 $a2 $var 4>]: None,
                            [<$a0 $a1 $a2 $var 5>]: None,
                            [<$a0 $a1 $a2 $var 6>]: None,
                            [<$a0 $a1 $a2 $var 7>]: None,
                        )*
                    })
                }
            }};
        }
        let inner = (llln_to_hhha! {make_anschlüsse}).map(|anschlüsse| {
            #[cfg(raspi)]
            let i2c_clone = anschlüsse.i2c.clone();

            let inner = Arc::new(Mutex::new(anschlüsse));
            let inner_clone = inner.clone();

            let (sender, receiver) = channel();
            let sender_clone = sender.clone();

            // erzeuge Thread der Rückgaben handelt
            thread::spawn(move || {
                Anschlüsse::listen_restore_messages(sender_clone, receiver, inner_clone)
            });

            // Eigener Block um borrow-lifetime von inner zu beschränken
            {
                let anschlüsse =
                    &mut *inner.lock().expect("Anschlüsse poisoned vor Initialisierung");
                macro_rules! port_value {
                    ($a0:ident $a1:ident $a2:ident $var:ident $port:expr) => {
                        paste! {
                            Some(
                                Port::neu(
                                    anschlüsse.[<$a0 $a1 $a2 $var>].clone(),
                                    u3::new($port),
                                    (level!{$a0}, level!{$a1}, level!{$a2}, variante!{$var}),
                                    sender.clone()
                                )
                            )
                        }
                    };
                }
                macro_rules! init_anschlüsse {
                    {$($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
                        paste! {
                            $(
                                anschlüsse.[<$k $l $m $n 0>] = $value!($k $l $m $n 0);
                                anschlüsse.[<$k $l $m $n 1>] = $value!($k $l $m $n 1);
                                anschlüsse.[<$k $l $m $n 2>] = $value!($k $l $m $n 2);
                                anschlüsse.[<$k $l $m $n 3>] = $value!($k $l $m $n 3);
                                anschlüsse.[<$k $l $m $n 4>] = $value!($k $l $m $n 4);
                                anschlüsse.[<$k $l $m $n 5>] = $value!($k $l $m $n 5);
                                anschlüsse.[<$k $l $m $n 6>] = $value!($k $l $m $n 6);
                                anschlüsse.[<$k $l $m $n 7>] = $value!($k $l $m $n 7);
                            )*
                        }
                    };
                }
                llln_to_hhha! {init_anschlüsse: port_value}
            }

            inner
        });

        Arc::new(Mutex::new(inner))
    }

    /// Gebe den Pcf8574 an Anschlüsse zurück, so dass er von anderen verwendet werden kann.
    ///
    /// Der Drop-Handler von Pcf8574 (und dem letzten Pcf8574Port) hat die selbe Auswirkung.
    /// Diese Methode ist explizit (keine Wartezeit, kann dafür blockieren).
    pub fn rückgabe(&mut self, port: Port<Pcf8574, pcf8574::Nachricht>) -> Result<(), SyncError> {
        if let Some(arc) = &self.0 {
            arc.lock()?.rückgabe(port)
        } else {
            Err(SyncError::InVerwendung)
        }
    }

    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Pin, Error> {
        if let 2 | 3 = pin {
            // Gpio 2,3 nicht verfügbar (durch I2C belegt)
            return Err(Error::Sync(SyncError::InVerwendung))
        }
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(Pin(self.gpio.get(pin)?))
            } else {
                Err(Error::Sync(SyncError::InVerwendung))
            }
        }
    }

    // TODO Direkt Port reservieren?
    /// Reserviere den spezifizierten Pcf8574 zur exklusiven Nutzung.
    pub fn reserviere_pcf8574port(
        &mut self,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u3,
    ) -> Result<Port<Pcf8574, pcf8574::Nachricht>, SyncError> {
        self.0.as_mut().ok_or(SyncError::WertDropped).and_then(|arc| {
            arc.lock().map_err(Into::into).and_then(|mut guard| {
                guard
                    .reserviere_pcf8574port(a0, a1, a2, variante, port)
                    .ok_or(SyncError::InVerwendung)
            })
        })
    }
}

type AnschlüsseInternal = Arc<Mutex<AnschlüsseData>>;
type AnschlüsseResult = Result<AnschlüsseInternal, Error>;
type AnschlüsseStatic = Arc<Mutex<AnschlüsseResult>>;
static ANSCHLÜSSE: Lazy<AnschlüsseStatic> = Lazy::new(Anschlüsse::erstelle_static);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
    Sync(SyncError),
}
impl From<SyncError> for Error {
    fn from(error: SyncError) -> Self {
        Error::Sync(error)
    }
}
cfg_if! {
    if #[cfg(raspi)] {
        impl From<rppal::gpio::Error> for Error {
            fn from(error: rppal::gpio::Error) -> Self {
                Error::Gpio(error)
            }
        }
        impl From<rppal::i2c::Error> for Error {
            fn from(error: rppal::i2c::Error) -> Self {
                Error::I2c(error)
            }
        }
        impl From<rppal::pwm::Error> for Error {
            fn from(error: rppal::pwm::Error) -> Self {
                Error::Pwm(error)
            }
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncError {
    PoisonError,
    InVerwendung,
    WertDropped,
}
impl<T> From<PoisonError<T>> for SyncError {
    fn from(_: PoisonError<T>) -> Self {
        SyncError::PoisonError
    }
}
