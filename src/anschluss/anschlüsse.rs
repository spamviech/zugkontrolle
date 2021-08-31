//! Singleton für Zugriffsrechte auf Anschlüsse.

#[cfg(not(raspi))]
use std::collections::HashSet;
use std::sync::{
    mpsc::{channel, Receiver, Sender},
    Arc, Mutex, PoisonError,
};
use std::thread;

use log::{debug, error};
use num_x::u3;
use once_cell::sync::Lazy;
use paste::paste;

use super::{
    level::Level,
    pcf8574::{self, Pcf8574, Port},
    pin::Pin,
};

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
                i2c: Arc<Mutex<rppal::i2c::I2c>>,
                #[cfg(not(raspi))]
                ausgegebene_pins: HashSet<u8>,
                #[cfg(not(raspi))]
                pin_rückgabe: Sender<u8>,
                $(
                    [<$k $l $m $n>]: Arc<Mutex<Pcf8574>>,
                    [<$k $l $m $n 0>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 1>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 2>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 3>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 4>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 5>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 6>]: Option<pcf8574::Port>,
                    [<$k $l $m $n 7>]: Option<pcf8574::Port>,
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
    fn rückgabe(&mut self, port: pcf8574::Port) -> Result<(), SyncFehler> {
        macro_rules! match_pcf8574 {
            {$($k:ident $l:ident $m:ident $n:ident),*} => {
                paste! {
                    match port.beschreibung() {
                        $(
                            pcf8574::Beschreibung {
                                a0: level!{$k},
                                a1: level!{$l},
                                a2: level!{$m},
                                variante: variante!{$n}
                            } => {
                                debug!("rückgabe pcf8574 {:?}-{:?}-{:?}-{:?}-{:?}"
                                    , level!($k), level!($l), level!($m), variante!($n), port.port());
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
    fn reserviere_pcf8574_port(
        &mut self,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u3,
    ) -> Option<pcf8574::Port> {
        // gebe aktuellen Wert zurück und speichere stattdessen None
        macro_rules! reserviere_pcf8574 {
            {$($k:ident $l:ident $m:ident $n:ident),*} => {
                paste! {
                    match (a0, a1,a2, variante) {
                        $(
                            (level!($k),level!($l),level!($m),variante!($n)) => {
                                debug!("reserviere pcf8574 {:?}-{:?}-{:?}-{:?}-{}"
                                        , level!($k), level!($l), level!($m), variante!($n), port);
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
            if let Err(Fehler::Sync(SyncFehler::SingletonInVerwendung)) = static_anschlüsse {
                if let Some(anschlüsse) = std::mem::replace(option_data, None) {
                    *static_anschlüsse = Ok(anschlüsse);
                } else {
                    error!("None-Wert in Anschlüsse während drop!",);
                }
            }
        }
    }
}
impl Anschlüsse {
    pub fn neu() -> Result<Anschlüsse, Fehler> {
        let mut guard = ANSCHLÜSSE.lock()?;
        let anschlüsse = &mut *guard;
        let data =
            std::mem::replace(anschlüsse, Err(Fehler::Sync(SyncFehler::SingletonInVerwendung)))?;
        Ok(Anschlüsse(Some(data)))
    }

    fn listen_restore_messages(
        sender: Sender<(pcf8574::Beschreibung, u3)>,
        receiver: Receiver<(pcf8574::Beschreibung, u3)>,
        inner: AnschlüsseInternal,
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
                                        pcf8574::Beschreibung {
                                            a0:level!{$a0},
                                            a1:level!{$a1},
                                            a2:level!{$a2},
                                            variante:variante!{$var}
                                        },
                                        $port,
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
                                            pcf8574::Beschreibung {
                                                a0: level!{$k},
                                                a1: level!{$l},
                                                a2: level!{$m},
                                                variante: variante!{$n}
                                            } => {
                                                $value! {$k $l $m $n port}
                                            }
                                        )*
                                    }
                                }
                            }
                        }
                        let port = llln_to_hhha! {match_nachricht: port_value};
                        if let Err(err) = anschlüsse.rückgabe(port) {
                            error!("Fehler bei rückgabe: {:?}", err);
                            break;
                        }
                    }
                    Err(err) => {
                        error!("Anschlüsse-static poisoned: {}", err);
                        break;
                    }
                },
                Err(err) => {
                    error!("Kanal für Pcf8574-Rückgabe geschlossen: {}", err);
                    break;
                }
            }
        }
    }

    #[cfg(not(raspi))]
    fn listen_pin_restore_messages(receiver: Receiver<u8>, inner: AnschlüsseInternal) {
        loop {
            match receiver.recv() {
                Ok(pin) => match inner.lock() {
                    Ok(mut guard) => {
                        let anschlüsse = &mut *guard;
                        debug!("rückgabe pin {}", pin);
                        anschlüsse.ausgegebene_pins.remove(&pin);
                    }
                    Err(err) => {
                        error!("Anschlüsse-static poisoned: {}", err);
                        break;
                    }
                },
                Err(err) => {
                    error!("Kanal für Pin-Rückgabe geschlossen: {}", err);
                    break;
                }
            }
        }
    }

    fn erstelle_static() -> AnschlüsseStatic {
        #[cfg(not(raspi))]
        let (pin_sender, pin_receiver) = channel();
        macro_rules! make_anschlüsse {
            {$($a0:ident $a1:ident $a2:ident $var:ident),*} => {{
                #[cfg(raspi)]
                let i2c = match rppal::i2c::I2c::new() {
                    Ok(i2c) => Arc::new(Mutex::new(i2c)),
                    Err(error) => return Arc::new(Mutex::new(Err(error.into()))),
                };
                #[cfg(raspi)]
                let gpio = match rppal::gpio::Gpio::new() {
                    Ok(gpio) => gpio,
                    Err(error) => return Arc::new(Mutex::new(Err(error.into()))),
                };
                paste! {
                    Ok(AnschlüsseData {
                        #[cfg(raspi)]
                        gpio,
                        #[cfg(raspi)]
                        i2c: i2c.clone(),
                        #[cfg(not(raspi))]
                        ausgegebene_pins: HashSet::new(),
                        #[cfg(not(raspi))]
                        pin_rückgabe: pin_sender,
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
            let inner = Arc::new(Mutex::new(anschlüsse));
            let inner_clone = inner.clone();

            let (sender, receiver) = channel();
            let sender_clone = sender.clone();

            // erzeuge Thread der Rückgaben behandelt
            thread::spawn(move || {
                Anschlüsse::listen_restore_messages(sender_clone, receiver, inner_clone)
            });

            #[cfg(not(raspi))]
            {
                let inner_clone = inner.clone();

                // erzeuge Thread der Pin-Rückgaben behandelt
                thread::spawn(move || {
                    Anschlüsse::listen_pin_restore_messages(pin_receiver, inner_clone)
                });
            }

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
                                    pcf8574::Beschreibung {
                                        a0:level!{$a0},
                                        a1:level!{$a1},
                                        a2:level!{$a2},
                                        variante:variante!{$var}
                                    },
                                    u3::new($port),
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
    pub fn rückgabe(&mut self, port: pcf8574::Port) -> Result<(), SyncFehler> {
        if let Some(arc) = &self.0 {
            arc.lock()?.rückgabe(port)
        } else {
            Err(SyncFehler::SingletonInVerwendung)
        }
    }

    /// Reserviere den spezifizierten Pin zur exklusiven Nutzung.
    /// Rückgabe über den Drop-Handler (nur bei Raspi).
    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Pin, Fehler> {
        debug!("reserviere pin {}", pin);
        if let 2 | 3 = pin {
            // Gpio 2,3 nicht verfügbar (durch I2C belegt)
            return Err(Fehler::Sync(SyncFehler::AnschlussInVerwendung(
                AnschlussBeschreibung::Pin(pin),
            )));
        }
        if let Some(arc) = self.0.as_ref() {
            #[cfg_attr(raspi, allow(unused_mut))]
            let mut anschlüsse = arc.lock()?;
            #[cfg(raspi)]
            {
                Ok(Pin::neu(anschlüsse.gpio.get(pin)?))
            }
            #[cfg(not(raspi))]
            {
                if anschlüsse.ausgegebene_pins.insert(pin) {
                    Ok(Pin::neu(pin, anschlüsse.pin_rückgabe.clone()))
                } else {
                    Err(Fehler::Sync(SyncFehler::AnschlussInVerwendung(
                        AnschlussBeschreibung::Pin(pin),
                    )))
                }
            }
        } else {
            Err(Fehler::Sync(SyncFehler::SingletonInVerwendung))
        }
    }

    /// Reserviere den spezifizierten Pcf8574-Port zur exklusiven Nutzung.
    pub fn reserviere_pcf8574_port(
        &mut self,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u3,
    ) -> Result<pcf8574::Port, SyncFehler> {
        self.0.as_mut().ok_or(SyncFehler::WertDropped).and_then(|arc| {
            // TODO besserer Fehler (welche Port wurde angefragt)
            arc.lock().map_err(SyncFehler::from).and_then(|mut guard| {
                guard.reserviere_pcf8574_port(a0, a1, a2, variante, port).ok_or(
                    SyncFehler::AnschlussInVerwendung(AnschlussBeschreibung::Pcf8574Port {
                        a0,
                        a1,
                        a2,
                        variante,
                        port,
                    }),
                )
            })
        })
    }
}

type AnschlüsseInternal = Arc<Mutex<AnschlüsseData>>;
type AnschlüsseResult = Result<AnschlüsseInternal, Fehler>;
type AnschlüsseStatic = Arc<Mutex<AnschlüsseResult>>;
static ANSCHLÜSSE: Lazy<AnschlüsseStatic> = Lazy::new(Anschlüsse::erstelle_static);

#[derive(Debug)]
pub enum Fehler {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
    Sync(SyncFehler),
}
impl From<SyncFehler> for Fehler {
    fn from(error: SyncFehler) -> Self {
        Fehler::Sync(error)
    }
}
impl<T> From<PoisonError<T>> for Fehler {
    fn from(error: PoisonError<T>) -> Self {
        SyncFehler::from(error).into()
    }
}
#[cfg(raspi)]
impl From<rppal::gpio::Error> for Fehler {
    fn from(error: rppal::gpio::Error) -> Self {
        Fehler::Gpio(error)
    }
}
#[cfg(raspi)]
impl From<rppal::i2c::Error> for Fehler {
    fn from(error: rppal::i2c::Error) -> Self {
        Fehler::I2c(error)
    }
}
#[cfg(raspi)]
impl From<rppal::pwm::Error> for Fehler {
    fn from(error: rppal::pwm::Error) -> Self {
        Fehler::Pwm(error)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnschlussBeschreibung {
    Pin(u8),
    Pcf8574Port { a0: Level, a1: Level, a2: Level, variante: pcf8574::Variante, port: u3 },
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyncFehler {
    PoisonFehler,
    SingletonInVerwendung,
    AnschlussInVerwendung(AnschlussBeschreibung),
    WertDropped,
}
impl<T> From<PoisonError<T>> for SyncFehler {
    fn from(_: PoisonError<T>) -> Self {
        SyncFehler::PoisonFehler
    }
}

#[cfg(test)]
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "anschlüsse/test.rs"]
mod test;
