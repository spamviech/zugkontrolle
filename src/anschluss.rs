//! Mit Raspberry Pi schaltbarer Anschluss

use std::ops::Not;
use std::sync::{
    mpsc::{channel, Receiver, Sender},
    Arc,
    Mutex,
    RwLock,
};
use std::thread;

use cfg_if::cfg_if;
use log::{debug, error};
use once_cell::sync::Lazy;
use paste::paste;

/// originally taken from: https://www.ecorax.net/macro-bunker-1/
/// adjusted to 4 arguments
macro_rules! matrix {
    ( $inner_macro:ident $ks:tt $ls:tt $ms:tt $ns:tt: $value:ident) => {
        matrix! { [identity] $inner_macro $ks $ls $ms $ns: $value}
    };
    ( [$prefix:expr] $inner_macro:ident [$($k:ident),+] $ls:tt $ms:tt $ns:tt: $value:ident) => {
        matrix! { [$prefix] $inner_macro $($k $ls $ms $ns)+: $value}
    };
    ( [$prefix:expr] $inner_macro:ident $($k:ident [$($l:tt),+] $ms:tt $ns:tt)+: $value:ident) => {
        matrix! { [$prefix] $inner_macro $($( $k $l $ms $ns )+)+: $value }
    };
    ( [$prefix:expr] $inner_macro:ident $($k:ident $l:ident [$($m:tt),+] $ns:tt)+: $value:ident) => {
        matrix! { [$prefix] $inner_macro $($( $k $l $m $ns )+)+: $value }
    };
    ( [$prefix:expr] $inner_macro:ident $($k:ident $l:ident $m:ident [$($n:tt),+])+: $value:ident) => {
         $inner_macro! { [$prefix] $($($k $l $m $n),+),+: $value }
    };
}
macro_rules! identity {
    ($($suffix:tt)*) => {
        $($suffix)*
    };
}
macro_rules! anschlüsse {
    {[$prefix:expr] $($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
        paste! {
            $prefix! {
                Anschlüsse {
                    #[cfg(raspi)]
                    gpio: rppal::gpio::Gpio,
                    #[cfg(raspi)]
                    i2c: rppal::gpio::I2c,
                    #[cfg(raspi)]
                    pwm: rppal::pwm::Pwm,
                    $(
                        [<$k $l $m $n>]: $value!($k $l $m $n)
                    ),*
                }
            }
        }
    };
}
macro_rules! pcf8574_type {
    ($($arg:tt)*) => {
        Option<Pcf8574>
    };
}
macro_rules! none {
    ($($arg:tt)*) => {
        None
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
        Pcf8574Variante::Normal
    };
    (a) => {
        Pcf8574Variante::A
    };
}
macro_rules! pub_struct_prefix {
    ($($suffix: tt)*) => {
        #[doc="Singleton für Zugriff auf raspberry pi Anschlüsse."]
        #[derive(Debug)]
        pub struct $($suffix)*
    };
}

matrix! { [pub_struct_prefix] anschlüsse [l,h] [l,h] [l,h] [n,a]: pcf8574_type}
impl Drop for Anschlüsse {
    fn drop(&mut self) {
        if let Ok(mut guard) = ANSCHLÜSSE.lock() {
            let anschlüsse = &mut *guard;
            if let Err(Error::InVerwendung) = anschlüsse {
                // let a = std::mem::replace(self);
                todo!()
                //TODO
            }
        }
    }
}
impl Anschlüsse {
    pub fn neu() -> Result<Anschlüsse, Error> {
        //TODO
        todo!()
    }

    fn erstelle_static() -> Arc<Mutex<Result<Anschlüsse, Error>>> {
        macro_rules! make_anschlüsse {
            {[$prefix:expr] $($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
                paste! {
                    $prefix! {
                        Ok(Anschlüsse {
                            #[cfg(raspi)]
                            gpio: rppal::gpio::Gpio::new()?,
                            #[cfg(raspi)]
                            i2c: rppal::gpio::I2c::new()?,
                            #[cfg(raspi)]
                            pwm: rppal::pwm::Pwm::new()?,
                            $(
                                [<$k $l $m $n>]: $value!($k $l $m $n)
                            ),*
                        })
                    }
                }
            };
        }
        let anschlüsse = matrix! {make_anschlüsse [l,h] [l,h] [l,h] [n,a]: none};
        let arc = Arc::new(Mutex::new(anschlüsse));

        let (sender, receiver) = channel();

        // erzeuge Thread der Rückgaben handelt
        let arc_clone = arc.clone();
        thread::spawn(move || {
            // zur Rückgabe gemeldet
            let mut stack = Vec::new();
            loop {
                match receiver.recv() {
                    Ok(pcf8574) => {
                        let mut guard = arc_clone.lock().expect("poisoned bei Initialisierung");
                        if let Ok(anschlüsse) = guard.as_mut() {
                            // TODO restore correct one
                            todo!("{:?}", anschlüsse)
                        } else {
                            stack.push(pcf8574);
                        }
                        //TODO
                        todo!()
                    },
                    Err(err) => {
                        error!("Kanal für Pcf8574-Rückgabe geschlossen: {}", err);
                        break
                    },
                }
            }
        });

        macro_rules! pcf8574_value {
            ($a0:ident $a1:ident $a2:ident $var:ident) => {
                Some(Pcf8574 {
                    a0: level!($a0),
                    a1: level!($a1),
                    a2: level!($a2),
                    variante: variante!($var),
                    wert: 0,
                    sender: sender.clone(),
                })
            };
        }

        // Eigener Block um borrow-lifetime von arc zu beschränken
        {
            let mut guard = arc.lock().expect("poisoned bei Initialisierung");
            if let Ok(anschlüsse) = guard.as_mut() {
                anschlüsse.llln = pcf8574_value! {l l l n};
                // TODO
            }
        }

        arc
    }

    pub fn llln(&mut self) -> Option<Pcf8574> {
        // gebe aktuellen Wert zurück und speichere stattdessen None
        std::mem::replace(&mut self.llln, None)
    }
}

pub static ANSCHLÜSSE: Lazy<Arc<Mutex<Result<Anschlüsse, Error>>>> =
    Lazy::new(Anschlüsse::erstelle_static);

/// Ein Anschluss
#[derive(Debug)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(Pcf8574Port),
}

/// Ein Gpio Pin.
#[derive(Debug)]
pub struct Pin(#[cfg(raspi)] rppal::gpio::Pin);
/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug)]
pub struct InputPin(#[cfg(raspi)] rppal::gpio::InputPin);
/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPin(#[cfg(raspi)] rppal::gpio::OutputPin);
/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug)]
pub struct PwmPin(#[cfg(raspi)] Pwm);
#[cfg(raspi)]
enum Pwm {
    Pwm(rppal::pwm::Pwm),
    Pin(rppal::gpio::Pin),
}

/// Ein PCF8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Pcf8574Variante,
    wert: u8,
    sender: Sender<Pcf8574>,
}
impl Pcf8574 {
    fn clone(&self) -> Self {
        let Pcf8574 { a0, a1, a2, variante, wert, sender } = self;
        Pcf8574 {
            a0: *a0,
            a1: *a1,
            a2: *a2,
            variante: *variante,
            wert: *wert,
            sender: sender.clone(),
        }
    }

    pub fn ports(self) -> Pcf8574Ports {
        // drop for self will be called when last Arc goes out of scope
        let arc = Arc::new(RwLock::new(self));
        Pcf8574Ports {
            p0: Pcf8574Port { pcf8574: arc.clone(), port: 0 },
            p1: Pcf8574Port { pcf8574: arc.clone(), port: 1 },
            p2: Pcf8574Port { pcf8574: arc.clone(), port: 2 },
            p3: Pcf8574Port { pcf8574: arc.clone(), port: 3 },
            p4: Pcf8574Port { pcf8574: arc.clone(), port: 4 },
            p5: Pcf8574Port { pcf8574: arc.clone(), port: 5 },
            p6: Pcf8574Port { pcf8574: arc.clone(), port: 6 },
            p7: Pcf8574Port { pcf8574: arc, port: 7 },
        }
    }
}
impl Drop for Pcf8574 {
    fn drop(&mut self) {
        let clone = self.clone();
        match self {
            Pcf8574 {
                a0: Level::Low,
                a1: Level::Low,
                a2: Level::Low,
                variante: Pcf8574Variante::Normal,
                wert: _,
                sender,
            } => {
                debug!("dropped llln");
                if let Err(err) = sender.send(clone) {
                    debug!("send error while dropping llln: {}", err)
                }
            },
            _ => {
                debug!("dropped {:?}", self)
            },
        }
    }
}

/// Alle Ports eines PCF8574.
#[derive(Debug)]
pub struct Pcf8574Ports {
    p0: Pcf8574Port,
    p1: Pcf8574Port,
    p2: Pcf8574Port,
    p3: Pcf8574Port,
    p4: Pcf8574Port,
    p5: Pcf8574Port,
    p6: Pcf8574Port,
    p7: Pcf8574Port,
}

/// Ein Port eines PCF8574.
#[derive(Debug)]
pub struct Pcf8574Port {
    pcf8574: Arc<RwLock<Pcf8574>>,
    port: u8,
}

#[derive(Debug, Clone, Copy)]
pub enum Pcf8574Variante {
    Normal,
    A,
}

cfg_if! {
    if #[cfg(raspi)] {
        pub use rppal::gpio::Level;
    } else {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Level {
            Low,
            High,
        }
        impl Not for Level {
            type Output = Self;

            fn not(self) -> Self::Output {
                match self {
                    Level::Low => Level::High,
                    Level::High => Level::Low,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
    InVerwendung,
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
