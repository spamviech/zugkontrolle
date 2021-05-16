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
    ( $inner_macro:ident [$($k:ident),+] $ls:tt $ms:tt $ns:tt: $value:ident) => {
        matrix! { $inner_macro $($k $ls $ms $ns)+: $value}
    };
    ( $inner_macro:ident $($k:ident [$($l:tt),+] $ms:tt $ns:tt)+: $value:ident) => {
        matrix! { $inner_macro $($( $k $l $ms $ns )+)+: $value }
    };
    ( $inner_macro:ident $($k:ident $l:ident [$($m:tt),+] $ns:tt)+: $value:ident) => {
        matrix! { $inner_macro $($( $k $l $m $ns )+)+: $value }
    };
    ( $inner_macro:ident $($k:ident $l:ident $m:ident [$($n:tt),+])+: $value:ident) => {
         $inner_macro! { $($($k $l $m $n),+),+: $value }
    };
}
macro_rules! anschlüsse_data {
    {$($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
        paste! {
            #[doc="Singleton für Zugriff auf raspberry pi Anschlüsse."]
            #[derive(Debug)]
            struct AnschlüsseData {
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
macro_rules! llln_to_hhha {
    ($inner_macro:ident : $value:ident) => {
        matrix! {$inner_macro  [l,h] [l,h] [l,h] [n,a]: $value}
    };
}

llln_to_hhha! { anschlüsse_data: pcf8574_type}
impl AnschlüsseData {
    /// Gebe den Pcf8574 an Anschlüsse zurück, so dass er von anderen verwendet werden kann.
    ///
    /// Wird vom Drop-handler ausgeführt, hier ist es explizit.
    fn rückgabe(&mut self, pcf8574: Pcf8574) {
        match pcf8574 {
            Pcf8574 {
                a0: Level::Low,
                a1: Level::Low,
                a2: Level::Low,
                variante: Pcf8574Variante::Normal,
                ..
            } => {
                debug!("rückgabe llln");
                self.llln = Some(pcf8574);
            },
            _ => {
                // TODO restore correct one
                debug!("rückgabe {:?}", pcf8574)
            },
        }
    }

    fn llln(&mut self) -> Option<Pcf8574> {
        // gebe aktuellen Wert zurück und speichere stattdessen None
        std::mem::replace(&mut self.llln, None)
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
            if let Err(Error::InVerwendung) = static_anschlüsse {
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
                let data = std::mem::replace(anschlüsse, Err(Error::InVerwendung))?;
                Ok(Anschlüsse(Some(data)))
            },
            Err(_er) => Err(Error::PoisonError),
        }
    }

    fn listen_restore_messages(
        sender: Sender<(Level, Level, Level, Pcf8574Variante, u8)>,
        receiver: Receiver<(Level, Level, Level, Pcf8574Variante, u8)>,
        inner: AnschlüsseInternal,
    ) {
        loop {
            match receiver.recv() {
                Ok((a0, a1, a2, variante, wert)) => match inner.lock() {
                    Ok(mut guard) => {
                        let pcf8574 =
                            Pcf8574 { a0, a1, a2, variante, wert, sender: sender.clone() };
                        guard.rückgabe(pcf8574)
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
            {$($k:ident $l:ident $m:ident $n:ident),*: $value:ident} => {
                paste! {
                    Ok(AnschlüsseData {
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
            };
        }
        let inner = (llln_to_hhha! {make_anschlüsse: none}).map(|anschlüsse| {
            let inner = Arc::new(Mutex::new(anschlüsse));
            let inner_clone = inner.clone();

            let (sender, receiver) = channel();
            let sender_clone = sender.clone();

            // erzeuge Thread der Rückgaben handelt
            thread::spawn(move || {
                Anschlüsse::listen_restore_messages(sender_clone, receiver, inner_clone)
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

            // Eigener Block um borrow-lifetime von inner zu beschränken
            {
                let anschlüsse =
                    &mut *inner.lock().expect("Anschlüsse poisoned vor Initialisierung");
                anschlüsse.llln = pcf8574_value! {l l l n};
                // TODO
            }

            inner
        });

        Arc::new(Mutex::new(inner))
    }

    /// Gebe den Pcf8574 an Anschlüsse zurück, so dass er von anderen verwendet werden kann.
    ///
    /// TODO
    /// Der Drop-Handler von Pcf8574 zeigt erst bei neu erstellten Anschlüsse-Strukturen Wirkung.
    /// Diese Methode funktioniert direkt.
    pub fn rückgabe(&mut self, pcf8574: Pcf8574) {
        if let Some(arc) = &self.0 {
            if let Ok(mut guard) = arc.lock() {
                guard.rückgabe(pcf8574)
            }
        }
    }

    /// Versuche Zugriff auf den Pcf8574 mit der Adresse lll, normale Variante zu erhalten.
    pub fn llln(&mut self) -> Option<Pcf8574> {
        if let Some(arc) = &self.0 {
            if let Ok(mut guard) = arc.lock() {
                guard.llln()
            } else {
                None
            }
        } else {
            None
        }
    }
}

type AnschlüsseInternal = Arc<Mutex<AnschlüsseData>>;
type AnschlüsseResult = Result<AnschlüsseInternal, Error>;
type AnschlüsseStatic = Arc<Mutex<AnschlüsseResult>>;
static ANSCHLÜSSE: Lazy<AnschlüsseStatic> = Lazy::new(Anschlüsse::erstelle_static);

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
    sender: Sender<(Level, Level, Level, Pcf8574Variante, u8)>,
}
impl Pcf8574 {
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
        let Pcf8574 { a0, a1, a2, variante, wert, sender } = self;
        debug!("dropped {:?} {:?} {:?} {:?}", a0, a1, a2, variante);
        // Schicke Werte als Tupel, um keine Probleme mit dem Drop-Handler zu bekommen.
        // (Ein Klon würde bei send-Fehler eine Endlos-Schleife erzeugen)
        if let Err(err) = sender.send((*a0, *a1, *a2, *variante, *wert)) {
            debug!("send error while dropping: {}", err)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
    PoisonError,
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

#[cfg(test)]
mod test {
    use std::thread::sleep;
    use std::time::Duration;

    use simple_logger::SimpleLogger;

    use super::{Anschlüsse, Level, Pcf8574Variante};

    #[test]
    fn drop_semantics() {
        SimpleLogger::new()
            .with_level(log::LevelFilter::Error)
            .with_module_level("zugkontrolle", log::LevelFilter::Debug)
            .init()
            .expect("failed to initialize error logging");

        let mut anschlüsse = Anschlüsse::neu().expect("1.ter Aufruf von neu.");
        Anschlüsse::neu().expect_err("2.ter Aufruf von neu.");
        let llln = anschlüsse.llln().expect("1. Aufruf von llln.");
        assert_eq!([llln.a0, llln.a1, llln.a2], [Level::Low, Level::Low, Level::Low]);
        assert_eq!(llln.variante, Pcf8574Variante::Normal);
        assert!(anschlüsse.llln().is_none(), "2. Aufruf von llln.");
        drop(llln);
        // Warte etwas, damit der restore-thread genug Zeit hat.
        sleep(Duration::from_secs(1));
        let llln = anschlüsse.llln().expect("Aufruf von llln nach drop.");
        drop(anschlüsse);

        // jetzt sollte Anschlüsse wieder verfügbar sein
        let mut anschlüsse = Anschlüsse::neu().expect("Aufruf von neu nach drop.");
        assert!(anschlüsse.llln().is_none(), "Aufruf von llln mit vorherigem Ergebnis in scope.");
        drop(llln);
        // Warte etwas, damit der restore-thread genug Zeit hat.
        sleep(Duration::from_secs(1));
        let llln = anschlüsse.llln().expect("Aufruf von llln nach drop.");
        drop(llln);
        drop(anschlüsse);
    }
}
