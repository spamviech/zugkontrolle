//! Pcf8574, gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::fmt::Debug;
use std::sync::{mpsc::Sender, Arc, Mutex, PoisonError};

use log::debug;
use log::error;
use num_x::u3;
#[cfg(raspi)]
use num_x::u7;
#[cfg(raspi)]
use rppal::{gpio, i2c};
use serde::{Deserialize, Serialize};

use super::pin::input;
use super::{level::Level, trigger::Trigger};

pub(super) enum Modus {
    Input { trigger: Trigger, callback: Option<Box<dyn FnMut(Level) + Send + 'static>> },
    High,
    Low,
}
impl From<Level> for Modus {
    fn from(level: Level) -> Self {
        match level {
            Level::High => Modus::High,
            Level::Low => Modus::Low,
        }
    }
}
impl Debug for Modus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Modus::Input { trigger, callback: Some(_) } => {
                write!(f, "Input {{trigger: {:?}, callback: Some(_)}}", trigger)
            }
            Modus::Input { trigger, callback: None } => {
                write!(f, "Input {{trigger: {:?}, callback: None}}", trigger)
            }
            Modus::High => write!(f, "High"),
            Modus::Low => write!(f, "Low"),
        }
    }
}
/// Gleichheit unabhängig vom callback.
impl PartialEq for Modus {
    fn eq(&self, other: &Modus) -> bool {
        match (self, other) {
            (Modus::Input { .. }, Modus::Input { .. }) => true,
            (Modus::High, Modus::High) => true,
            (Modus::Low, Modus::Low) => true,
            _ => false,
        }
    }
}
impl Eq for Modus {}

/// Ein Pcf8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    ports: [Modus; 8],
    interrupt: Option<input::Pin>,
    #[cfg(raspi)]
    i2c: Arc<Mutex<i2c::I2c>>,
}

pub(super) type Nachricht = (Level, Level, Level, Variante);

impl Pcf8574 {
    pub(super) fn neu(
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        #[cfg(raspi)] i2c: Arc<Mutex<i2c::I2c>>,
    ) -> Self {
        Pcf8574 {
            a0,
            a1,
            a2,
            variante,
            ports: [
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
                Modus::Input { trigger: Trigger::Disabled, callback: None },
            ],
            interrupt: None,
            #[cfg(raspi)]
            i2c,
        }
    }

    /// Assoziiere den angeschlossenen InterruptPin.
    /// Rückgabewert ist ein evtl. vorher konfigurierter InterruptPin.
    /// Interrupt-Callbacks von Ports werden nicht zurückgesetzt!
    fn set_interrupt_pin(
        arc: &mut Arc<Mutex<Self>>,
        mut interrupt: input::Pin,
    ) -> Result<Option<input::Pin>, Error> {
        let mut previous = {
            // set up callback.
            let pcf8574 = &mut *arc.lock()?;
            let mut last = pcf8574.read()?;
            let arc_clone = arc.clone();
            interrupt.set_async_interrupt(Trigger::FallingEdge, move |_level| {
                match arc_clone.lock() {
                    Ok(mut guard) => {
                        let pcf8574 = &mut *guard;
                        match pcf8574.read() {
                            Ok(current) => {
                                for i in 0..8 {
                                    match (&mut pcf8574.ports[i], current[i], &mut last[i]) {
                                        (
                                            Modus::Input {
                                                trigger: Trigger::Both | Trigger::FallingEdge,
                                                callback: Some(callback),
                                            },
                                            Some(current),
                                            Some(last),
                                        ) if last == &mut Level::High && current == Level::Low => {
                                            callback(current);
                                        }
                                        (
                                            Modus::Input {
                                                trigger: Trigger::Both | Trigger::RisingEdge,
                                                callback: Some(callback),
                                            },
                                            Some(current),
                                            Some(last),
                                        ) if last == &mut Level::High && current == Level::Low => {
                                            callback(current);
                                        }
                                        _ => {}
                                    }
                                    last[i] = current[i];
                                }
                            }
                            Err(error) => {
                                error!(
                                    "Error while reading Pcf8574 while reacting to interrupt: {:?}",
                                    error
                                );
                            }
                        }
                    }
                    Err(_error) => {
                        error!("Poison error on pcf8574-Mutex while reacting to interrupt!")
                    }
                }
            })?;
            std::mem::replace(&mut pcf8574.interrupt, Some(interrupt))
        };
        // clear interrupt on previous pin.
        previous.as_mut().map(input::Pin::clear_async_interrupt);
        Ok(previous)
    }

    /*
    /// Adress-Bits (a0, a1, a2) und Variante eines Pcf8574.
    #[inline]
    fn adresse(&self) -> (Level, Level, Level, Variante) {
        (self.a0, self.a1, self.a2, self.variante)
    }
    */

    #[cfg(raspi)]
    /// 7-bit i2c-Adresse ohne R/W-Bit
    fn i2c_adresse(&self) -> u7 {
        let Pcf8574 { a0, a1, a2, variante, .. } = self;
        let mut adresse = u7::new(match variante {
            Variante::Normal => 0x20,
            Variante::A => 0x38,
        });
        if let Level::High = a0 {
            adresse = adresse + u7::new(0b001);
        }
        if let Level::High = a1 {
            adresse = adresse + u7::new(0b010);
        }
        if let Level::High = a2 {
            adresse = adresse + u7::new(0b100);
        }
        adresse
    }

    /// Lese von einem Pcf8574.
    /// Nur als Input konfigurierte Ports werden als Some-Wert zurückgegeben.
    ///
    /// Bei Interrupt-basiertem lesen sollten alle Port gleichzeitig gelesen werden!
    fn read(&self) -> Result<[Option<Level>; 8], Error> {
        #[cfg(raspi)]
        {
            if let Ok(mut i2c_channel) = self.i2c.lock() {
                i2c_channel.set_slave_address(self.i2c_adresse().into())?;
                let mut buf = [0; 1];
                let bytes_read = i2c_channel.read(&mut buf)?;
                if bytes_read != 1 {
                    debug!("bytes_read = {} != 1", bytes_read)
                }
                let mut result = [None; 8];
                for (port, modus) in self.ports.iter().enumerate() {
                    let port_bit = 2u8.pow(port as u32) as u8;
                    result[port] = if let Modus::Input { .. } = modus {
                        Some(if (buf[0] & port_bit) > 0 { Level::High } else { Level::Low })
                    } else {
                        None
                    };
                }
                Ok(result)
            } else {
                error!("I2C-Mutex poisoned!");
                Err(Error::PoisonError)
            }
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.read()", self);
            Err(Error::KeinRaspberryPi)
        }
    }

    /// Konvertiere einen Port als Input.
    fn port_as_input<C: FnMut(Level) + Send + 'static>(
        &mut self,
        port: u3,
        trigger: Trigger,
        callback: Option<C>,
    ) -> Result<(), Error> {
        #[cfg(raspi)]
        self.write_port(port, Level::High)?;
        // type annotations need, so extra let binding required
        let callback: Option<Box<dyn FnMut(Level) + Send + 'static>> = match callback {
            Some(c) => Some(Box::new(c)),
            None => None,
        };
        self.ports[usize::from(port)] = Modus::Input { trigger, callback };
        Ok(())
    }

    /// Schreibe auf einen Port des Pcf8574.
    /// Der Port wird automatisch als Output gesetzt.
    fn write_port(&mut self, port: u3, level: Level) -> Result<(), Error> {
        self.ports[usize::from(port)] = level.into();
        #[cfg(raspi)]
        {
            if let Ok(mut i2c_channel) = self.i2c.lock() {
                i2c_channel.set_slave_address(self.i2c_adresse().into())?;
                let mut wert = 0;
                for (port, modus) in self.ports.iter().enumerate() {
                    wert |= match modus {
                        Modus::Input { .. } | Modus::High => 2u8.pow(port as u32) as u8,
                        Modus::Low => 0,
                    };
                }
                let buf = [wert; 1];
                let bytes_written = i2c_channel.write(&buf)?;
                if bytes_written != 1 {
                    error!("bytes_written = {} != 1", bytes_written)
                }
                Ok(())
            } else {
                error!("I2C-Mutex poisoned!");
                Err(Error::PoisonError)
            }
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.write_port({}, {:?})", self, port, level);
            Err(Error::KeinRaspberryPi)
        }
    }
}
impl PartialEq for Pcf8574 {
    fn eq(&self, other: &Self) -> bool {
        self.a0 == other.a0
            && self.a1 == other.a1
            && self.a2 == other.a2
            && self.variante == other.variante
    }
}
impl Eq for Pcf8574 {}
/// Variante eines Pcf8574, beeinflusst die I2C-Adresse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Variante {
    Normal,
    A,
}

/// Ein Port eines Pcf8574.
#[derive(Debug)]
pub struct Port {
    pcf8574: Arc<Mutex<Pcf8574>>,
    nachricht: Nachricht,
    port: u3,
    sender: Sender<(Nachricht, u3)>,
}
impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.port == other.port && self.nachricht == other.nachricht
    }
}
impl Eq for Port {}
impl Drop for Port {
    fn drop(&mut self) {
        let Port { port, nachricht, sender, .. } = self;
        debug!("dropped {:?} {:?} ", nachricht, port);
        // Schicke Werte als Tupel, um keine Probleme mit dem Drop-Handler zu bekommen.
        // (Ein Klon würde bei send-Fehler eine Endlos-Schleife erzeugen)
        if let Err(err) = sender.send((nachricht.clone(), port.clone())) {
            debug!("send error while dropping: {}", err)
        }
    }
}
impl Port {
    pub(super) fn neu(
        pcf8574: Arc<Mutex<Pcf8574>>,
        nachricht: Nachricht,
        port: u3,
        sender: Sender<(Nachricht, u3)>,
    ) -> Self {
        Port { pcf8574, port, nachricht, sender }
    }

    #[inline]
    pub fn adresse(&self) -> &Nachricht {
        &self.nachricht
    }

    #[inline]
    pub fn port(&self) -> u3 {
        self.port
    }

    /// Konfiguriere den Port für Output.
    pub fn into_output(self, level: Level) -> Result<OutputPort, Error> {
        {
            let pcf8574 = &mut *self.pcf8574.lock()?;
            #[cfg(raspi)]
            {
                pcf8574.write_port(self.port, level)?;
            }
            #[cfg(not(raspi))]
            {
                pcf8574.ports[usize::from(self.port)] = level.into();
            }
        }
        Ok(OutputPort(self))
    }

    /// Konfiguriere den Port für Input.
    pub fn into_input(self) -> Result<InputPort, Error> {
        {
            let pcf8574 = &mut *self.pcf8574.lock()?;
            pcf8574.port_as_input::<fn(Level)>(self.port, Trigger::Disabled, None)?;
        }
        Ok(InputPort(self))
    }
}

// Ein Port eines Pcf8574, konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPort(Port);

impl OutputPort {
    #[inline]
    pub fn adresse(&self) -> &Nachricht {
        self.0.adresse()
    }

    #[inline]
    pub fn port(&self) -> u3 {
        self.0.port()
    }

    pub fn write(&mut self, level: Level) -> Result<(), Error> {
        {
            let pcf8574 = &mut *self.0.pcf8574.lock()?;
            pcf8574.write_port(self.0.port, level)?;
        }
        Ok(())
    }

    pub fn is_set_high(&mut self) -> Result<bool, Error> {
        let pcf8574 = &mut *self.0.pcf8574.lock()?;
        Ok(pcf8574.ports[usize::from(self.port())] == Modus::High)
    }

    pub fn is_set_low(&mut self) -> Result<bool, Error> {
        let pcf8574 = &mut *self.0.pcf8574.lock()?;
        Ok(pcf8574.ports[usize::from(self.port())] == Modus::Low)
    }

    pub fn toggle(&mut self) -> Result<(), Error> {
        let level = {
            let pcf8574 = &*self.0.pcf8574.lock()?;
            let modus = &pcf8574.ports[usize::from(self.port())];
            match modus {
                Modus::High => Level::Low,
                Modus::Low => Level::High,
                Modus::Input { .. } => {
                    error!("Output pin configured as input: {:?}", self);
                    Level::Low
                }
            }
        };
        self.write(level)
    }
}

// Ein Port eines Pcf8574, konfiguriert für Input.
#[derive(Debug)]
pub struct InputPort(Port);

impl InputPort {
    #[inline]
    pub fn adresse(&self) -> &Nachricht {
        self.0.adresse()
    }

    #[inline]
    pub fn port(&self) -> u3 {
        self.0.port()
    }

    pub fn read(&self) -> Result<Level, Error> {
        let values = {
            let pcf8574 = &mut *self.0.pcf8574.lock()?;
            pcf8574.read()?
        };
        if let Some(value) = values[usize::from(self.0.port)] {
            Ok(value)
        } else {
            error!("{:?} war nicht als input korrigiert!", self);
            // war nicht als Input konfiguriert -> erneut konfigurieren und neu versuchen
            {
                let pcf8574 = &mut *self.0.pcf8574.lock()?;
                pcf8574.port_as_input::<fn(Level)>(self.0.port, Trigger::Disabled, None)?;
            }
            self.read()
        }
    }

    /// Aktuell konfigurierter Interrupt Pin.
    pub(super) fn interrupt_pin(&self) -> Result<Option<u8>, Error> {
        let pcf8574 = &mut *self.0.pcf8574.lock()?;
        Ok(pcf8574.interrupt.as_ref().map(input::Pin::pin))
    }

    /// Assoziiere den angeschlossenen InterruptPin für den Pcf8574.
    /// Rückgabewert ist ein evtl. vorher konfigurierter InterruptPin.
    /// Interrupt-Callbacks werden nicht zurückgesetzt!
    pub fn set_interrupt_pin(
        &mut self,
        interrupt: input::Pin,
    ) -> Result<Option<input::Pin>, Error> {
        Pcf8574::set_interrupt_pin(&mut self.0.pcf8574, interrupt)
    }

    /// Configures an asynchronous interrupt trigger, which executes the callback on a separate
    /// thread when the interrupt is triggered.
    ///
    /// The callback closure or function pointer is called with a single Level argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared when
    /// set_async_interrupt is called, or when InputPin goes out of scope.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline]
    pub fn set_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Error> {
        let port = self.port();
        let pcf8574 = &mut *self.0.pcf8574.lock()?;
        pcf8574.port_as_input(port, trigger, Some(callback))
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    #[inline]
    pub fn clear_async_interrupt(&mut self) -> Result<(), Error> {
        let port = self.port();
        let pcf8574 = &mut *self.0.pcf8574.lock()?;
        pcf8574.port_as_input::<fn(Level)>(port, Trigger::Disabled, None)
    }
}

// TODO genauere Eingrenzung auf einzelne Methoden
#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    I2c(i2c::Error),
    #[cfg(raspi)]
    Gpio(gpio::Error),
    #[cfg(not(raspi))]
    KeinRaspberryPi,
    PoisonError,
    KeinInterruptPin,
}
#[cfg(raspi)]
impl From<i2c::Error> for Error {
    fn from(error: i2c::Error) -> Self {
        Error::I2c(error)
    }
}
impl From<input::Error> for Error {
    fn from(error: input::Error) -> Self {
        match error {
            #[cfg(raspi)]
            input::Error::Gpio(err) => Error::Gpio(err),
            #[cfg(not(raspi))]
            input::Error::KeinRaspberryPi => Error::KeinRaspberryPi,
        }
    }
}
impl<T> From<PoisonError<T>> for Error {
    fn from(_: PoisonError<T>) -> Self {
        Error::PoisonError
    }
}
