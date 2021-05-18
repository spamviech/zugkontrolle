//! Pcf8574, gesteuert über I2C.

#[cfg(raspi)]
use std::sync::Mutex;
use std::sync::{mpsc::Sender, Arc, PoisonError, RwLock};

use cfg_if::cfg_if;
use log::debug;
use log::error;
use num_x::u3;
#[cfg(raspi)]
use num_x::u7;
#[cfg(raspi)]
use rppal::i2c;

use super::level::Level;
use super::pin::input;

#[derive(Debug, Clone, Copy)]
pub(super) enum Modus {
    Input,
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

/// Ein Pcf8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    ports: [Modus; 8],
    sender: Sender<(Level, Level, Level, Variante)>,
    #[cfg(raspi)]
    i2c: Arc<RwLock<i2c::I2C>>,
}

impl Pcf8574 {
    pub(super) fn neu(
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        sender: Sender<(Level, Level, Level, Variante)>,
        #[cfg(raspi)] i2c: Arc<i2c::I2c>,
    ) -> Self {
        Pcf8574 {
            a0,
            a1,
            a2,
            variante,
            sender,
            ports: [Modus::Input; 8],
            #[cfg(raspi)]
            i2c,
        }
    }

    /// Assoziiere den angeschlossenen InterruptPin.
    pub fn with_interrupt(self, interrupt: input::Pin) -> InterruptPcf8574 {
        InterruptPcf8574 { pcf8574: self, interrupt }
    }

    /// Adress-Bits (a0, a1, a2) und Variante eines Pcf8574.
    #[inline]
    pub fn adresse(&self) -> (Level, Level, Level, Variante) {
        (self.a0, self.a1, self.a2, self.variante)
    }

    #[cfg(raspi)]
    /// 7-bit i2c-Adresse ohne R/W-Bit
    fn i2c_adresse(&self) -> u7 {
        let Pcf8574 { a0, a1, a2, variante, .. } = self;
        let mut adresse = u7::new(match variante {
            Variante::Normal => 0x20,
            Variante::A => 0x38,
        });
        if let Level::High = a0 {
            adresse = adresse + u7::new(0b100);
        }
        if let Level::High = a1 {
            adresse = adresse + u7::new(0b010);
        }
        if let Level::High = a2 {
            adresse = adresse + u7::new(0b001);
        }
        adresse
    }

    /// Lese von einem Pcf8574.
    /// Nur als Input konfigurierte Ports werden als Some-Wert zurückgegeben.
    ///
    /// Bei Interrupt-basiertem lesen sollten alle Port gleichzeitig gelesen werden!
    fn read(&mut self) -> Result<[Option<Level>; 8], Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                if let Ok(mut i2c_channel) = &mut *self.i2c.lock() {
                    i2c_channel.set_slave_address(self.i2c_adresse.into())?;
                    let mut buf = [0; 1];
                    let bytes_read = i2c_channel.read(&mut buf)?;
                    if bytes_read != 1 {
                        debug!("bytes_read = {} != 1", bytes_read)
                    }
                    let mut result = [None; 8];
                    for (port, modus) in self.ports.iter().enumerate() {
                        let port_bit = 2u8.pow(port as u32) as u8;
                        if let Modus::Input = modus {
                            Some(if (buf[0] & port_bit) > 0 { Level::High } else { Level::Low })
                        } else {
                            None
                        }
                    }
                    Ok(result)
                } else {
                    error!("I2C-Mutex poisoned!");
                    Err(Error::PoisonError)
                }
            } else {
                debug!("{:?}.read()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Konvertiere einen Port als Input.
    fn port_as_input(&mut self, port: u3) -> Result<(), Error> {
        self.write_port(port, Level::High)?;
        self.ports[usize::from(port)] = Modus::Input;
        Ok(())
    }

    /// Schreibe auf einen Port des Pcf8574.
    /// Der Port wird automatisch als Output gesetzt.
    fn write_port(&mut self, port: u3, level: Level) -> Result<(), Error> {
        self.ports[usize::from(port)] = level.into();
        cfg_if! {
            if #[cfg(raspi)] {
                if let Ok(mut i2c_channel) = &mut *self.i2c.lock() {
                    i2c_channel.set_slave_address(self.i2c_adresse.into())?;
                    let mut wert = 0;
                    for (port, modus) in self.ports.iter().enumerate() {
                        wert |= match modus {
                            Modus::Input | Modus::High => 2u8.pow(port as u32) as u8,
                            Modus::Low => 0,
                        };
                    }
                    let buf = [wert; 1];
                    let bytes_written = i2c_channel.write(&buf)?;
                    if bytes_written != 1 {
                        debug!("bytes_written = {} != 1", bytes_written)
                    }
                    Ok(())
                } else {
                    error!("I2C-Mutex poisoned!");
                    Err(Error::PoisonError)
                }
            } else {
                debug!("{:?}.read()", self);
                Err(Error::KeinRaspberryPi)
            }
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
impl Drop for Pcf8574 {
    fn drop(&mut self) {
        let Pcf8574 { a0, a1, a2, variante, sender, .. } = self;
        debug!("dropped {:?} {:?} {:?} {:?}", a0, a1, a2, variante);
        // Schicke Werte als Tupel, um keine Probleme mit dem Drop-Handler zu bekommen.
        // (Ein Klon würde bei send-Fehler eine Endlos-Schleife erzeugen)
        if let Err(err) = sender.send((*a0, *a1, *a2, *variante)) {
            debug!("send error while dropping: {}", err)
        }
    }
}
/// Variante eines Pcf8574, beeinflusst die I2C-Adresse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variante {
    Normal,
    A,
}
/// Ein Pcf8574, konfiguriert für Input inklusive InterruptPin.
#[derive(Debug)]
pub struct InterruptPcf8574 {
    pcf8574: Pcf8574,
    interrupt: input::Pin,
}
impl InterruptPcf8574 {
    /// Adress-Bits (a0, a1, a2) und Variante eines Pcf8574.
    #[inline]
    pub fn adresse(&self) -> (Level, Level, Level, Variante) {
        self.pcf8574.adresse()
    }

    /// Lese von einem Pcf8574.
    /// Nur als Input konfigurierte Ports werden als Some-Wert zurückgegeben.
    ///
    /// Bei Interrupt-basiertem lesen sollten alle Port gleichzeitig gelesen werden!
    #[inline]
    fn read(&mut self) -> Result<[Option<Level>; 8], Error> {
        self.pcf8574.read()
    }

    /// Konvertiere einen Port als Input.
    fn port_as_input(&mut self, port: u3) -> Result<(), Error> {
        self.pcf8574.port_as_input(port)
    }

    /// Schreibe auf einen Port des Pcf8574.
    /// Der Port wird automatisch als Output gesetzt.
    #[inline]
    fn write_port(&mut self, port: u3, level: Level) -> Result<(), Error> {
        self.pcf8574.write_port(port, level)
    }
}

/// Ein Port eines Pcf8574.
#[derive(Debug)]
pub struct Port<T> {
    pcf8574: Arc<RwLock<T>>,
    port: u3,
}
impl<T> Port<T> {
    pub fn port(&self) -> u3 {
        self.port
    }
}
macro_rules! impl_port_into {
    ($type:ty) => {
        impl Port<$type> {
            /// Konfiguriere den Port für Output.
            pub fn into_output(self) -> Result<OutputPort<$type>, Error> {
                {
                    let pcf8574 = &mut *self.pcf8574.write()?;
                    pcf8574.write_port(self.port, Level::High)?;
                }
                Ok(OutputPort(self))
            }

            /// Konfiguriere den Port für Input.
            pub fn into_input(self) -> Result<OutputPort<$type>, Error> {
                {
                    let pcf8574 = &mut *self.pcf8574.write()?;
                    pcf8574.port_as_input(self.port)?;
                }
                Ok(OutputPort(self))
            }
        }
    };
}
impl_port_into! {Pcf8574}
impl_port_into! {InterruptPcf8574}

// Ein Port eines Pcf8574, konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPort<T>(Port<T>);
macro_rules! impl_port_output {
    ($type:ty) => {
        impl OutputPort<$type> {
            pub fn write(&mut self, level: Level) -> Result<(), Error> {
                {
                    let pcf8574 = &mut *self.0.pcf8574.write()?;
                    pcf8574.write_port(self.0.port, level)?;
                }
                Ok(())
            }
        }
    };
}
impl_port_output! {Pcf8574}
impl_port_output! {InterruptPcf8574}

// Ein Port eines Pcf8574, konfiguriert für Input.
#[derive(Debug)]
pub struct InputPort<T>(Port<T>);
macro_rules! impl_port_read {
    ($type:ty) => {
        impl InputPort<$type> {
            pub fn read(&mut self) -> Result<Level, Error> {
                let values = {
                    let pcf8574 = &mut *self.0.pcf8574.write()?;
                    pcf8574.read()?
                };
                if let Some(value) = values[usize::from(self.0.port)] {
                    Ok(value)
                } else {
                    error!("{:?} war nicht als input korrigiert!", self);
                    // war nicht als Input konfiguriert -> erneut konfigurieren und neu versuchen
                    {
                        let pcf8574 = &mut *self.0.pcf8574.write()?;
                        pcf8574.port_as_input(self.0.port)?;
                    }
                    self.read()
                }
            }
        }
    };
}
impl_port_read! {Pcf8574}
impl_port_read! {InterruptPcf8574}
// TODO Methoden für InputPort<InterruptPcf8574>
// poll_interrupt, poll_async_interrupt, clear_async_interrupt
//      interrupt pin is normally HIGH and switches to LOW on change

/// Alle Ports eines Pcf8574.
#[derive(Debug)]
pub struct Ports<T> {
    pub p0: Port<T>,
    pub p1: Port<T>,
    pub p2: Port<T>,
    pub p3: Port<T>,
    pub p4: Port<T>,
    pub p5: Port<T>,
    pub p6: Port<T>,
    pub p7: Port<T>,
}
impl<T> From<T> for Ports<T> {
    fn from(t: T) -> Self {
        // for Pcf8574 as T, drop will be called when last Arc goes out of scope
        let arc = Arc::new(RwLock::new(t));
        Ports {
            p0: Port { pcf8574: arc.clone(), port: u3::new(0) },
            p1: Port { pcf8574: arc.clone(), port: u3::new(1) },
            p2: Port { pcf8574: arc.clone(), port: u3::new(2) },
            p3: Port { pcf8574: arc.clone(), port: u3::new(3) },
            p4: Port { pcf8574: arc.clone(), port: u3::new(4) },
            p5: Port { pcf8574: arc.clone(), port: u3::new(5) },
            p6: Port { pcf8574: arc.clone(), port: u3::new(6) },
            p7: Port { pcf8574: arc, port: u3::new(7) },
        }
    }
}

pub enum Error {
    #[cfg(raspi)]
    I2c(i2c::Error),
    #[cfg(not(raspi))]
    KeinRaspberryPi,
    PoisonError,
}
#[cfg(raspi)]
impl From<i2c::Error> for Error {
    fn from(error: i2c::Error) -> Self {
        Error::I2c(error)
    }
}
impl<T> From<PoisonError<T>> for Error {
    fn from(_: PoisonError<T>) -> Self {
        Error::PoisonError
    }
}
