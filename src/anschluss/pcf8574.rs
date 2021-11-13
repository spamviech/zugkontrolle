//! Pcf8574, gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::fmt::Debug;
use std::sync::{mpsc::Sender, Arc, Mutex, RwLock};

use log::debug;
use log::error;
use num_x::u3;
use num_x::u7;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        pin::input,
        {level::Level, trigger::Trigger},
    },
    rppal::{gpio, i2c},
};

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
    i2c: Arc<RwLock<i2c::I2c>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Beschreibung {
    pub a0: Level,
    pub a1: Level,
    pub a2: Level,
    pub variante: Variante,
}

impl Pcf8574 {
    fn beschreibung(&self) -> Beschreibung {
        let Pcf8574 { a0, a1, a2, variante, .. } = self;
        Beschreibung { a0: *a0, a1: *a1, a2: *a2, variante: *variante }
    }

    pub(super) fn neu(
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        i2c: Arc<RwLock<i2c::I2c>>,
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
            i2c,
        }
    }

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
    fn read(&self) -> Result<[Option<Level>; 8], Fehler> {
        let beschreibung = self.beschreibung();
        let map_fehler = |fehler| Fehler::I2c { beschreibung: beschreibung.clone(), fehler };
        if let Ok(mut i2c_channel) = self.i2c.write() {
            i2c_channel.set_slave_address(self.i2c_adresse().into()).map_err(&map_fehler)?;
            let mut buf = [0; 1];
            let bytes_read = i2c_channel.read(&mut buf).map_err(map_fehler)?;
            if bytes_read != 1 {
                debug!("bytes_read = {} != 1", bytes_read)
            }
            let mut result = [None; 8];
            for (port, modus) in self.ports.iter().enumerate() {
                let port_bit = 2u8.pow(port as u32);
                result[port] = if let Modus::Input { .. } = modus {
                    Some(if (buf[0] & port_bit) > 0 { Level::High } else { Level::Low })
                } else {
                    None
                };
            }
            Ok(result)
        } else {
            error!("I2C-Mutex poisoned!");
            Err(Fehler::PoisonFehler(beschreibung))
        }
    }

    /// Konvertiere einen Port als Input.
    fn port_as_input<C: FnMut(Level) + Send + 'static>(
        &mut self,
        port: u3,
        trigger: Trigger,
        callback: Option<C>,
    ) -> Result<(), Fehler> {
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
    fn write_port(&mut self, port: u3, level: Level) -> Result<(), Fehler> {
        self.ports[usize::from(port)] = level.into();
        let beschreibung = self.beschreibung();
        let mut i2c_channel = if let Ok(i2c_channel) = self.i2c.write() {
            i2c_channel
        } else {
            error!("I2C-Mutex poisoned!");
            return Err(Fehler::PoisonFehler(beschreibung));
        };
        let map_fehler = |fehler| Fehler::I2c { beschreibung: beschreibung.clone(), fehler };
        i2c_channel.set_slave_address(self.i2c_adresse().into()).map_err(&map_fehler)?;
        let mut wert = 0;
        for (port, modus) in self.ports.iter().enumerate() {
            wert |= match modus {
                Modus::Input { .. } | Modus::High => 2u8.pow(port as u32),
                Modus::Low => 0,
            };
        }
        let buf = [wert; 1];
        let bytes_written = i2c_channel.write(&buf).map_err(map_fehler)?;
        if bytes_written != 1 {
            error!("bytes_written = {} != 1", bytes_written)
        }
        Ok(())
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
    beschreibung: Beschreibung,
    port: u3,
    sender: Sender<(Beschreibung, u3)>,
}
impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.port == other.port && self.beschreibung == other.beschreibung
    }
}
impl Eq for Port {}
impl Drop for Port {
    fn drop(&mut self) {
        let Port { port, beschreibung, sender, .. } = self;
        debug!("dropped {:?} {:?} ", beschreibung, port);
        // Schicke Werte als Tupel, um keine Probleme mit dem Drop-Handler zu bekommen.
        // (Ein Klon würde bei send-Fehler eine Endlos-Schleife erzeugen)
        if let Err(err) = sender.send((beschreibung.clone(), port.clone())) {
            debug!("send error while dropping: {}", err)
        }
    }
}
impl Port {
    pub(super) fn neu(
        pcf8574: Arc<Mutex<Pcf8574>>,
        beschreibung: Beschreibung,
        port: u3,
        sender: Sender<(Beschreibung, u3)>,
    ) -> Self {
        Port { pcf8574, port, beschreibung, sender }
    }

    #[inline(always)]
    pub fn beschreibung(&self) -> &Beschreibung {
        &self.beschreibung
    }

    #[inline(always)]
    pub fn port(&self) -> u3 {
        self.port
    }

    /// Konfiguriere den Port für Output.
    pub fn into_output(self, level: Level) -> Result<OutputPort, Fehler> {
        {
            let beschreibung = self.beschreibung().clone();
            let pcf8574 = &mut *self
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::PoisonFehler(beschreibung))?;
            pcf8574.write_port(self.port, level)?;
        }
        Ok(OutputPort(self))
    }

    /// Konfiguriere den Port für Input.
    pub fn into_input(self) -> Result<InputPort, Fehler> {
        {
            let beschreibung = self.beschreibung().clone();
            let pcf8574 = &mut *self
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
            pcf8574.port_as_input::<fn(Level)>(self.port, Trigger::Disabled, None)?;
        }
        Ok(InputPort(self))
    }
}

// Ein Port eines Pcf8574, konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPort(Port);

impl OutputPort {
    #[inline(always)]
    pub fn adresse(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    #[inline(always)]
    pub fn port(&self) -> u3 {
        self.0.port()
    }

    pub fn write(&mut self, level: Level) -> Result<(), Fehler> {
        {
            let beschreibung = self.adresse().clone();
            let pcf8574 = &mut *self
                .0
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
            pcf8574.write_port(self.0.port, level)?;
        }
        Ok(())
    }

    pub fn is_set_high(&mut self) -> Result<bool, Fehler> {
        let beschreibung = self.adresse().clone();
        let pcf8574 = &mut *self
            .0
            .pcf8574
            .lock()
            .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
        Ok(pcf8574.ports[usize::from(self.port())] == Modus::High)
    }

    pub fn is_set_low(&mut self) -> Result<bool, Fehler> {
        let beschreibung = self.adresse().clone();
        let pcf8574 = &mut *self
            .0
            .pcf8574
            .lock()
            .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
        Ok(pcf8574.ports[usize::from(self.port())] == Modus::Low)
    }

    pub fn toggle(&mut self) -> Result<(), Fehler> {
        let level = {
            let beschreibung = self.adresse().clone();
            let pcf8574 = &*self
                .0
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
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
    #[inline(always)]
    pub fn adresse(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    #[inline(always)]
    pub fn port(&self) -> u3 {
        self.0.port()
    }

    pub fn read(&self) -> Result<Level, Fehler> {
        let values = {
            let beschreibung = self.adresse().clone();
            let pcf8574 = &mut *self
                .0
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
            pcf8574.read()?
        };
        if let Some(value) = values[usize::from(self.0.port)] {
            Ok(value)
        } else {
            error!("{:?} war nicht als input korrigiert!", self);
            // war nicht als Input konfiguriert -> erneut konfigurieren und neu versuchen
            {
                let beschreibung = self.adresse().clone();
                let pcf8574 = &mut *self
                    .0
                    .pcf8574
                    .lock()
                    .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
                pcf8574.port_as_input::<fn(Level)>(self.0.port, Trigger::Disabled, None)?;
            }
            self.read()
        }
    }

    /// Aktuell konfigurierter Interrupt Pin.
    pub(super) fn interrupt_pin(&self) -> Result<Option<u8>, Fehler> {
        let beschreibung = self.adresse().clone();
        let pcf8574 = &mut *self
            .0
            .pcf8574
            .lock()
            .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
        Ok(pcf8574.interrupt.as_ref().map(input::Pin::pin))
    }

    /// Assoziiere den angeschlossenen InterruptPin für den Pcf8574.
    /// Rückgabewert ist ein evtl. vorher konfigurierter InterruptPin.
    /// Interrupt-Callbacks werden nicht zurückgesetzt!
    pub fn set_interrupt_pin(
        &mut self,
        mut interrupt: input::Pin,
    ) -> Result<Option<input::Pin>, Fehler> {
        let mut previous = {
            // set up callback.
            let beschreibung = self.adresse().clone();
            let pcf8574 = &mut *self
                .0
                .pcf8574
                .lock()
                .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
            let mut last = pcf8574.read()?;
            let arc_clone = self.0.pcf8574.clone();
            interrupt.set_async_interrupt(Trigger::FallingEdge, move |_level| {
                let mut pcf8574 = match arc_clone.lock() {
                    Ok(guard) => guard,
                    Err(_fehler) => {
                        error!(
                            "Poison error der pcf8574-Mutex bei der Reaktion auf einen interrupt!"
                        );
                        return;
                    }
                };
                let current = match pcf8574.read() {
                    Ok(current) => current,
                    Err(fehler) => {
                        error!(
                            "Fehler beim lesen des Pcf8574 bei der Reaktion auf einen interrupt: {:?}",
                            fehler
                        );
                        return;
                    }
                };
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
            }).map_err(|fehler| match fehler {
                input::Fehler::Gpio {pin:_, fehler} => Fehler::Gpio {beschreibung: pcf8574.beschreibung(), fehler},
            })?;
            std::mem::replace(&mut pcf8574.interrupt, Some(interrupt))
        };
        // clear interrupt on previous pin.
        let _ = previous.as_mut().map(input::Pin::clear_async_interrupt);
        Ok(previous)
    }

    /// Configures an asynchronous interrupt trigger, which executes the callback on a separate
    /// thread when the interrupt is triggered.
    ///
    /// The callback closure or function pointer is called with a single Level argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared when
    /// set_async_interrupt is called, or when InputPin goes out of scope.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline(always)]
    pub fn set_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Fehler> {
        let port = self.port();
        let beschreibung = self.adresse().clone();
        let pcf8574 = &mut *self
            .0
            .pcf8574
            .lock()
            .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
        pcf8574.port_as_input(port, trigger, Some(callback))
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    #[inline(always)]
    pub fn clear_async_interrupt(&mut self) -> Result<(), Fehler> {
        let port = self.port();
        let beschreibung = self.adresse().clone();
        let pcf8574 = &mut *self
            .0
            .pcf8574
            .lock()
            .map_err(|_poison_error| Fehler::KeinInterruptPin(beschreibung))?;
        pcf8574.port_as_input::<fn(Level)>(port, Trigger::Disabled, None)
    }
}

// TODO genauere Eingrenzung auf einzelne Methoden
#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub enum Fehler {
    I2c { beschreibung: Beschreibung, fehler: i2c::Error },
    Gpio { beschreibung: Beschreibung, fehler: gpio::Error },
    PoisonFehler(Beschreibung),
    KeinInterruptPin(Beschreibung),
}
