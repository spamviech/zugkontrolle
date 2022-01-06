//! Pcf8574, gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::{array, collections::HashMap, fmt::Debug, hash::Hash, sync::Arc};

use itertools::iproduct;
use log::{debug, error};
use parking_lot::{Mutex, RwLock};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        pin::{self, input, Pin},
        {level::Level, trigger::Trigger},
    },
    rppal::{
        gpio,
        i2c::{self, I2c},
    },
    ux::{u3, u7},
};

pub use crate::args::I2cSettings;

#[derive(Debug)]
struct I2cMitPins {
    i2c: I2c,
    #[allow(dead_code)]
    i2c_bus: I2cBus,
    #[allow(dead_code)]
    sda: Pin,
    #[allow(dead_code)]
    scl: Pin,
}

#[derive(Debug)]
pub enum InitFehler {
    I2c { i2c_bus: I2cBus, fehler: i2c::Error },
    Pin { i2c_bus: I2cBus, fehler: pin::ReservierenFehler },
}

#[derive(Debug, Clone, Copy)]
pub struct Deaktiviert(pub I2cBus);

impl I2cMitPins {
    fn neu(pin_status: &mut pin::Lager, i2c_bus: I2cBus) -> Result<I2cMitPins, InitFehler> {
        let i2c = i2c_bus.reserviere().map_err(|fehler| InitFehler::I2c { i2c_bus, fehler })?;
        let (sda, scl) = i2c_bus.sda_scl();
        let konvertiere_pin_fehler = |fehler| InitFehler::Pin { i2c_bus, fehler };
        let sda = pin_status.reserviere_pin(sda).map_err(&konvertiere_pin_fehler)?;
        let scl = pin_status.reserviere_pin(scl).map_err(konvertiere_pin_fehler)?;
        Ok(I2cMitPins { i2c, i2c_bus, sda, scl })
    }
}

impl I2cSettings {
    fn aktiviert(&self, i2c_bus: I2cBus) -> bool {
        match i2c_bus {
            I2cBus::I2c0_1 => self.i2c0_1,
            // I2cBus::I2c2 => self.i2c2,
            I2cBus::I2c3 => self.i2c3,
            I2cBus::I2c4 => self.i2c4,
            I2cBus::I2c5 => self.i2c5,
            I2cBus::I2c6 => self.i2c6,
        }
    }
}

fn alle_i2c_bus() -> array::IntoIter<I2cBus, 5> {
    [
        I2cBus::I2c0_1,
        // I2cBus::I2c2,
        I2cBus::I2c3,
        I2cBus::I2c4,
        I2cBus::I2c5,
        I2cBus::I2c6,
    ]
    .into_iter()
}

fn alle_level() -> array::IntoIter<Level, 2> {
    [Level::Low, Level::High].into_iter()
}

fn alle_varianten() -> array::IntoIter<Variante, 2> {
    [Variante::Normal, Variante::A].into_iter()
}

#[derive(Debug)]
pub struct Lager(Arc<RwLock<HashMap<(Beschreibung, u3), Port>>>);

impl Lager {
    pub fn neu(pin_status: &mut pin::Lager, settings: I2cSettings) -> Result<Lager, InitFehler> {
        let arc = Arc::new(RwLock::new(HashMap::new()));
        {
            let mut map = arc.write();
            for i2c_bus in alle_i2c_bus() {
                if !settings.aktiviert(i2c_bus) {
                    continue;
                }
                let i2c = Arc::new(Mutex::new(I2cMitPins::neu(pin_status, i2c_bus)?));
                let beschreibungen =
                    iproduct!(alle_level(), alle_level(), alle_level(), alle_varianten());
                for (a0, a1, a2, variante) in beschreibungen {
                    let beschreibung = Beschreibung { i2c_bus, a0, a1, a2, variante };
                    let pcf8574 = Arc::new(Mutex::new(Pcf8574::neu(beschreibung, i2c.clone())));
                    for port_num in u3::alle_werte() {
                        let port_struct =
                            Port::neu(pcf8574.clone(), Lager(arc.clone()), beschreibung, port_num);
                        if let Some(bisher) = map.insert((beschreibung, port_num), port_struct) {
                            error!("Pcf8574-Port doppelt erstellt: {:?}", bisher)
                        }
                    }
                }
            }
        }
        Ok(Lager(arc))
    }

    pub fn reserviere_pcf8574_port(
        &mut self,
        beschreibung: Beschreibung,
        port: u3,
    ) -> Result<Port, InVerwendung> {
        debug!("reserviere pcf8574 {:?}-{}", beschreibung, port);
        self.0.write().remove(&(beschreibung, port)).ok_or(InVerwendung { beschreibung, port })
    }

    pub fn rückgabe_pcf8574_port(&mut self, port: Port) {
        debug!("rückgabe {:?}", port);
        let port_opt = self.0.write().insert((port.beschreibung().clone(), port.port()), port);
        if let Some(bisher) = port_opt {
            error!("Bereits verfügbaren Pcf8574-Port ersetzt: {:?}", bisher)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum I2cBus {
    I2c0_1,
    // I2c2,
    I2c3,
    I2c4,
    I2c5,
    I2c6,
}

impl I2cBus {
    fn reserviere(&self) -> i2c::Result<I2c> {
        match self {
            I2cBus::I2c0_1 => I2c::with_bus(1).or_else(|_| I2c::with_bus(0)),
            // I2cBus::I2c2 => I2c::with_bus(2),
            I2cBus::I2c3 => I2c::with_bus(3),
            I2cBus::I2c4 => I2c::with_bus(4),
            I2cBus::I2c5 => I2c::with_bus(5),
            I2cBus::I2c6 => I2c::with_bus(6),
        }
    }

    fn sda_scl(&self) -> (u8, u8) {
        match self {
            I2cBus::I2c0_1 => (2, 3),
            // I2cBus::I2c2 => unreachable!(),
            I2cBus::I2c3 => (4, 5),
            I2cBus::I2c4 => (8, 9),
            I2cBus::I2c5 => (12, 13),
            I2cBus::I2c6 => (22, 23),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InVerwendung {
    pub beschreibung: Beschreibung,
    pub port: u3,
}

#[derive(Debug, Clone, Copy, zugkontrolle_derive::From)]
#[allow(variant_size_differences)]
pub enum ReservierenFehler {
    Deaktiviert(Deaktiviert),
    InVerwendung(InVerwendung),
}

pub(super) enum Modus {
    Input { trigger: Trigger, callback: Option<Arc<dyn Fn(Level) + Send + Sync + 'static>> },
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
    beschreibung: Beschreibung,
    ports: [Modus; 8],
    interrupt: Option<input::Pin>,
    i2c: Arc<Mutex<I2cMitPins>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Beschreibung {
    pub i2c_bus: I2cBus,
    pub a0: Level,
    pub a1: Level,
    pub a2: Level,
    pub variante: Variante,
}

impl Pcf8574 {
    fn beschreibung(&self) -> &Beschreibung {
        &self.beschreibung
    }

    fn neu(beschreibung: Beschreibung, i2c: Arc<Mutex<I2cMitPins>>) -> Self {
        Pcf8574 {
            beschreibung,
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
        let Pcf8574 { beschreibung: Beschreibung { i2c_bus: _, a0, a1, a2, variante }, .. } = self;
        let mut adresse = u7::try_from(match variante {
            Variante::Normal => 0x20,
            Variante::A => 0x38,
        })
        .unwrap();
        if let Level::High = a0 {
            adresse = adresse + u7::try_from(0b001).unwrap();
        }
        if let Level::High = a1 {
            adresse = adresse + u7::try_from(0b010).unwrap();
        }
        if let Level::High = a2 {
            adresse = adresse + u7::try_from(0b100).unwrap();
        }
        adresse
    }

    /// Lese von einem Pcf8574.
    /// Nur als Input konfigurierte Ports werden als Some-Wert zurückgegeben.
    ///
    /// Bei Interrupt-basiertem lesen sollten alle Port gleichzeitig gelesen werden!
    fn read(&self) -> Result<[Option<Level>; 8], Fehler> {
        let beschreibung = self.beschreibung();
        let map_fehler = |fehler| Fehler::I2c { beschreibung: *beschreibung, fehler };
        let mut i2c_with_pins = self.i2c.lock();
        i2c_with_pins.i2c.set_slave_address(self.i2c_adresse().into()).map_err(&map_fehler)?;
        let mut buf = [0; 1];
        let bytes_read = i2c_with_pins.i2c.read(&mut buf).map_err(map_fehler)?;
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
    }

    /// Konvertiere einen Port als Input.
    fn port_as_input<C: Fn(Level) + Send + Sync + 'static>(
        &mut self,
        port: u3,
        trigger: Trigger,
        callback: Option<C>,
    ) -> Result<(), Fehler> {
        self.write_port(port, Level::High)?;
        // type annotations need, so extra let binding required
        let callback: Option<Arc<dyn Fn(Level) + Send + Sync + 'static>> = match callback {
            Some(c) => Some(Arc::new(c)),
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
        let mut i2c_with_pins = self.i2c.lock();
        let map_fehler = |fehler| Fehler::I2c { beschreibung: *beschreibung, fehler };
        i2c_with_pins.i2c.set_slave_address(self.i2c_adresse().into()).map_err(&map_fehler)?;
        let mut wert = 0;
        for (port, modus) in self.ports.iter().enumerate() {
            wert |= match modus {
                Modus::Input { .. } | Modus::High => 2u8.pow(port as u32),
                Modus::Low => 0,
            };
        }
        let buf = [wert; 1];
        let bytes_written = i2c_with_pins.i2c.write(&buf).map_err(map_fehler)?;
        if bytes_written != 1 {
            error!("bytes_written = {} != 1", bytes_written)
        }
        Ok(())
    }
}
impl PartialEq for Pcf8574 {
    fn eq(&self, other: &Self) -> bool {
        self.beschreibung == other.beschreibung
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
    lager: Lager,
    beschreibung: Beschreibung,
    port: u3,
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.port == other.port && self.beschreibung == other.beschreibung
    }
}
impl Eq for Port {}

impl Drop for Port {
    fn drop(&mut self) {
        debug!("drop {:?}", self);
        let port_ersatz = Port::neu(
            self.pcf8574.clone(),
            Lager(self.lager.0.clone()),
            *self.beschreibung(),
            self.port(),
        );
        self.lager.rückgabe_pcf8574_port(port_ersatz);
    }
}

impl Port {
    fn neu(
        pcf8574: Arc<Mutex<Pcf8574>>,
        lager: Lager,
        beschreibung: Beschreibung,
        port: u3,
    ) -> Self {
        Port { pcf8574, lager, beschreibung, port }
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
        self.pcf8574.lock().write_port(self.port, level)?;
        Ok(OutputPort(self))
    }

    /// Konfiguriere den Port für Input.
    pub fn into_input(self) -> Result<InputPort, Fehler> {
        self.pcf8574.lock().port_as_input::<fn(Level)>(self.port, Trigger::Disabled, None)?;
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
        self.0.pcf8574.lock().write_port(self.0.port, level)
    }

    pub fn is_set_high(&mut self) -> Result<bool, Fehler> {
        Ok(self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::High)
    }

    pub fn is_set_low(&mut self) -> Result<bool, Fehler> {
        Ok(self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::Low)
    }

    pub fn toggle(&mut self) -> Result<(), Fehler> {
        let level = {
            let modus = &self.0.pcf8574.lock().ports[usize::from(self.port())];
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
        let values = self.0.pcf8574.lock().read()?;
        if let Some(value) = values[usize::from(self.0.port)] {
            Ok(value)
        } else {
            error!("{:?} war nicht als input korrigiert!", self);
            // war nicht als Input konfiguriert -> erneut konfigurieren und neu versuchen
            self.0.pcf8574.lock().port_as_input::<fn(Level)>(
                self.0.port,
                Trigger::Disabled,
                None,
            )?;
            self.read()
        }
    }

    /// Aktuell konfigurierter Interrupt Pin.
    pub(super) fn interrupt_pin(&self) -> Result<Option<u8>, Fehler> {
        Ok(self.0.pcf8574.lock().interrupt.as_ref().map(input::Pin::pin))
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
            let pcf8574 = &mut *self.0.pcf8574.lock();
            let mut last = pcf8574.read()?;
            let arc_clone = self.0.pcf8574.clone();
            interrupt.set_async_interrupt(Trigger::FallingEdge, move |_level| {
                let mut pcf8574 =  arc_clone.lock();
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
                input::Fehler::Gpio {pin:_, fehler} => Fehler::Gpio {beschreibung: *pcf8574.beschreibung(), fehler},
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
        callback: impl Fn(Level) + Send + Sync + 'static,
    ) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_as_input(port, trigger, Some(callback))
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    #[inline(always)]
    pub fn clear_async_interrupt(&mut self) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_as_input::<fn(Level)>(port, Trigger::Disabled, None)
    }
}

// TODO genauere Eingrenzung auf einzelne Methoden
#[derive(Debug)]
pub enum Fehler {
    I2c { beschreibung: Beschreibung, fehler: i2c::Error },
    Gpio { beschreibung: Beschreibung, fehler: gpio::Error },
    KeinInterruptPin(Beschreibung),
}

#[cfg(test)]
mod test;
