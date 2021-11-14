//! Pcf8574, gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::{
    fmt::Debug,
    mem,
    sync::{Arc, Mutex, RwLock},
};

use log::{debug, error};
use num_x::{u3, u7};
use once_cell::sync::Lazy;
use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        pin::input,
        {level::Level, trigger::Trigger},
    },
    rppal::{
        gpio::{self, Gpio, Pin},
        i2c::{self, I2c},
    },
};

#[derive(Debug)]
struct I2cMitPins {
    i2c: I2c,
    sda: Pin,
    scl: Pin,
}

#[derive(Debug)]
pub enum InitFehler {
    I2c { i2c_bus: I2cBus, fehler: i2c::Error },
    Gpio { i2c_bus: I2cBus, fehler: gpio::Error },
}

impl I2cMitPins {
    fn neu(i2c_bus: I2cBus) -> Result<I2cMitPins, InitFehler> {
        let i2c = i2c_bus.reserviere().map_err(|fehler| InitFehler::I2c { i2c_bus, fehler })?;
        let (sda, scl) = i2c_bus.sda_scl();
        let konvertiere_gpio_fehler = |fehler| InitFehler::Gpio { i2c_bus, fehler };
        let gpio = Gpio::new().map_err(&konvertiere_gpio_fehler)?;
        let sda = gpio.get(sda).map_err(&konvertiere_gpio_fehler)?;
        let scl = gpio.get(scl).map_err(konvertiere_gpio_fehler)?;
        Ok(I2cMitPins { i2c, sda, scl })
    }

    fn erstelle_arc_und_pcf8574_state(
        i2c_bus: I2cBus,
    ) -> Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler> {
        I2cMitPins::neu(i2c_bus).map(|i2c| {
            let arc = Arc::new(Mutex::new(i2c));
            (arc.clone(), RwLock::new(Pcf8574State::neu(i2c_bus, arc)))
        })
    }
}

#[derive(Debug)]
struct Pcf8574PortState {
    pcf8574: Arc<Mutex<Pcf8574>>,
    port0: Option<Port>,
    port1: Option<Port>,
    port2: Option<Port>,
    port3: Option<Port>,
    port4: Option<Port>,
    port5: Option<Port>,
    port6: Option<Port>,
    port7: Option<Port>,
}

impl Pcf8574PortState {
    fn neu(
        i2c_bus: I2cBus,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        i2c: Arc<Mutex<I2cMitPins>>,
    ) -> Pcf8574PortState {
        let pcf8574 = Arc::new(Mutex::new(Pcf8574::neu(i2c_bus, a0, a1, a2, variante, i2c)));
        let beschreibung = Beschreibung { i2c_bus, a0, a1, a2, variante };
        Pcf8574PortState {
            port0: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(0))),
            port1: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(1))),
            port2: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(2))),
            port3: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(3))),
            port4: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(4))),
            port5: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(5))),
            port6: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(6))),
            port7: Some(Port::neu(pcf8574.clone(), beschreibung, u3::new(7))),
            pcf8574,
        }
    }
}

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
    ( $inner_macro:ident $($k:ident $l:ident $m:ident [$($n:ident),+])+ $(: $value:ident)?) => {
         $inner_macro! { $($($k $l $m $n),+),+ $(: $value)? }
    };
}
macro_rules! llln_to_hhha {
    ($inner_macro:ident $(: $value:expr)?) => {
        matrix! {$inner_macro  [l,h] [l,h] [l,h] [n,a] $(: $value)?}
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
        Variante::Normal
    };
    (a) => {
        Variante::A
    };
}
macro_rules! pcf8574_state_struct {
    ( $($a0:ident $a1:ident $a2:ident $variante:ident),* ) => {
        paste! {
            #[doc="Singleton für Zugriff auf raspberry pi Anschlüsse."]
            #[derive(Debug)]
            struct Pcf8574State {
                $(
                    [<$a0 $a1 $a2 $variante>]: Pcf8574PortState,
                )*
            }
            impl Pcf8574State {
                fn neu(i2c_bus: I2cBus, i2c: Arc<Mutex<I2cMitPins>>) -> Pcf8574State {
                    Pcf8574State {
                        $(
                            [<$a0 $a1 $a2 $variante>]: Pcf8574PortState::neu(
                                i2c_bus,
                                level!($a0),
                                level!($a1),
                                level!($a2),
                                variante!($variante),
                                i2c.clone()
                            ),
                        )*
                    }
                }

                fn reserviere_pcf8574_port(&mut self, beschreibung: Beschreibung, port: u3)
                    -> Result<Port, InVerwendung>
                {
                    match beschreibung {
                        $(
                            Beschreibung {
                                i2c_bus,
                                a0: level!($a0),
                                a1: level!($a1),
                                a2: level!($a2),
                                variante: variante!($variante),
                            } => {
                                debug!("reserviere pcf8574 {:?}-{}", beschreibung, port);
                                let port_opt = match u8::from(port) {
                                    0 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port0, None),
                                    1 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port1, None),
                                    2 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port2, None),
                                    3 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port3, None),
                                    4 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port4, None),
                                    5 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port5, None),
                                    6 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port6, None),
                                    7 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port7, None),
                                    _ => None,
                                };
                                port_opt.ok_or(InVerwendung {beschreibung, port})
                            }
                        ),*
                    }
                }

                fn rückgabe_pcf8574_port(&mut self, port: Port)  {
                    match port.beschreibung() {
                        $(
                            Beschreibung {
                                i2c_bus,
                                a0: level!($a0),
                                a1: level!($a1),
                                a2: level!($a2),
                                variante: variante!($variante),
                            } => {
                                debug!("rückgabe {:?}", port);
                                let port_u8 = u8::from(port.port());
                                let s_port = Some(port);
                                let port_opt = match port_u8 {
                                    0 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port0, s_port),
                                    1 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port1, s_port),
                                    2 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port2, s_port),
                                    3 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port3, s_port),
                                    4 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port4, s_port),
                                    5 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port5, s_port),
                                    6 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port6, s_port),
                                    7 => mem::replace(&mut self.[<$a0 $a1 $a2 $variante>].port7, s_port),
                                    _ => None,
                                };
                                if let Some(bisher) = port_opt {
                                    error!("Bereits verfügbaren Pcf8574-Port ersetzt: {:?}", bisher)
                                }
                            }
                        ),*
                    }
                }
            }
        }
    };
}
llln_to_hhha! {pcf8574_state_struct}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// Singleton für Zugriff auf raspberry pi Anschlüsse.
#[derive(Debug)]
struct I2cState {
    i2c_0_1: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
    // i2c_2: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
    i2c_3: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
    i2c_4: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
    i2c_5: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
    i2c_6: Result<(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), InitFehler>,
}

#[derive(Debug, Clone, Copy)]
pub struct InVerwendung {
    pub beschreibung: Beschreibung,
    pub port: u3,
}

#[derive(Debug)]
pub enum ReserviereFehler<'t> {
    Init(&'t InitFehler),
    InVerwendung { i2c_bus: I2cBus, fehler: InVerwendung },
}

impl<'t> From<&'t InitFehler> for ReserviereFehler<'t> {
    fn from(fehler: &'t InitFehler) -> Self {
        ReserviereFehler::Init(fehler)
    }
}

impl I2cState {
    fn neu() -> I2cState {
        let i2c_0_1 = I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c0_1);
        // let i2c_2 =
        //     I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c2);
        let i2c_3 = I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c3);
        let i2c_4 = I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c4);
        let i2c_5 = I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c5);
        let i2c_6 = I2cMitPins::erstelle_arc_und_pcf8574_state(I2cBus::I2c6);
        I2cState { i2c_0_1, i2c_3, i2c_4, i2c_5, i2c_6 }
    }

    fn i2c_bus(
        &self,
        i2c_bus: I2cBus,
    ) -> Result<&(Arc<Mutex<I2cMitPins>>, RwLock<Pcf8574State>), &InitFehler> {
        let I2cState { i2c_0_1, i2c_3, i2c_4, i2c_5, i2c_6 } = self;
        let i2c = match i2c_bus {
            I2cBus::I2c0_1 => i2c_0_1,
            I2cBus::I2c3 => i2c_3,
            I2cBus::I2c4 => i2c_4,
            I2cBus::I2c5 => i2c_5,
            I2cBus::I2c6 => i2c_6,
        };
        i2c.as_ref()
    }

    fn reserviere_pcf8574_port(
        &self,
        i2c_bus: I2cBus,
        beschreibung: Beschreibung,
        port: u3,
    ) -> Result<Port, ReserviereFehler<'_>> {
        let (_i2c, pcf8574_state_lock) = self.i2c_bus(i2c_bus)?;
        let mut pcf8574_state = pcf8574_state_lock.write().unwrap_or_else(|poison_error| {
            error!("Pcf8574State-RwLock poisoned: {:?}", poison_error);
            poison_error.into_inner()
        });
        pcf8574_state
            .reserviere_pcf8574_port(beschreibung, port)
            .map_err(|fehler| ReserviereFehler::InVerwendung { i2c_bus, fehler })
    }

    fn rückgabe_pcf8574_port(&self, port: Port) -> Result<(), &InitFehler> {
        let pcf8574 = port.pcf8574.lock().unwrap_or_else(|poison_error| {
            error!("Pcf8574-Port Mutex bei rückgabe poisoned: {:?}", poison_error);
            poison_error.into_inner()
        });
        let i2c_bus = pcf8574.i2c_bus;
        drop(pcf8574);
        let (_i2c, pcf8574_state_lock) = self.i2c_bus(i2c_bus)?;
        let mut pcf8574_state = pcf8574_state_lock.write().unwrap_or_else(|poison_error| {
            error!("Pcf8574State-RwLock poisoned: {:?}", poison_error);
            poison_error.into_inner()
        });
        pcf8574_state.rückgabe_pcf8574_port(port);
        Ok(())
    }
}

static I2C: Lazy<I2cState> = Lazy::new(I2cState::neu);

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
    i2c_bus: I2cBus,
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    ports: [Modus; 8],
    interrupt: Option<input::Pin>,
    i2c: Arc<Mutex<I2cMitPins>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Beschreibung {
    pub i2c_bus: I2cBus,
    pub a0: Level,
    pub a1: Level,
    pub a2: Level,
    pub variante: Variante,
}

impl Pcf8574 {
    fn beschreibung(&self) -> Beschreibung {
        let Pcf8574 { i2c_bus, a0, a1, a2, variante, .. } = self;
        Beschreibung { i2c_bus: *i2c_bus, a0: *a0, a1: *a1, a2: *a2, variante: *variante }
    }

    fn neu(
        i2c_bus: I2cBus,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        i2c: Arc<Mutex<I2cMitPins>>,
    ) -> Self {
        Pcf8574 {
            i2c_bus,
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
        if let Ok(mut i2c_with_pins) = self.i2c.lock() {
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
        } else {
            error!("I2C-Mutex poisoned!");
            Err(Fehler::PoisonFehler(beschreibung))
        }
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
        let mut i2c_with_pins = if let Ok(i2c_with_pins) = self.i2c.lock() {
            i2c_with_pins
        } else {
            error!("I2C-Mutex poisoned!");
            return Err(Fehler::PoisonFehler(beschreibung));
        };
        let map_fehler = |fehler| Fehler::I2c { beschreibung: beschreibung.clone(), fehler };
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
}
impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.port == other.port && self.beschreibung == other.beschreibung
    }
}
impl Eq for Port {}
impl Drop for Port {
    fn drop(&mut self) {
        let port_replacement =
            Port::neu(self.pcf8574.clone(), self.beschreibung().clone(), self.port());
        if let Err(fehler) = I2C.rückgabe_pcf8574_port(port_replacement) {
            error!("Fehler bei Rückgabe eines Pcf8574-Ports: {:?}", fehler)
        }
    }
}
impl Port {
    pub fn reserviere<'t>(
        i2c_bus: I2cBus,
        beschreibung: Beschreibung,
        port: u3,
    ) -> Result<Port, ReserviereFehler<'t>> {
        I2C.reserviere_pcf8574_port(i2c_bus, beschreibung, port)
    }

    fn neu(pcf8574: Arc<Mutex<Pcf8574>>, beschreibung: Beschreibung, port: u3) -> Self {
        Port { pcf8574, port, beschreibung }
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
        callback: impl Fn(Level) + Send + Sync + 'static,
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

#[cfg(test)]
mod test;
