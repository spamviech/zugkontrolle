//! [Pcf8574], gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::{
    array,
    collections::HashMap,
    fmt::Debug,
    fmt::{self, Display, Formatter},
    hash::Hash,
    sync::Arc,
};

use itertools::iproduct;
use log::{debug, error};
use parking_lot::{Mutex, RwLock};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        pin::{self, input, Pin},
        {level::Level, trigger::Trigger},
    },
    eingeschränkt::{kleiner_128, kleiner_8},
    rppal::{
        gpio,
        i2c::{self, I2c},
    },
};

pub use crate::argumente::I2cSettings;

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

/// Fehler beim Initialisieren eines [Lager].
#[derive(Debug)]
pub enum InitFehler {
    /// Fehler bei einem I2C-Channel.
    I2c {
        /// Der betroffene [I2cBus].
        i2c_bus: I2cBus,
        /// Der aufgetretene Fehler.
        fehler: i2c::Error,
    },
    /// Nicht verfügbarer Pin für den konfigurierten I2C-Channel.
    Pin {
        /// Der betroffene [I2cBus].
        i2c_bus: I2cBus,
        /// Der aufgetretene Fehler.
        fehler: pin::ReservierenFehler,
    },
}

/// Ein I2C-Bus ist deaktiviert.
#[derive(Debug, Clone, Copy)]
pub struct Deaktiviert(pub I2cBus);

impl I2cMitPins {
    fn neu(lager: &mut pin::Lager, i2c_bus: I2cBus) -> Result<I2cMitPins, InitFehler> {
        let i2c = i2c_bus.reserviere().map_err(|fehler| InitFehler::I2c { i2c_bus, fehler })?;
        let (sda, scl) = i2c_bus.sda_scl();
        let konvertiere_pin_fehler = |fehler| InitFehler::Pin { i2c_bus, fehler };
        let sda = lager.reserviere_pin(sda).map_err(&konvertiere_pin_fehler)?;
        let scl = lager.reserviere_pin(scl).map_err(konvertiere_pin_fehler)?;
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

/// Noch verfügbare Pcf8574-[Port]s.
#[derive(Debug)]
pub struct Lager(Arc<RwLock<HashMap<(Beschreibung, kleiner_8), Port>>>);

impl Lager {
    /// Erstelle ein neues [Lager].
    pub fn neu(lager: &mut pin::Lager, settings: I2cSettings) -> Result<Lager, InitFehler> {
        let arc = Arc::new(RwLock::new(HashMap::new()));
        {
            let mut map = arc.write();
            for i2c_bus in alle_i2c_bus() {
                if !settings.aktiviert(i2c_bus) {
                    continue;
                }
                let i2c = Arc::new(Mutex::new(I2cMitPins::neu(lager, i2c_bus)?));
                let beschreibungen =
                    iproduct!(alle_level(), alle_level(), alle_level(), alle_varianten());
                for (a0, a1, a2, variante) in beschreibungen {
                    let beschreibung = Beschreibung { i2c_bus, a0, a1, a2, variante };
                    let pcf8574 = Arc::new(Mutex::new(Pcf8574::neu(beschreibung, i2c.clone())));
                    for port_num in kleiner_8::alle_werte() {
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

    /// Reserviere einen Pcf8574-[Port].
    pub fn reserviere_pcf8574_port(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
    ) -> Result<Port, InVerwendung> {
        debug!("reserviere pcf8574 {:?}-{}", beschreibung, port);
        self.0.write().remove(&(beschreibung, port)).ok_or(InVerwendung { beschreibung, port })
    }

    /// Gebe einen Pcf8574-[Port] zurück, damit er wieder verwendet werden kann.
    ///
    /// Wird vom [Drop]-Handler des [Port]s aufgerufen.
    pub fn rückgabe_pcf8574_port(&mut self, port: Port) {
        debug!("rückgabe {:?}", port);
        let port_opt = self.0.write().insert((port.beschreibung().clone(), port.port()), port);
        if let Some(bisher) = port_opt {
            error!("Bereits verfügbaren Pcf8574-Port ersetzt: {:?}", bisher)
        }
    }
}

/// Ein I2cBus.
///
/// Vor Raspberry Pi 4 wird nur [I2c0_1](I2cBus::I2c0_1) als Hardware-I2C unterstützt.
///
/// Anmerkung: Es ist möglich Software-I2C auf normalen Gpio-Pins zu aktivieren.
/// Beachte dazu den Abschnitt "Aktivieren zusätzlicher I2C-Busse" in der `README.md`.
/// ACHTUNG: Dabei werden nur die Standard-Pins, die auch bei Pi4 verwendet werden, unterstützt.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum I2cBus {
    /// I2C-Bus auf den GPIO-Pins 2 (SDA) und 3 (SCL), physisch 3 und 5.
    ///
    /// Auf dem Raspberry Pi B Rev 1 sind sie mit I2C0 verbunden,
    /// bei allen anderen Raspberry Pi Versionen mit I2C1.
    ///
    /// Der jeweils andere ist für Kommunikation mit dem EEPROM z.B. eines Hats
    /// und nicht für allgemeine Nutzung vorgesehen.
    I2c0_1,
    // /// I2C-Bus wird u.a. für HDMI und Kamera verwendet
    // /// und ist nicht für allgemeine Nutzung vorgesehen.
    // I2c2,
    /// I2C-Bus auf den GPIO-Pins 4 (SDA) und 5 (SCL), physisch 7 und 29.
    I2c3,
    /// I2C-Bus auf den GPIO-Pins 8 (SDA) und 9 (SCL), physisch 24 und 21.
    I2c4,
    /// I2C-Bus auf den GPIO-Pins 12 (SDA) und 13 (SCL), physisch 32 und 33.
    I2c5,
    /// I2C-Bus auf den GPIO-Pins 22 (SDA) und 23 (SCL), physisch 15 und 16.
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

/// Der [Port] wird bereits verwendet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InVerwendung {
    /// [Beschreibung] des [Pcf8574]s.
    pub beschreibung: Beschreibung,
    /// Port des [Pcf8574].
    pub port: kleiner_8,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Modus::Input { trigger, callback } => f
                .debug_struct("Input")
                .field("trigger", trigger)
                .field("callback", &callback.as_ref().map(|_| "<function>"))
                .finish(),
            Modus::High => f.debug_struct("High").finish(),
            Modus::Low => f.debug_struct("Low").finish(),
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

/// Beschreibung eines [Pcf8574].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Beschreibung {
    /// I2CBus, über den das [Pcf8574] angeschlossen ist.
    pub i2c_bus: I2cBus,
    /// Anliegendes [Level] an das `A0` Adress-Bit.
    pub a0: Level,
    /// Anliegendes [Level] an das `A1` Adress-Bit.
    pub a1: Level,
    /// Anliegendes [Level] an das `A2` Adress-Bit.
    pub a2: Level,
    /// Variante des [Pcf8574], beeinflusst die I2C-Adresse.
    pub variante: Variante,
}

impl PartialEq for Pcf8574 {
    fn eq(&self, other: &Self) -> bool {
        self.beschreibung == other.beschreibung
    }
}
impl Eq for Pcf8574 {}

fn display_bus_str(i2c_bus: &I2cBus) -> &str {
    match i2c_bus {
        I2cBus::I2c0_1 => "01",
        // I2cBus::I2c2 => " 2",
        I2cBus::I2c3 => " 3",
        I2cBus::I2c4 => " 4",
        I2cBus::I2c5 => " 5",
        I2cBus::I2c6 => " 6",
    }
}

fn display_level_str(level: &Level) -> &str {
    match level {
        Level::Low => "L",
        Level::High => "H",
    }
}

fn display_variante_str(variante: &Variante) -> &str {
    match variante {
        Variante::Normal => " ",
        Variante::A => "A",
    }
}

impl Display for Beschreibung {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Beschreibung { i2c_bus, a0, a1, a2, variante } = self;
        write!(
            f,
            "{}-{}{}{}{}",
            display_bus_str(i2c_bus),
            display_level_str(a0),
            display_level_str(a1),
            display_level_str(a2),
            display_variante_str(variante)
        )
    }
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
    fn i2c_adresse(&self) -> kleiner_128 {
        let Pcf8574 { beschreibung: Beschreibung { i2c_bus: _, a0, a1, a2, variante }, .. } = self;
        let mut adresse = match variante {
            Variante::Normal => 0x20,
            Variante::A => 0x38,
        };
        if let Level::High = a0 {
            adresse = adresse + 0b001;
        }
        if let Level::High = a1 {
            adresse = adresse + 0b010;
        }
        if let Level::High = a2 {
            adresse = adresse + 0b100;
        }
        kleiner_128::try_from(adresse).expect("I2C-Adresse eines Pcf8574 passt nicht in 7Bit!")
    }

    /// Lese von einem Pcf8574.
    /// Nur als Input konfigurierte Ports werden als Some-Wert zurückgegeben.
    ///
    /// Bei Interrupt-basiertem lesen sollten alle Port gleichzeitig gelesen werden!
    fn lese(&self) -> Result<[Option<Level>; 8], Fehler> {
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
    fn port_als_input<C: Fn(Level) + Send + Sync + 'static>(
        &mut self,
        port: kleiner_8,
        trigger: Trigger,
        callback: Option<C>,
    ) -> Result<(), Fehler> {
        self.schreibe_port(port, Level::High)?;
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
    fn schreibe_port(&mut self, port: kleiner_8, level: Level) -> Result<(), Fehler> {
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

/// Variante eines Pcf8574, beeinflusst die I2C-Adresse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Variante {
    /// Variante ohne Zusätze auf dem Chip-Aufdruck.
    Normal,
    /// Variante mit extra A auf dem Chip-Aufdruck.
    A,
}

/// Ein Port eines Pcf8574.
pub struct Port {
    pcf8574: Arc<Mutex<Pcf8574>>,
    lager: Lager,
    beschreibung: Beschreibung,
    port: kleiner_8,
}

// Explizite Implementierung um einen stack-overflow zu vermeiden.
impl Debug for Port {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Port")
            .field("pcf8574", &self.pcf8574)
            .field("lager", &"<Lager>")
            .field("beschreibung", &self.beschreibung)
            .field("port", &self.port)
            .finish()
    }
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.port == other.port && self.beschreibung == other.beschreibung
    }
}
impl Eq for Port {}

impl Display for Port {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Port { beschreibung, port, .. } = self;
        write!(f, "{beschreibung}-{port}",)
    }
}

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
        port: kleiner_8,
    ) -> Self {
        Port { pcf8574, lager, beschreibung, port }
    }

    /// Die Beschreibung des [Pcf8574].
    #[inline(always)]
    pub fn beschreibung(&self) -> &Beschreibung {
        &self.beschreibung
    }

    /// Der angesprochene Port des [Pcf8574].
    #[inline(always)]
    pub fn port(&self) -> kleiner_8 {
        self.port
    }

    /// Konfiguriere den Port für Output.
    pub fn als_output(self, level: Level) -> (OutputPort, Option<Fehler>) {
        let fehler = self.pcf8574.lock().schreibe_port(self.port, level).err();
        (OutputPort(self), fehler)
    }

    /// Konfiguriere den Port für Input.
    pub fn als_input(self) -> (InputPort, Option<Fehler>) {
        let fehler = self
            .pcf8574
            .lock()
            .port_als_input::<fn(Level)>(self.port, Trigger::Disabled, None)
            .err();
        (InputPort(self), fehler)
    }
}

/// Ein Port eines [Pcf8574], konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPort(Port);

impl Display for OutputPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl OutputPort {
    /// Die Beschreibung des [Pcf8574].
    #[inline(always)]
    pub fn beschreibung(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    /// Der angesprochene Port des [Pcf8574].
    #[inline(always)]
    pub fn port(&self) -> kleiner_8 {
        self.0.port()
    }

    /// Setze den [Port] auf das übergebene [Level].
    pub fn schreibe(&mut self, level: Level) -> Result<(), Fehler> {
        self.0.pcf8574.lock().schreibe_port(self.0.port, level)
    }

    /// Ist der aktuelle Level [High](Level::High)?
    pub fn ist_high(&self) -> bool {
        self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::High
    }

    /// Ist der aktuelle Level [Low](Level::Low)?
    pub fn ist_low(&self) -> bool {
        self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::Low
    }

    /// Wechsle den aktuellen anliegenden [Level] von [High](Level::High) auf [Low](Level::Low)
    /// und umgekehrt.
    pub fn umschalten(&mut self) -> Result<(), Fehler> {
        let level = {
            let modus = &self.0.pcf8574.lock().ports[usize::from(self.port())];
            match modus {
                Modus::High => Level::Low,
                Modus::Low => Level::High,
                Modus::Input { .. } => {
                    error!("Output pin configured as input: {:?}", self);
                    Level::Low
                },
            }
        };
        self.schreibe(level)
    }
}

/// Ein Port eines Pcf8574, konfiguriert für Input.
#[derive(Debug)]
pub struct InputPort(Port);

impl Display for InputPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl InputPort {
    /// Die Beschreibung des [Pcf8574].
    #[inline(always)]
    pub fn beschreibung(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    /// Der angesprochene Port des [Pcf8574].
    #[inline(always)]
    pub fn port(&self) -> kleiner_8 {
        self.0.port()
    }

    /// Lese das aktuell am [Port] anliegende [Level].
    pub fn lese(&self) -> Result<Level, Fehler> {
        let values = self.0.pcf8574.lock().lese()?;
        if let Some(value) = values[usize::from(self.0.port)] {
            Ok(value)
        } else {
            error!("{:?} war nicht als input korrigiert!", self);
            // war nicht als Input konfiguriert -> erneut konfigurieren und neu versuchen
            self.0.pcf8574.lock().port_als_input::<fn(Level)>(
                self.0.port,
                Trigger::Disabled,
                None,
            )?;
            self.lese()
        }
    }

    /// Aktuell konfigurierter Interrupt Pin.
    pub fn interrupt_pin(&self) -> Option<u8> {
        self.0.pcf8574.lock().interrupt.as_ref().map(input::Pin::pin)
    }

    /// Assoziiere den angeschlossenen InterruptPin für den [Pcf8574].
    /// Rückgabewert ist ein evtl. vorher konfigurierter InterruptPin.
    /// Interrupt-Callbacks werden nicht zurückgesetzt!
    pub fn setze_interrupt_pin(
        &mut self,
        mut interrupt: input::Pin,
    ) -> Result<Option<input::Pin>, Fehler> {
        let mut previous = {
            // set up callback.
            let pcf8574 = &mut *self.0.pcf8574.lock();
            let mut last = pcf8574.lese()?;
            let arc_clone = self.0.pcf8574.clone();
            let interrupt_callback = move |_level| {
                let mut pcf8574 = arc_clone.lock();
                let current = match pcf8574.lese() {
                    Ok(current) => current,
                    Err(fehler) => {
                        error!("Lese-Fehler bei Pcf8574 als Interrupt-Reaktion: {:?}", fehler);
                        return;
                    },
                };
                for i in 0..8 {
                    match (&mut pcf8574.ports[i], current[i], &mut last[i]) {
                        (
                            Modus::Input { trigger, callback: Some(callback) },
                            Some(aktueller_port_wert),
                            Some(letzter_port_wert),
                        ) if trigger.callback_aufrufen(aktueller_port_wert, *letzter_port_wert) => {
                            callback(aktueller_port_wert);
                        },
                        _ => {},
                    }
                    last[i] = current[i];
                }
            };
            interrupt.setze_async_interrupt(Trigger::FallingEdge, interrupt_callback).map_err(
                |pin_fehler| {
                    let input::Fehler { pin: _, fehler } = pin_fehler;
                    Fehler::Gpio { beschreibung: *pcf8574.beschreibung(), fehler }
                },
            )?;
            std::mem::replace(&mut pcf8574.interrupt, Some(interrupt))
        };
        // clear interrupt on previous pin.
        let _ = previous.as_mut().map(input::Pin::lösche_async_interrupt);
        Ok(previous)
    }

    /// Konfiguriere einen asynchronen Interrupt Trigger.
    /// Bei auftreten wird der callback in einem separaten Thread ausgeführt.
    ///
    /// Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald
    /// [setze_async_interrupt](input::Pin::setze_async_interrupt) oder
    /// [lösche_async_interrupt](input::Pin::lösche_async_interrupt) aufgerufen wird,
    /// oder der [input::Pin] out of scope geht.
    ///
    /// ## Keine synchronen Interrupts
    /// Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.
    /// Siehe die Dokumentation der
    /// [poll_interrupts](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)
    /// Methode.
    /// > Calling poll_interrupts blocks any other calls to poll_interrupts or
    /// > InputPin::poll_interrupt until it returns. If you need to poll multiple pins simultaneously
    /// > on different threads, consider using asynchronous interrupts with
    /// > InputPin::set_async_interrupt instead.
    #[inline(always)]
    pub fn setze_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl Fn(Level) + Send + Sync + 'static,
    ) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_als_input(port, trigger, Some(callback))
    }

    /// Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.
    #[inline(always)]
    pub fn lösche_async_interrupt(&mut self) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_als_input::<fn(Level)>(port, Trigger::Disabled, None)
    }
}

// TODO genauere Eingrenzung auf einzelne Methoden
/// Ein bei Interaktion mit einem [Pcf8574] aufgetretener Fehler.
#[derive(Debug)]
pub enum Fehler {
    /// Fehler beim I2C-Channel.
    I2c {
        /// Beschreibung des [Pcf8574].
        beschreibung: Beschreibung,
        /// Der aufgetretene Fehler.
        fehler: i2c::Error,
    },
    /// Ein Fehler bei den assoziierten [Pin]s.
    Gpio {
        /// Die Beschreibung des [Pcf8574].
        beschreibung: Beschreibung,
        /// Der aufgetretene Fehler.
        fehler: gpio::Error,
    },
    /// Für den [Pcf8574] ist kein Interrupt-Pin konfiguriert.
    KeinInterruptPin(Beschreibung),
}

#[cfg(test)]
mod test;
