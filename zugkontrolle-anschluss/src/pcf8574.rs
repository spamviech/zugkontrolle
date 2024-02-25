//! [`Pcf8574`], gesteuert über I2C.
//!
//! Alle Methoden in diesem Modul können an einem Mutex blocken (exklusiver I2C-Zugriff).
//! Der Zugriff auf diese Mutex ist auf dieses Modul beschränkt,
//! so dass es zu keinen Deadlocks kommen sollte.

use std::{
    collections::hash_map::{Entry, HashMap},
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    mem,
    sync::Arc,
};

use enum_iterator::{all, Sequence};
use itertools::iproduct;
use log::{debug, error};
use parking_lot::{Mutex, RwLock};
use serde::{Deserialize, Serialize};

use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_util::{
    eingeschränkt::{kleiner_128, kleiner_8},
    enumerate_checked::EnumerateCheckedExt,
};

use crate::{
    pin::{self, input, Pin},
    rppal::{
        gpio,
        i2c::{self, I2c},
    },
    {level::Level, trigger::Trigger},
};

/// Zugriff auf I2C-Kommunikation, inklusive der dafür notwendigen Pins.
#[derive(Debug)]
struct I2cMitPins {
    /// Zugriff auf die I2C-Kommunikation.
    i2c: I2c,
    #[allow(dead_code)]
    /// Der angesprochene Bus.
    i2c_bus: I2cBus,
    #[allow(dead_code)]
    /// Der SDA-Pin.
    sda: Pin,
    #[allow(dead_code)]
    /// Der SCL-Pin.
    scl: Pin,
}

/// Fehler beim Initialisieren eines [`Lager`].
#[derive(Debug)]
pub enum InitFehler {
    /// Fehler bei einem I2C-Channel.
    I2c {
        /// Der betroffene [`I2cBus`].
        i2c_bus: I2cBus,
        /// Der aufgetretene Fehler.
        fehler: i2c::Error,
    },
    /// Nicht verfügbarer Pin für den konfigurierten I2C-Channel.
    Pin {
        /// Der betroffene [`I2cBus`].
        i2c_bus: I2cBus,
        /// Der aufgetretene Fehler.
        fehler: pin::ReservierenFehler,
    },
}

/// Ein I2C-Bus ist deaktiviert.
#[derive(Debug, Clone, Copy)]
pub struct Deaktiviert(pub I2cBus);

impl I2cMitPins {
    /// Erhalte Zugriff auf die I2C-Kommunikation auf dem gewünschten [`I2cBus`].
    ///
    /// ## Errors
    ///
    /// Einer der Pins ist bereits anderweitig in Verwendung.
    fn neu(lager: &mut pin::Lager, i2c_bus: I2cBus) -> Result<I2cMitPins, InitFehler> {
        let i2c = i2c_bus.reserviere().map_err(|fehler| InitFehler::I2c { i2c_bus, fehler })?;
        let (sda, scl) = i2c_bus.sda_scl();
        let konvertiere_pin_fehler = |fehler| InitFehler::Pin { i2c_bus, fehler };
        let sda = lager.reserviere_pin(sda).map_err(konvertiere_pin_fehler)?;
        let scl = lager.reserviere_pin(scl).map_err(konvertiere_pin_fehler)?;
        Ok(I2cMitPins { i2c, i2c_bus, sda, scl })
    }
}

impl I2cBus {
    /// Ist der [`I2cBus`] aktiviert?
    #[must_use]
    pub fn aktiviert(self, i2c_settings: I2cSettings) -> bool {
        match self {
            I2cBus::I2c0_1 => i2c_settings.i2c0_1,
            // I2cBus::I2c2 => self.i2c2,
            I2cBus::I2c3 => i2c_settings.i2c3,
            I2cBus::I2c4 => i2c_settings.i2c4,
            I2cBus::I2c5 => i2c_settings.i2c5,
            I2cBus::I2c6 => i2c_settings.i2c6,
        }
    }
}

/// Ein [`Pcf8574`] und seine noch verfügbaren [`Ports`](Port).
type Pcf8574MitPorts = (Arc<Mutex<Pcf8574>>, [Option<Port>; 8]);

/// Noch verfügbare Pcf8574-[`Port`]s.
#[derive(Debug)]
pub struct Lager(Arc<RwLock<HashMap<Beschreibung, Pcf8574MitPorts>>>);

impl Lager {
    /// Erstelle ein neues [`Lager`].
    ///
    /// ## Errors
    ///
    /// Es konnten nicht Zugriff auf alle aktivierten I2C-Busse erhalten werden.
    pub fn neu(lager: &mut pin::Lager, settings: I2cSettings) -> Result<Lager, InitFehler> {
        let arc = Arc::new(RwLock::new(HashMap::new()));
        {
            let mut map = arc.write();
            for i2c_bus in all::<I2cBus>() {
                if !i2c_bus.aktiviert(settings) {
                    continue;
                }
                let i2c = Arc::new(Mutex::new(I2cMitPins::neu(lager, i2c_bus)?));
                let beschreibungen =
                    iproduct!(all::<Level>(), all::<Level>(), all::<Level>(), all::<Variante>());
                for (a0, a1, a2, variante) in beschreibungen {
                    let beschreibung = Beschreibung { i2c_bus, a0, a1, a2, variante };
                    let pcf8574 =
                        Arc::new(Mutex::new(Pcf8574::neu(beschreibung, Arc::clone(&i2c))));
                    let mut array = [None, None, None, None, None, None, None, None];
                    for port_num in kleiner_8::alle_werte() {
                        let port_struct = Port::neu(
                            Arc::clone(&pcf8574),
                            Lager(Arc::clone(&arc)),
                            beschreibung,
                            port_num,
                        );
                        // 0 <= port_num < 8 == array.len()
                        #[allow(clippy::indexing_slicing)]
                        {
                            array[usize::from(port_num)] = Some(port_struct);
                        }
                    }
                    if let Some(bisher) = map.insert(beschreibung, (pcf8574, array)) {
                        error!("Pcf8574 doppelt erstellt: {bisher:?}");
                    }
                }
            }
        }
        Ok(Lager(arc))
    }

    /// Reserviere einen Pcf8574-[`Port`].
    ///
    /// ## Errors
    ///
    /// Der gewünschte Port ist bereits in Verwendung.
    pub fn reserviere_pcf8574_port(
        &mut self,
        beschreibung: Beschreibung,
        port: kleiner_8,
    ) -> Result<Port, InVerwendung> {
        debug!("reserviere pcf8574 {beschreibung:?}-{port}");
        self.0
            .write()
            .get_mut(&beschreibung)
            .and_then(|(_pcf8574, ports)| {
                // 0 <= port < 8 == ports.len()
                #[allow(clippy::indexing_slicing)]
                ports[usize::from(port)].take()
            })
            .ok_or(InVerwendung { beschreibung, port })
    }

    /// Gebe einen Pcf8574-[`Port`] zurück, damit er wieder verwendet werden kann.
    ///
    /// Wird vom [Drop]-Handler des [`Port`]s aufgerufen.
    pub fn rückgabe_pcf8574_port(&mut self, port: Port) {
        debug!("rückgabe {port:?}");
        let mut guard = self.0.write();
        let entry = guard.entry(*port.beschreibung());
        let (_pcf8574, ports) = match entry {
            Entry::Occupied(occupied) => occupied.into_mut(),
            Entry::Vacant(vacant) => {
                error!("Port für unbekannten Pcf8574 zurückgegeben: {:?}", port.beschreibung());
                let pcf8574 = Arc::clone(&port.pcf8574);
                let array = [None, None, None, None, None, None, None, None];
                vacant.insert((pcf8574, array))
            },
        };
        // 0 <= port < 8 == ports.len()
        #[allow(clippy::indexing_slicing)]
        if let Some(bisher) = ports[usize::from(port.port())].replace(port) {
            error!("Bereits verfügbaren Pcf8574-Port ersetzt: {:?}", bisher);
        }
    }

    /// Der Interrupt-Pin für den [`Pcf8574`].
    ///
    /// ## Errors
    ///
    /// Es ist noch kein Interrupt-Pin gesetzt.
    #[must_use]
    pub fn interrupt_pin(&self, beschreibung: &Beschreibung) -> Option<u8> {
        debug!("lese Interrupt-Pin für pcf8574 {beschreibung:?}");
        self.0
            .read()
            .get(beschreibung)
            .and_then(|(pcf8574, _ports)| pcf8574.lock().interrupt.as_ref().map(input::Pin::pin))
    }
}

/// Ein `I2cBus`.
///
/// Vor Raspberry Pi 4 wird nur [`I2c0_1`](I2cBus::I2c0_1) als Hardware-I2C unterstützt.
///
/// Anmerkung: Es ist möglich Software-I2C auf normalen Gpio-Pins zu aktivieren.
/// Beachte dazu den Abschnitt "Aktivieren zusätzlicher I2C-Busse" in der `README.md`.
///
/// ACHTUNG: Dabei werden nur die Standard-Pins, die auch bei Pi4 verwendet werden, unterstützt.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence, Serialize, Deserialize)]
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
    /// Reserviere den Zugriff auf den gewünschten [`I2cBus`].
    fn reserviere(self) -> i2c::Result<I2c> {
        match self {
            I2cBus::I2c0_1 => I2c::with_bus(1).or_else(|_| I2c::with_bus(0)),
            // I2cBus::I2c2 => I2c::with_bus(2),
            I2cBus::I2c3 => I2c::with_bus(3),
            I2cBus::I2c4 => I2c::with_bus(4),
            I2cBus::I2c5 => I2c::with_bus(5),
            I2cBus::I2c6 => I2c::with_bus(6),
        }
    }

    /// SDA- und SCL-Pin für den [`I2cBus`].
    fn sda_scl(self) -> (u8, u8) {
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

/// Der [`Port`] wird bereits verwendet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InVerwendung {
    /// [Beschreibung] des [`Pcf8574`]s.
    pub beschreibung: Beschreibung,
    /// Port des [`Pcf8574`].
    pub port: kleiner_8,
}

/// Der aktuelle Zustand eines [`Pcf8574`]-[`Ports`](Port).
pub(super) enum Modus {
    /// Der Port ist im Input-Modus und wartet potentiell auf eine [`Level`]-Änderung.
    Input {
        /// Wann soll der `callback` ausgelöst werden.
        trigger: Trigger,
        /// Reaktion auf ein [`Trigger`]-Event.
        callback: Option<Arc<dyn Fn(Level) + Send + Sync + 'static>>,
    },
    /// Der Port ist im Output-Modus und erzeugt ein `High` Signal.
    High,
    /// Der Port ist im Output-Modus und erzeugt ein `Low` Signal.
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
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Modus::Input { trigger, callback } => formatter
                .debug_struct("Input")
                .field("trigger", trigger)
                .field("callback", &callback.as_ref().map(|_| "<function>"))
                .finish(),
            Modus::High => formatter.debug_struct("High").finish(),
            Modus::Low => formatter.debug_struct("Low").finish(),
        }
    }
}

/// Gleichheit unabhängig vom callback.
impl PartialEq for Modus {
    fn eq(&self, other: &Modus) -> bool {
        matches!(
            (self, other),
            (Modus::Input { .. }, Modus::Input { .. })
                | (Modus::High, Modus::High)
                | (Modus::Low, Modus::Low)
        )
    }
}
impl Eq for Modus {}

/// Ein Pcf8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    /// Die Einstellungen des Pcf8574.
    beschreibung: Beschreibung,
    /// Aktueller Zustand der [`Ports`](Port).
    ports: [Modus; 8],
    /// Der Interrupt-Pin.
    interrupt: Option<input::Pin>,
    /// Der I2C-Bus, mit dem sich der [`Pcf8574`] ansprechen lässt.
    i2c: Arc<Mutex<I2cMitPins>>,
}

/// Die Beschreibung eines [`Pcf8574`].
///
/// Enthält Anschluss-Details notwendig zur Adressierung.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Beschreibung {
    /// I2CBus, über den das [`Pcf8574`] angeschlossen ist.
    pub i2c_bus: I2cBus,
    /// Anliegendes [`Level`] an das `A0` Adress-Bit.
    pub a0: Level,
    /// Anliegendes [`Level`] an das `A1` Adress-Bit.
    pub a1: Level,
    /// Anliegendes [`Level`] an das `A2` Adress-Bit.
    pub a2: Level,
    /// Variante des [`Pcf8574`], beeinflusst die I2C-Adresse.
    pub variante: Variante,
}

impl PartialEq for Pcf8574 {
    fn eq(&self, other: &Self) -> bool {
        self.beschreibung == other.beschreibung
    }
}
impl Eq for Pcf8574 {}

/// Darstellung des [`I2cBus`] in der [Display]-Implementierung von [`Pcf8574`].
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

/// Darstellung eines [Levels](Level) in der [Display]-Implementierung von [`Pcf8574`].
fn display_level_str(level: &Level) -> &str {
    match level {
        Level::Low => "L",
        Level::High => "H",
    }
}

/// Darstellung der [Variante] in der [Display]-Implementierung von [`Pcf8574`].
fn display_variante_str(variante: &Variante) -> &str {
    match variante {
        Variante::Normal => " ",
        Variante::A => "A",
    }
}

impl Display for Beschreibung {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let Beschreibung { i2c_bus, a0, a1, a2, variante } = self;
        write!(
            formatter,
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
    /// Erhalte die [`Beschreibung`] des [`Pcf8574`].
    fn beschreibung(&self) -> &Beschreibung {
        &self.beschreibung
    }

    /// Erzeuge einen neuen [`Pcf8574`] passend zur [`Beschreibung`] auf dem übergebenen I2C-Bus.
    fn neu(beschreibung: Beschreibung, i2c: Arc<Mutex<I2cMitPins>>) -> Self {
        assert!(
            beschreibung.i2c_bus == i2c.lock().i2c_bus,
            "I2CBus aus der Beschreibung {:?} und verwendeter I2cBus {:?} stimmen nicht überein!",
            beschreibung.i2c_bus,
            i2c.lock().i2c_bus
        );
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
        // max value: 0x38 + 0b001 + 0b010 + 0b100 == 0x3f == 63 < 255 == u8::MAX
        #[allow(clippy::arithmetic_side_effects)]
        if let Level::High = a0 {
            adresse += 0b001;
        }
        #[allow(clippy::arithmetic_side_effects)]
        if let Level::High = a1 {
            adresse += 0b010;
        }
        #[allow(clippy::arithmetic_side_effects)]
        if let Level::High = a2 {
            adresse += 0b100;
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
        i2c_with_pins.i2c.set_slave_address(self.i2c_adresse().into()).map_err(map_fehler)?;
        let mut buf = [0; 1];
        let bytes_read = i2c_with_pins.i2c.read(&mut buf).map_err(map_fehler)?;
        if bytes_read != 1 {
            debug!("bytes_read = {bytes_read} != 1",);
        }
        let mut result = [None; 8];
        for (port, modus) in self.ports.iter().enumerate_checked() {
            let port: usize = port.expect("port passt nicht in usize!");
            let port_u32 = u32::try_from(port).expect("port passt nicht in u32!");
            let port_bit = 2u8.pow(port_u32);
            // 0-7 < 8 == result.len()
            #[allow(clippy::indexing_slicing)]
            {
                result[port] = if let Modus::Input { .. } = modus {
                    Some(if (buf[0] & port_bit) > 0 { Level::High } else { Level::Low })
                } else {
                    None
                };
            }
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
            Some(callback) => Some(Arc::new(callback)),
            None => None,
        };
        // 0-7 < 8 == self.ports.len()
        #[allow(clippy::indexing_slicing)]
        {
            self.ports[usize::from(port)] = Modus::Input { trigger, callback };
        }
        Ok(())
    }

    /// Schreibe auf einen Port des Pcf8574.
    /// Der Port wird automatisch als Output gesetzt.
    fn schreibe_port(&mut self, port: kleiner_8, level: Level) -> Result<(), Fehler> {
        // 0-7 < 8 == self.ports.len()
        #[allow(clippy::indexing_slicing)]
        {
            self.ports[usize::from(port)] = level.into();
        }
        let beschreibung = self.beschreibung();
        let mut i2c_with_pins = self.i2c.lock();
        let map_fehler = |fehler| Fehler::I2c { beschreibung: *beschreibung, fehler };
        i2c_with_pins.i2c.set_slave_address(self.i2c_adresse().into()).map_err(map_fehler)?;
        let mut wert = 0;
        for (it_port, modus) in self.ports.iter().enumerate_checked() {
            wert |= match modus {
                Modus::Input { .. } | Modus::High => {
                    let it_port = it_port.expect("port passt nicht in u32!");
                    2u8.pow(it_port)
                },
                Modus::Low => 0,
            };
        }
        let buf = [wert; 1];
        let bytes_written = i2c_with_pins.i2c.write(&buf).map_err(map_fehler)?;
        if bytes_written != 1 {
            error!("bytes_written = {bytes_written} != 1",);
        }
        Ok(())
    }
}

/// Variante eines Pcf8574, beeinflusst die I2C-Adresse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence, Serialize, Deserialize)]
#[allow(clippy::min_ident_chars)]
pub enum Variante {
    /// Variante ohne Zusätze auf dem Chip-Aufdruck.
    Normal,
    /// Variante mit extra A auf dem Chip-Aufdruck.
    A,
}

/// Ein Port eines [`Pcf8574`].
pub struct Port {
    /// Pointer auf den [`Pcf8574`].
    ///
    /// Notwendig für lese/schreibe-Implementierung.
    pcf8574: Arc<Mutex<Pcf8574>>,
    /// Pointer auf das Port-[`Lager`].
    ///
    /// Notwendig, für die [`Drop`]-Implementierung.
    lager: Lager,
    /// Die Beschreibung des [`Pcf8574`].
    beschreibung: Beschreibung,
    /// Die Port-Nummer.
    port: kleiner_8,
}

// Explizite Implementierung um einen stack-overflow zu vermeiden.
impl Debug for Port {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Port")
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
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let Port { beschreibung, port, .. } = self;
        write!(formatter, "{beschreibung}-{port}",)
    }
}

impl Drop for Port {
    fn drop(&mut self) {
        debug!("drop {:?}", self);
        let port_ersatz = Port::neu(
            Arc::clone(&self.pcf8574),
            Lager(Arc::clone(&self.lager.0)),
            *self.beschreibung(),
            self.port(),
        );
        self.lager.rückgabe_pcf8574_port(port_ersatz);
    }
}

impl Port {
    /// Erzeuge einen neuen [`Port`].
    fn neu(
        pcf8574: Arc<Mutex<Pcf8574>>,
        lager: Lager,
        beschreibung: Beschreibung,
        port: kleiner_8,
    ) -> Self {
        Port { pcf8574, lager, beschreibung, port }
    }

    /// Die Beschreibung des [`Pcf8574`].
    #[must_use]
    pub fn beschreibung(&self) -> &Beschreibung {
        &self.beschreibung
    }

    /// Der angesprochene Port des [`Pcf8574`].
    #[must_use]
    pub fn port(&self) -> kleiner_8 {
        self.port
    }

    /// Konfiguriere den Port für Output.
    #[must_use]
    pub fn als_output(self, level: Level) -> (OutputPort, Option<Fehler>) {
        let fehler = self.pcf8574.lock().schreibe_port(self.port, level).err();
        (OutputPort(self), fehler)
    }

    /// Konfiguriere den Port für Input.
    #[must_use]
    pub fn als_input(self) -> (InputPort, Option<Fehler>) {
        let fehler = self
            .pcf8574
            .lock()
            .port_als_input::<fn(Level)>(self.port, Trigger::Disabled, None)
            .err();
        (InputPort(self), fehler)
    }
}

/// Ein Port eines [`Pcf8574`], konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPort(Port);

impl Display for OutputPort {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, formatter)
    }
}

impl OutputPort {
    /// Die Beschreibung des [`Pcf8574`].
    #[must_use]
    pub fn beschreibung(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    /// Der angesprochene Port des [`Pcf8574`].
    #[must_use]
    pub fn port(&self) -> kleiner_8 {
        self.0.port()
    }

    /// Setze den [Port] auf das übergebene [`Level`].
    ///
    /// ## Errors
    ///
    /// Fehler beim setzten des aktuellen Werts für den [`Pcf8574`].
    pub fn schreibe(&mut self, level: Level) -> Result<(), Fehler> {
        self.0.pcf8574.lock().schreibe_port(self.0.port, level)
    }

    /// Ist der aktuelle Level [`High`](Level::High)?
    #[must_use]
    pub fn ist_high(&self) -> bool {
        // 0-7 < 8 == self.ports.len()
        #[allow(clippy::indexing_slicing)]
        {
            self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::High
        }
    }

    /// Ist der aktuelle Level [`Low`](Level::Low)?
    #[must_use]
    pub fn ist_low(&self) -> bool {
        // 0-7 < 8 == self.ports.len()
        #[allow(clippy::indexing_slicing)]
        {
            self.0.pcf8574.lock().ports[usize::from(self.port())] == Modus::Low
        }
    }

    /// Wechsle den aktuellen anliegenden [Level] von [High](Level::High) auf [`Low`](Level::Low)
    /// und umgekehrt.
    ///
    /// ## Errors
    ///
    /// Fehler beim schreiben des aktuellen Werts für den [`Pcf8574`].
    pub fn umschalten(&mut self) -> Result<(), Fehler> {
        let level = {
            // 0-7 < 8 == self.ports.len()
            #[allow(clippy::indexing_slicing)]
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
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, formatter)
    }
}

impl InputPort {
    /// Die Beschreibung des [`Pcf8574`].
    #[must_use]
    pub fn beschreibung(&self) -> &Beschreibung {
        self.0.beschreibung()
    }

    /// Der angesprochene Port des [`Pcf8574`].
    #[must_use]
    pub fn port(&self) -> kleiner_8 {
        self.0.port()
    }

    /// Lese das aktuell am [Port] anliegende [`Level`].
    ///
    /// ## Errors
    ///
    /// Fehler beim lesen des aktuellen Werts des [`Pcf8574`].
    pub fn lese(&self) -> Result<Level, Fehler> {
        let values = self.0.pcf8574.lock().lese()?;
        // 0-7 < 8 == self.ports.len()
        #[allow(clippy::indexing_slicing)]
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

    /// Assoziiere den angeschlossenen Interrupt-Pin für den [`Pcf8574`].
    /// Rückgabewert ist ein evtl. vorher konfigurierter Interrupt-Pin.
    /// Interrupt-Callbacks werden nicht zurückgesetzt!
    ///
    /// ## Errors
    ///
    /// Fehler beim setzten des Callback.
    pub fn setze_interrupt_pin(
        &mut self,
        mut interrupt: input::Pin,
    ) -> Result<Option<input::Pin>, Fehler> {
        let mut previous = {
            // set up callback.
            let pcf8574 = &mut *self.0.pcf8574.lock();
            let mut last = pcf8574.lese()?;
            let arc_clone = Arc::clone(&self.0.pcf8574);
            let interrupt_callback = move |_level| {
                // neuer Zugriff auf die selbe Mutex
                #[allow(clippy::shadow_unrelated)]
                let mut pcf8574 = arc_clone.lock();
                let current = match pcf8574.lese() {
                    Ok(current) => current,
                    Err(fehler) => {
                        error!("Lese-Fehler bei Pcf8574 als Interrupt-Reaktion: {:?}", fehler);
                        return;
                    },
                };
                // 0-7 < 8 == self.ports.len()
                #[allow(clippy::indexing_slicing)]
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
            mem::replace(&mut pcf8574.interrupt, Some(interrupt))
        };
        // clear interrupt on previous pin.
        let _ = previous.as_mut().map(input::Pin::lösche_async_interrupt);
        Ok(previous)
    }

    /// Konfiguriere einen asynchronen Interrupt Trigger.
    /// Bei auftreten wird der callback in einem separaten Thread ausgeführt.
    ///
    /// Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald
    /// [`setze_async_interrupt`](input::Pin::setze_async_interrupt) oder
    /// [`lösche_async_interrupt`](input::Pin::lösche_async_interrupt) aufgerufen wird,
    /// oder der [`input::Pin`] out of scope geht.
    ///
    /// ## Keine synchronen Interrupts
    /// Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.
    /// Siehe die Dokumentation der
    /// [`poll_interrupts`](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)
    /// Methode.
    /// > Calling `poll_interrupts` blocks any other calls to `poll_interrupts` or
    /// > `InputPin::poll_interrupt` until it returns. If you need to poll multiple pins simultaneously
    /// > on different threads, consider using asynchronous interrupts with
    /// > `InputPin::set_async_interrupt` instead.
    ///
    /// ## Errors
    ///
    /// Fehler beim setzten des async interrupts.
    pub fn setze_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl Fn(Level) + Send + Sync + 'static,
    ) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_als_input(port, trigger, Some(callback))
    }

    /// Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.
    ///
    /// ## Errors
    ///
    /// Fehler beim entfernen des async interrupts.
    pub fn lösche_async_interrupt(&mut self) -> Result<(), Fehler> {
        let port = self.port();
        self.0.pcf8574.lock().port_als_input::<fn(Level)>(port, Trigger::Disabled, None)
    }
}

/// Ein bei Interaktion mit einem [`Pcf8574`] aufgetretener Fehler.
#[derive(Debug)]
pub enum Fehler {
    /// Fehler beim I2C-Channel.
    I2c {
        /// Beschreibung des [`Pcf8574`].
        beschreibung: Beschreibung,
        /// Der aufgetretene Fehler.
        fehler: i2c::Error,
    },
    /// Ein Fehler bei den assoziierten [`Pin`]s.
    Gpio {
        /// Die Beschreibung des [`Pcf8574`].
        beschreibung: Beschreibung,
        /// Der aufgetretene Fehler.
        fehler: gpio::Error,
    },
    /// Für den [`Pcf8574`] ist kein Interrupt-Pin konfiguriert.
    KeinInterruptPin(Beschreibung),
}

#[cfg(test)]
mod test;
