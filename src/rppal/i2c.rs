//! Low level Steuerung eines i2c Kanals.

// Dokumentation ist (modulo backticks) copy+paste vom rppal-crate.
#![cfg_attr(not(feature = "raspi"), allow(clippy::missing_errors_doc))]

#[cfg(not(feature = "raspi"))]
use std::{collections::HashSet, fmt::Debug, io};

#[cfg(not(feature = "raspi"))]
use log::{debug, error};
#[cfg(not(feature = "raspi"))]
use parking_lot::MappedMutexGuard;

#[cfg(not(feature = "raspi"))]
use crate::rppal::LazyMutex;

#[cfg(not(feature = "raspi"))]
/// Verfügbare I2C-Busse.
#[derive(Debug)]
struct I2cStore {
    /// Noch verfügbare I2C-Busse.
    buses: HashSet<u8>,
}

#[cfg(not(feature = "raspi"))]
/// Kleinster unterstützter I2C-Bus.
const MIN_BUS: u8 = 0;
#[cfg(not(feature = "raspi"))]
/// Größter unterstützter I2C-Bus.
const MAX_BUS: u8 = 6;

#[cfg(not(feature = "raspi"))]
/// Globales Singleton mit den aktuell verfügbaren I2C-Bussen.
static I2C: LazyMutex<I2cStore> =
    LazyMutex::neu(|| I2cStore { buses: (MIN_BUS..=MAX_BUS).collect() });

#[cfg(not(feature = "raspi"))]
impl I2cStore {
    /// Erhalte Zugriff auf das [`Singleton`](I2C) mit den aktuell verfügbaren I2C-Bussen.
    ///
    /// Der Aufruf blockiert, bis der Zugriff erhalten wurde.
    fn lock_static<'t>() -> MappedMutexGuard<'t, I2cStore> {
        I2C.lock()
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::i2c::I2c;
#[cfg(not(feature = "raspi"))]
/// Provides access to the Raspberry Pi’s I2C peripheral.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct I2c {
    /// Der I2C-Bus.
    bus: u8,
    /// [`I2C::set_slave_address`]
    slave_address: u16,
}

#[cfg(not(feature = "raspi"))]
impl Drop for I2c {
    fn drop(&mut self) {
        if !I2cStore::lock_static().buses.insert(self.bus) {
            error!("Dropped i2c bus was still available: {:?}", self.bus);
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl I2c {
    /// Constructs a new `I2c` bound to physical pins 3 (SDA) and 5 (SCL).
    pub fn new() -> Result<I2c> {
        I2c::with_bus(1)
    }

    /// Constructs a new `I2c` using the specified bus.
    pub fn with_bus(bus: u8) -> Result<I2c> {
        if I2cStore::lock_static().buses.remove(&bus) {
            Ok(I2c { bus, slave_address: 0 })
        } else {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("I2c-bus already in use: {bus}"),
            )))
        }
    }

    /// Returns the I2C bus ID.
    #[must_use]
    pub fn bus(&self) -> u8 {
        self.bus
    }

    /// Set slave of the i2c bus the following [`read`] or [`write`] commands refer to.
    ///
    /// [`read`]: #method.read
    /// [`write`]: #method.write
    pub fn set_slave_address(&mut self, slave_address: u16) -> Result<()> {
        debug!("{:?}.set_slave_address({})", self, slave_address);
        self.slave_address = slave_address;
        Ok(())
    }

    /// Receives incoming data from the slave device and writes it to `buffer`.
    ///
    /// `read` reads as many bytes as can fit in `buffer`.
    ///
    /// Returns how many bytes were read.
    pub fn read(&mut self, buffer: &mut [u8]) -> Result<usize> {
        debug!("{:?}.read({:?})", self, buffer);
        let bytes = buffer.len();
        for element in buffer {
            *element = 0;
        }
        Ok(bytes)
    }

    /// Sends the outgoing data contained in `buffer` to the slave device.
    ///
    /// Returns how many bytes were written.
    pub fn write(&mut self, buffer: &[u8]) -> Result<usize> {
        debug!("{:?}.write({:?})", self, buffer);
        Ok(buffer.len())
    }
}

// disambiguate mit self::Result
#[allow(clippy::absolute_paths)]
/// Result with `i2c::Error`.
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::i2c::Error;
#[cfg(not(feature = "raspi"))]
/// Errors that can occur when accessing the I2C peripheral.
#[derive(Debug)]
#[allow(variant_size_differences, missing_docs)]
pub enum Error {
    Io(io::Error),
    InvalidSlaveAddress(u16),
    FeatureNotSupported,
    UnknownModel,
}
