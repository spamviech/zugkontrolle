//! Low level Steuerung eines i2c Kanals.

use std::{
    collections::HashSet,
    fmt::Debug,
    io,
    sync::{RwLock, RwLockWriteGuard},
};

use log::{debug, error};
use once_cell::sync::Lazy;

#[cfg(not(raspi))]
#[derive(Debug)]
struct I2cState {
    buses: HashSet<u8>,
}

#[cfg(not(raspi))]
const MAX_BUS: u8 = 6;

#[cfg(not(raspi))]
static I2C: Lazy<RwLock<I2cState>> =
    Lazy::new(|| RwLock::new(I2cState { buses: (0..=MAX_BUS).collect() }));

#[cfg(not(raspi))]
impl I2cState {
    fn write_static<'t>() -> RwLockWriteGuard<'t, I2cState> {
        match I2C.write() {
            Ok(guard) => guard,
            Err(poison_error) => {
                error!("Pwm-static poisoned: {:?}", poison_error);
                poison_error.into_inner()
            }
        }
    }
}

#[cfg(raspi)]
pub type I2c = rppal::i2c::I2c;
#[cfg(not(raspi))]
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct I2c {
    bus: u8,
    slave_address: u16,
}

#[cfg(not(raspi))]
impl Drop for I2c {
    fn drop(&mut self) {
        if !I2cState::write_static().buses.insert(self.bus) {
            error!("Dropped i2c bus was still available: {:?}", self.bus)
        }
    }
}

#[cfg(not(raspi))]
impl I2c {
    /// Constructs a new `I2c`.
    ///
    /// `new` attempts to identify which I2C bus is bound to physical pins 3 (SDA)
    /// and 5 (SCL) based on the Raspberry Pi model.
    ///
    /// More information on configuring the I2C buses can be found [here].
    ///
    /// [here]: index.html#i2c-buses
    pub fn new() -> Result<I2c> {
        I2c::with_bus(1)
    }

    /// Constructs a new `I2c` using the specified bus.
    ///
    /// `bus` indicates the selected I2C bus. You'll typically want to select the
    /// bus that's bound to physical pins 3 (SDA) and 5 (SCL). On the Raspberry
    /// Pi B Rev 1, those pins are tied to bus 0. On every other Raspberry
    /// Pi model, they're connected to bus 1. Additional I2C buses are available
    /// on the Raspberry Pi 4 B and 400.
    ///
    /// More information on configuring the I2C buses can be found [here].
    ///
    /// [here]: index.html#i2c-buses
    pub fn with_bus(bus: u8) -> Result<I2c> {
        if I2cState::write_static().buses.remove(&bus) {
            Ok(I2c { bus, slave_address: 0 })
        } else {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("I2c-bus already in use: {}", bus),
            )))
        }
    }

    /// Returns the I2C bus ID.
    pub fn bus(&self) -> u8 {
        self.bus
    }

    /// Sets a 7-bit or 10-bit slave address.
    ///
    /// `slave_address` refers to the slave device you're communicating with.
    /// The specified address shouldn't include the R/W bit.
    ///
    /// By default, 10-bit addressing is disabled, which means
    /// `set_slave_address` only accepts 7-bit addresses. 10-bit addressing
    /// can be enabled with [`set_addr_10bit`]. Note that setting a 7-bit
    /// address when 10-bit addressing is enabled won't correctly target a
    /// slave device that doesn't support 10-bit addresses.
    ///
    /// [`set_addr_10bit`]: #method.set_addr_10bit
    pub fn set_slave_address(&mut self, slave_address: u16) -> Result<()> {
        debug!("{:?}.set_slave_address({})", self, slave_address);
        self.slave_address = slave_address;
        Ok(())
    }

    /// Receives incoming data from the slave device and writes it to `buffer`.
    ///
    /// `read` reads as many bytes as can fit in `buffer`.
    ///
    /// Sequence: START → Address + Read Bit → Incoming Bytes → STOP
    ///
    /// Returns how many bytes were read.
    pub fn read(&mut self, buffer: &mut [u8]) -> Result<usize> {
        debug!("{:?}.read({:?})", self, buffer);
        Ok(0)
    }

    /// Sends the outgoing data contained in `buffer` to the slave device.
    ///
    /// Sequence: START → Address + Write Bit → Outgoing Bytes → STOP
    ///
    /// Returns how many bytes were written.
    pub fn write(&mut self, buffer: &[u8]) -> Result<usize> {
        debug!("{:?}.write({:?})", self, buffer);
        Ok(buffer.len())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(raspi)]
pub type Error = rppal::i2c::Error;
#[cfg(not(raspi))]
#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum Error {
    Io(io::Error),
    InvalidSlaveAddress(u16),
    FeatureNotSupported,
    UnknownModel,
}
