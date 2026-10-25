//! Informationen über das aktuelle System.

#![cfg_attr(
    feature = "raspi",
    expect(clippy::pub_use, reason = "Mit raspi-feature wird das rpi_pal-crate verwendet.")
)]
#![cfg_attr(
    not(feature = "raspi"),
    allow(
        clippy::missing_errors_doc,
        clippy::missing_docs_in_private_items,
        reason = "Dokumentation ist (modulo backticks) copy+paste vom rpi_pal-crate."
    )
)]

#[cfg(not(feature = "raspi"))]
use std::{error, fmt, result};

#[cfg(feature = "raspi")]
pub use rpi_pal::system::DeviceInfo;
#[cfg(not(feature = "raspi"))]
/// Retrieves Raspberry Pi device information.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct DeviceInfo {
    model: Model,
    soc: SoC,
    // Total number of supported hardware PWM channels
    pwm_channels: u8,
}

#[cfg(not(feature = "raspi"))]
impl DeviceInfo {
    /// Constructs a new `DeviceInfo`.
    ///
    /// `new` attempts to identify the Raspberry Pi's model and `SoC` based on
    /// the contents of `/proc/cpuinfo`, `/sys/firmware/devicetree/base/compatible`
    /// and `/sys/firmware/devicetree/base/model`.
    pub fn new() -> Result<DeviceInfo> {
        // Hard-code model 3B+ for simplicity.
        Ok(DeviceInfo { model: Model::RaspberryPi3BPlus, soc: SoC::Bcm2837B0, pwm_channels: 2 })
    }

    /// Returns the Raspberry Pi's model.
    #[expect(clippy::must_use_candidate, reason = "Selbes Interface wie die rpi_pal Variante.")]
    pub fn model(&self) -> Model {
        self.model
    }

    /// Returns the Raspberry Pi's `SoC`.
    #[expect(clippy::must_use_candidate, reason = "Selbes Interface wie die rpi_pal Variante.")]
    pub fn soc(&self) -> SoC {
        self.soc
    }

    /// Returns the number of hardware PWM channels supported by this Raspberry Pi model.
    #[expect(clippy::must_use_candidate, reason = "Selbes Interface wie die rpi_pal Variante.")]
    pub fn pwm_channels(&self) -> u8 {
        self.pwm_channels
    }
}

#[cfg(feature = "raspi")]
pub use rpi_pal::system::Model;
#[cfg(not(feature = "raspi"))]
/// Identifiable Raspberry Pi models.
///
/// `Model` might be extended with additional variants in a minor or
/// patch revision, and must not be exhaustively matched against.
/// Instead, add a `_` catch-all arm to match future variants.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[expect(missing_docs, reason = "Namen sind aussagekräftig genug.")]
#[non_exhaustive]
pub enum Model {
    RaspberryPiA,
    RaspberryPiAPlus,
    RaspberryPiBRev1,
    RaspberryPiBRev2,
    RaspberryPiBPlus,
    RaspberryPi2B,
    RaspberryPi3APlus,
    RaspberryPi3B,
    RaspberryPi3BPlus,
    RaspberryPi4B,
    RaspberryPi400,
    RaspberryPi5,
    RaspberryPi500,
    RaspberryPiComputeModule,
    RaspberryPiComputeModule3,
    RaspberryPiComputeModule3Plus,
    RaspberryPiComputeModule4,
    RaspberryPiComputeModule4S,
    RaspberryPiComputeModule5,
    RaspberryPiComputeModule5Lite,
    RaspberryPiZero,
    RaspberryPiZeroW,
    RaspberryPiZero2W,
}

#[cfg(feature = "raspi")]
pub use rpi_pal::system::SoC;
#[cfg(not(feature = "raspi"))]
/// Identifiable Raspberry Pi `SoC`s.
///
/// `SoC` might be extended with additional variants in a minor or
/// patch revision, and must not be exhaustively matched against.
/// Instead, add a `_` catch-all arm to match future variants.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[expect(missing_docs, reason = "Namen sind aussagekräftig genug.")]
#[non_exhaustive]
pub enum SoC {
    Bcm2835,
    Bcm2836,
    Bcm2837A1,
    Bcm2837B0,
    Bcm2711,
    Bcm2712,
}

#[cfg(not(feature = "raspi"))]
impl fmt::Display for SoC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            SoC::Bcm2835 => write!(f, "BCM2835"),
            SoC::Bcm2836 => write!(f, "BCM2836"),
            SoC::Bcm2837A1 => write!(f, "BCM2837A1"),
            SoC::Bcm2837B0 => write!(f, "BCM2837B0"),
            SoC::Bcm2711 => write!(f, "BCM2711"),
            SoC::Bcm2712 => write!(f, "BCM2712"),
        }
    }
}

#[cfg(not(feature = "raspi"))]
/// Errors that can occur when trying to identify the Raspberry Pi hardware.
#[derive(Debug)]
#[expect(
    missing_copy_implementations,
    clippy::error_impl_error,
    reason = "Selbes Interface wie die rpi_pal Variante."
)]
pub enum Error {
    /// Unknown model.
    ///
    /// `DeviceInfo` was unable to identify the Raspberry Pi model based on the
    /// contents of `/proc/cpuinfo`, `/sys/firmware/devicetree/base/compatible`
    /// and `/sys/firmware/devicetree/base/model`.
    ///
    /// Support for new models is usually added shortly after they are officially
    /// announced and available to the public. Make sure you're using the latest
    /// release of rpi-pal.
    ///
    /// You may also encounter this error if your Linux distribution
    /// doesn't provide any of the common user-accessible system files
    /// that are used to identify the model and `SoC`.
    UnknownModel,
    /// Unknown kernel.
    ///
    /// `KernelVersion` needed to identify the kernel version from file
    /// `/proc/version`, but was unable to either open the file or parse its
    /// content.
    UnknownKernel,
}

#[cfg(not(feature = "raspi"))]
impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::UnknownModel => write!(formatter, "Unknown Raspberry Pi model"),
            Error::UnknownKernel => write!(formatter, "Unknown Raspberry Pi kernel"),
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl error::Error for Error {}

#[cfg(not(feature = "raspi"))]
/// Result type returned from methods that can have `system::Error`s.
pub type Result<T> = result::Result<T, Error>;
