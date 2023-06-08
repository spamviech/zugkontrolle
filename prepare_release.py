#!/bin/python3

from build.action import build
import build.config as config

# add rust target
# rustup target add armv7-unknown-linux-gnueabihf

# WINDOWS
# download arm cross compiler toolchain
# AArch32 target with hard float (arm-none-linux-gnueabihf)
# Version 10.2, since current Rasperry Pi OS version (released October 30th 2021)
# only supports glibc 2.31
# https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads
# create `%USERPROFILE%\.cargo\config.toml` with these contents
# (make sure <toolchain-path>/bin is in PATH, alternatively specify full path here)
"""
[target.armv7-unknown-linux-gnueabihf]
linker = "arm-none-linux-gnueabihf-gcc"

[target.armv7-unknown-linux-musleabihf]
linker = "arm-none-linux-gnueabihf-gcc"
"""

# install windres (+ strip) and allow it to work
# (you might need to add "C:\msys64\mingw64\bin" and "C:\msys64\usr\bin" to your PATH)
"""
pacman -S mingw-w64-x86_64-binutils
pacman -S mingw-w64-x86_64-gcc
pacman -S pkg-config
"""

# can we use the msys-provided one?
# download & install cmake, make sure it is in PATH
# https://cmake.org/download/

# download Visual Studio build tools (we need at least `nmake` from it)
# https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022
# choose the equivalent to the german
# - under "Workload", enable "Buildtools für die universelle Windows-Platform"
# - under  "Einzelne Komponenten", enable "C++-CMake-Tools für Windows"
#   (this alone might be enough, but is definitely required)

# it should not be necessare, but it can help to run from Developer Powershell for VS 2022
# (or command promt if you prefer that)

# it might be necessary to run `cargo clean` before starting the build
# (e.g. after an installation with missing requirements)

# LINUX
# sudo apt install arm-linux-gnueabihf-gcc
# create `$HOME/.cargo/config.toml` with these contents
"""
[target.armv7-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"

[target.armv7-unknown-linux-musleabihf]
linker = "arm-linux-gnueabihf-gcc"
"""

# TODO: maybe there are similare steps necessary now (e.g. install cmake)
# I didn't try it on Linux with the current dependency set yet.

import os
# make sure the cc-crate uses the correct c++-compiler during cross-compilation
os.environ["CXX_armv7_unknown_linux_gnueabihf"] = "arm-none-linux-gnueabihf-g++"

# a step in the right direction, but not fully successful
# (lib still missing)
os.environ["PKG_CONFIG_ALLOW_CROSS"] = "1"
# https://github.com/servo/libfontconfig/issues/67
# https://github.com/servo/libfontconfig/issues/64
# https://github.com/slint-ui/slint/discussions/1165

# build for raspi in release mode
build(config.name, target=config.arm_target)

# build for host platform
build(config.name, binary_extension=config.host_extension)
