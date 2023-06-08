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

# install windres (+ strip, make) and allow it to work
# pacman -S mingw-w64-x86_64-binutils
# pacman -S mingw-w64-x86_64-gcc
# pacman -S make

# download Visual Studio build tools
# https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022
# execute build script from Developer Powershell for VS 2022 (or command line if you prefer that)
# it might be necessary to run `cargo clean` before starting the build

# TODO: the above should be enough to compile expat-sys, but freetype-sys still complains :(

# LINUX
# sudo apt install arm-linux-gnueabihf-gcc
# create `$HOME/.cargo/config.toml` with these contents
"""
[target.armv7-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"

[target.armv7-unknown-linux-musleabihf]
linker = "arm-linux-gnueabihf-gcc"
"""

# looks like it helps, but still not enough :(
#import os
#os.environ["CMAKE_GENERATOR"] = "MSYS Makefiles"

# build for raspi in release mode
build(config.name, target=config.arm_target)

# build for host platform
build(config.name, binary_extension=config.host_extension)
