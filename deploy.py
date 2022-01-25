#!/bin/python3

import subprocess
import sys
from build.action import build, send_to_raspi
import build.config as config

# add rust target
# rustup target add armv7-unknown-linux-gnueabihf

# register ssh key on raspberry pi
# https://arshovon.com/blog/logging-to-raspberry-pi-without-password/
# (need to be called from an msys2-shell)
# pacman -S openssh
# ssh-copy-id USER@RASPBERRY_IP_ADDRESS
# (create key if none present)
# ssh-keygen -t ed25519

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
# pacman -S mingw-w64-x86_64-binutils
# pacman -S mingw-w64-x86_64-gcc

# LINUX
# sudo apt install arm-linux-gnueabihf-gcc
# create `$HOME/.cargo/config.toml` with these contents
"""
[target.armv7-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"

[target.armv7-unknown-linux-musleabihf]
linker = "arm-linux-gnueabihf-gcc"
"""

bin_path = build(config.name, target=config.arm_target, strip_path=config.arm_strip_path)
# automatically transfer to raspi using scp
send_to_raspi(config.name, bin_path, config.raspberry_user, config.raspberry_address)
