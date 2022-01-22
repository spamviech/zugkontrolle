#!/bin/python3

import subprocess
import shutil
import sys

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

# install windres and allow it to work
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

class HostOsNotSupported(Exception):
    def __init__(self, name):
        super().__init__(name)
        self.name = name

binary_name = "zugkontrolle"
target_path = "/home/pi/" + binary_name
# musl fails, since some dependency (probably iced-backend) requires dynamic library loading
gnu_or_musl = "gnu"
target_arch = "armv7-unknown-linux-" + gnu_or_musl + "eabihf"
source_path = "./target/" + target_arch + "/release/" + binary_name
bin_path = "./bin/" + binary_name + "-" + target_arch
build_command = ["cargo", "build", "--release", "--target=" + target_arch, "--no-default-features"]
if sys.platform.startswith('linux'):
    strip_path = "arm-linux-" + gnu_or_musl + "eabihf-strip"
elif sys.platform.startswith('win32'):
    strip_path = "arm-none-linux-" + gnu_or_musl + "eabihf-strip"
else:
    raise HostOsNotSupported(sys.platform)
strip_command = [strip_path, bin_path]

def execute(command):
    print(" ".join(command))
    subprocess.run(command, check=True)

execute(build_command)
print()
print("shutil.copy2(" + source_path + ", " + bin_path + ")")
shutil.copy2(source_path, bin_path)
print()
execute(strip_command)
