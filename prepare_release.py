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

name = "zugkontrolle"
# TODO automatically transfer to raspi, e.g. using scp
# target_path = "/home/pi/" + binary_name
# musl fails, since some dependency (probably iced-backend) requires dynamic library loading
gnu_or_musl = "gnu"

class HostOsNotSupported(Exception):
    def __init__(self, name):
        super().__init__(name)
        self.name = name

def execute(command):
    print(" ".join(command))
    subprocess.run(command, check=True)
    
def move(src, dst):
    print("shutil.copy2(" + src + ", " + dst + ")")
    shutil.copy2(src, dst)

def build(program_name, release=True, target=None, strip_path=None, binary_extension=""):
    binary_name = name + binary_extension
    build_command = ["cargo", "build"]
    bin_path = "./bin/" + binary_name
    if release:
        build_command.append("--release")
        profile = "release"
    else:
        profile = "debug"
    if target is None:
        target_dir = ""
    else:
        build_command.append("--target=" + target)
        target_dir = target + "/"
        bin_path +=  "-" + target
    source_path = "./target/" + target_dir + profile + "/" + binary_name
    execute(build_command)
    print()
    move(source_path, bin_path)
    if strip_path is not None:
        strip_command = [strip_path, "--strip-all", bin_path]
        print()
        execute(strip_command)

if sys.platform.startswith('linux'):
    arm_strip_path = "arm-linux-gnueabihf-strip"
    host_extension = ""
elif sys.platform.startswith('win32'):
    arm_strip_path = "arm-none-linux-gnueabihf-strip"
    host_extension = ".exe"
else:
    raise HostOsNotSupported(sys.platform)

# build for raspi in release mode
arm_target = "armv7-unknown-linux-" + gnu_or_musl + "eabihf"
build(name, target=arm_target, strip_path=arm_strip_path)

#build for host platform
print()
build(name, strip_path="strip", binary_extension=host_extension)
