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
raspberry_address = "raspberrypi"
raspberry_user = "pi"
# musl fails, since some dependency (probably iced-backend) requires dynamic library loading
gnu_or_musl = "gnu"

class HostOsNotSupported(Exception):
    def __init__(self, name):
        super().__init__(name)
        self.name = name

first_command = True
def print_newline_after_first_call():
    global first_command
    if first_command:
        first_command = False
    else:
        print()

def execute(command):
    print_newline_after_first_call()
    print(" ".join(command))
    subprocess.run(command, check=True)
    
def move(src, dst):
    print_newline_after_first_call()
    print("shutil.copy2(" + src + ", " + dst + ")")
    shutil.copy2(src, dst)

def build(program_name, release=True, target=None, strip_path=None, binary_extension=""):
    """Build the program for the specified profile, copy it to ./bin, strip it"""
    binary_name = program_name + binary_extension
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
    move(source_path, bin_path)
    if strip_path is not None:
        strip_command = [strip_path, "--strip-all", bin_path]
        execute(strip_command)
    return bin_path

def send_to_raspi(program_name, bin_path, raspberry_user, raspberry_address, binary_extension=""):
    """Automatically transfer binary produced by `build` to raspi using scp"""
    binary_name = program_name + binary_extension
    target_path = "/home/" + raspberry_user + "/" + program_name + "/" + binary_name
    rasperry_user_address = raspberry_user + "@" + raspberry_address
    scp_dst = rasperry_user_address + ":" + target_path
    scp_command = ["scp", bin_path, scp_dst]
    execute(scp_command)
    ssh_command = ["ssh", rasperry_user_address, "\"chmod +x " + scp_dst + "\""]
    execute(ssh_command)

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
bin_path = build(name, target=arm_target, strip_path=arm_strip_path)
# automatically transfer to raspi using scp
try:
    send_to_raspi(name, bin_path, raspberry_user, raspberry_address)
except subprocess.CalledProcessError as e:
    print(e)

# build for host platform
build(name, strip_path="strip", binary_extension=host_extension)
