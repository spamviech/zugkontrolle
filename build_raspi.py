#!/bin/python3

import subprocess
import shutil
import sys

# download arm cross compiler toolchain
# AArch32 target with hard float (arm-none-linux-gnueabihf)
# https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads
# (make sure <toolchain-path>/bin is in PATH)

# install windres and allow it to work
# pacman -S mingw-w64-x86_64-binutils
# pacman -S mingw-w64-x86_64-gcc

binary_name = "zugkontrolle"
target_path = "/home/pi/" + binary_name
target_arch = "armv7-unknown-linux-gnueabihf"
source_path = "./target/" + target_arch + "/release/" + binary_name
bin_path = "./bin/" + binary_name + "-" + target_arch
build_command = ["cargo", "build", "--release", "--target=" + target_arch, "--no-default-features"]
strip_command = ["arm-none-linux-gnueabihf-strip", bin_path]

def execute(command):
    print(" ".join(command))
    subprocess.run(command, check=True)

execute(build_command)
print("shutil.copy2(" + source_path + ", " + bin_path + ")")
shutil.copy2(source_path, bin_path)
execute(strip_command)
