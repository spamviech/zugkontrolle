#!/bin/python3

import os
import os.path
import sys

raspberry_address = "raspberrypi"
raspberry_user = "pi"

cwd = os.getcwd()
name = os.path.split(cwd)[1]

class HostOsNotSupported(Exception):
    def __init__(self, name):
        super().__init__(name)
        self.name = name

if sys.platform.startswith('linux'):
    host_extension = ""
elif sys.platform.startswith('win32'):
    host_extension = ".exe"
else:
    raise HostOsNotSupported(sys.platform)

arm_target = "armv7-unknown-linux-gnueabihf"
arm64_target = "aarch64-unknown-linux-gnu"
