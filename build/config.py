#!/bin/python3

import sys

name = "zugkontrolle"
raspberry_address = "raspberrypi"
raspberry_user = "pi"
# musl fails, since some dependency (probably iced-backend) requires dynamic library loading
gnu_or_musl = "gnu"

class HostOsNotSupported(Exception):
    def __init__(self, name):
        super().__init__(name)
        self.name = name

if sys.platform.startswith('linux'):
    arm_strip_path = "arm-linux-gnueabihf-strip"
    host_extension = ""
elif sys.platform.startswith('win32'):
    arm_strip_path = "arm-none-linux-gnueabihf-strip"
    host_extension = ".exe"
else:
    raise HostOsNotSupported(sys.platform)

arm_target = "armv7-unknown-linux-" + gnu_or_musl + "eabihf"
host_strip_path = "strip"
