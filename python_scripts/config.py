#!/bin/python3

import os.path
import sys
from configparser import ConfigParser
from typing import Iterable

raspi32_target: str = "armv7-unknown-linux-gnueabihf"
raspi64_target: str = "aarch64-unknown-linux-gnu"
windows_target: str = "x86_64-pc-windows-gnu"
linux_target: str = "x86_64-unknown-linux-gnu"

section_raspi: str = "raspi"
key_address = "address"
key_user = "user"

section_targets: str = "targets"

default_values: dict[str, dict[str, str]] = {
    section_raspi: {
        key_address: "raspberrypi",
        key_user: "pi",
    },
    section_targets: {
        raspi32_target: "True",
        raspi64_target: "True",
        windows_target: "True",
        linux_target: "True",
    }
}

config_path: str = os.path.join(os.path.dirname(__file__), "config.ini")
config_parser = ConfigParser()

config_parser.read_dict(default_values)
config_parser.read(config_path)

raspberry_address: str = config_parser.get(section_raspi, key_address)
raspberry_user: str = config_parser.get(section_raspi, key_user)
targets: list[str] = []
for target in config_parser.options(section_targets):
    try:
        if config_parser.getboolean(section_targets, target):
            targets.append(target)
    except ValueError as e:
        print(f"Invalid value for target '{target}': {e}", file=sys.stderr)
