#!/bin/python3

import os.path
from configparser import ConfigParser
from typing import Iterable

# default values if the config.ini doesn't exist yet
raspberry_address: str = "raspberrypi"
raspberry_user: str = "pi"

raspi32_target: str = "armv7-unknown-linux-gnueabihf"
raspi64_target: str = "aarch64-unknown-linux-gnu"
windows_target: str = "x86_64-pc-windows-gnu"
linux_target: str = "x86_64-unknown-linux-gnu"

targets: list[str] = [
    raspi32_target,
    raspi64_target,
    windows_target,
    linux_target,
]

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
config_parser = ConfigParser(defaults=default_values)
config_parser.read(config_path)


def get_raspi_address() -> str:
    return config_parser.get(section_raspi, key_address)


def get_raspi_user() -> str:
    return config_parser.get(section_raspi, key_user)


def get_targets() -> Iterable[str]:
    return map(lambda t: t[0], filter(lambda t: t[1], config_parser.items(section_targets)))
