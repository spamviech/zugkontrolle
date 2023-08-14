#!/bin/python3

raspberry_address: str = "raspberrypi"
raspberry_user: str = "pi"

raspi32_target: str = "armv7-unknown-linux-gnueabihf"
raspi64_target: str = "aarch64-unknown-linux-gnu"
windows_target: str = "x86_64-pc-windows-gnu"
linux_target: str = "x86_64-unknown-linux-gnu"

targets: list[str] = [(raspi32_target, ""), (raspi64_target, ""),
                      (windows_target, ".exe"), (linux_target, "")]
