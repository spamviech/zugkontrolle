#!/bin/python3

from action import build, send_to_raspi, check_docker_podman

import config

# install either docker or podman
# the respective machine must be started before executing this script

# Windows (Docker Desktop not free for arbitrary use)
# https://podman-desktop.io/downloads
# https://www.docker.com/

# Linux/Debian (e.g. Ubuntu)
# sudo apt install docker:io
# sudo apt install podman

# install cross https://github.com/cross-rs/cross
# cargo install cross --git https://github.com/cross-rs/cross

# check if docker/podman is running
check_docker_podman()

# build for raspi 32bit in release mode
bin_path32 = build(target=config.raspi32_target, release=True)
# automatically transfer to raspi using scp
send_to_raspi(bin_path32, config.raspberry_user, config.raspberry_address)

# build for raspi 64bit in release mode
bin_path64 = build(target=config.raspi64_target, release=True)
# automatically transfer to raspi using scp
send_to_raspi(bin_path64, config.raspberry_user, config.raspberry_address)
