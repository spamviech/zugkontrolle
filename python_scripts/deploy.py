#!/bin/python3

from action import build, send_to_raspi, check_docker_podman
from util import get_name_and_version

import config

# install either docker or podman, must be started before executing this script

# Windows (Docker Desktop not free for arbitrary use)
# https://podman-desktop.io/downloads
# https://www.docker.com/

# Linux/Debian (e.g. Ubuntu)
# sudo apt install docker:io
# sudo apt install podman

# install cross https://github.com/cross-rs/cross
# cargo install cross --git https://github.com/cross-rs/cross

name, version = get_name_and_version()

# check if docker/podman is running
check_docker_podman()

# build for raspi in release mode
bin_path = build(name, version, target=config.raspi64_target, release=True)
# automatically transfer to raspi using scp
send_to_raspi(bin_path, config.raspberry_user, config.raspberry_address)
