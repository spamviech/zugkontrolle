#!/bin/python3

import subprocess
import sys
from build.action import build, send_to_raspi
import build.config as config

# install either docker or podman, must be started before executing this script

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

# build for raspi in release mode
bin_path = build(config.name, target=config.arm_target)
# automatically transfer to raspi using scp
send_to_raspi(config.name, bin_path, config.raspberry_user, config.raspberry_address)
