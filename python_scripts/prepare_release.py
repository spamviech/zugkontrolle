#!/bin/python3

from action import build, check_docker_podman
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

# check if docker/podman is running
check_docker_podman()

# build for raspi in release mode
build(config.name, release=True, target=config.arm_target)
# build for raspi 64bit in release mode
build(config.name, release=True, target=config.arm64_target)

# build for host platform in release mode
build(config.name, release=True, binary_extension=config.host_extension)
