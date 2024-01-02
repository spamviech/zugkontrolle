#!/bin/python3

from action import build, check_docker_podman

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

for target in config.targets:
    build(target=target, release=True)
