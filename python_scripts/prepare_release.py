#!/bin/python3

from action import build, check_docker_podman
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

for target, binary_extension in config.targets:
    build(program_name=name, program_version=version, target=target,
          release=True, binary_extension=binary_extension)
