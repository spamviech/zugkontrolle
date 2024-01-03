#!/bin/python3

import os.path
import sys

from action import get_bin_path, build, send_to_raspi, check_docker_podman
from util import is_raspi_target
import config

rebuild = ("-r" in sys.argv) or ("--rebuild" in sys.argv)
checked_docker_podman_running = False

# only deploy activated targets
for raspi_target in filter(is_raspi_target, config.targets):
    # build for raspi
    bin_path = get_bin_path(target=raspi_target)
    # re-build if the file doesn't exist
    if rebuild or (not os.path.exists(bin_path)):
        # check if docker/podman is running
        if not checked_docker_podman_running:
            checked_docker_podman_running = check_docker_podman()
        # build the zugkontrolle binary
        build(target=raspi_target)
    # automatically transfer to raspi using scp
    send_to_raspi(bin_path, config.raspberry_user, config.raspberry_address)
