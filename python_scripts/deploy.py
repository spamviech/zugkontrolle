#!/bin/python3

import os.path

from action import get_bin_path, send_to_raspi

import config

# This assumes, prepare_release.py was executed before running this script
# Hint: you can adjust the config.ini to only build the target binaries

# only deploy activated targets
for raspi_target in filter(lambda t: t in config.targets, [config.raspi32_target, config.raspi64_target]):
    # build for raspi
    bin_path = get_bin_path(target=raspi_target)
    # check if the file exists
    assert(os.path.isfile(bin_path))
    # automatically transfer to raspi using scp
    send_to_raspi(bin_path, config.raspberry_user, config.raspberry_address)
