#!/bin/python3

import sys

from util import execute, copy

def check_docker_podman():
    """Check if either podman or docker is running (hello-world runs with 0 exit-code)."""
    container_engines = ["docker", "podman"]
    for engine in container_engines:
        running = execute([engine, "run", "hello-world"], exit=False)
        if running:
            return True
    # if we reach here, we didn't find a running container engine
    sys.exit("Neither docker nor podman running!")

def build(program_name, release=True, target=None, binary_extension=""):
    """Build the program for the specified profile, copy it to ./bin, strip it"""
    binary_name = program_name + binary_extension
    build_command = []
    if target is None:
        build_command.append("cargo")
    else:
        build_command.append("cross")
    build_command.append("build")
    bin_path = "./bin/" + binary_name
    if release:
        build_command.append("--release")
        profile = "release"
    else:
        profile = "debug"
    if target is None:
        target_dir = ""
    else:
        build_command.append("--target=" + target)
        target_dir = target + "/"
        bin_path +=  "-" + target
    source_path = "./target/" + target_dir + profile + "/" + binary_name
    execute(build_command)
    copy(source_path, bin_path)
    return bin_path

def send_to_raspi(program_name, bin_path, raspberry_user, raspberry_address, binary_extension=""):
    """Automatically transfer binary produced by `build` to raspi using scp"""
    binary_name = program_name + binary_extension
    target_path = "/home/" + raspberry_user + "/bin/" + binary_name
    rasperry_user_address = raspberry_user + "@" + raspberry_address
    scp_dst = rasperry_user_address + ":" + target_path
    scp_command = ["scp", bin_path, scp_dst]
    execute(scp_command)
    ssh_command = ["ssh", rasperry_user_address, "chmod", "+x", target_path]
    execute(ssh_command)
