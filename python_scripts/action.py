#!/bin/python3

import sys
import os.path
from typing import Optional

from util import execute, copy, query_host_target_triple, get_repo_root

host_target_triple: Optional[str] = query_host_target_triple()
repo_root: str = get_repo_root()


def check_docker_podman(exit: bool = True) -> bool:
    """Check if either podman or docker is running (hello-world runs with 0 exit-code)."""
    container_engines = ["docker", "podman"]
    for engine in container_engines:
        running, output = execute(
            [engine, "run", "hello-world"], exit=False)
        if running:
            return True
    # if we reach here, we didn't find a running container engine
    if exit:
        sys.exit("Neither docker nor podman running!")
    else:
        return False


def build(program_name: str, program_version: str, target: str, release: bool = True, binary_extension: str = "") -> str:
    """Build the program for the specified profile, copy it to {repo_root}/bin"""
    if target == host_target_triple:
        build_program = "cargo"
    else:
        build_program = "cross"
    if release:
        profile = "release"
    else:
        profile = "debug"
    build_command = [build_program, "build", "--profile=" +
                     profile, "--target=" + target]
    execute(build_command)
    source_path = os.path.join(
        repo_root, "target",  target, profile, program_name + binary_extension)
    bin_path = os.path.join(
        repo_root, "bin", f"{program_name}-{program_version}-{target}{binary_extension}")
    copy(source_path, bin_path)
    return bin_path


def send_to_raspi(bin_path: str, raspberry_user: str, raspberry_address: str):
    """Automatically transfer binary produced by `build` to raspi using scp"""
    binary_name = os.path.basename(bin_path)
    target_path = "/home/" + raspberry_user + "/bin/" + binary_name
    rasperry_user_address = raspberry_user + "@" + raspberry_address
    scp_dst = rasperry_user_address + ":" + target_path
    scp_command = ["scp", bin_path, scp_dst]
    execute(scp_command)
    ssh_command = ["ssh", rasperry_user_address, "chmod", "+x", target_path]
    execute(ssh_command)
