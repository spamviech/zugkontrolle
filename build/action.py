#!/bin/python3

from build.util import execute, copy

def build(program_name, release=True, target=None, strip_path=None, binary_extension=""):
    """Build the program for the specified profile, copy it to ./bin, strip it"""
    binary_name = program_name + binary_extension
    build_command = ["cargo", "build"]
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
    if strip_path is not None:
        strip_command = [strip_path, "--strip-all", bin_path]
        execute(strip_command)
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
