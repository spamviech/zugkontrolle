#!/bin/python3

import os
import os.path
import subprocess
import shutil
import sys
from typing import Optional, Tuple

def get_repo_root() -> str:
    script_dir = os.path.dirname(__file__)
    return os.path.abspath(os.path.join(script_dir, ".."))

def get_name_and_version() -> Tuple[str, str]:
    repo_root = get_repo_root()
    cargo_toml = os.path.join(repo_root, "Cargo.toml")
    name_prefix = "name"
    version_prefix = "version"
    header=None
    header_prefix = '['
    header_suffix = ']'
    with open(cargo_toml, 'r') as f:
        for line in f:
            line = line.removesuffix('\n').removesuffix('\r')
            if line.startswith(header_prefix) and line.endswith(header_suffix):
                header = line.removeprefix(header_prefix).removesuffix(header_suffix)
                continue
            assert(line != "[package]")
            if header == "package":
                if line.startswith(name_prefix):
                    name = line.removeprefix(name_prefix).strip().removeprefix('=').strip().removeprefix('"').removesuffix('"')
                elif line.startswith(version_prefix):
                    version = line.removeprefix(version_prefix).strip().removeprefix('=').strip().removeprefix('"').removesuffix('"')
    return name, version

first_command = True
def print_newline_after_first_call():
    global first_command
    if first_command:
        first_command = False
    else:
        print()

def execute(command: list[str], exit=True) -> Tuple[bool, Optional[str]]:
    print_newline_after_first_call()
    print(" ".join(command))
    try:
        # display output + save it to a variable
        # https://stackoverflow.com/questions/4417546/constantly-print-subprocess-output-while-process-is-running
        p = subprocess.Popen(command, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        output = ""
        for line in iter(p.stdout.readline, ""):
            print(line, end="")
            output += line
        p.stdout.close()
        return_code = p.wait()
        if return_code:
            raise subprocess.CalledProcessError(return_code, command, output)
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        output = None
        if isinstance(e, subprocess.CalledProcessError):
            output = e.output
            if len(output) > 0:
                print(output)
        if exit:
            sys.exit(e)
        else:
            return False, output
    return True, output

def query_host_target_triple() -> Optional[str]:
    success, output = execute(["rustc", "--version", "--verbose"], exit=False)
    if not success:
        print(f"Failed to query host target_triple: {output}", sys.stderr)
        return None
    host_target_triple = None
    host_prefix = "host:"
    for line in output.splitlines():
        if line.startswith(host_prefix):
            host_target_triple = line.removeprefix(host_prefix).strip()
            break
    return host_target_triple

def copy(src: str, dst: str):
    print_newline_after_first_call()
    dir = os.path.split(dst)[0]
    if not os.path.isdir(dir):
        print(f"os.mkdir({dir})")
        os.mkdir(dir)
    print(f"shutil.copy2({src}, {dst})")
    shutil.copy2(src, dst)
