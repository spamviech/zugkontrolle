#!/bin/python3

import os
import os.path
import subprocess
import shutil
import sys

first_command = True
def print_newline_after_first_call():
    global first_command
    if first_command:
        first_command = False
    else:
        print()

def execute(command, exit=True):
    print_newline_after_first_call()
    print(" ".join(command))
    try:
        subprocess.run(command, check=True)
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        if exit:
            sys.exit(e)
        else:
            return False
    return True

def copy(src, dst):
    print_newline_after_first_call()
    dir = os.path.split(dst)[0]
    if not os.path.isdir(dir):
        print(f"os.mkdir({dir})")
        os.mkdir(dir)
    print(f"shutil.copy2({src}, {dst})")
    shutil.copy2(src, dst)
