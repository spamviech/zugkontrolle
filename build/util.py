#!/bin/python3

import subprocess
import shutil

first_command = True
def print_newline_after_first_call():
    global first_command
    if first_command:
        first_command = False
    else:
        print()

def execute(command):
    print_newline_after_first_call()
    print(" ".join(command))
    try:
        subprocess.run(command, check=True)
    except subprocess.CalledProcessError as e:
        sys.exit(e)
    
def copy(src, dst):
    print_newline_after_first_call()
    print("shutil.copy2(" + src + ", " + dst + ")")
    shutil.copy2(src, dst)
