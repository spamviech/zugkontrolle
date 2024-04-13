#!/bin/python3

import os.path
import sys

import util

# add submodule directory to import search path
# https://stackoverflow.com/a/3144107
this_directory = os.path.dirname(os.path.abspath(__file__))
sys.path.append(f"{this_directory}/nototools")

from nototools.subset import subset_font

repo_dir: str = util.get_repo_root()
font_dir: str = os.path.join(repo_dir, "fonts")
woff_dir: str = os.path.join(font_dir, "bootstrap-icons/font/fonts")
woff_path: str = os.path.join(woff_dir, "bootstrap-icons.woff")
ttf_path: str = os.path.join(font_dir, "bootstrap-icons.ttf")

subset_font(woff_path, ttf_path, include=[0xF7BF, 0xF7BF, 0xF7BF])
