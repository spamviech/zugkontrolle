#!/bin/python3

import os.path

import util
from woff2otf import woff2otf
from nototools.nototools.subset import subset_font

repo_dir: str = util.get_repo_root()
font_dir: str = os.path.join(repo_dir, "fonts")
woff_dir: str = os.path.join(font_dir, "bootstrap-icons/font/fonts")
woff_path: str = os.path.join(woff_dir, "bootstrap-icons.woff")
otf_path: str = os.path.join(font_dir, "bootstrap-icons.otf")

woff2otf.convert(woff_path, otf_path)
