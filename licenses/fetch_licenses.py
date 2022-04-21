import os
import os.path
import shutil

cargo_dir = os.path.join(os.path.expanduser("~"), ".cargo")
# hash(probably) at the end might change, possibly with cargo update
# crate-name followed by a `-` and the crate-version, e.g. "ab_glyph_rasterizer-0.1.5"
crates_io_dir = os.path.join(cargo_dir, "registry", "src", "github.com-1ecc6299db9ec823")
# crate-name followed by a `-` and some hash(probably), inside folder with the commit-hash (abbreviated)
git_dir = os.path.join(cargo_dir, "git", "checkouts")

license_files = [
    "LICENSE",
    "LICENSE.md",
    "LICENSE.txt",
    "LICENSE-MIT",
    "LICENSE-Apache-2.0_WITH_LLVM-exception",
    "LICENSE-APACHE",
    "UNLICENSE",
    "COPYING",
    "NOTICE",
]
cargo_toml = ["Cargo.toml", "Cargo.toml.orig"]
readme = ["README.md"]

name_prefix = "name = "
version_prefix = "version = "
source_prefix = "source = "

packages = []
name = None
version = None
git = None
def next_package():
    global name, version, git
    if name is not None:
        packages.append((name, version, git))
    name = None
    version = None
    git = None

with open(os.path.join("..", "Cargo.lock"), 'r') as cargo_lock:
    found_package = False
    for line in cargo_lock:
        line = line.removesuffix("\n").removesuffix("\r").removesuffix("\n\r")
        if line == "[[package]]":
            next_package()
        elif line.startswith(name_prefix):
            suffix = line[len(name_prefix):]
            name = suffix.strip('"')
        elif line.startswith(version_prefix):
            suffix = line[len(version_prefix):]
            version = suffix.strip('"')
        elif line.startswith(source_prefix):
            suffix = line[len(source_prefix):]
            source = suffix.strip('"')
            if source.startswith("registry"):
                git = False
            elif source.startswith("git"):
                git = True
next_package()

copy_filenames = license_files + cargo_toml
def copy_licenses(src_dir, dst_dir):
    global copy_filenames
    os.makedirs(dst_dir, exist_ok=True)
    for license in copy_filenames:
        src = os.path.join(src_dir, license)
        if os.path.isfile(src):
            dst = os.path.join(dst_dir, license)
            shutil.copy(src, dst)

for name, version, git in packages:
    if git is None:
        print(f"Skip {name}-{version}")
    elif git:
        dir_name = f"{name}-{version}"
        for dir in os.listdir(git_dir):
            if dir.startswith(name):
                dir_path = os.path.join(git_dir, dir)
                for rev in os.listdir(dir_path):
                    copy_licenses(os.path.join(dir_path, rev), os.path.join(dir_name, rev))
    else:
        dir_name = f"{name}-{version}"
        copy_licenses(os.path.join(crates_io_dir, dir_name), dir_name)
