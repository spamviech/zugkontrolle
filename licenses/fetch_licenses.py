import os
import os.path
import shutil
import urllib.request
import urllib.error

cargo_dir = os.path.join(os.path.expanduser("~"), ".cargo")
# hash(probably) at the end might change, possibly with cargo update
# crate-name followed by a `-` and the crate-version, e.g. "ab_glyph_rasterizer-0.1.5"
crates_io_dir = os.path.join(cargo_dir, "registry", "src", "github.com-1ecc6299db9ec823")
# crate-name followed by a `-` and some hash(probably), inside folder with the commit-hash (abbreviated)
git_dir = os.path.join(cargo_dir, "git", "checkouts")

license_roots = {
    "LICENSE",
    "LICENCE",
    "LICENSE-MIT",
    "LICENSE-Apache-2.0_WITH_LLVM-exception",
    "LICENSE-APACHE",
    "LICENSE-ZLIB"
    "UNLICENSE",
    "COPYING",
    "NOTICE",
}
license_exts = {"", ".md", ".txt"}
license_files = {root + ext for root in license_roots for ext in license_exts}
cargo_toml = {"Cargo.toml", "Cargo.toml.orig"}
readme = {"README.md"}

class Package:
    def __init__(self):
        self.name = None
        self.version = None
        self.git = None

    def __str__(self):
        print(f"Package(name = {self.name}, version = {self.version}, git = {self.git})")

def append_package(packages, current):
    if current.name is not None:
        packages.append(current)

def collect_cargo_lock_packages():
    name_prefix = "name = "
    version_prefix = "version = "
    source_prefix = "source = "

    packages = []
    current = Package()
    with open(os.path.join("..", "Cargo.lock"), 'r') as cargo_lock:
        found_package = False
        for line in cargo_lock:
            line = line.removesuffix("\n").removesuffix("\r").removesuffix("\n\r")
            if line == "[[package]]":
                append_package(packages, current)
                current = Package()
            elif line.startswith(name_prefix):
                suffix = line[len(name_prefix):]
                current.name = suffix.strip('"')
            elif line.startswith(version_prefix):
                suffix = line[len(version_prefix):]
                current.version = suffix.strip('"')
            elif line.startswith(source_prefix):
                suffix = line[len(source_prefix):]
                source = suffix.strip('"')
                if source.startswith("registry"):
                    current.git = False
                elif source.startswith("git"):
                    current.git = True
    append_package(packages, current)
    return packages

def extract_repository_and_license(cargo_toml_path):
    repository = None
    license = None
    # not a true toml parser, assume simplified Cargo.toml
    repository_prefix = "repository = "
    license_prefix = "license = "
    if os.path.isfile(cargo_toml_path):
        with open(cargo_toml_path, 'r') as f:
            package = False
            for line in f:
                line = line.removesuffix("\n").removesuffix("\r").removesuffix("\n\r")
                if line == "[package]":
                    package = True
                elif package:
                    if line.startswith(repository_prefix):
                        if repository is not None:
                            print(f"Overwrite duplicate repository entry: {repository}")
                        repository = line.removeprefix(repository_prefix).strip('"').removesuffix(".git")
                    if line.startswith(license_prefix):
                        if license is not None:
                            print(f"Overwrite duplicate license entry: {license}")
                        license = line.removeprefix(license_prefix).strip('"')
                    elif line.startswith("["):
                        break
    else:
        print(f"Cargo.toml not found in \"{cargo_toml_path}\"")
    return repository, license

def dowload_licenses(repository, dst_dir):
    github_prefix = "https://github.com"
    raw_prefix = "https://raw.githubusercontent.com"
    found = False
    if repository is not None and repository.startswith(github_prefix):
        raw_repository = raw_prefix + repository.removeprefix(github_prefix)
        for branch in ["master", "main"]:
            raw_branch_repository = raw_repository + "/" + branch
            for license in license_files:
                url = raw_branch_repository + "/" + license
                try:
                    root, ext = os.path.splitext(license)
                    file_path = os.path.join(dst_dir, f"{root}-GITHUB.{ext}")
                    with urllib.request.urlopen(url) as remote:
                        with open(file_path, 'wb') as f:
                            f.write(remote.read())
                        found = True
                except urllib.error.URLError as e:
                    pass
    return found

copy_filenames = license_files | cargo_toml | readme
def copy_licenses(src_dir, dst_dir):
    global copy_filenames
    os.makedirs(dst_dir, exist_ok=True)
    found = False
    for filename in copy_filenames:
        src = os.path.join(src_dir, filename)
        if os.path.isfile(src):
            if filename in license_files:
                found = True
            dst = os.path.join(dst_dir, filename)
            shutil.copy(src, dst)
    if not found:
        cargo_toml_path = os.path.join(src_dir, "Cargo.toml")
        repository, license = extract_repository_and_license(cargo_toml_path)
        found = dowload_licenses(repository, dst_dir)
    if not found:
        print(f"Missing License: {dst_dir}")
        print(f"\tRepository: {repository}")
        print(f"\tAnnounced License: {license}")

def copy_or_download_licenses(show_percent = 5):
    packages = collect_cargo_lock_packages()
    i = 0
    l = len(packages)
    step = int(show_percent * l / 100)
    for package in packages:
        name_and_version = f"{package.name}-{package.version}"
        if package.git is None:
            print(f"Skip {name_and_version}")
        elif package.git:
            for dir in os.listdir(git_dir):
                if dir.startswith(package.name):
                    dir_path = os.path.join(git_dir, dir)
                    for rev in os.listdir(dir_path):
                        copy_licenses(os.path.join(dir_path, rev), os.path.join(name_and_version, rev))
        else:
            copy_licenses(os.path.join(crates_io_dir, name_and_version), name_and_version)
        i += 1
        if i % step == 0:
            print(f"{i} / {l}")

if __name__ == "__main__":
    copy_or_download_licenses()
