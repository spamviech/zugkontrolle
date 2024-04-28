#!/usr/bin/python3

import os
import os.path
import shutil
import urllib.request
import urllib.error

script_name=os.path.splitext(os.path.basename(__file__))[0]
licenses_dir=os.path.dirname(os.path.abspath(__file__))

cargo_dir = os.path.join(os.path.expanduser("~"), ".cargo")
# hash(probably) at the end might change, possibly with cargo update
# crate-name followed by a `-` and the crate-version, e.g. "ab_glyph_rasterizer-0.1.5"
crates_io_dir = os.path.join(cargo_dir, "registry", "src", "index.crates.io-6f17d22bba15001f")
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
    "license",
    "licence",
    "license-mit",
    "license-apache-2.0_with_llvm-exception",
    "license-apache",
    "license-zlib"
    "unlicense",
    "copying",
    "notice",
}
license_exts = {"", ".md", ".txt"}
license_files = {root + ext for root in license_roots for ext in license_exts}
cargo_toml = {"Cargo.toml", "Cargo.toml.orig"}
readme = {"README.md"}

def log(message, log_file):
    print(message, end='\n')
    if log_file is not None:
        log_file.write(message)
        log_file.write('\n')

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
    with open(os.path.join(licenses_dir, "../..", "Cargo.lock"), 'r') as cargo_lock:
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

def extract_repository_and_license(cargo_toml_path, log_file):
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
                            log(f"Overwrite duplicate license entry: {license}", log_file)
                        license = line.removeprefix(license_prefix).strip('"')
                    elif line.startswith("["):
                        break
    else:
        log("Missing Cargo.toml", log_file)
        log(f"\t{cargo_toml_path}", log_file)
    return repository, license

def make_https(url):
    http = "http://"
    if url is not None and url.startswith(http):
        return "https://" + url.removeprefix(http)
    else:
        return url

def download_licenses(repository, dst_dir, log_file):
    github_prefix = "https://github.com"
    raw_prefix = "https://raw.githubusercontent.com"
    https_repository = make_https(repository)
    found = False
    if (https_repository is not None) and https_repository.startswith(github_prefix):
        tree_start = https_repository.find("/tree/")
        if tree_start > -1:
            base_repository = https_repository[:tree_start]
        else:
            base_repository = https_repository
        raw_repository = raw_prefix + base_repository.removeprefix(github_prefix)
        for branch in ["master", "main"]:
            branch_url = f"{base_repository}/tree/{branch}"
            try:
                urllib.request.urlopen(branch_url)
            except urllib.error.URLError:
                # branch existiert nicht
                continue
            raw_branch_repository = f"{raw_repository}/{branch}"
            for license in license_files:
                url = raw_branch_repository + "/" + license
                try:
                    root, ext = os.path.splitext(license)
                    file_path = os.path.join(dst_dir, f"{root}-GITHUB{ext}")
                    with urllib.request.urlopen(url) as remote:
                        with open(file_path, 'wb') as f:
                            f.write(remote.read())
                        found = True
                except urllib.error.URLError:
                    pass
    else:
        log(f"Non-GitHub-Repository: {repository}", log_file)
    return found

copy_filenames = license_files | cargo_toml | readme
def copy_licenses(src_dir, dst_dir, log_file):
    global copy_filenames
    # kopiere gefundene Dateien und überprüfe, ob eine Lizenz-Datei gefunden wurde
    os.makedirs(dst_dir, exist_ok=True)
    found = False
    for filename in copy_filenames:
        src = os.path.join(src_dir, filename)
        if os.path.isfile(src):
            if filename in license_files:
                found = True
            dst = os.path.join(dst_dir, filename)
            shutil.copy(src, dst)
    # suche OFL-Lizenz-Datei in einem fonts-Unterordner
    ofl_font_license = "fonts/OFL.txt"
    src = os.path.join(src_dir, ofl_font_license)
    if os.path.isfile(src):
        os.makedirs(os.path.join(dst_dir, "fonts"), exist_ok=True)
        dst = os.path.join(dst_dir, ofl_font_license)
        shutil.copy(src, dst)
    # suche github, sofern keine Lizenz-Datei gefunden wurde
    if not found:
        cargo_toml_path = os.path.join(src_dir, "Cargo.toml")
        repository, license = extract_repository_and_license(cargo_toml_path, log_file)
        found = download_licenses(repository, dst_dir, log_file)
    # Rückmeldung, falls auch im github-Repository keine Lizenz-Datei gefunden wurde
    if not found:
        log(f"Missing License: {dst_dir}", log_file)
        log(f"\tRepository: {repository}", log_file)
        log(f"\tAnnounced License: {license}", log_file)

def copy_or_download_licenses(show_percent = 5):
    packages = collect_cargo_lock_packages()
    i = 0
    l = len(packages)
    step = int(show_percent * l / 100)
    with open(os.path.join(os.path.join(licenses_dir, f"{script_name}.log")), 'w') as log_file:
        log(f"Fetch {l} licenses...", log_file)
        for package in packages:
            name_and_version = f"{package.name}-{package.version}"
            if package.git is None:
                log(f"Skip {name_and_version}", log_file)
            elif package.git:
                for dir in os.listdir(git_dir):
                    if dir.startswith(package.name):
                        dir_path = os.path.join(git_dir, dir)
                        for rev in os.listdir(dir_path):
                            os.path.join(dir_path, rev)
                            target_dir = os.path.join(licenses_dir, name_and_version, rev)
                            copy_licenses(src_dir, target_dir, log_file)
            else:
                src_dir = os.path.join(crates_io_dir, name_and_version)
                target_dir = os.path.join(licenses_dir, name_and_version)
                copy_licenses(src_dir, target_dir, log_file)
            i += 1
            if i % step == 0:
                log(f"{i} / {l}: {name_and_version}", log_file)

if __name__ == "__main__":
    copy_or_download_licenses()
