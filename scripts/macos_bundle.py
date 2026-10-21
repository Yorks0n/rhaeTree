#!/usr/bin/env python3
"""Bundle and validate the complete Mach-O dependency graph using Apple tools."""

import argparse
from dataclasses import dataclass
from pathlib import Path
import plistlib
import re
import shutil
import subprocess
import sys


@dataclass
class Image:
    dependencies: list[str]
    rpaths: list[str]
    minimum: tuple[int, int, int]
    architectures: set[str]


def command(*args: str) -> str:
    return subprocess.check_output(args, text=True)


def version(value: str) -> tuple[int, int, int]:
    if not re.fullmatch(r"\d+(?:\.\d+){0,2}", value):
        raise ValueError(f"Invalid macOS version: {value}")
    parts = [int(part) for part in value.split(".")]
    return tuple((parts + [0, 0])[:3])


def version_text(value: tuple[int, int, int]) -> str:
    return ".".join(str(part) for part in value[: 3 if value[2] else 2])


def inspect(path: Path) -> Image:
    dependencies = []
    rpaths = []
    minimum = None
    load_command = ""
    for line in command("otool", "-l", str(path)).splitlines():
        line = line.strip()
        if line.startswith("cmd "):
            load_command = line.split()[1]
        elif line.startswith("name ") and load_command in {
            "LC_LOAD_DYLIB",
            "LC_LOAD_WEAK_DYLIB",
            "LC_REEXPORT_DYLIB",
            "LC_LOAD_UPWARD_DYLIB",
        }:
            dependencies.append(line[5:].rsplit(" (offset ", 1)[0])
        elif line.startswith("path ") and load_command == "LC_RPATH":
            rpaths.append(line[5:].rsplit(" (offset ", 1)[0])
        elif line.startswith("minos ") and load_command == "LC_BUILD_VERSION":
            minimum = version(line.split()[1])
        elif line.startswith("version ") and load_command == "LC_VERSION_MIN_MACOSX":
            minimum = version(line.split()[1])
    if minimum is None:
        raise ValueError(f"No macOS deployment target in {path}")
    architectures = set(command("lipo", "-archs", str(path)).split())
    return Image(dependencies, rpaths, minimum, architectures)


def system_library(name: str) -> bool:
    return name.startswith(("/System/Library/", "/usr/lib/"))


def expand_path(name: str, loader: Path, executable: Path) -> Path:
    for token, base in (
        ("@loader_path", loader.parent),
        ("@executable_path", executable.parent),
    ):
        if name == token or name.startswith(token + "/"):
            return base / name[len(token) :].lstrip("/")
    if not name.startswith("/"):
        raise ValueError(f"Unsupported dependency/search path {name} in {loader}")
    return Path(name)


def resolve(name: str, loader: Path, executable: Path, rpaths: list[Path]) -> Path:
    if name.startswith("@rpath/"):
        candidates = [root / name[len("@rpath/") :] for root in rpaths]
    else:
        candidates = [expand_path(name, loader, executable)]
    for candidate in candidates:
        if candidate.is_file():
            return candidate.resolve()
    searched = ", ".join(str(path) for path in candidates) or "(no LC_RPATH)"
    raise ValueError(f"Unresolved dependency {name} in {loader}; searched: {searched}")


def check_architectures(path: Path, image: Image, required: set[str]) -> None:
    if not required.issubset(image.architectures):
        raise ValueError(f"{path} lacks required architectures: {sorted(required)}")


def bundle(binary: Path, executable: Path, frameworks: Path, minimum: str) -> str:
    binary = binary.resolve()
    executable = executable.resolve()
    frameworks = frameworks.resolve()
    root = inspect(binary)
    images = {}
    destinations = {binary: executable}
    names = {}
    queue = [(binary, [])]
    # Resolve against original loader locations before copying or rewriting anything.
    while queue:
        source, inherited = queue.pop(0)
        if source in images:
            continue
        image = inspect(source)
        check_architectures(source, image, root.architectures)
        rpaths = [expand_path(path, source, binary) for path in image.rpaths] + inherited
        edges = {}
        for name in image.dependencies:
            if system_library(name):
                continue
            dependency = resolve(name, source, binary, rpaths)
            if dependency.suffix != ".dylib":
                raise ValueError(f"Unsupported non-system dependency: {dependency}")
            previous = names.setdefault(dependency.name, dependency)
            if previous != dependency:
                raise ValueError(f"Conflicting library names: {previous} and {dependency}")
            destinations[dependency] = frameworks / dependency.name
            edges[name] = dependency
            queue.append((dependency, rpaths))
        images[source] = (image, edges)

    required = max((12, 0, 0), *(image.minimum for image, _ in images.values()))
    selected = required if minimum == "auto" else version(minimum)
    if selected < required:
        raise ValueError(
            f"Requested macOS {version_text(selected)}, but bundled code requires "
            f"{version_text(required)}. Rebuild native dependencies for the older target."
        )
    frameworks.mkdir(parents=True, exist_ok=True)
    for source, destination in destinations.items():
        if source != binary:
            shutil.copy2(source, destination)
            destination.chmod(0o755)
            subprocess.run(
                ["install_name_tool", "-id", f"@executable_path/../Frameworks/{destination.name}",
                 str(destination)],
                check=True,
            )
    for source, (_, edges) in images.items():
        for name, dependency in edges.items():
            subprocess.run(
                ["install_name_tool", "-change", name,
                 f"@executable_path/../Frameworks/{destinations[dependency].name}",
                 str(destinations[source])],
                check=True,
            )
    print(f"Bundled {len(destinations) - 1} libraries; minimum macOS "
          f"{version_text(selected)}", file=sys.stderr)
    return version_text(selected)


def verify(app: Path) -> None:
    app = app.resolve()
    with (app / "Contents/Info.plist").open("rb") as stream:
        info = plistlib.load(stream)
    executable = app / "Contents/MacOS" / info["CFBundleExecutable"]
    minimum = version(info["LSMinimumSystemVersion"])
    required = inspect(executable).architectures
    files = [executable, *sorted((app / "Contents/Frameworks").glob("*.dylib"))]
    for path in files:
        image = inspect(path)
        check_architectures(path, image, required)
        if image.minimum > minimum:
            raise ValueError(f"{path} requires macOS {version_text(image.minimum)}, "
                             f"above declared {version_text(minimum)}")
        for name in image.dependencies:
            if system_library(name):
                continue
            if not name.startswith(("@executable_path/", "@loader_path/")):
                raise ValueError(f"Non-relocatable dependency {name} in {path}")
            target = expand_path(name, path, executable).resolve()
            if not target.is_relative_to(app) or not target.is_file():
                raise ValueError(f"Missing or external dependency {name} in {path}")
    print(f"Verified {len(files)} Mach-O files in {app}", file=sys.stderr)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    build = commands.add_parser("bundle")
    build.add_argument("--binary", type=Path, required=True)
    build.add_argument("--executable", type=Path, required=True)
    build.add_argument("--frameworks", type=Path, required=True)
    build.add_argument("--min-macos", default="auto")
    validate = commands.add_parser("verify")
    validate.add_argument("app", type=Path)
    args = parser.parse_args()
    try:
        if args.command == "bundle":
            print(bundle(args.binary, args.executable, args.frameworks, args.min_macos))
        else:
            verify(args.app)
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"macOS bundle error: {error}\n")


if __name__ == "__main__":
    main()
