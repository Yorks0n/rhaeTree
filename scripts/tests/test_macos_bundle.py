"""Integration tests using small locally compiled Mach-O fixtures."""

from pathlib import Path
import plistlib
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import macos_bundle


@unittest.skipUnless(sys.platform == "darwin" and shutil.which("clang"),
                     "Requires macOS and Apple command line tools")
class BundleTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="macos-bundle-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.lib = self.root / "vendor/lib"
        self.lib.mkdir(parents=True)
        self.app = self.root / "Test.app"
        self.executable = self.app / "Contents/MacOS/Test"
        self.frameworks = self.app / "Contents/Frameworks"
        self.executable.parent.mkdir(parents=True)
        self.binary = self.root / "Test"
        self.compile("common.c", "int common(void) { return 0; }\n",
                     "-dynamiclib", "-install_name", "@rpath/libcommon.dylib",
                     "-o", str(self.lib / "libcommon.dylib"))
        self.compile("decoder.c", "int common(void); int decoder(void) { return common(); }\n",
                     "-dynamiclib", "-install_name", str(self.lib / "libdecoder.dylib"),
                     "-L" + str(self.lib), "-lcommon",
                     "-Wl,-rpath,@loader_path/../lib",
                     "-o", str(self.lib / "libdecoder.dylib"))
        self.compile("main.c", "int decoder(void); int main(void) { return decoder(); }\n",
                     "-L" + str(self.lib), "-ldecoder", "-o", str(self.binary))
        shutil.copy2(self.binary, self.executable)

    def compile(self, name, source, *args):
        path = self.root / name
        path.write_text(source)
        subprocess.run(["clang", "-Wl,-headerpad_max_install_names", str(path), *args],
                       check=True, capture_output=True)

    def package(self, minimum="auto"):
        selected = macos_bundle.bundle(
            self.binary, self.executable, self.frameworks, minimum
        )
        with (self.app / "Contents/Info.plist").open("wb") as stream:
            plistlib.dump({"CFBundleExecutable": "Test",
                          "LSMinimumSystemVersion": selected}, stream)
        return selected

    def test_transitive_rpath_dependency_is_bundled_and_relocatable(self):
        self.package()
        self.assertTrue((self.frameworks / "libcommon.dylib").is_file())
        # Removing the original libraries proves the bundle is self-contained.
        shutil.rmtree(self.root / "vendor")
        macos_bundle.verify(self.app)
        for path in [*self.frameworks.glob("*.dylib"), self.executable]:
            subprocess.run(["codesign", "--force", "--sign", "-", str(path)],
                           check=True, capture_output=True)
        subprocess.run([str(self.executable)], check=True, capture_output=True)

    def test_missing_transitive_library_fails_packaging(self):
        (self.lib / "libcommon.dylib").unlink()
        with self.assertRaisesRegex(ValueError, "Unresolved dependency @rpath/libcommon"):
            self.package()

    def test_loader_relative_dependency_is_bundled(self):
        subprocess.run(
            ["install_name_tool", "-change", "@rpath/libcommon.dylib",
             "@loader_path/libcommon.dylib", str(self.lib / "libdecoder.dylib")],
            check=True, capture_output=True,
        )
        self.package()
        macos_bundle.verify(self.app)

    def test_dependency_can_inherit_executable_rpath(self):
        subprocess.run(
            ["install_name_tool", "-delete_rpath", "@loader_path/../lib",
             str(self.lib / "libdecoder.dylib")],
            check=True, capture_output=True,
        )
        subprocess.run(
            ["install_name_tool", "-add_rpath", str(self.lib), str(self.binary)],
            check=True, capture_output=True,
        )
        shutil.copy2(self.binary, self.executable)
        self.package()
        macos_bundle.verify(self.app)

    def test_minimum_version_cannot_understate_dependencies(self):
        with self.assertRaisesRegex(ValueError, "bundled code requires"):
            self.package("10.0")

    def test_final_verifier_rejects_missing_library(self):
        self.package()
        (self.frameworks / "libcommon.dylib").unlink()
        with self.assertRaisesRegex(ValueError, "Missing or external dependency"):
            macos_bundle.verify(self.app)

    def test_final_verifier_rejects_incorrect_minimum(self):
        self.package()
        with (self.app / "Contents/Info.plist").open("wb") as stream:
            plistlib.dump({"CFBundleExecutable": "Test",
                          "LSMinimumSystemVersion": "10.0"}, stream)
        with self.assertRaisesRegex(ValueError, "above declared"):
            macos_bundle.verify(self.app)

    def test_shell_builder_produces_valid_bundle(self):
        script = Path(__file__).resolve().parents[1] / "build_macos_app.sh"
        destination = self.root / "distribution"
        subprocess.run(
            ["bash", str(script), "--binary-path", str(self.binary),
             "--output-dir", str(destination), "--app-name", "Test",
             "--icon-png", str(self.root / "no-icon.png"), "--no-lsregister"],
            check=True, capture_output=True, text=True,
        )
        macos_bundle.verify(destination / "Test.app")


if __name__ == "__main__":
    unittest.main()
