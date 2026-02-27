#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Build a macOS .app bundle for rhaeTree.

Usage:
  scripts/build_macos_app.sh [options]

Options:
  --debug                 Build debug profile (default: release)
  --bundle-id ID          CFBundleIdentifier (default: com.yourorg.rhaetree)
  --version VER           CFBundleVersion/CFBundleShortVersionString (default: 1.0.0)
  --app-name NAME         App bundle name and executable name (default: rhaeTree)
  --binary-path PATH      Use an existing binary instead of running cargo build
  --output-dir DIR        Output directory for .app (default: <repo>/dist)
  --icon-png PATH         PNG source for icon generation (default: <repo>/icon/icon_1024x1024.png)
  --min-macos VER         LSMinimumSystemVersion (default: 12.0)
  --codesign-identity ID  Code signing identity (default: - for ad-hoc signing)
  -h, --help              Show this help

Examples:
  scripts/build_macos_app.sh
  scripts/build_macos_app.sh --bundle-id com.acme.rhaetree --version 0.1.0
EOF
}

if [[ "${OSTYPE:-}" != darwin* ]]; then
  echo "This script only supports macOS."
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

PROFILE="release"
BUNDLE_ID="com.yourorg.rhaetree"
APP_VERSION="1.0.0"
APP_NAME="rhaeTree"
OUTPUT_DIR="${ROOT_DIR}/dist"
BIN_PATH=""
if [[ -f "${ROOT_DIR}/icon/icon_1024x1024.png" ]]; then
  ICON_PNG="${ROOT_DIR}/icon/icon_1024x1024.png"
else
  ICON_PNG="${ROOT_DIR}/icon/icon.png"
fi
MIN_MACOS="12.0"
CODESIGN_IDENTITY="-"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --debug)
      PROFILE="debug"
      shift
      ;;
    --bundle-id)
      BUNDLE_ID="$2"
      shift 2
      ;;
    --version)
      APP_VERSION="$2"
      shift 2
      ;;
    --app-name)
      APP_NAME="$2"
      shift 2
      ;;
    --binary-path)
      BIN_PATH="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --icon-png)
      ICON_PNG="$2"
      shift 2
      ;;
    --min-macos)
      MIN_MACOS="$2"
      shift 2
      ;;
    --codesign-identity)
      CODESIGN_IDENTITY="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1"
      usage
      exit 1
      ;;
  esac
done

if [[ -z "${BIN_PATH}" ]]; then
  BIN_PATH="${ROOT_DIR}/target/${PROFILE}/${APP_NAME}"
fi
APP_BUNDLE="${OUTPUT_DIR}/${APP_NAME}.app"
CONTENTS_DIR="${APP_BUNDLE}/Contents"
MACOS_DIR="${CONTENTS_DIR}/MacOS"
RESOURCES_DIR="${CONTENTS_DIR}/Resources"
FRAMEWORKS_DIR="${CONTENTS_DIR}/Frameworks"
PLIST_PATH="${CONTENTS_DIR}/Info.plist"
ICON_BASENAME="rhaetree"
ICON_ICNS_PATH="${RESOURCES_DIR}/${ICON_BASENAME}.icns"
ICONSET_PATH="${ROOT_DIR}/icon/${ICON_BASENAME}.iconset"
UTI_ID="${BUNDLE_ID}.tree"

if [[ -z "${BIN_PATH:-}" || "${BIN_PATH}" == "${ROOT_DIR}/target/${PROFILE}/${APP_NAME}" ]]; then
  echo "Building ${APP_NAME} (${PROFILE})..."
  if [[ "${PROFILE}" == "release" ]]; then
    cargo build --release
  else
    cargo build
  fi
else
  echo "Using existing binary: ${BIN_PATH}"
fi

if [[ ! -f "${BIN_PATH}" ]]; then
  echo "Expected binary not found: ${BIN_PATH}"
  exit 1
fi

echo "Creating bundle: ${APP_BUNDLE}"
rm -rf "${APP_BUNDLE}"
mkdir -p "${MACOS_DIR}" "${RESOURCES_DIR}" "${FRAMEWORKS_DIR}"
cp "${BIN_PATH}" "${MACOS_DIR}/${APP_NAME}"
chmod +x "${MACOS_DIR}/${APP_NAME}"

bundle_homebrew_dylibs() {
  local app_binary="$1"
  local -a queue=("${app_binary}")
  local processed="|"
  local dep
  local file
  local src
  local base
  local dst
  local new_ref

  while ((${#queue[@]})); do
    file="${queue[0]}"
    queue=("${queue[@]:1}")

    if [[ "${processed}" == *"|${file}|"* ]]; then
      continue
    fi
    processed="${processed}${file}|"

    while IFS= read -r dep; do
      if [[ "${dep}" != /opt/homebrew/* && "${dep}" != /usr/local/* ]]; then
        continue
      fi
      if [[ "${dep}" != *.dylib ]]; then
        continue
      fi
      if [[ ! -f "${dep}" ]]; then
        echo "Warning: dependent dylib not found: ${dep}"
        continue
      fi

      src="${dep}"
      base="$(basename "${src}")"
      dst="${FRAMEWORKS_DIR}/${base}"
      new_ref="@executable_path/../Frameworks/${base}"

      if [[ ! -f "${dst}" ]]; then
        cp "${src}" "${dst}"
        chmod 755 "${dst}"
        install_name_tool -id "${new_ref}" "${dst}"
        queue+=("${dst}")
      fi

      install_name_tool -change "${src}" "${new_ref}" "${file}"
    done < <(otool -L "${file}" | tail -n +2 | awk '{print $1}')
  done
}

echo "Bundling Homebrew dynamic libraries..."
bundle_homebrew_dylibs "${MACOS_DIR}/${APP_NAME}"

echo "Writing Info.plist..."
cat > "${PLIST_PATH}" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleInfoDictionaryVersion</key><string>6.0</string>
  <key>CFBundleName</key><string>${APP_NAME}</string>
  <key>CFBundleDisplayName</key><string>${APP_NAME}</string>
  <key>CFBundleIdentifier</key><string>${BUNDLE_ID}</string>
  <key>CFBundleVersion</key><string>${APP_VERSION}</string>
  <key>CFBundleShortVersionString</key><string>${APP_VERSION}</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleExecutable</key><string>${APP_NAME}</string>
  <key>CFBundleIconFile</key><string>${ICON_BASENAME}</string>
  <key>LSMinimumSystemVersion</key><string>${MIN_MACOS}</string>
  <key>NSHighResolutionCapable</key><true/>

  <key>CFBundleDocumentTypes</key>
  <array>
    <dict>
      <key>CFBundleTypeName</key><string>Phylogenetic Tree File</string>
      <key>CFBundleTypeRole</key><string>Viewer</string>
      <key>LSHandlerRank</key><string>Owner</string>
      <key>LSItemContentTypes</key>
      <array>
        <string>${UTI_ID}</string>
      </array>
    </dict>
  </array>

  <key>UTExportedTypeDeclarations</key>
  <array>
    <dict>
      <key>UTTypeIdentifier</key><string>${UTI_ID}</string>
      <key>UTTypeDescription</key><string>Phylogenetic Tree File</string>
      <key>UTTypeConformsTo</key>
      <array>
        <string>public.data</string>
      </array>
      <key>UTTypeTagSpecification</key>
      <dict>
        <key>public.filename-extension</key>
        <array>
          <string>tre</string>
          <string>tree</string>
          <string>trees</string>
          <string>nex</string>
          <string>nexus</string>
          <string>newick</string>
          <string>nwk</string>
          <string>rtr</string>
        </array>
      </dict>
    </dict>
  </array>
</dict>
</plist>
EOF

if [[ -f "${ICON_PNG}" ]] && command -v sips >/dev/null 2>&1; then
  echo "Generating icon from: ${ICON_PNG}"
  rm -rf "${ICONSET_PATH}"
  mkdir -p "${ICONSET_PATH}"

  if sips -z 16 16     "${ICON_PNG}" --out "${ICONSET_PATH}/icon_16x16.png" >/dev/null \
    && sips -z 32 32     "${ICON_PNG}" --out "${ICONSET_PATH}/icon_16x16@2x.png" >/dev/null \
    && sips -z 32 32     "${ICON_PNG}" --out "${ICONSET_PATH}/icon_32x32.png" >/dev/null \
    && sips -z 64 64     "${ICON_PNG}" --out "${ICONSET_PATH}/icon_32x32@2x.png" >/dev/null \
    && sips -z 128 128   "${ICON_PNG}" --out "${ICONSET_PATH}/icon_128x128.png" >/dev/null \
    && sips -z 256 256   "${ICON_PNG}" --out "${ICONSET_PATH}/icon_128x128@2x.png" >/dev/null \
    && sips -z 256 256   "${ICON_PNG}" --out "${ICONSET_PATH}/icon_256x256.png" >/dev/null \
    && sips -z 512 512   "${ICON_PNG}" --out "${ICONSET_PATH}/icon_256x256@2x.png" >/dev/null \
    && sips -z 512 512   "${ICON_PNG}" --out "${ICONSET_PATH}/icon_512x512.png" >/dev/null \
    && sips -z 1024 1024 "${ICON_PNG}" --out "${ICONSET_PATH}/icon_512x512@2x.png" >/dev/null; then
    echo "Iconset generated: ${ICONSET_PATH}"
  else
    echo "Warning: iconset generation failed; app will use default icon."
  fi

  if [[ -d "${ICONSET_PATH}" ]] && command -v iconutil >/dev/null 2>&1; then
    if iconutil -c icns "${ICONSET_PATH}" -o "${ICON_ICNS_PATH}"; then
      echo "Icon generated: ${ICON_ICNS_PATH}"
    else
      echo "Warning: iconutil failed; keeping iconset at ${ICONSET_PATH}"
    fi
  fi
else
  echo "Icon tools or source icon missing; skipping .icns generation."
fi

if command -v codesign >/dev/null 2>&1; then
  echo "Signing app bundle with identity: ${CODESIGN_IDENTITY}"
  if [[ -d "${FRAMEWORKS_DIR}" ]]; then
    find "${FRAMEWORKS_DIR}" -type f -name "*.dylib" -print0 | while IFS= read -r -d '' lib; do
      codesign --force --sign "${CODESIGN_IDENTITY}" "$lib"
    done
  fi
  codesign --force --sign "${CODESIGN_IDENTITY}" "${MACOS_DIR}/${APP_NAME}"
  codesign --force --sign "${CODESIGN_IDENTITY}" "${APP_BUNDLE}"
fi

echo "Done."
echo "Bundle: ${APP_BUNDLE}"
echo "Test open:"
echo "  open \"${APP_BUNDLE}\""
echo "  open -a \"${APP_BUNDLE}\" \"${ROOT_DIR}/examples/FigTree.tre\""
