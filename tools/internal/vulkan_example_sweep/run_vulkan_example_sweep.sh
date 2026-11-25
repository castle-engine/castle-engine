#!/usr/bin/env bash
set -euo pipefail

# ----------------------------------------------------------------------------
# Automated --render-backend=vulkan smoke & screenshot sweep across unmodified
# Castle Game Engine example projects in:
#   - examples/animations
#   - examples/shaders
#   - examples/viewport_and_scenes
#   - examples/physics
# ----------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
cd "${REPO_ROOT}"

WINDOW_BACKEND=""
COMPILE_ONLY="false"
CLEAN_AFTER="false"
AUTO_EXIT_FRAMES="${CASTLE_SWEEP_FRAMES:-3}"

for arg in "$@"; do
  case "${arg}" in
    --window-backend=*)
      WINDOW_BACKEND="${arg#*=}"
      ;;
    --compile-only)
      COMPILE_ONLY="true"
      ;;
    --clean-after)
      CLEAN_AFTER="true"
      ;;
    --frames=*)
      AUTO_EXIT_FRAMES="${arg#*=}"
      ;;
    *)
      echo "Unknown option: ${arg}" >&2
      exit 1
      ;;
  esac
done

if [ -z "${WINDOW_BACKEND}" ]; then
  case "$(uname -s)" in
    Darwin*)
      WINDOW_BACKEND="cocoa"
      ;;
    MINGW*|MSYS*|CYGWIN*)
      WINDOW_BACKEND="winapi"
      ;;
    *)
      WINDOW_BACKEND="xlib"
      ;;
  esac
fi

case "${WINDOW_BACKEND}" in
  xlib)
    COMPILER_WINDOW_DEF="-dCASTLE_WINDOW_XLIB"
    ;;
  cocoa)
    COMPILER_WINDOW_DEF="-dCASTLE_WINDOW_COCOA"
    ;;
  winapi)
    COMPILER_WINDOW_DEF="-dCASTLE_WINDOW_WINAPI"
    ;;
  *)
    echo "Unsupported --window-backend=${WINDOW_BACKEND} (expected xlib, cocoa, or winapi)" >&2
    exit 1
    ;;
esac

CGE_TOOL="${CASTLE_BUILD_TOOL:-}"
if [ -z "${CGE_TOOL}" ]; then
  case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*)
      if [ -x "./tools/build-tool/castle-engine.exe" ]; then
        CGE_TOOL="./tools/build-tool/castle-engine.exe"
      fi
      ;;
    *)
      if [ -x "./tools/build-tool/castle-engine" ]; then
        CGE_TOOL="./tools/build-tool/castle-engine"
      elif [ -x "./tools/build-tool/castle-engine.exe" ]; then
        CGE_TOOL="./tools/build-tool/castle-engine.exe"
      fi
      ;;
  esac
fi
if [ -z "${CGE_TOOL}" ] || [ ! -x "${CGE_TOOL}" ]; then
  echo "castle-engine build tool binary not found under ./tools/build-tool/" >&2
  exit 1
fi

SWEEP_TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/cge-vulkan-sweep.XXXXXX")"
cleanup_tmp() {
  rm -rf "${SWEEP_TMP_DIR}"
}
trap cleanup_tmp EXIT

EXAMPLES=(
  "examples/animations/animate_bones_by_code"
  "examples/animations/simultaneous_animations_one_scene"
  "examples/shaders/shader_effects_simple"
  "examples/shaders/shader_effects_on_lights"
  "examples/viewport_and_scenes/bump_mapping"
  "examples/viewport_and_scenes/multiple_viewports"
  "examples/physics/physics_3d_demo"
  "examples/physics/physics_2d_collisions"
)

export CASTLE_VULKAN_VALIDATION="${CASTLE_VULKAN_VALIDATION:-1}"

echo "=== Castle Game Engine Vulkan Example Catalog Sweep ==="
echo "Window backend : ${WINDOW_BACKEND} (${COMPILER_WINDOW_DEF})"
echo "Compile only   : ${COMPILE_ONLY}"
echo "Frames / run   : ${AUTO_EXIT_FRAMES}"
echo "Validation     : CASTLE_VULKAN_VALIDATION=${CASTLE_VULKAN_VALIDATION}"

PASS_COUNT=0
for proj in "${EXAMPLES[@]}"; do
  slug="$(echo "${proj}" | tr '/' '_')"
  shot_path="${SWEEP_TMP_DIR}/${slug}_vulkan.png"
  echo "--- [${proj}] Compiling..."
  "${CGE_TOOL}" --project "./${proj}/" compile "--compiler-option=${COMPILER_WINDOW_DEF}"

  if [ "${COMPILE_ONLY}" = "false" ]; then
    echo "--- [${proj}] Running --render-backend=vulkan --auto-exit-frames=${AUTO_EXIT_FRAMES}..."
    "${CGE_TOOL}" --project "./${proj}/" run -- \
      --render-backend=vulkan \
      --no-sound \
      "--auto-exit-frames=${AUTO_EXIT_FRAMES}" \
      "--capture-screenshot=${shot_path}"

    if [ ! -s "${shot_path}" ]; then
      echo "ERROR: Expected non-empty screenshot missing for ${proj}: ${shot_path}" >&2
      exit 1
    fi
    shot_bytes="$(wc -c < "${shot_path}" | tr -d '[:space:]')"
    if [ "${shot_bytes}" -lt 256 ]; then
      echo "ERROR: Screenshot too small (${shot_bytes} bytes) for ${proj}" >&2
      exit 1
    fi
    echo "--- [${proj}] OK (screenshot: ${shot_bytes} bytes)"
  fi

  if [ "${CLEAN_AFTER}" = "true" ]; then
    "${CGE_TOOL}" --project "./${proj}/" clean
  fi

  PASS_COUNT=$((PASS_COUNT + 1))
done

echo "=== Vulkan Example Catalog Sweep PASSED (${PASS_COUNT}/${#EXAMPLES[@]} projects) ==="
