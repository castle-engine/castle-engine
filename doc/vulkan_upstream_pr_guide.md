# Upstream Maintainer PR Review & Slicing Guide (`castle-engine/castle-engine`)

This guide documents how the `vulkan-renderer` branch (merge-base `9ba97754ec971ace540a7b478ebe61f974cc860b` on `castle-engine/castle-engine` `master`) is partitioned into **8 strictly acyclic, dependency-ordered pull request slices** so upstream maintainers can review, compile, and merge each subsystem incrementally with **zero risk to existing OpenGL / OpenGL ES users**.

---

## 1. Guiding Upstream Principles

1. **100% Default Backward Compatibility (`backendAuto` -> OpenGL)**:
   - `TCastleApplicationProperties.RenderBackend` defaults to `backendAuto`, which resolves to `rbOpenGL` / `rbOpenGLES` everywhere unless opted into `backendVulkan` via `CastleEngineManifest.xml`, `--render-backend=vulkan`, `CASTLE_RENDER_BACKEND=vulkan`, or Pascal code.
   - When `RHIDevice = nil` (default OpenGL path), all existing CGE OpenGL code paths execute unchanged.
2. **Strictly Acyclic Pascal `uses` Layering**:
   - Every `.pas` and `.inc` file in `Slices 1..8` satisfies a machine-verified topological invariant (`prepare_upstream_slices.sh --verify`): **no unit in `Slice K` ever `uses` a unit introduced or modified in `Slice M` (`M > K`)**.
3. **Mechanical `.dproj` Search-Path Noise Isolated**:
   - Adding `src/base_rendering/{rhi,vulkan,opengl}` touches `164` `examples/**/*.dproj` files by 1 line (`DCC_UnitSearchPath`) via `examples_regenerate_auto_files`.
   - In `slice_manifest.tsv`, those `164` files are tagged `auto-dproj` and exported into a separate `03b-examples-auto-dproj.patch` so reviewers of `Slice 3` only read the `28` core handwritten files (or regenerate `.dproj` files upstream via `examples_regenerate_auto_files`).
4. **Branch-Internal Handover Docs Excluded**:
   - `PLAN.md` and `AGENTS.md` are assigned to `Slice 0` (`branch-meta`) and excluded from upstream PRs `1..8`.

---

## 2. Dependency DAG & Overview of the 8 Upstream PR Slices

```mermaid
flowchart TD
  S1["PR #1: Vulkan 1.0-1.4 Pascal Bindings & vkxml2pas Generator (5 new files)"]
  S2["PR #2: RHI Core Abstraction, Shader Generator & Vulkan Backend (10 files)"]
  S3["PR #3: 2D/Low-Level Rendering Bridge, OpenGL RHI & Packages (28 core + 164 auto-dproj)"]
  S4["PR #4: 3D Scene, X3D/glTF Materials, Lighting, Shaders & Viewport (42 files)"]
  S5["PR #5: Pure-Vulkan TCastleWindow & Embedded TCastleControl WSI (19 files)"]
  S6["PR #6: castle-engine Build Tool Packaging & castle-editor Toggle (10 files)"]
  S7["PR #7: Dedicated Vulkan Examples & Catalog Sweep Runner (20 new files)"]
  S8["PR #8: TTestCastleVulkanRHI Suite, 3-OS CI Gate & Documentation (10 files)"]

  S1 --> S2 --> S3 --> S4 --> S5 --> S6 --> S7 --> S8
```

| PR Slice | Slug | Core Files | Auto `.dproj` | Existing Engine Files Modified? | Independent Verification |
| :---: | :--- | :---: | :---: | :--- | :--- |
| **PR #1** | `vulkan-bindings-generator` | `5` | `0` | **No (`0` modified)** — 100% additive | Compile & run `vkxml2pas.dpr`; compile `CastleInternalVulkan` & `CastleInternalRenderDoc` |
| **PR #2** | `rhi-core-and-vulkan-backend` | `10` | `0` | `4` (`src/base/castleapplicationproperties.pas`, `castlerenderoptions*`) | Compile `CastleInternalRHI`, `CastleInternalShaderRHI`, `CastleInternalVulkanMemory`, `CastleInternalVulkanWSI`, `CastleInternalShaderCompiler`, `CastleInternalRHI_Vulkan` |
| **PR #3** | `low-level-rendering-opengl-rhi-and-packages` | `28` | `164` | `26` (`castlerendercontext`, `castleglimages*`, `castleglutils*`, `castlefonts*`, packages) | Build `fpmake.pp`, `castle_engine_base.lpk`, `castle_engine.dpk`; run existing OpenGL test suite |
| **PR #4** | `scene-x3d-viewport-and-ui` | `42` | `0` | `42` (`src/transform/*`, `src/scene/*`, `src/ui/*`) | Compile full `TCastleViewport` + `TCastleScene` + `TCastleUserInterface` on FPC & Lazarus |
| **PR #5** | `window-and-embedded-controls-wsi` | `19` | `0` | `19` (`src/window/*`, `src/lcl/castlecontrol.pas`, `src/delphi/*`) | Compile `TCastleWindow` (Xlib/GTK/WinAPI/Cocoa/Android) & `castle_engine_lcl.lpk`; verify `--render-backend=vulkan` |
| **PR #6** | `build-tool-packaging-and-editor` | `10` | `0` | `10` (`tools/build-tool/*`, `tools/common-code/*`, `tools/castle-editor/*`) | Compile `castle-engine` CLI & `castle_editor.lpi`; test `bundle_vulkan_caches="true"` packaging |
| **PR #7** | `vulkan-examples-and-catalog-sweep` | `20` | `0` | **No (`0` modified)** — 100% additive | Run `vulkan_triangle`, `vulkan_sprite_batch`, `vulkan_3d_scene --headless` & `run_vulkan_example_sweep.sh` |
| **PR #8** | `tests-ci-and-documentation` | `10` | `0` | `3` (`tests/testcases.pas`, `CastleEngineManifest.xml`, `.lpi`) | Run full `castle_tester_standalone` (`391/391` `TTestCastleVulkanRHI` + 3-OS `vulkan-rhi.yml` CI) |

---

## 3. Detailed Per-Slice Scope & Maintainer Review Checklist

### PR #1 — `vulkan-bindings-generator` (`5` new files)
- **Files**:
  - `tools/internal/generate_vulkan_api/{vkxml2pas.dpr, CastleEngineManifest.xml, README.md}`
  - `src/base_rendering/vulkan/castleinternalvulkan.pas`
  - `src/base_rendering/rhi/castleinternalrenderdoc.pas`
- **What Reviewers Should Check**:
  - `castleinternalvulkan.pas` uses only `SysUtils`, `CastleDynLib`, and `CastleLog`, loading `vulkan-1.dll` / `libvulkan.so.1` / `libMoltenVK.dylib` lazily via `TDynLib` with zero link-time dependencies.
  - Flat C-ABI records (`{$PACKRECORDS C}`), 64-bit non-dispatchable handles (`UInt64` even on 32-bit ARM/x86 per Vulkan spec), and `stdcall`/`cdecl` `VKAPI_CALL` calling conventions.

### PR #2 — `rhi-core-and-vulkan-backend` (`10` files)
- **Files**:
  - Public enums/options: `src/base/castleapplicationproperties.pas` (`TRenderBackend`), `src/base/castlerenderoptions.pas` + `*.inc`.
  - New RHI & Vulkan units: `castleinternalrhi.pas`, `castleinternalshaderrhi.pas`, `castleinternalvulkanmemory.pas`, `castleinternalvulkanwsi.pas`, `castleinternalshadercompiler.pas`, `castleinternalrhi_vulkan.pas`.
- **What Reviewers Should Check**:
  - `ApplicationProperties.RenderBackend` defaults to `backendAuto` (preserving OpenGL).
  - `TCastleVulkanMemoryAllocator` sub-allocation pools (`64 MB` device-local / `16 MB` host-visible), `DefragmentPools`, and fence-synchronized deferred resource deletion (`FlushDeferredDeletions`).
  - `TCastleShaderCompiler` 3-tier strategy (`castle-data:/shader_cache/` -> `libshaderc` / `glslangValidator` -> 56 built-in SPIR-V fallback modules).

### PR #3 — `low-level-rendering-opengl-rhi-and-packages` (`28` core files + `164` `auto-dproj`)
- **Files**:
  - `castleinternalbatch2drhi.pas`, `castleinternalrhi_opengl.pas`
  - `castlerendercontext.pas`, `castleglimages*.pas`/`*.inc`, `castleglshaders.pas`, `castleglutils*.pas`/`*.inc`, `castlerenderprimitives*.pas`/`*.inc`, `castlevideos.pas`, `castlefonts*.pas`/`*.inc`
  - `castle-fpc.cfg`, `fpmake.pp`, `packages/lazarus/castle_engine_base.lpk`, `packages/delphi/castle_engine.{dpk,dproj}` (+ `03b` `examples/**/*.dproj`).
- **What Reviewers Should Check**:
  - Every RHI dispatch branch in `TDrawableImage`, `TCastleFont`, `DrawPrimitive2D`, `TCastleRenderUnlitMesh`, and `TRenderContext` is guarded by `(ApplicationProperties.RenderBackend = backendVulkan) and (RHIDevice <> nil)` so OpenGL behavior is untouched.
  - `TOpenGLRHIDevice` implements the exact same `TRHIDevice` contract (including compute, SSBO, storage images, indirect multi-draw, and `TRHIRenderGraph`).

### PR #4 — `scene-x3d-viewport-and-ui` (`42` files)
- **Files**: `src/transform/*`, `src/scene/*`, `src/scene/x3d/*.inc`, `src/ui/*`.
- **What Reviewers Should Check**:
  - `TX3DRendererShape`, `TShapeCache`, `TRenderer`, `TLightsRenderer`, and `TCastleViewport` RHI paths for PBR/Phong/Unlit, 32 lights, PCF/VSM & stencil shadow volumes, `TSkinNode` GPU skinning, `TEnvironmentLightNode` Split-Sum IBL, custom `TEffectNode` / `TComposedShaderNode` / `TScreenEffectNode` uniform/sampler reflection, and `TCastleMask` UI/3D stencil masking.

### PR #5 — `window-and-embedded-controls-wsi` (`19` files)
- **Files**: `src/window/*`, `src/lcl/castlecontrol.pas`, `src/delphi/*`.
- **What Reviewers Should Check**:
  - Pure-Vulkan window/widget open (`BypassOpenGLContextForVulkan`: no GLX/WGL/EGL/`NSOpenGLContext` created when Vulkan is active, with automatic fallback to OpenGL if Vulkan initialization fails).
  - CLI flags `--render-backend`, `--auto-exit-frames`, `--capture-screenshot` in `castlewindow.pas`, shared-device multi-window/multi-control `TCastleVulkanSwapchainSurface`, and live runtime `SwitchRenderBackend`.

### PR #6 — `build-tool-packaging-and-editor` (`10` files)
- **Files**: `src/files/*`, `tools/common-code/*`, `tools/build-tool/*`, `tools/castle-editor/*`.
- **What Reviewers Should Check**:
  - `CastleEngineManifest.xml` attributes `render_backend` and `bundle_vulkan_caches`, Android `${ANDROID_VULKAN_FEATURE}` manifest substitution, APK/macOS `.app`/desktop `shader_cache/*.spv` indexing in `CastleDataInformation.xml`, and `castle-editor` **Viewport -> Render Backend** live toggle.

### PR #7 — `vulkan-examples-and-catalog-sweep` (`20` new files)
- **Files**: `examples/vulkan/{vulkan_triangle,vulkan_sprite_batch,vulkan_3d_scene}/*`, `tools/internal/vulkan_example_sweep/run_vulkan_example_sweep.sh`.
- **What Reviewers Should Check**: Self-contained Vulkan example projects (supporting both interactive GUI and `--headless` CI modes) plus automated smoke/screenshot verification across existing catalog examples.

### PR #8 — `tests-ci-and-documentation` (`10` files)
- **Files**: `tests/code/testcases/testcastlevulkanrhi.pas`, `tests/testcases.pas`, `tests/CastleEngineManifest.xml`, `tests/castle_tester_standalone.lpi`, `.github/workflows/vulkan-rhi.yml`, `doc/vulkan_renderer.md`, `doc/vulkan_release_notes.md`, `doc/vulkan_upstream_pr_guide.md`, `tools/internal/vulkan_upstream_slices/*`.
- **What Reviewers Should Check**: `391` unit & OpenGL-vs-Vulkan visual regression tests (`0` `Heaptrc` leaks, `0` Khronos validation errors) and 3-OS GitHub Actions workflow (`Ubuntu Lavapipe`, `macOS MoltenVK`, `Windows x64`).

---

## 4. Automated Verification & Patch Export Tooling

Use [prepare_upstream_slices.sh](file:///F:/work/castle-engine/tools/internal/vulkan_upstream_slices/prepare_upstream_slices.sh) and [slice_manifest.tsv](file:///F:/work/castle-engine/tools/internal/vulkan_upstream_slices/slice_manifest.tsv) to audit slice completeness, verify the Pascal `uses` DAG, or materialize clean `.patch` series locally without touching your working tree:

```bash
# 1. Verify 100% diff coverage, zero duplicates, file existence & acyclic Pascal uses DAG:
./tools/internal/vulkan_upstream_slices/prepare_upstream_slices.sh --verify --summary

# 2. Export clean per-slice patch files (01..08 + separate 03b-examples-auto-dproj.patch):
./tools/internal/vulkan_upstream_slices/prepare_upstream_slices.sh \
  --export-patches=/tmp/cge-vulkan-upstream-patches
```
