# Castle Game Engine — Vulkan 1.3+ Rendering Backend Release Notes

## Executive Summary

Castle Game Engine now includes a production-ready **Vulkan 1.3+ (Vulkan 1.1–1.4 compatible) rendering backend** built on a unified **Render Hardware Interface (RHI)** alongside its existing OpenGL and OpenGL ES renderers.

- **100% Opt-In & Backward Compatible**: `ApplicationProperties.RenderBackend` defaults to `backendAuto`, which preserves **OpenGL / OpenGL ES** as the default backend unless Vulkan is explicitly requested via `CastleEngineManifest.xml` (`render_backend="vulkan"`), CLI (`--render-backend=vulkan`), environment (`CASTLE_RENDER_BACKEND=vulkan`), or Pascal code (`ApplicationProperties.RenderBackend := backendVulkan`).
- **Zero C/C++ Wrapper Dependencies**: Dynamically loads `vulkan-1.dll`, `libvulkan.so.1`, or `libMoltenVK.dylib` at runtime via auto-generated flat C-ABI Object Pascal bindings (`tools/internal/generate_vulkan_api/vkxml2pas.dpr`), compatible with both **Free Pascal (FPC)** and **Delphi**.
- **Full Scene Graph, 2D UI, Custom Shader & Editor Compatibility**: Existing CGE projects, glTF 2.0 / X3D 4 models, `.castle-user-interface` layouts, `TCastleTerrain`, custom X3D `Effect` / `ComposedShader` / `ScreenEffect` GLSL shaders, `TCastleWindow`, and embedded `TCastleControl` (Lazarus LCL, `castle-editor`, Delphi VCL & FMX) run unmodified on Vulkan — including **live runtime `OpenGL <-> Vulkan` hot-switching** without restarting the application.

---

## Headline Capabilities

### 1. Core Rendering & X3D / glTF 2.0 Feature Parity
- **Shading Models**: Physical (`TPhysicalMaterialNode` Cook-Torrance GGX metallic-roughness + clear coat, anisotropy, sheen, iridescence, subsurface/foliage transmission), Phong (`TMaterialNode`, `TTwoSidedMaterialNode`, VRML 1.0 `TMaterialNode_1`, `TCommonSurfaceShaderNode`), and Unlit (`TUnlitMaterialNode`, per-vertex `TColorNode` `cmModulate`/`cmReplace`).
- **Lighting & Shadows**: Up to **32 punctual directional/point/spot lights per shape** (plus 256+ Clustered Forward+ dynamic lights), multi-light `TGeneratedShadowMapNode` (`ssSimple`, `ssPCF4`, `ssPCF4Bilinear`, `ssPCF16`, `ssVarianceShadowMaps`), PCSS & Virtual Shadow Maps (VSM), full multi-pass **stencil shadow volumes** (`TGLShadowVolumeRenderer`), and GPU Split-Sum **Image-Based Lighting** (`TEnvironmentLightNode` / `TCastleVulkanIBLGenerator`).
- **Animation & Geometry**: 4-bone GPU skeletal skinning (`TSkinNode` `CastleSkinningBlock` & `mcGpuTexture`), morph targets, tangent-space normal/parallax/steep-parallax self-shadowing bump mapping, `TMultiTextureNode` (up to 4 stages across 2D, `samplerCube`, and `sampler3D`), `TCastleBackground` skyboxes, `TClipPlaneNode` (up to 4 planes), linear/exponential/volumetric `TFogNode`, hardware occlusion query culling (`TOcclusionCullingRenderer`), dynamic shape batching (`TBatchShapes`), wireframe/silhouette modes, and `TLinePropertiesNode` stipple/width.
- **2D UI & Offscreen Rendering**: High-throughput streaming quad batcher (`TCastleBatch2DRenderer`), SDF & bitmap `TCastleFont`, `TDrawableImage` (`RenderToImageBegin`/`End`, custom 2D shaders, S3TC/BC/ASTC/ETC2 & float textures), `TCastleMask` 2D/3D stencil masking, `TRenderedTextureNode`, `TGeneratedCubeMapTextureNode`, and `RenderControlToImage` / `SaveScreen`.

### 2. Custom User Shaders & Build-Tool Cache Packaging
- **Runtime GLSL -> SPIR-V Compilation**: Dynamically uses `libshaderc` or `glslangValidator` / `glslc` to compile arbitrary user X3D `Effect` (`PLUG_*` hooks across Appearance, Texture, Light, and State nodes), `ComposedShader`, `TCastleTerrain`, and `TCastleScreenEffects` shaders, automatically upgrading legacy GLSL constructs (`uniform`, `varying`, `attribute`, `texture2D`, per-light `FdEffects`) to Vulkan GLSL `#version 450`.
- **56 Built-In Zero-Dependency SPIR-V Modules**: Standard 2D UI, 3D PBR/Phong/Unlit, shadow, skinning, skybox, IBL compute, post-processing, ray-tracing, and mesh-shader pipelines run out-of-the-box with **zero external shader compiler dependencies**.
- **`bundle_vulkan_caches="true"` Packaging**: `castle-engine package` (desktop archives, macOS `.app` bundles, and Android APKs) automatically pre-warms and bundles `castle-data:/shader_cache/*.spv` (indexed in `CastleDataInformation.xml`) and optional `vulkan_pipeline_cache.bin` seeds for stutter-free startup.

### 3. Explicit Vulkan 1.3+ Architecture & Next-Gen GPU Subsystems
- **Dynamic Rendering & Vulkan 1.1/1.2 Fallback**: Uses `VK_KHR_dynamic_rendering`, `VK_KHR_synchronization2`, `VK_EXT_extended_dynamic_state`, and `VK_EXT_graphics_pipeline_library` 4-part fast PSO linking on modern drivers, with automatic cached `VkRenderPass`/`VkFramebuffer` fallback on Vulkan 1.1/1.2 mobile/integrated devices.
- **Pooled GPU Memory Sub-Allocator & Live Defragmentation**: `TCastleVulkanMemoryAllocator` sub-allocates 64 MB device-local and 16 MB host-visible pools to stay strictly below Windows WDDM's 4,096 allocation limit, with 3-phase `DefragmentPools` GPU buffer relocation and `VkDeviceMemory` block reclamation.
- **Multi-Threading, Async DMA & Bindless Indirect Multi-Draw**: Per-worker-thread `VkCommandPool` secondary command buffer recording (`TCastleViewport.ParallelCommandRecording`), timeline-semaphore async transfer queue uploads, and `VK_EXT_descriptor_indexing` bindless textures + compute frustum/Hi-Z culling + `vkCmdDrawIndexedIndirectCount` batching.
- **Declarative Render Graph & Next-Gen Rendering**: `TRHIRenderGraph` with `VK_IMAGE_CREATE_ALIAS_BIT` physical `VkDeviceMemory` transient aliasing and dead-pass culling; 9-pass HDR Bloom / Log-Luminance Auto-Exposure / ACES & AgX Tonemapping / Compute GTAO / Hi-Z SSR / Froxel Volumetric Fog / Halton TAA & TSR / Lumen Global SDF + Surface Cache / DDGI / ReSTIR / Weighted Blended OIT; plus live `VK_KHR_acceleration_structure` + `VK_KHR_ray_query` + `VK_KHR_ray_tracing_pipeline` and `VK_EXT_mesh_shader` dispatchers with automatic Software Compute LBVH & Meshlet fallbacks.
- **Cross-Backend `TOpenGLRHIDevice` Parity**: Unified compute (`glDispatchCompute` / `glDispatchComputeIndirect`), SSBO, `glBindImageTexture` storage-image, indirect multi-draw, query pool, and `TRHIRenderGraph` execution on OpenGL 4.3–4.6 / GLES 3.1+.

---

## Supported Platforms & WSI Targets

| Platform | Windowing & Embedded Controls | Vulkan Loader & Driver Coverage |
| :--- | :--- | :--- |
| **Windows (x86_64 / ARM64)** | `TCastleWindow` (WinAPI), Lazarus LCL, Delphi VCL & FMX (`HWND`) | `vulkan-1.dll` — Discrete NVIDIA, AMD, Intel & Qualcomm Adreno drivers |
| **Linux (x86_64 / AArch64)** | `TCastleWindow` (Xlib, GTK, Wayland), Lazarus LCL (GTK2/GTK3), FMX Linux | `libvulkan.so.1` — Mesa RADV, ANV, NVK, Turnip, Lavapipe & proprietary NVIDIA |
| **macOS & iOS (Apple Silicon / x86_64)** | `TCastleWindow` (Cocoa `CAMetalLayer`), Lazarus LCL Cocoa, FMX | `libvulkan.1.dylib` / `libMoltenVK.dylib` (`VK_KHR_portability_subset`) |
| **Android (ARM64 / ARMv7)** | `TCastleWindow` (`ANativeWindow` pure-Vulkan WSI) | `libvulkan.so` — Qualcomm Adreno, ARM Mali, Samsung Xclipse (Vulkan 1.1–1.3) |
| **Headless / Cloud CI** | `VK_EXT_headless_surface` & WSI-free `TVulkanRHIDevice.TryCreateOffscreen` | Displayless Linux/Windows/macOS GPU & Mesa Lavapipe software ICDs |

---

## Quality, Validation & Visual Parity Metrics

- **391 Dedicated Vulkan & RHI Unit Tests** (`TTestCastleVulkanRHI`, within the 968-test `castle_tester_standalone` suite) passing with **`0 unfreed memory blocks : 0`** (`Heaptrc`) and **`0` `VK_LAYER_KHRONOS_validation` errors** (`CASTLE_VULKAN_VALIDATION=1`).
- **3-OS Automated GitHub Actions Gate** (`.github/workflows/vulkan-rhi.yml`): Continuous compilation, `.spv` cache packaging, live Vulkan execution, and example catalog sweeps across **Ubuntu Linux (Mesa Lavapipe Vulkan 1.4)**, **macOS Apple Silicon (MoltenVK + `VK_KHR_portability_subset`)**, and **Windows x64**.
- **Side-by-Side `rbOpenGL` vs `rbVulkan` Perceptual RMSE Parity**: Automated pixel-comparison suites (`TRHIVisualRegressionSuite`) verify visual parity across 2D UI, Unlit, Phong, glTF 2.0 Metallic-Roughness PBR, custom `Effect` & per-light `TEffectNode` shaders, flagship CGE demos (`explore_impressive_castle`, `platformer`, `terrain`), and broader catalog examples (`examples/animations`, `examples/shaders`, `examples/viewport_and_scenes`, `examples/physics`).

---

## Migration Notes for Existing Projects

1. **No action required for OpenGL projects**: Existing projects keep `rbOpenGL` / `rbOpenGLES` by default until opted into Vulkan.
2. **Testing your game on Vulkan**: Run `./your_game --render-backend=vulkan` (with `CASTLE_VULKAN_VALIDATION=1` during development) or toggle **Viewport -> Render Backend (OpenGL / Vulkan 1.3+)** inside `castle-editor`.
3. **Shipping standalone builds with custom GLSL `Effect` shaders**: Add `bundle_vulkan_caches="true"` to `<project ...>` in `CastleEngineManifest.xml` (or ship `libshaderc` alongside the binary if your game generates procedural GLSL source strings dynamically at runtime).

See [vulkan_renderer.md](file:///F:/work/castle-engine/doc/vulkan_renderer.md) for the complete API, CLI, environment variable, and shader authoring reference.
