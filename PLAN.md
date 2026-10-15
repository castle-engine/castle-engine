# Castle Game Engine — Vulkan Renderer Implementation Plan (`PLAN.md`)

## 1. Executive Summary & Objectives
* **Primary Goal:** Introduce a modern, high-performance Vulkan 1.3+ rendering backend to Castle Game Engine (CGE) while preserving **100% backward compatibility** with the existing public Object Pascal API (`TCastleViewport`, `TCastleScene`, `TCastleUserInterface`, `TDrawableImage`, X3D/glTF nodes, and `Effect` shader nodes).
* **Coexistence:** The existing OpenGL / OpenGLES / WebGL renderer will remain first-class for web targets, legacy hardware, and lightweight deployments. The backend will be selectable via `TCastleApplicationProperties` or `CastleEngineManifest.xml`.
* **Core Paradigm Shift:** Transition CGE's internal rendering layer from immediate-state mutations (OpenGL state machine) to an explicit **Render Hardware Interface (RHI)** using baked Pipeline State Objects (PSOs), Descriptor Sets, and Command Buffers.

---

## 2. High-Level Architecture & RHI Abstraction

Currently, CGE abstracts OpenGL and OpenGLES differences through `CastleGL`, `CastleGLShaders`, `CastleGLImages`, and `CastleRendererInternalShader`. To support Vulkan cleanly without littering high-level scene classes with `{$ifdef}`, we will introduce an internal **Render Hardware Interface (`CastleInternalRHI`)**.

### Proposed Unit Hierarchy
```text
src/base_rendering/
├── rhi/
│   ├── castleinternalrhi.pas              # Abstract RHI classes (Device, CommandList, Pipeline, Buffer, Texture)
│   ├── castleinternalrhitypes.pas         # API-agnostic enums (TPixelFormat, TCompareOp, TBlendFactor)
│   └── castleinternalshadercompiler.pas   # GLSL-to-SPIR-V translation & reflection (shaderc / glslang / spirv-cross)
├── opengl/
│   └── castleinternalrhi_opengl.pas       # OpenGL/GLES implementation of RHI (wraps existing GL logic)
└── vulkan/
    ├── castleinternalvulkan.pas           # Dynamic Vulkan header bindings (FPC & Delphi compatible)
    ├── castleinternalvulkandevice.pas     # VkInstance, VkPhysicalDevice, VkDevice, Queues
    ├── castleinternalvulkanswapchain.pas  # WSI (Win32, X11, Wayland, Android, MoltenVK) & Swapchain
    ├── castleinternalvulkanmemory.pas     # Sub-allocation / Pascal block allocator (VMA-style)
    ├── castleinternalvulkanpipeline.pas   # VkPipeline, VkPipelineCache, VkDescriptorSetLayout
    └── castleinternalrhi_vulkan.pas       # Concrete Vulkan implementation of CastleInternalRHI

```


### Coordinate System & Clip-Space Alignment

* 

Y-Axis & Depth: Vulkan uses a top-left origin with Y pointing down in NDC and depth range [0,1][0, 1][0,1], whereas OpenGL uses bottom-left Y-up and depth [−1,1][-1, 1][−1,1].

* 

Solution: Require VK_KHR_maintenance1 (core since Vulkan 1.1) to pass a negative viewport height (Viewport.height = -Height; Viewport.y = Height;) so NDC Y-axis matches OpenGL without modifying existing CGE camera matrices (ProjectionMatrix) or winding orders. Use gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5 injected at the end of vertex shaders (or configure projection matrix depth range [0,1][0, 1][0,1] in CastleProjection).


## 3. Core Subsystem Design


### 3.1. Windowing & Context Creation (CastleWindow)

* 

Extend TCastleWindow backends (WinAPI, Xlib/Wayland, GTK, Android, Cocoa/MoltenVK) to create a VkSurfaceKHR instead of an OpenGL context (wgl/glX/EGL) when RenderBackend = rbVulkan is active.

* 

Implement Frames in Flight (default: 2) using per-frame command pools, command buffers, VkSemaphore (image available / render finished), and VkFence (CPU-GPU frame pacing).

* 

Support clean swapchain recreation on window resize (VK_ERROR_OUT_OF_DATE_KHR / SUBOPTIMAL_KHR).


### 3.2. GPU Memory Management

* 

Raw vkAllocateMemory calls are limited by maxMemoryAllocationCount.

* 

Implement TCastleVulkanMemoryAllocator: a native Object Pascal sub-allocator (inspired by VMA) using slab/TLSF pools for:

* 

gpu_only (VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT): Static meshes, textures, render targets.

* 

cpu_to_gpu (HOST_VISIBLE | HOST_COHERENT): Per-frame ring buffers for dynamic uniforms, UI sprites, and streaming vertices.

* 

staging: Reusable ring-buffer for asynchronous texture and glTF buffer uploads.


### 3.3. Shader Pipeline & CGE Composable Effects (CastleRendererInternalShader)


CGE's killer feature is its composable X3D shader pipeline (Effect and EffectPart nodes dynamically stitched together with internal lighting/material templates).

* 

GLSL Modernization: Upgrade internal shader templates (src/scene/glsl/source/*.fs/vs) to emit Vulkan-compatible GLSL (#version 450), replacing bare uniform variables with Uniform Buffer Objects (UBOs) and Push Constants for per-draw matrices (castle_ModelViewMatrix, castle_NormalMatrix).

* 

SPIR-V Compilation: Integrate libshaderc (or glslang via dynamic library / static link) to compile stitched GLSL strings into SPIR-V bytecode at runtime.

* 

Disk Caching: Hash the generated GLSL source string + macro defines. Store compiled SPIR-V binaries and VkPipelineCache blobs inside ApplicationConfig('shader_cache/') to eliminate runtime compilation stutter on subsequent launches.


### 3.4. Descriptor Management & Bindings


Group descriptor sets by binding frequency to minimize CPU overhead:

* 

Set 0 (Per-Pass / Global): Camera matrices, global time, fog parameters, directional/positional light arrays, shadow map samplers.

* 

Set 1 (Per-Material): PBR/Phong material uniform block, base color, normal, metallic-roughness, emissive, and occlusion textures.

* 

Set 2 (Per-Object / Dynamic): Bone matrices (skinning), per-instance data, or use Push Constants (128 bytes guaranteed) for ModelMatrix and material index.


### 3.5. Pipeline State Objects (PSO) & Dynamic Rendering

* 

Target Vulkan 1.3 (or Vulkan 1.1 + VK_KHR_dynamic_rendering) to avoid complex VkRenderPass and VkFramebuffer对象 combinatorial explosions.

* 

Build a TPipelineStateKey record (hashing Vertex Layout, SPIR-V module hashes, Blend Mode, Depth-Stencil state, Cull mode, and Attachment formats) mapped to a cached VkPipeline.

* 

Utilize dynamic states (VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR, VK_DYNAMIC_STATE_DEPTH_BIAS) to reduce the total number of unique PSOs.


## 4. Phased Implementation Roadmap


### Phase 1: Foundation, Windowing & Clear Screen

* 

[ ] Create CastleInternalVulkan unit with cross-platform dynamic loader (vulkan-1.dll, libvulkan.so.1, libMoltenVK.dylib).

* 

[ ] Implement VkInstance creation with VK_LAYER_KHRONOS_validation hooked into WritelnWarning / WritelnLog.

* 

[ ] Implement VkSurfaceKHR integration in CastleWindow for Windows, Linux, and Android.

* 

[ ] Select VkPhysicalDevice, create VkDevice, setup Graphics/Present queues, and initialize VkSwapchainKHR.

* 

[ ] Implement the frame loop (Acquire image -> Record command buffer -> Clear color via Dynamic Rendering -> Submit -> Present).

* 

Milestone 1: An empty TCastleWindow opens on Windows/Linux, clears to TCastleViewport.BackgroundColor, and resizes cleanly with zero validation errors.


### Phase 2: 2D Rendering & UI (TDrawableImage & TCastleUserInterface)

* 

[ ] Implement TCastleVulkanMemoryAllocator and staging buffer upload queue.

* 

[ ] Port TDrawableImage and TCastleImage texture uploads (VkImage, VkImageView, VkSampler, automatic mipmap generation via vkCmdBlitImage).

* 

[ ] Implement batched 2D quad rendering (dynamic vertex buffer ring + Push Constants for screen-space transforms and color tinting).

* 

[ ] Support UI scissor clipping (TCastleUserInterface clip rects -> vkCmdSetScissor) and blending modes.

* 

[ ] Verify TCastleLabel font atlas rendering and CGE Editor UI controls.

* 

Milestone 2: All 2D CGE examples (e.g., sprite_sheets, ui_gallery) and the CGE Visual Editor UI render identically to OpenGL.


### Phase 3: 3D Scene Rendering (TCastleScene & glTF/X3D)

* 

[ ] Abstract TShape vertex/index buffer allocation in CastleInternalRenderer to use RHI buffers instead of raw VBOs/VAOs.

* 

[ ] Upgrade TShader (CastleRendererInternalShader) to generate #version 450 UBO layouts alongside legacy GLSL.

* 

[ ] Integrate runtime GLSL-to-SPIR-V compilation and on-disk SPIR-V/Pipeline caching.

* 

[ ] Implement 3D forward pass (Opaque geometry sorted front-to-back, Blended geometry sorted back-to-front).

* 

[ ] Port GPU skinning (joint matrix Uniform Buffers / Storage Buffers) and morph targets.

* 

Milestone 3: A complex 3D glTF model with animations and PBR materials renders inside TCastleViewport.


### Phase 4: Lighting, Shadows, and Advanced Effects

* 

[ ] Implement multi-light UBOs / SSBOs supporting CGE's point, directional, and spot lights.

* 

[ ] Port Shadow Maps (VK_FORMAT_D32_SFLOAT depth-only dynamic rendering passes with VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL transitions) and Variance Shadow Maps (VSM).

* 

[ ] Port Cube Map reflections, Screen-Space Ambient Occlusion (SSAO), and post-processing (TCastleScreenEffects).

* 

[ ] Verify custom X3D Effect and EffectPart user shaders with automatic uniform injection.

* 

Milestone 4: Full visual parity across CGE's examples/ repository (3D games, shadows, custom shaders, screen effects).


### Phase 5: Optimization, Multi-Threading & Platform Polish

* 

[ ] Multi-threaded Command Recording: Record shadow cascades and main scene draw calls across worker threads using secondary command buffers.

* 

[ ] GPU Driven / Batching Enhancements: Leverage vkCmdDrawIndexedIndirect for TCastleScene static batching and terrain chunks.

* 

[ ] Platform Rollout:

* 

[ ] Android (Vulkan 1.1+ baseline, fallback to GLES3 on older devices).

* 

[ ] macOS / iOS via MoltenVK bundled in CGE build tool.

* 

[ ] Nintendo Switch (align with proprietary console backend where applicable).

* 

[ ] Integrate RenderDoc in-application API hooks for one-click frame capture from CGE Editor.


## 5. Build Tool (castle-engine) & Editor Integration

* 

CastleEngineManifest.xml Support:
XML
```delphi
<project name="my_game" render_backend="auto"> <!-- auto | opengl | vulkan -->

```

* 

Shader Pre-warming (Build Step): Extend castle-engine package to optionally pre-compile known core engine shaders to SPIR-V during packaging so mobile/console builds avoid first-run shader compilation.

* 

Editor Diagnostics: Update CastleRenderOptions and Viewport.Statistics to report active RHI backend, PSO cache hits/misses, Descriptor Set allocations, and Vulkan device memory budget (VK_EXT_memory_budget).


## 6. Risk Management & Mitigations
RiskImpactMitigation StrategyCustom User Shaders (Effect node) using legacy GLSL syntaxMediumKeep CastleRendererInternalShader string preprocessor capable of wrapping user PLUG_ functions into #version 450 templates; emit clear warnings if user shaders use deprecated raw GL uniforms.PSO Stutter during gameplay when new material permutations appearHighUse VkPipelineCache saved to disk, utilize VK_EXT_graphics_pipeline_library where available, and pre-warm pipelines during TCastleViewport.PrepareResources.FPC & Delphi Compatibility across 32/64-bit targetsMediumMaintain strict flat C-ABI bindings in CastleInternalVulkan with zero external C++ wrapper dependencies; use standard dynamic libraries (shaderc_shared).Older Mobile / Integrated GPUs lacking Vulkan 1.3MediumTarget Vulkan 1.1 as the strict minimum hardware baseline; fall back to standard VkRenderPass if VK_KHR_dynamic_rendering is absent, or automatically fall back to the OpenGLES 3.0 renderer.

## 7. Testing & Verification Criteria

* 

Automated Visual Regression Tests: Run tests/ suite comparing framebuffer readbacks (GrabImage) between rbOpenGL and rbVulkan with a perceptual diff threshold.

* 

Validation Cleanliness: CI pipeline must run headless tests using Lavapipe (software Vulkan rasterizer) with VK_LAYER_KHRONOS_validation and VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT enabled—failing on any validation warning or error.

* 

Performance Target: Achieve at least a 2.5x reduction in CPU render-thread time in draw-call-heavy scenes (5,000+ distinct TShape nodes) compared to the OpenGL 3.3 Core backend.
