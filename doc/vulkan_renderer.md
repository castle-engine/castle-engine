# Vulkan 1.3 Rendering Backend & Render Hardware Interface (RHI)

## 1. Overview & Architecture

Castle Game Engine (CGE) provides a modern, high-performance Vulkan 1.3 rendering backend alongside its established OpenGL and OpenGLES backends. This implementation introduces an extensible Render Hardware Interface (RHI) layer that unifies GPU abstractions while maintaining complete backward compatibility with CGE's scene graph, 2D/3D viewports, and UI controls.

```
+-----------------------------------------------------------------+
|          Castle Game Engine High-Level Systems                  |
|    (TCastleViewport, TCastleScene, TCastleTransform, 2D UI)     |
+-----------------------------------------------------------------+
                               |
                               v
+-----------------------------------------------------------------+
|            Render Hardware Interface (RHI)                      |
|  (TRHIDevice, TRHIBuffer, TRHITexture, TRHIPipeline, Commands)  |
+-----------------------------------------------------------------+
              /                                     \
             v                                       v
+-------------------------+             +-------------------------+
|     Vulkan 1.3 RHI      |             |     OpenGL / ES RHI     |
| (Dynamic Rendering,     |             | (State cache, VAO/VBO,  |
|  Negative Viewport,     |             |  Uniform Buffers,       |
|  Sub-allocator, WSI)    |             |  FBO compatibility)     |
+-------------------------+             +-------------------------+
```

### Key Highlights
- **Vulkan 1.3 Core & Dynamic Rendering**: Fully utilizes `VK_KHR_dynamic_rendering` (promoted to core in Vulkan 1.3), eliminating the overhead and boilerplate of traditional `VkRenderPass` and `VkFramebuffer` objects.
- **Zero Static C/C++ Dependencies**: Vulkan API functions are dynamically loaded at runtime using Free Pascal's `TDynLib` (`vulkan-1.dll` on Windows, `libvulkan.so.1` on Linux, `libMoltenVK.dylib` on macOS).
- **Sub-Allocation GPU Memory Manager**: Pure Object Pascal sub-allocator allocating large 64 MB (device local) and 16 MB (host visible) blocks to strictly respect driver `maxMemoryAllocationCount` limits.
- **Unified NDC Coordinates & Negative Viewport**: Exploits `VK_KHR_maintenance1` negative viewport height (`Viewport.Y = Height; Viewport.Height = -Height`) so CGE's coordinate conventions (Y-up, CCW winding) work naturally without vertex shader inversion hacks.
- **Runtime SPIR-V Compilation & Caching**: Integrates dynamic `shaderc` loading with persistent disk caching for compiled SPIR-V modules.
- **High-Performance 2D Batching**: Dedicated `TCastleBatch2DRenderer` supporting massive quad throughput with dynamic streaming vertex buffers and push-constant transforms.

---

## 2. Render Hardware Interface (RHI)

The RHI abstraction lives in `src/base_rendering/rhi/` and `src/base_rendering/vulkan/`:

| Unit | Description |
| :--- | :--- |
| `CastleInternalRHI` | Device, buffer, texture, sampler, pipeline, and command buffer abstractions. |
| `CastleInternalShaderRHI` | GLSL 450 shader generator, UBO layout management, push-constant bindings. |
| `CastleInternalBatch2DRHI` | High-throughput 2D sprite/quad batch renderer with dynamic vertex buffers. |
| `CastleInternalVulkan` | Dynamic loader, types, enums, structs, and Vulkan 1.0–1.3 API entry points. |
| `CastleInternalVulkanWSI` | Window System Integration (WSI) for Win32, X11, and Wayland. |
| `CastleInternalVulkanMemory`| Best-fit sub-allocation GPU memory manager. |
| `CastleInternalRHI_Vulkan` | Vulkan 1.3 backend implementation of RHI interfaces. |
| `CastleInternalRHI_OpenGL` | OpenGL / OpenGLES backend adapter for parity and validation. |
| `CastleInternalShaderCompiler` | Dynamic `libshaderc` wrapper with persistent disk caching. |

### Resource Lifecycle
All RHI resources inherit from `TRHIResource` and are reference-counted or owned by the parent `TRHIDevice`:
- `TRHIBuffer`: Encapsulates vertex, index, uniform (UBO), storage (SSBO), or staging buffers.
- `TRHITexture`: 2D, 3D, and Cube textures supporting linear or optimal tiling, depth-stencil attachments, and multisampling.
- `TRHIPipeline`: Immutable Pipeline State Objects (PSOs). PSOs are keyed by shader stages, vertex input layouts, rasterization, depth/stencil, and blend states. A fast FNV-1a state hash ensures PSOs are compiled once and cached deterministically.
- `TRHICommandList`: Records drawing commands, render pass boundaries (`BeginRendering`/`EndRendering`), barrier transitions, and dynamic scissor/viewport updates.

---

## 3. Memory Sub-Allocation Architecture

Vulkan implementations impose an implementation-dependent limit on the total number of simultaneous device allocations (`maxMemoryAllocationCount`, typically 4,096). Creating individual `VkDeviceMemory` allocations per buffer or texture quickly exhausts this limit in production games.

`TVulkanMemoryManager` solves this problem:
1. **Slab Allocation**: Allocates large memory blocks (`64 MB` for `VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT`, `16 MB` for `VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT`).
2. **Chunk Tracking**: Maintains doubly-linked chunk headers within each block, splitting free chunks on allocation and coalescing adjacent free chunks on deallocation.
3. **Alignment Compliance**: Enforces strict buffer-image granularity (`bufferImageGranularity`) and alignment requirements queried from the physical device.
4. **Dedicated Fallback**: Resources exceeding half of standard block size automatically allocate a dedicated `VkDeviceMemory` block.

---

## 4. Coordinate System & Negative Viewport

Vulkan's default clip space uses a right-handed system with the Y-axis pointing downwards ($[-1, 1]$ in X and Y, and $[0, 1]$ in Z). OpenGL uses Y pointing upwards and $[-1, 1]$ in Z.

To ensure shaders, models, and UI elements produce identical results across backends without altering vertex shaders:
1. **Y-Axis Inversion**: We configure Vulkan viewports using negative height:
   ```pascal
   VkViewport.x := ViewportRect.Left;
   VkViewport.y := ViewportRect.Top + ViewportRect.Height;
   VkViewport.width := ViewportRect.Width;
   VkViewport.height := -ViewportRect.Height;
   VkViewport.minDepth := 0.0;
   VkViewport.maxDepth := 1.0;
   ```
2. **Z-Axis Depth Remapping**: When generating GLSL 450 shaders for Vulkan, depth coordinates are mapped to Vulkan's $[0, 1]$ range via vertex shader transformation:
   ```glsl
   gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;
   ```

---

## 5. Descriptor Set Layout Strategy

Shaders adhere to a standard 4-set frequency-based binding convention:

| Set | Frequency | Contents |
| :---: | :--- | :--- |
| **Set 0** | Per-Frame | Camera View/Projection matrices, elapsed time, ambient light, fog parameters. |
| **Set 1** | Per-Pass / Material | Material parameters, albedo/normal/metallic-roughness textures, samplers. |
| **Set 2** | Per-Object / Node | Model transform matrix, normal matrix, bone matrices for skeletal animation. |
| **Set 3** | Per-Draw / Dynamic | Push constants, dynamic sprite tints, 2D batch offsets. |

---

## 6. Examples

Two runnable examples demonstrate the Vulkan backend:

1. **`examples/vulkan/vulkan_triangle`**:
   - Demonstrates standalone initialization of `TRHIDevice_Vulkan`, dynamic rendering with `VkRenderingInfo`, shader compilation, pipeline state setup, and frame synchronization.
   - Located at [examples/vulkan/vulkan_triangle](file:///examples/vulkan/vulkan_triangle/).

2. **`examples/vulkan/vulkan_sprite_batch`**:
   - Demonstrates high-throughput 2D rendering using `TCastleBatch2DRenderer`, animating hundreds of batched textured sprites with minimal CPU overhead.
   - Located at [examples/vulkan/vulkan_sprite_batch](file:///examples/vulkan/vulkan_sprite_batch/).

---

## 7. Running Unit Tests

The test suite validates RHI types, dynamic loading, pipeline caching, memory sub-allocation, and shader generation:

```bash
# Run test suite from repository root:
castle-engine --mode=debug compile --verbose tests/
./tests/castle-tester --test=TTestCastleVulkanRHI
```
Or directly using Free Pascal:
```bash
fpc @castle-fpc.cfg -Itests -Futests -Futests/code -Futests/code/testcases -Futests/code/tester-castle -Futools/common-code tests/castle_tester_standalone.dpr
./tests/castle_tester_standalone --test=TTestCastleVulkanRHI --console
```
