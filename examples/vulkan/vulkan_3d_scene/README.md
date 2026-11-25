# Vulkan 3D Scene Forward Pass & Lighting Example

This example demonstrates Castle Game Engine's Vulkan 1.3 RHI 3D rendering pipeline:

1. **Manifest Backend Selection**: Uses `render_backend="vulkan"` in `CastleEngineManifest.xml` and `SelectOptimalRenderBackend` in `CastleInternalVulkanWSI`.
2. **Shader Pre-warming**: Pre-warms the 5 core GLSL 450 shader stages (`PrewarmCoreEngineShaders`) for the SPIR-V disk cache.
3. **Frequency-Based UBOs**: Populates `TCastleFrameUniformBlock` (Set 0, Binding 0), `TCastleLightsUniformBlock` (Set 0, Binding 1) with directional/point lights and shadow map projector matrices, and `TCastleSkinningUniformBlock` (Set 3, Binding 0).
4. **Shadow Map Dynamic Rendering Pass**: Configures a 2048x2048 depth-only rendering pass via `TRHIRenderingInfo.CreateDepthOnly`.
