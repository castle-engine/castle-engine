# Vulkan RHI Triangle Example

This example demonstrates the usage of Castle Game Engine's explicit **Render Hardware Interface (RHI)** for Vulkan 1.3 and OpenGL.

## Key Features Demonstrated:
* **Explicit RHI Abstraction**: Uses `TRHIDevice`, `TRHIBuffer`, and `TRHICommandList` rather than immediate OpenGL calls.
* **VK_KHR_dynamic_rendering**: Avoids legacy Vulkan render passes and framebuffers, rendering directly to swapchain attachments.
* **Negative-Viewport Clip Space Alignment**: Demonstrates how OpenGL NDC conventions work identically in Vulkan.
