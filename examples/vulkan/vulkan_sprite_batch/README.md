# Vulkan RHI 2D Sprite Batching Example

Demonstrates high-throughput 2D quad and sprite coalescing with Castle Game Engine's `TCastleBatch2DRenderer`.

## Performance Advantages:
* **Zero Driver Overhead**: Quads are packed directly into dynamic mapped staging memory.
* **Push Constant Transform**: Orthographic viewport scale and bias `(2/W, 2/H, -1, -1)` uploaded directly without UBO overhead.
* **Automatic Coalescing**: 10,000+ sprites streamed in chunks up to 16,384 quads per single hardware draw call.
