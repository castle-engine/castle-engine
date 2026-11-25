# Generate Vulkan Pascal API (`CastleInternalVulkan`) from Khronos `vk.xml`

This internal generator utility (`vkxml2pas.dpr`, inspired by Benjamin "BeRo" Rosseaux's PasVulkan `vkxml2pas` tool) parses the official Khronos Vulkan XML API Registry (`vk.xml`) and automatically generates `src/base_rendering/vulkan/castleinternalvulkan.pas`.

## Why Generate from `vk.xml`?

Per feedback from Benjamin "BeRo" Rosseaux (author of PasVulkan):
- Maintaining Vulkan headers manually is error-prone (especially extension structure type enum offsets, bitmasks, and struct layout alignment across Vulkan 1.0–1.4 and KHR/EXT extensions).
- Generating `CastleInternalVulkan` directly from the official Khronos `vk.xml` registry guarantees exact enum values (`1000000000 + (extnumber - 1) * 1000 + offset`), C-ABI struct layouts, handle types, and `PFN_vk*` dynamic loader signatures.

## Usage

Compile and run with Free Pascal (on Linux/WSL where `/usr/share/vulkan/registry/vk.xml` is installed by default, or pass a custom path to `vk.xml`):

```bash
fpc tools/internal/generate_vulkan_api/vkxml2pas.dpr
./tools/internal/generate_vulkan_api/vkxml2pas /usr/share/vulkan/registry/vk.xml src/base_rendering/vulkan/castleinternalvulkan.pas
```

If invoked with no arguments from the repository root, `vkxml2pas` defaults to reading `/usr/share/vulkan/registry/vk.xml` and writing `src/base_rendering/vulkan/castleinternalvulkan.pas`.
