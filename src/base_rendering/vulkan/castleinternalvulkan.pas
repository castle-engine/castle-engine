{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cross-platform dynamic loader and flat C-ABI types for Vulkan 1.0, 1.1, 1.2, and 1.3.
  Provides dynamic loading via TDynLib without link-time dependencies on vulkan-1.dll. }
unit CastleInternalVulkan;

{$I castleconf.inc}

{$ifdef FPC}
  {$packrecords C}
{$endif}

interface

uses
  SysUtils, CastleDynLib, CastleLog;

const
  VK_NULL_HANDLE = 0;
  VK_WHOLE_SIZE = UInt64($FFFFFFFFFFFFFFFF);
  VK_ATTACHMENT_UNUSED = Cardinal($FFFFFFFF);
  VK_QUEUE_FAMILY_IGNORED = Cardinal($FFFFFFFF);
  VK_SUBPASS_EXTERNAL = Cardinal($FFFFFFFF);
  VK_MAX_MEMORY_TYPES = 32;
  VK_MAX_MEMORY_HEAPS = 16;
  VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256;
  VK_UUID_SIZE = 16;
  VK_MAX_EXTENSION_NAME_SIZE = 256;
  VK_MAX_DESCRIPTION_SIZE = 256;

  VK_TRUE = 1;
  VK_FALSE = 0;

  { API Versions }
  VK_API_VERSION_1_0 = (1 shl 22) or (0 shl 12);
  VK_API_VERSION_1_1 = (1 shl 22) or (1 shl 12);
  VK_API_VERSION_1_2 = (1 shl 22) or (2 shl 12);
  VK_API_VERSION_1_3 = (1 shl 22) or (3 shl 12);

  { Result Codes }
  VK_SUCCESS = 0;
  VK_NOT_READY = 1;
  VK_TIMEOUT = 2;
  VK_EVENT_SET = 3;
  VK_EVENT_RESET = 4;
  VK_INCOMPLETE = 5;
  VK_ERROR_OUT_OF_HOST_MEMORY = -1;
  VK_ERROR_OUT_OF_DEVICE_MEMORY = -2;
  VK_ERROR_INITIALIZATION_FAILED = -3;
  VK_ERROR_DEVICE_LOST = -4;
  VK_ERROR_MEMORY_MAP_FAILED = -5;
  VK_ERROR_LAYER_NOT_PRESENT = -6;
  VK_ERROR_EXTENSION_NOT_PRESENT = -7;
  VK_ERROR_FEATURE_NOT_PRESENT = -8;
  VK_ERROR_INCOMPATIBLE_DRIVER = -9;
  VK_ERROR_TOO_MANY_OBJECTS = -10;
  VK_ERROR_FORMAT_NOT_SUPPORTED = -11;
  VK_ERROR_FRAGMENTED_POOL = -12;
  VK_ERROR_SURFACE_LOST_KHR = -1000000000;
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = -1000000001;
  VK_SUBOPTIMAL_KHR = 1000001003;
  VK_ERROR_OUT_OF_DATE_KHR = -1000001004;

  { Structure Types }
  VK_STRUCTURE_TYPE_APPLICATION_INFO = 0;
  VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = 1;
  VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = 2;
  VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = 3;
  VK_STRUCTURE_TYPE_SUBMIT_INFO = 4;
  VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = 5;
  VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = 8;
  VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = 9;
  VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = 12;
  VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = 14;
  VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = 15;
  VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = 16;
  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = 18;
  VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = 19;
  VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = 20;
  VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = 22;
  VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = 23;
  VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = 24;
  VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = 25;
  VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = 26;
  VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = 27;
  VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = 28;
  VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = 30;
  VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = 31;
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = 32;
  VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = 33;
  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = 34;
  VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = 35;
  VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = 39;
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = 40;
  VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = 42;
  VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = 43;
  VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = 44;
  VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = 45;
  VK_STRUCTURE_TYPE_MEMORY_BARRIER = 46;
  VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = 1000001000;
  VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = 1000001001;
  VK_STRUCTURE_TYPE_RENDERING_INFO_KHR = 1000044000;
  VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR = 1000044001;
  VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR = 1000044002;
  VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION2_FEATURES_KHR = 1000314000;

  { Formats }
  VK_FORMAT_UNDEFINED = 0;
  VK_FORMAT_R8_UNORM = 9;
  VK_FORMAT_R8G8_UNORM = 16;
  VK_FORMAT_R8G8B8A8_UNORM = 37;
  VK_FORMAT_R8G8B8A8_SRGB = 43;
  VK_FORMAT_B8G8R8A8_UNORM = 44;
  VK_FORMAT_B8G8R8A8_SRGB = 50;
  VK_FORMAT_R16_SFLOAT = 76;
  VK_FORMAT_R16G16_SFLOAT = 83;
  VK_FORMAT_R16G16B16A16_SFLOAT = 97;
  VK_FORMAT_R32_SFLOAT = 100;
  VK_FORMAT_R32G32_SFLOAT = 103;
  VK_FORMAT_R32G32B32_SFLOAT = 106;
  VK_FORMAT_R32G32B32A32_SFLOAT = 109;
  VK_FORMAT_D16_UNORM = 124;
  VK_FORMAT_D32_SFLOAT = 126;
  VK_FORMAT_S8_UINT = 127;
  VK_FORMAT_D24_UNORM_S8_UINT = 129;
  VK_FORMAT_D32_SFLOAT_S8_UINT = 130;

  { Image Types & Layouts }
  VK_IMAGE_TYPE_1D = 0;
  VK_IMAGE_TYPE_2D = 1;
  VK_IMAGE_TYPE_3D = 2;

  VK_IMAGE_VIEW_TYPE_1D = 0;
  VK_IMAGE_VIEW_TYPE_2D = 1;
  VK_IMAGE_VIEW_TYPE_3D = 2;
  VK_IMAGE_VIEW_TYPE_CUBE = 3;
  VK_IMAGE_VIEW_TYPE_1D_ARRAY = 4;
  VK_IMAGE_VIEW_TYPE_2D_ARRAY = 5;
  VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = 6;

  VK_IMAGE_LAYOUT_UNDEFINED = 0;
  VK_IMAGE_LAYOUT_GENERAL = 1;
  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = 2;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = 3;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = 4;
  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = 5;
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = 6;
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = 7;
  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = 1000001002;
  VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR = 1000314000;
  VK_IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR = 1000314001;

  { Buffer Usage Flags }
  VK_BUFFER_USAGE_TRANSFER_SRC_BIT = $00000001;
  VK_BUFFER_USAGE_TRANSFER_DST_BIT = $00000002;
  VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = $00000004;
  VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = $00000008;
  VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = $00000010;
  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = $00000020;
  VK_BUFFER_USAGE_INDEX_BUFFER_BIT = $00000040;
  VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = $00000080;
  VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = $00000100;

  { Image Usage Flags }
  VK_IMAGE_USAGE_TRANSFER_SRC_BIT = $00000001;
  VK_IMAGE_USAGE_TRANSFER_DST_BIT = $00000002;
  VK_IMAGE_USAGE_SAMPLED_BIT = $00000004;
  VK_IMAGE_USAGE_STORAGE_BIT = $00000008;
  VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = $00000010;
  VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = $00000020;
  VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = $00000040;
  VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = $00000080;

  { Memory Property Flags }
  VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = $00000001;
  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = $00000002;
  VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = $00000004;
  VK_MEMORY_PROPERTY_HOST_CACHED_BIT = $00000008;
  VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = $00000010;

  { Pipeline Stage Flags }
  VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = $00000001;
  VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = $00000002;
  VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = $00000004;
  VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = $00000008;
  VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = $00000080;
  VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = $00000100;
  VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = $00000200;
  VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = $00000400;
  VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = $00000800;
  VK_PIPELINE_STAGE_TRANSFER_BIT = $00001000;
  VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = $00002000;
  VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = $00008000;
  VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = $00010000;

  { Access Flags }
  VK_ACCESS_INDIRECT_COMMAND_READ_BIT = $00000001;
  VK_ACCESS_INDEX_READ_BIT = $00000002;
  VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = $00000004;
  VK_ACCESS_UNIFORM_READ_BIT = $00000008;
  VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = $00000010;
  VK_ACCESS_SHADER_READ_BIT = $00000020;
  VK_ACCESS_SHADER_WRITE_BIT = $00000040;
  VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = $00000080;
  VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = $00000100;
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = $00000200;
  VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = $00000400;
  VK_ACCESS_TRANSFER_READ_BIT = $00000800;
  VK_ACCESS_TRANSFER_WRITE_BIT = $00001000;
  VK_ACCESS_HOST_READ_BIT = $00002000;
  VK_ACCESS_HOST_WRITE_BIT = $00004000;
  VK_ACCESS_MEMORY_READ_BIT = $00008000;
  VK_ACCESS_MEMORY_WRITE_BIT = $00010000;

  { Shader Stages }
  VK_SHADER_STAGE_VERTEX_BIT = $00000001;
  VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = $00000002;
  VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = $00000004;
  VK_SHADER_STAGE_GEOMETRY_BIT = $00000008;
  VK_SHADER_STAGE_FRAGMENT_BIT = $00000010;
  VK_SHADER_STAGE_COMPUTE_BIT = $00000020;
  VK_SHADER_STAGE_ALL_GRAPHICS = $0000001F;
  VK_SHADER_STAGE_ALL = $7FFFFFFF;

  { Sampler Filters & Modes }
  VK_FILTER_NEAREST = 0;
  VK_FILTER_LINEAR = 1;

  VK_SAMPLER_MIPMAP_MODE_NEAREST = 0;
  VK_SAMPLER_MIPMAP_MODE_LINEAR = 1;

  VK_SAMPLER_ADDRESS_MODE_REPEAT = 0;
  VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = 1;
  VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = 2;
  VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = 3;

  { Compare Operations }
  VK_COMPARE_OP_NEVER = 0;
  VK_COMPARE_OP_LESS = 1;
  VK_COMPARE_OP_EQUAL = 2;
  VK_COMPARE_OP_LESS_OR_EQUAL = 3;
  VK_COMPARE_OP_GREATER = 4;
  VK_COMPARE_OP_NOT_EQUAL = 5;
  VK_COMPARE_OP_GREATER_OR_EQUAL = 6;
  VK_COMPARE_OP_ALWAYS = 7;

  { Descriptor Types }
  VK_DESCRIPTOR_TYPE_SAMPLER = 0;
  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = 1;
  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = 2;
  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = 3;
  VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = 4;
  VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = 5;
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = 6;
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = 7;
  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = 8;
  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = 9;
  VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = 10;

  { Primitive Topology }
  VK_PRIMITIVE_TOPOLOGY_POINT_LIST = 0;
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST = 1;
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = 2;
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = 3;
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = 4;
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = 5;

  { Polygon & Cull Modes }
  VK_POLYGON_MODE_FILL = 0;
  VK_POLYGON_MODE_LINE = 1;
  VK_POLYGON_MODE_POINT = 2;

  VK_CULL_MODE_NONE = 0;
  VK_CULL_MODE_FRONT_BIT = $00000001;
  VK_CULL_MODE_BACK_BIT = $00000002;
  VK_CULL_MODE_FRONT_AND_BACK = $00000003;

  VK_FRONT_FACE_COUNTER_CLOCKWISE = 0;
  VK_FRONT_FACE_CLOCKWISE = 1;

  { Blend Factors & Operations }
  VK_BLEND_FACTOR_ZERO = 0;
  VK_BLEND_FACTOR_ONE = 1;
  VK_BLEND_FACTOR_SRC_COLOR = 2;
  VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = 3;
  VK_BLEND_FACTOR_DST_COLOR = 4;
  VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = 5;
  VK_BLEND_FACTOR_SRC_ALPHA = 6;
  VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = 7;
  VK_BLEND_FACTOR_DST_ALPHA = 8;
  VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = 9;

  VK_BLEND_OP_ADD = 0;
  VK_BLEND_OP_SUBTRACT = 1;
  VK_BLEND_OP_REVERSE_SUBTRACT = 2;
  VK_BLEND_OP_MIN = 3;
  VK_BLEND_OP_MAX = 4;

  VK_COLOR_COMPONENT_R_BIT = $00000001;
  VK_COLOR_COMPONENT_G_BIT = $00000002;
  VK_COLOR_COMPONENT_B_BIT = $00000004;
  VK_COLOR_COMPONENT_A_BIT = $00000008;

  { Dynamic States }
  VK_DYNAMIC_STATE_VIEWPORT = 0;
  VK_DYNAMIC_STATE_SCISSOR = 1;
  VK_DYNAMIC_STATE_LINE_WIDTH = 2;
  VK_DYNAMIC_STATE_DEPTH_BIAS = 3;
  VK_DYNAMIC_STATE_BLEND_CONSTANTS = 4;

  { Attachment Load / Store Ops }
  VK_ATTACHMENT_LOAD_OP_LOAD = 0;
  VK_ATTACHMENT_LOAD_OP_CLEAR = 1;
  VK_ATTACHMENT_LOAD_OP_DONT_CARE = 2;

  VK_ATTACHMENT_STORE_OP_STORE = 0;
  VK_ATTACHMENT_STORE_OP_DONT_CARE = 1;

  { Command Buffer / Pool }
  VK_COMMAND_BUFFER_LEVEL_PRIMARY = 0;
  VK_COMMAND_BUFFER_LEVEL_SECONDARY = 1;
  VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = $00000001;
  VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = $00000001;
  VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = $00000002;

  { Fence / Semaphore }
  VK_FENCE_CREATE_SIGNALED_BIT = $00000001;

  { Index Types }
  VK_INDEX_TYPE_UINT16 = 0;
  VK_INDEX_TYPE_UINT32 = 1;

  { Image Aspects }
  VK_IMAGE_ASPECT_COLOR_BIT = $00000001;
  VK_IMAGE_ASPECT_DEPTH_BIT = $00000002;
  VK_IMAGE_ASPECT_STENCIL_BIT = $00000004;

  { Present Modes }
  VK_PRESENT_MODE_IMMEDIATE_KHR = 0;
  VK_PRESENT_MODE_MAILBOX_KHR = 1;
  VK_PRESENT_MODE_FIFO_KHR = 2;
  VK_PRESENT_MODE_FIFO_RELAXED_KHR = 3;

  { Sharing Modes }
  VK_SHARING_MODE_EXCLUSIVE = 0;
  VK_SHARING_MODE_CONCURRENT = 1;

  { Color Spaces }
  VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = 0;

  { Sample Counts }
  VK_SAMPLE_COUNT_1_BIT = $00000001;
  VK_SAMPLE_COUNT_2_BIT = $00000002;
  VK_SAMPLE_COUNT_4_BIT = $00000004;
  VK_SAMPLE_COUNT_8_BIT = $00000008;

  { Compression Formats }
  VK_FORMAT_BC1_RGBA_UNORM_BLOCK = 133;
  VK_FORMAT_BC1_RGBA_SRGB_BLOCK = 134;
  VK_FORMAT_BC3_UNORM_BLOCK = 137;
  VK_FORMAT_BC3_SRGB_BLOCK = 138;
  VK_FORMAT_BC5_UNORM_BLOCK = 141;
  VK_FORMAT_BC7_UNORM_BLOCK = 145;
  VK_FORMAT_BC7_SRGB_BLOCK = 146;
  VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = 147;
  VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = 151;
  VK_FORMAT_ASTC_4x4_UNORM_BLOCK = 157;
  VK_FORMAT_ASTC_4x4_SRGB_BLOCK = 158;

  VK_IMAGE_TILING_OPTIMAL = 0;
  VK_IMAGE_TILING_LINEAR = 1;

  VK_PHYSICAL_DEVICE_TYPE_OTHER = 0;
  VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = 1;
  VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = 2;
  VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = 3;
  VK_PHYSICAL_DEVICE_TYPE_CPU = 4;

  VK_PIPELINE_BIND_POINT_GRAPHICS = 0;
  VK_PIPELINE_BIND_POINT_COMPUTE = 1;

  VK_STRUCTURE_TYPE_RENDERING_INFO = 1000044000;
  VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO = 1000044001;
  VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR = 1000044002;
  VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = 29;

  VK_EXT_debug_utils = 'VK_EXT_debug_utils';
  VK_KHR_maintenance1 = 'VK_KHR_maintenance1';
  VK_LAYER_KHRONOS_validation = 'VK_LAYER_KHRONOS_validation';

type
  { Base types }
  VkResult = Integer;
  VkStructureType = Integer;
  VkFlags = Cardinal;
  VkDeviceSize = UInt64;
  VkBool32 = Cardinal;
  VkSampleCountFlags = VkFlags;
  VkFormat = Integer;
  VkImageLayout = Integer;
  VkBufferUsageFlags = VkFlags;
  VkImageUsageFlags = VkFlags;
  VkMemoryPropertyFlags = VkFlags;
  VkPipelineStageFlags = VkFlags;
  VkAccessFlags = VkFlags;
  VkShaderStageFlags = VkFlags;
  VkColorComponentFlags = VkFlags;
  VkImageAspectFlags = VkFlags;
  VkIndexType = Integer;
  VkCompareOp = Integer;
  VkCullModeFlags = VkFlags;
  VkFrontFace = Integer;
  VkPolygonMode = Integer;
  VkPrimitiveTopology = Integer;
  VkDynamicState = Integer;
  VkDescriptorType = Integer;
  VkAttachmentLoadOp = Integer;
  VkAttachmentStoreOp = Integer;
  VkPresentModeKHR = Integer;
  VkSharingMode = Integer;
  VkColorSpaceKHR = Integer;
  VkFilter = Integer;
  VkSamplerMipmapMode = Integer;
  VkSamplerAddressMode = Integer;

  { Pointers to primitive types }
  PCardinal = ^Cardinal;
  PInteger = ^Integer;
  PSingle = ^Single;
  PPAnsiChar = ^PAnsiChar;
  PVkBool32 = ^VkBool32;
  PVkResult = ^VkResult;
  PVkDeviceSize = ^VkDeviceSize;
  PVkPipelineStageFlags = ^VkPipelineStageFlags;
  PVkDynamicState = ^VkDynamicState;
  PVkPresentModeKHR = ^VkPresentModeKHR;

  { Dispatchable handles }
  VkInstance = Pointer;
  VkPhysicalDevice = Pointer;
  VkDevice = Pointer;
  VkQueue = Pointer;
  VkCommandBuffer = Pointer;

  { Non-dispatchable handles }
  VkNonDispatchableHandle = UInt64;
  VkBuffer = VkNonDispatchableHandle;
  VkImage = VkNonDispatchableHandle;
  VkImageView = VkNonDispatchableHandle;
  VkDeviceMemory = VkNonDispatchableHandle;
  VkSampler = VkNonDispatchableHandle;
  VkShaderModule = VkNonDispatchableHandle;
  VkPipelineLayout = VkNonDispatchableHandle;
  VkPipeline = VkNonDispatchableHandle;
  VkPipelineCache = VkNonDispatchableHandle;
  VkDescriptorSetLayout = VkNonDispatchableHandle;
  VkDescriptorPool = VkNonDispatchableHandle;
  VkDescriptorSet = VkNonDispatchableHandle;
  VkFramebuffer = VkNonDispatchableHandle;
  VkRenderPass = VkNonDispatchableHandle;
  VkSemaphore = VkNonDispatchableHandle;
  VkFence = VkNonDispatchableHandle;
  VkSurfaceKHR = VkNonDispatchableHandle;
  VkSwapchainKHR = VkNonDispatchableHandle;
  VkCommandPool = VkNonDispatchableHandle;
  VkDebugUtilsMessengerEXT = VkNonDispatchableHandle;

  { Pointers to handles }
  PVkInstance = ^VkInstance;
  PVkPhysicalDevice = ^VkPhysicalDevice;
  PVkDevice = ^VkDevice;
  PVkQueue = ^VkQueue;
  PVkCommandBuffer = ^VkCommandBuffer;
  PVkBuffer = ^VkBuffer;
  PVkImage = ^VkImage;
  PVkImageView = ^VkImageView;
  PVkDeviceMemory = ^VkDeviceMemory;
  PVkSampler = ^VkSampler;
  PVkShaderModule = ^VkShaderModule;
  PVkPipelineLayout = ^VkPipelineLayout;
  PVkPipeline = ^VkPipeline;
  PVkPipelineCache = ^VkPipelineCache;
  PVkDescriptorSetLayout = ^VkDescriptorSetLayout;
  PVkDescriptorPool = ^VkDescriptorPool;
  PVkDescriptorSet = ^VkDescriptorSet;
  PVkSemaphore = ^VkSemaphore;
  PVkFence = ^VkFence;
  PVkSurfaceKHR = ^VkSurfaceKHR;
  PVkSwapchainKHR = ^VkSwapchainKHR;
  PVkCommandPool = ^VkCommandPool;

  PVkAllocationCallbacks = Pointer;

  { Geometry / Viewport }
  VkExtent2D = record
    width: Cardinal;
    height: Cardinal;
  end;
  PVkExtent2D = ^VkExtent2D;

  VkExtent3D = record
    width: Cardinal;
    height: Cardinal;
    depth: Cardinal;
  end;
  PVkExtent3D = ^VkExtent3D;

  VkOffset2D = record
    x: Integer;
    y: Integer;
  end;

  VkOffset3D = record
    x: Integer;
    y: Integer;
    z: Integer;
  end;

  VkRect2D = record
    offset: VkOffset2D;
    extent: VkExtent2D;
  end;
  PVkRect2D = ^VkRect2D;

  VkViewport = record
    x: Single;
    y: Single;
    width: Single;
    height: Single;
    minDepth: Single;
    maxDepth: Single;
  end;
  PVkViewport = ^VkViewport;

  { Memory requirements & properties }
  VkMemoryType = record
    propertyFlags: VkMemoryPropertyFlags;
    heapIndex: Cardinal;
  end;

  VkMemoryHeap = record
    size: VkDeviceSize;
    flags: VkFlags;
  end;

  VkPhysicalDeviceMemoryProperties = record
    memoryTypeCount: Cardinal;
    memoryTypes: array[0..VK_MAX_MEMORY_TYPES - 1] of VkMemoryType;
    memoryHeapCount: Cardinal;
    memoryHeaps: array[0..VK_MAX_MEMORY_HEAPS - 1] of VkMemoryHeap;
  end;
  PVkPhysicalDeviceMemoryProperties = ^VkPhysicalDeviceMemoryProperties;

  VkMemoryRequirements = record
    size: VkDeviceSize;
    alignment: VkDeviceSize;
    memoryTypeBits: Cardinal;
  end;
  PVkMemoryRequirements = ^VkMemoryRequirements;

  { Clear Values }
  VkClearColorValue = record
    case Integer of
      0: (float32: array[0..3] of Single);
      1: (int32: array[0..3] of Integer);
      2: (uint32: array[0..3] of Cardinal);
  end;

  VkClearDepthStencilValue = record
    depth: Single;
    stencil: Cardinal;
  end;

  VkClearValue = record
    case Integer of
      0: (color: VkClearColorValue);
      1: (depthStencil: VkClearDepthStencilValue);
  end;
  PVkClearValue = ^VkClearValue;

  { Physical device limits & properties }
  VkPhysicalDeviceLimits = record
    maxImageDimension1D: Cardinal;
    maxImageDimension2D: Cardinal;
    maxImageDimension3D: Cardinal;
    maxImageDimensionCube: Cardinal;
    maxImageArrayLayers: Cardinal;
    maxTexelBufferElements: Cardinal;
    maxUniformBufferRange: Cardinal;
    maxStorageBufferRange: Cardinal;
    maxPushConstantsSize: Cardinal;
    maxMemoryAllocationCount: Cardinal;
    maxSamplerAllocationCount: Cardinal;
    bufferImageGranularity: VkDeviceSize;
    sparseAddressSpaceSize: VkDeviceSize;
    maxBoundDescriptorSets: Cardinal;
    maxPerStageDescriptorSamplers: Cardinal;
    maxPerStageDescriptorUniformBuffers: Cardinal;
    maxPerStageDescriptorStorageBuffers: Cardinal;
    maxPerStageDescriptorSampledImages: Cardinal;
    maxPerStageDescriptorStorageImages: Cardinal;
    maxPerStageDescriptorInputAttachments: Cardinal;
    maxPerStageResources: Cardinal;
    maxDescriptorSetSamplers: Cardinal;
    maxDescriptorSetUniformBuffers: Cardinal;
    maxDescriptorSetUniformBuffersDynamic: Cardinal;
    maxDescriptorSetStorageBuffers: Cardinal;
    maxDescriptorSetStorageBuffersDynamic: Cardinal;
    maxDescriptorSetSampledImages: Cardinal;
    maxDescriptorSetStorageImages: Cardinal;
    maxDescriptorSetInputAttachments: Cardinal;
    maxVertexInputAttributes: Cardinal;
    maxVertexInputBindings: Cardinal;
    maxVertexInputAttributeOffset: Cardinal;
    maxVertexInputBindingStride: Cardinal;
    maxVertexOutputComponents: Cardinal;
    maxTessellationGenerationLevel: Cardinal;
    maxTessellationPatchSize: Cardinal;
    maxTessellationControlPerVertexInputComponents: Cardinal;
    maxTessellationControlPerVertexOutputComponents: Cardinal;
    maxTessellationControlPerPatchOutputComponents: Cardinal;
    maxTessellationControlTotalOutputComponents: Cardinal;
    maxTessellationEvaluationInputComponents: Cardinal;
    maxTessellationEvaluationOutputComponents: Cardinal;
    maxGeometryShaderInvocations: Cardinal;
    maxGeometryInputComponents: Cardinal;
    maxGeometryOutputComponents: Cardinal;
    maxGeometryOutputVertices: Cardinal;
    maxGeometryTotalOutputComponents: Cardinal;
    maxFragmentInputComponents: Cardinal;
    maxFragmentOutputAttachments: Cardinal;
    maxFragmentDualSrcAttachments: Cardinal;
    maxFragmentCombinedOutputResources: Cardinal;
    maxComputeSharedMemorySize: Cardinal;
    maxComputeWorkGroupCount: array[0..2] of Cardinal;
    maxComputeWorkGroupInvocations: Cardinal;
    maxComputeWorkGroupSize: array[0..2] of Cardinal;
    subPixelPrecisionBits: Cardinal;
    subTexelPrecisionBits: Cardinal;
    mipmapPrecisionBits: Cardinal;
    maxDrawIndexedIndexValue: Cardinal;
    maxDrawIndirectCount: Cardinal;
    maxSamplerLodBias: Single;
    maxSamplerAnisotropy: Single;
    maxViewports: Cardinal;
    maxViewportDimensions: array[0..1] of Cardinal;
    viewportBoundsRange: array[0..1] of Single;
    viewportSubPixelBits: Cardinal;
    minMemoryMapAlignment: PtrUInt;
    minTexelBufferOffsetAlignment: VkDeviceSize;
    minUniformBufferOffsetAlignment: VkDeviceSize;
    minStorageBufferOffsetAlignment: VkDeviceSize;
    minTexelOffset: Integer;
    maxTexelOffset: Cardinal;
    minTexelGatherOffset: Integer;
    maxTexelGatherOffset: Cardinal;
    minInterpolationOffset: Single;
    maxInterpolationOffset: Single;
    subPixelInterpolationOffsetBits: Cardinal;
    maxFramebufferWidth: Cardinal;
    maxFramebufferHeight: Cardinal;
    maxFramebufferLayers: Cardinal;
    framebufferColorSampleCounts: VkSampleCountFlags;
    framebufferDepthSampleCounts: VkSampleCountFlags;
    framebufferStencilSampleCounts: VkSampleCountFlags;
    framebufferNoAttachmentsSampleCounts: VkSampleCountFlags;
    maxColorAttachments: Cardinal;
    sampledImageColorSampleCounts: VkSampleCountFlags;
    sampledImageIntegerSampleCounts: VkSampleCountFlags;
    sampledImageDepthSampleCounts: VkSampleCountFlags;
    sampledImageStencilSampleCounts: VkSampleCountFlags;
    storageImageSampleCounts: VkSampleCountFlags;
    maxSampleMaskWords: Cardinal;
    timestampComputeAndGraphics: VkBool32;
    timestampPeriod: Single;
    maxClipDistances: Cardinal;
    maxCullDistances: Cardinal;
    maxCombinedClipAndCullDistances: Cardinal;
    discreteQueuePriorities: Cardinal;
    pointSizeRange: array[0..1] of Single;
    lineWidthRange: array[0..1] of Single;
    pointSizeGranularity: Single;
    lineWidthGranularity: Single;
    strictLines: VkBool32;
    standardSampleLocations: VkBool32;
    optimalBufferCopyOffsetAlignment: VkDeviceSize;
    optimalBufferCopyRowPitchAlignment: VkDeviceSize;
    nonCoherentAtomSize: VkDeviceSize;
  end;

  VkPhysicalDeviceProperties = record
    apiVersion: Cardinal;
    driverVersion: Cardinal;
    vendorID: Cardinal;
    deviceID: Cardinal;
    deviceType: Cardinal;
    deviceName: array[0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE - 1] of AnsiChar;
    pipelineCacheUUID: array[0..VK_UUID_SIZE - 1] of Byte;
    limits: VkPhysicalDeviceLimits;
    sparseProperties: array[0..4] of Cardinal;
  end;
  PVkPhysicalDeviceProperties = ^VkPhysicalDeviceProperties;

  VkPhysicalDeviceFeatures = record
    robustBufferAccess: VkBool32;
    fullDrawIndexUint32: VkBool32;
    imageCubeArray: VkBool32;
    independentBlend: VkBool32;
    geometryShader: VkBool32;
    tessellationShader: VkBool32;
    sampleRateShading: VkBool32;
    dualSrcBlend: VkBool32;
    logicOp: VkBool32;
    multiDrawIndirect: VkBool32;
    drawIndirectFirstInstance: VkBool32;
    depthClamp: VkBool32;
    depthBiasClamp: VkBool32;
    fillModeNonSolid: VkBool32;
    depthBounds: VkBool32;
    wideLines: VkBool32;
    largePoints: VkBool32;
    alphaToOne: VkBool32;
    multiViewport: VkBool32;
    samplerAnisotropy: VkBool32;
    textureCompressionETC2: VkBool32;
    textureCompressionASTC_LDR: VkBool32;
    textureCompressionBC: VkBool32;
    occlusionQueryPrecise: VkBool32;
    pipelineStatisticsQuery: VkBool32;
    vertexPipelineStoresAndAtomics: VkBool32;
    fragmentStoresAndAtomics: VkBool32;
    shaderTessellationAndGeometryPointSize: VkBool32;
    shaderImageGatherExtended: VkBool32;
    shaderStorageImageExtendedFormats: VkBool32;
    shaderStorageImageMultisample: VkBool32;
    shaderStorageImageReadWithoutFormat: VkBool32;
    shaderStorageImageWriteWithoutFormat: VkBool32;
    shaderUniformBufferArrayDynamicIndexing: VkBool32;
    shaderSampledImageArrayDynamicIndexing: VkBool32;
    shaderStorageBufferArrayDynamicIndexing: VkBool32;
    shaderStorageImageArrayDynamicIndexing: VkBool32;
    shaderClipDistance: VkBool32;
    shaderCullDistance: VkBool32;
    shaderFloat64: VkBool32;
    shaderInt64: VkBool32;
    shaderInt16: VkBool32;
    shaderResourceResidency: VkBool32;
    shaderResourceMinLod: VkBool32;
    sparseBinding: VkBool32;
    sparseResidencyBuffer: VkBool32;
    sparseResidencyImage2D: VkBool32;
    sparseResidencyImage3D: VkBool32;
    sparseResidency2Samples: VkBool32;
    sparseResidency4Samples: VkBool32;
    sparseResidency8Samples: VkBool32;
    sparseResidency16Samples: VkBool32;
    sparseResidencyAliased: VkBool32;
    variableMultisampleRate: VkBool32;
    inheritedQueries: VkBool32;
  end;
  PVkPhysicalDeviceFeatures = ^VkPhysicalDeviceFeatures;

  VkQueueFamilyProperties = record
    queueFlags: VkFlags;
    queueCount: Cardinal;
    timestampValidBits: Cardinal;
    minImageTransferGranularity: VkExtent3D;
  end;
  PVkQueueFamilyProperties = ^VkQueueFamilyProperties;

  VkExtensionProperties = record
    extensionName: array[0..VK_MAX_EXTENSION_NAME_SIZE - 1] of AnsiChar;
    specVersion: Cardinal;
  end;
  PVkExtensionProperties = ^VkExtensionProperties;

  VkLayerProperties = record
    layerName: array[0..VK_MAX_EXTENSION_NAME_SIZE - 1] of AnsiChar;
    specVersion: Cardinal;
    implementationVersion: Cardinal;
    description: array[0..VK_MAX_DESCRIPTION_SIZE - 1] of AnsiChar;
  end;
  PVkLayerProperties = ^VkLayerProperties;

  { Create infos }
  VkApplicationInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    pApplicationName: PAnsiChar;
    applicationVersion: Cardinal;
    pEngineName: PAnsiChar;
    engineVersion: Cardinal;
    apiVersion: Cardinal;
  end;
  PVkApplicationInfo = ^VkApplicationInfo;

  VkInstanceCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    pApplicationInfo: PVkApplicationInfo;
    enabledLayerCount: Cardinal;
    ppEnabledLayerNames: PPAnsiChar;
    enabledExtensionCount: Cardinal;
    ppEnabledExtensionNames: PPAnsiChar;
  end;
  PVkInstanceCreateInfo = ^VkInstanceCreateInfo;

  VkDeviceQueueCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    queueFamilyIndex: Cardinal;
    queueCount: Cardinal;
    pQueuePriorities: PSingle;
  end;
  PVkDeviceQueueCreateInfo = ^VkDeviceQueueCreateInfo;

  VkDeviceCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    queueCreateInfoCount: Cardinal;
    pQueueCreateInfos: PVkDeviceQueueCreateInfo;
    enabledLayerCount: Cardinal;
    ppEnabledLayerNames: PPAnsiChar;
    enabledExtensionCount: Cardinal;
    ppEnabledExtensionNames: PPAnsiChar;
    pEnabledFeatures: PVkPhysicalDeviceFeatures;
  end;
  PVkDeviceCreateInfo = ^VkDeviceCreateInfo;

  VkMemoryAllocateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    allocationSize: VkDeviceSize;
    memoryTypeIndex: Cardinal;
  end;
  PVkMemoryAllocateInfo = ^VkMemoryAllocateInfo;

  VkBufferCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    size: VkDeviceSize;
    usage: VkBufferUsageFlags;
    sharingMode: VkSharingMode;
    queueFamilyIndexCount: Cardinal;
    pQueueFamilyIndices: PCardinal;
  end;
  PVkBufferCreateInfo = ^VkBufferCreateInfo;

  VkImageCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    imageType: Integer;
    format: VkFormat;
    extent: VkExtent3D;
    mipLevels: Cardinal;
    arrayLayers: Cardinal;
    samples: VkSampleCountFlags;
    tiling: Integer;
    usage: VkImageUsageFlags;
    sharingMode: VkSharingMode;
    queueFamilyIndexCount: Cardinal;
    pQueueFamilyIndices: PCardinal;
    initialLayout: VkImageLayout;
  end;
  PVkImageCreateInfo = ^VkImageCreateInfo;

  VkComponentMapping = record
    r: Integer;
    g: Integer;
    b: Integer;
    a: Integer;
  end;

  VkImageSubresourceRange = record
    aspectMask: VkImageAspectFlags;
    baseMipLevel: Cardinal;
    levelCount: Cardinal;
    baseArrayLayer: Cardinal;
    layerCount: Cardinal;
  end;
  PVkImageSubresourceRange = ^VkImageSubresourceRange;

  VkImageViewCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    image: VkImage;
    viewType: Integer;
    format: VkFormat;
    components: VkComponentMapping;
    subresourceRange: VkImageSubresourceRange;
  end;
  PVkImageViewCreateInfo = ^VkImageViewCreateInfo;

  VkSamplerCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    magFilter: VkFilter;
    minFilter: VkFilter;
    mipmapMode: VkSamplerMipmapMode;
    addressModeU: VkSamplerAddressMode;
    addressModeV: VkSamplerAddressMode;
    addressModeW: VkSamplerAddressMode;
    mipLodBias: Single;
    anisotropyEnable: VkBool32;
    maxAnisotropy: Single;
    compareEnable: VkBool32;
    compareOp: VkCompareOp;
    minLod: Single;
    maxLod: Single;
    borderColor: Integer;
    unnormalizedCoordinates: VkBool32;
  end;
  PVkSamplerCreateInfo = ^VkSamplerCreateInfo;

  VkShaderModuleCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    codeSize: PtrUInt;
    pCode: PCardinal;
  end;
  PVkShaderModuleCreateInfo = ^VkShaderModuleCreateInfo;

  { Descriptor Set Layout & Pool }
  VkDescriptorSetLayoutBinding = record
    binding: Cardinal;
    descriptorType: VkDescriptorType;
    descriptorCount: Cardinal;
    stageFlags: VkShaderStageFlags;
    pImmutableSamplers: PVkSampler;
  end;
  PVkDescriptorSetLayoutBinding = ^VkDescriptorSetLayoutBinding;

  VkDescriptorSetLayoutCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    bindingCount: Cardinal;
    pBindings: PVkDescriptorSetLayoutBinding;
  end;
  PVkDescriptorSetLayoutCreateInfo = ^VkDescriptorSetLayoutCreateInfo;

  VkDescriptorPoolSize = record
    _type: VkDescriptorType;
    descriptorCount: Cardinal;
  end;
  PVkDescriptorPoolSize = ^VkDescriptorPoolSize;

  VkDescriptorPoolCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    maxSets: Cardinal;
    poolSizeCount: Cardinal;
    pPoolSizes: PVkDescriptorPoolSize;
  end;
  PVkDescriptorPoolCreateInfo = ^VkDescriptorPoolCreateInfo;

  VkDescriptorSetAllocateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    descriptorPool: VkDescriptorPool;
    descriptorSetCount: Cardinal;
    pSetLayouts: PVkDescriptorSetLayout;
  end;
  PVkDescriptorSetAllocateInfo = ^VkDescriptorSetAllocateInfo;

  VkDescriptorImageInfo = record
    sampler: VkSampler;
    imageView: VkImageView;
    imageLayout: VkImageLayout;
  end;
  PVkDescriptorImageInfo = ^VkDescriptorImageInfo;

  VkDescriptorBufferInfo = record
    buffer: VkBuffer;
    offset: VkDeviceSize;
    range: VkDeviceSize;
  end;
  PVkDescriptorBufferInfo = ^VkDescriptorBufferInfo;

  VkWriteDescriptorSet = record
    sType: VkStructureType;
    pNext: Pointer;
    dstSet: VkDescriptorSet;
    dstBinding: Cardinal;
    dstArrayElement: Cardinal;
    descriptorCount: Cardinal;
    descriptorType: VkDescriptorType;
    pImageInfo: PVkDescriptorImageInfo;
    pBufferInfo: PVkDescriptorBufferInfo;
    pTexelBufferView: Pointer;
  end;
  PVkWriteDescriptorSet = ^VkWriteDescriptorSet;

  { Pipeline Layout & Push Constants }
  VkPushConstantRange = record
    stageFlags: VkShaderStageFlags;
    offset: Cardinal;
    size: Cardinal;
  end;
  PVkPushConstantRange = ^VkPushConstantRange;

  VkPipelineLayoutCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    setLayoutCount: Cardinal;
    pSetLayouts: PVkDescriptorSetLayout;
    pushConstantRangeCount: Cardinal;
    pPushConstantRanges: PVkPushConstantRange;
  end;
  PVkPipelineLayoutCreateInfo = ^VkPipelineLayoutCreateInfo;

  { Graphics Pipeline States }
  VkSpecializationMapEntry = record
    constantID: Cardinal;
    offset: Cardinal;
    size: PtrUInt;
  end;
  PVkSpecializationMapEntry = ^VkSpecializationMapEntry;

  VkSpecializationInfo = record
    mapEntryCount: Cardinal;
    pMapEntries: PVkSpecializationMapEntry;
    dataSize: PtrUInt;
    pData: Pointer;
  end;
  PVkSpecializationInfo = ^VkSpecializationInfo;

  VkPipelineShaderStageCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    stage: Integer;
    module: VkShaderModule;
    pName: PAnsiChar;
    pSpecializationInfo: PVkSpecializationInfo;
  end;
  PVkPipelineShaderStageCreateInfo = ^VkPipelineShaderStageCreateInfo;

  VkVertexInputBindingDescription = record
    binding: Cardinal;
    stride: Cardinal;
    inputRate: Integer;
  end;
  PVkVertexInputBindingDescription = ^VkVertexInputBindingDescription;

  VkVertexInputAttributeDescription = record
    location: Cardinal;
    binding: Cardinal;
    format: VkFormat;
    offset: Cardinal;
  end;
  PVkVertexInputAttributeDescription = ^VkVertexInputAttributeDescription;

  VkPipelineVertexInputStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    vertexBindingDescriptionCount: Cardinal;
    pVertexBindingDescriptions: PVkVertexInputBindingDescription;
    vertexAttributeDescriptionCount: Cardinal;
    pVertexAttributeDescriptions: PVkVertexInputAttributeDescription;
  end;
  PVkPipelineVertexInputStateCreateInfo = ^VkPipelineVertexInputStateCreateInfo;

  VkPipelineInputAssemblyStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    topology: VkPrimitiveTopology;
    primitiveRestartEnable: VkBool32;
  end;
  PVkPipelineInputAssemblyStateCreateInfo = ^VkPipelineInputAssemblyStateCreateInfo;

  VkPipelineViewportStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    viewportCount: Cardinal;
    pViewports: PVkViewport;
    scissorCount: Cardinal;
    pScissors: PVkRect2D;
  end;
  PVkPipelineViewportStateCreateInfo = ^VkPipelineViewportStateCreateInfo;

  VkPipelineRasterizationStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    depthClampEnable: VkBool32;
    rasterizerDiscardEnable: VkBool32;
    polygonMode: VkPolygonMode;
    cullMode: VkCullModeFlags;
    frontFace: VkFrontFace;
    depthBiasEnable: VkBool32;
    depthBiasConstantFactor: Single;
    depthBiasClamp: Single;
    depthBiasSlopeFactor: Single;
    lineWidth: Single;
  end;
  PVkPipelineRasterizationStateCreateInfo = ^VkPipelineRasterizationStateCreateInfo;

  VkPipelineMultisampleStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    rasterizationSamples: VkSampleCountFlags;
    sampleShadingEnable: VkBool32;
    minSampleShading: Single;
    pSampleMask: PCardinal;
    alphaToCoverageEnable: VkBool32;
    alphaToOneEnable: VkBool32;
  end;
  PVkPipelineMultisampleStateCreateInfo = ^VkPipelineMultisampleStateCreateInfo;

  VkStencilOpState = record
    failOp: Integer;
    passOp: Integer;
    depthFailOp: Integer;
    compareOp: VkCompareOp;
    compareMask: Cardinal;
    writeMask: Cardinal;
    reference: Cardinal;
  end;

  VkPipelineDepthStencilStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    depthTestEnable: VkBool32;
    depthWriteEnable: VkBool32;
    depthCompareOp: VkCompareOp;
    depthBoundsTestEnable: VkBool32;
    stencilTestEnable: VkBool32;
    front: VkStencilOpState;
    back: VkStencilOpState;
    minDepthBounds: Single;
    maxDepthBounds: Single;
  end;
  PVkPipelineDepthStencilStateCreateInfo = ^VkPipelineDepthStencilStateCreateInfo;

  VkPipelineColorBlendAttachmentState = record
    blendEnable: VkBool32;
    srcColorBlendFactor: Integer;
    dstColorBlendFactor: Integer;
    colorBlendOp: Integer;
    srcAlphaBlendFactor: Integer;
    dstAlphaBlendFactor: Integer;
    alphaBlendOp: Integer;
    colorWriteMask: VkColorComponentFlags;
  end;
  PVkPipelineColorBlendAttachmentState = ^VkPipelineColorBlendAttachmentState;

  VkPipelineColorBlendStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    logicOpEnable: VkBool32;
    logicOp: Integer;
    attachmentCount: Cardinal;
    pAttachments: PVkPipelineColorBlendAttachmentState;
    blendConstants: array[0..3] of Single;
  end;
  PVkPipelineColorBlendStateCreateInfo = ^VkPipelineColorBlendStateCreateInfo;

  VkPipelineDynamicStateCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    dynamicStateCount: Cardinal;
    pDynamicStates: PVkDynamicState;
  end;
  PVkPipelineDynamicStateCreateInfo = ^VkPipelineDynamicStateCreateInfo;

  VkGraphicsPipelineCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    stageCount: Cardinal;
    pStages: PVkPipelineShaderStageCreateInfo;
    pVertexInputState: PVkPipelineVertexInputStateCreateInfo;
    pInputAssemblyState: PVkPipelineInputAssemblyStateCreateInfo;
    pTessellationState: Pointer;
    pViewportState: PVkPipelineViewportStateCreateInfo;
    pRasterizationState: PVkPipelineRasterizationStateCreateInfo;
    pMultisampleState: PVkPipelineMultisampleStateCreateInfo;
    pDepthStencilState: PVkPipelineDepthStencilStateCreateInfo;
    pColorBlendState: PVkPipelineColorBlendStateCreateInfo;
    pDynamicState: PVkPipelineDynamicStateCreateInfo;
    layout: VkPipelineLayout;
    renderPass: VkRenderPass;
    subpass: Cardinal;
    basePipelineHandle: VkPipeline;
    basePipelineIndex: Integer;
  end;
  PVkGraphicsPipelineCreateInfo = ^VkGraphicsPipelineCreateInfo;

  { Dynamic Rendering (VK_KHR_dynamic_rendering / Vulkan 1.3) }
  VkRenderingAttachmentInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    imageView: VkImageView;
    imageLayout: VkImageLayout;
    resolveMode: Integer;
    resolveImageView: VkImageView;
    resolveImageLayout: VkImageLayout;
    loadOp: VkAttachmentLoadOp;
    storeOp: VkAttachmentStoreOp;
    clearValue: VkClearValue;
  end;
  PVkRenderingAttachmentInfoKHR = ^VkRenderingAttachmentInfoKHR;

  VkRenderingInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    renderArea: VkRect2D;
    layerCount: Cardinal;
    viewMask: Cardinal;
    colorAttachmentCount: Cardinal;
    pColorAttachments: PVkRenderingAttachmentInfoKHR;
    pDepthAttachment: PVkRenderingAttachmentInfoKHR;
    pStencilAttachment: PVkRenderingAttachmentInfoKHR;
  end;
  PVkRenderingInfoKHR = ^VkRenderingInfoKHR;

  VkPhysicalDeviceDynamicRenderingFeaturesKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    dynamicRendering: VkBool32;
  end;
  PVkPhysicalDeviceDynamicRenderingFeaturesKHR = ^VkPhysicalDeviceDynamicRenderingFeaturesKHR;

  { Command Pools, Buffers, and Submission }
  VkCommandPoolCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    queueFamilyIndex: Cardinal;
  end;
  PVkCommandPoolCreateInfo = ^VkCommandPoolCreateInfo;

  VkCommandBufferAllocateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    commandPool: VkCommandPool;
    level: Integer;
    commandBufferCount: Cardinal;
  end;
  PVkCommandBufferAllocateInfo = ^VkCommandBufferAllocateInfo;

  VkCommandBufferInheritanceInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    renderPass: VkRenderPass;
    subpass: Cardinal;
    framebuffer: VkFramebuffer;
    occlusionQueryEnable: VkBool32;
    queryFlags: VkFlags;
    pipelineStatistics: VkFlags;
  end;
  PVkCommandBufferInheritanceInfo = ^VkCommandBufferInheritanceInfo;

  VkCommandBufferBeginInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    pInheritanceInfo: PVkCommandBufferInheritanceInfo;
  end;
  PVkCommandBufferBeginInfo = ^VkCommandBufferBeginInfo;

  VkFenceCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
  end;
  PVkFenceCreateInfo = ^VkFenceCreateInfo;

  VkSemaphoreCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
  end;
  PVkSemaphoreCreateInfo = ^VkSemaphoreCreateInfo;

  VkSubmitInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    waitSemaphoreCount: Cardinal;
    pWaitSemaphores: PVkSemaphore;
    pWaitDstStageMask: PVkPipelineStageFlags;
    commandBufferCount: Cardinal;
    pCommandBuffers: PVkCommandBuffer;
    signalSemaphoreCount: Cardinal;
    pSignalSemaphores: PVkSemaphore;
  end;
  PVkSubmitInfo = ^VkSubmitInfo;

  { Barriers & Copies }
  VkBufferMemoryBarrier = record
    sType: VkStructureType;
    pNext: Pointer;
    srcAccessMask: VkAccessFlags;
    dstAccessMask: VkAccessFlags;
    srcQueueFamilyIndex: Cardinal;
    dstQueueFamilyIndex: Cardinal;
    buffer: VkBuffer;
    offset: VkDeviceSize;
    size: VkDeviceSize;
  end;
  PVkBufferMemoryBarrier = ^VkBufferMemoryBarrier;

  VkImageMemoryBarrier = record
    sType: VkStructureType;
    pNext: Pointer;
    srcAccessMask: VkAccessFlags;
    dstAccessMask: VkAccessFlags;
    oldLayout: VkImageLayout;
    newLayout: VkImageLayout;
    srcQueueFamilyIndex: Cardinal;
    dstQueueFamilyIndex: Cardinal;
    image: VkImage;
    subresourceRange: VkImageSubresourceRange;
  end;
  PVkImageMemoryBarrier = ^VkImageMemoryBarrier;

  VkBufferCopy = record
    srcOffset: VkDeviceSize;
    dstOffset: VkDeviceSize;
    size: VkDeviceSize;
  end;
  PVkBufferCopy = ^VkBufferCopy;

  VkImageSubresourceLayers = record
    aspectMask: VkImageAspectFlags;
    mipLevel: Cardinal;
    baseArrayLayer: Cardinal;
    layerCount: Cardinal;
  end;

  VkBufferImageCopy = record
    bufferOffset: VkDeviceSize;
    bufferRowLength: Cardinal;
    bufferImageHeight: Cardinal;
    imageSubresource: VkImageSubresourceLayers;
    imageOffset: VkOffset3D;
    imageExtent: VkExtent3D;
  end;
  PVkBufferImageCopy = ^VkBufferImageCopy;

  { Swapchain & WSI }
  VkSurfaceCapabilitiesKHR = record
    minImageCount: Cardinal;
    maxImageCount: Cardinal;
    currentExtent: VkExtent2D;
    minImageExtent: VkExtent2D;
    maxImageExtent: VkExtent2D;
    maxImageArrayLayers: Cardinal;
    supportedTransforms: VkFlags;
    currentTransform: Integer;
    supportedCompositeAlpha: VkFlags;
    supportedUsageFlags: VkImageUsageFlags;
  end;
  PVkSurfaceCapabilitiesKHR = ^VkSurfaceCapabilitiesKHR;

  VkSurfaceFormatKHR = record
    format: VkFormat;
    colorSpace: VkColorSpaceKHR;
  end;
  PVkSurfaceFormatKHR = ^VkSurfaceFormatKHR;

  VkSwapchainCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    surface: VkSurfaceKHR;
    minImageCount: Cardinal;
    imageFormat: VkFormat;
    imageColorSpace: VkColorSpaceKHR;
    imageExtent: VkExtent2D;
    imageArrayLayers: Cardinal;
    imageUsage: VkImageUsageFlags;
    imageSharingMode: VkSharingMode;
    queueFamilyIndexCount: Cardinal;
    pQueueFamilyIndices: PCardinal;
    preTransform: Integer;
    compositeAlpha: Integer;
    presentMode: VkPresentModeKHR;
    clipped: VkBool32;
    oldSwapchain: VkSwapchainKHR;
  end;
  PVkSwapchainCreateInfoKHR = ^VkSwapchainCreateInfoKHR;

  VkPresentInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    waitSemaphoreCount: Cardinal;
    pWaitSemaphores: PVkSemaphore;
    swapchainCount: Cardinal;
    pSwapchains: PVkSwapchainKHR;
    pImageIndices: PCardinal;
    pResults: PVkResult;
  end;
  PVkPresentInfoKHR = ^VkPresentInfoKHR;

  PVkFormat = ^VkFormat;
  PPtrUInt = ^PtrUInt;
  VkRenderingInfo = VkRenderingInfoKHR;
  PVkRenderingInfo = ^VkRenderingInfo;
  VkRenderingAttachmentInfo = VkRenderingAttachmentInfoKHR;
  PVkRenderingAttachmentInfo = ^VkRenderingAttachmentInfo;
  VkPhysicalDeviceDynamicRenderingFeatures = VkPhysicalDeviceDynamicRenderingFeaturesKHR;
  PVkPhysicalDeviceDynamicRenderingFeatures = ^VkPhysicalDeviceDynamicRenderingFeatures;

  VkPipelineRenderingCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    viewMask: Cardinal;
    colorAttachmentCount: Cardinal;
    pColorAttachmentFormats: PVkFormat;
    depthAttachmentFormat: VkFormat;
    stencilAttachmentFormat: VkFormat;
  end;
  PVkPipelineRenderingCreateInfoKHR = ^VkPipelineRenderingCreateInfoKHR;

  VkDebugUtilsLabelEXT = record
    sType: VkStructureType;
    pNext: Pointer;
    pLabelName: PAnsiChar;
    color: array[0..3] of Single;
  end;
  PVkDebugUtilsLabelEXT = ^VkDebugUtilsLabelEXT;

  VkImageBlit = record
    srcSubresource: VkImageSubresourceLayers;
    srcOffsets: array[0..1] of VkOffset3D;
    dstSubresource: VkImageSubresourceLayers;
    dstOffsets: array[0..1] of VkOffset3D;
  end;
  PVkImageBlit = ^VkImageBlit;

  VkDrawIndexedIndirectCommand = record
    indexCount: Cardinal;
    instanceCount: Cardinal;
    firstIndex: Cardinal;
    vertexOffset: Integer;
    firstInstance: Cardinal;
  end;
  PVkDrawIndexedIndirectCommand = ^VkDrawIndexedIndirectCommand;

  VkPipelineCacheCreateInfo = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkFlags;
    initialDataSize: PtrUInt;
    pInitialData: Pointer;
  end;
  PVkPipelineCacheCreateInfo = ^VkPipelineCacheCreateInfo;

{ -------------------------------------------------------------------------- }
{ Function Pointers                                                          }
{ -------------------------------------------------------------------------- }

{$ifdef MSWINDOWS}
  {$define VKAPI_CALL := stdcall}
{$else}
  {$define VKAPI_CALL := cdecl}
{$endif}

type
  PFN_vkGetInstanceProcAddr = function(instance: VkInstance; pName: PAnsiChar): Pointer; VKAPI_CALL;
  PFN_vkGetDeviceProcAddr = function(device: VkDevice; pName: PAnsiChar): Pointer; VKAPI_CALL;

  { Instance functions }
  PFN_vkCreateInstance = function(const pCreateInfo: PVkInstanceCreateInfo; const pAllocator: PVkAllocationCallbacks; pInstance: PVkInstance): VkResult; VKAPI_CALL;
  PFN_vkDestroyInstance = procedure(instance: VkInstance; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkEnumeratePhysicalDevices = function(instance: VkInstance; pPhysicalDeviceCount: PCardinal; pPhysicalDevices: PVkPhysicalDevice): VkResult; VKAPI_CALL;
  PFN_vkGetPhysicalDeviceFeatures = procedure(physicalDevice: VkPhysicalDevice; pFeatures: PVkPhysicalDeviceFeatures); VKAPI_CALL;
  PFN_vkGetPhysicalDeviceProperties = procedure(physicalDevice: VkPhysicalDevice; pProperties: PVkPhysicalDeviceProperties); VKAPI_CALL;
  PFN_vkGetPhysicalDeviceQueueFamilyProperties = procedure(physicalDevice: VkPhysicalDevice; pQueueFamilyPropertyCount: PCardinal; pQueueFamilyProperties: PVkQueueFamilyProperties); VKAPI_CALL;
  PFN_vkGetPhysicalDeviceMemoryProperties = procedure(physicalDevice: VkPhysicalDevice; pMemoryProperties: PVkPhysicalDeviceMemoryProperties); VKAPI_CALL;
  PFN_vkCreateDevice = function(physicalDevice: VkPhysicalDevice; const pCreateInfo: PVkDeviceCreateInfo; const pAllocator: PVkAllocationCallbacks; pDevice: PVkDevice): VkResult; VKAPI_CALL;
  PFN_vkDestroyDevice = procedure(device: VkDevice; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkEnumerateInstanceExtensionProperties = function(pLayerName: PAnsiChar; pPropertyCount: PCardinal; pProperties: PVkExtensionProperties): VkResult; VKAPI_CALL;
  PFN_vkEnumerateDeviceExtensionProperties = function(physicalDevice: VkPhysicalDevice; pLayerName: PAnsiChar; pPropertyCount: PCardinal; pProperties: PVkExtensionProperties): VkResult; VKAPI_CALL;

  { Surface & WSI }
  PFN_vkDestroySurfaceKHR = procedure(instance: VkInstance; surface: VkSurfaceKHR; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkGetPhysicalDeviceSurfaceSupportKHR = function(physicalDevice: VkPhysicalDevice; queueFamilyIndex: Cardinal; surface: VkSurfaceKHR; pSupported: PVkBool32): VkResult; VKAPI_CALL;
  PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR = function(physicalDevice: VkPhysicalDevice; surface: VkSurfaceKHR; pSurfaceCapabilities: PVkSurfaceCapabilitiesKHR): VkResult; VKAPI_CALL;
  PFN_vkGetPhysicalDeviceSurfaceFormatsKHR = function(physicalDevice: VkPhysicalDevice; surface: VkSurfaceKHR; pSurfaceFormatCount: PCardinal; pSurfaceFormats: PVkSurfaceFormatKHR): VkResult; VKAPI_CALL;
  PFN_vkGetPhysicalDeviceSurfacePresentModesKHR = function(physicalDevice: VkPhysicalDevice; surface: VkSurfaceKHR; pPresentModeCount: PCardinal; pPresentModes: PVkPresentModeKHR): VkResult; VKAPI_CALL;

  { Device Queue & Sync }
  PFN_vkGetDeviceQueue = procedure(device: VkDevice; queueFamilyIndex: Cardinal; queueIndex: Cardinal; pQueue: PVkQueue); VKAPI_CALL;
  PFN_vkQueueSubmit = function(queue: VkQueue; submitCount: Cardinal; const pSubmits: PVkSubmitInfo; fence: VkFence): VkResult; VKAPI_CALL;
  PFN_vkQueueWaitIdle = function(queue: VkQueue): VkResult; VKAPI_CALL;
  PFN_vkDeviceWaitIdle = function(device: VkDevice): VkResult; VKAPI_CALL;
  PFN_vkCreateFence = function(device: VkDevice; const pCreateInfo: PVkFenceCreateInfo; const pAllocator: PVkAllocationCallbacks; pFence: PVkFence): VkResult; VKAPI_CALL;
  PFN_vkDestroyFence = procedure(device: VkDevice; fence: VkFence; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkResetFences = function(device: VkDevice; fenceCount: Cardinal; const pFences: PVkFence): VkResult; VKAPI_CALL;
  PFN_vkWaitForFences = function(device: VkDevice; fenceCount: Cardinal; const pFences: PVkFence; waitAll: VkBool32; timeout: UInt64): VkResult; VKAPI_CALL;
  PFN_vkCreateSemaphore = function(device: VkDevice; const pCreateInfo: PVkSemaphoreCreateInfo; const pAllocator: PVkAllocationCallbacks; pSemaphore: PVkSemaphore): VkResult; VKAPI_CALL;
  PFN_vkDestroySemaphore = procedure(device: VkDevice; semaphore: VkSemaphore; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;

  { Memory & Buffers }
  PFN_vkAllocateMemory = function(device: VkDevice; const pAllocateInfo: PVkMemoryAllocateInfo; const pAllocator: PVkAllocationCallbacks; pMemory: PVkDeviceMemory): VkResult; VKAPI_CALL;
  PFN_vkFreeMemory = procedure(device: VkDevice; memory: VkDeviceMemory; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkMapMemory = function(device: VkDevice; memory: VkDeviceMemory; offset: VkDeviceSize; size: VkDeviceSize; flags: VkFlags; ppData: PPointer): VkResult; VKAPI_CALL;
  PFN_vkUnmapMemory = procedure(device: VkDevice; memory: VkDeviceMemory); VKAPI_CALL;
  PFN_vkBindBufferMemory = function(device: VkDevice; buffer: VkBuffer; memory: VkDeviceMemory; memoryOffset: VkDeviceSize): VkResult; VKAPI_CALL;
  PFN_vkBindImageMemory = function(device: VkDevice; image: VkImage; memory: VkDeviceMemory; memoryOffset: VkDeviceSize): VkResult; VKAPI_CALL;
  PFN_vkGetBufferMemoryRequirements = procedure(device: VkDevice; buffer: VkBuffer; pMemoryRequirements: PVkMemoryRequirements); VKAPI_CALL;
  PFN_vkGetImageMemoryRequirements = procedure(device: VkDevice; image: VkImage; pMemoryRequirements: PVkMemoryRequirements); VKAPI_CALL;
  PFN_vkCreateBuffer = function(device: VkDevice; const pCreateInfo: PVkBufferCreateInfo; const pAllocator: PVkAllocationCallbacks; pBuffer: PVkBuffer): VkResult; VKAPI_CALL;
  PFN_vkDestroyBuffer = procedure(device: VkDevice; buffer: VkBuffer; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;

  { Images & Samplers }
  PFN_vkCreateImage = function(device: VkDevice; const pCreateInfo: PVkImageCreateInfo; const pAllocator: PVkAllocationCallbacks; pImage: PVkImage): VkResult; VKAPI_CALL;
  PFN_vkDestroyImage = procedure(device: VkDevice; image: VkImage; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkCreateImageView = function(device: VkDevice; const pCreateInfo: PVkImageViewCreateInfo; const pAllocator: PVkAllocationCallbacks; pView: PVkImageView): VkResult; VKAPI_CALL;
  PFN_vkDestroyImageView = procedure(device: VkDevice; imageView: VkImageView; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkCreateSampler = function(device: VkDevice; const pCreateInfo: PVkSamplerCreateInfo; const pAllocator: PVkAllocationCallbacks; pSampler: PVkSampler): VkResult; VKAPI_CALL;
  PFN_vkDestroySampler = procedure(device: VkDevice; sampler: VkSampler; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;

  { Shaders & Pipelines }
  PFN_vkCreateShaderModule = function(device: VkDevice; const pCreateInfo: PVkShaderModuleCreateInfo; const pAllocator: PVkAllocationCallbacks; pShaderModule: PVkShaderModule): VkResult; VKAPI_CALL;
  PFN_vkDestroyShaderModule = procedure(device: VkDevice; shaderModule: VkShaderModule; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkCreatePipelineLayout = function(device: VkDevice; const pCreateInfo: PVkPipelineLayoutCreateInfo; const pAllocator: PVkAllocationCallbacks; pPipelineLayout: PVkPipelineLayout): VkResult; VKAPI_CALL;
  PFN_vkDestroyPipelineLayout = procedure(device: VkDevice; pipelineLayout: VkPipelineLayout; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkCreateGraphicsPipelines = function(device: VkDevice; pipelineCache: VkPipelineCache; createInfoCount: Cardinal; const pCreateInfos: PVkGraphicsPipelineCreateInfo; const pAllocator: PVkAllocationCallbacks; pPipelines: PVkPipeline): VkResult; VKAPI_CALL;
  PFN_vkDestroyPipeline = procedure(device: VkDevice; pipeline: VkPipeline; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;

  { Descriptors }
  PFN_vkCreateDescriptorSetLayout = function(device: VkDevice; const pCreateInfo: PVkDescriptorSetLayoutCreateInfo; const pAllocator: PVkAllocationCallbacks; pSetLayout: PVkDescriptorSetLayout): VkResult; VKAPI_CALL;
  PFN_vkDestroyDescriptorSetLayout = procedure(device: VkDevice; descriptorSetLayout: VkDescriptorSetLayout; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkCreateDescriptorPool = function(device: VkDevice; const pCreateInfo: PVkDescriptorPoolCreateInfo; const pAllocator: PVkAllocationCallbacks; pDescriptorPool: PVkDescriptorPool): VkResult; VKAPI_CALL;
  PFN_vkDestroyDescriptorPool = procedure(device: VkDevice; descriptorPool: VkDescriptorPool; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkAllocateDescriptorSets = function(device: VkDevice; const pAllocateInfo: PVkDescriptorSetAllocateInfo; pDescriptorSets: PVkDescriptorSet): VkResult; VKAPI_CALL;
  PFN_vkFreeDescriptorSets = function(device: VkDevice; descriptorPool: VkDescriptorPool; descriptorSetCount: Cardinal; const pDescriptorSets: PVkDescriptorSet): VkResult; VKAPI_CALL;
  PFN_vkUpdateDescriptorSets = procedure(device: VkDevice; descriptorWriteCount: Cardinal; const pDescriptorWrites: PVkWriteDescriptorSet; descriptorCopyCount: Cardinal; const pDescriptorCopies: Pointer); VKAPI_CALL;

  { Commands }
  PFN_vkCreateCommandPool = function(device: VkDevice; const pCreateInfo: PVkCommandPoolCreateInfo; const pAllocator: PVkAllocationCallbacks; pCommandPool: PVkCommandPool): VkResult; VKAPI_CALL;
  PFN_vkDestroyCommandPool = procedure(device: VkDevice; commandPool: VkCommandPool; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkResetCommandPool = function(device: VkDevice; commandPool: VkCommandPool; flags: VkFlags): VkResult; VKAPI_CALL;
  PFN_vkAllocateCommandBuffers = function(device: VkDevice; const pAllocateInfo: PVkCommandBufferAllocateInfo; pCommandBuffers: PVkCommandBuffer): VkResult; VKAPI_CALL;
  PFN_vkFreeCommandBuffers = procedure(device: VkDevice; commandPool: VkCommandPool; commandBufferCount: Cardinal; const pCommandBuffers: PVkCommandBuffer); VKAPI_CALL;
  PFN_vkBeginCommandBuffer = function(commandBuffer: VkCommandBuffer; const pBeginInfo: PVkCommandBufferBeginInfo): VkResult; VKAPI_CALL;
  PFN_vkEndCommandBuffer = function(commandBuffer: VkCommandBuffer): VkResult; VKAPI_CALL;
  PFN_vkCmdBindPipeline = procedure(commandBuffer: VkCommandBuffer; pipelineBindPoint: Integer; pipeline: VkPipeline); VKAPI_CALL;
  PFN_vkCmdSetViewport = procedure(commandBuffer: VkCommandBuffer; firstViewport: Cardinal; viewportCount: Cardinal; const pViewports: PVkViewport); VKAPI_CALL;
  PFN_vkCmdSetScissor = procedure(commandBuffer: VkCommandBuffer; firstScissor: Cardinal; scissorCount: Cardinal; const pScissors: PVkRect2D); VKAPI_CALL;
  PFN_vkCmdBindDescriptorSets = procedure(commandBuffer: VkCommandBuffer; pipelineBindPoint: Integer; layout: VkPipelineLayout; firstSet: Cardinal; descriptorSetCount: Cardinal; const pDescriptorSets: PVkDescriptorSet; dynamicOffsetCount: Cardinal; const pDynamicOffsets: PCardinal); VKAPI_CALL;
  PFN_vkCmdBindIndexBuffer = procedure(commandBuffer: VkCommandBuffer; buffer: VkBuffer; offset: VkDeviceSize; indexType: VkIndexType); VKAPI_CALL;
  PFN_vkCmdBindVertexBuffers = procedure(commandBuffer: VkCommandBuffer; firstBinding: Cardinal; bindingCount: Cardinal; const pBuffers: PVkBuffer; const pOffsets: PVkDeviceSize); VKAPI_CALL;
  PFN_vkCmdDraw = procedure(commandBuffer: VkCommandBuffer; vertexCount: Cardinal; instanceCount: Cardinal; firstVertex: Cardinal; firstInstance: Cardinal); VKAPI_CALL;
  PFN_vkCmdDrawIndexed = procedure(commandBuffer: VkCommandBuffer; indexCount: Cardinal; instanceCount: Cardinal; firstIndex: Cardinal; vertexOffset: Integer; firstInstance: Cardinal); VKAPI_CALL;
  PFN_vkCmdPushConstants = procedure(commandBuffer: VkCommandBuffer; layout: VkPipelineLayout; stageFlags: VkShaderStageFlags; offset: Cardinal; size: Cardinal; const pValues: Pointer); VKAPI_CALL;
  PFN_vkCmdPipelineBarrier = procedure(commandBuffer: VkCommandBuffer; srcStageMask: VkPipelineStageFlags; dstStageMask: VkPipelineStageFlags; dependencyFlags: VkFlags; memoryBarrierCount: Cardinal; const pMemoryBarriers: Pointer; bufferMemoryBarrierCount: Cardinal; const pBufferMemoryBarriers: PVkBufferMemoryBarrier; imageMemoryBarrierCount: Cardinal; const pImageMemoryBarriers: PVkImageMemoryBarrier); VKAPI_CALL;
  PFN_vkCmdCopyBuffer = procedure(commandBuffer: VkCommandBuffer; srcBuffer: VkBuffer; dstBuffer: VkBuffer; regionCount: Cardinal; const pRegions: PVkBufferCopy); VKAPI_CALL;
  PFN_vkCmdCopyBufferToImage = procedure(commandBuffer: VkCommandBuffer; srcBuffer: VkBuffer; dstImage: VkImage; dstImageLayout: VkImageLayout; regionCount: Cardinal; const pRegions: PVkBufferImageCopy); VKAPI_CALL;

  { Dynamic Rendering }
  PFN_vkCmdBeginRendering = procedure(commandBuffer: VkCommandBuffer; const pRenderingInfo: PVkRenderingInfoKHR); VKAPI_CALL;
  PFN_vkCmdEndRendering = procedure(commandBuffer: VkCommandBuffer); VKAPI_CALL;

  { Swapchain }
  PFN_vkCreateSwapchainKHR = function(device: VkDevice; const pCreateInfo: PVkSwapchainCreateInfoKHR; const pAllocator: PVkAllocationCallbacks; pSwapchain: PVkSwapchainKHR): VkResult; VKAPI_CALL;
  PFN_vkDestroySwapchainKHR = procedure(device: VkDevice; swapchain: VkSwapchainKHR; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkGetSwapchainImagesKHR = function(device: VkDevice; swapchain: VkSwapchainKHR; pSwapchainImageCount: PCardinal; pSwapchainImages: PVkImage): VkResult; VKAPI_CALL;
  PFN_vkAcquireNextImageKHR = function(device: VkDevice; swapchain: VkSwapchainKHR; timeout: UInt64; semaphore: VkSemaphore; fence: VkFence; pImageIndex: PCardinal): VkResult; VKAPI_CALL;
  PFN_vkQueuePresentKHR = function(queue: VkQueue; const pPresentInfo: PVkPresentInfoKHR): VkResult; VKAPI_CALL;

  PFN_vkCmdBeginDebugUtilsLabelEXT = procedure(commandBuffer: VkCommandBuffer; const pLabelInfo: PVkDebugUtilsLabelEXT); VKAPI_CALL;
  PFN_vkCmdEndDebugUtilsLabelEXT = procedure(commandBuffer: VkCommandBuffer); VKAPI_CALL;
  PFN_vkCmdBlitImage = procedure(commandBuffer: VkCommandBuffer; srcImage: VkImage; srcImageLayout: VkImageLayout; dstImage: VkImage; dstImageLayout: VkImageLayout; regionCount: Cardinal; const pRegions: PVkImageBlit; filter: VkFilter); VKAPI_CALL;
  PFN_vkCmdDrawIndexedIndirect = procedure(commandBuffer: VkCommandBuffer; buffer: VkBuffer; offset: VkDeviceSize; drawCount: Cardinal; stride: Cardinal); VKAPI_CALL;
  PFN_vkCmdSetDepthBias = procedure(commandBuffer: VkCommandBuffer; depthBiasConstantFactor: Single; depthBiasClamp: Single; depthBiasSlopeFactor: Single); VKAPI_CALL;
  PFN_vkCreatePipelineCache = function(device: VkDevice; const pCreateInfo: PVkPipelineCacheCreateInfo; const pAllocator: PVkAllocationCallbacks; pPipelineCache: PVkPipelineCache): VkResult; VKAPI_CALL;
  PFN_vkDestroyPipelineCache = procedure(device: VkDevice; pipelineCache: VkPipelineCache; const pAllocator: PVkAllocationCallbacks); VKAPI_CALL;
  PFN_vkGetPipelineCacheData = function(device: VkDevice; pipelineCache: VkPipelineCache; pDataSize: PPtrUInt; pData: Pointer): VkResult; VKAPI_CALL;
  PFN_vkResetCommandBuffer = function(commandBuffer: VkCommandBuffer; flags: VkFlags): VkResult; VKAPI_CALL;

var
  { Global Function Pointers }
  vkGetInstanceProcAddr: PFN_vkGetInstanceProcAddr;
  vkGetDeviceProcAddr: PFN_vkGetDeviceProcAddr;

  vkCreateInstance: PFN_vkCreateInstance;
  vkDestroyInstance: PFN_vkDestroyInstance;
  vkEnumeratePhysicalDevices: PFN_vkEnumeratePhysicalDevices;
  vkGetPhysicalDeviceFeatures: PFN_vkGetPhysicalDeviceFeatures;
  vkGetPhysicalDeviceProperties: PFN_vkGetPhysicalDeviceProperties;
  vkGetPhysicalDeviceQueueFamilyProperties: PFN_vkGetPhysicalDeviceQueueFamilyProperties;
  vkGetPhysicalDeviceMemoryProperties: PFN_vkGetPhysicalDeviceMemoryProperties;
  vkCreateDevice: PFN_vkCreateDevice;
  vkDestroyDevice: PFN_vkDestroyDevice;
  vkEnumerateInstanceExtensionProperties: PFN_vkEnumerateInstanceExtensionProperties;
  vkEnumerateDeviceExtensionProperties: PFN_vkEnumerateDeviceExtensionProperties;

  vkDestroySurfaceKHR: PFN_vkDestroySurfaceKHR;
  vkGetPhysicalDeviceSurfaceSupportKHR: PFN_vkGetPhysicalDeviceSurfaceSupportKHR;
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR: PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR;
  vkGetPhysicalDeviceSurfaceFormatsKHR: PFN_vkGetPhysicalDeviceSurfaceFormatsKHR;
  vkGetPhysicalDeviceSurfacePresentModesKHR: PFN_vkGetPhysicalDeviceSurfacePresentModesKHR;

  vkGetDeviceQueue: PFN_vkGetDeviceQueue;
  vkQueueSubmit: PFN_vkQueueSubmit;
  vkQueueWaitIdle: PFN_vkQueueWaitIdle;
  vkDeviceWaitIdle: PFN_vkDeviceWaitIdle;
  vkCreateFence: PFN_vkCreateFence;
  vkDestroyFence: PFN_vkDestroyFence;
  vkResetFences: PFN_vkResetFences;
  vkWaitForFences: PFN_vkWaitForFences;
  vkCreateSemaphore: PFN_vkCreateSemaphore;
  vkDestroySemaphore: PFN_vkDestroySemaphore;

  vkAllocateMemory: PFN_vkAllocateMemory;
  vkFreeMemory: PFN_vkFreeMemory;
  vkMapMemory: PFN_vkMapMemory;
  vkUnmapMemory: PFN_vkUnmapMemory;
  vkBindBufferMemory: PFN_vkBindBufferMemory;
  vkBindImageMemory: PFN_vkBindImageMemory;
  vkGetBufferMemoryRequirements: PFN_vkGetBufferMemoryRequirements;
  vkGetImageMemoryRequirements: PFN_vkGetImageMemoryRequirements;
  vkCreateBuffer: PFN_vkCreateBuffer;
  vkDestroyBuffer: PFN_vkDestroyBuffer;

  vkCreateImage: PFN_vkCreateImage;
  vkDestroyImage: PFN_vkDestroyImage;
  vkCreateImageView: PFN_vkCreateImageView;
  vkDestroyImageView: PFN_vkDestroyImageView;
  vkCreateSampler: PFN_vkCreateSampler;
  vkDestroySampler: PFN_vkDestroySampler;

  vkCreateShaderModule: PFN_vkCreateShaderModule;
  vkDestroyShaderModule: PFN_vkDestroyShaderModule;
  vkCreatePipelineLayout: PFN_vkCreatePipelineLayout;
  vkDestroyPipelineLayout: PFN_vkDestroyPipelineLayout;
  vkCreateGraphicsPipelines: PFN_vkCreateGraphicsPipelines;
  vkDestroyPipeline: PFN_vkDestroyPipeline;

  vkCreateDescriptorSetLayout: PFN_vkCreateDescriptorSetLayout;
  vkDestroyDescriptorSetLayout: PFN_vkDestroyDescriptorSetLayout;
  vkCreateDescriptorPool: PFN_vkCreateDescriptorPool;
  vkDestroyDescriptorPool: PFN_vkDestroyDescriptorPool;
  vkAllocateDescriptorSets: PFN_vkAllocateDescriptorSets;
  vkFreeDescriptorSets: PFN_vkFreeDescriptorSets;
  vkUpdateDescriptorSets: PFN_vkUpdateDescriptorSets;

  vkCreateCommandPool: PFN_vkCreateCommandPool;
  vkDestroyCommandPool: PFN_vkDestroyCommandPool;
  vkResetCommandPool: PFN_vkResetCommandPool;
  vkAllocateCommandBuffers: PFN_vkAllocateCommandBuffers;
  vkFreeCommandBuffers: PFN_vkFreeCommandBuffers;
  vkBeginCommandBuffer: PFN_vkBeginCommandBuffer;
  vkEndCommandBuffer: PFN_vkEndCommandBuffer;
  vkCmdBindPipeline: PFN_vkCmdBindPipeline;
  vkCmdSetViewport: PFN_vkCmdSetViewport;
  vkCmdSetScissor: PFN_vkCmdSetScissor;
  vkCmdBindDescriptorSets: PFN_vkCmdBindDescriptorSets;
  vkCmdBindIndexBuffer: PFN_vkCmdBindIndexBuffer;
  vkCmdBindVertexBuffers: PFN_vkCmdBindVertexBuffers;
  vkCmdDraw: PFN_vkCmdDraw;
  vkCmdDrawIndexed: PFN_vkCmdDrawIndexed;
  vkCmdPushConstants: PFN_vkCmdPushConstants;
  vkCmdPipelineBarrier: PFN_vkCmdPipelineBarrier;
  vkCmdCopyBuffer: PFN_vkCmdCopyBuffer;
  vkCmdCopyBufferToImage: PFN_vkCmdCopyBufferToImage;

  vkCmdBeginRendering: PFN_vkCmdBeginRendering;
  vkCmdEndRendering: PFN_vkCmdEndRendering;

  vkCreateSwapchainKHR: PFN_vkCreateSwapchainKHR;
  vkDestroySwapchainKHR: PFN_vkDestroySwapchainKHR;
  vkGetSwapchainImagesKHR: PFN_vkGetSwapchainImagesKHR;
  vkAcquireNextImageKHR: PFN_vkAcquireNextImageKHR;
  vkQueuePresentKHR: PFN_vkQueuePresentKHR;

  vkCmdBeginDebugUtilsLabelEXT: PFN_vkCmdBeginDebugUtilsLabelEXT;
  vkCmdEndDebugUtilsLabelEXT: PFN_vkCmdEndDebugUtilsLabelEXT;
  vkCmdBlitImage: PFN_vkCmdBlitImage;
  vkCmdDrawIndexedIndirect: PFN_vkCmdDrawIndexedIndirect;
  vkCmdSetDepthBias: PFN_vkCmdSetDepthBias;
  vkCreatePipelineCache: PFN_vkCreatePipelineCache;
  vkDestroyPipelineCache: PFN_vkDestroyPipelineCache;
  vkGetPipelineCacheData: PFN_vkGetPipelineCacheData;
  vkResetCommandBuffer: PFN_vkResetCommandBuffer;

{ Loads the Vulkan shared library dynamically. Returns True if successfully loaded. }
function LoadVulkan: Boolean;
{ Populates instance-level function pointers using vkGetInstanceProcAddr. }
procedure LoadVulkanInstance(const Instance: VkInstance);
{ Populates device-level function pointers using vkGetDeviceProcAddr. }
procedure LoadVulkanDevice(const Device: VkDevice);
{ Unloads the Vulkan library. }
procedure FreeVulkan;
{ Returns True if Vulkan library is currently loaded. }
function IsVulkanLoaded: Boolean;

implementation

var
  VulkanLibrary: TDynLib = nil;

function IsVulkanLoaded: Boolean;
begin
  Result := VulkanLibrary <> nil;
end;

function LoadVulkan: Boolean;
const
  {$if defined(MSWINDOWS)}
  VulkanLibNames: array[0..0] of String = ('vulkan-1.dll');
  {$elseif defined(DARWIN)}
  VulkanLibNames: array[0..2] of String = (
    'libMoltenVK.dylib',
    'libvulkan.1.dylib',
    'libvulkan.dylib'
  );
  {$else}
  VulkanLibNames: array[0..2] of String = (
    'libvulkan.so.1',
    'libvulkan.so',
    'libvulkan.so.0'
  );
  {$endif}
var
  I: Integer;
begin
  if VulkanLibrary <> nil then
    Exit(True);

  for I := Low(VulkanLibNames) to High(VulkanLibNames) do
  begin
    try
      VulkanLibrary := TDynLib.Load(VulkanLibNames[I], false);
      if VulkanLibrary <> nil then
        Break;
    except
      VulkanLibrary := nil;
    end;
  end;

  if VulkanLibrary = nil then
  begin
    WritelnLog('Vulkan', 'Could not locate Vulkan dynamic library');
    Exit(False);
  end;

  Pointer(vkGetInstanceProcAddr) := VulkanLibrary.Symbol('vkGetInstanceProcAddr');
  if not Assigned(vkGetInstanceProcAddr) then
  begin
    WritelnLog('Vulkan', 'vkGetInstanceProcAddr not exported by Vulkan library');
    FreeAndNil(VulkanLibrary);
    Exit(False);
  end;

  Pointer(vkCreateInstance) := vkGetInstanceProcAddr(nil, 'vkCreateInstance');
  Pointer(vkEnumerateInstanceExtensionProperties) := vkGetInstanceProcAddr(nil, 'vkEnumerateInstanceExtensionProperties');

  Result := True;
end;

procedure LoadVulkanInstance(const Instance: VkInstance);
begin
  if not Assigned(vkGetInstanceProcAddr) then Exit;

  Pointer(vkDestroyInstance) := vkGetInstanceProcAddr(Instance, 'vkDestroyInstance');
  Pointer(vkEnumeratePhysicalDevices) := vkGetInstanceProcAddr(Instance, 'vkEnumeratePhysicalDevices');
  Pointer(vkGetPhysicalDeviceFeatures) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceFeatures');
  Pointer(vkGetPhysicalDeviceProperties) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceProperties');
  Pointer(vkGetPhysicalDeviceQueueFamilyProperties) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceQueueFamilyProperties');
  Pointer(vkGetPhysicalDeviceMemoryProperties) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceMemoryProperties');
  Pointer(vkCreateDevice) := vkGetInstanceProcAddr(Instance, 'vkCreateDevice');
  Pointer(vkDestroyDevice) := vkGetInstanceProcAddr(Instance, 'vkDestroyDevice');
  Pointer(vkEnumerateDeviceExtensionProperties) := vkGetInstanceProcAddr(Instance, 'vkEnumerateDeviceExtensionProperties');

  Pointer(vkDestroySurfaceKHR) := vkGetInstanceProcAddr(Instance, 'vkDestroySurfaceKHR');
  Pointer(vkGetPhysicalDeviceSurfaceSupportKHR) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceSurfaceSupportKHR');
  Pointer(vkGetPhysicalDeviceSurfaceCapabilitiesKHR) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR');
  Pointer(vkGetPhysicalDeviceSurfaceFormatsKHR) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceSurfaceFormatsKHR');
  Pointer(vkGetPhysicalDeviceSurfacePresentModesKHR) := vkGetInstanceProcAddr(Instance, 'vkGetPhysicalDeviceSurfacePresentModesKHR');
end;

procedure LoadVulkanDevice(const Device: VkDevice);
begin
  if not Assigned(vkGetInstanceProcAddr) then Exit;

  Pointer(vkGetDeviceProcAddr) := vkGetInstanceProcAddr(nil, 'vkGetDeviceProcAddr');

  Pointer(vkGetDeviceQueue) := vkGetInstanceProcAddr(nil, 'vkGetDeviceQueue');
  Pointer(vkQueueSubmit) := vkGetInstanceProcAddr(nil, 'vkQueueSubmit');
  Pointer(vkQueueWaitIdle) := vkGetInstanceProcAddr(nil, 'vkQueueWaitIdle');
  Pointer(vkDeviceWaitIdle) := vkGetInstanceProcAddr(nil, 'vkDeviceWaitIdle');
  Pointer(vkCreateFence) := vkGetInstanceProcAddr(nil, 'vkCreateFence');
  Pointer(vkDestroyFence) := vkGetInstanceProcAddr(nil, 'vkDestroyFence');
  Pointer(vkResetFences) := vkGetInstanceProcAddr(nil, 'vkResetFences');
  Pointer(vkWaitForFences) := vkGetInstanceProcAddr(nil, 'vkWaitForFences');
  Pointer(vkCreateSemaphore) := vkGetInstanceProcAddr(nil, 'vkCreateSemaphore');
  Pointer(vkDestroySemaphore) := vkGetInstanceProcAddr(nil, 'vkDestroySemaphore');

  Pointer(vkAllocateMemory) := vkGetInstanceProcAddr(nil, 'vkAllocateMemory');
  Pointer(vkFreeMemory) := vkGetInstanceProcAddr(nil, 'vkFreeMemory');
  Pointer(vkMapMemory) := vkGetInstanceProcAddr(nil, 'vkMapMemory');
  Pointer(vkUnmapMemory) := vkGetInstanceProcAddr(nil, 'vkUnmapMemory');
  Pointer(vkBindBufferMemory) := vkGetInstanceProcAddr(nil, 'vkBindBufferMemory');
  Pointer(vkBindImageMemory) := vkGetInstanceProcAddr(nil, 'vkBindImageMemory');
  Pointer(vkGetBufferMemoryRequirements) := vkGetInstanceProcAddr(nil, 'vkGetBufferMemoryRequirements');
  Pointer(vkGetImageMemoryRequirements) := vkGetInstanceProcAddr(nil, 'vkGetImageMemoryRequirements');
  Pointer(vkCreateBuffer) := vkGetInstanceProcAddr(nil, 'vkCreateBuffer');
  Pointer(vkDestroyBuffer) := vkGetInstanceProcAddr(nil, 'vkDestroyBuffer');

  Pointer(vkCreateImage) := vkGetInstanceProcAddr(nil, 'vkCreateImage');
  Pointer(vkDestroyImage) := vkGetInstanceProcAddr(nil, 'vkDestroyImage');
  Pointer(vkCreateImageView) := vkGetInstanceProcAddr(nil, 'vkCreateImageView');
  Pointer(vkDestroyImageView) := vkGetInstanceProcAddr(nil, 'vkDestroyImageView');
  Pointer(vkCreateSampler) := vkGetInstanceProcAddr(nil, 'vkCreateSampler');
  Pointer(vkDestroySampler) := vkGetInstanceProcAddr(nil, 'vkDestroySampler');

  Pointer(vkCreateShaderModule) := vkGetInstanceProcAddr(nil, 'vkCreateShaderModule');
  Pointer(vkDestroyShaderModule) := vkGetInstanceProcAddr(nil, 'vkDestroyShaderModule');
  Pointer(vkCreatePipelineLayout) := vkGetInstanceProcAddr(nil, 'vkCreatePipelineLayout');
  Pointer(vkDestroyPipelineLayout) := vkGetInstanceProcAddr(nil, 'vkDestroyPipelineLayout');
  Pointer(vkCreateGraphicsPipelines) := vkGetInstanceProcAddr(nil, 'vkCreateGraphicsPipelines');
  Pointer(vkDestroyPipeline) := vkGetInstanceProcAddr(nil, 'vkDestroyPipeline');

  Pointer(vkCreateDescriptorSetLayout) := vkGetInstanceProcAddr(nil, 'vkCreateDescriptorSetLayout');
  Pointer(vkDestroyDescriptorSetLayout) := vkGetInstanceProcAddr(nil, 'vkDestroyDescriptorSetLayout');
  Pointer(vkCreateDescriptorPool) := vkGetInstanceProcAddr(nil, 'vkCreateDescriptorPool');
  Pointer(vkDestroyDescriptorPool) := vkGetInstanceProcAddr(nil, 'vkDestroyDescriptorPool');
  Pointer(vkAllocateDescriptorSets) := vkGetInstanceProcAddr(nil, 'vkAllocateDescriptorSets');
  Pointer(vkFreeDescriptorSets) := vkGetInstanceProcAddr(nil, 'vkFreeDescriptorSets');
  Pointer(vkUpdateDescriptorSets) := vkGetInstanceProcAddr(nil, 'vkUpdateDescriptorSets');

  Pointer(vkCreateCommandPool) := vkGetInstanceProcAddr(nil, 'vkCreateCommandPool');
  Pointer(vkDestroyCommandPool) := vkGetInstanceProcAddr(nil, 'vkDestroyCommandPool');
  Pointer(vkResetCommandPool) := vkGetInstanceProcAddr(nil, 'vkResetCommandPool');
  Pointer(vkAllocateCommandBuffers) := vkGetInstanceProcAddr(nil, 'vkAllocateCommandBuffers');
  Pointer(vkFreeCommandBuffers) := vkGetInstanceProcAddr(nil, 'vkFreeCommandBuffers');
  Pointer(vkBeginCommandBuffer) := vkGetInstanceProcAddr(nil, 'vkBeginCommandBuffer');
  Pointer(vkEndCommandBuffer) := vkGetInstanceProcAddr(nil, 'vkEndCommandBuffer');
  Pointer(vkCmdBindPipeline) := vkGetInstanceProcAddr(nil, 'vkCmdBindPipeline');
  Pointer(vkCmdSetViewport) := vkGetInstanceProcAddr(nil, 'vkCmdSetViewport');
  Pointer(vkCmdSetScissor) := vkGetInstanceProcAddr(nil, 'vkCmdSetScissor');
  Pointer(vkCmdBindDescriptorSets) := vkGetInstanceProcAddr(nil, 'vkCmdBindDescriptorSets');
  Pointer(vkCmdBindIndexBuffer) := vkGetInstanceProcAddr(nil, 'vkCmdBindIndexBuffer');
  Pointer(vkCmdBindVertexBuffers) := vkGetInstanceProcAddr(nil, 'vkCmdBindVertexBuffers');
  Pointer(vkCmdDraw) := vkGetInstanceProcAddr(nil, 'vkCmdDraw');
  Pointer(vkCmdDrawIndexed) := vkGetInstanceProcAddr(nil, 'vkCmdDrawIndexed');
  Pointer(vkCmdPushConstants) := vkGetInstanceProcAddr(nil, 'vkCmdPushConstants');
  Pointer(vkCmdPipelineBarrier) := vkGetInstanceProcAddr(nil, 'vkCmdPipelineBarrier');
  Pointer(vkCmdCopyBuffer) := vkGetInstanceProcAddr(nil, 'vkCmdCopyBuffer');
  Pointer(vkCmdCopyBufferToImage) := vkGetInstanceProcAddr(nil, 'vkCmdCopyBufferToImage');

  { Dynamic Rendering }
  Pointer(vkCmdBeginRendering) := vkGetInstanceProcAddr(nil, 'vkCmdBeginRendering');
  if not Assigned(vkCmdBeginRendering) then
    Pointer(vkCmdBeginRendering) := vkGetInstanceProcAddr(nil, 'vkCmdBeginRenderingKHR');

  Pointer(vkCmdEndRendering) := vkGetInstanceProcAddr(nil, 'vkCmdEndRendering');
  if not Assigned(vkCmdEndRendering) then
    Pointer(vkCmdEndRendering) := vkGetInstanceProcAddr(nil, 'vkCmdEndRenderingKHR');

  Pointer(vkCreateSwapchainKHR) := vkGetInstanceProcAddr(nil, 'vkCreateSwapchainKHR');
  Pointer(vkDestroySwapchainKHR) := vkGetInstanceProcAddr(nil, 'vkDestroySwapchainKHR');
  Pointer(vkGetSwapchainImagesKHR) := vkGetInstanceProcAddr(nil, 'vkGetSwapchainImagesKHR');
  Pointer(vkAcquireNextImageKHR) := vkGetInstanceProcAddr(nil, 'vkAcquireNextImageKHR');
  Pointer(vkQueuePresentKHR) := vkGetInstanceProcAddr(nil, 'vkQueuePresentKHR');
  Pointer(vkCmdBeginDebugUtilsLabelEXT) := vkGetInstanceProcAddr(nil, 'vkCmdBeginDebugUtilsLabelEXT');
  Pointer(vkCmdEndDebugUtilsLabelEXT) := vkGetInstanceProcAddr(nil, 'vkCmdEndDebugUtilsLabelEXT');
  Pointer(vkCmdBlitImage) := vkGetInstanceProcAddr(nil, 'vkCmdBlitImage');
  Pointer(vkCmdDrawIndexedIndirect) := vkGetInstanceProcAddr(nil, 'vkCmdDrawIndexedIndirect');
  Pointer(vkCmdSetDepthBias) := vkGetInstanceProcAddr(nil, 'vkCmdSetDepthBias');
  Pointer(vkCreatePipelineCache) := vkGetInstanceProcAddr(nil, 'vkCreatePipelineCache');
  Pointer(vkDestroyPipelineCache) := vkGetInstanceProcAddr(nil, 'vkDestroyPipelineCache');
  Pointer(vkGetPipelineCacheData) := vkGetInstanceProcAddr(nil, 'vkGetPipelineCacheData');
  Pointer(vkResetCommandBuffer) := vkGetInstanceProcAddr(nil, 'vkResetCommandBuffer');

end;

procedure FreeVulkan;
begin
  FreeAndNil(VulkanLibrary);
  vkGetInstanceProcAddr := nil;
  vkGetDeviceProcAddr := nil;
  vkCreateInstance := nil;
  vkDestroyInstance := nil;
  vkCreateDevice := nil;
  vkDestroyDevice := nil;
end;

finalization
  FreeVulkan;
end.
