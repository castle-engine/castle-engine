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

{ Abstract Render Hardware Interface (RHI) for Castle Game Engine.
  Provides an explicit, low-overhead rendering abstraction capable of driving
  both modern explicit APIs (Vulkan) and state-machine APIs (OpenGL / OpenGLES). }
unit CastleInternalRHI;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Math, Generics.Collections,
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleRenderOptions;

type
  { Forward declarations }
  TRHIDevice = class;
  TRHIBuffer = class;
  TRHITexture = class;
  TRHISampler = class;
  TRHIShaderModule = class;
  TRHIDescriptorSetLayout = class;
  TRHIDescriptorSet = class;
  TRHIPipeline = class;
  TRHIComputePipeline = class;
  TRHIQueryPool = class;
  TRHICommandList = class;
  TRHIRenderGraphPass = class;
  TRHIRenderGraph = class;

  { Exceptions }
  ERHIError = class(Exception);
  ERHIOutOfMemory = class(ERHIError);
  ERHIShaderCompilationError = class(ERHIError);

  { -------------------------------------------------------------------------- }
  { Fundamental Enums & Types                                                  }
  { -------------------------------------------------------------------------- }

  TRHIBackendType = (
    rbOpenGL,
    rbOpenGLES,
    rbVulkan
  );

  { Memory residency and CPU/GPU access patterns }
  TRHIMemoryUsage = (
    { GPU-only memory (VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT).
      Fastest for rendering; requires a staging buffer upload. }
    muGpuOnly,
    { CPU-writable, GPU-readable (HOST_VISIBLE | HOST_COHERENT).
      Ideal for per-frame dynamic UBOs, streaming vertices, and UI batches. }
    muCpuToGpu,
    { GPU-writable, CPU-readable. Used for screenshots / framebuffer readbacks. }
    muGpuToCpu
  );

  TRHIBufferUsage = (
    buVertex,
    buIndex,
    buUniform,
    buStorage,
    buIndirect,
    buTransferSrc,
    buTransferDst,
    buShaderDeviceAddress,
    buAccelerationStructureBuildInput,
    buAccelerationStructureStorage,
    buShaderBindingTable
  );
  TRHIBufferUsageFlags = set of TRHIBufferUsage;

  TRHIIndexType = (
    itUInt16,
    itUInt32
  );

  TRHIPixelFormat = (
    pfUndefined,
    { 8-bit normalized }
    pfR8_UNorm,
    pfRG8_UNorm,
    pfRGBA8_UNorm,
    pfRGBA8_sRGB,
    pfBGRA8_UNorm,
    pfBGRA8_sRGB,
    { 16-bit / 32-bit float (HDR & Data) }
    pfR16_Float,
    pfRG16_Float,
    pfRGBA16_Float,
    pfR32_Float,
    pfRGBA32_Float,
    { Depth / Stencil }
    pfD16_UNorm,
    pfD24_UNorm_S8_UInt,
    pfD32_Float,
    pfD32_Float_S8_UInt,
    { Compressed formats (BC / ASTC / ETC2) }
    pfBC1_RGBA_UNorm,
    pfBC1_RGBA_sRGB,
    pfBC3_RGBA_UNorm,
    pfBC3_RGBA_sRGB,
    pfBC5_RG_UNorm,
    pfBC7_RGBA_UNorm,
    pfBC7_RGBA_sRGB,
    pfASTC_4x4_UNorm,
    pfASTC_4x4_sRGB,
    pfETC2_RGB8_UNorm,
    pfETC2_RGBA8_UNorm
  );

  TRHITextureDimension = (
    td2D,
    td2DArray,
    td3D,
    tdCube,
    tdCubeArray
  );

  TRHITextureUsage = (
    tuSampled,            // Sampled in shaders
    tuColorAttachment,    // Render target color attachment
    tuDepthStencil,       // Render target depth/stencil attachment
    tuStorage,            // Compute shader image load/store
    tuTransferSrc,        // Blit / readback source
    tuTransferDst         // Upload / blit destination
  );
  TRHITextureUsageFlags = set of TRHITextureUsage;

  { Explicit resource states for Vulkan pipeline barriers (no-op in OpenGL) }
  TRHIResourceState = (
    rsUndefined,
    rsTransferDst,
    rsTransferSrc,
    rsShaderRead,
    rsUnorderedAccess,
    rsIndirectRead,
    rsColorAttachment,
    rsDepthStencilWrite,
    rsDepthStencilRead,
    rsPresent
  );

  TRHIShaderStage = (
    ssVertex,
    ssFragment,
    ssGeometry,
    ssCompute,
    ssTask,
    ssMesh,
    ssRayGen,
    ssAnyHit,
    ssClosestHit,
    ssMiss,
    ssIntersection,
    ssCallable
  );
  TRHIShaderStageFlags = set of TRHIShaderStage;

  TRHIPrimitiveTopology = (
    ptTriangleList,
    ptTriangleStrip,
    ptLineList,
    ptLineStrip,
    ptPointList
  );

  TRHIPolygonMode = (
    pmFill,
    pmLine,
    pmPoint
  );

  TRHICullMode = (
    cmNone,
    cmBack,
    cmFront
  );

  TRHIFrontFace = (
    ffCounterClockwise,
    ffClockwise
  );

  TRHICompareOp = (
    coNever,
    coLess,
    coEqual,
    coLessOrEqual,
    coGreater,
    coNotEqual,
    coGreaterOrEqual,
    coAlways
  );

  { Stencil buffer operation (VK_STENCIL_OP_* / GL_KEEP..GL_DECR_WRAP) }
  TRHIStencilOp = (
    soKeep,
    soZero,
    soReplace,
    soIncrementAndClamp,
    soDecrementAndClamp,
    soInvert,
    soIncrementAndWrap,
    soDecrementAndWrap
  );

  TRHIBlendFactor = (
    bfZero,
    bfOne,
    bfSrcColor,
    bfOneMinusSrcColor,
    bfDstColor,
    bfOneMinusDstColor,
    bfSrcAlpha,
    bfOneMinusSrcAlpha,
    bfDstAlpha,
    bfOneMinusDstAlpha,
    bfConstantColor,
    bfOneMinusConstantColor,
    bfConstantAlpha,
    bfOneMinusConstantAlpha,
    bfSrcAlphaSaturate
  );

  TRHIBlendOp = (
    boAdd,
    boSubtract,
    boReverseSubtract,
    boMin,
    boMax
  );

  TRHISamplerFilter = (
    sfNearest,
    sfLinear
  );

  TRHISamplerAddressMode = (
    amRepeat,
    amMirroredRepeat,
    amClampToEdge,
    amClampToBorder
  );

  TRHIAttachmentLoadOp = (
    loLoad,
    loClear,
    loDontCare
  );

  TRHIAttachmentStoreOp = (
    soStore,
    soDontCare
  );

  { Multisample attachment resolve mode (VK_RESOLVE_MODE_*_BIT) }
  TRHIResolveMode = (
    rmNone,
    rmAverage,
    rmSampleZero,
    rmMin,
    rmMax
  );

  { Hardware query pool type (VK_QUERY_TYPE_OCCLUSION / VK_QUERY_TYPE_TIMESTAMP) }
  TRHIQueryType = (
    qtOcclusion,
    qtTimestamp
  );

  TRHIVertexAttributeFormat = (
    vafFloat,
    vafVector2,
    vafVector3,
    vafVector4,
    vafByte4Normalized,
    vafUByte4Normalized,
    vafShort2,
    vafShort4,
    vafUInt4
  );

  TRHICommandListLevel = (
    cllPrimary,
    cllSecondary
  );

  { Indirect draw command matching standard GPU layout (VkDrawIndirectCommand, OpenGL DrawArraysIndirect). }
  TRHIDrawIndirectCommand = record
    VertexCount: Cardinal;
    InstanceCount: Cardinal;
    FirstVertex: Cardinal;
    FirstInstance: Cardinal;
  end;
  PRHIDrawIndirectCommand = ^TRHIDrawIndirectCommand;

  { Indirect indexed draw command matching standard GPU layout (VkDrawIndexedIndirectCommand, OpenGL DrawElementsIndirect). }
  TRHIDrawIndexedIndirectCommand = record
    IndexCount: Cardinal;
    InstanceCount: Cardinal;
    FirstIndex: Cardinal;
    VertexOffset: Integer;
    FirstInstance: Cardinal;
  end;
  PRHIDrawIndexedIndirectCommand = ^TRHIDrawIndexedIndirectCommand;

  { Indirect compute dispatch command matching standard GPU layout (VkDispatchIndirectCommand). }
  TRHIDispatchIndirectCommand = record
    GroupCountX: Cardinal;
    GroupCountY: Cardinal;
    GroupCountZ: Cardinal;
  end;
  PRHIDispatchIndirectCommand = ^TRHIDispatchIndirectCommand;

  TRHIBufferDesc = record
    Size: PtrUInt;
    Usage: TRHIBufferUsageFlags;
    MemoryUsage: TRHIMemoryUsage;
    DebugName: String;
    class function Create(const ASize: PtrUInt;
      const AUsage: TRHIBufferUsageFlags;
      const AMemoryUsage: TRHIMemoryUsage = muGpuOnly;
      const ADebugName: String = ''): TRHIBufferDesc; static;
  end;

  TRHITextureDesc = record
    Dimension: TRHITextureDimension;
    Width: Cardinal;
    Height: Cardinal;
    DepthOrArrayLayers: Cardinal;
    MipLevels: Cardinal;
    SampleCount: Cardinal;
    Format: TRHIPixelFormat;
    Usage: TRHITextureUsageFlags;
    DebugName: String;
    class function Create2D(const AWidth, AHeight: Cardinal;
      const AFormat: TRHIPixelFormat;
      const AUsage: TRHITextureUsageFlags = [tuSampled, tuTransferDst];
      const AMipLevels: Cardinal = 1;
      const ASampleCount: Cardinal = 1): TRHITextureDesc; static;
    class function Create3D(const AWidth, AHeight, ADepth: Cardinal;
      const AFormat: TRHIPixelFormat;
      const AUsage: TRHITextureUsageFlags = [tuSampled, tuTransferDst];
      const AMipLevels: Cardinal = 1): TRHITextureDesc; static;
    class function CreateCube(const ASize: Cardinal;
      const AFormat: TRHIPixelFormat;
      const AUsage: TRHITextureUsageFlags = [tuSampled, tuTransferDst];
      const AMipLevels: Cardinal = 1): TRHITextureDesc; static;
  end;

  TRHISamplerDesc = record
    MinFilter: TRHISamplerFilter;
    MagFilter: TRHISamplerFilter;
    MipFilter: TRHISamplerFilter;
    AddressModeU: TRHISamplerAddressMode;
    AddressModeV: TRHISamplerAddressMode;
    AddressModeW: TRHISamplerAddressMode;
    MaxAnisotropy: Single;
    CompareEnable: Boolean;
    CompareOp: TRHICompareOp;
    MinLod: Single;
    MaxLod: Single;
  end;

  { Vertex Input Layout }

  TRHIVertexBindingDesc = record
    Binding: Cardinal;
    Stride: Cardinal;
    PerInstance: Boolean;
  end;

  TRHIVertexAttributeDesc = record
    Location: Cardinal;
    Binding: Cardinal;
    Format: TRHIVertexAttributeFormat;
    Offset: Cardinal;
    { Attribute name for OpenGL 2/3 fallback binding if layout(location) is absent }
    AttributeName: String;
  end;

  TRHIVertexInputDesc = record
    Bindings: array of TRHIVertexBindingDesc;
    Attributes: array of TRHIVertexAttributeDesc;
  end;

  { Descriptor Set Layout }

  TRHIDescriptorType = (
    dtUniformBuffer,
    dtUniformBufferDynamic,
    dtStorageBuffer,
    dtCombinedImageSampler,
    dtSampledImage,
    dtSampler,
    dtStorageImage,
    dtAccelerationStructure
  );

  { Optional descriptor binding flags for Vulkan 1.2 / VK_EXT_descriptor_indexing bindless arrays }
  TRHIDescriptorBindingFlag = (
    dbfPartiallyBound,
    dbfUpdateUnusedWhilePending,
    dbfUpdateAfterBind,
    dbfVariableDescriptorCount
  );
  TRHIDescriptorBindingFlags = set of TRHIDescriptorBindingFlag;

  TRHIDescriptorBindingDesc = record
    Binding: Cardinal;
    DescriptorType: TRHIDescriptorType;
    Count: Cardinal;
    StageFlags: TRHIShaderStageFlags;
    BindingFlags: TRHIDescriptorBindingFlags;
  end;

  { Graphics Pipeline State Object (PSO) Description }

  TRHIBlendStateDesc = record
    BlendEnable: Boolean;
    SrcColorBlendFactor: TRHIBlendFactor;
    DstColorBlendFactor: TRHIBlendFactor;
    ColorBlendOp: TRHIBlendOp;
    SrcAlphaBlendFactor: TRHIBlendFactor;
    DstAlphaBlendFactor: TRHIBlendFactor;
    AlphaBlendOp: TRHIBlendOp;
    ColorWriteMask: Byte; // Bitmask: R=1, G=2, B=4, A=8 ($0F = all)
    DisableColorWrites: Boolean; // When True, forces colorWriteMask = 0 (e.g. stencil-only shadow volume pass)
    class function DefaultOpaque: TRHIBlendStateDesc; static;
    class function DefaultAlphaBlend: TRHIBlendStateDesc; static;
    class function DefaultColorMaskNone: TRHIBlendStateDesc; static;
  end;

  TRHIRasterizerStateDesc = record
    PolygonMode: TRHIPolygonMode;
    CullMode: TRHICullMode;
    FrontFace: TRHIFrontFace;
    DepthBiasEnable: Boolean;
    DepthClampEnable: Boolean;
    class function DefaultCullBack: TRHIRasterizerStateDesc; static;
  end;

  { Per-face stencil operation and comparison state (VkStencilOpState). }
  TRHIStencilOpStateDesc = record
    FailOp: TRHIStencilOp;
    PassOp: TRHIStencilOp;
    DepthFailOp: TRHIStencilOp;
    CompareOp: TRHICompareOp;
    CompareMask: Cardinal;
    WriteMask: Cardinal;
    Reference: Cardinal;
    class function DefaultKeep: TRHIStencilOpStateDesc; static;
  end;

  TRHIDepthStencilStateDesc = record
    DepthTestEnable: Boolean;
    DepthWriteEnable: Boolean;
    DepthCompareOp: TRHICompareOp;
    StencilTestEnable: Boolean;
    FrontStencil: TRHIStencilOpStateDesc;
    BackStencil: TRHIStencilOpStateDesc;
    class function DefaultDepthTestWrite: TRHIDepthStencilStateDesc; static;
    class function DefaultDepthReadOnly: TRHIDepthStencilStateDesc; static;
    class function DefaultDisabled: TRHIDepthStencilStateDesc; static;
    class function DefaultShadowMap: TRHIDepthStencilStateDesc; static;
    { Two-sided stencil shadow volume Z-Pass state:
      Front faces increment (wrap) on depth pass; back faces decrement (wrap) on depth pass. }
    class function DefaultStencilShadowVolumeZPass: TRHIDepthStencilStateDesc; static;
    { Two-sided stencil shadow volume Z-Fail (Carmack's Reverse) state:
      Front faces decrement (wrap) on depth fail; back faces increment (wrap) on depth fail. }
    class function DefaultStencilShadowVolumeZFail: TRHIDepthStencilStateDesc; static;
    { Stencil-masked lit scene pass state (renders only where stencil == AStencilReference). }
    class function DefaultStencilLitPass(const AStencilReference: Cardinal = 0;
      const ACompareMask: Cardinal = $FF): TRHIDepthStencilStateDesc; static;
  end;

  TRHIGraphicsPipelineDesc = record
    VertexShader: TRHIShaderModule;
    FragmentShader: TRHIShaderModule;
    GeometryShader: TRHIShaderModule; // Optional (nil if unused)
    TaskShader: TRHIShaderModule;     // Optional (VK_EXT_mesh_shader task stage)
    MeshShader: TRHIShaderModule;     // Optional (VK_EXT_mesh_shader mesh stage)
    VertexInput: TRHIVertexInputDesc;
    Topology: TRHIPrimitiveTopology;
    Rasterizer: TRHIRasterizerStateDesc;
    DepthStencil: TRHIDepthStencilStateDesc;
    BlendState: TRHIBlendStateDesc;
    { Descriptor Set Layouts (Set 0 = Global/Pass, Set 1 = Material, Set 2 = Object) }
    DescriptorSetLayouts: array of TRHIDescriptorSetLayout;
    PushConstantSize: Cardinal;
    PushConstantStages: TRHIShaderStageFlags;
    { Attachment formats for Vulkan 1.3 Dynamic Rendering }
    ColorAttachmentFormats: array of TRHIPixelFormat;
    DepthAttachmentFormat: TRHIPixelFormat;
    SampleCount: Cardinal;
    DebugName: String;
  end;

  { Compute Pipeline State Object (PSO) Description }
  TRHIComputePipelineDesc = record
    ComputeShader: TRHIShaderModule;
    DescriptorSetLayouts: array of TRHIDescriptorSetLayout;
    PushConstantSize: Cardinal;
    PushConstantStages: TRHIShaderStageFlags;
    DebugName: String;
  end;

  { Hardware Query Pool Description (Timestamp / Occlusion Queries) }
  TRHIQueryPoolDesc = record
    QueryType: TRHIQueryType;
    QueryCount: Cardinal;
    DebugName: String;
    class function Create(const AQueryType: TRHIQueryType;
      const AQueryCount: Cardinal;
      const ADebugName: String = ''): TRHIQueryPoolDesc; static;
  end;

  { Dynamic Rendering Pass Attachments (Vulkan VK_KHR_dynamic_rendering) }

  TRHIRenderingAttachment = record
    Texture: TRHITexture;
    MipLevel: Cardinal;
    ArrayLayer: Cardinal;
    LoadOp: TRHIAttachmentLoadOp;
    StoreOp: TRHIAttachmentStoreOp;
    ClearColor: TCastleColor;
    ClearDepth: Single;
    ClearStencil: Byte;
    { Single-pass MSAA resolve target (1x SampleCount texture) and mode.
      When ResolveTexture <> nil and Texture.Desc.SampleCount > 1, the GPU resolves
      the multisampled attachment into ResolveTexture at the end of the pass. }
    ResolveTexture: TRHITexture;
    ResolveMipLevel: Cardinal;
    ResolveArrayLayer: Cardinal;
    ResolveMode: TRHIResolveMode;
  end;

  TRHIRenderingInfo = record
    RenderArea: TRectangle;
    ColorAttachments: array of TRHIRenderingAttachment;
    HasDepthAttachment: Boolean;
    DepthAttachment: TRHIRenderingAttachment;
    ContentsSecondaryCommandBuffers: Boolean;
    class function CreateColorDepth(const ARenderArea: TRectangle;
      const AColorTexture: TRHITexture; const AColorLoadOp: TRHIAttachmentLoadOp;
      const AClearColor: TCastleColor; const ADepthTexture: TRHITexture;
      const ADepthLoadOp: TRHIAttachmentLoadOp; const AClearDepth: Single = 1.0): TRHIRenderingInfo; static;
    class function CreateColorDepthMSAA(const ARenderArea: TRectangle;
      const AMSAAColorTexture, AResolveColorTexture: TRHITexture;
      const AColorLoadOp: TRHIAttachmentLoadOp; const AClearColor: TCastleColor;
      const AMSAADepthTexture: TRHITexture; const ADepthLoadOp: TRHIAttachmentLoadOp;
      const AClearDepth: Single = 1.0;
      const AResolveDepthTexture: TRHITexture = nil): TRHIRenderingInfo; static;
    class function CreateDepthOnly(const ARenderArea: TRectangle;
      const ADepthTexture: TRHITexture; const ADepthLoadOp: TRHIAttachmentLoadOp;
      const ADepthStoreOp: TRHIAttachmentStoreOp = soStore;
      const AClearDepth: Single = 1.0): TRHIRenderingInfo; static;
  end;

  { Device Capabilities & Limits }

  TRHIDeviceCapabilities = record
    Backend: TRHIBackendType;
    DeviceName: String;
    VendorId: Cardinal;
    MaxTextureSize: Cardinal;
    MaxAnisotropy: Single;
    MaxUniformBufferRange: Cardinal;
    MinUniformBufferOffsetAlignment: Cardinal;
    MaxPushConstantsSize: Cardinal;
    MaxFramebufferSampleCount: Cardinal;
    SupportsBCCompression: Boolean;
    SupportsASTCCompression: Boolean;
    SupportsETC2Compression: Boolean;
    SupportsComputeShaders: Boolean;
    SupportsMultiDrawIndirect: Boolean;
    SupportsDrawIndirectCount: Boolean;
    SupportsBindlessTextures: Boolean;
    MaxBindlessTextures: Cardinal;
    SupportsExtendedDynamicState: Boolean;
    SupportsSynchronization2: Boolean;
    SupportsTimelineSemaphores: Boolean;
    SupportsBufferDeviceAddress: Boolean;
    SupportsAccelerationStructure: Boolean;
    SupportsRayQuery: Boolean;
    SupportsRayTracingPipeline: Boolean;
    SupportsMeshShaders: Boolean;
    TimestampPeriodNanoseconds: Single;
  end;

  { -------------------------------------------------------------------------- }
  { Resource Classes                                                           }
  { -------------------------------------------------------------------------- }

  { Base class for all GPU resources owned by a TRHIDevice. }
  TRHIResource = class abstract
  private
    FDevice: TRHIDevice;
    FDebugName: String;
    FUniqueId: QWord;
  protected
    { Releases backend GPU handles owned by this resource while FDevice is still live.
      Invoked either from Destroy or by TRHIDevice.ReleaseRemainingDeviceResources
      when a resource wrapper outlives its TRHIDevice. }
    procedure ReleaseDeviceResources; virtual;
  public
    constructor Create(const ADevice: TRHIDevice; const ADebugName: String = '');
    destructor Destroy; override;
    property Device: TRHIDevice read FDevice;
    property DebugName: String read FDebugName;
    property UniqueId: QWord read FUniqueId;
  end;

  { GPU Buffer (Vertex, Index, Uniform, Storage, Staging, AccelerationStructure, SBT). }
  TRHIBuffer = class abstract(TRHIResource)
  protected
    FDesc: TRHIBufferDesc;
    function GetDeviceAddress: UInt64; virtual;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIBufferDesc);
    { Maps CPU-visible memory (muCpuToGpu or muGpuToCpu). }
    function Map: Pointer; virtual; abstract;
    procedure Unmap; virtual; abstract;
    { Convenience helper to upload data. Uses staging buffer if muGpuOnly. }
    procedure UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt = 0); virtual; abstract;
    property Desc: TRHIBufferDesc read FDesc;
    property DeviceAddress: UInt64 read GetDeviceAddress;
  end;

  { GPU Texture (2D, 3D, Cube, Array, Render Target). }
  TRHITexture = class abstract(TRHIResource)
  protected
    FDesc: TRHITextureDesc;
    FCurrentState: TRHIResourceState;
    FIsMemoryAliased: Boolean;
    FAliasedCarrierTexture: TRHITexture;
    function GetEstimatedByteSize: UInt64; virtual;
    function GetRenderedLayerMask: Cardinal; virtual;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHITextureDesc);
    { Uploads a TEncodedImage (TCastleImage or TGPUCompressedImage) into a mip/layer. }
    procedure UploadImage(const AImage: TEncodedImage;
      const AMipLevel: Cardinal = 0; const AArrayLayer: Cardinal = 0); virtual; abstract;
    { Generates the full mipmap chain from mip level 0 down to Desc.MipLevels - 1. }
    procedure GenerateMipmaps; virtual;
    class function EstimateDescriptorByteSize(const ADesc: TRHITextureDesc): UInt64; static;
    property Desc: TRHITextureDesc read FDesc;
    property CurrentState: TRHIResourceState read FCurrentState write FCurrentState;
    property IsMemoryAliased: Boolean read FIsMemoryAliased;
    property AliasedCarrierTexture: TRHITexture read FAliasedCarrierTexture;
    property EstimatedByteSize: UInt64 read GetEstimatedByteSize;
    property RenderedLayerMask: Cardinal read GetRenderedLayerMask;
  end;

  { Immutable texture sampler state. }
  TRHISampler = class abstract(TRHIResource)
  protected
    FDesc: TRHISamplerDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHISamplerDesc);
    property Desc: TRHISamplerDesc read FDesc;
  end;

  { Compiled shader stage (SPIR-V module on Vulkan, GLuint shader on OpenGL). }
  TRHIShaderModule = class abstract(TRHIResource)
  protected
    FStage: TRHIShaderStage;
  public
    constructor Create(const ADevice: TRHIDevice; const AStage: TRHIShaderStage;
      const ADebugName: String = '');
    property Stage: TRHIShaderStage read FStage;
  end;

  { Layout schema for a Descriptor Set (Set 0, Set 1, etc.). }
  TRHIDescriptorSetLayout = class abstract(TRHIResource)
  end;

  { Bound collection of UBOs, SSBOs, and Combined Image Samplers. }
  TRHIDescriptorSet = class abstract(TRHIResource)
  public
    procedure BindUniformBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); virtual; abstract;
    procedure BindStorageBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); virtual; abstract;
    procedure BindTexture(const Binding: Cardinal; const Texture: TRHITexture;
      const Sampler: TRHISampler); virtual; abstract;
    { Binds a combined image sampler into a specific descriptor array element
      (e.g. for Vulkan 1.2 / VK_EXT_descriptor_indexing bindless texture arrays). }
    procedure BindTextureAtIndex(const Binding, ArrayElement: Cardinal;
      const Texture: TRHITexture; const Sampler: TRHISampler); virtual;
    { Binds a specific 2D subresource slice (MipLevel, ArrayLayer) of a 2D, array, or cubemap texture. }
    procedure BindTextureSubresource(const Binding: Cardinal;
      const Texture: TRHITexture; const MipLevel, ArrayLayer: Cardinal;
      const Sampler: TRHISampler = nil); virtual;
    { Commits pending binding updates to the underlying descriptor set. }
    procedure Update; virtual; abstract;
  end;

  { Baked Graphics Pipeline State Object (PSO). }
  TRHIPipeline = class abstract(TRHIResource)
  protected
    FDesc: TRHIGraphicsPipelineDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIGraphicsPipelineDesc);
    procedure UpdateDynamicStateDesc(const ADesc: TRHIGraphicsPipelineDesc);
    property Desc: TRHIGraphicsPipelineDesc read FDesc;
  end;

  { Baked Compute Pipeline State Object (PSO). }
  TRHIComputePipeline = class abstract(TRHIResource)
  protected
    FDesc: TRHIComputePipelineDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIComputePipelineDesc);
    property Desc: TRHIComputePipelineDesc read FDesc;
  end;

  { Hardware Query Pool (GPU Timestamps and Occlusion Queries). }
  TRHIQueryPool = class abstract(TRHIResource)
  protected
    FDesc: TRHIQueryPoolDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIQueryPoolDesc);
    { Reads back 64-bit query results for [FirstQuery .. FirstQuery + QueryCount - 1].
      Returns True if all requested queries have completed and Results is populated. }
    function GetResults(const FirstQuery, QueryCount: Cardinal;
      out Results: array of UInt64; const WaitForResults: Boolean = False): Boolean; virtual; abstract;
    property Desc: TRHIQueryPoolDesc read FDesc;
  end;

  { -------------------------------------------------------------------------- }
  { Command List (Command Buffer Recording)                                    }
  { -------------------------------------------------------------------------- }

  { Records rendering, compute, and transfer commands for execution on the GPU.
    In Vulkan, wraps VkCommandBuffer. In OpenGL, executes immediately or via a lightweight queue. }
  TRHICommandList = class abstract
  protected
    FLevel: TRHICommandListLevel;
    FIsRendering: Boolean;
    FActiveRenderingInfo: TRHIRenderingInfo;
    FActiveRenderingHasColor: Boolean;
    FActiveRenderingColorFormat: TRHIPixelFormat;
    FActiveRenderingHasDepth: Boolean;
    FActiveRenderingDepthFormat: TRHIPixelFormat;
    FActiveRenderingSampleCount: Cardinal;
    FCurrentPassHasCommands: Boolean;
  public
    constructor Create(const ALevel: TRHICommandListLevel = cllPrimary);
    property Level: TRHICommandListLevel read FLevel;
    property IsRendering: Boolean read FIsRendering;
    property CurrentPassHasCommands: Boolean read FCurrentPassHasCommands;
    property ActiveRenderingInfo: TRHIRenderingInfo read FActiveRenderingInfo;
    property ActiveRenderingHasColor: Boolean read FActiveRenderingHasColor;
    property ActiveRenderingColorFormat: TRHIPixelFormat read FActiveRenderingColorFormat;
    property ActiveRenderingHasDepth: Boolean read FActiveRenderingHasDepth;
    property ActiveRenderingDepthFormat: TRHIPixelFormat read FActiveRenderingDepthFormat;
    property ActiveRenderingSampleCount: Cardinal read FActiveRenderingSampleCount;

    procedure BeginRecording; virtual; abstract;
    procedure EndRecording; virtual; abstract;

    { Resource Barriers & Transitions }
    procedure TransitionTexture(const Texture: TRHITexture;
      const NewState: TRHIResourceState); virtual; abstract;
    { Emits an explicit physical memory aliasing barrier before activating NewTexture
      on shared transient memory previously written/read through OldTexture (Phase 111.1). }
    procedure EmitAliasingBarrier(const OldTexture, NewTexture: TRHITexture); virtual;
    procedure GenerateMipmaps(const Texture: TRHITexture); virtual; abstract;

    { Dynamic Rendering Pass Delimiters }
    procedure BeginRendering(const Info: TRHIRenderingInfo); virtual; abstract;
    procedure EndRendering; virtual; abstract;
    procedure ClearAttachments(const ClearColorBuf: Boolean; const ClearColor: TCastleColor;
      const ClearDepthBuf: Boolean = False; const ClearDepth: Single = 1.0;
      const ClearStencilBuf: Boolean = False; const ClearStencil: Byte = 0); virtual;

    { Dynamic Viewport & Scissor State }
    procedure SetViewport(const ViewportRect: TFloatRectangle;
      const MinDepth: Single = 0.0; const MaxDepth: Single = 1.0); virtual; abstract;
    procedure SetScissor(const ScissorRect: TRectangle); virtual; abstract;
    procedure SetDepthBias(const ConstantFactor, SlopeFactor: Single); virtual; abstract;
    procedure SetLineWidth(const LineWidth: Single); virtual;
    procedure SetBlendConstants(const ABlendConstants: TVector4); virtual;

    { Extended Dynamic State (VK_EXT_extended_dynamic_state / Vulkan 1.3 Core) }
    procedure SetCullMode(const CullMode: TRHICullMode); virtual;
    procedure SetFrontFace(const FrontFace: TRHIFrontFace); virtual;
    procedure SetDepthTestEnable(const Enable: Boolean); virtual;
    procedure SetDepthWriteEnable(const Enable: Boolean); virtual;
    procedure SetDepthCompareOp(const CompareOp: TRHICompareOp); virtual;
    procedure SetPrimitiveTopology(const Topology: TRHIPrimitiveTopology); virtual;

    { Pipeline & Resource Binding }
    procedure BindPipeline(const Pipeline: TRHIPipeline); virtual; abstract;
    procedure BindDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); virtual; abstract;
    procedure PushConstants(const StageFlags: TRHIShaderStageFlags;
      const Offset, Size: Cardinal; const Data); virtual; abstract;

    { Compute Pipeline & Dispatch (Phase 6.5) }
    procedure BindComputePipeline(const Pipeline: TRHIComputePipeline); virtual;
    procedure BindComputeDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); virtual;
    procedure Dispatch(const GroupCountX, GroupCountY, GroupCountZ: Cardinal); reintroduce; virtual;
    procedure DispatchIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt = 0); virtual;

    { Hardware Queries (Timestamps & Occlusion Queries - Phase 6.7) }
    procedure ResetQueryPool(const QueryPool: TRHIQueryPool;
      const FirstQuery, QueryCount: Cardinal); virtual;
    procedure BeginQuery(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal; const Precise: Boolean = False); virtual;
    procedure EndQuery(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal); virtual;
    procedure WriteTimestamp(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal; const Stage: TRHIShaderStage = ssFragment); virtual;

    { Geometry Buffers }
    procedure BindVertexBuffers(const FirstBinding: Cardinal;
      const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt); virtual; abstract;
    procedure BindIndexBuffer(const Buffer: TRHIBuffer;
      const Offset: PtrUInt; const IndexType: TRHIIndexType); virtual; abstract;

    { Draw Calls }
    procedure Draw(const VertexCount, InstanceCount, FirstVertex, FirstInstance: Cardinal); virtual; abstract;
    procedure DrawIndexed(const IndexCount, InstanceCount, FirstIndex: Cardinal;
      const VertexOffset: Integer; const FirstInstance: Cardinal); virtual; abstract;
    procedure DrawIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); virtual; abstract;
    procedure DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); virtual; abstract;
    { GPU-driven indirect draw with count read from a GPU buffer
      (Vulkan 1.2 Core / VK_KHR_draw_indirect_count). }
    procedure DrawIndirectCount(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
      const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal); virtual;
    procedure DrawIndexedIndirectCount(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
      const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal); virtual;
    { Mesh Shader Task/Mesh Workgroup Dispatch (VK_EXT_mesh_shader). }
    procedure DrawMeshTasks(const GroupCountX, GroupCountY, GroupCountZ: Cardinal); virtual;
    procedure DrawMeshTasksIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); virtual;

    { Secondary Command List Recording & Execution (Multi-threaded recording) }
    procedure BeginSecondaryRecording(const AInheritedInfo: TRHIRenderingInfo); overload; virtual;
    procedure BeginSecondaryRecording(const AInheritedInfo: TRHIRenderingInfo;
      const AViewportRect: TFloatRectangle; const AScissorRect: TRectangle;
      const AMinDepth: Single = 0.0; const AMaxDepth: Single = 1.0); overload; virtual;
    procedure ExecuteCommands(const SecondaryCommandLists: array of TRHICommandList); virtual; abstract;
    procedure ExecuteSecondaryCommandsInPass(
      const SecondaryCommandLists: array of TRHICommandList); virtual;

    { Debug Markers (RenderDoc / NSight / VK_EXT_debug_utils integration) }
    procedure PushDebugGroup(const LabelName: String; const Color: TCastleColor); virtual; abstract;
    procedure PopDebugGroup; virtual; abstract;
    procedure InsertDebugMarker(const LabelName: String; const Color: TCastleColor); virtual;
  end;

  { -------------------------------------------------------------------------- }
  { Render Device (Factory & Frame Lifecycle)                                  }
  { -------------------------------------------------------------------------- }

  { Main entry point for the RHI backend. Manages GPU device lifecycles,
    swapchain presentation, frames-in-flight synchronization, and resource creation. }
  TRHIDevice = class abstract
  private
    FResources: TFPList;
  protected
    FCapabilities: TRHIDeviceCapabilities;
    FFrameIndex: Cardinal;
    FTotalFramesStarted: Cardinal;
    { Releases backend GPU handles on any surviving TRHIResource wrappers still tracked
      in FResources and detaches their FDevice reference before native device teardown. }
    procedure ReleaseRemainingDeviceResources;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Frame Lifecycle & Swapchain }
    function BeginFrame: TRHICommandList; virtual; abstract;
    procedure EndFrameAndPresent; virtual; abstract;
    { Records and submits an offscreen command list without acquiring or presenting
      a window swapchain image (used e.g. by out-of-frame TCastleContainer.SaveScreen). }
    function BeginOffscreenFrame: TRHICommandList; virtual;
    procedure EndOffscreenFrame; virtual;
    procedure ResizeSwapchain(const NewWidth, NewHeight: Cardinal); virtual; abstract;
    procedure WaitIdle; virtual; abstract;

    { Secondary Command List Allocation (for multi-threaded command recording) }
    function CreateSecondaryCommandList: TRHICommandList; virtual; abstract;

    { Returns the backbuffer color and depth targets for the current frame in flight }
    function GetSwapchainColorTexture: TRHITexture; virtual; abstract;
    function GetSwapchainDepthTexture: TRHITexture; virtual; abstract;

    { Resource Creation Factory Methods }
    function CreateBuffer(const ADesc: TRHIBufferDesc;
      const InitialData: Pointer = nil): TRHIBuffer; virtual; abstract;
    function CreateTexture(const ADesc: TRHITextureDesc): TRHITexture; virtual; abstract;
    { Creates a transient carrier texture (with VK_IMAGE_CREATE_ALIAS_BIT on Vulkan)
      backed by at least AMinCarrierBytes so non-overlapping transient textures of
      different formats, resolutions, or usages can alias its physical GPU memory. }
    function CreateTransientCarrierTexture(const ADesc: TRHITextureDesc;
      const AMinCarrierBytes: UInt64 = 0): TRHITexture; virtual;
    { Attempts to create an aliased texture bound to the exact physical GPU memory
      of ACarrierTexture without allocating any new VRAM (Phase 111.1).
      Returns nil if ACarrierTexture cannot host ADesc. }
    function TryCreateAliasedTexture(const ADesc: TRHITextureDesc;
      const ACarrierTexture: TRHITexture): TRHITexture; virtual;
    function CreateSampler(const ADesc: TRHISamplerDesc): TRHISampler; virtual; abstract;

    { Compiles GLSL source (or loads cached SPIR-V) into a Shader Module. }
    function CreateShaderModuleFromGLSL(const AStage: TRHIShaderStage;
      const AGLSLSource: String; const ADebugName: String = ''): TRHIShaderModule; virtual; abstract;
    function CreateShaderModuleFromSPIRV(const AStage: TRHIShaderStage;
      const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt;
      const ADebugName: String = ''): TRHIShaderModule; virtual; abstract;

    function CreateDescriptorSetLayout(
      const Bindings: array of TRHIDescriptorBindingDesc): TRHIDescriptorSetLayout; virtual; abstract;
    function CreateDescriptorSet(
      const Layout: TRHIDescriptorSetLayout): TRHIDescriptorSet; virtual; abstract;

    { Creates or retrieves a cached Pipeline State Object matching ADesc. }
    function GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline; virtual; abstract;

    { Creates or retrieves a cached Compute Pipeline State Object matching ADesc. }
    function GetOrCreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline; virtual;
    function CreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline; virtual;

    { Creates a hardware query pool for GPU timestamps or occlusion queries. }
    function CreateQueryPool(const ADesc: TRHIQueryPoolDesc): TRHIQueryPool; virtual;

    { Readback current swapchain or texture into a CPU TCastleImage (for TCastleWindow.SaveScreen). }
    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaImage; overload; virtual; abstract;
    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle; const DestImage: TCastleImage): Boolean; overload; virtual;
    { Reads back a specific 2D subresource slice (MipLevel, ArrayLayer) into a TRGBAlphaImage. }
    function GrabTextureSubresourceImage(const SourceTexture: TRHITexture;
      const AMipLevel, AArrayLayer: Cardinal): TRGBAlphaImage; virtual;
    { Reads back any color/HDR/float/depth texture into a full-precision TRGBAlphaFloatImage. }
    function GrabTextureFloatImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaFloatImage; virtual;

    { Pipeline Cache persistence (loads/saves VkPipelineCache to disk) }
    procedure LoadPipelineCache(const CacheFileUrl: String); virtual; abstract;
    procedure SavePipelineCache(const CacheFileUrl: String); virtual; abstract;

    { Multiline and one-line summary strings describing the active RHI device and capabilities. }
    function GetInformationString: String; virtual;
    function GetInformationStringSummary: String; virtual;

    { Registers a (Texture, Sampler) pair in the device's global bindless descriptor array
      (Set 1, Binding 0) and returns its 0-based bindless slot index (or 0 if Texture = nil). }
    function RegisterBindlessTexture(const ATexture: TRHITexture;
      const ASampler: TRHISampler = nil): Cardinal; virtual;

    { Registers or updates an 80-byte std430 bindless material entry in the device's
      global Material SSBO table (Set 1, Binding 1) and returns its 0-based material index. }
    function RegisterOrUpdateBindlessMaterial(var AMaterialIndex: Cardinal;
      var AAlreadyRegistered: Boolean;
      const AEntryData; const AEntrySizeInBytes: Cardinal): Cardinal; virtual;

    { Returns the device's global bindless descriptor set (Set 1, holding castle_BindlessTextures[]
      at Binding 0 and CastleBindlessMaterialsBlock at Binding 1), or nil if not supported. }
    function GetBindlessDescriptorSet: TRHIDescriptorSet; virtual;

    property Capabilities: TRHIDeviceCapabilities read FCapabilities;
    { Current frame-in-flight index (0 .. MaxFramesInFlight - 1) }
    property FrameIndex: Cardinal read FFrameIndex;
    { Monotonically increasing count of frames started via BeginFrame / BeginOffscreenFrame }
    property TotalFramesStarted: Cardinal read FTotalFramesStarted;
  end;

  { -------------------------------------------------------------------------- }
  { Phase 15.1: Declarative Render Dependency Graph (TRHIRenderGraph)          }
  { -------------------------------------------------------------------------- }

  TRHIRenderGraphPassType = (
    rgptGraphics,
    rgptCompute,
    rgptTransfer
  );

  TRHIRenderGraphResourceHandle = Integer;

  TRHIRenderGraphTextureBarrier = record
    TextureHandle: TRHIRenderGraphResourceHandle;
    PreviousAliasedTexture: TRHITexture;
    IsAliasingHandoff: Boolean;
    BeforeState: TRHIResourceState;
    AfterState: TRHIResourceState;
  end;

  TRHIRenderGraphTextureAccess = record
    Handle: TRHIRenderGraphResourceHandle;
    State: TRHIResourceState;
    LoadOp: TRHIAttachmentLoadOp;
    StoreOp: TRHIAttachmentStoreOp;
    ClearColor: TCastleColor;
    ClearDepth: Single;
    IsDepthAttachment: Boolean;
    IsColorAttachment: Boolean;
  end;

  TRHIRenderGraphPassExecuteEvent = procedure(const AGraph: TRHIRenderGraph;
    const APass: TRHIRenderGraphPass; const ACmd: TRHICommandList) of object;
  TRHIRenderGraphPassExecuteProc = procedure(const AGraph: TRHIRenderGraph;
    const APass: TRHIRenderGraphPass; const ACmd: TRHICommandList);

  { Represents a single graphics, compute, or transfer pass registered in TRHIRenderGraph. }
  TRHIRenderGraphPass = class
  private
    FName: String;
    FPassIndex: Integer;
    FPassType: TRHIRenderGraphPassType;
    FHasSideEffects: Boolean;
    FIsCulled: Boolean;
    FExecutionOrder: Integer;
    FTextureReads: array of TRHIRenderGraphTextureAccess;
    FTextureWrites: array of TRHIRenderGraphTextureAccess;
    FBufferReads: array of TRHIRenderGraphResourceHandle;
    FBufferWrites: array of TRHIRenderGraphResourceHandle;
    FPrePassBarriers: array of TRHIRenderGraphTextureBarrier;
    FOnExecute: TRHIRenderGraphPassExecuteEvent;
    FOnExecuteProc: TRHIRenderGraphPassExecuteProc;
  public
    constructor Create(const AName: String; const APassIndex: Integer;
      const APassType: TRHIRenderGraphPassType; const AHasSideEffects: Boolean = False);

    { Declares that this pass reads AHandle in AReadState (default rsShaderRead). }
    procedure ReadTexture(const AHandle: TRHIRenderGraphResourceHandle;
      const AReadState: TRHIResourceState = rsShaderRead);

    { Declares that this pass renders into AHandle as a color attachment. }
    procedure WriteColorAttachment(const AHandle: TRHIRenderGraphResourceHandle;
      const ALoadOp: TRHIAttachmentLoadOp = loClear;
      const AStoreOp: TRHIAttachmentStoreOp = soStore); overload;
    procedure WriteColorAttachment(const AHandle: TRHIRenderGraphResourceHandle;
      const ALoadOp: TRHIAttachmentLoadOp;
      const AClearColor: TCastleColor;
      const AStoreOp: TRHIAttachmentStoreOp = soStore); overload;

    { Declares that this pass writes into AHandle as a depth/stencil attachment. }
    procedure WriteDepthAttachment(const AHandle: TRHIRenderGraphResourceHandle;
      const ALoadOp: TRHIAttachmentLoadOp = loClear;
      const AClearDepth: Single = 1.0;
      const AStoreOp: TRHIAttachmentStoreOp = soStore);

    { Declares that this compute/transfer pass writes into AHandle in AWriteState. }
    procedure WriteStorageTexture(const AHandle: TRHIRenderGraphResourceHandle;
      const AWriteState: TRHIResourceState = rsUnorderedAccess);

    { Declares buffer read/write dependencies for topological ordering. }
    procedure ReadBuffer(const AHandle: TRHIRenderGraphResourceHandle);
    procedure WriteBuffer(const AHandle: TRHIRenderGraphResourceHandle);

    function PrePassBarriersCount: Integer;
    function GetPrePassBarrier(const AIndex: Integer): TRHIRenderGraphTextureBarrier;

    property Name: String read FName;
    property PassIndex: Integer read FPassIndex;
    property PassType: TRHIRenderGraphPassType read FPassType;
    property HasSideEffects: Boolean read FHasSideEffects write FHasSideEffects;
    property IsCulled: Boolean read FIsCulled;
    property ExecutionOrder: Integer read FExecutionOrder;
    property OnExecute: TRHIRenderGraphPassExecuteEvent read FOnExecute write FOnExecute;
    property OnExecuteProc: TRHIRenderGraphPassExecuteProc read FOnExecuteProc write FOnExecuteProc;
  end;

  { Declarative Frame Render Graph performing:
    1. Pass-level read/write dependency analysis and topological sorting (Kahn's DAG algorithm).
    2. Backward liveness analysis to automatically cull unused passes (dead-pass culling).
    3. Transient render-target lifetime tracking and physical texture aliasing across non-overlapping passes.
    4. Automatic pre-pass TransitionTexture barrier hoisting and Dynamic Rendering BeginRendering/EndRendering. }
  TRHIRenderGraph = class
  private
    type
      TVirtualTextureEntry = record
        Name: String;
        Desc: TRHITextureDesc;
        IsImported: Boolean;
        InitialState: TRHIResourceState;
        FinalState: TRHIResourceState;
        PhysicalTexture: TRHITexture;
        PoolSlotIndex: Integer;
        FirstUseOrder: Integer;
        LastUseOrder: Integer;
        LastProducerPass: Integer;
      end;
      TVirtualBufferEntry = record
        Name: String;
        PhysicalBuffer: TRHIBuffer;
        LastProducerPass: Integer;
      end;
      TAliasedTextureVariant = record
        Desc: TRHITextureDesc;
        Texture: TRHITexture;
      end;
      TTransientPoolSlot = record
        Desc: TRHITextureDesc;
        Texture: TRHITexture;
        CarrierCapacityBytes: UInt64;
        AliasedVariants: array of TAliasedTextureVariant;
        LastAssignedOrder: Integer;
        LastActiveTexture: TRHITexture;
      end;
      TRHIRenderGraphPassList = {$ifdef FPC}specialize{$endif} TObjectList<TRHIRenderGraphPass>;
  private
    FDevice: TRHIDevice;
    FPasses: TRHIRenderGraphPassList;
    FCompiledPasses: array of TRHIRenderGraphPass;
    FVirtualTextures: array of TVirtualTextureEntry;
    FVirtualBuffers: array of TVirtualBufferEntry;
    FTransientPool: array of TTransientPoolSlot;
    FFinalExportBarriers: array of TRHIRenderGraphTextureBarrier;
    FIsCompiled: Boolean;
    FCulledPassCount: Integer;
    FEmittedBarrierCount: Integer;
    FAliasingBarrierCount: Integer;
    FTransientTextureCount: Integer;
    FAliasedTextureSavingsCount: Integer;
    FMemoryAliasedTextureCount: Integer;
    FAliasedMemorySavedBytes: UInt64;
    FExecutionCount: Cardinal;
    FDebugScopesEmittedCount: Cardinal;
    class function AreTextureDescsCompatibleForAliasing(
      const A, B: TRHITextureDesc): Boolean; static;
    function GetPassCount: Integer;
    function GetCompiledPassCount: Integer;
    function GetPhysicalTransientTextureCount: Integer;
  public
    constructor Create(const ADevice: TRHIDevice);
    destructor Destroy; override;

    { Clears declared passes and virtual resources for a new frame while keeping
      the pooled physical transient textures alive for zero-allocation frame reuse. }
    procedure Reset;

    { Destroys all pooled physical transient textures. }
    procedure ReleaseTransientPool;

    { Declares a transient graph-managed texture that will be allocated (or aliased) during Compile. }
    function DeclareTransientTexture(const AName: String;
      const ADesc: TRHITextureDesc): TRHIRenderGraphResourceHandle;

    { Imports an persistent external texture (e.g. Swapchain backbuffer or persistent history target). }
    function ImportExternalTexture(const AName: String;
      const ATexture: TRHITexture;
      const AInitialState: TRHIResourceState = rsUndefined;
      const AFinalState: TRHIResourceState = rsUndefined): TRHIRenderGraphResourceHandle;

    { Imports an external buffer for pass dependency tracking. }
    function ImportExternalBuffer(const AName: String;
      const ABuffer: TRHIBuffer): TRHIRenderGraphResourceHandle;

    { Adds a pass to the render graph. }
    function AddPass(const AName: String;
      const APassType: TRHIRenderGraphPassType = rgptGraphics;
      const AHasSideEffects: Boolean = False): TRHIRenderGraphPass;

    { Compiles the render graph: resolves producer-consumer edges, culls dead passes,
      topologically sorts active passes, aliases non-overlapping transient textures,
      and hoists pre-pass resource state transitions. }
    procedure Compile;

    { Executes the compiled render graph onto ACmd, automatically recording hoisted
      TransitionTexture barriers and wrapping graphics passes in BeginRendering/EndRendering. }
    procedure Execute(const ACmd: TRHICommandList);

    { Returns the physical TRHITexture bound to AHandle (after Compile). }
    function ResolvePhysicalTexture(const AHandle: TRHIRenderGraphResourceHandle): TRHITexture;

    { Returns the physical TRHIBuffer bound to AHandle. }
    function ResolvePhysicalBuffer(const AHandle: TRHIRenderGraphResourceHandle): TRHIBuffer;

    { Returns the compiled pass at AOrderIndex (0 .. CompiledPassCount - 1). }
    function GetCompiledPass(const AOrderIndex: Integer): TRHIRenderGraphPass;

    property Device: TRHIDevice read FDevice;
    property IsCompiled: Boolean read FIsCompiled;
    property PassCount: Integer read GetPassCount;
    property CompiledPassCount: Integer read GetCompiledPassCount;
    property CulledPassCount: Integer read FCulledPassCount;
    property EmittedBarrierCount: Integer read FEmittedBarrierCount;
    property TransientTextureCount: Integer read FTransientTextureCount;
    property PhysicalTransientTextureCount: Integer read GetPhysicalTransientTextureCount;
    property AliasedTextureSavingsCount: Integer read FAliasedTextureSavingsCount;
    property MemoryAliasedTextureCount: Integer read FMemoryAliasedTextureCount;
    property AliasedMemorySavedBytes: UInt64 read FAliasedMemorySavedBytes;
    property AliasingBarrierCount: Integer read FAliasingBarrierCount;
    property ExecutionCount: Cardinal read FExecutionCount;
    property DebugScopesEmittedCount: Cardinal read FDebugScopesEmittedCount;
  end;

const
  { Maximum number of frames simultaneously processed by CPU and GPU. }
  MaxFramesInFlight = 2;

type
  TRHIDeviceChangeCallback = procedure(const ADevice: TRHIDevice);

{ Global active RHI device instance managed by CastleWindow / TCastleViewport. }
function RHIDevice: TRHIDevice;
procedure SetRHIDevice(const ADevice: TRHIDevice);
procedure RegisterRHIDeviceChangeCallback(const ACallback: TRHIDeviceChangeCallback);

{ Global active RHI command list being recorded for current frame/pass. }
function ActiveCommandList: TRHICommandList;
procedure SetActiveCommandList(const ACmdList: TRHICommandList);

{ Converts TCastleWindow.AntiAliasing (TAntiAliasing) to an RHI MSAA sample count (1, 2, 4, 8, 16). }
function AntiAliasingToSampleCount(const AAntiAliasing: TAntiAliasing): Cardinal;

{ Clamps a requested MSAA sample count to the highest power-of-two sample count
  (1, 2, 4, 8, 16, 32, 64) supported by the device's MaxSupportedSamples. }
function ClampSampleCountToCapabilities(const ARequestedSamples, AMaxSupportedSamples: Cardinal): Cardinal;

{ Computes the exact full mipmap pyramid level count (floor(log2(max(1, AMaxDimension))) + 1)
  using integer bit-shifting (avoiding IEEE-754 Double Ln(8)/Ln(2) = 2.9999999999999996
  truncation on Win64 and ARM targets). }
function CalculateTextureMipLevels(const AMaxDimension: Cardinal): Cardinal;

{ Compares two framebuffer readback images (e.g. OpenGL vs Vulkan GrabTextureImage)
  by computing the normalized Root Mean Square Error (RMSE in [0..1]) across all channels.
  Returns True if dimensions/formats match and OutNormalizedRMSE <= MaxNormalizedRMSE. }
function CompareImagesPerceptualDiff(const ImageA, ImageB: TCastleImage;
  const MaxNormalizedRMSE: Single; out OutNormalizedRMSE: Single): Boolean;

type
  { Detailed perceptual comparison metrics between two rendered framebuffer images
    (e.g. side-by-side rbOpenGL vs rbVulkan visual regression verification, Phase 112.2). }
  TRHIVisualDiffMetrics = record
    DimensionsMatch: Boolean;
    NormalizedRMSE: Single;
    PSNRdB: Single;
    MaxChannelDelta: Byte;
    DiffPixelCount: Cardinal;
    TotalPixelCount: Cardinal;
    DiffPixelRatio: Single;
    Passed: Boolean;
  end;

  { Named visual regression comparison entry recorded by TRHIVisualRegressionSuite. }
  TRHIVisualRegressionCaseResult = record
    CaseName: String;
    MaxAllowedRMSE: Single;
    Metrics: TRHIVisualDiffMetrics;
  end;

  { Structured side-by-side rbOpenGL vs rbVulkan visual regression suite runner
    and summary report generator (Phase 112.2). }
  TRHIVisualRegressionSuite = class
  private
    FCases: array of TRHIVisualRegressionCaseResult;
    FChannelDeltaTolerance: Byte;
    function GetCaseCount: Integer;
    function GetPassedCount: Integer;
    function GetFailedCount: Integer;
    function GetAllPassed: Boolean;
    function GetWorstRMSE: Single;
  public
    constructor Create(const AChannelDeltaTolerance: Byte = 2);
    procedure Clear;
    function RecordComparison(const ACaseName: String;
      const AReferenceImage, ACandidateImage: TCastleImage;
      const AMaxAllowedRMSE: Single): TRHIVisualRegressionCaseResult;
    function GetCaseResult(const AIndex: Integer): TRHIVisualRegressionCaseResult;
    function BuildSummaryReport: String;

    property ChannelDeltaTolerance: Byte read FChannelDeltaTolerance write FChannelDeltaTolerance;
    property CaseCount: Integer read GetCaseCount;
    property PassedCount: Integer read GetPassedCount;
    property FailedCount: Integer read GetFailedCount;
    property AllPassed: Boolean read GetAllPassed;
    property WorstRMSE: Single read GetWorstRMSE;
  end;

{ Computes full perceptual diff metrics (NormalizedRMSE, PSNRdB, MaxChannelDelta,
  DiffPixelCount, DiffPixelRatio) between two images and sets OutMetrics.Passed :=
  DimensionsMatch and (NormalizedRMSE <= MaxNormalizedRMSE). }
function CompareImagesDetailedDiff(const ImageA, ImageB: TCastleImage;
  const MaxNormalizedRMSE: Single; out OutMetrics: TRHIVisualDiffMetrics;
  const ChannelDeltaTolerance: Byte = 2): Boolean;

{ Maps any TEncodedImage (including floating-point images TRGBAlphaFloatImage,
  TRGBFloatImage, TGrayscaleFloatImage, TGrayscaleAlphaFloatImage and GPU-compressed
  images TGPUCompressedImage) to the corresponding TRHIPixelFormat. }
function EncodedImageToRHIPixelFormat(const AImage: TEncodedImage): TRHIPixelFormat;

{ Returns the GPU texture memory size in bytes for AImage uploaded with AFormat. }
function EncodedImageToRHIGpuBytes(const AImage: TEncodedImage;
  const AFormat: TRHIPixelFormat): Int64;

implementation

var
  FRHIDevice: TRHIDevice = nil;
  FActiveCommandList: TRHICommandList = nil;
  FRHIDeviceChangeCallbacks: array of TRHIDeviceChangeCallback;

function RHIDevice: TRHIDevice;
begin
  Result := FRHIDevice;
end;

procedure RegisterRHIDeviceChangeCallback(const ACallback: TRHIDeviceChangeCallback);
var
  I, L: Integer;
begin
  if not Assigned(ACallback) then Exit;
  for I := 0 to High(FRHIDeviceChangeCallbacks) do
    if {$ifndef FPC}@{$endif} FRHIDeviceChangeCallbacks[I] = {$ifndef FPC}@{$endif} ACallback then
      Exit;
  L := Length(FRHIDeviceChangeCallbacks);
  SetLength(FRHIDeviceChangeCallbacks, L + 1);
  FRHIDeviceChangeCallbacks[L] := ACallback;
end;

procedure SetRHIDevice(const ADevice: TRHIDevice);
var
  Comps: TTextureCompressions;
  I: Integer;
begin
  FRHIDevice := ADevice;
  if ADevice <> nil then
  begin
    Comps := [];
    if ADevice.Capabilities.SupportsBCCompression then
      Comps := Comps + [tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5];
    if ADevice.Capabilities.SupportsETC2Compression then
      Comps := Comps + [tcETC1];
    if ADevice.Capabilities.SupportsASTCCompression then
      Comps := Comps + [
        tcASTC_4x4_RGBA, tcASTC_5x4_RGBA, tcASTC_5x5_RGBA,
        tcASTC_6x5_RGBA, tcASTC_6x6_RGBA, tcASTC_8x5_RGBA,
        tcASTC_8x6_RGBA, tcASTC_8x8_RGBA, tcASTC_10x5_RGBA,
        tcASTC_10x6_RGBA, tcASTC_10x8_RGBA, tcASTC_10x10_RGBA,
        tcASTC_12x10_RGBA, tcASTC_12x12_RGBA,
        tcASTC_4x4_SRGB8_ALPHA8, tcASTC_5x4_SRGB8_ALPHA8, tcASTC_5x5_SRGB8_ALPHA8,
        tcASTC_6x5_SRGB8_ALPHA8, tcASTC_6x6_SRGB8_ALPHA8, tcASTC_8x5_SRGB8_ALPHA8,
        tcASTC_8x6_SRGB8_ALPHA8, tcASTC_8x8_SRGB8_ALPHA8, tcASTC_10x5_SRGB8_ALPHA8,
        tcASTC_10x6_SRGB8_ALPHA8, tcASTC_10x8_SRGB8_ALPHA8, tcASTC_10x10_SRGB8_ALPHA8,
        tcASTC_12x10_SRGB8_ALPHA8, tcASTC_12x12_SRGB8_ALPHA8];
    SupportedTextureCompression := Comps;
    SupportedTextureCompressionKnown := True;
  end;
  for I := 0 to High(FRHIDeviceChangeCallbacks) do
    if Assigned(FRHIDeviceChangeCallbacks[I]) then
      FRHIDeviceChangeCallbacks[I](ADevice);
end;

function ActiveCommandList: TRHICommandList;
begin
  Result := FActiveCommandList;
end;

procedure SetActiveCommandList(const ACmdList: TRHICommandList);
begin
  FActiveCommandList := ACmdList;
end;

function AntiAliasingToSampleCount(const AAntiAliasing: TAntiAliasing): Cardinal;
begin
  case AAntiAliasing of
    aaNone: Result := 1;
    aa2SamplesFaster, aa2SamplesNicer: Result := 2;
    aa4SamplesFaster, aa4SamplesNicer: Result := 4;
    aa8SamplesFaster, aa8SamplesNicer: Result := 8;
    aa16SamplesFaster, aa16SamplesNicer: Result := 16;
  else
    Result := 1;
  end;
end;

function ClampSampleCountToCapabilities(const ARequestedSamples, AMaxSupportedSamples: Cardinal): Cardinal;
var
  MaxAllowed: Cardinal;
begin
  if (ARequestedSamples <= 1) or (AMaxSupportedSamples <= 1) then
    Exit(1);
  MaxAllowed := ARequestedSamples;
  if MaxAllowed > AMaxSupportedSamples then
    MaxAllowed := AMaxSupportedSamples;
  if MaxAllowed >= 64 then Result := 64 else
  if MaxAllowed >= 32 then Result := 32 else
  if MaxAllowed >= 16 then Result := 16 else
  if MaxAllowed >= 8 then Result := 8 else
  if MaxAllowed >= 4 then Result := 4 else
  if MaxAllowed >= 2 then Result := 2 else
    Result := 1;
end;

function CalculateTextureMipLevels(const AMaxDimension: Cardinal): Cardinal;
var
  Dim: Cardinal;
begin
  Result := 1;
  Dim := AMaxDimension;
  while Dim > 1 do
  begin
    Dim := Dim shr 1;
    Inc(Result);
  end;
end;

function CompareImagesPerceptualDiff(const ImageA, ImageB: TCastleImage;
  const MaxNormalizedRMSE: Single; out OutNormalizedRMSE: Single): Boolean;
var
  TotalSamples, I: PtrUInt;
  PA, PB: PByte;
  Diff, SumSq: Double;
begin
  OutNormalizedRMSE := 1.0;
  if (ImageA = nil) or (ImageB = nil) or
     (ImageA.Width <> ImageB.Width) or
     (ImageA.Height <> ImageB.Height) or
     (ImageA.Depth <> ImageB.Depth) or
     (ImageA.PixelSize <> ImageB.PixelSize) then
    Exit(False);

  TotalSamples := PtrUInt(ImageA.Width) * PtrUInt(ImageA.Height) *
                  PtrUInt(ImageA.Depth) * PtrUInt(ImageA.PixelSize);
  if TotalSamples = 0 then
  begin
    OutNormalizedRMSE := 0.0;
    Exit(True);
  end;

  PA := PByte(ImageA.RawPixels);
  PB := PByte(ImageB.RawPixels);
  SumSq := 0.0;
  for I := 0 to TotalSamples - 1 do
  begin
    Diff := (Integer(PA[I]) - Integer(PB[I])) / 255.0;
    SumSq := SumSq + Diff * Diff;
  end;

  OutNormalizedRMSE := Sqrt(SumSq / TotalSamples);
  Result := OutNormalizedRMSE <= MaxNormalizedRMSE;
end;

function CompareImagesDetailedDiff(const ImageA, ImageB: TCastleImage;
  const MaxNormalizedRMSE: Single; out OutMetrics: TRHIVisualDiffMetrics;
  const ChannelDeltaTolerance: Byte): Boolean;
var
  TotalPixels, TotalSamples, PIdx, CIdx, SampleIdx: PtrUInt;
  ChannelsPerPixel: PtrUInt;
  PA, PB: PByte;
  AbsDelta, PixelMaxDelta: Cardinal;
  Diff, SumSq: Double;
begin
  FillChar(OutMetrics, SizeOf(OutMetrics), 0);
  OutMetrics.NormalizedRMSE := 1.0;
  OutMetrics.PSNRdB := 0.0;
  if (ImageA = nil) or (ImageB = nil) or
     (ImageA.Width <> ImageB.Width) or
     (ImageA.Height <> ImageB.Height) or
     (ImageA.Depth <> ImageB.Depth) or
     (ImageA.PixelSize <> ImageB.PixelSize) or
     (ImageA.PixelSize = 0) then
    Exit(False);

  OutMetrics.DimensionsMatch := True;
  TotalPixels := PtrUInt(ImageA.Width) * PtrUInt(ImageA.Height) * PtrUInt(ImageA.Depth);
  ChannelsPerPixel := PtrUInt(ImageA.PixelSize);
  TotalSamples := TotalPixels * ChannelsPerPixel;
  OutMetrics.TotalPixelCount := Cardinal(TotalPixels);

  if TotalSamples = 0 then
  begin
    OutMetrics.NormalizedRMSE := 0.0;
    OutMetrics.PSNRdB := 99.0;
    OutMetrics.Passed := True;
    Exit(True);
  end;

  PA := PByte(ImageA.RawPixels);
  PB := PByte(ImageB.RawPixels);
  SumSq := 0.0;
  for PIdx := 0 to TotalPixels - 1 do
  begin
    PixelMaxDelta := 0;
    SampleIdx := PIdx * ChannelsPerPixel;
    for CIdx := 0 to ChannelsPerPixel - 1 do
    begin
      AbsDelta := Cardinal(Abs(Integer(PA[SampleIdx + CIdx]) - Integer(PB[SampleIdx + CIdx])));
      if AbsDelta > PixelMaxDelta then
        PixelMaxDelta := AbsDelta;
      if AbsDelta > OutMetrics.MaxChannelDelta then
        OutMetrics.MaxChannelDelta := Byte(Min(Cardinal(255), AbsDelta));
      Diff := AbsDelta / 255.0;
      SumSq := SumSq + Diff * Diff;
    end;
    if PixelMaxDelta > ChannelDeltaTolerance then
      Inc(OutMetrics.DiffPixelCount);
  end;

  OutMetrics.NormalizedRMSE := Sqrt(SumSq / TotalSamples);
  if OutMetrics.NormalizedRMSE <= 1e-7 then
    OutMetrics.PSNRdB := 99.0
  else
    OutMetrics.PSNRdB := -20.0 * Log10(OutMetrics.NormalizedRMSE);
  OutMetrics.DiffPixelRatio := OutMetrics.DiffPixelCount / TotalPixels;
  OutMetrics.Passed := OutMetrics.NormalizedRMSE <= MaxNormalizedRMSE;
  Result := OutMetrics.Passed;
end;

{ TRHIVisualRegressionSuite }

constructor TRHIVisualRegressionSuite.Create(const AChannelDeltaTolerance: Byte);
begin
  inherited Create;
  FChannelDeltaTolerance := AChannelDeltaTolerance;
end;

procedure TRHIVisualRegressionSuite.Clear;
begin
  SetLength(FCases, 0);
end;

function TRHIVisualRegressionSuite.GetCaseCount: Integer;
begin
  Result := Length(FCases);
end;

function TRHIVisualRegressionSuite.GetPassedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FCases) do
    if FCases[I].Metrics.Passed then
      Inc(Result);
end;

function TRHIVisualRegressionSuite.GetFailedCount: Integer;
begin
  Result := Length(FCases) - GetPassedCount;
end;

function TRHIVisualRegressionSuite.GetAllPassed: Boolean;
begin
  Result := (Length(FCases) > 0) and (GetFailedCount = 0);
end;

function TRHIVisualRegressionSuite.GetWorstRMSE: Single;
var
  I: Integer;
begin
  Result := 0.0;
  for I := 0 to High(FCases) do
    if FCases[I].Metrics.NormalizedRMSE > Result then
      Result := FCases[I].Metrics.NormalizedRMSE;
end;

function TRHIVisualRegressionSuite.RecordComparison(const ACaseName: String;
  const AReferenceImage, ACandidateImage: TCastleImage;
  const AMaxAllowedRMSE: Single): TRHIVisualRegressionCaseResult;
var
  Idx: Integer;
begin
  Result.CaseName := ACaseName;
  Result.MaxAllowedRMSE := AMaxAllowedRMSE;
  CompareImagesDetailedDiff(AReferenceImage, ACandidateImage,
    AMaxAllowedRMSE, Result.Metrics, FChannelDeltaTolerance);
  Idx := Length(FCases);
  SetLength(FCases, Idx + 1);
  FCases[Idx] := Result;
end;

function TRHIVisualRegressionSuite.GetCaseResult(
  const AIndex: Integer): TRHIVisualRegressionCaseResult;
begin
  if (AIndex >= 0) and (AIndex < Length(FCases)) then
    Result := FCases[AIndex]
  else
    Result := Default(TRHIVisualRegressionCaseResult);
end;

function TRHIVisualRegressionSuite.BuildSummaryReport: String;
var
  I: Integer;
  StatusStr: String;
begin
  Result := Format('RHI Visual Regression Suite (%d/%d passed, worst RMSE=%.5f):',
    [GetPassedCount, Length(FCases), GetWorstRMSE]) + LineEnding;
  for I := 0 to High(FCases) do
  begin
    if FCases[I].Metrics.Passed then
      StatusStr := 'PASS'
    else
      StatusStr := 'FAIL';
    Result := Result + Format(
      '  [%s] %s: RMSE=%.5f <= %.5f, PSNR=%.2f dB, MaxDelta=%d, DiffPixels=%d/%d (%.2f%%)',
      [StatusStr,
       FCases[I].CaseName,
       FCases[I].Metrics.NormalizedRMSE,
       FCases[I].MaxAllowedRMSE,
       FCases[I].Metrics.PSNRdB,
       FCases[I].Metrics.MaxChannelDelta,
       FCases[I].Metrics.DiffPixelCount,
       FCases[I].Metrics.TotalPixelCount,
       FCases[I].Metrics.DiffPixelRatio * 100.0]) + LineEnding;
  end;
end;

{ TRHIBufferDesc }

class function TRHIBufferDesc.Create(const ASize: PtrUInt;
  const AUsage: TRHIBufferUsageFlags;
  const AMemoryUsage: TRHIMemoryUsage;
  const ADebugName: String): TRHIBufferDesc;
begin
  Result.Size := ASize;
  Result.Usage := AUsage;
  Result.MemoryUsage := AMemoryUsage;
  Result.DebugName := ADebugName;
end;

{ TRHITextureDesc }

class function TRHITextureDesc.Create2D(const AWidth, AHeight: Cardinal;
  const AFormat: TRHIPixelFormat; const AUsage: TRHITextureUsageFlags;
  const AMipLevels: Cardinal; const ASampleCount: Cardinal): TRHITextureDesc;
begin
  Result := Default(TRHITextureDesc);
  Result.Dimension := td2D;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.DepthOrArrayLayers := 1;
  Result.MipLevels := AMipLevels;
  if ASampleCount > 1 then
    Result.SampleCount := ASampleCount
  else
    Result.SampleCount := 1;
  Result.Format := AFormat;
  Result.Usage := AUsage;
end;

class function TRHITextureDesc.Create3D(const AWidth, AHeight, ADepth: Cardinal;
  const AFormat: TRHIPixelFormat; const AUsage: TRHITextureUsageFlags;
  const AMipLevels: Cardinal): TRHITextureDesc;
begin
  Result := Create2D(AWidth, AHeight, AFormat, AUsage, AMipLevels, 1);
  Result.Dimension := td3D;
  if ADepth > 1 then
    Result.DepthOrArrayLayers := ADepth
  else
    Result.DepthOrArrayLayers := 1;
end;

class function TRHITextureDesc.CreateCube(const ASize: Cardinal;
  const AFormat: TRHIPixelFormat; const AUsage: TRHITextureUsageFlags;
  const AMipLevels: Cardinal): TRHITextureDesc;
begin
  Result := Create2D(ASize, ASize, AFormat, AUsage, AMipLevels, 1);
  Result.Dimension := tdCube;
  Result.DepthOrArrayLayers := 6;
end;

{ TRHIQueryPoolDesc }

class function TRHIQueryPoolDesc.Create(const AQueryType: TRHIQueryType;
  const AQueryCount: Cardinal; const ADebugName: String): TRHIQueryPoolDesc;
begin
  Result.QueryType := AQueryType;
  Result.QueryCount := AQueryCount;
  Result.DebugName := ADebugName;
end;

{ TRHIBlendStateDesc }

class function TRHIBlendStateDesc.DefaultOpaque: TRHIBlendStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.BlendEnable := False;
  Result.SrcColorBlendFactor := bfOne;
  Result.DstColorBlendFactor := bfZero;
  Result.ColorBlendOp := boAdd;
  Result.SrcAlphaBlendFactor := bfOne;
  Result.DstAlphaBlendFactor := bfZero;
  Result.AlphaBlendOp := boAdd;
  Result.ColorWriteMask := $0F;
  Result.DisableColorWrites := False;
end;

class function TRHIBlendStateDesc.DefaultAlphaBlend: TRHIBlendStateDesc;
begin
  Result := DefaultOpaque;
  Result.BlendEnable := True;
  Result.SrcColorBlendFactor := bfSrcAlpha;
  Result.DstColorBlendFactor := bfOneMinusSrcAlpha;
  Result.SrcAlphaBlendFactor := bfOne;
  Result.DstAlphaBlendFactor := bfOneMinusSrcAlpha;
end;

class function TRHIBlendStateDesc.DefaultColorMaskNone: TRHIBlendStateDesc;
begin
  Result := DefaultOpaque;
  Result.ColorWriteMask := 0;
  Result.DisableColorWrites := True;
end;

{ TRHIRasterizerStateDesc }

class function TRHIRasterizerStateDesc.DefaultCullBack: TRHIRasterizerStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.PolygonMode := pmFill;
  Result.CullMode := cmBack;
  Result.FrontFace := ffCounterClockwise;
  Result.DepthBiasEnable := False;
  Result.DepthClampEnable := False;
end;

{ TRHIStencilOpStateDesc }

class function TRHIStencilOpStateDesc.DefaultKeep: TRHIStencilOpStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.FailOp := soKeep;
  Result.PassOp := soKeep;
  Result.DepthFailOp := soKeep;
  Result.CompareOp := coAlways;
  Result.CompareMask := $FF;
  Result.WriteMask := $FF;
  Result.Reference := 0;
end;

{ TRHIDepthStencilStateDesc }

class function TRHIDepthStencilStateDesc.DefaultDepthTestWrite: TRHIDepthStencilStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.DepthTestEnable := True;
  Result.DepthWriteEnable := True;
  Result.DepthCompareOp := coLessOrEqual;
  Result.StencilTestEnable := False;
  Result.FrontStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.BackStencil := TRHIStencilOpStateDesc.DefaultKeep;
end;

class function TRHIDepthStencilStateDesc.DefaultDepthReadOnly: TRHIDepthStencilStateDesc;
begin
  Result := DefaultDepthTestWrite;
  Result.DepthWriteEnable := False;
end;

class function TRHIDepthStencilStateDesc.DefaultDisabled: TRHIDepthStencilStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.DepthTestEnable := False;
  Result.DepthWriteEnable := False;
  Result.DepthCompareOp := coAlways;
  Result.StencilTestEnable := False;
  Result.FrontStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.BackStencil := TRHIStencilOpStateDesc.DefaultKeep;
end;

class function TRHIDepthStencilStateDesc.DefaultShadowMap: TRHIDepthStencilStateDesc;
begin
  Result := DefaultDepthTestWrite;
end;

class function TRHIDepthStencilStateDesc.DefaultStencilShadowVolumeZPass: TRHIDepthStencilStateDesc;
begin
  Result := DefaultDepthReadOnly;
  Result.StencilTestEnable := True;
  { Front faces: increment stencil (with wrap) when depth test passes }
  Result.FrontStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.FrontStencil.PassOp := soIncrementAndWrap;
  { Back faces: decrement stencil (with wrap) when depth test passes }
  Result.BackStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.BackStencil.PassOp := soDecrementAndWrap;
end;

class function TRHIDepthStencilStateDesc.DefaultStencilShadowVolumeZFail: TRHIDepthStencilStateDesc;
begin
  Result := DefaultDepthReadOnly;
  Result.StencilTestEnable := True;
  { Front faces: decrement stencil (with wrap) when depth test fails }
  Result.FrontStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.FrontStencil.DepthFailOp := soDecrementAndWrap;
  { Back faces: increment stencil (with wrap) when depth test fails }
  Result.BackStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.BackStencil.DepthFailOp := soIncrementAndWrap;
end;

class function TRHIDepthStencilStateDesc.DefaultStencilLitPass(
  const AStencilReference: Cardinal; const ACompareMask: Cardinal): TRHIDepthStencilStateDesc;
begin
  Result := DefaultDepthReadOnly;
  Result.StencilTestEnable := True;
  Result.FrontStencil := TRHIStencilOpStateDesc.DefaultKeep;
  Result.FrontStencil.CompareOp := coEqual;
  Result.FrontStencil.CompareMask := ACompareMask;
  Result.FrontStencil.WriteMask := 0;
  Result.FrontStencil.Reference := AStencilReference;
  Result.BackStencil := Result.FrontStencil;
end;

{ TRHIRenderingInfo }

class function TRHIRenderingInfo.CreateColorDepth(const ARenderArea: TRectangle;
  const AColorTexture: TRHITexture; const AColorLoadOp: TRHIAttachmentLoadOp;
  const AClearColor: TCastleColor; const ADepthTexture: TRHITexture;
  const ADepthLoadOp: TRHIAttachmentLoadOp; const AClearDepth: Single): TRHIRenderingInfo;
begin
  Result := Default(TRHIRenderingInfo);
  Result.RenderArea := ARenderArea;
  if AColorTexture <> nil then
  begin
    SetLength(Result.ColorAttachments, 1);
    Result.ColorAttachments[0].Texture := AColorTexture;
    Result.ColorAttachments[0].LoadOp := AColorLoadOp;
    Result.ColorAttachments[0].StoreOp := soStore;
    Result.ColorAttachments[0].ClearColor := AClearColor;
  end;
  if ADepthTexture <> nil then
  begin
    Result.HasDepthAttachment := True;
    Result.DepthAttachment.Texture := ADepthTexture;
    Result.DepthAttachment.LoadOp := ADepthLoadOp;
    Result.DepthAttachment.StoreOp := soStore;
    Result.DepthAttachment.ClearDepth := AClearDepth;
  end;
end;

class function TRHIRenderingInfo.CreateColorDepthMSAA(const ARenderArea: TRectangle;
  const AMSAAColorTexture, AResolveColorTexture: TRHITexture;
  const AColorLoadOp: TRHIAttachmentLoadOp; const AClearColor: TCastleColor;
  const AMSAADepthTexture: TRHITexture; const ADepthLoadOp: TRHIAttachmentLoadOp;
  const AClearDepth: Single;
  const AResolveDepthTexture: TRHITexture): TRHIRenderingInfo;
begin
  Result := Default(TRHIRenderingInfo);
  Result.RenderArea := ARenderArea;
  if AMSAAColorTexture <> nil then
  begin
    SetLength(Result.ColorAttachments, 1);
    Result.ColorAttachments[0].Texture := AMSAAColorTexture;
    Result.ColorAttachments[0].LoadOp := AColorLoadOp;
    { When resolving in a single tile-local pass, transient MSAA samples do not need
      to be written back to external memory after resolving into AResolveColorTexture. }
    if AResolveColorTexture <> nil then
    begin
      Result.ColorAttachments[0].StoreOp := soDontCare;
      Result.ColorAttachments[0].ResolveTexture := AResolveColorTexture;
      Result.ColorAttachments[0].ResolveMode := rmAverage;
    end else
      Result.ColorAttachments[0].StoreOp := soStore;
    Result.ColorAttachments[0].ClearColor := AClearColor;
  end;
  if AMSAADepthTexture <> nil then
  begin
    Result.HasDepthAttachment := True;
    Result.DepthAttachment.Texture := AMSAADepthTexture;
    Result.DepthAttachment.LoadOp := ADepthLoadOp;
    if AResolveDepthTexture <> nil then
    begin
      Result.DepthAttachment.StoreOp := soDontCare;
      Result.DepthAttachment.ResolveTexture := AResolveDepthTexture;
      Result.DepthAttachment.ResolveMode := rmSampleZero;
    end else
      Result.DepthAttachment.StoreOp := soDontCare;
    Result.DepthAttachment.ClearDepth := AClearDepth;
  end;
end;

class function TRHIRenderingInfo.CreateDepthOnly(const ARenderArea: TRectangle;
  const ADepthTexture: TRHITexture; const ADepthLoadOp: TRHIAttachmentLoadOp;
  const ADepthStoreOp: TRHIAttachmentStoreOp;
  const AClearDepth: Single): TRHIRenderingInfo;
begin
  Result := Default(TRHIRenderingInfo);
  Result.RenderArea := ARenderArea;
  SetLength(Result.ColorAttachments, 0);
  Result.HasDepthAttachment := True;
  Result.DepthAttachment.Texture := ADepthTexture;
  Result.DepthAttachment.LoadOp := ADepthLoadOp;
  Result.DepthAttachment.StoreOp := ADepthStoreOp;
  Result.DepthAttachment.ClearDepth := AClearDepth;
end;

{ TRHIResource }

var
  {$if defined(FPC) and not defined(CPU64)}
  GlobalRHIResourceSerial32: LongInt = 0;
  {$else}
  GlobalRHIResourceSerial: Int64 = 0;
  {$endif}

constructor TRHIResource.Create(const ADevice: TRHIDevice; const ADebugName: String);
begin
  inherited Create;
  FDevice := ADevice;
  FDebugName := ADebugName;
  {$if defined(FPC) and defined(CPU64)}
  FUniqueId := QWord(InterlockedIncrement64(GlobalRHIResourceSerial));
  {$elseif defined(FPC)}
  FUniqueId := QWord(Cardinal(InterlockedIncrement(GlobalRHIResourceSerial32)));
  {$else}
  FUniqueId := UInt64(AtomicIncrement(GlobalRHIResourceSerial));
  {$endif}
  if (FDevice <> nil) and (FDevice.FResources <> nil) then
    FDevice.FResources.Add(Self);
end;

procedure TRHIResource.ReleaseDeviceResources;
begin
  { Subclasses override to release native GPU handles while FDevice is still valid. }
end;

destructor TRHIResource.Destroy;
begin
  ReleaseDeviceResources;
  if (FDevice <> nil) and (FDevice.FResources <> nil) then
    FDevice.FResources.Remove(Self);
  FDevice := nil;
  inherited;
end;

{ TRHIBuffer }

constructor TRHIBuffer.Create(const ADevice: TRHIDevice; const ADesc: TRHIBufferDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

function TRHIBuffer.GetDeviceAddress: UInt64;
begin
  Result := 0;
end;

{ TRHITexture }

constructor TRHITexture.Create(const ADevice: TRHIDevice; const ADesc: TRHITextureDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
  FCurrentState := rsUndefined;
end;

procedure TRHITexture.GenerateMipmaps;
begin
  { Subclasses override to record or execute mipmap chain blits. }
end;

class function TRHITexture.EstimateDescriptorByteSize(const ADesc: TRHITextureDesc): UInt64;
var
  Bpp, MipW, MipH, MipD, Layers, Samples, TotalBytes: UInt64;
  M, MipCnt: Cardinal;
begin
  case ADesc.Format of
    pfRGBA32_Float: Bpp := 16;
    pfRGBA16_Float, pfD32_Float_S8_UInt: Bpp := 8;
    pfR16_Float, pfRG8_UNorm, pfD16_UNorm: Bpp := 2;
    pfR8_UNorm: Bpp := 1;
  else
    Bpp := 4;
  end;
  Samples := Max(Cardinal(1), ADesc.SampleCount);
  MipCnt := Max(Cardinal(1), ADesc.MipLevels);
  MipW := Max(Cardinal(1), ADesc.Width);
  MipH := Max(Cardinal(1), ADesc.Height);
  if ADesc.Dimension = td3D then
  begin
    MipD := Max(Cardinal(1), ADesc.DepthOrArrayLayers);
    Layers := 1;
  end else
  begin
    MipD := 1;
    Layers := Max(Cardinal(1), ADesc.DepthOrArrayLayers);
  end;
  TotalBytes := 0;
  for M := 0 to MipCnt - 1 do
  begin
    Inc(TotalBytes, MipW * MipH * MipD * Layers * Samples * Bpp);
    if MipW > 1 then MipW := MipW div 2;
    if MipH > 1 then MipH := MipH div 2;
    if MipD > 1 then MipD := MipD div 2;
  end;
  Result := TotalBytes;
end;

function TRHITexture.GetEstimatedByteSize: UInt64;
begin
  Result := EstimateDescriptorByteSize(FDesc);
end;

function TRHITexture.GetRenderedLayerMask: Cardinal;
begin
  Result := 0;
end;

{ TRHISampler }

constructor TRHISampler.Create(const ADevice: TRHIDevice; const ADesc: TRHISamplerDesc);
begin
  inherited Create(ADevice, '');
  FDesc := ADesc;
end;

{ TRHIShaderModule }

constructor TRHIShaderModule.Create(const ADevice: TRHIDevice;
  const AStage: TRHIShaderStage; const ADebugName: String);
begin
  inherited Create(ADevice, ADebugName);
  FStage := AStage;
end;

{ TRHIDescriptorSet }

procedure TRHIDescriptorSet.BindTextureAtIndex(const Binding, ArrayElement: Cardinal;
  const Texture: TRHITexture; const Sampler: TRHISampler);
begin
  BindTexture(Binding, Texture, Sampler);
end;

procedure TRHIDescriptorSet.BindTextureSubresource(const Binding: Cardinal;
  const Texture: TRHITexture; const MipLevel, ArrayLayer: Cardinal;
  const Sampler: TRHISampler);
begin
  BindTexture(Binding, Texture, Sampler);
end;

{ TRHIPipeline }

constructor TRHIPipeline.Create(const ADevice: TRHIDevice; const ADesc: TRHIGraphicsPipelineDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

procedure TRHIPipeline.UpdateDynamicStateDesc(const ADesc: TRHIGraphicsPipelineDesc);
begin
  FDesc.Topology := ADesc.Topology;
  FDesc.Rasterizer.CullMode := ADesc.Rasterizer.CullMode;
  FDesc.Rasterizer.FrontFace := ADesc.Rasterizer.FrontFace;
  FDesc.DepthStencil.DepthTestEnable := ADesc.DepthStencil.DepthTestEnable;
  FDesc.DepthStencil.DepthWriteEnable := ADesc.DepthStencil.DepthWriteEnable;
  FDesc.DepthStencil.DepthCompareOp := ADesc.DepthStencil.DepthCompareOp;
end;

{ TRHIComputePipeline }

constructor TRHIComputePipeline.Create(const ADevice: TRHIDevice; const ADesc: TRHIComputePipelineDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

{ TRHIQueryPool }

constructor TRHIQueryPool.Create(const ADevice: TRHIDevice; const ADesc: TRHIQueryPoolDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

{ TRHICommandList }

constructor TRHICommandList.Create(const ALevel: TRHICommandListLevel);
begin
  inherited Create;
  FLevel := ALevel;
  FIsRendering := False;
  FActiveRenderingHasColor := False;
  FActiveRenderingColorFormat := pfUndefined;
  FActiveRenderingHasDepth := False;
  FActiveRenderingDepthFormat := pfUndefined;
  FActiveRenderingSampleCount := 1;
end;

procedure TRHICommandList.EmitAliasingBarrier(const OldTexture, NewTexture: TRHITexture);
begin
  if (OldTexture = nil) and (NewTexture = nil) then ;
end;

procedure TRHICommandList.ClearAttachments(const ClearColorBuf: Boolean; const ClearColor: TCastleColor;
  const ClearDepthBuf: Boolean; const ClearDepth: Single;
  const ClearStencilBuf: Boolean; const ClearStencil: Byte);
begin
end;

procedure TRHICommandList.SetCullMode(const CullMode: TRHICullMode);
begin
end;

procedure TRHICommandList.SetFrontFace(const FrontFace: TRHIFrontFace);
begin
end;

procedure TRHICommandList.SetDepthTestEnable(const Enable: Boolean);
begin
end;

procedure TRHICommandList.SetLineWidth(const LineWidth: Single);
begin
end;

procedure TRHICommandList.SetBlendConstants(const ABlendConstants: TVector4);
begin
end;

procedure TRHICommandList.SetDepthWriteEnable(const Enable: Boolean);
begin
end;

procedure TRHICommandList.SetDepthCompareOp(const CompareOp: TRHICompareOp);
begin
end;

procedure TRHICommandList.SetPrimitiveTopology(const Topology: TRHIPrimitiveTopology);
begin
end;

procedure TRHICommandList.BindComputePipeline(const Pipeline: TRHIComputePipeline);
begin
end;

procedure TRHICommandList.BindComputeDescriptorSet(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet;
  const DynamicOffsets: array of Cardinal);
begin
  BindDescriptorSet(SetIndex, DescriptorSet, DynamicOffsets);
end;

procedure TRHICommandList.Dispatch(const GroupCountX, GroupCountY, GroupCountZ: Cardinal);
begin
end;

procedure TRHICommandList.DispatchIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt);
begin
end;

procedure TRHICommandList.ResetQueryPool(const QueryPool: TRHIQueryPool;
  const FirstQuery, QueryCount: Cardinal);
begin
end;

procedure TRHICommandList.BeginQuery(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal; const Precise: Boolean);
begin
end;

procedure TRHICommandList.EndQuery(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal);
begin
end;

procedure TRHICommandList.WriteTimestamp(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal; const Stage: TRHIShaderStage);
begin
end;

procedure TRHICommandList.DrawIndirectCount(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
  const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal);
begin
  DrawIndirect(IndirectBuffer, Offset, MaxDrawCount, Stride);
end;

procedure TRHICommandList.DrawIndexedIndirectCount(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
  const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal);
begin
  DrawIndexedIndirect(IndirectBuffer, Offset, MaxDrawCount, Stride);
end;

procedure TRHICommandList.DrawMeshTasks(const GroupCountX, GroupCountY, GroupCountZ: Cardinal);
begin
  if (GroupCountX = 0) and (GroupCountY = 0) and (GroupCountZ = 0) then ;
end;

procedure TRHICommandList.DrawMeshTasksIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const DrawCount, Stride: Cardinal);
begin
  if (IndirectBuffer = nil) and (Offset = 0) and (DrawCount = 0) and (Stride = 0) then ;
end;

procedure TRHICommandList.BeginSecondaryRecording(const AInheritedInfo: TRHIRenderingInfo);
begin
  BeginSecondaryRecording(
    AInheritedInfo,
    FloatRectangle(AInheritedInfo.RenderArea),
    AInheritedInfo.RenderArea,
    0.0, 1.0);
end;

procedure TRHICommandList.BeginSecondaryRecording(const AInheritedInfo: TRHIRenderingInfo;
  const AViewportRect: TFloatRectangle; const AScissorRect: TRectangle;
  const AMinDepth, AMaxDepth: Single);
begin
  BeginRecording;
  FIsRendering := True;
  FActiveRenderingInfo := AInheritedInfo;
  FActiveRenderingInfo.ColorAttachments := Copy(AInheritedInfo.ColorAttachments);
  FActiveRenderingHasColor := (Length(AInheritedInfo.ColorAttachments) > 0) and
    (AInheritedInfo.ColorAttachments[0].Texture <> nil);
  if FActiveRenderingHasColor then
  begin
    FActiveRenderingColorFormat := AInheritedInfo.ColorAttachments[0].Texture.Desc.Format;
    FActiveRenderingSampleCount := Max(Cardinal(1),
      AInheritedInfo.ColorAttachments[0].Texture.Desc.SampleCount);
  end else
  begin
    FActiveRenderingColorFormat := pfUndefined;
    FActiveRenderingSampleCount := 1;
  end;
  FActiveRenderingHasDepth := AInheritedInfo.HasDepthAttachment and
    (AInheritedInfo.DepthAttachment.Texture <> nil);
  if FActiveRenderingHasDepth then
  begin
    FActiveRenderingDepthFormat := AInheritedInfo.DepthAttachment.Texture.Desc.Format;
    if not FActiveRenderingHasColor then
      FActiveRenderingSampleCount := Max(Cardinal(1),
        AInheritedInfo.DepthAttachment.Texture.Desc.SampleCount);
  end else
    FActiveRenderingDepthFormat := pfUndefined;
  SetViewport(AViewportRect, AMinDepth, AMaxDepth);
  SetScissor(AScissorRect);
end;

procedure TRHICommandList.ExecuteSecondaryCommandsInPass(
  const SecondaryCommandLists: array of TRHICommandList);
begin
  ExecuteCommands(SecondaryCommandLists);
end;

procedure TRHICommandList.InsertDebugMarker(const LabelName: String; const Color: TCastleColor);
begin
  { Subclasses override to emit single-point GPU debug markers (e.g. vkCmdInsertDebugUtilsLabelEXT). }
  if (LabelName = '') and (Color.X < -1e30) then ;
end;

{ TRHIDevice }

constructor TRHIDevice.Create;
begin
  inherited Create;
  FResources := TFPList.Create;
  FFrameIndex := 0;
end;

procedure TRHIDevice.ReleaseRemainingDeviceResources;
var
  I: Integer;
  Res: TRHIResource;
begin
  if FResources = nil then
    Exit;
  for I := FResources.Count - 1 downto 0 do
  begin
    Res := TRHIResource(FResources[I]);
    Res.ReleaseDeviceResources;
    Res.FDevice := nil;
  end;
  FResources.Clear;
end;

destructor TRHIDevice.Destroy;
begin
  ReleaseRemainingDeviceResources;
  FreeAndNil(FResources);
  if FRHIDevice = Self then
    FRHIDevice := nil;
  inherited;
end;

function TRHIDevice.BeginOffscreenFrame: TRHICommandList;
begin
  Result := BeginFrame;
end;

procedure TRHIDevice.EndOffscreenFrame;
begin
  EndFrameAndPresent;
end;

function TRHIDevice.GetOrCreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline;
begin
  Result := CreateComputePipeline(ADesc);
end;

function TRHIDevice.CreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline;
begin
  Result := nil;
end;

function TRHIDevice.CreateQueryPool(const ADesc: TRHIQueryPoolDesc): TRHIQueryPool;
begin
  Result := nil;
end;

function TRHIDevice.GrabTextureImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle; const DestImage: TCastleImage): Boolean;
var
  Grabbed: TRGBAlphaImage;
begin
  Result := False;
  if DestImage = nil then
    Exit;
  Grabbed := GrabTextureImage(SourceTexture, ReadArea);
  if Grabbed <> nil then
  try
    DestImage.Assign(Grabbed);
    Result := True;
  finally
    FreeAndNil(Grabbed);
  end;
end;

function TRHIDevice.GrabTextureSubresourceImage(const SourceTexture: TRHITexture;
  const AMipLevel, AArrayLayer: Cardinal): TRGBAlphaImage;
var
  MipW, MipH: Cardinal;
begin
  if SourceTexture = nil then
    Exit(nil);
  MipW := Max(Cardinal(1), SourceTexture.Desc.Width shr AMipLevel);
  MipH := Max(Cardinal(1), SourceTexture.Desc.Height shr AMipLevel);
  if (AMipLevel = 0) and (AArrayLayer = 0) then
    Result := GrabTextureImage(SourceTexture, Rectangle(0, 0, Integer(MipW), Integer(MipH)))
  else
    Result := nil;
end;

function TRHIDevice.GrabTextureFloatImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle): TRGBAlphaFloatImage;
var
  Grabbed: TRGBAlphaImage;
begin
  Result := nil;
  Grabbed := GrabTextureImage(SourceTexture, ReadArea);
  if Grabbed <> nil then
  try
    Result := TRGBAlphaFloatImage.Create(Grabbed.Width, Grabbed.Height);
    Result.Assign(Grabbed);
  finally
    FreeAndNil(Grabbed);
  end;
end;

function TRHIDevice.GetInformationString: String;
const
  BackendNames: array[TRHIBackendType] of String = ('OpenGL', 'OpenGLES', 'Vulkan');
begin
  Result :=
    BackendNames[FCapabilities.Backend] + ' RHI information:' + LineEnding +
    '  Device: ' + FCapabilities.DeviceName + LineEnding +
    '  Vendor ID: ' + IntToStr(FCapabilities.VendorId) + LineEnding +
    '  Max texture size: ' + IntToStr(FCapabilities.MaxTextureSize) + LineEnding +
    '  Max framebuffer MSAA samples: ' + IntToStr(FCapabilities.MaxFramebufferSampleCount) + LineEnding +
    '  Compute shaders: ' + BoolToStr(FCapabilities.SupportsComputeShaders, True) + LineEnding +
    '  Multi-draw indirect: ' + BoolToStr(FCapabilities.SupportsMultiDrawIndirect, True) + LineEnding +
    '  Draw indirect count: ' + BoolToStr(FCapabilities.SupportsDrawIndirectCount, True) + LineEnding +
    '  Bindless textures: ' + BoolToStr(FCapabilities.SupportsBindlessTextures, True) + LineEnding +
    '  Extended dynamic state: ' + BoolToStr(FCapabilities.SupportsExtendedDynamicState, True) + LineEnding +
    '  Synchronization2: ' + BoolToStr(FCapabilities.SupportsSynchronization2, True) + LineEnding +
    '  Timeline semaphores: ' + BoolToStr(FCapabilities.SupportsTimelineSemaphores, True);
end;

function TRHIDevice.CreateTransientCarrierTexture(const ADesc: TRHITextureDesc;
  const AMinCarrierBytes: UInt64): TRHITexture;
begin
  if AMinCarrierBytes = 0 then ;
  Result := CreateTexture(ADesc);
end;

function TRHIDevice.TryCreateAliasedTexture(const ADesc: TRHITextureDesc;
  const ACarrierTexture: TRHITexture): TRHITexture;
begin
  if (ACarrierTexture = nil) and (ADesc.Width = 0) then ;
  Result := nil;
end;

function TRHIDevice.RegisterBindlessTexture(const ATexture: TRHITexture;
  const ASampler: TRHISampler): Cardinal;
begin
  Result := 0;
end;

function TRHIDevice.RegisterOrUpdateBindlessMaterial(var AMaterialIndex: Cardinal;
  var AAlreadyRegistered: Boolean;
  const AEntryData; const AEntrySizeInBytes: Cardinal): Cardinal;
begin
  AAlreadyRegistered := True;
  Result := AMaterialIndex;
end;

function TRHIDevice.GetBindlessDescriptorSet: TRHIDescriptorSet;
begin
  Result := nil;
end;

function TRHIDevice.GetInformationStringSummary: String;
const
  BackendNames: array[TRHIBackendType] of String = ('OpenGL', 'OpenGLES', 'Vulkan');
begin
  if FCapabilities.DeviceName <> '' then
    Result := Format('%s (%s, modern rendering: True)', [
      BackendNames[FCapabilities.Backend],
      FCapabilities.DeviceName
    ])
  else
    Result := Format('%s (modern rendering: True)', [
      BackendNames[FCapabilities.Backend]
    ]);
end;

{ ---------------------------------------------------------------------------- }
{ TRHIRenderGraphPass                                                          }
{ ---------------------------------------------------------------------------- }

constructor TRHIRenderGraphPass.Create(const AName: String;
  const APassIndex: Integer; const APassType: TRHIRenderGraphPassType;
  const AHasSideEffects: Boolean);
begin
  inherited Create;
  FName := AName;
  FPassIndex := APassIndex;
  FPassType := APassType;
  FHasSideEffects := AHasSideEffects;
  FIsCulled := False;
  FExecutionOrder := -1;
end;

procedure TRHIRenderGraphPass.ReadTexture(
  const AHandle: TRHIRenderGraphResourceHandle;
  const AReadState: TRHIResourceState);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FTextureReads);
  SetLength(FTextureReads, Idx + 1);
  FillChar(FTextureReads[Idx], SizeOf(TRHIRenderGraphTextureAccess), 0);
  FTextureReads[Idx].Handle := AHandle;
  FTextureReads[Idx].State := AReadState;
end;

procedure TRHIRenderGraphPass.WriteColorAttachment(
  const AHandle: TRHIRenderGraphResourceHandle;
  const ALoadOp: TRHIAttachmentLoadOp;
  const AStoreOp: TRHIAttachmentStoreOp);
begin
  WriteColorAttachment(AHandle, ALoadOp, Vector4(0.0, 0.0, 0.0, 1.0), AStoreOp);
end;

procedure TRHIRenderGraphPass.WriteColorAttachment(
  const AHandle: TRHIRenderGraphResourceHandle;
  const ALoadOp: TRHIAttachmentLoadOp;
  const AClearColor: TCastleColor;
  const AStoreOp: TRHIAttachmentStoreOp);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FTextureWrites);
  SetLength(FTextureWrites, Idx + 1);
  FillChar(FTextureWrites[Idx], SizeOf(TRHIRenderGraphTextureAccess), 0);
  FTextureWrites[Idx].Handle := AHandle;
  FTextureWrites[Idx].State := rsColorAttachment;
  FTextureWrites[Idx].LoadOp := ALoadOp;
  FTextureWrites[Idx].StoreOp := AStoreOp;
  FTextureWrites[Idx].ClearColor := AClearColor;
  FTextureWrites[Idx].IsColorAttachment := True;
end;

procedure TRHIRenderGraphPass.WriteDepthAttachment(
  const AHandle: TRHIRenderGraphResourceHandle;
  const ALoadOp: TRHIAttachmentLoadOp;
  const AClearDepth: Single;
  const AStoreOp: TRHIAttachmentStoreOp);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FTextureWrites);
  SetLength(FTextureWrites, Idx + 1);
  FillChar(FTextureWrites[Idx], SizeOf(TRHIRenderGraphTextureAccess), 0);
  FTextureWrites[Idx].Handle := AHandle;
  FTextureWrites[Idx].State := rsDepthStencilWrite;
  FTextureWrites[Idx].LoadOp := ALoadOp;
  FTextureWrites[Idx].StoreOp := AStoreOp;
  FTextureWrites[Idx].ClearDepth := AClearDepth;
  FTextureWrites[Idx].IsDepthAttachment := True;
end;

procedure TRHIRenderGraphPass.WriteStorageTexture(
  const AHandle: TRHIRenderGraphResourceHandle;
  const AWriteState: TRHIResourceState);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FTextureWrites);
  SetLength(FTextureWrites, Idx + 1);
  FillChar(FTextureWrites[Idx], SizeOf(TRHIRenderGraphTextureAccess), 0);
  FTextureWrites[Idx].Handle := AHandle;
  FTextureWrites[Idx].State := AWriteState;
  FTextureWrites[Idx].StoreOp := soStore;
end;

procedure TRHIRenderGraphPass.ReadBuffer(
  const AHandle: TRHIRenderGraphResourceHandle);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FBufferReads);
  SetLength(FBufferReads, Idx + 1);
  FBufferReads[Idx] := AHandle;
end;

procedure TRHIRenderGraphPass.WriteBuffer(
  const AHandle: TRHIRenderGraphResourceHandle);
var
  Idx: Integer;
begin
  if AHandle < 0 then Exit;
  Idx := Length(FBufferWrites);
  SetLength(FBufferWrites, Idx + 1);
  FBufferWrites[Idx] := AHandle;
end;

function TRHIRenderGraphPass.PrePassBarriersCount: Integer;
begin
  Result := Length(FPrePassBarriers);
end;

function TRHIRenderGraphPass.GetPrePassBarrier(
  const AIndex: Integer): TRHIRenderGraphTextureBarrier;
begin
  if (AIndex >= 0) and (AIndex < Length(FPrePassBarriers)) then
    Result := FPrePassBarriers[AIndex]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

{ ---------------------------------------------------------------------------- }
{ TRHIRenderGraph                                                              }
{ ---------------------------------------------------------------------------- }

constructor TRHIRenderGraph.Create(const ADevice: TRHIDevice);
begin
  inherited Create;
  FDevice := ADevice;
  FPasses := TRHIRenderGraphPassList.Create(True);
  Reset;
end;

destructor TRHIRenderGraph.Destroy;
begin
  ReleaseTransientPool;
  FreeAndNil(FPasses);
  inherited;
end;

procedure TRHIRenderGraph.Reset;
begin
  FPasses.Clear;
  SetLength(FCompiledPasses, 0);
  SetLength(FVirtualTextures, 0);
  SetLength(FVirtualBuffers, 0);
  SetLength(FFinalExportBarriers, 0);
  FIsCompiled := False;
  FCulledPassCount := 0;
  FEmittedBarrierCount := 0;
  FAliasingBarrierCount := 0;
  FTransientTextureCount := 0;
  FAliasedTextureSavingsCount := 0;
  FMemoryAliasedTextureCount := 0;
  FAliasedMemorySavedBytes := 0;
  FDebugScopesEmittedCount := 0;
end;

procedure TRHIRenderGraph.ReleaseTransientPool;
var
  I, J: Integer;
begin
  for I := 0 to Length(FTransientPool) - 1 do
  begin
    { Destroy aliased VkImage wrappers before freeing the carrier allocation }
    for J := 0 to Length(FTransientPool[I].AliasedVariants) - 1 do
      FreeAndNil(FTransientPool[I].AliasedVariants[J].Texture);
    SetLength(FTransientPool[I].AliasedVariants, 0);
    FreeAndNil(FTransientPool[I].Texture);
    FTransientPool[I].LastActiveTexture := nil;
  end;
  SetLength(FTransientPool, 0);
end;

class function TRHIRenderGraph.AreTextureDescsCompatibleForAliasing(
  const A, B: TRHITextureDesc): Boolean;
begin
  Result :=
    (A.Dimension = B.Dimension) and
    (A.Width = B.Width) and
    (A.Height = B.Height) and
    (A.DepthOrArrayLayers = B.DepthOrArrayLayers) and
    (A.MipLevels = B.MipLevels) and
    (A.SampleCount = B.SampleCount) and
    (A.Format = B.Format) and
    (A.Usage = B.Usage);
end;

function TRHIRenderGraph.GetPassCount: Integer;
begin
  Result := FPasses.Count;
end;

function TRHIRenderGraph.GetCompiledPassCount: Integer;
begin
  Result := Length(FCompiledPasses);
end;

function TRHIRenderGraph.GetPhysicalTransientTextureCount: Integer;
begin
  Result := Length(FTransientPool);
end;

function TRHIRenderGraph.DeclareTransientTexture(const AName: String;
  const ADesc: TRHITextureDesc): TRHIRenderGraphResourceHandle;
var
  Idx: Integer;
begin
  FIsCompiled := False;
  Idx := Length(FVirtualTextures);
  SetLength(FVirtualTextures, Idx + 1);
  FVirtualTextures[Idx] := Default(TVirtualTextureEntry);
  FVirtualTextures[Idx].Name := AName;
  FVirtualTextures[Idx].Desc := ADesc;
  FVirtualTextures[Idx].Desc.DebugName := AName;
  FVirtualTextures[Idx].IsImported := False;
  FVirtualTextures[Idx].InitialState := rsUndefined;
  FVirtualTextures[Idx].FinalState := rsUndefined;
  FVirtualTextures[Idx].PhysicalTexture := nil;
  FVirtualTextures[Idx].PoolSlotIndex := -1;
  FVirtualTextures[Idx].FirstUseOrder := High(Integer);
  FVirtualTextures[Idx].LastUseOrder := -1;
  FVirtualTextures[Idx].LastProducerPass := -1;
  Inc(FTransientTextureCount);
  Result := Idx;
end;

function TRHIRenderGraph.ImportExternalTexture(const AName: String;
  const ATexture: TRHITexture;
  const AInitialState, AFinalState: TRHIResourceState): TRHIRenderGraphResourceHandle;
var
  Idx: Integer;
begin
  FIsCompiled := False;
  Idx := Length(FVirtualTextures);
  SetLength(FVirtualTextures, Idx + 1);
  FVirtualTextures[Idx] := Default(TVirtualTextureEntry);
  FVirtualTextures[Idx].Name := AName;
  if ATexture <> nil then
    FVirtualTextures[Idx].Desc := ATexture.Desc;
  FVirtualTextures[Idx].IsImported := True;
  if (AInitialState = rsUndefined) and (ATexture <> nil) then
    FVirtualTextures[Idx].InitialState := ATexture.CurrentState
  else
    FVirtualTextures[Idx].InitialState := AInitialState;
  FVirtualTextures[Idx].FinalState := AFinalState;
  FVirtualTextures[Idx].PhysicalTexture := ATexture;
  FVirtualTextures[Idx].PoolSlotIndex := -1;
  FVirtualTextures[Idx].FirstUseOrder := High(Integer);
  FVirtualTextures[Idx].LastUseOrder := -1;
  FVirtualTextures[Idx].LastProducerPass := -1;
  Result := Idx;
end;

function TRHIRenderGraph.ImportExternalBuffer(const AName: String;
  const ABuffer: TRHIBuffer): TRHIRenderGraphResourceHandle;
var
  Idx: Integer;
begin
  FIsCompiled := False;
  Idx := Length(FVirtualBuffers);
  SetLength(FVirtualBuffers, Idx + 1);
  FVirtualBuffers[Idx].Name := AName;
  FVirtualBuffers[Idx].PhysicalBuffer := ABuffer;
  FVirtualBuffers[Idx].LastProducerPass := -1;
  Result := Idx;
end;

function TRHIRenderGraph.AddPass(const AName: String;
  const APassType: TRHIRenderGraphPassType;
  const AHasSideEffects: Boolean): TRHIRenderGraphPass;
begin
  FIsCompiled := False;
  Result := TRHIRenderGraphPass.Create(AName, FPasses.Count, APassType, AHasSideEffects);
  FPasses.Add(Result);
end;

procedure TRHIRenderGraph.Compile;
var
  NPasses, P, Q, I, J, H, H2, V, OrderIdx, NextOrd, SlotIdx, ChosenSlot, BarrierIdx: Integer;
  Edges: array of array of Boolean;
  InDegree: array of Integer;
  IsLive: array of Boolean;
  Queue: array of Integer;
  QueueHead, QueueTail: Integer;
  Pass, OtherPass: TRHIRenderGraphPass;
  LastTexWriter: array of Integer;
  LastBufWriter: array of Integer;
  TrackedTexState: array of TRHIResourceState;
  PoolTexState: array of TRHIResourceState;
  SlotActiveTex: array of TRHITexture;
  AssignedTex, AliasTex, PrevActiveTex: TRHITexture;
  ChainEnd: Integer;
  CarrierBytes, CandBytes: UInt64;
  IsDepthDesc, SlotIsDepth, DidAliasSwitch: Boolean;
  CurState, NeededState: TRHIResourceState;
  AlreadyHasBarrier: Boolean;
begin
  NPasses := FPasses.Count;
  SetLength(FCompiledPasses, 0);
  SetLength(FFinalExportBarriers, 0);
  FCulledPassCount := 0;
  FEmittedBarrierCount := 0;
  FAliasingBarrierCount := 0;
  FAliasedTextureSavingsCount := 0;
  FMemoryAliasedTextureCount := 0;
  FAliasedMemorySavedBytes := 0;

  if NPasses = 0 then
  begin
    FIsCompiled := True;
    Exit;
  end;

  { 1. Build DAG dependency edges between passes (Edges[Producer, Consumer] = True) }
  SetLength(Edges, NPasses, NPasses);
  for P := 0 to NPasses - 1 do
    for Q := 0 to NPasses - 1 do
      Edges[P][Q] := False;

  SetLength(LastTexWriter, Length(FVirtualTextures));
  for I := 0 to Length(LastTexWriter) - 1 do
    LastTexWriter[I] := -1;

  SetLength(LastBufWriter, Length(FVirtualBuffers));
  for I := 0 to Length(LastBufWriter) - 1 do
    LastBufWriter[I] := -1;

  { First pass: resolve producer -> consumer edges regardless of declaration order
    by matching any pass Q that reads resource R to any pass P that writes resource R }
  for P := 0 to NPasses - 1 do
  begin
    Pass := FPasses[P];
    for I := 0 to Length(Pass.FTextureWrites) - 1 do
    begin
      H := Pass.FTextureWrites[I].Handle;
      if (H >= 0) and (H < Length(FVirtualTextures)) then
      begin
        { Any earlier writer of the same texture must precede this writer (WAW) }
        if (LastTexWriter[H] >= 0) and (LastTexWriter[H] <> P) then
          Edges[LastTexWriter[H]][P] := True;
        LastTexWriter[H] := P;

        { Any pass Q that reads H depends on writer P (unless Q also writes H before P) }
        for Q := 0 to NPasses - 1 do
          if Q <> P then
          begin
            OtherPass := FPasses[Q];
            for J := 0 to Length(OtherPass.FTextureReads) - 1 do
              if OtherPass.FTextureReads[J].Handle = H then
              begin
                { Avoid creating a 2-cycle if Q already precedes P }
                if not Edges[Q][P] then
                  Edges[P][Q] := True;
              end;
          end;
      end;
    end;

    for I := 0 to Length(Pass.FBufferWrites) - 1 do
    begin
      H := Pass.FBufferWrites[I];
      if (H >= 0) and (H < Length(FVirtualBuffers)) then
      begin
        if (LastBufWriter[H] >= 0) and (LastBufWriter[H] <> P) then
          Edges[LastBufWriter[H]][P] := True;
        LastBufWriter[H] := P;

        for Q := 0 to NPasses - 1 do
          if Q <> P then
          begin
            OtherPass := FPasses[Q];
            for J := 0 to Length(OtherPass.FBufferReads) - 1 do
              if OtherPass.FBufferReads[J] = H then
                if not Edges[Q][P] then
                  Edges[P][Q] := True;
          end;
      end;
    end;
  end;

  { 2. Backward Liveness Analysis (Dead-Pass Culling) }
  SetLength(IsLive, NPasses);
  SetLength(Queue, NPasses);
  QueueHead := 0;
  QueueTail := 0;

  for P := 0 to NPasses - 1 do
  begin
    Pass := FPasses[P];
    IsLive[P] := Pass.FHasSideEffects;

    if not IsLive[P] then
    begin
      for I := 0 to Length(Pass.FTextureWrites) - 1 do
      begin
        H := Pass.FTextureWrites[I].Handle;
        if (H >= 0) and (H < Length(FVirtualTextures)) and FVirtualTextures[H].IsImported then
        begin
          IsLive[P] := True;
          Break;
        end;
      end;
    end;

    if not IsLive[P] then
    begin
      for I := 0 to Length(Pass.FBufferWrites) - 1 do
      begin
        H := Pass.FBufferWrites[I];
        if (H >= 0) and (H < Length(FVirtualBuffers)) then
        begin
          IsLive[P] := True;
          Break;
        end;
      end;
    end;

    if IsLive[P] then
    begin
      Queue[QueueTail] := P;
      Inc(QueueTail);
    end;
  end;

  while QueueHead < QueueTail do
  begin
    Q := Queue[QueueHead];
    Inc(QueueHead);
    for P := 0 to NPasses - 1 do
      if Edges[P][Q] and (not IsLive[P]) then
      begin
        IsLive[P] := True;
        Queue[QueueTail] := P;
        Inc(QueueTail);
      end;
  end;

  for P := 0 to NPasses - 1 do
  begin
    FPasses[P].FIsCulled := not IsLive[P];
    FPasses[P].FExecutionOrder := -1;
    SetLength(FPasses[P].FPrePassBarriers, 0);
    if not IsLive[P] then
      Inc(FCulledPassCount);
  end;

  { 3. Topological Sort (Kahn's Algorithm) over live passes }
  SetLength(InDegree, NPasses);
  for P := 0 to NPasses - 1 do
  begin
    InDegree[P] := 0;
    if IsLive[P] then
      for Q := 0 to NPasses - 1 do
        if IsLive[Q] and Edges[Q][P] then
          Inc(InDegree[P]);
  end;

  QueueHead := 0;
  QueueTail := 0;
  for P := 0 to NPasses - 1 do
    if IsLive[P] and (InDegree[P] = 0) then
    begin
      Queue[QueueTail] := P;
      Inc(QueueTail);
    end;

  OrderIdx := 0;
  SetLength(FCompiledPasses, NPasses - FCulledPassCount);
  while QueueHead < QueueTail do
  begin
    P := Queue[QueueHead];
    Inc(QueueHead);
    FPasses[P].FExecutionOrder := OrderIdx;
    FCompiledPasses[OrderIdx] := FPasses[P];
    Inc(OrderIdx);

    for Q := 0 to NPasses - 1 do
      if IsLive[Q] and Edges[P][Q] then
      begin
        Dec(InDegree[Q]);
        if InDegree[Q] = 0 then
        begin
          Queue[QueueTail] := Q;
          Inc(QueueTail);
        end;
      end;
  end;

  if OrderIdx <> (NPasses - FCulledPassCount) then
    raise ERHIError.Create('TRHIRenderGraph.Compile: Dependency cycle detected in render graph passes.');

  { 4. Compute Transient Texture Lifetimes & Perform Physical Texture Aliasing }
  for H := 0 to Length(FVirtualTextures) - 1 do
  begin
    FVirtualTextures[H].FirstUseOrder := High(Integer);
    FVirtualTextures[H].LastUseOrder := -1;
    FVirtualTextures[H].PoolSlotIndex := -1;
    if not FVirtualTextures[H].IsImported then
      FVirtualTextures[H].PhysicalTexture := nil;
  end;

  for OrderIdx := 0 to Length(FCompiledPasses) - 1 do
  begin
    Pass := FCompiledPasses[OrderIdx];
    for I := 0 to Length(Pass.FTextureReads) - 1 do
    begin
      H := Pass.FTextureReads[I].Handle;
      if (H >= 0) and (H < Length(FVirtualTextures)) then
      begin
        if OrderIdx < FVirtualTextures[H].FirstUseOrder then
          FVirtualTextures[H].FirstUseOrder := OrderIdx;
        if OrderIdx > FVirtualTextures[H].LastUseOrder then
          FVirtualTextures[H].LastUseOrder := OrderIdx;
      end;
    end;
    for I := 0 to Length(Pass.FTextureWrites) - 1 do
    begin
      H := Pass.FTextureWrites[I].Handle;
      if (H >= 0) and (H < Length(FVirtualTextures)) then
      begin
        if OrderIdx < FVirtualTextures[H].FirstUseOrder then
          FVirtualTextures[H].FirstUseOrder := OrderIdx;
        if OrderIdx > FVirtualTextures[H].LastUseOrder then
          FVirtualTextures[H].LastUseOrder := OrderIdx;
      end;
    end;
  end;

  { Reset pool slot usage for this compilation }
  for SlotIdx := 0 to Length(FTransientPool) - 1 do
    FTransientPool[SlotIdx].LastAssignedOrder := -1;

  { Assign physical textures in increasing FirstUseOrder:
    1st pass per texture: exact descriptor reuse (zero new VkImage).
    2nd pass per texture: cross-format / cross-resolution / cross-usage physical
      VkDeviceMemory aliasing via TryCreateAliasedTexture (VK_IMAGE_CREATE_ALIAS_BIT).
    3rd fallback: allocate a new carrier slot sized to the max byte footprint of
      its greedy non-overlapping interval chain so downstream transients can alias it! }
  for OrderIdx := 0 to Length(FCompiledPasses) - 1 do
    for H := 0 to Length(FVirtualTextures) - 1 do
      if (not FVirtualTextures[H].IsImported) and
         (FVirtualTextures[H].FirstUseOrder = OrderIdx) and
         (FVirtualTextures[H].LastUseOrder >= OrderIdx) and
         (FVirtualTextures[H].PhysicalTexture = nil) then
      begin
        ChosenSlot := -1;
        AssignedTex := nil;

        { 4a. Prefer exact descriptor match (either carrier texture or cached aliased variant) }
        for SlotIdx := 0 to Length(FTransientPool) - 1 do
          if FTransientPool[SlotIdx].LastAssignedOrder < FVirtualTextures[H].FirstUseOrder then
          begin
            if AreTextureDescsCompatibleForAliasing(FTransientPool[SlotIdx].Desc, FVirtualTextures[H].Desc) then
            begin
              ChosenSlot := SlotIdx;
              AssignedTex := FTransientPool[SlotIdx].Texture;
              if FTransientPool[SlotIdx].LastAssignedOrder >= 0 then
              begin
                Inc(FAliasedTextureSavingsCount);
                Inc(FAliasedMemorySavedBytes, TRHITexture.EstimateDescriptorByteSize(FVirtualTextures[H].Desc));
              end;
              Break;
            end;
            for V := 0 to Length(FTransientPool[SlotIdx].AliasedVariants) - 1 do
              if AreTextureDescsCompatibleForAliasing(
                   FTransientPool[SlotIdx].AliasedVariants[V].Desc, FVirtualTextures[H].Desc) then
              begin
                ChosenSlot := SlotIdx;
                AssignedTex := FTransientPool[SlotIdx].AliasedVariants[V].Texture;
                if FTransientPool[SlotIdx].LastAssignedOrder >= 0 then
                begin
                  Inc(FAliasedTextureSavingsCount);
                  Inc(FMemoryAliasedTextureCount);
                  Inc(FAliasedMemorySavedBytes, TRHITexture.EstimateDescriptorByteSize(FVirtualTextures[H].Desc));
                end;
                Break;
              end;
            if ChosenSlot >= 0 then
              Break;
          end;

        { 4b. Cross-format / cross-resolution / cross-usage physical GPU memory aliasing }
        if (ChosenSlot < 0) and (FDevice <> nil) then
        begin
          for SlotIdx := 0 to Length(FTransientPool) - 1 do
            if (FTransientPool[SlotIdx].LastAssignedOrder < FVirtualTextures[H].FirstUseOrder) and
               (FTransientPool[SlotIdx].Texture <> nil) then
            begin
              AliasTex := FDevice.TryCreateAliasedTexture(
                FVirtualTextures[H].Desc, FTransientPool[SlotIdx].Texture);
              if AliasTex <> nil then
              begin
                V := Length(FTransientPool[SlotIdx].AliasedVariants);
                SetLength(FTransientPool[SlotIdx].AliasedVariants, V + 1);
                FTransientPool[SlotIdx].AliasedVariants[V].Desc := FVirtualTextures[H].Desc;
                FTransientPool[SlotIdx].AliasedVariants[V].Texture := AliasTex;
                ChosenSlot := SlotIdx;
                AssignedTex := AliasTex;
                if FTransientPool[SlotIdx].LastAssignedOrder >= 0 then
                begin
                  Inc(FAliasedTextureSavingsCount);
                  Inc(FMemoryAliasedTextureCount);
                  Inc(FAliasedMemorySavedBytes, TRHITexture.EstimateDescriptorByteSize(FVirtualTextures[H].Desc));
                end;
                Break;
              end;
            end;
        end;

        { 4c. Allocate a new carrier slot sized to cover its greedy future non-overlapping chain }
        if ChosenSlot < 0 then
        begin
          CarrierBytes := TRHITexture.EstimateDescriptorByteSize(FVirtualTextures[H].Desc) + 65536;
          IsDepthDesc := tuDepthStencil in FVirtualTextures[H].Desc.Usage;
          ChainEnd := FVirtualTextures[H].LastUseOrder;
          for NextOrd := ChainEnd + 1 to Length(FCompiledPasses) - 1 do
            for H2 := 0 to Length(FVirtualTextures) - 1 do
              if (not FVirtualTextures[H2].IsImported) and
                 (FVirtualTextures[H2].PhysicalTexture = nil) and
                 (FVirtualTextures[H2].FirstUseOrder = NextOrd) and
                 (FVirtualTextures[H2].FirstUseOrder > ChainEnd) and
                 (FVirtualTextures[H2].LastUseOrder >= NextOrd) then
              begin
                SlotIsDepth := tuDepthStencil in FVirtualTextures[H2].Desc.Usage;
                if SlotIsDepth = IsDepthDesc then
                begin
                  CandBytes := TRHITexture.EstimateDescriptorByteSize(FVirtualTextures[H2].Desc) + 65536;
                  if CandBytes > CarrierBytes then
                    CarrierBytes := CandBytes;
                  ChainEnd := FVirtualTextures[H2].LastUseOrder;
                  Break;
                end;
              end;

          ChosenSlot := Length(FTransientPool);
          SetLength(FTransientPool, ChosenSlot + 1);
          FTransientPool[ChosenSlot] := Default(TTransientPoolSlot);
          FTransientPool[ChosenSlot].Desc := FVirtualTextures[H].Desc;
          FTransientPool[ChosenSlot].CarrierCapacityBytes := CarrierBytes;
          if FDevice <> nil then
            FTransientPool[ChosenSlot].Texture := FDevice.CreateTransientCarrierTexture(
              FVirtualTextures[H].Desc, CarrierBytes)
          else
            FTransientPool[ChosenSlot].Texture := nil;
          AssignedTex := FTransientPool[ChosenSlot].Texture;
        end;

        FTransientPool[ChosenSlot].LastAssignedOrder := FVirtualTextures[H].LastUseOrder;
        FVirtualTextures[H].PoolSlotIndex := ChosenSlot;
        FVirtualTextures[H].PhysicalTexture := AssignedTex;
      end;

  { 5. Hoist Pre-Pass Resource State Transitions & Physical Memory Aliasing Barriers }
  SetLength(TrackedTexState, Length(FVirtualTextures));
  for H := 0 to Length(FVirtualTextures) - 1 do
    TrackedTexState[H] := FVirtualTextures[H].InitialState;

  SetLength(PoolTexState, Length(FTransientPool));
  SetLength(SlotActiveTex, Length(FTransientPool));
  for SlotIdx := 0 to Length(FTransientPool) - 1 do
  begin
    SlotActiveTex[SlotIdx] := FTransientPool[SlotIdx].LastActiveTexture;
    if SlotActiveTex[SlotIdx] <> nil then
      PoolTexState[SlotIdx] := SlotActiveTex[SlotIdx].CurrentState
    else if FTransientPool[SlotIdx].Texture <> nil then
      PoolTexState[SlotIdx] := FTransientPool[SlotIdx].Texture.CurrentState
    else
      PoolTexState[SlotIdx] := rsUndefined;
  end;

  for OrderIdx := 0 to Length(FCompiledPasses) - 1 do
  begin
    Pass := FCompiledPasses[OrderIdx];
    SetLength(Pass.FPrePassBarriers, 0);

    { Check texture reads }
    for I := 0 to Length(Pass.FTextureReads) - 1 do
    begin
      H := Pass.FTextureReads[I].Handle;
      NeededState := Pass.FTextureReads[I].State;
      if (H >= 0) and (H < Length(FVirtualTextures)) then
      begin
        SlotIdx := FVirtualTextures[H].PoolSlotIndex;
        DidAliasSwitch := False;
        PrevActiveTex := nil;
        if (not FVirtualTextures[H].IsImported) and (SlotIdx >= 0) then
        begin
          AssignedTex := FVirtualTextures[H].PhysicalTexture;
          if (SlotActiveTex[SlotIdx] <> nil) and (SlotActiveTex[SlotIdx] <> AssignedTex) then
          begin
            DidAliasSwitch := True;
            PrevActiveTex := SlotActiveTex[SlotIdx];
            CurState := rsUndefined;
          end else
            CurState := PoolTexState[SlotIdx];
          SlotActiveTex[SlotIdx] := AssignedTex;
        end else
          CurState := TrackedTexState[H];

        if DidAliasSwitch or (CurState <> NeededState) then
        begin
          AlreadyHasBarrier := False;
          for J := 0 to Length(Pass.FPrePassBarriers) - 1 do
            if Pass.FPrePassBarriers[J].TextureHandle = H then
            begin
              AlreadyHasBarrier := True;
              Break;
            end;
          if not AlreadyHasBarrier then
          begin
            BarrierIdx := Length(Pass.FPrePassBarriers);
            SetLength(Pass.FPrePassBarriers, BarrierIdx + 1);
            FillChar(Pass.FPrePassBarriers[BarrierIdx], SizeOf(TRHIRenderGraphTextureBarrier), 0);
            Pass.FPrePassBarriers[BarrierIdx].TextureHandle := H;
            Pass.FPrePassBarriers[BarrierIdx].PreviousAliasedTexture := PrevActiveTex;
            Pass.FPrePassBarriers[BarrierIdx].IsAliasingHandoff := DidAliasSwitch;
            Pass.FPrePassBarriers[BarrierIdx].BeforeState := CurState;
            Pass.FPrePassBarriers[BarrierIdx].AfterState := NeededState;
            Inc(FEmittedBarrierCount);
            if DidAliasSwitch then
              Inc(FAliasingBarrierCount);
          end;
          TrackedTexState[H] := NeededState;
          if (not FVirtualTextures[H].IsImported) and (SlotIdx >= 0) then
            PoolTexState[SlotIdx] := NeededState;
        end;
      end;
    end;

    { Check texture writes }
    for I := 0 to Length(Pass.FTextureWrites) - 1 do
    begin
      H := Pass.FTextureWrites[I].Handle;
      NeededState := Pass.FTextureWrites[I].State;
      if (H >= 0) and (H < Length(FVirtualTextures)) then
      begin
        SlotIdx := FVirtualTextures[H].PoolSlotIndex;
        DidAliasSwitch := False;
        PrevActiveTex := nil;
        if (not FVirtualTextures[H].IsImported) and (SlotIdx >= 0) then
        begin
          AssignedTex := FVirtualTextures[H].PhysicalTexture;
          if (SlotActiveTex[SlotIdx] <> nil) and (SlotActiveTex[SlotIdx] <> AssignedTex) then
          begin
            DidAliasSwitch := True;
            PrevActiveTex := SlotActiveTex[SlotIdx];
            CurState := rsUndefined;
          end else
            CurState := PoolTexState[SlotIdx];
          SlotActiveTex[SlotIdx] := AssignedTex;
        end else
          CurState := TrackedTexState[H];

        if DidAliasSwitch or (CurState <> NeededState) then
        begin
          AlreadyHasBarrier := False;
          for J := 0 to Length(Pass.FPrePassBarriers) - 1 do
            if Pass.FPrePassBarriers[J].TextureHandle = H then
            begin
              Pass.FPrePassBarriers[J].AfterState := NeededState;
              if DidAliasSwitch then
              begin
                Pass.FPrePassBarriers[J].IsAliasingHandoff := True;
                Pass.FPrePassBarriers[J].PreviousAliasedTexture := PrevActiveTex;
                Pass.FPrePassBarriers[J].BeforeState := rsUndefined;
                Inc(FAliasingBarrierCount);
              end;
              AlreadyHasBarrier := True;
              Break;
            end;
          if not AlreadyHasBarrier then
          begin
            BarrierIdx := Length(Pass.FPrePassBarriers);
            SetLength(Pass.FPrePassBarriers, BarrierIdx + 1);
            FillChar(Pass.FPrePassBarriers[BarrierIdx], SizeOf(TRHIRenderGraphTextureBarrier), 0);
            Pass.FPrePassBarriers[BarrierIdx].TextureHandle := H;
            Pass.FPrePassBarriers[BarrierIdx].PreviousAliasedTexture := PrevActiveTex;
            Pass.FPrePassBarriers[BarrierIdx].IsAliasingHandoff := DidAliasSwitch;
            Pass.FPrePassBarriers[BarrierIdx].BeforeState := CurState;
            Pass.FPrePassBarriers[BarrierIdx].AfterState := NeededState;
            Inc(FEmittedBarrierCount);
            if DidAliasSwitch then
              Inc(FAliasingBarrierCount);
          end;
          TrackedTexState[H] := NeededState;
          if (not FVirtualTextures[H].IsImported) and (SlotIdx >= 0) then
            PoolTexState[SlotIdx] := NeededState;
        end;
      end;
    end;
  end;

  for SlotIdx := 0 to Length(FTransientPool) - 1 do
    FTransientPool[SlotIdx].LastActiveTexture := SlotActiveTex[SlotIdx];

  { Check FinalState transitions for imported textures }
  for H := 0 to Length(FVirtualTextures) - 1 do
    if FVirtualTextures[H].IsImported and
       (FVirtualTextures[H].FinalState <> rsUndefined) and
       (TrackedTexState[H] <> FVirtualTextures[H].FinalState) then
    begin
      BarrierIdx := Length(FFinalExportBarriers);
      SetLength(FFinalExportBarriers, BarrierIdx + 1);
      FFinalExportBarriers[BarrierIdx].TextureHandle := H;
      FFinalExportBarriers[BarrierIdx].BeforeState := TrackedTexState[H];
      FFinalExportBarriers[BarrierIdx].AfterState := FVirtualTextures[H].FinalState;
      Inc(FEmittedBarrierCount);
    end;

  FIsCompiled := True;
end;

procedure TRHIRenderGraph.Execute(const ACmd: TRHICommandList);
var
  OrderIdx, I, ColorCount: Integer;
  Pass: TRHIRenderGraphPass;
  PhysTex: TRHITexture;
  RenderInfo: TRHIRenderingInfo;
  TargetW, TargetH: Cardinal;
  HasAttachments: Boolean;
  ScopeColor: TCastleColor;
begin
  if not FIsCompiled then
    Compile;

  for OrderIdx := 0 to Length(FCompiledPasses) - 1 do
  begin
    Pass := FCompiledPasses[OrderIdx];

    if ACmd <> nil then
    begin
      case Pass.FPassType of
        rgptGraphics: ScopeColor := Vector4(0.22, 0.62, 0.96, 1.0);
        rgptCompute:  ScopeColor := Vector4(0.95, 0.65, 0.18, 1.0);
      else
        ScopeColor := Vector4(0.22, 0.82, 0.48, 1.0);
      end;
      ACmd.PushDebugGroup(Pass.FName, ScopeColor);
      Inc(FDebugScopesEmittedCount);
    end;
    try
      { 1. Emit hoisted pre-pass physical memory aliasing & layout transition barriers }
      if ACmd <> nil then
        for I := 0 to Length(Pass.FPrePassBarriers) - 1 do
        begin
          PhysTex := ResolvePhysicalTexture(Pass.FPrePassBarriers[I].TextureHandle);
          if PhysTex <> nil then
          begin
            if Pass.FPrePassBarriers[I].IsAliasingHandoff then
            begin
              ACmd.EmitAliasingBarrier(Pass.FPrePassBarriers[I].PreviousAliasedTexture, PhysTex);
              { Per Vulkan spec Memory Aliasing rules, activating an aliased image discards
                prior metadata and transitions from VK_IMAGE_LAYOUT_UNDEFINED }
              PhysTex.CurrentState := rsUndefined;
            end;
            ACmd.TransitionTexture(PhysTex, Pass.FPrePassBarriers[I].AfterState);
          end;
        end;

      { 2. Automatically wrap graphics passes with attachments in BeginRendering/EndRendering }
      HasAttachments := False;
      if (ACmd <> nil) and (Pass.FPassType = rgptGraphics) and (Length(Pass.FTextureWrites) > 0) then
      begin
        RenderInfo := Default(TRHIRenderingInfo);
        ColorCount := 0;
        TargetW := 0;
        TargetH := 0;

        for I := 0 to Length(Pass.FTextureWrites) - 1 do
        begin
          PhysTex := ResolvePhysicalTexture(Pass.FTextureWrites[I].Handle);
          if (PhysTex <> nil) and (TargetW = 0) then
          begin
            TargetW := PhysTex.Desc.Width;
            TargetH := PhysTex.Desc.Height;
          end;

          if Pass.FTextureWrites[I].IsColorAttachment then
          begin
            SetLength(RenderInfo.ColorAttachments, ColorCount + 1);
            FillChar(RenderInfo.ColorAttachments[ColorCount], SizeOf(TRHIRenderingAttachment), 0);
            RenderInfo.ColorAttachments[ColorCount].Texture := PhysTex;
            RenderInfo.ColorAttachments[ColorCount].LoadOp := Pass.FTextureWrites[I].LoadOp;
            RenderInfo.ColorAttachments[ColorCount].StoreOp := Pass.FTextureWrites[I].StoreOp;
            RenderInfo.ColorAttachments[ColorCount].ClearColor := Pass.FTextureWrites[I].ClearColor;
            Inc(ColorCount);
            HasAttachments := True;
          end else
          if Pass.FTextureWrites[I].IsDepthAttachment then
          begin
            RenderInfo.HasDepthAttachment := True;
            FillChar(RenderInfo.DepthAttachment, SizeOf(TRHIRenderingAttachment), 0);
            RenderInfo.DepthAttachment.Texture := PhysTex;
            RenderInfo.DepthAttachment.LoadOp := Pass.FTextureWrites[I].LoadOp;
            RenderInfo.DepthAttachment.StoreOp := Pass.FTextureWrites[I].StoreOp;
            RenderInfo.DepthAttachment.ClearDepth := Pass.FTextureWrites[I].ClearDepth;
            HasAttachments := True;
          end;
        end;

        if HasAttachments then
        begin
          if (TargetW = 0) or (TargetH = 0) then
          begin
            TargetW := 1;
            TargetH := 1;
          end;
          RenderInfo.RenderArea := Rectangle(0, 0, TargetW, TargetH);
          ACmd.BeginRendering(RenderInfo);
          ACmd.SetViewport(FloatRectangle(0, 0, TargetW, TargetH));
          ACmd.SetScissor(RenderInfo.RenderArea);
        end;
      end;

      { 3. Execute pass callback }
      if Assigned(Pass.FOnExecute) then
        Pass.FOnExecute(Self, Pass, ACmd);
      if Assigned(Pass.FOnExecuteProc) then
        Pass.FOnExecuteProc(Self, Pass, ACmd);

      if (ACmd <> nil) and HasAttachments then
        ACmd.EndRendering;
    finally
      if ACmd <> nil then
        ACmd.PopDebugGroup;
    end;
  end;

  { 4. Emit final export barriers }
  if ACmd <> nil then
    for I := 0 to Length(FFinalExportBarriers) - 1 do
    begin
      PhysTex := ResolvePhysicalTexture(FFinalExportBarriers[I].TextureHandle);
      if PhysTex <> nil then
        ACmd.TransitionTexture(PhysTex, FFinalExportBarriers[I].AfterState);
    end;

  Inc(FExecutionCount);
end;

function TRHIRenderGraph.ResolvePhysicalTexture(
  const AHandle: TRHIRenderGraphResourceHandle): TRHITexture;
begin
  if (AHandle >= 0) and (AHandle < Length(FVirtualTextures)) then
    Result := FVirtualTextures[AHandle].PhysicalTexture
  else
    Result := nil;
end;

function TRHIRenderGraph.ResolvePhysicalBuffer(
  const AHandle: TRHIRenderGraphResourceHandle): TRHIBuffer;
begin
  if (AHandle >= 0) and (AHandle < Length(FVirtualBuffers)) then
    Result := FVirtualBuffers[AHandle].PhysicalBuffer
  else
    Result := nil;
end;

function TRHIRenderGraph.GetCompiledPass(const AOrderIndex: Integer): TRHIRenderGraphPass;
begin
  if (AOrderIndex >= 0) and (AOrderIndex < Length(FCompiledPasses)) then
    Result := FCompiledPasses[AOrderIndex]
  else
    Result := nil;
end;

function EncodedImageToRHIPixelFormat(const AImage: TEncodedImage): TRHIPixelFormat;
begin
  if AImage = nil then
    Exit(pfRGBA8_UNorm);

  if (AImage is TRGBAlphaFloatImage) or
     (AImage is TRGBFloatImage) or
     (AImage is TGrayscaleFloatImage) or
     (AImage is TGrayscaleAlphaFloatImage) then
    Exit(pfRGBA32_Float);

  if AImage is TGPUCompressedImage then
  begin
    case TGPUCompressedImage(AImage).Compression of
      tcDxt1_RGB, tcDxt1_RGBA:
        Result := pfBC1_RGBA_UNorm;
      tcDxt3, tcDxt5:
        Result := pfBC3_RGBA_UNorm;
      tcETC1:
        Result := pfETC2_RGB8_UNorm;
      tcASTC_4x4_RGBA .. tcASTC_12x12_RGBA:
        Result := pfASTC_4x4_UNorm;
      tcASTC_4x4_SRGB8_ALPHA8 .. tcASTC_12x12_SRGB8_ALPHA8:
        Result := pfASTC_4x4_sRGB;
    else
      Result := pfRGBA8_UNorm;
    end;
    Exit;
  end;

  Result := pfRGBA8_UNorm;
end;

function EncodedImageToRHIGpuBytes(const AImage: TEncodedImage;
  const AFormat: TRHIPixelFormat): Int64;
var
  W, H, D: Int64;
begin
  if AImage = nil then
    Exit(0);

  if (AImage is TGPUCompressedImage) and
     (AFormat in [pfBC1_RGBA_UNorm, pfBC1_RGBA_sRGB,
                  pfBC3_RGBA_UNorm, pfBC3_RGBA_sRGB,
                  pfBC5_RG_UNorm, pfBC7_RGBA_UNorm, pfBC7_RGBA_sRGB,
                  pfASTC_4x4_UNorm, pfASTC_4x4_sRGB,
                  pfETC2_RGB8_UNorm, pfETC2_RGBA8_UNorm]) then
    Exit(Int64(TGPUCompressedImage(AImage).Size));

  W := Int64(AImage.Width);
  if W < 1 then W := 1;
  H := Int64(AImage.Height);
  if H < 1 then H := 1;
  D := Int64(AImage.Depth);
  if D < 1 then D := 1;

  case AFormat of
    pfRGBA32_Float:
      Result := W * H * D * 16;
    pfRGBA16_Float:
      Result := W * H * D * 8;
  else
    Result := W * H * D * 4;
  end;
end;

end.
