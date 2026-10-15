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
  SysUtils, Classes, Generics.Collections,
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
  TRHICommandList = class;

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
    buTransferDst
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
    rsColorAttachment,
    rsDepthStencilWrite,
    rsDepthStencilRead,
    rsPresent
  );

  TRHIShaderStage = (
    ssVertex,
    ssFragment,
    ssGeometry,
    ssCompute
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
    bfOneMinusDstAlpha
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

  { -------------------------------------------------------------------------- }
  { Descriptors & Creation Records                                             }
  { -------------------------------------------------------------------------- }

  TRHIBufferDesc = record
    Size: PtrUInt;
    Usage: TRHIBufferUsageFlags;
    MemoryUsage: TRHIMemoryUsage;
    DebugName: String;
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
    dtStorageImage
  );

  TRHIDescriptorBindingDesc = record
    Binding: Cardinal;
    DescriptorType: TRHIDescriptorType;
    Count: Cardinal;
    StageFlags: TRHIShaderStageFlags;
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
    class function DefaultOpaque: TRHIBlendStateDesc; static;
    class function DefaultAlphaBlend: TRHIBlendStateDesc; static;
  end;

  TRHIRasterizerStateDesc = record
    PolygonMode: TRHIPolygonMode;
    CullMode: TRHICullMode;
    FrontFace: TRHIFrontFace;
    DepthBiasEnable: Boolean;
    DepthClampEnable: Boolean;
    class function DefaultCullBack: TRHIRasterizerStateDesc; static;
  end;

  TRHIDepthStencilStateDesc = record
    DepthTestEnable: Boolean;
    DepthWriteEnable: Boolean;
    DepthCompareOp: TRHICompareOp;
    StencilTestEnable: Boolean;
    class function DefaultDepthTestWrite: TRHIDepthStencilStateDesc; static;
    class function DefaultDepthReadOnly: TRHIDepthStencilStateDesc; static;
    class function DefaultDisabled: TRHIDepthStencilStateDesc; static;
  end;

  TRHIGraphicsPipelineDesc = record
    VertexShader: TRHIShaderModule;
    FragmentShader: TRHIShaderModule;
    GeometryShader: TRHIShaderModule; // Optional (nil if unused)
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
  end;

  TRHIRenderingInfo = record
    RenderArea: TRectangle;
    ColorAttachments: array of TRHIRenderingAttachment;
    HasDepthAttachment: Boolean;
    DepthAttachment: TRHIRenderingAttachment;
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
    SupportsBCCompression: Boolean;
    SupportsASTCCompression: Boolean;
    SupportsETC2Compression: Boolean;
    SupportsComputeShaders: Boolean;
    SupportsMultiDrawIndirect: Boolean;
  end;

  { -------------------------------------------------------------------------- }
  { Resource Classes                                                           }
  { -------------------------------------------------------------------------- }

  { Base class for all GPU resources owned by a TRHIDevice. }
  TRHIResource = class abstract
  private
    FDevice: TRHIDevice;
    FDebugName: String;
  public
    constructor Create(const ADevice: TRHIDevice; const ADebugName: String = '');
    property Device: TRHIDevice read FDevice;
    property DebugName: String read FDebugName;
  end;

  { GPU Buffer (Vertex, Index, Uniform, Storage, or Staging). }
  TRHIBuffer = class abstract(TRHIResource)
  protected
    FDesc: TRHIBufferDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIBufferDesc);
    { Maps CPU-visible memory (muCpuToGpu or muGpuToCpu). }
    function Map: Pointer; virtual; abstract;
    procedure Unmap; virtual; abstract;
    { Convenience helper to upload data. Uses staging buffer if muGpuOnly. }
    procedure UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt = 0); virtual; abstract;
    property Desc: TRHIBufferDesc read FDesc;
  end;

  { GPU Texture (2D, 3D, Cube, Array, Render Target). }
  TRHITexture = class abstract(TRHIResource)
  protected
    FDesc: TRHITextureDesc;
    FCurrentState: TRHIResourceState;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHITextureDesc);
    { Uploads a TEncodedImage (TCastleImage or TGPUCompressedImage) into a mip/layer. }
    procedure UploadImage(const AImage: TEncodedImage;
      const AMipLevel: Cardinal = 0; const AArrayLayer: Cardinal = 0); virtual; abstract;
    property Desc: TRHITextureDesc read FDesc;
    property CurrentState: TRHIResourceState read FCurrentState write FCurrentState;
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
    { Commits pending binding updates to the underlying descriptor set. }
    procedure Update; virtual; abstract;
  end;

  { Baked Graphics Pipeline State Object (PSO). }
  TRHIPipeline = class abstract(TRHIResource)
  protected
    FDesc: TRHIGraphicsPipelineDesc;
  public
    constructor Create(const ADevice: TRHIDevice; const ADesc: TRHIGraphicsPipelineDesc);
    property Desc: TRHIGraphicsPipelineDesc read FDesc;
  end;

  { -------------------------------------------------------------------------- }
  { Command List (Command Buffer Recording)                                    }
  { -------------------------------------------------------------------------- }

  { Records rendering, compute, and transfer commands for execution on the GPU.
    In Vulkan, wraps VkCommandBuffer. In OpenGL, executes immediately or via a lightweight queue. }
  TRHICommandList = class abstract
  public
    procedure BeginRecording; virtual; abstract;
    procedure EndRecording; virtual; abstract;

    { Resource Barriers & Transitions }
    procedure TransitionTexture(const Texture: TRHITexture;
      const NewState: TRHIResourceState); virtual; abstract;
    procedure GenerateMipmaps(const Texture: TRHITexture); virtual; abstract;

    { Dynamic Rendering Pass Delimiters }
    procedure BeginRendering(const Info: TRHIRenderingInfo); virtual; abstract;
    procedure EndRendering; virtual; abstract;

    { Dynamic Viewport & Scissor State }
    procedure SetViewport(const ViewportRect: TFloatRectangle;
      const MinDepth: Single = 0.0; const MaxDepth: Single = 1.0); virtual; abstract;
    procedure SetScissor(const ScissorRect: TRectangle); virtual; abstract;
    procedure SetDepthBias(const ConstantFactor, SlopeFactor: Single); virtual; abstract;

    { Pipeline & Resource Binding }
    procedure BindPipeline(const Pipeline: TRHIPipeline); virtual; abstract;
    procedure BindDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); virtual; abstract;
    procedure PushConstants(const StageFlags: TRHIShaderStageFlags;
      const Offset, Size: Cardinal; const Data); virtual; abstract;

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
    procedure DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); virtual; abstract;

    { Debug Markers (RenderDoc / NSight integration) }
    procedure PushDebugGroup(const LabelName: String; const Color: TCastleColor); virtual; abstract;
    procedure PopDebugGroup; virtual; abstract;
  end;

  { -------------------------------------------------------------------------- }
  { Render Device (Factory & Frame Lifecycle)                                  }
  { -------------------------------------------------------------------------- }

  { Main entry point for the RHI backend. Manages GPU device lifecycles,
    swapchain presentation, frames-in-flight synchronization, and resource creation. }
  TRHIDevice = class abstract
  protected
    FCapabilities: TRHIDeviceCapabilities;
    FFrameIndex: Cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Frame Lifecycle & Swapchain }
    function BeginFrame: TRHICommandList; virtual; abstract;
    procedure EndFrameAndPresent; virtual; abstract;
    procedure ResizeSwapchain(const NewWidth, NewHeight: Cardinal); virtual; abstract;
    procedure WaitIdle; virtual; abstract;

    { Returns the backbuffer color and depth targets for the current frame in flight }
    function GetSwapchainColorTexture: TRHITexture; virtual; abstract;
    function GetSwapchainDepthTexture: TRHITexture; virtual; abstract;

    { Resource Creation Factory Methods }
    function CreateBuffer(const ADesc: TRHIBufferDesc;
      const InitialData: Pointer = nil): TRHIBuffer; virtual; abstract;
    function CreateTexture(const ADesc: TRHITextureDesc): TRHITexture; virtual; abstract;
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

    { Readback current swapchain or texture into a CPU TCastleImage (for TCastleWindow.SaveScreen). }
    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaImage; virtual; abstract;

    { Pipeline Cache persistence (loads/saves VkPipelineCache to disk) }
    procedure LoadPipelineCache(const CacheFileUrl: String); virtual; abstract;
    procedure SavePipelineCache(const CacheFileUrl: String); virtual; abstract;

    property Capabilities: TRHIDeviceCapabilities read FCapabilities;
    { Current frame-in-flight index (0 .. MaxFramesInFlight - 1) }
    property FrameIndex: Cardinal read FFrameIndex;
  end;

const
  { Maximum number of frames simultaneously processed by CPU and GPU. }
  MaxFramesInFlight = 2;

{ Global active RHI device instance managed by CastleWindow / TCastleViewport. }
function RHIDevice: TRHIDevice;
procedure SetRHIDevice(const ADevice: TRHIDevice);

implementation

var
  FRHIDevice: TRHIDevice = nil;

function RHIDevice: TRHIDevice;
begin
  Result := FRHIDevice;
end;

procedure SetRHIDevice(const ADevice: TRHIDevice);
begin
  FRHIDevice := ADevice;
end;

{ TRHITextureDesc }

class function TRHITextureDesc.Create2D(const AWidth, AHeight: Cardinal;
  const AFormat: TRHIPixelFormat; const AUsage: TRHITextureUsageFlags;
  const AMipLevels: Cardinal): TRHITextureDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Dimension := td2D;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.DepthOrArrayLayers := 1;
  Result.MipLevels := AMipLevels;
  Result.SampleCount := 1;
  Result.Format := AFormat;
  Result.Usage := AUsage;
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

{ TRHIDepthStencilStateDesc }

class function TRHIDepthStencilStateDesc.DefaultDepthTestWrite: TRHIDepthStencilStateDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.DepthTestEnable := True;
  Result.DepthWriteEnable := True;
  Result.DepthCompareOp := coLessOrEqual;
  Result.StencilTestEnable := False;
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
end;

{ TRHIResource }

constructor TRHIResource.Create(const ADevice: TRHIDevice; const ADebugName: String);
begin
  inherited Create;
  FDevice := ADevice;
  FDebugName := ADebugName;
end;

{ TRHIBuffer }

constructor TRHIBuffer.Create(const ADevice: TRHIDevice; const ADesc: TRHIBufferDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

{ TRHITexture }

constructor TRHITexture.Create(const ADevice: TRHIDevice; const ADesc: TRHITextureDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
  FCurrentState := rsUndefined;
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

{ TRHIPipeline }

constructor TRHIPipeline.Create(const ADevice: TRHIDevice; const ADesc: TRHIGraphicsPipelineDesc);
begin
  inherited Create(ADevice, ADesc.DebugName);
  FDesc := ADesc;
end;

{ TRHIDevice }

constructor TRHIDevice.Create;
begin
  inherited Create;
  FFrameIndex := 0;
end;

destructor TRHIDevice.Destroy;
begin
  if FRHIDevice = Self then
    FRHIDevice := nil;
  inherited;
end;

end.
