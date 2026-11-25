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

{ OpenGL 3.3–4.6 and OpenGL ES 3.0–3.2 implementation of CastleInternalRHI.
  Wraps Castle Game Engine's OpenGL/OpenGLES context and translates explicit RHI
  Graphics & Compute Pipelines, UBO/SSBO/Sampler/Storage-Image Descriptor Sets,
  Indirect Multi-Draw (including Indirect-Count), Hardware Query Pools
  (Timestamps & Occlusion), and TRHIRenderGraph passes into OpenGL execution,
  alongside a deterministic headless simulation mode for context-free unit tests. }
unit CastleInternalRHI_OpenGL;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Math, Generics.Collections,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleLog,
  CastleGLUtils, CastleGLImages, CastleRenderOptions, CastleApplicationProperties,
  CastleTextureImages,
  CastleInternalRHI;

const
  { Stride between Descriptor Set indices in OpenGL binding slots
    (Set 0 = 0..7, Set 1 = 8..15, Set 2 = 16..23) }
  GL_DESCRIPTOR_SET_SLOT_STRIDE = 8;
  { Reserved Uniform Buffer binding slot for emulating Vulkan Push Constants in OpenGL }
  GL_PUSH_CONSTANT_UBO_BINDING  = 24;

type
  TOpenGLRHIDevice = class;
  TOpenGLRHICommandList = class;

  { -------------------------------------------------------------------------- }
  { Concrete OpenGL Resource Classes                                           }
  { -------------------------------------------------------------------------- }

  TOpenGLRHIBuffer = class(TRHIBuffer)
  private
    FHandle: TGLuint;
    FTarget: TGLenum;
    FMappedFallback: Pointer;
    FIsMappedForWrite: Boolean;
    FMappedUploadedViaSubData: Boolean;
    FHostShadow: TBytes;
    function OpenGLDevice: TOpenGLRHIDevice;
    procedure SyncMappedToGL(const AOffset: PtrUInt = 0; const ASize: PtrUInt = 0);
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHIBufferDesc;
      const InitialData: Pointer);
    destructor Destroy; override;

    function Map: Pointer; override;
    procedure Unmap; override;
    procedure UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt = 0); override;
    procedure ReadData(out Data; const Size: PtrUInt; const Offset: PtrUInt = 0);

    property Handle: TGLuint read FHandle;
    property Target: TGLenum read FTarget;
  end;

  TOpenGLRHITexture = class(TRHITexture)
  private
    FHandle: TGLuint;
    FTarget: TGLenum;
    FInternalFormat: TGLint;
    FImageUnitFormat: TGLenum;
    FIsDefaultBackbuffer: Boolean;
    FImmutableStorage: Boolean;
    FHostPixels: TBytes;
    function OpenGLDevice: TOpenGLRHIDevice;
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHITextureDesc); overload;
    constructor CreateDefaultBackbufferWrapper(const ADevice: TOpenGLRHIDevice;
      const ADesc: TRHITextureDesc); overload;
    destructor Destroy; override;

    procedure UploadImage(const AImage: TEncodedImage;
      const AMipLevel: Cardinal = 0; const AArrayLayer: Cardinal = 0); override;

    property Handle: TGLuint read FHandle;
    property Target: TGLenum read FTarget;
    property InternalFormat: TGLint read FInternalFormat;
    property ImageUnitFormat: TGLenum read FImageUnitFormat;
    property IsDefaultBackbuffer: Boolean read FIsDefaultBackbuffer;
    property ImmutableStorage: Boolean read FImmutableStorage;
  end;

  TOpenGLRHISampler = class(TRHISampler)
  private
    FHandle: TGLuint;
    function OpenGLDevice: TOpenGLRHIDevice;
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHISamplerDesc);
    destructor Destroy; override;
    property Handle: TGLuint read FHandle;
  end;

  TOpenGLRHIShaderModule = class(TRHIShaderModule)
  private
    FHandle: TGLuint;
    FTranslatedSource: String;
    FSourceHash: UInt64;
    function OpenGLDevice: TOpenGLRHIDevice;
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const AStage: TRHIShaderStage;
      const AGLSLSource: String; const ADebugName: String);
    destructor Destroy; override;
    property Handle: TGLuint read FHandle;
    property TranslatedSource: String read FTranslatedSource;
    property SourceHash: UInt64 read FSourceHash;
  end;

  TOpenGLRHIDescriptorSetLayout = class(TRHIDescriptorSetLayout)
  private
    FBindings: array of TRHIDescriptorBindingDesc;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ABindings: array of TRHIDescriptorBindingDesc);
    function FindBindingType(const ABinding: Cardinal;
      out AType: TRHIDescriptorType): Boolean;
    function BindingCount: Integer;
    function GetBinding(const AIndex: Integer): TRHIDescriptorBindingDesc;
  end;

  TOpenGLBoundBuffer = record
    Binding: Cardinal;
    Buffer: TOpenGLRHIBuffer;
    Offset: PtrUInt;
    Range: PtrUInt;
    IsStorage: Boolean;
    IsDynamic: Boolean;
  end;

  TOpenGLBoundTexture = record
    Binding: Cardinal;
    ArrayElement: Cardinal;
    Texture: TOpenGLRHITexture;
    Sampler: TOpenGLRHISampler;
    MipLevel: Cardinal;
    ArrayLayer: Cardinal;
    HasSubresourceOverride: Boolean;
    IsStorageImage: Boolean;
  end;

  TOpenGLRHIDescriptorSet = class(TRHIDescriptorSet)
  private
    FLayout: TOpenGLRHIDescriptorSetLayout;
    FBoundBuffers: array of TOpenGLBoundBuffer;
    FBoundTextures: array of TOpenGLBoundTexture;
    FUpdateCount: Cardinal;
    procedure SetBufferEntry(const ABinding: Cardinal; const ABuffer: TOpenGLRHIBuffer;
      const AOffset, ARange: PtrUInt; const AIsStorage: Boolean);
    procedure SetTextureEntry(const ABinding, AArrayElement: Cardinal;
      const ATexture: TOpenGLRHITexture; const ASampler: TOpenGLRHISampler;
      const AMipLevel, AArrayLayer: Cardinal; const AHasSubresource: Boolean;
      const AForceStorageImage: Boolean);
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ALayout: TOpenGLRHIDescriptorSetLayout);
    procedure BindUniformBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindStorageBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindTexture(const Binding: Cardinal; const Texture: TRHITexture;
      const Sampler: TRHISampler); override;
    procedure BindTextureAtIndex(const Binding, ArrayElement: Cardinal;
      const Texture: TRHITexture; const Sampler: TRHISampler); override;
    procedure BindTextureSubresource(const Binding: Cardinal;
      const Texture: TRHITexture; const MipLevel, ArrayLayer: Cardinal;
      const Sampler: TRHISampler = nil); override;
    procedure BindStorageImage(const Binding: Cardinal;
      const Texture: TRHITexture; const MipLevel: Cardinal = 0;
      const ArrayLayer: Cardinal = 0);
    procedure Update; override;

    property Layout: TOpenGLRHIDescriptorSetLayout read FLayout;
    property UpdateCount: Cardinal read FUpdateCount;
  end;

  TOpenGLRHIPipeline = class(TRHIPipeline)
  private
    FProgramHandle: TGLuint;
    FVAOHandle: TGLuint;
    FPushConstantUBO: TGLuint;
    FPushConstantCapacity: Cardinal;
    FPrimitiveMode: TGLenum;
    function OpenGLDevice: TOpenGLRHIDevice;
    procedure EnsurePushConstantCapacity(const AMinBytes: Cardinal);
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ADesc: TRHIGraphicsPipelineDesc);
    destructor Destroy; override;

    property ProgramHandle: TGLuint read FProgramHandle;
    property VAOHandle: TGLuint read FVAOHandle;
    property PrimitiveMode: TGLenum read FPrimitiveMode;
    property PushConstantUBO: TGLuint read FPushConstantUBO;
  end;

  TOpenGLRHIComputePipeline = class(TRHIComputePipeline)
  private
    FProgramHandle: TGLuint;
    FPushConstantUBO: TGLuint;
    FPushConstantCapacity: Cardinal;
    FWorkGroupSize: TVector3Cardinal;
    function OpenGLDevice: TOpenGLRHIDevice;
    procedure EnsurePushConstantCapacity(const AMinBytes: Cardinal);
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ADesc: TRHIComputePipelineDesc);
    destructor Destroy; override;

    property ProgramHandle: TGLuint read FProgramHandle;
    property PushConstantUBO: TGLuint read FPushConstantUBO;
    property WorkGroupSize: TVector3Cardinal read FWorkGroupSize;
  end;

  TOpenGLRHIQueryPool = class(TRHIQueryPool)
  private
    FQueries: array of TGLuint;
    FActiveTargets: array of TGLenum;
    FQueryWritten: array of Boolean;
    FSimulatedResults: array of UInt64;
    FResetCount: Cardinal;
    function OpenGLDevice: TOpenGLRHIDevice;
  protected
    procedure ReleaseDeviceResources; override;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHIQueryPoolDesc);
    destructor Destroy; override;

    function GetResults(const FirstQuery, QueryCount: Cardinal;
      out Results: array of UInt64; const WaitForResults: Boolean = False): Boolean; override;

    property ResetCount: Cardinal read FResetCount;
  end;

  { -------------------------------------------------------------------------- }
  { OpenGL Command List (Immediate State Machine Execution)                    }
  { -------------------------------------------------------------------------- }

  TOpenGLRHICommandList = class(TRHICommandList)
  private
    FDevice: TOpenGLRHIDevice;
    FScratchFBO: TGLuint;
    FResolveFBO: TGLuint;
    FCurrentPipeline: TOpenGLRHIPipeline;
    FCurrentComputePipeline: TOpenGLRHIComputePipeline;
    FCurrentPrimitiveMode: TGLenum;
    FCurrentIndexBuffer: TOpenGLRHIBuffer;
    FCurrentIndexOffset: PtrUInt;
    FCurrentIndexType: TGLenum;
    FCurrentIndexSize: Cardinal;
    FScissorEnabled: Boolean;
    FDynamicScissorRect: TRectangle;
    FDynamicViewportRect: TFloatRectangle;
    FIsRecording: Boolean;
    FComputeDispatchCount: Cardinal;
    FDrawCallCount: Cardinal;
    FIndirectDrawCallCount: Cardinal;
    FBarrierCount: Cardinal;
    FAliasingBarrierCount: Cardinal;
    FDebugGroupDepth: Integer;
    FDebugGroupPushCount: Cardinal;
    FDebugMarkerCount: Cardinal;
    procedure ApplyPipelineState(const NewPipeline: TOpenGLRHIPipeline);
    procedure ApplyDescriptorSetInternal(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal);
    procedure ApplyVertexLayout(const Pipeline: TOpenGLRHIPipeline;
      const FirstBinding: Cardinal; const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt);
    procedure EmitMemoryBarrierForState(const NewState: TRHIResourceState);
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ALevel: TRHICommandListLevel = cllPrimary);
    destructor Destroy; override;

    procedure BeginRecording; override;
    procedure EndRecording; override;

    procedure TransitionTexture(const Texture: TRHITexture;
      const NewState: TRHIResourceState); override;
    procedure EmitAliasingBarrier(const OldTexture, NewTexture: TRHITexture); override;
    procedure GenerateMipmaps(const Texture: TRHITexture); override;

    procedure BeginRendering(const Info: TRHIRenderingInfo); override;
    procedure EndRendering; override;
    procedure ClearAttachments(const ClearColorBuf: Boolean; const ClearColor: TCastleColor;
      const ClearDepthBuf: Boolean = False; const ClearDepth: Single = 1.0;
      const ClearStencilBuf: Boolean = False; const ClearStencil: Byte = 0); override;

    procedure SetViewport(const ViewportRect: TFloatRectangle;
      const MinDepth: Single = 0.0; const MaxDepth: Single = 1.0); override;
    procedure SetScissor(const ScissorRect: TRectangle); override;
    procedure SetDepthBias(const ConstantFactor, SlopeFactor: Single); override;
    procedure SetLineWidth(const LineWidth: Single); override;
    procedure SetBlendConstants(const ABlendConstants: TVector4); override;

    procedure SetCullMode(const CullMode: TRHICullMode); override;
    procedure SetFrontFace(const FrontFace: TRHIFrontFace); override;
    procedure SetDepthTestEnable(const Enable: Boolean); override;
    procedure SetDepthWriteEnable(const Enable: Boolean); override;
    procedure SetDepthCompareOp(const CompareOp: TRHICompareOp); override;
    procedure SetPrimitiveTopology(const Topology: TRHIPrimitiveTopology); override;

    procedure BindPipeline(const Pipeline: TRHIPipeline); override;
    procedure BindDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); override;
    procedure PushConstants(const StageFlags: TRHIShaderStageFlags;
      const Offset, Size: Cardinal; const Data); override;

    procedure BindComputePipeline(const Pipeline: TRHIComputePipeline); override;
    procedure BindComputeDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); override;
    procedure Dispatch(const GroupCountX, GroupCountY, GroupCountZ: Cardinal); override;
    procedure DispatchIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt = 0); override;

    procedure ResetQueryPool(const QueryPool: TRHIQueryPool;
      const FirstQuery, QueryCount: Cardinal); override;
    procedure BeginQuery(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal; const Precise: Boolean = False); override;
    procedure EndQuery(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal); override;
    procedure WriteTimestamp(const QueryPool: TRHIQueryPool;
      const QueryIndex: Cardinal; const Stage: TRHIShaderStage = ssFragment); override;

    procedure BindVertexBuffers(const FirstBinding: Cardinal;
      const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt); override;
    procedure BindIndexBuffer(const Buffer: TRHIBuffer;
      const Offset: PtrUInt; const IndexType: TRHIIndexType); override;

    procedure Draw(const VertexCount, InstanceCount, FirstVertex, FirstInstance: Cardinal); override;
    procedure DrawIndexed(const IndexCount, InstanceCount, FirstIndex: Cardinal;
      const VertexOffset: Integer; const FirstInstance: Cardinal); override;
    procedure DrawIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); override;
    procedure DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); override;
    procedure DrawIndirectCount(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
      const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal); override;
    procedure DrawIndexedIndirectCount(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
      const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal); override;

    procedure ExecuteCommands(const SecondaryCommandLists: array of TRHICommandList); override;

    procedure PushDebugGroup(const LabelName: String; const Color: TCastleColor); override;
    procedure PopDebugGroup; override;
    procedure InsertDebugMarker(const LabelName: String; const Color: TCastleColor); override;

    property IsRecording: Boolean read FIsRecording;
    property ComputeDispatchCount: Cardinal read FComputeDispatchCount;
    property DrawCallCount: Cardinal read FDrawCallCount;
    property IndirectDrawCallCount: Cardinal read FIndirectDrawCallCount;
    property BarrierCount: Cardinal read FBarrierCount;
    property AliasingBarrierCount: Cardinal read FAliasingBarrierCount;
    property DebugGroupPushCount: Cardinal read FDebugGroupPushCount;
    property DebugMarkerCount: Cardinal read FDebugMarkerCount;
  end;

  { -------------------------------------------------------------------------- }
  { OpenGL Render Device                                                       }
  { -------------------------------------------------------------------------- }

  TOpenGLPipelineMap = {$ifdef FPC}specialize{$endif} TObjectDictionary<UInt64, TOpenGLRHIPipeline>;
  TOpenGLComputePipelineMap = {$ifdef FPC}specialize{$endif} TObjectDictionary<UInt64, TOpenGLRHIComputePipeline>;

  TOpenGLRHIDevice = class(TRHIDevice)
  private
    FHasLiveGLContext: Boolean;
    FCommandList: TOpenGLRHICommandList;
    FDefaultColorTarget: TOpenGLRHITexture;
    FDefaultDepthTarget: TOpenGLRHITexture;
    FPipelineMap: TOpenGLPipelineMap;
    FComputePipelineMap: TOpenGLComputePipelineMap;
    FReadbackFBO: TGLuint;
    FReadbackResolveFBO: TGLuint;
    FMaxImageUnits: Cardinal;
    FPipelineCacheHits: Cardinal;
    FPipelineCacheMisses: Cardinal;
    FComputePipelineCacheHits: Cardinal;
    FComputePipelineCacheMisses: Cardinal;
    FComputeDispatchCount: Cardinal;
    FIndirectDrawCallCount: Cardinal;
    procedure QueryDeviceCapabilities;
    function ComputePipelineHash(const ADesc: TRHIGraphicsPipelineDesc): UInt64;
    function ComputeComputePipelineHash(const ADesc: TRHIComputePipelineDesc): UInt64;
    function GetPipelineCount: Integer;
    function GetComputePipelineCount: Integer;
  public
    { Creates an OpenGL RHI device wrapping the currently active CGE OpenGL/OpenGLES context. }
    constructor Create(const InitialWidth, InitialHeight: Cardinal); reintroduce; overload;
    { Creates a deterministic headless OpenGL RHI device for unit testing without an active GL context. }
    constructor CreateHeadless(const InitialWidth: Cardinal = 128;
      const InitialHeight: Cardinal = 128;
      const ASupportsCompute: Boolean = True;
      const ASupportsMultiDrawIndirect: Boolean = True;
      const ASupportsDrawIndirectCount: Boolean = True); overload;
    class function TryCreateFromCurrentGLContext(out ADevice: TOpenGLRHIDevice;
      const InitialWidth, InitialHeight: Cardinal): Boolean;
    destructor Destroy; override;

    function BeginFrame: TRHICommandList; override;
    procedure EndFrameAndPresent; override;
    function BeginOffscreenFrame: TRHICommandList; override;
    procedure EndOffscreenFrame; override;
    procedure ResizeSwapchain(const NewWidth, NewHeight: Cardinal); override;
    procedure WaitIdle; override;
    function CreateSecondaryCommandList: TRHICommandList; override;

    function GetSwapchainColorTexture: TRHITexture; override;
    function GetSwapchainDepthTexture: TRHITexture; override;

    function CreateBuffer(const ADesc: TRHIBufferDesc;
      const InitialData: Pointer = nil): TRHIBuffer; override;
    function CreateTexture(const ADesc: TRHITextureDesc): TRHITexture; override;
    function CreateSampler(const ADesc: TRHISamplerDesc): TRHISampler; override;

    function CreateShaderModuleFromGLSL(const AStage: TRHIShaderStage;
      const AGLSLSource: String; const ADebugName: String = ''): TRHIShaderModule; override;
    function CreateShaderModuleFromSPIRV(const AStage: TRHIShaderStage;
      const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt;
      const ADebugName: String = ''): TRHIShaderModule; override;

    function CreateDescriptorSetLayout(
      const Bindings: array of TRHIDescriptorBindingDesc): TRHIDescriptorSetLayout; override;
    function CreateDescriptorSet(
      const Layout: TRHIDescriptorSetLayout): TRHIDescriptorSet; override;

    function GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline; override;
    function GetOrCreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline; override;
    function CreateComputePipeline(const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline; override;
    function CreateQueryPool(const ADesc: TRHIQueryPoolDesc): TRHIQueryPool; override;

    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaImage; overload; override;
    function GrabTextureSubresourceImage(const SourceTexture: TRHITexture;
      const AMipLevel, AArrayLayer: Cardinal): TRGBAlphaImage; override;
    function GrabTextureFloatImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaFloatImage; override;

    procedure LoadPipelineCache(const CacheFileUrl: String); override;
    procedure SavePipelineCache(const CacheFileUrl: String); override;

    property HasLiveGLContext: Boolean read FHasLiveGLContext;
    property MaxImageUnits: Cardinal read FMaxImageUnits;
    property PipelineCount: Integer read GetPipelineCount;
    property ComputePipelineCount: Integer read GetComputePipelineCount;
    property PipelineCacheHits: Cardinal read FPipelineCacheHits;
    property PipelineCacheMisses: Cardinal read FPipelineCacheMisses;
    property ComputePipelineCacheHits: Cardinal read FComputePipelineCacheHits;
    property ComputePipelineCacheMisses: Cardinal read FComputePipelineCacheMisses;
    property ComputeDispatchCount: Cardinal read FComputeDispatchCount;
    property IndirectDrawCallCount: Cardinal read FIndirectDrawCallCount;
  end;

{ Translates Vulkan-style #version 450 / #version 460 RHI GLSL source
  (layout(set = S, binding = B), layout(push_constant), gl_VertexIndex, gl_InstanceIndex)
  into OpenGL 4.3+ (#version 430 core) or OpenGL ES 3.1+ (#version 310 es) GLSL
  with flattened binding slots (SetIdx * GL_DESCRIPTOR_SET_SLOT_STRIDE + BindIdx)
  and UBO push-constant binding slot GL_PUSH_CONSTANT_UBO_BINDING.
  Leaves native #version 330 / #version 430 / #version 300 es / #version 310 es
  sources intact unless Vulkan layout qualifiers are present. }
function TranslateVulkanGLSLToOpenGL(const AStage: TRHIShaderStage;
  const AGLSLSource: String;
  const ATargetOpenGLES: Boolean = {$ifdef OpenGLES}True{$else}False{$endif}): String;

implementation

uses
  StrUtils, CastleStringUtils;

{$ifndef OpenGLES}
type
  TGLintptr = GLintptr;
{$endif}

const
  FNV1aOffsetBasis64 = UInt64($CBF29CE484222325);
  FNV1aPrime64       = UInt64($00000100000001B3);
  GL_COMPUTE_WORK_GROUP_SIZE = $8267;
  GL_MAX_SAMPLES_RHI         = $8D57;

  {$ifdef OpenGLES}
  GL_COMPUTE_SHADER                   = $91B9;
  GL_SHADER_STORAGE_BUFFER            = $90D2;
  GL_DRAW_INDIRECT_BUFFER             = $8F3F;
  GL_DISPATCH_INDIRECT_BUFFER         = $90EE;
  GL_PARAMETER_BUFFER                 = $80EE;
  GL_READ_ONLY                        = $88B8;
  GL_WRITE_ONLY                       = $88B9;
  GL_READ_WRITE                       = $88BA;
  GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT  = $00000001;
  GL_ELEMENT_ARRAY_BARRIER_BIT        = $00000002;
  GL_UNIFORM_BARRIER_BIT              = $00000004;
  GL_TEXTURE_FETCH_BARRIER_BIT        = $00000008;
  GL_SHADER_IMAGE_ACCESS_BARRIER_BIT  = $00000020;
  GL_COMMAND_BARRIER_BIT              = $00000040;
  GL_BUFFER_UPDATE_BARRIER_BIT        = $00000200;
  GL_FRAMEBUFFER_BARRIER_BIT          = $00000400;
  GL_SHADER_STORAGE_BARRIER_BIT       = $00002000;
  GL_ALL_BARRIER_BITS                 = $FFFFFFFF;
  GL_MAX_IMAGE_UNITS                  = $8F38;
  GL_BGRA                             = $80E1;
  GL_TEXTURE_2D_MULTISAMPLE           = $9100;
  GL_CLAMP_TO_BORDER                  = $812D;
  {$endif}

{ ---------------------------------------------------------------------------- }
{ Deterministic 64-bit FNV-1a Hashing                                          }
{ ---------------------------------------------------------------------------- }

{$push}{$Q-}{$R-}
procedure HashBytesFNV1a(var AHash: UInt64; const AData; const ASize: PtrUInt); inline;
var
  P: PByte;
  I: PtrUInt;
begin
  if ASize = 0 then Exit;
  P := @AData;
  for I := 0 to ASize - 1 do
  begin
    AHash := AHash xor P[I];
    AHash := UInt64(AHash * FNV1aPrime64);
  end;
end;
{$pop}

procedure HashUInt64FNV1a(var AHash: UInt64; const AValue: UInt64); inline;
begin
  HashBytesFNV1a(AHash, AValue, SizeOf(AValue));
end;

procedure HashStringFNV1a(var AHash: UInt64; const AStr: String); inline;
begin
  if AStr <> '' then
    HashBytesFNV1a(AHash, AStr[1], Length(AStr));
end;

{ ---------------------------------------------------------------------------- }
{ Vulkan #version 450 -> OpenGL #version 430 / GLES #version 310 es Translator }
{ ---------------------------------------------------------------------------- }

function IsIdentChar(const C: Char): Boolean; inline;
begin
  Result := (C in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
end;

function ReplaceWholeIdentifier(const ASource, AOldIdent, ANewIdent: String): String;
var
  SB: TStringBuilder;
  PosIdx, MatchIdx, AfterIdx, SrcLen, OldLen: Integer;
begin
  SrcLen := Length(ASource);
  OldLen := Length(AOldIdent);
  if (OldLen = 0) or (SrcLen < OldLen) or (Pos(AOldIdent, ASource) = 0) then
    Exit(ASource);

  SB := TStringBuilder.Create(SrcLen + 32);
  try
    PosIdx := 1;
    while PosIdx <= SrcLen do
    begin
      MatchIdx := PosEx(AOldIdent, ASource, PosIdx);
      if MatchIdx = 0 then
      begin
        SB.Append(Copy(ASource, PosIdx, SrcLen - PosIdx + 1));
        Break;
      end;

      AfterIdx := MatchIdx + OldLen;
      if ((MatchIdx = 1) or (not IsIdentChar(ASource[MatchIdx - 1]))) and
         ((AfterIdx > SrcLen) or (not IsIdentChar(ASource[AfterIdx]))) then
      begin
        if MatchIdx > PosIdx then
          SB.Append(Copy(ASource, PosIdx, MatchIdx - PosIdx));
        SB.Append(ANewIdent);
        PosIdx := AfterIdx;
      end else
      begin
        SB.Append(Copy(ASource, PosIdx, MatchIdx - PosIdx + 1));
        PosIdx := MatchIdx + 1;
      end;
    end;
    Result := SB.ToString;
  finally
    FreeAndNil(SB);
  end;
end;

function RewriteSingleLayoutClause(const AInside: String): String;
var
  Parts: TStringList;
  OutParts: TStringList;
  I, EqPos, SetVal, BindVal: Integer;
  Item, Trimmed, KeyName, ValStr: String;
  HasPushConstant, HasSet, HasBinding: Boolean;
begin
  Parts := TStringList.Create;
  OutParts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := ',';
    Parts.DelimitedText := AInside;

    HasPushConstant := False;
    HasSet := False;
    HasBinding := False;
    SetVal := 0;
    BindVal := 0;

    for I := 0 to Parts.Count - 1 do
    begin
      Trimmed := Trim(Parts[I]);
      if SameText(Trimmed, 'push_constant') then
        HasPushConstant := True
      else
      begin
        EqPos := Pos('=', Trimmed);
        if EqPos > 0 then
        begin
          KeyName := Trim(Copy(Trimmed, 1, EqPos - 1));
          ValStr := Trim(Copy(Trimmed, EqPos + 1, MaxInt));
          if SameText(KeyName, 'set') and TryStrToInt(ValStr, SetVal) then
            HasSet := True
          else if SameText(KeyName, 'binding') and TryStrToInt(ValStr, BindVal) then
            HasBinding := True;
        end;
      end;
    end;

    if HasPushConstant then
    begin
      Result := Format('std140, binding = %d', [GL_PUSH_CONSTANT_UBO_BINDING]);
      Exit;
    end;

    if not HasSet then
    begin
      Result := AInside;
      Exit;
    end;

    for I := 0 to Parts.Count - 1 do
    begin
      Item := Trim(Parts[I]);
      if Item = '' then Continue;
      EqPos := Pos('=', Item);
      if EqPos > 0 then
      begin
        KeyName := Trim(Copy(Item, 1, EqPos - 1));
        if SameText(KeyName, 'set') then
          Continue;
        if SameText(KeyName, 'binding') and HasBinding then
        begin
          OutParts.Add(Format('binding = %d',
            [SetVal * GL_DESCRIPTOR_SET_SLOT_STRIDE + BindVal]));
          Continue;
        end;
      end;
      OutParts.Add(Item);
    end;

    if not HasBinding then
      OutParts.Add(Format('binding = %d', [SetVal * GL_DESCRIPTOR_SET_SLOT_STRIDE]));

    Result := '';
    for I := 0 to OutParts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      Result := Result + OutParts[I];
    end;
  finally
    FreeAndNil(OutParts);
    FreeAndNil(Parts);
  end;
end;

function RewriteLayoutQualifiers(const ASource: String): String;
var
  SB: TStringBuilder;
  SrcLen, Cursor, MatchIdx, OpenParen, CloseParen, Depth, ScanIdx: Integer;
  Inside: String;
begin
  SrcLen := Length(ASource);
  if Pos('layout', ASource) = 0 then
    Exit(ASource);

  SB := TStringBuilder.Create(SrcLen + 64);
  try
    Cursor := 1;
    while Cursor <= SrcLen do
    begin
      MatchIdx := PosEx('layout', ASource, Cursor);
      if MatchIdx = 0 then
      begin
        SB.Append(Copy(ASource, Cursor, SrcLen - Cursor + 1));
        Break;
      end;

      if ((MatchIdx > 1) and IsIdentChar(ASource[MatchIdx - 1])) or
         ((MatchIdx + 6 <= SrcLen) and IsIdentChar(ASource[MatchIdx + 6])) then
      begin
        SB.Append(Copy(ASource, Cursor, MatchIdx - Cursor + 6));
        Cursor := MatchIdx + 6;
        Continue;
      end;

      ScanIdx := MatchIdx + 6;
      while (ScanIdx <= SrcLen) and (ASource[ScanIdx] in [' ', #9, #10, #13]) do
        Inc(ScanIdx);

      if (ScanIdx > SrcLen) or (ASource[ScanIdx] <> '(') then
      begin
        SB.Append(Copy(ASource, Cursor, ScanIdx - Cursor));
        Cursor := ScanIdx;
        Continue;
      end;

      OpenParen := ScanIdx;
      Depth := 1;
      CloseParen := OpenParen + 1;
      while (CloseParen <= SrcLen) and (Depth > 0) do
      begin
        if ASource[CloseParen] = '(' then
          Inc(Depth)
        else if ASource[CloseParen] = ')' then
          Dec(Depth);
        if Depth > 0 then
          Inc(CloseParen);
      end;

      if Depth <> 0 then
      begin
        SB.Append(Copy(ASource, Cursor, SrcLen - Cursor + 1));
        Break;
      end;

      if MatchIdx > Cursor then
        SB.Append(Copy(ASource, Cursor, MatchIdx - Cursor));
      Inside := Copy(ASource, OpenParen + 1, CloseParen - OpenParen - 1);
      SB.Append('layout(');
      SB.Append(RewriteSingleLayoutClause(Inside));
      SB.Append(')');
      Cursor := CloseParen + 1;
    end;
    Result := SB.ToString;
  finally
    FreeAndNil(SB);
  end;
end;

function TranslateVulkanGLSLToOpenGL(const AStage: TRHIShaderStage;
  const AGLSLSource: String; const ATargetOpenGLES: Boolean): String;
var
  Lines: TStringList;
  SB: TStringBuilder;
  I: Integer;
  Line, Trimmed: String;
  NeedsTranslation: Boolean;
begin
  NeedsTranslation :=
    (Pos('#version 450', AGLSLSource) > 0) or
    (Pos('#version 460', AGLSLSource) > 0) or
    (Pos('push_constant', AGLSLSource) > 0) or
    (Pos('gl_VertexIndex', AGLSLSource) > 0) or
    (Pos('gl_InstanceIndex', AGLSLSource) > 0) or
    (Pos('set =', AGLSLSource) > 0) or
    (Pos('set=', AGLSLSource) > 0);

  if not NeedsTranslation then
    Exit(AGLSLSource);

  Lines := TStringList.Create;
  SB := TStringBuilder.Create(Length(AGLSLSource) + 128);
  try
    Lines.Text := AGLSLSource;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      Trimmed := Trim(Line);

      if IsPrefix('#version 450', Trimmed, False) or
         IsPrefix('#version 460', Trimmed, False) then
      begin
        if ATargetOpenGLES then
        begin
          SB.AppendLine('#version 310 es');
          SB.AppendLine('precision highp float;');
          SB.AppendLine('precision highp int;');
        end else
          SB.AppendLine('#version 430 core');
        Continue;
      end;

      if IsPrefix('#extension GL_ARB_separate_shader_objects', Trimmed, False) or
         IsPrefix('#extension GL_EXT_nonuniform_qualifier', Trimmed, False) or
         IsPrefix('#extension GL_GOOGLE_include_directive', Trimmed, False) then
        Continue;

      SB.AppendLine(Line);
    end;

    Result := RewriteLayoutQualifiers(SB.ToString);
    Result := ReplaceWholeIdentifier(Result, 'gl_VertexIndex', 'gl_VertexID');
    Result := ReplaceWholeIdentifier(Result, 'gl_InstanceIndex', 'gl_InstanceID');
  finally
    FreeAndNil(SB);
    FreeAndNil(Lines);
  end;

  if AStage = ssCompute then ; { Silence unused hint when stage-agnostic }
end;

{ ---------------------------------------------------------------------------- }
{ OpenGL Enum Conversion Helpers                                               }
{ ---------------------------------------------------------------------------- }

procedure RHIToGLFormat(const Format: TRHIPixelFormat;
  out InternalFormat: TGLint; out PixelFormat, PixelType, ImageUnitFormat: TGLenum);
begin
  case Format of
    pfR8_UNorm:
      begin
        InternalFormat := GL_R8;
        PixelFormat := GL_RED;
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_R8;
      end;
    pfRG8_UNorm:
      begin
        InternalFormat := GL_RG8;
        PixelFormat := GL_RG;
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_RG8;
      end;
    pfRGBA8_UNorm:
      begin
        InternalFormat := GL_RGBA8;
        PixelFormat := GL_RGBA;
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_RGBA8;
      end;
    pfRGBA8_sRGB:
      begin
        InternalFormat := GL_SRGB8_ALPHA8;
        PixelFormat := GL_RGBA;
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_RGBA8;
      end;
    pfBGRA8_UNorm:
      begin
        InternalFormat := GL_RGBA8;
        {$ifdef OpenGLES}
        PixelFormat := GL_RGBA;
        {$else}
        PixelFormat := GL_BGRA;
        {$endif}
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_RGBA8;
      end;
    pfBGRA8_sRGB:
      begin
        InternalFormat := GL_SRGB8_ALPHA8;
        {$ifdef OpenGLES}
        PixelFormat := GL_RGBA;
        {$else}
        PixelFormat := GL_BGRA;
        {$endif}
        PixelType := GL_UNSIGNED_BYTE;
        ImageUnitFormat := GL_RGBA8;
      end;
    pfR16_Float:
      begin
        InternalFormat := GL_R16F;
        PixelFormat := GL_RED;
        PixelType := GL_HALF_FLOAT;
        ImageUnitFormat := GL_R16F;
      end;
    pfRG16_Float:
      begin
        InternalFormat := GL_RG16F;
        PixelFormat := GL_RG;
        PixelType := GL_HALF_FLOAT;
        ImageUnitFormat := GL_RG16F;
      end;
    pfRGBA16_Float:
      begin
        InternalFormat := GL_RGBA16F;
        PixelFormat := GL_RGBA;
        PixelType := GL_HALF_FLOAT;
        ImageUnitFormat := GL_RGBA16F;
      end;
    pfR32_Float:
      begin
        InternalFormat := GL_R32F;
        PixelFormat := GL_RED;
        PixelType := GL_FLOAT;
        ImageUnitFormat := GL_R32F;
      end;
    pfRGBA32_Float:
      begin
        InternalFormat := GL_RGBA32F;
        PixelFormat := GL_RGBA;
        PixelType := GL_FLOAT;
        ImageUnitFormat := GL_RGBA32F;
      end;
    pfD16_UNorm:
      begin
        InternalFormat := GL_DEPTH_COMPONENT16;
        PixelFormat := GL_DEPTH_COMPONENT;
        PixelType := GL_UNSIGNED_SHORT;
        ImageUnitFormat := GL_R16F;
      end;
    pfD24_UNorm_S8_UInt:
      begin
        InternalFormat := GL_DEPTH24_STENCIL8;
        PixelFormat := GL_DEPTH_STENCIL;
        PixelType := GL_UNSIGNED_INT_24_8;
        ImageUnitFormat := GL_RGBA8;
      end;
    pfD32_Float:
      begin
        InternalFormat := GL_DEPTH_COMPONENT32F;
        PixelFormat := GL_DEPTH_COMPONENT;
        PixelType := GL_FLOAT;
        ImageUnitFormat := GL_R32F;
      end;
    pfD32_Float_S8_UInt:
      begin
        InternalFormat := GL_DEPTH32F_STENCIL8;
        PixelFormat := GL_DEPTH_STENCIL;
        PixelType := GL_FLOAT_32_UNSIGNED_INT_24_8_REV;
        ImageUnitFormat := GL_R32F;
      end;
  else
    begin
      InternalFormat := GL_RGBA8;
      PixelFormat := GL_RGBA;
      PixelType := GL_UNSIGNED_BYTE;
      ImageUnitFormat := GL_RGBA8;
    end;
  end;
end;

function RHIToGLCompareOp(const Op: TRHICompareOp): TGLenum;
begin
  case Op of
    coNever:          Result := GL_NEVER;
    coLess:           Result := GL_LESS;
    coEqual:          Result := GL_EQUAL;
    coLessOrEqual:    Result := GL_LEQUAL;
    coGreater:        Result := GL_GREATER;
    coNotEqual:       Result := GL_NOTEQUAL;
    coGreaterOrEqual: Result := GL_GEQUAL;
    coAlways:         Result := GL_ALWAYS;
  else
    Result := GL_LEQUAL;
  end;
end;

function RHIToGLStencilOp(const Op: TRHIStencilOp): TGLenum;
begin
  case Op of
    soKeep:              Result := GL_KEEP;
    soZero:              Result := GL_ZERO;
    soReplace:           Result := GL_REPLACE;
    soIncrementAndClamp: Result := GL_INCR;
    soDecrementAndClamp: Result := GL_DECR;
    soInvert:            Result := GL_INVERT;
    soIncrementAndWrap:  Result := GL_INCR_WRAP;
    soDecrementAndWrap:  Result := GL_DECR_WRAP;
  else
    Result := GL_KEEP;
  end;
end;

function RHIToGLWrapMode(const Mode: TRHISamplerAddressMode): TGLint;
begin
  case Mode of
    amRepeat:         Result := GL_REPEAT;
    amMirroredRepeat: Result := GL_MIRRORED_REPEAT;
    amClampToEdge:    Result := GL_CLAMP_TO_EDGE;
    amClampToBorder:  Result := GL_CLAMP_TO_BORDER;
  else
    Result := GL_CLAMP_TO_EDGE;
  end;
end;

function RHIToGLBlendFactor(const Factor: TRHIBlendFactor): TGLenum;
const
  {$ifndef OpenGLES}
  GL_CONSTANT_COLOR_COMPAT = GL_CONSTANT_COLOR_ARB;
  {$else}
  GL_CONSTANT_COLOR_COMPAT = GL_CONSTANT_COLOR;
  {$endif}
begin
  case Factor of
    bfZero:                  Result := GL_ZERO;
    bfOne:                   Result := GL_ONE;
    bfSrcColor:              Result := GL_SRC_COLOR;
    bfOneMinusSrcColor:      Result := GL_ONE_MINUS_SRC_COLOR;
    bfDstColor:              Result := GL_DST_COLOR;
    bfOneMinusDstColor:      Result := GL_ONE_MINUS_DST_COLOR;
    bfSrcAlpha:              Result := GL_SRC_ALPHA;
    bfOneMinusSrcAlpha:      Result := GL_ONE_MINUS_SRC_ALPHA;
    bfDstAlpha:              Result := GL_DST_ALPHA;
    bfOneMinusDstAlpha:      Result := GL_ONE_MINUS_DST_ALPHA;
    bfConstantColor:         Result := GL_CONSTANT_COLOR_COMPAT;
    bfOneMinusConstantColor: Result := GL_ONE_MINUS_CONSTANT_COLOR;
    bfConstantAlpha:         Result := GL_CONSTANT_ALPHA;
    bfOneMinusConstantAlpha: Result := GL_ONE_MINUS_CONSTANT_ALPHA;
    bfSrcAlphaSaturate:      Result := GL_SRC_ALPHA_SATURATE;
  else
    Result := GL_ONE;
  end;
end;

function RHIToGLBlendOp(const Op: TRHIBlendOp): TGLenum;
begin
  case Op of
    boAdd:             Result := GL_FUNC_ADD;
    boSubtract:        Result := GL_FUNC_SUBTRACT;
    boReverseSubtract: Result := GL_FUNC_REVERSE_SUBTRACT;
    {$ifndef OpenGLES}
    boMin:             Result := GL_MIN;
    boMax:             Result := GL_MAX;
    {$else}
    boMin:             Result := GL_MIN;
    boMax:             Result := GL_MAX;
    {$endif}
  else
    Result := GL_FUNC_ADD;
  end;
end;

function RHIToGLTopology(const Topology: TRHIPrimitiveTopology): TGLenum;
begin
  case Topology of
    ptTriangleList:  Result := GL_TRIANGLES;
    ptTriangleStrip: Result := GL_TRIANGLE_STRIP;
    ptLineList:      Result := GL_LINES;
    ptLineStrip:     Result := GL_LINE_STRIP;
    ptPointList:     Result := GL_POINTS;
  else
    Result := GL_TRIANGLES;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIBuffer                                                             }
{ ---------------------------------------------------------------------------- }

function TOpenGLRHIBuffer.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

constructor TOpenGLRHIBuffer.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIBufferDesc; const InitialData: Pointer);
var
  GLUsage: TGLenum;
  ByteSize: PtrUInt;
begin
  inherited Create(ADevice, ADesc);

  if buIndex in ADesc.Usage then
    FTarget := GL_ELEMENT_ARRAY_BUFFER
  else if buUniform in ADesc.Usage then
    FTarget := GL_UNIFORM_BUFFER
  else if buStorage in ADesc.Usage then
    FTarget := GL_SHADER_STORAGE_BUFFER
  else if buIndirect in ADesc.Usage then
    FTarget := GL_DRAW_INDIRECT_BUFFER
  else
    FTarget := GL_ARRAY_BUFFER;

  if ADesc.MemoryUsage = muCpuToGpu then
    GLUsage := GL_DYNAMIC_DRAW
  else if ADesc.MemoryUsage = muGpuToCpu then
    GLUsage := GL_STREAM_READ
  else if (buStorage in ADesc.Usage) or (buIndirect in ADesc.Usage) then
    GLUsage := GL_DYNAMIC_DRAW
  else
    GLUsage := GL_STATIC_DRAW;

  ByteSize := Max(PtrUInt(1), ADesc.Size);

  if (ADevice <> nil) and ADevice.HasLiveGLContext then
  begin
    glGenBuffers(1, @FHandle);
    glBindBuffer(FTarget, FHandle);
    glBufferData(FTarget, ByteSize, InitialData, GLUsage);
    glBindBuffer(FTarget, 0);
  end else
  begin
    FHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FHandle = 0 then
      FHandle := 1;
    SetLength(FHostShadow, ByteSize);
    if (InitialData <> nil) and (ADesc.Size > 0) then
      Move(InitialData^, FHostShadow[0], ADesc.Size)
    else if ByteSize > 0 then
      FillChar(FHostShadow[0], ByteSize, 0);
  end;
end;

procedure TOpenGLRHIBuffer.ReleaseDeviceResources;
begin
  if FMappedFallback <> nil then
  begin
    FreeMem(FMappedFallback);
    FMappedFallback := nil;
  end;
  FIsMappedForWrite := False;
  FMappedUploadedViaSubData := False;
  if (FHandle <> 0) and (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
    glDeleteBuffers(1, @FHandle);
  FHandle := 0;
  SetLength(FHostShadow, 0);
end;

destructor TOpenGLRHIBuffer.Destroy;
begin
  inherited;
end;

procedure TOpenGLRHIBuffer.SyncMappedToGL(const AOffset: PtrUInt; const ASize: PtrUInt);
var
  TotalBytes, SyncOffset, SyncBytes: PtrUInt;
begin
  if (not FIsMappedForWrite) or (FMappedFallback = nil) or
     (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
    Exit;

  TotalBytes := Max(PtrUInt(1), FDesc.Size);
  if AOffset >= TotalBytes then
    Exit;
  SyncOffset := AOffset;
  if (ASize = 0) or (SyncOffset + ASize > TotalBytes) then
    SyncBytes := TotalBytes - SyncOffset
  else
    SyncBytes := ASize;

  glBindBuffer(FTarget, FHandle);
  glBufferSubData(FTarget, SyncOffset, SyncBytes, PByte(FMappedFallback) + SyncOffset);
  glBindBuffer(FTarget, 0);
end;

function TOpenGLRHIBuffer.Map: Pointer;
var
  ByteSize: PtrUInt;
begin
  ByteSize := Max(PtrUInt(1), FDesc.Size);

  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
  begin
    if Length(FHostShadow) < PtrInt(ByteSize) then
      SetLength(FHostShadow, ByteSize);
    Exit(@FHostShadow[0]);
  end;

  { Use a persistent CPU staging mirror (FMappedFallback) instead of leaving the
    OpenGL buffer object in GL_BUFFER_MAPPED state via glMapBufferRange without
    GL_MAP_PERSISTENT_BIT. Per OpenGL 4.6 Core Spec 6.3.2, issuing glDraw* or
    glDispatch* while a buffer is mapped without MAP_PERSISTENT_BIT raises
    GL_INVALID_OPERATION on strict hardware drivers (NVIDIA, AMD, Intel), and
    GL_MAP_INVALIDATE_BUFFER_BIT would discard earlier sub-ranges written in
    multi-pass streaming ring buffers (such as TCastleBatch2DRenderer). }
  if FMappedFallback = nil then
  begin
    GetMem(FMappedFallback, ByteSize);
    FillChar(FMappedFallback^, ByteSize, 0);
  end;

  {$ifndef OpenGLES}
  if ((FDesc.MemoryUsage = muGpuToCpu) or (buStorage in FDesc.Usage) or (buIndirect in FDesc.Usage)) and
     Assigned(glGetBufferSubData) then
  begin
    glBindBuffer(FTarget, FHandle);
    glGetBufferSubData(FTarget, 0, ByteSize, FMappedFallback);
    glBindBuffer(FTarget, 0);
  end;
  {$endif}

  FIsMappedForWrite := (FDesc.MemoryUsage <> muGpuToCpu);
  FMappedUploadedViaSubData := False;
  Result := FMappedFallback;
end;

procedure TOpenGLRHIBuffer.Unmap;
begin
  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
    Exit;

  if FIsMappedForWrite and (not FMappedUploadedViaSubData) then
    SyncMappedToGL(0, 0);
  FIsMappedForWrite := False;
  FMappedUploadedViaSubData := False;
end;

procedure TOpenGLRHIBuffer.UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt);
var
  ByteSize: PtrUInt;
begin
  if Size = 0 then Exit;

  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
  begin
    if Offset + Size > PtrUInt(Length(FHostShadow)) then
      SetLength(FHostShadow, Offset + Size);
    Move(Data, FHostShadow[Offset], Size);
    Exit;
  end;

  ByteSize := Max(PtrUInt(1), FDesc.Size);
  if (FMappedFallback <> nil) and (Offset + Size <= ByteSize) and
     (Pointer(@Data) <> Pointer(PByte(FMappedFallback) + Offset)) then
    Move(Data, (PByte(FMappedFallback) + Offset)^, Size);
  if FIsMappedForWrite then
    FMappedUploadedViaSubData := True;

  glBindBuffer(FTarget, FHandle);
  glBufferSubData(FTarget, Offset, Size, @Data);
  glBindBuffer(FTarget, 0);
end;

procedure TOpenGLRHIBuffer.ReadData(out Data; const Size: PtrUInt; const Offset: PtrUInt);
var
  MappedPtr: PByte;
begin
  if Size = 0 then Exit;
  FillChar(Data, Size, 0);

  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
  begin
    if Offset + Size <= PtrUInt(Length(FHostShadow)) then
      Move(FHostShadow[Offset], Data, Size);
    Exit;
  end;

  {$ifndef OpenGLES}
  if Assigned(glGetBufferSubData) then
  begin
    glBindBuffer(FTarget, FHandle);
    glGetBufferSubData(FTarget, Offset, Size, @Data);
    glBindBuffer(FTarget, 0);
    Exit;
  end;
  {$endif}

  glBindBuffer(FTarget, FHandle);
  MappedPtr := PByte(glMapBufferRange(FTarget, Offset, Size, GL_MAP_READ_BIT));
  if MappedPtr <> nil then
  begin
    Move(MappedPtr^, Data, Size);
    glUnmapBuffer(FTarget);
  end;
  glBindBuffer(FTarget, 0);
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHITexture                                                            }
{ ---------------------------------------------------------------------------- }

function TOpenGLRHITexture.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

constructor TOpenGLRHITexture.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHITextureDesc);
var
  PixelFormat, PixelType: TGLenum;
  Mip, MipCount, W, H, D, Face, Samples: Cardinal;
begin
  inherited Create(ADevice, ADesc);
  FIsDefaultBackbuffer := False;
  FImmutableStorage := False;

  Samples := Max(Cardinal(1), ADesc.SampleCount);
  MipCount := Max(Cardinal(1), ADesc.MipLevels);

  if (ADesc.Dimension = td2D) and (Samples > 1) then
    FTarget := GL_TEXTURE_2D_MULTISAMPLE
  else
  begin
    case ADesc.Dimension of
      td2D:      FTarget := GL_TEXTURE_2D;
      td2DArray: FTarget := GL_TEXTURE_2D_ARRAY;
      tdCube:    FTarget := GL_TEXTURE_CUBE_MAP;
      td3D:      FTarget := GL_TEXTURE_3D;
    else
      FTarget := GL_TEXTURE_2D;
    end;
  end;

  RHIToGLFormat(ADesc.Format, FInternalFormat, PixelFormat, PixelType, FImageUnitFormat);

  if (ADevice = nil) or (not ADevice.HasLiveGLContext) then
  begin
    FHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FHandle = 0 then
      FHandle := 1;
    SetLength(FHostPixels,
      PtrUInt(Max(Cardinal(1), ADesc.Width)) *
      PtrUInt(Max(Cardinal(1), ADesc.Height)) * 4);
    Exit;
  end;

  glGenTextures(1, @FHandle);
  glBindTexture(FTarget, FHandle);

  {$ifndef OpenGLES}
  if FTarget = GL_TEXTURE_2D_MULTISAMPLE then
  begin
    if Assigned(glTexStorage2DMultisample) then
    begin
      glTexStorage2DMultisample(FTarget, Samples, TGLenum(FInternalFormat),
        Max(Cardinal(1), ADesc.Width), Max(Cardinal(1), ADesc.Height), GL_TRUE);
      FImmutableStorage := True;
    end else
    if Assigned(glTexImage2DMultisample) then
      glTexImage2DMultisample(FTarget, Samples, FInternalFormat,
        Max(Cardinal(1), ADesc.Width), Max(Cardinal(1), ADesc.Height), GL_TRUE);
    glBindTexture(FTarget, 0);
    Exit;
  end;
  {$endif}

  { Use immutable storage (glTexStorage2D / glTexStorage3D) when available, especially
    for tuStorage textures bound via glBindImageTexture where OpenGL requires immutable
    or level-complete mip pyramids. }
  {$ifndef OpenGLES}
  if Assigned(glTexStorage2D) and (ADesc.Dimension in [td2D, tdCube]) then
  begin
    glTexStorage2D(FTarget, MipCount, TGLenum(FInternalFormat),
      Max(Cardinal(1), ADesc.Width), Max(Cardinal(1), ADesc.Height));
    FImmutableStorage := True;
  end else
  if Assigned(glTexStorage3D) and (ADesc.Dimension in [td2DArray, td3D]) then
  begin
    glTexStorage3D(FTarget, MipCount, TGLenum(FInternalFormat),
      Max(Cardinal(1), ADesc.Width), Max(Cardinal(1), ADesc.Height),
      Max(Cardinal(1), ADesc.DepthOrArrayLayers));
    FImmutableStorage := True;
  end else
  {$endif}
  begin
    glTexParameteri(FTarget, GL_TEXTURE_BASE_LEVEL, 0);
    glTexParameteri(FTarget, GL_TEXTURE_MAX_LEVEL, TGLint(MipCount - 1));

    if ADesc.Dimension = tdCube then
    begin
      for Face := 0 to 5 do
      begin
        W := Max(Cardinal(1), ADesc.Width);
        H := Max(Cardinal(1), ADesc.Height);
        for Mip := 0 to MipCount - 1 do
        begin
          glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + Face, Mip, FInternalFormat,
            W, H, 0, PixelFormat, PixelType, nil);
          W := Max(1, W div 2);
          H := Max(1, H div 2);
        end;
      end;
    end else
    if ADesc.Dimension in [td2DArray, td3D] then
    begin
      W := Max(Cardinal(1), ADesc.Width);
      H := Max(Cardinal(1), ADesc.Height);
      D := Max(Cardinal(1), ADesc.DepthOrArrayLayers);
      for Mip := 0 to MipCount - 1 do
      begin
        glTexImage3D(FTarget, Mip, FInternalFormat, W, H, D, 0, PixelFormat, PixelType, nil);
        W := Max(1, W div 2);
        H := Max(1, H div 2);
        if ADesc.Dimension = td3D then
          D := Max(1, D div 2);
      end;
    end else
    begin
      W := Max(Cardinal(1), ADesc.Width);
      H := Max(Cardinal(1), ADesc.Height);
      for Mip := 0 to MipCount - 1 do
      begin
        glTexImage2D(FTarget, Mip, FInternalFormat, W, H, 0, PixelFormat, PixelType, nil);
        W := Max(1, W div 2);
        H := Max(1, H div 2);
      end;
    end;
  end;

  if FDesc.Format in [pfD16_UNorm, pfD24_UNorm_S8_UInt, pfD32_Float, pfD32_Float_S8_UInt] then
  begin
    glTexParameteri(FTarget, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(FTarget, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  end else
  begin
    if MipCount > 1 then
      glTexParameteri(FTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
    else
      glTexParameteri(FTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(FTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end;
  glTexParameteri(FTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(FTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  if ADesc.Dimension in [tdCube, td3D, td2DArray] then
    glTexParameteri(FTarget, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  glBindTexture(FTarget, 0);
end;

constructor TOpenGLRHITexture.CreateDefaultBackbufferWrapper(
  const ADevice: TOpenGLRHIDevice; const ADesc: TRHITextureDesc);
var
  PixelFormat, PixelType: TGLenum;
begin
  inherited Create(ADevice, ADesc);
  FIsDefaultBackbuffer := True;
  FHandle := 0;
  FTarget := GL_TEXTURE_2D;
  RHIToGLFormat(ADesc.Format, FInternalFormat, PixelFormat, PixelType, FImageUnitFormat);
end;

procedure TOpenGLRHITexture.ReleaseDeviceResources;
begin
  if (not FIsDefaultBackbuffer) and (FHandle <> 0) and
     (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
    glDeleteTextures(1, @FHandle);
  FHandle := 0;
  SetLength(FHostPixels, 0);
end;

destructor TOpenGLRHITexture.Destroy;
begin
  inherited;
end;

procedure TOpenGLRHITexture.UploadImage(const AImage: TEncodedImage;
  const AMipLevel: Cardinal; const AArrayLayer: Cardinal);
var
  UploadFormat, UploadType, SubTarget: TGLenum;
  ConvertedRgba: TRGBAlphaImage;
  ConvertedFloat: TRGBAlphaFloatImage;
  SrcPixels: Pointer;
  ImgW, ImgH, ImgD: Cardinal;
begin
  if FIsDefaultBackbuffer or (AImage = nil) then Exit;

  ImgW := Max(Cardinal(1), AImage.Width);
  ImgH := Max(Cardinal(1), AImage.Height);
  ImgD := Max(Cardinal(1), AImage.Depth);

  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
  begin
    if (AMipLevel = 0) and (AArrayLayer = 0) and (AImage is TCastleImage) then
    begin
      ConvertedRgba := TCastleImage(AImage).MakeCopy as TRGBAlphaImage;
      try
        SetLength(FHostPixels, PtrUInt(ImgW) * PtrUInt(ImgH) * 4);
        Move(ConvertedRgba.RawPixels^, FHostPixels[0], Length(FHostPixels));
      finally
        FreeAndNil(ConvertedRgba);
      end;
    end;
    Exit;
  end;

  glBindTexture(FTarget, FHandle);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  ConvertedRgba := nil;
  ConvertedFloat := nil;
  try
    if FDesc.Format in [pfR16_Float, pfRG16_Float, pfRGBA16_Float, pfR32_Float, pfRGBA32_Float] then
    begin
      if AImage is TRGBAlphaFloatImage then
        SrcPixels := TRGBAlphaFloatImage(AImage).RawPixels
      else if AImage is TCastleImage then
      begin
        ConvertedFloat := TRGBAlphaFloatImage.Create(ImgW, ImgH, ImgD);
        ConvertedFloat.Assign(TCastleImage(AImage));
        SrcPixels := ConvertedFloat.RawPixels;
      end else
        Exit;
      UploadFormat := GL_RGBA;
      UploadType := GL_FLOAT;
    end else
    begin
      if AImage is TRGBAlphaImage then
        SrcPixels := TRGBAlphaImage(AImage).RawPixels
      else if AImage is TCastleImage then
      begin
        ConvertedRgba := TRGBAlphaImage.Create(ImgW, ImgH, ImgD);
        ConvertedRgba.Assign(TCastleImage(AImage));
        SrcPixels := ConvertedRgba.RawPixels;
      end else
        Exit;
      UploadFormat := GL_RGBA;
      UploadType := GL_UNSIGNED_BYTE;
    end;

    if FDesc.Dimension = tdCube then
    begin
      SubTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X + Min(Cardinal(5), AArrayLayer);
      glTexSubImage2D(SubTarget, AMipLevel, 0, 0, ImgW, ImgH,
        UploadFormat, UploadType, SrcPixels);
    end else
    if FDesc.Dimension = td2DArray then
      glTexSubImage3D(FTarget, AMipLevel, 0, 0, AArrayLayer, ImgW, ImgH, 1,
        UploadFormat, UploadType, SrcPixels)
    else
    if FDesc.Dimension = td3D then
      glTexSubImage3D(FTarget, AMipLevel, 0, 0, 0, ImgW, ImgH, ImgD,
        UploadFormat, UploadType, SrcPixels)
    else
      glTexSubImage2D(FTarget, AMipLevel, 0, 0, ImgW, ImgH,
        UploadFormat, UploadType, SrcPixels);
  finally
    FreeAndNil(ConvertedFloat);
    FreeAndNil(ConvertedRgba);
    glBindTexture(FTarget, 0);
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHISampler, ShaderModule, DescriptorSetLayout, DescriptorSet          }
{ ---------------------------------------------------------------------------- }

function TOpenGLRHISampler.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

constructor TOpenGLRHISampler.Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHISamplerDesc);
var
  MinFilterGL: TGLint;
begin
  inherited Create(ADevice, ADesc);

  if (ADevice = nil) or (not ADevice.HasLiveGLContext) then
  begin
    FHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FHandle = 0 then
      FHandle := 1;
    Exit;
  end;

  glGenSamplers(1, @FHandle);
  if ADesc.MagFilter = sfNearest then
    glSamplerParameteri(FHandle, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  else
    glSamplerParameteri(FHandle, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  if ADesc.MaxLod > 0.0 then
  begin
    if ADesc.MinFilter = sfNearest then
    begin
      if ADesc.MipFilter = sfNearest then
        MinFilterGL := GL_NEAREST_MIPMAP_NEAREST
      else
        MinFilterGL := GL_NEAREST_MIPMAP_LINEAR;
    end else
    begin
      if ADesc.MipFilter = sfNearest then
        MinFilterGL := GL_LINEAR_MIPMAP_NEAREST
      else
        MinFilterGL := GL_LINEAR_MIPMAP_LINEAR;
    end;
  end else
  begin
    if ADesc.MinFilter = sfNearest then
      MinFilterGL := GL_NEAREST
    else
      MinFilterGL := GL_LINEAR;
  end;
  glSamplerParameteri(FHandle, GL_TEXTURE_MIN_FILTER, MinFilterGL);

  glSamplerParameteri(FHandle, GL_TEXTURE_WRAP_S, RHIToGLWrapMode(ADesc.AddressModeU));
  glSamplerParameteri(FHandle, GL_TEXTURE_WRAP_T, RHIToGLWrapMode(ADesc.AddressModeV));
  glSamplerParameteri(FHandle, GL_TEXTURE_WRAP_R, RHIToGLWrapMode(ADesc.AddressModeW));

  if ADesc.MinLod >= 0.0 then
    glSamplerParameterf(FHandle, GL_TEXTURE_MIN_LOD, ADesc.MinLod);
  if ADesc.MaxLod > 0.0 then
    glSamplerParameterf(FHandle, GL_TEXTURE_MAX_LOD, ADesc.MaxLod);

  {$ifndef OpenGLES}
  if (ADesc.MaxAnisotropy > 1.0) and (ADevice.Capabilities.MaxAnisotropy > 1.0) then
    glSamplerParameterf(FHandle, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      EnsureRange(ADesc.MaxAnisotropy, 1.0, ADevice.Capabilities.MaxAnisotropy));
  {$endif}

  if ADesc.CompareEnable then
  begin
    glSamplerParameteri(FHandle, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glSamplerParameteri(FHandle, GL_TEXTURE_COMPARE_FUNC, RHIToGLCompareOp(ADesc.CompareOp));
  end;
end;

procedure TOpenGLRHISampler.ReleaseDeviceResources;
begin
  if (FHandle <> 0) and (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
    glDeleteSamplers(1, @FHandle);
  FHandle := 0;
end;

destructor TOpenGLRHISampler.Destroy;
begin
  inherited;
end;

function TOpenGLRHIShaderModule.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

constructor TOpenGLRHIShaderModule.Create(const ADevice: TOpenGLRHIDevice;
  const AStage: TRHIShaderStage; const AGLSLSource: String; const ADebugName: String);
var
  GLStage: TGLenum;
  AnsiSrc: AnsiString;
  SrcPtr: PGLchar;
  SrcLen, CompileStatus, LogLen: TGLint;
  ErrorLog: AnsiString;
begin
  inherited Create(ADevice, AStage, ADebugName);

  FTranslatedSource := TranslateVulkanGLSLToOpenGL(AStage, AGLSLSource);
  FSourceHash := FNV1aOffsetBasis64;
  HashStringFNV1a(FSourceHash, FTranslatedSource);

  if (ADevice = nil) or (not ADevice.HasLiveGLContext) then
  begin
    FHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FHandle = 0 then
      FHandle := 1;
    Exit;
  end;

  case AStage of
    ssVertex:   GLStage := GL_VERTEX_SHADER;
    ssFragment: GLStage := GL_FRAGMENT_SHADER;
    {$ifndef OpenGLES}
    ssGeometry: GLStage := GL_GEOMETRY_SHADER;
    {$endif}
    ssCompute:  GLStage := GL_COMPUTE_SHADER;
  else
    GLStage := GL_VERTEX_SHADER;
  end;

  FHandle := glCreateShader(GLStage);
  AnsiSrc := AnsiString(FTranslatedSource);
  SrcPtr := PGLchar(AnsiSrc);
  SrcLen := Length(AnsiSrc);
  glShaderSource(FHandle, 1, @SrcPtr, @SrcLen);
  glCompileShader(FHandle);

  CompileStatus := GL_FALSE;
  glGetShaderiv(FHandle, GL_COMPILE_STATUS, @CompileStatus);
  if CompileStatus = GL_FALSE then
  begin
    LogLen := 0;
    glGetShaderiv(FHandle, GL_INFO_LOG_LENGTH, @LogLen);
    SetLength(ErrorLog, Max(0, LogLen));
    if LogLen > 0 then
      glGetShaderInfoLog(FHandle, LogLen, nil, PGLchar(ErrorLog));
    glDeleteShader(FHandle);
    FHandle := 0;
    raise ERHIShaderCompilationError.CreateFmt('OpenGL Shader (%s) compilation failed:%s%s',
      [ADebugName, LineEnding, String(ErrorLog)]);
  end;
end;

procedure TOpenGLRHIShaderModule.ReleaseDeviceResources;
begin
  if (FHandle <> 0) and (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
    glDeleteShader(FHandle);
  FHandle := 0;
end;

destructor TOpenGLRHIShaderModule.Destroy;
begin
  inherited;
end;

constructor TOpenGLRHIDescriptorSetLayout.Create(const ADevice: TOpenGLRHIDevice;
  const ABindings: array of TRHIDescriptorBindingDesc);
var
  I: Integer;
begin
  inherited Create(ADevice, '');
  SetLength(FBindings, Length(ABindings));
  for I := 0 to High(ABindings) do
    FBindings[I] := ABindings[I];
end;

function TOpenGLRHIDescriptorSetLayout.FindBindingType(const ABinding: Cardinal;
  out AType: TRHIDescriptorType): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FBindings) do
    if FBindings[I].Binding = ABinding then
    begin
      AType := FBindings[I].DescriptorType;
      Exit(True);
    end;
  AType := dtCombinedImageSampler;
  Result := False;
end;

function TOpenGLRHIDescriptorSetLayout.BindingCount: Integer;
begin
  Result := Length(FBindings);
end;

function TOpenGLRHIDescriptorSetLayout.GetBinding(const AIndex: Integer): TRHIDescriptorBindingDesc;
begin
  Result := FBindings[AIndex];
end;

constructor TOpenGLRHIDescriptorSet.Create(const ADevice: TOpenGLRHIDevice;
  const ALayout: TOpenGLRHIDescriptorSetLayout);
begin
  inherited Create(ADevice, '');
  FLayout := ALayout;
end;

procedure TOpenGLRHIDescriptorSet.SetBufferEntry(const ABinding: Cardinal;
  const ABuffer: TOpenGLRHIBuffer; const AOffset, ARange: PtrUInt;
  const AIsStorage: Boolean);
var
  I, Idx: Integer;
  DeclType: TRHIDescriptorType;
  EffStorage, EffDynamic: Boolean;
begin
  EffStorage := AIsStorage;
  EffDynamic := False;
  if (FLayout <> nil) and FLayout.FindBindingType(ABinding, DeclType) then
  begin
    if DeclType = dtStorageBuffer then
      EffStorage := True
    else if DeclType = dtUniformBufferDynamic then
      EffDynamic := True;
  end;

  Idx := -1;
  for I := 0 to High(FBoundBuffers) do
    if FBoundBuffers[I].Binding = ABinding then
    begin
      Idx := I;
      Break;
    end;

  if Idx < 0 then
  begin
    Idx := Length(FBoundBuffers);
    SetLength(FBoundBuffers, Idx + 1);
  end;

  FBoundBuffers[Idx].Binding := ABinding;
  FBoundBuffers[Idx].Buffer := ABuffer;
  FBoundBuffers[Idx].Offset := AOffset;
  if (ARange = 0) and (ABuffer <> nil) then
    FBoundBuffers[Idx].Range := Max(PtrUInt(1), ABuffer.Desc.Size - Min(AOffset, ABuffer.Desc.Size))
  else
    FBoundBuffers[Idx].Range := ARange;
  FBoundBuffers[Idx].IsStorage := EffStorage;
  FBoundBuffers[Idx].IsDynamic := EffDynamic;
end;

procedure TOpenGLRHIDescriptorSet.SetTextureEntry(const ABinding, AArrayElement: Cardinal;
  const ATexture: TOpenGLRHITexture; const ASampler: TOpenGLRHISampler;
  const AMipLevel, AArrayLayer: Cardinal; const AHasSubresource: Boolean;
  const AForceStorageImage: Boolean);
var
  I, Idx: Integer;
  DeclType: TRHIDescriptorType;
  EffStorageImg: Boolean;
begin
  EffStorageImg := AForceStorageImage;
  if not EffStorageImg then
  begin
    if (FLayout <> nil) and FLayout.FindBindingType(ABinding, DeclType) then
      EffStorageImg := (DeclType = dtStorageImage)
    else if (ASampler = nil) and (ATexture <> nil) and (tuStorage in ATexture.Desc.Usage) and
            not (tuSampled in ATexture.Desc.Usage) then
      EffStorageImg := True;
  end;

  Idx := -1;
  for I := 0 to High(FBoundTextures) do
    if (FBoundTextures[I].Binding = ABinding) and
       (FBoundTextures[I].ArrayElement = AArrayElement) then
    begin
      Idx := I;
      Break;
    end;

  if Idx < 0 then
  begin
    Idx := Length(FBoundTextures);
    SetLength(FBoundTextures, Idx + 1);
  end;

  FBoundTextures[Idx].Binding := ABinding;
  FBoundTextures[Idx].ArrayElement := AArrayElement;
  FBoundTextures[Idx].Texture := ATexture;
  FBoundTextures[Idx].Sampler := ASampler;
  FBoundTextures[Idx].MipLevel := AMipLevel;
  FBoundTextures[Idx].ArrayLayer := AArrayLayer;
  FBoundTextures[Idx].HasSubresourceOverride := AHasSubresource;
  FBoundTextures[Idx].IsStorageImage := EffStorageImg;
end;

procedure TOpenGLRHIDescriptorSet.BindUniformBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
begin
  SetBufferEntry(Binding, Buffer as TOpenGLRHIBuffer, Offset, Range, False);
end;

procedure TOpenGLRHIDescriptorSet.BindStorageBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
begin
  SetBufferEntry(Binding, Buffer as TOpenGLRHIBuffer, Offset, Range, True);
end;

procedure TOpenGLRHIDescriptorSet.BindTexture(const Binding: Cardinal;
  const Texture: TRHITexture; const Sampler: TRHISampler);
begin
  SetTextureEntry(Binding, 0, Texture as TOpenGLRHITexture,
    Sampler as TOpenGLRHISampler, 0, 0, False, False);
end;

procedure TOpenGLRHIDescriptorSet.BindTextureAtIndex(const Binding, ArrayElement: Cardinal;
  const Texture: TRHITexture; const Sampler: TRHISampler);
begin
  SetTextureEntry(Binding, ArrayElement, Texture as TOpenGLRHITexture,
    Sampler as TOpenGLRHISampler, 0, 0, False, False);
end;

procedure TOpenGLRHIDescriptorSet.BindTextureSubresource(const Binding: Cardinal;
  const Texture: TRHITexture; const MipLevel, ArrayLayer: Cardinal;
  const Sampler: TRHISampler);
begin
  SetTextureEntry(Binding, 0, Texture as TOpenGLRHITexture,
    Sampler as TOpenGLRHISampler, MipLevel, ArrayLayer, True, False);
end;

procedure TOpenGLRHIDescriptorSet.BindStorageImage(const Binding: Cardinal;
  const Texture: TRHITexture; const MipLevel, ArrayLayer: Cardinal);
begin
  SetTextureEntry(Binding, 0, Texture as TOpenGLRHITexture,
    nil, MipLevel, ArrayLayer, True, True);
end;

procedure TOpenGLRHIDescriptorSet.Update;
begin
  Inc(FUpdateCount);
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIPipeline & TOpenGLRHIComputePipeline                               }
{ ---------------------------------------------------------------------------- }

function TOpenGLRHIPipeline.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

procedure TOpenGLRHIPipeline.EnsurePushConstantCapacity(const AMinBytes: Cardinal);
var
  AllocBytes: Cardinal;
begin
  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) or (AMinBytes = 0) then
    Exit;
  if (FPushConstantUBO <> 0) and (FPushConstantCapacity >= AMinBytes) then
    Exit;

  AllocBytes := Max(Cardinal(256), AMinBytes);
  if FPushConstantUBO = 0 then
    glGenBuffers(1, @FPushConstantUBO);
  glBindBuffer(GL_UNIFORM_BUFFER, FPushConstantUBO);
  glBufferData(GL_UNIFORM_BUFFER, AllocBytes, nil, GL_STREAM_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
  FPushConstantCapacity := AllocBytes;
end;

constructor TOpenGLRHIPipeline.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIGraphicsPipelineDesc);
var
  LinkStatus, LogLen: TGLint;
  ErrorLog: AnsiString;
  I: Integer;
begin
  inherited Create(ADevice, ADesc);
  FPrimitiveMode := RHIToGLTopology(ADesc.Topology);

  if (ADevice = nil) or (not ADevice.HasLiveGLContext) then
  begin
    FProgramHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FProgramHandle = 0 then
      FProgramHandle := 1;
    FVAOHandle := FProgramHandle;
    Exit;
  end;

  FProgramHandle := glCreateProgram();
  if ADesc.VertexShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.VertexShader as TOpenGLRHIShaderModule).Handle);
  if ADesc.FragmentShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.FragmentShader as TOpenGLRHIShaderModule).Handle);
  if ADesc.GeometryShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.GeometryShader as TOpenGLRHIShaderModule).Handle);

  for I := 0 to High(ADesc.VertexInput.Attributes) do
    if ADesc.VertexInput.Attributes[I].AttributeName <> '' then
      glBindAttribLocation(FProgramHandle, ADesc.VertexInput.Attributes[I].Location,
        PGLchar(AnsiString(ADesc.VertexInput.Attributes[I].AttributeName)));

  glLinkProgram(FProgramHandle);
  LinkStatus := GL_FALSE;
  glGetProgramiv(FProgramHandle, GL_LINK_STATUS, @LinkStatus);
  if LinkStatus = GL_FALSE then
  begin
    LogLen := 0;
    glGetProgramiv(FProgramHandle, GL_INFO_LOG_LENGTH, @LogLen);
    SetLength(ErrorLog, Max(0, LogLen));
    if LogLen > 0 then
      glGetProgramInfoLog(FProgramHandle, LogLen, nil, PGLchar(ErrorLog));
    glDeleteProgram(FProgramHandle);
    FProgramHandle := 0;
    raise ERHIShaderCompilationError.CreateFmt('OpenGL Program (%s) link failed:%s%s',
      [ADesc.DebugName, LineEnding, String(ErrorLog)]);
  end;

  glGenVertexArrays(1, @FVAOHandle);
  if ADesc.PushConstantSize > 0 then
    EnsurePushConstantCapacity(ADesc.PushConstantSize);
end;

procedure TOpenGLRHIPipeline.ReleaseDeviceResources;
begin
  if (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
  begin
    if FPushConstantUBO <> 0 then
      glDeleteBuffers(1, @FPushConstantUBO);
    if FVAOHandle <> 0 then
      glDeleteVertexArrays(1, @FVAOHandle);
    if FProgramHandle <> 0 then
      glDeleteProgram(FProgramHandle);
  end;
  FPushConstantUBO := 0;
  FPushConstantCapacity := 0;
  FVAOHandle := 0;
  FProgramHandle := 0;
end;

destructor TOpenGLRHIPipeline.Destroy;
begin
  inherited;
end;

function TOpenGLRHIComputePipeline.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

procedure TOpenGLRHIComputePipeline.EnsurePushConstantCapacity(const AMinBytes: Cardinal);
var
  AllocBytes: Cardinal;
begin
  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) or (AMinBytes = 0) then
    Exit;
  if (FPushConstantUBO <> 0) and (FPushConstantCapacity >= AMinBytes) then
    Exit;

  AllocBytes := Max(Cardinal(256), AMinBytes);
  if FPushConstantUBO = 0 then
    glGenBuffers(1, @FPushConstantUBO);
  glBindBuffer(GL_UNIFORM_BUFFER, FPushConstantUBO);
  glBufferData(GL_UNIFORM_BUFFER, AllocBytes, nil, GL_STREAM_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
  FPushConstantCapacity := AllocBytes;
end;

constructor TOpenGLRHIComputePipeline.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIComputePipelineDesc);
var
  LinkStatus, LogLen: TGLint;
  WgSizes: array[0..2] of TGLint;
  ErrorLog: AnsiString;
begin
  inherited Create(ADevice, ADesc);
  FWorkGroupSize := Vector3Cardinal(1, 1, 1);

  if (ADevice = nil) or (not ADevice.HasLiveGLContext) then
  begin
    FProgramHandle := TGLuint(UniqueId and $7FFFFFFF);
    if FProgramHandle = 0 then
      FProgramHandle := 1;
    Exit;
  end;

  if ADesc.ComputeShader = nil then
    raise ERHIShaderCompilationError.CreateFmt(
      'OpenGL Compute Pipeline (%s) requires a non-nil ComputeShader module',
      [ADesc.DebugName]);

  FProgramHandle := glCreateProgram();
  glAttachShader(FProgramHandle, (ADesc.ComputeShader as TOpenGLRHIShaderModule).Handle);
  glLinkProgram(FProgramHandle);

  LinkStatus := GL_FALSE;
  glGetProgramiv(FProgramHandle, GL_LINK_STATUS, @LinkStatus);
  if LinkStatus = GL_FALSE then
  begin
    LogLen := 0;
    glGetProgramiv(FProgramHandle, GL_INFO_LOG_LENGTH, @LogLen);
    SetLength(ErrorLog, Max(0, LogLen));
    if LogLen > 0 then
      glGetProgramInfoLog(FProgramHandle, LogLen, nil, PGLchar(ErrorLog));
    glDeleteProgram(FProgramHandle);
    FProgramHandle := 0;
    raise ERHIShaderCompilationError.CreateFmt('OpenGL Compute Program (%s) link failed:%s%s',
      [ADesc.DebugName, LineEnding, String(ErrorLog)]);
  end;

  WgSizes[0] := 1;
  WgSizes[1] := 1;
  WgSizes[2] := 1;
  glGetProgramiv(FProgramHandle, GL_COMPUTE_WORK_GROUP_SIZE, @WgSizes[0]);
  FWorkGroupSize := Vector3Cardinal(
    Max(1, WgSizes[0]),
    Max(1, WgSizes[1]),
    Max(1, WgSizes[2]));

  if ADesc.PushConstantSize > 0 then
    EnsurePushConstantCapacity(ADesc.PushConstantSize);
end;

procedure TOpenGLRHIComputePipeline.ReleaseDeviceResources;
begin
  if (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
  begin
    if FPushConstantUBO <> 0 then
      glDeleteBuffers(1, @FPushConstantUBO);
    if FProgramHandle <> 0 then
      glDeleteProgram(FProgramHandle);
  end;
  FPushConstantUBO := 0;
  FPushConstantCapacity := 0;
  FProgramHandle := 0;
end;

destructor TOpenGLRHIComputePipeline.Destroy;
begin
  inherited;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIQueryPool                                                          }
{ ---------------------------------------------------------------------------- }

function TOpenGLRHIQueryPool.OpenGLDevice: TOpenGLRHIDevice;
begin
  Result := Device as TOpenGLRHIDevice;
end;

constructor TOpenGLRHIQueryPool.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIQueryPoolDesc);
var
  Count, I: Cardinal;
begin
  inherited Create(ADevice, ADesc);
  Count := Max(Cardinal(1), ADesc.QueryCount);
  SetLength(FQueries, Count);
  SetLength(FActiveTargets, Count);
  SetLength(FQueryWritten, Count);
  SetLength(FSimulatedResults, Count);

  if (ADevice <> nil) and ADevice.HasLiveGLContext then
    glGenQueries(Count, @FQueries[0])
  else
    for I := 0 to Count - 1 do
      FQueries[I] := I + 1;
end;

procedure TOpenGLRHIQueryPool.ReleaseDeviceResources;
begin
  if (Length(FQueries) > 0) and (FQueries[0] <> 0) and
     (OpenGLDevice <> nil) and OpenGLDevice.HasLiveGLContext then
    glDeleteQueries(Length(FQueries), @FQueries[0]);
  SetLength(FQueries, 0);
  SetLength(FActiveTargets, 0);
  SetLength(FQueryWritten, 0);
  SetLength(FSimulatedResults, 0);
end;

destructor TOpenGLRHIQueryPool.Destroy;
begin
  inherited;
end;

function TOpenGLRHIQueryPool.GetResults(const FirstQuery, QueryCount: Cardinal;
  out Results: array of UInt64; const WaitForResults: Boolean): Boolean;
var
  I, Idx: Cardinal;
  Avail: TGLuint;
  {$ifndef OpenGLES}
  Val64: TGLuint64;
  {$endif}
  Val32: TGLuint;
begin
  Result := False;
  if QueryCount = 0 then
    Exit(True);
  if (FirstQuery + QueryCount > Cardinal(Length(FQueries))) or
     (Cardinal(Length(Results)) < QueryCount) then
    Exit(False);

  for I := 0 to QueryCount - 1 do
    if not FQueryWritten[FirstQuery + I] then
      Exit(False);

  if (OpenGLDevice = nil) or (not OpenGLDevice.HasLiveGLContext) then
  begin
    for I := 0 to QueryCount - 1 do
      Results[I] := FSimulatedResults[FirstQuery + I];
    Exit(True);
  end;

  if not WaitForResults then
  begin
    for I := 0 to QueryCount - 1 do
    begin
      Idx := FirstQuery + I;
      Avail := 0;
      glGetQueryObjectuiv(FQueries[Idx], GL_QUERY_RESULT_AVAILABLE, @Avail);
      if Avail = GL_FALSE then
        Exit(False);
    end;
  end;

  for I := 0 to QueryCount - 1 do
  begin
    Idx := FirstQuery + I;
    {$ifndef OpenGLES}
    if Assigned(glGetQueryObjectui64v) then
    begin
      Val64 := 0;
      glGetQueryObjectui64v(FQueries[Idx], GL_QUERY_RESULT, @Val64);
      Results[I] := UInt64(Val64);
      Continue;
    end;
    {$endif}
    Val32 := 0;
    glGetQueryObjectuiv(FQueries[Idx], GL_QUERY_RESULT, @Val32);
    Results[I] := UInt64(Val32);
  end;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHICommandList                                                        }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHICommandList.Create(const ADevice: TOpenGLRHIDevice;
  const ALevel: TRHICommandListLevel);
begin
  inherited Create(ALevel);
  FDevice := ADevice;
  FCurrentPrimitiveMode := GL_TRIANGLES;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    glGenFramebuffers(1, @FScratchFBO);
    glGenFramebuffers(1, @FResolveFBO);
  end;
end;

destructor TOpenGLRHICommandList.Destroy;
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    if FScratchFBO <> 0 then
      glDeleteFramebuffers(1, @FScratchFBO);
    if FResolveFBO <> 0 then
      glDeleteFramebuffers(1, @FResolveFBO);
  end;
  inherited;
end;

procedure TOpenGLRHICommandList.BeginRecording;
begin
  FIsRecording := True;
  FIsRendering := False;
  FCurrentPipeline := nil;
  FCurrentComputePipeline := nil;
  FCurrentIndexBuffer := nil;
  FDebugGroupDepth := 0;
end;

procedure TOpenGLRHICommandList.EndRecording;
begin
  if FIsRendering then
    EndRendering;
  while FDebugGroupDepth > 0 do
    PopDebugGroup;
  FIsRecording := False;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    if FScissorEnabled then
    begin
      glDisable(GL_SCISSOR_TEST);
      FScissorEnabled := False;
    end;
    glBindVertexArray(0);
    glUseProgram(0);
  end;
end;

procedure TOpenGLRHICommandList.EmitMemoryBarrierForState(const NewState: TRHIResourceState);
{$ifndef OpenGLES}
var
  Bits: TGLbitfield;
{$endif}
begin
  Inc(FBarrierCount);
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  {$ifndef OpenGLES}
  if Assigned(glMemoryBarrier) then
  begin
    case NewState of
      rsShaderRead:
        Bits := GL_TEXTURE_FETCH_BARRIER_BIT or GL_SHADER_IMAGE_ACCESS_BARRIER_BIT or
                GL_SHADER_STORAGE_BARRIER_BIT or GL_UNIFORM_BARRIER_BIT;
      rsUnorderedAccess:
        Bits := GL_SHADER_IMAGE_ACCESS_BARRIER_BIT or GL_SHADER_STORAGE_BARRIER_BIT;
      rsIndirectRead:
        Bits := GL_COMMAND_BARRIER_BIT or GL_SHADER_STORAGE_BARRIER_BIT or
                GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT or GL_ELEMENT_ARRAY_BARRIER_BIT;
      rsColorAttachment, rsDepthStencilWrite, rsDepthStencilRead:
        Bits := GL_FRAMEBUFFER_BARRIER_BIT or GL_TEXTURE_FETCH_BARRIER_BIT;
      rsTransferSrc, rsTransferDst:
        Bits := GL_BUFFER_UPDATE_BARRIER_BIT or GL_FRAMEBUFFER_BARRIER_BIT or
                GL_SHADER_IMAGE_ACCESS_BARRIER_BIT;
    else
      Bits := 0;
    end;
    if Bits <> 0 then
      glMemoryBarrier(Bits);
  end;
  {$endif}
end;

procedure TOpenGLRHICommandList.TransitionTexture(const Texture: TRHITexture;
  const NewState: TRHIResourceState);
begin
  if Texture = nil then Exit;
  EmitMemoryBarrierForState(NewState);
  Texture.CurrentState := NewState;
end;

procedure TOpenGLRHICommandList.EmitAliasingBarrier(const OldTexture, NewTexture: TRHITexture);
begin
  Inc(FAliasingBarrierCount);
  Inc(FBarrierCount);
  if NewTexture <> nil then
    NewTexture.CurrentState := rsUndefined;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    {$ifndef OpenGLES}
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(GL_ALL_BARRIER_BITS);
    {$endif}
  end;
  if OldTexture = nil then ; { Silence unused hint }
end;

procedure TOpenGLRHICommandList.GenerateMipmaps(const Texture: TRHITexture);
var
  GLTex: TOpenGLRHITexture;
begin
  if Texture = nil then Exit;
  GLTex := Texture as TOpenGLRHITexture;
  if GLTex.IsDefaultBackbuffer then Exit;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    glBindTexture(GLTex.Target, GLTex.Handle);
    glGenerateMipmap(GLTex.Target);
    glBindTexture(GLTex.Target, 0);
  end;
end;

procedure TOpenGLRHICommandList.BeginRendering(const Info: TRHIRenderingInfo);
var
  ClearMask: TGLbitfield;
  ColorTex, DepthTex: TOpenGLRHITexture;
  DepthAttachmentPoint: TGLenum;
  DrawBufs: array[0..7] of TGLenum;
  UseDefaultFBO: Boolean;
  I, AttCount: Integer;
begin
  FIsRendering := True;
  FCurrentPassHasCommands := False;
  FActiveRenderingInfo := Info;
  FActiveRenderingInfo.ColorAttachments := Copy(Info.ColorAttachments);
  FActiveRenderingHasColor := (Length(Info.ColorAttachments) > 0) and
    (Info.ColorAttachments[0].Texture <> nil);
  if FActiveRenderingHasColor then
  begin
    FActiveRenderingColorFormat := Info.ColorAttachments[0].Texture.Desc.Format;
    FActiveRenderingSampleCount := Max(Cardinal(1),
      Info.ColorAttachments[0].Texture.Desc.SampleCount);
  end else
  begin
    FActiveRenderingColorFormat := pfUndefined;
    FActiveRenderingSampleCount := 1;
  end;
  FActiveRenderingHasDepth := Info.HasDepthAttachment and
    (Info.DepthAttachment.Texture <> nil);
  if FActiveRenderingHasDepth then
  begin
    FActiveRenderingDepthFormat := Info.DepthAttachment.Texture.Desc.Format;
    if not FActiveRenderingHasColor then
      FActiveRenderingSampleCount := Max(Cardinal(1),
        Info.DepthAttachment.Texture.Desc.SampleCount);
  end else
    FActiveRenderingDepthFormat := pfUndefined;

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  UseDefaultFBO :=
    ((Length(Info.ColorAttachments) > 0) and
     (Info.ColorAttachments[0].Texture <> nil) and
     (Info.ColorAttachments[0].Texture as TOpenGLRHITexture).IsDefaultBackbuffer) or
    ((Length(Info.ColorAttachments) = 0) and
     Info.HasDepthAttachment and
     (Info.DepthAttachment.Texture <> nil) and
     (Info.DepthAttachment.Texture as TOpenGLRHITexture).IsDefaultBackbuffer);

  if UseDefaultFBO then
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
  else
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FScratchFBO);
    AttCount := Min(8, Length(Info.ColorAttachments));
    for I := 0 to AttCount - 1 do
    begin
      ColorTex := Info.ColorAttachments[I].Texture as TOpenGLRHITexture;
      DrawBufs[I] := GL_COLOR_ATTACHMENT0 + Cardinal(I);
      if ColorTex <> nil then
      begin
        if ColorTex.Desc.Dimension = tdCube then
          glFramebufferTexture2D(GL_FRAMEBUFFER, DrawBufs[I],
            GL_TEXTURE_CUBE_MAP_POSITIVE_X + Min(Cardinal(5), Info.ColorAttachments[I].ArrayLayer),
            ColorTex.Handle, Info.ColorAttachments[I].MipLevel)
        else if ColorTex.Desc.Dimension in [td2DArray, td3D] then
          glFramebufferTextureLayer(GL_FRAMEBUFFER, DrawBufs[I],
            ColorTex.Handle, Info.ColorAttachments[I].MipLevel,
            Info.ColorAttachments[I].ArrayLayer)
        else
          glFramebufferTexture2D(GL_FRAMEBUFFER, DrawBufs[I],
            ColorTex.Target, ColorTex.Handle, Info.ColorAttachments[I].MipLevel);
      end else
        glFramebufferTexture2D(GL_FRAMEBUFFER, DrawBufs[I], GL_TEXTURE_2D, 0, 0);
    end;
    for I := AttCount to 3 do
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + Cardinal(I), GL_TEXTURE_2D, 0, 0);

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0, 0);

    if Info.HasDepthAttachment and (Info.DepthAttachment.Texture <> nil) then
    begin
      DepthTex := Info.DepthAttachment.Texture as TOpenGLRHITexture;
      if DepthTex.Desc.Format in [pfD24_UNorm_S8_UInt, pfD32_Float_S8_UInt] then
        DepthAttachmentPoint := GL_DEPTH_STENCIL_ATTACHMENT
      else
        DepthAttachmentPoint := GL_DEPTH_ATTACHMENT;

      if DepthTex.Desc.Dimension = tdCube then
        glFramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentPoint,
          GL_TEXTURE_CUBE_MAP_POSITIVE_X + Min(Cardinal(5), Info.DepthAttachment.ArrayLayer),
          DepthTex.Handle, Info.DepthAttachment.MipLevel)
      else if DepthTex.Desc.Dimension in [td2DArray, td3D] then
        glFramebufferTextureLayer(GL_FRAMEBUFFER, DepthAttachmentPoint,
          DepthTex.Handle, Info.DepthAttachment.MipLevel,
          Info.DepthAttachment.ArrayLayer)
      else
        glFramebufferTexture2D(GL_FRAMEBUFFER, DepthAttachmentPoint,
          DepthTex.Target, DepthTex.Handle, Info.DepthAttachment.MipLevel);
    end;

    {$ifndef OpenGLES}
    if AttCount = 0 then
    begin
      glDrawBuffer(GL_NONE);
      glReadBuffer(GL_NONE);
    end else
    begin
      if (AttCount > 1) and Assigned(glDrawBuffers) then
        glDrawBuffers(AttCount, @DrawBufs[0])
      else
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
      glReadBuffer(GL_COLOR_ATTACHMENT0);
    end;
    {$endif}
  end;

  if FScissorEnabled then
    glDisable(GL_SCISSOR_TEST);

  ClearMask := 0;
  if (Length(Info.ColorAttachments) > 0) and (Info.ColorAttachments[0].LoadOp = loClear) then
  begin
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glClearColor(
      Info.ColorAttachments[0].ClearColor.X,
      Info.ColorAttachments[0].ClearColor.Y,
      Info.ColorAttachments[0].ClearColor.Z,
      Info.ColorAttachments[0].ClearColor.W);
    ClearMask := ClearMask or GL_COLOR_BUFFER_BIT;
  end;

  if Info.HasDepthAttachment and (Info.DepthAttachment.LoadOp = loClear) then
  begin
    glDepthMask(GL_TRUE);
    {$ifdef OpenGLES}
    glClearDepthf(Info.DepthAttachment.ClearDepth);
    {$else}
    glClearDepth(Info.DepthAttachment.ClearDepth);
    {$endif}
    ClearMask := ClearMask or GL_DEPTH_BUFFER_BIT;
    if FActiveRenderingDepthFormat in [pfD24_UNorm_S8_UInt, pfD32_Float_S8_UInt] then
    begin
      glStencilMask($FF);
      glClearStencil(Info.DepthAttachment.ClearStencil);
      ClearMask := ClearMask or GL_STENCIL_BUFFER_BIT;
    end;
  end;

  if ClearMask <> 0 then
    glClear(ClearMask);

  if FScissorEnabled then
    glEnable(GL_SCISSOR_TEST);

  if (Info.RenderArea.Width > 0) and (Info.RenderArea.Height > 0) then
  begin
    SetViewport(FloatRectangle(Info.RenderArea), 0.0, 1.0);
    SetScissor(Info.RenderArea);
  end;
end;

procedure TOpenGLRHICommandList.EndRendering;
var
  I, W, H: Integer;
  MsaaTex, ResolveTex: TOpenGLRHITexture;
begin
  if FIsRendering and (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    { Perform single-pass MSAA color resolve via glBlitFramebuffer when ResolveTexture is bound }
    for I := 0 to High(FActiveRenderingInfo.ColorAttachments) do
      if (FActiveRenderingInfo.ColorAttachments[I].Texture <> nil) and
         (FActiveRenderingInfo.ColorAttachments[I].ResolveTexture <> nil) and
         (FActiveRenderingInfo.ColorAttachments[I].Texture.Desc.SampleCount > 1) then
      begin
        MsaaTex := FActiveRenderingInfo.ColorAttachments[I].Texture as TOpenGLRHITexture;
        ResolveTex := FActiveRenderingInfo.ColorAttachments[I].ResolveTexture as TOpenGLRHITexture;
        W := Max(1, Integer(ResolveTex.Desc.Width));
        H := Max(1, Integer(ResolveTex.Desc.Height));
        if FScissorEnabled then
          glDisable(GL_SCISSOR_TEST);
        glBindFramebuffer(GL_READ_FRAMEBUFFER, FScratchFBO);
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
          MsaaTex.Target, MsaaTex.Handle, 0);
        if ResolveTex.IsDefaultBackbuffer then
          glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)
        else
        begin
          glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FResolveFBO);
          glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
            ResolveTex.Target, ResolveTex.Handle,
            FActiveRenderingInfo.ColorAttachments[I].ResolveMipLevel);
        end;
        glBlitFramebuffer(0, 0, W, H, 0, 0, W, H, GL_COLOR_BUFFER_BIT, GL_NEAREST);
        if FScissorEnabled then
          glEnable(GL_SCISSOR_TEST);
      end;

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
  end;
  FIsRendering := False;
  FCurrentPassHasCommands := False;
end;

procedure TOpenGLRHICommandList.ClearAttachments(const ClearColorBuf: Boolean;
  const ClearColor: TCastleColor; const ClearDepthBuf: Boolean;
  const ClearDepth: Single; const ClearStencilBuf: Boolean;
  const ClearStencil: Byte);
var
  ClearMask: TGLbitfield;
begin
  if FIsRendering then
    FCurrentPassHasCommands := True;
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  ClearMask := 0;
  if ClearColorBuf then
  begin
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glClearColor(ClearColor.X, ClearColor.Y, ClearColor.Z, ClearColor.W);
    ClearMask := ClearMask or GL_COLOR_BUFFER_BIT;
  end;
  if ClearDepthBuf then
  begin
    glDepthMask(GL_TRUE);
    {$ifdef OpenGLES}
    glClearDepthf(ClearDepth);
    {$else}
    glClearDepth(ClearDepth);
    {$endif}
    ClearMask := ClearMask or GL_DEPTH_BUFFER_BIT;
  end;
  if ClearStencilBuf then
  begin
    glStencilMask($FF);
    glClearStencil(ClearStencil);
    ClearMask := ClearMask or GL_STENCIL_BUFFER_BIT;
  end;
  if ClearMask <> 0 then
    glClear(ClearMask);
end;

procedure TOpenGLRHICommandList.SetViewport(const ViewportRect: TFloatRectangle;
  const MinDepth, MaxDepth: Single);
begin
  FDynamicViewportRect := ViewportRect;
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;
  glViewport(Round(ViewportRect.Left), Round(ViewportRect.Bottom),
    Max(0, Round(ViewportRect.Width)), Max(0, Round(ViewportRect.Height)));
  {$ifdef OpenGLES}
  glDepthRangef(MinDepth, MaxDepth);
  {$else}
  glDepthRange(MinDepth, MaxDepth);
  {$endif}
end;

procedure TOpenGLRHICommandList.SetScissor(const ScissorRect: TRectangle);
begin
  FDynamicScissorRect := ScissorRect;
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;
  if not FScissorEnabled then
  begin
    glEnable(GL_SCISSOR_TEST);
    FScissorEnabled := True;
  end;
  glScissor(ScissorRect.Left, ScissorRect.Bottom,
    Max(0, ScissorRect.Width), Max(0, ScissorRect.Height));
end;

procedure TOpenGLRHICommandList.SetDepthBias(const ConstantFactor, SlopeFactor: Single);
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glPolygonOffset(SlopeFactor, ConstantFactor);
end;

procedure TOpenGLRHICommandList.SetLineWidth(const LineWidth: Single);
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glLineWidth(Max(1.0, LineWidth));
end;

procedure TOpenGLRHICommandList.SetBlendConstants(const ABlendConstants: TVector4);
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext and
     ((GLFeatures = nil) or GLFeatures.BlendConstant) then
    glBlendColor(ABlendConstants.X, ABlendConstants.Y, ABlendConstants.Z, ABlendConstants.W);
end;

procedure TOpenGLRHICommandList.SetCullMode(const CullMode: TRHICullMode);
begin
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;
  if CullMode = cmNone then
    glDisable(GL_CULL_FACE)
  else
  begin
    glEnable(GL_CULL_FACE);
    if CullMode = cmBack then
      glCullFace(GL_BACK)
    else
      glCullFace(GL_FRONT);
  end;
end;

procedure TOpenGLRHICommandList.SetFrontFace(const FrontFace: TRHIFrontFace);
begin
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;
  if FrontFace = ffCounterClockwise then
    glFrontFace(GL_CCW)
  else
    glFrontFace(GL_CW);
end;

procedure TOpenGLRHICommandList.SetDepthTestEnable(const Enable: Boolean);
begin
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;
  if Enable then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TOpenGLRHICommandList.SetDepthWriteEnable(const Enable: Boolean);
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glDepthMask(TGLboolean(Enable));
end;

procedure TOpenGLRHICommandList.SetDepthCompareOp(const CompareOp: TRHICompareOp);
begin
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glDepthFunc(RHIToGLCompareOp(CompareOp));
end;

procedure TOpenGLRHICommandList.SetPrimitiveTopology(const Topology: TRHIPrimitiveTopology);
begin
  FCurrentPrimitiveMode := RHIToGLTopology(Topology);
end;

procedure TOpenGLRHICommandList.ApplyPipelineState(const NewPipeline: TOpenGLRHIPipeline);
var
  D: TRHIGraphicsPipelineDesc;
  WriteMask: Byte;
begin
  D := NewPipeline.Desc;
  FCurrentPrimitiveMode := NewPipeline.PrimitiveMode;

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  glUseProgram(NewPipeline.ProgramHandle);
  glBindVertexArray(NewPipeline.VAOHandle);

  SetCullMode(D.Rasterizer.CullMode);
  SetFrontFace(D.Rasterizer.FrontFace);

  {$ifndef OpenGLES}
  case D.Rasterizer.PolygonMode of
    pmFill:  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    pmLine:  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    pmPoint: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
  end;
  {$endif}

  if D.Rasterizer.DepthBiasEnable then
    glEnable(GL_POLYGON_OFFSET_FILL)
  else
    glDisable(GL_POLYGON_OFFSET_FILL);

  if D.DepthStencil.DepthTestEnable then
  begin
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(RHIToGLCompareOp(D.DepthStencil.DepthCompareOp));
  end else
    glDisable(GL_DEPTH_TEST);

  glDepthMask(TGLboolean(D.DepthStencil.DepthWriteEnable));

  if D.DepthStencil.StencilTestEnable then
  begin
    glEnable(GL_STENCIL_TEST);
    glStencilFuncSeparate(GL_FRONT,
      RHIToGLCompareOp(D.DepthStencil.FrontStencil.CompareOp),
      TGLint(D.DepthStencil.FrontStencil.Reference),
      D.DepthStencil.FrontStencil.CompareMask);
    glStencilOpSeparate(GL_FRONT,
      RHIToGLStencilOp(D.DepthStencil.FrontStencil.FailOp),
      RHIToGLStencilOp(D.DepthStencil.FrontStencil.DepthFailOp),
      RHIToGLStencilOp(D.DepthStencil.FrontStencil.PassOp));
    glStencilMaskSeparate(GL_FRONT, D.DepthStencil.FrontStencil.WriteMask);

    glStencilFuncSeparate(GL_BACK,
      RHIToGLCompareOp(D.DepthStencil.BackStencil.CompareOp),
      TGLint(D.DepthStencil.BackStencil.Reference),
      D.DepthStencil.BackStencil.CompareMask);
    glStencilOpSeparate(GL_BACK,
      RHIToGLStencilOp(D.DepthStencil.BackStencil.FailOp),
      RHIToGLStencilOp(D.DepthStencil.BackStencil.DepthFailOp),
      RHIToGLStencilOp(D.DepthStencil.BackStencil.PassOp));
    glStencilMaskSeparate(GL_BACK, D.DepthStencil.BackStencil.WriteMask);
  end else
    glDisable(GL_STENCIL_TEST);

  if D.BlendState.BlendEnable then
  begin
    glEnable(GL_BLEND);
    glBlendFuncSeparate(
      RHIToGLBlendFactor(D.BlendState.SrcColorBlendFactor),
      RHIToGLBlendFactor(D.BlendState.DstColorBlendFactor),
      RHIToGLBlendFactor(D.BlendState.SrcAlphaBlendFactor),
      RHIToGLBlendFactor(D.BlendState.DstAlphaBlendFactor));
    glBlendEquationSeparate(
      RHIToGLBlendOp(D.BlendState.ColorBlendOp),
      RHIToGLBlendOp(D.BlendState.AlphaBlendOp));
  end else
    glDisable(GL_BLEND);

  if D.BlendState.DisableColorWrites then
    WriteMask := 0
  else if D.BlendState.ColorWriteMask <> 0 then
    WriteMask := D.BlendState.ColorWriteMask
  else
    WriteMask := $0F;

  glColorMask(
    TGLboolean((WriteMask and 1) <> 0),
    TGLboolean((WriteMask and 2) <> 0),
    TGLboolean((WriteMask and 4) <> 0),
    TGLboolean((WriteMask and 8) <> 0));
end;

procedure TOpenGLRHICommandList.BindPipeline(const Pipeline: TRHIPipeline);
var
  GLPipe: TOpenGLRHIPipeline;
begin
  if Pipeline = nil then Exit;
  GLPipe := Pipeline as TOpenGLRHIPipeline;
  FCurrentComputePipeline := nil;
  ApplyPipelineState(GLPipe);
  FCurrentPipeline := GLPipe;
  if (FCurrentIndexBuffer <> nil) and (FDevice <> nil) and FDevice.HasLiveGLContext then
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);
end;

procedure TOpenGLRHICommandList.ApplyDescriptorSetInternal(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet; const DynamicOffsets: array of Cardinal);
var
  GLSet: TOpenGLRHIDescriptorSet;
  I, DynIdx: Integer;
  Slot, ImgSlot: Cardinal;
  ActualOffset, ActualRange: PtrUInt;
  TargetEnum: TGLenum;
  TexEntry: TOpenGLBoundTexture;
  IsLayered: TGLboolean;
begin
  if (DescriptorSet = nil) or (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  GLSet := DescriptorSet as TOpenGLRHIDescriptorSet;
  DynIdx := 0;

  { 1. Bind Uniform Buffers (GL_UNIFORM_BUFFER) & Shader Storage Buffers (GL_SHADER_STORAGE_BUFFER) }
  for I := 0 to High(GLSet.FBoundBuffers) do
    if GLSet.FBoundBuffers[I].Buffer <> nil then
    begin
      Slot := SetIndex * GL_DESCRIPTOR_SET_SLOT_STRIDE + GLSet.FBoundBuffers[I].Binding;
      ActualOffset := GLSet.FBoundBuffers[I].Offset;
      if (DynIdx <= High(DynamicOffsets)) and
         (GLSet.FBoundBuffers[I].IsDynamic or (High(DynamicOffsets) >= 0)) then
      begin
        Inc(ActualOffset, DynamicOffsets[DynIdx]);
        Inc(DynIdx);
      end;

      ActualRange := GLSet.FBoundBuffers[I].Range;
      if ActualRange = 0 then
        ActualRange := Max(PtrUInt(1), GLSet.FBoundBuffers[I].Buffer.Desc.Size);

      if GLSet.FBoundBuffers[I].IsStorage then
        TargetEnum := GL_SHADER_STORAGE_BUFFER
      else
        TargetEnum := GL_UNIFORM_BUFFER;

      if GLSet.FBoundBuffers[I].Buffer.FIsMappedForWrite and
         (not GLSet.FBoundBuffers[I].Buffer.FMappedUploadedViaSubData) then
        GLSet.FBoundBuffers[I].Buffer.SyncMappedToGL(ActualOffset, ActualRange);

      glBindBufferRange(TargetEnum, Slot,
        GLSet.FBoundBuffers[I].Buffer.Handle, ActualOffset, ActualRange);
    end;

  { 2. Bind Combined Image Samplers (glBindTexture + glBindSampler) & Storage Images (glBindImageTexture) }
  for I := 0 to High(GLSet.FBoundTextures) do
  begin
    TexEntry := GLSet.FBoundTextures[I];
    if TexEntry.Texture = nil then
      Continue;

    Slot := SetIndex * GL_DESCRIPTOR_SET_SLOT_STRIDE + TexEntry.Binding + TexEntry.ArrayElement;
    if TexEntry.IsStorageImage then
    begin
      {$ifndef OpenGLES}
      if Assigned(glBindImageTexture) then
      begin
        ImgSlot := Slot;
        if (FDevice.MaxImageUnits > 0) and (ImgSlot >= FDevice.MaxImageUnits) then
          ImgSlot := ImgSlot mod FDevice.MaxImageUnits;
        if (not TexEntry.HasSubresourceOverride) and
           (TexEntry.Texture.Desc.Dimension in [tdCube, td2DArray, td3D]) then
          IsLayered := GL_TRUE
        else
          IsLayered := GL_FALSE;
        glBindImageTexture(ImgSlot, TexEntry.Texture.Handle,
          TGLint(TexEntry.MipLevel), IsLayered, TGLint(TexEntry.ArrayLayer),
          GL_READ_WRITE, TexEntry.Texture.ImageUnitFormat);
      end;
      {$endif}
    end else
    begin
      glActiveTexture(GL_TEXTURE0 + Slot);
      glBindTexture(TexEntry.Texture.Target, TexEntry.Texture.Handle);
      if TexEntry.Sampler <> nil then
        glBindSampler(Slot, TexEntry.Sampler.Handle)
      else
        glBindSampler(Slot, 0);
    end;
  end;
end;

procedure TOpenGLRHICommandList.BindDescriptorSet(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet; const DynamicOffsets: array of Cardinal);
begin
  ApplyDescriptorSetInternal(SetIndex, DescriptorSet, DynamicOffsets);
end;

procedure TOpenGLRHICommandList.PushConstants(const StageFlags: TRHIShaderStageFlags;
  const Offset, Size: Cardinal; const Data);
var
  TargetUBO: TGLuint;
begin
  if (Size = 0) or (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  TargetUBO := 0;
  if (ssCompute in StageFlags) and (FCurrentComputePipeline <> nil) and
     ((FCurrentPipeline = nil) or (StageFlags = [ssCompute])) then
  begin
    FCurrentComputePipeline.EnsurePushConstantCapacity(Offset + Size);
    TargetUBO := FCurrentComputePipeline.PushConstantUBO;
  end else
  if FCurrentPipeline <> nil then
  begin
    FCurrentPipeline.EnsurePushConstantCapacity(Offset + Size);
    TargetUBO := FCurrentPipeline.PushConstantUBO;
  end else
  if FCurrentComputePipeline <> nil then
  begin
    FCurrentComputePipeline.EnsurePushConstantCapacity(Offset + Size);
    TargetUBO := FCurrentComputePipeline.PushConstantUBO;
  end;

  if TargetUBO <> 0 then
  begin
    glBindBuffer(GL_UNIFORM_BUFFER, TargetUBO);
    glBufferSubData(GL_UNIFORM_BUFFER, Offset, Size, @Data);
    glBindBufferBase(GL_UNIFORM_BUFFER, GL_PUSH_CONSTANT_UBO_BINDING, TargetUBO);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
  end;
end;

procedure TOpenGLRHICommandList.BindComputePipeline(const Pipeline: TRHIComputePipeline);
var
  GLComp: TOpenGLRHIComputePipeline;
begin
  if Pipeline = nil then Exit;
  GLComp := Pipeline as TOpenGLRHIComputePipeline;
  FCurrentComputePipeline := GLComp;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glUseProgram(GLComp.ProgramHandle);
end;

procedure TOpenGLRHICommandList.BindComputeDescriptorSet(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet; const DynamicOffsets: array of Cardinal);
begin
  ApplyDescriptorSetInternal(SetIndex, DescriptorSet, DynamicOffsets);
end;

procedure TOpenGLRHICommandList.Dispatch(const GroupCountX, GroupCountY, GroupCountZ: Cardinal);
begin
  Inc(FComputeDispatchCount);
  if FDevice <> nil then
    Inc(FDevice.FComputeDispatchCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  {$ifndef OpenGLES}
  if Assigned(glDispatchCompute) then
  begin
    glDispatchCompute(Max(1, GroupCountX), Max(1, GroupCountY), Max(1, GroupCountZ));
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(
        GL_SHADER_IMAGE_ACCESS_BARRIER_BIT or
        GL_SHADER_STORAGE_BARRIER_BIT or
        GL_TEXTURE_FETCH_BARRIER_BIT or
        GL_BUFFER_UPDATE_BARRIER_BIT or
        GL_COMMAND_BARRIER_BIT or
        GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT or
        GL_ELEMENT_ARRAY_BARRIER_BIT);
  end;
  {$endif}
end;

procedure TOpenGLRHICommandList.DispatchIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt);
var
  GLBuf: TOpenGLRHIBuffer;
  CmdStruct: TRHIDispatchIndirectCommand;
begin
  if IndirectBuffer = nil then Exit;
  Inc(FComputeDispatchCount);
  if FDevice <> nil then
    Inc(FDevice.FComputeDispatchCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  GLBuf := IndirectBuffer as TOpenGLRHIBuffer;
  {$ifndef OpenGLES}
  if Assigned(glDispatchComputeIndirect) then
  begin
    glBindBuffer(GL_DISPATCH_INDIRECT_BUFFER, GLBuf.Handle);
    glDispatchComputeIndirect(TGLintptr(Offset));
    glBindBuffer(GL_DISPATCH_INDIRECT_BUFFER, 0);
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(
        GL_SHADER_IMAGE_ACCESS_BARRIER_BIT or
        GL_SHADER_STORAGE_BARRIER_BIT or
        GL_TEXTURE_FETCH_BARRIER_BIT or
        GL_BUFFER_UPDATE_BARRIER_BIT or
        GL_COMMAND_BARRIER_BIT);
  end else
  if Assigned(glDispatchCompute) then
  begin
    FillChar(CmdStruct, SizeOf(CmdStruct), 0);
    GLBuf.ReadData(CmdStruct, SizeOf(CmdStruct), Offset);
    glDispatchCompute(
      Max(1, CmdStruct.GroupCountX),
      Max(1, CmdStruct.GroupCountY),
      Max(1, CmdStruct.GroupCountZ));
  end;
  {$else}
  if GLBuf = nil then ;
  if CmdStruct.GroupCountX = 0 then ;
  {$endif}
end;

procedure TOpenGLRHICommandList.ResetQueryPool(const QueryPool: TRHIQueryPool;
  const FirstQuery, QueryCount: Cardinal);
var
  GLPool: TOpenGLRHIQueryPool;
  I, EndIdx: Cardinal;
begin
  if (QueryPool = nil) or (QueryCount = 0) then Exit;
  GLPool := QueryPool as TOpenGLRHIQueryPool;
  Inc(GLPool.FResetCount);
  if FirstQuery >= Cardinal(Length(GLPool.FQueryWritten)) then
    Exit;
  EndIdx := Min(FirstQuery + QueryCount, Cardinal(Length(GLPool.FQueryWritten))) - 1;
  for I := FirstQuery to EndIdx do
  begin
    GLPool.FQueryWritten[I] := False;
    GLPool.FSimulatedResults[I] := 0;
    GLPool.FActiveTargets[I] := 0;
  end;
end;

procedure TOpenGLRHICommandList.BeginQuery(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal; const Precise: Boolean);
var
  GLPool: TOpenGLRHIQueryPool;
  Target: TGLenum;
begin
  if QueryPool = nil then Exit;
  GLPool := QueryPool as TOpenGLRHIQueryPool;
  if QueryIndex >= Cardinal(Length(GLPool.FQueries)) then Exit;

  if GLPool.Desc.QueryType = qtTimestamp then
  begin
    {$ifndef OpenGLES}
    Target := GL_TIME_ELAPSED;
    {$else}
    Target := GL_ANY_SAMPLES_PASSED;
    {$endif}
  end else
  begin
    {$ifndef OpenGLES}
    if Precise then
      Target := GL_SAMPLES_PASSED
    else
      Target := GL_ANY_SAMPLES_PASSED;
    {$else}
    Target := GL_ANY_SAMPLES_PASSED;
    {$endif}
  end;

  GLPool.FActiveTargets[QueryIndex] := Target;
  GLPool.FQueryWritten[QueryIndex] := True;
  if GLPool.FSimulatedResults[QueryIndex] = 0 then
    GLPool.FSimulatedResults[QueryIndex] := 64;

  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glBeginQuery(Target, GLPool.FQueries[QueryIndex]);
end;

procedure TOpenGLRHICommandList.EndQuery(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal);
var
  GLPool: TOpenGLRHIQueryPool;
  Target: TGLenum;
begin
  if QueryPool = nil then Exit;
  GLPool := QueryPool as TOpenGLRHIQueryPool;
  if QueryIndex >= Cardinal(Length(GLPool.FQueries)) then Exit;

  Target := GLPool.FActiveTargets[QueryIndex];
  if Target = 0 then
    Target := GL_ANY_SAMPLES_PASSED;
  GLPool.FQueryWritten[QueryIndex] := True;

  if (FDevice <> nil) and FDevice.HasLiveGLContext then
    glEndQuery(Target);
end;

procedure TOpenGLRHICommandList.WriteTimestamp(const QueryPool: TRHIQueryPool;
  const QueryIndex: Cardinal; const Stage: TRHIShaderStage);
var
  GLPool: TOpenGLRHIQueryPool;
begin
  if QueryPool = nil then Exit;
  GLPool := QueryPool as TOpenGLRHIQueryPool;
  if QueryIndex >= Cardinal(Length(GLPool.FQueries)) then Exit;

  GLPool.FQueryWritten[QueryIndex] := True;
  GLPool.FSimulatedResults[QueryIndex] := UInt64(QueryIndex + 1) * UInt64(1000000);

  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    {$ifndef OpenGLES}
    if Assigned(glQueryCounter) then
      glQueryCounter(GLPool.FQueries[QueryIndex], GL_TIMESTAMP);
    {$endif}
  end;
  if Stage = ssCompute then ; { Silence unused hint }
end;

procedure TOpenGLRHICommandList.ApplyVertexLayout(const Pipeline: TOpenGLRHIPipeline;
  const FirstBinding: Cardinal; const Buffers: array of TRHIBuffer;
  const Offsets: array of PtrUInt);
var
  I, J, BIdx: Integer;
  Attr: TRHIVertexAttributeDesc;
  BindDesc: TRHIVertexBindingDesc;
  FoundBind: Boolean;
  GLBuf: TOpenGLRHIBuffer;
  CompCount: TGLint;
  CompType: TGLenum;
  Normalized: TGLboolean;
  IsIntAttr: Boolean;
  BufOffset: PtrUInt;
begin
  if (FDevice = nil) or (not FDevice.HasLiveGLContext) or (Pipeline = nil) then
    Exit;

  glBindVertexArray(Pipeline.VAOHandle);

  for I := 0 to High(Pipeline.Desc.VertexInput.Attributes) do
  begin
    Attr := Pipeline.Desc.VertexInput.Attributes[I];
    BIdx := Integer(Attr.Binding) - Integer(FirstBinding);
    if (BIdx < 0) or (BIdx > High(Buffers)) or (Buffers[BIdx] = nil) then
      Continue;

    FoundBind := False;
    FillChar(BindDesc, SizeOf(BindDesc), 0);
    for J := 0 to High(Pipeline.Desc.VertexInput.Bindings) do
      if Pipeline.Desc.VertexInput.Bindings[J].Binding = Attr.Binding then
      begin
        BindDesc := Pipeline.Desc.VertexInput.Bindings[J];
        FoundBind := True;
        Break;
      end;
    if not FoundBind then
      Continue;

    GLBuf := Buffers[BIdx] as TOpenGLRHIBuffer;
    if BIdx <= High(Offsets) then
      BufOffset := Offsets[BIdx]
    else
      BufOffset := 0;

    if GLBuf.FIsMappedForWrite and (not GLBuf.FMappedUploadedViaSubData) then
      GLBuf.SyncMappedToGL(BufOffset, 0);

    glBindBuffer(GL_ARRAY_BUFFER, GLBuf.Handle);
    glEnableVertexAttribArray(Attr.Location);

    Normalized := GL_FALSE;
    IsIntAttr := False;
    case Attr.Format of
      vafFloat:
        begin CompCount := 1; CompType := GL_FLOAT; end;
      vafVector2:
        begin CompCount := 2; CompType := GL_FLOAT; end;
      vafVector3:
        begin CompCount := 3; CompType := GL_FLOAT; end;
      vafVector4:
        begin CompCount := 4; CompType := GL_FLOAT; end;
      vafByte4Normalized:
        begin CompCount := 4; CompType := GL_BYTE; Normalized := GL_TRUE; end;
      vafUByte4Normalized:
        begin CompCount := 4; CompType := GL_UNSIGNED_BYTE; Normalized := GL_TRUE; end;
      vafShort2:
        begin CompCount := 2; CompType := GL_SHORT; end;
      vafShort4:
        begin CompCount := 4; CompType := GL_SHORT; end;
      vafUInt4:
        begin CompCount := 4; CompType := GL_UNSIGNED_INT; IsIntAttr := True; end;
    else
      begin CompCount := 4; CompType := GL_FLOAT; end;
    end;

    if IsIntAttr then
      glVertexAttribIPointer(Attr.Location, CompCount, CompType,
        BindDesc.Stride, Pointer(BufOffset + Attr.Offset))
    else
      glVertexAttribPointer(Attr.Location, CompCount, CompType, Normalized,
        BindDesc.Stride, Pointer(BufOffset + Attr.Offset));

    if BindDesc.PerInstance then
      glVertexAttribDivisor(Attr.Location, 1)
    else
      glVertexAttribDivisor(Attr.Location, 0);
  end;

  if FCurrentIndexBuffer <> nil then
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);
end;

procedure TOpenGLRHICommandList.BindVertexBuffers(const FirstBinding: Cardinal;
  const Buffers: array of TRHIBuffer; const Offsets: array of PtrUInt);
begin
  if FCurrentPipeline = nil then Exit;
  ApplyVertexLayout(FCurrentPipeline, FirstBinding, Buffers, Offsets);
end;

procedure TOpenGLRHICommandList.BindIndexBuffer(const Buffer: TRHIBuffer;
  const Offset: PtrUInt; const IndexType: TRHIIndexType);
begin
  if Buffer = nil then Exit;
  FCurrentIndexBuffer := Buffer as TOpenGLRHIBuffer;
  FCurrentIndexOffset := Offset;
  if IndexType = itUInt16 then
  begin
    FCurrentIndexType := GL_UNSIGNED_SHORT;
    FCurrentIndexSize := 2;
  end else
  begin
    FCurrentIndexType := GL_UNSIGNED_INT;
    FCurrentIndexSize := 4;
  end;
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    if FCurrentIndexBuffer.FIsMappedForWrite and
       (not FCurrentIndexBuffer.FMappedUploadedViaSubData) then
      FCurrentIndexBuffer.SyncMappedToGL(Offset, 0);
    if FCurrentPipeline <> nil then
      glBindVertexArray(FCurrentPipeline.VAOHandle);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);
  end;
end;

procedure TOpenGLRHICommandList.Draw(const VertexCount, InstanceCount,
  FirstVertex, FirstInstance: Cardinal);
begin
  if (VertexCount = 0) or (InstanceCount = 0) or (FCurrentPipeline = nil) then
    Exit;
  FCurrentPassHasCommands := True;
  Inc(FDrawCallCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  glUseProgram(FCurrentPipeline.ProgramHandle);
  glBindVertexArray(FCurrentPipeline.VAOHandle);

  {$ifndef OpenGLES}
  if (FirstInstance > 0) and Assigned(glDrawArraysInstancedBaseInstance) then
    glDrawArraysInstancedBaseInstance(FCurrentPrimitiveMode,
      FirstVertex, VertexCount, InstanceCount, FirstInstance)
  else
  {$endif}
  if InstanceCount <= 1 then
    glDrawArrays(FCurrentPrimitiveMode, FirstVertex, VertexCount)
  else
    glDrawArraysInstanced(FCurrentPrimitiveMode, FirstVertex, VertexCount, InstanceCount);
end;

procedure TOpenGLRHICommandList.DrawIndexed(const IndexCount, InstanceCount,
  FirstIndex: Cardinal; const VertexOffset: Integer; const FirstInstance: Cardinal);
var
  IndexPtr: Pointer;
begin
  if (IndexCount = 0) or (InstanceCount = 0) or (FCurrentPipeline = nil) then
    Exit;
  FCurrentPassHasCommands := True;
  Inc(FDrawCallCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  glUseProgram(FCurrentPipeline.ProgramHandle);
  glBindVertexArray(FCurrentPipeline.VAOHandle);
  if FCurrentIndexBuffer <> nil then
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);

  IndexPtr := Pointer(FCurrentIndexOffset + PtrUInt(FirstIndex) * PtrUInt(FCurrentIndexSize));
  {$ifndef OpenGLES}
  if (FirstInstance > 0) and Assigned(glDrawElementsInstancedBaseVertexBaseInstance) then
    glDrawElementsInstancedBaseVertexBaseInstance(FCurrentPrimitiveMode,
      IndexCount, FCurrentIndexType, IndexPtr, InstanceCount, VertexOffset, FirstInstance)
  else if (VertexOffset <> 0) and (InstanceCount > 1) and Assigned(glDrawElementsInstancedBaseVertex) then
    glDrawElementsInstancedBaseVertex(FCurrentPrimitiveMode,
      IndexCount, FCurrentIndexType, IndexPtr, InstanceCount, VertexOffset)
  else if (VertexOffset <> 0) and (InstanceCount = 1) and Assigned(glDrawElementsBaseVertex) then
    glDrawElementsBaseVertex(FCurrentPrimitiveMode,
      IndexCount, FCurrentIndexType, IndexPtr, VertexOffset)
  else
  {$endif}
  if InstanceCount <= 1 then
    glDrawElements(FCurrentPrimitiveMode, IndexCount, FCurrentIndexType, IndexPtr)
  else
    glDrawElementsInstanced(FCurrentPrimitiveMode, IndexCount, FCurrentIndexType, IndexPtr, InstanceCount);
end;

procedure TOpenGLRHICommandList.DrawIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const DrawCount, Stride: Cardinal);
var
  GLBuf: TOpenGLRHIBuffer;
  EffStride, I: Cardinal;
  CmdItem: TRHIDrawIndirectCommand;
begin
  if (IndirectBuffer = nil) or (DrawCount = 0) or (FCurrentPipeline = nil) then
    Exit;
  FCurrentPassHasCommands := True;

  Inc(FDrawCallCount);
  Inc(FIndirectDrawCallCount);
  if FDevice <> nil then
    Inc(FDevice.FIndirectDrawCallCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  GLBuf := IndirectBuffer as TOpenGLRHIBuffer;
  if Stride = 0 then
    EffStride := SizeOf(TRHIDrawIndirectCommand)
  else
    EffStride := Stride;

  glUseProgram(FCurrentPipeline.ProgramHandle);
  glBindVertexArray(FCurrentPipeline.VAOHandle);

  {$ifndef OpenGLES}
  if Assigned(glMultiDrawArraysIndirect) then
  begin
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLBuf.Handle);
    glMultiDrawArraysIndirect(FCurrentPrimitiveMode, Pointer(Offset), DrawCount, EffStride);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;

  if Assigned(glDrawArraysIndirect) then
  begin
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLBuf.Handle);
    for I := 0 to DrawCount - 1 do
      glDrawArraysIndirect(FCurrentPrimitiveMode,
        Pointer(Offset + PtrUInt(I) * PtrUInt(EffStride)));
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;
  {$endif}

  for I := 0 to DrawCount - 1 do
  begin
    FillChar(CmdItem, SizeOf(CmdItem), 0);
    GLBuf.ReadData(CmdItem, SizeOf(CmdItem), Offset + PtrUInt(I) * PtrUInt(EffStride));
    if (CmdItem.VertexCount > 0) and (CmdItem.InstanceCount > 0) then
      Draw(CmdItem.VertexCount, CmdItem.InstanceCount, CmdItem.FirstVertex, CmdItem.FirstInstance);
  end;
end;

procedure TOpenGLRHICommandList.DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const DrawCount, Stride: Cardinal);
var
  GLBuf: TOpenGLRHIBuffer;
  EffStride, I: Cardinal;
  CmdItem: TRHIDrawIndexedIndirectCommand;
begin
  if (IndirectBuffer = nil) or (DrawCount = 0) or (FCurrentPipeline = nil) then
    Exit;
  FCurrentPassHasCommands := True;

  Inc(FDrawCallCount);
  Inc(FIndirectDrawCallCount);
  if FDevice <> nil then
    Inc(FDevice.FIndirectDrawCallCount);

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
    Exit;

  GLBuf := IndirectBuffer as TOpenGLRHIBuffer;
  if Stride = 0 then
    EffStride := SizeOf(TRHIDrawIndexedIndirectCommand)
  else
    EffStride := Stride;

  glUseProgram(FCurrentPipeline.ProgramHandle);
  glBindVertexArray(FCurrentPipeline.VAOHandle);
  if FCurrentIndexBuffer <> nil then
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);

  {$ifndef OpenGLES}
  if (FCurrentIndexOffset = 0) and Assigned(glMultiDrawElementsIndirect) then
  begin
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLBuf.Handle);
    glMultiDrawElementsIndirect(FCurrentPrimitiveMode, FCurrentIndexType,
      Pointer(Offset), DrawCount, EffStride);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;

  if (FCurrentIndexOffset = 0) and Assigned(glDrawElementsIndirect) then
  begin
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLBuf.Handle);
    for I := 0 to DrawCount - 1 do
      glDrawElementsIndirect(FCurrentPrimitiveMode, FCurrentIndexType,
        Pointer(Offset + PtrUInt(I) * PtrUInt(EffStride)));
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;
  {$endif}

  for I := 0 to DrawCount - 1 do
  begin
    FillChar(CmdItem, SizeOf(CmdItem), 0);
    GLBuf.ReadData(CmdItem, SizeOf(CmdItem), Offset + PtrUInt(I) * PtrUInt(EffStride));
    if (CmdItem.IndexCount > 0) and (CmdItem.InstanceCount > 0) then
      DrawIndexed(CmdItem.IndexCount, CmdItem.InstanceCount,
        CmdItem.FirstIndex, CmdItem.VertexOffset, CmdItem.FirstInstance);
  end;
end;

procedure TOpenGLRHICommandList.DrawIndirectCount(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
  const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal);
var
  GLIndBuf, GLCountBuf: TOpenGLRHIBuffer;
  EffStride, ActualCount: Cardinal;
begin
  if (IndirectBuffer = nil) or (CountBuffer = nil) or (MaxDrawCount = 0) or
     (FCurrentPipeline = nil) then
    Exit;

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
  begin
    Inc(FDrawCallCount);
    Inc(FIndirectDrawCallCount);
    if FDevice <> nil then
      Inc(FDevice.FIndirectDrawCallCount);
    Exit;
  end;

  GLIndBuf := IndirectBuffer as TOpenGLRHIBuffer;
  GLCountBuf := CountBuffer as TOpenGLRHIBuffer;
  if Stride = 0 then
    EffStride := SizeOf(TRHIDrawIndirectCommand)
  else
    EffStride := Stride;

  {$ifndef OpenGLES}
  if Assigned(glMultiDrawArraysIndirectCount) then
  begin
    Inc(FDrawCallCount);
    Inc(FIndirectDrawCallCount);
    if FDevice <> nil then
      Inc(FDevice.FIndirectDrawCallCount);

    glUseProgram(FCurrentPipeline.ProgramHandle);
    glBindVertexArray(FCurrentPipeline.VAOHandle);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLIndBuf.Handle);
    glBindBuffer(GL_PARAMETER_BUFFER, GLCountBuf.Handle);
    glMultiDrawArraysIndirectCount(FCurrentPrimitiveMode, Pointer(Offset),
      TGLintptr(CountBufferOffset), MaxDrawCount, EffStride);
    glBindBuffer(GL_PARAMETER_BUFFER, 0);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;
  {$endif}

  ActualCount := 0;
  GLCountBuf.ReadData(ActualCount, SizeOf(ActualCount), CountBufferOffset);
  ActualCount := Min(ActualCount, MaxDrawCount);
  if ActualCount > 0 then
    DrawIndirect(IndirectBuffer, Offset, ActualCount, EffStride);
end;

procedure TOpenGLRHICommandList.DrawIndexedIndirectCount(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const CountBuffer: TRHIBuffer;
  const CountBufferOffset: PtrUInt; const MaxDrawCount, Stride: Cardinal);
var
  GLIndBuf, GLCountBuf: TOpenGLRHIBuffer;
  EffStride, ActualCount: Cardinal;
begin
  if (IndirectBuffer = nil) or (CountBuffer = nil) or (MaxDrawCount = 0) or
     (FCurrentPipeline = nil) then
    Exit;

  if (FDevice = nil) or (not FDevice.HasLiveGLContext) then
  begin
    Inc(FDrawCallCount);
    Inc(FIndirectDrawCallCount);
    if FDevice <> nil then
      Inc(FDevice.FIndirectDrawCallCount);
    Exit;
  end;

  GLIndBuf := IndirectBuffer as TOpenGLRHIBuffer;
  GLCountBuf := CountBuffer as TOpenGLRHIBuffer;
  if Stride = 0 then
    EffStride := SizeOf(TRHIDrawIndexedIndirectCommand)
  else
    EffStride := Stride;

  {$ifndef OpenGLES}
  if (FCurrentIndexOffset = 0) and Assigned(glMultiDrawElementsIndirectCount) then
  begin
    Inc(FDrawCallCount);
    Inc(FIndirectDrawCallCount);
    if FDevice <> nil then
      Inc(FDevice.FIndirectDrawCallCount);

    glUseProgram(FCurrentPipeline.ProgramHandle);
    glBindVertexArray(FCurrentPipeline.VAOHandle);
    if FCurrentIndexBuffer <> nil then
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, GLIndBuf.Handle);
    glBindBuffer(GL_PARAMETER_BUFFER, GLCountBuf.Handle);
    glMultiDrawElementsIndirectCount(FCurrentPrimitiveMode, FCurrentIndexType,
      Pointer(Offset), TGLintptr(CountBufferOffset), MaxDrawCount, EffStride);
    glBindBuffer(GL_PARAMETER_BUFFER, 0);
    glBindBuffer(GL_DRAW_INDIRECT_BUFFER, 0);
    Exit;
  end;
  {$endif}

  ActualCount := 0;
  GLCountBuf.ReadData(ActualCount, SizeOf(ActualCount), CountBufferOffset);
  ActualCount := Min(ActualCount, MaxDrawCount);
  if ActualCount > 0 then
    DrawIndexedIndirect(IndirectBuffer, Offset, ActualCount, EffStride);
end;

procedure TOpenGLRHICommandList.ExecuteCommands(const SecondaryCommandLists: array of TRHICommandList);
begin
  { On OpenGL, secondary command lists execute immediately on the GL context thread }
  if Length(SecondaryCommandLists) < 0 then ;
end;

procedure TOpenGLRHICommandList.PushDebugGroup(const LabelName: String; const Color: TCastleColor);
var
  AnsiLabel: AnsiString;
begin
  Inc(FDebugGroupDepth);
  Inc(FDebugGroupPushCount);
  if (FDevice <> nil) and FDevice.HasLiveGLContext and (LabelName <> '') then
  begin
    {$ifndef OpenGLES}
    if Assigned(glPushDebugGroup) then
    begin
      AnsiLabel := AnsiString(LabelName);
      glPushDebugGroup(GL_DEBUG_SOURCE_APPLICATION, 0, Length(AnsiLabel), PGLchar(AnsiLabel));
    end;
    {$endif}
  end;
  if Color.X < -1e30 then ;
end;

procedure TOpenGLRHICommandList.PopDebugGroup;
begin
  if FDebugGroupDepth <= 0 then
    Exit;
  Dec(FDebugGroupDepth);
  if (FDevice <> nil) and FDevice.HasLiveGLContext then
  begin
    {$ifndef OpenGLES}
    if Assigned(glPopDebugGroup) then
      glPopDebugGroup();
    {$endif}
  end;
end;

procedure TOpenGLRHICommandList.InsertDebugMarker(const LabelName: String; const Color: TCastleColor);
var
  AnsiLabel: AnsiString;
begin
  Inc(FDebugMarkerCount);
  if (FDevice <> nil) and FDevice.HasLiveGLContext and (LabelName <> '') then
  begin
    {$ifndef OpenGLES}
    if Assigned(glDebugMessageInsert) then
    begin
      AnsiLabel := AnsiString(LabelName);
      glDebugMessageInsert(GL_DEBUG_SOURCE_APPLICATION, GL_DEBUG_TYPE_MARKER,
        0, GL_DEBUG_SEVERITY_NOTIFICATION, Length(AnsiLabel), PGLchar(AnsiLabel));
    end;
    {$endif}
  end;
  if Color.X < -1e30 then ;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIDevice                                                             }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHIDevice.Create(const InitialWidth, InitialHeight: Cardinal);
begin
  inherited Create;
  FHasLiveGLContext := (ApplicationProperties <> nil) and
    ApplicationProperties.IsGLContextOpen and (GLFeatures <> nil) and
    {$ifdef OpenGLES} GLFeatures.Version_3_0 {$else} GLFeatures.Version_3_0 {$endif} and
    Assigned(glGenFramebuffers) and Assigned(glDeleteFramebuffers);
  FPipelineMap := TOpenGLPipelineMap.Create([doOwnsValues]);
  FComputePipelineMap := TOpenGLComputePipelineMap.Create([doOwnsValues]);
  FMaxImageUnits := 8;
  QueryDeviceCapabilities;

  if FHasLiveGLContext then
  begin
    glGenFramebuffers(1, @FReadbackFBO);
    glGenFramebuffers(1, @FReadbackResolveFBO);
  end;

  FCommandList := TOpenGLRHICommandList.Create(Self);
  FDefaultColorTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), InitialWidth), Max(Cardinal(1), InitialHeight),
      pfRGBA8_UNorm, [tuColorAttachment, tuTransferSrc]));
  FDefaultDepthTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), InitialWidth), Max(Cardinal(1), InitialHeight),
      pfD24_UNorm_S8_UInt, [tuDepthStencil]));
end;

constructor TOpenGLRHIDevice.CreateHeadless(const InitialWidth, InitialHeight: Cardinal;
  const ASupportsCompute, ASupportsMultiDrawIndirect, ASupportsDrawIndirectCount: Boolean);
begin
  inherited Create;
  FHasLiveGLContext := False;
  FPipelineMap := TOpenGLPipelineMap.Create([doOwnsValues]);
  FComputePipelineMap := TOpenGLComputePipelineMap.Create([doOwnsValues]);
  FMaxImageUnits := 8;

  {$ifdef OpenGLES}
  FCapabilities.Backend := rbOpenGLES;
  {$else}
  FCapabilities.Backend := rbOpenGL;
  {$endif}
  FCapabilities.DeviceName := 'Headless OpenGL 4.6 RHI Device';
  FCapabilities.MaxTextureSize := 16384;
  FCapabilities.MaxAnisotropy := 16.0;
  FCapabilities.MaxUniformBufferRange := 65536;
  FCapabilities.MinUniformBufferOffsetAlignment := 256;
  FCapabilities.MaxPushConstantsSize := 256;
  FCapabilities.MaxFramebufferSampleCount := 8;
  FCapabilities.SupportsBCCompression := True;
  FCapabilities.SupportsComputeShaders := ASupportsCompute;
  FCapabilities.SupportsMultiDrawIndirect := ASupportsMultiDrawIndirect;
  FCapabilities.SupportsDrawIndirectCount := ASupportsDrawIndirectCount;
  FCapabilities.SupportsExtendedDynamicState := True;
  FCapabilities.TimestampPeriodNanoseconds := 1.0;

  FCommandList := TOpenGLRHICommandList.Create(Self);
  FDefaultColorTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), InitialWidth), Max(Cardinal(1), InitialHeight),
      pfRGBA8_UNorm, [tuColorAttachment, tuTransferSrc]));
  FDefaultDepthTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), InitialWidth), Max(Cardinal(1), InitialHeight),
      pfD24_UNorm_S8_UInt, [tuDepthStencil]));
end;

class function TOpenGLRHIDevice.TryCreateFromCurrentGLContext(out ADevice: TOpenGLRHIDevice;
  const InitialWidth, InitialHeight: Cardinal): Boolean;
begin
  ADevice := nil;
  if (ApplicationProperties = nil) or (not ApplicationProperties.IsGLContextOpen) or
     (GLFeatures = nil) then
    Exit(False);

  try
    ADevice := TOpenGLRHIDevice.Create(InitialWidth, InitialHeight);
    Result := ADevice.HasLiveGLContext;
    if not Result then
      FreeAndNil(ADevice);
  except
    on Exception do
    begin
      FreeAndNil(ADevice);
      Result := False;
    end;
  end;
end;

destructor TOpenGLRHIDevice.Destroy;
begin
  FreeAndNil(FComputePipelineMap);
  FreeAndNil(FPipelineMap);
  FreeAndNil(FDefaultColorTarget);
  FreeAndNil(FDefaultDepthTarget);
  FreeAndNil(FCommandList);
  ReleaseRemainingDeviceResources;
  if FHasLiveGLContext then
  begin
    if (FReadbackFBO <> 0) and Assigned(glDeleteFramebuffers) then
      glDeleteFramebuffers(1, @FReadbackFBO);
    if (FReadbackResolveFBO <> 0) and Assigned(glDeleteFramebuffers) then
      glDeleteFramebuffers(1, @FReadbackResolveFBO);
  end;
  inherited;
end;

procedure TOpenGLRHIDevice.QueryDeviceCapabilities;
var
  RendererPtr: PAnsiChar;
  IntVal: TGLint;
begin
  {$ifdef OpenGLES}
  FCapabilities.Backend := rbOpenGLES;
  {$else}
  FCapabilities.Backend := rbOpenGL;
  {$endif}
  FCapabilities.TimestampPeriodNanoseconds := 1.0;
  FCapabilities.MaxPushConstantsSize := 256;
  FCapabilities.SupportsExtendedDynamicState := True;

  if not FHasLiveGLContext then
  begin
    FCapabilities.DeviceName := 'Unbound OpenGL RHI Device';
    FCapabilities.MaxTextureSize := 4096;
    FCapabilities.MaxFramebufferSampleCount := 1;
    Exit;
  end;

  RendererPtr := PAnsiChar(glGetString(GL_RENDERER));
  if RendererPtr <> nil then
    FCapabilities.DeviceName := String(RendererPtr)
  else
    FCapabilities.DeviceName := 'OpenGL Renderer';

  FCapabilities.MaxTextureSize := GLFeatures.MaxTextureSize;
  FCapabilities.MaxAnisotropy := Max(1.0, GLFeatures.MaxTextureMaxAnisotropy);
  IntVal := 1;
  if GLFeatures.FBOMultiSampling then
    glGetIntegerv(GL_MAX_SAMPLES_RHI, @IntVal);
  FCapabilities.MaxFramebufferSampleCount := Max(1, IntVal);
  FCapabilities.SupportsBCCompression :=
    [tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5] * GLFeatures.TextureCompression <> [];
  FCapabilities.SupportsASTCCompression := tcASTC_4x4_RGBA in GLFeatures.TextureCompression;
  FCapabilities.SupportsETC2Compression := tcETC1 in GLFeatures.TextureCompression;

  IntVal := 16384;
  if {$ifdef OpenGLES} GLFeatures.Version_3_0 {$else} GLFeatures.Version_3_1 {$endif} then
    glGetIntegerv(GL_MAX_UNIFORM_BLOCK_SIZE, @IntVal);
  FCapabilities.MaxUniformBufferRange := Max(16384, IntVal);

  IntVal := 256;
  if {$ifdef OpenGLES} GLFeatures.Version_3_0 {$else} GLFeatures.Version_3_1 {$endif} then
    glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, @IntVal);
  FCapabilities.MinUniformBufferOffsetAlignment := Max(1, IntVal);

  {$ifndef OpenGLES}
  FCapabilities.SupportsComputeShaders :=
    Assigned(glDispatchCompute) and Assigned(glBindImageTexture) and Assigned(glMemoryBarrier);
  FCapabilities.SupportsMultiDrawIndirect :=
    Assigned(glMultiDrawElementsIndirect) and Assigned(glMultiDrawArraysIndirect);
  FCapabilities.SupportsDrawIndirectCount :=
    Assigned(glMultiDrawElementsIndirectCount) and Assigned(glMultiDrawArraysIndirectCount);

  if FCapabilities.SupportsComputeShaders then
  begin
    IntVal := 8;
    glGetIntegerv(GL_MAX_IMAGE_UNITS, @IntVal);
    FMaxImageUnits := Max(1, IntVal);
  end;
  {$else}
  FCapabilities.SupportsComputeShaders := False;
  FCapabilities.SupportsMultiDrawIndirect := False;
  FCapabilities.SupportsDrawIndirectCount := False;
  {$endif}
end;

function TOpenGLRHIDevice.GetPipelineCount: Integer;
begin
  if FPipelineMap <> nil then
    Result := FPipelineMap.Count
  else
    Result := 0;
end;

function TOpenGLRHIDevice.GetComputePipelineCount: Integer;
begin
  if FComputePipelineMap <> nil then
    Result := FComputePipelineMap.Count
  else
    Result := 0;
end;

function TOpenGLRHIDevice.BeginFrame: TRHICommandList;
begin
  Inc(FTotalFramesStarted);
  FCommandList.BeginRecording;
  Result := FCommandList;
  SetActiveCommandList(Result);
end;

procedure TOpenGLRHIDevice.EndFrameAndPresent;
begin
  FCommandList.EndRecording;
  SetActiveCommandList(nil);
  FFrameIndex := (FFrameIndex + 1) mod MaxFramesInFlight;
end;

function TOpenGLRHIDevice.BeginOffscreenFrame: TRHICommandList;
begin
  Inc(FTotalFramesStarted);
  FCommandList.BeginRecording;
  Result := FCommandList;
  SetActiveCommandList(Result);
end;

procedure TOpenGLRHIDevice.EndOffscreenFrame;
begin
  FCommandList.EndRecording;
  SetActiveCommandList(nil);
end;

procedure TOpenGLRHIDevice.ResizeSwapchain(const NewWidth, NewHeight: Cardinal);
begin
  FreeAndNil(FDefaultColorTarget);
  FreeAndNil(FDefaultDepthTarget);
  FDefaultColorTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), NewWidth), Max(Cardinal(1), NewHeight),
      pfRGBA8_UNorm, [tuColorAttachment, tuTransferSrc]));
  FDefaultDepthTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(Max(Cardinal(1), NewWidth), Max(Cardinal(1), NewHeight),
      pfD24_UNorm_S8_UInt, [tuDepthStencil]));
end;

procedure TOpenGLRHIDevice.WaitIdle;
begin
  if FHasLiveGLContext then
    glFinish;
end;

function TOpenGLRHIDevice.CreateSecondaryCommandList: TRHICommandList;
begin
  Result := TOpenGLRHICommandList.Create(Self, cllSecondary);
end;

function TOpenGLRHIDevice.GetSwapchainColorTexture: TRHITexture;
begin
  Result := FDefaultColorTarget;
end;

function TOpenGLRHIDevice.GetSwapchainDepthTexture: TRHITexture;
begin
  Result := FDefaultDepthTarget;
end;

function TOpenGLRHIDevice.CreateBuffer(const ADesc: TRHIBufferDesc;
  const InitialData: Pointer): TRHIBuffer;
begin
  Result := TOpenGLRHIBuffer.Create(Self, ADesc, InitialData);
end;

function TOpenGLRHIDevice.CreateTexture(const ADesc: TRHITextureDesc): TRHITexture;
begin
  Result := TOpenGLRHITexture.Create(Self, ADesc);
end;

function TOpenGLRHIDevice.CreateSampler(const ADesc: TRHISamplerDesc): TRHISampler;
begin
  Result := TOpenGLRHISampler.Create(Self, ADesc);
end;

function TOpenGLRHIDevice.CreateShaderModuleFromGLSL(const AStage: TRHIShaderStage;
  const AGLSLSource: String; const ADebugName: String): TRHIShaderModule;
begin
  Result := TOpenGLRHIShaderModule.Create(Self, AStage, AGLSLSource, ADebugName);
end;

function TOpenGLRHIDevice.CreateShaderModuleFromSPIRV(const AStage: TRHIShaderStage;
  const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt;
  const ADebugName: String): TRHIShaderModule;
begin
  Result := nil;
  if (ASPIRVBinary = nil) and (ASizeInBytes = 0) and (AStage = ssCompute) and (ADebugName = '') then ;
  raise ERHIError.Create('SPIR-V binary loading is only used by the Vulkan backend; pass GLSL source to OpenGL.');
end;

function TOpenGLRHIDevice.CreateDescriptorSetLayout(
  const Bindings: array of TRHIDescriptorBindingDesc): TRHIDescriptorSetLayout;
begin
  Result := TOpenGLRHIDescriptorSetLayout.Create(Self, Bindings);
end;

function TOpenGLRHIDevice.CreateDescriptorSet(
  const Layout: TRHIDescriptorSetLayout): TRHIDescriptorSet;
begin
  Result := TOpenGLRHIDescriptorSet.Create(Self, Layout as TOpenGLRHIDescriptorSetLayout);
end;

function TOpenGLRHIDevice.ComputePipelineHash(const ADesc: TRHIGraphicsPipelineDesc): UInt64;
var
  I: Integer;
begin
  Result := FNV1aOffsetBasis64;
  HashUInt64FNV1a(Result, PtrUInt(ADesc.VertexShader));
  HashUInt64FNV1a(Result, PtrUInt(ADesc.FragmentShader));
  HashUInt64FNV1a(Result, PtrUInt(ADesc.GeometryShader));
  HashBytesFNV1a(Result, ADesc.Topology, SizeOf(ADesc.Topology));
  HashBytesFNV1a(Result, ADesc.Rasterizer, SizeOf(ADesc.Rasterizer));
  HashBytesFNV1a(Result, ADesc.DepthStencil, SizeOf(ADesc.DepthStencil));
  HashBytesFNV1a(Result, ADesc.BlendState, SizeOf(ADesc.BlendState));
  HashBytesFNV1a(Result, ADesc.PushConstantSize, SizeOf(ADesc.PushConstantSize));
  HashBytesFNV1a(Result, ADesc.SampleCount, SizeOf(ADesc.SampleCount));

  for I := 0 to High(ADesc.VertexInput.Bindings) do
    HashBytesFNV1a(Result, ADesc.VertexInput.Bindings[I], SizeOf(TRHIVertexBindingDesc));
  for I := 0 to High(ADesc.VertexInput.Attributes) do
  begin
    HashBytesFNV1a(Result, ADesc.VertexInput.Attributes[I].Location, SizeOf(Cardinal));
    HashBytesFNV1a(Result, ADesc.VertexInput.Attributes[I].Binding, SizeOf(Cardinal));
    HashBytesFNV1a(Result, ADesc.VertexInput.Attributes[I].Format, SizeOf(TRHIVertexAttributeFormat));
    HashBytesFNV1a(Result, ADesc.VertexInput.Attributes[I].Offset, SizeOf(Cardinal));
    HashStringFNV1a(Result, ADesc.VertexInput.Attributes[I].AttributeName);
  end;
end;

function TOpenGLRHIDevice.ComputeComputePipelineHash(const ADesc: TRHIComputePipelineDesc): UInt64;
var
  I: Integer;
begin
  Result := FNV1aOffsetBasis64;
  HashUInt64FNV1a(Result, PtrUInt(ADesc.ComputeShader));
  HashBytesFNV1a(Result, ADesc.PushConstantSize, SizeOf(ADesc.PushConstantSize));
  HashBytesFNV1a(Result, ADesc.PushConstantStages, SizeOf(ADesc.PushConstantStages));
  for I := 0 to High(ADesc.DescriptorSetLayouts) do
    HashUInt64FNV1a(Result, PtrUInt(ADesc.DescriptorSetLayouts[I]));
end;

function TOpenGLRHIDevice.GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline;
var
  HashKey: UInt64;
  FoundPipeline: TOpenGLRHIPipeline;
begin
  HashKey := ComputePipelineHash(ADesc);
  if FPipelineMap.TryGetValue(HashKey, FoundPipeline) then
  begin
    Inc(FPipelineCacheHits);
    FoundPipeline.UpdateDynamicStateDesc(ADesc);
  end else
  begin
    Inc(FPipelineCacheMisses);
    FoundPipeline := TOpenGLRHIPipeline.Create(Self, ADesc);
    FPipelineMap.Add(HashKey, FoundPipeline);
  end;
  Result := FoundPipeline;
end;

function TOpenGLRHIDevice.GetOrCreateComputePipeline(
  const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline;
var
  HashKey: UInt64;
  FoundPipeline: TOpenGLRHIComputePipeline;
begin
  HashKey := ComputeComputePipelineHash(ADesc);
  if (FComputePipelineMap <> nil) and FComputePipelineMap.TryGetValue(HashKey, FoundPipeline) then
  begin
    Inc(FComputePipelineCacheHits);
  end else
  begin
    Inc(FComputePipelineCacheMisses);
    FoundPipeline := TOpenGLRHIComputePipeline.Create(Self, ADesc);
    if FComputePipelineMap <> nil then
      FComputePipelineMap.Add(HashKey, FoundPipeline);
  end;
  Result := FoundPipeline;
end;

function TOpenGLRHIDevice.CreateComputePipeline(
  const ADesc: TRHIComputePipelineDesc): TRHIComputePipeline;
begin
  Result := TOpenGLRHIComputePipeline.Create(Self, ADesc);
end;

function TOpenGLRHIDevice.CreateQueryPool(const ADesc: TRHIQueryPoolDesc): TRHIQueryPool;
begin
  Result := TOpenGLRHIQueryPool.Create(Self, ADesc);
end;

function TOpenGLRHIDevice.GrabTextureImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle): TRGBAlphaImage;
var
  GLTex: TOpenGLRHITexture;
  PrevReadFBO, PrevDrawFBO: TGLint;
  TempResolveTex: TGLuint;
  FloatBuf: array of Single;
  PIdx, TotalPixels: PtrUInt;
  DstBytes: PByte;
  SrcW, SrcH: Integer;

  function Float01ToByte(const V: Single): Byte; inline;
  begin
    Result := Byte(Round(EnsureRange(V, 0.0, 1.0) * 255.0));
  end;

begin
  if (ReadArea.Width <= 0) or (ReadArea.Height <= 0) then
    Exit(nil);

  Result := TRGBAlphaImage.Create(ReadArea.Width, ReadArea.Height);

  if not FHasLiveGLContext then
  begin
    if (SourceTexture is TOpenGLRHITexture) and
       (Length(TOpenGLRHITexture(SourceTexture).FHostPixels) >=
        PtrUInt(ReadArea.Width) * PtrUInt(ReadArea.Height) * 4) then
      Move(TOpenGLRHITexture(SourceTexture).FHostPixels[0], Result.RawPixels^,
        PtrUInt(ReadArea.Width) * PtrUInt(ReadArea.Height) * 4);
    Exit;
  end;

  if (SourceTexture = nil) or (SourceTexture as TOpenGLRHITexture).IsDefaultBackbuffer then
  begin
    PrevReadFBO := 0;
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, @PrevReadFBO);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
    {$ifndef OpenGLES}
    glReadBuffer(GL_BACK);
    {$endif}
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
      GL_RGBA, GL_UNSIGNED_BYTE, Result.RawPixels);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, TGLuint(PrevReadFBO));
    Exit;
  end;

  GLTex := SourceTexture as TOpenGLRHITexture;
  PrevReadFBO := 0;
  PrevDrawFBO := 0;
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, @PrevReadFBO);
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @PrevDrawFBO);
  TempResolveTex := 0;
  try
    {$ifndef OpenGLES}
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(GL_FRAMEBUFFER_BARRIER_BIT or GL_TEXTURE_UPDATE_BARRIER_BIT or
        GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    {$endif}

    glPixelStorei(GL_PACK_ALIGNMENT, 1);

    if GLTex.Desc.Format in [pfD16_UNorm, pfD24_UNorm_S8_UInt, pfD32_Float, pfD32_Float_S8_UInt] then
    begin
      glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackFBO);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GLTex.Target, GLTex.Handle, 0);
      TotalPixels := PtrUInt(ReadArea.Width) * PtrUInt(ReadArea.Height);
      SetLength(FloatBuf, TotalPixels);
      glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
        GL_DEPTH_COMPONENT, GL_FLOAT, @FloatBuf[0]);
      DstBytes := PByte(Result.RawPixels);
      for PIdx := 0 to TotalPixels - 1 do
      begin
        DstBytes[PIdx * 4 + 0] := Float01ToByte(FloatBuf[PIdx]);
        DstBytes[PIdx * 4 + 1] := DstBytes[PIdx * 4 + 0];
        DstBytes[PIdx * 4 + 2] := DstBytes[PIdx * 4 + 0];
        DstBytes[PIdx * 4 + 3] := 255;
      end;
    end else
    if GLTex.Desc.SampleCount > 1 then
    begin
      SrcW := Max(1, Integer(GLTex.Desc.Width));
      SrcH := Max(1, Integer(GLTex.Desc.Height));
      glGenTextures(1, @TempResolveTex);
      glBindTexture(GL_TEXTURE_2D, TempResolveTex);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, SrcW, SrcH, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      glBindTexture(GL_TEXTURE_2D, 0);

      glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackFBO);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GLTex.Target, GLTex.Handle, 0);

      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FReadbackResolveFBO);
      glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, TempResolveTex, 0);

      glBlitFramebuffer(0, 0, SrcW, SrcH, 0, 0, SrcW, SrcH, GL_COLOR_BUFFER_BIT, GL_NEAREST);

      glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackResolveFBO);
      {$ifndef OpenGLES}
      glReadBuffer(GL_COLOR_ATTACHMENT0);
      {$endif}
      glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
        GL_RGBA, GL_UNSIGNED_BYTE, Result.RawPixels);
    end else
    begin
      glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackFBO);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
      if GLTex.Desc.Dimension = tdCube then
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
          GL_TEXTURE_CUBE_MAP_POSITIVE_X, GLTex.Handle, 0)
      else if GLTex.Desc.Dimension in [td2DArray, td3D] then
        glFramebufferTextureLayer(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GLTex.Handle, 0, 0)
      else
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GLTex.Target, GLTex.Handle, 0);
      {$ifndef OpenGLES}
      glReadBuffer(GL_COLOR_ATTACHMENT0);
      {$endif}
      glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
        GL_RGBA, GL_UNSIGNED_BYTE, Result.RawPixels);
    end;
  finally
    if TempResolveTex <> 0 then
      glDeleteTextures(1, @TempResolveTex);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, TGLuint(PrevReadFBO));
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, TGLuint(PrevDrawFBO));
  end;
end;

function TOpenGLRHIDevice.GrabTextureSubresourceImage(const SourceTexture: TRHITexture;
  const AMipLevel, AArrayLayer: Cardinal): TRGBAlphaImage;
var
  GLTex: TOpenGLRHITexture;
  MipW, MipH: Cardinal;
  PrevReadFBO: TGLint;
begin
  if SourceTexture = nil then
    Exit(nil);

  MipW := Max(Cardinal(1), SourceTexture.Desc.Width shr AMipLevel);
  MipH := Max(Cardinal(1), SourceTexture.Desc.Height shr AMipLevel);

  if (not FHasLiveGLContext) or (SourceTexture as TOpenGLRHITexture).IsDefaultBackbuffer then
    Exit(GrabTextureImage(SourceTexture, Rectangle(0, 0, Integer(MipW), Integer(MipH))));

  GLTex := SourceTexture as TOpenGLRHITexture;
  Result := TRGBAlphaImage.Create(MipW, MipH);
  PrevReadFBO := 0;
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, @PrevReadFBO);
  try
    {$ifndef OpenGLES}
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(GL_FRAMEBUFFER_BARRIER_BIT or GL_TEXTURE_UPDATE_BARRIER_BIT or
        GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    {$endif}
    glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackFBO);
    glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
    if GLTex.Desc.Dimension = tdCube then
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_CUBE_MAP_POSITIVE_X + Min(Cardinal(5), AArrayLayer),
        GLTex.Handle, AMipLevel)
    else if GLTex.Desc.Dimension in [td2DArray, td3D] then
      glFramebufferTextureLayer(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
        GLTex.Handle, AMipLevel, AArrayLayer)
    else
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
        GLTex.Target, GLTex.Handle, AMipLevel);
    {$ifndef OpenGLES}
    glReadBuffer(GL_COLOR_ATTACHMENT0);
    {$endif}
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(0, 0, Integer(MipW), Integer(MipH),
      GL_RGBA, GL_UNSIGNED_BYTE, Result.RawPixels);
  finally
    glBindFramebuffer(GL_READ_FRAMEBUFFER, TGLuint(PrevReadFBO));
  end;
end;

function TOpenGLRHIDevice.GrabTextureFloatImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle): TRGBAlphaFloatImage;
var
  GLTex: TOpenGLRHITexture;
  PrevReadFBO: TGLint;
  DepthBuf: array of Single;
  PIdx, TotalPixels: PtrUInt;
  DstVec4: PVector4;
begin
  if (ReadArea.Width <= 0) or (ReadArea.Height <= 0) then
    Exit(nil);

  if (not FHasLiveGLContext) or (SourceTexture = nil) or
     (SourceTexture as TOpenGLRHITexture).IsDefaultBackbuffer then
    Exit(inherited GrabTextureFloatImage(SourceTexture, ReadArea));

  GLTex := SourceTexture as TOpenGLRHITexture;
  Result := TRGBAlphaFloatImage.Create(ReadArea.Width, ReadArea.Height);
  PrevReadFBO := 0;
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, @PrevReadFBO);
  try
    {$ifndef OpenGLES}
    if Assigned(glMemoryBarrier) then
      glMemoryBarrier(GL_FRAMEBUFFER_BARRIER_BIT or GL_TEXTURE_UPDATE_BARRIER_BIT or
        GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    {$endif}
    glBindFramebuffer(GL_READ_FRAMEBUFFER, FReadbackFBO);
    glPixelStorei(GL_PACK_ALIGNMENT, 4);

    if GLTex.Desc.Format in [pfD16_UNorm, pfD24_UNorm_S8_UInt, pfD32_Float, pfD32_Float_S8_UInt] then
    begin
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GLTex.Target, GLTex.Handle, 0);
      TotalPixels := PtrUInt(ReadArea.Width) * PtrUInt(ReadArea.Height);
      SetLength(DepthBuf, TotalPixels);
      glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
        GL_DEPTH_COMPONENT, GL_FLOAT, @DepthBuf[0]);
      DstVec4 := Result.Pixels;
      for PIdx := 0 to TotalPixels - 1 do
        DstVec4[PIdx] := Vector4(DepthBuf[PIdx], DepthBuf[PIdx], DepthBuf[PIdx], 1.0);
    end else
    begin
      glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
      if GLTex.Desc.Dimension = tdCube then
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
          GL_TEXTURE_CUBE_MAP_POSITIVE_X, GLTex.Handle, 0)
      else if GLTex.Desc.Dimension in [td2DArray, td3D] then
        glFramebufferTextureLayer(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GLTex.Handle, 0, 0)
      else
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GLTex.Target, GLTex.Handle, 0);
      {$ifndef OpenGLES}
      glReadBuffer(GL_COLOR_ATTACHMENT0);
      {$endif}
      glReadPixels(ReadArea.Left, ReadArea.Bottom, ReadArea.Width, ReadArea.Height,
        GL_RGBA, GL_FLOAT, Result.RawPixels);
    end;
  finally
    glBindFramebuffer(GL_READ_FRAMEBUFFER, TGLuint(PrevReadFBO));
  end;
end;

procedure TOpenGLRHIDevice.LoadPipelineCache(const CacheFileUrl: String);
begin
  if CacheFileUrl = '' then ;
end;

procedure TOpenGLRHIDevice.SavePipelineCache(const CacheFileUrl: String);
begin
  if CacheFileUrl = '' then ;
end;

end.
