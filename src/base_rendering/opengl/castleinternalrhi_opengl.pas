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

{ OpenGL and OpenGLES implementation of CastleInternalRHI.
  Wraps Castle Game Engine's existing GL context and translates explicit RHI
  Pipelines, Descriptor Sets, and Dynamic Rendering passes into GL state changes. }
unit CastleInternalRHI_OpenGL;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Math, Generics.Collections,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleLog,
  CastleGLUtils, CastleGLImages, CastleRenderOptions,
  CastleInternalRHI;

type
  TOpenGLRHIDevice = class;

  { -------------------------------------------------------------------------- }
  { Concrete OpenGL Resource Classes                                           }
  { -------------------------------------------------------------------------- }

  TOpenGLRHIBuffer = class(TRHIBuffer)
  private
    FHandle: TGLuint;
    FTarget: TGLenum;
    FMappedFallback: Pointer;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHIBufferDesc;
      const InitialData: Pointer);
    destructor Destroy; override;

    function Map: Pointer; override;
    procedure Unmap; override;
    procedure UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt = 0); override;

    property Handle: TGLuint read FHandle;
    property Target: TGLenum read FTarget;
  end;

  TOpenGLRHITexture = class(TRHITexture)
  private
    FHandle: TGLuint;
    FTarget: TGLenum;
    FIsDefaultBackbuffer: Boolean;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHITextureDesc); overload;
    constructor CreateDefaultBackbufferWrapper(const ADevice: TOpenGLRHIDevice;
      const ADesc: TRHITextureDesc); overload;
    destructor Destroy; override;

    procedure UploadImage(const AImage: TEncodedImage;
      const AMipLevel: Cardinal = 0; const AArrayLayer: Cardinal = 0); override;

    property Handle: TGLuint read FHandle;
    property Target: TGLenum read FTarget;
    property IsDefaultBackbuffer: Boolean read FIsDefaultBackbuffer;
  end;

  TOpenGLRHISampler = class(TRHISampler)
  private
    FHandle: TGLuint;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHISamplerDesc);
    destructor Destroy; override;
    property Handle: TGLuint read FHandle;
  end;

  TOpenGLRHIShaderModule = class(TRHIShaderModule)
  private
    FHandle: TGLuint;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice; const AStage: TRHIShaderStage;
      const AGLSLSource: String; const ADebugName: String);
    destructor Destroy; override;
    property Handle: TGLuint read FHandle;
  end;

  TOpenGLRHIDescriptorSetLayout = class(TRHIDescriptorSetLayout)
  private
    FBindings: array of TRHIDescriptorBindingDesc;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ABindings: array of TRHIDescriptorBindingDesc);
  end;

  TOpenGLBoundBuffer = record
    Binding: Cardinal;
    Buffer: TOpenGLRHIBuffer;
    Offset: PtrUInt;
    Range: PtrUInt;
    IsStorage: Boolean;
  end;

  TOpenGLBoundTexture = record
    Binding: Cardinal;
    Texture: TOpenGLRHITexture;
    Sampler: TOpenGLRHISampler;
  end;

  TOpenGLRHIDescriptorSet = class(TRHIDescriptorSet)
  private
    FLayout: TOpenGLRHIDescriptorSetLayout;
    FBoundBuffers: array of TOpenGLBoundBuffer;
    FBoundTextures: array of TOpenGLBoundTexture;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ALayout: TOpenGLRHIDescriptorSetLayout);
    procedure BindUniformBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindStorageBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindTexture(const Binding: Cardinal; const Texture: TRHITexture;
      const Sampler: TRHISampler); override;
    procedure Update; override;
  end;

  TOpenGLRHIPipeline = class(TRHIPipeline)
  private
    FProgramHandle: TGLuint;
    FVAOHandle: TGLuint;
    FPushConstantUniformLoc: TGLint;
    FPushConstantUBO: TGLuint;
    FPrimitiveMode: TGLenum;
  public
    constructor Create(const ADevice: TOpenGLRHIDevice;
      const ADesc: TRHIGraphicsPipelineDesc);
    destructor Destroy; override;

    property ProgramHandle: TGLuint read FProgramHandle;
    property VAOHandle: TGLuint read FVAOHandle;
    property PrimitiveMode: TGLenum read FPrimitiveMode;
    property PushConstantUBO: TGLuint read FPushConstantUBO;
  end;

  { -------------------------------------------------------------------------- }
  { OpenGL Command List (Immediate State Machine Execution)                    }
  { -------------------------------------------------------------------------- }

  TOpenGLRHICommandList = class(TRHICommandList)
  private
    FDevice: TOpenGLRHIDevice;
    FScratchFBO: TGLuint;
    FCurrentPipeline: TOpenGLRHIPipeline;
    FCurrentIndexBuffer: TOpenGLRHIBuffer;
    FCurrentIndexOffset: PtrUInt;
    FCurrentIndexType: TGLenum;
    FCurrentIndexSize: Cardinal;
    FScissorEnabled: Boolean;
    procedure ApplyPipelineState(const NewPipeline: TOpenGLRHIPipeline);
    procedure ApplyVertexLayout(const Pipeline: TOpenGLRHIPipeline;
      const FirstBinding: Cardinal; const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt);
  public
    constructor Create(const ADevice: TOpenGLRHIDevice);
    destructor Destroy; override;

    procedure BeginRecording; override;
    procedure EndRecording; override;

    procedure TransitionTexture(const Texture: TRHITexture;
      const NewState: TRHIResourceState); override;
    procedure GenerateMipmaps(const Texture: TRHITexture); override;

    procedure BeginRendering(const Info: TRHIRenderingInfo); override;
    procedure EndRendering; override;

    procedure SetViewport(const ViewportRect: TFloatRectangle;
      const MinDepth: Single = 0.0; const MaxDepth: Single = 1.0); override;
    procedure SetScissor(const ScissorRect: TRectangle); override;
    procedure SetDepthBias(const ConstantFactor, SlopeFactor: Single); override;

    procedure BindPipeline(const Pipeline: TRHIPipeline); override;
    procedure BindDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); override;
    procedure PushConstants(const StageFlags: TRHIShaderStageFlags;
      const Offset, Size: Cardinal; const Data); override;

    procedure BindVertexBuffers(const FirstBinding: Cardinal;
      const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt); override;
    procedure BindIndexBuffer(const Buffer: TRHIBuffer;
      const Offset: PtrUInt; const IndexType: TRHIIndexType); override;

    procedure Draw(const VertexCount, InstanceCount, FirstVertex, FirstInstance: Cardinal); override;
    procedure DrawIndexed(const IndexCount, InstanceCount, FirstIndex: Cardinal;
      const VertexOffset: Integer; const FirstInstance: Cardinal); override;
    procedure DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); override;

    procedure PushDebugGroup(const LabelName: String; const Color: TCastleColor); override;
    procedure PopDebugGroup; override;
  end;

  { -------------------------------------------------------------------------- }
  { OpenGL Render Device                                                       }
  { -------------------------------------------------------------------------- }

  TOpenGLPipelineMap = {$ifdef FPC}specialize{$endif} TObjectDictionary<UInt64, TOpenGLRHIPipeline>;

  TOpenGLRHIDevice = class(TRHIDevice)
  private
    FCommandList: TOpenGLRHICommandList;
    FDefaultColorTarget: TOpenGLRHITexture;
    FDefaultDepthTarget: TOpenGLRHITexture;
    FPipelineMap: TOpenGLPipelineMap;
    procedure QueryDeviceCapabilities;
    function ComputePipelineHash(const ADesc: TRHIGraphicsPipelineDesc): UInt64;
  public
    constructor Create(const InitialWidth, InitialHeight: Cardinal); reintroduce;
    destructor Destroy; override;

    function BeginFrame: TRHICommandList; override;
    procedure EndFrameAndPresent; override;
    procedure ResizeSwapchain(const NewWidth, NewHeight: Cardinal); override;
    procedure WaitIdle; override;

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

    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaImage; override;

    procedure LoadPipelineCache(const CacheFileUrl: String); override;
    procedure SavePipelineCache(const CacheFileUrl: String); override;
  end;

implementation

const
  { Reserved Uniform Buffer binding slot for emulating Vulkan Push Constants in OpenGL }
  GL_PUSH_CONSTANT_UBO_BINDING = 15;
  { Stride between Descriptor Set indices in OpenGL binding slots (Set 0 = 0..3, Set 1 = 4..7, etc.) }
  GL_DESCRIPTOR_SET_SLOT_STRIDE = 4;

{ ---------------------------------------------------------------------------- }
{ OpenGL Enum Conversion Helpers                                               }
{ ---------------------------------------------------------------------------- }

procedure RHIToGLFormat(const Format: TRHIPixelFormat;
  out InternalFormat: TGLint; out PixelFormat, PixelType: TGLenum);
begin
  case Format of
    pfR8_UNorm:
      begin InternalFormat := GL_R8; PixelFormat := GL_RED; PixelType := GL_UNSIGNED_BYTE; end;
    pfRG8_UNorm:
      begin InternalFormat := GL_RG8; PixelFormat := GL_RG; PixelType := GL_UNSIGNED_BYTE; end;
    pfRGBA8_UNorm:
      begin InternalFormat := GL_RGBA8; PixelFormat := GL_RGBA; PixelType := GL_UNSIGNED_BYTE; end;
    pfRGBA8_sRGB:
      begin InternalFormat := GL_SRGB8_ALPHA8; PixelFormat := GL_RGBA; PixelType := GL_UNSIGNED_BYTE; end;
    pfRGBA16_Float:
      begin InternalFormat := GL_RGBA16F; PixelFormat := GL_RGBA; PixelType := GL_HALF_FLOAT; end;
    pfR32_Float:
      begin InternalFormat := GL_R32F; PixelFormat := GL_RED; PixelType := GL_FLOAT; end;
    pfRGBA32_Float:
      begin InternalFormat := GL_RGBA32F; PixelFormat := GL_RGBA; PixelType := GL_FLOAT; end;
    pfD16_UNorm:
      begin InternalFormat := GL_DEPTH_COMPONENT16; PixelFormat := GL_DEPTH_COMPONENT; PixelType := GL_UNSIGNED_SHORT; end;
    pfD24_UNorm_S8_UInt:
      begin InternalFormat := GL_DEPTH24_STENCIL8; PixelFormat := GL_DEPTH_STENCIL; PixelType := GL_UNSIGNED_INT_24_8; end;
    pfD32_Float:
      begin InternalFormat := GL_DEPTH_COMPONENT32F; PixelFormat := GL_DEPTH_COMPONENT; PixelType := GL_FLOAT; end;
    else
      begin InternalFormat := GL_RGBA8; PixelFormat := GL_RGBA; PixelType := GL_UNSIGNED_BYTE; end;
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
  end;
end;

function RHIToGLBlendFactor(const Factor: TRHIBlendFactor): TGLenum;
begin
  case Factor of
    bfZero:             Result := GL_ZERO;
    bfOne:              Result := GL_ONE;
    bfSrcColor:         Result := GL_SRC_COLOR;
    bfOneMinusSrcColor: Result := GL_ONE_MINUS_SRC_COLOR;
    bfDstColor:         Result := GL_DST_COLOR;
    bfOneMinusDstColor: Result := GL_ONE_MINUS_DST_COLOR;
    bfSrcAlpha:         Result := GL_SRC_ALPHA;
    bfOneMinusSrcAlpha: Result := GL_ONE_MINUS_SRC_ALPHA;
    bfDstAlpha:         Result := GL_DST_ALPHA;
    bfOneMinusDstAlpha: Result := GL_ONE_MINUS_DST_ALPHA;
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
    else               Result := GL_FUNC_ADD;
    {$endif}
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
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIBuffer                                                             }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHIBuffer.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIBufferDesc; const InitialData: Pointer);
var
  GLUsage: TGLenum;
begin
  inherited Create(ADevice, ADesc);

  if buIndex in ADesc.Usage then
    FTarget := GL_ELEMENT_ARRAY_BUFFER
  else if buUniform in ADesc.Usage then
    FTarget := GL_UNIFORM_BUFFER
  else
    FTarget := GL_ARRAY_BUFFER;

  if ADesc.MemoryUsage = muCpuToGpu then
    GLUsage := GL_DYNAMIC_DRAW
  else if ADesc.MemoryUsage = muGpuToCpu then
    GLUsage := GL_STREAM_READ
  else
    GLUsage := GL_STATIC_DRAW;

  glGenBuffers(1, @FHandle);
  glBindBuffer(FTarget, FHandle);
  glBufferData(FTarget, ADesc.Size, InitialData, GLUsage);
  glBindBuffer(FTarget, 0);
end;

destructor TOpenGLRHIBuffer.Destroy;
begin
  if FMappedFallback <> nil then
    FreeMem(FMappedFallback);
  if FHandle <> 0 then
    glDeleteBuffers(1, @FHandle);
  inherited;
end;

function TOpenGLRHIBuffer.Map: Pointer;
begin
  glBindBuffer(FTarget, FHandle);
  {$ifndef OpenGLES}
  Result := glMapBufferRange(FTarget, 0, FDesc.Size, GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_BUFFER_BIT);
  if Result <> nil then Exit;
  {$endif}
  { Fallback for WebGL / older OpenGLES without glMapBufferRange }
  if FMappedFallback = nil then
    GetMem(FMappedFallback, FDesc.Size);
  Result := FMappedFallback;
end;

procedure TOpenGLRHIBuffer.Unmap;
begin
  glBindBuffer(FTarget, FHandle);
  if FMappedFallback <> nil then
    glBufferSubData(FTarget, 0, FDesc.Size, FMappedFallback)
  else
  begin
    {$ifndef OpenGLES}
    glUnmapBuffer(FTarget);
    {$endif}
  end;
  glBindBuffer(FTarget, 0);
end;

procedure TOpenGLRHIBuffer.UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt);
begin
  glBindBuffer(FTarget, FHandle);
  glBufferSubData(FTarget, Offset, Size, @Data);
  glBindBuffer(FTarget, 0);
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHITexture                                                            }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHITexture.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHITextureDesc);
var
  InternalFormat: TGLint;
  PixelFormat, PixelType: TGLenum;
  Mip, W, H: Cardinal;
begin
  inherited Create(ADevice, ADesc);
  FIsDefaultBackbuffer := False;

  case ADesc.Dimension of
    td2D:      FTarget := GL_TEXTURE_2D;
    tdCube:    FTarget := GL_TEXTURE_CUBE_MAP;
    td3D:      FTarget := GL_TEXTURE_3D;
    else       FTarget := GL_TEXTURE_2D;
  end;

  RHIToGLFormat(ADesc.Format, InternalFormat, PixelFormat, PixelType);

  glGenTextures(1, @FHandle);
  glBindTexture(FTarget, FHandle);

  W := ADesc.Width;
  H := ADesc.Height;
  for Mip := 0 to ADesc.MipLevels - 1 do
  begin
    glTexImage2D(FTarget, Mip, InternalFormat, W, H, 0, PixelFormat, PixelType, nil);
    W := Max(1, W div 2);
    H := Max(1, H div 2);
  end;

  glTexParameteri(FTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(FTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindTexture(FTarget, 0);
end;

constructor TOpenGLRHITexture.CreateDefaultBackbufferWrapper(
  const ADevice: TOpenGLRHIDevice; const ADesc: TRHITextureDesc);
begin
  inherited Create(ADevice, ADesc);
  FIsDefaultBackbuffer := True;
  FHandle := 0;
  FTarget := GL_TEXTURE_2D;
end;

destructor TOpenGLRHITexture.Destroy;
begin
  if (not FIsDefaultBackbuffer) and (FHandle <> 0) then
    glDeleteTextures(1, @FHandle);
  inherited;
end;

procedure TOpenGLRHITexture.UploadImage(const AImage: TEncodedImage;
  const AMipLevel: Cardinal; const AArrayLayer: Cardinal);
var
  InternalFormat: TGLint;
  PixelFormat, PixelType: TGLenum;
  CastleImg: TCastleImage;
begin
  if FIsDefaultBackbuffer then Exit;

  RHIToGLFormat(FDesc.Format, InternalFormat, PixelFormat, PixelType);
  glBindTexture(FTarget, FHandle);

  if AImage is TCastleImage then
  begin
    CastleImg := TCastleImage(AImage);
    glTexSubImage2D(FTarget, AMipLevel, 0, 0, CastleImg.Width, CastleImg.Height,
      PixelFormat, PixelType, CastleImg.RawPixels);
  end;

  glBindTexture(FTarget, 0);
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHISampler, ShaderModule, DescriptorSetLayout, DescriptorSet          }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHISampler.Create(const ADevice: TOpenGLRHIDevice; const ADesc: TRHISamplerDesc);
begin
  inherited Create(ADevice, ADesc);
  glGenSamplers(1, @FHandle);
  if ADesc.MagFilter = sfNearest then
    glSamplerParameteri(FHandle, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  else
    glSamplerParameteri(FHandle, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  if ADesc.MinFilter = sfNearest then
    glSamplerParameteri(FHandle, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
  else
    glSamplerParameteri(FHandle, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  if ADesc.CompareEnable then
  begin
    glSamplerParameteri(FHandle, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glSamplerParameteri(FHandle, GL_TEXTURE_COMPARE_FUNC, RHIToGLCompareOp(ADesc.CompareOp));
  end;
end;

destructor TOpenGLRHISampler.Destroy;
begin
  if FHandle <> 0 then
    glDeleteSamplers(1, @FHandle);
  inherited;
end;

constructor TOpenGLRHIShaderModule.Create(const ADevice: TOpenGLRHIDevice;
  const AStage: TRHIShaderStage; const AGLSLSource: String; const ADebugName: String);
var
  GLStage: TGLenum;
  SrcPtr: PGLchar;
  SrcLen, CompileStatus, LogLen: TGLint;
  ErrorLog: String;
begin
  inherited Create(ADevice, AStage, ADebugName);

  case AStage of
    ssVertex:   GLStage := GL_VERTEX_SHADER;
    ssFragment: GLStage := GL_FRAGMENT_SHADER;
    {$ifndef OpenGLES}
    ssGeometry: GLStage := GL_GEOMETRY_SHADER;
    ssCompute:  GLStage := GL_COMPUTE_SHADER;
    {$endif}
    else        GLStage := GL_VERTEX_SHADER;
  end;

  FHandle := glCreateShader(GLStage);
  SrcPtr := PGLchar(AnsiString(AGLSLSource));
  SrcLen := Length(AGLSLSource);
  glShaderSource(FHandle, 1, @SrcPtr, @SrcLen);
  glCompileShader(FHandle);

  glGetShaderiv(FHandle, GL_COMPILE_STATUS, @CompileStatus);
  if CompileStatus = GL_FALSE then
  begin
    glGetShaderiv(FHandle, GL_INFO_LOG_LENGTH, @LogLen);
    SetLength(ErrorLog, LogLen);
    if LogLen > 0 then
      glGetShaderInfoLog(FHandle, LogLen, nil, PGLchar(ErrorLog));
    glDeleteShader(FHandle);
    FHandle := 0;
    raise ERHIShaderCompilationError.CreateFmt('OpenGL Shader (%s) compilation failed:%s%s',
      [ADebugName, LineEnding, ErrorLog]);
  end;
end;

destructor TOpenGLRHIShaderModule.Destroy;
begin
  if FHandle <> 0 then
    glDeleteShader(FHandle);
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

constructor TOpenGLRHIDescriptorSet.Create(const ADevice: TOpenGLRHIDevice;
  const ALayout: TOpenGLRHIDescriptorSetLayout);
begin
  inherited Create(ADevice, '');
  FLayout := ALayout;
end;

procedure TOpenGLRHIDescriptorSet.BindUniformBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
var
  Idx: Integer;
begin
  Idx := Length(FBoundBuffers);
  SetLength(FBoundBuffers, Idx + 1);
  FBoundBuffers[Idx].Binding := Binding;
  FBoundBuffers[Idx].Buffer := Buffer as TOpenGLRHIBuffer;
  FBoundBuffers[Idx].Offset := Offset;
  if Range = 0 then
    FBoundBuffers[Idx].Range := Buffer.Desc.Size
  else
    FBoundBuffers[Idx].Range := Range;
  FBoundBuffers[Idx].IsStorage := False;
end;

procedure TOpenGLRHIDescriptorSet.BindStorageBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
begin
  BindUniformBuffer(Binding, Buffer, Offset, Range);
  FBoundBuffers[High(FBoundBuffers)].IsStorage := True;
end;

procedure TOpenGLRHIDescriptorSet.BindTexture(const Binding: Cardinal;
  const Texture: TRHITexture; const Sampler: TRHISampler);
var
  Idx: Integer;
begin
  Idx := Length(FBoundTextures);
  SetLength(FBoundTextures, Idx + 1);
  FBoundTextures[Idx].Binding := Binding;
  FBoundTextures[Idx].Texture := Texture as TOpenGLRHITexture;
  FBoundTextures[Idx].Sampler := Sampler as TOpenGLRHISampler;
end;

procedure TOpenGLRHIDescriptorSet.Update;
begin
  { No-op on OpenGL; bindings are applied when BindDescriptorSet is called on the CommandList }
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIPipeline                                                           }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHIPipeline.Create(const ADevice: TOpenGLRHIDevice;
  const ADesc: TRHIGraphicsPipelineDesc);
var
  LinkStatus, LogLen: TGLint;
  ErrorLog: String;
  I: Integer;
begin
  inherited Create(ADevice, ADesc);
  FPrimitiveMode := RHIToGLTopology(ADesc.Topology);

  FProgramHandle := glCreateProgram();
  if ADesc.VertexShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.VertexShader as TOpenGLRHIShaderModule).Handle);
  if ADesc.FragmentShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.FragmentShader as TOpenGLRHIShaderModule).Handle);
  if ADesc.GeometryShader <> nil then
    glAttachShader(FProgramHandle, (ADesc.GeometryShader as TOpenGLRHIShaderModule).Handle);

  { Bind explicit vertex attribute locations before linking for GL 3.2 compatibility }
  for I := 0 to High(ADesc.VertexInput.Attributes) do
    if ADesc.VertexInput.Attributes[I].AttributeName <> '' then
      glBindAttribLocation(FProgramHandle, ADesc.VertexInput.Attributes[I].Location,
        PGLchar(AnsiString(ADesc.VertexInput.Attributes[I].AttributeName)));

  glLinkProgram(FProgramHandle);
  glGetProgramiv(FProgramHandle, GL_LINK_STATUS, @LinkStatus);
  if LinkStatus = GL_FALSE then
  begin
    glGetProgramiv(FProgramHandle, GL_INFO_LOG_LENGTH, @LogLen);
    SetLength(ErrorLog, LogLen);
    if LogLen > 0 then
      glGetProgramInfoLog(FProgramHandle, LogLen, nil, PGLchar(ErrorLog));
    glDeleteProgram(FProgramHandle);
    FProgramHandle := 0;
    raise ERHIShaderCompilationError.CreateFmt('OpenGL Program (%s) link failed:%s%s',
      [ADesc.DebugName, LineEnding, ErrorLog]);
  end;

  glGenVertexArrays(1, @FVAOHandle);

  { Allocate a tiny dedicated UBO if the pipeline uses Push Constants }
  if ADesc.PushConstantSize > 0 then
  begin
    glGenBuffers(1, @FPushConstantUBO);
    glBindBuffer(GL_UNIFORM_BUFFER, FPushConstantUBO);
    glBufferData(GL_UNIFORM_BUFFER, ADesc.PushConstantSize, nil, GL_STREAM_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
  end;
end;

destructor TOpenGLRHIPipeline.Destroy;
begin
  if FPushConstantUBO <> 0 then
    glDeleteBuffers(1, @FPushConstantUBO);
  if FVAOHandle <> 0 then
    glDeleteVertexArrays(1, @FVAOHandle);
  if FProgramHandle <> 0 then
    glDeleteProgram(FProgramHandle);
  inherited;
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHICommandList                                                        }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHICommandList.Create(const ADevice: TOpenGLRHIDevice);
begin
  inherited Create;
  FDevice := ADevice;
  glGenFramebuffers(1, @FScratchFBO);
end;

destructor TOpenGLRHICommandList.Destroy;
begin
  if FScratchFBO <> 0 then
    glDeleteFramebuffers(1, @FScratchFBO);
  inherited;
end;

procedure TOpenGLRHICommandList.BeginRecording;
begin
  FCurrentPipeline := nil;
  FCurrentIndexBuffer := nil;
end;

procedure TOpenGLRHICommandList.EndRecording;
begin
  glBindVertexArray(0);
  glUseProgram(0);
end;

procedure TOpenGLRHICommandList.TransitionTexture(const Texture: TRHITexture;
  const NewState: TRHIResourceState);
begin
  { OpenGL drivers track image layout transitions implicitly }
  Texture.CurrentState := NewState;
end;

procedure TOpenGLRHICommandList.GenerateMipmaps(const Texture: TRHITexture);
var
  GLTex: TOpenGLRHITexture;
begin
  GLTex := Texture as TOpenGLRHITexture;
  if GLTex.IsDefaultBackbuffer then Exit;
  glBindTexture(GLTex.Target, GLTex.Handle);
  glGenerateMipmap(GLTex.Target);
  glBindTexture(GLTex.Target, 0);
end;

procedure TOpenGLRHICommandList.BeginRendering(const Info: TRHIRenderingInfo);
var
  ClearMask: TGLbitfield;
  ColorTex, DepthTex: TOpenGLRHITexture;
  UseDefaultFBO: Boolean;
  I: Integer;
begin
  UseDefaultFBO := (Length(Info.ColorAttachments) > 0) and
    (Info.ColorAttachments[0].Texture <> nil) and
    (Info.ColorAttachments[0].Texture as TOpenGLRHITexture).IsDefaultBackbuffer;

  if UseDefaultFBO then
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
  else
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FScratchFBO);
    for I := 0 to High(Info.ColorAttachments) do
    begin
      ColorTex := Info.ColorAttachments[I].Texture as TOpenGLRHITexture;
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + I,
        ColorTex.Target, ColorTex.Handle, Info.ColorAttachments[I].MipLevel);
    end;

    if Info.HasDepthAttachment and (Info.DepthAttachment.Texture <> nil) then
    begin
      DepthTex := Info.DepthAttachment.Texture as TOpenGLRHITexture;
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
        DepthTex.Target, DepthTex.Handle, Info.DepthAttachment.MipLevel);
    end else
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, 0, 0);
  end;

  { Temporarily disable scissor test so glClear affects the full render area }
  if FScissorEnabled then
    glDisable(GL_SCISSOR_TEST);

  ClearMask := 0;
  if (Length(Info.ColorAttachments) > 0) and (Info.ColorAttachments[0].LoadOp = loClear) then
  begin
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
  end;

  if ClearMask <> 0 then
    glClear(ClearMask);

  if FScissorEnabled then
    glEnable(GL_SCISSOR_TEST);
end;

procedure TOpenGLRHICommandList.EndRendering;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TOpenGLRHICommandList.SetViewport(const ViewportRect: TFloatRectangle;
  const MinDepth, MaxDepth: Single);
begin
  glViewport(Round(ViewportRect.Left), Round(ViewportRect.Bottom),
    Round(ViewportRect.Width), Round(ViewportRect.Height));
  {$ifdef OpenGLES}
  glDepthRangef(MinDepth, MaxDepth);
  {$else}
  glDepthRange(MinDepth, MaxDepth);
  {$endif}
end;

procedure TOpenGLRHICommandList.SetScissor(const ScissorRect: TRectangle);
begin
  if not FScissorEnabled then
  begin
    glEnable(GL_SCISSOR_TEST);
    FScissorEnabled := True;
  end;
  glScissor(ScissorRect.Left, ScissorRect.Bottom, ScissorRect.Width, ScissorRect.Height);
end;

procedure TOpenGLRHICommandList.SetDepthBias(const ConstantFactor, SlopeFactor: Single);
begin
  glPolygonOffset(SlopeFactor, ConstantFactor);
end;

procedure TOpenGLRHICommandList.ApplyPipelineState(const NewPipeline: TOpenGLRHIPipeline);
var
  D: TRHIGraphicsPipelineDesc;
begin
  D := NewPipeline.Desc;

  glUseProgram(NewPipeline.ProgramHandle);
  glBindVertexArray(NewPipeline.VAOHandle);

  { Rasterizer State }
  if D.Rasterizer.CullMode = cmNone then
    glDisable(GL_CULL_FACE)
  else
  begin
    glEnable(GL_CULL_FACE);
    if D.Rasterizer.CullMode = cmBack then
      glCullFace(GL_BACK)
    else
      glCullFace(GL_FRONT);
  end;

  if D.Rasterizer.FrontFace = ffCounterClockwise then
    glFrontFace(GL_CCW)
  else
    glFrontFace(GL_CW);

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

  { Depth State }
  if D.DepthStencil.DepthTestEnable then
  begin
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(RHIToGLCompareOp(D.DepthStencil.DepthCompareOp));
  end else
    glDisable(GL_DEPTH_TEST);

  glDepthMask( TGLboolean(D.DepthStencil.DepthWriteEnable) );

  { Blend State }
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

  glColorMask(
    TGLboolean((D.BlendState.ColorWriteMask and 1) <> 0),
    TGLboolean((D.BlendState.ColorWriteMask and 2) <> 0),
    TGLboolean((D.BlendState.ColorWriteMask and 4) <> 0),
    TGLboolean((D.BlendState.ColorWriteMask and 8) <> 0));
end;

procedure TOpenGLRHICommandList.BindPipeline(const Pipeline: TRHIPipeline);
var
  GLPipe: TOpenGLRHIPipeline;
begin
  GLPipe := Pipeline as TOpenGLRHIPipeline;
  if FCurrentPipeline <> GLPipe then
  begin
    ApplyPipelineState(GLPipe);
    FCurrentPipeline := GLPipe;
  end;
end;

procedure TOpenGLRHICommandList.BindDescriptorSet(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet; const DynamicOffsets: array of Cardinal);
var
  GLSet: TOpenGLRHIDescriptorSet;
  I, DynIdx: Integer;
  Slot: Cardinal;
  ActualOffset: PtrUInt;
begin
  GLSet := DescriptorSet as TOpenGLRHIDescriptorSet;
  DynIdx := 0;

  { 1. Bind Uniform / Storage Buffers }
  for I := 0 to High(GLSet.FBoundBuffers) do
  begin
    Slot := SetIndex * GL_DESCRIPTOR_SET_SLOT_STRIDE + GLSet.FBoundBuffers[I].Binding;
    ActualOffset := GLSet.FBoundBuffers[I].Offset;
    if DynIdx <= High(DynamicOffsets) then
    begin
      Inc(ActualOffset, DynamicOffsets[DynIdx]);
      Inc(DynIdx);
    end;

    glBindBufferRange(GL_UNIFORM_BUFFER, Slot,
      GLSet.FBoundBuffers[I].Buffer.Handle, ActualOffset, GLSet.FBoundBuffers[I].Range);
  end;

  { 2. Bind Combined Image Samplers }
  for I := 0 to High(GLSet.FBoundTextures) do
  begin
    Slot := SetIndex * GL_DESCRIPTOR_SET_SLOT_STRIDE + GLSet.FBoundTextures[I].Binding;
    glActiveTexture(GL_TEXTURE0 + Slot);
    glBindTexture(GLSet.FBoundTextures[I].Texture.Target, GLSet.FBoundTextures[I].Texture.Handle);
    if GLSet.FBoundTextures[I].Sampler <> nil then
      glBindSampler(Slot, GLSet.FBoundTextures[I].Sampler.Handle);
  end;
end;

procedure TOpenGLRHICommandList.PushConstants(const StageFlags: TRHIShaderStageFlags;
  const Offset, Size: Cardinal; const Data);
begin
  Assert(FCurrentPipeline <> nil, 'BindPipeline must be called before PushConstants');
  if FCurrentPipeline.PushConstantUBO <> 0 then
  begin
    glBindBuffer(GL_UNIFORM_BUFFER, FCurrentPipeline.PushConstantUBO);
    glBufferSubData(GL_UNIFORM_BUFFER, Offset, Size, @Data);
    glBindBufferBase(GL_UNIFORM_BUFFER, GL_PUSH_CONSTANT_UBO_BINDING, FCurrentPipeline.PushConstantUBO);
  end;
end;

procedure TOpenGLRHICommandList.ApplyVertexLayout(const Pipeline: TOpenGLRHIPipeline;
  const FirstBinding: Cardinal; const Buffers: array of TRHIBuffer;
  const Offsets: array of PtrUInt);
var
  I, BIdx: Integer;
  Attr: TRHIVertexAttributeDesc;
  BindDesc: TRHIVertexBindingDesc;
  GLBuf: TOpenGLRHIBuffer;
  CompCount: TGLint;
  CompType: TGLenum;
  Normalized: TGLboolean;
begin
  for I := 0 to High(Pipeline.Desc.VertexInput.Attributes) do
  begin
    Attr := Pipeline.Desc.VertexInput.Attributes[I];
    BIdx := Integer(Attr.Binding) - Integer(FirstBinding);
    if (BIdx < 0) or (BIdx > High(Buffers)) then Continue;

    BindDesc := Pipeline.Desc.VertexInput.Bindings[Attr.Binding];
    GLBuf := Buffers[BIdx] as TOpenGLRHIBuffer;

    glBindBuffer(GL_ARRAY_BUFFER, GLBuf.Handle);
    glEnableVertexAttribArray(Attr.Location);

    Normalized := GL_FALSE;
    case Attr.Format of
      vafFloat:            begin CompCount := 1; CompType := GL_FLOAT; end;
      vafVector2:          begin CompCount := 2; CompType := GL_FLOAT; end;
      vafVector3:          begin CompCount := 3; CompType := GL_FLOAT; end;
      vafVector4:          begin CompCount := 4; CompType := GL_FLOAT; end;
      vafUByte4Normalized: begin CompCount := 4; CompType := GL_UNSIGNED_BYTE; Normalized := GL_TRUE; end;
      else                 begin CompCount := 4; CompType := GL_FLOAT; end;
    end;

    glVertexAttribPointer(Attr.Location, CompCount, CompType, Normalized,
      BindDesc.Stride, Pointer(Offsets[BIdx] + Attr.Offset));
  end;
end;

procedure TOpenGLRHICommandList.BindVertexBuffers(const FirstBinding: Cardinal;
  const Buffers: array of TRHIBuffer; const Offsets: array of PtrUInt);
begin
  Assert(FCurrentPipeline <> nil, 'BindPipeline must be called before BindVertexBuffers');
  ApplyVertexLayout(FCurrentPipeline, FirstBinding, Buffers, Offsets);
end;

procedure TOpenGLRHICommandList.BindIndexBuffer(const Buffer: TRHIBuffer;
  const Offset: PtrUInt; const IndexType: TRHIIndexType);
begin
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
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FCurrentIndexBuffer.Handle);
end;

procedure TOpenGLRHICommandList.Draw(const VertexCount, InstanceCount,
  FirstVertex, FirstInstance: Cardinal);
begin
  if InstanceCount <= 1 then
    glDrawArrays(FCurrentPipeline.PrimitiveMode, FirstVertex, VertexCount)
  else
    glDrawArraysInstanced(FCurrentPipeline.PrimitiveMode, FirstVertex, VertexCount, InstanceCount);
end;

procedure TOpenGLRHICommandList.DrawIndexed(const IndexCount, InstanceCount,
  FirstIndex: Cardinal; const VertexOffset: Integer; const FirstInstance: Cardinal);
var
  IndexPtr: Pointer;
begin
  IndexPtr := Pointer(FCurrentIndexOffset + FirstIndex * FCurrentIndexSize);
  if InstanceCount <= 1 then
    glDrawElements(FCurrentPipeline.PrimitiveMode, IndexCount, FCurrentIndexType, IndexPtr)
  else
    glDrawElementsInstanced(FCurrentPipeline.PrimitiveMode, IndexCount, FCurrentIndexType, IndexPtr, InstanceCount);
end;

procedure TOpenGLRHICommandList.DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const DrawCount, Stride: Cardinal);
begin
  { Bind GL_DRAW_INDIRECT_BUFFER and call glMultiDrawElementsIndirect when supported }
end;

procedure TOpenGLRHICommandList.PushDebugGroup(const LabelName: String; const Color: TCastleColor);
begin
  { Optional GL_KHR_debug glPushDebugGroup }
end;

procedure TOpenGLRHICommandList.PopDebugGroup;
begin
  { Optional GL_KHR_debug glPopDebugGroup }
end;

{ ---------------------------------------------------------------------------- }
{ TOpenGLRHIDevice                                                             }
{ ---------------------------------------------------------------------------- }

constructor TOpenGLRHIDevice.Create(const InitialWidth, InitialHeight: Cardinal);
begin
  inherited Create;
  FPipelineMap := TOpenGLPipelineMap.Create([doOwnsValues]);
  QueryDeviceCapabilities;

  FCommandList := TOpenGLRHICommandList.Create(Self);
  FDefaultColorTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(InitialWidth, InitialHeight, pfRGBA8_UNorm, [tuColorAttachment]));
  FDefaultDepthTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(InitialWidth, InitialHeight, pfD24_UNorm_S8_UInt, [tuDepthStencil]));
end;

destructor TOpenGLRHIDevice.Destroy;
begin
  FreeAndNil(FPipelineMap);
  FreeAndNil(FDefaultColorTarget);
  FreeAndNil(FDefaultDepthTarget);
  FreeAndNil(FCommandList);
  inherited;
end;

procedure TOpenGLRHIDevice.QueryDeviceCapabilities;
begin
  {$ifdef OpenGLES}
  FCapabilities.Backend := rbOpenGLES;
  {$else}
  FCapabilities.Backend := rbOpenGL;
  {$endif}
  FCapabilities.DeviceName := String(PAnsiChar(glGetString(GL_RENDERER)));
  FCapabilities.MaxTextureSize := GLFeatures.MaxTextureSize;
  FCapabilities.MaxPushConstantsSize := 256;
end;

function TOpenGLRHIDevice.BeginFrame: TRHICommandList;
begin
  FCommandList.BeginRecording;
  Result := FCommandList;
end;

procedure TOpenGLRHIDevice.EndFrameAndPresent;
begin
  FCommandList.EndRecording;
  { Note: Actual SwapBuffers (wglSwapBuffers / glXSwapBuffers / eglSwapBuffers)
    is still called by TCastleWindow after EventRender finishes }
  FFrameIndex := (FFrameIndex + 1) mod MaxFramesInFlight;
end;

procedure TOpenGLRHIDevice.ResizeSwapchain(const NewWidth, NewHeight: Cardinal);
begin
  FreeAndNil(FDefaultColorTarget);
  FreeAndNil(FDefaultDepthTarget);
  FDefaultColorTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(NewWidth, NewHeight, pfRGBA8_UNorm, [tuColorAttachment]));
  FDefaultDepthTarget := TOpenGLRHITexture.CreateDefaultBackbufferWrapper(Self,
    TRHITextureDesc.Create2D(NewWidth, NewHeight, pfD24_UNorm_S8_UInt, [tuDepthStencil]));
end;

procedure TOpenGLRHIDevice.WaitIdle;
begin
  glFinish;
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
begin
  Result := PtrUInt(ADesc.VertexShader) xor (PtrUInt(ADesc.FragmentShader) shl 13) xor
    (UInt64(Ord(ADesc.BlendState.BlendEnable)) shl 32) xor
    (UInt64(Ord(ADesc.DepthStencil.DepthTestEnable)) shl 33) xor
    (UInt64(Ord(ADesc.Rasterizer.CullMode)) shl 35);
end;

function TOpenGLRHIDevice.GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline;
var
  HashKey: UInt64;
  FoundPipeline: TOpenGLRHIPipeline;
begin
  HashKey := ComputePipelineHash(ADesc);
  if not FPipelineMap.TryGetValue(HashKey, FoundPipeline) then
  begin
    FoundPipeline := TOpenGLRHIPipeline.Create(Self, ADesc);
    FPipelineMap.Add(HashKey, FoundPipeline);
  end;
  Result := FoundPipeline;
end;

function TOpenGLRHIDevice.GrabTextureImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle): TRGBAlphaImage;
begin
  Result := SaveScreen_NoFlush(TRGBAlphaImage, ReadArea, cbBack) as TRGBAlphaImage;
end;

procedure TOpenGLRHIDevice.LoadPipelineCache(const CacheFileUrl: String);
begin
  { Optional GL_ARB_get_program_binary cache }
end;

procedure TOpenGLRHIDevice.SavePipelineCache(const CacheFileUrl: String);
begin
  { Optional GL_ARB_get_program_binary cache }
end;

end.
