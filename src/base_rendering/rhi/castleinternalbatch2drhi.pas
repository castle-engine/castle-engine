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

{ High-performance RHI 2D Quad Batcher for TDrawableImage and TCastleUserInterface.
  Coalesces 2D sprites, UI controls, 9-slices, and font glyphs into minimal draw
  calls using per-frame streaming vertex buffers and Push Constants. }
unit CastleInternalBatch2DRHI;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Math,
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleRenderOptions,
  CastleUtils, CastleInternalRHI, CastleInternalShaderRHI;

const
  { Maximum quads per single draw call (16,384 * 4 = 65,536 vertices, fitting in UInt16 indices) }
  MaxQuadsPerBatch = 16384;
  { Maximum total 2D quads streamed per frame before wrapping/growing (65,536 quads = 4 MB) }
  MaxQuadsPerFrame = 65536;

type
  { 24-byte packed 2D vertex (Position, UV, RGBA8 Color Tint) }
  PCastle2DVertex = ^TCastle2DVertex;
  TCastle2DVertex = packed record
    Position: TVector2;       // 8 bytes (Screen space X, Y in pixels)
    TexCoord: TVector2;       // 8 bytes (Normalized UV 0..1)
    ColorRgba8: Cardinal;     // 4 bytes (Packed R8G8B8A8 UNorm tint)
  end;

  { Push Constant payload for 2D rendering (64 bytes = 4 x vec4 in CastleDraw.u_ModelViewMatrix[0..3]) }
  TCastle2DPushConstants = packed record
    { (2.0 / ViewportWidth, 2.0 / ViewportHeight, -1.0, -1.0) }
    ViewportScaleBias: TVector4;
    { xyz = ClipLine (A, B, C), w = 1.0 when Clip is enabled else 0.0 }
    ClipLineAndEnable: TVector4;
    { x = AlphaCutoff (0.5 when FinalAlpha = acTest else 0.0),
      y = 1.0 when TextureHasOnlyAlpha else 0.0, zw = reserved }
    AlphaParams: TVector4;
    { Custom 2D shader uniform parameters mapped to CastleDraw.u_ModelViewMatrix[3] }
    CustomParams: TVector4;
  end;

  { Blend preset for fast 2D PSO lookup }
  TCastle2DBlendMode = (
    bmNone,
    bmAlphaBlend,
    bmAdditive,
    bmMultiply
  );

  { RHI-backed 2D Batch Renderer used by TDrawableImage. }
  TCastleBatch2DRenderer = class
  private
    FDevice: TRHIDevice;

    { Shaders, Layouts & Cached PSOs per blend mode }
    FVertShader: TRHIShaderModule;
    FFragShader: TRHIShaderModule;
    FTextureSetLayout: TRHIDescriptorSetLayout;
    FBasePipeDescs: array[TCastle2DBlendMode] of TRHIGraphicsPipelineDesc;
    FPipelines: array[TCastle2DBlendMode] of TRHIPipeline;
    FLastPipeline: TRHIPipeline;
    FLastBoundPipelineDesc: TRHIGraphicsPipelineDesc;
    FSamplers: array[Boolean, Boolean, Boolean] of TRHISampler;
    FLinearSampler: TRHISampler;
    FNearestSampler: TRHISampler;
    FWhitePixelTexture: TRHITexture;

    { Static Index Buffer (0,1,2, 2,3,0 ...) & Per-Frame Streaming Vertex Buffers }
    FStaticQuadIBO: TRHIBuffer;
    FFrameVBOs: array[0 .. MaxFramesInFlight - 1] of TRHIBuffer;
    FMappedVertices: PCastle2DVertex;
    FFramePassBaseQuad: Cardinal;
    FFrameWriteQuadOffset: Cardinal;
    FLastMappedFrameIndex: Cardinal;
    FLastMappedFramesPresented: Cardinal;
    FLastMappedCmd: TRHICommandList;
    FFrameQuadCount: Cardinal;
    FBatchStartQuad: Cardinal;
    FBatchFlushCount: Cardinal;

    { Active batch state }
    FIn2DPass: Boolean;
    FCurrentCmd: TRHICommandList;
    FViewportWidth: Single;
    FViewportHeight: Single;
    FCurrentTexture: TRHITexture;
    FCurrentDescriptorSet: TRHIDescriptorSet;
    FCurrentBlendMode: TCastle2DBlendMode;
    FCurrentSmoothScaling: Boolean;
    FCurrentScissor: TRectangle;
    FCurrentClip: Boolean;
    FCurrentClipLine: TVector3;
    FCurrentAlphaCutoff: Single;
    FCurrentTextureHasOnlyAlpha: Boolean;
    FCurrentCustomBlendFactors: Boolean;
    FCurrentSrcBlend: TRHIBlendFactor;
    FCurrentDstBlend: TRHIBlendFactor;
    FCurrentCustomFragShader: TRHIShaderModule;
    FCurrentCustomParams: TVector4;
    FLastCustomFragShader: TRHIShaderModule;
    FLastPushConstants: TCastle2DPushConstants;
    FTotalQuadsDrawn: Cardinal;
    FTotalFlushes: Cardinal;

    procedure CreateShadersAndPipelines;
    procedure CreateStaticIndexBuffer;
    procedure CreateFrameVertexBuffers;
    procedure CreateFallbackWhiteTexture;
  public
    constructor Create(const ADevice: TRHIDevice);
    destructor Destroy; override;

    { Called at the start of TCastleWindow / TCastleViewport 2D pass }
    procedure Begin2DPass(const ACmd: TRHICommandList;
      const AViewportWidth, AViewportHeight: Single);

    { Flushes any queued quads to the active TRHICommandList }
    procedure Flush;

    { Called at the end of the 2D pass; flushes remaining quads and unmaps buffers }
    procedure End2DPass;

    { Queues a list of screen/image rectangle pairs (used by TDrawableImage.Draw) }
    procedure DrawQuads(
      const ATexture: TRHITexture;
      const ADescriptorSet: TRHIDescriptorSet;
      const ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer;
      const ColorTint: TCastleColor;
      const BlendMode: TCastle2DBlendMode;
      const SmoothScaling: Boolean;
      const ScissorRect: TRectangle;
      const RotationRad: Single;
      const RotationCenter: TVector2;
      const RotationCenterInScreenCoords: Boolean;
      const AClip: Boolean;
      const AClipLine: TVector3;
      const AAlphaCutoff: Single;
      const ACustomBlendFactors: Boolean;
      const ASrcBlend: TRHIBlendFactor;
      const ADstBlend: TRHIBlendFactor;
      const ATextureHasOnlyAlpha: Boolean;
      const ACustomFragShader: TRHIShaderModule;
      const ACustomParams: TVector4); overload;

    procedure DrawQuads(
      const ATexture: TRHITexture;
      const ADescriptorSet: TRHIDescriptorSet;
      const ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer;
      const ColorTint: TCastleColor;
      const BlendMode: TCastle2DBlendMode;
      const SmoothScaling: Boolean;
      const ScissorRect: TRectangle;
      const RotationRad: Single;
      const RotationCenter: TVector2;
      const RotationCenterInScreenCoords: Boolean;
      const AClip: Boolean;
      const AClipLine: TVector3;
      const AAlphaCutoff: Single = 0.0;
      const ACustomBlendFactors: Boolean = False;
      const ASrcBlend: TRHIBlendFactor = bfSrcAlpha;
      const ADstBlend: TRHIBlendFactor = bfOneMinusSrcAlpha); overload;

    procedure DrawQuads(
      const ATexture: TRHITexture;
      const ADescriptorSet: TRHIDescriptorSet;
      const ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer;
      const ColorTint: TCastleColor;
      const BlendMode: TCastle2DBlendMode;
      const SmoothScaling: Boolean;
      const ScissorRect: TRectangle;
      const RotationRad: Single;
      const RotationCenter: TVector2;
      const RotationCenterInScreenCoords: Boolean = False); overload;

    procedure DrawQuads(
      const ATexture: TRHITexture;
      const ADescriptorSet: TRHIDescriptorSet;
      const ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer;
      const ColorTint: TCastleColor;
      const BlendMode: TCastle2DBlendMode;
      const SmoothScaling: Boolean;
      const ScissorRect: TRectangle); overload;

    property Device: TRHIDevice read FDevice;
    property In2DPass: Boolean read FIn2DPass;
    property ViewportWidth: Single read FViewportWidth write FViewportWidth;
    property ViewportHeight: Single read FViewportHeight write FViewportHeight;
    procedure SetViewportSize(const AViewportWidth, AViewportHeight: Single);
    property LinearSampler: TRHISampler read FLinearSampler;
    property NearestSampler: TRHISampler read FNearestSampler;
    function GetSampler(const ASmoothScaling: Boolean;
      const ARepeatS: Boolean = False; const ARepeatT: Boolean = False): TRHISampler;
    property TextureSetLayout: TRHIDescriptorSetLayout read FTextureSetLayout;
    property WhitePixelTexture: TRHITexture read FWhitePixelTexture;
    property FramePassBaseQuad: Cardinal read FFramePassBaseQuad;
    property FrameWriteQuadOffset: Cardinal read FFrameWriteQuadOffset;
    property FrameQuadCount: Cardinal read FFrameQuadCount;
    property BatchFlushCount: Cardinal read FBatchFlushCount;
    property TotalQuadsDrawn: Cardinal read FTotalQuadsDrawn;
    property TotalFlushes: Cardinal read FTotalFlushes;
    property LastPipeline: TRHIPipeline read FLastPipeline;
    property LastBoundPipelineDesc: TRHIGraphicsPipelineDesc read FLastBoundPipelineDesc;
    property LastCustomFragShader: TRHIShaderModule read FLastCustomFragShader;
    property LastPushConstants: TCastle2DPushConstants read FLastPushConstants;
  end;

{ Currently active 2D batch renderer for TDrawableImage / UI rendering. }
function ActiveBatch2DRenderer: TCastleBatch2DRenderer;
procedure SetActiveBatch2DRenderer(const ARenderer: TCastleBatch2DRenderer);

{ Returns ActiveBatch2DRenderer if already bound for ADevice, or lazily creates
  and caches a shared TCastleBatch2DRenderer instance for ADevice. }
function EnsureBatch2DRenderer(const ADevice: TRHIDevice): TCastleBatch2DRenderer;
procedure FreeGlobalBatch2DRenderer;

{ Packs a floating-point TCastleColor into a 32-bit R8G8B8A8 normalized integer }
function PackColorRgba8(const C: TCastleColor): Cardinal; inline;

implementation

uses
  CastleRenderContext;

var
  FActiveBatch2DRenderer: TCastleBatch2DRenderer = nil;
  GAutoBatch2DRenderer: TCastleBatch2DRenderer = nil;

function ActiveBatch2DRenderer: TCastleBatch2DRenderer;
begin
  Result := FActiveBatch2DRenderer;
end;

procedure SetActiveBatch2DRenderer(const ARenderer: TCastleBatch2DRenderer);
begin
  FActiveBatch2DRenderer := ARenderer;
end;

function EnsureBatch2DRenderer(const ADevice: TRHIDevice): TCastleBatch2DRenderer;
begin
  if ADevice = nil then
    Exit(nil);
  if (FActiveBatch2DRenderer <> nil) and (FActiveBatch2DRenderer.Device = ADevice) and
     (FActiveBatch2DRenderer.WhitePixelTexture <> nil) and
     (FActiveBatch2DRenderer.WhitePixelTexture.Device = ADevice) then
    Exit(FActiveBatch2DRenderer);
  if (GAutoBatch2DRenderer = nil) or
     (GAutoBatch2DRenderer.Device <> ADevice) or
     (GAutoBatch2DRenderer.WhitePixelTexture = nil) or
     (GAutoBatch2DRenderer.WhitePixelTexture.Device <> ADevice) then
  begin
    FreeAndNil(GAutoBatch2DRenderer);
    GAutoBatch2DRenderer := TCastleBatch2DRenderer.Create(ADevice);
  end;
  Result := GAutoBatch2DRenderer;
end;

procedure FreeGlobalBatch2DRenderer;
begin
  FreeAndNil(GAutoBatch2DRenderer);
end;

const
  NL = LineEnding;

function PackColorRgba8(const C: TCastleColor): Cardinal; inline;
var
  R, G, B, A: Byte;
begin
  R := Clamped(Round(C.X * 255.0), 0, 255);
  G := Clamped(Round(C.Y * 255.0), 0, 255);
  B := Clamped(Round(C.Z * 255.0), 0, 255);
  A := Clamped(Round(C.W * 255.0), 0, 255);
  Result := Cardinal(R) or (Cardinal(G) shl 8) or (Cardinal(B) shl 16) or (Cardinal(A) shl 24);
end;

{ ---------------------------------------------------------------------------- }
{ TCastleBatch2DRenderer                                                       }
{ ---------------------------------------------------------------------------- }

constructor TCastleBatch2DRenderer.Create(const ADevice: TRHIDevice);
var
  SamplerDesc: TRHISamplerDesc;
  Smooth, RepS, RepT: Boolean;
begin
  inherited Create;
  FDevice := ADevice;

  for Smooth := Low(Boolean) to High(Boolean) do
    for RepS := Low(Boolean) to High(Boolean) do
      for RepT := Low(Boolean) to High(Boolean) do
      begin
        FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
        if Smooth then
        begin
          SamplerDesc.MinFilter := sfLinear;
          SamplerDesc.MagFilter := sfLinear;
          SamplerDesc.MipFilter := sfLinear;
        end else
        begin
          SamplerDesc.MinFilter := sfNearest;
          SamplerDesc.MagFilter := sfNearest;
          SamplerDesc.MipFilter := sfNearest;
        end;
        if RepS then
          SamplerDesc.AddressModeU := amRepeat
        else
          SamplerDesc.AddressModeU := amClampToEdge;
        if RepT then
          SamplerDesc.AddressModeV := amRepeat
        else
          SamplerDesc.AddressModeV := amClampToEdge;
        SamplerDesc.AddressModeW := amClampToEdge;
        SamplerDesc.MaxLod := 1000.0;
        FSamplers[Smooth, RepS, RepT] := FDevice.CreateSampler(SamplerDesc);
      end;
  FLinearSampler := FSamplers[True, False, False];
  FNearestSampler := FSamplers[False, False, False];

  CreateFallbackWhiteTexture;
  CreateShadersAndPipelines;
  CreateStaticIndexBuffer;
  CreateFrameVertexBuffers;
end;

destructor TCastleBatch2DRenderer.Destroy;
var
  I: Integer;
  Smooth, RepS, RepT: Boolean;
begin
  for I := 0 to MaxFramesInFlight - 1 do
    FreeAndNil(FFrameVBOs[I]);
  FreeAndNil(FStaticQuadIBO);
  FreeAndNil(FTextureSetLayout);
  FreeAndNil(FVertShader);
  FreeAndNil(FFragShader);
  FreeAndNil(FWhitePixelTexture);
  FLinearSampler := nil;
  FNearestSampler := nil;
  for Smooth := Low(Boolean) to High(Boolean) do
    for RepS := Low(Boolean) to High(Boolean) do
      for RepT := Low(Boolean) to High(Boolean) do
        FreeAndNil(FSamplers[Smooth, RepS, RepT]);
  if FActiveBatch2DRenderer = Self then
    FActiveBatch2DRenderer := nil;
  if GAutoBatch2DRenderer = Self then
    GAutoBatch2DRenderer := nil;
  inherited;
end;

function TCastleBatch2DRenderer.GetSampler(const ASmoothScaling, ARepeatS, ARepeatT: Boolean): TRHISampler;
begin
  Result := FSamplers[ASmoothScaling, ARepeatS, ARepeatT];
end;

procedure TCastleBatch2DRenderer.CreateFallbackWhiteTexture;
var
  WhiteImg: TRGBAlphaImage;
  TexDesc: TRHITextureDesc;
begin
  WhiteImg := TRGBAlphaImage.Create(1, 1);
  try
    WhiteImg.Clear(Vector4Byte(255, 255, 255, 255));
    TexDesc := TRHITextureDesc.Create2D(1, 1, pfRGBA8_UNorm, [tuSampled, tuTransferDst], 1);
    TexDesc.DebugName := 'Castle2D_WhitePixel';
    FWhitePixelTexture := FDevice.CreateTexture(TexDesc);
    FWhitePixelTexture.UploadImage(WhiteImg);
  finally
    FreeAndNil(WhiteImg);
  end;
end;

procedure TCastleBatch2DRenderer.CreateShadersAndPipelines;
var
  Gen: TCastleShaderGeneratorRHI;
  VertSrc, FragSrc: String;
  BindingDesc: TRHIDescriptorBindingDesc;
  PipeDesc: TRHIGraphicsPipelineDesc;
  BM: TCastle2DBlendMode;
begin
  Gen := TCastleShaderGeneratorRHI.Create(FDevice.Capabilities.Backend);
  try
    { 1. Generate Unified 2D Vertex & Fragment Shaders }
    VertSrc :=
      Gen.GeneratePreamble(ssVertex) +
      'void main() {' + NL +
      '  castle_tex_coord0 = castle_MultiTexCoord0;' + NL +
      '  castle_color_per_vertex = castle_ColorPerVertex;' + NL +
      '  vec2 ndc = castle_Vertex.xy * CastleDraw.u_ModelViewMatrix[0].xy + CastleDraw.u_ModelViewMatrix[0].zw;' + NL +
      '  gl_Position = vec4(ndc, 0.0, 1.0);' + NL +
      '}';
    VertSrc := Gen.FinalizeVertexShaderSource(VertSrc);

    FragSrc :=
      Gen.GeneratePreamble(ssFragment) +
      'CASTLE_SAMPLER(0, 0) uniform sampler2D u_SpriteTexture;' + NL +
      'void main() {' + NL +
      '  if (CastleDraw.u_ModelViewMatrix[1].w > 0.5 &&' + NL +
      '      (dot(CastleDraw.u_ModelViewMatrix[1].xy, castle_tex_coord0) + CastleDraw.u_ModelViewMatrix[1].z < 0.0))' + NL +
      '    discard;' + NL +
      '  vec4 texCol = texture(u_SpriteTexture, castle_tex_coord0);' + NL +
      '  vec4 fragCol = (CastleDraw.u_ModelViewMatrix[2].y > 0.5)' + NL +
      '    ? vec4(castle_color_per_vertex.rgb, castle_color_per_vertex.a * texCol.a)' + NL +
      '    : (texCol * castle_color_per_vertex);' + NL +
      '  if (CastleDraw.u_ModelViewMatrix[2].x > 0.0 && fragCol.a < CastleDraw.u_ModelViewMatrix[2].x)' + NL +
      '    discard;' + NL +
      '  castle_FragColor = fragCol;' + NL +
      '}';

    FVertShader := FDevice.CreateShaderModuleFromGLSL(ssVertex, VertSrc, 'CastleBatch2D.vert');
    FFragShader := FDevice.CreateShaderModuleFromGLSL(ssFragment, FragSrc, 'CastleBatch2D.frag');
  finally
    FreeAndNil(Gen);
  end;

  { 2. Descriptor Set Layout (Set 0, Binding 0 = Combined Image Sampler) }
  BindingDesc.Binding := 0;
  BindingDesc.DescriptorType := dtCombinedImageSampler;
  BindingDesc.Count := 1;
  BindingDesc.StageFlags := [ssFragment];
  FTextureSetLayout := FDevice.CreateDescriptorSetLayout([BindingDesc]);

  { 3. Build PSOs for each 2D Blend Mode }
  PipeDesc := Default(TRHIGraphicsPipelineDesc);
  PipeDesc.VertexShader := FVertShader;
  PipeDesc.FragmentShader := FFragShader;
  PipeDesc.Topology := ptTriangleList;

  SetLength(PipeDesc.VertexInput.Bindings, 1);
  PipeDesc.VertexInput.Bindings[0].Binding := 0;
  PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TCastle2DVertex); // 20 bytes
  PipeDesc.VertexInput.Bindings[0].PerInstance := False;

  SetLength(PipeDesc.VertexInput.Attributes, 3);
  // Location 0: Position (vec2)
  PipeDesc.VertexInput.Attributes[0].Location := Ord(vlPosition);
  PipeDesc.VertexInput.Attributes[0].Binding := 0;
  PipeDesc.VertexInput.Attributes[0].Format := vafVector2;
  PipeDesc.VertexInput.Attributes[0].Offset := 0;
  PipeDesc.VertexInput.Attributes[0].AttributeName := 'castle_Vertex';
  // Location 2: TexCoord0 (vec2)
  PipeDesc.VertexInput.Attributes[1].Location := Ord(vlTexCoord0);
  PipeDesc.VertexInput.Attributes[1].Binding := 0;
  PipeDesc.VertexInput.Attributes[1].Format := vafVector2;
  PipeDesc.VertexInput.Attributes[1].Offset := 8;
  PipeDesc.VertexInput.Attributes[1].AttributeName := 'castle_MultiTexCoord0';
  // Location 4: ColorPerVertex (ubyte4 normalized)
  PipeDesc.VertexInput.Attributes[2].Location := Ord(vlColor);
  PipeDesc.VertexInput.Attributes[2].Binding := 0;
  PipeDesc.VertexInput.Attributes[2].Format := vafUByte4Normalized;
  PipeDesc.VertexInput.Attributes[2].Offset := 16;
  PipeDesc.VertexInput.Attributes[2].AttributeName := 'castle_ColorPerVertex';

  PipeDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
  PipeDesc.Rasterizer.CullMode := cmNone;
  PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;

  SetLength(PipeDesc.DescriptorSetLayouts, 1);
  PipeDesc.DescriptorSetLayouts[0] := FTextureSetLayout;
  PipeDesc.PushConstantSize := SizeOf(TCastle2DPushConstants);
  PipeDesc.PushConstantStages := [ssVertex, ssFragment];

  SetLength(PipeDesc.ColorAttachmentFormats, 1);
  if FDevice.GetSwapchainColorTexture <> nil then
    PipeDesc.ColorAttachmentFormats[0] := FDevice.GetSwapchainColorTexture.Desc.Format
  else
    PipeDesc.ColorAttachmentFormats[0] := pfBGRA8_UNorm;
  if FDevice.GetSwapchainDepthTexture <> nil then
    PipeDesc.DepthAttachmentFormat := FDevice.GetSwapchainDepthTexture.Desc.Format
  else
    PipeDesc.DepthAttachmentFormat := pfD32_Float;
  PipeDesc.SampleCount := 1;

  for BM := Low(TCastle2DBlendMode) to High(TCastle2DBlendMode) do
  begin
    case BM of
      bmNone:
        PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
      bmAlphaBlend:
        PipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
      bmAdditive:
        begin
          PipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
          PipeDesc.BlendState.SrcColorBlendFactor := bfSrcAlpha;
          PipeDesc.BlendState.DstColorBlendFactor := bfOne;
        end;
      bmMultiply:
        begin
          PipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
          PipeDesc.BlendState.SrcColorBlendFactor := bfDstColor;
          PipeDesc.BlendState.DstColorBlendFactor := bfZero;
        end;
    end;
    PipeDesc.DebugName := 'CastleBatch2D_PSO_' + IntToStr(Ord(BM));
    FBasePipeDescs[BM] := PipeDesc;
    FPipelines[BM] := FDevice.GetOrCreatePipeline(PipeDesc);
  end;
end;

procedure TCastleBatch2DRenderer.CreateStaticIndexBuffer;
var
  Indices: array of UInt16;
  I, VBase, IBase: Cardinal;
  BufDesc: TRHIBufferDesc;
begin
  SetLength(Indices, MaxQuadsPerBatch * 6);
  for I := 0 to MaxQuadsPerBatch - 1 do
  begin
    VBase := I * 4;
    IBase := I * 6;
    Indices[IBase + 0] := VBase + 0;
    Indices[IBase + 1] := VBase + 1;
    Indices[IBase + 2] := VBase + 2;
    Indices[IBase + 3] := VBase + 2;
    Indices[IBase + 4] := VBase + 3;
    Indices[IBase + 5] := VBase + 0;
  end;

  BufDesc := Default(TRHIBufferDesc);
  BufDesc.Size := Length(Indices) * SizeOf(UInt16);
  BufDesc.Usage := [buIndex, buTransferDst];
  BufDesc.MemoryUsage := muGpuOnly;
  BufDesc.DebugName := 'CastleBatch2D_StaticIBO';

  FStaticQuadIBO := FDevice.CreateBuffer(BufDesc, @Indices[0]);
end;

procedure TCastleBatch2DRenderer.CreateFrameVertexBuffers;
var
  I: Integer;
  BufDesc: TRHIBufferDesc;
begin
  BufDesc := Default(TRHIBufferDesc);
  BufDesc.Size := MaxQuadsPerFrame * 4 * SizeOf(TCastle2DVertex);
  BufDesc.Usage := [buVertex];
  BufDesc.MemoryUsage := muCpuToGpu; // Persistently mapped host-visible memory in Vulkan

  for I := 0 to MaxFramesInFlight - 1 do
  begin
    BufDesc.DebugName := 'CastleBatch2D_FrameVBO_' + IntToStr(I);
    FFrameVBOs[I] := FDevice.CreateBuffer(BufDesc, nil);
  end;
end;

procedure TCastleBatch2DRenderer.Begin2DPass(const ACmd: TRHICommandList;
  const AViewportWidth, AViewportHeight: Single);
begin
  SetActiveBatch2DRenderer(Self);
  FIn2DPass := True;
  FCurrentCmd := ACmd;
  FViewportWidth := Max(1.0, AViewportWidth);
  FViewportHeight := Max(1.0, AViewportHeight);

  if (FDevice <> nil) and
     ((FLastMappedFrameIndex <> FDevice.FrameIndex) or
      (FLastMappedFramesPresented <> FDevice.TotalFramesStarted) or
      (FLastMappedCmd <> ACmd)) then
  begin
    FFrameWriteQuadOffset := 0;
    FLastMappedFrameIndex := FDevice.FrameIndex;
    FLastMappedFramesPresented := FDevice.TotalFramesStarted;
    FLastMappedCmd := ACmd;
  end;
  if FFrameWriteQuadOffset >= MaxQuadsPerFrame then
    FFrameWriteQuadOffset := 0;

  FFramePassBaseQuad := FFrameWriteQuadOffset;
  FFrameQuadCount := 0;
  FBatchStartQuad := 0;
  FBatchFlushCount := 0;
  FCurrentTexture := nil;
  FCurrentDescriptorSet := nil;
  FCurrentClip := False;
  FCurrentClipLine := TVector3.Zero;
  FCurrentAlphaCutoff := 0.0;
  FCurrentTextureHasOnlyAlpha := False;
  FCurrentCustomBlendFactors := False;
  FCurrentSrcBlend := bfSrcAlpha;
  FCurrentDstBlend := bfOneMinusSrcAlpha;
  FCurrentCustomFragShader := nil;
  FCurrentCustomParams := TVector4.Zero;
  FCurrentScissor := Rectangle(0, 0, Round(FViewportWidth), Round(FViewportHeight));

  { Map the streaming VBO for the current frame-in-flight }
  if (FDevice <> nil) and (FFrameVBOs[FDevice.FrameIndex] <> nil) then
    FMappedVertices := PCastle2DVertex(FFrameVBOs[FDevice.FrameIndex].Map);
end;

procedure TCastleBatch2DRenderer.SetViewportSize(const AViewportWidth, AViewportHeight: Single);
var
  NewW, NewH: Single;
begin
  NewW := Max(1.0, AViewportWidth);
  NewH := Max(1.0, AViewportHeight);
  if FIn2DPass and ((NewW <> FViewportWidth) or (NewH <> FViewportHeight)) then
    Flush;
  FViewportWidth := NewW;
  FViewportHeight := NewH;
  FCurrentScissor := Rectangle(0, 0, Round(FViewportWidth), Round(FViewportHeight));
end;

procedure TCastleBatch2DRenderer.Flush;
var
  QueuedQuads: Cardinal;
  ActiveVBO: TRHIBuffer;
  VertexByteOffset: PtrUInt;
  PushData: TCastle2DPushConstants;
  PipeDesc: TRHIGraphicsPipelineDesc;
  ClipFlag: Single;
begin
  QueuedQuads := FFrameQuadCount - FBatchStartQuad;
  if (QueuedQuads = 0) or (FCurrentCmd = nil) or (FCurrentDescriptorSet = nil) then
    Exit;

  ActiveVBO := FFrameVBOs[FDevice.FrameIndex];

  { 1. Bind PSO & Scissor (apply active rendering attachment formats, custom fragment shader, custom blend factors, and RenderContext stencil / color mask) }
  if FCurrentCmd.IsRendering or
     (FCurrentCustomFragShader <> nil) or
     FCurrentCustomBlendFactors or
     ((RenderContext <> nil) and
      (RenderContext.StencilTest or (RenderContext.ColorChannels <> AllColorChannels))) then
  begin
    PipeDesc := FBasePipeDescs[FCurrentBlendMode];
    if FCurrentCustomFragShader <> nil then
      PipeDesc.FragmentShader := FCurrentCustomFragShader;
    if FCurrentCustomBlendFactors then
    begin
      PipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
      PipeDesc.BlendState.SrcColorBlendFactor := FCurrentSrcBlend;
      PipeDesc.BlendState.DstColorBlendFactor := FCurrentDstBlend;
    end;
    if FCurrentCmd.IsRendering then
    begin
      if FCurrentCmd.ActiveRenderingHasColor then
      begin
        SetLength(PipeDesc.ColorAttachmentFormats, 1);
        PipeDesc.ColorAttachmentFormats[0] := FCurrentCmd.ActiveRenderingColorFormat;
      end else
        SetLength(PipeDesc.ColorAttachmentFormats, 0);
      if FCurrentCmd.ActiveRenderingHasDepth then
        PipeDesc.DepthAttachmentFormat := FCurrentCmd.ActiveRenderingDepthFormat
      else
        PipeDesc.DepthAttachmentFormat := pfUndefined;
      PipeDesc.SampleCount := Max(Cardinal(1), FCurrentCmd.ActiveRenderingSampleCount);
    end;
    if (RenderContext <> nil) and
       (RenderContext.StencilTest or (RenderContext.ColorChannels <> AllColorChannels)) then
    begin
      RenderContext.ApplyRHIStencilAndColorMask(PipeDesc.DepthStencil, PipeDesc.BlendState);
      if PipeDesc.DepthStencil.StencilTestEnable and
         not (PipeDesc.DepthAttachmentFormat in [pfD24_UNorm_S8_UInt, pfD32_Float_S8_UInt]) then
        PipeDesc.DepthAttachmentFormat := pfD24_UNorm_S8_UInt;
    end;
    FLastBoundPipelineDesc := PipeDesc;
    FLastPipeline := FDevice.GetOrCreatePipeline(PipeDesc);
  end else
  begin
    FLastBoundPipelineDesc := FBasePipeDescs[FCurrentBlendMode];
    FLastPipeline := FPipelines[FCurrentBlendMode];
  end;

  FLastCustomFragShader := FCurrentCustomFragShader;
  FCurrentCmd.BindPipeline(FLastPipeline);
  FCurrentCmd.SetScissor(FCurrentScissor);

  { 2. Push 2D orthographic scale/bias, ClipLine, AlphaParams (cutoff + TextureHasOnlyAlpha), and CustomParams }
  PushData.ViewportScaleBias := Vector4(
    2.0 / FViewportWidth,
    2.0 / FViewportHeight,
    -1.0,
    -1.0);
  if FCurrentClip then
    ClipFlag := 1.0
  else
    ClipFlag := 0.0;
  PushData.ClipLineAndEnable := Vector4(
    FCurrentClipLine.X,
    FCurrentClipLine.Y,
    FCurrentClipLine.Z,
    ClipFlag);
  if FCurrentTextureHasOnlyAlpha then
    PushData.AlphaParams := Vector4(FCurrentAlphaCutoff, 1.0, 0.0, 0.0)
  else
    PushData.AlphaParams := Vector4(FCurrentAlphaCutoff, 0.0, 0.0, 0.0);
  PushData.CustomParams := FCurrentCustomParams;
  FLastPushConstants := PushData;
  FCurrentCmd.PushConstants([ssVertex, ssFragment], 0, SizeOf(PushData), PushData);

  { 3. Bind Sprite Texture Descriptor Set }
  FCurrentCmd.BindDescriptorSet(0, FCurrentDescriptorSet, []);

  { 4. Bind Streaming VBO at the current batch offset & Static Quad IBO }
  VertexByteOffset := PtrUInt(FFramePassBaseQuad + FBatchStartQuad) * 4 * SizeOf(TCastle2DVertex);
  if (FDevice <> nil) and
     (FDevice.Capabilities.Backend in [rbOpenGL, rbOpenGLES]) and
     (FMappedVertices <> nil) and (ActiveVBO <> nil) then
    ActiveVBO.UploadData(
      (PByte(FMappedVertices) + VertexByteOffset)^,
      PtrUInt(QueuedQuads) * 4 * SizeOf(TCastle2DVertex),
      VertexByteOffset);
  FCurrentCmd.BindVertexBuffers(0, [ActiveVBO], [VertexByteOffset]);
  FCurrentCmd.BindIndexBuffer(FStaticQuadIBO, 0, itUInt16);

  { 5. Issue single indexed draw call for all coalesced quads }
  FCurrentCmd.DrawIndexed(QueuedQuads * 6, 1, 0, 0, 0);
  Inc(FBatchFlushCount);
  Inc(FTotalFlushes);

  FBatchStartQuad := FFrameQuadCount;
  FFrameWriteQuadOffset := FFramePassBaseQuad + FFrameQuadCount;
end;

procedure TCastleBatch2DRenderer.End2DPass;
begin
  Flush;
  FIn2DPass := False;
  if (FDevice <> nil) and (FFrameVBOs[FDevice.FrameIndex] <> nil) then
    FFrameVBOs[FDevice.FrameIndex].Unmap;
  FMappedVertices := nil;
  FCurrentCmd := nil;
  if FActiveBatch2DRenderer = Self then
    SetActiveBatch2DRenderer(nil);
end;

procedure TCastleBatch2DRenderer.DrawQuads(
  const ATexture: TRHITexture;
  const ADescriptorSet: TRHIDescriptorSet;
  const ScreenRects, ImageRects: PFloatRectangleArray;
  const Count: Integer;
  const ColorTint: TCastleColor;
  const BlendMode: TCastle2DBlendMode;
  const SmoothScaling: Boolean;
  const ScissorRect: TRectangle;
  const RotationRad: Single;
  const RotationCenter: TVector2;
  const RotationCenterInScreenCoords: Boolean;
  const AClip: Boolean;
  const AClipLine: TVector3;
  const AAlphaCutoff: Single;
  const ACustomBlendFactors: Boolean;
  const ASrcBlend: TRHIBlendFactor;
  const ADstBlend: TRHIBlendFactor;
  const ATextureHasOnlyAlpha: Boolean;
  const ACustomFragShader: TRHIShaderModule;
  const ACustomParams: TVector4);
var
  I: Integer;
  SR, IR: TFloatRectangle;
  InvTexW, InvTexH, U0, V0, U1, V1: Single;
  X0, Y0, X1, Y1, SinA, CosA, CX, CY: Single;
  PackedTint: Cardinal;
  VPtr: PCastle2DVertex;
  FlipRenderedVulkanV: Boolean;

  procedure WriteRotVertex(const VX, VY, U, V: Single); inline;
  var
    DX, DY: Single;
  begin
    if RotationRad <> 0.0 then
    begin
      DX := VX - CX;
      DY := VY - CY;
      VPtr^.Position.X := CX + DX * CosA - DY * SinA;
      VPtr^.Position.Y := CY + DX * SinA + DY * CosA;
    end else
    begin
      VPtr^.Position.X := VX;
      VPtr^.Position.Y := VY;
    end;
    VPtr^.TexCoord.X := U;
    VPtr^.TexCoord.Y := V;
    VPtr^.ColorRgba8 := PackedTint;
    Inc(VPtr);
  end;

begin
  if (Count <= 0) or (FMappedVertices = nil) or (ATexture = nil) then Exit;

  { Flush current batch if state changes or batch limit is reached }
  if (FCurrentTexture <> ATexture) or
     (FCurrentDescriptorSet <> ADescriptorSet) or
     (FCurrentBlendMode <> BlendMode) or
     (FCurrentSmoothScaling <> SmoothScaling) or
     (not FCurrentScissor.Equals(ScissorRect)) or
     (FCurrentClip <> AClip) or
     (AClip and (not TVector3.PerfectlyEquals(FCurrentClipLine, AClipLine))) or
     (FCurrentAlphaCutoff <> AAlphaCutoff) or
     (FCurrentTextureHasOnlyAlpha <> ATextureHasOnlyAlpha) or
     (FCurrentCustomBlendFactors <> ACustomBlendFactors) or
     (ACustomBlendFactors and ((FCurrentSrcBlend <> ASrcBlend) or (FCurrentDstBlend <> ADstBlend))) or
     (FCurrentCustomFragShader <> ACustomFragShader) or
     ((ACustomFragShader <> nil) and (not TVector4.PerfectlyEquals(FCurrentCustomParams, ACustomParams))) or
     ((FFrameQuadCount - FBatchStartQuad) + Cardinal(Count) > MaxQuadsPerBatch) then
  begin
    Flush;
    FCurrentTexture := ATexture;
    FCurrentDescriptorSet := ADescriptorSet;
    FCurrentBlendMode := BlendMode;
    FCurrentSmoothScaling := SmoothScaling;
    FCurrentScissor := ScissorRect;
    FCurrentClip := AClip;
    FCurrentClipLine := AClipLine;
    FCurrentAlphaCutoff := AAlphaCutoff;
    FCurrentTextureHasOnlyAlpha := ATextureHasOnlyAlpha;
    FCurrentCustomBlendFactors := ACustomBlendFactors;
    FCurrentSrcBlend := ASrcBlend;
    FCurrentDstBlend := ADstBlend;
    FCurrentCustomFragShader := ACustomFragShader;
    FCurrentCustomParams := ACustomParams;
  end;

  if FFramePassBaseQuad + FFrameQuadCount + Cardinal(Count) > MaxQuadsPerFrame then
    Exit; // Guard against buffer overflow

  InvTexW := 1.0 / Max(1, ATexture.Desc.Width);
  InvTexH := 1.0 / Max(1, ATexture.Desc.Height);
  FlipRenderedVulkanV := (FDevice <> nil) and
    (FDevice.Capabilities.Backend = rbVulkan) and
    (ATexture.RenderedLayerMask <> 0);
  PackedTint := PackColorRgba8(ColorTint);

  if RotationRad <> 0.0 then
    SinCos(RotationRad, SinA, CosA);

  VPtr := FMappedVertices;
  Inc(VPtr, (FFramePassBaseQuad + FFrameQuadCount) * 4);

  for I := 0 to Count - 1 do
  begin
    SR := ScreenRects^[I];
    IR := ImageRects^[I];

    X0 := SR.Left;
    Y0 := SR.Bottom;
    X1 := SR.Left + SR.Width;
    Y1 := SR.Bottom + SR.Height;

    U0 := IR.Left * InvTexW;
    U1 := (IR.Left + IR.Width) * InvTexW;
    if FlipRenderedVulkanV then
    begin
      V0 := 1.0 - (IR.Bottom * InvTexH);
      V1 := 1.0 - ((IR.Bottom + IR.Height) * InvTexH);
    end else
    begin
      V0 := IR.Bottom * InvTexH;
      V1 := (IR.Bottom + IR.Height) * InvTexH;
    end;

    if RotationRad <> 0.0 then
    begin
      if RotationCenterInScreenCoords then
      begin
        CX := RotationCenter.X;
        CY := RotationCenter.Y;
      end else
      begin
        CX := SR.Left + RotationCenter.X * SR.Width;
        CY := SR.Bottom + RotationCenter.Y * SR.Height;
      end;
    end;

    WriteRotVertex(X0, Y0, U0, V0); // Bottom-Left
    WriteRotVertex(X1, Y0, U1, V0); // Bottom-Right
    WriteRotVertex(X1, Y1, U1, V1); // Top-Right
    WriteRotVertex(X0, Y1, U0, V1); // Top-Left
  end;

  Inc(FFrameQuadCount, Count);
  Inc(FTotalQuadsDrawn, Count);
end;

procedure TCastleBatch2DRenderer.DrawQuads(
  const ATexture: TRHITexture;
  const ADescriptorSet: TRHIDescriptorSet;
  const ScreenRects, ImageRects: PFloatRectangleArray;
  const Count: Integer;
  const ColorTint: TCastleColor;
  const BlendMode: TCastle2DBlendMode;
  const SmoothScaling: Boolean;
  const ScissorRect: TRectangle;
  const RotationRad: Single;
  const RotationCenter: TVector2;
  const RotationCenterInScreenCoords: Boolean;
  const AClip: Boolean;
  const AClipLine: TVector3;
  const AAlphaCutoff: Single;
  const ACustomBlendFactors: Boolean;
  const ASrcBlend: TRHIBlendFactor;
  const ADstBlend: TRHIBlendFactor);
begin
  DrawQuads(ATexture, ADescriptorSet, ScreenRects, ImageRects, Count,
    ColorTint, BlendMode, SmoothScaling, ScissorRect, RotationRad, RotationCenter,
    RotationCenterInScreenCoords, AClip, AClipLine, AAlphaCutoff,
    ACustomBlendFactors, ASrcBlend, ADstBlend, False, nil, TVector4.Zero);
end;

procedure TCastleBatch2DRenderer.DrawQuads(
  const ATexture: TRHITexture;
  const ADescriptorSet: TRHIDescriptorSet;
  const ScreenRects, ImageRects: PFloatRectangleArray;
  const Count: Integer;
  const ColorTint: TCastleColor;
  const BlendMode: TCastle2DBlendMode;
  const SmoothScaling: Boolean;
  const ScissorRect: TRectangle;
  const RotationRad: Single;
  const RotationCenter: TVector2;
  const RotationCenterInScreenCoords: Boolean);
begin
  DrawQuads(ATexture, ADescriptorSet, ScreenRects, ImageRects, Count,
    ColorTint, BlendMode, SmoothScaling, ScissorRect, RotationRad, RotationCenter,
    RotationCenterInScreenCoords, False, TVector3.Zero, 0.0, False, bfSrcAlpha, bfOneMinusSrcAlpha);
end;

procedure TCastleBatch2DRenderer.DrawQuads(
  const ATexture: TRHITexture;
  const ADescriptorSet: TRHIDescriptorSet;
  const ScreenRects, ImageRects: PFloatRectangleArray;
  const Count: Integer;
  const ColorTint: TCastleColor;
  const BlendMode: TCastle2DBlendMode;
  const SmoothScaling: Boolean;
  const ScissorRect: TRectangle);
begin
  DrawQuads(ATexture, ADescriptorSet, ScreenRects, ImageRects, Count,
    ColorTint, BlendMode, SmoothScaling, ScissorRect, 0.0, Vector2(0, 0), False,
    False, TVector3.Zero, 0.0, False, bfSrcAlpha, bfOneMinusSrcAlpha);
end;

initialization
finalization
  FreeGlobalBatch2DRenderer;
end.
