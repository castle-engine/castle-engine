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

  { Push Constant payload for 2D rendering (16 bytes) }
  TCastle2DPushConstants = packed record
    { (2.0 / ViewportWidth, 2.0 / ViewportHeight, -1.0, -1.0) }
    ViewportScaleBias: TVector4;
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
    FPipelines: array[TCastle2DBlendMode] of TRHIPipeline;
    FLinearSampler: TRHISampler;
    FNearestSampler: TRHISampler;
    FWhitePixelTexture: TRHITexture;

    { Static Index Buffer (0,1,2, 2,3,0 ...) & Per-Frame Streaming Vertex Buffers }
    FStaticQuadIBO: TRHIBuffer;
    FFrameVBOs: array[0 .. MaxFramesInFlight - 1] of TRHIBuffer;
    FMappedVertices: PCastle2DVertex;
    FFrameQuadCount: Cardinal;
    FBatchStartQuad: Cardinal;

    { Active batch state }
    FCurrentCmd: TRHICommandList;
    FViewportWidth: Single;
    FViewportHeight: Single;
    FCurrentTexture: TRHITexture;
    FCurrentDescriptorSet: TRHIDescriptorSet;
    FCurrentBlendMode: TCastle2DBlendMode;
    FCurrentSmoothScaling: Boolean;
    FCurrentScissor: TRectangle;

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
      const RotationCenter: TVector2); overload;

    procedure DrawQuads(
      const ATexture: TRHITexture;
      const ADescriptorSet: TRHIDescriptorSet;
      const ScreenRects, ImageRects: PFloatRectangleArray;
      const Count: Integer;
      const ColorTint: TCastleColor;
      const BlendMode: TCastle2DBlendMode;
      const SmoothScaling: Boolean;
      const ScissorRect: TRectangle); overload;

    property LinearSampler: TRHISampler read FLinearSampler;
    property NearestSampler: TRHISampler read FNearestSampler;
    property TextureSetLayout: TRHIDescriptorSetLayout read FTextureSetLayout;
    property WhitePixelTexture: TRHITexture read FWhitePixelTexture;
  end;

{ Packs a floating-point TCastleColor into a 32-bit R8G8B8A8 normalized integer }
function PackColorRgba8(const C: TCastleColor): Cardinal; inline;

implementation

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
begin
  inherited Create;
  FDevice := ADevice;

  FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
  SamplerDesc.MinFilter := sfLinear;
  SamplerDesc.MagFilter := sfLinear;
  SamplerDesc.MipFilter := sfLinear;
  SamplerDesc.AddressModeU := amClampToEdge;
  SamplerDesc.AddressModeV := amClampToEdge;
  SamplerDesc.AddressModeW := amClampToEdge;
  SamplerDesc.MaxLod := 1000.0;
  FLinearSampler := FDevice.CreateSampler(SamplerDesc);

  SamplerDesc.MinFilter := sfNearest;
  SamplerDesc.MagFilter := sfNearest;
  SamplerDesc.MipFilter := sfNearest;
  FNearestSampler := FDevice.CreateSampler(SamplerDesc);

  CreateFallbackWhiteTexture;
  CreateShadersAndPipelines;
  CreateStaticIndexBuffer;
  CreateFrameVertexBuffers;
end;

destructor TCastleBatch2DRenderer.Destroy;
var
  I: Integer;
begin
  for I := 0 to MaxFramesInFlight - 1 do
    FreeAndNil(FFrameVBOs[I]);
  FreeAndNil(FStaticQuadIBO);
  FreeAndNil(FTextureSetLayout);
  FreeAndNil(FVertShader);
  FreeAndNil(FFragShader);
  FreeAndNil(FWhitePixelTexture);
  FreeAndNil(FLinearSampler);
  FreeAndNil(FNearestSampler);
  inherited;
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
      '  castle_FragColor = texture(u_SpriteTexture, castle_tex_coord0) * castle_color_per_vertex;' + NL +
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
  FillChar(PipeDesc, SizeOf(PipeDesc), 0);
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
  PipeDesc.PushConstantStages := [ssVertex];

  SetLength(PipeDesc.ColorAttachmentFormats, 1);
  PipeDesc.ColorAttachmentFormats[0] := FDevice.GetSwapchainColorTexture.Desc.Format;
  PipeDesc.DepthAttachmentFormat := FDevice.GetSwapchainDepthTexture.Desc.Format;
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
          PipeDesc.BlendState.DstColorBlendFactor := bfOneMinusSrcAlpha;
        end;
    end;
    PipeDesc.DebugName := 'CastleBatch2D_PSO_' + IntToStr(Ord(BM));
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

  FillChar(BufDesc, SizeOf(BufDesc), 0);
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
  FillChar(BufDesc, SizeOf(BufDesc), 0);
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
  FCurrentCmd := ACmd;
  FViewportWidth := Max(1.0, AViewportWidth);
  FViewportHeight := Max(1.0, AViewportHeight);

  FFrameQuadCount := 0;
  FBatchStartQuad := 0;
  FCurrentTexture := nil;
  FCurrentDescriptorSet := nil;
  FCurrentScissor := Rectangle(0, 0, Round(FViewportWidth), Round(FViewportHeight));

  { Map the streaming VBO for the current frame-in-flight }
  FMappedVertices := PCastle2DVertex(FFrameVBOs[FDevice.FrameIndex].Map);
end;

procedure TCastleBatch2DRenderer.Flush;
var
  QueuedQuads: Cardinal;
  ActiveVBO: TRHIBuffer;
  VertexByteOffset: PtrUInt;
  PushData: TCastle2DPushConstants;
begin
  QueuedQuads := FFrameQuadCount - FBatchStartQuad;
  if (QueuedQuads = 0) or (FCurrentCmd = nil) or (FCurrentDescriptorSet = nil) then
    Exit;

  ActiveVBO := FFrameVBOs[FDevice.FrameIndex];

  { 1. Bind PSO & Scissor }
  FCurrentCmd.BindPipeline(FPipelines[FCurrentBlendMode]);
  FCurrentCmd.SetScissor(FCurrentScissor);

  { 2. Push 2D orthographic scale/bias }
  PushData.ViewportScaleBias := Vector4(
    2.0 / FViewportWidth,
    2.0 / FViewportHeight,
    -1.0,
    -1.0);
  FCurrentCmd.PushConstants([ssVertex], 0, SizeOf(PushData), PushData);

  { 3. Bind Sprite Texture Descriptor Set }
  FCurrentCmd.BindDescriptorSet(0, FCurrentDescriptorSet, []);

  { 4. Bind Streaming VBO at the current batch offset & Static Quad IBO }
  VertexByteOffset := FBatchStartQuad * 4 * SizeOf(TCastle2DVertex);
  FCurrentCmd.BindVertexBuffers(0, [ActiveVBO], [VertexByteOffset]);
  FCurrentCmd.BindIndexBuffer(FStaticQuadIBO, 0, itUInt16);

  { 5. Issue single indexed draw call for all coalesced quads }
  FCurrentCmd.DrawIndexed(QueuedQuads * 6, 1, 0, 0, 0);

  FBatchStartQuad := FFrameQuadCount;
end;

procedure TCastleBatch2DRenderer.End2DPass;
begin
  Flush;
  FFrameVBOs[FDevice.FrameIndex].Unmap;
  FMappedVertices := nil;
  FCurrentCmd := nil;
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
  const RotationCenter: TVector2);
var
  I: Integer;
  SR, IR: TFloatRectangle;
  InvTexW, InvTexH, U0, V0, U1, V1: Single;
  X0, Y0, X1, Y1, SinA, CosA, CX, CY: Single;
  PackedTint: Cardinal;
  VPtr: PCastle2DVertex;

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
  if (Count <= 0) or (FMappedVertices = nil) then Exit;

  { Flush current batch if state changes or batch limit is reached }
  if (FCurrentTexture <> ATexture) or
     (FCurrentBlendMode <> BlendMode) or
     (FCurrentSmoothScaling <> SmoothScaling) or
     (not FCurrentScissor.Equals(ScissorRect)) or
     ((FFrameQuadCount - FBatchStartQuad) + Cardinal(Count) > MaxQuadsPerBatch) then
  begin
    Flush;
    FCurrentTexture := ATexture;
    FCurrentDescriptorSet := ADescriptorSet;
    FCurrentBlendMode := BlendMode;
    FCurrentSmoothScaling := SmoothScaling;
    FCurrentScissor := ScissorRect;
  end;

  if FFrameQuadCount + Cardinal(Count) > MaxQuadsPerFrame then
    Exit; // Guard against buffer overflow

  InvTexW := 1.0 / Max(1, ATexture.Desc.Width);
  InvTexH := 1.0 / Max(1, ATexture.Desc.Height);
  PackedTint := PackColorRgba8(ColorTint);

  if RotationRad <> 0.0 then
    SinCos(RotationRad, SinA, CosA);

  VPtr := FMappedVertices;
  Inc(VPtr, FFrameQuadCount * 4);

  for I := 0 to Count - 1 do
  begin
    SR := ScreenRects^[I];
    IR := ImageRects^[I];

    X0 := SR.Left;
    Y0 := SR.Bottom;
    X1 := SR.Left + SR.Width;
    Y1 := SR.Bottom + SR.Height;

    U0 := IR.Left * InvTexW;
    V0 := IR.Bottom * InvTexH;
    U1 := (IR.Left + IR.Width) * InvTexW;
    V1 := (IR.Bottom + IR.Height) * InvTexH;

    if RotationRad <> 0.0 then
    begin
      CX := SR.Left + RotationCenter.X;
      CY := SR.Bottom + RotationCenter.Y;
    end;

    WriteRotVertex(X0, Y0, U0, V0); // Bottom-Left
    WriteRotVertex(X1, Y0, U1, V0); // Bottom-Right
    WriteRotVertex(X1, Y1, U1, V1); // Top-Right
    WriteRotVertex(X0, Y1, U0, V1); // Top-Left
  end;

  Inc(FFrameQuadCount, Count);
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
    ColorTint, BlendMode, SmoothScaling, ScissorRect, 0.0, Vector2(0, 0));
end;

end.
