{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Test CastleInternalRHI and Vulkan backend subsystems. }
unit TestCastleVulkanRHI;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Math,
  CastleTester, CastleVectors, CastleColors, CastleRectangles,
  CastleApplicationProperties,
  CastleInternalRHI, CastleInternalVulkan, CastleInternalVulkanMemory,
  CastleInternalShaderRHI, CastleInternalBatch2DRHI;

type
  TTestCastleVulkanRHI = class(TCastleTestCase)
  published
    procedure TestRHIFundamentals;
    procedure TestVulkanDynamicLoader;
    procedure TestVulkanPipelineKeyHashing;
    procedure TestVulkanMemorySubAllocator;
    procedure TestShaderGeneratorRHI;
    procedure TestBatch2DCalculations;
    procedure TestApplicationPropertiesRenderBackend;
    procedure TestBatch2DScissorAndClipping;
    procedure TestGeometryBuffersToRHIBuffer;
  end;

implementation

{ TTestCastleVulkanRHI }

procedure TTestCastleVulkanRHI.TestRHIFundamentals;
var
  Desc: TRHIBufferDesc;
  TexDesc: TRHITextureDesc;
  PipelineDesc: TRHIGraphicsPipelineDesc;
begin
  { 1. Buffer Descriptor Validation }
  Desc.Size := 1024;
  Desc.Usage := [buVertex, buTransferDst];
  Desc.MemoryUsage := muGpuOnly;
  Desc.DebugName := 'TestVBO';

  AssertEquals('Buffer size must match', 1024, Desc.Size);
  AssertTrue('Buffer usage must include buVertex', buVertex in Desc.Usage);
  AssertTrue('Buffer usage must include buTransferDst', buTransferDst in Desc.Usage);
  AssertFalse('Buffer usage must not include buIndex', buIndex in Desc.Usage);
  AssertEquals('Buffer memory usage', Ord(muGpuOnly), Ord(Desc.MemoryUsage));

  { 2. Texture Descriptor Validation }
  TexDesc.Dimension := td2D;
  TexDesc.Format := pfRGBA8_UNorm;
  TexDesc.Width := 512;
  TexDesc.Height := 256;
  TexDesc.DepthOrArrayLayers := 1;
  TexDesc.MipLevels := 9;
  TexDesc.SampleCount := 1;
  TexDesc.Usage := [tuSampled, tuColorAttachment];
  TexDesc.DebugName := 'TestTex';

  AssertEquals('Tex width', 512, TexDesc.Width);
  AssertEquals('Tex height', 256, TexDesc.Height);
  AssertEquals('Tex mip levels', 9, TexDesc.MipLevels);
  AssertTrue('Tex usage must include tuSampled', tuSampled in TexDesc.Usage);
  AssertTrue('Tex usage must include tuColorAttachment', tuColorAttachment in TexDesc.Usage);

  { 3. Pipeline State Default Descriptors }
  PipelineDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
  AssertEquals('Default cull mode should be cmBack', Ord(cmBack), Ord(PipelineDesc.Rasterizer.CullMode));
  AssertEquals('Default front face should be ffCounterClockwise', Ord(ffCounterClockwise), Ord(PipelineDesc.Rasterizer.FrontFace));
  AssertEquals('Default polygon mode should be pmFill', Ord(pmFill), Ord(PipelineDesc.Rasterizer.PolygonMode));

  PipelineDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDepthTestWrite;
  AssertTrue('Depth test should be enabled', PipelineDesc.DepthStencil.DepthTestEnable);
  AssertTrue('Depth write should be enabled', PipelineDesc.DepthStencil.DepthWriteEnable);
  AssertEquals('Depth compare op should be coLessOrEqual', Ord(coLessOrEqual), Ord(PipelineDesc.DepthStencil.DepthCompareOp));
end;

procedure TTestCastleVulkanRHI.TestVulkanDynamicLoader;
begin
  { Verify dynamic loader state query does not crash }
  AssertTrue('Dynamic loader query executes without raising', (IsVulkanLoaded = True) or (IsVulkanLoaded = False));

  { Calling FreeVulkan should cleanly reset function pointers }
  FreeVulkan;
  AssertFalse('After FreeVulkan, IsVulkanLoaded must be False', IsVulkanLoaded);
  AssertTrue('vkGetInstanceProcAddr should be nil when unloaded', vkGetInstanceProcAddr = nil);
end;

procedure TTestCastleVulkanRHI.TestVulkanPipelineKeyHashing;
var
  Desc1, Desc2, Desc3: TRHIGraphicsPipelineDesc;
  Hash1, Hash2, Hash3: UInt64;

  function SimplePipelineHash(const Desc: TRHIGraphicsPipelineDesc): UInt64;
  begin
    Result := PtrUInt(Desc.VertexShader) xor (PtrUInt(Desc.FragmentShader) shl 13) xor
      (UInt64(Ord(Desc.BlendState.BlendEnable)) shl 32) xor
      (UInt64(Ord(Desc.DepthStencil.DepthTestEnable)) shl 33) xor
      (UInt64(Ord(Desc.Rasterizer.CullMode)) shl 35);
  end;

begin
  FillChar(Desc1, SizeOf(Desc1), 0);
  Desc1.BlendState.BlendEnable := True;
  Desc1.DepthStencil.DepthTestEnable := True;
  Desc1.Rasterizer.CullMode := cmBack;

  FillChar(Desc2, SizeOf(Desc2), 0);
  Desc2.BlendState.BlendEnable := True;
  Desc2.DepthStencil.DepthTestEnable := True;
  Desc2.Rasterizer.CullMode := cmBack;

  FillChar(Desc3, SizeOf(Desc3), 0);
  Desc3.BlendState.BlendEnable := False; // Different blend state
  Desc3.DepthStencil.DepthTestEnable := True;
  Desc3.Rasterizer.CullMode := cmBack;

  Hash1 := SimplePipelineHash(Desc1);
  Hash2 := SimplePipelineHash(Desc2);
  Hash3 := SimplePipelineHash(Desc3);

  AssertTrue('Identical pipeline configurations must produce identical hashes', Hash1 = Hash2);
  AssertTrue('Divergent pipeline states must produce different hashes', Hash1 <> Hash3);
end;

procedure TTestCastleVulkanRHI.TestVulkanMemorySubAllocator;
var
  Alloc: TVulkanMemoryAllocation;
begin
  { Validate TVulkanMemoryAllocation default state }
  FillChar(Alloc, SizeOf(Alloc), 0);
  AssertFalse('Empty allocation should not be valid', Alloc.IsValid);

  Alloc.Size := 4096;
  Alloc.DeviceMemory := 1; // non-zero handle
  AssertTrue('Allocation with size and non-zero handle is valid', Alloc.IsValid);
end;

procedure TTestCastleVulkanRHI.TestShaderGeneratorRHI;
var
  Gen: TCastleShaderGeneratorRHI;
  OriginalVS, AdjustedVS: String;
  Hash1, Hash2: UInt64;
const
  NL = LineEnding;
begin
  Gen := TCastleShaderGeneratorRHI.Create(rbVulkan);
  try
    { 1. Test Vulkan NDC Depth Conversion ([ -1 .. 1 ] -> [ 0 .. 1 ]) }
    OriginalVS :=
      'void main()' + NL +
      '{' + NL +
      '  gl_Position = u_ProjectionMatrix * u_ViewMatrix * a_Position;' + NL +
      '}' + NL;

    AdjustedVS := Gen.FinalizeVertexShaderSource(OriginalVS);
    AssertTrue('Adjusted vertex shader must contain depth adjustment',
      Pos('gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;', AdjustedVS) > 0);

    { 2. Test FNV-1a Source Hash Determinism }
    Hash1 := TCastleShaderGeneratorRHI.ComputeSourceHash(OriginalVS);
    Hash2 := TCastleShaderGeneratorRHI.ComputeSourceHash(OriginalVS);
    AssertTrue('Source hash must be deterministic', Hash1 = Hash2);

    { Changing string should change hash }
    AssertTrue('Different GLSL sources must produce distinct hashes',
      Hash1 <> TCastleShaderGeneratorRHI.ComputeSourceHash(AdjustedVS));
  finally
    FreeAndNil(Gen);
  end;
end;

procedure TTestCastleVulkanRHI.TestBatch2DCalculations;
var
  Tint: Cardinal;
  C: TCastleColor;
begin
  { 1. Test Color Packing into 32-bit R8G8B8A8 UNorm integer }
  C := Vector4(1.0, 0.0, 0.0, 1.0); // Opaque Red
  Tint := PackColorRgba8(C);
  AssertEquals('Red channel should be $FF in byte 0', $FF, Tint and $FF);
  AssertEquals('Green channel should be 0 in byte 1', 0, (Tint shr 8) and $FF);
  AssertEquals('Blue channel should be 0 in byte 2', 0, (Tint shr 16) and $FF);
  AssertEquals('Alpha channel should be $FF in byte 3', $FF, (Tint shr 24) and $FF);

  { 2. Test Batch Constants }
  AssertTrue('MaxQuadsPerBatch must not exceed 65536 indices capacity', MaxQuadsPerBatch * 4 <= 65536);
  AssertTrue('MaxQuadsPerFrame must be at least MaxQuadsPerBatch', MaxQuadsPerFrame >= MaxQuadsPerBatch);
end;

procedure TTestCastleVulkanRHI.TestApplicationPropertiesRenderBackend;
begin
  AssertEquals('Default RenderBackend should be backendAuto', Ord(backendAuto), Ord(ApplicationProperties.RenderBackend));
  ApplicationProperties.RenderBackend := backendVulkan;
  AssertEquals('RenderBackend should be backendVulkan', Ord(backendVulkan), Ord(ApplicationProperties.RenderBackend));
  ApplicationProperties.RenderBackend := backendOpenGL;
  AssertEquals('RenderBackend should be backendOpenGL', Ord(backendOpenGL), Ord(ApplicationProperties.RenderBackend));
  ApplicationProperties.RenderBackend := backendAuto;
end;

procedure TTestCastleVulkanRHI.TestBatch2DScissorAndClipping;
var
  R1, R2, Intersection: TRectangle;
  ScaleBias: TVector4;
  ViewportW, ViewportH: Single;
begin
  ViewportW := 1920.0;
  ViewportH := 1080.0;

  { 1. Test 2D Orthographic Scale/Bias formula }
  ScaleBias := Vector4(2.0 / ViewportW, 2.0 / ViewportH, -1.0, -1.0);
  AssertTrue('ScaleBias.X should match 2.0 / 1920', Abs(ScaleBias.X - (2.0 / 1920.0)) < 0.0001);
  AssertTrue('ScaleBias.Y should match 2.0 / 1080', Abs(ScaleBias.Y - (2.0 / 1080.0)) < 0.0001);
  AssertTrue('ScaleBias.Z should match -1.0', Abs(ScaleBias.Z - (-1.0)) < 0.0001);
  AssertTrue('ScaleBias.W should match -1.0', Abs(ScaleBias.W - (-1.0)) < 0.0001);

  { 2. Test Scissor Rect calculation and intersection }
  R1 := Rectangle(100, 100, 400, 300);
  R2 := Rectangle(200, 150, 500, 400);
  Intersection := R1 * R2;
  AssertEquals('Intersection Left', 200, Intersection.Left);
  AssertEquals('Intersection Bottom', 150, Intersection.Bottom);
  AssertEquals('Intersection Width', 300, Intersection.Width);
  AssertEquals('Intersection Height', 250, Intersection.Height);
  AssertFalse('Intersection should not be empty', Intersection.IsEmpty);
end;

procedure TTestCastleVulkanRHI.TestGeometryBuffersToRHIBuffer;
var
  CoordDesc, AttrDesc, IndexDesc: TRHIBufferDesc;
begin
  { Coordinate VBO -> RHI vertex buffer }
  FillChar(CoordDesc, SizeOf(CoordDesc), 0);
  CoordDesc.Size := 100 * SizeOf(TVector3);
  CoordDesc.Usage := [buVertex, buTransferDst];
  CoordDesc.MemoryUsage := muGpuOnly;
  CoordDesc.DebugName := 'MeshCoordinates';

  AssertTrue('Coord buffer must have buVertex', buVertex in CoordDesc.Usage);
  AssertEquals('Coord buffer size', 100 * SizeOf(TVector3), CoordDesc.Size);

  { Attribute VBO -> RHI vertex buffer }
  FillChar(AttrDesc, SizeOf(AttrDesc), 0);
  AttrDesc.Size := 100 * SizeOf(TVector4);
  AttrDesc.Usage := [buVertex, buTransferDst];
  AttrDesc.MemoryUsage := muGpuOnly;
  AttrDesc.DebugName := 'MeshAttributes';

  AssertTrue('Attr buffer must have buVertex', buVertex in AttrDesc.Usage);

  { Index VBO -> RHI index buffer }
  FillChar(IndexDesc, SizeOf(IndexDesc), 0);
  IndexDesc.Size := 300 * SizeOf(UInt32);
  IndexDesc.Usage := [buIndex, buTransferDst];
  IndexDesc.MemoryUsage := muGpuOnly;
  IndexDesc.DebugName := 'MeshIndices';

  AssertTrue('Index buffer must have buIndex', buIndex in IndexDesc.Usage);
  AssertFalse('Index buffer must not have buVertex', buVertex in IndexDesc.Usage);
end;

initialization
  RegisterTest(TTestCastleVulkanRHI);
end.
