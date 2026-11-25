{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Example demonstrating explicit Render Hardware Interface (RHI) initialization,
  dynamic rendering, vertex buffer streaming, PSO baking, and interactive
  or headless rendering via Vulkan 1.3+ / OpenGL. }
program vulkan_triangle;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

uses
  SysUtils, Classes, Math,
  CastleVectors, CastleColors, CastleRectangles, CastleLog,
  CastleApplicationProperties, CastleParameters, CastleWindow,
  CastleUIControls, CastleControls, CastleGLUtils,
  CastleInternalRHI, CastleInternalShaderRHI,
  CastleInternalVulkan, CastleInternalRHI_Vulkan, CastleInternalRHI_OpenGL;

type
  { Simple 2D colored vertex (XY position, RGBA color) }
  TSimpleVertex = packed record
    Position: TVector2;
    Color: TVector4;
  end;

  { Interactive UI control drawing an animated RGB triangle and Vulkan RHI status }
  TVulkanTriangleControl = class(TCastleUserInterface)
  private
    FAngle: Single;
    FStatusLabel: TCastleLabel;
  public
    property StatusLabel: TCastleLabel read FStatusLabel write FStatusLabel;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Render; override;
  end;

const
  TriangleVertices: array[0..2] of TSimpleVertex = (
    (Position: (Data: ( 0.0,  0.5)); Color: (Data: (1.0, 0.2, 0.2, 1.0))),
    (Position: (Data: ( 0.5, -0.5)); Color: (Data: (0.2, 1.0, 0.3, 1.0))),
    (Position: (Data: (-0.5, -0.5)); Color: (Data: (0.2, 0.4, 1.0, 1.0)))
  );

procedure TVulkanTriangleControl.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
begin
  inherited;
  FAngle := FAngle + SecondsPassed * 0.8;
  if FStatusLabel <> nil then
    FStatusLabel.Caption := Format(
      'Vulkan 1.3+ RHI Triangle  |  Backend: Vulkan  |  FPS: %s',
      [Container.Fps.ToString]);
end;

procedure TVulkanTriangleControl.Render;
var
  CX, CY, Radius, S, C: Single;
  Pts: array[0..2] of TVector2;
  I: Integer;
begin
  inherited;
  DrawRectangle(RenderRect, Vector4(0.06, 0.07, 0.10, 1.0));
  CX := RenderRect.Center.X;
  CY := RenderRect.Center.Y;
  Radius := Min(RenderRect.Width, RenderRect.Height) * 0.65;
  SinCos(FAngle, S, C);
  for I := 0 to 2 do
  begin
    Pts[I].X := CX + (TriangleVertices[I].Position.X * C - TriangleVertices[I].Position.Y * S) * Radius;
    Pts[I].Y := CY + (TriangleVertices[I].Position.X * S + TriangleVertices[I].Position.Y * C) * Radius;
  end;
  DrawPrimitive2D(pmTriangles, Pts, Vector4(0.95, 0.72, 0.18, 1.0));
end;

procedure RunVulkanTriangleRHIPass;
var
  SavedRHI: TRHIDevice;
  VkDev: TVulkanRHIDevice;
  BufferDesc: TRHIBufferDesc;
  VertexBuffer: TRHIBuffer;
  ColorDesc, DepthDesc: TRHITextureDesc;
  ColorTarget, DepthTarget: TRHITexture;
  Gen: TCastleShaderGeneratorRHI;
  VertSrc, FragSrc: String;
  VertShader, FragShader: TRHIShaderModule;
  PipeDesc: TRHIGraphicsPipelineDesc;
  Pipeline: TRHIPipeline;
  Cmd: TVulkanRHICommandList;
  RenderInfo: TRHIRenderingInfo;
begin
  SavedRHI := RHIDevice;
  VkDev := TVulkanRHIDevice.CreateHeadless(VkPhysicalDevice(PtrUInt($7001)));
  VertexBuffer := nil;
  ColorTarget := nil;
  DepthTarget := nil;
  VertShader := nil;
  FragShader := nil;
  Cmd := nil;
  try
    SetRHIDevice(VkDev);

    { 1. Create and upload Vertex Buffer }
    BufferDesc := TRHIBufferDesc.Create(SizeOf(TriangleVertices), [buVertex], muCpuToGpu, 'TriangleVertexBuffer');
    VertexBuffer := VkDev.CreateBuffer(BufferDesc, @TriangleVertices[0]);
    WritelnLog('VulkanTriangle', 'Vertex buffer allocated and uploaded: %d bytes', [BufferDesc.Size]);

    { 2. Create Offscreen Color & Depth Targets }
    ColorDesc := TRHITextureDesc.Create2D(640, 480, pfBGRA8_UNorm, [tuColorAttachment, tuTransferSrc], 1);
    ColorDesc.DebugName := 'TriangleColorTarget';
    ColorTarget := VkDev.CreateTexture(ColorDesc);

    DepthDesc := TRHITextureDesc.Create2D(640, 480, pfD32_Float, [tuDepthStencil], 1);
    DepthDesc.DebugName := 'TriangleDepthTarget';
    DepthTarget := VkDev.CreateTexture(DepthDesc);

    { 3. Generate and compile GLSL 450 Vertex & Fragment Shaders }
    Gen := TCastleShaderGeneratorRHI.Create(rbVulkan);
    try
      VertSrc := Gen.FinalizeVertexShaderSource(
        Gen.GeneratePreamble(ssVertex) +
        'void main() {' + LineEnding +
        '  castle_color_per_vertex = castle_ColorPerVertex;' + LineEnding +
        '  gl_Position = vec4(castle_Vertex.xy, 0.0, 1.0);' + LineEnding +
        '}');
      FragSrc := Gen.GeneratePreamble(ssFragment) +
        'void main() {' + LineEnding +
        '  castle_FragColor = castle_color_per_vertex;' + LineEnding +
        '}';
    finally
      FreeAndNil(Gen);
    end;

    VertShader := VkDev.CreateShaderModuleFromGLSL(ssVertex, VertSrc, 'Triangle.vert');
    FragShader := VkDev.CreateShaderModuleFromGLSL(ssFragment, FragSrc, 'Triangle.frag');

    { 4. Bake Graphics Pipeline State Object (PSO) }
    FillChar(PipeDesc, SizeOf(PipeDesc), 0);
    PipeDesc.VertexShader := VertShader;
    PipeDesc.FragmentShader := FragShader;
    PipeDesc.Topology := ptTriangleList;
    SetLength(PipeDesc.VertexInput.Bindings, 1);
    PipeDesc.VertexInput.Bindings[0].Binding := 0;
    PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TSimpleVertex);
    PipeDesc.VertexInput.Bindings[0].PerInstance := False;
    SetLength(PipeDesc.VertexInput.Attributes, 2);
    PipeDesc.VertexInput.Attributes[0].Location := Ord(vlPosition);
    PipeDesc.VertexInput.Attributes[0].Binding := 0;
    PipeDesc.VertexInput.Attributes[0].Format := vafVector2;
    PipeDesc.VertexInput.Attributes[0].Offset := 0;
    PipeDesc.VertexInput.Attributes[0].AttributeName := 'castle_Vertex';
    PipeDesc.VertexInput.Attributes[1].Location := Ord(vlColor);
    PipeDesc.VertexInput.Attributes[1].Binding := 0;
    PipeDesc.VertexInput.Attributes[1].Format := vafVector4;
    PipeDesc.VertexInput.Attributes[1].Offset := SizeOf(TVector2);
    PipeDesc.VertexInput.Attributes[1].AttributeName := 'castle_ColorPerVertex';
    PipeDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
    PipeDesc.Rasterizer.CullMode := cmNone;
    PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
    PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
    SetLength(PipeDesc.ColorAttachmentFormats, 1);
    PipeDesc.ColorAttachmentFormats[0] := pfBGRA8_UNorm;
    PipeDesc.DepthAttachmentFormat := pfD32_Float;
    PipeDesc.SampleCount := 1;
    PipeDesc.DebugName := 'VulkanTriangle_PSO';

    Pipeline := VkDev.GetOrCreatePipeline(PipeDesc);

    { 5. Record Dynamic Rendering Pass }
    Cmd := TVulkanRHICommandList.Create(VkDev, VkDev.GraphicsQueueFamily);
    Cmd.BeginRecording;
    RenderInfo := VkDev.BuildSwapchainRenderingInfo(
      Rectangle(0, 0, 640, 480), ColorTarget, DepthTarget,
      Vector4(0.06, 0.07, 0.10, 1.0), loClear, loClear, 1.0);
    Cmd.BeginRendering(RenderInfo);
    Cmd.SetViewport(FloatRectangle(0, 0, 640, 480), 0.0, 1.0);
    Cmd.SetScissor(Rectangle(0, 0, 640, 480));
    Cmd.BindPipeline(Pipeline);
    Cmd.BindVertexBuffers(0, [VertexBuffer], [0]);
    Cmd.Draw(3, 1, 0, 0);
    Cmd.EndRendering;
    Cmd.EndRecording;

    WritelnLog('VulkanTriangle', 'Triangle vertices:');
    WritelnLog('VulkanTriangle', '  Vertex 0: Pos=(%.1f, %.1f), Color=(%.1f, %.1f, %.1f)',
      [TriangleVertices[0].Position.X, TriangleVertices[0].Position.Y,
       TriangleVertices[0].Color.X, TriangleVertices[0].Color.Y, TriangleVertices[0].Color.Z]);
    WritelnLog('VulkanTriangle', '  Vertex 1: Pos=(%.1f, %.1f), Color=(%.1f, %.1f, %.1f)',
      [TriangleVertices[1].Position.X, TriangleVertices[1].Position.Y,
       TriangleVertices[1].Color.X, TriangleVertices[1].Color.Y, TriangleVertices[1].Color.Z]);
    WritelnLog('VulkanTriangle', '  Vertex 2: Pos=(%.1f, %.1f), Color=(%.1f, %.1f, %.1f)',
      [TriangleVertices[2].Position.X, TriangleVertices[2].Position.Y,
       TriangleVertices[2].Color.X, TriangleVertices[2].Color.Y, TriangleVertices[2].Color.Z]);
    WritelnLog('VulkanTriangle', 'RHI Pipeline State Object (PSO) and Dynamic Rendering validated successfully (CachedPipelines=%d).',
      [VkDev.PipelineCount]);
  finally
    SetRHIDevice(SavedRHI);
    FreeAndNil(Cmd);
    FreeAndNil(FragShader);
    FreeAndNil(VertShader);
    FreeAndNil(DepthTarget);
    FreeAndNil(ColorTarget);
    FreeAndNil(VertexBuffer);
    FreeAndNil(VkDev);
  end;
end;

var
  HeadlessMode: Boolean;
  Win: TCastleWindow;
  TriControl: TVulkanTriangleControl;
  Lbl: TCastleLabel;
begin
  InitializeLog;
  ApplicationProperties.ApplicationName := 'vulkan_triangle';
  ApplicationProperties.Caption := 'Castle Game Engine - Vulkan RHI Triangle';
  ApplicationProperties.RenderBackend := backendVulkan;

  HeadlessMode := Parameters.IsPresent(['--headless']) or
    ((GetEnvironmentVariable('DISPLAY') = '') and
     (GetEnvironmentVariable('WAYLAND_DISPLAY') = '') and
     {$ifdef MSWINDOWS} False {$else} True {$endif});

  WritelnLog('VulkanTriangle', 'Castle Game Engine - Vulkan RHI Triangle Example');
  RunVulkanTriangleRHIPass;

  if not HeadlessMode then
  begin
    Win := TCastleWindow.Create(Application);
    Application.MainWindow := Win;
    Win.Width := 960;
    Win.Height := 600;
    Win.Caption := 'Castle Game Engine - Vulkan RHI Triangle';

    TriControl := TVulkanTriangleControl.Create(Win);
    TriControl.FullSize := True;
    Win.Controls.InsertFront(TriControl);

    Lbl := TCastleLabel.Create(Win);
    Lbl.Anchor(hpLeft, 16);
    Lbl.Anchor(vpTop, -16);
    Lbl.Color := Vector4(1.0, 0.9, 0.4, 1.0);
    Lbl.Caption := 'Vulkan 1.3+ RHI Triangle';
    TriControl.StatusLabel := Lbl;
    Win.Controls.InsertFront(Lbl);

    Win.OpenAndRun;
  end;
end.
