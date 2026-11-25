{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Example demonstrating high-throughput 2D sprite batching with
  TCastleBatch2DRenderer, Push Constants, streaming vertex buffers, and
  multi-batch draw call coalescing on the Vulkan 1.3+ RHI backend. }
program vulkan_sprite_batch;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

uses
  SysUtils, Classes, Math,
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleLog,
  CastleApplicationProperties, CastleParameters, CastleWindow,
  CastleUIControls, CastleControls, CastleGLUtils,
  CastleInternalRHI, CastleInternalBatch2DRHI,
  CastleInternalVulkan, CastleInternalRHI_Vulkan;

const
  SpriteCount = 20000;

type
  { Interactive 2D sprite swarm UI control }
  TVulkanSpriteSwarmControl = class(TCastleUserInterface)
  private
    FTime: Single;
    FStatusLabel: TCastleLabel;
  public
    property StatusLabel: TCastleLabel read FStatusLabel write FStatusLabel;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Render; override;
  end;

var
  ScreenRects: array[0..SpriteCount - 1] of TFloatRectangle;
  ImageRects: array[0..SpriteCount - 1] of TFloatRectangle;

procedure TVulkanSpriteSwarmControl.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
begin
  inherited;
  FTime := FTime + SecondsPassed;
  if FStatusLabel <> nil then
    FStatusLabel.Caption := Format(
      'Vulkan 1.3+ RHI 2D Sprite Batcher  |  Sprites: %d  |  MaxQuads/Batch: %d  |  FPS: %s',
      [SpriteCount, MaxQuadsPerBatch, Container.Fps.ToString]);
end;

procedure TVulkanSpriteSwarmControl.Render;
var
  Col, Row: Integer;
  WaveX, WaveY: Single;
  R: TFloatRectangle;
begin
  inherited;
  DrawRectangle(RenderRect, Vector4(0.05, 0.06, 0.09, 1.0));
  for Row := 0 to 19 do
    for Col := 0 to 29 do
    begin
      WaveX := Sin(FTime * 2.5 + Row * 0.35) * 8.0;
      WaveY := Cos(FTime * 2.0 + Col * 0.35) * 8.0;
      R := FloatRectangle(
        RenderRect.Left + 40.0 + Col * 28.0 + WaveX,
        RenderRect.Bottom + 30.0 + Row * 26.0 + WaveY,
        18.0, 18.0);
      DrawRectangle(R, Vector4(
        0.3 + 0.7 * ((Col mod 5) / 4.0),
        0.4 + 0.6 * ((Row mod 5) / 4.0),
        0.9, 0.92));
    end;
end;

procedure RunVulkanSpriteBatchBenchmark;
var
  SavedRHI: TRHIDevice;
  VkDev: TVulkanRHIDevice;
  ColorDesc, DepthDesc, AtlasDesc: TRHITextureDesc;
  ColorTarget, DepthTarget, AtlasTexture: TRHITexture;
  AtlasImage: TRGBAlphaImage;
  Batch2D: TCastleBatch2DRenderer;
  AtlasSet: TRHIDescriptorSet;
  Cmd: TVulkanRHICommandList;
  RenderInfo: TRHIRenderingInfo;
  I: Integer;
  PackedColor: Cardinal;
begin
  { Generate 20,000 sprite quad coordinates (exceeding MaxQuadsPerBatch = 16,384 to verify auto-flush) }
  for I := 0 to SpriteCount - 1 do
  begin
    ScreenRects[I] := FloatRectangle((I mod 200) * 6.0, (I div 200) * 6.0, 5.0, 5.0);
    ImageRects[I] := FloatRectangle(0, 0, 32, 32);
  end;

  PackedColor := PackColorRgba8(Vector4(1.0, 0.8, 0.2, 1.0));
  WritelnLog('VulkanSpriteBatch', 'Generated %d sprite quad coordinates (PackedColor=$%08X)',
    [SpriteCount, Int64(PackedColor)]);

  SavedRHI := RHIDevice;
  VkDev := TVulkanRHIDevice.CreateHeadless(VkPhysicalDevice(PtrUInt($7002)));
  ColorTarget := nil;
  DepthTarget := nil;
  AtlasTexture := nil;
  AtlasImage := nil;
  Batch2D := nil;
  AtlasSet := nil;
  Cmd := nil;
  try
    SetRHIDevice(VkDev);

    ColorDesc := TRHITextureDesc.Create2D(1280, 720, pfBGRA8_UNorm, [tuColorAttachment, tuTransferSrc], 1);
    ColorDesc.DebugName := 'SpriteBatchColorTarget';
    ColorTarget := VkDev.CreateTexture(ColorDesc);

    DepthDesc := TRHITextureDesc.Create2D(1280, 720, pfD32_Float, [tuDepthStencil], 1);
    DepthDesc.DebugName := 'SpriteBatchDepthTarget';
    DepthTarget := VkDev.CreateTexture(DepthDesc);

    { Create 32x32 Sprite Atlas Texture }
    AtlasImage := TRGBAlphaImage.Create(32, 32);
    AtlasImage.Clear(Vector4Byte(255, 220, 120, 255));
    AtlasDesc := TRHITextureDesc.Create2D(32, 32, pfRGBA8_UNorm, [tuSampled, tuTransferDst], 1);
    AtlasDesc.DebugName := 'SpriteBatchAtlas';
    AtlasTexture := VkDev.CreateTexture(AtlasDesc);
    AtlasTexture.UploadImage(AtlasImage);

    { Initialize TCastleBatch2DRenderer and Texture Descriptor Set }
    Batch2D := TCastleBatch2DRenderer.Create(VkDev);
    AtlasSet := VkDev.CreateDescriptorSet(Batch2D.TextureSetLayout);
    AtlasSet.BindTexture(0, AtlasTexture, Batch2D.LinearSampler);
    AtlasSet.Update;

    { Record 2D Sprite Pass }
    Cmd := TVulkanRHICommandList.Create(VkDev, VkDev.GraphicsQueueFamily);
    Cmd.BeginRecording;
    RenderInfo := VkDev.BuildSwapchainRenderingInfo(
      Rectangle(0, 0, 1280, 720), ColorTarget, DepthTarget,
      Vector4(0.05, 0.06, 0.09, 1.0), loClear, loClear, 1.0);
    Cmd.BeginRendering(RenderInfo);
    Cmd.SetViewport(FloatRectangle(0, 0, 1280, 720), 0.0, 1.0);

    Batch2D.Begin2DPass(Cmd, 1280, 720);
    { Submit first 10,000 quads and second 10,000 quads -> triggers auto-split at 16,384 }
    Batch2D.DrawQuads(AtlasTexture, AtlasSet,
      PFloatRectangleArray(@ScreenRects[0]), PFloatRectangleArray(@ImageRects[0]),
      10000, Vector4(1.0, 0.8, 0.2, 1.0), bmAlphaBlend, True, Rectangle(0, 0, 1280, 720));
    Batch2D.DrawQuads(AtlasTexture, AtlasSet,
      PFloatRectangleArray(@ScreenRects[10000]), PFloatRectangleArray(@ImageRects[10000]),
      10000, Vector4(0.4, 0.9, 1.0, 1.0), bmAlphaBlend, True, Rectangle(0, 0, 1280, 720));
    Batch2D.End2DPass;

    Cmd.EndRendering;
    Cmd.EndRecording;

    WritelnLog('VulkanSpriteBatch',
      'Coalesced %d sprites into %d indexed draw calls (MaxQuadsPerBatch=%d, MaxQuadsPerFrame=%d).',
      [Batch2D.FrameQuadCount, Batch2D.BatchFlushCount, MaxQuadsPerBatch, MaxQuadsPerFrame]);
  finally
    SetRHIDevice(SavedRHI);
    FreeAndNil(Cmd);
    FreeAndNil(AtlasSet);
    FreeAndNil(Batch2D);
    FreeAndNil(AtlasTexture);
    FreeAndNil(AtlasImage);
    FreeAndNil(DepthTarget);
    FreeAndNil(ColorTarget);
    FreeAndNil(VkDev);
  end;
end;

var
  HeadlessMode: Boolean;
  Win: TCastleWindow;
  SwarmControl: TVulkanSpriteSwarmControl;
  Lbl: TCastleLabel;
begin
  InitializeLog;
  ApplicationProperties.ApplicationName := 'vulkan_sprite_batch';
  ApplicationProperties.Caption := 'Castle Game Engine - Vulkan RHI 2D Sprite Batcher';
  ApplicationProperties.RenderBackend := backendVulkan;

  HeadlessMode := Parameters.IsPresent(['--headless']) or
    ((GetEnvironmentVariable('DISPLAY') = '') and
     (GetEnvironmentVariable('WAYLAND_DISPLAY') = '') and
     {$ifdef MSWINDOWS} False {$else} True {$endif});

  WritelnLog('VulkanSpriteBatch', 'Castle Game Engine - Vulkan 2D Sprite Batching Benchmark');
  RunVulkanSpriteBatchBenchmark;

  if not HeadlessMode then
  begin
    Win := TCastleWindow.Create(Application);
    Application.MainWindow := Win;
    Win.Width := 960;
    Win.Height := 600;
    Win.Caption := 'Castle Game Engine - Vulkan RHI 2D Sprite Batcher';

    SwarmControl := TVulkanSpriteSwarmControl.Create(Win);
    SwarmControl.FullSize := True;
    Win.Controls.InsertFront(SwarmControl);

    Lbl := TCastleLabel.Create(Win);
    Lbl.Anchor(hpLeft, 16);
    Lbl.Anchor(vpTop, -16);
    Lbl.Color := Vector4(1.0, 0.9, 0.4, 1.0);
    Lbl.Caption := 'Vulkan 1.3+ RHI 2D Sprite Batcher';
    SwarmControl.StatusLabel := Lbl;
    Win.Controls.InsertFront(Lbl);

    Win.OpenAndRun;
  end;
end.
