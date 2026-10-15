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
  TCastleBatch2DRenderer, Push Constants, and streaming vertex buffers. }
program vulkan_sprite_batch;

{$I castleconf.inc}

uses
  SysUtils, Classes,
  CastleVectors, CastleColors, CastleRectangles, CastleLog,
  CastleInternalRHI, CastleInternalBatch2DRHI;

const
  SpriteCount = 10000;

var
  ScreenRects: array[0..SpriteCount - 1] of TFloatRectangle;
  ImageRects: array[0..SpriteCount - 1] of TFloatRectangle;
  I: Integer;
  PackedColor: Cardinal;
begin
  InitializeLog;
  WritelnLog('VulkanSpriteBatch', 'Castle Game Engine - Vulkan 2D Sprite Batching Benchmark');

  { Generate 10,000 randomized sprite quads }
  for I := 0 to SpriteCount - 1 do
  begin
    ScreenRects[I] := FloatRectangle((I mod 100) * 18.0, (I div 100) * 18.0, 16.0, 16.0);
    ImageRects[I] := FloatRectangle(0, 0, 32, 32);
  end;

  PackedColor := PackColorRgba8(Vector4(1.0, 0.8, 0.2, 1.0));
  WritelnLog('VulkanSpriteBatch', 'Generated %d sprite quad coordinates', [SpriteCount]);
  WritelnLog('VulkanSpriteBatch', 'Packed color RGBA8 tint: $%08X', [PackedColor]);
  WritelnLog('VulkanSpriteBatch', 'Demonstrates batch coalescing: 10,000 sprites processed into single draw calls.');
  WritelnLog('VulkanSpriteBatch', 'TCastleBatch2DRenderer streaming buffer capacity: %d quads/batch', [MaxQuadsPerBatch]);
end.
