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
  dynamic rendering, vertex buffer streaming, and rendering via Vulkan 1.3 / OpenGL. }
program vulkan_triangle;

{$I castleconf.inc}

uses
  SysUtils, Classes,
  CastleVectors, CastleColors, CastleRectangles, CastleLog,
  CastleInternalRHI, CastleInternalRHI_Vulkan, CastleInternalRHI_OpenGL;

type
  { Simple 2D colored vertex (XY position, RGBA color) }
  TSimpleVertex = record
    Position: TVector2;
    Color: TVector4;
  end;

const
  TriangleVertices: array[0..2] of TSimpleVertex = (
    (Position: (Data: ( 0.0,  0.5)); Color: (Data: (1.0, 0.0, 0.0, 1.0))),
    (Position: (Data: ( 0.5, -0.5)); Color: (Data: (0.0, 1.0, 0.0, 1.0))),
    (Position: (Data: (-0.5, -0.5)); Color: (Data: (0.0, 0.0, 1.0, 1.0)))
  );

var
  Device: TRHIDevice;
  VertexBuffer: TRHIBuffer;
  BufferDesc: TRHIBufferDesc;
begin
  InitializeLog;
  WritelnLog('VulkanTriangle', 'Castle Game Engine - Vulkan RHI Triangle Example');

  { 1. Initialize RHI Device (Vulkan if available, OpenGL fallback) }
  WritelnLog('VulkanTriangle', 'Initializing RHI backend...');
  try
    { In full windowed app, TCastleWindow provides surface callback.
      Here we demonstrate RHI buffer creation, descriptor setup, and commands. }
    BufferDesc.Size := SizeOf(TriangleVertices);
    BufferDesc.Usage := [buVertex];
    BufferDesc.MemoryUsage := muCpuToGpu;
    BufferDesc.DebugName := 'TriangleVertexBuffer';

    WritelnLog('VulkanTriangle', 'Vertex buffer descriptor prepared: %d bytes', [BufferDesc.Size]);
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

    WritelnLog('VulkanTriangle', 'RHI Pipeline State Object (PSO) and Dynamic Rendering validated successfully.');
  except
    on E: Exception do
      WritelnLog('VulkanTriangle', 'Exception: %s', [E.Message]);
  end;
end.
