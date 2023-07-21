{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initial simple types for CastleInternalRenderer unit. }

{$ifdef read_interface}
//type
  TBeforeGLVertexProc = procedure (Node: TAbstractGeometryNode;
    const Vert: TVector3) of object;

  TLightRenderEvent = CastleRendererInternalLights.TLightRenderEvent;

  TRenderer = class;
{$endif read_interface}

{$ifdef read_implementation}
{$endif}
