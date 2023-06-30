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

{$ifdef read_interface}

  { @exclude Internal.
    Rendering pass for TShapesRendering, non-wireframe vs wireframe. }
  TWireframeRenderingPass = 0..1;

  { @exclude Internal.
    Rendering pass combining
    TInternalRenderingPass,
    TWireframeRenderingPass and
    TUserRenderingPass. }
  TTotalRenderingPass = 0..
    (High(TInternalRenderingPass) + 1) *
    (High(TWireframeRenderingPass) + 1) *
    (High(TUserRenderingPass) + 1)
    - 1;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
