{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base types and concepts related to rendering.
  Independent from OpenGL, X3D and other higher-level types. }
unit CastleRendererBaseTypes;

{$I castleconf.inc}

interface

type
  { Shader types. }
  TShaderType = (stVertex, stGeometry, stFragment);

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

type
  { Should we use backface-culling (ignore some faces during rendering).
    This controls whether OpenGL GL_CULL_FACE flag is enabled or not,
    and whether glCullFace is set to back or front.

    Note that we never use glFrontFace, we assume that it's always CCW
    for normal rendering. This way planar mirrors implementation may use
    glFrontFace (see research_special_rendering_methods/plane_mirror_and_shadow/ ,
    not yet integrated with the engine core).

    @seealso TRenderContext.CullFaces }
  TCullFaces = (
    { No backface-culling. }
    cfNone,
    { Cull the faces oriented clockwise. }
    cfCullCw,
    { Cull the faces oriented counter-clockwise. }
    cfCullCcw
  );

implementation

end.
