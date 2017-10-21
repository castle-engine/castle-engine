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

implementation

end.
