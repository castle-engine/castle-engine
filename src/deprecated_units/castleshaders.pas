{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shaders types. }
unit CastleShaders deprecated 'use CastleRenderOptions';

interface

{$I castleconf.inc}

uses CastleRenderOptions;

type
  { Shader types. }
  TShaderType = CastleRenderOptions.TShaderType;

const
  stVertex = CastleRenderOptions.stVertex;
  stGeometry = CastleRenderOptions.stGeometry;
  stFragment = CastleRenderOptions.stFragment;

  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

  ShaderTypeNameX3D: array [TShaderType] of string =
  ( 'VERTEX', 'GEOMETRY', 'FRAGMENT' )
  deprecated 'do not use this constant, it should be internal in X3D units';

implementation

end.
