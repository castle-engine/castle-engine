{
  Copyright 2016-2019 Michalis Kamburelis.

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

  { Type of @link(TAbstractColorNode.Mode). }
  TColorMode = (cmReplace, cmModulate);

  TToneMapping = (
    tmNone,
    tmUncharted,
    tmHejlRichard,
    tmACES
  );

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

var
  { Log shadow volume information.
    See https://castle-engine.io/manual_log.php about the log. }
  LogShadowVolumes: Boolean = false;

  GammaCorrection: Boolean = false;

  ToneMapping: TToneMapping = tmNone;

implementation

end.
