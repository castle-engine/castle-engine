{
  Copyright 2021-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various internal OpenGL(ES) features, to help OpenGL(ES) rendering. }
unit CastleInternalGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses
  SysUtils, Math, Generics.Collections,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleImages, CastleUtils, CastleVectors, CastleRectangles,
  CastleColors, CastleProjection, CastleRenderOptions, CastleGLShaders,
  CastleGLUtils;

{$define read_interface}

{$I castleinternalglutils_errors.inc}
{$I castleinternalglutils_helpers.inc}
{$I castleinternalglutils_mipmaps.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  CastleFilesUtils, CastleStringUtils, CastleGLVersion,
  CastleLog, CastleApplicationProperties, CastleRenderContext;

{$I castleinternalglutils_errors.inc}
{$I castleinternalglutils_helpers.inc}
{$I castleinternalglutils_mipmaps.inc}

end.
