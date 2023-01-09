{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering of primitives. }
unit CastleRenderPrimitives;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses
  SysUtils, Math, Generics.Collections,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleImages, CastleUtils, CastleVectors, CastleRectangles,
  CastleColors, CastleProjection, CastleRenderOptions, CastleGLUtils,
  CastleGLShaders;

{$define read_interface}

{$I castlerenderprimitives_render_unlit_mesh.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  CastleFilesUtils, CastleStringUtils, CastleGLVersion,
  CastleLog, CastleApplicationProperties, CastleRenderContext, CastleInternalGLUtils;

{$I castlerenderprimitives_render_unlit_mesh.inc}

end.
