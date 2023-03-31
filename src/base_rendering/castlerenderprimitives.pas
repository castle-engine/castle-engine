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

{ Directly rendering simple things (use e.g. inside @link(TCastleUserInterface.Render)). }
unit CastleRenderPrimitives;

{$I castleconf.inc}

interface

uses SysUtils,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleVectors, CastleGLShaders, CastleGLUtils, CastleInternalGLUtils,
  CastleColors, CastleUtils;

{$define read_interface}

{$I castlerenderprimitives_render_unlit_mesh.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses CastleRenderContext, CastleRenderOptions, CastleApplicationProperties;

{$I castlerenderprimitives_render_unlit_mesh.inc}

procedure ContextClose;
begin
  TCastleRenderUnlitMesh.StaticGLContextClose;
end;

initialization
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
end.
