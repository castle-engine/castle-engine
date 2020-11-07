{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various low-level utilities for working with OpenGL. }
unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses SysUtils, Math, Matrix, Generics.Collections,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleImages, CastleUtils, CastleVectors, CastleRectangles,
  CastleColors, CastleProjection, CastleRenderOptions;

{$define read_interface}

{$I castleglutils_types.inc}
{$I castleglutils_errors.inc}
{$I castleglutils_helpers.inc}
{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_ext_framebuffer_blit.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders,
  CastleLog, CastleApplicationProperties, CastleRenderContext;

{$I castleglutils_types.inc}
{$I castleglutils_errors.inc}
{$I castleglutils_helpers.inc}
{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_ext_framebuffer_blit.inc}

{ initialization, finalization ----------------------------------------------- }

procedure ContextClose;
begin
  FreeAndNil(Primitive2DRes);

  { free things created by GLInformationInitialize }
  FreeAndNil(GLVersion);

  FreeAndNil(GLFeatures);
end;

initialization
  { Our GLVersion, GLFeatures should be freed at the very end,
    as a lot of code uses them. So place ContextClose to be called last,
    OnGLContextClose[0].
    Every other unit initializion does OnGLContextClose.Add,
    so our initialization will stay as OnGLContextClose[0]. }
  ApplicationProperties.OnGLContextClose.Insert(0, @ContextClose);
end.
