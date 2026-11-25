{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various utilities for OpenGL(ES) rendering. }
unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses
  SysUtils, Math, Generics.Collections, Classes,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  {$ifdef CASTLE_WEBGL} CastleInternalJobWeb, {$endif}
  CastleImages, CastleUtils, CastleVectors, CastleRectangles,
  CastleColors, CastleProjection, CastleRenderOptions;

{$define read_interface}

{$I castleglutils_types.inc}
{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_vertex_array_object.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders,
  CastleLog, CastleApplicationProperties, CastleRenderContext, CastleGLImages,
  CastleInternalGLUtils, CastleTimeUtils, CastleInternalRHI
  {$ifdef WASI} , Job.Js {$endif};

{$I castleglutils_types.inc}
{$I castleglutils_features_debug.inc} // only implementation
{$I castleglutils_features.inc}
{$I castleglutils_draw_primitive_2d.inc}
{$I castleglutils_information.inc}
{$I castleglutils_vertex_array_object.inc}

{ initialization, finalization ----------------------------------------------- }

procedure ContextClose;
begin
  FreeAndNil(Primitive2DRes);

  { free things created by GLInformationInitialize }
  FreeAndNil(GLVersion);

  FreeAndNil(GLFeatures);
end;

procedure Primitive2DRHIDeviceChanged(const ADevice: TRHIDevice);
begin
  if (Primitive2DRes <> nil) and (Primitive2DRes.RHIOwnerDevice <> ADevice) then
  begin
    FreeAndNil(Primitive2DRes.RHIVbo);
    Primitive2DRes.RHIVboWriteOffset := 0;
    Primitive2DRes.RHILastVertexByteOffset := 0;
    Primitive2DRes.RHILastFrameIndex := 0;
    Primitive2DRes.RHILastFramesPresented := 0;
    Primitive2DRes.RHILastCmdList := nil;
    FreeAndNil(Primitive2DRes.RHIVertShader);
    FreeAndNil(Primitive2DRes.RHIFragShader);
    Primitive2DRes.RHIPipeline := nil;
    Primitive2DRes.RHIOwnerDevice := ADevice;
    if (ADevice = nil) and ((ApplicationProperties = nil) or (not ApplicationProperties.IsGLContextOpen)) then
      FreeAndNil(Primitive2DRes);
  end;
end;

initialization
  { Our GLVersion, GLFeatures should be freed at the very end,
    as a lot of code uses them. So place ContextClose to be called last,
    OnGLContextClose[0].
    Every other unit initializion does OnGLContextClose.Add,
    so our initialization will stay as OnGLContextClose[0]. }
  ApplicationProperties.OnGLContextClose.Insert(0, @ContextClose);
  RegisterRHIDeviceChangeCallback({$ifdef FPC}@{$endif}Primitive2DRHIDeviceChanged);
finalization
  FreeAndNil(Primitive2DRes);
end.
