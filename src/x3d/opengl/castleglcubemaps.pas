{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL utilities for cube (environment) maps. }
unit CastleGLCubeMaps;

{$I castleconf.inc}

interface

uses {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleVectors, CastleCubeMaps, CastleImages, CastleCompositeImage,
  CastleGLImages, CastleTransform, CastleGLUtils;

type
  TCubeMapRenderSimpleFunction = procedure (ForCubeMap: boolean); experimental;

{ Calculate spherical harmonics basis describing environment rendered
  by OpenGL. Environment is rendered by
  Render(true) callback, from the
  CapturePoint. It's rendered to color buffer, and captured as grayscale.
  Captured pixel value is just assumed to be the value of spherical function
  at this direction. It's also scaled by ScaleColor (since rendering
  to OpenGL catches values in 0..1 range, but SH vector can express
  values from any range).

  This changes RenderContext.Viewport,
  so be sure to reset RenderContext.Viewport to something normal after calling this.

  The maps will be drawn in the color buffer (from positions MapScreenX, Y),
  so will actually be visible (call this before RenderContext.Clear or such if you
  want to hide them). }
procedure SHVectorGLCapture(
  var SHVector: array of Single;
  const CapturePoint: TVector3;
  const Render: TCubeMapRenderSimpleFunction;
  const MapScreenX, MapScreenY: Integer;
  const ScaleColor: Single); experimental;

{ Capture cube map by rendering environment from CapturePoint.

  Environment is rendered by the Render callback.
  It will be called with custom TRenderingCamera instance,
  with TRenderingCamera.Target set to rtCubeMapEnvironment,
  and matrix set to appropriate view from the CapturePoint.

  Cube map is recorded in six images you provide in the Images parameter.
  These must be already created TCastleImage instances, with the exact same size.
  (They do not have to be square, or have power-of-two
  size, or honor GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB limit,
  as we do not initialize actual OpenGL cube map here.
  You can use generated images for any purpose.)
  The classes of these images will also matter --- e.g. use TGrayscaleImage
  to capture scene as grayscale, use TRGBImage for RGB colors.

  This changes RenderContext.Viewport,
  so be sure to reset RenderContext.Viewport to something normal after calling this.

  ProjectionNear, ProjectionFar parameters will be used to set GL
  projection matrix. ProjectionFar may be equal to ZFarInfinity,
  as always. }
procedure GLCaptureCubeMapImages(
  const Images: TCubeMapImages;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single);

{ Capture cube map to composite (DDS, KTX...) image by rendering environment from CapturePoint.

  See GLCaptureCubeMapImages for documentation, this works the same,
  but it creates TCompositeImage instance containing all six images. }
function GLCaptureCubeMapComposite(
  const Size: Cardinal;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single): TCompositeImage;

{ Capture cube map to OpenGL cube map texture by rendering environment
  from CapturePoint.

  See GLCaptureCubeMapImages for documentation, this works the same,
  but it captures images to given OpenGL texture name Tex.
  Tex must already be created cube map texture (with OpenGL
  size and internal formats set), with square images of Size.
  This also means that Size must be a valid OpenGL cube map texture size,
  you can check it by GLImages.IsCubeMapTextureSized.

  This captures the cube map images to "zero" texture level.
  If you use mipmaps, it's your problem how to generate other texture levels
  --- in the simplest case, call GenerateMipmap(GL_TEXTURE_CUBE_MAP).

  It uses RenderToTexture to render to the texture, so it will use
  framebuffer if available, and it's fast. }
procedure GLCaptureCubeMapTexture(
  const Tex: TGLuint;
  const Size: Cardinal;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  RenderToTexture: TGLRenderToTexture);

implementation

uses SysUtils, CastleSphericalHarmonics, CastleRectangles;

procedure SHVectorGLCapture(
  var SHVector: array of Single;
  const CapturePoint: TVector3;
  const Render: TCubeMapRenderSimpleFunction;
  const MapScreenX, MapScreenY: Integer;
  const ScaleColor: Single);

  procedure DrawMap(Side: TCubeMapSide);
  var
    Map: TGrayscaleImage;
    I, SHBasis, ScreenX, ScreenY: Integer;
  begin
    ScreenX := CubeMapInfo[Side].ScreenX * CubeMapSize + MapScreenX;
    ScreenY := CubeMapInfo[Side].ScreenY * CubeMapSize + MapScreenY;

    RenderContext.Viewport := Rectangle(ScreenX, ScreenY, CubeMapSize, CubeMapSize);

    {$ifndef OpenGLES}
    // TODO-es
    glPushMatrix;
      glLoadMatrix(LookDirMatrix(CapturePoint, CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up));
    {$endif}
      Render(true);
    {$ifndef OpenGLES}
    glPopMatrix;
    {$endif}

    Map := SaveScreen_noflush(TGrayscaleImage,
      Rectangle(ScreenX, ScreenY, CubeMapSize, CubeMapSize), cbBack) as TGrayscaleImage;
    try
      { Use the Map to calculate SHVector[SHBasis] (this is the actual
        purpose of drawing the Render). SHVector[SHBasis] is just a dot product
        for all directions (for all pixels, in this case) of
        light intensity values * SH basis values. }

      for I := 0 to Sqr(CubeMapSize) - 1 do
        for SHBasis := 0 to High(SHVector) do
          SHVector[SHBasis] := SHVector[SHBasis] + ((Map.Pixels[I]/255) *
            ScaleColor *
            SHBasisMap[SHBasis, Side, I]);
    finally FreeAndNil(Map) end;
  end;

var
  SHBasis: Integer;
  Side: TCubeMapSide;
  SavedProjectionMatrix: TMatrix4;
begin
  InitializeSHBasisMap;

  { Call all DrawMap. This wil draw maps, get them,
    and calculate SHVector describing them. }

  for SHBasis := 0 to High(SHVector) do
    SHVector[SHBasis] := 0;

  SavedProjectionMatrix := RenderContext.ProjectionMatrix;
  PerspectiveProjection(90, 1, 0.01, 100);

  for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
    DrawMap(Side);

  RenderContext.ProjectionMatrix := SavedProjectionMatrix;

  for SHBasis := 0 to High(SHVector) do
  begin
    { Each SHVector[SHBasis] is now calculated for all sphere points.
      We want this to be integral over a sphere, so normalize now.

      Since SHBasisMap contains result of SH function * solid angle of pixel
      (on cube map, pixels have different solid angles),
      so below we divide by 4*Pi (sphere area, sum of solid angles for every
      pixel). }
    SHVector[SHBasis] /= 4 * Pi;
  end;
end;

procedure SetRenderingCamera(const RenderingCamera: TRenderingCamera;
  const CapturePoint: TVector3;
  const Side: TCubeMapSide);
begin
  RenderingCamera.FromMatrix(
    LookDirMatrix(CapturePoint, CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up),
    FastLookDirMatrix(CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up),
    RenderContext.ProjectionMatrix);
end;

procedure GLCaptureCubeMapImages(
  const Images: TCubeMapImages;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single);
var
  Width, Height: Cardinal;
  RenderToTexture: TGLRenderToTexture;

  procedure DrawMap(Side: TCubeMapSide);
  var
    RenderingCamera: TRenderingCamera;
  begin
    RenderToTexture.RenderBegin;

      RenderContext.Viewport := Rectangle(0, 0, Width, Height);

      RenderingCamera := TRenderingCamera.Create;
      try
        RenderingCamera.Target := rtCubeMapEnvironment;
        SetRenderingCamera(RenderingCamera, CapturePoint, Side);
        Render(RenderingCamera);
      finally FreeAndNil(RenderingCamera) end;

      SaveScreen_NoFlush(Images[Side], 0, 0, RenderToTexture.ColorBuffer);

    RenderToTexture.RenderEnd(Side < High(Side));
  end;

var
  Side: TCubeMapSide;
  SavedProjectionMatrix: TMatrix4;
begin
  Width  := Images[csPositiveX].Width ;
  Height := Images[csPositiveX].Height;

  RenderToTexture := TGLRenderToTexture.Create(Width, Height);
  try
    RenderToTexture.Buffer := tbNone;
    RenderToTexture.GLContextOpen;

    SavedProjectionMatrix := RenderContext.ProjectionMatrix;
    PerspectiveProjection(90, 1, ProjectionNear, ProjectionFar);

    for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
      DrawMap(Side);

    RenderContext.ProjectionMatrix := SavedProjectionMatrix;
  finally FreeAndNil(RenderToTexture) end;
end;

function GLCaptureCubeMapComposite(
  const Size: Cardinal;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single): TCompositeImage;
var
  Images: TCubeMapImages;
  Side: TCubeMapSide;
begin
  for Side := Low(Side) to High(Side) do
    Images[Side] := TRGBImage.Create(Size, Size);

  GLCaptureCubeMapImages(Images, CapturePoint, Render,
    ProjectionNear, ProjectionFar);

  Result := TCompositeImage.Create;
  Result.Width := Size;
  Result.Height := Size;
  Result.CompositeType := ctCubeMap;
  Result.CubeMapSides := AllCubeMapSides;
  Result.Mipmaps := false;
  Result.MipmapsCount := 1;
  Result.AddCubeMapImages(Images);
end;

procedure GLCaptureCubeMapTexture(
  const Tex: TGLuint;
  const Size: Cardinal;
  const CapturePoint: TVector3;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  RenderToTexture: TGLRenderToTexture);

  procedure DrawMap(Side: TCubeMapSide);
  var
    RenderingCamera: TRenderingCamera;
  begin
    RenderToTexture.RenderBegin;
    RenderToTexture.SetTexture(Tex, GL_TEXTURE_CUBE_MAP_POSITIVE_X + Ord(Side));

      RenderContext.Viewport := Rectangle(0, 0, Size, Size);

      RenderingCamera := TRenderingCamera.Create;
      try
        RenderingCamera.Target := rtCubeMapEnvironment;
        SetRenderingCamera(RenderingCamera, CapturePoint, Side);
        Render(RenderingCamera);
      finally FreeAndNil(RenderingCamera) end;

    RenderToTexture.RenderEnd(Side < High(Side));
  end;

var
  Side: TCubeMapSide;
  SavedProjectionMatrix: TMatrix4;
begin
  RenderToTexture.CompleteTextureTarget := GL_TEXTURE_CUBE_MAP;

  SavedProjectionMatrix := RenderContext.ProjectionMatrix;
  PerspectiveProjection(90, 1, ProjectionNear, ProjectionFar);

  for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
    DrawMap(Side);

  RenderContext.ProjectionMatrix := SavedProjectionMatrix;
end;

end.
