{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ OpenGL utilities for cube (environment) maps. }
unit GLCubeMap;

interface

uses VectorMath, CubeMap, Images, Frustum;

type
  TCubeMapRenderSimpleFunction = procedure (ForCubeMap: boolean);

  TCubeMapRenderFunction = procedure (
    const ForCubeMap: boolean;
    const CameraMatrix, CameraRotationOnlyMatrix: TMatrix4Single;
    const Frustum: TFrustum);

{ Calculate spherical harmonics basis describing environment rendered
  by OpenGL. Environment is rendered by Render(true) callback, from the
  CapturePoint. It's rendered to color buffer, and captured as grayscale.
  Captured pixel value is just assumed to be the value of spherical function
  at this direction. It's also scaled by ScaleColor (since rendering
  to OpenGL catches values in 0..1 range, but SH vector can express
  values from any range).

  This changes glViewport, so be sure to reset glViewport to something
  normal after calling this.

  The maps will be drawn in the color buffer (from positions MapScreenX, Y),
  so will actually be visible (call this before glClear or such if you
  want to hide them). }
procedure SHVectorGLCapture(
  var SHVector: array of Single;
  const CapturePoint: TVector3Single;
  const Render: TCubeMapRenderSimpleFunction;
  const MapScreenX, MapScreenY: Integer;
  const ScaleColor: Single);

type
  TCubeMapImages = array [TCubeMapSide] of TImage;

{ Capture cube map by rendering environment from CapturePoint.

  Environment is rendered by Render(true, CameraMatrix, ...) callback.
  CameraMatrix describes desired camera matrix, with camera position
  from the CapturePoint. You should load CameraMatrix
  to OpenGL modelview matrix before rendering your 3D scene,
  For specialized uses, like skybox rendering,
  also CubeMapCameraRotationOnlyMatrix may be useful, and for
  frustum culling you may want to use CubeMapFrustum.

  Scene is rendered to color buffer, captured as appropriate
  for Images classes (e.g. TGrayscaleImage, or TRGBImage).

  Cube map is recorded in six images provided in Images parameter.
  These must be already created TImage instances with the exact same size.
  (But we do not require here the images to be square, or have power-of-two
  size, or honor GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB limit,
  as we do not initialize actual OpenGL cube map here.
  You can use generated images for any purpose.)

  This changes glViewport, so be sure to reset glViewport to something
  normal after calling this.

  @param(MapsOverlap

    If @false then the six maps will be drawn on non-overlapping
    locations in the color buffer, and will be shifted by MapScreenX, Y.
    This has two benefits: you can actually show the generated maps then
    (useful for debug purposes). And, more important, you can sometimes
    get away then with only one glClear before GLCaptureCubeMapImages
    (avoiding doing glClear in each Render).

    Otherwise (when NiceMapsLayout = @false),
    then MapScreenX, Y will be ignored, and all images will be simply
    drawn in screen lower-left corner starting from (0, 0).
    The benefit is that this has the most chance of fitting into
    your window size.) }

procedure GLCaptureCubeMapImages(
  const Images: TCubeMapImages;
  const CapturePoint: TVector3Single;
  const Render: TCubeMapRenderFunction;
  const MapsOverlap: boolean;
  const MapScreenX, MapScreenY: Integer);

implementation

uses SysUtils, GL, GLU, GLImages, KambiGLUtils, SphericalHarmonics;

procedure SHVectorGLCapture(
  var SHVector: array of Single;
  const CapturePoint: TVector3Single;
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

    { We have to clear the buffer first. Clearing with glClear is fast,
      but it must be clipped with scissor (glViewport does not clip glClear).
      Later: actually, clearing is not needed now, since we call DrawLightMap
      at the beginning, right after clearing the whole screen.

    glScissor(ScreenX, ScreenY, CubeMapSize, CubeMapSize);
    glEnable(GL_SCISSOR_TEST);
      glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    }

    glViewport(ScreenX, ScreenY, CubeMapSize, CubeMapSize);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadIdentity;
      gluPerspective(90, 1, 0.01, 100);
      glMatrixMode(GL_MODELVIEW);

      glPushMatrix;

        glLoadIdentity;
        gluLookDirv(CapturePoint, CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up);
        Render(true);

      glPopMatrix;

      glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

    Map := TGrayscaleImage(SaveScreen_noflush(TGrayscaleImage,
      ScreenX, ScreenY, CubeMapSize, CubeMapSize, GL_BACK));
    try
      { Use the Map to calculate SHVector[SHBasis] (this is the actual
        purpose of drawing the Render). SHVector[SHBasis] is just a dot product
        for all directions (for all pixels, in this case) of
        light intensity values * SH basis values. }

      for I := 0 to Sqr(CubeMapSize) - 1 do
        for SHBasis := 0 to High(SHVector) do
          SHVector[SHBasis] += (Map.GrayscalePixels[I]/255) *
            ScaleColor *
            SHBasisMap[SHBasis, Side, I];
    finally FreeAndNil(Map) end;
  end;

var
  SHBasis: Integer;
  Side: TCubeMapSide;
begin
  InitializeSHBasisMap;

  { Call all DrawMap. This wil draw maps, get them,
    and calculate SHVector describing them. }

  for SHBasis := 0 to High(SHVector) do
    SHVector[SHBasis] := 0;

  for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
    DrawMap(Side);

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

procedure GLCaptureCubeMapImages(
  const Images: TCubeMapImages;
  const CapturePoint: TVector3Single;
  const Render: TCubeMapRenderFunction;
  const MapsOverlap: boolean;
  const MapScreenX, MapScreenY: Integer);
var
  Width, Height: Cardinal;

  procedure DrawMap(Side: TCubeMapSide);
  var
    ScreenX, ScreenY: Integer;
    ProjectionMatrix, CameraMatrix, CameraRotationOnlyMatrix: TMatrix4Single;
    Frustum: TFrustum;
  begin
    if MapsOverlap then
    begin
      ScreenX := 0;
      ScreenY := 0;
    end else
    begin
      ScreenX := CubeMapInfo[Side].ScreenX * Integer(Width ) + MapScreenX;
      ScreenY := CubeMapInfo[Side].ScreenY * Integer(Height) + MapScreenY;
    end;

    glViewport(ScreenX, ScreenY, Width, Height);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadIdentity;
      gluPerspective(90, 1, 0.01, 100);
      glMatrixMode(GL_MODELVIEW);

        CameraMatrix := LookDirMatrix(CapturePoint, CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up);
        CameraRotationOnlyMatrix := LookDirMatrix(ZeroVector3Single, CubeMapInfo[Side].Dir, CubeMapInfo[Side].Up);
        glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
        Frustum.Init(ProjectionMatrix, CameraMatrix);

        Render(true, CameraMatrix, CameraRotationOnlyMatrix, Frustum);

      glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

    SaveScreen_noflush(Images[Side], ScreenX, ScreenY, GL_BACK);
  end;

var
  Side: TCubeMapSide;
begin
  Width  := Images[csPositiveX].Width ;
  Height := Images[csPositiveX].Height;

  for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
    DrawMap(Side);
end;

end.
