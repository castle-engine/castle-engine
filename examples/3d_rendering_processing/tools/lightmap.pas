{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering lightmaps. }

unit LightMap;

interface

uses CastleVectors, X3DNodes, CastleImages, X3DTriangles, CastleColors, CastleTriangles;

type
  TQuad3Single = packed array[0..3]of TVector3Single;

{ Render one triangle of the image, storing there colors
  that would appear on a given surface in the 3D space.
  Effectively, this generates a texture with lighting, shadows and such
  already calculated inside.

  @param(Image The resulting image. We store colors by TCastleImage.Colors,
    see also TClassicRayTracer.Image comments.)

  @param(LeftDownImagePart If @true, then the lower-left image triangle
    is rendered, otherwise the other one.)

  @param(Lights Lights that potentially shine here.
    We use these lights properties to calculate the generated colors.

    We follow the VRML/X3D lighting equation parameters, just like
    our regular ray-tracer. Except we don't have
    here the material description, and we don't know the camera configuration
    (light map must not be dependent on camera).)

  @param(Octree Describes the scene geometry.)

  @param(TrianglePos The triangle position in 3D space.

    When LeftDownImagePart = @true, then
    TrianglePos[0] corresponds to upper-left image pixel,
    TrianglePos[1] corresponds to lower-left image pixel,
    TrianglePos[2] corresponds to lower-right image pixel.

    When LeftDownImagePart = @false, then
    TrianglePos[0] corresponds to lower-right image pixel,
    TrianglePos[1] corresponds to upper-right image pixel,
    TrianglePos[2] corresponds to upper-left image pixel.)

  @param(RenderDir From which the side of the TrianglePos we capture lighting.
    It's any vector not parallel to TrianglePos plane.)

}
procedure TriangleLightMapVar(const Image: TCastleImage;
  LeftDownImagePart: boolean;
  Lights: TLightInstancesList; Octree: TBaseTrianglesOctree;
  const TrianglePos: TTriangle3Single;
  const RenderDir: TVector3Single);

{ Render the light map on a quad. Everything works exactly like with
  TriangleLightMapVar, except that now we render for the quad,
  filling the whole resulting Image.

  Quad[0] point corresponds to the lower-left image corner,
  Quad[1] corresponds to the lower-right, and so on CCW.

  We show progress of operation using CastleProgress, if ProgresTitle <> ''.

  @seealso TriangleLightMapVar }
procedure QuadLightMapVar(const Image: TCastleImage;
  Lights: TLightInstancesList; Octree: TBaseTrianglesOctree;
  const Quad: TQuad3Single;
  const RenderDir: TVector3Single;
  const ProgresTitle: string);

implementation

uses CastleUtils, CastleProgress;

function PointLightMap(const Point, PointPlaneNormal: TVector3Single;
  Lights: TLightInstancesList; Octree: TBaseTrianglesOctree;
  const RenderDir: TVector3Single): TVector3Single;
var i: Integer;
begin
 result := ZeroVector3Single;
 for i := 0 to Lights.Count-1 do
  if Octree.LightNotBlocked(Lights.List^[i], Point, PointPlaneNormal,
    RenderDir, nil, true) then
   VectorAddVar(result, Lights.List^[i].ContributionCameraIndependent(
     Point, PointPlaneNormal, WhiteRGB));
end;

procedure TriangleLightMapVar(const Image: TCastleImage;
  LeftDownImagePart: boolean;
  Lights: TLightInstancesList; Octree: TBaseTrianglesOctree;
  const TrianglePos: TTriangle3Single;
  const RenderDir: TVector3Single);

var RayNormVector: TVector3Single;

  function Color(const Tri10Pos, Tri12Pos: Single): TVector3Single;
  { Gdyby TrianglePos[1] bylo srodkiem ukladu wspolrzednych o ramionach
      TrianglePos[0]---TrianglePos[1] i
      TrianglePos[0]---TrianglePos[2]
    (takie afiniczny ale nie prostokatny uklad wspolrzednych) to
    Tri10Pos i Tri12Pos jest pozycja punktu ktorego kolor mamy wyliczyc.
    W ten sposob argumenty podawane do tej funkcji juz nie mowia nic
    o Image - one tylko podaja pozycje w swiecie 3d w ktorym jest
    TrianglePos i Lights. }
  var RayOrigin: TVector3Single;
  begin
   RayOrigin := TrianglePos[1];
   VectorAddVar(RayOrigin, VectorScale( VectorSubtract(TrianglePos[0], TrianglePos[1]), Tri10Pos));
   VectorAddVar(RayOrigin, VectorScale( VectorSubtract(TrianglePos[2], TrianglePos[1]), Tri12Pos));
   result := PointLightMap(RayOrigin, RayNormVector, Lights, Octree, RenderDir);
  end;

  function PrzekatnaXFromY(y: Integer): Integer;
  begin
   result := Clamped(Round((Image.Width-1) * (1-y/(Image.Height-1))),
     0, Image.Width-1);
  end;

var
  x, y: Integer;
  C: TCastleColor;
  ColRGB: TCastleColorRGB absolute C;
begin
 { RayNormVector bedzie caly czas taki sam, bedzie to wektor normalny
   TrianglePos w kierunku RenderDir }
 RayNormVector := PlaneDirInDirection(TriangleNormPlane(TrianglePos), RenderDir);

 { dzieki temu ze zawsze TrianglePos[1] jest mapowane na rog obrazka
   ktory jest caly pokryty tekstura (tzn. wycinek tekstury na obrazku
   (= trojkat prostokatny) ma kat prosty wlasnie w TrianglePos[1],
   bez wzgledu na LeftDownImagePart) mamy w obu przypadkach dosc
   proste obliczanie parametrow Tri10Pos, Tri12Pos dla Color()
   na podstawie x, y. }

 if LeftDownImagePart then
 begin
  for y := 0 to Image.Height-1 do
   for x := 0 to PrzekatnaXFromY(y) do
   begin
    C := Image.Colors[X, Y, 0];
    ColRGB := Color(y/(Image.Height-1), x/(Image.Width-1));
    Image.Colors[X, Y, 0] := C;
   end;
 end else
 begin
  for y := 0 to Image.Height-1 do
   for x := PrzekatnaXFromY(y) to Image.Width-1 do
   begin
    C := Image.Colors[X, Y, 0];
    ColRGB := Color(1-y/(Image.Height-1), 1-x/(Image.Width-1));
    Image.Colors[X, Y, 0] := C;
   end;
 end;
end;

function QuadNormPlane(Quad: TQuad3Single): TVector4Single;
{ w naszym module Quad zawsze powinien byc planarny i jego oba trojkaty
  musza byc niezdegenerowane. Wiec mozemy po prostu policzyc normal
  quada jako normal jego dowolnego trojkata. }
var Tri: TTriangle3Single absolute Quad;
begin
 result := TriangleNormPlane(Tri);
end;

procedure QuadLightMapVar(const Image: TCastleImage;
  Lights: TLightInstancesList; Octree: TBaseTrianglesOctree;
  const Quad: TQuad3Single;
  const RenderDir: TVector3Single;
  const ProgresTitle: string);

var RayNormVector: TVector3Single;

  function Color(const Quad01Pos, Quad03Pos: Single): TVector3Single;
  var RayOrigin: TVector3Single;
  begin
   RayOrigin := Quad[0];
   VectorAddVar(RayOrigin, VectorScale( VectorSubtract(Quad[1], Quad[0]), Quad01Pos));
   VectorAddVar(RayOrigin, VectorScale( VectorSubtract(Quad[3], Quad[0]), Quad03Pos));
   result := PointLightMap(RayOrigin, RayNormVector, Lights, Octree, RenderDir);
  end;

  procedure DoPixel(x, y: Integer);
  var
    C: TCastleColor;
    ColRGB: TCastleColorRGB absolute C;
  begin
    C := Image.Colors[X, Y, 0];
    ColRGB := Color(x/(Image.Width-1), y/(Image.Height-1));
    Image.Colors[X, Y, 0] := C;
  end;

var x, y: Integer;
begin
{ prosto:
    TriangleLightMapVar(Image, true , Lights, Octree, Triangle3Single(Quad[3], Quad[0], Quad[1]), RenderDir);
    TriangleLightMapVar(Image, false, Lights, Octree, Triangle3Single(Quad[1], Quad[2], Quad[3]), RenderDir);
  Ale nie robimy tak zeby zrobic tutaj progres. (no i zyskujemy w ten
  sposob odrobinke szybkosci).
}

 { RayNormVector bedzie caly czas taki sam, bedzie to wektor normalny
   TrianglePos w kierunku RenderDir }
 RayNormVector := PlaneDirInDirection(QuadNormPlane(Quad), RenderDir);

 if ProgresTitle <> '' then
 begin
  Progress.Init(Image.Width * Image.Height, ProgresTitle);
  try
   for y := 0 to Image.Height-1 do
    for x := 0 to Image.Width-1 do
    begin
     DoPixel(x, y);
     Progress.Step;
    end;
  finally Progress.Fini end;
 end else
 begin
  for y := 0 to Image.Height-1 do
   for x := 0 to Image.Width-1 do
    DoPixel(x, y);
 end;
end;

end.
