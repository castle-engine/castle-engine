{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Rendering lightmaps from VRML models.) }

unit VRMLLightMap;

interface

uses VectorMath, VRMLNodes, Images, VRMLTriangle;

type
  TQuad3Single = packed array[0..3]of TVector3Single;

{ RenderTriangleMapTo1st zapisuje lewa dolna (o ile LeftDownImagePart = true)
  czesc obrazka (to ktore dokladnie pixle przekatnej narysuje jest
  niewyspecyfikowane, wewnetrznie sa tam jakies liczby zmiennoprzec.
  wiec chyba nigdy tego nie ustale).

  Zapisany trojkat pixli odpowiada texturze jaka powinnismy nalozyc
  na trojkat TrianglePos na scenie Octree ze swiatlami Lights
  (zazwyczaj bedziesz otrzymywal Lights jako [pod]zbior jakichs swiatel
  obecnych w tym samym TVRMLNode z ktorego wygenerowales Octree
  ale nie jest to nigdzie wymagane) : textura ta oryginalnie jest
  cala czarna ale tam gdzie pada na nia swiatlo dodajemy (kolor tego swiatla
  * on swiatla * attenuation swiatla * spot swiatla * intensity swiatla *
  cosinus pomiedzy Normal a Light, patrz specyfikacja lighting w VRMLu 97)
  czyli robimy wszystko co mozemy nie posiadajac ani dokladnego opisu materialu
  ani pozycji i kierunku patrzenia kamery. Powstaje cos jak mapa cieni,
  albo mapa swiatel, jak na to patrzec, zreszta uzywanie takiej techniki
  (nakladanie tekstur symulujacych swiatlo) nazywa sie wlasnie mapa swiatel
  (o ile wiem).

  Mapowanie TrianglePos na obrazek : w przypadku LeftDownImagePart = true
  kolejne (nr 0, 1 i 2) elementy TrianglePos odpowiadaja, odpowiednio,
  lewemu-gornemu, lewemu-dolnemu i prawemu-dolnemu rogu obrazka.
  If not LeftDownImagePart to kolejne elementy TrianglePos
  odpowiadaja prawemu-dolnemu, prawemu-gornemu i lewemu-gornemu rozkowi
  obrazka.

  RenderDir moze byc dowolnym wektorem nierownoleglym do plaszczyzny
  TrianglePos. RenderDir wyznacza z ktorej strony TrianglePos swiatla
  uwzgledniamy (czyli z ktorej strony trojkata "jest zewnetrze" na ktore
  bedzie nalozona tekstura).

  Wyliczone pixele zapisuje przy pomocy TImage.SetColorRGB, a wiec
  uwagi wypowiedziane przy RaytraceTo1st odnosnie straty nie tylko precyzji
  ale i zakresu kolorow przy uzywaniu formatow Image innych niz TRGBEImage
  dotycza takze tej procedury.
}
procedure TriangleLightMapTo1st(const Image: TImage;
  LeftDownImagePart: boolean;
  Lights: TDynActiveLightArray; Octree: TVRMLBaseTrianglesOctree;
  const TrianglePos: TTriangle3Single;
  const RenderDir: TVector3Single);

{ renderuje quad na calym obrazku, patrz opis TriangleLightMapTo1st.

  Mapowanie QuadPos na obrazek : punkt lewy-dolny obrazka (czyli 0, 0) to
  QuadPos[0], prawy dolny to QuadPos[1], prawy gorny to QuadPos[2] i wiadomo.

  Jesli ProgresTitle <> '' to uzywa Progress (with given Title)
  aby zaznaczac postep operacji. }
procedure QuadLightMapTo1st(const Image: TImage;
  Lights: TDynActiveLightArray; Octree: TVRMLBaseTrianglesOctree;
  const Quad: TQuad3Single;
  const RenderDir: TVector3Single;
  const ProgresTitle: string);

implementation

uses KambiUtils, IllumModels, ProgressUnit;

function PointLightMap(const Point, PointPlaneNormal: TVector3Single;
  Lights: TDynActiveLightArray; Octree: TVRMLBaseTrianglesOctree;
  const RenderDir: TVector3Single): TVector3Single;
var i: Integer;
begin
 result := ZeroVector3Single;
 for i := 0 to Lights.Count-1 do
  if Octree.ActiveLightNotBlocked(Lights.Items[i], Point, PointPlaneNormal,
    RenderDir, nil, true) then
   VectorAddTo1st(result, VRML97LightContribution_CameraIndependent(
     Lights.Items[i], Point, PointPlaneNormal, White3Single));
end;

procedure TriangleLightMapTo1st(const Image: TImage;
  LeftDownImagePart: boolean;
  Lights: TDynActiveLightArray; Octree: TVRMLBaseTrianglesOctree;
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
  var Ray0: TVector3Single;
  begin
   Ray0 := TrianglePos[1];
   VectorAddTo1st(Ray0, VectorScale( VectorSubtract(TrianglePos[0], TrianglePos[1]), Tri10Pos));
   VectorAddTo1st(Ray0, VectorScale( VectorSubtract(TrianglePos[2], TrianglePos[1]), Tri12Pos));
   result := PointLightMap(Ray0, RayNormVector, Lights, Octree, RenderDir);
  end;

  function PrzekatnaXFromY(y: Integer): Integer;
  begin
   result := Clamped(Round((Image.Width-1) * (1-y/(Image.Height-1))),
     0, Image.Width-1);
  end;

var x, y: Integer;
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
    Image.SetColorRGB(x, y, Color(y/(Image.Height-1), x/(Image.Width-1)));
 end else
 begin
  for y := 0 to Image.Height-1 do
   for x := PrzekatnaXFromY(y) to Image.Width-1 do
    Image.SetColorRGB(x, y, Color(1-y/(Image.Height-1), 1-x/(Image.Width-1)));
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

procedure QuadLightMapTo1st(const Image: TImage;
  Lights: TDynActiveLightArray; Octree: TVRMLBaseTrianglesOctree;
  const Quad: TQuad3Single;
  const RenderDir: TVector3Single;
  const ProgresTitle: string);

var RayNormVector: TVector3Single;

  function Color(const Quad01Pos, Quad03Pos: Single): TVector3Single;
  var Ray0: TVector3Single;
  begin
   Ray0 := Quad[0];
   VectorAddTo1st(Ray0, VectorScale( VectorSubtract(Quad[1], Quad[0]), Quad01Pos));
   VectorAddTo1st(Ray0, VectorScale( VectorSubtract(Quad[3], Quad[0]), Quad03Pos));
   result := PointLightMap(Ray0, RayNormVector, Lights, Octree, RenderDir);
  end;

  procedure DoPixel(x, y: Integer);
  begin
   Image.SetColorRGB(x, y, Color(x/(Image.Width-1), y/(Image.Height-1)));
  end;

var x, y: Integer;
begin
{ prosto:
    TriangleLightMapTo1st(Image, true , Lights, Octree, Triangle3Single(Quad[3], Quad[0], Quad[1]), RenderDir);
    TriangleLightMapTo1st(Image, false, Lights, Octree, Triangle3Single(Quad[1], Quad[2], Quad[3]), RenderDir);
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