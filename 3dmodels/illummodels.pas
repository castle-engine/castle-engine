{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Ten modul ma zawierac implementacje roznorakich rownan lokalnych modeli
  oswietlenia i BRDFow po to zeby (chociaz czasami, kiedy bedzie to mozliwe)
  oddzielic implementacje tych rzeczy od implementacji roznych ray tracerow
  (i ew. innych rendererow).)  }

unit IllumModels;

interface

uses VectorMath, VRMLNodes, VRMLTriangleOctree, Math, KambiUtils, Matrix;

{ This returns VRML 2.0 material emissiveColor for lighting equation.
  I.e. the @code(O_Ergb) part of lighting equation in
  VRML 2.0 spec section "4.14.4 Lighting equations".

  This takes also into account VRML 1.0, when emissiveColor
  of VRML 1.0 material is taken.

  When LightingCalculationOn we do something special:
  we take material's diffuseColor instead of emissiveColor.
  This is supposed to be used when you use ray-tracer with
  recursion 0 (i.e., actually it's a ray-caster in this case).
  Using emissiveColor in such case would almost always
  give a completely black, useles image. }
function VRML97Emission(const IntersectNode: TOctreeItem;
  LightingCalculationOn: boolean): TVector3_Single;

{ This returns VRML 2.0 light contribution to the specified
  vertex color. In other words, this calculates the following
  equation part from VRML 2.0 spec section
  "4.14.4 Lighting equations" :
@preformatted(
  on_i × attenuation_i × spot_i × I_Lrgb
    × (ambient_i + diffuse_i + specular_i)
)

  In some cases we do something different than VRML 2.0 spec:

  @unorderedList(
    @item(
      For VRML 1.0 SpotLight, we have to calculate spot light differently
      (because VRML 1.0 SpotLight gives me dropOffRate instead of
      beamWidth), so we use spot factor equation following OpenGL equation.)

    @item(
      For VRML 1.0, we have to calculate ambientFactor in a little different way:
      see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_light_attenuation],
      when light's ambientIntensity is < 0 then we just return 0 and
      otherwise we use material's ambientColor.)

    @item(
      O ile dobrze zrozumialem, rownania oswietlenia VMRLa 97 proponuja oswietlac
      powierzchnie tylko z jednej strony, tam gdzie wskazuje podany wektor
      normalny (tak jak w OpenGLu przy TWO_SIDED_LIGHTING OFF).
      (patrz definicja "modified dot product" w specyfikacji oswietlenia VRMLa 97)
      Dla mnie jest to bez sensu i oswietlam powierzchnie z obu stron, tak jakby
      kazda powierzchnia byla DWOMA powierzchniami, kazda z nich o przeciwnym
      wektorze normalnym (tak jak w OpenGLu przy TWO_SIDED_LIGHTING ON).)
  )

  Jeszcze slowo : wszystkie funkcje zwracaja kolor w postaci RGB ale NIE
  clamped do (0, 1) (robienie clamp przez te funkcje byloby czysta strata
  czasu, i tak te funkcje sa zazwyczaj opakowane w wiekszy kod liczacy
  kolory i ten nadrzedny kod musi robic clamp - o ile chce, np. raytracer
  zapisujacy kolory do rgbe nie musi nigdzie robic clamp). }
function VRML97LightContribution(const Light: TActiveLight;
  const Intersection: TVector3_Single; const IntersectNode: TOctreeItem;
  const CamPosition: TVector3_Single): TVector3_Single;

{ Bardzo specjalna wersja VRML97LightContribution, stworzona na potrzeby
  VRMLLightMap. Idea jest taka ze mamy punkt (Point) w scenie,
  wiemy ze lezy on na jakiejs plaszczyznie ktorej kierunek (znormalizowany)
  to PointPlaneNormal, mamy zadane Light w tej scenie i chcemy
  policzyc lokalny wplyw swiatla na punkt. Zeby uscislic :
  w przeciwienstwie do VRML97LightContribution NIE MAMY
  - CameraPos w scenie (ani zadnego CameraDir/Up)
  - materialu z ktorego wykonany jest material.

  Mamy wiec wyjatkowa sytuacje. Mimo to, korzystajac z rownan oswietlenia,
  mozemy policzyc calkiem sensowne light contribution:
  - odczucamy komponent Specular (zostawiamy sobie tylko ambient i diffuse)
  - kolor diffuse materialu przyjmujemy po prostu jako podany MaterialDiffuseColor

  Przy takich uproszczeniach mozemy zrobic odpowiednik VRML97LightContribution
  ktory wymaga mniej danych a generuje wynik niezalezny od polozenia
  kamery (i mozemy go wykonac dla kazdego punktu sceny, nie tylko tych
  ktore leza na jakichs plaszczyznach sceny). To jest wlasnie ta funkcja. }
function VRML97LightContribution_CameraIndependent(const Light: TActiveLight;
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3_Single): TVector3_Single;

{ FogNode jak zwykle moze byc = nil aby powiedziec ze nie uzywamy mgly.
  Jak zwykle mamy tutaj FogDistanceScaling, podobnie jak w
  TVRMLFlatScene.

  FogType: Integer musi byc poprzednio wyliczone przez VRML97FogType.
  (to po to zeby VRMLFog() nie musiala za kazdym razem porownywac stringow,
  co jest raczej wazne gdy np. uzywasz jej jako elementu raytracera).
  Jesli FogType = -1 to wiesz ze mgla nie bedzie uwzgledniana przez VRML97Fog
  (byc moze bo FogNode = nil, byc moze bo FogNode.FogType bylo nieznane ale
  VRMLNonFatalError pozwolilo nam kontynuowac, byc moze
  FogNode.FdVisibilityRange = 0 itp.), tzn. VRML97Fog
  zwroci wtedy po prostu Color. Mozesz wtedy przekazac
  cokolwiek do DistanceFromCamera (tzn. nie musisz liczyc DistanceFromCamera,
  co zazwyczaj jest czasochlonne bo wiaze sie z pierwiastkowaniem)
  no i mozesz naturalnie w ogole nie wywolywac VRML97Fog.

  Podany Color to suma VRML97Emission i VRML97LightContribution dla
  kazdego swiatla oswietlajacego element. Ta funkcja uwzgledni node mgly
  i ew. zrobi Vector_Lerp pomiedzy kolorem mgly a podanym Color. }
function VRML97FogType(FogNode: TNodeFog): Integer;
function VRML97Fog(const Color: TVector3_Single; const DistanceFromCamera: Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single; FogType: Integer):
  TVector3_Single;

implementation

uses VRMLErrors;

{$I VectorMathInlines.inc}

function VRML97Emission(const IntersectNode: TOctreeItem;
  LightingCalculationOn: boolean): TVector3_Single;
var
  M1: TNodeMaterial_1;
  M2: TNodeMaterial_2;
begin
  if IntersectNode.State.ParentShape <> nil then
  begin
    M2 := IntersectNode.State.ParentShape.Material;
    if M2 <> nil then
    begin
      if LightingCalculationOn then
        Result := M2.FdEmissiveColor.Value else
        Result := M2.FdDiffuseColor.Value;
    end else
    begin
      if LightingCalculationOn then
        { Default VRML 2.0 Material.emissiveColor }
        Result.Init_Zero else
        { Default VRML 2.0 Material.diffuseColor }
        Result.Init(0.8, 0.8, 0.8);
    end;
  end else
  begin
    M1 := IntersectNode.State.LastNodes.Material;
    if LightingCalculationOn then
      Result := M1.EmissiveColor3Single(IntersectNode.MatNum) else
      Result := M1.DiffuseColor3Single(IntersectNode.MatNum);
  end;
end;

function VRML97LightContribution(const Light: TActiveLight;
  const Intersection: TVector3_Single; const IntersectNode: TOctreeItem;
  const CamPosition: TVector3_Single): TVector3_Single;
{$I illummodels_vrml97lightcontribution.inc}

function VRML97LightContribution_CameraIndependent(const Light: TActiveLight;
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3_Single)
  :TVector3_Single;
{$define CAMERA_INDEP}
{$I illummodels_vrml97lightcontribution.inc}
{$undef CAMERA_INDEP}

function VRML97FogType(FogNode: TNodeFog): Integer;
begin
 if (FogNode = nil) or (FogNode.FdVisibilityRange.Value = 0.0) then Exit(-1);

 result := ArrayPosStr(FogNode.FdFogType.Value, ['LINEAR', 'EXPONENTIAL']);
 if result = -1 then
  VRMLNonFatalError('Unknown fog type '''+FogNode.FdFogType.Value+'''');
end;

function VRML97Fog(const Color: TVector3_Single; const DistanceFromCamera: Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single; FogType: Integer):
  TVector3_Single;
var
  F: Single;
  FogVisibilityRangeScaled: Single;
begin
 if FogType <> -1 then
 begin
  FogVisibilityRangeScaled :=
    FogNode.FdVisibilityRange.Value * FogDistanceScaling;
  if DistanceFromCamera >= FogVisibilityRangeScaled-SingleEqualityEpsilon then
   result := FogNode.FdColor.Value else
  begin
   case FogType of
    0: f := (FogVisibilityRangeScaled - DistanceFromCamera) / FogVisibilityRangeScaled;
    1: f := Exp(-DistanceFromCamera / (FogVisibilityRangeScaled - DistanceFromCamera));
   end;
   result := Vector_Init_Lerp(f, FogNode.FdColor.Value, Color);
  end;
 end else
  result := Color;
end;

end.
