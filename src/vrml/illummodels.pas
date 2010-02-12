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

{ @abstract(Illumination models, BRDF equations.)
  For now, this just contains VRML 97 lighting equations. }
unit IllumModels;

interface

uses VectorMath, VRMLNodes, VRMLTriangle, Math, KambiUtils;

{ Returns VRML 2.0 material emissiveColor for lighting equation.
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
function VRML97Emission(const IntersectNode: TVRMLTriangle;
  LightingCalculationOn: boolean): TVector3Single;

{ Returns VRML 2.0 light contribution to the specified
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
  const Intersection: TVector3Single; const IntersectNode: TVRMLTriangle;
  const CamPosition: TVector3Single): TVector3Single;

{ Bardzo specjalna wersja VRML97LightContribution, stworzona na potrzeby
  VRMLLightMap. Idea jest taka ze mamy punkt (Point) w scenie,
  wiemy ze lezy on na jakiejs plaszczyznie ktorej kierunek (znormalizowany)
  to PointPlaneNormal, mamy zadane Light w tej scenie i chcemy
  policzyc lokalny wplyw swiatla na punkt. Zeby uscislic :
  w przeciwienstwie do VRML97LightContribution NIE MAMY
  - Position w scenie (ani zadnego Direction/Up)
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
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single): TVector3Single;

type
  TVRMLFogType = type Integer;

function VRML97FogType(FogNode: TNodeFog): TVRMLFogType;

{ Apply fog to color of the vertex.

  Given Color is assumed to contain already the sum of
@preformatted(
  material emission (VRML97Emission)
  + for each light:
    materuial * lighting properties (VRML97LightContribution)
)

  This procedure will apply the fog, making the linear interpolation
  between original Color and the fog color, as appropriate.

  @param(FogType Must be calculated by VRML97FogType.
    The idea is that VRML97FogType has to compare strings to calculate
    fog type from user-specified type. You don't want to do this in each
    VRML97FogTo1st call, that is e.g. called for every ray in ray-tracer.)

  @param(FogNode If the fog is not used, FogNode may be @nil.
    In this case FogType must be -1, which is always satisfied by
    VRML97FogType(nil) resulting in -1.

    Note that FogType may be -1 for other reasons too,
    for example FogNode.FogType was unknown or FogNode.FdVisibilityRange = 0.)

  @param(FogDistanceScaling, taken from Fog node transformation.
    See @link(TVRMLScene.FogDistanceScaling).) }
procedure VRML97FogTo1st(
  var Color: TVector3Single;
  const Position, VertexPos: TVector3Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single; FogType: Integer);

implementation

uses VRMLErrors;

{$I VectorMathInlines.inc}

function VRML97Emission(const IntersectNode: TVRMLTriangle;
  LightingCalculationOn: boolean): TVector3Single;
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
        Result := ZeroVector3Single else
        { Default VRML 2.0 Material.diffuseColor }
        Result := Vector3Single(0.8, 0.8, 0.8);
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
  const Intersection: TVector3Single; const IntersectNode: TVRMLTriangle;
  const CamPosition: TVector3Single): TVector3Single;
{$I illummodels_vrml97lightcontribution.inc}

function VRML97LightContribution_CameraIndependent(const Light: TActiveLight;
  const Point, PointPlaneNormal, MaterialDiffuseColor: TVector3Single)
  :TVector3Single;
{$define CAMERA_INDEP}
{$I illummodels_vrml97lightcontribution.inc}
{$undef CAMERA_INDEP}

function VRML97FogType(FogNode: TNodeFog): TVRMLFogType;
begin
  if (FogNode = nil) or
     (FogNode.FdVisibilityRange.Value = 0.0) then
    Exit(-1);

  Result := ArrayPosStr(FogNode.FdFogType.Value, ['LINEAR', 'EXPONENTIAL']);
  if Result = -1 then
    VRMLWarning(vwSerious, 'Unknown fog type '''+FogNode.FdFogType.Value+'''');
end;

procedure VRML97FogTo1st(var Color: TVector3Single;
  const Position, VertexPos: TVector3Single;
  FogNode: TNodeFog; const FogDistanceScaling: Single; FogType: Integer);
var
  FogVisibilityRangeScaled: Single;

  procedure ApplyDistance(const Distance: Single);
  var
    F: Single;
  begin
    case FogType of
      0: F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      1: F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
    end;
    Color := Vector_Init_Lerp(F, FogNode.FdColor.Value, Color);
  end;

var
  FogVolumetricVisibilityStart: Single;
  Distance, DistanceSqr: Single;
  VertProjected: TVector3Single;
begin
  if FogType <> -1 then
  begin
    FogVisibilityRangeScaled :=
      FogNode.FdVisibilityRange.Value * FogDistanceScaling;

    if FogNode.FdVolumetric.Value then
    begin
      FogVolumetricVisibilityStart :=
        FogNode.FdVolumetricVisibilityStart.Value * FogDistanceScaling;

      { Calculation of Distance for volumetric fog
        below is analogous to TVRMLMeshRenderer.DoBeforeGLVertex
        caculation. }

      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, FogNode.FdVolumetricDirection.Value, VertexPos);
      Distance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, FogNode.FdVolumetricDirection.Value) then
        Distance := -Distance;
      { Now I want
        - Distance = FogVolumetricVisibilityStart -> 0
        - Distance = FogVolumetricVisibilityStart + X -> X
          (so that Distance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      Distance -= FogVolumetricVisibilityStart;

      { When Distance < 0 our intention is to have no fog.
        So Distance < 0 should be equivalent to Distance = 0. }
      MaxTo1st(Distance, 0);

      if Distance >= FogVisibilityRangeScaled - SingleEqualityEpsilon then
        Color := FogNode.FdColor.Value else
        ApplyDistance(Distance);
    end else
    begin
      DistanceSqr := PointsDistanceSqr(Position, VertexPos);

      if DistanceSqr >= Sqr(FogVisibilityRangeScaled - SingleEqualityEpsilon) then
        Color := FogNode.FdColor.Value else
        ApplyDistance(Sqrt(DistanceSqr));

    end;
  end;
end;

end.
