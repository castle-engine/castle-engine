{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base class for all VRML / X3D light nodes.

    Note that even the old VRML 1.0 light nodes inherit from this.
    Although they interpret some bits differently
    ("ambientIntensity" < 0 has special meaning,
    and "global" is ignored --- VRML 1.0 has own light scoping rules),
    most of the fields behave identically. }
  TNodeX3DLightNode = class(TNodeX3DChildNode)
  private
    FTransform: TMatrix4Single;
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;

    private FFdOn: TSFBool;
    public property FdOn: TSFBool read FFdOn;

    private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdglobal: TSFBool;
    public property Fdglobal: TSFBool read FFdglobal;

    private FFdKambiShadows: TSFBool;
    public property FdKambiShadows: TSFBool read FFdKambiShadows;

    private FFdKambiShadowsMain: TSFBool;
    public property FdKambiShadowsMain: TSFBool read FFdKambiShadowsMain;

    private FFdshowProxyGeometry: TSFBool;
    { showProxyGeometry field is an Avalon extension, see
      [http://instant-reality.com/documentation/nodetype/Light/]. }
    public property FdshowProxyGeometry: TSFBool read FFdshowProxyGeometry;

    private FFdProjectionNear: TSFFloat;
    { projectionNear / projectionFar / up are Kambi extensions, see
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_light_projective] }
    public property FdProjectionNear: TSFFloat read FFdProjectionNear;

    private FFdProjectionFar: TSFFloat;
    public property FdProjectionFar: TSFFloat read FFdProjectionFar;

    private FFdUp: TSFVec3f;
    public property FdUp: TSFVec3f read FFdUp;

    private FFdDefaultShadowMap: TSFNode;
    public property FdDefaultShadowMap: TSFNode read FFdDefaultShadowMap;

    private FFdShadows: TSFBool;
    public property FdShadows: TSFBool read FFdShadows;

    private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    { Transformation of this light node.
      Normal lights can be instanced many times within the scene, with
      various transformation, so @italic(this transformation property
      cannot be used).

      However, in special cases, you know that light node occurs only once
      within the scene (see
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadow_maps]).
      Then it's useful.

      It is gathered during traversing. Last BeforeTraverse call for this
      node sets Transform properties. By default, it represents identity
      transformation.

      @groupBegin }
    property Transform: TMatrix4Single read FTransform;
    { @groupEnd }

    { Matrices for rendering shadow map from this light.
      Identity in this class, override for subclasses able to do shadow mapping.
      @groupBegin }
    function ProjectionMatrix: TMatrix4Single; virtual;
    function ModelviewMatrix: TMatrix4Single; virtual;
    function ModelviewRotationMatrix: TMatrix4Single; virtual;
    function GetProjectorMatrix: TMatrix4Single;
    { @groupEnd }

    { Light location, direction and up vectors.
      Useful for example when you think of lights as cameras (for shadow maps).

      DirectionLocal must be exactly zero for PointLight (that doesn't
      have a direction).

      @groupBegin }
    function LocationLocal: TVector3Single; virtual;
    function Location: TVector3Single;
    function DirectionLocal: TVector3Single; virtual;
    function Direction: TVector3Single;
    procedure GetView(out Pos, Dir, Up: TVector3Single);
    { @groupEnd }

    { Calculate distances between the given Box and this light source.
      This is intended to capture the depth distances where the box
      resides, useful for calculating e.g. depth ranges to capture in
      the shadow maps.
      Depending on light source type, various distance measures may be used,
      appropriate to light sources projection.

      Always MinDistance <= MaxDistance. They may be negative when
      we measure along the light's direction.

      @raises EBox3DEmpty When used with an empty box. }
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); virtual; abstract;

    { Create TActiveLight record describing this light node under given
      State. }
    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight;

    { Update TActiveLight record when lighting State changes.
      Assumes that ActiveLight.LightNode = Self.

      This will set ActiveLight.Transform properties, and recalculate
      all ActiveLight.TransfXxx properties. }
    procedure UpdateActiveLightState(
      var ActiveLight: TActiveLight;
      State: TVRMLGraphTraverseState);

    { Update TActiveLight record when lighting location/direction (and other
      properties precalculated on TActiveLight) change.
      Assumes that ActiveLight.LightNode = Self. }
    procedure UpdateActiveLight(
      var ActiveLight: TActiveLight); virtual;

    function TransformationChange: TNodeTransformationChange; override;
  end;

  { Deprecated name for TNodeX3DLightNode. @deprecated }
  TVRMLLightNode = TNodeX3DLightNode;

  TVRMLDirectionalLightNode = class(TNodeX3DLightNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdProjectionRectangle: TSFVec4f;
    public property FdProjectionRectangle: TSFVec4f read FFdProjectionRectangle;

    private FFdProjectionLocation: TSFVec3f;
    public property FdProjectionLocation: TSFVec3f read FFdProjectionLocation;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;

    function ProjectionMatrix: TMatrix4Single; override;
    function ModelviewMatrix: TMatrix4Single; override;
    function ModelviewRotationMatrix: TMatrix4Single; override;
    function LocationLocal: TVector3Single; override;
    function DirectionLocal: TVector3Single; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  end;

  TVRMLPositionalLightNode = class(TNodeX3DLightNode)
  public
    procedure CreateNode; override;

    private FFdLocation: TSFVec3f;
    public property FdLocation: TSFVec3f read FFdLocation;

    private FFdAttenuation: TSFVec3f;
    public property FdAttenuation: TSFVec3f read FFdAttenuation;

    { Calculate light intensity drop because of the distance to the light.
      This follows the equation @code(1/max( attenuation[0] + ... ))
      from the VRML/X3D specification.

      Since calculating the DistanceToLight for the @link(Attenuation)
      method may be time-consuming in some situations,
      you can check DistanceNeededForAttenuation first.
      When the DistanceNeededForAttenuation returns @false,
      then the value of DistanceToLight parameter is ignored (you can
      pass anything).

      Remember that the DistanceToLight should be a distance in the light source
      local coordinate system.
      TODO: our ray-tracer currently uses global coord system.
      TODO: OpenGL renderer also uses global coord system, since OpenGL
      attenuation is calculated after modelview transformation
      (right? check).

      @groupBegin }
    function DistanceNeededForAttenuation: boolean;
    function Attenuation(const DistanceToLight: Single): Single; overload;
    function Attenuation(const DistanceToLight: Double): Double; overload;
    function HasAttenuation: boolean;
    { @groupEnd }

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

  TVRMLPointLightNode = class(TVRMLPositionalLightNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    function LocationLocal: TVector3Single; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  end;

  TNodeDirectionalLight_2 = class(TVRMLDirectionalLightNode)
  public
    procedure CreateNode; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;
  end;

  TNodePointLight_2 = class(TVRMLPointLightNode)
  public
    procedure CreateNode; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;
  end;

  TNodeSpotLight_2 = class(TVRMLPositionalLightNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbeamWidth: TSFFloat;
    public property FdbeamWidth: TSFFloat read FFdbeamWidth;

    private FFdcutOffAngle: TSFFloat;
    public property FdcutOffAngle: TSFFloat read FFdcutOffAngle;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdProjectionAngle: TSFFloat;
    public property FdProjectionAngle: TSFFloat read FFdProjectionAngle;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;

    function ProjectionMatrix: TMatrix4Single; override;
    function ModelviewMatrix: TMatrix4Single; override;
    function ModelviewRotationMatrix: TMatrix4Single; override;
    function LocationLocal: TVector3Single; override;
    function DirectionLocal: TVector3Single; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  FallbackProjectionNear = 1;
  FallbackProjectionFar = 100;
  FallbackProjectionRectangle: TVector4Single = (-10, -10, 10, 10);

procedure TNodeX3DLightNode.CreateNode;
begin
  inherited;

  FFdglobal := TSFBool.Create(Self, 'global', false);
   Fdglobal.ChangesAlways := [chEverything];
  Fields.Add(FFdglobal);

  FFdon := TSFBool.Create(Self, 'on', true);
   Fdon.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdon);

  FFdintensity := TSFFloat.Create(Self, 'intensity', 1);
   Fdintensity.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdintensity);

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   Fdcolor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdcolor);

  FFdambientIntensity := TSFFloat.Create(Self, 'ambientIntensity', 0);
   FdambientIntensity.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdambientIntensity);

  FFdkambiShadows := TSFBool.Create(Self, 'kambiShadows', false);
   FdkambiShadows.ChangesAlways := [chLightForShadowVolumes];
  Fields.Add(FFdkambiShadows);

  FFdkambiShadowsMain := TSFBool.Create(Self, 'kambiShadowsMain', false);
   FdkambiShadowsMain.ChangesAlways := [chLightForShadowVolumes];
  Fields.Add(FFdkambiShadowsMain);

  FFdshowProxyGeometry := TSFBool.Create(Self, 'showProxyGeometry', false);
  Fields.Add(FFdshowProxyGeometry);

  FFdProjectionNear := TSFFloat.Create(Self, 'projectionNear', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionNear.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdProjectionNear);

  FFdProjectionFar := TSFFloat.Create(Self, 'projectionFar', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionFar.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdProjectionFar);

  FFdUp := TSFVec3f.Create(Self, 'up', ZeroVector3Single);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdUp.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdUp);

  FFdDefaultShadowMap := TSFNode.Create(Self, 'defaultShadowMap', [TNodeGeneratedShadowMap]);
   FdDefaultShadowMap.Exposed := false;
   FdDefaultShadowMap.ChangesAlways := [chEverything];
  Fields.Add(FFdDefaultShadowMap);

  FFdShadows := TSFBool.Create(Self, 'shadows', false);
   FdShadows.Exposed := false;
   FdShadows.ChangesAlways := [chEverything];
  Fields.Add(FFdShadows);

  FFdEffects := TMFNode.Create(Self, 'effects', [TNodeEffect]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  Fields.Add(FFdEffects);

  DefaultContainerField := 'children';

  FTransform := IdentityMatrix4Single;
end;

procedure TNodeX3DLightNode.BeforeTraverse(
  StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;

  FTransform := StateStack.Top.Transform;
end;

function TNodeX3DLightNode.ProjectionMatrix: TMatrix4Single;
begin
  Result := IdentityMatrix4Single;
end;

function TNodeX3DLightNode.ModelviewMatrix: TMatrix4Single;
begin
  Result := IdentityMatrix4Single;
end;

function TNodeX3DLightNode.ModelviewRotationMatrix: TMatrix4Single;
begin
  Result := IdentityMatrix4Single;
end;

function TNodeX3DLightNode.GetProjectorMatrix: TMatrix4Single;
begin
  Result := ProjectionMatrix * ModelviewMatrix;
end;

function TNodeX3DLightNode.LocationLocal: TVector3Single;
begin
  Result := ZeroVector3Single;
end;

function TNodeX3DLightNode.Location: TVector3Single;
begin
  Result := MatrixMultPoint(Transform, LocationLocal);
end;

function TNodeX3DLightNode.Direction: TVector3Single;
begin
  Result := MatrixMultDirection(Transform, DirectionLocal);
end;

function TNodeX3DLightNode.DirectionLocal: TVector3Single;
begin
  Result := ZeroVector3Single;
end;

procedure TNodeX3DLightNode.GetView(out Pos, Dir, Up: TVector3Single);
begin
  Pos := MatrixMultPoint    (Transform, LocationLocal);
  Dir := MatrixMultDirection(Transform, DirectionLocal);

  Up := FdUp.Value;
  { Up = zero means "calculate anything suitable".
    We could let AnyOrthogonalVector do this job, but when +Y is sensible
    (when it results in something non-parallel to Dir), let's use it.
    This makes calculated "up" more deterministic. }
  if PerfectlyZeroVector(Up) then
    Up := Vector3Single(0, 1, 0);
  Up := MatrixMultDirection(Transform, Up);
  { When the "up" vector is parallel to the dir then fix it.
    Note: when "up" is not parallel, then ModelviewMatrix will
    take care of adjusting it to be orthogonal. }
  if VectorsParallel(Up, Dir) then
    Up := AnyOrthogonalVector(Dir);
end;

function TNodeX3DLightNode.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result.LightNode := Self;
  UpdateActiveLightState(Result, State);
end;

procedure TNodeX3DLightNode.UpdateActiveLightState(
  var ActiveLight: TActiveLight;
  State: TVRMLGraphTraverseState);
begin
  ActiveLight.Transform := State.Transform;
  ActiveLight.TransformScale := State.TransformScale;
  UpdateActiveLight(ActiveLight);
end;

procedure TNodeX3DLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  { Nothing to do in this class. }
  Assert(ActiveLight.LightNode = Self);
end;

procedure TNodeX3DLightNode.MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.AddVRML1ActiveLight(CreateActiveLight(StateStack.Top));
end;

function TNodeX3DLightNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLight;
end;

procedure TVRMLDirectionalLightNode.CreateNode;
begin
  inherited;

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
   Fddirection.ChangesAlways := [chLightActiveProperty, chLightLocationDirection];
  Fields.Add(FFddirection);

  FFdProjectionRectangle := TSFVec4f.Create(Self, 'projectionRectangle', ZeroVector4Single);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionRectangle.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdProjectionRectangle);

  FFdprojectionLocation := TSFVec3f.Create(Self, 'projectionLocation', ZeroVector3Single);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdprojectionLocation.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdprojectionLocation);
end;

class function TVRMLDirectionalLightNode.ClassNodeTypeName: string;
begin
  Result := 'DirectionalLight';
end;

procedure TVRMLDirectionalLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;
  ActiveLight.TransfNormDirection :=
    Normalized( MatrixMultDirection(ActiveLight.Transform, FdDirection.Value) );
end;

function TVRMLDirectionalLightNode.ProjectionMatrix: TMatrix4Single;
var
  N, F: Single;
  R: TVector4Single;
begin
  { If author didn't provide and VRMLShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  R := FdProjectionRectangle.Value;
  if PerfectlyZeroVector(R) then R := FallbackProjectionRectangle;

  { Beware: order of projectionRectangle
    is different than typical OpenGL and our OrthoProjMatrix params. }
  Result := OrthoProjMatrix(R[0], R[2], R[1], R[3], N, F);
end;

function TVRMLDirectionalLightNode.ModelviewMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);
  Result := LookDirMatrix(Pos, Dir, Up);
end;

function TVRMLDirectionalLightNode.ModelviewRotationMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);
  Result := LookDirMatrix(ZeroVector3Single, Dir, Up);
end;

function TVRMLDirectionalLightNode.LocationLocal: TVector3Single;
begin
  Result := FdProjectionLocation.Value;
end;

function TVRMLDirectionalLightNode.DirectionLocal: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TVRMLDirectionalLightNode.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  Box3DDirectionDistances(Box, Location, Direction, MinDistance, MaxDistance);
end;

procedure TVRMLPositionalLightNode.CreateNode;
begin
  inherited;

  FFdlocation := TSFVec3f.Create(Self, 'location', Vector3Single(0, 0, 0));
   Fdlocation.ChangesAlways := [chLightActiveProperty, chLightLocationDirection];
  Fields.Add(FFdlocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdattenuation := TSFVec3f.Create(Self, 'attenuation', Vector3Single(1, 0, 0));
   Fdattenuation.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdattenuation);
  { X3D specification comment: [0,Inf) }
end;

function TVRMLPositionalLightNode.DistanceNeededForAttenuation: boolean;
begin
  Result := (FdAttenuation.Value[1] <> 0) or
            (FdAttenuation.Value[2] <> 0);
end;

function TVRMLPositionalLightNode.HasAttenuation: boolean;
begin
  Result := ((FdAttenuation.Value[0] <> 1) and
             (FdAttenuation.Value[0] <> 0) ) or
            (FdAttenuation.Value[1] <> 0) or
            (FdAttenuation.Value[2] <> 0);
end;

procedure TVRMLPositionalLightNode.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  ActiveLight.TransfLocation := MatrixMultPoint(ActiveLight.Transform,
    FdLocation.Value);
end;

{$define ATTENUATION_IMPLEMENTATION:=
begin
 (* moglibysmy tu nie badac czy DistanceNeededForAttenuation i zawsze
    robic wersje pelna (bo przeciez
      FdAttenuation.Value[1] * DistanceToLight +
      FdAttenuation.Value[2] * Sqr(DistanceToLight)
    i tak bedzie = 0 gdy FdAttenuation.Value[1] = FdAttenuation.Value[2] = 0.
    Ale wydaje mi sie ze tak jest szybciej - testowanie kosztuje nas
    troszke czasu ale mozemy sobie w ten sposob ocalic 2 x mnozenie i dodawanie. *)

 (* we check whether attenuation = (0, 0, 0). VRML 97 spec says that specifying
    (0, 0, 0) should be equal to specifying (1, 0, 0). (well, we avoid
    division by zero possibility this way so it's quite sensible, even
    if it wastes some time) *)
 if (FdAttenuation.Value[0] = 0) and
    (FdAttenuation.Value[1] = 0) and
    (FdAttenuation.Value[2] = 0) then result := 1;

 if DistanceNeededForAttenuation then
  result := 1/ KambiUtils.max(FdAttenuation.Value[0] +
                   FdAttenuation.Value[1] * DistanceToLight +
                   FdAttenuation.Value[2] * Sqr(DistanceToLight), Single(1.0)) else
  result := 1/ KambiUtils.max(FdAttenuation.Value[0], Single(1.0));
end;}

function TVRMLPositionalLightNode.Attenuation(const DistanceToLight: Single): Single;
ATTENUATION_IMPLEMENTATION

function TVRMLPositionalLightNode.Attenuation(const DistanceToLight: Double): Double;
ATTENUATION_IMPLEMENTATION

procedure TVRMLPointLightNode.CreateNode;
begin
  inherited;
  { no new fields - this is just TVRMLPositionalLightNode }
end;

class function TVRMLPointLightNode.ClassNodeTypeName: string;
begin
  Result := 'PointLight';
end;

function TVRMLPointLightNode.LocationLocal: TVector3Single;
begin
  Result := FdLocation.Value;
end;

procedure TVRMLPointLightNode.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  Box3DPointDistances(Box, Location, MinDistance, MaxDistance);
end;

procedure TNodeDirectionalLight_2.CreateNode;
begin
  inherited;

  FdGlobal.Value := false;

  DefaultContainerField := 'children';
end;

function TNodeDirectionalLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeDirectionalLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeDirectionalLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodePointLight_2.CreateNode;
begin
  inherited;

  FFdradius := TSFFloat.Create(Self, 'radius', 100);
   Fdradius.ChangesAlways := [chLightActiveProperty];
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }

  FdGlobal.Value := true;

  DefaultContainerField := 'children';
end;

function TNodePointLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePointLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodePointLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodePointLight_2.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  { TODO: For non-uniform scale, this will simply use average scale.
    This is not fully correct, VRML spec doesn't clarify this
    but I guess that the intention was that the non-uniform scale will
    make radius non-uniform, i.e. light volume will not be a regular sphere
    but some 3d ellipsoid. Unfortunately this would require quite more
    work, UpdateVRML2ActiveLights would then have to check for collision
    between
      sphere transformed by matrix Transform
    and
      bounding box
    which I don't know how to do *easily*... }
  ActiveLight.TransfRadius := FdRadius.Value * ActiveLight.TransformScale;
end;

procedure TNodeSpotLight_2.CreateNode;
begin
  inherited;

  FFdbeamWidth := TSFFloat.Create(Self, 'beamWidth', Pi / 2);
   FdbeamWidth.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdbeamWidth);
  { X3D specification comment: (0,Pi/2] }

  FFdcutOffAngle := TSFFloat.Create(Self, 'cutOffAngle', Pi / 4);
   FdcutOffAngle.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdcutOffAngle);
  { X3D specification comment: (0,Pi/2] }

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
   Fddirection.ChangesAlways := [chLightActiveProperty, chLightLocationDirection];
  Fields.Add(FFddirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdradius := TSFFloat.Create(Self, 'radius', 100);
   Fdradius.ChangesAlways := [chEverything];
  Fields.Add(FFdradius);
  { X3D specification comment: [0,Inf) }

  FdGlobal.Value := true;

  FFdprojectionAngle := TSFFloat.Create(Self, 'projectionAngle', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdprojectionAngle.ChangesAlways := [chVisibleGeometry];
  Fields.Add(FFdprojectionAngle);

  DefaultContainerField := 'children';
end;

class function TNodeSpotLight_2.ClassNodeTypeName: string;
begin
  Result := 'SpotLight';
end;

function TNodeSpotLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeSpotLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSpotLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeSpotLight_2.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;

  ActiveLight.TransfNormDirection :=
    Normalized( MatrixMultDirection(ActiveLight.Transform, FdDirection.Value) );

  { TODO: For non-uniform scale, this is too easy,
    see TNodePointLight_2.CreateActiveLight for more comments. }
  ActiveLight.TransfRadius := FdRadius.Value * ActiveLight.TransformScale;
end;

function TNodeSpotLight_2.ProjectionMatrix: TMatrix4Single;
var
  Angle, N, F: Single;
begin
  { If author didn't provide and VRMLShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  if FdprojectionAngle.Value <= 0 then
    Angle := 2 * FdCutOffAngle.Value else
    Angle := FdprojectionAngle.Value;

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  Result := PerspectiveProjMatrixRad(Angle, 1, N, F);
end;

function TNodeSpotLight_2.ModelviewMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);
  Result := LookDirMatrix(Pos, Dir, Up);
end;

function TNodeSpotLight_2.ModelviewRotationMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);
  Result := LookDirMatrix(ZeroVector3Single, Dir, Up);
end;

function TNodeSpotLight_2.LocationLocal: TVector3Single;
begin
  Result := FdLocation.Value;
end;

function TNodeSpotLight_2.DirectionLocal: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TNodeSpotLight_2.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  { TODO: MaxDistance should be a little larger, as spot light rays
    are not parallel. }
  Box3DDirectionDistances(Box, Location, Direction, MinDistance, MaxDistance);
end;

procedure RegisterLightingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeDirectionalLight_2,
    TNodePointLight_2,
    TNodeSpotLight_2
  ]);
end;

{$endif read_implementation}
