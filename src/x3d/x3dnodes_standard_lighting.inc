{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TGeneratedShadowMapNode = class;

  TLightScope = (
    { Light shines everywhere. VRML/X3D >= 2 calls these lights global. }
    lsGlobal,
    { Light shines only within it's VRML/X3D grouping node.
      VRML/X3D >= 2 calls these lights not global. }
    lsLocal,
    { Light shines only on the following shapes within
      it's VRML/X3D grouping node.
      This is used by VRML 1.0 (and Inventor) light sources. }
    lsLocalVRML1);

  { Base class for all the light nodes. }
  TAbstractLightNode = class(TAbstractChildNode)
  private
    FTransformation: TTransformation;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    procedure CreateNode; override;

    strict private FFdOn: TSFBool;
    public property FdOn: TSFBool read FFdOn;

    strict private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    strict private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    strict private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    strict private FFdGlobal: TSFBool;
    public property FdGlobal: TSFBool read FFdGlobal;

    strict private FFdShadowVolumes: TSFBool;
    public property FdShadowVolumes: TSFBool read FFdShadowVolumes;

    strict private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    { Create TLightInstance record describing this light node under given
      State. }
    function CreateLightInstance(State: TX3DGraphTraverseState): TLightInstance;

    { Update TLightInstance record when lighting State changes.
      Assumes that LightInstance.Node = Self.

      This will set LightInstance.Transform properties, and recalculate
      all LightInstance properties based on Transform. }
    procedure UpdateLightInstanceState(
      var LightInstance: TLightInstance;
      State: TX3DGraphTraverseState);

    { Update TLightInstance record when lighting location/direction (and other
      properties precalculated on TLightInstance) change.
      Assumes that LightInstance.Node = Self. }
    procedure UpdateLightInstance(
      var LightInstance: TLightInstance); virtual;

    { Light scope. Default implementation returns lsGlobal or lsLocal,
      depending on "global" field value (this follows VRML/X3D >= 2.0 rules). }
    function Scope: TLightScope; virtual;

    { Transformation of this light node.
      Beware: In X3D, light nodes can be instantiated many times within the scene,
      with different transformations, so @italic(this transformation property
      should not be used in general).

      Use this only if you you know that light node occurs only once within the scene
      (see [https://castle-engine.io/x3d_extensions.php#section_ext_shadow_maps]).
      Or if you're fine with just getting an undefined transformation,
      for any of the light instances.

      This value is calculated during traversing. Last @link(BeforeTraverse) call for this
      node sets this property. By default, it represents identity transformation.

      @groupBegin }
    function Transform: TMatrix4;
    function InverseTransform: TMatrix4;
    { @groupEnd }

    function TransformationChange: TNodeTransformationChange; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dlightnode.inc}
  end;

  { Base class for all "punctual" light nodes (that have a position and/or direction).

    Note that even the old VRML 1.0 light nodes inherit from this.
    Although they interpret some bits differently
    ("ambientIntensity" < 0 has special meaning).
    But most of the fields behave identically, they only have different
    default values. }
  TAbstractPunctualLightNode = class(TAbstractLightNode)
  strict private
    function  GetProjectionSceneLocation: TVector3;
    procedure SetProjectionSceneLocation(const Value: TVector3);
    function  GetProjectionSceneDirection: TVector3;
    procedure SetProjectionSceneDirection(const Value: TVector3);
  protected
    function  GetProjectionLocationLocal: TVector3; virtual;
    procedure SetProjectionLocationLocal(const Value: TVector3); virtual;
    function  GetProjectionDirectionLocal: TVector3; virtual;
    procedure SetProjectionDirectionLocal(const Value: TVector3); virtual;
  public
    procedure CreateNode; override;

    strict private FFdShadowVolumesMain: TSFBool;
    public property FdShadowVolumesMain: TSFBool read FFdShadowVolumesMain;

    strict private FFdShowProxyGeometry: TSFBool;
    { showProxyGeometry field is an InstantReality extension, see
      [http://instant-reality.com/documentation/nodetype/Light/]. }
    public property FdShowProxyGeometry: TSFBool read FFdShowProxyGeometry;

    strict private FFdProjectionNear: TSFFloat;
    { projectionNear / projectionFar / up are Kambi extensions, see
      [https://castle-engine.io/x3d_extensions_shadow_maps.php#section_light_parameters] }
    public property FdProjectionNear: TSFFloat read FFdProjectionNear;

    strict private FFdProjectionFar: TSFFloat;
    public property FdProjectionFar: TSFFloat read FFdProjectionFar;

    strict private FFdUp: TSFVec3f;
    public property FdUp: TSFVec3f read FFdUp;

    strict private FFdDefaultShadowMap: TSFNode;
    public property FdDefaultShadowMap: TSFNode read FFdDefaultShadowMap;

    strict private FFdShadows: TSFBool;
    public property FdShadows: TSFBool read FFdShadows;

    { Matrices for rendering shadow map from this light.
      Identity in this class, override for subclasses able to do shadow mapping.
      @groupBegin }
    function ProjectionMatrix: TMatrix4; virtual;
    function ModelviewMatrix: TMatrix4; virtual;
    function ModelviewRotationMatrix: TMatrix4; virtual;
    function GetProjectorMatrix: TMatrix4;
    { @groupEnd }

    { Light location, direction and up vectors, for projection.
      Useful when you think of lights as cameras (for shadow maps).

      ProjectionDirectionLocal will always be exactly zero
      for a PointLight (that doesn't have a direction).

      The GetView returns vectors in global (scene) space.
      It guarantees that Direction, Side and Up are normalized
      and orthogonal to each other.

      @groupBegin }
    property   ProjectionLocationLocal: TVector3
      read  GetProjectionLocationLocal
      write SetProjectionLocationLocal;

    property   ProjectionDirectionLocal: TVector3
      read  GetProjectionDirectionLocal
      write SetProjectionDirectionLocal;

    property   ProjectionSceneLocation: TVector3
      read  GetProjectionSceneLocation
      write SetProjectionSceneLocation;

    property   ProjectionSceneDirection: TVector3
      read  GetProjectionSceneDirection
      write SetProjectionSceneDirection;

    procedure GetView(out Pos, Dir, Side, Up: TVector3);
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

    { Position and direction expressed in homogeneous coordinates.
      For positional lights, the last component is always 1.
      For directional lights, the last component is always 0.

      Note that this is expressed in the local light node coordinate system. }
    function PositionAndDirection: TVector4; virtual; abstract;

    {$I auto_generated_node_helpers/x3dnodes_x3dpunctuallightnode.inc}
  end;

  { Base class for all directional lights. }
  TAbstractDirectionalLightNode = class(TAbstractPunctualLightNode)
  strict private
    function GetDirection: TVector3;
    procedure SetDirection(const Value: TVector3);
    function GetProjectionRectangle: TVector4;
    procedure SetProjectionRectangle(const Value: TVector4);
    function GetProjectionLocation: TVector3;
    procedure SetProjectionLocation(const Value: TVector3);
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
    function  GetProjectionDirectionLocal: TVector3; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;
    property Direction: TVector3 read GetDirection write SetDirection;

    strict private FFdProjectionRectangle: TSFVec4f;
    public property FdProjectionRectangle: TSFVec4f read FFdProjectionRectangle;
    property ProjectionRectangle: TVector4 read GetProjectionRectangle write SetProjectionRectangle;

    strict private FFdProjectionLocation: TSFVec3f;
    public property FdProjectionLocation: TSFVec3f read FFdProjectionLocation;
    property ProjectionLocation: TVector3 read GetProjectionLocation write SetProjectionLocation;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;

    function ProjectionMatrix: TMatrix4; override;
    function ModelviewMatrix: TMatrix4; override;
    function ModelviewRotationMatrix: TMatrix4; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
    function PositionAndDirection: TVector4; override;
  end;

  { Base class for all positional lights. }
  TAbstractPositionalLightNode = class(TAbstractPunctualLightNode)
  strict private
    function GetAttenuation: TVector3;
    procedure SetAttenuation(const Value: TVector3);
    function GetLocation: TVector3;
    procedure SetLocation(const Value: TVector3);
    function GetSceneLocation: TVector3;
    procedure SetSceneLocation(const Value: TVector3);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
  public
    procedure CreateNode; override;

    strict private FFdAttenuation: TSFVec3f;
    public property FdAttenuation: TSFVec3f read FFdAttenuation;
    property Attenuation: TVector3 read GetAttenuation write SetAttenuation;

    strict private FFdLocation: TSFVec3f;
    public property FdLocation: TSFVec3f read FFdLocation;
    property Location: TVector3 read GetLocation write SetLocation;

    { Light location, expressed in scene (current TCastleScene) coordinates.
      @italic(Do not use this if the light node may be instantiated multiple
      times in the scene.) The same warning as for @link(Transform) applies
      also here. }
    property SceneLocation: TVector3
      read GetSceneLocation write SetSceneLocation;

    { Calculate light intensity drop because of the distance to the light.
      This follows the equation @code(1/max( attenuation[0] + ... ))
      from the VRML/X3D specification, it is the same as OpenGL attenuation.

      Since calculating the DistanceToLight for the @link(Attenuation)
      method may be time-consuming in some situations,
      you can check DistanceNeededForAttenuation first.
      When the DistanceNeededForAttenuation returns @false,
      then the value of DistanceToLight parameter is ignored (you can
      pass anything).

      The DistanceToLight should be a distance in the light source
      local coordinate system. TODO: although our renderers currently
      ignore this: ray-tracer uses global coord system,
      OpenGL (fixed-function and shader) renderer uses eye coord system
      (should be equal to global coord system for normal cameras).

      @groupBegin }
    function DistanceNeededForAttenuation: boolean;
    function CalculateAttenuation(const DistanceToLight: Single): Single; overload;
    { @groupEnd }

    { Is attenuation relevant. When @false, you know that @link(Attenuation)
      function always returns 1, so there's no point in using it at all. }
    function HasAttenuation: boolean;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;
    property Radius: Single read GetRadius write SetRadius;

    { Should the "radius" field be taken into account. }
    function HasRadius: boolean; virtual;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;
    function PositionAndDirection: TVector4; override;
  end;

  { Point light source, that shines from a given point in all directions around,
    for all VRML and X3D versions. }
  TAbstractPointLightNode = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  end;

  { Light source that shines along a given direction, like a sun. }
  TDirectionalLightNode = class(TAbstractDirectionalLightNode)
  public
    procedure CreateNode; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    {$I auto_generated_node_helpers/x3dnodes_directionallight.inc}
  end;
  TDirectionalLightNode_2 = TDirectionalLightNode;

  { Point light source, that shines from a given point in all directions around,
    for X3D. }
  TPointLightNode = class(TAbstractPointLightNode)
  public
    procedure CreateNode; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    {$I auto_generated_node_helpers/x3dnodes_pointlight.inc}
  end;
  TPointLightNode_2 = TPointLightNode;

  { Light source that emits light from a specific point along a specific direction
    constrained within a cone. }
  TSpotLightNode = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
    function  GetProjectionDirectionLocal: TVector3; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdBeamWidth: TSFFloat;
    public property FdBeamWidth: TSFFloat read FFdBeamWidth;

    strict private FFdCutOffAngle: TSFFloat;
    public property FdCutOffAngle: TSFFloat read FFdCutOffAngle;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdProjectionAngle: TSFFloat;
    public property FdProjectionAngle: TSFFloat read FFdProjectionAngle;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;

    function ProjectionMatrix: TMatrix4; override;
    function ModelviewMatrix: TMatrix4; override;
    function ModelviewRotationMatrix: TMatrix4; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;

    { Spot cutoff angle (based on cutOffAngle).

      Expressed in degrees, clamped to correct range
      (since user can input any value in VRML, and also conversion
      radians -> degrees could accidentally raise value slightly > 90,
      so cutOffAngle = 1.5708 is in degrees 90.0002104591,
      which would cause OpenGL fixed-function error). }
    function SpotCutoffDeg: Single;

    function SpotCosCutoff: Single;

    { Approximate spot exponent that could be used to render this spot light.
      Do not use this, unless you really have to.
      X3D spot light should have a linear fallback, from beamWidth to cutOffAngle,
      and there is no sensible way to approximate it by an exponent.
      Use this only if you have to render spot light with exponent,
      e.g. for OpenGL fixed-function pipeline. }
    function SpotExponentApproximate: Single;

    {$I auto_generated_node_helpers/x3dnodes_spotlight.inc}
  end;
  TSpotLightNode_2 = TSpotLightNode;

  TEnvironmentLightNode = class(TAbstractLightNode)
  public
    {$I auto_generated_node_helpers/x3dnodes_environmentlight.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  FallbackProjectionNear = 1;
  FallbackProjectionFar = 100;
  FallbackProjectionRectangle: TVector4 = (Data: (-10, -10, 10, 10));

{ TAbstractLightNode --------------------------------------------------------- }

procedure TAbstractLightNode.CreateNode;
begin
  inherited;

  FFdGlobal := TSFBool.Create(Self, true, 'global', false);
   FdGlobal.ChangeAlways := chEverything;
  AddField(FFdGlobal);

  FFdOn := TSFBool.Create(Self, true, 'on', true);
   FdOn.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdOn);

  FFdIntensity := TSFFloat.Create(Self, true, 'intensity', 1);
   FdIntensity.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdIntensity);

  FFdColor := TSFColor.Create(Self, true, 'color', Vector3(1, 1, 1));
   FdColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdColor);

  FFdAmbientIntensity := TSFFloat.Create(Self, true, 'ambientIntensity', 0);
   FdAmbientIntensity.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientIntensity);

  FFdShadowVolumes := TSFBool.Create(Self, true, 'shadowVolumes', false);
   FdShadowVolumes.AddAlternativeName('kambiShadows', 0);
   FdShadowVolumes.ChangeAlways := chLightForShadowVolumes;
  AddField(FFdShadowVolumes);

  FFdEffects := TMFNode.Create(Self, false, 'effects', [TEffectNode]);
   FdEffects.ChangeAlways := chEverything;
  AddField(FFdEffects);

  DefaultContainerField := 'children';

  FTransformation.Init;
end;

procedure TAbstractLightNode.BeforeTraverse(
  StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;

  FTransformation := StateStack.Top.Transformation;
end;

procedure TAbstractLightNode.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  if Scope = lsLocalVRML1 then
    StateStack.Top.AddLight(CreateLightInstance(StateStack.Top));
end;

function TAbstractLightNode.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal
  else
    Result := lsLocal;
end;

procedure TAbstractLightNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  inherited;
  if Scope = lsLocal then
  begin
    (*We append lights with lsLocal scope (global=FALSE and VRML/X3D >= 2.0)
      to the current State.

      It may seem wasteful to enumerate children twice
      (first we enumerate in TAbstractGroupingNode.BeforeTraverse,
      then actual TX3DNode.Traverse enumerates again,
      this time recursively time). But it turned out to
      be the fastest and simplest method of propagating lsLocal lights
      correctly...

      Other (rejected) approach was to add ParentGroup to TLightInstance
      and do it as post-processing step, i.e. in TCastleSceneCore in
      AddGlobalLights take this ParentGroup and add light everywhere.
      But that was

      1. even slower, since it must traverse ParentGroup once again
         for each directional light, and for each shape within this ParentGroup,
         it must find all it's occurrences inside Shapes, and add light there

      2. it fails ugly in case of DEF / USE of shapes.

         See demo_models/lights_materials/directional_light_scope.wrl test. Imagine
         (simplified VRML below) :

           Group {
             DEF S Shape { .... some sphere .... }
             Group {
               USE S
               DirectionalLight { }
               USE S
             }
             USE S
           }

         What would happen here? ParentGroup.Enumerate would find S *two* times.
         For each of these occurrences, it would find *four* shapes with
         matching node value. So 1. all four spheres would be lighted (incorrect,
         only two spheres in the middle should be lighted) 2. all of them would
         be lighted two times by the same light... Fix for 1st problem would
         require us to record some list of parents within State (which would
         awfully slow down Traverse work, that already is too crowded). Fix for 2nd
         problem would require some intelligent avoiding of duplicates
         (set light only for first node, that is both matching and has the light
         not set yet).

         So fixing the above solution would be quite more convoluted and slower
         than simple, correct solution below.

    *)

    { Note: We do CreateLightInstance with State from TAbstractGroupingNode node,
      while precisely we should rather use State from TAbstractLightNode
      traverse. But, fortunately, State of TAbstractLightNode doesn't
      change inside (TAbstractLightNode doesn't do anything inside
      BeforeTraverse), so this is the same thing. }

    { TODO: we ignore here light radius, which may be relevant
      for TAbstractPositionalLightNode with HasRadius = true.
      E.g. PointLight with global = false should be limited by *both*
      scope (in graph hierarchy) and radius, as far as I understand. }

    State.AddLight(CreateLightInstance(State));
  end;
end;

function TAbstractLightNode.CreateLightInstance(
  State: TX3DGraphTraverseState): TLightInstance;
begin
  Result.Node := Self;
  Result.WorldCoordinates := false;
  UpdateLightInstanceState(Result, State);
end;

procedure TAbstractLightNode.UpdateLightInstanceState(
  var LightInstance: TLightInstance;
  State: TX3DGraphTraverseState);
begin
  LightInstance.Transform := State.Transformation.Transform;
  LightInstance.TransformScale := State.Transformation.Scale;
  UpdateLightInstance(LightInstance);
end;

procedure TAbstractLightNode.UpdateLightInstance(
  var LightInstance: TLightInstance);
begin
  { Nothing to do in this class. }
  Assert(LightInstance.Node = Self);
end;

function TAbstractLightNode.Transform: TMatrix4;
begin
  Result := FTransformation.Transform;
end;

function TAbstractLightNode.InverseTransform: TMatrix4;
begin
  Result := FTransformation.InverseTransform;
end;

function TAbstractLightNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLight;
end;

{ TAbstractPunctualLightNode --------------------------------------------------------- }

procedure TAbstractPunctualLightNode.CreateNode;
begin
  inherited;

  FFdShadowVolumesMain := TSFBool.Create(Self, true, 'shadowVolumesMain', false);
   FdShadowVolumesMain.AddAlternativeName('kambiShadowsMain', 0);
   FdShadowVolumesMain.ChangeAlways := chLightForShadowVolumes;
  AddField(FFdShadowVolumesMain);

  FFdShowProxyGeometry := TSFBool.Create(Self, true, 'showProxyGeometry', false);
  AddField(FFdShowProxyGeometry);

  FFdProjectionNear := TSFFloat.Create(Self, true, 'projectionNear', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionNear.ChangeAlways := chVisibleGeometry;
  AddField(FFdProjectionNear);

  FFdProjectionFar := TSFFloat.Create(Self, true, 'projectionFar', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionFar.ChangeAlways := chVisibleGeometry;
  AddField(FFdProjectionFar);

  FFdUp := TSFVec3f.Create(Self, true, 'up', TVector3.Zero);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdUp.ChangeAlways := chVisibleGeometry;
  AddField(FFdUp);

  FFdDefaultShadowMap := TSFNode.Create(Self, false, 'defaultShadowMap', [TGeneratedShadowMapNode]);
   FdDefaultShadowMap.ChangeAlways := chEverything;
  AddField(FFdDefaultShadowMap);

  FFdShadows := TSFBool.Create(Self, false, 'shadows', false);
   FdShadows.ChangeAlways := chShadowMaps;
  AddField(FFdShadows);
end;

function TAbstractPunctualLightNode.ProjectionMatrix: TMatrix4;
begin
  Result := TMatrix4.Identity;
end;

function TAbstractPunctualLightNode.ModelviewMatrix: TMatrix4;
begin
  Result := TMatrix4.Identity;
end;

function TAbstractPunctualLightNode.ModelviewRotationMatrix: TMatrix4;
begin
  Result := TMatrix4.Identity;
end;

function TAbstractPunctualLightNode.GetProjectorMatrix: TMatrix4;
begin
  Result := ProjectionMatrix * ModelviewMatrix;
end;

function TAbstractPunctualLightNode.GetProjectionLocationLocal: TVector3;
begin
  Result := TVector3.Zero;
end;

procedure TAbstractPunctualLightNode.SetProjectionLocationLocal(const Value: TVector3);
begin
  { ignored }
end;

function TAbstractPunctualLightNode.GetProjectionDirectionLocal: TVector3;
begin
  Result := TVector3.Zero;
end;

procedure TAbstractPunctualLightNode.SetProjectionDirectionLocal(const Value: TVector3);
begin
  { ignored }
end;

function TAbstractPunctualLightNode.GetProjectionSceneLocation: TVector3;
begin
  Result := FTransformation.Transform.MultPoint(ProjectionLocationLocal);
end;

procedure TAbstractPunctualLightNode.SetProjectionSceneLocation(const Value: TVector3);
begin
  ProjectionLocationLocal := FTransformation.InverseTransform.MultPoint(Value);
end;

function TAbstractPunctualLightNode.GetProjectionSceneDirection: TVector3;
begin
  Result := Transform.MultDirection(ProjectionDirectionLocal);
end;

procedure TAbstractPunctualLightNode.SetProjectionSceneDirection(const Value: TVector3);
begin
  ProjectionDirectionLocal := FTransformation.InverseTransform.MultDirection(Value);
end;

procedure TAbstractPunctualLightNode.GetView(out Pos, Dir, Side, Up: TVector3);
begin
  Pos := ProjectionSceneLocation;
  Dir := ProjectionSceneDirection;

  Up := FdUp.Value;
  { Up = zero means "calculate anything suitable".
    We could let AnyOrthogonalVector do this job, but when +Y is sensible
    (when it results in something non-parallel to Dir), let's use it.
    This makes calculated "up" more deterministic. }
  if Up.IsPerfectlyZero then
    Up := Vector3(0, 1, 0);
  Up := Transform.MultDirection(Up);
  { When the "up" vector is parallel to the dir then fix it. }
  if VectorsParallel(Up, Dir) then
    Up := AnyOrthogonalVector(Dir);

  { now normalize all vectors,
    calculate Side,
    and make Up orthogonal to Dir }
  Side := TVector3.CrossProduct(Dir, Up).Normalize;
  Dir.NormalizeMe;
  Up := TVector3.CrossProduct(Side, Dir);
end;

{ TAbstractDirectionalLightNode ---------------------------------------------- }

procedure TAbstractDirectionalLightNode.CreateNode;
begin
  inherited;

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 0, -1));
   FdDirection.ChangeAlways := chLightLocationDirection;
  AddField(FFdDirection);

  FFdProjectionRectangle := TSFVec4f.Create(Self, true, 'projectionRectangle', TVector4.Zero);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionRectangle.ChangeAlways := chVisibleGeometry;
  AddField(FFdProjectionRectangle);

  FFdProjectionLocation := TSFVec3f.Create(Self, true, 'projectionLocation', TVector3.Zero);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionLocation.ChangeAlways := chVisibleGeometry;
  AddField(FFdProjectionLocation);
end;

class function TAbstractDirectionalLightNode.ClassX3DType: string;
begin
  Result := 'DirectionalLight';
end;

procedure TAbstractDirectionalLightNode.UpdateLightInstance(
  var LightInstance: TLightInstance);
begin
  inherited;
  LightInstance.Direction :=
    LightInstance.Transform.MultDirection(FdDirection.Value).Normalize;
end;

function TAbstractDirectionalLightNode.ProjectionMatrix: TMatrix4;
var
  N, F: Single;
  R: TVector4;
  Dimensions: TFloatRectangle;
begin
  { If author didn't provide and CastleInternalShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  R := FdProjectionRectangle.Value;
  if R.IsPerfectlyZero then R := FallbackProjectionRectangle;

  Dimensions.Left   := R[0];
  Dimensions.Bottom := R[1];
  Dimensions.Width  := R[2] - Dimensions.Left;
  Dimensions.Height := R[3] - Dimensions.Bottom;

  Result := OrthoProjectionMatrix(Dimensions, N, F);
end;

function TAbstractDirectionalLightNode.ModelviewMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := LookDirMatrix(Pos, Dir, Side, AUp);
end;

function TAbstractDirectionalLightNode.ModelviewRotationMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := FastLookDirMatrix(Dir, AUp);
end;

function TAbstractDirectionalLightNode.GetProjectionLocationLocal: TVector3;
begin
  Result := FdProjectionLocation.Value;
end;

procedure TAbstractDirectionalLightNode.SetProjectionLocationLocal(const Value: TVector3);
begin
  FdProjectionLocation.Send(Value);
end;

function TAbstractDirectionalLightNode.GetProjectionDirectionLocal: TVector3;
begin
  Result := FdDirection.Value;
end;

procedure TAbstractDirectionalLightNode.SetProjectionDirectionLocal(const Value: TVector3);
begin
  FdDirection.Send(Value);
end;

procedure TAbstractDirectionalLightNode.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  Box.DirectionDistances(ProjectionSceneLocation, ProjectionSceneDirection, MinDistance, MaxDistance);
end;

function TAbstractDirectionalLightNode.PositionAndDirection: TVector4;
begin
  Result := Vector4(-FdDirection.Value, 0);
end;

function TAbstractDirectionalLightNode.GetDirection: TVector3;
begin
  Result := FdDirection.Value;
end;

procedure TAbstractDirectionalLightNode.SetDirection(const Value: TVector3);
begin
  FdDirection.Send(Value);
end;

function TAbstractDirectionalLightNode.GetProjectionRectangle: TVector4;
begin
  Result := FdProjectionRectangle.Value;
end;

procedure TAbstractDirectionalLightNode.SetProjectionRectangle(const Value: TVector4);
begin
  FdProjectionRectangle.Send(Value);
end;

function TAbstractDirectionalLightNode.GetProjectionLocation: TVector3;
begin
  Result := FdProjectionLocation.Value;
end;

procedure TAbstractDirectionalLightNode.SetProjectionLocation(const Value: TVector3);
begin
  FdProjectionLocation.Send(Value);
end;

procedure TAbstractPositionalLightNode.CreateNode;
begin
  inherited;

  FFdLocation := TSFVec3f.Create(Self, true, 'location', Vector3(0, 0, 0));
   FdLocation.ChangeAlways := chLightLocationDirection;
  AddField(FFdLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdAttenuation := TSFVec3f.Create(Self, true, 'attenuation', Vector3(1, 0, 0));
   FdAttenuation.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAttenuation);
  { X3D specification comment: [0,Inf) }

  FFdRadius := TSFFloat.Create(Self, true, 'radius', 100);
   FdRadius.ChangeAlways := chLightInstanceProperty;
  AddField(FFdRadius);
  { X3D specification comment: [0,Inf) }
end;

function TAbstractPositionalLightNode.DistanceNeededForAttenuation: boolean;
begin
  Result := (FdAttenuation.Value[1] <> 0) or
            (FdAttenuation.Value[2] <> 0);
end;

function TAbstractPositionalLightNode.HasAttenuation: boolean;
begin
  Result := ((FdAttenuation.Value[0] <> 1) and
             (FdAttenuation.Value[0] <> 0)) or
            (FdAttenuation.Value[1] <> 0) or
            (FdAttenuation.Value[2] <> 0);
end;

function TAbstractPositionalLightNode.HasRadius: boolean;
begin
  { When radius < 0, behave like radius is infinite.
    Useful for implementing glTF punctual lights. }
  Result := Radius >= 0;
end;

procedure TAbstractPositionalLightNode.UpdateLightInstance(
  var LightInstance: TLightInstance);
begin
  inherited;

  LightInstance.Location := LightInstance.Transform.MultPoint(
    FdLocation.Value);

  { TODO: For non-uniform scale, this will simply use average scale.
    This is not fully correct, VRML spec doesn't clarify this
    but I guess that the intention was that the non-uniform scale will
    make radius non-uniform, i.e. light volume will not be a regular sphere
    but some 3d ellipsoid. Unfortunately this would require quite more
    work, AddGlobalLights (in TCastleSceneCore) would then have to check for collision
    between
      sphere transformed by matrix Transform
    and
      bounding box
    which I don't know how to do *easily*... }
  LightInstance.Radius := FdRadius.Value * LightInstance.TransformScale;
end;

function TAbstractPositionalLightNode.CalculateAttenuation(const DistanceToLight: Single): Single;
begin
  { We don't really need to check DistanceNeededForAttenuation here.
    But it may be more optimal, since it's often false,
    and then we don't need to do 2x multiplications and 1x addition. }
  if DistanceNeededForAttenuation then
    Result := 1 / Max(
      FdAttenuation.Value[0] +
      FdAttenuation.Value[1] * DistanceToLight +
      FdAttenuation.Value[2] * Sqr(DistanceToLight), 1.0) else
    Result := 1 / Max(
      FdAttenuation.Value[0], 1.0);
end;

function TAbstractPositionalLightNode.PositionAndDirection: TVector4;
begin
  Result := Vector4(FdLocation.Value, 1);
end;

function TAbstractPositionalLightNode.GetAttenuation: TVector3;
begin
  Result := FdAttenuation.Value;
end;

procedure TAbstractPositionalLightNode.SetAttenuation(const Value: TVector3);
begin
  FdAttenuation.Send(Value);
end;

function TAbstractPositionalLightNode.GetLocation: TVector3;
begin
  Result := FdLocation.Value;
end;

procedure TAbstractPositionalLightNode.SetLocation(const Value: TVector3);
begin
  FdLocation.Send(Value);
end;

function TAbstractPositionalLightNode.GetSceneLocation: TVector3;
begin
  Result := Transform.MultPoint(Location);
end;

procedure TAbstractPositionalLightNode.SetSceneLocation(const Value: TVector3);
begin
  Location := FTransformation.InverseTransform.MultPoint(Value);
end;

function TAbstractPositionalLightNode.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TAbstractPositionalLightNode.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

procedure TAbstractPointLightNode.CreateNode;
begin
  inherited;
  { no new fields - this is just TAbstractPositionalLightNode }
end;

class function TAbstractPointLightNode.ClassX3DType: string;
begin
  Result := 'PointLight';
end;

function TAbstractPointLightNode.GetProjectionLocationLocal: TVector3;
begin
  Result := FdLocation.Value;
end;

procedure TAbstractPointLightNode.SetProjectionLocationLocal(const Value: TVector3);
begin
  FdLocation.Send(Value);
end;

procedure TAbstractPointLightNode.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  Box.PointDistances(ProjectionSceneLocation, MinDistance, MaxDistance);
end;

procedure TDirectionalLightNode.CreateNode;
begin
  inherited;

  FdGlobal.Value := false;

  DefaultContainerField := 'children';
end;

class function TDirectionalLightNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TPointLightNode.CreateNode;
begin
  inherited;

  FdGlobal.Value := true;

  DefaultContainerField := 'children';
end;

class function TPointLightNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TSpotLightNode.CreateNode;
begin
  inherited;

  FFdBeamWidth := TSFFloat.Create(Self, true, 'beamWidth', Pi / 2);
   FdBeamWidth.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdBeamWidth);
  { X3D specification comment: (0,Pi/2] }

  FFdCutOffAngle := TSFFloat.Create(Self, true, 'cutOffAngle', Pi / 4);
   FdCutOffAngle.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdCutOffAngle);
  { X3D specification comment: (0,Pi/2] }

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 0, -1));
   FdDirection.ChangeAlways := chLightLocationDirection;
  AddField(FFdDirection);
  { X3D specification comment: (-Inf,Inf) }

  FdGlobal.Value := true;

  FFdProjectionAngle := TSFFloat.Create(Self, true, 'projectionAngle', 0);
   { We want to set UpdateNeeded := true for all GeneratedShadowMap using
     this light. For now, simply send chVisibleGeometry that will
     force updating all GeneratedShadowMaps. }
   FdProjectionAngle.ChangeAlways := chVisibleGeometry;
  AddField(FFdProjectionAngle);

  DefaultContainerField := 'children';
end;

class function TSpotLightNode.ClassX3DType: string;
begin
  Result := 'SpotLight';
end;

class function TSpotLightNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TSpotLightNode.UpdateLightInstance(
  var LightInstance: TLightInstance);
begin
  inherited;
  LightInstance.Direction :=
    LightInstance.Transform.MultDirection(FdDirection.Value).Normalize;
end;

function TSpotLightNode.ProjectionMatrix: TMatrix4;
var
  Angle, N, F: Single;
begin
  { If author didn't provide and CastleInternalShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  if FdprojectionAngle.Value <= 0 then
    Angle := 2 * FdCutOffAngle.Value else
    Angle := FdProjectionAngle.Value;

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  Result := PerspectiveProjectionMatrixRad(Angle, 1, N, F);
end;

function TSpotLightNode.ModelviewMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := LookDirMatrix(Pos, Dir, Side, AUp);
end;

function TSpotLightNode.ModelviewRotationMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := FastLookDirMatrix(Dir, AUp);
end;

function TSpotLightNode.GetProjectionLocationLocal: TVector3;
begin
  Result := FdLocation.Value;
end;

procedure TSpotLightNode.SetProjectionLocationLocal(const Value: TVector3);
begin
  FdLocation.Send(Value);
end;

function TSpotLightNode.GetProjectionDirectionLocal: TVector3;
begin
  Result := FdDirection.Value;
end;

procedure TSpotLightNode.SetProjectionDirectionLocal(const Value: TVector3);
begin
  FdDirection.Send(Value);
end;

procedure TSpotLightNode.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  { TODO: MaxDistance should be a little larger, as spot light rays
    are not parallel. }
  Box.DirectionDistances(ProjectionSceneLocation, ProjectionSceneDirection, MinDistance, MaxDistance);
end;

function TSpotLightNode.SpotCutoffDeg: Single;
begin
  Result := Min(90, RadToDeg(FdCutOffAngle.Value));
end;

function TSpotLightNode.SpotCosCutoff: Single;
begin
  Result := Cos(FdCutOffAngle.Value);
end;

function TSpotLightNode.SpotExponentApproximate: Single;
begin
  { There is no way to exactly translate beamWidth to OpenGL GL_SPOT_EXPONENT.
    GL_SPOT_EXPONENT is an exponent for cosinus.
    beamWidth says to use constant intensity within beamWidth angle,
    and linear drop off to cutOffAngle.
    See [https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.opengl_rendering.html#section.vrml_lights]
    for more discussion. }

  if FdBeamWidth.Value >= FdCutOffAngle.Value then
    Result := 0 else
    Result := Clamped(0.5 / Max(FdBeamWidth.Value, 0.0001), 0.0, 128.0);
end;

{ TEnvironmentLightNode ------------------------------------------------------ }

procedure RegisterLightingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TDirectionalLightNode,
    TPointLightNode,
    TSpotLightNode,
    TEnvironmentLightNode
  ]);
end;

{$endif read_implementation}
