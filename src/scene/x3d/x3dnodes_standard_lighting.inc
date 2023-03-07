{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

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
  strict private
    FTransformation: TTransformation;
    WarningWorldTransformDone: Boolean;
    procedure DoWarningWorldTransform;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    { Does this node act as a headlight.
      Set and used internally in CGE.
      @exclude }
    InternalHeadlight: Boolean;

    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

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

    { Transformation of this light node, to world space.
      This accounts for transformation to scene space (TCastleScene)
      and then to viewport root (TCastleViewport.Items) space.

      Beware: In X3D, light nodes can be instantiated many times within the scene,
      with different transformations, so @italic(this transformation property
      should not be used in general).

      Use this only if you you know that light node occurs only once within the scene.
      For example shadow maps assume this,
      see [https://castle-engine.io/x3d_extensions.php#section_ext_shadow_maps .

      For this to work, we multiply transformation from last traverse
      (@link(BeforeTraverse) collects transformation to space)
      and then use TCastleTransform.WorldTransform.

      @groupBegin }
    function Transform: TMatrix4;
    function InverseTransform: TMatrix4;
    { @groupEnd }

    function TransformationChange: TNodeTransformationChange; override;

  {$I auto_generated_node_helpers/x3dnodes_x3dlightnode.inc}
  end;

  { Base class for all "punctual" light nodes (that have a position and/or direction). }
  TAbstractPunctualLightNode = class(TAbstractLightNode)
  strict private
    function  GetProjectionWorldLocation: TVector3;
    procedure SetProjectionWorldLocation(const Value: TVector3);
    function  GetProjectionWorldDirection: TVector3;
    procedure SetProjectionWorldDirection(const Value: TVector3);
  protected
    function  GetProjectionLocationLocal: TVector3; virtual;
    procedure SetProjectionLocationLocal(const Value: TVector3); virtual;
    function  GetProjectionDirectionLocal: TVector3; virtual;
    procedure SetProjectionDirectionLocal(const Value: TVector3); virtual;
  public
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

    property   ProjectionWorldLocation: TVector3
      read  GetProjectionWorldLocation
      write SetProjectionWorldLocation;

    property   ProjectionWorldDirection: TVector3
      read  GetProjectionWorldDirection
      write SetProjectionWorldDirection;

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
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
    function  GetProjectionDirectionLocal: TVector3; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3); override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;
    function ProjectionMatrix: TMatrix4; override;
    function ModelviewMatrix: TMatrix4; override;
    function ModelviewRotationMatrix: TMatrix4; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
    function PositionAndDirection: TVector4; override;
  {$I auto_generated_node_helpers/x3dnodes_x3ddirectionallightnode.inc}
  end;

  { Base class for all positional lights. }
  TAbstractPositionalLightNode = class(TAbstractPunctualLightNode)
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

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

    { Should the "radius" field be taken into account. }
    function HasRadius: boolean; virtual;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;
    function PositionAndDirection: TVector4; override;
  {$I auto_generated_node_helpers/x3dnodes_x3dpositionallightnode.inc}
  end;

  { Point light source, that shines from a given point in all directions around,
    for all VRML and X3D versions. }
  TAbstractPointLightNode = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
  public
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  {$I auto_generated_node_helpers/x3dnodes_x3dpointlightnode.inc}
  end;

  { Light source that shines along a given direction, like a sun. }
  TDirectionalLightNode = class(TAbstractDirectionalLightNode)
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_directionallight.inc}
  end;
  TDirectionalLightNode_2 = TDirectionalLightNode;

  { Point light source, that shines from a given point in all directions around,
    for X3D. }
  TPointLightNode = class(TAbstractPointLightNode)
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
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

    { Projection angle, from @link(ProjectionAngle) or 2 * @link(CutOffAngle).
      See https://castle-engine.io/x3d_extensions_shadow_maps.php . }
    function EffectiveProjectionAngle: Single;

  {$I auto_generated_node_helpers/x3dnodes_spotlight.inc}
  end;
  TSpotLightNode_2 = TSpotLightNode;

  TEnvironmentLightNode = class(TAbstractLightNode)
  {$I auto_generated_node_helpers/x3dnodes_environmentlight.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  FallbackProjectionNear = 1;
  FallbackProjectionFar = 100;
  FallbackProjectionRectangle: TVector4 = (X: -10; Y: -10; Z: 10; W: 10);

{ TAbstractLightNode --------------------------------------------------------- }

constructor TAbstractLightNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformation.Init;
end;

procedure TAbstractLightNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
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
      (first we enumerate in TAbstractInternalGroupingNode.BeforeTraverse,
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

    { Note: We do CreateLightInstance with State from TAbstractInternalGroupingNode node,
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
  if (Scene <> nil) and Scene.HasWorldTransform then
    Result := Scene.WorldTransform * Result
  else
    DoWarningWorldTransform;
end;

function TAbstractLightNode.InverseTransform: TMatrix4;
begin
  Result := FTransformation.InverseTransform;
  if (Scene <> nil) and Scene.HasWorldTransform then
    Result := Result * Scene.WorldInverseTransform
  else
    DoWarningWorldTransform;
end;

procedure TAbstractLightNode.DoWarningWorldTransform;
begin
  if not WarningWorldTransformDone then
  begin
    WarningWorldTransformDone := true;
    WritelnWarning(NiceName + ' does not know the world transformation, results (like shadow maps) may be incorrect');
  end;
end;

function TAbstractLightNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcLight;
end;

{ TAbstractPunctualLightNode --------------------------------------------------------- }

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

function TAbstractPunctualLightNode.GetProjectionWorldLocation: TVector3;
begin
  Result := Transform.MultPoint(ProjectionLocationLocal);
end;

procedure TAbstractPunctualLightNode.SetProjectionWorldLocation(const Value: TVector3);
begin
  ProjectionLocationLocal := InverseTransform.MultPoint(Value);
end;

function TAbstractPunctualLightNode.GetProjectionWorldDirection: TVector3;
begin
  Result := Transform.MultDirection(ProjectionDirectionLocal);
end;

procedure TAbstractPunctualLightNode.SetProjectionWorldDirection(const Value: TVector3);
begin
  ProjectionDirectionLocal := InverseTransform.MultDirection(Value);
end;

procedure TAbstractPunctualLightNode.GetView(out Pos, Dir, Side, Up: TVector3);
begin
  Pos := ProjectionWorldLocation;
  Dir := ProjectionWorldDirection;

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
  Dir := Dir.Normalize;
  Up := TVector3.CrossProduct(Side, Dir);
end;

{ TAbstractDirectionalLightNode ---------------------------------------------- }

constructor TAbstractDirectionalLightNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdGlobal.Value := false;
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
  Box.DirectionDistances(ProjectionWorldLocation, ProjectionWorldDirection, MinDistance, MaxDistance);
end;

function TAbstractDirectionalLightNode.PositionAndDirection: TVector4;
begin
  Result := Vector4(-FdDirection.Value, 0);
end;

constructor TAbstractPositionalLightNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdGlobal.Value := true;
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
  Box.PointDistances(ProjectionWorldLocation, MinDistance, MaxDistance);
end;

class function TDirectionalLightNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

class function TPointLightNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
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

function TSpotLightNode.EffectiveProjectionAngle: Single;
begin
  if ProjectionAngle <= 0 then
    Result := 2 * CutOffAngle
  else
    Result := ProjectionAngle;
end;

function TSpotLightNode.ProjectionMatrix: TMatrix4;
var
  Angle, N, F: Single;
begin
  Angle := EffectiveProjectionAngle;

  { If author didn't provide and CastleInternalShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

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
  Box.DirectionDistances(ProjectionWorldLocation, ProjectionWorldDirection, MinDistance, MaxDistance);
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
