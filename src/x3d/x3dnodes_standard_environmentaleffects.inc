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
  { }
  TBackgroundSide = (bsBack, bsBottom, bsFront, bsLeft, bsRight, bsTop);

  { Abstract type from which all backgrounds inherit. }
  TAbstractBackgroundNode = class(TAbstractBindableNode)
  strict private
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;

    { Shaders used to render background geometry. }
    strict private FFdShaders: TMFNode;
    public property FdShaders: TMFNode read FFdShaders;

    { Shader effects (see https://castle-engine.io/compositing_shaders.php)
      used to render background geometry. }
    strict private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    { Extract only rotation (axis-angle format) from the transformation. }
    function TransformRotation: TVector4;

    {$I auto_generated_node_helpers/x3dnodes_x3dbackgroundnode.inc}
  end;

  { Abstract type from which 3D backgrounds inherit. }
  TAbstract3DBackgroundNode = class(TAbstractBackgroundNode)
  public
    procedure CreateNode; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Texture node for given background side.
      May return @nil if none.

      It is the caller's responsibility to take care of freeing the result,
      but only if it's not otherwise used. In other words, use TX3DNode.FreeIfUnused
      (or some routine based on it), unless you're sure that you always
      make the returned node a child of another node (in this case
      the returned node is always used, and remains managed by parent node). }
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; virtual; abstract;

    strict private FFdGroundAngle: TMFFloat;
    public property FdGroundAngle: TMFFloat read FFdGroundAngle;

    strict private FFdGroundColor: TMFColor;
    public property FdGroundColor: TMFColor read FFdGroundColor;

    strict private FFdSkyAngle: TMFFloat;
    public property FdSkyAngle: TMFFloat read FFdSkyAngle;

    strict private FFdSkyColor: TMFColor;
    public property FdSkyColor: TMFColor read FFdSkyColor;

    strict private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    {$I auto_generated_node_helpers/x3dnodes_x3d3dbackgroundnode.inc}
  end;

  TFogTypeOrNone = (ftLinear, ftExp, ftNone);
  TFogType = ftLinear..ftExp;

  { Functionality that describes a fog (blend objects with a fog color).
    Used by various X3D fog nodes, like TLocalFogNode, TFogNode. }
  TFogFunctionality = class(TNodeFunctionality)
  public
    function Color: TCastleColorRGB; virtual; abstract;
    function FogType: TFogType; virtual; abstract;
    function VisibilityRange: Single; virtual; abstract;
    function TransformScale: Single; virtual; abstract;
    function Volumetric: Boolean; virtual; abstract;
    function VolumetricDirection: TVector3; virtual; abstract;
    function VolumetricVisibilityStart: Single; virtual; abstract;
  end;

  { 3D background of a scene, comprised of sky and ground colors (gradients)
    and optional six textures (skybox). }
  TBackgroundNode = class(TAbstract3DBackgroundNode)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; override;

    strict private FFdBackUrl: TMFString;
    public property FdBackUrl: TMFString read FFdBackUrl;

    strict private FFdBottomUrl: TMFString;
    public property FdBottomUrl: TMFString read FFdBottomUrl;

    strict private FFdFrontUrl: TMFString;
    public property FdFrontUrl: TMFString read FFdFrontUrl;

    strict private FFdLeftUrl: TMFString;
    public property FdLeftUrl: TMFString read FFdLeftUrl;

    strict private FFdRightUrl: TMFString;
    public property FdRightUrl: TMFString read FFdRightUrl;

    strict private FFdTopUrl: TMFString;
    public property FdTopUrl: TMFString read FFdTopUrl;

    {$I auto_generated_node_helpers/x3dnodes_background.inc}
  end;

  { Simulate atmospheric fog effects (for the whole scene) by blending
    with the fog colour, based on the distance from the viewer. }
  TFogNode = class(TAbstractBindableNode)
  strict private
    type
      TFogNodeFogFunctionality = class(TFogFunctionality)
      strict private
        FParent: TFogNode;
      public
        constructor Create(const AParent: TFogNode);
        property Parent: TFogNode read FParent;

        function Color: TCastleColorRGB; override;
        function FogType: TFogType; override;
        function VisibilityRange: Single; override;
        function TransformScale: Single; override;
        function Volumetric: Boolean; override;
        function VolumetricDirection: TVector3; override;
        function VolumetricVisibilityStart: Single; override;
      end;

    var
      FogFunctionality: TFogNodeFogFunctionality;

    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    function TransformationChange: TNodeTransformationChange; override;
    function Functionality(const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality; override;

    strict private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    strict private FFdFogType: TSFString;
    public property FdFogType: TSFString read FFdFogType;

    strict private FFdVisibilityRange: TSFFloat;
    public property FdVisibilityRange: TSFFloat read FFdVisibilityRange;

    { Fields below are Kambi's fog extensions, see
      https://castle-engine.io/x3d_extensions.php#ext_fog_volumetric

      @groupBegin }
    strict private FFdVolumetric: TSFBool;
    public property FdVolumetric: TSFBool read FFdVolumetric;

    strict private FFdVolumetricDirection: TSFVec3f;
    public property FdVolumetricDirection: TSFVec3f read FFdVolumetricDirection;

    strict private FFdVolumetricVisibilityStart: TSFFloat;
    public property FdVolumetricVisibilityStart: TSFFloat read FFdVolumetricVisibilityStart;

    strict private FFdAlternative: TSFNode;
    public property FdAlternative: TSFNode read FFdAlternative;
    { @groupEnd }

    { Fog type. }
    function FogType: TFogType;

    { Fog type, but may also return ftNone if we are @nil (checks Self <> nil)
      or visibilityRange = 0. }
    function FogTypeOrNone: TFogTypeOrNone;

    { Apply fog to the color of the vertex.

      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      After all the lighting is summed up (see TX3DGraphTraverseState.Emission and
      TLightInstance.Contribution), process color by this method to apply fog.

      Does nothing if AFogType = ftNone. In this special case, it's also
      allowed to call this on @nil instance. }
    procedure ApplyFog(var Color: TVector3;
      const Position, VertexPos: TVector3; const AFogType: TFogTypeOrNone);

    {$I auto_generated_node_helpers/x3dnodes_fog.inc}
  end;

  { Provide explicit fog depths on a per-vertex basis. }
  TFogCoordinateNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDepth: TMFFloat;
    public property FdDepth: TMFFloat read FFdDepth;

    {$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
  end;

  { Simulate atmospheric fog effects (for a part of the scene) by blending
    with the fog colour, based on the distance from the viewer.
    This is the "local" version of the @link(TFogNode), this affects
    only the sibling nodes of the @code(LocalFog) node, not the whole scene. }
  TLocalFogNode = class(TAbstractChildNode)
  strict private
    type
      TFogNodeFogFunctionality = class(TFogFunctionality)
      strict private
        FParent: TLocalFogNode;
      public
        constructor Create(const AParent: TLocalFogNode);
        property Parent: TLocalFogNode read FParent;

        function Color: TCastleColorRGB; override;
        function FogType: TFogType; override;
        function VisibilityRange: Single; override;
        function TransformScale: Single; override;
        function Volumetric: Boolean; override;
        function VolumetricDirection: TVector3; override;
        function VolumetricVisibilityStart: Single; override;
      end;

    var
      FogFunctionality: TFogNodeFogFunctionality;
      FTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    function Functionality(const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality; override;

    strict private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdFogType: TSFString;
    public property FdFogType: TSFString read FFdFogType;

    strict private FFdVisibilityRange: TSFFloat;
    public property FdVisibilityRange: TSFFloat read FFdVisibilityRange;

    { Fields below are Kambi's fog extensions, see
      https://castle-engine.io/x3d_extensions.php#ext_fog_volumetric

      @groupBegin }
    strict private FFdVolumetric: TSFBool;
    public property FdVolumetric: TSFBool read FFdVolumetric;

    strict private FFdVolumetricDirection: TSFVec3f;
    public property FdVolumetricDirection: TSFVec3f read FFdVolumetricDirection;

    strict private FFdVolumetricVisibilityStart: TSFFloat;
    public property FdVolumetricVisibilityStart: TSFFloat read FFdVolumetricVisibilityStart;
    { @groupEnd }

    function FogType: TFogType;

    {$I auto_generated_node_helpers/x3dnodes_localfog.inc}
  end;

  { 3D background of a scene, comprised of sky and ground colors (gradients)
    and optional six textures (skybox), with flexible texture nodes.
    This is somewhat more flexible version of the @link(TBackgroundNode),
    with this you can use e.g. @link(TMovieTextureNode) as the skybox textures. }
  TTextureBackgroundNode = class(TAbstract3DBackgroundNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; override;

    strict private FFdBackTexture: TSFNode;
    public property FdBackTexture: TSFNode read FFdBackTexture;

    strict private FFdBottomTexture: TSFNode;
    public property FdBottomTexture: TSFNode read FFdBottomTexture;

    strict private FFdFrontTexture: TSFNode;
    public property FdFrontTexture: TSFNode read FFdFrontTexture;

    strict private FFdLeftTexture: TSFNode;
    public property FdLeftTexture: TSFNode read FFdLeftTexture;

    strict private FFdRightTexture: TSFNode;
    public property FdRightTexture: TSFNode read FFdRightTexture;

    strict private FFdTopTexture: TSFNode;
    public property FdTopTexture: TSFNode read FFdTopTexture;

    {$I auto_generated_node_helpers/x3dnodes_texturebackground.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractBackgroundNode ---------------------------------------------------- }

procedure TAbstractBackgroundNode.CreateNode;
begin
  inherited;

  FFdShaders := TMFNode.Create(Self, true, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangeAlways := chEverything;
  AddField(FFdShaders);

  FFdEffects := TMFNode.Create(Self, false, 'effects', [TEffectNode]);
   FdEffects.ChangeAlways := chEverything;
  AddField(FFdEffects);

  DefaultContainerField := 'children';

  Eventset_bind.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BindReceive);
end;

function TAbstractBackgroundNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FFdEffects.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FFdShaders.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TAbstractBackgroundNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
  begin
    Scene.GetBackgroundStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Background is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Background node. }
    Scene.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TAbstractBackgroundNode.TransformRotation: TVector4;
var
  IgnoreTranslation, IgnoreScale: TVector3;
begin
  MatrixDecompose(Transform, IgnoreTranslation, Result, IgnoreScale);
end;

{ TAbstract3DBackgroundNode -------------------------------------------------- }

procedure TAbstract3DBackgroundNode.CreateNode;
begin
  inherited;

  FFdGroundAngle := TMFFloat.Create(Self, true, 'groundAngle', []);
   FdGroundAngle.Angle := true;
   FdGroundAngle.ChangeAlways := chBackground;
  AddField(FFdGroundAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdGroundColor := TMFColor.Create(Self, true, 'groundColor', []);
   FdGroundColor.ChangeAlways := chBackground;
  AddField(FFdGroundColor);
  { X3D specification comment: [0,1] }

  FFdSkyAngle := TMFFloat.Create(Self, true, 'skyAngle', []);
   FdSkyAngle.Angle := true;
   FdSkyAngle.ChangeAlways := chBackground;
  AddField(FFdSkyAngle);
  { X3D specification comment: [0,Pi] }

  FFdSkyColor := TMFColor.Create(Self, true, 'skyColor', Vector3(0, 0, 0));
   FdSkyColor.ChangeAlways := chBackground;
  AddField(FFdSkyColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, true, 'transparency', 0);
   FdTransparency.ChangeAlways := chBackground;
  AddField(FFdTransparency);
  { X3D specification comment: [0,1] }
end;

function TAbstract3DBackgroundNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcBackground;
end;

{ TBackgroundNode ------------------------------------------------------------ }

procedure TBackgroundNode.CreateNode;
begin
  inherited;

  { TODO: changing XxxUrl should only cause reloading of the Textures
    (even better: only of the appropriate background side),
    and then chBackground (to create need display list for rendering background).
    Current chEverything is very unoptimal. }

  FFdBackUrl := TMFString.Create(Self, true, 'backUrl', []);
   FdBackUrl.ChangeAlways := chEverything;
  AddField(FFdBackUrl);
  { X3D specification comment: [URI] }

  FFdBottomUrl := TMFString.Create(Self, true, 'bottomUrl', []);
   FdBottomUrl.ChangeAlways := chEverything;
  AddField(FFdBottomUrl);
  { X3D specification comment: [URI] }

  FFdFrontUrl := TMFString.Create(Self, true, 'frontUrl', []);
   FdFrontUrl.ChangeAlways := chEverything;
  AddField(FFdFrontUrl);
  { X3D specification comment: [URI] }

  FFdLeftUrl := TMFString.Create(Self, true, 'leftUrl', []);
   FdLeftUrl.ChangeAlways := chEverything;
  AddField(FFdLeftUrl);
  { X3D specification comment: [URI] }

  FFdRightUrl := TMFString.Create(Self, true, 'rightUrl', []);
   FdRightUrl.ChangeAlways := chEverything;
  AddField(FFdRightUrl);
  { X3D specification comment: [URI] }

  FFdTopUrl := TMFString.Create(Self, true, 'topUrl', []);
   FdTopUrl.ChangeAlways := chEverything;
  AddField(FFdTopUrl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

class function TBackgroundNode.ClassX3DType: string;
begin
  Result := 'Background';
end;

function TBackgroundNode.Texture(const Side: TBackgroundSide): TAbstractTextureNode;
var
  TextureField: TMFString;
  ResultImgTex: TImageTextureNode;
begin
  case Side of
    bsBack  : TextureField := FdBackUrl;
    bsBottom: TextureField := FdBottomUrl;
    bsFront : TextureField := FdFrontUrl;
    bsLeft  : TextureField := FdLeftUrl;
    bsRight : TextureField := FdRightUrl;
    bsTop   : TextureField := FdTopUrl;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TTextureBackgroundNode.Texture:Side?');
    {$endif}
  end;
  if TextureField.Items.Count <> 0 then
  begin
    ResultImgTex := TImageTextureNode.Create('', BaseUrl);
    ResultImgTex.FdUrl.Assign(TextureField);
    Result := ResultImgTex;
  end else
    Result := nil;
end;

{ TFogNode.TFogNodeFogFunctionality ------------------------------------------ }

constructor TFogNode.TFogNodeFogFunctionality.Create(const AParent: TFogNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

function TFogNode.TFogNodeFogFunctionality.Color: TCastleColorRGB;
begin
  Result := Parent.Color;
end;

function TFogNode.TFogNodeFogFunctionality.FogType: TFogType;
begin
  Result := Parent.FogType;
end;

function TFogNode.TFogNodeFogFunctionality.VisibilityRange: Single;
begin
  Result := Parent.VisibilityRange;
end;

function TFogNode.TFogNodeFogFunctionality.TransformScale: Single;
begin
  Result := Parent.TransformScale;
end;

function TFogNode.TFogNodeFogFunctionality.Volumetric: Boolean;
begin
  Result := Parent.Volumetric;
end;

function TFogNode.TFogNodeFogFunctionality.VolumetricDirection: TVector3;
begin
  Result := Parent.VolumetricDirection;
end;

function TFogNode.TFogNodeFogFunctionality.VolumetricVisibilityStart: Single;
begin
  Result := Parent.VolumetricVisibilityStart;
end;

{ TFogNode ------------------------------------------------------------------- }

procedure TFogNode.CreateNode;
begin
  inherited;

  FFdColor := TSFColor.Create(Self, true, 'color', Vector3(1, 1, 1));
   FdColor.ChangeAlways := chRedisplay;
  AddField(FFdColor);
  { X3D specification comment: [0,1] }

  FFdFogType := TSFString.Create(Self, true, 'fogType', 'LINEAR');
   FdFogType.ChangeAlways := chRedisplay;
  AddField(FFdFogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdVisibilityRange := TSFFloat.Create(Self, true, 'visibilityRange', 0);
   FdVisibilityRange.ChangeAlways := chRedisplay;
  AddField(FFdVisibilityRange);
  { X3D specification comment: [0,Inf) }

  FFdVolumetric := TSFBool.Create(Self, true, 'volumetric', false);
   FdVolumetric.ChangeAlways := chRedisplay;
  AddField(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, true, 'volumetricDirection', Vector3(0, -1, 0));
   FdVolumetricDirection.ChangeAlways := chRedisplay;
  AddField(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, true, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangeAlways := chRedisplay;
  AddField(FFdVolumetricVisibilityStart);

  { Only for backward compatibility. Not used anymore. }
  FFdAlternative := TSFNode.Create(Self, true, 'alternative', [TFogNode]);
   FdAlternative.ChangeAlways := chNone;
  AddField(FFdAlternative);

  DefaultContainerField := 'children';

  Eventset_bind.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BindReceive);

  FogFunctionality := TFogNode.TFogNodeFogFunctionality.Create(Self);
end;

destructor TFogNode.Destroy;
begin
  FreeAndNil(FogFunctionality);
  inherited;
end;

class function TFogNode.ClassX3DType: string;
begin
  Result := 'Fog';
end;

procedure TFogNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
  begin
    Scene.GetFogStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Fog is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Fog node. }
    Scene.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TFogNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcFog;
end;

function TFogNode.FogType: TFogType;
begin
  if FdFogType.Value = 'LINEAR' then
    Result := ftLinear else
  if FdFogType.Value = 'EXPONENTIAL' then
    Result := ftExp else
  begin
    WritelnWarning('VRML/X3D', 'Unknown fog type "' + FdFogType.Value + '"');
    Result := ftLinear;
  end;
end;

function TFogNode.FogTypeOrNone: TFogTypeOrNone;
begin
  if (Self = nil) or
     (FdVisibilityRange.Value = 0.0) then
    Exit(ftNone);

  Result := FogType;
end;

procedure TFogNode.ApplyFog(var Color: TVector3;
  const Position, VertexPos: TVector3; const AFogType: TFogTypeOrNone);
var
  FogVisibilityRangeScaled: Single;

  procedure ApplyDistance(const Distance: Single);
  var
    F: Single;
  begin
    case AFogType of
      ftLinear: F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      ftExp   : F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
      ftNone  : F := 1;
    end;
    Color := Lerp(F, FdColor.Value, Color);
  end;

var
  FogVolumetricVisibilityStart: Single;
  Distance, DistanceSqr: Single;
  VertProjected: TVector3;
begin
  if AFogType <> ftNone then
  begin
    FogVisibilityRangeScaled := FdVisibilityRange.Value * TransformScale;

    if FdVolumetric.Value then
    begin
      FogVolumetricVisibilityStart := FdVolumetricVisibilityStart.Value * TransformScale;

      { Calculation of Distance for volumetric fog below is analogous to
        GetFogVolumetric inside ArraysGenerator. }

      VertProjected := PointOnLineClosestToPoint(
        TVector3.Zero, FdVolumetricDirection.Value, VertexPos);
      Distance := VertProjected.Length;
      if not AreParallelVectorsSameDirection(
        VertProjected, FdVolumetricDirection.Value) then
        Distance := -Distance;
      { Now I want
        - Distance = FogVolumetricVisibilityStart -> 0
        - Distance = FogVolumetricVisibilityStart + X -> X
          (so that Distance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      Distance := Distance - FogVolumetricVisibilityStart;

      { When Distance < 0 our intention is to have no fog.
        So Distance < 0 should be equivalent to Distance = 0. }
      MaxVar(Distance, 0);

      if Distance >= FogVisibilityRangeScaled - SingleEpsilon then
        Color := FdColor.Value else
        ApplyDistance(Distance);
    end else
    begin
      DistanceSqr := PointsDistanceSqr(Position, VertexPos);

      if DistanceSqr >= Sqr(FogVisibilityRangeScaled - SingleEpsilon) then
        Color := FdColor.Value else
        ApplyDistance(Sqrt(DistanceSqr));

    end;
  end;
end;

function TFogNode.Functionality(
  const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality;
begin
  if FogFunctionality.InheritsFrom(FunctionalityClass) then
    Result := FogFunctionality
  else
    Result := nil;
end;

{ TFogCoordinateNode --------------------------------------------------------- }

procedure TFogCoordinateNode.CreateNode;
begin
  inherited;

  FFdDepth := TMFFloat.Create(Self, true, 'depth', []);
  AddField(FFdDepth);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'fogCoord';
end;

class function TFogCoordinateNode.ClassX3DType: string;
begin
  Result := 'FogCoordinate';
end;

{ TLocalFogNode.TFogNodeFogFunctionality ------------------------------------------ }

constructor TLocalFogNode.TFogNodeFogFunctionality.Create(const AParent: TLocalFogNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

function TLocalFogNode.TFogNodeFogFunctionality.Color: TCastleColorRGB;
begin
  Result := Parent.Color;
end;

function TLocalFogNode.TFogNodeFogFunctionality.FogType: TFogType;
begin
  Result := Parent.FogType;
end;

function TLocalFogNode.TFogNodeFogFunctionality.VisibilityRange: Single;
begin
  Result := Parent.VisibilityRange;
end;

function TLocalFogNode.TFogNodeFogFunctionality.TransformScale: Single;
begin
  { TODO: this is actually bad, each LocalFog occurrence may have
    different scale, depending on parent transform node.
    We should keep LocalFogScale in State, and pass it around. }
  Result := Parent.FTransformScale;
end;

function TLocalFogNode.TFogNodeFogFunctionality.Volumetric: Boolean;
begin
  Result := Parent.Volumetric;
end;

function TLocalFogNode.TFogNodeFogFunctionality.VolumetricDirection: TVector3;
begin
  Result := Parent.VolumetricDirection;
end;

function TLocalFogNode.TFogNodeFogFunctionality.VolumetricVisibilityStart: Single;
begin
  Result := Parent.VolumetricVisibilityStart;
end;

{ TLocalFogNode -------------------------------------------------------------- }

procedure TLocalFogNode.CreateNode;
begin
  inherited;

  FFdColor := TSFColor.Create(Self, true, 'color', Vector3(1, 1, 1));
  AddField(FFdColor);
  { X3D specification comment: [0,1] }

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdFogType := TSFString.Create(Self, true, 'fogType', 'LINEAR');
  AddField(FFdFogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdVisibilityRange := TSFFloat.Create(Self, true, 'visibilityRange', 0);
  AddField(FFdVisibilityRange);
  { X3D specification comment: [0,-Inf) }

  FFdVolumetric := TSFBool.Create(Self, true, 'volumetric', false);
   FdVolumetric.ChangeAlways := chEverything;
  AddField(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, true, 'volumetricDirection', Vector3(0, -1, 0));
   FdVolumetricDirection.ChangeAlways := chEverything;
  AddField(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, true, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangeAlways := chEverything;
  AddField(FFdVolumetricVisibilityStart);

  DefaultContainerField := 'children';

  FTransformScale := 1;

  FogFunctionality := TLocalFogNode.TFogNodeFogFunctionality.Create(Self);
end;

destructor TLocalFogNode.Destroy;
begin
  FreeAndNil(FogFunctionality);
  inherited;
end;

procedure TLocalFogNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  FTransformScale := StateStack.Top.Transformation.Scale;
end;

procedure TLocalFogNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  inherited;
  State.LocalFog := Self;
end;

class function TLocalFogNode.ClassX3DType: string;
begin
  Result := 'LocalFog';
end;

function TLocalFogNode.FogType: TFogType;
begin
  if FdFogType.Value = 'LINEAR' then
    Result := ftLinear else
  if FdFogType.Value = 'EXPONENTIAL' then
    Result := ftExp else
  begin
    WritelnWarning('VRML/X3D', 'Unknown fog type "' + FdFogType.Value + '"');
    Result := ftLinear;
  end;
end;

function TLocalFogNode.Functionality(
  const FunctionalityClass: TNodeFunctionalityClass): TNodeFunctionality;
begin
  if FogFunctionality.InheritsFrom(FunctionalityClass) then
    Result := FogFunctionality
  else
    Result := nil;
end;

{ TTextureBackgroundNode ----------------------------------------------------- }

procedure TTextureBackgroundNode.CreateNode;
begin
  inherited;

  FFdBackTexture := TSFNode.Create(Self, true, 'backTexture', [TAbstractTextureNode]);
  AddField(FFdBackTexture);

  FFdBottomTexture := TSFNode.Create(Self, true, 'bottomTexture', [TAbstractTextureNode]);
  AddField(FFdBottomTexture);

  FFdFrontTexture := TSFNode.Create(Self, true, 'frontTexture', [TAbstractTextureNode]);
  AddField(FFdFrontTexture);

  FFdLeftTexture := TSFNode.Create(Self, true, 'leftTexture', [TAbstractTextureNode]);
  AddField(FFdLeftTexture);

  FFdRightTexture := TSFNode.Create(Self, true, 'rightTexture', [TAbstractTextureNode]);
  AddField(FFdRightTexture);

  FFdTopTexture := TSFNode.Create(Self, true, 'topTexture', [TAbstractTextureNode]);
  AddField(FFdTopTexture);

  DefaultContainerField := 'children';
end;

function TTextureBackgroundNode.Texture(const Side: TBackgroundSide): TAbstractTextureNode;
var
  TextureField: TSFNode;
begin
  case Side of
    bsBack  : TextureField := FdBackTexture;
    bsBottom: TextureField := FdBottomTexture;
    bsFront : TextureField := FdFrontTexture;
    bsLeft  : TextureField := FdLeftTexture;
    bsRight : TextureField := FdRightTexture;
    bsTop   : TextureField := FdTopTexture;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TTextureBackgroundNode.Texture:Side?');
    {$endif}
  end;
  if TextureField.Value is TAbstractTextureNode then
    Result := TAbstractTextureNode(TextureField.Value) else
    Result := nil;
end;

class function TTextureBackgroundNode.ClassX3DType: string;
begin
  Result := 'TextureBackground';
end;

{ registration --------------------------------------------------------------- }

procedure RegisterEnvironmentalEffectsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBackgroundNode,
    TFogNode,
    TFogCoordinateNode,
    TLocalFogNode,
    TTextureBackgroundNode
  ]);
end;

{$endif read_implementation}
