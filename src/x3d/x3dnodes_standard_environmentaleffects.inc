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
    strict private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

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

  { Interface that describes a node that influences the look by blending with fog. }
  IAbstractFogObject = interface(IX3DNode)
  ['{F5C4E0A9-2214-4E3F-8E90-1E0305B2EBC6}']
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;

    property FdColor: TSFColor read GetFdcolor;
    property FdFogType: TSFString read GetFdfogType;
    property FdVisibilityRange: TSFFloat read GetFdvisibilityRange;
    property FdVolumetric: TSFBool read GetFdvolumetric;
    property FdVolumetricDirection: TSFVec3f read GetFdvolumetricDirection;
    property FdVolumetricVisibilityStart: TSFFloat read GetFdvolumetricVisibilityStart;

    function GetTransformScale: Single;
    property TransformScale: Single read GetTransformScale;

    function FogType: TFogType;
  end;

  { 3D background of a scene, comprised of sky and ground colors (gradients)
    and optional six textures (skybox). }
  TBackgroundNode = class(TAbstract3DBackgroundNode)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
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
  TFogNode = class(TAbstractBindableNode, IAbstractFogObject)
  strict private
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;
    function GetTransformScale: Single;
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

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
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdDepth: TMFFloat;
    public property FdDepth: TMFFloat read FFdDepth;

    {$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
  end;

  { Simulate atmospheric fog effects (for a part of the scene) by blending
    with the fog colour, based on the distance from the viewer.
    This is the "local" version of the @link(TFogNode), this affects
    only the sibling nodes of the @code(LocalFog) node, not the whole scene. }
  TLocalFogNode = class(TAbstractChildNode, IAbstractFogObject)
  strict private
    FTransformScale: Single;
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;
    function GetTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

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
    class function URNMatching(const URN: string): boolean; override;
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

  FFdShaders := TMFNodeShaders.Create(Self, true, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  AddField(FFdShaders);

  FFdEffects := TMFNode.Create(Self, false, 'effects', [TEffectNode]);
   FdEffects.ChangesAlways := [chEverything];
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
   FdGroundAngle.ChangesAlways := [chBackground];
  AddField(FFdGroundAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdGroundColor := TMFColor.Create(Self, true, 'groundColor', []);
   FdGroundColor.ChangesAlways := [chBackground];
  AddField(FFdGroundColor);
  { X3D specification comment: [0,1] }

  FFdSkyAngle := TMFFloat.Create(Self, true, 'skyAngle', []);
   FdSkyAngle.Angle := true;
   FdSkyAngle.ChangesAlways := [chBackground];
  AddField(FFdSkyAngle);
  { X3D specification comment: [0,Pi] }

  FFdSkyColor := TMFColor.Create(Self, true, 'skyColor', Vector3(0, 0, 0));
   FdSkyColor.ChangesAlways := [chBackground];
  AddField(FFdSkyColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, true, 'transparency', 0);
   FdTransparency.ChangesAlways := [chBackground];
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
   FdBackUrl.ChangesAlways := [chEverything];
  AddField(FFdBackUrl);
  { X3D specification comment: [URI] }

  FFdBottomUrl := TMFString.Create(Self, true, 'bottomUrl', []);
   FdBottomUrl.ChangesAlways := [chEverything];
  AddField(FFdBottomUrl);
  { X3D specification comment: [URI] }

  FFdFrontUrl := TMFString.Create(Self, true, 'frontUrl', []);
   FdFrontUrl.ChangesAlways := [chEverything];
  AddField(FFdFrontUrl);
  { X3D specification comment: [URI] }

  FFdLeftUrl := TMFString.Create(Self, true, 'leftUrl', []);
   FdLeftUrl.ChangesAlways := [chEverything];
  AddField(FFdLeftUrl);
  { X3D specification comment: [URI] }

  FFdRightUrl := TMFString.Create(Self, true, 'rightUrl', []);
   FdRightUrl.ChangesAlways := [chEverything];
  AddField(FFdRightUrl);
  { X3D specification comment: [URI] }

  FFdTopUrl := TMFString.Create(Self, true, 'topUrl', []);
   FdTopUrl.ChangesAlways := [chEverything];
  AddField(FFdTopUrl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

class function TBackgroundNode.ClassX3DType: string;
begin
  Result := 'Background';
end;

class function TBackgroundNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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
    else raise EInternalError.Create('TTextureBackgroundNode.Texture:Side?');
  end;
  if TextureField.Items.Count <> 0 then
  begin
    ResultImgTex := TImageTextureNode.Create('', BaseUrl);
    ResultImgTex.FdUrl.Assign(TextureField);
    Result := ResultImgTex;
  end else
    Result := nil;
end;

{ TFogNode ------------------------------------------------------------------- }

procedure TFogNode.CreateNode;
begin
  inherited;

  { TODO: this is very unoptimal.
    With new renderer, we possible don't have to do *anything*
    when fog property changes, they will be automatically used? }

  FFdColor := TSFColor.Create(Self, true, 'color', Vector3(1, 1, 1));
   FdColor.ChangesAlways := [chEverything];
  AddField(FFdColor);
  { X3D specification comment: [0,1] }

  FFdFogType := TSFString.Create(Self, true, 'fogType', 'LINEAR');
   FdFogType.ChangesAlways := [chEverything];
  AddField(FFdFogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdVisibilityRange := TSFFloat.Create(Self, true, 'visibilityRange', 0);
   FdVisibilityRange.ChangesAlways := [chEverything];
  AddField(FFdVisibilityRange);
  { X3D specification comment: [0,Inf) }

  FFdVolumetric := TSFBool.Create(Self, true, 'volumetric', false);
   FdVolumetric.ChangesAlways := [chEverything];
  AddField(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, true, 'volumetricDirection', Vector3(0, -1, 0));
   FdVolumetricDirection.ChangesAlways := [chEverything];
  AddField(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, true, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangesAlways := [chEverything];
  AddField(FFdVolumetricVisibilityStart);

  { Only for backward compatibility. Not used anymore. }
  FFdAlternative := TSFNode.Create(Self, true, 'alternative', [TFogNode]);
   FdAlternative.ChangesAlways := [chEverything];
  AddField(FFdAlternative);

  DefaultContainerField := 'children';

  Eventset_bind.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BindReceive);
end;

class function TFogNode.ClassX3DType: string;
begin
  Result := 'Fog';
end;

class function TFogNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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

function TFogNode.GetFdcolor: TSFColor;
begin
  Result := FFdColor;
end;

function TFogNode.GetFdfogType: TSFString;
begin
  Result := FdFogType;
end;

function TFogNode.GetFdvisibilityRange: TSFFloat;
begin
  Result := FdVisibilityRange;
end;

function TFogNode.GetFdvolumetric: TSFBool;
begin
  Result := FFdVolumetric;
end;

function TFogNode.GetFdvolumetricDirection: TSFVec3f;
begin
  Result := FFdVolumetricDirection;
end;

function TFogNode.GetFdvolumetricVisibilityStart: TSFFloat;
begin
  Result := FFdVolumetricVisibilityStart;
end;

function TFogNode.GetTransformScale: Single;
begin
  Result := TransformScale;
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

class function TFogCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
   FdVolumetric.ChangesAlways := [chEverything];
  AddField(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, true, 'volumetricDirection', Vector3(0, -1, 0));
   FdVolumetricDirection.ChangesAlways := [chEverything];
  AddField(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, true, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangesAlways := [chEverything];
  AddField(FFdVolumetricVisibilityStart);

  DefaultContainerField := 'children';

  FTransformScale := 1;
end;

procedure TLocalFogNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  FTransformScale := StateStack.Top.TransformScale;
end;

class function TLocalFogNode.ClassX3DType: string;
begin
  Result := 'LocalFog';
end;

class function TLocalFogNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TLocalFogNode.GetFdcolor: TSFColor;
begin
  Result := FFdColor;
end;

function TLocalFogNode.GetFdfogType: TSFString;
begin
  Result := FdFogType;
end;

function TLocalFogNode.GetFdvisibilityRange: TSFFloat;
begin
  Result := FdVisibilityRange;
end;

function TLocalFogNode.GetFdvolumetric: TSFBool;
begin
  Result := FFdVolumetric;
end;

function TLocalFogNode.GetFdvolumetricDirection: TSFVec3f;
begin
  Result := FFdVolumetricDirection;
end;

function TLocalFogNode.GetFdvolumetricVisibilityStart: TSFFloat;
begin
  Result := FFdVolumetricVisibilityStart;
end;

function TLocalFogNode.GetTransformScale: Single;
begin
  { TODO: this is actually bad, each LocalFog occurrence may have
    different scale, depending on parent transform node.
    We should keep LocalFogScale in State, and pass it along
    to RenderShape. This method should not exist. }
  Result := FTransformScale;
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
    else raise EInternalError.Create('TTextureBackgroundNode.Texture:Side?');
  end;
  if TextureField.Value is TAbstractTextureNode then
    Result := TAbstractTextureNode(TextureField.Value) else
    Result := nil;
end;

class function TTextureBackgroundNode.ClassX3DType: string;
begin
  Result := 'TextureBackground';
end;

class function TTextureBackgroundNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
