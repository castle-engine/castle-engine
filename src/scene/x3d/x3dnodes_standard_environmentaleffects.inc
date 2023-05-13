{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Side of the background (skybox), used with TBackgroundNode. }
  TBackgroundSide = (bsBack, bsBottom, bsFront, bsLeft, bsRight, bsTop);

  { Abstract type from which all backgrounds inherit. }
  TAbstractBackgroundNode = class(TAbstractBindableNode)
  strict private
    procedure EventSet_BindReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    { Extract only rotation (axis-angle format) from the transformation. }
    function TransformRotation: TVector4;

  {$I auto_generated_node_helpers/x3dnodes_x3dbackgroundnode.inc}
  end;

  { Abstract type from which 3D backgrounds inherit. }
  TAbstract3DBackgroundNode = class(TAbstractBackgroundNode)
  public
    function TransformationChange: TNodeTransformationChange; override;

    { Texture node for given background side.
      May return @nil if none.

      It is the caller's responsibility to take care of freeing the result,
      but only if it's not otherwise used. In other words, use TX3DNode.FreeIfUnused
      (or some routine based on it), unless you're sure that you always
      make the returned node a child of another node (in this case
      the returned node is always used, and remains managed by parent node). }
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; virtual; abstract;

  {$I auto_generated_node_helpers/x3dnodes_x3d3dbackgroundnode.inc}
  end;

  TFogTypeOrNone = (ftLinear, ftExponential, ftNone);
  TFogType = ftLinear..ftExponential;

  { Functionality of node that describes a fog (blend objects with a fog color).
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
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; override;
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

    procedure EventSet_BindReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    function TransformationChange: TNodeTransformationChange; override;

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
  {$I auto_generated_node_helpers/x3dnodes_fogcoordinate.inc}
  end;

  { Simulate atmospheric fog effects (for a part of the scene) by blending
    with the fog colour, based on the distance from the viewer.
    This is the "local" version of the @link(TFogNode), this affects
    only the sibling nodes of the @code(LocalFog) node, not the whole scene. }
  TLocalFogNode = class(TAbstractChildNode)
  strict private
    type
      TLocalFogNodeFogFunctionality = class(TFogFunctionality)
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
      FTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

  {$I auto_generated_node_helpers/x3dnodes_localfog.inc}
  end;

  { 3D background of a scene, comprised of sky and ground colors (gradients)
    and optional six textures (skybox), with flexible texture nodes.
    This is somewhat more flexible version of the @link(TBackgroundNode),
    with this you can use e.g. @link(TMovieTextureNode) as the skybox textures. }
  TTextureBackgroundNode = class(TAbstract3DBackgroundNode)
  public
    function Texture(const Side: TBackgroundSide): TAbstractTextureNode; override;
  {$I auto_generated_node_helpers/x3dnodes_texturebackground.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractBackgroundNode ---------------------------------------------------- }

constructor TAbstractBackgroundNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_bind.AddNotification({$ifdef FPC}@{$endif} EventSet_BindReceive);
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
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
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

function TAbstract3DBackgroundNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcBackground;
end;

{ TBackgroundNode ------------------------------------------------------------ }

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

const
  FogTypeNames: array [TFogType] of String = ('LINEAR', 'EXPONENTIAL');

constructor TFogNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_bind.AddNotification({$ifdef FPC}@{$endif} EventSet_BindReceive);
  AddFunctionality(TFogNode.TFogNodeFogFunctionality.Create(Self));
end;

procedure TFogNode.EventSet_BindReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
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

function TFogNode.FogTypeOrNone: TFogTypeOrNone;
begin
  if (Self = nil) or
     (VisibilityRange = 0.0) then
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
      ftLinear     : F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      ftExponential: F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
      ftNone       : F := 1;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('AFogType?');
      {$endif}
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

{ TLocalFogNode.TLocalFogNodeFogFunctionality ------------------------------------------ }

constructor TLocalFogNode.TLocalFogNodeFogFunctionality.Create(const AParent: TLocalFogNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.Color: TCastleColorRGB;
begin
  Result := Parent.Color;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.FogType: TFogType;
begin
  Result := Parent.FogType;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.VisibilityRange: Single;
begin
  Result := Parent.VisibilityRange;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.TransformScale: Single;
begin
  { TODO: this is actually bad, each LocalFog occurrence may have
    different scale, depending on parent transform node.
    We should keep LocalFogScale in State, and pass it around. }
  Result := Parent.FTransformScale;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.Volumetric: Boolean;
begin
  Result := Parent.Volumetric;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.VolumetricDirection: TVector3;
begin
  Result := Parent.VolumetricDirection;
end;

function TLocalFogNode.TLocalFogNodeFogFunctionality.VolumetricVisibilityStart: Single;
begin
  Result := Parent.VolumetricVisibilityStart;
end;

{ TLocalFogNode -------------------------------------------------------------- }

constructor TLocalFogNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TLocalFogNode.TLocalFogNodeFogFunctionality.Create(Self));
  FTransformScale := 1;
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

{ TTextureBackgroundNode ----------------------------------------------------- }

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
