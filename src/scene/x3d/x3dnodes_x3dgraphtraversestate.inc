{%MainUnit x3dnodes.pas}
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
  { Current state (transformation and such) when traversing VRML/X3D graph.

    For VRML/X3D >= 2.0 this could be simpler, as VRML/X3D >= 2.0 doesn't need
    to keep track for example of the @link(VRML1State).
    But we want to still handle VRML 1.0, 100% correctly, so here we are:
    this class contains whole state needed for any VRML/X3D version. }
  TX3DGraphTraverseState = class
  strict private
    FVRML1State: TVRML1State;
    procedure CommonCreate;
  public
    { Nodes that affect how following nodes are rendered,
      mostly for VRML 1.0 state. }
    property VRML1State: TVRML1State read FVRML1State;
    {$ifdef FPC}
    property LastNodes: TVRML1State read FVRML1State; deprecated 'use VRML1State';
    {$endif}
  public
    { Lights active in this state.

      May be @nil if empty. This way we optimize creation / assignment time,
      which happen very often with TX3DGraphTraverseState during VRML/X3D
      traversing.

      Note that VRML >= 2.0 "global" lights are added from TCastleSceneCore,
      not during the traverse pass. }
    Lights: TLightInstancesList;

    procedure AddLight(const Light: TLightInstance);
  public
    { Transformation of this state.
      Calculated efficiently while traversing the X3D graph. }
    Transformation: TTransformation;

    { Copy transformation-related fields from Source.
      Copies @link(Transformation).
      Copies also the @link(ClipPlanes) list, as it contains the transformation information. }
    procedure AssignTransform(Source: TX3DGraphTraverseState);

    function Transform: TMatrix4; deprecated 'use Transformation.Transform';
    function InvertedTransform: TMatrix4; deprecated 'use Transformation.InverseTransform';

  public
    { Current texture transformation. Usable only for VRML 1.0, in VRML 2.0
      texture transformations don't accumulate like modelview transformations. }
    TextureTransform: TMatrix4;

    ShapeNode: TAbstractShapeNode;

    constructor CreateCopy(Source: TX3DGraphTraverseState);

    { Standard constructor.
      Uses global VRML1DefaultState as default nodes for VRML1State.
      This makes it fast, and improves cache (more nodes have equal reference). }
    constructor Create;

    destructor Destroy; override;

    procedure Assign(Source: TX3DGraphTraverseState);

    { Clear the whole state, just like this TX3DGraphTraverseState instance
      would be just constructed. }
    procedure Clear;

    { Compare with other TX3DGraphTraverseState instance.
      True if these two states, when applied to the same geometry,
      result in the same TGeometryArrays output.
      If IgnoreTransform then we should ignore transformation during comparison
      (it means that renderer is absolutely sure that different transformation
      of geometry doesn't affect the generated arrays). }
    function Equals(SecondValue: TX3DGraphTraverseState;
      const IgnoreTransform: boolean): boolean; {$ifdef TOBJECT_HAS_EQUALS} reintroduce; {$endif}

    { Primary texture that determines RGB color and opacity.
      See @link(TAppearanceNode.MainTexture) for definition. }
    function MainTexture: TAbstractTextureNode; overload;
    function MainTexture(const GeometryForTexCoord: TAbstractGeometryNode;
      out TexCoordIndex: Integer): TAbstractTextureNode; overload;

    { Returns BlendMode for this state, or @nil if not present. }
    function BlendMode: TBlendModeNode;

  public
    { Information if you're within any inline node or expanded prototype.
      InsideInline = 0 means you're not inside any inline node,
      1 means you're inside one inline, 2 means you're within content
      inlined from yet another inline node, and so on.
      Analogous for InsidePrototype.

      These are measured from the node where you
      started TX3DNode.Traverse call, that is they assume that the initial
      node from where you're traversing is at level 0 (not inside inline
      or expanded prototype).

      These are useful to establish "run-time name scope" of X3D,
      see X3D spec 4.4.7 (needed e.g. when handling Anchor node with
      "#Viewpoint" URL).
      Interpreting this for our implementation,
      specification says that if you traverse
      from node X, then all traversed nodes with
      InsideInline = InsidePrototype = 0 are within the same name scope.

      Also this is useful for searching for the first bindable node after
      loading the file. Specification says to ignore inline content
      in this case (although prototype content is Ok in this case).

      When scriping will be implemented, probably analogous
      InsideScriptCreatedNode will also be needed, as the spec says
      that bindable nodes should not be searched within things like
      "Browser.createX3DFromString()".

      @groupBegin }
    InsideInline: Cardinal;
    InsidePrototype: Cardinal;
    { @groupEnd }

    { This is > 0 when traversing nodes that do not participate in
      collision detection.

      This counts how many times are we inside Collision node that
      prevents us from colliding.
      More precise, this is increased when we traverse inside
      Collision.children with Collision.enabled = FALSE or
      Collision.proxy <> NULL (since then Collision.children are
      not collidable). }
    InsideIgnoreCollision: Cardinal;

    { This is > 0 when traversing nodes that are not visible.

      This counts how many times are we inside Collision.proxy
      (with Collision.enabled = TRUE). Collision.proxy is never
      visible. }
    InsideInvisible: Cardinal;

    { Active pointing device sensors in this state.
      This can contain only nodes descending from
      X3DPointingDeviceSensorNode, and additionally an Anchor node.

      This list automatically honours VRML / X3D rules for what
      pointing device sensor is active: pointing device within some
      group node affects all children in this group node.
      (And when multiple pointing device sensors are within the same
      grouping node, they all work.) }
    PointingDeviceSensors: TPointingDeviceSensorList;

    { For Humanoid skeleton, these contain cummulated joint transformation. }
    HumanoidTransformation: TTransformation;
    { Humanoid node containing us, or @nil if none. }
    Humanoid: THAnimHumanoidNode;

    { ClipPlanes affecting nodes within this state.

      They are collected here regardless of whether they are enabled or not.
      This allows efficient implementation of @code(ClipPlane.enabled)
      dynamic changes.

      Ordered from the most global to most local ones.
      So, following the X3D specification, we should consider the first
      clip planes on this list more important.

      Always @nil if empty. This allows us to optimize TX3DGraphTraverseState
      processing. }
    ClipPlanes: TClipPlaneList;

    { Local fog settings. When @nil, it means use global fog (or no fog,
      if no global fog defined in file). }
    LocalFog: TLocalFogNode;

    { Effects (TEffectNode) affecting this state. }
    Effects: TX3DNodeList;

    function AddClipPlane: PClipPlane;

    { Material information at this state.
      See TMaterialInfo for usage description.
      Returns @nil when no node determines material properties
      (which indicates white unlit look).

      Returned TMaterialInfo is valid only as long as the underlying
      Material or CommonSurfaceShader node exists.
      Do not free it yourself, it will be automatically freed. }
    function MaterialInfo: TMaterialInfo;
  end;

  { Stack of TX3DGraphTraverseState.

    Allows you for much faster
    creation/destruction of TX3DGraphTraverseState instances.
    Although you can always construct / destruct TX3DGraphTraverseState
    as normal objects, in some cases this is too slow: when traversing VRML/X3D graph
    (e.g. profile animate_3d_model_by_code_2), merely
    creating/destroying TX3DGraphTraverseState instances takes a noticeable
    amount of time.

    This stack allows you to do this faster, first of all by
    internally using a prepared pool of instances.

    Each PushClear call creates a clear state instance, and places
    it on the stack.
    Each Push call creates a copy of current top and places it on the stack.
    Each Pop removes and destroys the last instance added by Push.

    Naturally, you can call Push and Top only when the stack is not empty.
    In practice, using the stack always starts in TX3DNode.Traverse,
    where we push initial clear state. So the stack passed to various
    callbacks, TX3DNode.BeforeTraverse and such is always guaranteed non-empty.

    Note that for speed purposes all Traverse calls actually
    share a single stack. That is,
    to avoid creating TX3DGraphTraverseStateStack instance each time
    (because even creating TX3DGraphTraverseStateStack
    takes some time (as it prepares a pool of TX3DGraphTraverseState
    instances, to allow fast push/pop)), TX3DNode simply reuses a single
    global TX3DGraphTraverseStateStack instance. This means that,
    if you execute Traverse while being inside other Traverse, you
    must first finish innermost Traverse before continuing with the outer. }
  TX3DGraphTraverseStateStack = class
  strict private
    Items: array of TX3DGraphTraverseState;
    ItemsAllocated: Cardinal;
    procedure GrowItems;
  public
    constructor Create;
    destructor Destroy; override;

    { Remove everything. }
    procedure Clear;

    { Push a clear state on the stack. Clear state has everything set
      like a TX3DGraphTraverseState right after creating. }
    procedure PushClear;
    { Push a copy of current top on the stack. }
    procedure Push; overload;
    { Push a copy of given Item on the stack.
      We copy by TX3DGraphTraverseState.Assign, we don't copy the reference. }
    procedure Push(const Item: TX3DGraphTraverseState); overload;
    procedure Pop;

    { Peek at the top of the stack. }
    function Top: TX3DGraphTraverseState;
    function PreviousTop: TX3DGraphTraverseState;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DGraphTraverseState ---------------------------------------------------- }

var
  { Starting state nodes for TX3DGraphTraverseState.Create. }
  VRML1DefaultState: TVRML1State;

procedure TX3DGraphTraverseState.CommonCreate;
begin
  inherited Create;
  PointingDeviceSensors := TPointingDeviceSensorList.Create(false);
  FVRML1State := TVRML1State.Create;
end;

constructor TX3DGraphTraverseState.CreateCopy(Source: TX3DGraphTraverseState);
begin
  CommonCreate;
  Assign(Source);
end;

constructor TX3DGraphTraverseState.Create;
begin
  CommonCreate;

  Transformation.Init;

  { THAnimHumanoidNode.BeforeTraverse will initialize it anyway.
    But set it also here, just in case we have Joint without surrounding
    Humanoid node. (Otherwise Matrix.MultPoint may raise errors in
    THAnimJointNode.ApplyTransform, when multiplying points with 0 matrix,
    testcase is
    view3dscene ~/3dmodels/vrmlx3d/hanim/tecfa.unige.ch/vrml/objects/avatars/blaxxun/kambi_hanim_10_test.wrl.) }
  HumanoidTransformation.Init;

  TextureTransform := TMatrix4.Identity;
  FVRML1State.Assign(VRML1DefaultState);
end;

destructor TX3DGraphTraverseState.Destroy;
begin
  FreeAndNil(FVRML1State);
  FreeAndNil(Lights);
  FreeAndNil(PointingDeviceSensors);
  FreeAndNil(ClipPlanes);
  FreeAndNil(Effects);
  inherited;
end;

procedure TX3DGraphTraverseState.Clear;
begin
  Transformation.Init;
  HumanoidTransformation.Init;

  TextureTransform := TMatrix4.Identity;
  FVRML1State.Assign(VRML1DefaultState);
  ShapeNode := nil;
  InsideInline := 0;
  InsidePrototype := 0;
  InsideIgnoreCollision := 0;
  InsideInvisible := 0;
  LocalFog := nil;

  PointingDeviceSensors.Count := 0;
  FreeAndNil(Lights);
  FreeAndNil(ClipPlanes);
  FreeAndNil(Effects);
end;

procedure TX3DGraphTraverseState.AddLight(const Light: TLightInstance);
begin
  if Lights = nil then
    Lights := TLightInstancesList.Create;
  Lights.Add(Light);
end;

function TX3DGraphTraverseState.AddClipPlane: PClipPlane;
begin
  if ClipPlanes = nil then
    ClipPlanes := TClipPlaneList.Create;
  Result := PClipPlane(ClipPlanes.Add());
end;

procedure TX3DGraphTraverseState.Assign(Source: TX3DGraphTraverseState);
begin
  AssignTransform(Source);

  TextureTransform := Source.TextureTransform;
  FVRML1State.Assign(Source.FVRML1State);
  ShapeNode := Source.ShapeNode;
  InsideInline := Source.InsideInline;
  InsidePrototype := Source.InsidePrototype;
  InsideIgnoreCollision := Source.InsideIgnoreCollision;
  InsideInvisible := Source.InsideInvisible;
  Humanoid := Source.Humanoid;
  LocalFog := Source.LocalFog;

  PointingDeviceSensors.Assign(Source.PointingDeviceSensors);

  if Source.Lights <> nil then
  begin
    if Lights = nil then
      Lights := TLightInstancesList.Create;
    Lights.Assign(Source.Lights);
  end else
    FreeAndNil(Lights);
end;

procedure TX3DGraphTraverseState.AssignTransform(
  Source: TX3DGraphTraverseState);
begin
  Transformation := Source.Transformation;
  HumanoidTransformation := Source.HumanoidTransformation;

  if PointingDeviceSensors.Count <> 0 then
    PointingDeviceSensors.Transformation := Source.PointingDeviceSensors.Transformation;

  if Source.ClipPlanes <> nil then
  begin
    if ClipPlanes = nil then
      ClipPlanes := TClipPlaneList.Create;
    ClipPlanes.Assign(Source.ClipPlanes);
  end else
    FreeAndNil(ClipPlanes);

  if Source.Effects <> nil then
  begin
    if Effects = nil then
      Effects := TX3DNodeList.Create(false);
    Effects.Assign(Source.Effects);
  end else
    FreeAndNil(Effects);
end;

function TX3DGraphTraverseState.Transform: TMatrix4;
begin
  Result := Transformation.Transform;
end;

function TX3DGraphTraverseState.InvertedTransform: TMatrix4;
begin
  Result := Transformation.InverseTransform;
end;

function TX3DGraphTraverseState.Equals(SecondValue: TX3DGraphTraverseState;
  const IgnoreTransform: boolean): boolean;
var
  SN: TVRML1StateNode;
begin
  { Many fields are ignored by Equals, as they have no effect on generated
    TGeometryArrays for shapes. Like InsideInline, InsidePrototype and many
    others. }

  Result :=
    (IgnoreTransform or TMatrix4.PerfectlyEquals(
      Transformation.Transform,
      SecondValue.Transformation.Transform)) and
    TMatrix4.PerfectlyEquals(TextureTransform, SecondValue.TextureTransform) and
    (ShapeNode = SecondValue.ShapeNode) and
    (LocalFog = SecondValue.LocalFog);

  if Result then
  begin
    for SN := Low(SN) to High(SN) do
      if SecondValue.VRML1State.Nodes[SN] <> VRML1State.Nodes[SN] then
        Exit(false);
  end;
end;

function TX3DGraphTraverseState.MainTexture: TAbstractTextureNode;
begin
  Result := nil;
  if ShapeNode = nil then
    Result := VRML1State.Texture2
  else
  if ShapeNode.Appearance <> nil then
    Result := ShapeNode.Appearance.MainTexture;
end;

function TX3DGraphTraverseState.MainTexture(const GeometryForTexCoord: TAbstractGeometryNode;
  out TexCoordIndex: Integer): TAbstractTextureNode;
begin
  Result := nil;
  TexCoordIndex := -1;
  if ShapeNode = nil then
    Result := VRML1State.Texture2
  else
  if ShapeNode.Appearance <> nil then
    Result := ShapeNode.Appearance.MainTexture(GeometryForTexCoord, TexCoordIndex);
end;

function TX3DGraphTraverseState.BlendMode: TBlendModeNode;
var
  Node: TX3DNode;
begin
  Result := nil;
  if ShapeNode <> nil then
  begin
    Node := ShapeNode.FdAppearance.Value;
    if (Node <> nil) and (Node is TAppearanceNode) then
    begin
      Node := TAppearanceNode(Node).FdBlendMode.Value;
      if (Node <> nil) and (Node is TBlendModeNode) then
        Result := TBlendModeNode(Node);
    end;
  end;
end;

function TX3DGraphTraverseState.MaterialInfo: TMaterialInfo;
var
  Mat: TAbstractMaterialNode;
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  Result := nil;
  if ShapeNode <> nil then
  begin
    if ShapeNode.Appearance <> nil then
    begin
      {$warnings off} // using deprecated to keep backward compatibility
      SurfaceShader := ShapeNode.Appearance.CommonSurfaceShader;
      {$warnings on}
      if SurfaceShader <> nil then
        Result := SurfaceShader.MaterialInfo
      else
      begin
        Mat := ShapeNode.Appearance.Material;
        if Mat <> nil then
          Result := Mat.MaterialInfo
        else
          Result := nil;
      end;
    end;
  end else
    Result := VRML1State.Material.MaterialInfo;
end;

{ TX3DGraphTraverseStateStack --------------------------------------------- }

constructor TX3DGraphTraverseStateStack.Create;
begin
  inherited;
end;

destructor TX3DGraphTraverseStateStack.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    FreeAndNil(Items[I]);
  inherited;
end;

procedure TX3DGraphTraverseStateStack.GrowItems;
var
  I, OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(Items, OldLen + 8);
  for I := OldLen to Length(Items) - 1 do
    Items[I] := nil;
end;

procedure TX3DGraphTraverseStateStack.PushClear;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

  { We could instead do Clear in Pop, and then we would know that all
    non allocated instances are always clear.

    But this would be slower: Push is called very often,
    much more often than PushClear (which is called only once
    for every traverse, while Push possibly many times).
    And Pop is called for every Push and PushClear.
    So it's better to optimize the most often called (Pop) than the
    least often called (PushClear). }

  Items[ItemsAllocated].Clear;
  Inc(ItemsAllocated);
end;

procedure TX3DGraphTraverseStateStack.Push;
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

  Items[ItemsAllocated].Assign(Items[ItemsAllocated - 1]);
  Inc(ItemsAllocated);
end;

procedure TX3DGraphTraverseStateStack.Push(const Item: TX3DGraphTraverseState);
begin
  if ItemsAllocated = Cardinal(Length(Items)) then GrowItems;
  if Items[ItemsAllocated] = nil then
    Items[ItemsAllocated] := TX3DGraphTraverseState.Create;

  Items[ItemsAllocated].Assign(Item);
  Inc(ItemsAllocated);
end;

procedure TX3DGraphTraverseStateStack.Pop;
begin
  Dec(ItemsAllocated);
end;

function TX3DGraphTraverseStateStack.Top: TX3DGraphTraverseState;
begin
  Result := Items[ItemsAllocated - 1];
end;

function TX3DGraphTraverseStateStack.PreviousTop: TX3DGraphTraverseState;
begin
  Result := Items[ItemsAllocated - 2];
end;

procedure TX3DGraphTraverseStateStack.Clear;
begin
  ItemsAllocated := 0;
end;

{$endif read_implementation}
