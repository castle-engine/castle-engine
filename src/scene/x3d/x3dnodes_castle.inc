{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined specifically by Castle Game Engine.
  See [https://castle-engine.io/x3d_extensions.php]. } { }

{$ifdef read_interface}
  { Text with 3D depth.
    @bold(The depth is not implemented now -- this is equivalent to @link(TTextNode) now.) }
  TText3DNode = class(TTextNode)
  {$I auto_generated_node_helpers/x3dnodes_text3d.inc}
  end;

  { Customize blending (partial transparency) mode.
    See https://castle-engine.io/x3d_extensions.php#section_ext_blending . }
  TBlendModeNode = class(TAbstractAppearanceChildNode)
  {$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
  end;

  { Appearance node with a special name @code(KambiAppearance).
    Useful if you want to use EXTERNPROTO to declare Castle Game Engine
    extensions the Appearance in a clean way. }
  TKambiAppearanceNode = class(TAppearanceNode)
  {$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
  end;

  { Embed a scene from another file, and replace some of it's nodes.
    See https://castle-engine.io/x3d_extensions.php#section_ext_kambi_inline . }
  TKambiInlineNode = class(TInlineNode)
  strict private
    { This is internal only for DoReplaceNodes, initialized in AfterLoadInlined. }
    ReplaceCount: Integer;
    procedure DoReplaceNodes(ParentNode: TX3DNode; var Node: TX3DNode);
  protected
    procedure AfterLoadInlined; override;
  public
    {$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
  end;

  { Navigation information with some Castle Game Engine extensions.
    @deprecated Use TNavigationInfoNode, it already has all CGE extensions. }
  TKambiNavigationInfoNode = class(TNavigationInfoNode)
  {$I auto_generated_node_helpers/x3dnodes_kambinavigationinfo.inc}
  end;

  TShadowMapCompareMode = (smCompareRLEqual, smCompareRGEqual, smNone);

  { Generate texture by rendering a depth (shadow) map.
    See https://castle-engine.io/x3d_extensions_shadow_maps.php }
  TGeneratedShadowMapNode = class(TAbstractSingleTextureNode)
  strict private
    type
      TGenShadowMapGenTex = class(TGeneratedTextureFunctionality)
      strict private
        FParent: TGeneratedShadowMapNode;
      protected
        function GetUpdate: TTextureUpdate; override;
        procedure SetUpdate(const Value: TTextureUpdate); override;
      public
        constructor Create(const AParent: TGeneratedShadowMapNode);
      end;
    var
      FGenTexFunctionality: TGenShadowMapGenTex;
  public
    const
      DefaultSize = 128;
      DefaultScale = 4.0;
      DefaultBias = 4.0;

    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

  {$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
  end;

  { Generate texture coordinates by projection.
    See https://castle-engine.io/x3d_extensions_shadow_maps.php }
  TProjectedTextureCoordinateNode = class(TAbstractSingleTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
  end;

  { Screen-space shader effect.
    See https://castle-engine.io/x3d_extensions_screen_effects.php . }
  TScreenEffectNode = class(TAbstractChildNode)
  strict private
    FInternalRendererResource: TInternalRendererResource;
    FContextCloseEventSetup: Boolean;
    procedure SetInternalRendererResource(const Value: TInternalRendererResource);
    procedure GLContextCloseEvent(Sender: TObject);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    { Internal data stored here and used by the renderer.
      Specific to this TX3DNode.

      This holds a reference count to OpenGL resource in RendererCache,
      freeing this InternalRendererResource decreases the reference count,
      and it is done when destroying this node or when last GL context
      is lost.

      @exclude }
    property InternalRendererResource: TInternalRendererResource
      read FInternalRendererResource write SetInternalRendererResource;

    { Free InternalRendererResource, if any. }
    procedure InternalRendererResourceFree;

    destructor Destroy; override;

  {$I auto_generated_node_helpers/x3dnodes_screeneffect.inc}
  end;

  { Internal notes to MultiGeneratedTextureCoordinate:

    It's implemented as simple MultiTextureCoordinate descendant,
    this is conforming with the specification. (MultiTextureCoordinate
    allows a little more children, but this will be detected and ignored
    by renderer.)

    Also, this way fields like Sphere.texCoord may require just
    MultiTextureCoordinate, and so I have backward compatibility
    and also accept MultiGeneratedTextureCoordinate.

    Also, shadow maps processing can just use MultiTextureCoordinate
    (regardless if we're on primitives or not). }

  { Generated texture coordinates for multi-texturing.
    See [https://castle-engine.io/x3d_extensions.php#section_ext_tex_coord]. }
  TMultiGeneratedTextureCoordinateNode = class(TMultiTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
  end;

  TEffectPartNode = class;

  { Shader effect, that can be composed with other effect or standard rendering.
    See https://castle-engine.io/shaders . }
  TEffectNode = class(TAbstractChildNode)
  protected
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_effect.inc}
  end;

  { Part of a shader effect, used with @link(TEffectNode).
    See https://castle-engine.io/shaders . }
  TEffectPartNode = class(TAbstractNode)
  strict private
    ContentsCached: boolean;
    ContentsCache: String;
    procedure EventUrlReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    function GetContents: String;
    procedure SetContents(const Value: String);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    { The shader code.

      Getting this automatically loads the shader code pointed by the URL field
      of this node. The shader code may be specified in an external file,
      or inline using "data URI", see https://castle-engine.io/url .
      The shader code may also be specified using the XML "CDATA" in X3D XML encoding.
      Returns '' of no valid URLs are present
      (loading errors are reported to WritelnWarning).

      Setting this overrides the URL field, to specify only the given shader code. }
    property Contents: String read GetContents write SetContents;

  {$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
  end;

  { Procedural texture defined by shaders.
    See https://castle-engine.io/shaders . }
  TShaderTextureNode = class(TAbstractSingleTextureNode)
  {$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
  end;

  { Viewpoint for making reflections on flat objects.
    See https://castle-engine.io/x3d_extensions_mirror_plane.php . }
  TViewpointMirrorNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_viewpointmirror.inc}
  end;

  { Interpolate (animate) a 2D orientation,
    during which angle changes but axis remains constant. }
  TOrientationInterpolator2DNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFRotation>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFRotation; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFRotation; const Index: Integer); override;
    function CreateOutputField: TSFRotation; override;
  {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator2d.inc}
  end;

  { Defines how a set of joints influences meshes,
    thus enabling skinned animation in a way that is simple,
    efficient and perfectly aligned with glTF.
    See @url(https://castle-engine.io/skin skinned animation documentation)
    for usage details. }
  TSkinNode = class(TAbstractChildNode)
  strict private
    { Having this ready, and of proper size, speeds up InternalUpdateSkin. }
    JointMatrix: TMatrix4List;
    DoneWarningNoCoords: Boolean;
    DoneWarningNoCoordNode: Boolean;
    DoneWarningNoNormals: Boolean;
    DoneWarningNoTangents: Boolean;
    DoneWarningNormalsCount: Boolean;
    DoneWarningTangentsCount: Boolean;
    DoneWarningNoWeightsJoints: Boolean;
    DoneWarningInverseBindMatricesCount: Boolean;
    DoneWarningJointTransformCount: Boolean;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    class var
      { Set by TCastleScene to indicate do we use shaders for skin. }
      InternalSkinInShaders: Boolean;

    destructor Destroy; override;
    { Update meshes in @link(FdShapes) to reflect the current joints transformation.
      This does the core of "skinned animation" logic. }
    procedure InternalUpdateSkin;
  {$I auto_generated_node_helpers/x3dnodes_skin.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TKambiInlineNode.AfterLoadInlined;
var
  I: Integer;
begin
  ReplaceCount := Min(FdreplaceNames.Count, FdreplaceNodes.Count);
  if (FdreplaceNames.Count <> ReplaceCount) or
     (FdreplaceNodes.Count <> ReplaceCount) then
    WritelnWarning('VRML/X3D', 'replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successful loading, there's always exactly one child. }
  Assert(VRML1ChildrenCount = 1);

  { First, a special case when Children[0] node has the required name.
    Needs to be checked explicitly, EnumerateReplaceNodes cannot check this. }
  for I := 0 to ReplaceCount - 1 do
    { Ignore empty names, since VRML node cannot have empty name
      (it's only our implementation detail that we mark "noname" as empty name) }
    if (FdReplaceNames.Items[I] <> '') and
       (FdReplaceNames.Items[I] = VRML1Children[0].X3DName) then
    begin
      VRML1ChildRemove(0);
      VRML1ChildAdd(0, FdReplaceNodes[I]);
      Exit;
    end;

  VRML1Children[0].EnumerateReplaceChildren({$ifdef FPC}@{$endif} DoReplaceNodes);
end;

procedure TKambiInlineNode.DoReplaceNodes(ParentNode: TX3DNode;
  var Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to ReplaceCount - 1 do
    if (FdReplaceNames.Items[I] <> '') and
       (FdReplaceNames.Items[I] = Node.X3DName) then
    begin
      Node := FdReplaceNodes[I];
      Exit;
    end;
end;

{ TGeneratedShadowMapNode.TGenShadowMapGenTex -------------------------------- }

constructor TGeneratedShadowMapNode.TGenShadowMapGenTex.Create(const AParent: TGeneratedShadowMapNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

function TGeneratedShadowMapNode.TGenShadowMapGenTex.GetUpdate: TTextureUpdate;
begin
  Result := FParent.Update;
end;

procedure TGeneratedShadowMapNode.TGenShadowMapGenTex.SetUpdate(const Value: TTextureUpdate);
begin
  FParent.Update := Value;
end;

{ TGeneratedShadowMapNode ---------------------------------------------------- }

 { Note that TGeneratedShadowMapNode.FdLight is not listed in
   TGeneratedShadowMapNode.DirectEnumerateActive,
   because the light doesn't shine here. We don't want
   to override it's transform with transformation of a GeneratedShadowMap. }

const
  CompareModeNames: array [TShadowMapCompareMode] of string = (
    'COMPARE_R_LEQUAL',
    'COMPARE_R_GEQUAL',
    'NONE'
  );

constructor TGeneratedShadowMapNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FGenTexFunctionality := TGenShadowMapGenTex.Create(Self);
  AddFunctionality(FGenTexFunctionality);
end;

{ TScreenEffectNode ---------------------------------------------------------- }

function TScreenEffectNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FFdShaders.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ TODO: TScreenEffectNode stuff related to InternalRendererResource
  is copy-pasted from TAbstractTextureNode.

  There doesn't seem much opportunity to gain by sharing their code,
  and we don't want to move this mechanism to TX3DNode for now to not
  complicate API of all nodes.

  This duplication seems OK for now. }

procedure TScreenEffectNode.SetInternalRendererResource(const Value: TInternalRendererResource);
begin
  if FInternalRendererResource <> Value then
  begin
    FInternalRendererResource := Value;
    if not FContextCloseEventSetup then
    begin
      FContextCloseEventSetup := true;
      ApplicationProperties.OnGLContextCloseObject.Add(
        {$ifdef FPC}@{$endif} GLContextCloseEvent);
    end;
  end;
end;

procedure TScreenEffectNode.GLContextCloseEvent(Sender: TObject);
begin
  InternalRendererResourceFree;
end;

procedure TScreenEffectNode.InternalRendererResourceFree;
begin
  FreeAndNil(FInternalRendererResource);
end;

destructor TScreenEffectNode.Destroy;
begin
  GLContextCloseEvent(nil);
  if FContextCloseEventSetup then
    ApplicationProperties.OnGLContextCloseObject.Remove(
      {$ifdef FPC}@{$endif} GLContextCloseEvent);
  inherited;
end;

{ TEffectNode ---------------------------------------------------------------- }

constructor TEffectNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  HasInterfaceDeclarations := AllAccessTypes;
end;

procedure TEffectNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  inherited;
  if State.Effects = nil then
    State.Effects := TX3DNodeList.Create(false);
  State.Effects.Add(Self);
end;

{ TEffectPartNode ------------------------------------------------------------ }

constructor TEffectPartNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
  FdUrl.AddNotification({$ifdef FPC}@{$endif} EventUrlReceive);
  CDataField := FdUrl;
end;

function TEffectPartNode.GetContents: String;
begin
  if not ContentsCached then
  begin
    ContentsCache := ShaderLoadContents(Self, FdUrl);
    ContentsCached := true;
  end;
  Result := ContentsCache;
end;

procedure TEffectPartNode.SetContents(const Value: String);
begin
  SetUrl(['data:text/plain,' + Value]);

  //Assert(Value = GetContents);
  // an optimized way to initialize ContentsCache
  ContentsCache := Value;
  ContentsCached := true;
end;

procedure TEffectPartNode.EventUrlReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  ContentsCached := false;
end;

{ TOrientationInterpolator2DNode ---------------------------------- }

function TOrientationInterpolator2DNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TOrientationInterpolator2DNode.InterpolatorLerp(const Target: TSFRotation;
  const A: Single; const Index1, Index2: Integer);
var
  OutputValueAngle: Single;
begin
  OutputValueAngle := AngleLerp(A,
    FdKeyValue.Items.L[Index1],
    FdKeyValue.Items.L[Index2]);
  Target.Value := Vector4(FdAxis.Value, OutputValueAngle);
end;

procedure TOrientationInterpolator2DNode.InterpolatorSet(const Target: TSFRotation; const Index: Integer);
var
  OutputValueAngle: Single;
begin
  OutputValueAngle := FdKeyValue.Items.L[Index];
  Target.Value := Vector4(FdAxis.Value, OutputValueAngle);
end;

function TOrientationInterpolator2DNode.CreateOutputField: TSFRotation;
begin
  Result := TSFRotation.CreateUndefined(Self, false, '');
end;

{ TSkinNode ------------------------------------------------------------------ }

destructor TSkinNode.Destroy;
begin
  FreeAndNil(JointMatrix);
  inherited;
end;

function TSkinNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdSkeleton.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdShapes.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TSkinNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Push;
  StateStack.Top.Skin := Self;
end;

procedure TSkinNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  { THAnimHumanoid does this.
    - But it should not be necessary to do from AfterTraverse.
    - And it could be harmful: if we do it at loading, we risk that we initialize
      Shaep.Geometry.InternalOriginal* before InternalSkinInShaders is set
      to true, which tells us we should not this.
    - And it would waste loading time, unnecessarily. }
  // InternalUpdateSkin;
  StateStack.Pop;
  inherited;
end;

procedure TSkinNode.InternalUpdateSkin;

  { Calculate our JointMatrix based on current transformation of FdJoints.
    This work is independent of any particular shape.
    This work also makes sense for both GPU skinning and CPU skinning. }
  procedure PrepareJointMatrix;
  var
    I: Integer;
    {$ifndef CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
    SkeletonRootInverse: TMatrix4;
    {$endif not CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
  begin
    {$ifndef CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
    SkeletonRootInverse := JointTransform(Skeleton)^.InverseTransform;
    {$endif not CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}

    { For each Joint, we calculate JointMatrix following
      https://www.slideshare.net/Khronos_Group/gltf-20-reference-guide }
    if JointMatrix = nil then
      JointMatrix := TMatrix4List.Create;
    JointMatrix.Count := FdJoints.Count;
    if FdJoints.Count <> FdInverseBindMatrices.Count then
    begin
      if FdInverseBindMatrices.Count <> 0 then
        WritelnWarningOnce(DoneWarningInverseBindMatricesCount,
          'Count of TSkinNode.inverseBindMatrices (%d) does not match count of TSkinNode.joints (%d)', [
          FdInverseBindMatrices.Count,
          FdJoints.Count
        ]);
      for I := 0 to FdJoints.Count - 1 do
        JointMatrix.L[I] :=
          {$ifdef CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
          FdJoints[I].TransformFunctionality.SkinJointTransform.Transform
          {$else}
          SkeletonRootInverse *
          JointTransform(FdJoints[I])^.Transform
          {$endif CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
        ;
    end else
    begin
      for I := 0 to FdJoints.Count - 1 do
        JointMatrix.L[I] :=
          {$ifdef CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
          FdJoints[I].TransformFunctionality.SkinJointTransform.Transform
          {$else}
          SkeletonRootInverse *
          JointTransform(FdJoints[I])^.Transform
          {$endif CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL} *
          FdInverseBindMatrices.Items.L[I];
    end;
  end;

  function ShapeLit(const ShapeNode: TAbstractShapeNode): Boolean;
  begin
    Result := (ShapeNode.Appearance <> nil) and
      (
        (ShapeNode.Appearance.Material is TMaterialNode) or
        (ShapeNode.Appearance.Material is TPhysicalMaterialNode)
      );
  end;

  procedure UpdateShape(const Shape: TAbstractShapeNode);

    { Get various per-vertex lists from the Shape's geometry.

      Returns @true if we got at least:

      - non-nil Coords, Weights0, Joints0
      - Normals are either @nil, or they are of the same count as Coords
      - Tangents are either @nil, or they are of the same count as Coords

      The Xxxs and XxxNode are always either both nil, or both non-nil.
      E.g. we always return NormalNode = nil and Normals = nil, or both non-nil.
       }
    function GetArrays(out Coords, Normals, Tangents: TVector3List;
      out CoordNode: TCoordinateNode;
      out NormalNode: TNormalNode;
      out TangentNode: TTangentNode;
      out Weights0: TVector4List;
      out Joints0: TInt32List): Boolean;
    var
      CoordField: TSFNode;
    begin
      // initialize output to defaults
      Result := false;
      Coords := nil;
      Normals := nil;
      Tangents := nil;
      CoordNode := nil;
      NormalNode := nil;
      TangentNode := nil;
      Weights0 := nil;
      Joints0 := nil;

      // start by calculating Coords (and helper CoordField, CoordNode)
      CoordField := Shape.Geometry.CoordField;
      if CoordField = nil then
      begin
        WritelnWarningOnce(DoneWarningNoCoords,
          'Cannot animate using skin geometry %s, it does not have coordinates', [
          Shape.Geometry.NiceName
        ]);
        Exit;
      end;
      if not (CoordField.Value is TCoordinateNode) then
      begin
        WritelnWarningOnce(DoneWarningNoCoordNode,
          'Cannot animate using skin geometry %s, the coordinates are not expressed as Coordinate node', [
          Shape.Geometry.NiceName
        ]);
        Exit;
      end;
      CoordNode := CoordField.Value as TCoordinateNode;
      Coords := CoordNode.FdPoint.Items;

      // calculate Normals (and helper NormalNode)
      if (Shape.Geometry.NormalField <> nil) and
         (Shape.Geometry.NormalField.Value is TNormalNode) then
      begin
        NormalNode := TNormalNode(Shape.Geometry.NormalField.Value);
        Normals := NormalNode.FdVector.Items;
        // SampleSkinAnimation assumes that normals and coords counts are equal
        if Normals.Count <> Coords.Count then
        begin
          WritelnWarningOnce(DoneWarningNormalsCount,
            'When animating using skin geometry %s, coords and normals counts different', [
            Shape.Geometry.NiceName
          ]);
          NormalNode := nil;
          Normals := nil;
        end;
      end else
      begin
        if ShapeLit(Shape) then
          WritelnWarningOnce(DoneWarningNoNormals,
            'TODO: Normal vectors are not provided for a skinned geometry (using lit material), and in effect the resulting animation will be slow as we''ll recalculate normals more often than necessary. ' + 'For now it is adviced to generate models with normals included for skinned meshes.');
      end;

      // calculate Tangent
      if (Shape.Geometry.TangentField <> nil) and
         (Shape.Geometry.TangentField.Value is TTangentNode) then
      begin
        TangentNode := TTangentNode(Shape.Geometry.TangentField.Value);
        Tangents := TangentNode.FdVector.Items;
        // SampleSkinAnimation assumes that tangents and coords counts are equal
        if Tangents.Count <> Coords.Count then
        begin
          WritelnWarningOnce(DoneWarningTangentsCount,
            'When animating using skin geometry %s, coords and tangents counts different', [
            Shape.Geometry.NiceName
          ]);
          TangentNode := nil;
          Tangents := nil;
        end;
      end else
      begin
        if ShapeLit(Shape) and
          ((Shape.Appearance.Material as TAbstractOneSidedMaterialNode).NormalTexture <> nil) then
          WritelnWarningOnce(DoneWarningNoTangents,
            'TODO: Tangent vectors are not provided for a skinned geometry (using lit material with normalmap), and in effect the resulting animation will be slow as we''ll recalculate tangents more often than necessary. ' + 'For now it is adviced to generate models with tangents included for skinned meshes.');
      end;

      if (not Shape.Geometry.SkinWeightsJoints(Weights0, Joints0)) or
         (Weights0.Count      <> Coords.Count) or
         (Joints0.Count div 4 <> Coords.Count) then
      begin
        WritelnWarningOnce(DoneWarningNoWeightsJoints,
          'Cannot animate using skin geometry %s, JOINTS_0 and WEIGHTS_0 information for the mesh not provided or has invalid size', [
          Shape.Geometry.NiceName
        ]);
        Exit;
      end;

      Result := true;
    end;

    { Update list supposed to contain original (before animation started) values
      of some array. }
    procedure UpdateGeometryOriginalArray(var OriginalArray: TVector3List;
      const NewArray: TVector3List);
    begin
      if NewArray = nil then
      begin
        FreeAndNil(OriginalArray);
      end else
      begin
        if OriginalArray = nil then
          OriginalArray := TVector3List.Create;
        // Change OriginalArray contents based on NewArray only if sizes changed
        if OriginalArray.Count <> NewArray.Count then
        begin
          OriginalArray.Clear;
          OriginalArray.AddRange(NewArray);
        end;
      end;
    end;

    { Initialize Geometry.InternalOriginal is necessary. }
    procedure UpdateGeometryOriginalArrays(const Geometry: TAbstractGeometryNode;
      const Coords, Normals, Tangents: TVector3List);
    begin
      UpdateGeometryOriginalArray(Geometry.InternalOriginalCoords, Coords);
      UpdateGeometryOriginalArray(Geometry.InternalOriginalNormals, Normals);
      UpdateGeometryOriginalArray(Geometry.InternalOriginalTangents, Tangents);
    end;

    {$ifndef CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}
    { Return transformation, as matrix, of this joint (from the root of the
      X3D graph, so within this TCastleScene).
      Returns pointer to read-only TTransformation that contains
      the transformation matrix in both ways. }
    function JointTransform(const Joint: TX3DNode): PTransformation;
    var
      ShapeTransform: TShapeTreeTransform;
      C: Integer;
    begin
      C := TShapeTree.AssociatedShapesCount(Joint);
      if C <> 1 then
      begin
        WritelnWarningOnce(DoneWarningJointTransformCount,
          'Joint %s is not present in the scene exactly once, cannot determine transformation', [
          Joint.X3DName
        ]);
        Exit(@IdentityTransformation);
      end;
      ShapeTransform := TShapeTree.AssociatedShape(Joint, 0) as TShapeTreeTransform;
      Result := @ShapeTransform.TransformState.Transformation;
    end;
    {$endif not CASTLE_SKIN_JOINTS_TRANSFORM_LOCAL}

    { Update skin Coords (and optionally Normals, Tangents)
      based on current joints transformation.
      OriginalCoords contains original (not animated) coords. }
    procedure SampleSkinAnimation(
      const OriginalCoords, AnimatedCoords: TVector3List;
      const OriginalNormals, AnimatedNormals: TVector3List;
      const OriginalTangents, AnimatedTangents: TVector3List;
      const Joints0: TInt32List;
      const Weights0: TVector4List);
    var
      I: Integer;
      SkinMatrix: TMatrix4;
      VertexJointsPtr: PVector4Integer;
      VertexWeights: TVector4;
    begin
      Assert((AnimatedNormals = nil) = (OriginalNormals = nil));
      Assert((OriginalNormals = nil) or (OriginalNormals.Count = OriginalCoords.Count));
      Assert((AnimatedTangents = nil) = (OriginalTangents = nil));
      Assert((OriginalTangents = nil) or (OriginalTangents.Count = OriginalCoords.Count));

      { For each vertex, calculate SkinMatrix as linear combination of JointMatrix[...]
        for all joints indicated by Joints0 values for this vertex. }
      for I := 0 to OriginalCoords.Count - 1 do
      begin
        VertexWeights := Weights0[I];
        VertexJointsPtr := PVector4Integer(Joints0.Ptr(I * 4));
        if VertexWeights.IsPerfectlyZero then
        begin
          { Happens with glTF files generated by Blender.
            This is not correct (glTF spec says that weights should sum to 1.0).
            Solution that works: Transform it with weight 1 by the joint number 0
            (relying that Blender put root joint at this position). See
            https://github.com/KhronosGroup/glTF/issues/1213
            https://github.com/KhronosGroup/glTF-Blender-IO/issues/308
            https://github.com/KhronosGroup/glTF-Blender-IO/issues/308#issuecomment-531355129
              """it's not exactly a satisfying fix, but in practice using 1, 0, 0, 0 when the weights would otherwise be zero has avoided these issues in threejs."""
            https://github.com/KhronosGroup/glTF/pull/1352
            https://github.com/Franck-Dernoncourt/NeuroNER/issues/91 }
          SkinMatrix := JointMatrix.L[0];
        end else
        begin
          SkinMatrix :=
            JointMatrix.L[VertexJointsPtr^.X] * VertexWeights.X +
            JointMatrix.L[VertexJointsPtr^.Y] * VertexWeights.Y +
            JointMatrix.L[VertexJointsPtr^.Z] * VertexWeights.Z +
            JointMatrix.L[VertexJointsPtr^.W] * VertexWeights.W;
        end;
        { Note: On Delphi, we *have to* use L[...] below and depend on $pointermath on,
          instead of using List^[...].
          That's because on Delphi, List^[...] may have too small (declared) upper size
          due to Delphi not supporting SizeOf(T) in generics.
          See https://github.com/castle-engine/castle-engine/issues/474 . }
        AnimatedCoords.L[I] := SkinMatrix.MultPoint(OriginalCoords[I]);
        if AnimatedNormals <> nil then
          AnimatedNormals.L[I] := SkinMatrix.MultDirection(OriginalNormals[I]);
        if AnimatedTangents <> nil then
          AnimatedTangents.L[I] := SkinMatrix.MultDirection(OriginalTangents[I]);
      end;
    end;

  var
    Coords, Normals, Tangents: TVector3List;
    CoordNode: TCoordinateNode;
    NormalNode: TNormalNode;
    TangentNode: TTangentNode;
    Weights0: TVector4List;
    Joints0: TInt32List;
  begin
    if InternalSkinInShaders then
    begin
      if (Shape.Geometry.InternalOriginalCoords <> nil) and
         (Shape.Geometry.InternalOriginalNormals <> nil) and
         (Shape.Geometry.InternalOriginalTangents <> nil) then
        WritelnWarning('TCastleScene started skinned animation before rendering context was initialized, in effect we moved "original" data to special lists.' + NL +
          '  It has to be undone now to do correct skinning in GPU.' + NL +
          '  TODO: For now, this "undoing" is not implemented, animation will not be correct.');
      // rest of the UpdateShape is for non-GPU skinning
      Exit;
    end;

    if not GetArrays(
        Coords, Normals, Tangents,
        CoordNode, NormalNode, TangentNode,
        Weights0, Joints0) then
      Exit;
    UpdateGeometryOriginalArrays(Shape.Geometry, Coords, Normals, Tangents);
    SampleSkinAnimation(
      Shape.Geometry.InternalOriginalCoords, Coords,
      Shape.Geometry.InternalOriginalNormals, Normals,
      Shape.Geometry.InternalOriginalTangents, Tangents,
      Joints0, Weights0);

    // Call Changed to update the display
    CoordNode.FdPoint.Changed;
    if NormalNode <> nil then
      NormalNode.FdVector.Changed;
    if TangentNode <> nil then
      TangentNode.FdVector.Changed;
  end;

var
  I: Integer;
begin
  if FdShapes.Count = 0 then
    Exit; // early exit if nothing needs to be done

  PrepareJointMatrix;

  for I := 0 to FdShapes.Count - 1 do
    if FdShapes.Items[I] is TAbstractShapeNode then
      UpdateShape(TAbstractShapeNode(FdShapes.Items[I]));
end;

{ registration --------------------------------------------------------------- }

procedure RegisterCastleEngineNodes;
begin
  NodesManager.RegisterNodeClasses([
    TText3DNode,
    TBlendModeNode,
    TKambiAppearanceNode,
    TKambiInlineNode,
    TKambiNavigationInfoNode,
    TGeneratedShadowMapNode,
    TProjectedTextureCoordinateNode,
    TScreenEffectNode,
    TMultiGeneratedTextureCoordinateNode,
    TEffectNode,
    TEffectPartNode,
    TShaderTextureNode,
    TViewpointMirrorNode,
    TOrientationInterpolator2DNode,
    TSkinNode
  ]);
end;

{$endif read_implementation}
