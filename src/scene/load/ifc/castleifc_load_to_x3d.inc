{%MainUnit castleifc.pas}
{
  Copyright 2024-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ IFC conversion to X3D. Allows to load IFC in our engine. }

{$ifdef read_interface_types}

  { Maintains mapping between IFC concepts and X3D nodes.
    Allows to comfortably load IFC file and convert it to X3D multple
    times (using @link(Load), @link(Update), @link(RootNode)).
    Allows also to resolve X3D nodes back to IFC concepts
    (using @link(NodeToProduct)). }
  TCastleIfcMapping = class
  strict private
    type
      TShapeToProduct =
        {$ifdef FPC}specialize{$endif} TDictionary<TAbstractShapeNode, TIfcProduct>;
    var
      FBaseUrl: String;
      FRootNode: TX3DRootNode;

      { Map X3D shape nodes to IFC products.

        Allows to implement NodeToProduct, ProductToNode.
        Use this only to map TShapeNode instances (pointers)
        that definitely exist and are valid.

        We cleanup freed TShapeNode instances from this dictionary,
        so only valid TShapeNode instances are keys.
        This accounts for the fact that @link(Update) may recycle
        some, but not all, TShapeNode instances, and it frees unused ones.

        Note that we cannot maintain inverse dictionary
        product -> shape because

        1. during Update, products may point to multiple
           shapes (the ones from previous update, and the new ones),
           and we don't know which ones will survive until we finish updating.

        2. moreover, the product may really be represented by multiple shapes. }
      ShapeToProduct: TShapeToProduct;

    { Use ObjectPlacementToParentNodes and ChildrenNodesToObjectPlacement
      to move the nodes (change parents) to reflect
      TIfcObjectPlacement.PlacementRelTo instructions. }
    procedure FixParentChildRelationships(const RootNode: TX3DRootNode);

    function CreateRootNode(const Ifc: TIfcFile): TX3DRootNode;
    procedure ShapeDestroyed(const Node: TX3DNode);
  private
    { Within the CastleIfc unit, this class is a container for information
      and actions useful to all
      node-building routines that build/update X3D stuff from IFC.
      The things below are used by BuildNode methods of IFC classes. }

    type
      TObjectPlacementToParentNodes =
        {$ifdef FPC}specialize{$endif} TDictionary<TIfcObjectPlacement, TTransformNode>;
      TChildrenNodesToObjectPlacement =
        {$ifdef FPC}specialize{$endif} TDictionary<TTransformNode, TIfcObjectPlacement>;

    var
      { Map each encountered TIfcObjectPlacement to an X3D TTransformNode. }
      ObjectPlacementToParentNodes: TObjectPlacementToParentNodes;

      { Map each children node (TTransformNode) to a desired parent,
        expressed as TIfcObjectPlacement.
        Together, ObjectPlacementToParentNodes and ChildrenNodesToObjectPlacement
        allow to set the visual transformation (parent-child relationships
        between X3D nodes) to follow IFC TIfcObjectPlacement.PlacementRelTo idea. }
      ChildrenNodesToObjectPlacement: TChildrenNodesToObjectPlacement;

    { BaseUrl should be passed to all X3D nodes created by this class,
      so they have TX3DRootNode.BaseUrl equal to this. }
    property BaseUrl: String read FBaseUrl;

    { Remember association between X3D Shape and IFC Product.
      This is used to implement NodeToProduct, ProductToNode.

      When Product is @nil, this is ignored.
      This is comfortable when used from TIfcRepresentationItem.BuildNode
      overrides, that may get @nil Product. }
    procedure ConnectShapeAndProduct(const Shape: TAbstractShapeNode;
      const Product: TIfcProduct);
  public
    constructor Create;
    destructor Destroy; override;

    { Load IFC instance (TIfcFile) converting it to a new X3D node
      (available in @link(RootNode)).

      The BaseUrl will be used to resolve relative URLs,
      e.g. texture URLs mentioned in IFC information.

      After calling this (and before destroying this class or calling
      @link(Load) again) remember to free the resulting @link(RootNode),
      it is not freed automatically by this class (because in all practical
      use-cases, you want to keep the X3D node around,
      e.g. to be owned by TCastleScene or freed manually later). }
    procedure Load(const Ifc: TIfcFile; const ABaseUrl: String);

    { Latest X3D root node, built from the latest IFC instance. }
    property RootNode: TX3DRootNode read FRootNode;

    { Update the nodes in @link(RootNode) based on the new IFC contents.
      Call this only after calling @link(Load) once.

      Tries to be efficient and do as little work as possible
      and update X3D graph in @link(RootNode) to reflect the new IFC contents.
      For example, changing a translation in IFC will just change the corresponding
      @link(TTransformNode.Translation), without recreating any X3D nodes. }
    procedure Update(const Ifc: TIfcFile);

    { For a shape node (TAbstractShapeNode), return the corresponding IFC product
      (TIfcProduct descendant, like a TIfcWall, TIfcWindow, TIfcDoor).
      Returns @nil if cannot be determined.
      Assumes that the nodes have been updated to reflect the latest state
      of the IFC classes using @link(Update).

      @seealso ProductToNode }
    function NodeToProduct(const Node: TAbstractShapeNode): TIfcProduct;

    { For IFC product, return the corresponding shape nodes.
      Assumes that the nodes have been updated to reflect the latest state
      of the IFC classes using @link(Update).
      All the nodes returned are of TAbstractShapeNode class
      and map back (using @link(NodeToProduct)) to the given product.

      It is the caller's responsibility to free the resulting list.
      The TX3DNodeList does not own the children.

      @seealso NodeToProduct }
    function ProductToNodes(const Product: TIfcProduct): TX3DNodeList;
  end;

{$endif read_interface_types}

{$ifdef read_interface}

{ Convert IFC file to X3D. }
function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;

{$endif read_interface}

{$ifdef read_implementation}

{ Debug transformations (translation, rotation, scale). }
{.$define CASTLE_DEBUG_IFC_TRANSFORM}

{ Set metadata of a node. }
procedure SetMetadata(const Node: TAbstractNode; const Key, Value: String);
begin
  if Value <> '' then // do not set empty (default in IFC) values, to keep X3D output smaller
  begin
    if Node is TX3DRootNode then
      { TODO: "metadata" of TX3DRootNode is not serialized, but META is.
        Yeah, there are effectively two ways to store metadata in X3D. }
      TX3DRootNode(Node).Meta[Key] := Value
    else
      Node.MetadataString[Key] := Value;
  end;
end;

{ TCastleIfcMapping ---------------------------------------------------------- }

constructor TCastleIfcMapping.Create;
begin
  inherited;
  ShapeToProduct := TShapeToProduct.Create;
end;

destructor TCastleIfcMapping.Destroy;
var
  Shape: TAbstractShapeNode;
begin
  if ShapeToProduct <> nil then
  begin
    { We must remove all destruction notifications leading to Self. }
    for Shape in ShapeToProduct.Keys do
      Shape.RemoveDestructionNotification({$ifdef FPC}@{$endif} ShapeDestroyed);
    FreeAndNil(ShapeToProduct);
  end;
  inherited;
end;

procedure TCastleIfcMapping.FixParentChildRelationships(const RootNode: TX3DRootNode);

  { Create TTransformNode for TIfcObjectPlacement instances
    that do not (yet) have the corresponding TTransformNode.

    In the usual case, TIfcObjectDefinition.BuildNode
    creates TTransformNode which is then filled with TIfcObjectPlacement
    information. But some TIfcObjectPlacement instances
    are not directly inside any TIfcObjectDefinition,
    yet they are referenced by placementRelTo from other TIfcObjectPlacement instances.

    Testcase: ifc_window_test/1_initial_tests/new-wall-with-opening-and-window.ifcjson
  }
  procedure AddTransformNodeForObjectPlacement(const ObjectPlacement: TIfcObjectPlacement);
  var
    Transform: TTransformNode;
  begin
    Transform := ObjectPlacement.BuildNode(Self);
    RootNode.AddChildren(Transform);
  end;

  { Go up the nodes hierachy, returning direct parent (any TAbstractGroupingNode)
    and possibly non-direct TransformOrRootParent (TTransformNode or TX3DRootNode)
    parent.

    Returns @true and for sure non-nil DirectParent, TransformOrRootParent on success.
    On failure returns @false and DirectParent, TransformOrRootParent are undefined.

    This way of searching for 2 parents accounts for the fact that
    things in TIfcProduct are wrapped in additional groups related to 'Representation'.
    We don't need to move things out of these groups,
    if ultimately they are placed in the right TTransformNode. }
  function GetParentNode(const Node: TTransformNode;
    out DirectParent: TAbstractGroupingNode;
    out TransformOrRootParent: TAbstractGroupingNode): Boolean;

    function GetDirectParent(const Node: TAbstractGroupingNode): TAbstractGroupingNode;
    begin
      Result := nil;
      if Node.ParentFieldsCount > 1 then
      begin
        WritelnWarning('IFC', 'Corresponding X3D node has more than one parent field, cannot fix parents to follow placementRelTo', [
          Node.NiceName
        ]);
      end else
      if Node.ParentFieldsCount = 1 then
      begin
        if Node.ParentFields[0].ParentNode is TAbstractGroupingNode then
        begin
          Result := TAbstractGroupingNode(Node.ParentFields[0].ParentNode)
        end else
        begin
          WritelnWarning('IFC', 'Corresponding X3D node has parent field that is not a TAbstractGroupingNode, cannot fix parents to follow placementRelTo', [
            Node.NiceName
          ]);
        end;
      end;
    end;

  var
    PotentialTransformOrRootParent: TAbstractGroupingNode;
  begin
    Result := false;
    DirectParent := GetDirectParent(Node);
    TransformOrRootParent := nil; // initialize to nil
    if DirectParent = nil then
      Exit; // exit with Result = false

    PotentialTransformOrRootParent := DirectParent;
    while not (
      (PotentialTransformOrRootParent is TTransformNode) or
      (PotentialTransformOrRootParent is TX3DRootNode) ) do
    begin
      PotentialTransformOrRootParent := GetDirectParent(PotentialTransformOrRootParent);
      if PotentialTransformOrRootParent = nil then
        Exit; // exit with Result = false
    end;
    Assert(
      (PotentialTransformOrRootParent is TTransformNode) or
      (PotentialTransformOrRootParent is TX3DRootNode)
    );
    TransformOrRootParent := PotentialTransformOrRootParent;
    Result := true;
  end;

var
  ChildToPlacement:
    {$ifdef FPC}TChildrenNodesToObjectPlacement.TDictionaryPair
    {$else}TPair<TTransformNode, TIfcObjectPlacement>
    {$endif};
  Child, NewParent: TTransformNode;
  OldParentDirect, OldParentTransformOrRoot: TAbstractGroupingNode;
  SearchObjectPlacementsWithoutNodes: Boolean;
begin
  SearchObjectPlacementsWithoutNodes := true;
  while SearchObjectPlacementsWithoutNodes do
  begin
    SearchObjectPlacementsWithoutNodes := false;
    for ChildToPlacement in ChildrenNodesToObjectPlacement do
    begin
      if not ObjectPlacementToParentNodes.ContainsKey(ChildToPlacement.Value) then
      begin
        AddTransformNodeForObjectPlacement(ChildToPlacement.Value);
        { The dictionaties ChildrenNodesToObjectPlacement,
          ObjectPlacementToParentNodes have been modified (new pair was added)
          by TIfcObjectPlacement.UpdateNode.
          And possibly new TIfcObjectPlacement was discovered, that now needs
          new TTransformNode.
          So break this iteration over ChildrenNodesToObjectPlacement,
          and start searching again. }
        SearchObjectPlacementsWithoutNodes := true;
        Break;
      end;
    end;
  end;

  for ChildToPlacement in ChildrenNodesToObjectPlacement do
  begin
    Child := ChildToPlacement.Key;
    if GetParentNode(Child, OldParentDirect, OldParentTransformOrRoot) then
    begin
      if ObjectPlacementToParentNodes.TryGetValue(ChildToPlacement.Value, NewParent) then
      begin
        if OldParentTransformOrRoot <> NewParent then
        begin
          // This is normal (and even used in examples/ifc/ to attach window to wall), don't log.
          // WritelnLog('IFC', 'Fixing parent of "%s" to be relative to ObjectPlacement GUID "%s" to follow placementRelTo', [
          //   Child.NiceName,
          //   ChildToPlacement.Value.GlobalId
          // ]);
          Child.KeepExistingBegin; // do not free Child because ref count temporarily drops to zero below
          OldParentDirect.RemoveChildren(Child);
          NewParent.AddChildren(Child);
          Child.KeepExistingEnd;
        end;
      end else
        WritelnWarning('IFC', 'Cannot find TTransformNode corresponding to IfcObjectPlacement with GUID "%s", so the placementRelTo is not resolved correctly. This means taht transformation of some objects may be incorrect. ' + 'Please submit a bug, the code *should* have created all necessary TTransformNode nodes.', [
          ChildToPlacement.Value.GlobalId
        ]);
    end else
      WritelnWarning('IFC', 'Cannot find parent of "%s". This means that transformation of some objects may be incorrect due to unresolved placementRelTo. Please submit a bug.', [
        Child.NiceName
      ]);
  end;
end;

function TCastleIfcMapping.CreateRootNode(const Ifc: TIfcFile): TX3DRootNode;
begin
  { Do not do call ShapeToProduct.Clear, as older shapes still exist,
    and (due to how smart TCastleIfcMapping.Update works) may stil exist
    after updating.
    So they should still map to IFC products.
    If we would do

      ShapeToProduct.Clear; // will be reinitialized by BuildNode methods

    now, our NodeToProduct would no longer enable to pick shapes that survived. }

  { Initialize things used only temporarily during Load. }
  ObjectPlacementToParentNodes := TObjectPlacementToParentNodes.Create;
  ChildrenNodesToObjectPlacement := TChildrenNodesToObjectPlacement.Create;
  try
    Result := Ifc.BuildNode(Self);
    FixParentChildRelationships(Result);
  finally
    FreeAndNil(ObjectPlacementToParentNodes);
    FreeAndNil(ChildrenNodesToObjectPlacement);
  end;
end;

procedure TCastleIfcMapping.Load(const Ifc: TIfcFile; const ABaseUrl: String);
begin
  { Node that we don't free previous FRootNode, we don't own it. }
  FBaseUrl := ABaseUrl;
  FRootNode := CreateRootNode(Ifc);
end;

procedure TCastleIfcMapping.Update(const Ifc: TIfcFile);

  function MergeNodesList(const TargetNodes, SourceNodes: TX3DNodeList): Boolean; forward;

  { Merge contents of two nodes, checking also is it possible to merge.
    One or both of the nodes may be @nil.

    When TargetNode and SourceNode are not of the same class
    we return @false (impossible to merge) without any changes.
    Otherwise, we may return @false (if impossible to merge),
    but some undefined changes to TargetNode may be done anyway.
    Or we will make all necessary changes to TargetNode to reflect SourceNode
    and return @true.

    Some of the contents of SourceNode may become referenced from the TargetNode
    structure as an effect of this call.

    When the nodes cannot be merged, returns @false.
    The circumstances of it may change.
    For now, any structural change -> @false.

    TODO:
    In the future, we may add / remove children to reconcile the structural change.
    For now, it would be of little use, as TAbstractGroupingNode.AddChildren
    and the like are not optimized at TCastleSceneCore anyway (they result
    in ChangedAll) so there's little gain from trying to be smart when
    structural changes happen (well, the gain would be "stable node instances",
    but update time would not improve). }
  function MergeNodes(const TargetNode, SourceNode: TX3DNode): Boolean;
  var
    I: Integer;
    TargetField, SourceField: TX3DField;
  begin
    if (TargetNode = nil) <> (SourceNode = nil) then
      Exit(false);

    if TargetNode = nil then
    begin
      Assert(SourceNode = nil);
      Exit(true); // nothing more to do
    end;

    if TargetNode.ClassType <> SourceNode.ClassType then
      Exit(false);

    // load TInlineNode contents before merging
    if TargetNode is TInlineNode then
    begin
      Assert(SourceNode is TInlineNode);
      TInlineNode(TargetNode).LoadInlined(false);
      TInlineNode(SourceNode).LoadInlined(false);
    end;

    // copy TX3DRootNode specials
    if TargetNode is TX3DRootNode then
    begin
      Assert(SourceNode is TX3DRootNode);
      TX3DRootNode(TargetNode).InternalAssignRootNodeProps(TX3DRootNode(SourceNode));
    end;

    // copy TX3DNode stuff
    TargetNode.X3DName := SourceNode.X3DName;
    TargetNode.BaseUrl := SourceNode.BaseUrl;

    // merge VRML 1 children, recursively
    if TargetNode.VRML1ChildrenCount <> SourceNode.VRML1ChildrenCount then
      Exit(false);
    for I := 0 to TargetNode.VRML1ChildrenCount - 1 do
    begin
      if not MergeNodes(TargetNode.VRML1Children[I], SourceNode.VRML1Children[I]) then
        Exit(false);
    end;

    // merge fields; SFNode and MFNode mean merging recursively
    if TargetNode.FieldsCount <> SourceNode.FieldsCount then
      Exit(false);
    for I := 0 to TargetNode.FieldsCount - 1 do
    begin
      TargetField := TargetNode.Fields[I];
      SourceField := SourceNode.Fields[I];
      if TargetField.ClassType <> SourceField.ClassType then
        Exit(false);

      if TargetField is TSFNode then
      begin
        Assert(SourceField is TSFNode);
        if not MergeNodes(TSFNode(TargetField).Value, TSFNode(SourceField).Value) then
          Exit(false);
      end else
      if TargetField is TMFNode then
      begin
        Assert(SourceField is TMFNode);
        if not MergeNodesList(
            TMFNode(TargetField).InternalItems,
            TMFNode(SourceField).InternalItems) then
          Exit(false);
      end else
        TargetNode.Fields[I].Send(SourceNode.Fields[I]);
    end;

    Result := true;
  end;

  { Like MergeNode, but merge all nodes on the lists. }
  function MergeNodesList(const TargetNodes, SourceNodes: TX3DNodeList): Boolean;
  var
    I: Integer;
  begin
    if TargetNodes.Count <> SourceNodes.Count then
      Exit(false);
    for I := 0 to TargetNodes.Count - 1 do
      if not MergeNodes(TargetNodes[I], SourceNodes[I]) then
        Exit(false);
    Result := true;
  end;

  procedure ReplaceRootContents(const TargetRoot, SourceRoot: TX3DRootNode);
  var
    ChildNode: TAbstractChildNode;
    I: Integer;
  begin
    TargetRoot.ClearChildren;
    for I := 0 to SourceRoot.FdChildren.Count - 1 do
    begin
      ChildNode := SourceRoot.FdChildren[I] as TAbstractChildNode;
      TargetRoot.AddChildren(ChildNode);
    end;
    { This is justifiable now when structure changes, e.g. new wall is added.
      Not justifiable when only transformation, like translation, changes. }
    // WritelnLog('IFC', 'TCastleIfcMapping.Update fallback on unoptimal implementation: Replaced root contents');
  end;

var
  NewRootNode: TX3DRootNode;
begin
  NewRootNode := CreateRootNode(Ifc);
  try
    if not MergeNodes(FRootNode, NewRootNode) then
      ReplaceRootContents(FRootNode, NewRootNode);
  finally FreeAndNil(NewRootNode) end;
end;

procedure TCastleIfcMapping.ShapeDestroyed(const Node: TX3DNode);
begin
  ShapeToProduct.Remove(Node as TAbstractShapeNode);
end;

procedure TCastleIfcMapping.ConnectShapeAndProduct(const Shape: TAbstractShapeNode;
  const Product: TIfcProduct);
begin
  if Product = nil then Exit;
  Shape.AddDestructionNotification({$ifdef FPC}@{$endif} ShapeDestroyed);
  ShapeToProduct.AddOrSetValue(Shape, Product);
end;

function TCastleIfcMapping.NodeToProduct(const Node: TAbstractShapeNode): TIfcProduct;
begin
  if not ShapeToProduct.TryGetValue(Node, Result) then
    Result := nil;
end;

function TCastleIfcMapping.ProductToNodes(const Product: TIfcProduct): TX3DNodeList;
var
  Pair: {$ifdef FPC} TShapeToProduct.TDictionaryPair {$else} TPair<TAbstractShapeNode, TIfcProduct> {$endif};
begin
  Result := TX3DNodeList.Create(false);
  for Pair in ShapeToProduct do
    if Pair.Value = Product then
      Result.Add(Pair.Key);
end;

{ BuildNode methods implementations ----------------------------------------- }

function TIfcFile.BuildNode(const Mapping: TCastleIfcMapping): TX3DRootNode;
begin
  Result := TX3DRootNode.Create('', Mapping.BaseUrl);
  SetMetadata(Result, 'IFC_Version', Version);
  SetMetadata(Result, 'IFC_SchemaIdentifier', SchemaIdentifier);
  SetMetadata(Result, 'IFC_OriginatingSystem', OriginatingSystem);
  SetMetadata(Result, 'IFC_PreprocessorVersion', PreprocessorVersion);
  SetMetadata(Result, 'IFC_TimeStamp', TimeStamp);

  Result.AddChildren(Project.BuildNode(Mapping));
end;

function TIfcRepresentationItemList.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  RepresentationItem: TIfcRepresentationItem;
  Group: TGroupNode;
  Child: TAbstractChildNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ListOfChildren', ClassOfListChildren(Self).ClassName);
  Result := Group;

  for RepresentationItem in Self do
  begin
    Child := RepresentationItem.BuildNode(Mapping, ParentProduct,
      LitAppearance, UnlitAppearance);
    if Child <> nil then
      Group.AddChildren(Child);
  end;
end;

function TIfcRepresentationList.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  Representation: TIfcRepresentation;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ListOfChildren', ClassOfListChildren(Self).ClassName);
  Result := Group;

  for Representation in Self do
    Group.AddChildren(Representation.Items.BuildNode(Mapping, ParentProduct,
      LitAppearance, UnlitAppearance));
end;

function TIfcProductRepresentation.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  Group.AddChildren(Representations.BuildNode(Mapping, ParentProduct,
    LitAppearance, UnlitAppearance));
end;

function TIfcRelAggregates.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  RelatedObject: TIfcObjectDefinition;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  for RelatedObject in RelatedObjects do
    Group.AddChildren(RelatedObject.BuildNode(Mapping));
end;

function TIfcRelContainedInSpatialStructure.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  Product: TIfcProduct;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  for Product in RelatedElements do
    Group.AddChildren(Product.BuildNode(Mapping));
end;

function TIfcRelVoidsElement.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  Result := Group;

  if RelatedOpeningElement <> nil then
    Group.AddChildren(RelatedOpeningElement.BuildNode(Mapping));
end;

procedure TIfcCartesianPointList2D.UpdateCoordinate(const Coord: TCoordinateNode);
var
  I: Integer;
begin
  Coord.FdPoint.Count := CoordList.Count;
  for I := 0 to CoordList.Count - 1 do
    Coord.FdPoint.Items[I] := Vector3(CoordList[I], 0);
end;

procedure TIfcCartesianPointList3D.UpdateCoordinate(const Coord: TCoordinateNode);
// var
//   I: Integer;
begin
  Coord.FdPoint.Items.Assign(CoordList);
  // Coord.FdPoint.Count := CoordList.Count;
  // for I := 0 to CoordList.Count - 1 do
  //   Coord.FdPoint.Items[I] := CoordList[I];
end;

function TIfcGridAxis.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
begin
  if AxisCurve <> nil then
    Result := AxisCurve.BuildNode(Mapping, ParentProduct,
      LitAppearance, UnlitAppearance)
  else
    Result := nil;
end;

{ TIfcProfileDef.UpdateCrossSection method overrides -------------------------- }

procedure TIfcProfileDef.UpdateCrossSection(
  const CrossSection: TVector2List; var CrossSectionConvex: Boolean;
  const ParentName: String);
begin
  WritelnWarning('IFC', 'Unsupported class used for %s: %s', [
    ParentName,
    ClassName
  ]);
end;

procedure TIfcArbitraryClosedProfileDef.UpdateCrossSection(
  const CrossSection: TVector2List; var CrossSectionConvex: Boolean;
  const ParentName: String);
begin
  if OuterCurve <> nil then
    OuterCurve.UpdateCrossSection(CrossSection, CrossSectionConvex);
end;

procedure TIfcArbitraryProfileDefWithVoids.UpdateCrossSection(
  const CrossSection: TVector2List; var CrossSectionConvex: Boolean;
  const ParentName: String);
{.$define CASTLE_DEBUG_IFC_CURVE_HOLES}
{$ifdef CASTLE_DEBUG_IFC_CURVE_HOLES}
var
  I: Integer;
  NewCrossSection: TVector2List;
{$endif}
begin
  inherited;
  if InnerCurves.Count <> 0 then
  begin
    WritelnWarning('IFC', 'TODO: IfcArbitraryProfileDefWithVoids.InnerCurves not implemented, we do not create holes');

    {$ifdef CASTLE_DEBUG_IFC_CURVE_HOLES}
    // TODO: add holes, set also CrossSectionConvex:=false
    for I := 0 to InnerCurves.Count - 1 do
    begin
      NewCrossSection := TVector2List.Create;
      try
        InnerCurves[I].UpdateCrossSection(NewCrossSection);
        CrossSection.AddRange(NewCrossSection);
      finally FreeAndNil(NewCrossSection) end;
    end;
    {$endif}
  end;
end;

procedure TIfcRectangleProfileDef.UpdateCrossSection(
  const CrossSection: TVector2List; var CrossSectionConvex: Boolean;
  const ParentName: String);
var
  Translation: TVector3;
  Rotation: TVector4;
  V: TVector3;
  I: Integer;
begin
  CrossSection.AddRange([
    Vector2(-XDim / 2, -YDim / 2),
    Vector2( XDim / 2, -YDim / 2),
    Vector2( XDim / 2,  YDim / 2),
    Vector2(-XDim / 2,  YDim / 2),
    // repeat 1st point to close
    // testcase: https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/annex_e/mapped-geometric-shape/mapped-shape-with-transformation.html
    Vector2(-XDim / 2, -YDim / 2)
  ]);

  if Position <> nil then
  begin
    { Testcase with TIfcParameterizedProfileDef.Position:
      Community-Sample-Test-Files-main/IFC 4.0.2.1 (IFC 4)/ISO Spec archive/wall-standard-case.ifcjson }
    Position.GetTransform(Translation, Rotation);
    for I := 0 to CrossSection.Count - 1 do
    begin
      V := Vector3(CrossSection[I], 0);
      V := RotatePointAroundAxis(Rotation, V);
      V := V + Translation;
      CrossSection[I] := V.XY;
    end;
  end;
end;

{ TIfcCurve.UpdateCrossSection method overrides ---------------------------------------- }

procedure TIfcCurve.UpdateCrossSection(const CrossSection: TVector2List;
  var CrossSectionConvex: Boolean);

  procedure HandleIndexedLineSetNode(const IndexedLineSet: TIndexedLineSetNode);
  var
    Coord: TCoordinateNode;
    I, LastIndex, Index: Integer;
    ConnectNextPointToPreviousSegment: Boolean;
  begin
    Coord := IndexedLineSet.Coord as TCoordinateNode;
    Assert(Coord <> nil); // these X3D nodes are build by us, so we know they have Coord
    LastIndex := -1;
    ConnectNextPointToPreviousSegment := false;
    for I := 0 to IndexedLineSet.FdCoordIndex.Count - 1 do
    begin
      Index := IndexedLineSet.FdCoordIndex.Items[I];
      if Index < 0 then
      begin
        { Negative index *does not* imply that we should start a new cross-section.

          All the segments of IfcIndexedPolyCurve
          ( https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedPolyCurve.htm )
          are supposed to be connected, and IfcSweptAreaSolid has
          *one* cross-section, not many
          ( https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptAreaSolid.htm
          """The swept area is defined by a cross section,
          represented by a subtype if IfcProfileDef, that is provided as
          a closed two-dimensional boundary on an implicit plane. """

          So we just make sure that segments indeed connect, and then
          we generate a single CrossSection as a list of points. }
        ConnectNextPointToPreviousSegment := true;
      end else
      begin
        if ConnectNextPointToPreviousSegment and (LastIndex <> -1) then
        begin
          if Index <> LastIndex then
            WritelnWarning('Cross-section segments, from IfcIndexedPolyCurve, are not connected. This is either invalid IFC (disconnected IfcIndexedPolyCurve segments) or a bug in our code.');
          ConnectNextPointToPreviousSegment := false;
          // ignore this point, as it should equal to the last point
        end else
        begin
          Assert(Index < Coord.FdPoint.Count);
          CrossSection.Add(Coord.FdPoint.Items[Index].XY);
          LastIndex := Index;
        end;
      end;
    end;
  end;

  procedure HandleLineSetNode(const LineSet: TLineSetNode);
  var
    Coord: TCoordinateNode;
    I: Integer;
  begin
    Coord := LineSet.Coord as TCoordinateNode;
    for I := 0 to Coord.FdPoint.Count - 1 do
      CrossSection.Add(Coord.FdPoint.Items[I].XY);
  end;

  { Returns @false if Geometry class was not handled or was @nil. }
  function HandleGeometry(const Geometry: TAbstractGeometryNode;
    const ResultingCrossSectionWillBeConvex: Boolean): Boolean;
  begin
    if Geometry is TIndexedLineSetNode then
    begin
      Result := true;
      CrossSectionConvex := ResultingCrossSectionWillBeConvex;
      HandleIndexedLineSetNode(TIndexedLineSetNode(Geometry))
    end else
    if Geometry is TLineSetNode then
    begin
      Result := true;
      CrossSectionConvex := ResultingCrossSectionWillBeConvex;
      HandleLineSetNode(TLineSetNode(Geometry))
    end else
      Result := false;
  end;

  procedure HandleShape(const Shape: TShapeNode);
  var
    ProxyState: TX3DGraphTraverseState;
    ProxyGeometry: TAbstractGeometryNode;
    OriginalGeometryConvex: Boolean;
  begin
    if Shape.Geometry <> nil then
    begin
      if not HandleGeometry(Shape.Geometry, false) then
      begin
        { Try using Proxy and handling this.
          This allows us to deal e.g. with TCircle2DNode,
          which in turn appears from IfcCircle, IfcEllipse. }
        ProxyState := TX3DGraphTraverseState.Create;
        ProxyState.ShapeNode := Shape;
        ProxyGeometry := Shape.Geometry.Proxy(ProxyState);
        { When the geometry is known to be convex,
          keep CrossSectionConvex = true.
          This allows to avoid costly triangulation of IfcExtrudedAreaSolid
          top/bottom caps, which is costly but also can remove some
          almost-degenerate triangles (testcase: thin triangles
          at the bottom of table legs in
          https://github.com/castle-engine/demo-models/blob/master/ifc/created_using_bonsaibim/chairs_rotations_from_furniture_library.ifcjson ) }
        OriginalGeometryConvex :=
          (Shape.Geometry is TCircle2DNode) or
          (Shape.Geometry is TRectangle2DNode);
        if ProxyGeometry <> nil then
        begin
          if not HandleGeometry(ProxyGeometry, OriginalGeometryConvex) then
            WritelnWarning('IFC', 'Unsupported geometry class for TIfcCurve.UpdateCrossSection: %s, proxy to %s', [
              Shape.Geometry.ClassName,
              ProxyGeometry.ClassName
            ]);
        end else
        begin
          WritelnWarning('IFC', 'Unsupported geometry class for TIfcCurve.UpdateCrossSection: %s, no proxy', [
            Shape.Geometry.ClassName
          ]);
        end;
      end;
    end;
  end;

  procedure TransformCrossSection(const Transform: TTransformNode; const CrossSection: TVector2List);
  var
    T: TTransformation;
    I: Integer;
    V: TVector3;
  begin
    T.Init;
    Transform.TransformFunctionality.ApplyTransform(T);
    for I := 0 to CrossSection.Count - 1 do
    begin
      V := Vector3(CrossSection[I], 0);
      V := T.Transform.MultPoint(V);
      CrossSection[I] := V.XY;
    end;
  end;

var
  Mapping: TCastleIfcMapping;
  ShapeOrTransform: TAbstractChildNode;
  Transform: TTransformNode;
begin
  Mapping := TCastleIfcMapping.Create;
  try
    ShapeOrTransform := BuildNode(Mapping, nil, nil, nil);
    try
      if ShapeOrTransform is TShapeNode then
        HandleShape(TShapeNode(ShapeOrTransform))
      else
      if (ShapeOrTransform is TTransformNode) and
         (TTransformNode(ShapeOrTransform).FdChildren.Count = 1) and
         (TTransformNode(ShapeOrTransform).FdChildren[0] is TShapeNode) then
      begin
        Transform := TTransformNode(ShapeOrTransform);
        HandleShape(TShapeNode(Transform.FdChildren[0]));
        TransformCrossSection(Transform, CrossSection);
      end else
        WritelnWarning('IFC', 'Unsupported class for TIfcCurve.UpdateCrossSection: %s', [
          ShapeOrTransform.ClassName
        ]);
    finally FreeIfUnusedAndNil(ShapeOrTransform) end;
  finally FreeAndNil(Mapping) end;
end;

procedure TIfcPolyline.UpdateCrossSection(const CrossSection: TVector2List;
  var CrossSectionConvex: Boolean);
var
  I: Integer;
begin
  CrossSectionConvex := false;
  CrossSection.Count := Points.Count;
  for I := 0 to Points.Count - 1 do
    CrossSection[I] := Points[I].Coordinates.Value.XY;
end;

{ BuildNode of TIfcRepresentationItem and descendants ------------------------- }

function TIfcRepresentationItem.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
begin
  Result := nil;
end;

function TIfcPolyline.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  Shape: TShapeNode;
  LineSet: TLineSetNode;
  Coord: TCoordinateNode;
  I: Integer;
begin
  Coord := TCoordinateNode.Create('', Mapping.BaseUrl);
  Coord.FdPoint.Count := Points.Count;
  for I := 0 to Points.Count - 1 do
    Coord.FdPoint.Items[I] := Points[I].Coordinates.Value;

  LineSet := TLineSetNode.Create('', Mapping.BaseUrl);
  LineSet.Coord := Coord;
  LineSet.SetVertexCount([Points.Count]);

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := LineSet;
  Shape.Appearance := UnlitAppearance;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
  Result := Shape;
end;

function TIfcTrimmedCurve.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
begin
  if BasisCurve <> nil then
  begin
    WritelnWarning('TODO: IfcTrimmedCurve is only partially supported now. We ignore trim1, trim2, we display whole curve.');
    Result := BasisCurve.BuildNode(Mapping, ParentProduct,
      LitAppearance, UnlitAppearance);
  end else
    Result := nil;
end;

function TIfcConic.BuildEllipseOrCircle(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode;
  const Radius, RadiusY: Single): TAbstractChildNode;

  { Larger radius -> more slices, better quality circle.
    Since IFC files don't use scaling (except IfcMappedItem),
    this makes sense, as circle radius corresponds to how large
    it is usually on screen. }
  function SlicesFromRadius(const R: Single): Integer;
  begin
    if R < 1 then
      Result := Round(MapRangeClamped(R, 0.25, 1, 16, 32))
    else
      Result := Round(MapRangeClamped(R, 1, 5, 32, 64));
    // WritelnLog('Radius %f results in circle slices %d', [
    //   R,
    //   Result
    // ]);
  end;

var
  Transform: TTransformNode;
  Translation: TVector3;
  Rotation: TVector4;
  Shape: TShapeNode;
  Circle: TCircle2DNode;
begin
  Circle := TCircle2DNode.Create('', Mapping.BaseUrl);
  Circle.Slices := SlicesFromRadius(Max(Radius, RadiusY));
  Circle.Radius := Radius;
  Circle.RadiusY := RadiusY;

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := Circle;
  Shape.Appearance := UnlitAppearance;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);

  { We optimize below common case, when IfcConic doesn't need extra
    TTransformNode (this also means we don't need later to
    TransformCrossSection in TIfcCurve.UpdateCrossSection). }
  if Position <> nil then
  begin
    Position.GetTransform(Translation, Rotation);
    if (not Translation.IsZero) or (not IsZero(Rotation.W)) then
    begin
      Transform := TTransformNode.Create('', Mapping.BaseUrl);
      Transform.Translation := Translation;
      Transform.Rotation := Rotation;
      Transform.AddChildren(Shape);
      Result := Transform;
    end else
    begin
      {$ifdef CASTLE_DEBUG_IFC_TRANSFORM}
      WritelnLog('IFC', 'Optimizing away TTransformNode for IfcConic.Position');
      {$endif CASTLE_DEBUG_IFC_TRANSFORM}
      Result := Shape;
    end;
  end else
    Result := Shape;
end;

function TIfcEllipse.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
begin
  Result := BuildEllipseOrCircle(Mapping, ParentProduct,
    LitAppearance, UnlitAppearance, SemiAxis1, SemiAxis2);
end;

function TIfcCircle.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
begin
  Result := BuildEllipseOrCircle(Mapping, ParentProduct,
    LitAppearance, UnlitAppearance, Radius, 0);
end;

function TIfcIndexedPolyCurve.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;

  { Add integers for NewIndexes to Indexes, decrementing all values
    on NewIndexes by 1.
    Useful as IFC indices are 1-based, X3D is 0-based.
    So instead of

      IndexedLineSet.FdCoordIndex.Items.AddRange(LineIndex.Value);

    you must do

      AddRangeDecrement(IndexedLineSet.FdCoordIndex.Items, LineIndex.Value);
  }
  procedure AddRangeDecrement(const Indexes: TInt32List; const NewIndexes: TIntegerList);
  var
    I: Integer;
  begin
    for I := 0 to NewIndexes.Count - 1 do
      Indexes.Add(NewIndexes[I] - 1);
  end;

var
  Shape: TShapeNode;
  IndexedLineSet: TIndexedLineSetNode;
  LineSet: TLineSetNode;
  Coord: TCoordinateNode;
  Segment: TIfcSegmentIndexSelect;
  LineIndex: TIfcLineIndex;
  ArcIndex: TIfcArcIndex;
begin
  Coord := TCoordinateNode.Create('', Mapping.BaseUrl);
  Points.UpdateCoordinate(Coord);

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Appearance := UnlitAppearance;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  if Segments.Count = 0 then
  begin
    { IFC spec says that Segments are optional:
      "In the case that the list of Segments is not provided,
      all points in the IfcCartesianPointList are connected by straight
      line segments in the order they appear in the IfcCartesianPointList." }

    LineSet := TLineSetNode.Create('', Mapping.BaseUrl);
    LineSet.Coord := Coord;
    LineSet.SetVertexCount([Coord.FdPoint.Count]);
    Shape.Geometry := LineSet;
  end else
  begin
    IndexedLineSet := TIndexedLineSetNode.Create('', Mapping.BaseUrl);
    IndexedLineSet.Coord := Coord;
    IndexedLineSet.Mode := lmStrip;
    Shape.Geometry := IndexedLineSet;

    for Segment in Segments do
    begin
      if Segment is TIfcLineIndex then
      begin
        LineIndex := TIfcLineIndex(Segment);
        AddRangeDecrement(IndexedLineSet.FdCoordIndex.Items, LineIndex.Value);
        IndexedLineSet.FdCoordIndex.Items.Add(-1);
      end else
      if Segment is TIfcArcIndex then
      begin
        ArcIndex := TIfcArcIndex(Segment);
        AddRangeDecrement(IndexedLineSet.FdCoordIndex.Items, ArcIndex.Value);
        IndexedLineSet.FdCoordIndex.Items.Add(-1);
      end else
        WritelnWarning('IFC', 'Unsupported Segment class: ' + Segment.ClassName);
    end;
  end;

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
  Result := Shape;
end;

function TIfcExtrudedAreaSolid.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;

  procedure AddGeometry(const Transform: TTransformNode; const Geometry: TAbstractGeometryNode);
  var
    Shape: TShapeNode;
  begin
    Shape := TShapeNode.Create('', Mapping.BaseUrl);
    Shape.Geometry := Geometry;
    Shape.Appearance := LitAppearance;
    SetMetadata(Shape, 'IFC_ClassName', ClassName);
    Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
    Transform.AddChildren(Shape);
  end;

  { TCoordinateNode created here is shared by 2 X3D geometries,
    use nice unique name for it to allow saving to X3D with DEF/USE sharing. }
  function CoordinateX3DName: String;
  begin
    Result := 'ExtrudedAreaSolid_Coordinate_';
    if GlobalId <> '' then
      Result := Result + GlobalId
    else
      Result := Result + RandomGuidIfc;
  end;

var
  SideQuads: TIndexedQuadSetNode;
  TopBottomCaps: TIndexedFaceSetNode;
  Coord: TCoordinateNode;
  CrossSection: TVector2List;
  CrossSectionConvex: Boolean;
  Transform: TTransformNode;
  ExtrusionDir, Translation: TVector3;
  Rotation: TVector4;
  C, I: Integer;
begin
  { Note: Not using TExtrusionNode, as it tries to rotate the cross-section
    around spine, resulting in various errors e.g. when DirectionRatios is -Z.

    Instead we build geometry on our side:
    - quads for sides
    - polygons (potentially concave) for top and bottom caps

    Testcases:
    - ifc_window_test/4_rotations_scale/test_relative_positions_orientations*.ifcjson
      ifc_window_test/4_rotations_scale/move_edit_mode_test.ifcjson

      Reproduce by moving the rectangle of slab in *edit mode* in BonsaiBIM.
      If we would use TExtrusionNode, the X movement is OK, the Y is inverted.
      In the past we had an ugly hack for this, that worked in some cases:
      negate Y coordinates of cross-section points.
      BonsaiBIM translates this into moving actual points on the list.

      With new approach -> no hacks, this testcase is handled correctly.

    - ifc_window_test/5_window_by_bonsaibim/window_simple.ifcjson
      ifc_window_test/5_window_by_bonsaibim/window_rotated.ifcjson

      Reproduce by adding default window geometry from BonsaiBIM.
      If we would use TExtrusionNode, there's weird rotation for a part of the
      window, and we needed to counter this by ugly hack
      "Rotation.W := -Rotation.W;"

      With new approach -> no hacks, this testcase is handled correctly.
      We apply rotation in a regular way.

    - ifc_window_test/8_link/chairs_rotations.ifcjson

      Reproduce by adding a furniture from BonsaiBIM library,
      with circular table and 14 chairs around.
      Each chair has 6 extrusions, 4 legs (with DirectionRatios.Z positive)
      and 2 other quads (1 for sitting, 1 for chair back,
      with DirectionRatios.Z negative).

      If we would use TExtrusionNode... this just seems too uncontrollable
      to do correctly.
      OTOH, it's easy with current approach, we move points by ExtrusionDir,
      and this is just correct for any ExtrusionDir.
  }

  SideQuads := TIndexedQuadSetNode.Create('', Mapping.BaseUrl);
  TopBottomCaps := TIndexedFaceSetNode.Create('', Mapping.BaseUrl);

  { Default value results in more efficient rendering, and matches
    TIndexedFaceSetNode.Convex default. }
  CrossSectionConvex := true;

  if SweptArea <> nil then
  begin
    CrossSection := TVector2List.Create;
    try
      SweptArea.UpdateCrossSection(CrossSection, CrossSectionConvex,
        'IfcExtrudedAreaSolid.SweptArea');
      ExtrusionDir := ExtrudedDirection.DirectionRatios.Value * Depth;

      C := CrossSection.Count;
      if C <> 0 then
      begin
        Coord := TCoordinateNode.Create(CoordinateX3DName, Mapping.BaseUrl);
        Coord.FdPoint.Count := C * 2;
        for I := 0 to C - 1 do
        begin
          Coord.FdPoint.Items[I * 2    ] := Vector3(CrossSection[I], 0);
          Coord.FdPoint.Items[I * 2 + 1] := Vector3(CrossSection[I], 0) + ExtrusionDir;
        end;

        // both sides and top/bottom caps reuse the same TCoordinateNode
        SideQuads.Coord := Coord;
        TopBottomCaps.Coord := Coord;

        SideQuads.FdIndex.Count := (C - 1) * 4;
        for I := 0 to C - 2 do
        begin
          SideQuads.FdIndex.Items[I * 4    ] := I * 2;
          SideQuads.FdIndex.Items[I * 4 + 1] := (I + 1) * 2;
          SideQuads.FdIndex.Items[I * 4 + 2] := (I + 1) * 2 + 1;
          SideQuads.FdIndex.Items[I * 4 + 3] := I * 2 + 1;
        end;

        // show caps only if CrossSection is closed
        if (C > 1) and
           (TVector2.PerfectlyEquals(CrossSection[0], CrossSection[C - 1])) then
        begin
          TopBottomCaps.FdCoordIndex.Count := (C - 1) * 2 + 1;
          // indxed dividing between top and bottom caps
          TopBottomCaps.FdCoordIndex.Items[C - 1] := -1;
          for I := 0 to C - 2 do
          begin
            TopBottomCaps.FdCoordIndex.Items[I] := I * 2;
            TopBottomCaps.FdCoordIndex.Items[I + C] := I * 2 + 1;
          end;
        end;
      end;
    finally FreeAndNil(CrossSection) end;
  end;

  { The extrusion curves in IFC may be provided in an arbitrary order.
    We have samples where both extrusions use TIfcCurve.UpdateCrossSection,
    and sometimes it would need (with Solid=true) to do CrossSection.Reverse,
    sometimes not.

    Testcases made using BonsaiBIM, add slabs and walls, and save to IFC JSON:
      ifc_window_test/4_rotations_scale/simplest_slab.ifcjson
      ifc_window_test/4_rotations_scale/blend_simple_rotations_and_scale.ifcjson
      ifc_window_test/4_rotations_scale/rotate_scale_slabs.ifcjson

    The simplest solution to render them all OK seems to just disable
    backface culling for all extrusions. }
  SideQuads.Solid := false;
  TopBottomCaps.Solid := false;

  // don't smooth normals, as extrusions may have sharp edges, e.g. rectangular chair legs
  SideQuads.NormalPerVertex := false;
  TopBottomCaps.CreaseAngle := 0;

  // top and bottom caps may be concave if CrossSectionConvex=false
  TopBottomCaps.Convex := CrossSectionConvex;

  Transform := TTransformNode.Create('', Mapping.BaseUrl);

  // handle TIfcSweptAreaSolid.Position (this is a transformation, may contain rotation too)
  if Position <> nil then
  begin
    Position.GetTransform(Translation, Rotation);
    {$ifdef CASTLE_DEBUG_IFC_TRANSFORM}
    WritelnLog('IfcExtrudedAreaSolid.Position translation %s, rotation %s', [Translation.ToString, Rotation.ToString]);
    {$endif CASTLE_DEBUG_IFC_TRANSFORM}
    Transform.Translation := Translation;
    Transform.Rotation := Rotation;
  end;

  AddGeometry(Transform, SideQuads);
  AddGeometry(Transform, TopBottomCaps);

  Result := Transform;
end;

function TIfcPolygonalFaceSet.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  IndexedFaceSet: TIndexedFaceSetNode;
  Coord: TCoordinateNode;
  Face: TIfcIndexedPolygonalFace;
  I, Index: Integer;
  Shape: TShapeNode;
begin
  Coord := TCoordinateNode.Create('', Mapping.BaseUrl);
  if Coordinates <> nil then
    Coord.SetPoint(Coordinates.CoordList);

  IndexedFaceSet := TIndexedFaceSetNode.Create('', Mapping.BaseUrl);
  IndexedFaceSet.Coord := Coord;
  IndexedFaceSet.Solid := Closed;

  // do not smooth, we don't know how smooth is the mesh, using flat shading is safer
  IndexedFaceSet.CreaseAngle := 0;

  for Face in Faces do
  begin
    for I := 0 to Face.CoordIndex.Count - 1 do
    begin
      Index := Face.CoordIndex[I];
      if Index < PnIndex.Count then
        Index := PnIndex[Index];
      Dec(Index); // IFC indices are 1-based
      IndexedFaceSet.FdCoordIndex.Items.Add(Index);
    end;
    IndexedFaceSet.FdCoordIndex.Items.Add(-1);
  end;

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := IndexedFaceSet;
  Shape.Appearance := LitAppearance;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
  Result := Shape;
end;

function TIfcTriangulatedFaceSet.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  IndexedTriangleSet: TIndexedTriangleSetNode;
  Coord: TCoordinateNode;
  I, Index, TriangleIndex: Integer;
  Shape: TShapeNode;
begin
  Coord := TCoordinateNode.Create('', Mapping.BaseUrl);
  if Coordinates <> nil then
    Coord.SetPoint(Coordinates.CoordList);

  IndexedTriangleSet := TIndexedTriangleSetNode.Create('', Mapping.BaseUrl);
  IndexedTriangleSet.Coord := Coord;
  IndexedTriangleSet.Solid := Closed;
  { Do not smooth, we don't know how smooth is the mesh,
    using flat shading is safest.
    And consistent with TIfcExtrudedAreaSolid.BuildNode. }
  IndexedTriangleSet.NormalPerVertex := false;

  // TODO: Use Normals

  IndexedTriangleSet.FdIndex.Count := CoordIndex.Count * 3;
  for TriangleIndex := 0 to CoordIndex.Count - 1 do
  begin
    for I := 0 to 2 do
    begin
      Index := CoordIndex[TriangleIndex][I];
      if Index < PnIndex.Count then
        Index := PnIndex[Index];
      { Ignore invalid indexes, like equal 0 or even less.
        They would cause us to add negative index to IndexedTriangleSet.FdIndex,
        which is invalid. }
      if Index < 1 then Continue;
      Dec(Index); // IFC indices are 1-based
      IndexedTriangleSet.FdIndex.Items[TriangleIndex * 3 + I] := Index;
    end;
  end;

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := IndexedTriangleSet;
  Shape.Appearance := LitAppearance;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
  Result := Shape;
end;

function TIfcBoundingBox.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  Box: TBoxNode;
  Shape: TShapeNode;
  Transform: TTransformNode;
  CornerPoint: TVector3;
begin
  Box := TBoxNode.Create('', Mapping.BaseUrl);
  Box.Size := Vector3(XDim, YDim, ZDim);

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := Box;
  Shape.Appearance := UnlitAppearance;

  { Set IFC_ClassName on Shape, not Transform.
    This is consistent with other TShapeNode creation,
    and allows easy IfcDebugDisplayToggleShape implementation in examples/ifc/. }
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  // bounding box is not the final geometry, so show only wireframe
  Shape.Shading := shWireframe;

  // move the box by CornerPoint + (X3D box position) Box.Size / 2
  CornerPoint := TVector3.Zero;
  if Corner <> nil then
    CornerPoint := Corner.Coordinates.Value;

  Transform := TTransformNode.Create('', Mapping.BaseUrl);
  Transform.Translation := CornerPoint + Box.Size / 2;
  Transform.AddChildren(Shape);

  Mapping.ConnectShapeAndProduct(Shape, ParentProduct);
  Result := Transform;
end;

function TIfcMappedItem.BuildNode(const Mapping: TCastleIfcMapping;
  const ParentProduct: TIfcProduct;
  const LitAppearance, UnlitAppearance: TAppearanceNode): TAbstractChildNode;
var
  TransformMappingTarget, TransformMappingOrigin: TTransformNode;
  OriginTranslation, TargetTranslation: TVector3;
  OriginRotation, TargetRotation: TVector4;
  TargetScale: TVector3;
begin
  // create TransformMappingTarget that expresses IFC MappingTarget transformation
  TransformMappingTarget := TTransformNode.Create('', Mapping.BaseUrl);
  SetMetadata(TransformMappingTarget, 'IFC_ClassName', ClassName);
  SetMetadata(TransformMappingTarget, 'IFC_MappedItem_Purpose', 'MappingTarget');
  if MappingTarget <> nil then
  begin
    MappingTarget.GetTransform(TargetTranslation, TargetRotation, TargetScale);
    {$ifdef CASTLE_DEBUG_IFC_TRANSFORM}
    WritelnLog('MappingTarget is translation %s, rotation %s', [TargetTranslation.ToString, TargetRotation.ToString]);
    {$endif CASTLE_DEBUG_IFC_TRANSFORM}
    TransformMappingTarget.Translation := TargetTranslation;
    TransformMappingTarget.Rotation := TargetRotation;
    TransformMappingTarget.Scale := TargetScale;
  end;

  if MappingSource <> nil then
  begin
    // create TransformMappingOrigin that expresses negated IFC MappingSource.MappingOrigin transformation
    TransformMappingOrigin := TTransformNode.Create('', Mapping.BaseUrl);
    SetMetadata(TransformMappingOrigin, 'IFC_ClassName', ClassName);
    SetMetadata(TransformMappingOrigin, 'IFC_MappedItem_Purpose', 'MappingSource.MappingOrigin');
    if MappingSource.MappingOrigin <> nil then
    begin
      MappingSource.MappingOrigin.GetTransform(OriginTranslation, OriginRotation);
      {$ifdef CASTLE_DEBUG_IFC_TRANSFORM}
      WritelnLog('MappingOrigin is translation %s, rotation %s', [OriginTranslation.ToString, OriginRotation.ToString]);
      {$endif CASTLE_DEBUG_IFC_TRANSFORM}
      TransformMappingOrigin.Translation := -OriginTranslation;
      TransformMappingOrigin.Rotation := Vector4(OriginRotation.XYZ, -OriginRotation.W);
    end;
    TransformMappingTarget.AddChildren(TransformMappingOrigin);

    // add objects from MappingSource.MappedRepresentation to TransformMappingOrigin
    if MappingSource.MappedRepresentation <> nil then
    begin
      TransformMappingOrigin.AddChildren(
        MappingSource.MappedRepresentation.Items.BuildNode(Mapping, ParentProduct,
          LitAppearance, UnlitAppearance));
    end;
  end;

  Result := TransformMappingTarget;
end;

{ BuildNode of TIfcObjectDefinition and descendants -------------------------- }

function TIfcObjectDefinition.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
var
  RelAggregates: TIfcRelAggregates;
begin
  Result := TTransformNode.Create('', Mapping.BaseUrl);
  SetMetadata(Result, 'IFC_ClassName', ClassName);
  SetMetadata(Result, 'IFC_Name', Name);
  SetMetadata(Result, 'IFC_Description', Description);

  for RelAggregates in IsDecomposedBy do
    Result.AddChildren(RelAggregates.BuildNode(Mapping));
end;

function TIfcSpatialElement.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
var
  RelContainedInSpatialStructure: TIfcRelContainedInSpatialStructure;
begin
  Result := inherited;
  for RelContainedInSpatialStructure in ContainsElements do
    Result.AddChildren(RelContainedInSpatialStructure.BuildNode(Mapping));
end;

function TIfcProduct.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;

  { Derive X3D TAppearanceNode from IFC TIfcObjectDefinition.HasAssociations. }
  function AppearanceFromAssociations(const Associations: TIfcRelAssociatesList): TAppearanceNode;
  begin
    (*
    for Rel in Associations do
    begin
      if Rel is TIfcRelAssociatesMaterial then
      begin
        RelMat := TIfcRelAssociatesMaterial(Rel);
        Result := TAppearanceNode.Create('', Mapping.BaseUrl);
        Result.Material := TPhysicalMaterialNode.Create('', Mapping.BaseUrl);
        // we would like to set Result.Material.BaseColor based on IFC information,
        // but it seems IFC materials don't specify simple color
        Exit;
      end;
    *)

    Result := TAppearanceNode.Create('', Mapping.BaseUrl);
    //Result.Material := TPhysicalMaterialNode.Create('', Mapping.BaseUrl);
    // Phong material looks better alone, with default color and lighting
    Result.Material := TMaterialNode.Create('', Mapping.BaseUrl);
  end;

  { Unlit appearance, good for unlit thigns -- lines, bboxes etc. }
  function CreateDefaultUnlitAppearance: TAppearanceNode;
  var
    UnlitMaterial: TUnlitMaterialNode;
  begin
    UnlitMaterial := TUnlitMaterialNode.Create('', Mapping.BaseUrl);
    UnlitMaterial.EmissiveColor := WhiteRgb;
    UnlitMaterial.Transparency := 0.5;

    Result := TAppearanceNode.Create('', Mapping.BaseUrl);
    Result.Material := UnlitMaterial;
  end;

var
  LitAppearance, UnlitAppearance: TAppearanceNode;
begin
  Result := inherited;

  if ObjectPlacement <> nil then
    ObjectPlacement.UpdateNode(Mapping, Result);

  if Representation <> nil then
  begin
    // Things inside are affected by given appearances
    LitAppearance := AppearanceFromAssociations(HasAssociations);
    try
      UnlitAppearance := CreateDefaultUnlitAppearance;
      try
        Result.AddChildren(Representation.BuildNode(Mapping, Self,
          LitAppearance, UnlitAppearance));
      finally FreeIfUnusedAndNil(UnlitAppearance) end;
    finally FreeIfUnusedAndNil(LitAppearance) end;
  end;
end;

function TIfcElement.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;

  { For all shapes within Node, multiply their main color (RGB) and opacity.
    Also potentially turn shape into unlit. }
  procedure MaterialMultiply(const Node: TAbstractChildNode;
    const Color: TCastleColorRgb; const Opacity: Single;
    const MakeUnlit: Boolean);
  var
    Shape: TShapeNode;
    Group: TAbstractGroupingNode;
    ChildNode: TAbstractChildNode;
    I: Integer;
    MatInfo: TMaterialInfo;
  begin
    if Node is TShapeNode then
    begin
      Shape := TShapeNode(Node);
      if MakeUnlit then
        Shape.Shading := shWireframe;

      if Shape.Appearance = nil then
        Shape.Appearance := TAppearanceNode.Create('', Mapping.BaseUrl);
      Assert(Shape.Appearance <> nil);

      if Shape.Appearance.Material = nil then
        // Material equal to nil implies white unlit in X3D
        Shape.Appearance.Material := TUnlitMaterialNode.Create('', Mapping.BaseUrl);
      Assert(Shape.Appearance.Material <> nil);

      MatInfo := Shape.Appearance.Material.MaterialInfo;
      MatInfo.MainColor := MatInfo.MainColor * Color;
      MatInfo.Opacity := MatInfo.Opacity * Opacity;
      if MakeUnlit then
        MatInfo.EmissiveColor := MatInfo.MainColor;
    end;

    if Node is TAbstractGroupingNode then
    begin
      Group := TAbstractGroupingNode(Node);
      for I := 0 to Group.FdChildren.Count - 1 do
        if Group.FdChildren[I] is TAbstractChildNode then
        begin
          ChildNode := TAbstractChildNode(Group.FdChildren[I]);
          MaterialMultiply(ChildNode, Color, Opacity, MakeUnlit);
        end;
    end;
  end;

const
  LightRedRgb: TCastleColorRgb = (X: 1.0; Y: 0.33; Z: 0.33);
var
  RelVoidsElement: TIfcRelVoidsElement;
  ChildNode: TAbstractChildNode;
begin
  Result := inherited;
  SetMetadata(Result, 'IFC_Tag', Tag);

  { TODO: We don't support CSG for now. So, for debug,

    1. We just display the geometry inside HasOpenings.
       It actually should subtract geometry.
       We mark it light-red, to signify this is not the final geometry.

    2, We set our own geometry (but not the one in HasOpenings)
       to be transparent, to see-through the geometry inside HasOpenings,
       and other geometry that should be visible (e.g. window geometry in wall). }

  if HasOpenings.Count > 0 then
    MaterialMultiply(Result, WhiteRgb, 0.2, false);

  for RelVoidsElement in HasOpenings do
  begin
    ChildNode := RelVoidsElement.BuildNode(Mapping);
    MaterialMultiply(ChildNode, LightRedRgb, 1.0, true);
    Result.AddChildren(ChildNode);
  end;
end;

function TIfcContext.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
begin
  Result := inherited;
  SetMetadata(Result, 'IFC_ObjectType', ObjectType);
  SetMetadata(Result, 'IFC_LongName', LongName);
  SetMetadata(Result, 'IFC_Phase', Phase);
end;

function TIfcGrid.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;

  procedure ProcessList(const Axes: TIfcGridAxisList; const AxisCoord: Integer);
  var
    Axis: TIfcGridAxis;
    AxisNode: TAbstractChildNode;
    Color: TCastleColorRgb;
    GridMaterial: TUnlitMaterialNode;
    GridAppearance: TAppearanceNode;
  begin
    // Let color correspond to the axis U/V/W.
    Color := BlackRgb;
    Color.Data[AxisCoord] := 1.0;

    // Grid is not the final geometry, so show only unlit material.
    GridMaterial := TUnlitMaterialNode.Create('', Mapping.BaseUrl);
    GridMaterial.EmissiveColor := Color;
    GridMaterial.Transparency := 0.5;
    GridAppearance := TAppearanceNode.Create('', Mapping.BaseUrl);
    GridAppearance.Material := GridMaterial;

    try
      for Axis in Axes do
      begin
        AxisNode := Axis.BuildNode(Mapping, Self, nil, GridAppearance);
        if AxisNode <> nil then
          Result.AddChildren(AxisNode);
      end;
    finally FreeIfUnusedAndNil(GridAppearance) end;
  end;

begin
  Result := inherited;
  ProcessList(UAxes, 0);
  ProcessList(VAxes, 1);
  ProcessList(WAxes, 2);
end;

{ TIfc*Placement methods implementations ----------------------------------------- }

function TIfcObjectPlacement.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
begin
  Result := TTransformNode.Create('', Mapping.BaseUrl);
  SetMetadata(Result, 'IFC_ClassName', ClassName);
  SetMetadata(Result, 'IFC_Name', Name);
  UpdateNode(Mapping, Result);
end;

procedure TIfcObjectPlacement.UpdateNode(const Mapping: TCastleIfcMapping;
  const Transform: TTransformNode);
begin
  // inherited; // no UpdateNode in ancestor

  Mapping.ObjectPlacementToParentNodes.AddOrSetValue(Self, Transform);
  if PlacementRelTo <> nil then
    Mapping.ChildrenNodesToObjectPlacement.AddOrSetValue(Transform, PlacementRelTo);
end;

procedure TIfcLocalPlacement.UpdateNode(const Mapping: TCastleIfcMapping;
  const Transform: TTransformNode);
var
  Translation: TVector3;
  Rotation: TVector4;
begin
  inherited;
  if RelativePlacement <> nil then
  begin
    RelativePlacement.GetTransform(Translation, Rotation);
    {$ifdef CASTLE_DEBUG_IFC_TRANSFORM}
    WritelnLog('LocalPlacement translation %s, rotation %s', [Translation.ToString, Rotation.ToString]);
    {$endif CASTLE_DEBUG_IFC_TRANSFORM}
    Transform.Translation := Translation;
    Transform.Rotation := Rotation;
  end;
end;

{ TIfcPlacement.GetTransform and overrides ----------------------------------- }

procedure TIfcPlacement.GetTransform(out Translation: TVector3;
  out Rotation: TVector4);
begin
  // inherited; // no GetTransform in ancestor

  if Location is TIfcCartesianPoint then
    Translation := TIfcCartesianPoint(Location).Coordinates.Value
  else
    Translation := TVector3.Zero;

  // descendants should set non-zero Rotation
  Rotation := TVector4.Zero;
end;

procedure TIfcAxis2Placement2D.GetTransform(out Translation: TVector3;
  out Rotation: TVector4);
begin
  inherited; // let TIfcPlacement.GetTransform set Translation

  if not TVector2.Equals(P(0), Vector2(1, 0)) then
    WritelnLog('Axis2Placement2D.GetTransform with P(0) not X axis - TODO: should work, but untested');

  // set Rotation based on P()
  Rotation := Vector4(0, 0, 1,
    RotationAngleRadBetweenVectors(
      Vector3(1, 0, 0), // original X axis
      Vector3(P(0), 0), // desired X axis
      Vector3(0, 0, 1)  // rotation axis, same as given above for Transform.Rotation.XYZ
    )
  );
end;

procedure TIfcAxis2Placement3D.GetTransform(out Translation: TVector3;
  out Rotation: TVector4);
var
  NewAxis: T3Axis;
  I: Integer;
begin
  inherited; // let TIfcPlacement.GetTransform set Translation

  // set Rotation based on P()
  for I := 0 to 2 do
    NewAxis[I] := P(I);
  Rotation := RotationFrom3Axis(NewAxis);
end;

{ IfcToX3D ----------------------------------------------------------------- }

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;
var
  Mapping: TCastleIfcMapping;
begin
  Mapping := TCastleIfcMapping.Create;
  try
    Mapping.Load(Ifc, BaseUrl);
    Result := Mapping.RootNode;
  finally FreeAndNil(Mapping) end;
end;

{ Main routine that converts IFC -> X3D nodes, doing most of the work. }
function LoadIfc(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

  { Debug save back IFC and X3D.
    Save URL is similar to BaseUrl but with -debug-roundtrip.ifcjson,
    -debug-roundtrip.x3d. }
  {.$define CASTLE_DEBUG_IFC_ROUND_TRIP}

  {$ifdef CASTLE_DEBUG_IFC_ROUND_TRIP}
  { Only for debugging purposes: save back the IFC data to JSON.
    This is an easy test whether IfcJsonLoad understood everything correctly,
    and whether IfcJsonSave saves everything correctly. }
  procedure DebugSaveBack(const IfcFile: TIfcFile);
  var
    JsonObj: TJsonObject;
    JsonString: String;
    Stream: TStream;
  begin
    Stream := UrlSaveStream(DeleteUriExt(BaseUrl) + '-debug-roundtrip.ifcjson');
    try
      JsonObj := IfcJsonSave(IfcFile);
      try
        JsonString := JsonObj.FormatJSON;
        WriteStr(Stream, JsonString);
      finally FreeAndNil(JsonObj) end;
    finally FreeAndNil(Stream) end;
    WritelnLog('Saved to ', DeleteUriExt(BaseUrl) + '-debug-roundtrip.ifcjson');
  end;
  {$endif CASTLE_DEBUG_IFC_ROUND_TRIP}

var
  JsonParser: TJsonParser;
  Json: TJsonData;
  IfcFile: TIfcFile;
begin
  JsonParser := TJsonParser.Create(Stream, [joComments]);
  try
    Json := JsonParser.Parse;
    try
      IfcFile := IfcJsonLoad(Json);
      try
        Result := IfcToX3D(IfcFile, BaseUrl);
        {$ifdef CASTLE_DEBUG_IFC_ROUND_TRIP}
        DebugSaveBack(IfcFile);
        SaveNode(Result, DeleteUriExt(BaseUrl) + '-debug-roundtrip.x3d');
        WritelnLog('Saved to ', DeleteUriExt(BaseUrl) + '-debug-roundtrip.x3d');
        {$endif CASTLE_DEBUG_IFC_ROUND_TRIP}
      finally FreeAndNil(IfcFile) end;
    finally FreeAndNil(Json) end;
  finally FreeAndNil(JsonParser) end;
end;

{$endif read_implementation}
