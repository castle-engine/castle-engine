{%MainUnit castleinternalloadsaveifc.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ IFC conversion to X3D. }

{$ifdef read_interface_types}
  TObjectPlacementToParentNodes =
    {$ifdef FPC}specialize{$endif} TDictionary<TIfcObjectPlacement, TTransformNode>;
  TChildrenNodesToObjectPlacement =
    {$ifdef FPC}specialize{$endif} TDictionary<TTransformNode, TIfcObjectPlacement>;

  { Information and actions useful to all node-building routines
    that build/update X3D stuff from IFC. }
  TCastleIfcMapping = class
  private
    { Use ObjectPlacementToParentNodes and ChildrenNodesToObjectPlacement
      to move the nodes (change parents) to reflect
      TIfcObjectPlacement.PlacementRelTo instructions. }
    procedure FixParentChildRelationships;
  public
    BaseUrl: String;

    { Map each encountered TIfcObjectPlacement to an X3D TTransformNode. }
    ObjectPlacementToParentNodes: TObjectPlacementToParentNodes;

    { Map each children node (TTransformNode) to a desired parent,
      expressed as TIfcObjectPlacement.
      Together, ObjectPlacementToParentNodes and ChildrenNodesToObjectPlacement
      allow to set the visual transformation (parent-child relationships
      between X3D nodes) to follow IFC TIfcObjectPlacement.PlacementRelTo idea. }
    ChildrenNodesToObjectPlacement: TChildrenNodesToObjectPlacement;

    constructor Create;
    destructor Destroy; override;
  end;
{$endif read_interface_types}

{$ifdef read_interface}

{ Convert IFC file to X3D. }
function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;

{ Convert IFC file to X3D. }
function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;

{$endif read_interface}

{$ifdef read_implementation}

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
  ObjectPlacementToParentNodes := TObjectPlacementToParentNodes.Create;
  ChildrenNodesToObjectPlacement := TChildrenNodesToObjectPlacement.Create;
end;

destructor TCastleIfcMapping.Destroy;
begin
  FreeAndNil(ObjectPlacementToParentNodes);
  FreeAndNil(ChildrenNodesToObjectPlacement);
  inherited;
end;

procedure TCastleIfcMapping.FixParentChildRelationships;

  { Go up the nodes hierachy, returning direct parent (any TAbstractGroupingNode)
    and possibly non-direct TransformParent (TTransformNode) parent.
    Returns @true and for sure non-nil DirectParent, TransformParent on success.
    On failure returns @false and DirectParent, TransformParent are undefined.

    This way of searching for 2 parents accounts for the fact that
    things in TIfcProduct are wrapped in additional groups related to 'Representation'.
    We don't need to move things out of these groups,
    if ultimately they are placed in the right TTransformNode. }
  function GetParentNode(const Node: TTransformNode;
    out DirectParent: TAbstractGroupingNode; out TransformParent: TTransformNode): Boolean;

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
    PotentialTransformParent: TAbstractGroupingNode;
  begin
    Result := false;
    DirectParent := GetDirectParent(Node);
    TransformParent := nil; // initialize to nil
    if DirectParent = nil then
      Exit; // exit with Result = false

    PotentialTransformParent := DirectParent;
    while not (PotentialTransformParent is TTransformNode) do
    begin
      PotentialTransformParent := GetDirectParent(PotentialTransformParent);
      if PotentialTransformParent = nil then
        Exit; // exit with Result = false
    end;
    Assert(PotentialTransformParent is TTransformNode);
    TransformParent := TTransformNode(PotentialTransformParent);
    Result := true;
  end;

var
  ChildToPlacement:
    {$ifdef FPC}TChildrenNodesToObjectPlacement.TDictionaryPair
    {$else}TPair<TTransformNode, TIfcObjectPlacement>
    {$endif};
  Child, OldParentTransform, NewParent: TTransformNode;
  OldParentDirect: TAbstractGroupingNode;
begin
  for ChildToPlacement in ChildrenNodesToObjectPlacement do
  begin
    Child := ChildToPlacement.Key;
    if GetParentNode(Child, OldParentDirect, OldParentTransform) then
    begin
      if ObjectPlacementToParentNodes.TryGetValue(ChildToPlacement.Value, NewParent) and
         (OldParentTransform <> NewParent) then
      begin
        WritelnLog('IFC', 'Fixing parent of "%s" to be relative to ObjectPlacement GUID "%s" to follow placementRelTo', [
          Child.NiceName,
          ChildToPlacement.Value.GlobalId
        ]);
        Child.KeepExistingBegin; // do not free Child because ref count temporarily drops to zero below
        OldParentDirect.RemoveChildren(Child);
        NewParent.AddChildren(Child);
        Child.KeepExistingEnd;
      end;
    end;
  end;
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

function TIfcRepresentationItemList.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
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
    Child := RepresentationItem.BuildNode(Mapping);
    if Child <> nil then
      Group.AddChildren(Child);
  end;
end;

function TIfcRepresentationList.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  Representation: TIfcRepresentation;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ListOfChildren', ClassOfListChildren(Self).ClassName);
  Result := Group;

  for Representation in Self do
    Group.AddChildren(Representation.Items.BuildNode(Mapping));
end;

function TIfcProductRepresentation.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', Mapping.BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  Group.AddChildren(Representations.BuildNode(Mapping));
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

{ BuildNode of TIfcRepresentationItem and descendants ------------------------- }

function TIfcRepresentationItem.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
begin
  Result := nil;
end;

function TIfcPolyline.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
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
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Result := Shape;
end;

procedure TIfcPolyline.UpdateCrossSection(const Extrusion: TExtrusionNode);
var
  CrossSection: TVector2List;
  I: Integer;
begin
  CrossSection := TVector2List.Create;
  try
    CrossSection.Count := Points.Count;
    for I := 0 to Points.Count - 1 do
      CrossSection[I] := Points[I].Coordinates.Value.XY;
    Extrusion.SetCrossSection(CrossSection);
  finally FreeAndNil(CrossSection) end;
end;

function TIfcExtrudedAreaSolid.BuildNode(const Mapping: TCastleIfcMapping): TAbstractChildNode;
var
  Extrusion: TExtrusionNode;
  SweptAreaProf: TIfcArbitraryClosedProfileDef;
  SweptAreaCurvePolyline: TIfcPolyline;
  Shape: TShapeNode;
begin
  Extrusion := TExtrusionNode.Create('', Mapping.BaseUrl);
  Extrusion.SetSpine([
    TVector3.Zero,
    ExtrudedDirection.DirectionRatios.Value * Depth
  ]);
  if SweptArea is TIfcArbitraryClosedProfileDef then // also checks is it <> nil
  begin
    SweptAreaProf := TIfcArbitraryClosedProfileDef(SweptArea);
    if SweptAreaProf.OuterCurve is TIfcPolyline then
    begin
      SweptAreaCurvePolyline := TIfcPolyline(SweptAreaProf.OuterCurve);
      SweptAreaCurvePolyline.UpdateCrossSection(Extrusion);
    end;
  end;

  Shape := TShapeNode.Create('', Mapping.BaseUrl);
  Shape.Geometry := Extrusion;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Result := Shape;
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
begin
  Result := inherited;

  if ObjectPlacement <> nil then
    ObjectPlacement.UpdateNode(Mapping, Result);

  if Representation <> nil then
    Result.AddChildren(Representation.BuildNode(Mapping));
end;

function TIfcElement.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
var
  RelVoidsElement: TIfcRelVoidsElement;
begin
  Result := inherited;
  SetMetadata(Result, 'IFC_Tag', Tag);

  // TODO: This is just for debug, the things in HasOpenings should actually subtract geometry
  for RelVoidsElement in HasOpenings do
    Result.AddChildren(RelVoidsElement.BuildNode(Mapping));
end;

function TIfcContext.BuildNode(const Mapping: TCastleIfcMapping): TTransformNode;
begin
  Result := inherited;
  SetMetadata(Result, 'IFC_ObjectType', ObjectType);
  SetMetadata(Result, 'IFC_LongName', LongName);
  SetMetadata(Result, 'IFC_Phase', Phase);
end;

{ UpdateNode methods implementations ----------------------------------------- }

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
begin
  inherited;
  if RelativePlacement <> nil then
    RelativePlacement.UpdateNode(Mapping, Transform);
end;

procedure TIfcPlacement.UpdateNode(const Mapping: TCastleIfcMapping;
  const Transform: TTransformNode);
begin
  // inherited; // no UpdateNode in ancestor

  if Location is TIfcCartesianPoint then
  begin
    Transform.Translation := TIfcCartesianPoint(Location).Coordinates.Value;
  end;
  // TODO: TIfcAxis2Placement2D/3D may define axis and ref direction, should cause rotation?
end;

{ IfcToX3D and back ---------------------------------------------------------- }

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;
var
  Mapping: TCastleIfcMapping;
begin
  Mapping := TCastleIfcMapping.Create;
  try
    Mapping.BaseUrl := BaseUrl;
    Result := Ifc.BuildNode(Mapping);
    Mapping.FixParentChildRelationships;
  finally FreeAndNil(Mapping) end;
end;

function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;
begin
  Result := TIfcFile.Create(nil);
  // TODO
end;

{$endif read_implementation}