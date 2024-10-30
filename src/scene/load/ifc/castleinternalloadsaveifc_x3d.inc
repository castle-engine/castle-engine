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

{ BuildNode methods implementations ----------------------------------------- }

function TIfcFile.BuildNode(const BaseUrl: String): TX3DRootNode;
begin
  Result := TX3DRootNode.Create('', BaseUrl);
  SetMetadata(Result, 'IFC_Version', Version);
  SetMetadata(Result, 'IFC_SchemaIdentifier', SchemaIdentifier);
  SetMetadata(Result, 'IFC_OriginatingSystem', OriginatingSystem);
  SetMetadata(Result, 'IFC_PreprocessorVersion', PreprocessorVersion);
  SetMetadata(Result, 'IFC_TimeStamp', TimeStamp);

  Result.AddChildren(Project.BuildNode(BaseUrl));
end;

function TIfcRepresentationItemList.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  RepresentationItem: TIfcRepresentationItem;
  Group: TGroupNode;
  Child: TAbstractChildNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  SetMetadata(Group, 'IFC_ListOfChildren', ClassOfListChildren(Self).ClassName);
  Result := Group;

  for RepresentationItem in Self do
  begin
    Child := RepresentationItem.BuildNode(BaseUrl);
    if Child <> nil then
      Group.AddChildren(Child);
  end;
end;

function TIfcRepresentationList.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  Representation: TIfcRepresentation;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  SetMetadata(Group, 'IFC_ListOfChildren', ClassOfListChildren(Self).ClassName);
  Result := Group;

  for Representation in Self do
    Group.AddChildren(Representation.Items.BuildNode(BaseUrl));
end;

function TIfcProductRepresentation.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  Group.AddChildren(Representations.BuildNode(BaseUrl));
end;

function TIfcRelAggregates.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  RelatedObject: TIfcObjectDefinition;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  for RelatedObject in RelatedObjects do
    Group.AddChildren(RelatedObject.BuildNode(BaseUrl));
end;

function TIfcRelContainedInSpatialStructure.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  Product: TIfcProduct;
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  SetMetadata(Group, 'IFC_ClassName', ClassName);
  SetMetadata(Group, 'IFC_Name', Name);
  SetMetadata(Group, 'IFC_Description', Description);
  Result := Group;

  for Product in RelatedElements do
    Group.AddChildren(Product.BuildNode(BaseUrl));
end;

{ BuildNode of TIfcRepresentationItem and descendants ------------------------- }

function TIfcRepresentationItem.BuildNode(const BaseUrl: String): TAbstractChildNode;
begin
  Result := nil;
end;

function TIfcPolyline.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  Shape: TShapeNode;
  LineSet: TLineSetNode;
  Coord: TCoordinateNode;
  I: Integer;
begin
  Coord := TCoordinateNode.Create('', BaseUrl);
  Coord.FdPoint.Count := Points.Count;
  for I := 0 to Points.Count - 1 do
    Coord.FdPoint.Items[I] := Points[I].Coordinates.Value;

  LineSet := TLineSetNode.Create('', BaseUrl);
  LineSet.Coord := Coord;
  LineSet.SetVertexCount([Points.Count]);

  Shape := TShapeNode.Create('', BaseUrl);
  Shape.Geometry := LineSet;
  SetMetadata(Shape, 'IFC_ClassName', ClassName);

  Result := Shape;
end;

function TIfcExtrudedAreaSolid.BuildNode(const BaseUrl: String): TAbstractChildNode;
var
  SweptAreaProf: TIfcArbitraryClosedProfileDef;
begin
  // TODO: Temporarily, render the outer curve, ignoring the 3D extrusion
  Result := nil;
  if SweptArea is TIfcArbitraryClosedProfileDef then // also checks is it <> nil
  begin
    SweptAreaProf := TIfcArbitraryClosedProfileDef(SweptArea);
    if SweptAreaProf.OuterCurve <> nil then
      Result := SweptAreaProf.OuterCurve.BuildNode(BaseUrl);
  end;
end;

{ BuildNode of TIfcObjectDefinition and descendants -------------------------- }

function TIfcObjectDefinition.BuildNode(const BaseUrl: String): TTransformNode;
var
  RelAggregates: TIfcRelAggregates;
begin
  Result := TTransformNode.Create('', BaseUrl);
  SetMetadata(Result, 'IFC_ClassName', ClassName);
  SetMetadata(Result, 'IFC_Name', Name);
  SetMetadata(Result, 'IFC_Description', Description);

  for RelAggregates in IsDecomposedBy do
    Result.AddChildren(RelAggregates.BuildNode(BaseUrl));
end;

function TIfcSpatialStructureElement.BuildNode(const BaseUrl: String): TTransformNode;
var
  RelContainedInSpatialStructure: TIfcRelContainedInSpatialStructure;
begin
  Result := inherited;
  for RelContainedInSpatialStructure in ContainsElements do
    Result.AddChildren(RelContainedInSpatialStructure.BuildNode(BaseUrl));
end;

function TIfcProduct.BuildNode(const BaseUrl: String): TTransformNode;
begin
  Result := inherited;

  if ObjectPlacement <> nil then
    ObjectPlacement.UpdateNode(Result);

  if Representation <> nil then
    Result.AddChildren(Representation.BuildNode(BaseUrl));
end;

function TIfcContext.BuildNode(const BaseUrl: String): TTransformNode;
begin
  Result := inherited;
  SetMetadata(Result, 'IFC_ObjectType', ObjectType);
  SetMetadata(Result, 'IFC_LongName', LongName);
  SetMetadata(Result, 'IFC_Phase', Phase);
end;

{ UpdateNode methods implementations ----------------------------------------- }

procedure TIfcObjectPlacement.UpdateNode(const Transform: TTransformNode);
begin
  // nothing to do in this class
end;

procedure TIfcLocalPlacement.UpdateNode(const Transform: TTransformNode);
begin
  if RelativePlacement <> nil then
    RelativePlacement.UpdateNode(Transform);
end;

procedure TIfcPlacement.UpdateNode(const Transform: TTransformNode);
begin
  if Location is TIfcCartesianPoint then
  begin
    Transform.Translation := TIfcCartesianPoint(Location).Coordinates.Value;
  end;
  // TODO: TIfcAxis2Placement2D/3D may define axis and ref direction, should cause rotation?
end;

{ IfcToX3D and back ---------------------------------------------------------- }

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;
begin
  Result := Ifc.BuildNode(BaseUrl);
end;

function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;
begin
  Result := TIfcFile.Create(nil);
  // TODO
end;

{$endif read_implementation}