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

{ BuildNodes methods implementations ----------------------------------------- }

function TIfcRepresentationItem.BuildNodes(const BaseUrl: String): TAbstractChildNode;
begin
  Result := nil;
end;

function TIfcPolyline.BuildNodes(const BaseUrl: String): TAbstractChildNode;
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
  Shape.MetadataString['IFC_ClassName'] := ClassName;

  Result := Shape;
end;

function TIfcRepresentationItemList.BuildNodes(const BaseUrl: String): TAbstractChildNode;
var
  RepresentationItem: TIfcRepresentationItem;
  Group: TGroupNode;
  Child: TAbstractChildNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  // Group.MetadataString['IFC_ClassName'] := ClassName; // lists are not official IFC classes
  Result := Group;

  for RepresentationItem in Self do
  begin
    Child := RepresentationItem.BuildNodes(BaseUrl);
    if Child <> nil then
      Group.AddChildren(Child);
  end;
end;

function TIfcRepresentationList.BuildNodes(const BaseUrl: String): TAbstractChildNode;
var
  Representation: TIfcRepresentation;
  group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  // Group.MetadataString['IFC_ClassName'] := ClassName; // lists are not official IFC classes
  Result := Group;

  for Representation in Self do
    Group.AddChildren(Representation.Items.BuildNodes(BaseUrl));
end;

function TIfcProductRepresentation.BuildNodes(const BaseUrl: String): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  Group.MetadataString['IFC_ClassName'] := ClassName;
  Group.MetadataString['IFC_Name'] := Name;
  Group.MetadataString['IFC_Description'] := Description;
  Result := Group;

  Group.AddChildren(Representations.BuildNodes(BaseUrl));
end;

function TIfcProduct.BuildNodes(const BaseUrl: String): TAbstractChildNode;
var
  Group: TGroupNode;
begin
  Group := TGroupNode.Create('', BaseUrl);
  Group.MetadataString['IFC_ClassName'] := ClassName;
  Result := Group;

  if Representation <> nil then
    Group.AddChildren(Representation.BuildNodes(BaseUrl));
end;

{ IfcToX3D (dispatching work to BuildNodes) ---------------------------------- }

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;

  procedure ReadObjectDefinition(const Parent: TAbstractGroupingNode; const IfcObjDef: TIfcObjectDefinition); forward;

  procedure ReadRelAggregates(const Parent: TAbstractGroupingNode; const RelAggregates: TIfcRelAggregates);
  var
    RelatedObject: TIfcObjectDefinition;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create('', BaseUrl);
    Parent.AddChildren(Group);
    Group.MetadataString['IFC_ClassName'] := RelAggregates.ClassName;
    Group.MetadataString['IFC_Name'] := RelAggregates.Name;
    Group.MetadataString['IFC_Description'] := RelAggregates.Description;

    for RelatedObject in RelAggregates.RelatedObjects do
      ReadObjectDefinition(Group, RelatedObject);
  end;

  procedure ReadProduct(const Parent: TAbstractGroupingNode; const Product: TIfcProduct);
  //var
    //Element: TIfcElement;
    //RelVoidsElement: TIfcRelVoidsElement;
  begin
    Parent.AddChildren(Product.Representation.BuildNodes(BaseUrl));

    if Product is TIfcElement then
    begin
      //Element := TIfcElement(Product);
      // for RelVoidsElement in Element.HasOpenings do // TODO
    end;
  end;

  procedure ReadRelContainedInSpatialStructure(const Parent: TAbstractGroupingNode;
    const RelContainedInSpatialStructure: TIfcRelContainedInSpatialStructure);
  var
    Product: TIfcProduct;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create('', BaseUrl);
    Parent.AddChildren(Group);
    Group.MetadataString['IFC_ClassName'] := RelContainedInSpatialStructure.ClassName;
    Group.MetadataString['IFC_Name'] := RelContainedInSpatialStructure.Name;
    Group.MetadataString['IFC_Description'] := RelContainedInSpatialStructure.Description;

    for Product in RelContainedInSpatialStructure.RelatedElements do
      ReadProduct(Group, Product);
  end;

  procedure ReadObjectDefinition(const Parent: TAbstractGroupingNode; const IfcObjDef: TIfcObjectDefinition);
  var
    RelAggregates: TIfcRelAggregates;
    Group: TGroupNode;
    IfcSpatialStructureElement: TIfcSpatialStructureElement;
    RelContainedInSpatialStructure: TIfcRelContainedInSpatialStructure;
  begin
    Group := TGroupNode.Create('', BaseUrl);
    Parent.AddChildren(Group);
    Group.MetadataString['IFC_ClassName'] := IfcObjDef.ClassName;
    Group.MetadataString['IFC_Name'] := IfcObjDef.Name;
    Group.MetadataString['IFC_Description'] := IfcObjDef.Description;

    for RelAggregates in IfcObjDef.IsDecomposedBy do
      ReadRelAggregates(Group, RelAggregates);

    if IfcObjDef is TIfcSpatialStructureElement then
    begin
      IfcSpatialStructureElement := TIfcSpatialStructureElement(IfcObjDef);
      for RelContainedInSpatialStructure in IfcSpatialStructureElement.ContainsElements do
        ReadRelContainedInSpatialStructure(Group, RelContainedInSpatialStructure);
    end;
  end;

begin
  Result := TX3DRootNode.Create('', BaseUrl);
  try
    if Ifc.Version <> '' then
      { TODO: "metadata" of TX3DRootNode is not serialized, but META is.
        Yeah, there are effectively two ways to store metadata in X3D. }
      //Result.MetadataString['IFC_Version'] := Ifc.Version;
      Result.Meta['IFC_Version'] := Ifc.Version;
    if Ifc.SchemaIdentifier <> '' then
      Result.Meta['IFC_SchemaIdentifier'] := Ifc.SchemaIdentifier;
    if Ifc.OriginatingSystem <> '' then
      Result.Meta['IFC_OriginatingSystem'] := Ifc.OriginatingSystem;
    if Ifc.PreprocessorVersion <> '' then
      Result.Meta['IFC_PreprocessorVersion'] := Ifc.PreprocessorVersion;
    if Ifc.TimeStamp <> '' then
      Result.Meta['IFC_TimeStamp'] := Ifc.TimeStamp;

    ReadObjectDefinition(Result, Ifc.Project);
  except FreeAndNil(Result); raise end;
end;

function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;
begin
  Result := TIfcFile.Create(nil);
  // TODO
end;

{$endif read_implementation}