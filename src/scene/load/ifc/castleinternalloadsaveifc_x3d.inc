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

function IfcToX3D(const Ifc: TIfcFile; const BaseUrl: String): TX3DRootNode;

  procedure ReadObjectDefinition(const Parent: TAbstractGroupingNode; const IfcObjDef: TIfcObjectDefinition); forward;

  procedure ReadRelAggregates(const Parent: TAbstractGroupingNode; const RelAggregates: TIfcRelAggregates);
  var
    RelatedObject: TIfcObjectDefinition;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create('', BaseUrl);
    Parent.AddChildren(Group);
    Group.MetadataString['IFC_Type'] := RelAggregates.ClassName;
    Group.MetadataString['IFC_Name'] := RelAggregates.Name;
    Group.MetadataString['IFC_Description'] := RelAggregates.Description;

    for RelatedObject in RelAggregates.RelatedObjects do
      ReadObjectDefinition(Group, RelatedObject);
  end;

  procedure ReadProduct(const Parent: TAbstractGroupingNode; const Product: TIfcProduct);
  var
    Element: TIfcElement;
  begin
    // TODO: read Product.Representation - IfcProductDefinitionShape in testcase

    if Product is TIfcElement then
    begin
      Element := TIfcElement(Product);
      // read Element.HasOpenings
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
    Group.MetadataString['IFC_Type'] := RelContainedInSpatialStructure.ClassName;
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
    Group.MetadataString['IFC_Type'] := IfcObjDef.ClassName;
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