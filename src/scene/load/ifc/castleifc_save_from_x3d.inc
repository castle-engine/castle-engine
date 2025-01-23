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

{ X3D conversion to IFC. Allows to save IFC from arbitrary other formats
  (like X3D, glTF) in our engine. }

{$ifdef read_interface}

{ Convert X3D content to IFC. }
function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;

{$endif read_interface}

{$ifdef read_implementation}

function X3DToIfc(const RootNode: TX3DRootNode): TIfcFile;
var
  IfcSite: TIfcSite;
  IfcBuilding: TIfcBuilding;
  IfcBuildingStorey: TIfcBuildingStorey;
  IfcContainer: TIfcSpatialElement;
  Element: TIfcBuildingElementProxy;
  Owner: TComponent;
begin
  Result := TIfcFile.Create(nil);
  Owner := Result; // all IFC classes are owned by the TIfcFile

  { Obligatory initialization of new IFC files structure.
    We must set Result.Project,
    each project must have units,
    and each project must have a 3D model context. }
  Result.Project := TIfcProject.Create(Owner);
  Result.Project.SetupUnits;
  Result.Project.SetupModelContext;

  { We need IfcContainer inside the project.
    Reason: We cannot add elements directly to IfcProject, we need
    to add them to a spatial root element: (IfcSite || IfcBuilding || IfcSpatialZone)
    (see https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProject.htm ).

    The IfcSpatialStructureElement
    (see https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialStructureElement.htm )
    defines the hierarchy of root classes, and implies that root hierarchy
    can be (one case, seems most common -- BlenderBIM also follows it):

    - IfcSite inside IfcProject,
    - with IfcBuilding inside,
    - with IfcBuildingStorey inside,
    inserted into each other using "is composed by" relationship. }

  IfcSite := TIfcSite.Create(Owner);
  IfcSite.Name := 'My Site';
  Result.Project.AddIsDecomposedBy(IfcSite);

  IfcBuilding := TIfcBuilding.Create(Owner);
  IfcBuilding.Name := 'My Building';
  IfcSite.AddIsDecomposedBy(IfcBuilding);

  IfcBuildingStorey := TIfcBuildingStorey.Create(Owner);
  IfcBuildingStorey.Name := 'My Building Storey';
  IfcBuilding.AddIsDecomposedBy(IfcBuildingStorey);

  IfcContainer := IfcBuildingStorey;

  Element := TIfcBuildingElementProxy.Create(Owner);
  //Element.AddMeshRepresentation(IfcFile.Project.ModelContext, Vertexes, ...);
  IfcContainer.AddContainedElement(Element);
end;

{ Registered save routine, converting X3D to IFC and saving it to a stream. }
procedure SaveIfc(const Node: TX3DRootNode; const Stream: TStream;
  const Generator: String; const Source: String);
var
  IfcFile: TIfcFile;
  JsonObj: TJsonObject;
  JsonString: String;
begin
  IfcFile := X3DToIfc(Node);
  try
    JsonObj := IfcJsonSave(IfcFile);
    try
      JsonString := JsonObj.FormatJSON;
      WriteStr(Stream, JsonString);
    finally FreeAndNil(JsonObj) end;
  finally FreeAndNil(IfcFile) end;
end;

{$endif read_implementation}
