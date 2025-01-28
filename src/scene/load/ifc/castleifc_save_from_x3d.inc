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
function X3DToIfc(const Node: TX3DRootNode): TIfcFile;

{$endif read_interface}

{$ifdef read_implementation}

type
  { Helper class to save IFC.
    Enumerates all triangle and add them to Vertexes, Indexes.
    This is rather unoptimal and simple way to save IFC.
    For each triangle, we just expand the Vertexes and Indexes. }
  TSaveTriangulateHelper = class
    Vertexes: TVector3List;
    Indexes: TIntegerList;
    procedure ProcessTriangle(Shape: TObject;
      const Triangle: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  end;

procedure TSaveTriangulateHelper.ProcessTriangle(Shape: TObject;
  const Triangle: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
begin
  Indexes.Add(Vertexes.Count);
  Vertexes.Add(Triangle.Data[0]);

  Indexes.Add(Vertexes.Count);
  Vertexes.Add(Triangle.Data[1]);

  Indexes.Add(Vertexes.Count);
  Vertexes.Add(Triangle.Data[2]);

  Indexes.Add(-1);
end;

function X3DToIfc(const Node: TX3DRootNode): TIfcFile;
var
  IfcSite: TIfcSite;
  IfcBuilding: TIfcBuilding;
  IfcBuildingStorey: TIfcBuildingStorey;
  IfcContainer: TIfcSpatialElement;
  Element: TIfcBuildingElementProxy;
  Owner: TComponent;
  Scene: TCastleSceneCore;
  Shape: TShape;
  Helper: TSaveTriangulateHelper;
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

  { Create temporary scene to enumerate triangles in Node.
    See STL saving code for similar trick. }
  Scene := TCastleSceneCore.Create(nil);
  try
    if Node.Scene <> nil then
    begin
      { Do DeepCopy, to not change nodes (Scene, associated nodes in shapes...). }
      Scene.Load(Node.DeepCopy as TX3DRootNode, true);
    end else
    begin
      // more efficient: use Node directly, we will unassociate from it later
      Scene.Load(Node, false);
    end;

    Helper := TSaveTriangulateHelper.Create;
    try
      Helper.Vertexes := TVector3List.Create;
      Helper.Indexes := TIntegerList.Create;
      for Shape in Scene.Shapes.TraverseList(true) do
        Shape.Triangulate({$ifdef FPC}@{$endif} Helper.ProcessTriangle,
          { FrontFaceAlwaysCcw unnecessary,
            as we pass Solid = false below to AddMeshRepresentation,
            so backface culling is disabled. }
          false);
      Element.AddMeshRepresentation(Result.Project.ModelContext,
        Helper.Vertexes.ToArray, Helper.Indexes.ToArray, false);
    finally FreeAndNil(Helper) end;
  finally FreeAndNil(Scene) end;

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
