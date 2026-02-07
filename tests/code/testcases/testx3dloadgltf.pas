// -*- compile-command: "./test_single_testcase.sh TTestX3DLoadGltf" -*-
{
  Copyright 2021-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test X3DLoadInternalGltf unit. }
unit TestX3DLoadGltf;

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestX3DLoadGltf = class(TCastleTestCase)
  published
    procedure TestSaveLoadTransform;
    procedure TestSaveLoadPhysicalMaterial;
    procedure TestSaveLoadUnlitMaterial;
    procedure TestSaveLoadMesh;
    procedure TestRoundTripGltfFile;
  end;

implementation

uses X3DLoad, X3DNodes, CastleVectors, X3DLoadInternalGltf, CastleUtils,
  CastleUriUtils;

{ Helper function to recursively find a TShapeNode in the node tree }
function FindShapeNode(const Node: TX3DNode): TShapeNode;
var
  I: Integer;
  Group: TAbstractGroupingNode;
  ChildResult: TShapeNode;
begin
  Result := nil;

  if Node is TShapeNode then
    Exit(TShapeNode(Node));

  if Node is TAbstractGroupingNode then
  begin
    Group := TAbstractGroupingNode(Node);
    for I := 0 to Group.FdChildren.Count - 1 do
    begin
      ChildResult := FindShapeNode(Group.FdChildren[I]);
      if ChildResult <> nil then
        Exit(ChildResult);
    end;
  end;
end;

procedure TTestX3DLoadGltf.TestSaveLoadTransform;
var
  Original, Loaded: TX3DRootNode;
  Stream: TMemoryStream;
  Transform: TTransformNode;
  Shape: TShapeNode;
  Box: TBoxNode;
  Appearance: TAppearanceNode;
  Material: TPhysicalMaterialNode;
  LoadedTransform: TTransformNode;
begin
  Original := TX3DRootNode.Create;
  try
    // Create a transform with a simple box inside
    Transform := TTransformNode.Create('TestTransform');
    Transform.Translation := Vector3(1, 2, 3);
    Transform.Rotation := Vector4(0, 1, 0, Pi / 2);
    Transform.Scale := Vector3(2, 2, 2);

    Shape := TShapeNode.Create;
    Box := TBoxNode.Create;
    Shape.Geometry := Box;

    Appearance := TAppearanceNode.Create;
    Material := TPhysicalMaterialNode.Create;
    Material.BaseColor := Vector3(1, 0, 0);
    Appearance.Material := Material;
    Shape.Appearance := Appearance;

    Transform.AddChildren(Shape);
    Original.AddChildren(Transform);

    // Save to glTF
    Stream := TMemoryStream.Create;
    try
      SaveNode(Original, Stream, 'model/gltf+json');
      Stream.Position := 0;

      // Load back
      Loaded := LoadNode(Stream, '', 'model/gltf+json');
      try
        // Verify structure: should have at least one child
        AssertTrue('Loaded node should have children', Loaded.FdChildren.Count > 0);

        // Find the transform node
        // Account for the fact our save + load possibly wraps things in a group.
        if Loaded.FdChildren[0] is TTransformNode then
          LoadedTransform := TTransformNode(Loaded.FdChildren[0])
        else
        if (Loaded.FdChildren[0] is TGroupNode) and
           (TGroupNode(Loaded.FdChildren[0]).FdChildren.Count > 0) and
           (TGroupNode(Loaded.FdChildren[0]).FdChildren[0] is TTransformNode) then
          LoadedTransform := TTransformNode(TGroupNode(Loaded.FdChildren[0]).FdChildren[0])
        else
        begin
          Fail('Expected transform node in loaded scene');
          Exit;
        end;

        // Verify transform values (with some tolerance due to floating point)
        AssertVectorEquals(Vector3(1, 2, 3), LoadedTransform.Translation, 0.001);
        AssertVectorEquals(Vector3(2, 2, 2), LoadedTransform.Scale, 0.001);
        // Note: rotation comparison is tricky due to axis-angle/quaternion conversion
      finally
        FreeAndNil(Loaded);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

procedure TTestX3DLoadGltf.TestSaveLoadPhysicalMaterial;
var
  Original, Loaded: TX3DRootNode;
  Stream: TMemoryStream;
  Shape: TShapeNode;
  Box: TBoxNode;
  Appearance: TAppearanceNode;
  Material: TPhysicalMaterialNode;
  LoadedMaterial: TPhysicalMaterialNode;
  LoadedShape: TShapeNode;
  I: Integer;
begin
  Original := TX3DRootNode.Create;
  try
    Shape := TShapeNode.Create;
    Box := TBoxNode.Create;
    Shape.Geometry := Box;

    Appearance := TAppearanceNode.Create;
    Material := TPhysicalMaterialNode.Create;
    Material.BaseColor := Vector3(0.8, 0.2, 0.1);
    Material.Metallic := 0.7;
    Material.Roughness := 0.3;
    Material.EmissiveColor := Vector3(0.1, 0.1, 0.0);
    Appearance.Material := Material;
    Shape.Appearance := Appearance;

    Original.AddChildren(Shape);

    // Save to glTF
    Stream := TMemoryStream.Create;
    try
      SaveNode(Original, Stream, 'model/gltf+json');
      Stream.Position := 0;

      // Load back
      Loaded := LoadNode(Stream, '', 'model/gltf+json');
      try
        // Find the shape with material (recursively)
        LoadedShape := FindShapeNode(Loaded);

        AssertTrue('Should find a shape node', LoadedShape <> nil);
        AssertTrue('Shape should have appearance', LoadedShape.Appearance <> nil);
        AssertTrue('Appearance should have material', LoadedShape.Appearance.Material <> nil);
        AssertTrue('Material should be TPhysicalMaterialNode',
          LoadedShape.Appearance.Material is TPhysicalMaterialNode);

        LoadedMaterial := TPhysicalMaterialNode(LoadedShape.Appearance.Material);

        // Verify material properties
        AssertVectorEquals(Vector3(0.8, 0.2, 0.1), LoadedMaterial.BaseColor, 0.01);
        AssertSameValue(0.7, LoadedMaterial.Metallic, 0.01);
        AssertSameValue(0.3, LoadedMaterial.Roughness, 0.01);
        AssertVectorEquals(Vector3(0.1, 0.1, 0.0), LoadedMaterial.EmissiveColor, 0.01);
      finally
        FreeAndNil(Loaded);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

procedure TTestX3DLoadGltf.TestSaveLoadUnlitMaterial;
var
  Original, Loaded: TX3DRootNode;
  Stream: TMemoryStream;
  Shape: TShapeNode;
  Box: TBoxNode;
  Appearance: TAppearanceNode;
  Material: TUnlitMaterialNode;
  LoadedMaterial: TUnlitMaterialNode;
  LoadedShape: TShapeNode;
begin
  Original := TX3DRootNode.Create;
  try
    Shape := TShapeNode.Create;
    Box := TBoxNode.Create;
    Shape.Geometry := Box;

    Appearance := TAppearanceNode.Create;
    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := Vector3(0.5, 0.8, 0.2);
    Appearance.Material := Material;
    Shape.Appearance := Appearance;

    Original.AddChildren(Shape);

    // Save to glTF
    Stream := TMemoryStream.Create;
    try
      SaveNode(Original, Stream, 'model/gltf+json');
      Stream.Position := 0;

      // Load back
      Loaded := LoadNode(Stream, '', 'model/gltf+json');
      try
        // Find the shape with material (recursively)
        LoadedShape := FindShapeNode(Loaded);

        AssertTrue('Should find a shape node', LoadedShape <> nil);
        AssertTrue('Shape should have appearance', LoadedShape.Appearance <> nil);
        AssertTrue('Appearance should have material', LoadedShape.Appearance.Material <> nil);
        AssertTrue('Material should be TUnlitMaterialNode',
          LoadedShape.Appearance.Material is TUnlitMaterialNode);

        LoadedMaterial := TUnlitMaterialNode(LoadedShape.Appearance.Material);

        // Verify material properties
        // Note: In unlit material, emissiveColor is stored as baseColor in glTF
        AssertVectorEquals(Vector3(0.5, 0.8, 0.2), LoadedMaterial.EmissiveColor, 0.01);
      finally
        FreeAndNil(Loaded);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

procedure TTestX3DLoadGltf.TestSaveLoadMesh;
var
  Original, Loaded: TX3DRootNode;
  Stream: TMemoryStream;
  Shape: TShapeNode;
  TriSet: TIndexedTriangleSetNode;
  Coord: TCoordinateNode;
  Normal: TNormalNode;
  Appearance: TAppearanceNode;
  Material: TPhysicalMaterialNode;
  LoadedShape: TShapeNode;
  LoadedGeom: TAbstractGeometryNode;
begin
  Original := TX3DRootNode.Create;
  try
    Shape := TShapeNode.Create;

    // Create a simple triangle
    TriSet := TIndexedTriangleSetNode.Create;
    Coord := TCoordinateNode.Create;
    Coord.FdPoint.Items.Add(Vector3(0, 0, 0));
    Coord.FdPoint.Items.Add(Vector3(1, 0, 0));
    Coord.FdPoint.Items.Add(Vector3(0.5, 1, 0));
    TriSet.Coord := Coord;
    TriSet.FdIndex.Items.Add(0);
    TriSet.FdIndex.Items.Add(1);
    TriSet.FdIndex.Items.Add(2);

    Normal := TNormalNode.Create;
    Normal.FdVector.Items.Add(Vector3(0, 0, 1));
    Normal.FdVector.Items.Add(Vector3(0, 0, 1));
    Normal.FdVector.Items.Add(Vector3(0, 0, 1));
    TriSet.Normal := Normal;

    Shape.Geometry := TriSet;

    Appearance := TAppearanceNode.Create;
    Material := TPhysicalMaterialNode.Create;
    Material.BaseColor := Vector3(0, 1, 0);
    Appearance.Material := Material;
    Shape.Appearance := Appearance;

    Original.AddChildren(Shape);

    // Save to glTF
    Stream := TMemoryStream.Create;
    try
      SaveNode(Original, Stream, 'model/gltf+json');
      Stream.Position := 0;

      // Load back
      Loaded := LoadNode(Stream, '', 'model/gltf+json');
      try
        // Find the shape (recursively)
        LoadedShape := FindShapeNode(Loaded);

        AssertTrue('Should find a shape node', LoadedShape <> nil);
        AssertTrue('Shape should have geometry', LoadedShape.Geometry <> nil);

        LoadedGeom := LoadedShape.Geometry;

        // glTF loading creates TIndexedTriangleSetNode for triangles
        AssertTrue('Geometry should be triangle-based',
          (LoadedGeom is TIndexedTriangleSetNode) or (LoadedGeom is TTriangleSetNode));

        // Verify it has coordinates
        if LoadedGeom is TIndexedTriangleSetNode then
        begin
          AssertTrue('Should have coordinates',
            TIndexedTriangleSetNode(LoadedGeom).Coord <> nil);
          AssertEquals('Should have 3 vertices', 3,
            (TIndexedTriangleSetNode(LoadedGeom).Coord as TCoordinateNode).FdPoint.Count);
        end;
      finally
        FreeAndNil(Loaded);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

procedure TTestX3DLoadGltf.TestRoundTripGltfFile;
var
  Original, Loaded: TX3DRootNode;
  Stream: TMemoryStream;
  TestFile: String;
begin
  // Test loading a glTF file, saving it, and loading again
  TestFile := 'castle-data:/gltf/quaternius/Bunny.gltf';

  // Skip if test file doesn't exist
  if not UriFileExists(TestFile) then
    Exit;

  Original := LoadNode(TestFile);
  try
    // Save to memory stream
    Stream := TMemoryStream.Create;
    try
      SaveNode(Original, Stream, 'model/gltf+json');
      Stream.Position := 0;

      // Load back
      Loaded := LoadNode(Stream, '', 'model/gltf+json');
      try
        // Basic verification - both should have children
        AssertTrue('Original should have children', Original.FdChildren.Count > 0);
        AssertTrue('Loaded should have children', Loaded.FdChildren.Count > 0);
      finally
        FreeAndNil(Loaded);
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(Original);
  end;
end;

initialization
  RegisterTest(TTestX3DLoadGltf);
end.
