{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D models in the glTF 2.0 format (@link(LoadGLTF)). }
unit X3DLoadInternalGLTF;

interface

uses X3DNodes;

{ Load 3D model in the GLTF format, converting it to an X3D nodes graph.
  This routine is internally used by the @link(Load3D) to load an GLTF file. }
function LoadGLTF(const URL: string): TX3DRootNode;

implementation

uses SysUtils, Classes, TypInfo,
  PasGLTF,
  CastleClassUtils, CastleDownload, CastleUtils, CastleURIUtils, CastleLog,
  CastleVectors;

function LoadGLTF(const URL: string): TX3DRootNode;
var
  Document: TPasGLTF.TDocument;

  function AccessorTypeToStr(const AccessorType: TPasGLTF.TAccessor.TType): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TAccessor.TType), Ord(AccessorType));
  end;

  function PrimitiveModeToStr(const Mode: TPasGLTF.TMesh.TPrimitive.TMode): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TMesh.TPrimitive.TMode), Ord(Mode));
  end;

  function Vector3FromGltf(const V: TPasGLTF.TVector3): TVector3;
  begin
    // as it happens, both structures have the same memory layout, so copy fast
    Assert(SizeOf(V) = SizeOf(Result));
    Move(V, Result, SizeOf(Result));
  end;

  procedure ReadPrimitive(const Primitive: TPasGLTF.TMesh.TPrimitive);
  var
    Accessor: TPasGLTF.TAccessor;
    AttributeName: TPasGLTFUTF8String;
    AttributeAccessorIndex: Int64;
    Indices: TPasGLTFUInt32DynamicArray;
    Position: TPasGLTF.TVector3DynamicArray;
    Shape: TShapeNode;
    Geometry: TAbstractGeometryNode;
    GeometryTriangleSet: TTriangleSetNode;
    GeometryIndexedTriangleSet: TIndexedTriangleSetNode;
    Coordinate: TCoordinateNode;
    I: Integer;
  begin
    WritelnLog('glTF', 'Got primitive ' + PrimitiveModeToStr(Primitive.Mode));
    if Primitive.Mode <> TPasGLTF.TMesh.TPrimitive.TMode.Triangles then
      Exit; // we don't handle other types yet

    if Primitive.Indices <> -1 then
    begin
      if Primitive.Indices >= Document.Accessors.Count then
      begin
        WritelnWarning('glTF', 'glTF missing accessor for indices %d', [Primitive.Indices]);
        Exit;
      end;

      Accessor := Document.Accessors[Primitive.Indices];
      Indices := Accessor.DecodeAsUInt32Array(false); // tested, false is necessary
      WritelnLog('glTF', '  Primitive has indexes with accessor of type %s, extracted to array with count %d',
        [AccessorTypeToStr(Accessor.Type_), Length(Indices)]);
    end;

    for AttributeName in Primitive.Attributes.Keys do
    begin
      WritelnLog('glTF', '  Primitive has attribute named ' + AttributeName);
      AttributeAccessorIndex := Primitive.Attributes[AttributeName];

      if AttributeAccessorIndex >= Document.Accessors.Count then
      begin
        WritelnWarning('glTF', 'glTF missing accessor for attribute, accessor %d', [AttributeAccessorIndex]);
        Exit;
      end;

      Accessor := Document.Accessors[AttributeAccessorIndex];
      WritelnLog('glTF', '  Primitive has attribute with accessor of type ' + AccessorTypeToStr(Accessor.Type_));
      if AttributeName = 'POSITION' then
      begin
        Position := Accessor.DecodeAsVector3Array(true);
        WritelnLog('glTF', '  Extracted position to array with count %d',
          [Length(Position)]);
      end;
    end;

    { Convert Indices, Position to X3D }

    Coordinate := TCoordinateNode.Create;
    Coordinate.FdPoint.Count := Length(Position);
    for I := 0 to High(Position) do
      // TODO: could be copied with one Move
      Coordinate.FdPoint.Items[I] := Vector3FromGltf(Position[I]);

    if Primitive.Indices <> -1 then
    begin
      GeometryIndexedTriangleSet := TIndexedTriangleSetNode.CreateWithShape(Shape);
      GeometryIndexedTriangleSet.Coord := Coordinate;
      GeometryIndexedTriangleSet.FdIndex.Count := Length(Indices);
      for I := 0 to High(Indices) do
        GeometryIndexedTriangleSet.FdIndex.Items[I] := Indices[I];
      Geometry := GeometryIndexedTriangleSet;
    end else
    begin
      GeometryTriangleSet := TTriangleSetNode.CreateWithShape(Shape);
      GeometryTriangleSet.Coord := Coordinate;
      Geometry := GeometryTriangleSet;
    end;

    Result.AddChildren(Shape);
  end;

var
  Stream: TStream;
  Mesh: TPasGLTF.TMesh;
  Primitive: TPasGLTF.TMesh.TPrimitive;
begin
  Stream := Download(URL, []);
  try
    Result := TX3DRootNode.Create('', URL);
    try
      Document := TPasGLTF.TDocument.Create;
      try
        Document.RootPath := InclPathDelim(ExtractFilePath(URIToFilenameSafe(URL)));
        Document.LoadFromStream(Stream);

        WritelnLogMultiline('glTF', Format(
          'Asset.Copyright: %s' + NL +
          'Asset.Generator: %s' + NL +
          'Asset.MinVersion: %s' + NL +
          'Asset.Version: %s' + NL +
          'Asset.Empty: %s' + NL +
          'Accessors: %d' + NL +
          'Animations: %d' + NL +
          'Buffers: %d' + NL +
          'BufferViews: %d' + NL +
          'Cameras: %d' + NL +
          'Images: %d' + NL +
          'Materials: %d' + NL +
          'Meshes: %d' + NL +
          'Nodes: %d' + NL +
          'Samplers: %d' + NL +
          'Scenes: %d' + NL +
          'Skins: %d' + NL +
          'Textures: %d' + NL +
          'ExtensionsUsed: %s' + NL +
          'ExtensionsRequired: %s' + NL +
          '',
          [Document.Asset.Copyright,
           Document.Asset.Generator,
           Document.Asset.MinVersion,
           Document.Asset.Version,
           BoolToStr(Document.Asset.Empty, true),

           Document.Accessors.Count,
           Document.Animations.Count,
           Document.Buffers.Count,
           Document.BufferViews.Count,
           Document.Cameras.Count,
           Document.Images.Count,
           Document.Materials.Count,
           Document.Meshes.Count,
           Document.Nodes.Count,
           Document.Samplers.Count,
           Document.Scenes.Count,
           Document.Skins.Count,
           Document.Textures.Count,
           Document.ExtensionsUsed.Text,
           Document.ExtensionsRequired.Text
          ])
        );
        if Document.Scene <> -1 then
          WritelnLog('glTF', 'Current scene %d name: "%s"',
            [Document.Scene, Document.Scenes[Document.Scene].Name]);
        if Document.ExtensionsRequired.IndexOf('KHR_draco_mesh_compression') <> -1 then
          WritelnWarning('Required extension KHR_draco_mesh_compression not supported by glTF reader');

        for Mesh in Document.Meshes do
        begin
          WritelnLog('glTF', 'Mesh %s', [Mesh.Name]);
          for Primitive in Mesh.Primitives do
            ReadPrimitive(Primitive);
        end;
      finally FreeAndNil(Document) end;
    except FreeAndNil(Result); raise end;
  finally FreeAndNil(Stream) end;
end;

end.

