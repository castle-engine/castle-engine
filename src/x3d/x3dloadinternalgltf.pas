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
    // as it happens, both structures have the same memory layout, so copy by a fast Move
    Assert(SizeOf(V) = SizeOf(Result));
    Move(V, Result, SizeOf(Result));
  end;

  function GetAccessor(const AccessorIndex: Integer): TPasGLTF.TAccessor;
  begin
    if AccessorIndex < Document.Accessors.Count then
      Result := Document.Accessors[AccessorIndex]
    else
    begin
      Result := nil;
      WritelnWarning('glTF', 'Missing glTF accessor (index %d, but we only have %d accessors)',
        [AccessorIndex, Document.Accessors.Count]);
    end;
  end;

  procedure ReadPrimitive(const Primitive: TPasGLTF.TMesh.TPrimitive);
  var
    IndicesAccessor, AttributeAccessor: TPasGLTF.TAccessor;
    AttributeName: TPasGLTFUTF8String;
    IndicesArray: TPasGLTFInt32DynamicArray;
    PositionArray: TPasGLTF.TVector3DynamicArray;
    Shape: TShapeNode;
    // Geometry: TAbstractGeometryNode; // not needed for now
    GeometryTriangleSet: TTriangleSetNode;
    GeometryIndexedTriangleSet: TIndexedTriangleSetNode;
    Coord: TCoordinateNode;
    // TODO: not used yet: TexCoord: TTextureCoordinateNode;
    Len: Integer;
  begin
    if Primitive.Mode <> TPasGLTF.TMesh.TPrimitive.TMode.Triangles then
    begin
      WritelnWarning('glTF', 'Primitive mode not implemented yet: ' + PrimitiveModeToStr(Primitive.Mode));
      Exit;
    end;

    Coord := nil;
    // TODO TexCoord := nil;

    // parse attributes, initializing Coord, TexCoord and other such nodes
    for AttributeName in Primitive.Attributes.Keys do
    begin
      AttributeAccessor := GetAccessor(Primitive.Attributes[AttributeName]);
      if AttributeAccessor <> nil then
      begin
        if AttributeName = 'POSITION' then
        begin
          PositionArray := AttributeAccessor.DecodeAsVector3Array(true);
          Coord := TCoordinateNode.Create;
          Len := Length(PositionArray);
          Coord.FdPoint.Count := Len;
          // for I := 0 to High(Position) do
          //   Coord.FdPoint.Items[I] := Vector3FromGltf(Position[I]);
          // Faster version:
          if Len <> 0 then
            Move(PositionArray[0], Coord.FdPoint.Items.List^[0], SizeOf(TVector3) * Len);
        end;
      end;
    end;

    // read indexes
    if Primitive.Indices <> -1 then
    begin
      IndicesAccessor := GetAccessor(Primitive.Indices);
      if IndicesAccessor <> nil then
      begin
        { The argument aForVertex below is false.
          Tested that this is correct.
          This reflects the glTF spec:
          """
          For performance and compatibility reasons, each element of
          a vertex attribute must be aligned to 4-byte boundaries
          inside bufferView
          """
        }
        IndicesArray := IndicesAccessor.DecodeAsInt32Array(false);
      end;
    end;

    // create X3D geometry and shape nodes
    if Primitive.Indices <> -1 then
    begin
      GeometryIndexedTriangleSet := TIndexedTriangleSetNode.CreateWithShape(Shape);
      GeometryIndexedTriangleSet.Coord := Coord;
      Len := Length(IndicesArray);
      GeometryIndexedTriangleSet.FdIndex.Count := Len;
      // for I := 0 to High(Indices) do
      //   GeometryIndexedTriangleSet.FdIndex.Items[I] := Indices[I];
      // Faster version:
      if Len <> 0 then
        Move(IndicesArray[0], GeometryIndexedTriangleSet.FdIndex.Items.List^[0], SizeOf(LongInt) * Len);
      // Geometry := GeometryIndexedTriangleSet;
    end else
    begin
      GeometryTriangleSet := TTriangleSetNode.CreateWithShape(Shape);
      GeometryTriangleSet.Coord := Coord;
      // Geometry := GeometryTriangleSet;
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
