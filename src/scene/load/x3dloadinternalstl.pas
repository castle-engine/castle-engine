{
  Copyright 2017-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load and save models in the STL format.
  See https://en.wikipedia.org/wiki/STL_%28file_format%29 for
  more information about STL. }
unit X3DLoadInternalSTL;

{$I castleconf.inc}

interface

implementation

uses SysUtils, Classes,
  X3DNodes, X3DLoad, CastleClassUtils, CastleVectors, CastleUtils, CastleDownload,
  CastleTriangles, CastleLog, CastleStreamUtils,
  CastleShapes, CastleSceneCore;

{ Load STL text (ASCII) variation. }
procedure LoadSTLText(const Stream: TStream;
  const Coordinate, Normal: TVector3List);

  procedure AddTriangle(const Coordinate, Normal: TVector3List;
    NormalVector: TVector3; const Triangle: TTriangle3);
  begin
    { if the STL file specifies zero vector, calculate it }
    if NormalVector.IsPerfectlyZero then
      NormalVector := Triangle.Normal;

    { add 3 times the same NormalVector.
      See TODO about TriangleSet.NormalPerVertex lower in this file. }
    Normal.Add(NormalVector);
    Normal.Add(NormalVector);
    Normal.Add(NormalVector);

    Coordinate.Add(Triangle.Data[0]);
    Coordinate.Add(Triangle.Data[1]);
    Coordinate.Add(Triangle.Data[2]);
  end;

var
  TextReader: TTextReader;

  { Read a word expecting the ExpectedValue. Returns @true if found expected,
    otherwise returns @false (and the warning is already emitted). }
  function ReadExpect(const ExpectedValue: String): boolean;
  var
    S: String;
  begin
    S := TextReader.Read;
    Result := S = ExpectedValue;
    if not Result then
    begin
      if S = '' then
        WritelnWarning('STL', 'Unexpected end of the file, expected "' + ExpectedValue + '"')
      else
        WritelnWarning('STL', 'Unexpected word "' + S + '" in file, expected "' + ExpectedValue + '"');
    end;
  end;

var
  S: String;
  NormalVector: TVector3;
  Triangle: TTriangle3;
  I: Integer;
begin
  TextReader := TTextReader.Create(Stream, false);
  try
    TextReader.Readln; // read header line

    repeat
      { read triangle }
      S := TextReader.Read;
      if S = 'facet' then
      begin
        { S = facet -> new triangle }
        if not ReadExpect('normal') then Exit;
        NormalVector := TextReader.ReadVector3;
        if not ReadExpect('outer') then Exit;
        if not ReadExpect('loop') then Exit;
        for I := 0 to 2 do
        begin
          if not ReadExpect('vertex') then Exit;
          Triangle.Data[I] := TextReader.ReadVector3;
        end;
        AddTriangle(Coordinate, Normal, NormalVector, Triangle);
        if not ReadExpect('endloop') then Exit;
        if not ReadExpect('endfacet') then Exit;
      end else
      begin
        { S = anything else -> end of reading }
        if S <> 'endsolid' then
          WritelnWarning('STL', 'Unexpected word "' + S + '" in file, expected "facet" or "endsolid"');
        Exit;
      end;
    until false;

  finally FreeAndNil(TextReader) end;
end;

const
  BinaryHeader = 80;

procedure LittleEndianToNative(var V: TVector3);
begin
  {$ifdef ENDIAN_BIG}
  SwapEndian(V.X);
  SwapEndian(V.Y);
  SwapEndian(V.Z);
  {$endif ENDIAN_BIG}
end;

procedure NativeToLittleEndian(var V: TVector3);
begin
  {$ifdef ENDIAN_BIG}
  SwapEndian(V.X);
  SwapEndian(V.Y);
  SwapEndian(V.Z);
  {$endif ENDIAN_BIG}
end;

{ Load STL binary variation. }
procedure LoadSTLBinary(const Stream: TStream;
  const Coordinate, Normal: TVector3List);
var
  TriangleCount: UInt32;
  NormalVector: TVector3;
  Triangle: TTriangle3;
  I: Integer;
  TriangleAttribute: Word;
begin
  Stream.Position := BinaryHeader;
  Stream.ReadLE(TriangleCount);

  Coordinate.Count := TriangleCount * 3;
  Normal.Count := TriangleCount * 3;

  for I := 0 to TriangleCount - 1 do
  begin
    Stream.ReadBuffer(NormalVector, SizeOf(NormalVector));
    LittleEndianToNative(NormalVector);

    Stream.ReadBuffer(Triangle, SizeOf(Triangle));
    LittleEndianToNative(Triangle.Data[0]);
    LittleEndianToNative(Triangle.Data[1]);
    LittleEndianToNative(Triangle.Data[2]);

    { we read and ignore for now the TriangleAttribute,
      see https://en.wikipedia.org/wiki/STL_%28file_format%29 for it's meaning }
    Stream.ReadLE(TriangleAttribute);

    { Add the triangle to Coordinate and Normal arrays now.
      We could use the same AddTriangle procedure as the LoadSTLText,
      but it would be less efficient -- it causes a resize when adding each triangle.
      Here, we know the triangle count beforehand. }

    { if the STL file specifies zero vector, calculate it }
    if NormalVector.IsPerfectlyZero then
      NormalVector := Triangle.Normal;

    Coordinate[I * 3    ] := Triangle.Data[0];
    Coordinate[I * 3 + 1] := Triangle.Data[1];
    Coordinate[I * 3 + 2] := Triangle.Data[2];

    Normal[I * 3    ] := NormalVector;
    Normal[I * 3 + 1] := NormalVector;
    Normal[I * 3 + 2] := NormalVector;
  end;
end;

{ Is STL a text file.

  We used to detect it by looking if it starts with "solid", like:

    Stream.ReadBuffer(Header, 5);
    if Header = 'solid' then...

  But this fails for some STL files, see
  https://github.com/castle-engine/castle-engine/issues/433 .
  We now know follow Blender's approach,
  https://github.com/blender/blender-addons/blob/main/io_mesh_stl/stl_utils.py#L60 .

  This resets Stream.Position to 0 after read. }
function DetectSTLText(const Stream: TStream): Boolean;
const
  { Matches triangle read by LoadSTLBinary. }
  StlTriangleSize = SizeOf(TVector3) + SizeOf(TTriangle3) + SizeOf(Word);
var
  TriangleCount: UInt32;
begin
  if Stream.Size < BinaryHeader + SizeOf(TriangleCount) then
    Exit(true);

  Stream.Position := BinaryHeader;
  Stream.ReadLE(TriangleCount);

  Result := not (
    Stream.Size = BinaryHeader + SizeOf(TriangleCount) + TriangleCount * StlTriangleSize
  );

  Stream.Position := 0;
end;

{ Load model in the STL format, converting it to an X3D nodes graph.
  This is used by @link(LoadNode) when the file format is STL. }
function LoadSTL(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Material: TMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
  TriangleSet: TTriangleSetNode;
  Coordinate: TCoordinateNode;
  Normal: TNormalNode;
begin
  Result := TX3DRootNode.Create;
  try
    { setup common X3D nodes }
    Coordinate := TCoordinateNode.Create;

    Normal := TNormalNode.Create;

    { The TriangleSet is perfect for STL geometry, see
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/rendering.html#TriangleSet
      Just a list of vertexes, each 3 vertexes make a triangle. }
    TriangleSet := TTriangleSetNode.Create;
    TriangleSet.Coord := Coordinate;
    { TODO: NormalPerVertex := true on TriangleSet not supported (would allow
      to be more compact) }
    TriangleSet.NormalPerVertex := false;
    TriangleSet.Normal := Normal;

    { create some Material only to make it lit }
    Material := TMaterialNode.Create;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Appearance := Appearance;
    Shape.Geometry := TriangleSet;
    Result.AddChildren(Shape);

    { actually read the file, filling Coordinate and Normal nodes }
    if DetectSTLText(Stream) then
      LoadSTLText(Stream, Coordinate.FdPoint.Items, Normal.FdVector.Items)
    else
      LoadSTLBinary(Stream, Coordinate.FdPoint.Items, Normal.FdVector.Items);

  except FreeAndNil(Result); raise end;
end;

type
  { Helper class to save STL.
    Enumerates all triangle and saves them to Stream. }
  TSaveStlHelper = class
    Stream: TStream;
    TriangleCount: UInt32;
    procedure ProcessTriangle(Shape: TObject;
      const Triangle: TTriangle3;
      const Normal: TTriangle3; const TexCoord: TTriangle4;
      const Face: TFaceIndex);
  end;

procedure TSaveStlHelper.ProcessTriangle(Shape: TObject;
  const Triangle: TTriangle3;
  const Normal: TTriangle3; const TexCoord: TTriangle4;
  const Face: TFaceIndex);
var
  N: TVector3;
  T: TTriangle3;
begin
  Inc(TriangleCount);

  N := Triangle.Normal;
  NativeToLittleEndian(N);
  Stream.WriteBuffer(N, SizeOf(N));

  T := Triangle;
  NativeToLittleEndian(T.Data[0]);
  NativeToLittleEndian(T.Data[1]);
  NativeToLittleEndian(T.Data[2]);
  Stream.WriteBuffer(T, SizeOf(T));

  { According to Wikipedia:
    https://en.wikipedia.org/wiki/STL_%28file_format%29
    """
    2-byte ("short") unsigned integer that is the "attribute byte count" –
    in the standard format, this should be zero because most software
    does not understand anything else.
    """ }
  Stream.WriteLE(0);
end;

{ Save model to STL format.
  This is used by @link(SaveNode) when the file format is STL. }
procedure SaveSTL(const Node: TX3DRootNode; const Stream: TStream;
  const Generator: String; const Source: String);
var
  HeaderStr: AnsiString;
  TriangleCount: UInt32;
  Helper: TSaveStlHelper;
  Scene: TCastleSceneCore;
  Shape: TShape;
begin
  HeaderStr := 'Binary STL generated by ' + Generator;
  // limit to BinaryHeader chars
  HeaderStr := Copy(HeaderStr, 1, BinaryHeader);
  // expand to BinaryHeader chars
  HeaderStr := HeaderStr + StringOfChar(' ', BinaryHeader - Length(HeaderStr));
  Assert(Length(HeaderStr) = BinaryHeader);

  Stream.WriteBuffer(HeaderStr[1], Length(HeaderStr));

  TriangleCount := 0;
  Stream.WriteLE(TriangleCount); // we will write this correctly later

  Scene := TCastleSceneCore.Create(nil);
  try
    if Node.Scene <> nil then
    begin
      { Do DeepCopy, to not change nodes (Scene, associated nodes in shapes...).

        Testcase: in castle-model-viewer,
        - open water_final_using_noise_from_shaders.x3dv
        - note that water animates (time sensor is associated with Scene),
        - save to STL,
        - should be no crash, and water should animate.

        If we would not do DeepCopy, then above would crash, as FreeAndNil(Scene)
        would clear time sensor scene. }
      Scene.Load(Node.DeepCopy as TX3DRootNode, true);
    end else
    begin
      // more efficient: use Node directly, we will unassociate from it later
      Scene.Load(Node, false);
    end;

    Helper := TSaveStlHelper.Create;
    try
      Helper.Stream := Stream;
      for Shape in Scene.Shapes.TraverseList(true) do
        Shape.Triangulate({$ifdef FPC}@{$endif} Helper.ProcessTriangle);
      TriangleCount := Helper.TriangleCount;
    finally FreeAndNil(Helper) end;
  finally FreeAndNil(Scene) end;

  Stream.Position := BinaryHeader;
  Stream.WriteLE(TriangleCount); // write TriangleCount correctly now
end;

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadSTL;
  ModelFormat.OnLoadForceMemoryStream := true;
  ModelFormat.OnSave := {$ifdef FPC}@{$endif} SaveSTL;
  ModelFormat.MimeTypes.Add('application/x-stl');
  // other STL mime types
  ModelFormat.MimeTypes.Add('application/wavefront-stl');
  ModelFormat.MimeTypes.Add('application/vnd.ms-pki.stl');
  ModelFormat.MimeTypes.Add('application/x-navistyle');
  ModelFormat.FileFilterName := 'STereo Lithography (*.stl)';
  ModelFormat.Extensions.Add('.stl');
  RegisterModelFormat(ModelFormat);
end.
