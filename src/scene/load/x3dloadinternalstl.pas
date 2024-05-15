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

uses SysUtils, Classes,
  X3DNodes;

{ Load model in the STL format, converting it to an X3D nodes graph.
  This is used by @link(LoadNode) when the file format is STL. }
function LoadSTL(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

{ Save model to STL format.
  This is used by @link(SaveNode) when the file format is STL. }
procedure SaveSTL(const Node: TX3DRootNode; const Stream: TStream;
  const Generator: String; const Source: String);

implementation

uses CastleClassUtils, CastleVectors, CastleUtils, CastleDownload, CastleTriangles,
  CastleLog, CastleStreamUtils;

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

procedure SaveSTL(const Node: TX3DRootNode; const Stream: TStream;
  const Generator: String; const Source: String);
var
  HeaderStr: AnsiString;
  Normal: TVector3;
  Triangle: TTriangle3;
  TriangleCount: UInt32;
  I: Integer;
begin
  HeaderStr := 'Binary STL generated by ' + Generator;
  // limit to BinaryHeader chars
  HeaderStr := Copy(HeaderStr, 1, BinaryHeader);
  // expand to BinaryHeader chars
  HeaderStr := HeaderStr + StringOfChar(' ', BinaryHeader - Length(HeaderStr));
  Assert(Length(HeaderStr) = BinaryHeader);

  Stream.WriteBuffer(HeaderStr[1], Length(HeaderStr));

  TriangleCount := 1;
  Stream.WriteLE(TriangleCount);

  for I := 0 to TriangleCount - 1 do
  begin
    Triangle[0] := Vector3(0, 0, 0);
    Triangle[1] := Vector3(1, 0, 0);
    Triangle[2] := Vector3(0, 1, 0);
    Normal := Triangle.Normal;

    NativeToLittleEndian(Normal);
    Stream.WriteBuffer(Normal, SizeOf(Normal));

    NativeToLittleEndian(Triangle.Data[0]);
    NativeToLittleEndian(Triangle.Data[1]);
    NativeToLittleEndian(Triangle.Data[2]);
    Stream.WriteBuffer(Triangle, SizeOf(Triangle));

    { According to Wikipedia:
      https://en.wikipedia.org/wiki/STL_%28file_format%29
      """
      2-byte ("short") unsigned integer that is the "attribute byte count" –
      in the standard format, this should be zero because most software
      does not understand anything else.
      """ }
    Stream.WriteLE(0);
  end;
end;

end.
