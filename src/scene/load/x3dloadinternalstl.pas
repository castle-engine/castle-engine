{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D models in the STL format (@link(LoadSTL)). }
unit X3DLoadInternalSTL;

interface

uses SysUtils, Classes,
  X3DNodes;

{ Load 3D model in the STL format, converting it to an X3D nodes graph.
  This routine is internally used by the @link(LoadNode) to load an STL file.
  See https://en.wikipedia.org/wiki/STL_%28file_format%29 for
  more information about STL. }
function LoadSTL(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

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
  function ReadExpect(const ExpectedValue: string): boolean;
  var
    S: string;
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
  S: string;
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

{ Load STL binary variation. }
procedure LoadSTLBinary(const Stream: TStream;
  const Coordinate, Normal: TVector3List);
var
  RestOfHeader: array [0..79 - 5] of char;
  TriangleCount: LongWord;
  NormalVector: TVector3;
  Triangle: TTriangle3;
  I, J: Integer;
  TriangleAttribute: Word;
begin
  { read the rest of (ignored) binary header with ReadBuffer
    (to work with streams that don't support seeking). }
  Stream.ReadBuffer(RestOfHeader, 80 - 5);
  Stream.ReadLE(TriangleCount);

  Coordinate.Count := TriangleCount * 3;
  Normal.Count := TriangleCount * 3;

  for I := 0 to TriangleCount - 1 do
  begin
    Stream.ReadLE(NormalVector.X);
    Stream.ReadLE(NormalVector.Y);
    Stream.ReadLE(NormalVector.Z);
    for J := 0 to 2 do
    begin
      Stream.ReadLE(Triangle.Data[J].X);
      Stream.ReadLE(Triangle.Data[J].Y);
      Stream.ReadLE(Triangle.Data[J].Z);
    end;
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

function LoadSTL(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Header: array [0..4] of char;
  Shape: TShapeNode;
  TriangleSet: TTriangleSetNode;
  Coordinate: TCoordinateNode;
  Normal: TNormalNode;
begin
  Result := TX3DRootNode.Create('', BaseUrl);
  try
    { setup common X3D nodes }
    Coordinate := TCoordinateNode.Create('', BaseUrl);

    Normal := TNormalNode.Create('', BaseUrl);

    { The TriangleSet is perfect for STL geometry, see
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/rendering.html#TriangleSet
      Just a list of vertexes, each 3 vertexes make a triangle. }
    TriangleSet := TTriangleSetNode.Create('', BaseUrl);
    TriangleSet.Coord := Coordinate;
    { TODO: NormalPerVertex := true on TriangleSet not supported (would allow
      to be more compact) }
    TriangleSet.NormalPerVertex := false;
    TriangleSet.Normal := Normal;

    Shape := TShapeNode.Create('', BaseUrl);
    { assign some Material only to make it lit }
    Shape.Material := TMaterialNode.Create('', BaseUrl);
    Shape.Geometry := TriangleSet;
    Result.AddChildren(Shape);

    { actually read the file, filling Coordinate and Normal nodes }

    Stream.ReadBuffer(Header, 5);
    if Header = 'solid' then
      LoadSTLText(Stream, Coordinate.FdPoint.Items, Normal.FdVector.Items)
    else
      LoadSTLBinary(Stream, Coordinate.FdPoint.Items, Normal.FdVector.Items);

  except FreeAndNil(Result); raise end;
end;

end.
