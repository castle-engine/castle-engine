{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load models in the PLY format.
  See
  - https://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
    Examples on:
    - https://people.sc.fsu.edu/~jburkardt/data/ply/apple.ply
    - https://people.sc.fsu.edu/~jburkardt/data/ply/airplane.ply
  - https://paulbourke.net/dataformats/ply/
  - https://en.wikipedia.org/wiki/PLY_(file_format)
}
unit X3DLoadInternalPLY;

{$I castleconf.inc}

interface

implementation

uses SysUtils, Classes,
  X3DNodes, X3DLoad, CastleClassUtils, CastleVectors, CastleUtils, CastleDownload,
  CastleLog, CastleStreamUtils, CastleStringUtils, CastleSceneCore;

{ PLY property type ---------------------------------------------------------- }

type
  TPlyPropertyType = (
    ptInt8,
    ptUInt8,
    ptInt16,
    ptUInt16,
    ptInt32,
    ptUInt32,
    ptFloat32,
    ptFloat64
  );

const
  PropertyTypeNames: array [TPlyPropertyType] of String = (
    'int8',
    'uint8',
    'int16',
    'uint16',
    'int32',
    'uint32',
    'float32',
    'float64'
  );
  AltPropertyTypeNames: array [TPlyPropertyType] of String = (
    'char',
    'uchar', // visible on https://sketchfab.com/3d-models/kaktus-ply-7b7cc7188f17468595506500e186a9c0 }
    'short',
    'ushort',
    'int',
    'uint',
    'float', // visible on https://paulbourke.net/dataformats/ply/
    'double' // visible on https://sketchfab.com/3d-models/kaktus-ply-7b7cc7188f17468595506500e186a9c0 }
  );

function StringToPropertyType(const S: String): TPlyPropertyType;
var
  LowerS: String;
begin
  LowerS := LowerCase(S);
  for Result := Low(TPlyPropertyType) to High(TPlyPropertyType) do
    if (LowerS = PropertyTypeNames[Result]) or
      (LowerS = AltPropertyTypeNames[Result]) then
      Exit;
  raise EReadError.CreateFmt('Unknown PLY property type "%s"', [S]);
end;

const
  PropertyTypeSize: array [TPlyPropertyType] of Integer = (
    1, { ptInt8 }
    1, { ptUInt8 }
    2, { ptInt16 }
    2, { ptUInt16 }
    4, { ptInt32 }
    4, { ptUInt32 }
    4, { ptFloat32 }
    8  { ptFloat64 }
  );

{ PLY property --------------------------------------------------------------- }

type
  TPlyPropertyKnownNames = (
    pnUnknown,
    // vertex positions
    pnX, pnY, pnZ,
    // vertex normals
    pnNormalX, pnNormalY, pnNormalZ,
    // vertex colors
    pnRed, pnGreen, pnBlue, pnAlpha,
    // face vertex indices
    pnVertexIndices
  );

  { Property of a PLY, which can be either a scalar (like a vertex coordinate)
    or a list (like a list of vertex indices for a face). }
  TPlyProperty = record
    Name: String;
    { Meaning of @link(Name), if it is one of the known property names. }
    KnownName: TPlyPropertyKnownNames;
    { Is the property a list, used for face vertex indices. }
    IsList: Boolean;
    { Scalar type, or element type when @link(IsList). }
    PropertyType: TPlyPropertyType;
    { Count type, only when @link(IsList). }
    CountType: TPlyPropertyType;
  end;

  TPlyPropertyList = {$ifdef FPC}specialize{$endif} TStructList<TPlyProperty>;

const
  PlyPropertyKnownNames: array [TPlyPropertyKnownNames] of String = (
    '',
    'x', 'y', 'z',
    'nx', 'ny', 'nz',
    'red', 'green', 'blue', 'alpha',
    'vertex_indices'
  );

{ Determine TPlyPropertyKnownNames from S, or return pnUnknown. }
function StringToPlyPropertyKnownName(const S: String): TPlyPropertyKnownNames;
var
  LowerS: String;
begin
  LowerS := LowerCase(S);
  for Result := Succ(Low(TPlyPropertyKnownNames)) to High(TPlyPropertyKnownNames) do
    if LowerS = PlyPropertyKnownNames[Result] then
      Exit;
  if LowerS = 'vertex_index' then // alternative name for vertex_indices, visible on https://paulbourke.net/dataformats/ply/
    Exit(pnVertexIndices);
  Exit(pnUnknown);
end;

{ PLY element ---------------------------------------------------------------- }

type
  { Element of a PLY file, like "vertex" or "face". }
  TPlyElement = class
    Name: String;
    Count: Integer;
    Properties: TPlyPropertyList;
    constructor Create;
    destructor Destroy; override;
  end;

  TPlyElementList = {$ifdef FPC}specialize{$endif} TStructList<TPlyElement>;

constructor TPlyElement.Create;
begin
  inherited;
  Properties := TPlyPropertyList.Create;
end;

destructor TPlyElement.Destroy;
begin
  FreeAndNil(Properties);
  inherited;
end;

{ PLY format ----------------------------------------------------------------- }

type
  TPlyFormat = (pfAscii, pfBinaryLittleEndian, pfBinaryBigEndian);

const
  PlyFormatNames: array [TPlyFormat] of String = (
    'ascii',
    'binary_little_endian',
    'binary_big_endian'
  );

function StringToPlyFormat(const S: String): TPlyFormat;
var
  LowerS: String;
begin
  LowerS := LowerCase(S);
  for Result := Low(TPlyFormat) to High(TPlyFormat) do
    if LowerS = PlyFormatNames[Result] then
      Exit;
  raise EReadError.CreateFmt('Unknown PLY format "%s"', [S]);
end;

{ PLY reader ----------------------------------------------------------------- }

type
  { Read PLY contents. }
  TPlyReader = class
  strict private
    { Used to read data when format is ASCII. }
    TextReader: TCastleTextReader;

    { Read Element with vertex data.
      Element given here is always equal to VertexElement now. }
    procedure ReadVertices(const Element: TPlyElement;
      const Coordinates: TVector3List;
      const Normals: TVector3List;
      const ColorsRgba: TVector4List;
      const ColorsRgb: TVector3List);

    { Read Element with face data.
      Element given here is always equal to FaceElement now. }
    procedure ReadFaces(const Element: TPlyElement; const CoordIndex: TInt32List);

    { Skip Element that we don't want to read. }
    procedure SkipElement(const Element: TPlyElement);

    { Skip value, from ASCII or binary format depending on PlyFormat. }
    procedure SkipValue(const PropertyType: TPlyPropertyType);

    { Read integer, from ASCII or binary format depending on PlyFormat. }
    function ReadInteger(const PropertyType: TPlyPropertyType): Integer;

    { Read float (Single in Pascal), from ASCII or binary format depending on PlyFormat. }
    function ReadFloat(const T: TPlyPropertyType): Single;
  public
    // Set before calling @link(ReadHeader).
    Stream: TStream;

    // Rest of the public fields are set by @link(ReadHeader).

    PlyFormat: TPlyFormat;
    Elements: TPlyElementList;

    { Element describing vertices. Never @nil after @link(ReadHeader). }
    VertexElement: TPlyElement;

    { Element describing faces. May be @nil after @link(ReadHeader), if this is vertex data only. }
    FaceElement: TPlyElement;

    { For each TPlyPropertyKnownNames, index to the property in the vertex
      or face element, or -1 if not found.
      For pmUnknown, this is always -1.
      For pmVertexIndices, this refers to index in FaceElement.Properties.
      For others, this refers to index in VertexElement.Properties. }
    PropertyIndexes: array [TPlyPropertyKnownNames] of Integer;

    constructor Create;
    destructor Destroy; override;

    { Parse PLY header from Stream, setting PlyFormat, Elements
      an other public fields.
      After this, Stream position is right after "end_header" line.

      @link(Stream) must be set before calling this.

      This does not deal with X3D nodes. }
    procedure ReadHeader;

    { Read PLY data into the provided lists.
      Provide Normals <> nil only if the PLY has per-vertex normals.
      Provide Colors <> nil only if the PLY has per-vertex RGB(A) colors.
      Provide CoordIndex <> nil only if the PLY has face vertex indices. }
    procedure ReadData(
      const Coordinates: TVector3List;
      const Normals: TVector3List;
      const ColorsRgba: TVector4List;
      const ColorsRgb: TVector3List;
      const CoordIndex: TInt32List);
  end;

constructor TPlyReader.Create;
var
  PropertyIndex: TPlyPropertyKnownNames;
begin
  inherited;
  Elements := TPlyElementList.Create;
  // initialize all PropertyIndexes to -1
  for PropertyIndex := Low(TPlyPropertyKnownNames) to High(TPlyPropertyKnownNames) do
    PropertyIndexes[PropertyIndex] := -1;
end;

destructor TPlyReader.Destroy;
begin
  FreeAndNil(Elements);
  inherited;
end;

procedure TPlyReader.ReadHeader;

  { Read next line, including newline.
    Note: We cannot use TCastleTextReader here, as PLY format
    may switch to binary in the middle. }
  function ReadLine: String;
  var
    C: AnsiChar;
    S: AnsiString;
  begin
    S := '';
    while Stream.Read(C, SizeOf(C)) <> 0 do
    begin
      // support LF or CRLF line endings
      if C = #10 then
        Break;
      if C <> #13 then // just skip #13, assuming it's part of CRLF
        S := S + C;
    end;
    Result := S;
  end;

var
  Line, FirstToken: String;
  SeekPos: Integer;
  HasPlyFormat: Boolean;
  CurrentElement: TPlyElement;
  Prop: TPlyProperty;
  Token: String;
  PropIndex: Integer;
begin
  HasPlyFormat := false;
  CurrentElement := nil;
  Line := ReadLine;
  if Line <> 'ply' then
    raise EReadError.Create('PLY file must start with "ply"');

  repeat
    Line := ReadLine;

    if Line = 'end_header' then
      Break;

    if Line = '' then
      Continue;

    SeekPos := 1;
    FirstToken := NextToken(Line, SeekPos);

    if FirstToken = 'comment' then
      Continue; { ignore comments }

    if FirstToken = 'format' then
    begin
      Token := NextToken(Line, SeekPos);
      HasPlyFormat := true;
      PlyFormat := StringToPlyFormat(Token);
    end else
    if FirstToken = 'element' then
    begin
      CurrentElement := TPlyElement.Create;
      CurrentElement.Name := NextToken(Line, SeekPos);
      CurrentElement.Count := StrToInt(NextToken(Line, SeekPos));
      Elements.Add(CurrentElement);

      if CurrentElement.Name = 'vertex' then
        VertexElement := CurrentElement
      else
      if CurrentElement.Name = 'face' then
        FaceElement := CurrentElement
      else
        WritelnWarning('PLY', 'Unknown PLY element "%s", still parsing but ignoring it', [CurrentElement.Name]);
    end else
    if FirstToken = 'property' then
    begin
      if CurrentElement = nil then
      begin
        WritelnWarning('PLY', 'Property declaration before any element, ignoring');
        Continue;
      end;
      Token := NextToken(Line, SeekPos);
      if Token = 'list' then
      begin
        Prop.IsList := true;
        Prop.CountType := StringToPropertyType(NextToken(Line, SeekPos));
        Prop.PropertyType := StringToPropertyType(NextToken(Line, SeekPos));
      end else
      begin
        Prop.IsList := false;
        Prop.PropertyType := StringToPropertyType(Token);
        Prop.CountType := Default(TPlyPropertyType); // unused, set to anything to be deterministic
      end;
      Prop.Name := NextToken(Line, SeekPos);
      Prop.KnownName := StringToPlyPropertyKnownName(Prop.Name);

      PropIndex := CurrentElement.Properties.Count;
      CurrentElement.Properties.Add(Prop);

      if Prop.KnownName <> pnUnknown then
      begin
        if CurrentElement = VertexElement then
        begin
          if Prop.KnownName = pnVertexIndices then
          begin
            WritelnWarning('PLY', 'Vertex element should not have vertex_indices property, ignoring it');
            Continue;
          end;
          PropertyIndexes[Prop.KnownName] := PropIndex;
        end else
        if CurrentElement = FaceElement then
        begin
          if Prop.KnownName <> pnVertexIndices then
          begin
            WritelnWarning('PLY', 'Face element should only have vertex_indices property, ignoring known property name %s', [Prop.Name]);
            Continue;
          end;
          PropertyIndexes[Prop.KnownName] := PropIndex;
        end;
      end;
    end else
    begin
      WritelnWarning('PLY', 'Unknown PLY header keyword "%s", ignoring', [FirstToken]);
    end;

  until false;

  if not HasPlyFormat then
    raise EReadError.Create('PLY header missing "format" declaration');
  if VertexElement = nil then
    raise EReadError.Create('PLY header missing "vertex" element');
end;

function TPlyReader.ReadInteger(const PropertyType: TPlyPropertyType): Integer;

  { Read an integer value of type T in binary format. }
  function ReadBinaryAsInt(const T: TPlyPropertyType): Integer;
  var
    VInt8: Int8;
    VUInt8: UInt8;
    VInt16: Int16;
    VUInt16: UInt16;
    VInt32: Int32;
    VUInt32: UInt32;
    VFloat32: Single;
    VFloat64: Double;
  begin
    case T of
      ptInt8:
        begin
          Stream.ReadBuffer(VInt8, SizeOf(VInt8));
          Result := VInt8;
        end;
      ptUInt8:
        begin
          Stream.ReadBuffer(VUInt8, SizeOf(VUInt8));
          Result := VUInt8;
        end;
      ptInt16:
        begin
          Stream.ReadEndianess(VInt16, PlyFormat = pfBinaryLittleEndian);
          Result := VInt16;
        end;
      ptUInt16:
        begin
          Stream.ReadEndianess(VUInt16, PlyFormat = pfBinaryLittleEndian);
          Result := VUInt16;
        end;
      ptInt32:
        begin
          Stream.ReadEndianess(VInt32, PlyFormat = pfBinaryLittleEndian);
          Result := VInt32;
        end;
      ptUInt32:
        begin
          Stream.ReadEndianess(VUInt32, PlyFormat = pfBinaryLittleEndian);
          Result := VUInt32;
        end;
      ptFloat32:
        begin
          Stream.ReadEndianess(VFloat32, PlyFormat = pfBinaryLittleEndian);
          Result := Round(VFloat32);
        end;
      ptFloat64:
        begin
          Stream.ReadEndianess(VFloat64, PlyFormat = pfBinaryLittleEndian);
          Result := Round(VFloat64);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Unknown TPlyPropertyType value');
      {$endif}
    end;
  end;

begin
  if PlyFormat = pfAscii then
    Result := TextReader.ReadInteger
  else
    Result := ReadBinaryAsInt(PropertyType);
end;

procedure TPlyReader.SkipValue(const PropertyType: TPlyPropertyType);

  procedure SkipBinary(const T: TPlyPropertyType);
  var
    Dummy: array [0..7] of Byte;
  begin
    Assert(PropertyTypeSize[T] <= SizeOf(Dummy));
    // We skip by reading to Dummy, which is supported by any TStream type.
    // We don't use Stream.Seek, as some TStream types may not implement seeking.
    Stream.ReadBuffer(Dummy, PropertyTypeSize[T]);
  end;

begin
  if PlyFormat = pfAscii then
    TextReader.Read
  else
    SkipBinary(PropertyType);
end;

function TPlyReader.ReadFloat(const T: TPlyPropertyType): Single;

  { Read Single value of type T in binary format. }
  function ReadBinaryAsFloat(const T: TPlyPropertyType): Single;
  var
    VInt8: Int8;
    VUInt8: UInt8;
    VInt16: Int16;
    VUInt16: UInt16;
    VInt32: Int32;
    VUInt32: UInt32;
    VFloat32: Single;
    VFloat64: Double;
  begin
    case T of
      ptInt8:
        begin
          Stream.ReadBuffer(VInt8, SizeOf(VInt8));
          Result := VInt8;
        end;
      ptUInt8:
        begin
          Stream.ReadBuffer(VUInt8, SizeOf(VUInt8));
          Result := VUInt8;
        end;
      ptInt16:
        begin
          Stream.ReadEndianess(VInt16, PlyFormat = pfBinaryLittleEndian);
          Result := VInt16;
        end;
      ptUInt16:
        begin
          Stream.ReadEndianess(VUInt16, PlyFormat = pfBinaryLittleEndian);
          Result := VUInt16;
        end;
      ptInt32:
        begin
          Stream.ReadEndianess(VInt32, PlyFormat = pfBinaryLittleEndian);
          Result := VInt32;
        end;
      ptUInt32:
        begin
          Stream.ReadEndianess(VUInt32, PlyFormat = pfBinaryLittleEndian);
          Result := VUInt32;
        end;
      ptFloat32:
        begin
          Stream.ReadEndianess(VFloat32, PlyFormat = pfBinaryLittleEndian);
          Result := VFloat32;
        end;
      ptFloat64:
        begin
          Stream.ReadEndianess(VFloat64, PlyFormat = pfBinaryLittleEndian);
          Result := VFloat64;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Unknown TPlyPropertyType value');
      {$endif}
    end;
  end;

begin
  if PlyFormat = pfAscii then
    Result := TextReader.ReadSingle
  else
    Result := ReadBinaryAsFloat(T);
end;

procedure TPlyReader.SkipElement(const Element: TPlyElement);
var
  I, K, ListCount: Integer;
  Prop: TPlyProperty;
begin
  for I := 0 to Element.Count - 1 do
    for Prop in Element.Properties do
    begin
      if Prop.IsList then
      begin
        ListCount := ReadInteger(Prop.CountType);
        for K := 0 to ListCount - 1 do
          SkipValue(Prop.PropertyType);
      end else
        SkipValue(Prop.PropertyType);
    end;
end;

procedure TPlyReader.ReadVertices(const Element: TPlyElement;
  const Coordinates: TVector3List;
  const Normals: TVector3List;
  const ColorsRgba: TVector4List;
  const ColorsRgb: TVector3List);
var
  I, K, ListCount: Integer;
  Prop: TPlyProperty;
  V: TVector3;
  N: TVector3;
  C: TVector4;
begin
  for I := 0 to Element.Count - 1 do
  begin
    V := TVector3.Zero;
    N := TVector3.Zero;
    C := Vector4(1, 1, 1, 1);

    for Prop in Element.Properties do
    begin
      if Prop.IsList then
      begin
        { List properties in vertex element are unusual; skip them. }
        WritelnWarning('PLY reader: skipping list property in vertex element, which is unusual');
        ListCount := ReadInteger(Prop.CountType);
        for K := 0 to ListCount - 1 do
          SkipValue(Prop.PropertyType);
      end else
      begin
        case Prop.KnownName of
          pnX: V.X := ReadFloat(Prop.PropertyType);
          pnY: V.Y := ReadFloat(Prop.PropertyType);
          pnZ: V.Z := ReadFloat(Prop.PropertyType);
          pnNormalX: N.X := ReadFloat(Prop.PropertyType);
          pnNormalY: N.Y := ReadFloat(Prop.PropertyType);
          pnNormalZ: N.Z := ReadFloat(Prop.PropertyType);
          pnRed: C.X := ReadFloat(Prop.PropertyType) / 255.0;
          pnGreen: C.Y := ReadFloat(Prop.PropertyType) / 255.0;
          pnBlue: C.Z := ReadFloat(Prop.PropertyType) / 255.0;
          pnAlpha: C.W := ReadFloat(Prop.PropertyType) / 255.0;
          pnUnknown: SkipValue(Prop.PropertyType);
          pnVertexIndices:
            begin
              WritelnWarning('PLY reader: vertex element should not have vertex_indices property, ignoring it');
              SkipValue(Prop.PropertyType);
            end;
        end;
      end;
    end;

    Coordinates.Add(V);
    if Normals <> nil then
      Normals.Add(N);
    if ColorsRgba <> nil then
      ColorsRgba.Add(C);
    if ColorsRgb <> nil then
      ColorsRgb.Add(C.XYZ);
  end;
end;

procedure TPlyReader.ReadFaces(const Element: TPlyElement; const CoordIndex: TInt32List);
var
  I, K, ListCount: Integer;
  Prop: TPlyProperty;
begin
  for I := 0 to Element.Count - 1 do
    for Prop in Element.Properties do
    begin
      if Prop.IsList then
      begin
        ListCount := ReadInteger(Prop.CountType);
        if Prop.KnownName = pnVertexIndices then
        begin
          for K := 0 to ListCount - 1 do
            CoordIndex.Add(ReadInteger(Prop.PropertyType));
          CoordIndex.Add(-1);
        end else
        begin
          WritelnWarning('PLY', 'Face element has list property "%s" that is not vertex_indices, ignoring it', [Prop.Name]);
          for K := 0 to ListCount - 1 do
            SkipValue(Prop.PropertyType);
        end;
      end else
      begin
        WritelnWarning('PLY', 'Face element has scalar property "%s", ignoring it', [Prop.Name]);
        SkipValue(Prop.PropertyType);
      end;
    end;
end;

procedure TPlyReader.ReadData(
  const Coordinates: TVector3List;
  const Normals: TVector3List;
  const ColorsRgba: TVector4List;
  const ColorsRgb: TVector3List;
  const CoordIndex: TInt32List);
var
  Element: TPlyElement;
begin
  // for ASCII format, continue reading with a TCastleTextReader
  if PlyFormat = pfAscii then
    TextReader := TCastleTextReader.Create(Stream, false);
  try
    for Element in Elements do
    begin
      if Element = VertexElement then
        ReadVertices(Element, Coordinates, Normals, ColorsRgba, ColorsRgb)
      else
      if Element = FaceElement then
        ReadFaces(Element, CoordIndex)
      else
        SkipElement(Element);
    end;
  finally FreeAndNil(TextReader) end;
end;

{ Loading PLY to X3D ---------------------------------------------------------------- }

{ Load model in the PLY format, converting it to an X3D nodes graph.
  This is used by @link(LoadNode) when the file format is PLY. }
function LoadPLY(const Stream: TStream; const BaseUrl: String;
  const LoadOptions: TCastleSceneLoadOptions): TX3DRootNode;
var
  Reader: TPlyReader;

  // nodes
  CoordinateNode: TCoordinateNode;
  NormalNode: TNormalNode;
  // either TColorNode or TColorRgbaNode, depending on whether PLY has alpha
  ColorNode: TAbstractColorNode;
  // either TIndexedFaceSetNode or TPointSetNode, depending on whether PLY has faces
  Geometry: TAbstractGeometryNode;
  PointSet: TPointSetNode absolute Geometry;
  FaceSet: TIndexedFaceSetNode absolute Geometry;
  Material: TMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;

  // lists referencing the data in X3D nodes
  ColorsRgba: TVector4List;
  ColorsRgb: TVector3List;
  Coordinates: TVector3List;
  Normals: TVector3List;
  CoordIndex: TInt32List;
begin
  Result := TX3DRootNode.Create;
  try
    Reader := TPlyReader.Create;
    try
      Reader.Stream := Stream;
      Reader.ReadHeader;

      // create necessary X3D nodes, depending on what properties the PLY has

      CoordinateNode := TCoordinateNode.Create;
      Coordinates := CoordinateNode.FdPoint.Items;

      NormalNode := nil;
      Normals := nil;
      if (Reader.PropertyIndexes[pnNormalX] <> -1) and
         (Reader.PropertyIndexes[pnNormalY] <> -1) and
         (Reader.PropertyIndexes[pnNormalZ] <> -1) then
      begin
        NormalNode := TNormalNode.Create;
        Normals := NormalNode.FdVector.Items;
      end;

      ColorNode := nil;
      ColorsRgba := nil;
      ColorsRgb := nil;
      if (Reader.PropertyIndexes[pnRed] <> -1) and
         (Reader.PropertyIndexes[pnGreen] <> -1) and
         (Reader.PropertyIndexes[pnBlue] <> -1) then
      begin
        if Reader.PropertyIndexes[pnAlpha] <> -1 then
        begin
          ColorNode := TColorRgbaNode.Create;
          ColorsRgba := TColorRgbaNode(ColorNode).FdColor.Items;
        end else
        begin
          ColorNode := TColorNode.Create;
          ColorsRgb := TColorNode(ColorNode).FdColor.Items;
        end;
      end;

      CoordIndex := nil;
      if (Reader.FaceElement <> nil) and
         (Reader.PropertyIndexes[pnVertexIndices] <> -1) then
      begin
        FaceSet := TIndexedFaceSetNode.Create;
        CoordIndex := FaceSet.FdCoordIndex.Items;
        FaceSet.Coord := CoordinateNode;
        FaceSet.Solid := false;
        { PLY faces can be concave polygons in general. }
        FaceSet.Convex := false;
        FaceSet.NormalPerVertex := true;
        FaceSet.Normal := NormalNode;
        FaceSet.ColorPerVertex := true;
        FaceSet.Color := ColorNode;
        Assert(Geometry = FaceSet); // by absolute declaration
      end else
      begin
        PointSet := TPointSetNode.Create;
        PointSet.Coord := CoordinateNode;
        PointSet.Normal := NormalNode;
        PointSet.Color := ColorNode;
        Assert(Geometry = PointSet); // by absolute declaration
      end;

      Reader.ReadData(Coordinates, Normals, ColorsRgba, ColorsRgb, CoordIndex);
    finally FreeAndNil(Reader) end;

    { Create a lit material so geometry gets proper shading. }
    Material := TMaterialNode.Create;
    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;
    Shape := TShapeNode.Create;
    Shape.Appearance := Appearance;
    Shape.Geometry := Geometry;

    Result.AddChildren(Shape);
  except FreeAndNil(Result); raise end;
end;

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadPLY;
  ModelFormat.MimeTypes.Add('application/x-ply');
  ModelFormat.FileFilterName := 'Polygon File Format (*.ply)';
  ModelFormat.Extensions.Add('.ply');
  RegisterModelFormat(ModelFormat);
end.
