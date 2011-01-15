{
  Copyright 2010-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Geometry represented as arrays (TGeometryArrays). }
unit GeometryArrays;

interface

uses KambiUtils, VectorMath, FGL;

type
  { Primitive geometry types. Analogous to OpenGL primitives. }
  TGeometryPrimitive = (gpTriangles);

  TTexCoordDimensions = 2..4;

  { @exclude Internally used by TGeometryArrays }
  TGeometryTexCoord = class
    Dimensions: TTexCoordDimensions;
    Offset: Integer;
  end;
  TGeometryTexCoordsList = specialize TFPGObjectList<TGeometryTexCoord>;

  { Geometry represented as arrays of indexes, vertex positions,
    texture coordinates and such. Many (eventually, all) geometry nodes
    (TVRMLGeometryNode) can be processed into an instance of this class.

    This can be used to render, as arrays here map very naturally to
    an efficient OpenGL vertex arrays and VBOs. We use interleaving,
    storing everything in two arrays: 1st one for positions and normals
    (stuff that changes during coordinate morphing, most common dynamic shapes,
    so we specifically think about optimizing it).
    2nd one for everything else (colors, tex coords, also GLSL attributes).
    This should allow for the most efficient usage, making use of interleaving
    and still allowing fast dynamic updates in common cases. }
  TGeometryArrays = class
  private
    FIndexes: TDynLongIntArray;
    FPrimitive: TGeometryPrimitive;
    FCount: Integer;

    FAttributeArray: Pointer;
    FAttributeSize: Cardinal;

    FCoordinateArray: Pointer;
    FCoordinateSize: Cardinal;

    HasColor: boolean;
    ColorOffset: Integer;

    FTexCoords: TGeometryTexCoordsList;

    { TODO: GLSLAttributesOffsets: TStringList; }
    procedure SetCount(const Value: Integer);

    procedure AddTexCoord(const Dimensions: TTexCoordDimensions;
      const TextureUnit: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    { Indexes to remaining arrays.

      If non-nil, we will render using these indexes,
      which means that items on the remaining lists (vertex positions,
      tex coords etc.) may be used multiple times. This is good
      (the lists may be possibly shorter, and GPU will be able to reuse
      more calculation results), but it's also limited: a vertex
      must always have the same properties in this case (e.g. the same
      normal vector, so shape must be completely smooth).

      When this is nil, we will simply use all the vertexes in order.
      So every item of the remaining lists will be processed exactly once,
      in the given order. This seems dumb, but actually we're often forced
      to use this: when you use flat (per-face) normals or colors,
      then the same vertex position must be used many times with different
      normal/color. If you want to use OpenGL vertex arrays for whole rendering,
      this vertex position will just have to be duplicated (which is OK,
      as the calculation results couldn't be shared anyway,
      since normal/color are different). }
    property Indexes: TDynLongIntArray read FIndexes write FIndexes;

    property Primitive: TGeometryPrimitive read FPrimitive write FPrimitive;

    { Memory containing vertex positions and normals, that is everything
      that changes during Coordinate.coord animation.
      CoordinateSize is size, in bytes, of one item of this array
      (currently just constant, 2 * TVector3Single).
      @groupBegin }
    property CoordinateArray: Pointer read FCoordinateArray;
    property CoordinateSize: Cardinal read FCoordinateSize;
    { @groupEnd }

    { Memory containing everything other vertex attribute, like color,
      texture coordinates and GLSL attributes.
      AttributeSize is size, in bytes, of one item of this array.
      @groupBegin }
    property AttributeArray: Pointer read FAttributeArray;
    property AttributeSize: Cardinal read FAttributeSize;
    { @groupEnd }

    function Position: PVector3Single;
    procedure IncPosition(var P: PVector3Single);

    { Allocated number of items in vertex positions, normals, colors
      and such arrays.
      You can only set this once.
      You must do all necessary AddColor / AddAttribute calls before setting this.
      You can access all Position / Normal etc. pointers ony after setting this. }
    property Count: Integer read FCount write SetCount;

    function Normal: PVector3Single;
    function Normal(const Index: Cardinal): PVector3Single;
    procedure IncNormal(var P: PVector3Single);

    procedure AddColor;
    function Color: PVector3Single;
    procedure IncColor(var P: PVector3Single);

    { TODO:
    procedure AddGLSLAttribute(const Name: string; const Size: Cardinal);
    function GLSLAttribute: Pointer;
    procedure IncGLSLAttribute(var P: Pointer); }

    { Allocated in AttributeArray texture coords.
      Index is texture unit (counted from renderer first available texture
      unit). If given item is @nil on this list, then this texture unit
      is not allocated (just like it would be outside of TexCoords.Count). }
    property TexCoords: TGeometryTexCoordsList read FTexCoords;

    procedure AddTexCoord2D(const TextureUnit: Cardinal);
    procedure AddTexCoord3D(const TextureUnit: Cardinal);
    procedure AddTexCoord4D(const TextureUnit: Cardinal);

    function TexCoord(const Dimensions: TTexCoordDimensions;
      const TextureUnit, Index: Cardinal): Pointer;

    function TexCoord2D(const TextureUnit, Index: Cardinal): PVector2Single;
    function TexCoord3D(const TextureUnit, Index: Cardinal): PVector3Single;
    function TexCoord4D(const TextureUnit, Index: Cardinal): PVector4Single;
  end;

implementation

uses SysUtils;

{ TGeometryArrays ------------------------------------------------------------ }

constructor TGeometryArrays.Create;
begin
  inherited;
  FCoordinateSize := SizeOf(TVector3Single) * 2;
  FAttributeSize := 0;
  FTexCoords := TGeometryTexCoordsList.Create;
end;

destructor TGeometryArrays.Destroy;
begin
  FreeAndNil(FIndexes);
  FreeAndNil(FTexCoords);
  FreeMemNiling(FCoordinateArray);
  FreeMemNiling(FAttributeArray);
  inherited;
end;

procedure TGeometryArrays.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    ReallocMem(FCoordinateArray, CoordinateSize * Value);
    ReallocMem(FAttributeArray, AttributeSize * Value);
  end;
end;

function TGeometryArrays.Position: PVector3Single;
begin
  Result := FCoordinateArray;
end;

procedure TGeometryArrays.IncPosition(var P: PVector3Single);
begin
  PtrUInt(P) += {CoordinateSize} SizeOf(TVector3Single) * 2;
end;

function TGeometryArrays.Normal: PVector3Single;
begin
  Result := PVector3Single(PtrUInt(PtrUInt(FCoordinateArray) +
    SizeOf(TVector3Single)));
end;

function TGeometryArrays.Normal(const Index: Cardinal): PVector3Single;
begin
  Result := PVector3Single(PtrUInt(PtrUInt(FCoordinateArray) +
    SizeOf(TVector3Single) + CoordinateSize * Index));
end;

procedure TGeometryArrays.IncNormal(var P: PVector3Single);
begin
  PtrUInt(P) += {CoordinateSize} SizeOf(TVector3Single) * 2;
end;

procedure TGeometryArrays.AddColor;
begin
  if not HasColor then
  begin
    HasColor := true;
    ColorOffset := AttributeSize;
    FAttributeSize += SizeOf(TVector3Single);
  end;
end;

function TGeometryArrays.Color: PVector3Single;
begin
  if HasColor then
    Result := PVector3Single(PtrUInt(PtrUInt(FAttributeArray) + ColorOffset)) else
    Result := nil;
end;

procedure TGeometryArrays.IncColor(var P: PVector3Single);
begin
  PtrUInt(P) += AttributeSize;
end;

procedure TGeometryArrays.AddTexCoord(const Dimensions: TTexCoordDimensions;
  const TextureUnit: Cardinal);
var
  OldCount, I: Integer;
begin
  if TextureUnit >= TexCoords.Count then
  begin
    OldCount := TexCoords.Count;
    TexCoords.Count := TextureUnit + 1;
    for I := OldCount to TexCoords.Count - 1 do
      TexCoords[I] := nil; { make sure new items are nil }
  end;

  Assert(TextureUnit < TexCoords.Count);

  if TexCoords[TextureUnit] = nil then
  begin
    TexCoords[TextureUnit] := TGeometryTexCoord.Create;
    TexCoords[TextureUnit].Dimensions := Dimensions;
    TexCoords[TextureUnit].Offset := AttributeSize;
    FAttributeSize += SizeOf(Single) * Dimensions;
  end else
  if TexCoords[TextureUnit].Dimensions <> Dimensions then
  begin
    raise Exception.CreateFmt('Texture unit %d is already allocated but for %-dimensional tex coords (while %d requested)',
      [TextureUnit, TexCoords[TextureUnit].Dimensions, Dimensions]);
  end;
end;

procedure TGeometryArrays.AddTexCoord2D(const TextureUnit: Cardinal);
begin
  AddTexCoord(2, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoord3D(const TextureUnit: Cardinal);
begin
  AddTexCoord(3, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoord4D(const TextureUnit: Cardinal);
begin
  AddTexCoord(4, TextureUnit);
end;

function TGeometryArrays.TexCoord(const Dimensions: TTexCoordDimensions;
  const TextureUnit, Index: Cardinal): Pointer;
begin
  if (TextureUnit < TexCoords.Count) and
     (TexCoords[TextureUnit] <> nil) then
  begin
    Assert(TexCoords[TextureUnit].Dimensions = Dimensions, 'Texture coord allocated but for different dimensions');
    Result := Pointer(PtrUInt(PtrUInt(FAttributeArray) +
      TexCoords[TextureUnit].Offset + Index * AttributeSize));
  end else
    Result := nil;
end;

function TGeometryArrays.TexCoord2D(const TextureUnit, Index: Cardinal): PVector2Single;
begin
  Result := PVector2Single(TexCoord(2, TextureUnit, Index));
end;

function TGeometryArrays.TexCoord3D(const TextureUnit, Index: Cardinal): PVector3Single;
begin
  Result := PVector3Single(TexCoord(3, TextureUnit, Index));
end;

function TGeometryArrays.TexCoord4D(const TextureUnit, Index: Cardinal): PVector4Single;
begin
  Result := PVector4Single(TexCoord(4, TextureUnit, Index));
end;

end.
