{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Geometry represented as arrays (TGeometryArrays). }
unit CastleGeometryArrays;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors, CastleTriangles, CastleRendererBaseTypes;

type
  { Primitive geometry types. Analogous to OpenGL / OpenGLES primitives. }
  TGeometryPrimitive = (gpTriangles, {$ifndef OpenGLES} gpQuads, {$endif}
    gpTriangleFan, gpTriangleStrip, gpLineStrip, gpPoints);

  TTexCoordDimensions = 2..4;

  { Texture coordinate generation methods.

    For their meaning, see the X3D specification about
    TextureCoordinateGenerator.mode values.
    We also add some extensions, see
    [https://castle-engine.io/x3d_extensions.php#section_ext_tex_coord_worldspace] and
    [https://castle-engine.io/x3d_extensions.php#section_ext_tex_coord_bounds].
    We also support ProjectedTextureCoordinate, see
    [https://castle-engine.io/x3d_extensions_shadow_maps.php#section_ext_texture_gen_projective].

    Special value tgExplicit means that texture coordinates are not generated. }
  TTextureCoordinateGeneration = (
    tgExplicit,
    tgBounds2d,
    tgBounds3d,
    tgSphereMap,
    tgCoord,
    tgCoordEye,
    tgCameraSpaceNormal,
    tgWorldSpaceNormal,
    tgCameraSpaceReflectionVector,
    tgWorldSpaceReflectionVector,
    tgProjection,
    tgMirrorPlane);

  TProjectorMatrixFunction = function: TMatrix4 of object;

  TTextureGenerationVectors = array [0..2] of TVector4;

  { Texture coord array information, for TGeometryArrays.
    If Generation <> tgExplicit, then the actual array data is not stored. }
  TGeometryTexCoord = class
    Generation: TTextureCoordinateGeneration;

    { If Generation is tgBounds2d or tgBounds3d, then
      these are vectors used to generate
      texture coords from local (object space) vertex positions.
      TextureGen[0] says how to generate S texture coord,
      TextureGen[1] says how to generate T texture coord,
      and TextureGen[2] (only for tgBounds3d) is fo R tex coord.}
    GenerationBoundsVector: TTextureGenerationVectors;

    { For Generation = tgProjection, this is the function that generates
      matrix used for glTexGen }
    GenerationProjectorMatrix: TProjectorMatrixFunction;

    { Dimensions, only for Generation = tgExplicit. }
    Dimensions: TTexCoordDimensions;

    { Offset, only for Generation = tgExplicit. }
    Offset: Integer;
  end;
  TGeometryTexCoordList = specialize TObjectList<TGeometryTexCoord>;

  TGeometryAttribType = (atFloat, atVector2, atVector3, atVector4,
    atMatrix3, atMatrix4);

  { GLSL attributes array information, for TGeometryArrays. }
  TGeometryAttrib = class
    Name: string;
    { Internal for our engine (as opposed to specified in 3D model file).
      This is only used to change warnings related to this attribute. }
    Internal: boolean;
    AType: TGeometryAttribType;
    Offset: Integer;
  end;
  TGeometryAttribList = class(specialize TObjectList<TGeometryAttrib>)
  public
    function Find(const Name: string): TGeometryAttrib;
  end;

  TGeometryIndex = {$ifdef GLIndexesShort} Word {$else} LongWord {$endif};
  TGeometryIndexList = {$ifdef GLIndexesShort} TWordList {$else} TLongWordList {$endif};

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
    FIndexes: TGeometryIndexList;
    FIndexesCount: Cardinal;
    FHasIndexes: boolean;
    FPrimitive: TGeometryPrimitive;
    FCount: Integer;
    FCounts: TCardinalList;
    FDataFreed: boolean;

    FAttributeArray: Pointer;
    FAttributeSize: Cardinal;

    FCoordinateArray: Pointer;
    FCoordinateSize: Cardinal;

    FHasColor: boolean;
    ColorOffset: Integer;
    FHasDefaultColor: boolean;
    FDefaultColor: TVector4;

    FHasFogCoord: boolean;
    FogCoordOffset: Integer;
    FFogDirectValues: boolean;

    FTexCoords: TGeometryTexCoordList;
    FAttribs: TGeometryAttribList;

    FCullFace: boolean;
    FFrontFaceCcw: boolean;
    FForceFlatShading: boolean;

    FFaces: TFaceIndexesList;

    procedure SetCount(const Value: Integer);
    procedure AddTexCoord(const Generation: TTextureCoordinateGeneration;
      const Dimensions: TTexCoordDimensions;
      const TextureUnit: Cardinal);
    procedure AddGLSLAttribute(const AType: TGeometryAttribType;
      const Name: string; const Internal: boolean);
    function GLSLAttribute(const AType: TGeometryAttribType;
      const Name: string; const Index: Cardinal): PtrUInt;
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
    property Indexes: TGeometryIndexList read FIndexes write FIndexes;

    (*Information about Indexes.

      Before using FreeData, you could as well use the @link(Indexes) property
      to get the same information. You can use Indexes[Index], Indexes <> nil,
      Indexes.Count and such. However, FreeData call (that you should use
      to conserve memory usage after loading arrays to VBO)
      releases the Indexes property, while these properties stay the same.

      IndexesPtr may be casted to "^TGeometryIndex", before the FreeData call.

      @groupBegin *)
    function IndexesPtr(const Index: Cardinal): PtrUInt;
    property IndexesCount: Cardinal read FIndexesCount;
    property HasIndexes: boolean read FHasIndexes;
    { @groupEnd }

    property Primitive: TGeometryPrimitive read FPrimitive write FPrimitive;

    { If this is assigned, then the vertexes are divided into groups.
      This is the only way to put many triangle strips, triangle fans and such
      into one TGeometryArrays instance. For normal sets of triangles and quads
      this has no use, as there's never a need to divide them for rendering.

      Each value of this list specifies to take consecutive number of vertexes
      for next primitive.
      If Indexes are assigned, then they are divided into groups.
      Otherwise, the other arrays (positions, normals etc.)
      are divided into groups.

      The sum of values must be equal to the Indexes.Count
      (if Indexes assigned) or arrays Count (if Indexes not assigned). }
    property Counts: TCardinalList read FCounts write FCounts;

    { Memory containing vertex positions and normals, that is everything
      that changes during Coordinate.coord animation.
      CoordinateSize is size, in bytes, of one item of this array
      (currently just constant, 2 * TVector3).
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

    function Position: PVector3;
    function Position(const Index: Cardinal): PVector3;
    procedure IncPosition(var P: PVector3);

    { Allocated number of items in vertex positions, normals, colors
      and such arrays.

      You can only set this once.
      You must do all necessary AddColor / AddAttribute calls before setting this.

      You can access all Position / Normal etc. pointers only after setting this.
      Also, IndexesCount and HasIndexes is stored at this point. }
    property Count: Integer read FCount write SetCount;

    function Normal: PVector3;
    function Normal(const Index: Cardinal): PVector3;
    procedure IncNormal(var P: PVector3);

    procedure AddColor;
    function Color(const Index: Cardinal = 0): PVector4;
    procedure IncColor(var P: PVector4);
    property HasColor: boolean read FHasColor;

    { When Color array is not initialized and HasDefaultColor,
      then the default color will be set to DefaultColor.
      @groupBegin }
    property HasDefaultColor: boolean read FHasDefaultColor write FHasDefaultColor default false;
    property DefaultColor: TVector4 read FDefaultColor write FDefaultColor;
    { @groupEnd }

    procedure AddFogCoord;
    function FogCoord(const Index: Cardinal = 0): PSingle;
    property HasFogCoord: boolean read FHasFogCoord;

    { If FogCoord present, does it specify direct fog intensities,
      that should be used to change pixel colors without any further processing.
      When this is @false, then fog coordinates are understood
      as distance from the eye, and they are processed by linear/exp equations
      before being used to blend pixel colors. }
    property FogDirectValues: boolean
      read FFogDirectValues write FFogDirectValues default false;

    { Allocated in AttributeArray texture coords.
      Index is texture unit (counted from renderer first available texture
      unit). If given item is @nil on this list, then this texture unit
      is not allocated (just like it would be outside of TexCoords.Count). }
    property TexCoords: TGeometryTexCoordList read FTexCoords;

    procedure AddTexCoord2D(const TextureUnit: Cardinal);
    procedure AddTexCoord3D(const TextureUnit: Cardinal);
    procedure AddTexCoord4D(const TextureUnit: Cardinal);
    { Add generated texture coord.
      Such texture coord will not have actual data allocated in the array
      (you're expected to instead setup and enable glTexGen when rendering).
      Generation passed here must not be tgExplicit. }
    procedure AddTexCoordGenerated(const Generation: TTextureCoordinateGeneration;
      const TextureUnit: Cardinal);
    { Add texture coord, with configuration copied from existing texture coord. }
    procedure AddTexCoordCopy(const NewTextureUnit, ExistingTextureUnit: Cardinal);

    function TexCoord(const TextureUnit, Index: Cardinal): Pointer;
    function TexCoord2D(const TextureUnit, Index: Cardinal): PVector2;
    function TexCoord3D(const TextureUnit, Index: Cardinal): PVector3;
    function TexCoord4D(const TextureUnit, Index: Cardinal): PVector4;

    property Attribs: TGeometryAttribList read FAttribs;

    procedure AddGLSLAttributeFloat(const Name: string; const Internal: boolean);
    procedure AddGLSLAttributeVector2(const Name: string; const Internal: boolean);
    procedure AddGLSLAttributeVector3(const Name: string; const Internal: boolean);
    procedure AddGLSLAttributeVector4(const Name: string; const Internal: boolean);
    procedure AddGLSLAttributeMatrix3(const Name: string; const Internal: boolean);
    procedure AddGLSLAttributeMatrix4(const Name: string; const Internal: boolean);

    function GLSLAttribute(A: TGeometryAttrib; const Offset: PtrUInt = 0): PtrUInt;

    function GLSLAttributeFloat(const Name: string; const Index: Cardinal = 0): PSingle;
    function GLSLAttributeVector2(const Name: string; const Index: Cardinal = 0): PVector2;
    function GLSLAttributeVector3(const Name: string; const Index: Cardinal = 0): PVector3;
    function GLSLAttributeVector4(const Name: string; const Index: Cardinal = 0): PVector4;
    function GLSLAttributeMatrix3(const Name: string; const Index: Cardinal = 0): PMatrix3;
    function GLSLAttributeMatrix4(const Name: string; const Index: Cardinal = 0): PMatrix4;

    { Should we use backface-culling (ignore some faces during rendering).

      Which faces are "back" (and will be culled) is determined by FrontFaceCcw.
      When FrontFaceCcw = @true, the the faces ordered counter-clockwise are front,
      and thus the faces ordered clockwise will be culled.
      When FrontFaceCcw = @false, the faces ordered counter-clockwise
      will be culled. }
    property CullFace: boolean
      read FCullFace write FCullFace default false;

    { Which faces are front, for backface-culling (see @link(CullFace))
      and for normals data (see @link(Normal)). }
    property FrontFaceCcw: boolean
      read FFrontFaceCcw write FFrontFaceCcw default false;

    { Make the whole rendering with flat shading. }
    property ForceFlatShading: boolean
      read FForceFlatShading write FForceFlatShading default false;

    { Release the allocated memory for arrays (CoordinateArray, AttributeArray,
      Indexes). Further calls to IndexesPtr, Normal, Color and such will
      return only an offset relative to the original arrays pointer.
      This is necessary if you loaded arrays data into GPU memory
      (like Vertex Buffer Object of OpenGL), and it is also optimal
      -- you should not need the data anymore, once loaded to VBO. }
    procedure FreeData;

    { Was FreeData called. }
    property DataFreed: boolean read FDataFreed;

    { Information about faces. Generated for some geometry types.
      Generated only when TArraysGenerator.FacesNeeded is @true.
      Generated only for indexed shapes. When Indexes <> nil,
      these have the same count as Indexes.Count. Otherwise these
      have the same count as our @link(Count). }
    property Faces: TFaceIndexesList read FFaces write FFaces;
  end;

implementation

uses SysUtils, CastleStringUtils;

{ TGeometryAttribList ------------------------------------------------------- }

function TGeometryAttribList.Find(const Name: string): TGeometryAttrib;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);

  Result := nil;
end;

{ TGeometryArrays ------------------------------------------------------------ }

constructor TGeometryArrays.Create;
begin
  inherited;
  FCoordinateSize := SizeOf(TVector3) * 2;
  FAttributeSize := 0;
  FTexCoords := TGeometryTexCoordList.Create;
  FAttribs := TGeometryAttribList.Create;
end;

destructor TGeometryArrays.Destroy;
begin
  FreeAndNil(FIndexes);
  FreeAndNil(FTexCoords);
  FreeAndNil(FAttribs);
  FreeAndNil(FCounts);
  FreeMemNiling(FCoordinateArray);
  FreeMemNiling(FAttributeArray);
  FreeAndNil(FFaces);
  inherited;
end;

procedure TGeometryArrays.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    ReallocMem(FCoordinateArray, CoordinateSize * Value);
    ReallocMem(FAttributeArray, AttributeSize * Value);

    { calculate FHasIndexes, FIndexesCount now }
    FHasIndexes := Indexes <> nil;
    if FHasIndexes then
      FIndexesCount := Indexes.Count else
      FIndexesCount := 0;
  end;
end;

function TGeometryArrays.IndexesPtr(const Index: Cardinal): PtrUInt;
begin
  Result := Index * SizeOf(TGeometryIndex);
  if not DataFreed then
    PtrUInt(Result) += PtrUInt(FIndexes.L);
end;

function TGeometryArrays.Position: PVector3;
begin
  { When DataFreed, FCoordinateArray is already nil }
  Result := FCoordinateArray;
end;

function TGeometryArrays.Position(const Index: Cardinal): PVector3;
begin
  { When DataFreed, FCoordinateArray is already nil }
  Result := PVector3(PtrUInt(FCoordinateArray) + CoordinateSize * Index);
end;

procedure TGeometryArrays.IncPosition(var P: PVector3);
begin
  PtrUInt(P) += {CoordinateSize} SizeOf(TVector3) * 2;
end;

function TGeometryArrays.Normal: PVector3;
begin
  { When DataFreed, FCoordinateArray is already nil }
  Result := PVector3(PtrUInt(PtrUInt(FCoordinateArray) +
    SizeOf(TVector3)));
end;

function TGeometryArrays.Normal(const Index: Cardinal): PVector3;
begin
  { When DataFreed, FCoordinateArray is already nil }
  Result := PVector3(PtrUInt(PtrUInt(FCoordinateArray) +
    SizeOf(TVector3) + CoordinateSize * Index));
end;

procedure TGeometryArrays.IncNormal(var P: PVector3);
begin
  PtrUInt(P) += {CoordinateSize} SizeOf(TVector3) * 2;
end;

procedure TGeometryArrays.AddColor;
begin
  if not HasColor then
  begin
    FHasColor := true;
    ColorOffset := AttributeSize;
    FAttributeSize += SizeOf(TVector4);
  end;
end;

function TGeometryArrays.Color(const Index: Cardinal): PVector4;
begin
  if HasColor then
    { When DataFreed, FAttributeArray is already nil }
    Result := PVector4(PtrUInt(PtrUInt(FAttributeArray) +
      ColorOffset + Index * AttributeSize)) else
    Result := nil;
end;

procedure TGeometryArrays.IncColor(var P: PVector4);
begin
  PtrUInt(P) += AttributeSize;
end;

procedure TGeometryArrays.AddFogCoord;
begin
  if not HasFogCoord then
  begin
    FHasFogCoord := true;
    FogCoordOffset := AttributeSize;
    FAttributeSize += SizeOf(Single);
  end;
end;

function TGeometryArrays.FogCoord(const Index: Cardinal = 0): PSingle;
begin
  if HasFogCoord then
    { When DataFreed, FAttributeArray is already nil }
    Result := PSingle(PtrUInt(PtrUInt(FAttributeArray) +
      FogCoordOffset + Index * AttributeSize)) else
    Result := nil;
end;

procedure TGeometryArrays.AddTexCoord(
  const Generation: TTextureCoordinateGeneration;
  const Dimensions: TTexCoordDimensions;
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
    TexCoords[TextureUnit].Generation := Generation;
    TexCoords[TextureUnit].Dimensions := Dimensions;
    if Generation = tgExplicit then
    begin
      TexCoords[TextureUnit].Offset := AttributeSize;
      FAttributeSize += SizeOf(Single) * Dimensions;
    end;
  end else
  if TexCoords[TextureUnit].Dimensions <> Dimensions then
  begin
    raise Exception.CreateFmt('Texture unit %d is already allocated but for %-dimensional tex coords (while %d requested)',
      [TextureUnit, TexCoords[TextureUnit].Dimensions, Dimensions]);
  end else
  if TexCoords[TextureUnit].Generation <> Generation then
  begin
    raise Exception.CreateFmt('Texture unit %d is already allocated but for different tex coords generation method',
      [TextureUnit]);
  end
end;

procedure TGeometryArrays.AddTexCoordGenerated(const Generation: TTextureCoordinateGeneration;
  const TextureUnit: Cardinal);
begin
  Assert(Generation <> tgExplicit);
  AddTexCoord(Generation, 2 { doesn't matter }, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoord2D(const TextureUnit: Cardinal);
begin
  AddTexCoord(tgExplicit, 2, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoord3D(const TextureUnit: Cardinal);
begin
  AddTexCoord(tgExplicit, 3, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoord4D(const TextureUnit: Cardinal);
begin
  AddTexCoord(tgExplicit, 4, TextureUnit);
end;

procedure TGeometryArrays.AddTexCoordCopy(
  const NewTextureUnit, ExistingTextureUnit: Cardinal);
begin
  if TexCoords[ExistingTextureUnit].Generation <> tgExplicit then
  begin
    AddTexCoordGenerated(TexCoords[ExistingTextureUnit].Generation, NewTextureUnit);
    TexCoords[NewTextureUnit].GenerationBoundsVector       := TexCoords[ExistingTextureUnit].GenerationBoundsVector;
    TexCoords[NewTextureUnit].GenerationProjectorMatrix    := TexCoords[ExistingTextureUnit].GenerationProjectorMatrix;
  end else
  case TexCoords[ExistingTextureUnit].Dimensions of
    2: AddTexCoord2D(NewTextureUnit);
    3: AddTexCoord3D(NewTextureUnit);
    4: AddTexCoord4D(NewTextureUnit);
    else raise EInternalError.Create('TexCoords[ExistingTextureUnit].Dimensions?');
  end;
end;

function TGeometryArrays.TexCoord(const TextureUnit, Index: Cardinal): Pointer;
begin
  if (TextureUnit < TexCoords.Count) and
     (TexCoords[TextureUnit] <> nil) then
  begin
    Assert(TexCoords[TextureUnit].Generation = tgExplicit, 'Texture coords are generated, not explicit, for this unit');
    { When DataFreed, FAttributeArray is already nil }
    Result := Pointer(PtrUInt(PtrUInt(FAttributeArray) +
      TexCoords[TextureUnit].Offset + Index * AttributeSize));
  end else
    Result := nil;
end;

function TGeometryArrays.TexCoord2D(const TextureUnit, Index: Cardinal): PVector2;
begin
  Assert(TexCoords[TextureUnit].Dimensions = 2, 'Texture coord allocated but for different dimensions');
  Result := PVector2(TexCoord(TextureUnit, Index));
end;

function TGeometryArrays.TexCoord3D(const TextureUnit, Index: Cardinal): PVector3;
begin
  Assert(TexCoords[TextureUnit].Dimensions = 3, 'Texture coord allocated but for different dimensions');
  Result := PVector3(TexCoord(TextureUnit, Index));
end;

function TGeometryArrays.TexCoord4D(const TextureUnit, Index: Cardinal): PVector4;
begin
  Assert(TexCoords[TextureUnit].Dimensions = 4, 'Texture coord allocated but for different dimensions');
  Result := PVector4(TexCoord(TextureUnit, Index));
end;

const
  AttribTypeName: array[TGeometryAttribType] of string =
  ( 'float', 'vec2', 'vec3', 'vec4', 'mat3', 'mat4' );

procedure TGeometryArrays.AddGLSLAttribute(const AType: TGeometryAttribType;
  const Name: string; const Internal: boolean);
const
  AttribSizes: array[TGeometryAttribType] of Cardinal =
  ( SizeOf(Single),
    SizeOf(TVector2),
    SizeOf(TVector3),
    SizeOf(TVector4),
    SizeOf(TMatrix3),
    SizeOf(TMatrix4)
  );
var
  A: TGeometryAttrib;
begin
  A := Attribs.Find(Name);
  if A <> nil then
  begin
    if A.AType <> AType then
      raise Exception.CreateFmt('GLSL attribute "%s" is already allocated but for different type (%s) than currently requested (%s)',
        [Name, AttribTypeName[A.AType], AttribTypeName[AType]]);
    if A.Internal <> Internal then
      raise Exception.CreateFmt('GLSL attribute "%s" is already allocated but for different internal (%s) than currently requested (%s)',
        [Name, BoolToStr(A.Internal, true), BoolToStr(Internal, true)]);
  end else
  begin
    A := TGeometryAttrib.Create;
    A.Name := Name;
    A.AType := AType;
    A.Offset := AttributeSize;
    A.Internal := Internal;
    FAttributeSize += AttribSizes[AType];

    Attribs.Add(A);
  end;
end;

procedure TGeometryArrays.AddGLSLAttributeFloat(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atFloat, Name, Internal);
end;

procedure TGeometryArrays.AddGLSLAttributeVector2(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atVector2, Name, Internal);
end;

procedure TGeometryArrays.AddGLSLAttributeVector3(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atVector3, Name, Internal);
end;

procedure TGeometryArrays.AddGLSLAttributeVector4(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atVector4, Name, Internal);
end;

procedure TGeometryArrays.AddGLSLAttributeMatrix3(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atMatrix3, Name, Internal);
end;

procedure TGeometryArrays.AddGLSLAttributeMatrix4(const Name: string; const Internal: boolean);
begin
  AddGLSLAttribute(atMatrix4, Name, Internal);
end;

function TGeometryArrays.GLSLAttribute(const AType: TGeometryAttribType;
  const Name: string; const Index: Cardinal): PtrUInt;
var
  A: TGeometryAttrib;
begin
  A := Attribs.Find(Name);

  if A <> nil then
  begin
    if A.AType <> AType then
      raise Exception.CreateFmt('GLSL attribute "%s" is allocated but for different type (%s) than currently requested (%s)',
        [Name, AttribTypeName[A.AType], AttribTypeName[AType]]);
    { When DataFreed, FAttributeArray is already nil }
    Result := PtrUInt(FAttributeArray) + A.Offset + Index * AttributeSize;
    Exit;
  end;

  raise Exception.CreateFmt('GLSL attribute "%s" is not allocated', [Name]);
end;

function TGeometryArrays.GLSLAttribute(A: TGeometryAttrib; const Offset: PtrUInt): PtrUInt;
begin
  { When DataFreed, FAttributeArray is already nil }
  Result := PtrUInt(FAttributeArray) + A.Offset + Offset;
end;

function TGeometryArrays.GLSLAttributeFloat(const Name: string; const Index: Cardinal = 0): PSingle;
begin
  Result := PSingle(GLSLAttribute(atFloat, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector2(const Name: string; const Index: Cardinal = 0): PVector2;
begin
  Result := PVector2(GLSLAttribute(atVector2, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector3(const Name: string; const Index: Cardinal = 0): PVector3;
begin
  Result := PVector3(GLSLAttribute(atVector3, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector4(const Name: string; const Index: Cardinal = 0): PVector4;
begin
  Result := PVector4(GLSLAttribute(atVector4, Name, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix3(const Name: string; const Index: Cardinal = 0): PMatrix3;
begin
  Result := PMatrix3(GLSLAttribute(atMatrix3, Name, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix4(const Name: string; const Index: Cardinal = 0): PMatrix4;
begin
  Result := PMatrix4(GLSLAttribute(atMatrix4, Name, Index));
end;

procedure TGeometryArrays.FreeData;
begin
  FDataFreed := true;
  FreeAndNil(FIndexes);
  FreeMemNiling(FCoordinateArray);
  FreeMemNiling(FAttributeArray);
  FreeAndNil(FFaces);
end;

end.
