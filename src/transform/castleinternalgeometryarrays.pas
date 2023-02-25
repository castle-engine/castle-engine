{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Geometry represented as arrays (TGeometryArrays). }
unit CastleInternalGeometryArrays;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors, CastleTriangles, CastleRenderOptions,
  CastleInternalGLUtils;

type
  { Primitive geometry types. Analogous to OpenGL / OpenGLES primitives. }
  TGeometryPrimitive = (
    gpTriangles,
    gpTriangleFan,
    gpTriangleStrip,
    gpLineStrip,
    gpLineLoop,
    gpLines,
    gpPoints
  );

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
  TGeometryTexCoordList = {$ifdef FPC}specialize{$endif} TObjectList<TGeometryTexCoord>;

  TGeometryAttribType = (atFloat, atVector2, atVector3, atVector4,
    atMatrix3, atMatrix4);

  TColorPerVertexType = (ctNone, ctRgb, ctRgbAlpha);

  { GLSL attributes array information, for TGeometryArrays. }
  TGeometryAttrib = class
    Name: string;
    { Internal for our engine (as opposed to specified in 3D model file).
      This is only used to change warnings related to this attribute. }
    Internal: boolean;
    AType: TGeometryAttribType;
    Offset: Integer;
  end;
  TGeometryAttribList = class({$ifdef FPC}specialize{$endif} TObjectList<TGeometryAttrib>)
  public
    function Find(const Name: string): TGeometryAttrib;
  end;

  TGeometryIndex = TGLIndex;
  TGeometryIndexList = TGLIndexList;

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
    FCoordinatePreserveGeometryOrder: Boolean;

    FHasTangent: Boolean;
    FTangentOffset: Integer;

    FColorType: TColorPerVertexType;
    FColorMode: TColorMode;
    ColorOffset: Integer;
    FForceUnlit: boolean;
    FForcedUnlitColor: TVector4;

    FHasFogCoord: boolean;
    FogCoordOffset: Integer;
    FFogDirectValues: boolean;

    FTexCoords: TGeometryTexCoordList;
    FAttribs: TGeometryAttribList;

    FCullFace: boolean;
    FFrontFaceCcw: boolean;

    FFaces: TFaceIndexesList;

    procedure SetCount(const Value: Integer);
    procedure AddTexCoord(const Generation: TTextureCoordinateGeneration;
      const Dimensions: TTexCoordDimensions;
      const TextureUnit: Cardinal);
    function AddGLSLAttribute(const AType: TGeometryAttribType;
      const Name: string; const Internal: boolean): TGeometryAttrib;
    function GLSLAttributeIndex(const AType: TGeometryAttribType;
      const Name: string; const Index: Cardinal): PtrUInt; overload;
    function GLSLAttributeIndex(const AType: TGeometryAttribType;
      const A: TGeometryAttrib; const Index: Cardinal): PtrUInt; overload;
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

    { Memory containing vertex positions, normals and optionally tangents,
      that is everything that changes during Coordinate.coord animation.
      CoordinateSize is size, in bytes, of one item of this array.
      @groupBegin }
    property CoordinateArray: Pointer read FCoordinateArray;
    property CoordinateSize: Cardinal read FCoordinateSize;
    { @groupEnd }

    { Does the order of data in CoordinateArray preserves the order
      of geometry (order of vectors in TCoordinateNode.Coord
      in TAbstractGeometryNode.CoordField). }
    property CoordinatePreserveGeometryOrder: Boolean
      read FCoordinatePreserveGeometryOrder
      write FCoordinatePreserveGeometryOrder default false;

    { Memory containing everything other vertex attribute, like color,
      texture coordinates and GLSL attributes.
      AttributeSize is size, in bytes, of one item of this array.
      @groupBegin }
    property AttributeArray: Pointer read FAttributeArray;
    property AttributeSize: Cardinal read FAttributeSize;
    { @groupEnd }

    function Position: PVector3; overload;
    function Position(const Index: Cardinal): PVector3; overload;

    { Allocated number of items in vertex positions, normals, colors
      and such arrays.

      You can only set this once.
      You must do all necessary AddColor / AddAttribute calls before setting this.

      You can access all Position / Normal etc. pointers only after setting this.
      Also, IndexesCount and HasIndexes is stored at this point. }
    property Count: Integer read FCount write SetCount;

    function Normal: PVector3; overload;
    function Normal(const Index: Cardinal): PVector3; overload;

    { Add tangent information per-vertex to CoordinateArray. }
    procedure AddTangent;
    { Was AddTangent used. }
    property HasTangent: Boolean read FHasTangent;
    { Access tangent information per-vertex. }
    function Tangent(const Index: Cardinal = 0): PVector3;

    { Add color-per-vertex attribute.
      @param AMode Rendering mode, determines what shader does.
      @param WithAlpha If @true, we have RGBA colors (TVector4), otherwise we have RGB colors (TVector3).
    }
    procedure AddColor(const AMode: TColorMode; const WithAlpha: Boolean);
    { Access attribute created by AddColor(..., false) }
    function ColorRgb(const Index: Cardinal = 0): PVector3;
    { Access attribute created by AddColor(..., true) }
    function ColorRgbAlpha(const Index: Cardinal = 0): PVector4;
    property ColorType: TColorPerVertexType read FColorType;
    property ColorMode: TColorMode read FColorMode;

    { When ForceUnlit, the shape must be rendered like with UnlitMaterial,
      with UnlitMaterial.emissiveColor/alpha = ForcedUnlitColor.

      Note that (as with UnlitMaterial) the colors may be overridden
      per-vertex using Color array (X3D Color/ColorRGBA nodes).
      @groupBegin }
    property ForceUnlit: boolean read FForceUnlit write FForceUnlit default false;
    property ForcedUnlitColor: TVector4 read FForcedUnlitColor write FForcedUnlitColor;
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

    function TexCoord(const TextureUnit: Cardinal; const Index: Cardinal = 0): Pointer;
    function TexCoord2D(const TextureUnit: Cardinal; const Index: Cardinal = 0): PVector2;
    function TexCoord3D(const TextureUnit: Cardinal; const Index: Cardinal = 0): PVector3;
    function TexCoord4D(const TextureUnit: Cardinal; const Index: Cardinal = 0): PVector4;

    property Attribs: TGeometryAttribList read FAttribs;

    function AddGLSLAttributeFloat(const Name: string; const Internal: boolean): TGeometryAttrib;
    function AddGLSLAttributeVector2(const Name: string; const Internal: boolean): TGeometryAttrib;
    function AddGLSLAttributeVector3(const Name: string; const Internal: boolean): TGeometryAttrib;
    function AddGLSLAttributeVector4(const Name: string; const Internal: boolean): TGeometryAttrib;
    function AddGLSLAttributeMatrix3(const Name: string; const Internal: boolean): TGeometryAttrib;
    function AddGLSLAttributeMatrix4(const Name: string; const Internal: boolean): TGeometryAttrib;

    function GLSLAttribute(const A: TGeometryAttrib; const Offset: PtrUInt = 0): PtrUInt;

    function GLSLAttributeFloat(const Name: string; const Index: Cardinal = 0): PSingle; overload; deprecated 'use GLSLAttributeFloat with TGeometryAttrib parameter, it is faster in the usual case';
    function GLSLAttributeVector2(const Name: string; const Index: Cardinal = 0): PVector2; overload; deprecated 'use GLSLAttributeVector2 with TGeometryAttrib parameter, it is faster in the usual case';
    function GLSLAttributeVector3(const Name: string; const Index: Cardinal = 0): PVector3; overload; deprecated 'use GLSLAttributeVector3 with TGeometryAttrib parameter, it is faster in the usual case';
    function GLSLAttributeVector4(const Name: string; const Index: Cardinal = 0): PVector4; overload; deprecated 'use GLSLAttributeVector4 with TGeometryAttrib parameter, it is faster in the usual case';
    function GLSLAttributeMatrix3(const Name: string; const Index: Cardinal = 0): PMatrix3; overload; deprecated 'use GLSLAttributeMatrix3 with TGeometryAttrib parameter, it is faster in the usual case';
    function GLSLAttributeMatrix4(const Name: string; const Index: Cardinal = 0): PMatrix4; overload; deprecated 'use GLSLAttributeMatrix4 with TGeometryAttrib parameter, it is faster in the usual case';

    function GLSLAttributeFloat(const A: TGeometryAttrib; const Index: Cardinal = 0): PSingle; overload;
    function GLSLAttributeVector2(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector2; overload;
    function GLSLAttributeVector3(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector3; overload;
    function GLSLAttributeVector4(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector4; overload;
    function GLSLAttributeMatrix3(const A: TGeometryAttrib; const Index: Cardinal = 0): PMatrix3; overload;
    function GLSLAttributeMatrix4(const A: TGeometryAttrib; const Index: Cardinal = 0): PMatrix4; overload;

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
    PtrUInt(Result) := PtrUInt(Result) + PtrUInt(FIndexes.L);
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

procedure TGeometryArrays.AddTangent;
begin
  if not FHasTangent then
  begin
    FHasTangent := true;
    FTangentOffset := FCoordinateSize;
    FCoordinateSize := FCoordinateSize + SizeOf(TVector3);
  end;
end;

function TGeometryArrays.Tangent(const Index: Cardinal = 0): PVector3;
begin
  { When DataFreed, FCoordinateArray is already nil }
  Result := PVector3(PtrUInt(PtrUInt(FCoordinateArray) +
    FTangentOffset + CoordinateSize * Index));
end;

procedure TGeometryArrays.AddColor(const AMode: TColorMode; const WithAlpha: Boolean);
begin
  Assert(FColorType = ctNone, 'Color attribute already added');

  FColorMode := AMode;
  ColorOffset := AttributeSize;
  if WithAlpha then
  begin
    FColorType := ctRgbAlpha;
    FAttributeSize := FAttributeSize + SizeOf(TVector4);
  end else
  begin
    FColorType := ctRgb;
    FAttributeSize := FAttributeSize + SizeOf(TVector3);
  end;
end;

function TGeometryArrays.ColorRgbAlpha(const Index: Cardinal): PVector4;
begin
  Assert(FColorType = ctRgbAlpha);

  { When DataFreed, FAttributeArray is already nil }
  Result := PVector4(PtrUInt(PtrUInt(FAttributeArray) +
    ColorOffset + Index * AttributeSize));
end;

function TGeometryArrays.ColorRgb(const Index: Cardinal): PVector3;
begin
  Assert(FColorType = ctRgb);

  { When DataFreed, FAttributeArray is already nil }
  Result := PVector3(PtrUInt(PtrUInt(FAttributeArray) +
    ColorOffset + Index * AttributeSize));
end;

procedure TGeometryArrays.AddFogCoord;
begin
  if not HasFogCoord then
  begin
    FHasFogCoord := true;
    FogCoordOffset := AttributeSize;
    FAttributeSize := FAttributeSize + SizeOf(Single);
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
      FAttributeSize := FAttributeSize + SizeOf(Single) * Dimensions;
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
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TexCoords[ExistingTextureUnit].Dimensions?');
    {$endif}
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

function TGeometryArrays.AddGLSLAttribute(const AType: TGeometryAttribType;
  const Name: string; const Internal: boolean): TGeometryAttrib;
const
  AttribSizes: array[TGeometryAttribType] of Cardinal =
  ( SizeOf(Single),
    SizeOf(TVector2),
    SizeOf(TVector3),
    SizeOf(TVector4),
    SizeOf(TMatrix3),
    SizeOf(TMatrix4)
  );
begin
  Result := Attribs.Find(Name);
  if Result <> nil then
  begin
    if Result.AType <> AType then
      raise Exception.CreateFmt('GLSL attribute "%s" is already allocated but for different type (%s) than currently requested (%s)', [
        Name,
        AttribTypeName[Result.AType],
        AttribTypeName[AType]
      ]);
    if Result.Internal <> Internal then
      raise Exception.CreateFmt('GLSL attribute "%s" is already allocated but for different internal (%s) than currently requested (%s)', [
        Name,
        BoolToStr(Result.Internal, true),
        BoolToStr(Internal, true)
      ]);
  end else
  begin
    Result := TGeometryAttrib.Create;
    Result.Name := Name;
    Result.AType := AType;
    Result.Offset := AttributeSize;
    Result.Internal := Internal;
    FAttributeSize := FAttributeSize + AttribSizes[AType];

    Attribs.Add(Result);
  end;
end;

function TGeometryArrays.AddGLSLAttributeFloat(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atFloat, Name, Internal);
end;

function TGeometryArrays.AddGLSLAttributeVector2(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atVector2, Name, Internal);
end;

function TGeometryArrays.AddGLSLAttributeVector3(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atVector3, Name, Internal);
end;

function TGeometryArrays.AddGLSLAttributeVector4(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atVector4, Name, Internal);
end;

function TGeometryArrays.AddGLSLAttributeMatrix3(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atMatrix3, Name, Internal);
end;

function TGeometryArrays.AddGLSLAttributeMatrix4(const Name: string; const Internal: boolean): TGeometryAttrib;
begin
  Result := AddGLSLAttribute(atMatrix4, Name, Internal);
end;

function TGeometryArrays.GLSLAttributeIndex(const AType: TGeometryAttribType;
  const A: TGeometryAttrib; const Index: Cardinal): PtrUInt;
begin
  Assert(A.AType = AType, Format('GLSL attribute "%s" is allocated but for different type (%s) than currently requested (%s)', [
    A.Name,
    AttribTypeName[A.AType],
    AttribTypeName[AType]
  ]));

  { When DataFreed, FAttributeArray is already nil }
  Result := PtrUInt(FAttributeArray) + A.Offset + Index * AttributeSize;
end;

function TGeometryArrays.GLSLAttributeIndex(const AType: TGeometryAttribType;
  const Name: string; const Index: Cardinal): PtrUInt;
var
  A: TGeometryAttrib;
begin
  A := Attribs.Find(Name);

  if A <> nil then
    Result := GLSLAttributeIndex(AType, A, Index)
  else
    raise Exception.CreateFmt('GLSL attribute "%s" is not allocated', [Name]);
end;

function TGeometryArrays.GLSLAttribute(const A: TGeometryAttrib; const Offset: PtrUInt): PtrUInt;
begin
  { When DataFreed, FAttributeArray is already nil }
  Result := PtrUInt(FAttributeArray) + A.Offset + Offset;
end;

function TGeometryArrays.GLSLAttributeFloat(const Name: string; const Index: Cardinal = 0): PSingle;
begin
  Result := PSingle(GLSLAttributeIndex(atFloat, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector2(const Name: string; const Index: Cardinal = 0): PVector2;
begin
  Result := PVector2(GLSLAttributeIndex(atVector2, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector3(const Name: string; const Index: Cardinal = 0): PVector3;
begin
  Result := PVector3(GLSLAttributeIndex(atVector3, Name, Index));
end;

function TGeometryArrays.GLSLAttributeVector4(const Name: string; const Index: Cardinal = 0): PVector4;
begin
  Result := PVector4(GLSLAttributeIndex(atVector4, Name, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix3(const Name: string; const Index: Cardinal = 0): PMatrix3;
begin
  Result := PMatrix3(GLSLAttributeIndex(atMatrix3, Name, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix4(const Name: string; const Index: Cardinal = 0): PMatrix4;
begin
  Result := PMatrix4(GLSLAttributeIndex(atMatrix4, Name, Index));
end;

function TGeometryArrays.GLSLAttributeFloat(const A: TGeometryAttrib; const Index: Cardinal = 0): PSingle;
begin
  Result := PSingle(GLSLAttributeIndex(atFloat, A, Index));
end;

function TGeometryArrays.GLSLAttributeVector2(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector2;
begin
  Result := PVector2(GLSLAttributeIndex(atVector2, A, Index));
end;

function TGeometryArrays.GLSLAttributeVector3(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector3;
begin
  Result := PVector3(GLSLAttributeIndex(atVector3, A, Index));
end;

function TGeometryArrays.GLSLAttributeVector4(const A: TGeometryAttrib; const Index: Cardinal = 0): PVector4;
begin
  Result := PVector4(GLSLAttributeIndex(atVector4, A, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix3(const A: TGeometryAttrib; const Index: Cardinal = 0): PMatrix3;
begin
  Result := PMatrix3(GLSLAttributeIndex(atMatrix3, A, Index));
end;

function TGeometryArrays.GLSLAttributeMatrix4(const A: TGeometryAttrib; const Index: Cardinal = 0): PMatrix4;
begin
  Result := PMatrix4(GLSLAttributeIndex(atMatrix4, A, Index));
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
