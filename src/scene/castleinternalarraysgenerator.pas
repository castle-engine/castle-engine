{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating TGeometryArrays for shapes (TArraysGenerator). }
unit CastleInternalArraysGenerator;

{$I castleconf.inc}

interface

uses CastleShapes, X3DNodes, X3DFields, CastleUtils, CastleInternalGeometryArrays,
  CastleVectors;

type
  { Generate TGeometryArrays for a shape. This is the basis
    of our renderer, TRenderer will pass such arrays to OpenGL.
    Geometry must be based on coordinates when using this,
    that is TAbstractGeometryNode.Coord must return @true. }
  TArraysGenerator = class
  strict private
    FShape: TShape;
    FState: TX3DGraphTraverseState;
    FGeometry: TAbstractGeometryNode;

    FCurrentRangeNumber: Cardinal;
    FCoord: TMFVec3f;

    { Generalized version of AssignAttribute, AssignCoordinate. }
    procedure AssignAttributeOrCoordinate(
      const AttributeName: String;
      const TargetPtr: Pointer; const TargetItemSize: SizeInt;
      const SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
      const TrivialIndex: Boolean);
  protected
    { Indexes, only when Arrays.Indexes = nil but original node was indexed.
      This has only indexes >= 0.

      This is TUInt32List, not TGeometryIndexList -- eventual conversion
      32-bit indexes -> 16-bit (if necessary because of old OpenGLES) will happen later. }
    IndexesFromCoordIndex: TUInt32List;

    { Similar to IndexesFromCoordIndex, but not processed by CoordIndex[].
      That is,

        IndexesFromCoordIndex[Something] := CoordIndex.Items[SomethingElse];

      where

        TrivialIndexesFromCoordIndex[Something] := SomethingElse;

      This is only non-nil when

      - IndexesFromCoordIndex is non-nil
      - moreover, node generator could use TrivialIndex.

      Which is possible only when node may be indexed, but also use niPerVertexNonIndexed
      at the same time, which is only possible for IndexedFaceSet with
      non-trivial creaseAngle (the "Shape.NormalsCreaseAngle" then force
      niPerVertexNonIndexed).

      This is TUInt32List, not TGeometryIndexList -- eventual conversion
      32-bit indexes -> 16-bit (if necessary because of old OpenGLES) will happen later. }
    TrivialIndexesFromCoordIndex: TUInt32List;

    { Index to Arrays. Suitable always to index Arrays.Position / Color / Normal
      and other Arrays attribute arrays. Calculated in
      each TAbstractCoordinateGenerator.GenerateVertex,
      always call "inherited" first from GenerateVertex overrides.

      There are three cases:

      1. When CoordIndex <> nil (so we have indexed node) and
         Arrays.Indexes <> nil (so we can render it by indexes,
         because AllowIndexed = true) then it's an index to
         node coordinates. It's equivalent to CoordIndex[IndexNum],
         and it can be used to index node's Coord as well as Arrays.Position
         (since they are ordered the same in this case).

      2. When CoordIndex <> nil (so we have indexed node) and
         Arrays.Indexes = nil (so we cannot render it by indexes,
         because AllowIndexed = false) then it's a number of vertex,
         that is it's incremented in each TAbstractCoordinateGenerator.GenerateVertex
         call.

         In this case IndexesFromCoordIndex <> nil,
         and Arrays attributes have the same count as IndexesFromCoordIndex.Count.
         GenerateVertex must be called in exactly the same order
         as IndexesFromCoordIndex were generated for this.

      3. When CoordIndex = nil (so we don't have an indexed node,
         also Arrays.Indexes = IndexesFromCoordIndex = nil always in this case)
         then it's an index to attributes. This is the trivial case,
         as Arrays attributes are then ordered just like node's Coord.
         It's equal to IndexNum then.
    }
    ArrayIndexNum: Integer;

    { Generated TGeometryArrays instance, available inside GenerateCoordinate*. }
    Arrays: TGeometryArrays;

    { Coordinate index, taken from Geometry.CoordIndex.

      If @nil, then GenerateVertex (and all other
      routines taking some index) will just directly index Coord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). }
    CoordIndex: TMFLong;

    { Current shape properties, constant for the whole
      lifetime of the generator, set in constructor.
      @groupBegin }
    property Shape: TShape read FShape;
    property State: TX3DGraphTraverseState read FState;
    property Geometry: TAbstractGeometryNode read FGeometry;
    { @groupEnd }

    { Coordinates, taken from Geometry.Coord.
      Usually coming from (coord as Coordinate).points field.
      If @nil then nothing will be rendered.

      In our constructor we initialize Coord and CoordIndex
      from Geometry, using TAbstractGeometryNode.Coord and
      TAbstractGeometryNode.CoordIndex values. }
    property Coord: TMFVec3f read FCoord;

    { Generate arrays content for given vertex.
      Given IndexNum indexes Coord, or (if CoordIndex is assigned)
      indexes CoordIndex (and CoordIndex then indexes actual Coord). }
    procedure GenerateVertex(IndexNum: Integer); virtual;

    { Get vertex coordinate. Returned vertex is in local coordinate space
      (use State.Transform if you want to get global coordinates). }
    function GetVertex(IndexNum: integer): TVector3;

    { Count of indexes. You can pass index between 0 and CoordCount - 1
      to various methods taking an index, like GenerateVertex. }
    function CoordCount: Integer;

    { Generate contents of Arrays.
      These are all called only when Coord is assigned.

      GenerateCoordinate can be overridden only by the class
      that actually knows how to deconstruct (triangulate etc.) this node.
      It must call GenerateVertex (or call GenerateCoordsRange,
      that has to be then overridden to call GenerateVertex after inherited).

      GenerateCoordinateBegin, GenerateCoordinateEnd will be called
      before / after GenerateCoordinate. It's useful to override them for
      intermediate classes in this file, that cannot triangulate,
      but still want to add something before / after GenerateCoordinate.
      When overriding GenerateCoordinateBegin, always call inherited
      at the begin. When overriding GenerateCoordinateEnd, always call inherited
      at the end.
      @groupBegin }
    procedure GenerateCoordinate; virtual; abstract;
    procedure GenerateCoordinateBegin; virtual;
    procedure GenerateCoordinateEnd; virtual;
    { @groupEnd }

    { Generate arrays content for one coordinate range (like a face).
      This is not called, not used, anywhere in this base
      TAbstractCoordinateGenerator class.
      In descendants, it may be useful to use this, like
      Geometry.InternalMakeCoordRanges(State, @@GenerateCoordsRange).

      GenerateCoordsRange is supposed to generate the parts of the mesh
      between BeginIndex and EndIndex - 1 vertices.
      BeginIndex and EndIndex are indexes to CoordIndex array,
      if CoordIndex is assigned, or just indexes to Coord. }
    procedure GenerateCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); virtual;

    { The number of current range (like a face), equal to RangeNumber passed to
      GenerateCoordsRange. Read this only while in GenerateCoordsRange.
      In fact, this is just set by GenerateCoordsRange in this class
      (so call @code(inherited) first when overriding it).

      It's comfortable e.g. when you need RangeNumber inside GenerateVertex,
      and you know that GenerateVertex will be called only from
      GenerateCoordsRange. }
    property CurrentRangeNumber: Cardinal read FCurrentRangeNumber;

    { If CoordIndex assigned (this VRML/X3D node is IndexedXxx)
      then calculate and set IndexesFromCoordIndex here.
      This is also the place to set Arrays.Primitive and Arrays.Counts. }
    procedure PrepareIndexesPrimitives; virtual; abstract;

    { Called when constructing Arrays, before the Arrays.Count is set.

      Descendants can override this to do stuff like Arrays.AddColor or
      Arrays.AddAttribute('foo'). After this method, the other code assumes that
      Arrays.CoordinateSize and Arrays.AttributeSize is fully determined.

      Descendants can also set AllowIndexed
      to @false, if we can't use indexed rendering (because e.g. we have
      colors per-face, which means that the same vertex position may have
      different colors,  which means it has to be duplicated in arrays anyway,
      so there's no point in indexing). }
    procedure PrepareAttributes(var AllowIndexed: boolean); virtual;

    { Fill some attribute inside Arrays (in the Arrays.AttributeArray, Arrays.AttributeSize).

      Suitable for use when the SourceCount corresponds to the original shape Coordinate count,
      so the Source array (defined by SourcePtr, SourceItemSize, SourceCount) is accessed
      in the same way as vertex coordinates: it is either indexed by shape coordIndex/index,
      or it is not indexed. }
    procedure AssignAttribute(const AttributeName: String;
      const TargetPtr, SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
      const TrivialIndex: Boolean = false);

    { Fill some attribute inside Arrays (in the Arrays.CoordinateArray, Arrays.CoordinateSize).

      Suitable for use when the SourceCount corresponds to the original shape Coordinate count,
      so the Source array (defined by SourcePtr, SourceItemSize, SourceCount) is accessed
      in the same way as vertex coordinates: it is either indexed by shape coordIndex/index,
      or it is not indexed.

      AttributeName is only for debug (to display nice logs / warnings). }
    procedure AssignCoordinate(const AttributeName: String;
      const TargetPtr, SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
      const TrivialIndex: Boolean = false);
  public
    { Assign these before calling GenerateArrays.
      @groupBegin }
    TexCoordsNeeded: Cardinal;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;
    ShapeBumpMappingUsed: boolean;
    ShapeBumpMappingTextureCoordinatesId: Cardinal;
    { Do we need TGeometryArrays.Faces }
    FacesNeeded: boolean;
    { @groupEnd }

    constructor Create(AShape: TShape); virtual;

    { Create and generate Arrays contents. }
    function GenerateArrays: TGeometryArrays;

    class function BumpMappingAllowed: boolean; virtual;
  end;

  TArraysGeneratorClass = class of TArraysGenerator;

{ TArraysGenerator class suitable for given geometry.
  Returns @nil if not suitable generator for this node,
  which means that this node cannot be rendered through TGeometryArrays. }
function GetArraysGenerator(AGeometry: TAbstractGeometryNode): TArraysGeneratorClass;

implementation

uses SysUtils, Math, Generics.Collections,
  CastleLog, CastleTriangles, CastleColors, CastleBoxes, CastleTriangulate,
  CastleStringUtils, CastleRenderOptions, CastleInternalGLUtils;

{ Copying to interleaved memory utilities ------------------------------------ }

type
  EAssignInterleavedRangeError = class(Exception);

{ Copy Source contents to given Target memory. Each item in Target
  is separated by the TargetItemSize bytes.

  We copy CopyCount items (you usually want to pass the count of Target
  data here). Source.Count must be >= CopyCount,
  we check it and eventually raise EAssignInterleavedRangeError.

  Warning: this is safely usable only for arrays of types that don't
  require initialization / finalization. Otherwise target memory data
  will not be properly referenced.

  @raises EAssignInterleavedRangeError When Count < CopyCount. }
procedure AssignToInterleaved(
  const AttributeName: String;
  Target: Pointer; const TargetItemSize, CopyCount: SizeInt;
  Source: Pointer; const SourceItemSize, SourceCount: SizeInt);
var
  I: Integer;
begin
  if SourceCount < CopyCount then
    raise EAssignInterleavedRangeError.CreateFmt('Not enough items in %s: %d, but at least %d required', [
      AttributeName,
      SourceCount,
      CopyCount
    ]);

  for I := 0 to CopyCount - 1 do
  begin
    Move(Source^, Target^, SourceItemSize);
    PtrUInt(Source) := PtrUInt(Source) + SourceItemSize;
    PtrUInt(Target) := PtrUInt(Target) + TargetItemSize;
  end;
end;

{ Copy Source contents to given Target memory. Each item in Target
  is separated by the TargetItemSize bytes.

  We copy CopyCount items (you usually want to pass the count of Target
  data here). Indexes.Count must be >= CopyCount,
  we check it and eventually raise EAssignInterleavedRangeError.

  Item number I is taken from Source array, at Indexes[I], like Source[Indexes[I]].
  All values on Indexes list must be valid (that is >= 0 and < SourceCount),
  or we raise EAssignInterleavedRangeError.

  Warning: this is safely usable only for arrays of types that don't
  require initialization / finalization. Otherwise target memory data
  will not be properly referenced.

  @raises(EAssignInterleavedRangeError When Indexes.Count < CopyCount,
    or some index points outside of array.) }
procedure AssignToInterleavedIndexed(
  const AttributeName: String;
  Target: Pointer; const TargetItemSize, CopyCount: SizeInt;
  Source: Pointer; const SourceItemSize, SourceCount: SizeInt;
  const Indexes: TUInt32List);
var
  I: Integer;
  Index: UInt32;
begin
  if Indexes.Count < CopyCount then
    raise EAssignInterleavedRangeError.CreateFmt('Not enough items in %s: %d, but at least %d required', [
      AttributeName,
      Indexes.Count,
      CopyCount
    ]);

  for I := 0 to CopyCount - 1 do
  begin
    Index := Indexes.L[I];
    if Index >= SourceCount then
      raise EAssignInterleavedRangeError.CreateFmt('Invalid index: %d, but we have %d items in %s', [
        Index,
        SourceCount,
        AttributeName
      ]);

    { Beware to not make multiplication below (* ItemSize) using 64-bit ints.
      This would cause noticeable slowdown when using AssignToInterleavedIndexed
      for ArraysGenerator, that in turn affects dynamic scenes
      and especially dynamic shading like radiance_transfer. }
    Move(Pointer(PtrUInt(Source) + PtrUInt(Index) * PtrUInt(SourceItemSize))^,
      Target^, SourceItemSize);
    PtrUInt(Target) := PtrUInt(Target) + TargetItemSize;
  end;
end;

{ Like AssignToInterleaved, but there is only one Source value,
  that should be copied to all destinations. }
procedure AssignToInterleavedConstant(
  Target: Pointer; const TargetItemSize, CopyCount: SizeInt;
  const Source: Pointer; const SourceItemSize: SizeInt);
var
  I: Integer;
begin
  for I := 0 to CopyCount - 1 do
  begin
    Move(Source^, Target^, SourceItemSize);
    PtrUInt(Target) := PtrUInt(Target) + TargetItemSize;
  end;
end;

{ classes -------------------------------------------------------------------- }

type
  TTextureCoordsImplementation = (
    { Texture coords not generated, because not needed by Renderer. }
    tcNotGenerated,
    { All texture coords (on all texture units, for multi-texturing)
      are automatically generated.

      Actually we could use some other value
      like tcTexIndexed (anything else than tcNotGenerated) to indicate
      this, since for everything <> tcNotGenerated we actually check
      TexCoordGen to know if coords should be explicit or automatic.
      But using special tcAllGenerated feels cleaner. }
    tcAllGenerated,
    { IndexNum is an index to TexCoordIndex, as this indexes TexCoordGen/Array[TextureUnit]. }
    tcTexIndexed,
    { IndexNum is an index to CoordIndex, as this indexes TexCoordGen/Array[TextureUnit]. }
    tcCoordIndexed,
    { IndexNum is a direct index to TexCoordGen/Array[TextureUnit]. }
    tcNonIndexed);

  { Handle texture coordinates.

    Usage:

    - TexCoord is already set from TAbstractGeometryNode.TexCoord,
      so just be sure it's overriden for your Geometry node class.

      Don't worry, we will automatically check TexCoord class,
      and do something useful with it only if we can.
      When TexCoord = @nil,
      then we'll generate default texture coordinates,
      following VRML 2.0 / X3D IndexedFaceSet default texture coord algorithm.

      (X3D spec doesn't say what happens for nodes like
      IndexedTriangleSet when texture is specified (in Appearance.texture)
      but texture coords are not present.
      For now, we just generate texture coords for them just like for
      IndexedFaceSet.)

    - Set TexCoordIndex, if available. This works just like CoordIndex:

      If @nil, then GenerateVertex (and all other
      routines taking some index) will just directly index TexCoord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). Otherwise, they will index TexCoordIndex,
      and then TexCoordIndex provides index to TexCoord.

      As a special case, if TexCoordIndex is assigned but empty
      (actually, just checked as "shorter than CoordIndex")
      then IndexNum indexed CoordIndex. So CoordIndex acts as
      TexCoordIndex in this case. This case is specially for
      for IndexedFaceSet in VRML >= 2.0.

      Always when TexCoordIndex is non-nil, also make sure that CoordIndex
      is non-nil. When TexCoordIndex is nil, make sure that CoordIndex is nil.
      (This restriction may be removed in the future,
      but for now nothing needs it.)

    This class takes care to generate tex coords, or use supplied
    TexCoord and TexCoordIndex. At least for texture units in
    TexCoordsNeeded (although we may pass some more, will be unused).
    So you do not have to generate any texture coordinates
    in descendants. Everything related to textures is already
    handled in this class. }
  TAbstractTextureCoordinateGenerator = class(TArraysGenerator)
  private
    TexImplementation: TTextureCoordsImplementation;

    { Source of explicit texture coordinates, for each texture unit
      that has Arrays.TexCoord[I].Generation = tgExplicit.
      The value of Arrays.TexCoord[I].Dimensions determines
      which of these is actually used.

      These arrays have always exactly the same length,
      equal to Arrays.TexCoords.Count. }
    TexCoordArray2d: array of TMFVec2f;
    TexCoordArray3d: array of TMFVec3f;
    TexCoordArray4d: array of TMFVec4f;

    TexCoord: TX3DNode;
  protected
    TexCoordIndex: TMFLong;

    { Return texture coordinate for given vertex, identified by IndexNum.
      IndexNum indexes TexCoordGen/Array[TextureUnit], or TexCoordIndex
      (if TexCoordIndex assigned),
      or CoordIndex (if TexCoordIndex assigned but empty, for IndexedFaceSet).

      Returns @false if no texture coords are available, for given
      TextureUnit.

      Works in all cases when we actually render some texture.

      Overloaded version with only TVector2 just ignores the 3rd and
      4th texture coordinate, working only when texture coord is
      normal 2D coord. }
    function GetTextureCoord(IndexNum: integer;
      const TextureUnit: Cardinal; out Tex: TVector4): boolean; overload;
    function GetTextureCoord(IndexNum: integer;
      const TextureUnit: Cardinal; out Tex: TVector2): boolean; overload;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;

    procedure GenerateVertex(IndexNum: Integer); override;

    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
  end;

  TMaterials1Implementation = (miOverall,
    miPerVertexCoordIndexed,
    miPerVertexMatIndexed,
    miPerFace,
    miPerFaceMatIndexed);

  { Handle per-face or per-vertex VRML 1.0 materials.

    Usage:
    - Just set MaterialIndex and MaterialBinding using your node's fields.
      Call UpdateMat1Implementation afterwards.
    - For VRML >= 2.0 nodes, you don't have to do anything.
      You can just leave MaterialBinding as default BIND_DEFAULT,
      but it really doesn't matter: the only effect of this class
      are calls to Render_BindMaterial_1. And Render_BindMaterial_1
      simply does nothing for VRML >= 2.0 geometry nodes.

    Since this must have a notion of what "face" is, it assumes that
    your GenerateCoordsRange constitutes rendering of a single face.
    If this isn't true, then "per face" materials will not work
    correctly.

    Note that "per vertex" materials require smooth shading,
    so you should set this in your Render. There's no way to implement
    them with flat shading.

    Everything related to VRML 1.0 materials is already handled in this class. }
  TAbstractMaterial1Generator = class(TAbstractTextureCoordinateGenerator)
  private
    { Must be set in constructor. MaterialsBegin (may someday) depend on this.

      For this reason, call UpdateMat1Implementation inside descendant
      constructor after changing this. }
    Mat1Implementation: TMaterials1Implementation;
    FaceMaterial1Color: TVector4;
    function GetMaterial1Color(const MaterialIndex: Integer): TVector4;
  protected
    { You can leave MaterialIndex as nil if you are sure that
      MaterialBinding will not be any _INDEXED value.

      Be sure to change these only inside constructor,
      and call UpdateMat1Implementation afterwards. We want Mat1Implementation
      ready after constructor. }
    MaterialIndex: TMFLong;
    MaterialBinding: Integer;
    procedure UpdateMat1Implementation;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: integer); override;
  public
    constructor Create(AShape: TShape); override;
  end;

  { Handle per-face or per-vertex VRML >= 2.0 colors.

    Usage: set Color or ColorRGBA (at most one of them), ColorPerVertex,
    ColorIndex.

    If Color and ColorRGBA = @nil, this class will not do anything.
    Otherwise, colors will be used.

    ColorPerVertex specifies per-vertex or per-face.
    Just like for VRML 1.0, the same restrictions apply:
    - if you want per-face to work, then GenerateCoordsRange must
      correspond to a single face.
    - if you want per-vertex to work, you must use smooth shading.

    ColorIndex:

    - When ColorIndex is set and non-empty (worse optimized, seldom case), then

      - when ColorPerVertex: vertex IndexNum is an index to ColorIndex
      - when not ColorPerVertex: face number is an index to ColorIndex
      And then ColorIndex indexes Color items.

    - When ColorIndex is empty (better optimized, common case):

      - when ColorPerVertex: vertex IndexNum is an index to CoordIndex
        and CoordIndex in turn is an index to Color.
        Unless CoordIndex assigned, then vertex IndexNum indexes Color.
      - when not ColorPerVertex: face number in an index for Color.

  }
  TAbstractColorGenerator = class(TAbstractMaterial1Generator)
  strict private
    { Per-face color (used when ColorPerVertex=false), used when ColorRGBA is non-nil. }
    FaceColorRgbAlpha: TCastleColor;
    { Per-face color (used when ColorPerVertex=false), used when Color is non-nil. }
    FaceColorRgb: TCastleColorRGB;
    { Is ColorIndex assigned and non-empty. }
    function UseColorIndex: Boolean;
  protected
    Color: TMFVec3f;
    ColorRGBA: TMFColorRGBA;
    ColorPerVertex: boolean;
    ColorIndex: TMFLong;
    ColorNode: TAbstractColorNode;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateVertex(IndexNum: integer); override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  TNormalsImplementation = (
    { TAbstractNormalGenerator does nothing about normals.
      Descendants must override GetNormal and GenerateVertex to provide normals
      calculated using a custom method. }
    niNone,
    { The first item of Normals specifies the one and only normal
      for the whole geometry. }
    niOverall,
    { Shape does not have any way to define reasonable normals, and so is rendered unlit
      (e.g. LineSet, IndexedLineSet, PointSet, at least when no explicit
      normals are provided (X3D v4 may allow providing normals for them)). }
    niUnlit,
    { Each vertex has it's normal vector, IndexNum specifies direct index
      to Normals. }
    niPerVertexNonIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      CoordIndex and this is an index to Normals. }
    niPerVertexCoordIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      NormalIndex and this is an index to Normals. }
    niPerVertexNormalIndexed,
    { Face number is the index to Normals. }
    niPerFace,
    { Face number is the index to NormalIndex, and this indexes Normals. }
    niPerFaceNormalIndexed
  );

  { Handle normals, both taken from user data (that is, stored in VRML file)
    and generated.

    Usage:

    - You have to set NorImplementation in descendant.
      Default value, NorImplementation = niNone, simply means that
      this class does nothing and it's your responsibility to generate
      and use normal vectors. See TNormalsImplementation for other meanings,
      and which properties from
        Normals
        NormalsCcw (should always be set when setting Normals)
        NormalIndex
      you also have to assign to make them work.

      For VRML 1.0, you most definitely want to set both Normals
      and NormalIndex and then call NorImplementation :=
      NorImplementationFromVRML1Binding. This should take care of
      VRML 1.0 needs completely.

    - If and only if NorImplementation = niNone (either you left it as
      default, or NorImplementationFromVRML1Binding returned this,
      or you set this...) you have to

      - Override GetNormal and return correct normals.
        GetNormal results (vectors) must point from side set as NormalsCcw.
        When NorImplementation <> niNone then GetNormal should return inherited.

      - Override GenerateVertex and set Arrays.Normal(ArrayIndexNum)^ := ...;

    Note that PerVertexXxx normals require smooth shading to work Ok. }
  TAbstractNormalGenerator = class(TAbstractColorGenerator)
  private
    FaceNormal: TVector3;
    function NormalsSafe(const Index: Integer): TVector3;
  protected
    NormalIndex: TMFLong;
    Normals: TVector3List;
    NormalsCcw: boolean;

    { This is calculated in constructor. Unlike similar TexImplementation
      (which is calculated only in GenerateCoordinateBegin).
      Reasons:
      - Descendants may want to change NorImplementation. In other words,
        full automatic detection only in TAbstractNormalGenerator
        is not done, it's possible in descendants to explicitly change this.
      - NodeLit uses this, so it must be available after creation and
        before rendering. }
    NorImplementation: TNormalsImplementation;

    function NorImplementationFromVRML1Binding(
      NormalBinding: Integer): TNormalsImplementation;

    { Returns normal vector for given vertex, identified by IndexNum
      (IndexNum has the same meaning as for GenerateVertex) and FaceNumber
      (since normals may be available per-face, we need to know face number
      as well as vertex number).

      Returns normal from the side specified as NormalsCcw.

      Override this in descendants only to handle
      NorImplementation = niNone case. }
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3); virtual;

    { If @true, then it's guaranteed that normals for the same face will
      be equal. This may be useful for various optimization purposes.

      Override this in descendants only to handle
      NorImplementation = niNone case. The implementation in this class
      just derives it from NorImplementation, and for niNone answers @false
      (safer answer). }
    function NormalsFlat: boolean; virtual;

    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinateEnd; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;

    procedure GenerateVertex(IndexNum: Integer); override;
  end;

  { Handle fog coordinate.
    Descendants don't have to do anything, this just works
    (using TAbstractGeometryNode.FogCoord). }
  TAbstractFogGenerator = class(TAbstractNormalGenerator)
  private
    FogCoord: TSingleList;
  protected
    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateVertex(IndexNum: Integer); override;
  public
    constructor Create(AShape: TShape); override;
  end;

  TX3DVertexAttributeNodes = {$ifdef FPC}specialize{$endif} TObjectList<TAbstractVertexAttributeNode>;

  { Handle GLSL custom attributes from X3D "attrib" field.
    Descendants don't have to do anything, this just works
    (using TAbstractGeometryNode.Attrib). }
  TAbstractShaderAttribGenerator = class(TAbstractFogGenerator)
  strict private
    type
      TAttribInfo = record
        Node: TAbstractVertexAttributeNode;
        GeometryAttrib: TGeometryAttrib;
      end;
    var
      Attribs: array of TAttribInfo;
  protected
    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(AShape: TShape); override;
  end;

  { Handle bump mapping.

    Descendants should:
    - override BumpMappingAllowed to return true,
    - call CalculateTangentVectors when needed.
    - Also make sure that GetNormal always
      works (since it's called by CalculateTangentVectors),
      so if you may use NorImplementation = niNone: be sure to override
      GetNormal to return correct normal. }
  TAbstractBumpMappingGenerator = class(TAbstractShaderAttribGenerator)
  strict private
    Tangent: TVector3;
    CalculateTangents: Boolean;
  protected
    { If tangents are provided in the TAbstractComposedGeometryNode.FdTangent,
      descendants should set them here. }
    TangentsFromNode: TVector3List;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateVertex(IndexNum: Integer); override;

    { Update tangent vectors (Tangent, Bitangent).
      Without this, bump mapping will be wrong.
      Give triangle indexes (like IndexNum for GenerateVertex). }
    procedure CalculateTangentVectors(
      const TriangleIndex1, TriangleIndex2, TriangleIndex3: Integer);
  end;

  { Most complete implementation of arrays generator,
    should be used to derive non-abstract renderers for nodes. }
  TAbstractCompleteGenerator = TAbstractBumpMappingGenerator;

{ TArraysGenerator ------------------------------------------------------ }

constructor TArraysGenerator.Create(AShape: TShape);
begin
  inherited Create;

  FShape := AShape;
  FGeometry := FShape.Geometry;
  FState := FShape.State;

  Check(Geometry.InternalCoord(State, FCoord),
    'TAbstractCoordinateRenderer is only for coordinate-based nodes');
  CoordIndex := Geometry.CoordIndexField;
end;

function TArraysGenerator.GenerateArrays: TGeometryArrays;
var
  AllowIndexed: boolean;
  MaxIndex: UInt32;
begin
  Arrays := TGeometryArrays.Create;
  Result := Arrays;

  { no geometry if coordinates are empty. Leave empty Arrays. }
  if Coord = nil then Exit;

  { initialize stuff for generating }
  IndexesFromCoordIndex := nil;
  try
    ArrayIndexNum := -1;

    PrepareIndexesPrimitives;

    { Assert about Arrays.Counts.Sum. Note that even when
      IndexesFromCoordIndex = nil, Arrays.Counts.Sum should usually be equal
      to final Arrays.Count. But not always: it may not be equal
      for invalid nodes, with non-complete triangle strips / fans etc.
      (then Arrays.Counts will not contain all coordinates). }
    Assert(
      (Arrays.Counts = nil) or
      (IndexesFromCoordIndex = nil) or
      (Arrays.Counts.Sum = IndexesFromCoordIndex.Count) );

    AllowIndexed := true;

    if IndexesFromCoordIndex <> nil then
    begin
      Assert(CoordIndex <> nil);
      { Do not check IndexesFromCoordIndex.Max when IndexesFromCoordIndex is empty.
        The "Max" would be then 0 (it has to be, for an unsigned value),
        and would cause invalid warnings on empty IndexedFaceSet
        (with empty Coordinate and empty coordIndex). }
      if IndexesFromCoordIndex.Count <> 0 then
      begin
        MaxIndex := IndexesFromCoordIndex.Max;

        { check do we have enough coordinates. TGeometryArrays data may be passed
          quite raw to OpenGL, so this may be our last chance to check correctness
          and avoid passing data that would cause OpenGL errors. }
        if MaxIndex >= Coord.Count then
        begin
          CoordIndex.WritelnWarning_WrongVertexIndex(Geometry.X3DType,
            MaxIndex, Coord.Count);
          Exit; { leave Arrays created but empty }
        end;

        {$ifdef GLIndexesShort}
        { In case our indexes could not be expressed using TGeometryIndexList,
          do not use indexes (i.e. leave Arrays.Indexes = nil).
          This is important in case we have to use 16-bit indexes for OpenGLES
          (when GLIndexesShort is defined and TGLIndex = UInt16)
          but our input data needs larger indexes.

          This code would actually compile when GLIndexesShort is not defined too,
          but the condition is then always false (and causes FPC warning).

          See GLIndexesShort notes for other approaches to it in the future. }
        if MaxIndex > High(TGLIndex) then
          AllowIndexed := false;
        {$endif}
      end;
    end;

    PrepareAttributes(AllowIndexed);

    try
      Arrays.CoordinatePreserveGeometryOrder := AllowIndexed or (IndexesFromCoordIndex = nil);
      if Arrays.CoordinatePreserveGeometryOrder then
      begin
        if IndexesFromCoordIndex <> nil then
        begin
          { Set Arrays.Indexes to be equal to IndexesFromCoordIndex }
          {$ifdef GLIndexesShort}
          { Convert 32-bit indexes in IndexesFromCoordIndex into 16-bit indexes in Arrays.Indexes }
          Arrays.Indexes := TGeometryIndexList.Create;
          Arrays.Indexes.Assign(IndexesFromCoordIndex);
          FreeAndNil(IndexesFromCoordIndex);
          {$else}
          { Just use 32-bit indexes in IndexesFromCoordIndex as Arrays.Indexes }
          Arrays.Indexes := IndexesFromCoordIndex;
          IndexesFromCoordIndex := nil;
          {$endif}
        end;

        Arrays.Count := Coord.Count;
      end else
      begin
        { Each AssignCoordinate, AssignAttribute will expand the IndexesFromCoordIndex,
          to specify vertexes multiple times in this case. }
        Arrays.Count := IndexesFromCoordIndex.Count;
        Assert(Arrays.Indexes = nil); // leaving Arrays.Indexes empty
      end;

      AssignCoordinate('coordinates', Arrays.Position, Coord.Items.L, Coord.Items.ItemSize, Coord.Items.Count);

      if LogShapes then
        WritelnLog('Renderer', Format('Shape %s rendered:' + NL +
          '- using original indexes (desired, makes more compact GPU representation (array count)): %s' + NL +
          '- preserving geometry order (desired, makes updating CoordinateInterpolator faster): %s' + NL +
          '- array count: %d', [
          Shape.NiceName,
          BoolToStr(AllowIndexed, true),
          BoolToStr(Arrays.CoordinatePreserveGeometryOrder, true),
          Arrays.Count
        ]));

      GenerateCoordinateBegin;
      try
        GenerateCoordinate;
      finally GenerateCoordinateEnd; end;
    except
      on E: EAssignInterleavedRangeError do
        WritelnWarning('X3D', Format('Invalid number of items in an attribute array for shape "%s": %s',
          [Shape.NiceName, E.Message]));
    end;
  finally
    FreeAndNil(IndexesFromCoordIndex);
    FreeAndNil(TrivialIndexesFromCoordIndex);
  end;
end;

procedure TArraysGenerator.AssignAttributeOrCoordinate(
  const AttributeName: String;
  const TargetPtr: Pointer; const TargetItemSize: SizeInt;
  const SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
  const TrivialIndex: Boolean);
begin
  { Following TArraysGenerator.GenerateArrays logic, there are various cases:

    - Arrays.CoordinatePreserveGeometryOrder = true

      This means:
      - Arrays.Indexes may be nil or non-nil
        (it will be nil if geometry has no indexes, like LineSet, PointSet, TriangleSet;
         it will be non-nil if geometry has indexes, like IndexedFaceSet, IndexedTriangleSet etc.)
      - IndexesFromCoordIndex is nil when this function is called
        (TArraysGenerator.GenerateArrays maybe saw it non-nil at some point)
      - maybe something set "AllowIndexed := false" during PrepareAttributes,
        maybe not.
        AllowIndexed is ignored if geometry has no indexes anyway (LineSet, PointSet, TriangleSet).
      - Arrays.Count = Coord.Count
      - So the target data layout (in Arrays.AttributeArray) is in the same order
        as source data (defined by SourcePtr, SourceItemSize, SourceCount)

    - Arrays.CoordinatePreserveGeometryOrder = false

      This means:
      - Arrays.Indexes = nil
      - IndexesFromCoordIndex <> nil
      - something set "AllowIndexed := false" during PrepareAttributes
      - So we have coordIndex on geometry, but we have to "expand" data when copying,
        making it non-indexed, because for some reason we cannot render it indexed
  }
  if TrivialIndex then
  begin
    Assert(not Arrays.CoordinatePreserveGeometryOrder, 'Set AllowIndexed := false when there''s a possibility to call with TrivialIndex');
    Assert(TrivialIndexesFromCoordIndex <> nil, 'Generate TrivialIndexesFromCoordIndex for all geometries that may call with TrivialIndex');
    AssignToInterleavedIndexed(AttributeName, TargetPtr, TargetItemSize, Arrays.Count, SourcePtr, SourceItemSize, SourceCount, TrivialIndexesFromCoordIndex);
  end else
  if Arrays.CoordinatePreserveGeometryOrder then
    AssignToInterleaved       (AttributeName, TargetPtr, TargetItemSize, Arrays.Count, SourcePtr, SourceItemSize, SourceCount)
  else
    AssignToInterleavedIndexed(AttributeName, TargetPtr, TargetItemSize, Arrays.Count, SourcePtr, SourceItemSize, SourceCount, IndexesFromCoordIndex);
end;

procedure TArraysGenerator.AssignAttribute(const AttributeName: String;
  const TargetPtr, SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
  const TrivialIndex: Boolean);
begin
  AssignAttributeOrCoordinate(AttributeName, TargetPtr, Arrays.AttributeSize, SourcePtr, SourceItemSize, SourceCount, TrivialIndex);
end;

procedure TArraysGenerator.AssignCoordinate(const AttributeName: String;
  const TargetPtr, SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt;
  const TrivialIndex: Boolean);
begin
  AssignAttributeOrCoordinate(AttributeName, TargetPtr, Arrays.CoordinateSize, SourcePtr, SourceItemSize, SourceCount, TrivialIndex);
end;

procedure TArraysGenerator.PrepareAttributes(var AllowIndexed: boolean);
begin
  if Geometry is TAbstractComposedGeometryNode then
  begin
    Arrays.CullFace := Geometry.Solid;
    Arrays.FrontFaceCcw := (Geometry as TAbstractComposedGeometryNode).FdCcw.Value;
  end;
end;

procedure TArraysGenerator.GenerateCoordinateBegin;
begin
  { nothing to do in this class }
end;

procedure TArraysGenerator.GenerateCoordinateEnd;
begin
  { nothing to do in this class }
end;

procedure TArraysGenerator.GenerateVertex(IndexNum: integer);
begin
  if CoordIndex <> nil then
  begin
    if Arrays.Indexes = nil then
      Inc(ArrayIndexNum) else
      ArrayIndexNum := CoordIndex.Items.L[IndexNum];
  end else
    ArrayIndexNum := IndexNum;
end;

function TArraysGenerator.GetVertex(IndexNum: integer): TVector3;
begin
  { This assertion should never fail, it's the responsibility
    of the programmer. }
  Assert(IndexNum < CoordCount);

  if CoordIndex <> nil then
    Result := Coord.ItemsSafe[CoordIndex.Items.L[IndexNum]]
  else
    Result := Coord.Items.L[IndexNum];
end;

function TArraysGenerator.CoordCount: Integer;
begin
  if CoordIndex <> nil then
    Result := CoordIndex.Items.Count
  else
    Result := Coord.Items.Count;
end;

procedure TArraysGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  FCurrentRangeNumber := RangeNumber;
end;

class function TArraysGenerator.BumpMappingAllowed: boolean;
begin
  Result := false;
end;

{ TAbstractTextureCoordinateGenerator ----------------------------------------- }

constructor TAbstractTextureCoordinateGenerator.Create(AShape: TShape);
begin
  inherited;
  if not Geometry.InternalTexCoord(State, TexCoord) then
    TexCoord := nil;
end;

procedure TAbstractTextureCoordinateGenerator.PrepareAttributes(
  var AllowIndexed: boolean);

  { Is a texture used on given unit, and it's 3D texture. }
  function IsTexture3D(const TexUnit: Cardinal): boolean;

    { Knowing that Tex is not nil,
      check is it a single (not MultiTexture) 3D texture. }
    function IsSingleTexture3D(Tex: TX3DNode): boolean;
    begin
      Result :=
         (Tex is TAbstractTexture3DNode) or
        ((Tex is TShaderTextureNode) and
         (TShaderTextureNode(Tex).FdDefaultTexCoord.Value = 'BOUNDS3D'));
    end;

  var
    Tex: TAbstractTextureNode;
  begin
    Tex := State.MainTexture;
    Result := (
      (Tex <> nil) and
      ( ( (TexUnit = 0) and IsSingleTexture3D(Tex) )
        or
        ( (Tex is TMultiTextureNode) and
          (TMultiTextureNode(Tex).FdTexture.Count > TexUnit) and
          IsSingleTexture3D(TMultiTextureNode(Tex).FdTexture[TexUnit])
        )));
  end;

  { Set length of TexCoordArray* arrays. }
  procedure SetTexLengths(const Count: Integer);
  begin
    SetLength(TexCoordArray2d, Count);
    SetLength(TexCoordArray3d, Count);
    SetLength(TexCoordArray4d, Count);
  end;

  function Bounds2DTextureGenVectors: TTextureGenerationVectors;
  var
    LocalBBox: TBox3D;
    LocalBBoxSize: TVector3;

    { Setup and enable glTexGen to make automatic 2D texture coords
      based on shape bounding box. On texture unit 0. }
    procedure SetupCoordGen(out Gen: TVector4;
      const Coord: integer; const GenStart, GenEnd: Single);

    { We want to map float from range
        LocalBBox[0, Coord]...LocalBBox[0, Coord] + LocalBBoxSize[Coord]
      to
        GenStart...GenEnd.

      For a 3D point V let's define S1 as
        S1 = (V[Coord] - LocalBBox[0, Coord]) / LocalBBoxSize[Coord]
      and so S1 is in 0..1 range, now
        S = S1 * (GenEnd - GenStart) + GenStart
      and so S is in GenStart...GenEnd range, like we wanted.

      It remains to rewrite this to a form that we can pass to OpenGL
      glTexGenfv(..., GL_OBJECT_PLANE, ...).

        S = V[Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
           - LocalBBox[0, Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
           + GenStart

      Simple check: for GenStart = 0, GenEnd = 1 this simplifies to
        S = V[Coord] / LocalBBoxSize[Coord] -
            LocalBBox[0, Coord] / LocalBBoxSize[Coord]
          = (V[Coord] - LocalBBox[0, Coord]) / LocalBBoxSize[Coord]
          = S1
    }

    begin
      FillChar(Gen, SizeOf(Gen), 0);
      Gen.Data[Coord] := (GenEnd - GenStart) / LocalBBoxSize[Coord];
      Gen.Data[3] :=
        - LocalBBox.Data[0].Data[Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
        + GenStart;
    end;

  var
    SCoord, TCoord: T3DAxis;
  begin
    LocalBBox := Shape.LocalBoundingBox;

    if not LocalBBox.IsEmpty then
    begin
      LocalBBoxSize := LocalBBox.Size;

      Geometry.GetTextureBounds2DST(LocalBBoxSize, SCoord, TCoord);

      { Calculate TextureGen[0..1]. }
      SetupCoordGen(Result[0], SCoord, 0, 1);
      SetupCoordGen(Result[1], TCoord, 0, LocalBBoxSize[TCoord] / LocalBBoxSize[SCoord]);
    end else
    begin
      { When local bounding box is empty, set these to any sensible value }
      Result[0] := Vector4(1, 0, 0, 0);
      Result[1] := Vector4(0, 1, 0, 0);
    end;

    Result[2] := Vector4(0, 0, 1, 0); //< whatever, just to be defined.
  end;

  function Bounds3DTextureGenVectors: TTextureGenerationVectors;
  var
    Box: TBox3D;
    XStart, YStart, ZStart, XSize, YSize, ZSize: Single;
  begin
    Box := Shape.LocalBoundingBox;

    if not Box.IsEmpty then
    begin
      { Texture S should range from 0..1 when X changes from X1 .. X2.
        So S = X / (X2 - X1) - X1 / (X2 - X1).
        Same for T.
        For R, X3D spec says that coords go backwards, so just SwapValues. }

      SwapValues(Box.Data[0].Z, Box.Data[1].Z);

      XStart := Box.Data[0].X;
      YStart := Box.Data[0].Y;
      ZStart := Box.Data[0].Z;

      XSize := Box.Data[1].X - Box.Data[0].X;
      YSize := Box.Data[1].Y - Box.Data[0].Y;
      ZSize := Box.Data[1].Z - Box.Data[0].Z;

      Result[0] := Vector4(1 / XSize, 0, 0, - XStart / XSize);
      Result[1] := Vector4(0, 1 / YSize, 0, - YStart / YSize);
      Result[2] := Vector4(0, 0, 1 / ZSize, - ZStart / ZSize);
    end else
    begin
      { When local bounding box is empty, set these to any sensible value }
      Result[0] := Vector4(1, 0, 0, 0);
      Result[1] := Vector4(0, 1, 0, 0);
      Result[2] := Vector4(0, 0, 1, 0);
    end;
  end;

  { Initialize Arrays.TexCoords and TexCoordArray2/3/4d, based on TexCoord.

    If any usable tex coords are found
    (that is, Arrays.TexCoords.Count <> 0 on exit) then this array
    contains at least TexCoordsNeeded units. }
  procedure InitializeTexCoordGenArray;

    { Add single texture coord configuration
      to Arrays.TexCoord and TexCoordArray2/3/4d.
      Assume that TexCoordArray2/3/4d already have required length.
      Pass any TexCoord node except TMultiTextureCoordinateNode. }
    procedure AddSingleTexCoord(const TextureUnit: Cardinal; TexCoord: TX3DNode);

      function TexCoordGenFromString(const S: string; const IsTexture3D: boolean): TTextureCoordinateGeneration;
      begin
        if S = 'SPHERE' then
          Result := tgSphereMap else
        if S = 'COORD' then
          Result := tgCoord else
        if (S = 'COORD-EYE') or (S = 'CAMERASPACEPOSITION') then
          Result := tgCoordEye else
        if S = 'CAMERASPACENORMAL' then
          Result := tgCameraSpaceNormal else
        if S = 'WORLDSPACENORMAL' then
          Result := tgWorldSpaceNormal else
        if S = 'CAMERASPACEREFLECTIONVECTOR' then
          Result := tgCameraSpaceReflectionVector else
        if S = 'WORLDSPACEREFLECTIONVECTOR' then
          Result := tgWorldSpaceReflectionVector else
        if S = 'PROJECTION' then
          Result := tgProjection else
        if S = 'BOUNDS' then
        begin
          if IsTexture3D then
            Result := tgBounds3d
          else
            Result := tgBounds2d;
        end else
        if S = 'BOUNDS2D' then
          Result := tgBounds2d else
        if S = 'BOUNDS3D' then
          Result := tgBounds3d else
        if S = 'MIRROR-PLANE' then
          Result := tgMirrorPlane else
        begin
          Result := tgCoord;
          WritelnWarning('X3D', Format('Unsupported TextureCoordinateGenerator.mode: "%s", will use "COORD" instead',
            [S]));
        end;
      end;

      { For tgProjection generation, calculate function that should be
        later used to calculate matrix to pass to glTexGen as eye plane.

        Gets projector matrices from a TextureCoordinateGenerator or
        ProjectedTextureCoordinate node.
        If this projector is a correct light source or viewpoint,
        then get it's projection and modelview matrix and return @true.
        Returns @false (and does correct WritelnWarning)
        if it's empty or incorrect. }
      function GetProjectorMatrixFunction(GeneratorNode: TX3DNode): TProjectorMatrixFunction;
      var
        ProjectorValue: TX3DNode; { possible ProjectorLight or ProjectorViewpoint }
      begin
        Result := nil;

        if GeneratorNode is TTextureCoordinateGeneratorNode then
        begin
          ProjectorValue := TTextureCoordinateGeneratorNode(GeneratorNode).FdProjectedLight.Value;
          if (ProjectorValue <> nil) and
             (ProjectorValue is TAbstractPunctualLightNode) then
          begin
            Result := {$ifdef FPC}@{$endif}TAbstractPunctualLightNode(ProjectorValue).GetProjectorMatrix;
          end else
            WritelnWarning('X3D', 'Using TextureCoordinateGenerator.mode = "PROJECTION", but TextureCoordinateGenerator.projectedLight is NULL or incorrect');
        end else
        if GeneratorNode is TProjectedTextureCoordinateNode then
        begin
          ProjectorValue := TProjectedTextureCoordinateNode(GeneratorNode).FdProjector.Value;
          if ProjectorValue is TAbstractPunctualLightNode then // checks also ProjectorValue <> nil
          begin
            Result := {$ifdef FPC}@{$endif}TAbstractPunctualLightNode(ProjectorValue).GetProjectorMatrix;
          end else
          if ProjectorValue is TAbstractViewpointNode then // checks also ProjectorValue <> nil
          begin
            Result := {$ifdef FPC}@{$endif}TAbstractViewpointNode(ProjectorValue).GetProjectorMatrix;
          end else
            WritelnWarning('X3D', 'ProjectedTextureCoordinate.projector is NULL or incorrect');
        end else
          { This should not actually happen (GeneratorNode passed here should be like this) }
          WritelnWarning('X3D', 'Invalid texture generator node');
      end;

    begin
      if TexCoord is TTextureCoordinateNode then
      begin
        Arrays.AddTexCoord2D(TextureUnit);
        TexCoordArray2d[TextureUnit] := TTextureCoordinateNode(TexCoord).FdPoint;
      end else
      if TexCoord is TTextureCoordinate2Node_1 then
      begin
        Arrays.AddTexCoord2D(TextureUnit);
        TexCoordArray2d[TextureUnit] := TTextureCoordinate2Node_1(TexCoord).FdPoint;
      end else
      if TexCoord is TTextureCoordinate3DNode then
      begin
        Arrays.AddTexCoord3D(TextureUnit);
        TexCoordArray3d[TextureUnit] := TTextureCoordinate3DNode(TexCoord).FdPoint;
      end else
      if TexCoord is TTextureCoordinate4DNode then
      begin
        Arrays.AddTexCoord4D(TextureUnit);
        TexCoordArray4d[TextureUnit] := TTextureCoordinate4DNode(TexCoord).FdPoint;
      end else
      if TexCoord is TTextureCoordinateGeneratorNode then
      begin
        Arrays.AddTexCoordGenerated(
          TexCoordGenFromString(TTextureCoordinateGeneratorNode(TexCoord).FdMode.Value,
            IsTexture3D(TextureUnit)), TextureUnit);
      end else
      if TexCoord is TProjectedTextureCoordinateNode then
      begin
        Arrays.AddTexCoordGenerated(tgProjection, TextureUnit);
      end else
      begin
        { dummy default }
        Arrays.AddTexCoordGenerated(tgBounds2d, TextureUnit);
        if TexCoord <> nil then
          WritelnWarning('X3D', Format('Unsupported texture coordinate node: %s, inside multiple texture coordinate', [TexCoord.X3DType])) else
          WritelnWarning('X3D', 'NULL texture coordinate node');
      end;

      { Calculate some Generation-specific values }
      case Arrays.TexCoords[TextureUnit].Generation of
        tgBounds2d: Arrays.TexCoords[TextureUnit].GenerationBoundsVector := Bounds2DTextureGenVectors;
        tgBounds3d: Arrays.TexCoords[TextureUnit].GenerationBoundsVector := Bounds3DTextureGenVectors;
        tgProjection: Arrays.TexCoords[TextureUnit].GenerationProjectorMatrix := GetProjectorMatrixFunction(TexCoord);
        else ;
      end;
    end;

  var
    MultiTexCoord: TMFNode;
    I, LastCoord: Integer;
  begin
    if TexCoord = nil then
    begin
      { Leave TexCoords.Count = 0, no WritelnWarning }
      Exit;
    end else
    if TexCoord is TMultiTextureCoordinateNode then
    begin
      MultiTexCoord := TMultiTextureCoordinateNode(TexCoord).FdTexCoord;
      SetTexLengths(MultiTexCoord.Count);
      for I := 0 to MultiTexCoord.Count - 1 do
        AddSingleTexCoord(I, MultiTexCoord[I]);
    end else
    begin
      SetTexLengths(1);
      AddSingleTexCoord(0, TexCoord);
    end;

    Assert(Arrays.TexCoords.Count = Length(TexCoordArray2d));
    Assert(Arrays.TexCoords.Count = Length(TexCoordArray3d));
    Assert(Arrays.TexCoords.Count = Length(TexCoordArray4d));

    if (Arrays.TexCoords.Count <> 0) and
       (Arrays.TexCoords.Count < TexCoordsNeeded) then
    begin
      LastCoord := Arrays.TexCoords.Count - 1;
      SetTexLengths(TexCoordsNeeded);

      { We copy tex coord LastCoord values to all following items.
        This way we do what X3D spec says:
        - if non-MultiTextureCoordinate is used for multitexturing,
          channel 0 is replicated
        - if MultiTextureCoordinate is used, but with too few items,
          last channel is replicated. }
      for I := LastCoord + 1 to TexCoordsNeeded - 1 do
      begin
        Arrays.AddTexCoordCopy(I, LastCoord);
        TexCoordArray2d[I] := TexCoordArray2d[LastCoord];
        TexCoordArray3d[I] := TexCoordArray3d[LastCoord];
        TexCoordArray4d[I] := TexCoordArray4d[LastCoord];
      end;
    end;
  end;

  { Setup 2D texture coordinates suitable for IndexedFaceSet without
    explicit texture coords, following X3D 3D Texturing spec.
    We set texture generation to tgBounds2d
    on all texture units within TexCoordsNeeded.
    We also set TexImplementation := tcAllGenerated. }
  procedure Bounds2DTextureGen;
  var
    I: integer;
    TexGenVectors: TTextureGenerationVectors;
  begin
    TexGenVectors := Bounds2DTextureGenVectors;

    TexImplementation := tcAllGenerated;
    SetTexLengths(TexCoordsNeeded);
    for I := 0 to TexCoordsNeeded - 1 do
    begin
      Arrays.AddTexCoordGenerated(tgBounds2d, I);
      Arrays.TexCoords[I].GenerationBoundsVector := TexGenVectors;
    end;
  end;

  { Setup 3D texture coordinates suitable for "primitives without
    explicit texture coordinates", following X3D 3D Texturing spec.
    We set tgBounds3d on all texture units within TexCoordsNeeded.
    We also set TexImplementation := tcAllGenerated. }
  procedure Bounds3DTextureGen;
  var
    I: Integer;
    TexGenVectors: TTextureGenerationVectors;
  begin
    TexGenVectors := Bounds3DTextureGenVectors;

    TexImplementation := tcAllGenerated;
    SetTexLengths(TexCoordsNeeded);
    for I := 0 to TexCoordsNeeded - 1 do
    begin
      Arrays.AddTexCoordGenerated(tgBounds3d, I);
      Arrays.TexCoords[I].GenerationBoundsVector := TexGenVectors;
    end;
  end;

begin
  inherited;

  TexImplementation := tcNotGenerated;

  { Make sure they are initially empty. }
  Assert(Arrays.TexCoords.Count = 0);
  Assert(Length(TexCoordArray2d) = 0);
  Assert(Length(TexCoordArray3d) = 0);
  Assert(Length(TexCoordArray4d) = 0);

  if TexCoordsNeeded > 0 then
  begin
    if { Original shape node is a primitive, without explicit texCoord }
       Shape.OriginalGeometry.AutoGenerate3DTexCoords and
       { First texture is 3D texture }
       IsTexture3D(0) then
    begin
      Bounds3DTextureGen;
    end else

    { Handle VRML 1.0 case when TexCoord present but TexCoordIndex empty
      before calling InitializeTexCoordGenArray. That's because we have
      to ignore TexCoord in this case (so says VRML 1.0 spec, and it's
      sensible since for VRML 1.0 there is always some last TexCoord node).
      So we don't want to let InitializeTexCoordGenArray to call
      any Arrays.AddTexCoord. }
    if (State.ShapeNode = nil) and
       (TexCoordIndex <> nil) and
       (TexCoordIndex.Count < CoordIndex.Count) then
    begin
      Bounds2DTextureGen;
    end else

    begin
      InitializeTexCoordGenArray;
      if Arrays.TexCoords.Count > 0 then
      begin
        if TexCoordIndex = nil then
        begin
          { This happens only for X3D non-indexed primitives:
            Triangle[Fan/Strip]Set, QuadSet. Spec says that TexCoord should be
            used just like Coord, so IndexNum indexes it directly. }
          TexImplementation := tcNonIndexed;
          Assert(CoordIndex = nil);
        end else
        if { Use texIndexed if texture coordinates are indexed by *something else*
             than CoordIndex. So check "TexCoordIndex <> CoordIndex".

             In case of X3D primitives like IndexedTriangle[Fan/Strip]Set, QuadSet,
             they always have TexCoordIndex = CoordIndex (just taken from "index" field).
             And we don't want to use tcTexIndexed for them (because it is less optimal,
             as sets AllowIndexed=false, CoordinatePreserveGeometryOrder=false). }
           (TexCoordIndex <> CoordIndex) and
           (TexCoordIndex.Count >= CoordIndex.Count) then
        begin
          TexImplementation := tcTexIndexed;
        end else
        begin
          { X3D primitives like IndexedTriangle[Fan/Strip]Set, QuadSet
            will arrive here, because they always have TexCoordIndex = CoordIndex.

            Others will arrive here when TexCoord <> nil
            (non-zero Arrays.TexCoords.Count guarantees this)
            but TexCoordIndex is empty. Then:

            - VRML 2.0 spec of IndexedFaceSet says that coordIndex is used
              to index texture coordinates.

            - VRML 1.0 spec says that in this case default texture
              coordinates should be generated (that's because for
              VRML 1.0 there is always some TexCoord <> nil,
              so it cannot be used to produce different behavior).
              We handled this case in code above.
          }
          Assert(State.ShapeNode <> nil);
          TexImplementation := tcCoordIndexed;
        end;
      end else
        Bounds2DTextureGen;
    end;
  end;

  if TexImplementation in [tcTexIndexed, tcNonIndexed] then
    AllowIndexed := false;
end;

procedure TAbstractTextureCoordinateGenerator.GenerateCoordinateBegin;

  { Set explicit texture coordinates in Arrays,
    for texture units where Arrays.TexCoords[].Generation = tgExplicit.
    This is only for TexImplementation in [tcNonIndexed, tcCoordIndexed],
    other TexImplementation have to be handled elsewhere. }
  procedure EnableExplicitTexCoord;
  var
    I: Integer;
  begin
    for I := 0 to Arrays.TexCoords.Count - 1 do
      if Arrays.TexCoords[I].Generation = tgExplicit then
        case Arrays.TexCoords[I].Dimensions of
          2: AssignAttribute('2D texture coordinates', Arrays.TexCoord(I), TexCoordArray2d[I].Items.L, TexCoordArray2d[I].Items.ItemSize, TexCoordArray2d[I].Items.Count);
          3: AssignAttribute('3D texture coordinates', Arrays.TexCoord(I), TexCoordArray3d[I].Items.L, TexCoordArray3d[I].Items.ItemSize, TexCoordArray3d[I].Items.Count);
          4: AssignAttribute('4D texture coordinates', Arrays.TexCoord(I), TexCoordArray4d[I].Items.L, TexCoordArray4d[I].Items.ItemSize, TexCoordArray4d[I].Items.Count);
        end;
  end;

begin
  inherited;

  if TexImplementation in [tcNonIndexed, tcCoordIndexed] then
    EnableExplicitTexCoord;
end;

function TAbstractTextureCoordinateGenerator.GetTextureCoord(
  IndexNum: integer; const TextureUnit: Cardinal;
  out Tex: TVector4): boolean;

  function GenerateTexCoord(const TexCoord: TGeometryTexCoord): TVector4;
  var
    Vertex: TVector3;
  begin
    case TexCoord.Generation of
      tgBounds2d:
        begin
          Vertex := GetVertex(IndexNum);
          Result.X := TVector4.DotProduct(Vector4(Vertex, 1), TexCoord.GenerationBoundsVector[0]);
          Result.Y := TVector4.DotProduct(Vector4(Vertex, 1), TexCoord.GenerationBoundsVector[1]);
          Result.Z := 0;
          Result.W := 1;
        end;
      tgBounds3d:
        begin
          Vertex := GetVertex(IndexNum);
          Result.X := TVector4.DotProduct(Vector4(Vertex, 1), TexCoord.GenerationBoundsVector[0]);
          Result.Y := TVector4.DotProduct(Vector4(Vertex, 1), TexCoord.GenerationBoundsVector[1]);
          Result.Z := TVector4.DotProduct(Vector4(Vertex, 1), TexCoord.GenerationBoundsVector[2]);
          Result.W := 1;
        end;
      tgCoord:
        begin
          Vertex := GetVertex(IndexNum);
          Result.X := Vertex.X;
          Result.Y := Vertex.Y;
          Result.Z := Vertex.Z;
          Result.W := 1;
        end;
      else WritelnWarning('X3D', Format('Generating on CPU texture coordinates with %d not implemented yet',
        [Integer(TexCoord.Generation)]));
    end;
  end;

  { Sets Tex to TexCoordArray*[TextureUnit][Index] value. }
  procedure SetTexFromTexCoordArray(const Dimensions: TTexCoordDimensions;
    const Index: Integer);
  var
    Tex2d: TVector2 absolute Tex;
    Tex3d: TVector3 absolute Tex;
  begin
    case Dimensions of
      2: Tex2d := TexCoordArray2d[TextureUnit].ItemsSafe[Index];
      3: Tex3d := TexCoordArray3d[TextureUnit].ItemsSafe[Index];
      4: Tex   := TexCoordArray4d[TextureUnit].ItemsSafe[Index];
    end;
  end;

begin
  Result := TexImplementation <> tcNotGenerated;

  if Result then
  begin
    { This assertion should never fail, it's the responsibility
      of the programmer. Note that we don't need any TexCoordCount
      here, since IndexNum allowed for GetTextureCoord are the same
      and come from the same range as coords. }
    Assert(IndexNum < CoordCount);

    { Initialize to common values }
    Tex := Vector4(0, 0, 0, 1);

    Result := TextureUnit < Arrays.TexCoords.Count;
    if not Result then Exit;

    if Arrays.TexCoords[TextureUnit].Generation = tgExplicit then
    begin
      case TexImplementation of
        tcTexIndexed:
          { tcTexIndexed is set only if
            TexCoordIndex.Count >= CoordIndex.Count, so the IndexNum index
            is Ok for sure. That's why we don't do "ItemsSafe"
            for TexCoordIndex. }
          SetTexFromTexCoordArray(Arrays.TexCoords[TextureUnit].Dimensions, TexCoordIndex.Items.L[IndexNum]);
        tcCoordIndexed:
          { We already checked that IndexNum < CoordCount, so the first index
            is Ok for sure. }
          SetTexFromTexCoordArray(Arrays.TexCoords[TextureUnit].Dimensions, CoordIndex.Items.L[IndexNum]);
        tcNonIndexed:
          SetTexFromTexCoordArray(Arrays.TexCoords[TextureUnit].Dimensions, IndexNum);
        else raise EInternalError.Create('TAbstractTextureCoordinateGenerator.GetTextureCoord?');
      end;
    end else
      Tex := GenerateTexCoord(Arrays.TexCoords[TextureUnit]);
  end;
end;

function TAbstractTextureCoordinateGenerator.GetTextureCoord(
  IndexNum: integer; const TextureUnit: Cardinal;
  out Tex: TVector2): boolean;
var
  Tex4f: TVector4;
begin
  Result := GetTextureCoord(IndexNum, TextureUnit, Tex4f);
  Tex.X := Tex4f.X;
  Tex.Y := Tex4f.Y;
end;

procedure TAbstractTextureCoordinateGenerator.GenerateVertex(indexNum: integer);

  procedure DoTexCoord(Index: Integer);
  var
    TextureUnit: Integer;
  begin
    Assert(Arrays.Indexes = nil);
    for TextureUnit := 0 to Arrays.TexCoords.Count - 1 do
      if Arrays.TexCoords[TextureUnit].Generation = tgExplicit then
        case Arrays.TexCoords[TextureUnit].Dimensions of
          2: Arrays.TexCoord2D(TextureUnit, ArrayIndexNum)^ := TexCoordArray2d[TextureUnit].ItemsSafe[Index];
          3: Arrays.TexCoord3D(TextureUnit, ArrayIndexNum)^ := TexCoordArray3d[TextureUnit].ItemsSafe[Index];
          4: Arrays.TexCoord4D(TextureUnit, ArrayIndexNum)^ := TexCoordArray4d[TextureUnit].ItemsSafe[Index];
        end;
  end;

begin
  inherited;

  { Observe that tcTexIndexed (seldom case, not even possible in glTF) is much worse optimized than
    tcNonIndexed or tcCoordIndexed (common case).
    tcTexIndexed needs to be performed in GenerateVertex.

    I considered optimizing tcTexIndexed, but it would hurt the common case.

    - We would need to extend AssignAttributeOrCoordinate to handle "IndexForSource: TInt32List".

    - To implement IndexForSource, we must know how indexes (from coordIndex) map
      to vertexes. This information is no longer present in IndexesFromCoordIndex,
      as IndexesFromCoordIndex doesn't correspond to coordIndex anymore
      (e.g. coordIndex may have -1 in case of IndexedFaceSet,
      but IndexesFromCoordIndex has only >= indexes,
      after faces have been triangulated).

    - So it would require creating IndexesFromCoordIndexMap, that maps index of indexes -> where to find it.
      E.g. instead of this:

        IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[BeginIndex]; Inc(IFSNextIndex);
        IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[I + 1];      Inc(IFSNextIndex);
        IndexesFromCoordIndex.L[IFSNextIndex] := CoordIndex.Items.L[I + 2];      Inc(IFSNextIndex);

      => we would need this:

        IndexesFromCoordIndexMap.L[IFSNextIndex] := BeginIndex; Inc(IFSNextIndex);
        IndexesFromCoordIndexMap.L[IFSNextIndex] := I + 1;      Inc(IFSNextIndex);
        IndexesFromCoordIndexMap.L[IFSNextIndex] := I + 2;      Inc(IFSNextIndex);

    - However, in many cases this IndexesFromCoordIndexMap would be useless.
      For tcNonIndexed or tcCoordIndexed it would be useless.
      And so making IndexesFromCoordIndexMap would be a waste of time.
      E.g. right now IndexedTriangleSetNode does this in TTriangleSetGenerator.PrepareIndexesPrimitives:

        IndexesFromCoordIndex := TUInt32List.Create;
        IndexesFromCoordIndex.Assign(CoordIndex.Items);
        IndexesFromCoordIndex.Count := (IndexesFromCoordIndex.Count div 3) * 3;

      It would require extra iteration instead of simple "IndexesFromCoordIndex.Assign"
      to create IndexedTriangleSetNode.
  }
  if TexImplementation = tcTexIndexed then
    DoTexCoord(TexCoordIndex.Items.L[IndexNum]);
end;

{ TAbstractMaterial1Generator ------------------------------------------ }

{$warnings off} // using deprecated to keep supporting VRML 1
constructor TAbstractMaterial1Generator.Create(AShape: TShape);
begin
  inherited;
  MaterialBinding := BIND_DEFAULT;
  UpdateMat1Implementation;
end;

procedure TAbstractMaterial1Generator.UpdateMat1Implementation;

  function IndexListNotEmpty(MFIndexes: TMFLong): boolean;
  begin
    Result :=
      (MFIndexes.Count > 0) and
      { For VRML 1.0, [-1] value is default for materialIndex
        and should be treated as "empty", as far as I understand
        the spec. }
      (not ((MFIndexes.Count = 1) and (MFIndexes.Items.L[0] = -1)));
  end;

begin
  { Calculate Mat1Implementation }

  Mat1Implementation := miOverall;

  case MaterialBinding of
    { BIND_OVERALL, BIND_DEFAULT: take default miOverall }
    BIND_PER_VERTEX:
      Mat1Implementation := miPerVertexCoordIndexed;
    BIND_PER_VERTEX_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
        Mat1Implementation := miPerVertexMatIndexed;
    BIND_PER_PART, BIND_PER_FACE:
      Mat1Implementation := miPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
        Mat1Implementation := miPerFaceMatIndexed;
  end;

  { TODO: we handle all material bindings, but we handle BIND_PER_PART
    and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }
end;
{$warnings on}

procedure TAbstractMaterial1Generator.PrepareAttributes(var AllowIndexed: boolean);
begin
  inherited;

  if Mat1Implementation in
    [ miPerFace, miPerFaceMatIndexed,
      miPerVertexCoordIndexed, miPerVertexMatIndexed ] then
  begin
    Arrays.AddColor(cmReplace, true);
    if Mat1Implementation in
      [ miPerFace, miPerFaceMatIndexed, miPerVertexMatIndexed ] then
      AllowIndexed := false;
  end;
end;

function TAbstractMaterial1Generator.GetMaterial1Color(
  const MaterialIndex: Integer): TVector4;
var
  M: TPhongMaterialInfo; // VRML 1.0 has only Phong lighting model
begin
  M := State.VRML1State.Material.MaterialInfo(MaterialIndex);
  if M.PureEmissive then
    Result := Vector4(M.EmissiveColor, M.Opacity)
  else
    Result := Vector4(M.DiffuseColor, M.Opacity);
end;

procedure TAbstractMaterial1Generator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  case Mat1Implementation of
    miPerVertexCoordIndexed:
      Arrays.ColorRgbAlpha(ArrayIndexNum)^ := GetMaterial1Color(CoordIndex.ItemsSafe[IndexNum]);
    miPerVertexMatIndexed:
      Arrays.ColorRgbAlpha(ArrayIndexNum)^ := GetMaterial1Color(MaterialIndex.ItemsSafe[IndexNum]);
    miPerFace, miPerFaceMatIndexed:
      Arrays.ColorRgbAlpha(ArrayIndexNum)^ := FaceMaterial1Color;
    else ;
  end;
end;

procedure TAbstractMaterial1Generator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  case Mat1Implementation of
    miPerFace:
      FaceMaterial1Color := GetMaterial1Color(RangeNumber);
    miPerFaceMatIndexed:
      FaceMaterial1Color := GetMaterial1Color(MaterialIndex.Items.L[RangeNumber]);
    else ;
  end;
end;

{ TAbstractColorGenerator --------------------------------------- }

procedure TAbstractColorGenerator.PrepareAttributes(var AllowIndexed: boolean);
begin
  inherited;

  if ColorNode <> nil then
  begin
    Assert((Color <> nil) or (ColorRGBA <> nil));
    Arrays.AddColor(ColorNode.Mode, ColorRGBA <> nil);
    { In case of X3D primitives like IndexedTriangle[Fan/Strip]Set,
      ColorIndex is set but always equal to CoordIndex.
      This means we don't have to disable AllowIndexed (which is beneficial for speed).

      Testcase: test examples/fps_game/data/knight_creature/knight.gltf
      with LogShapes, observe it should have AllowIndexed=true
      and CoordinatePreserveGeometryOrder=true. }
    if ((ColorIndex <> nil) and (ColorIndex <> CoordIndex)) or
       (not ColorPerVertex) then
      AllowIndexed := false;
  end;
end;

function TAbstractColorGenerator.UseColorIndex: Boolean;
begin
  Result := (ColorIndex <> nil) and (ColorIndex.Count <> 0);
end;

procedure TAbstractColorGenerator.GenerateCoordinateBegin;
begin
  inherited;

  { This method, and GenerateVertex, closely fill all possible cases.
    When possible, this method (which results in more optimal code) takes care
    of the job, and then GenerateVertex does nothing. }

  if Color <> nil then
  begin
    if ColorPerVertex then
    begin
      if not UseColorIndex then
        AssignAttribute('colors', Arrays.ColorRgb, Color.Items.L, Color.Items.ItemSize, Color.Items.Count);
    end;
  end else
  if ColorRGBA <> nil then
  begin
    if ColorPerVertex then
    begin
      if not UseColorIndex then
        AssignAttribute('RGB colors', Arrays.ColorRgbAlpha, ColorRGBA.Items.L, ColorRGBA.Items.ItemSize, ColorRGBA.Items.Count);
    end;
  end;
end;

procedure TAbstractColorGenerator.GenerateVertex(IndexNum: integer);
begin
  inherited;
  if Color <> nil then
  begin
    if ColorPerVertex then
    begin
      if UseColorIndex then
        Arrays.ColorRgb(ArrayIndexNum)^ := Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]];
    end else
      Arrays.ColorRgb(ArrayIndexNum)^ := FaceColorRgb;
  end else
  if ColorRGBA <> nil then
  begin
    if ColorPerVertex then
    begin
      if UseColorIndex then
        Arrays.ColorRgbAlpha(ArrayIndexNum)^ := ColorRGBA.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]];
    end else
      Arrays.ColorRgbAlpha(ArrayIndexNum)^ := FaceColorRgbAlpha;
  end;
end;

procedure TAbstractColorGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  { Implement different color per face here. }
  if (Color <> nil) and (not ColorPerVertex) then
  begin
    if UseColorIndex then
      FaceColorRgb := Color.ItemsSafe[ColorIndex.ItemsSafe[RangeNumber]]
    else
      FaceColorRgb := Color.ItemsSafe[RangeNumber];
  end else
  if (ColorRGBA <> nil) and (not ColorPerVertex) then
  begin
    if UseColorIndex then
      FaceColorRgbAlpha := ColorRGBA.ItemsSafe[ColorIndex.ItemsSafe[RangeNumber]]
    else
      FaceColorRgbAlpha := ColorRGBA.ItemsSafe[RangeNumber];
  end;
end;

{ TAbstractNormalGenerator ----------------------------------------------------- }

procedure TAbstractNormalGenerator.PrepareAttributes(
  var AllowIndexed: boolean);
begin
  inherited;

  if not (
      { When IndexNum for normal works exactly like for position,
        then normals can be indexed. This is true in two cases:
        - there is no coordIndex, and normal vectors are not indexed
        - there is coordIndex, and normal vectors are indexed by coordIndex
        - also niOverall, niUnlit can work in every possible case
          (they just set the same value always).

        Note that when niPerVertexNonIndexed and CoordIndex <> nil,
        then we set AllowIndexed := false.
        This happens in particular when we have auto-generated normals based
        on creaseAngle in IndexedFaceSet.
        Testcase: auto_normals_indexed_geometry.x3dv
      }
      (NorImplementation in [niPerVertexCoordIndexed, niOverall, niUnlit]) or
     ((NorImplementation = niPerVertexNonIndexed) and (CoordIndex = nil)) ) then
    AllowIndexed := false;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TAbstractNormalGenerator.
  NorImplementationFromVRML1Binding(NormalBinding: Integer): TNormalsImplementation;
begin
  Result := niNone;

  if (Normals = nil) or (NormalIndex = nil) then
    Exit;

  case NormalBinding of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.L[0] >= 0) then
        Result := niPerVertexNormalIndexed;
    BIND_PER_VERTEX:
      if CoordIndex <> nil then
        Result := niPerVertexCoordIndexed;
    BIND_OVERALL:
      if Normals.Count > 0 then
        Result := niOverall;
    BIND_PER_PART, BIND_PER_FACE:
      Result := niPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.L[0] >= 0) then
        Result := niPerFaceNormalIndexed;
  end;

  { If no normals are provided (for VRML 1.0, this means that last Normal
    node was empty, or it's the default empty node from VRML1DefaultState)
    then generate normals. }
  if Normals.Count = 0 then
    Result := niNone;
end;
{$warnings on}

function TAbstractNormalGenerator.NormalsSafe(const Index: Integer): TVector3;
begin
  if Index < Normals.Count then
    Result := Normals.L[Index]
  else
    Result := TVector3.Zero;
end;

procedure TAbstractNormalGenerator.GetNormal(
  IndexNum: Integer; RangeNumber: Integer; out N: TVector3);
begin
  case NorImplementation of
    niOverall:
      N := Normals.L[0];
    niUnlit:
      N := TVector3.Zero; // return anything defined
    niPerVertexNonIndexed:
      N := Normals.L[IndexNum];
    niPerVertexCoordIndexed:
      N := Normals.L[CoordIndex.Items.L[IndexNum]];
    niPerVertexNormalIndexed:
      N := Normals.L[NormalIndex.ItemsSafe[IndexNum]];
    niPerFace:
      N := Normals.L[RangeNumber];
    niPerFaceNormalIndexed:
      N := Normals.L[NormalIndex.ItemsSafe[RangeNumber]];
    else
      raise EInternalError.CreateFmt('NorImplementation unknown (probably niNone, and not overridden GetNormal) in class %s',
        [ClassName]);
  end;
end;

function TAbstractNormalGenerator.NormalsFlat: boolean;
begin
  Result := NorImplementation in [niUnlit, niOverall, niPerFace, niPerFaceNormalIndexed];
end;

procedure TAbstractNormalGenerator.GenerateVertex(IndexNum: Integer);
begin
  inherited;
  case NorImplementation of
    niPerVertexNormalIndexed:
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := NormalsSafe(NormalIndex.ItemsSafe[IndexNum]);
      end;
    niPerVertexNonIndexed:
      if CoordIndex <> nil then
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := NormalsSafe(IndexNum);
      end;
    niPerFace, niPerFaceNormalIndexed:
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := FaceNormal;
      end;
    else ;
  end;
end;

procedure TAbstractNormalGenerator.GenerateCoordinateBegin;

  procedure SetAllNormals(const Value: TVector3);
  begin
    AssignToInterleavedConstant(Arrays.Normal, Arrays.CoordinateSize, Arrays.Count, @Value, SizeOf(TVector3));
  end;

begin
  inherited;

  case NorImplementation of
    niPerVertexCoordIndexed:
      AssignCoordinate('normals', Arrays.Normal, Normals.L, Normals.ItemSize, Normals.Count);
    niPerVertexNonIndexed:
      AssignCoordinate('normals', Arrays.Normal, Normals.L, Normals.ItemSize, Normals.Count, CoordIndex <> nil);
    niOverall: SetAllNormals(NormalsSafe(0));
    niUnlit: SetAllNormals(TVector3.Zero);
    else ;
  end;
end;

procedure TAbstractNormalGenerator.GenerateCoordinateEnd;
begin
  inherited;
end;

procedure TAbstractNormalGenerator.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  { FaceNormal will be actually copied to Arrays.Normal in GenerateVertex
    for this face. }
  case NorImplementation of
    niPerFace:
      FaceNormal := NormalsSafe(RangeNumber);
    niPerFaceNormalIndexed:
      FaceNormal := NormalsSafe(NormalIndex.ItemsSafe[RangeNumber]);
    else ;
  end;
end;

{ TAbstractFogGenerator --------------------------------- }

constructor TAbstractFogGenerator.Create(AShape: TShape);
begin
  inherited;

  if Geometry.InternalFogCoord <> nil then
    FogCoord := Geometry.InternalFogCoord.Items;
end;

procedure TAbstractFogGenerator.PrepareAttributes(
  var AllowIndexed: boolean);
begin
  inherited;

  if (FogCoord <> nil) or FogVolumetric then
  begin
    Arrays.AddFogCoord;
    Arrays.FogDirectValues := FogCoord <> nil;
  end;
end;

procedure TAbstractFogGenerator.GenerateVertex(
  IndexNum: Integer);

  function GetFogCoord: Single;
  begin
    { make IndexNum independent of coordIndex, always work like index to coords }
    if CoordIndex <> nil then
      IndexNum := CoordIndex.Items.L[IndexNum];

    { calculate Fog, based on FogCoord and IndexNum }
    if IndexNum < FogCoord.Count then
      Result := FogCoord.L[IndexNum]
    else
    if FogCoord.Count <> 0 then
      Result := FogCoord.Last
    else
      Result := 0; //< some default
  end;

  function GetFogVolumetric: Single;
  var
    Position, Projected: TVector3;
  begin
    { calculate global vertex position }
    if CoordIndex <> nil then
      Position := Coord.Items.L[CoordIndex.Items.L[IndexNum]]
    else
      Position := Coord.Items.L[IndexNum];
    Position := State.Transformation.Transform.MultPoint(Position);

    Projected := PointOnLineClosestToPoint(
      TVector3.Zero, FogVolumetricDirection, Position);
    Result := Projected.Length;
    if not AreParallelVectorsSameDirection(
      Projected, FogVolumetricDirection) then
      Result := -Result;
    { Now I want
      - Result = FogVolumetricVisibilityStart -> 0
      - Result = FogVolumetricVisibilityStart + X -> X
        (so that Result = FogVolumetricVisibilityStart +
        FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
    Result := Result - FogVolumetricVisibilityStart;
  end;

  procedure SetFog(const Fog: Single);
  begin
    { When Result < 0 our intention is to have no fog (at least
      for volumetric fog, for explicit FogCoordinate we don't know what
      was user's intention).
      So Result < 0 should be equivalent to Result = 0.
      However, OpenGL doesn't necessarily interpret it like this.

      Since factor given by glFogCoordfEXT is interpreted just like
      eye distance (i.e. it's processed by appopriate linear or exp or exp2
      equations), negative values may produce quite unexpected results
      (unless you really look at the equations).

      This is mentioned in the extension specification
      [http://oss.sgi.com/projects/ogl-sample/registry/EXT/fog_coord.txt].
      First is says:

        * Should the specified value be used directly as the fog weighting
          factor, or in place of the z input to the fog equations?

          As the z input; more flexible and meets ISV requests.

      ... which means that what glFogCoordfEXT gives is interpreted
      just like eye distance for normal fog (so it's e.g. affected
      by fog linear / exp / exp2 modes, affected by fog start and end values,
      etc.). Later it says:

        * Should the fog coordinate be restricted to non-negative values?

          Perhaps. Eye-coordinate distance of fragments will be
          non-negative due to clipping. Specifying explicit negative
          coordinates may result in very large computed f values, although
          they are defined to be clipped after computation.

      ... and this is precisely why specifying negative glFogCoordfEXT
      parameters is a bad idea: you don't really know what OpenGL
      implementation will do. NVidia OpenGL seems to actually assume
      that factor < 0 means the same as factor = 0, so my code
      worked OK without clamping below
      (because NVidia OpenGL was actually doing it anyway).
      Mesa 3D (and Radeon, as I suspect, because similar problems
      were reported for "The Castle" on Radeon) seem to just use the negative
      value directly, which causes strange artifacts
      (see e.g. "The Castle" gate_final.wrl VRML file).

      The clamping below makes volumetric fog work
      OK as expected for all OpenGL implementations.

      Note: we don't limit Fog to be <= 1 (although we could
      for X3D FogCoordinate). glFogCoord is like a distance from the eye,
      so it may be >= 1, and GetFogVolumetric depends on it to work. }
    Arrays.FogCoord(ArrayIndexNum)^ := Max(Fog, 0);
  end;

begin
  inherited;
  if FogCoord <> nil then
    SetFog(GetFogCoord) else
  if FogVolumetric then
    SetFog(GetFogVolumetric);
end;

{ TAbstractShaderAttribGenerator ------------------------------ }

constructor TAbstractShaderAttribGenerator.Create(AShape: TShape);
var
  A: TMFNode;
  I: Integer;
begin
  inherited;

  A := Geometry.AttribField;
  if A <> nil then
  begin
    SetLength(Attribs, A.Count); // in the most common case, when A.Count, this does nothing
    for I := 0 to A.Count - 1 do
      if A[I] is TAbstractVertexAttributeNode then
        Attribs[I].Node := TAbstractVertexAttributeNode(A[I]);
  end;
end;

procedure TAbstractShaderAttribGenerator.PrepareAttributes(var AllowIndexed: boolean);
var
  I: Integer;
  NumComponents: Integer;
begin
  inherited;
  for I := 0 to High(Attribs) do
  begin
    // this will be used later to check, with assert, that all clauses below set Attribs[I].GeometryAttrib
    {$ifdef DEBUG} Attribs[I].GeometryAttrib := TGeometryAttrib(Pointer(1)); {$endif}

    { call Arrays.AddGLSLAttribute* }
    if Attribs[I].Node is TFloatVertexAttributeNode then
    begin
      NumComponents := TFloatVertexAttributeNode(Attribs[I].Node).NumComponents;
      case NumComponents of
        1: Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeFloat(Attribs[I].Node.NameField, false);
        2: Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeVector2(Attribs[I].Node.NameField, false);
        3: Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeVector3(Attribs[I].Node.NameField, false);
        4: Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeVector4(Attribs[I].Node.NameField, false);
        else
        begin
          Attribs[I].GeometryAttrib := nil;
          WritelnWarning('X3D', Format('Invalid FloatVertexAttribute.numComponents: %d (should be between 1..4)',
            [NumComponents]));
        end;
      end;
    end else
    if Attribs[I].Node is TMatrix3VertexAttributeNode then
    begin
      Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeMatrix3(Attribs[I].Node.NameField, false);
    end else
    if Attribs[I].Node is TMatrix4VertexAttributeNode then
    begin
      Attribs[I].GeometryAttrib := Arrays.AddGLSLAttributeMatrix4(Attribs[I].Node.NameField, false);
    end else
    begin
      Attribs[I].GeometryAttrib := nil;
      WritelnWarning('X3D', Format('Not handled vertex attribute class %s',
        [Attribs[I].Node.X3DType]));
    end;

    Assert(Attribs[I].GeometryAttrib <> TGeometryAttrib(Pointer(1))); // all cases above should have set Attribs[I].GeometryAttrib explicitly
  end;
end;

procedure TAbstractShaderAttribGenerator.GenerateCoordinateBegin;

  procedure SetAttrib(const AttributeName: String;
    const TargetAttrib: TGeometryAttrib; const SourcePtr: Pointer; const SourceItemSize, SourceCount: SizeInt);
  begin
    AssignAttribute(AttributeName,
      Pointer(Arrays.GLSLAttribute(TargetAttrib)), SourcePtr, SourceItemSize, SourceCount);
  end;

var
  I: Integer;
  NumComponents: Integer;
  AttributeName: String;
begin
  inherited;
  for I := 0 to High(Attribs) do
  begin
    AttributeName := TAbstractVertexAttributeNode(Attribs[I].Node).NameField;

    { Note we don't do some warnings here, that were already done
      in PrepareAttributes (and set Attribs[I].Node = nil). }
    if Attribs[I].Node is TFloatVertexAttributeNode then
    begin
      NumComponents := TFloatVertexAttributeNode(Attribs[I].Node).NumComponents;
      { For NumComponents = 1, each TFloatVertexAttributeNode item is one attribute.
        For NumComponents = 2,3,4:
        Vector attributes in X3D are expressed using TFloatVertexAttributeNode,
        with each vector component one after another.
        We copy them fast, knowing that memory layout of TSingleList is the same as TVectorXxxList. }
      SetAttrib(AttributeName, Attribs[I].GeometryAttrib,
        TFloatVertexAttributeNode(Attribs[I].Node).FdValue.Items.L, SizeOf(Single) * NumComponents,
        TFloatVertexAttributeNode(Attribs[I].Node).FdValue.Items.Count div NumComponents);
    end else
    if Attribs[I].Node is TMatrix3VertexAttributeNode then
    begin
      SetAttrib(AttributeName, Attribs[I].GeometryAttrib,
        TMatrix3VertexAttributeNode(Attribs[I].Node).FdValue.Items.L, SizeOf(TMatrix3),
        TMatrix3VertexAttributeNode(Attribs[I].Node).FdValue.Items.Count);
    end else
    if Attribs[I].Node is TMatrix4VertexAttributeNode then
    begin
      SetAttrib(AttributeName, Attribs[I].GeometryAttrib,
        TMatrix4VertexAttributeNode(Attribs[I].Node).FdValue.Items.L, SizeOf(TMatrix4),
        TMatrix4VertexAttributeNode(Attribs[I].Node).FdValue.Items.Count);
    end;
  end;
end;

{ TAbstractBumpMappingGenerator ----------------------------------------------- }

procedure TAbstractBumpMappingGenerator.PrepareAttributes(var AllowIndexed: boolean);
begin
  inherited;
  if ShapeBumpMappingUsed then
    Arrays.AddTangent;
end;

procedure TAbstractBumpMappingGenerator.GenerateCoordinateBegin;
begin
  inherited;

  CalculateTangents := false;
  if ShapeBumpMappingUsed then
  begin
    if (TangentsFromNode <> nil) and
       (NorImplementation in [niPerVertexCoordIndexed, niPerVertexNonIndexed]) then
    begin
      AssignCoordinate('tangents', Arrays.Tangent, TangentsFromNode.L, TangentsFromNode.ItemSize, TangentsFromNode.Count)
    end else
    begin
      { If ShapeBumpMappingUsed, but we cannot use TangentsFromNode, then we need
        to calculate tangents per-vertex or per-face here. }
      CalculateTangents := true;
      { Too verbose? Would also show when we only do this once
        (because the geometry is not animated), which is not really a problem. }
      // WritelnWarning('Calculating tangent vectors during rendering. This is slow. Export model with "Tangents" selected to make it much faster to render.');
    end;
  end;
end;

procedure TAbstractBumpMappingGenerator.GenerateVertex(IndexNum: Integer);

  procedure SetTangents;
  var
    Normal: TVector3;
  begin
    if NormalsFlat then
    begin
      Arrays.Tangent(ArrayIndexNum)^ := Tangent;
    end else
    begin
      { If NormalsFlat = false,
        we want to calculate *local* Tangent,
        which is Tangent adjusted to the current vertex
        (since every vertex on this face may have a different normal).

        Without doing this, you would see strange artifacts, smoothed
        faces would look somewhat like flat faces.
        Conceptually, for smoothed
        faces, whole tangent space should vary for each vertex, so Normal,
        and both tangents/bitangents may be different on each vertex. }

      GetNormal(IndexNum, CurrentRangeNumber, Normal);
      Arrays.Tangent(ArrayIndexNum)^ := MakeVectorOrthogonal(Tangent, Normal);
    end;
  end;

begin
  inherited;
  if CalculateTangents then
    SetTangents;
end;

procedure TAbstractBumpMappingGenerator.CalculateTangentVectors(
  const TriangleIndex1, TriangleIndex2, TriangleIndex3: Integer);
var
  TriangleCoord: TTriangle3;
  TriangleTexCoord: TTriangle2;
begin
  if CalculateTangents then
  begin
    { calculate TriangleCoord }
    TriangleCoord[0] := GetVertex(TriangleIndex1);
    TriangleCoord[1] := GetVertex(TriangleIndex2);
    TriangleCoord[2] := GetVertex(TriangleIndex3);

    if not (
      { calculate TriangleTexCoord }
      GetTextureCoord(TriangleIndex1, ShapeBumpMappingTextureCoordinatesId, TriangleTexCoord.Data[0]) and
      GetTextureCoord(TriangleIndex2, ShapeBumpMappingTextureCoordinatesId, TriangleTexCoord.Data[1]) and
      GetTextureCoord(TriangleIndex3, ShapeBumpMappingTextureCoordinatesId, TriangleTexCoord.Data[2]) and
      { calculate Tangent }
      CalculateTangent(true , Tangent, TriangleCoord, TriangleTexCoord) ) then
    begin
      { Would be more correct to set Tangent as anything perpendicular to Normal. }
      Tangent := TVector3.One[0];
    end;

    { It *is* possible that we'll get somewhat incorrect tangents in practice,
      but it's not really useful to check it (at least in non-debug builds)
      because we don't really have here a better fallback.
      Using above "TVector3.One[0]" isn't really better. }
    {
    if not ( (Abs(TVector3.DotProduct(Tangent, Bitangent)) < 0.95) and
             (Abs(TVector3.DotProduct(Tangent, Normal)) < 0.95) and
             (Abs(TVector3.DotProduct(Bitangent, Normal)) < 0.95) ) then
      WritelnWarning('Tangents are likely incorrect');
    }
  end;
end;

{ non-abstract generators ---------------------------------------------------- }

{$I castleinternalarraysgenerator_rendering.inc}
{$I castleinternalarraysgenerator_geometry3d.inc}

{ global routines ------------------------------------------------------------ }

function GetArraysGenerator(AGeometry: TAbstractGeometryNode): TArraysGeneratorClass;
begin
  if AGeometry is TIndexedTriangleMeshNode_1 then
    Result := TTriangleStripSetGenerator else
  if AGeometry is TIndexedFaceSetNode_1 then
    Result := TIndexedFaceSet_1Generator else
  if AGeometry is TIndexedFaceSetNode then
    Result := TIndexedFaceSetGenerator else
  if AGeometry is TIndexedLineSetNode_1 then
    Result := TIndexedLineSet_1Generator else
  if (AGeometry is TIndexedLineSetNode) or
     (AGeometry is TLineSetNode) then
    Result := TLineSetGenerator else
  if AGeometry is TPointSetNode_1 then
    Result := TPointSet_1Generator else
  if AGeometry is TPointSetNode then
    Result := TPointSetGenerator else
  if (AGeometry is TTriangleSetNode) or
     (AGeometry is TIndexedTriangleSetNode) or
     { That is correct, TQuadSetNode goes to TTriangleSetGenerator,
       as TQuadSetNode.InternalTrianglesCoordIndex allows to treat it pretty much
       like TIndexedTriangleSetNode. }
     (AGeometry is TQuadSetNode) then
    Result := TTriangleSetGenerator else
  if (AGeometry is TTriangleFanSetNode) or
     (AGeometry is TIndexedTriangleFanSetNode) then
    Result := TTriangleFanSetGenerator else
  if (AGeometry is TTriangleStripSetNode) or
     (AGeometry is TIndexedTriangleStripSetNode) then
    Result := TTriangleStripSetGenerator else
  if (AGeometry is TIndexedQuadSetNode) then
    Result := TQuadSetGenerator else
    Result := nil;
end;

end.
