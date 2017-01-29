{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3DS loader (Load3DS procedure). }

unit X3DLoadInternal3DS;

{$I castleconf.inc}

interface

uses X3DNodes;

function Load3DS(const URL: string): TX3DRootNode;

implementation

uses CastleUtils, Classes, CastleClassUtils, SysUtils, CastleVectors, X3DCameraUtils,
  FGL, X3DLoadInternalUtils, CastleLog, CastleDownload, CastleURIUtils,
  CastleStreamUtils;

{ 3DS reading mostly based on spec from
  [http://www.martinreddy.net/gfx/3d/3DS.spec].

  TScene3DS corresponds to the whole 3DS file,
  that is the MAIN chunk, and also (since we don't handle keyframes from 3DS)
  the OBJMESH chunk.

  It contains lists of triangle meshes, cameras and lights. They are all TObject3DS,
  and correspond to OBJBLOCK chunk, with inside TRIMESH, CAMERA or LIGHT chunk.
  As far as I understand, OBJBLOCK in 3DS may contain only *one* of
  trimesh, light or camera. We assume this.
  - Trimesh wraps OBJBLOCK chunk with VERTLIST subchunk.

  Moreover TScene3DS has a list of TMaterial3ds, that correspond
  to the MATERIAL chunk.

  TODO:
  - properties that are read from 3ds but not used anywhere because their exact
    interpretation is not known for me:
      TCamera3ds.Lens
      TMaterialMap3ds.Offset (I don't know wheteher use it before of after
        Scale, again I need some test 3ds scenes to check that)
      TMaterial3ds.TextureMap2
      TMaterial3ds.ShininessStrenth, TransparencyFalloff, ReflectBlur

  - TFace3ds.Wrap interpretation is known but it is not used (because
    1. implementing it requires some mess in code
       since this Wrap is the property of _each face_ (instead of _each texture_,
       which would be simpler to map to OpenGL and VRML)
    2. and I don't have enough test 3ds scenes with textures to really
       see the difference. So, I will probably implement it someday when
       I'll need it.
}

{ Declare classes ------------------------------------------------------------ }

type
  EInvalid3dsFile = class(Exception);
  EMaterialNotInitialized = class(EInvalid3dsFile);

  TMaterialMap3ds = record
    Exists: boolean;
    MapURL: string;
    Scale, Offset: TVector2Single;
  end;

  TMaterial3ds = class
  strict private
    FName: string;
    FInitialized: boolean;
  public
    property Name: string read FName;

    { When @false, this material was found in TTrimesh but was not yet
      defined in 3DS file. }
    property Initialized: boolean read FInitialized default false;
  public
    { Material properties. Have default values (following VRML and OpenGL
      defaults, as I don't know 3DS defaults) in case they would be
      undefined in 3DS file. }
    AmbientColor: TVector4Single;
    DiffuseColor: TVector4Single;
    SpecularColor: TVector4Single;

    { Texture maps, initialized with Exists = false }
    TextureMap1, TextureMap2, TextureMapBump: TMaterialMap3ds;

    { All Singles below are always read from 3ds file (they are required
      subchunks of material chunk). They are in range 0..1. } { }
    Shininess: Single;
    ShininessStrenth, Transparency,
      TransparencyFalloff, ReflectBlur :Single; {< By default 0 }

    constructor Create(const AName: string);

    { Read CHUNK_MATERIAL, initializing our fields and changing
      @link(Initialized) to @true. }
    procedure ReadFromStream(Stream: TStream; EndPos: Int64);
  end;

  TMaterial3dsList = class(specialize TFPGObjectList<TMaterial3ds>)
  public
    { Index of material with given name. If material doesn't exist,
      it will be added. }
    function MaterialIndex(const MatName: string): Integer;
    { Raises EMaterialNotInitialized if any not TMaterial3ds.Initialized
      material present on the list. You should call it at the end of reading
      3DS file, to make sure all used materials were found in 3DS file.  }
    procedure CheckAllInitialized;
    procedure ReadMaterial(Stream: TStream; EndPos: Int64);
  end;

  { }
  TScene3DS = class;

  { @abstract(Abstract class for 3DS triangle mesh, camera or light source.)

    Use CreateObject3DS method to read contents from stream,
    creating appropriate non-abstract TObject3DS descendant. }
  TObject3DS = class
  strict private
    FName: string;
    FScene: TScene3DS;
  public
    property Name: string read FName;
    { Scene containing this object. }
    property Scene: Tscene3DS read FScene;

    { Constructor reading 3DS chunk of object part,
      like CHUNK_TRIMESH or CHUNK_CAMERA or CHUNK_LIGHT.

      Assumes that we just read from stream chunk header (with id =
      one of the above and length indicating that Stream.Position >= ChunkEndPos
      is after the object part), then we read AName (so now we can read subchunks).

      You don't have to finish reading at ChunkEndPos, we'll rewind
      the stream position afterwards if necessary. }
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; const ChunkEndPos: Int64); virtual;
  end;

  TFace3ds = record
    VertsIndices: TVector3Cardinal;
    EdgeFlags: packed array[0..2]of boolean;
    Wrap: packed array[0..1]of boolean;
    { Index to the Scene.Materials.
      -1 means "not specified in 3DS file" and means that face
      uses default material. There are some 3DS that have faces without
      material defined --- for example therack.3ds. }
    FaceMaterialIndex: Integer;
  end;
  TArray_Face3ds = packed array [0 .. MaxInt div SizeOf(TFace3ds) - 1] of TFace3ds;
  PArray_Face3ds = ^TArray_Face3ds;

  { Vertex information from 3DS. }
  TVertex3ds = packed record
    Pos: TVector3Single;
    { Texture coordinates. (0, 0) if the object doesn't have texture coords
      (HasTexCoords is @false). }
    TexCoord: TVector2Single;
  end;
  TArray_Vertex3ds = packed array [0 .. MaxInt div SizeOf(TVertex3ds) - 1] of TVertex3ds;
  PArray_Vertex3ds = ^TArray_Vertex3ds;

  { Triangle mesh. }
  TTrimesh3ds = class(TObject3DS)
  strict private
    FVertsCount, FFacesCount: Word;
    FHasTexCoords: boolean;
  public
    { Vertexes and faces. Read-only from outside of this class.
      @groupBegin }
    Verts: PArray_Vertex3ds;
    Faces: PArray_Face3ds;
    { @groupEnd }
    { Do the vertexes have meaningful texture coordinates? }
    property HasTexCoords: boolean read FHasTexCoords;
    { Number of vertexes and faces.
      Remember that VertsCount = FacesCount = 0 is possible, e.g. 0155.3ds.
      @groupBegin }
    property VertsCount: Word read FVertsCount;
    property FacesCount: Word read FFacesCount;
    { @groupEnd }

    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; const ChunkEndPos: Int64); override;
    destructor Destroy; override;
  end;

  TCamera3ds = class(TObject3DS)
  strict private
    FPosition, FTarget: TVector3Single;
    FBank, FLens: Single;
  public
    property Position: TVector3Single read FPosition;
    property Target: TVector3Single read FTarget;
    property Bank: Single read FBank;
    property Lens: Single read FLens;
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; const ChunkEndPos: Int64); override;

    { Camera direction. Calculated from Position and Target. }
    function Direction: TVector3Single;
    { Camera up. Calculated from Direction and Bank. }
    function Up: TVector3Single;
  end;

  TLight3ds = class(TObject3DS)
  public
    Pos: TVector3Single;
    Col: TVector3Single;
    Enabled: boolean;
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; const ChunkEndPos: Int64); override;
  end;

  TTrimesh3dsList = specialize TFPGObjectList<TTrimesh3ds>;
  TCamera3dsList = specialize TFPGObjectList<TCamera3ds>;
  TLight3dsList = specialize TFPGObjectList<TLight3ds>;

  { 3DS loader. }
  TScene3DS = class
  public
    { Triangle meshes, cameras and other properties of the scene.
      They are created and destroyed by TScene3DS
      object --- so these fields are read-only from outside of this class.
      @groupBegin }
    Trimeshes: TTrimesh3dsList;
    Cameras: TCamera3dsList;
    Lights: TLight3dsList;
    Materials: TMaterial3dsList;
    { @groupEnd }
    { Autodesk version used to create this 3DS. }
    Version: LongWord;
    constructor Create(Stream: TStream); overload;
    constructor Create(const URL: string); overload;
    destructor Destroy; override;
  end;

{ Chunks utilities ----------------------------------------------------------- }

{ The indentation below corresponds to chunk relations in 3DS file.
  Based on example 3dsRdr.c, with new needed chunks added, based
  on various Internal sources about 3DS and MLI, with some new comments.

  Some subchunks that are required within parent chunks are marked as such. }
const
  { Color chunks may be in various places in 3DS file.
    _GAMMA suffix means that these colors are already gamma corrected.
    @groupBegin }
  CHUNK_RGBF       = $0010;
  CHUNK_RGBB       = $0011;
  CHUNK_RGBB_GAMMA = $0012;
  CHUNK_RGBF_GAMMA = $0013;
  { @groupEnd }

  { CHUNK_DOUBLE_BYTE from MLI specification. Experiments show
    that this has 0..100 range, at least when used for shininess subchunks
    (in other uses, like transparency, the range @italic(may) be smaller).

    lib3ds confirms this, by even calling this as INT_PERCENTAGE instead
    of DOUBLE_BYTE. Used for CHUNK_SHININESS, CHUNK_SHININESS_STRENTH,
    CHUNK_TRANSPARENCY, CHUNK_TRANSPARENCY_FALLOFF, CHUNK_REFLECT_BLUR. }
  CHUNK_DOUBLE_BYTE = $0030;

  { Root file chunks.
    MAIN chunk is the whole 3DS file.
    MLI chunk is the whole MLI (Material-Library) file.
    Probably PRJ chunk is also something like that (some "project" ?).
    @groupBegin }
  CHUNK_PRJ       = $C23D;
  CHUNK_MLI       = $3DAA;
  CHUNK_MAIN      = $4D4D;
  { @groupEnd }

    CHUNK_VERSION   = $0002;
    CHUNK_OBJMESH   = $3D3D;
      CHUNK_BKGCOLOR  = $1200;
      CHUNK_AMBCOLOR  = $2100;
      { As I understand, exactly one of the subchunks TRIMESH, LIGHT
        and CAMERA appears in one OBJBLOCK chunk. } { }
      CHUNK_OBJBLOCK  = $4000;
        CHUNK_TRIMESH   = $4100;
          CHUNK_VERTLIST  = $4110;
          CHUNK_FACELIST  = $4120;
            CHUNK_FACEMAT   = $4130;
          CHUNK_MAPLIST   = $4140; {< texture coordinates }
            CHUNK_SMOOLIST  = $4150;
          CHUNK_TRMATRIX  = $4160;
        CHUNK_LIGHT     = $4600;
          CHUNK_SPOTLIGHT = $4610;
        CHUNK_CAMERA    = $4700;
          CHUNK_HIERARCHY = $4F00;
    CHUNK_VIEWPORT  = $7001;

    CHUNK_MATERIAL  = $AFFF;
      CHUNK_MATNAME   = $A000; {< required; asciiz, no subchunks }
      CHUNK_AMBIENT   = $A010; {< required; subchunks are RGB chunk[s] }
      CHUNK_DIFFUSE   = $A020; {< required; subchunks are RGB chunk[s] }
      CHUNK_SPECULAR  = $A030; {< required; subchunks are RGB chunk[s] }
      CHUNK_SHININESS = $A040;            {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_SHININESS_STRENTH = $A041;    {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_TRANSPARENCY = $A050;         {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_TRANSPARENCY_FALLOFF = $A052; {< required; subchunks are DOUBLE_BYTE chunk[s] }
      CHUNK_REFLECT_BLUR = $A053;         {< required; subchunks are DOUBLE_BYTE chunk[s] }

      CHUNK_TEXMAP_1  = $A200; {< two texture maps }
      CHUNK_TEXMAP_2  = $A33A;
      CHUNK_BUMPMAP   = $A230;
        { All MAP chunks below can be subchunks of all MAP chunks above. } { }
        CHUNK_MAP_FILE   = $A300; {< asciiz, no subchunks }
        CHUNK_MAP_USCALE = $A356; {< single, no subchunks }
        CHUNK_MAP_VSCALE = $A354; {< single, no subchunks }
        CHUNK_MAP_UOFFSET = $A358; {< single, no subchunks }
        CHUNK_MAP_VOFFSET = $A35A; {< single, no subchunks }

    CHUNK_KEYFRAMER = $B000;
      CHUNK_AMBIENTKEY    = $B001;
      CHUNK_TRACKINFO = $B002;
        CHUNK_TRACKOBJNAME  = $B010;
        CHUNK_TRACKPIVOT    = $B013;
        CHUNK_TRACKPOS      = $B020;
        CHUNK_TRACKROTATE   = $B021;
        CHUNK_TRACKSCALE    = $B022;
        CHUNK_OBJNUMBER     = $B030;
      CHUNK_TRACKCAMERA = $B003;
        CHUNK_TRACKFOV  = $B023;
        CHUNK_TRACKROLL = $B024;
      CHUNK_TRACKCAMTGT = $B004;
      CHUNK_TRACKLIGHT  = $B005;
      CHUNK_TRACKLIGTGT = $B006;
      CHUNK_TRACKSPOTL  = $B007;
      CHUNK_FRAMES    = $B008;

type
  TChunkHeader = packed record
    id: Word;
    len: LongWord;
  end;

  T3dsStreamHelper = class helper(TStreamHelper) for TStream
    procedure ReadChunkHeader(out Buffer: TChunkHeader); inline;
  end;

procedure T3dsStreamHelper.ReadChunkHeader(out Buffer: TChunkHeader); inline;
begin
  ReadBuffer(Buffer, SizeOf(Buffer));
  Buffer.id := LEtoN(Buffer.id);
  Buffer.len := LEtoN(Buffer.len);
end;

procedure Check3dsFile(TrueValue: boolean; const ErrMessg: string);
begin
  if not TrueValue then raise EInvalid3dsFile.Create(ErrMessg);
end;

{ Read 3DS subchunks until EndPos, ignoring everything except color information.
  It's guaranteed that Stream.Position is EndPos at the end.

  Returns did we find any color information. If @false,
  Col is left not modified (it's intentionally a "var" parameter,
  not an "out" parameter).

  Overloaded version with 4 components always returns alpha = 1. }
function TryReadColorInSubchunks(var Col: TVector3Single;
  Stream: TStream; EndPos: Int64): boolean;
var
  h: TChunkHeader;
  hEnd: Int64;
  Col3Byte: TVector3Byte;
{$ifdef ENDIAN_BIG}
  b: Byte;
{$endif ENDIAN_BIG}
begin
  result := false;
  while Stream.Position < EndPos do
  begin
    Stream.ReadChunkHeader(h);
    hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
    { TODO: we ignore gamma correction entirely so we don't distinct
      gamma corrected and not corrected colors }
    case h.id of
      CHUNK_RGBF, CHUNK_RGBF_GAMMA:
        begin
          Stream.ReadLE(Col);
          result := true;
          break;
        end;
      CHUNK_RGBB, CHUNK_RGBB_GAMMA:
        begin
          Stream.ReadBuffer(Col3Byte, SizeOf(Col3Byte));
          {$ifdef ENDIAN_BIG}
          b := Col3Byte[0];
          Col3Byte[0] := Col3Byte[2];
          Col3Byte[2] := b;
          {$endif ENDIAN_BIG}
          Col := Vector3Single(Col3Byte);
          result := true;
          break;
        end;
      else Stream.Position := hEnd;
    end;
  end;
  Stream.Position := EndPos;
end;

function TryReadColorInSubchunks(var Col: TVector4Single;
  Stream: TStream; EndPos: Int64): boolean;
var
  Col3Single: TVector3Single;
begin
  result := TryReadColorInSubchunks(Col3Single, Stream, EndPos);
  if result then Col := Vector4Single(Col3Single);
end;

{ Read 3DS subchunks until EndPos, ignoring everything except CHUNK_DOUBLE_BYTE
  value. Returns the value / 100.
  Similar comments as for TryReadColorInSubchunks. }
function TryReadPercentageInSubchunks(var Value: Single;
  Stream: TStream; EndPos: Int64): boolean;
var
  h: TChunkHeader;
  hEnd: Int64;
  DoubleByte: Word;
begin
  result := false;
  while Stream.Position < EndPos do
  begin
    Stream.ReadChunkHeader(h);
    hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
    if h.id = CHUNK_DOUBLE_BYTE then
    begin
      Stream.ReadLE(DoubleByte);
      result := true;
      break;
    end else
      Stream.Position := hEnd;
  end;
  Stream.Position := EndPos;
  if result then Value := Min(DoubleByte / 100, 1.0);
end;

{ 3DS materials -------------------------------------------------------------- }

const
  { TODO: I don't know default 3DS material parameters.
    Below I just use some default OpenGL and VRML 1.0 values. } { }
  Default3dsMatAmbient: TVector4Single = (0.2, 0.2, 0.2, 1.0);
  Default3dsMatDiffuse: TVector4Single = (0.8, 0.8, 0.8, 1.0);
  Default3dsMatSpecular: TVector4Single = (0, 0, 0, 1.0);
  Default3dsMatShininess: Single = 0.2; {< in range 0..1 }

constructor TMaterial3ds.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FInitialized := false;
  AmbientColor := Default3dsMatAmbient;
  DiffuseColor := Default3dsMatDiffuse;
  SpecularColor := Default3dsMatSpecular;
  Shininess := Default3dsMatShininess;
  TextureMap1.Exists := false;
  TextureMap2.Exists := false;
  TextureMapBump.Exists := false;
end;

procedure TMaterial3ds.ReadFromStream(Stream: TStream; EndPos: Int64);

  function ReadMaterialMap(EndPos: Int64): TMaterialMap3ds;
  const
    InitialExistingMatMap: TMaterialMap3ds =
    (Exists: true; MapURL: ''; Scale: (1, 1); Offset: (0, 0));
  var
    h: TChunkHeader;
    hEnd: Int64;
  begin
    result := InitialExistingMatMap;

    { read MAP subchunks }
    while Stream.Position < EndPos do
    begin
      Stream.ReadChunkHeader(h);
      hEnd := Stream.Position -SizeOf(TChunkHeader) +h.len;
      case h.id of
        CHUNK_MAP_FILE: Result.MapURL := StreamReadZeroEndString(Stream);
        CHUNK_MAP_USCALE: Stream.ReadLE(Result.Scale[0]);
        CHUNK_MAP_VSCALE: Stream.ReadLE(Result.Scale[1]);
        CHUNK_MAP_UOFFSET: Stream.ReadLE(Result.Offset[0]);
        CHUNK_MAP_VOFFSET: Stream.ReadLE(Result.Offset[1]);
        else Stream.Position := hEnd;
      end;
    end;
  end;

var
  h: TChunkHeader;
  hEnd: Int64;
begin
  { read material subchunks }
  while Stream.Position < EndPos do
  begin
    Stream.ReadChunkHeader(h);
    hEnd := Stream.Position -SizeOf(TChunkHeader) +h.len;
    case h.id of
      { Colors }
      CHUNK_AMBIENT: TryReadColorInSubchunks(AmbientColor, Stream, hEnd);
      CHUNK_DIFFUSE: TryReadColorInSubchunks(DiffuseColor, Stream, hEnd);
      CHUNK_SPECULAR: TryReadColorInSubchunks(SpecularColor, Stream, hEnd);

      { Percentage values }
      CHUNK_SHININESS:
        TryReadPercentageInSubchunks(Shininess, Stream, hEnd);
      CHUNK_SHININESS_STRENTH:
        TryReadPercentageInSubchunks(ShininessStrenth, Stream, hEnd);
      CHUNK_TRANSPARENCY:
        TryReadPercentageInSubchunks(Transparency, Stream, hEnd);
      CHUNK_TRANSPARENCY_FALLOFF:
        TryReadPercentageInSubchunks(TransparencyFalloff, Stream, hEnd);
      CHUNK_REFLECT_BLUR:
        TryReadPercentageInSubchunks(ReflectBlur, Stream, hEnd);

      CHUNK_TEXMAP_1: TextureMap1 := ReadMaterialMap(hEnd);
      CHUNK_TEXMAP_2: TextureMap2 := ReadMaterialMap(hEnd);
      CHUNK_BUMPMAP : TextureMapBump := ReadMaterialMap(hEnd);
      else Stream.Position := hEnd;
    end;
  end;

  FInitialized := true;
end;

{ TMaterial3dsList ----------------------------------------------------- }

function TMaterial3dsList.MaterialIndex(const MatName: string): Integer;
begin
  for result := 0 to Count-1 do
    if Items[result].Name = MatName then exit;
  {material not found ?}
  Add(TMaterial3ds.Create(MatName));
  result := Count-1;
end;

procedure TMaterial3dsList.CheckAllInitialized;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    if not Items[i].Initialized then
      raise EMaterialNotInitialized.Create(
        'Material "'+Items[i].Name+'" used but does not exist');
end;

procedure TMaterial3dsList.ReadMaterial(Stream: TStream; EndPos: Int64);
var
  ind: Integer;
  MatName: string;
  StreamStartPos: Int64;
  h: TChunkHeader;
begin
  { every material must have chunk MATNAME, otherwise it's useless
    (visible objects may use the material only by name).
    As far as I know, only one MATNAME is allowed (only one name for one material).
    Look for MATNAME chunk now. }
  StreamStartPos := Stream.Position;
  MatName := '';

  while Stream.Position < EndPos do
  begin
    Stream.ReadChunkHeader(h);
    if h.id = CHUNK_MATNAME then
    begin
      MatName := StreamReadZeroEndString(Stream);
      break;
    end else
      Stream.Position := Stream.Position -SizeOf(TChunkHeader) +h.len;
  end;

  Stream.Position := StreamStartPos; { restore original position in Stream }
  if MatName = '' then raise EInvalid3dsFile.Create('Unnamed material');

  { found MatName. Add it to Items, unless it's already there.
    In the latter case, mark it initialized (if not already), or raise exception
    (because it means material properties were specified two times). }
  ind := MaterialIndex(MatName);
  with Items[ind] do
  begin
    Check3dsFile( not Initialized, 'Duplicate material '+MatName+' specification');
    { if not Initialized then material was added to the list by MaterialIndex
      or was used in a trimesh already --- but was not yet defined in 3DS.
      So initialize it now. }
    ReadFromStream(Stream, EndPos);
  end;
end;

{ Useful consts -------------------------------------------------------------- }

const
  { Bit flags for checking is there an edge flag between I and ((I+1) mod 3) vertex. }
  FACEFLAG_EDGES: array[0..2]of Word = ($4, $2, $1);
  { Bit flags for checking texture wrap u,v. }
  FACEFLAG_WRAP: array[0..1]of Word = ($8, $10);

{ TObject3DS ----------------------------------------------------------------- }

constructor TObject3DS.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; const ChunkEndPos: Int64);
{ don't ever call directly this constructor - we can get here
  only by  "inherited" call from Descendant's constructor }
begin
  inherited Create;
  FName := AName;
  FScene := AScene;
end;

function CreateObject3DS(AScene: TScene3DS; Stream: TStream; const ObjectEndPos: Int64): TObject3DS;
var
  ObjName: string;
  ChunkEndPos: Int64;
  h: TChunkHeader;
begin
  ObjName := StreamReadZeroEndString(Stream);
  Result := nil;

  { searching for chunk TRIMESH / CAMERA / LIGHT }
  while Stream.Position < ObjectEndPos do
  begin
    Stream.ReadChunkHeader(h);
    ChunkEndPos := Stream.Position + h.len - SizeOf(TChunkHeader);
    case h.id of
      CHUNK_TRIMESH: Result := TTrimesh3ds.Create(ObjName, AScene, Stream, ChunkEndPos);
      CHUNK_CAMERA: Result := TCamera3ds.Create(ObjName, AScene, Stream, ChunkEndPos);
      CHUNK_LIGHT: Result := TLight3ds.Create(ObjName, AScene, Stream, ChunkEndPos);
    end;
    if Result <> nil then break;
    Stream.Position := ChunkEndPos;
  end;

  {if none of the TRIMESH / CAMERA / LIGHT chunks found raise error}
  Check3dsFile( Result <> nil, 'No object recorded under the name '+ObjName);

  { set stream pos at the end of object }
  Stream.Position := ObjectEndPos;
end;

{ TTrimesh3ds --------------------------------------------------------------- }

constructor TTrimesh3ds.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; const ChunkEndPos: Int64);

  { Read FVertsCount, initialize Verts.
    Unless Verts is already non-zero, then only check it's correct
    (as FVertsCount of trimesh may be specified in a couple places,
    like VERTLIST and MAPLIST chunk). }
  procedure ReadVertsCount;
  var
    VertsCountCheck: Word;
  begin
    if VertsCount = 0 then
    begin
      Stream.ReadLE(FVertsCount);
      { Use GetClearMem to have Verts memory safely initialized to 0 }
      Verts := GetClearMem(SizeOf(TVertex3ds)*VertsCount);
    end else
    begin
      Stream.ReadLE(VertsCountCheck);
      Check3dsFile( VertsCountCheck = VertsCount,
        'Different VertexCount info for 3ds object '+Name);
    end;
  end;

  procedure ReadVertlist;
  var
    i: integer;
  begin
    ReadVertsCount;
    for i := 0 to VertsCount-1 do
      Stream.ReadLE(Verts^[i].Pos);
  end;

  procedure ReadMaplist(chunkEnd: Int64);
  var
    i: integer;
  begin
    FHasTexCoords := true;
    ReadVertsCount;
    for i := 0 to VertsCount-1 do
      Stream.ReadLE(Verts^[i].TexCoord);
    Stream.Position := chunkEnd; { skip subchunks }
  end;

  procedure ReadFacelist(chunkEnd: Int64);

    procedure ReadFacemat;
    var
      MatName: string;
      MatFaceCount: Word;
      FaceNum: Word;
      i, MatIndex: Integer;
    begin
      MatName := StreamReadZeroEndString(Stream);
      MatIndex := Scene.Materials.MaterialIndex(MatName);
      Stream.ReadLE(MatFaceCount);
      for i := 1 to MatFaceCount do
      begin
        Stream.ReadLE(FaceNum);
        Check3dsFile(FaceNum < FacesCount,
          'Invalid face number for material '+MatName);
        Check3dsFile(Faces^[FaceNum].FaceMaterialIndex = -1,
          'Duplicate material specification for face');
        Faces^[FaceNum].FaceMaterialIndex := MatIndex;
      end;
    end;

  var
    i, j: integer;
    Flags: Word;
    Word3: packed array[0..2]of Word;
    h: TChunkHeader;
    hEnd: Int64;
  begin
    Check3dsFile(FacesCount = 0, 'Duplicate faces specification for 3ds object '+Name);
    Stream.ReadLE(FFacesCount);
    Faces := GetMem(SizeOf(TFace3ds)*FacesCount);
    for i := 0 to FacesCount-1 do
    with Faces^[i] do
    begin
      { init face }
      Stream.ReadBuffer(Word3, SizeOf(Word3));
      for j := 0 to 2 do
        VertsIndices[j] := LEtoN(Word3[j]);
      Stream.ReadLE(Flags);
      { decode Flags }
      for j := 0 to 2 do
        EdgeFlags[j]:=(FACEFLAG_EDGES[j] and Flags) <> 0;
      for j := 0 to 1 do
        Wrap[j]:=(FACEFLAG_WRAP[j] and Flags) <> 0;
      FaceMaterialIndex := -1;
    end;

    { read subchunks - look for FACEMAT chunk }
    while Stream.Position < chunkEnd do
    begin
      Stream.ReadChunkHeader(h);
      hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
      if h.id = CHUNK_FACEMAT then
        ReadFacemat else
        Stream.Position := hEnd;
    end;
  end;

  (*
  procedure ReadMatrix(ChunkEnd: Int64);
  var
    I: Integer;
    Matrix: TMatrix4Single;
    MatrixTransform: TMatrixTransformNode;
  begin
    Matrix := IdentityMatrix4Single;
    for I := 0 to 3 do
      Stream.ReadLE(Matrix[I]);
    { The 4th row of our matrix will remain (0,0,0,1). }

    if not MatricesPerfectlyEqual(Matrix, IdentityMatrix4Single) then
    begin
      FreeIfUnusedAndNil(Group);
      MatrixTransform := TMatrixTransformNode.Create;
      MatrixTransform.FdMatrix.Value := Matrix;
      Group := MatrixTransform;
    end;

    if Stream.Position <> ChunkEnd then
    begin
      WritelnWarning('3DS', 'CHUNK_TRMATRIX should contain only 4x3 floats');
      Stream.Position := ChunkEnd;
    end;
  end;
  *)

var
  h: TChunkHeader;
  hEnd: Int64;
begin
  inherited;

  FHasTexCoords := false;

  { read subchunks inside CHUNK_TRIMESH }
  while Stream.Position < ChunkEndPos do
  begin
    Stream.ReadChunkHeader(h);
    hend := Stream.Position + h.len - SizeOf(TChunkHeader);
    case h.id of
      CHUNK_VERTLIST: ReadVertlist;
      CHUNK_FACELIST: ReadFacelist(hEnd);
      CHUNK_MAPLIST: ReadMaplist(hEnd);
      { The correct behavior to handle CHUNK_TRMATRIX is to ignore it, it seems.
        Looking at what Blender 3DS importer does: it seems they use matrix
        to transform object, but they also multiply mesh by the matrix inverse.
        IOW, matrix stores the transformation, which is useful for 3D modelling,
        but if you just want to display the model, then just ignore it.
      CHUNK_TRMATRIX: ReadMatrix(hEnd); }
      else Stream.Position := hEnd;
    end;
  end;
end;

destructor TTrimesh3ds.Destroy;
begin
  FreeMemNiling(Verts);
  FreeMemNiling(Faces);
  inherited;
end;

{ TCamera3ds --------------------------------------------------------------- }

constructor TCamera3ds.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; const ChunkEndPos: Int64);
begin
  inherited;

  { read CHUNK_CAMERA }
  Stream.ReadLE(FPosition);
  Stream.ReadLE(FTarget);
  Stream.ReadLE(FBank);
  Stream.ReadLE(FLens);
end;

function TCamera3ds.Direction: TVector3Single;
begin
  result := VectorSubtract(Target, Position);
end;

function TCamera3ds.Up: TVector3Single;
var
  D: TVector3Single;
const
  StandardUp: TVector3Single = (0, 0, 1);
  StandardUpAlt: TVector3Single = (0, 1, 0);
begin
  D := Direction;

  { TODO: this is just a guesswork. We're just guessing that standard
    up is StandardUp or StandardUpAlt,
    we're just guessing that Bank works this way.
    Results on test models seem sensible, consistent with view3ds. }

  if VectorsParallel(D, StandardUp) then
    result := StandardUpAlt else
    result := StandardUp;
  result := RotatePointAroundAxisDeg(Bank, result, D);
end;

{ TLights3ds --------------------------------------------------------------- }

constructor TLight3ds.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; const ChunkEndPos: Int64);
begin
  inherited;

  { init defaults }
  Enabled := true; { TODO: we could read this from 3ds file }

  { read inside CHUNK_LIGHT }
  Stream.ReadLE(Pos);
  TryReadColorInSubchunks(Col, Stream, ChunkEndPos);
end;

{ TScene3DS ----------------------------------------------------------------- }

constructor TScene3DS.Create(Stream: TStream);
var
  hmain, hsubmain, hsubObjMesh: TChunkHeader;
  hsubmainEnd, hsubObjMeshEnd: Int64;
  Object3DS: TObject3DS;
begin
  inherited Create;

  Trimeshes := TTrimesh3dsList.Create;
  Cameras := TCamera3dsList.Create;
  Lights := TLight3dsList.Create;
  Materials := TMaterial3dsList.Create;

  Stream.ReadChunkHeader(hmain);
  Check3dsFile(hmain.id = CHUNK_MAIN, 'First chunk id <> CHUNK_MAIN');

  while Stream.Position < hmain.len do
  begin
    Stream.ReadChunkHeader(hsubmain);
    hsubmainEnd := Stream.Position+hsubmain.len-SizeOf(TChunkHeader);
    case hsubmain.id of
      CHUNK_OBJMESH:
        begin
          { look for chunks OBJBLOCK and MATERIAL }
          while Stream.Position < hsubmainEnd do
          begin
            Stream.ReadChunkHeader(hsubObjMesh);
            hsubObjMeshEnd := Stream.Position + hsubObjMesh.len - SizeOf(TChunkHeader);
            case hsubObjMesh.id of
              CHUNK_OBJBLOCK:
                begin
                  Object3DS := CreateObject3DS(Self, Stream, hsubObjMeshEnd);
                  if Object3DS is TTrimesh3ds then
                    Trimeshes.Add(TTrimesh3ds(Object3DS)) else
                  if Object3DS is TCamera3ds then
                    Cameras.Add(TCamera3ds(Object3DS)) else
                    Lights.Add(Object3DS as TLight3ds);
                end;
              CHUNK_MATERIAL: Materials.ReadMaterial(Stream, hsubObjMeshEnd);
              else Stream.Position := hsubObjMeshEnd;
            end;
          end;
        end;
      CHUNK_VERSION: Stream.ReadLE(Version);
      else Stream.Position := hsubmainEnd;
    end;
  end;

  Materials.CheckAllInitialized;
end;

constructor TScene3DS.Create(const URL: string);
var
  S: TStream;
begin
  S := Download(URL, [soForceMemoryStream]);
  try Create(S);
  finally S.Free end;
end;

destructor TScene3DS.Destroy;
begin
  FreeAndNil(Trimeshes);
  FreeAndNil(Cameras);
  FreeAndNil(Lights);
  FreeAndNil(Materials);
  inherited;
end;

{ Load3DS -------------------------------------------------------------------- }

function Load3DS(const URL: string): TX3DRootNode;
var
  BaseUrl: string;
  O3ds: TScene3DS;

  { Prefix names with things like "Material_", to make sure these
    names not collide with each other. (I don't know if they are in
    separate namespace in 3DS, but better be safe.)

    Note that we don't check here whether all names are really unique,
    as they don't have to be unique for VRML/X3D. We just make reasonable
    effort to keep them unique (to help storing them with DEF/USE),
    if they were unique in 3DS. }

  function MaterialVRMLName(const Mat3dsName: string): string;
  begin Result := 'Material_' + ToX3DName(Mat3dsName) end;

  function TrimeshVRMLName(const Tri3dsName: string): string;
  begin Result := 'Trimesh_' + ToX3DName(Tri3dsName) end;

  function ViewpointVRMLName(const Camera3dsName: string): string;
  begin Result := 'Camera_' + ToX3DName(Camera3dsName) end;

  function LightVRMLName(const Light3dsName: string): string;
  begin Result := 'Light_' + ToX3DName(Light3dsName) end;

  procedure AddViewpoints;
  var
    Viewpoint: TX3DNode;
    I: Integer;
  begin
    for I := 0 to O3ds.Cameras.Count - 1 do
    begin
      Viewpoint := MakeCameraNode(cvVrml2_X3d, BaseUrl,
        O3ds.Cameras[I].Position,
        O3ds.Cameras[I].Direction,
        O3ds.Cameras[I].Up,
        O3ds.Cameras[I].Up { GravityUp equals Up });
      Viewpoint.X3DName := ViewpointVRMLName(O3ds.Cameras[I].Name);
      Result.FdChildren.Add(Viewpoint);

      { TODO: use other 3ds camera fields }
    end;
  end;

  procedure AddLights;
  var
    I: Integer;
    Light: TPointLightNode;
  begin
    for I := 0 to O3ds.Lights.Count - 1 do
    begin
      Light := TPointLightNode.Create(LightVRMLName(
        O3ds.Lights[I].Name), BaseUrl);
      Result.FdChildren.Add(Light);

      Light.FdOn.Value := O3ds.Lights[I].Enabled;
      Light.FdLocation.Value := O3ds.Lights[I].Pos;
      Light.FdColor.Value := O3ds.Lights[I].Col;
    end;
  end;

  function MaterialToVRML(Material: TMaterial3ds): TAppearanceNode;
  var
    Mat: TMaterialNode;
    Tex: TImageTextureNode;
    TexTransform: TTextureTransformNode;
  begin
    Result := TAppearanceNode.Create(MaterialVRMLName(Material.Name), BaseUrl);

    Mat := TMaterialNode.Create('', BaseUrl);
    Mat.FdDiffuseColor.Value := Vector3SingleCut(Material.DiffuseColor);
    Mat.FdAmbientIntensity.Value := AmbientIntensity(Material.AmbientColor, Material.DiffuseColor);
    Mat.FdSpecularColor.Value := Vector3SingleCut(Material.SpecularColor);
    Mat.FdShininess.Value := Material.Shininess;
    Mat.FdTransparency.Value := Material.Transparency;
    Result.FdMaterial.Value := Mat;

    if Material.TextureMap1.Exists then
    begin
      Tex := TImageTextureNode.Create('', BaseUrl);
      Tex.FdUrl.Items.Add(SearchTextureFile(BaseUrl,
        Material.TextureMap1.MapURL));
      Result.FdTexture.Value := Tex;

      TexTransform := TTextureTransformNode.Create('', BaseUrl);
      TexTransform.FdScale.Value := Material.TextureMap1.Scale;
      Result.FdTextureTransform.Value := TexTransform;

      if Material.TextureMapBump.Exists then
      begin
        Tex := TImageTextureNode.Create('', BaseUrl);
        Tex.FdUrl.Items.Add(SearchTextureFile(BaseUrl,
          Material.TextureMapBump.MapURL));
        Result.FdNormalMap.Value := Tex;

        { We don't have separate TextureTransform for bump map.
          Just check that in 3DS bump map and diffuse textures have equal transform. }
        if not VectorsEqual(
            Material.TextureMap1.Scale,
            Material.TextureMapBump.Scale) then
          WritelnWarning('VRML/X3D', 'Texture scale for diffuse and normal (bump) maps is different in the 3DS file. Currently this is not correctly handled when converting to VRML/X3D');
      end;
    end;
  end;

  { How many faces have the same material index.
    Starts, and compares, with the face numnbered StartFace (must be < FacesCount). }
  function SameMaterialFacesCount(Faces: PArray_Face3ds; FacesCount: integer;
    StartFace: integer): integer;
  var
    I: Integer;
  begin
    I := StartFace + 1;
    while (I < FacesCount) and
          (Faces^[I].FaceMaterialIndex = Faces^[StartFace].FaceMaterialIndex) do
      Inc(i);
    Result := I - StartFace;
  end;

var
  Trimesh3ds: TTrimesh3ds;
  Appearances: TX3DNodeList;
  Coord: TCoordinateNode;
  IFS: TIndexedFaceSetNode;
  TexCoord: TTextureCoordinateNode;
  Shape: TShapeNode;
  I, J, FaceMaterialNum, ThisMaterialFacesCount, FaceNum: Integer;
begin
  BaseUrl := AbsoluteURI(URL);
  Appearances := nil;
  O3ds := TScene3DS.Create(URL);
  try
    Result := TX3DRootNode.Create('', BaseUrl);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := X3DVersion;

      AddViewpoints;
      AddLights;

      { Convert every 3DS material into VRML/X3D Appearance node }
      Appearances := TX3DNodeList.Create(false);
      Appearances.Count := O3ds.Materials.Count;
      for i := 0 to O3ds.Materials.Count - 1 do
        Appearances[I] := MaterialToVRML(O3ds.Materials[i]);

      { Add 3DS triangle meshes. Each trimesh is split into a number of
        VRML/X3D IndexedFaceSet nodes, sharing common Coordinate node,
        but having different materials. }
      for i := 0 to O3ds.Trimeshes.Count-1 do
      begin
        Trimesh3ds := O3ds.Trimeshes[I];

        { Create Coordinate node }
        Coord := TCoordinateNode.Create('Coord_' + TrimeshVRMLName(Trimesh3ds.Name), BaseUrl);
        Coord.FdPoint.Count := Trimesh3ds.VertsCount;
        for J := 0 to Trimesh3ds.VertsCount-1 do
          Coord.FdPoint.Items.L[J] := Trimesh3ds.Verts^[J].Pos;

        { Create TextureCoordinate node, or nil if not available }
        if Trimesh3ds.HasTexCoords then
        begin
          TexCoord := TTextureCoordinateNode.Create('TexCoord_' + TrimeshVRMLName(Trimesh3ds.Name), BaseUrl);
          TexCoord.FdPoint.Count := Trimesh3ds.VertsCount;
          for j := 0 to Trimesh3ds.VertsCount - 1 do
            TexCoord.FdPoint.Items.L[J] := Trimesh3ds.Verts^[J].TexCoord;
        end else
          TexCoord := nil;

        { Convert faces with equal material to IndexedFaceSet }
        J := 0;
        while J < Trimesh3ds.FacesCount do
        begin
          IFS := TIndexedFaceSetNode.Create('', BaseUrl);
          IFS.FdTexCoord.Value := TexCoord;
          IFS.FdCoord.Value := Coord;
          { We don't support 3DS smoothing groups.
            So instead assign some sensible non-zero crease angle. }
          IFS.FdCreaseAngle.Value := NiceCreaseAngle;
          IFS.FdSolid.Value := false;

          Shape := TShapeNode.Create('', BaseUrl);
          Shape.FdGeometry.Value := IFS;

          FaceMaterialNum := Trimesh3ds.Faces^[j].FaceMaterialIndex;
          if FaceMaterialNum <> -1 then
            Shape.Appearance := TAppearanceNode(Appearances[FaceMaterialNum]);

          Result.FdChildren.Add(Shape);

          ThisMaterialFacesCount := SameMaterialFacesCount(Trimesh3ds.Faces,
            Trimesh3ds.FacesCount, J);

          IFS.FdCoordIndex.Count := ThisMaterialFacesCount * 4;
          for FaceNum := 0 to ThisMaterialFacesCount - 1 do
          begin
            with IFS.FdCoordIndex.Items do
            begin
              L[FaceNum * 4    ] := Trimesh3ds.Faces^[J].VertsIndices[0];
              L[FaceNum * 4 + 1] := Trimesh3ds.Faces^[J].VertsIndices[1];
              L[FaceNum * 4 + 2] := Trimesh3ds.Faces^[J].VertsIndices[2];
              L[FaceNum * 4 + 3] := -1;
            end;
            Inc(J);
          end;

          { If Trimesh3ds.HasTexCoords then IFS.texCoordIndex should be taken
            from IFS.coordIndex. And VRML 2.0/X3D do this by default, so no need
            to do anything. }
        end;

        Coord.FreeIfUnused;
        Coord := nil;
        if TexCoord <> nil then
        begin
          TexCoord.FreeIfUnused;
          TexCoord := nil;
        end;
      end;

      for I := 0 to Appearances.Count - 1 do
        Appearances[I].FreeIfUnused;
    except FreeAndNil(result); raise end;
  finally
    FreeAndNil(O3ds);
    FreeAndNil(Appearances)
  end;
end;

end.
