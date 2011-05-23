{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3DS loader (TScene3DS). }

unit Object3DS;

{ TODO
  - properties that are read from 3ds but not used anywhere (not in
    Object3DOpenGL nor in Object3DAsVRML) because their exact
    interpretation is not known for me:
      TCamera3ds.Lens
      TMaterialMap3ds.Offset (I don't know wheteher use it before of after
        Scale, again I need some test 3ds scenes to check that)
      TMaterial3ds.TextureMap2
      TMaterial3ds.ShininessStrenth, TransparencyFalloff, ReflectBlur

  - TFace3ds.Wrap interpretation is known but it is not used by Object3DOpenGL
    nor Object3DAsVRML (because
    1. implementing it requires some mess in code
       since this Wrap is the property of _each face_ (instead of _each texture_,
       which would be simpler to map to OpenGL and VRML)
    2. and I don't have enough test 3ds scenes with textures to really
       see the difference. So, I will probably implement it someday when
       I'll need it.
    )
}

interface

uses KambiUtils, Classes, KambiClassUtils, SysUtils, Boxes3D, VectorMath;

{$define read_interface}

type
  EInvalid3dsFile = class(Exception);
  EMaterialNotInitialized = class(EInvalid3dsFile);

  TMaterialMap3ds = record
    Exists: boolean;
    MapFilename: string;
    Scale, Offset: TVector2Single;
  end;

  TMaterial3ds = class
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

  TObjectsListItem_4 = TMaterial3ds;
  {$I objectslist_4.inc}
  TMaterial3dsList = class(TObjectsList_4)
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
  private
    FName: string;
    FScene: TScene3DS;
  public
    property Name: string read FName;
    { Scene containing this object. }
    property Scene: Tscene3DS read FScene;
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; ObjectEndPos: Int64); virtual;
  end;

  TObject3DSClass = class of TObject3DS;

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
  private
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

    { Constructor reading OBJBLOCK 3DS chunk into triangle mesh.
      Assumes that we just read from stream chunk header (with id =
      CHUNK_OBJBLOCK and length indicating thatStream.Position >= ObjectEndPos
      is after the object), then we read AName (so now we can read subchunks). }
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; ObjectEndPos: Int64); override;
    destructor Destroy; override;
  end;

  TCamera3ds = class(TObject3DS)
  private
    FPosition, FTarget: TVector3Single;
    FBank, FLens: Single;
  public
    property Position: TVector3Single read FPosition;
    property Target: TVector3Single read FTarget;
    property Bank: Single read FBank;
    property Lens: Single read FLens;
    constructor Create(const AName: string; AScene: TScene3DS;
      Stream: TStream; ObjectEndPos: Int64); override;

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
      Stream: TStream; ObjectEndPos: Int64); override;
  end;

  TObjectsListItem_1 = TTrimesh3ds;
  {$I objectslist_1.inc}
  TTrimesh3dsList = TObjectsList_1;
  TObjectsListItem_2 = TCamera3ds;
  {$I objectslist_2.inc}
  TCamera3dsList = TObjectsList_2;
  TObjectsListItem_3 = TLight3ds;
  {$I objectslist_3.inc}
  TLight3dsList = TObjectsList_3;

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
    constructor Create(const filename: string); overload;
    destructor Destroy; override;

    function SumTrimeshesVertsCount: Cardinal;
    function SumTrimeshesFacesCount: Cardinal;
  end;

{$undef read_interface}

implementation

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
  to the MATERIAL chunk. }

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}
{$I objectslist_4.inc}

function CreateObject3DS(AScene: Tscene3DS; Stream: TStream;
  ObjectEndPos: Int64): TObject3DS; forward;

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
begin
  result := false;
  while Stream.Position < EndPos do
  begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
    { TODO: we ignore gamma correction entirely so we don't distinct
      gamma corrected and not corrected colors }
    case h.id of
      CHUNK_RGBF, CHUNK_RGBF_GAMMA:
        begin
          Stream.ReadBuffer(Col, SizeOf(Col));
          result := true;
          break;
        end;
      CHUNK_RGBB, CHUNK_RGBB_GAMMA:
        begin
          Stream.ReadBuffer(Col3Byte, SizeOf(Col3Byte));
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
type
  T3dsDoubleByte = SmallInt;
var
  h: TChunkHeader;
  hEnd: Int64;
  DoubleByte: T3dsDoubleByte;
begin
  result := false;
  while Stream.Position < EndPos do
  begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd := Stream.Position -SizeOf(TChunkHeader) + h.len;
    if h.id = CHUNK_DOUBLE_BYTE then
    begin
      Stream.ReadBuffer(DoubleByte, SizeOf(DoubleByte));
      result := true;
      break;
    end else
      Stream.Position := hEnd;
  end;
  Stream.Position := EndPos;
  if result then Value := DoubleByte/100;
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
    (Exists: true; MapFileName: ''; Scale: (1, 1); Offset: (0, 0));
  var
    h: TChunkHeader;
    hEnd: Int64;
  begin
    result := InitialExistingMatMap;

    { read MAP subchunks }
    while Stream.Position < EndPos do
    begin
      Stream.ReadBuffer(h, SizeOf(h));
      hEnd := Stream.Position -SizeOf(TChunkHeader) +h.len;
      case h.id of
        CHUNK_MAP_FILE: Result.MapFilename := StreamReadZeroEndString(Stream);
        CHUNK_MAP_USCALE: Stream.ReadBuffer(Result.Scale[0], SizeOf(Single));
        CHUNK_MAP_VSCALE: Stream.ReadBuffer(Result.Scale[1], SizeOf(Single));
        CHUNK_MAP_UOFFSET: Stream.ReadBuffer(Result.Offset[0], SizeOf(Single));
        CHUNK_MAP_VOFFSET: Stream.ReadBuffer(Result.Offset[1], SizeOf(Single));
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
    Stream.ReadBuffer(h, SizeOf(h));
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
    Stream.ReadBuffer(h, SizeOf(h));
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
  Stream: TStream; ObjectEndPos: Int64);
{ don't ever call directly this constructor - we can get here
  only by  "inherited" call from Descendant's constructor }
begin
  inherited Create;
  FName := AName;
  FScene := AScene;
end;

function CreateObject3DS(AScene: TScene3DS; Stream: TStream; ObjectEndPos: Int64): TObject3DS;
var
  ObjClass: TObject3DSClass;
  ObjName: string;
  ObjBeginPos: Int64;
  h: TChunkHeader;
begin
  ObjClass := nil;
  ObjName := StreamReadZeroEndString(Stream);
  ObjBeginPos := Stream.Position;

  {seek all chunks searching for chunk TRIMESH / CAMERA / LIGHT}
  while Stream.Position < ObjectEndPos do
  begin
    Stream.ReadBuffer(h, SizeOf(h));
    case h.id of
      CHUNK_TRIMESH: ObjClass := TTrimesh3ds;
      CHUNK_CAMERA: ObjClass := TCamera3ds;
      CHUNK_LIGHT: ObjClass := TLight3ds;
    end;
    if ObjClass <> nil then break;
    Stream.Position := Stream.Position + h.len - SizeOf(TChunkHeader);
  end;

  {if none of the TRIMESH / CAMERA / LIGHT chunks found raise error}
  Check3dsFile( ObjClass <> nil, 'No object recorded under the name '+ObjName);

  {restore stream pos, create object using appropriate class}
  Stream.Position := ObjBeginPos;
  result := ObjClass.Create(ObjName, AScene, Stream, ObjectEndPos);
end;

{ TTrimesh3ds --------------------------------------------------------------- }

constructor TTrimesh3ds.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; ObjectEndPos: Int64);

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
      Stream.ReadBuffer(FVertsCount, SizeOf(FVertsCount));
      { Use GetClearMem to have Verts memory safely initialized to 0 }
      Verts := GetClearMem(SizeOf(TVertex3ds)*VertsCount);
    end else
    begin
      Stream.ReadBuffer(VertsCountCheck, SizeOf(VertsCountCheck));
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
      Stream.ReadBuffer(Verts^[i].Pos, SizeOf(Verts^[i].Pos));
  end;

  procedure ReadMaplist(chunkEnd: Int64);
  var
    i: integer;
  begin
    FHasTexCoords := true;
    ReadVertsCount;
    for i := 0 to VertsCount-1 do
      Stream.ReadBuffer(Verts^[i].TexCoord, SizeOf(Verts^[i].TexCoord));
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
      Stream.ReadBuffer(MatFaceCount, SizeOf(MatFaceCount));
      for i := 1 to MatFaceCount do
      begin
        Stream.ReadBuffer(FaceNum, SizeOf(FaceNum));
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
    Stream.ReadBuffer(FFacesCount, SizeOf(FFacesCount));
    Faces := GetMem(SizeOf(TFace3ds)*FacesCount);
    for i := 0 to FacesCount-1 do
    with Faces^[i] do
    begin
      { init face }
      Stream.ReadBuffer(Word3, SizeOf(Word3));
      for j := 0 to 2 do
        VertsIndices[j] := Word3[j];
      Stream.ReadBuffer(Flags, SizeOf(Flags));
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
      Stream.ReadBuffer(h, SizeOf(h));
      hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
      if h.id = CHUNK_FACEMAT then
        ReadFacemat else
        Stream.Position := hEnd;
    end;
  end;

var
  h, htrimesh: TChunkHeader;
  trimeshEnd, hEnd: Int64;
begin
  inherited;

  { init properties }
  FHasTexCoords := false;

  { look for chunk TRIMESH }
  while Stream.Position < ObjectEndPos do
  begin
    Stream.ReadBuffer(htrimesh, SizeOf(htrimesh));
    trimeshEnd := Stream.Position + htrimesh.len - SizeOf(TChunkHeader);
    if htrimesh.id = CHUNK_TRIMESH then
    begin
      { look for chunks VERTLIST, FACELIST, TRMATRIX, MAPLIST }
      while Stream.Position < trimeshEnd do
      begin
        Stream.ReadBuffer(h, SizeOf(h));
        hend := Stream.Position + h.len - SizeOf(TChunkHeader);
        case h.id of
          CHUNK_VERTLIST: ReadVertlist;
          CHUNK_FACELIST: ReadFacelist(hEnd);
          CHUNK_MAPLIST: ReadMaplist(hEnd);
          else Stream.Position := hEnd;
        end;
      end;

      break; { tylko jeden TRIMESH moze byc w jednym OBJBLOCK }
    end else
      Stream.Position := trimeshEnd;
  end;

  Stream.Position := ObjectEndPos;
end;

destructor TTrimesh3ds.Destroy;
begin
  FreeMemNiling(Verts);
  FreeMemNiling(Faces);
  inherited;
end;

{ TCamera3ds --------------------------------------------------------------- }

constructor TCamera3ds.Create(const AName: string; AScene: TScene3DS;
  Stream: TStream; ObjectEndPos: Int64);
var
  h: TChunkHeader;
  hEnd: Int64;
begin
  inherited;

  { look for chunk CAMERA }
  while Stream.Position < ObjectEndPos do
  begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
    if h.id = CHUNK_CAMERA then
    begin
      { read camera chunk }
      Stream.ReadBuffer(FPosition, SizeOf(FPosition));
      Stream.ReadBuffer(FTarget, SizeOf(FTarget));
      Stream.ReadBuffer(FBank, SizeOf(FBank));
      Stream.ReadBuffer(FLens, SizeOf(FLens));

      Stream.Position := hEnd; { skip CHUNK_CAMERA subchunks }
      break; { only one chunk CAMERA allowed in one OBJBLOCK }
    end else
      Stream.Position := hEnd;
  end;

  Stream.Position := ObjectEndPos;
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
  Stream: TStream; ObjectEndPos: Int64);
var
  h: TChunkHeader;
  hEnd: Int64;
begin
  inherited;

  { init defaults }
  Enabled := true; { TODO: we could read this from 3ds file }

  { look for chunk LIGHT }
  while Stream.Position < ObjectEndPos do
  begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
    if h.id = CHUNK_LIGHT then
    begin
      { read LIGHT chunk }
      Stream.ReadBuffer(Pos, SizeOf(Pos));
      TryReadColorInSubchunks(Col, Stream, ObjectEndPos);

      break; { only one LIGHT allowed in one OBJBLOCK }
    end else
      Stream.Position := hEnd;
  end;

  Stream.Position := ObjectEndPos;
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

  Stream.ReadBuffer(hmain, SizeOf(hmain));
  Check3dsFile(hmain.id = CHUNK_MAIN, 'First chunk id <> CHUNK_MAIN');

  while Stream.Position < hmain.len do
  begin
    Stream.ReadBuffer(hsubmain, SizeOf(hsubmain));
    hsubmainEnd := Stream.Position+hsubmain.len-SizeOf(TChunkHeader);
    case hsubmain.id of
      CHUNK_OBJMESH:
        begin
          { look for chunks OBJBLOCK and MATERIAL }
          while Stream.Position < hsubmainEnd do
          begin
            Stream.ReadBuffer(hsubObjMesh, SizeOf(hsubObjMesh));
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
      CHUNK_VERSION: Stream.ReadBuffer(Version, SizeOf(Version));
      else Stream.Position := hsubmainEnd;
    end;
  end;

  Materials.CheckAllInitialized;
end;

constructor TScene3DS.Create(const filename: string);
var
  S: TStream;
begin
  S := CreateReadFileStream(filename);
  try Create(S);
  finally S.Free end;
end;

destructor TScene3DS.Destroy;
begin
  FreeWithContentsAndNil(Trimeshes);
  FreeWithContentsAndNil(Cameras);
  FreeWithContentsAndNil(Lights);
  FreeWithContentsAndNil(Materials);
  inherited;
end;

function TScene3DS.SumTrimeshesVertsCount: Cardinal;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Trimeshes.Count-1 do result += Trimeshes[i].VertsCount;
end;

function TScene3DS.SumTrimeshesFacesCount: Cardinal;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Trimeshes.Count-1 do result += Trimeshes[i].FacesCount;
end;

end.
