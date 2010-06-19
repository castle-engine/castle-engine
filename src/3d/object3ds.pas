{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Read 3D object from a 3DS file.) }

unit Object3Ds;

{TODO
- properties that are read from 3ds but not used anywhere (not in
  Object3DOpenGL nor in Object3DAsVRML) because their exact
  interpretation is not known for me:
    TCamera3ds.CamLens
    TMaterialMap3ds.U/VOffset (I don't know wheteher use it before of after
      U/VScale, again I need some test 3ds scenes to check that)
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

uses KambiUtils, Classes, KambiClassUtils, SysUtils, Object3DsMaterial,
  Object3DsChunks, Boxes3D, VectorMath, Base3D;

{$define read_interface}

type
  { }
  TScene3ds = class;

  { @abstract(This is an abstract class that wraps OBJBLOCK chunk of 3DS file:
    trimesh, camera or light source.)

    Jej nieabstrakcyjni potomkowie to TCamera3ds, TLight3ds i najwazniejszy
    TTrimesh3ds. Uzywaj funkcji CreateObject3Ds aby odczytac ze strumienia
    chunk OBJBLOCK i rozpoznac automatycznie jaki z trzech wymienionych rodzai
    on opisuje i stworzyc egzemplarz odpowiedniej nieabstrakcyjnej klasy.
    Nigdy nie konstruuj obiektow dokladnie tej klasy !

    O ile dobrze rozumiem 3dsy OBJBLOCK to ALBO jedno Trimesh, albo jedno light,
    albo jedna Camera. Kod TObjects3ds i hierarchia wszystkich podklas
    sa oparte na tym zalozeniu : jeden chunk OBJBLOCK daje jeden
    z tych trzech obiektow.

    Kamery i swiatla beda mialy EmptyBoundingBox (podobnie jak Trimeshe
    bez zadnych vertexow; moga takie byc). }
  TObject3Ds = class
  private
    FName: string;
    FScene: TScene3ds;
  public
    property Name: string read FName;
    { Scena do ktorej nalezy ten obiekt; chwilowo uzywane tylko w
      trimeshach do wiazania sie z odpowiednimi materialami }
    property Scene: Tscene3ds read FScene;
    constructor Create(const AName: string; AScene: TScene3ds;
      Stream: TStream; ObjectEndPos: Int64); virtual;
  end;

  TObject3DsClass = class of TObject3Ds;

  TFace3ds = record
    VertsIndices: TVector3Cardinal;
    EdgeFlags: packed array[0..2]of boolean;
    Wrap: packed array[0..1]of boolean;
    { index in Scene.Materials.
      -1 means "not specified in 3ds file" and implicated that face
      uses default material. There ARE some 3ds that have faces without
      material defined - for example therack.3ds. }
    FaceMaterialIndex: Integer;
    {ten rekord moze byc rozszerzany aby objac wiecej wlasciwosci Face z 3ds}
  end;
  TArray_Face3ds = packed array[0..MaxInt div SizeOf(TFace3ds)-1]of TFace3ds;
  PArray_Face3ds = ^TArray_Face3ds;

  { TVertex3ds reprezentuje cala informacje o vertexach jaka zgromadzilismy
    czytajac chunk danego trimesha. }
  TVertex3ds = packed record
    Pos: TVector3Single;
    { jesli obiekt not HasTexCoords to wszystkie TexCoord = (0, 0) }
    TexCoord: TVector2Single;
    {ten rekord moze byc rozszerzany aby objac wiecej wlasciwosci Vertexa z 3ds}
  end;
  TArray_Vertex3ds = packed array[0..MaxInt div SizeOf(TVertex3ds)-1]of TVertex3ds;
  PArray_Vertex3ds = ^TArray_Vertex3ds;

  { @abstract(This class wraps OBJBLOCK chunk of 3DS file with VERTLIST subchunk.
    Putting it simpler, this represents a set of triangles.) }
  TTrimesh3ds = class(TObject3Ds)
  private
    FVertsCount, FFacesCount: Word;
    FHasTexCoords: boolean;
    FBoundingBox: TBox3D;
  public
    { Verts i Faces read-only from outside }
    Verts: PArray_Vertex3ds;
    Faces: PArray_Face3ds;
    { czy Verts mialy zapisane w pliku TexCoords ? }
    property HasTexCoords: boolean read FHasTexCoords;
    { pamietaj ze przypadek VertsCount = FacesCount = 0 jest mozliwy,
      np. 0155.3ds. }
    property VertsCount: Word read FVertsCount;
    property FacesCount: Word read FFacesCount;

    { constructor TTrimesh3ds ma za zadanie odczytac caly chunk
      CHUNK_OBJBLOCK. Zaklada on ze przed chwila ze strumienia
      odczytano chunk header o id = CHUNK_OBJBLOCK i jakims len
      z ktorego wynika ze gdy Stream.Position >= ObjectEndPos
      to juz jestesmy za tym obiekte,.
      Potem odczytano AName. (czyli teraz mozna przystapic
      do odczytywania subchunkow). }
    constructor Create(const AName: string; AScene: TScene3ds;
      Stream: TStream; ObjectEndPos: Int64); override;
    destructor Destroy; override;

    property BoundingBox: TBox3D read FBoundingBox;
  end;

  TCamera3ds = class(TObject3Ds)
  private
    FCamPos, FCamTarget: TVector3Single;
    FCamBank, FCamLens: Single;
  public
    property CamPos: TVector3Single read FCamPos;
    property CamTarget: TVector3Single read FCamTarget;
    property CamBank: Single read FCamBank;
    property CamLens: Single read FCamLens;
    constructor Create(const AName: string; AScene: TScene3ds;
      Stream: TStream; ObjectEndPos: Int64); override;
    destructor Destroy; override;

    { Zwroc transformacje ktorej zaaplikowanie jest rownoznaczne z
      uwzglednieniem tej kamery. Innymi slowy, w OpenGLu zamiast gluLookAt(...)
      wywoluj glMultMatrix(Camera.Matrix) (a czasami glLoadMatrix wystarczy). }
    function Matrix: TMatrix4Single;

    { liczone na podstawie CamPos i CamTarget }
    function CamDir: TVector3Single;
    { liczone na podstawie CamDir i CamBank }
    function CamUp: TVector3Single;
  end;

  TLight3ds = class(TObject3Ds)
  public
    Pos: TVector3Single;
    Col: TVector3Single;
    Enabled: boolean;
    constructor Create(const AName: string; AScene: TScene3ds;
      Stream: TStream; ObjectEndPos: Int64); override;
    destructor Destroy; override;
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

  TScene3ds = class(T3D)
  private
    FBoundingBox: TBox3D;
  public
    {Trimeshes objects on this list are created and destroyed by TScene3ds
     object - so Trimeshes property is read-only from outside of this class !
     Same goes with Lights, Cameras and Materials.}
    Trimeshes: TTrimesh3dsList;
    Cameras: TCamera3dsList;
    Lights: TLight3dsList;
    Materials: TMaterial3dsList;
    {Wersja Autodeska uzyta do stworzenia tego 3ds'a}
    Version: LongWord;
    constructor Create(Stream: TStream); reintroduce; overload;
    constructor Create(const filename: string); reintroduce; overload;
    destructor Destroy; override;

    function SumTrimeshesVertsCount: Cardinal;
    function SumTrimeshesFacesCount: Cardinal;

    procedure WritelnSceneInfo; {output some info about 3ds file}

    { jezeli istanieje kamera o numerze CamNumber to zwraca jej Matrix,
      wpp. zwraca IdentityMatrix. }
    function TryCameraMatrix(CamNumber: integer): TMatrix4Single;

    function BoundingBox: TBox3D; override;
  end;

function CreateObject3Ds(AScene: Tscene3ds; Stream: TStream;
  ObjectEndPos: Int64): TObject3Ds;

{$undef read_interface}

implementation

{ 3DS reading mostly based on spec from
  [http://www.martinreddy.net/gfx/3d/3DS.spec].

  TScene3ds corresponds to the whole 3DS file,
  that is the MAIN chunk, and also (since we don't handle keyframes from 3DS)
  the OBJMESH chunk.

  It contains a lists trimeshes, cameras and lights. They are all TObject3Ds,
  and correspond to OBJBLOCK chunk, with inside TRIMESH, CAMERA or LIGHT chunk.

  Moreover TScene3ds has a list of TMaterial3ds, that correspond
  to the MATERIAL chunk. }

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}

const
  { 3ds format consts : }

  { czy jest edge flag pomiedzy i a (i+1) mod 3 vertexem ?
    Odpowiedz := (FACEFLAG_EDGES[i] and Face.flags) <> 0 }
  FACEFLAG_EDGES: array[0..2]of Word = ($4, $2, $1);
  { WrapU i WrapV - ta sa (chyba) wskazowki czy tekstura na scianie
    ma byc clamped czy repeated. Chwilowo unused. }
  FACEFLAG_WRAP: array[0..1]of Word = ($8, $10);

{ TObject3Ds ----------------------------------------------------------------- }

constructor TObject3Ds.Create(const AName: string; AScene: TScene3ds;
  Stream: TStream; ObjectEndPos: Int64);
{ don't ever call directly this constructor - we can get here
  only by  "inherited" call from Descendant's constructor }
begin
 inherited Create;
 FName := AName;
 FScene := AScene;
end;

function CreateObject3Ds(AScene: TScene3ds; Stream: TStream; ObjectEndPos: Int64): TObject3Ds;
var ObjClass: TObject3DsClass;
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

constructor TTrimesh3ds.Create(const AName: string; AScene: TScene3ds;
  Stream: TStream; ObjectEndPos: Int64);

  procedure ReadVertsCount;
  {odczytaj ze strumienia dwubajtowo zapisane FVertsCount i zainicjuj
   FVertsCount i Verts obiektu, ew. tylko sprawdz czy nowa informacja
   zgadza sie z tym co juz mamy (bo FVertsCount trimesha moze byc podane
   w paru miejscach, m.in. w chunku VERTLIST i MAPLIST) }
  var VertsCountCheck: Word;
  begin
   if VertsCount = 0 then
   begin
    Stream.ReadBuffer(FVertsCount, SizeOf(FVertsCount));
    { ladujemy przez GetClearMem zeby ustawic wszystko czego tu nie
      zainicjujemy na 0 }
    Verts := GetClearMem(SizeOf(TVertex3ds)*VertsCount);
   end else
   begin
    Stream.ReadBuffer(VertsCountCheck, SizeOf(VertsCountCheck));
    Check3dsFile( VertsCountCheck = VertsCount,
      'Different VertexCount info for 3ds object '+Name);
   end;
  end;

  procedure ReadVertlist;
  var i: integer;
  begin
   ReadVertsCount;
   for i := 0 to VertsCount-1 do
    Stream.ReadBuffer(Verts^[i].Pos, SizeOf(Verts^[i].Pos));
  end;

  procedure ReadMaplist(chunkEnd: Int64);
  var i: integer;
  begin
   FHasTexCoords := true;
   ReadVertsCount;
   for i := 0 to VertsCount-1 do
    Stream.ReadBuffer(Verts^[i].TexCoord, SizeOf(Verts^[i].TexCoord));
   Stream.Position := chunkEnd; { skip subchunks }
  end;

  procedure ReadFacelist(chunkEnd: Int64);

    procedure ReadFacemat;
    var MatName: string;
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

  var i, j: integer;
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
    {init face}
    Stream.ReadBuffer(Word3, SizeOf(Word3));
    for j := 0 to 2 do
     VertsIndices[j] := Word3[j];
    Stream.ReadBuffer(Flags, SizeOf(Flags));
    {rozkoduj pole Flags}
    for j := 0 to 2 do
     EdgeFlags[j]:=(FACEFLAG_EDGES[j] and Flags) <> 0;
    for j := 0 to 1 do
     Wrap[j]:=(FACEFLAG_WRAP[j] and Flags) <> 0;
    FaceMaterialIndex := -1;
   end;

   {read subchunks - look for FACEMAT chunk}
   while Stream.Position < chunkEnd do
   begin
    Stream.ReadBuffer(h, SizeOf(h));
    hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
    if h.id = CHUNK_FACEMAT then
     ReadFacemat else
     Stream.Position := hEnd;
   end;
  end;

var h, htrimesh: TChunkHeader;
    trimeshEnd, hEnd: Int64;
begin
 inherited;

 {init properties}
 FHasTexCoords := false;

 {szukamy chunka TRIMESH}
 while Stream.Position < ObjectEndPos do
 begin
  Stream.ReadBuffer(htrimesh, SizeOf(htrimesh));
  trimeshEnd := Stream.Position + htrimesh.len - SizeOf(TChunkHeader);
  if htrimesh.id = CHUNK_TRIMESH then
  begin

   {szukaj chunkow VERTLIST, FACELIST, TRMATRIX, MAPLIST}
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

 fBoundingBox := CalculateBoundingBox(
   @Verts^[0].Pos, VertsCount, SizeOf(TVertex3ds));
end;

destructor TTrimesh3ds.Destroy;
begin
 FreeMemNiling(Verts);
 FreeMemNiling(Faces);
 inherited;
end;

{ TCamera3ds --------------------------------------------------------------- }

constructor TCamera3ds.Create(const AName: string; AScene: TScene3ds;
  Stream: TStream; ObjectEndPos: Int64);
var h: TChunkHeader;
    hEnd: Int64;
begin
 inherited;

 {szukamy chunka CAMERA}
 while Stream.Position < ObjectEndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
  if h.id = CHUNK_CAMERA then
  begin
   {read camera chunk}
   Stream.ReadBuffer(FCamPos, SizeOf(FCamPos));
   Stream.ReadBuffer(FCamTarget, SizeOf(FCamTarget));
   Stream.ReadBuffer(FCamBank, SizeOf(FCamBank));
   Stream.ReadBuffer(FCamLens, SizeOf(FCamLens));

   Stream.Position := hEnd; { skip CHUNK_CAMERA subchunks }
   break; { tylko jeden chunk CAMERA moze byc w jednym OBJBLOCK }
  end else
   Stream.Position := hEnd;
 end;

 Stream.Position := ObjectEndPos;
end;

destructor TCamera3ds.Destroy;
begin
 inherited;
end;

function TCamera3ds.Matrix: TMatrix4Single;
begin
 result := LookAtMatrix(CamPos, CamTarget, CamUp);
end;

function TCamera3ds.CamDir: TVector3Single;
begin
 result := VectorSubtract(CamTarget, CamPos);
end;

function TCamera3ds.CamUp: TVector3Single;
var dir: TVector3Single;
const
  Std3dsCamUp: TVector3Single = (0, 0, 1);
  Std3dsCamUp_Second: TVector3Single = (0, 1, 0);
begin
 dir := CamDir;

 { TODO: poniewaz nie mam prawdziwej specyfikacji 3dsa cala ta funkcja
   to czyste zgadywanie. Zgadywaniem jest Std3dsCamUp, zgadywaniem jest
   ze CamBank dziala tak a nie inaczej. Ale generowane kamery wygladaja
   sensownie i tak samo jak dla view3ds.
   Wartosc Std3dsCamUp_Second to juz zupelnie czysta nieprzetestowana
   fantazja. }

 if VectorsParallel(dir, Std3dsCamUp) then
  result := Std3dsCamUp_Second else
  result := Std3dsCamUp;
 result := RotatePointAroundAxisDeg(CamBank, result, dir);
end;

{ TLights3ds --------------------------------------------------------------- }

constructor TLight3ds.Create(const AName: string; AScene: TScene3ds;
  Stream: TStream; ObjectEndPos: Int64);
var h: TChunkHeader;
    hEnd: Int64;
begin
 inherited;

 {init defaults}
 Enabled := true; { TODO: we could read this from 3ds file }

 {szukamy chunka LIGHT}
 while Stream.Position < ObjectEndPos do
 begin
  Stream.ReadBuffer(h, SizeOf(h));
  hEnd := Stream.Position + h.len - SizeOf(TChunkHeader);
  if h.id = CHUNK_LIGHT then
  begin

   {read LIGHT chunk}
   Stream.ReadBuffer(Pos, SizeOf(Pos));
   TryReadColorInSubchunks(Col, Stream, ObjectEndPos);

   break; { tylko jeden LIGHT moze byc w jednym OBJBLOCK }
  end else
   Stream.Position := hEnd;
 end;

 Stream.Position := ObjectEndPos;
end;

destructor TLight3ds.Destroy;
begin
 inherited;
end;

{ TScene3ds ----------------------------------------------------------------- }

constructor TScene3ds.Create(Stream: TStream);

  procedure CalcBBox;
  var i: integer;
  begin
   {calculate bounding box as a sum of BoundingBoxes of all trimeshes}
   fBoundingBox := EmptyBox3D;
   for i := 0 to Trimeshes.Count-1 do
    Box3DSumTo1st(fBoundingBox, Trimeshes[i].BoundingBox);
  end;

var hmain, hsubmain, hsubObjMesh: TChunkHeader;
    hsubmainEnd, hsubObjMeshEnd: Int64;
    Object3Ds: TObject3Ds;
begin
 inherited Create(nil);

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
      {szukamy chunkow OBJBLOCK i MATERIAL}
      while Stream.Position < hsubmainEnd do
      begin
       Stream.ReadBuffer(hsubObjMesh, SizeOf(hsubObjMesh));
       hsubObjMeshEnd := Stream.Position + hsubObjMesh.len - SizeOf(TChunkHeader);
       case hsubObjMesh.id of
        CHUNK_OBJBLOCK:
          begin
           Object3Ds := CreateObject3Ds(Self, Stream, hsubObjMeshEnd);
           if Object3Ds is TTrimesh3ds then
            Trimeshes.Add(TTrimesh3ds(Object3Ds)) else
           if Object3Ds is TCamera3ds then
            Cameras.Add(TCamera3ds(Object3Ds)) else
            Lights.Add(Object3Ds as TLight3ds);
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

 CalcBBox;
end;

constructor TScene3ds.Create(const filename: string);
var S: TStream;
begin
 S := CreateReadFileStream(filename);
 try Create(S);
 finally S.Free end;
end;

destructor TScene3ds.Destroy;
begin
 FreeWithContentsAndNil(Trimeshes);
 FreeWithContentsAndNil(Cameras);
 FreeWithContentsAndNil(Lights);
 FreeWithContentsAndNil(Materials);
 inherited;
end;

function TScene3ds.SumTrimeshesVertsCount: Cardinal;
var i: integer;
begin
 result := 0;
 for i := 0 to Trimeshes.Count-1 do result += Trimeshes[i].VertsCount;
end;

function TScene3ds.SumTrimeshesFacesCount: Cardinal;
var i: integer;
begin
 result := 0;
 for i := 0 to Trimeshes.Count-1 do result += Trimeshes[i].FacesCount;
end;

procedure TScene3ds.WritelnSceneInfo;

  function ColToStr(const v: TVector4Single): string;
  begin result := Format('%f,%f,%f', [v[0], v[1], v[2]]) end;

var i: integer;
begin
 Writeln('3ds version : ',Version);
 for i := 0 to Cameras.Count-1 do
  Writeln('  Camera ',i, ': "',Cameras[i].Name, '"');
 for i := 0 to Lights.Count-1 do
  Writeln('  Light ',i, ': "',Lights[i].Name, '"',
          Format(' Pos(%f,%f,%f)',
          [Lights[i].Pos[0], Lights[i].Pos[1], Lights[i].Pos[2]]),
          ' Col',VectorToNiceStr(Lights[i].Col));
 for i := 0 to Materials.Count-1 do
 begin
  Write('  Material ',i, ': "',Materials[i].Name, '"'
         ,' Amb(',ColToStr(Materials[i].AmbientCol), ')'
         ,' Diff(',ColToStr(Materials[i].DiffuseCol), ')'
         ,' Spec(',ColToStr(Materials[i].SpecularCol), ')');
  if Materials[i].TextureMap1.Exists then
   Write(' Texture1('+Materials[i].TextureMap1.MapFilename+')');
  if Materials[i].TextureMap2.Exists then
   Write(' Texture2('+Materials[i].TextureMap2.MapFilename+')');
  Writeln;
 end;
 for i := 0 to Trimeshes.Count-1 do
  Writeln('  Trimesh ',i, ': "',Trimeshes[i].Name, '" - ',
    Trimeshes[i].VertsCount, ' vertices, ',Trimeshes[i].FacesCount, ' triangles');
 Writeln('All trimeshes sum : ',SumTrimeshesVertsCount, ' vertices, ',
   SumTrimeshesFacesCount, ' triangles');
 Writeln('Bounding box : ',Box3DToNiceStr(BoundingBox),
   ', average size : ',Format('%f', [Box3DAvgSize(BoundingBox)]) );
end;

function TScene3ds.TryCameraMatrix(CamNumber: integer): TMatrix4Single;
begin
 if CamNumber < Cameras.Count then
  result := Cameras[CamNumber].Matrix else
  result := IdentityMatrix4Single;
end;

function TScene3ds.BoundingBox: TBox3D;
begin result := FBoundingBox end;

end.
