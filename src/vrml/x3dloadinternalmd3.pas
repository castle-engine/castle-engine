{
  Copyright 2007-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading MD3 (Quake3 and derivatives) format. Format specification is on
  [http://icculus.org/homepages/phaethon/q3a/formats/md3format.html]. }
unit X3DLoadInternalMD3;

interface

uses SysUtils, Classes, KambiUtils, KambiClassUtils, VectorMath, VRMLNodes,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif}, GenericStructList;

type
  TMd3Triangle = record
    Indexes: array [0..2] of LongWord;
  end;
  PMd3Triangle = ^TMd3Triangle;

  TDynMd3TriangleArray = class(specialize TGenericStructList<TMd3Triangle>)
    function Add: PMd3Triangle;
  end;

  TMd3Vertex = record
    Position: array [0..2] of SmallInt;
    Normal: SmallInt;
  end;
  PMd3Vertex = ^TMd3Vertex;

  TDynMd3VertexArray = specialize TGenericStructList<TMd3Vertex>;

  TMd3TexCoord = TVector2Single;
  TDynMd3TexCoordArray = TDynVector2SingleArray;

  TMd3Surface = class
  private
    { Read surface from file, and advance position to next surface. }
    procedure Read(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;

  public
    Name: string;

    Vertexes: TDynMd3VertexArray;
    TextureCoords: TDynMd3TexCoordArray;
    Triangles: TDynMd3TriangleArray;

    { Frames within this surface.
      This is always the same as the TObject3DMD3.FramesCount of enclosing
      TObject3DMD3 instance (yes, this assumption is checked when loading
      MD3, as it must be @true for any valid MD3 file). }
    FramesCount: Cardinal;

    { Vertexes array has VertexesInFrameCount * FramesCount items,
      each set of VertexesInFrameCount is for a different animation frame.
      TextureCoords always has VertexesInFrameCount (texture is not
      animated). }
    VertexesInFrameCount: Cardinal;
  end;

  TMd3SurfaceList = specialize TFPGObjectList<TMd3Surface>;

  { MD3 (Quake3 engine model format) reader. }
  TObject3DMD3 = class
  public
    { Reads MD3 from a file.

      Associated skin file is also read,
      to get texture filename: for xxx.md3, we will try to read
      xxx_default.skin file, looking there for a texture filename.
      Texture filename found there will be trimmed to a name
      (i.e. without directory part, as it usually specifies directory
      within quake/tremulous/etc. data dir, not relative to md3 model dir). }
    constructor Create(const FileName: string);

    { Reads MD3 from a stream. The stream must be freely seekable
      (i.e. setting Position to any value must be supported) ---
      if unsure, you can wrap your stream in something like TMemoryStream.

      The Stream instance is not owned by this object --- which means
      that if you created it, you should also free it, this class will not
      do it for you. You can free Stream right after constructor finished
      it's work. }
    constructor Create(Stream: TStream; const ATextureFileName: string);

    destructor Destroy; override;

  public
    Name: string;

    Surfaces: TMd3SurfaceList;

    FramesCount: Cardinal;

    { Texture filename to use for this object. This is not read from md3
      file (it's not recorded there), it's read (if available)
      from accompanying xxx_default.skin file. It's empty '' if
      no skin file was found, or it didn't specify any texture filename. }
    TextureFileName: string;

    { Searches for a skin file accompanying given MD3 model filename,
      and reads it. Returns @true and sets TextureFileName if skin file
      found and some texture filename was recorded there. }
    class function ReadSkinFile(const Md3FileName: string;
      out ATextureFileName: string): boolean;
  end;

{ Load a specific animation frame from a given MD3 model.
  @param Md3 is the MD3 file to use.
  @param FrameNumber is the frame number to load, must be < Md3.Count.
  @param WWWBasePath is the base URL, set for TVRMLNode.WWWBasePath. }
function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLRootNode;

{ Load MD3 animation into a single animated X3D model. }
function LoadMD3(const FileName: string): TVRMLRootNode;

{ Load MD3 animation as a sequence of static X3D models. }
procedure LoadMD3Sequence(
  const FileName: string;
  RootNodes: TVRMLNodeList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

implementation

uses KambiFilesUtils, KambiStringUtils, Boxes3D, X3DLoadInternalUtils,
  VRMLCameraUtils;

type
  EInvalidMD3 = class(Exception);

const
  Md3XyzScale = 1/64;
  GoodIdent = 'IDP3';
  GoodVersion = 15;

  { Is always 0, but for future purposes (it seems that Md3 data may
    be embedded in other things) it's a constant here. }
  Md3Start = 0;

  Md3MaxQPath = 64;

type
  TMd3Header = record
    Ident: array [0..3] of char;
    Version: LongInt;
    Name: array [0..Md3MaxQPath - 1] of char;
    Flags: LongInt;
    NumFrames: LongInt;
    NumTags: LongInt;
    NumSurfaces: LongInt;
    NumSkins: LongInt;
    OffsetFrames: LongInt;
    OffsetTags: LongInt;
    OffsetSurfaces: LongInt;
    OffsetEof: LongInt;
  end;

  TMd3Frame = record
    MinBounds: TVector3Single;
    MaxBounds: TVector3Single;
    LocalOrigin: TVector3Single;
    Radius: Single;
    Name: array [0..15] of char;
  end;

  TMd3Tag = record
    Name: array [0..Md3MaxQPath - 1] of char;
    Origin: TVector3Single;
    Axis: array [0..2] of TVector3Single;
  end;

  TMd3FileSurface = record
    Ident: array [0..3] of char;
    Name: array [0..Md3MaxQPath - 1] of char;
    Flags: LongInt;
    NumFrames: LongInt;
    NumShaders: LongInt;
    NumVerts: LongInt;
    NumTriangles: LongInt;
    OffsetTriangles: LongInt;
    OffsetShaders: LongInt;
    OffsetST: LongInt;
    OffsetXYZNormal: LongInt;
    OffsetEnd: LongInt;
  end;

function TDynMd3TriangleArray.Add: PMd3Triangle;
begin
  Count := Count + 1;
  Result := @(List^[Count - 1]);
end;

{ TMd3Surface ---------------------------------------------------------------- }

constructor TMd3Surface.Create;
begin
  inherited;
  Vertexes := TDynMd3VertexArray.Create;
  Triangles := TDynMd3TriangleArray.Create;
  TextureCoords := TDynMd3TexCoordArray.Create;
end;

destructor TMd3Surface.Destroy;
begin
  FreeAndNil(Vertexes);
  FreeAndNil(Triangles);
  FreeAndNil(TextureCoords);
  inherited;
end;

procedure TMd3Surface.Read(Stream: TStream);
var
  SurfaceStart: Int64;
  Surface: TMd3FileSurface;
  I: Integer;
begin
  SurfaceStart := Stream.Position;
  Stream.ReadBuffer(Surface, SizeOf(Surface));

  if not CompareMem(@Surface.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 Surface must be "%s"',
      [GoodIdent]);

  Name := Surface.Name;
  FramesCount := Surface.NumFrames;
  VertexesInFrameCount := Surface.NumVerts;

  (* Tests:
  Writeln('Surface');
  Writeln('  Name "', Surface.Name, '"');
  Writeln('  NumFrames "', Surface.NumFrames, '"');
  Writeln('  NumShaders "', Surface.NumShaders, '"');
  Writeln('  NumVerts "', Surface.NumVerts, '"');
  Writeln('  NumTriangles "', Surface.NumTriangles, '"');
  Writeln('  OffsetTriangles "', Surface.OffsetTriangles, '"');
  Writeln('  OffsetShaders "', Surface.OffsetShaders, '"');
  Writeln('  OffsetST "', Surface.OffsetST, '"');
  Writeln('  OffsetXYZNormal "', Surface.OffsetXYZNormal, '"');
  Writeln('  OffsetEnd "', Surface.OffsetEnd, '"');
  *)

  if Surface.NumTriangles <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetTriangles;
    for I := 0 to Surface.NumTriangles - 1 do
    begin
      Stream.ReadBuffer(Triangles.Add^, SizeOf(TMd3Triangle));
    end;
  end;

  Vertexes.Count := Surface.NumVerts * Surface.NumFrames;
  if Vertexes.Count <> 0 then
  begin
    { For animations: actually we have here Surface.NumFrames times
      the vertexes array. For each frame, separate collection of
      vertices is added. }
    Stream.Position := SurfaceStart + Surface.OffsetXYZNormal;
    for I := 0 to Vertexes.Count - 1 do
    begin
      Stream.ReadBuffer(Vertexes.List^[I], SizeOf(TMd3Vertex));
    end;
  end;

  TextureCoords.Count := VertexesInFrameCount;
  if VertexesInFrameCount <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetST;
    for I := 0 to VertexesInFrameCount - 1 do
    begin
      Stream.ReadBuffer(TextureCoords.List^[I], SizeOf(TMd3TexCoord));
    end;
  end;

  Stream.Position := SurfaceStart + Surface.OffsetEnd;
end;

{ TObject3DMD3 --------------------------------------------------------------- }

constructor TObject3DMD3.Create(const FileName: string);
var
  Stream: TStream;
  ATextureFileName: string;
begin
  if not ReadSkinFile(FileName, ATextureFileName) then
    ATextureFileName := '';

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(Stream, ATextureFileName);
  finally Stream.Free end;
end;

constructor TObject3DMD3.Create(Stream: TStream; const ATextureFileName: string);
var
  Header: TMd3Header;
  Frame: TMd3Frame;
  Tag: TMd3Tag;
  I: Integer;
  NewSurface: TMd3Surface;
begin
  inherited Create;

  TextureFileName := ATextureFileName;

  Stream.ReadBuffer(Header, SizeOf(TMd3Header));

  if not CompareMem(@Header.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 file must be "%s"',
      [GoodIdent]);

  if Header.Version <> GoodVersion then
    raise EInvalidMD3.CreateFmt('Only supported version of MD3 file is "%d"',
      [GoodVersion]);

  Name := Header.Name;
  FramesCount := Header.NumFrames;

  if Header.NumFrames <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetFrames;
    for I := 0 to Header.NumFrames - 1 do
    begin
      Stream.ReadBuffer(Frame, SizeOf(Frame));
    end;
  end;

  if Header.NumTags <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetTags;
    for I := 0 to Header.NumTags - 1 do
    begin
      Stream.ReadBuffer(Tag, SizeOf(Tag));
    end;
  end;

  Surfaces := TMd3SurfaceList.Create(true);

  if Header.NumSurfaces <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetSurfaces;
    for I := 0 to Header.NumSurfaces - 1 do
    begin
      NewSurface := TMd3Surface.Create;
      NewSurface.Read(Stream);
      Surfaces.Add(NewSurface);

      if FramesCount <> NewSurface.FramesCount then
        raise EInvalidMD3.CreateFmt('Surface frame count (%d) is different than ' +
          'model''s frame count (%d)', [FramesCount, NewSurface.FramesCount]);
    end;
  end;
end;

destructor TObject3DMD3.Destroy;
begin
  FreeAndNil(Surfaces);
  inherited;
end;

class function TObject3DMD3.ReadSkinFile(const Md3FileName: string;
  out ATextureFileName: string): boolean;
var
  SkinFile: TextFile;
  S: string;
  SkinFileName: string;
  CommaPos: Integer;
  Md3Path, NoExt: string;
begin
  Result := false;
  NoExt := DeleteFileExt(Md3FileName);
  SkinFileName := NoExt + '_default.skin';
  Md3Path := ExtractFilePath(Md3FileName);
  if FileExists(SkinFileName) then
  begin
    SafeReset(SkinFile, SkinFileName, true);
    try
      while not Eof(SkinFile) do
      begin
        Readln(SkinFile, S);
        CommaPos := Pos(',', S);
        if CommaPos <> 0 then
        begin
          ATextureFileName := Trim(SEnding(S, CommaPos + 1));
          if ATextureFileName <> '' then
          begin
            { Directory recorded in ATextureFileName is useless for us,
              it's usually a relative directory inside quake/tremulous/etc.
              data. We just assume that this is inside the same dir as
              md3 model file, so we strip the directory part. }
            ATextureFileName := ExtractFileName(ATextureFileName);

            if not FileExists(Md3Path + ATextureFileName) then
            begin
              { Now, I know this is stupid, but we simply cannot trust
                the extension of texture given in ATextureFileName in skin file.
                It's common in Quake3 data files that texture filename is given
                as xxx.tga, while in fact only xxx.jpg exists and should be used. }
              if FileExists(Md3Path + ChangeFileExt(ATextureFileName, '.jpg')) then
                ATextureFileName := ChangeFileExt(ATextureFileName, '.jpg') else

              { Also, some files have texture names with missing extension...
                So we have to check also for tga here.
                E.g. tremulous-unpacked-data/models/players/human_base/head }
              if FileExists(Md3Path + ChangeFileExt(ATextureFileName, '.tga')) then
                ATextureFileName := ChangeFileExt(ATextureFileName, '.tga') else

              { Now this is also stupid... But some tremulous data
                has texture recorded as "null". We should ignore this
                texture, and look at the next line. (We do it only
                if above checks proved that texture filename doesn't
                exist, and texture filename with .jpg also doesn't exist) }
              { TODO: actually,
                ~/3dmodels/tremulous-unpacked-data/models/players/human_base/upper_default.skin
                shows that MD3 models may have various parts textured with
                different textures or even not textured.
                We should read each pair as PartName, PartTextureFileName,
                where null means "no texture". Then we should apply each
                part separately. PairName is somewhere recorded in MD3
                file, right ? }
              if ATextureFileName = 'null' then
                Continue;
            end;

            Result := true;
            Exit;
          end;
        end;
      end;
    finally CloseFile(SkinFile) end;
  end else
  begin
    { I see this convention used in a couple of tremulous data places:
      object may be without skin file, but just texture with
      basename same as model exists. }
    ATextureFileName := NoExt + '.jpg';
    if FileExists(ATextureFileName) then
      Result := true;
  end;
end;

{ Converting to X3D ---------------------------------------------------------- }

function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLRootNode;
var
  Texture: TNodeImageTexture;
  SceneBox: TBox3D;

  function MakeCoordinates(Vertexes: TDynMd3VertexArray;
    VertexesInFrameCount: Cardinal): TNodeCoordinate;
  var
    I: Integer;
    V: PMd3Vertex;
  begin
    Result := TNodeCoordinate.Create('', WWWBasePath);
    Result.FdPoint.Items.Count := VertexesInFrameCount;
    V := @(Vertexes.List^[VertexesInFrameCount * FrameNumber]);
    for I := 0 to VertexesInFrameCount - 1 do
    begin
      Result.FdPoint.Items.List^[I] := Vector3Single(
        V^.Position[0] * Md3XyzScale,
        V^.Position[1] * Md3XyzScale,
        V^.Position[2] * Md3XyzScale);
      Inc(V);
    end;

    Box3DSumTo1st(SceneBox, CalculateBoundingBox(Result.FdPoint.Items));
  end;

  function MakeTextureCoordinates(
    TextureCoords: TDynMd3TexCoordArray): TNodeTextureCoordinate;
  var
    I: Integer;
    V: PVector2Single;
  begin
    Result := TNodeTextureCoordinate.Create('', WWWBasePath);
    Result.FdPoint.Items.Count := TextureCoords.Count;
    V := PVector2Single(TextureCoords.List);
    for I := 0 to TextureCoords.Count - 1 do
    begin
      Result.FdPoint.Items.List^[I] := Vector2Single(V^[0], 1-V^[1]);
      Inc(V);
    end;
  end;

  function MakeIndexes(Triangles: TDynMd3TriangleArray): TNodeIndexedFaceSet;
  var
    I: Integer;
  begin
    Result := TNodeIndexedFaceSet.Create('', WWWBasePath);
    Result.FdCreaseAngle.Value := NiceCreaseAngle;
    Result.FdSolid.Value := false;
    Result.FdCoordIndex.Items.Count := Triangles.Count * 4;
    Result.FdTexCoordIndex.Items.Count := Triangles.Count * 4;
    for I := 0 to Triangles.Count - 1 do
    begin
      Result.FdCoordIndex.Items.List^[I*4 + 0] := Triangles.List^[I].Indexes[0];
      Result.FdCoordIndex.Items.List^[I*4 + 1] := Triangles.List^[I].Indexes[1];
      Result.FdCoordIndex.Items.List^[I*4 + 2] := Triangles.List^[I].Indexes[2];
      Result.FdCoordIndex.Items.List^[I*4 + 3] := -1;

      Result.FdTexCoordIndex.Items.List^[I*4 + 0] := Triangles.List^[I].Indexes[0];
      Result.FdTexCoordIndex.Items.List^[I*4 + 1] := Triangles.List^[I].Indexes[1];
      Result.FdTexCoordIndex.Items.List^[I*4 + 2] := Triangles.List^[I].Indexes[2];
      Result.FdTexCoordIndex.Items.List^[I*4 + 3] := -1;
    end;
  end;

  function MakeShape(Surface: TMd3Surface): TNodeShape;
  var
    IFS: TNodeIndexedFaceSet;
  begin
    IFS := MakeIndexes(Surface.Triangles);
    IFS.FdCoord.Value := MakeCoordinates(Surface.Vertexes, Surface.VertexesInFrameCount);
    IFS.FdTexCoord.Value := MakeTextureCoordinates(Surface.TextureCoords);

    Result := TNodeShape.Create(ToVRMLName(Surface.Name), WWWBasePath);
    Result.FdGeometry.Value := IFS;
    Result.Material := TNodeMaterial.Create('', WWWBasePath);
    Result.Texture := Texture;
  end;

var
  I: Integer;
begin
  Result := TVRMLRootNode.Create(
    ToVRMLName(Md3.Name
      { Although adding here FrameNumber is not a bad idea, but VRMLGLAnimation
        requires for now that sequence of VRML models have the same node names }
      { + '_Frame' + IntToStr(FrameNumber) }), WWWBasePath);

  Result.HasForceVersion := true;
  Result.ForceVersion := X3DVersion;

  SceneBox := EmptyBox3D;

  if Md3.TextureFileName <> '' then
  begin
    Texture := TNodeImageTexture.Create('', WWWBasePath);
    Texture.FdUrl.Items.Add(Md3.TextureFileName);
  end else
    Texture := nil;

  for I := 0 to Md3.Surfaces.Count - 1 do
    Result.FdChildren.Add(MakeShape(Md3.Surfaces[I]));

  { MD3 files have no camera. I add camera here, just to force GravityUp
    to be in +Z, since this is the convention used in all MD3 files that
    I saw (so I guess that Quake3 engine generally uses this convention). }
  Result.FdChildren.Add(CameraNodeForWholeScene(2, WWWBasePath, SceneBox,
    0, 2, false, true));

  if Texture <> nil then
  begin
    Texture.FreeIfUnused;
    Texture := nil;
  end;
end;

function LoadMD3(const FileName: string): TVRMLRootNode;
var
  Md3: TObject3DMD3;
  WWWBasePath: string;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3DMD3.Create(FileName);
  try
    Result := LoadMD3Frame(Md3, 0, WWWBasePath);
  finally FreeAndNil(Md3) end;
end;

procedure LoadMD3Sequence(
  const FileName: string;
  RootNodes: TVRMLNodeList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);
var
  Md3: TObject3DMD3;
  WWWBasePath: string;
  I: Integer;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3DMD3.Create(FileName);
  try
    { handle each MD3 frame }
    for I := 0 to Md3.FramesCount - 1 do
    begin
      RootNodes.Add(LoadMD3Frame(Md3, I, WWWBasePath));
      Times.Add(I / 30);
    end;

    ScenesPerTime := 30;
    { Default ScenesPerTime and times are set such that one MD3
      frame will result in one frame inside TVRMLGLAnimation.
      So don't try to merge these frames (on the assumption that
      they are not merged in MD3... so hopefully there's no need for it ?). }
    EqualityEpsilon := 0.0;

    { Really, no sensible default for Loop/Backwards here...
      I set Loop to @false, otherwise it's not clear for user when
      animation ends. }
    TimeLoop := false;
    TimeBackwards := false;
  finally FreeAndNil(Md3) end;
end;

end.
