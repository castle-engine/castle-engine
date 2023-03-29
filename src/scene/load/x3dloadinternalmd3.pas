{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading MD3 (used by Quake3 and derivatives like Tremulous) format.

  References:
  - Format spec: http://icculus.org/homepages/phaethon/q3a/formats/md3format.html
  - Another format spec: https://mino-git.github.io/rtcw-wet-blender-model-tools/md3.html
  - One sample implementation: https://github.com/edems96/MD3-Model---Animation-Viewer/tree/master/src
}
unit X3DLoadInternalMD3;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleVectors, X3DNodes,
  CastleInternalNodeInterpolator;

{ Load MD3 model, with skin (from Md3Skin) and animations. }
function LoadMD3(const Stream: TStream; const BaseUrl: string): TX3DRootNode;

var
  Md3Skin: String = 'default';

implementation

uses CastleFilesUtils, CastleStringUtils, CastleBoxes, X3DLoadInternalUtils,
  X3DCameraUtils, CastleDownload, CastleURIUtils, CastleLog;

const
  Md3MaxQPath = 64;
  Md3Fps = 30;

type
  TMd3Triangle = record
    Indexes: array [0..2] of UInt32;
  end;
  TMd3TriangleList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Triangle>;

  TMd3Vertex = record
    Position: array [0..2] of SmallInt;
    Normal: SmallInt;
  end;
  PMd3Vertex = ^TMd3Vertex;
  TMd3VertexList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Vertex>;

  TMd3TexCoord = TVector2;
  TMd3TexCoordList = TVector2List;

  TMd3Shader = record
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    ShaderIndex: UInt32;
  end;
  TMd3ShaderList = {$ifdef FPC}specialize{$endif} TStructList<TMd3Shader>;

  TMd3Surface = class
  private
    Shaders: TMd3ShaderList;

    { Read surface from file, and advance position to next surface. }
    procedure Read(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;

  public
    Name: string;

    Vertexes: TMd3VertexList;
    TextureCoords: TMd3TexCoordList;
    Triangles: TMd3TriangleList;

    { Shader name, or '' if not found in file.
      Each surface may use a different "shader" which is really just a way to specify a texture
      (possibly indirectly, if using Skin dictionary).
      See sandyhead in example MD3 in https://forum.castle-engine.io/t/weirdness-with-zeronextsecondspassed-when-used-with-tupdate/775/21 }
    ShaderName: String;

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

  TMd3SurfaceList = {$ifdef FPC}specialize{$endif} TObjectList<TMd3Surface>;

  { MD3 (Quake3 engine model format) reader. }
  TObject3DMD3 = class
  strict private
    { Read the indicated .skin file and fill the Skin contents, if skin file exists. }
    procedure ReadSkinFile(const SkinUrl: string);
    { Read the stream containing MD3 file. }
    procedure ReadMd3(const Stream: TStream);
  public
    Name: string;
    Surfaces: TMd3SurfaceList;
    FramesCount: Cardinal;

    { Skin information maps shader names -> texture URLs.
      Empty (but never @nil) if the skin file was not found. }
    Skin: TStringList;

    { Reads MD3 from a stream, with given absolute base URL.

      We also read the associated skin file, if it exists.
      We look for file called '<basename>_<Md3Skin>.skin',
      where <basename> is the basename (no path, no extension) from BaseUrl,
      and Md3Skin is a global variable in this unit.
      So e.g. for 'head.md3' we read 'head_default.skin' by default.

      We strip the path from texture URLs found in the skin file,
      because the texture URLs are often given relative to main quake/tremulous/etc.
      data directory, and we don't know this directory.
      So we strip path part, and assume that textures for MD3 models
      are just inside the same directory as the MD3 file.

      The Stream must be freely seekable
      (i.e. setting Position to any value must be supported) ---
      if unsure, you can wrap your stream in something like TMemoryStream.

      The Stream instance is not owned by this object --- which means
      that if you created it, you should also free it, this class will not
      do it for you. You can free Stream right after constructor finished
      it's work. }
    constructor Create(const Stream: TStream; const BaseUrl: string);
    destructor Destroy; override;
  end;

type
  EInvalidMD3 = class(Exception);

const
  Md3XyzScale = 1/64;
  GoodIdent = 'IDP3';
  GoodVersion = 15;

  { Is always 0, but for future purposes (it seems that Md3 data may
    be embedded in other things) it's a constant here. }
  Md3Start = 0;

{ TMd3Surface ---------------------------------------------------------------- }

constructor TMd3Surface.Create;
begin
  inherited;
  Vertexes := TMd3VertexList.Create;
  Triangles := TMd3TriangleList.Create;
  TextureCoords := TMd3TexCoordList.Create;
  Shaders := TMd3ShaderList.Create;
end;

destructor TMd3Surface.Destroy;
begin
  FreeAndNil(Vertexes);
  FreeAndNil(Triangles);
  FreeAndNil(TextureCoords);
  FreeAndNil(Shaders);
  inherited;
end;

procedure TMd3Surface.Read(Stream: TStream);
type
  TMd3FileSurface = record
    Ident: array [0..3] of AnsiChar;
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    Flags: Int32;
    NumFrames: Int32;
    NumShaders: Int32;
    NumVerts: Int32;
    NumTriangles: Int32;
    OffsetTriangles: Int32;
    OffsetShaders: Int32;
    OffsetST: Int32;
    OffsetXYZNormal: Int32;
    OffsetEnd: Int32;
  end;
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

  Shaders.Count := Surface.NumShaders;
  if Shaders.Count <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetShaders;
    Stream.ReadBuffer(Shaders.List^, SizeOf(TMd3Shader) * Shaders.Count);
    { We only use 1st shader name.
      Interpretation of other shader names is unknown. }
    if Shaders.Count > 0 then
      ShaderName := Shaders[0].Name;
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

{ Convert texture URL found in skin file to a URL, relative to the MD3 file,
  pointing to valid texture file.
  This strips path from URL, always.
  This can also fix (or add) URL extension. }
function FixTextureUrl(const TextureUrl, TexturePath: String): String;
begin
  { Directory recorded in TextureUrl is useless for us,
    it's usually a relative directory inside quake/tremulous/etc.
    data. We just assume that this is inside the same dir as
    md3 model file, so we strip the directory part. }
  Result := ExtractURIName(TextureUrl);

  if not URIFileExists(TexturePath + Result) then
  begin
    { We cannot trust
      the extension of texture given in Result in skin file.
      It's common in Quake3 data files that texture URL is given
      as xxx.tga, while in fact only xxx.jpg exists and should be used. }
    if URIFileExists(TexturePath + ChangeURIExt(Result, '.jpg')) then
      Result := ChangeURIExt(Result, '.jpg') else

    { Also, some files have texture names with missing extension...
      So we have to check also for tga here.
      E.g. tremulous-unpacked-data/models/players/human_base/head }
    if URIFileExists(TexturePath + ChangeURIExt(Result, '.tga')) then
      Result := ChangeURIExt(Result, '.tga') else
  end;
end;

{ TObject3DMD3 --------------------------------------------------------------- }

constructor TObject3DMD3.Create(const Stream: TStream; const BaseUrl: string);
var
  SkinUrl: String;
begin
  inherited Create;

  Skin := TStringList.Create;
  SkinUrl := DeleteURIExt(BaseUrl) + '_' + Md3Skin + '.skin';
  ReadSkinFile(SkinUrl);

  ReadMd3(Stream);
end;

destructor TObject3DMD3.Destroy;
begin
  FreeAndNil(Surfaces);
  FreeAndNil(Skin);
  inherited;
end;

procedure TObject3DMD3.ReadSkinFile(const SkinUrl: string);
var
  SkinFile: TTextReader;
  S, SkinKey, TextureUrl: string;
  CommaPos: Integer;
  TexturePath: string;
begin
  if URIFileExists(SkinUrl) then
  begin
    TexturePath := ExtractURIPath(SkinUrl);
    SkinFile := TTextReader.Create(SkinUrl);
    try
      { Note: We don't just do
          Skin.NameValueSeparator := ',';
          Skin.LoadFromURL(ATextureUrl);
        because
        - MissingNameValueSeparatorAction doesn't seem supported by Delphi
          ( https://www.freepascal.org/docs-html/rtl/classes/tstrings.missingnamevalueseparatoraction.html )
        - we want to process texture URL anyway before we add it.
      }

      while not SkinFile.Eof do
      begin
        S := SkinFile.Readln;
        CommaPos := Pos(',', S);
        if CommaPos <> 0 then
        begin
          SkinKey := Trim(Copy(S, 1, CommaPos - 1));
          TextureUrl := Trim(SEnding(S, CommaPos + 1));
          if (TextureUrl <> '') and
             (TextureUrl <> 'null') then
            TextureUrl := FixTextureUrl(TextureUrl, TexturePath);
          Skin.Add(SkinKey + '=' + TextureUrl);
        end else
          WritelnWarning('Unexpected line in MD3 .skin file (no comma): %s', S);
      end;
    finally FreeAndNil(SkinFile) end;

    //WritelnLog('MD3 skin file found, with %d pairs (shader name -> texture URL): %s', [Skin.Count, Skin.Text]);
  end;
end;

procedure TObject3DMD3.ReadMd3(const Stream: TStream);
type
  TMd3Header = record
    Ident: array [0..3] of AnsiChar;
    Version: Int32;
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    Flags: Int32;
    NumFrames: Int32;
    NumTags: Int32;
    NumSurfaces: Int32;
    NumSkins: Int32;
    OffsetFrames: Int32;
    OffsetTags: Int32;
    OffsetSurfaces: Int32;
    OffsetEof: Int32;
  end;

  (* Unused for now
  TMd3Frame = record
    MinBounds: TVector3;
    MaxBounds: TVector3;
    LocalOrigin: TVector3;
    Radius: Single;
    Name: array [0..15] of AnsiChar;
  end;

  TMd3Tag = record
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    Origin: TVector3;
    Axis: array [0..2] of TVector3;
  end;
  *)

var
  Header: TMd3Header;
  (* Unused for now
  Frame: TMd3Frame;
  Tag: TMd3Tag;
  *)
  I: Integer;
  NewSurface: TMd3Surface;
begin
  inherited Create;

  Stream.ReadBuffer(Header, SizeOf(TMd3Header));

  if not CompareMem(@Header.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 file must be "%s"',
      [GoodIdent]);

  if Header.Version <> GoodVersion then
    raise EInvalidMD3.CreateFmt('Only supported version of MD3 file is "%d"',
      [GoodVersion]);

  Name := Header.Name;
  FramesCount := Header.NumFrames;

  (* Unused for now
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
  *)

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

{ Converting to X3D ---------------------------------------------------------- }

{ Load a specific animation frame from a given MD3 model.
  @param Md3 is the MD3 file to use.
  @param FrameNumber is the frame number to load, must be < Md3.Count.
  @param BaseUrl is the base URL, set for TX3DNode.BaseUrl. }
function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const BaseUrl: string; var SceneBox: TBox3D): TGroupNode;

  function MakeCoordinates(Vertexes: TMd3VertexList;
    VertexesInFrameCount: Cardinal): TCoordinateNode;
  var
    I: Integer;
    V: PMd3Vertex;
  begin
    Result := TCoordinateNode.Create('', BaseUrl);
    Result.FdPoint.Items.Count := VertexesInFrameCount;
    V := PMd3Vertex(Vertexes.Ptr(VertexesInFrameCount * FrameNumber));
    for I := 0 to VertexesInFrameCount - 1 do
    begin
      Result.FdPoint.Items.List^[I] := Vector3(
        V^.Position[0] * Md3XyzScale,
        V^.Position[1] * Md3XyzScale,
        V^.Position[2] * Md3XyzScale);
      Inc(V);
    end;

    SceneBox.Include(CalculateBoundingBox(Result.FdPoint.Items));
  end;

  function MakeTextureCoordinates(
    TextureCoords: TMd3TexCoordList): TTextureCoordinateNode;
  var
    I: Integer;
    V: PVector2;
  begin
    Result := TTextureCoordinateNode.Create('', BaseUrl);
    Result.FdPoint.Items.Count := TextureCoords.Count;
    V := PVector2(TextureCoords.L);
    for I := 0 to TextureCoords.Count - 1 do
    begin
      Result.FdPoint.Items.List^[I] := Vector2(V^[0], 1-V^[1]);
      Inc(V);
    end;
  end;

  function MakeIndexes(Triangles: TMd3TriangleList): TIndexedFaceSetNode;
  var
    I: Integer;
  begin
    Result := TIndexedFaceSetNode.Create('', BaseUrl);
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

  function MakeTexture(const ShaderName: String): TImageTextureNode;
  var
    TextureUrl: String;
  begin
    TextureUrl := Md3.Skin.Values[ShaderName];
    { If .skin file was not found, or was empty, or ShaderName isn't found there... }
    if TextureUrl = '' then
      TextureUrl := ShaderName;
    { Use first texture specified in .skin, as a fallback (Tremulous creatures use it) }
    if (TextureUrl = '') and (Md3.Skin.Count <> 0) then
      TextureUrl := Md3.Skin.ValueFromIndex[0];
    { Use basename of MD3 for texture basename. }
    if TextureUrl = '' then
      TextureUrl := DeleteURIExt(ExtractURIName(BaseUrl));

    if TextureUrl = 'none' then
    begin
      { In this case, we explicitly don't want to use a texture. }
      Result := nil;
    end else
    begin
      Result := TImageTextureNode.Create('', BaseUrl);
      TextureUrl := FixTextureUrl(TextureUrl, ExtractURIPath(BaseUrl));
      Result.SetUrl([TextureUrl]);
    end;
  end;

  function MakeShape(Surface: TMd3Surface): TShapeNode;
  var
    IFS: TIndexedFaceSetNode;
  begin
    IFS := MakeIndexes(Surface.Triangles);
    IFS.Coord := MakeCoordinates(Surface.Vertexes, Surface.VertexesInFrameCount);
    IFS.TexCoord := MakeTextureCoordinates(Surface.TextureCoords);

    Result := TShapeNode.Create(Surface.Name, BaseUrl);
    Result.Geometry := IFS;
    Result.Material := TMaterialNode.Create('', BaseUrl);
    Result.Texture := MakeTexture(Surface.ShaderName);
  end;

var
  I: Integer;
begin
  Result := TGroupNode.Create('', BaseUrl);

  for I := 0 to Md3.Surfaces.Count - 1 do
    Result.AddChildren(MakeShape(Md3.Surfaces[I]));
end;

function LoadMD3(const Stream: TStream; const BaseUrl: string): TX3DRootNode;
var
  Md3: TObject3DMD3;
  I: Integer;
  SceneBox: TBox3D;
  Switch: TSwitchNode;
  TimeSensor: TTimeSensorNode;
  IntSequencer: TIntegerSequencerNode;
begin
  Md3 := TObject3DMD3.Create(Stream, BaseUrl);
  try
    Result := TX3DRootNode.Create(Md3.Name, BaseUrl);
    Result.HasForceVersion := true;
    Result.ForceVersion := X3DVersion;

    SceneBox := TBox3D.Empty;

    Switch := TSwitchNode.Create('', BaseUrl);
    Switch.WhichChoice := 0; // show something non-empty on load
    for I := 0 to Md3.FramesCount - 1 do
      Switch.AddChildren(LoadMD3Frame(Md3, I, BaseUrl, SceneBox));
    Result.AddChildren(Switch);

    TimeSensor := TTimeSensorNode.Create(TNodeInterpolator.DefaultAnimationName, BaseUrl);
    TimeSensor.CycleInterval := Md3.FramesCount / Md3Fps;
    Result.AddChildren(TimeSensor);

    IntSequencer := TIntegerSequencerNode.Create(TNodeInterpolator.DefaultAnimationName + '_IntegerSequencer', BaseUrl);
    { Using ForceContinuousValue_Changed for the same reason why
      castleinternalnodeinterpolator.pas .
      See https://castle-engine.io/x3d_implementation_eventutilities_extensions.php#section_ext_forceContinuousValue_Changed .
      As multiple IntegerSequencer may affect this Switch (once we add support
      for other animations) this is important. }
    IntSequencer.ForceContinuousValue_Changed := true;
    IntSequencer.FdKey.Count := Md3.FramesCount;
    IntSequencer.FdKeyValue.Count := Md3.FramesCount;
    for I := 0 to Md3.FramesCount - 1 do
    begin
      IntSequencer.FdKey.Items[I] := I / Md3.FramesCount;
      IntSequencer.FdKeyValue.Items[I] := I;
    end;
    Result.AddChildren(IntSequencer);

    Result.AddRoute(TimeSensor.EventFraction_changed, IntSequencer.EventSet_fraction);
    Result.AddRoute(IntSequencer.EventValue_changed, Switch.FdWhichChoice);
    Result.ExportNode(TimeSensor);

    { MD3 files have no camera. I add camera here, just to force GravityUp
      to be in +Z, since this is the convention used in all MD3 files that
      I saw (so I guess that Quake3 engine generally uses this convention). }
    Result.AddChildren(CameraNodeForWholeScene(cvVrml2_X3d,
      BaseUrl, SceneBox, 0, 2, false, true));
  finally FreeAndNil(Md3) end;
end;

end.
