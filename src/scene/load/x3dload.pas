{
  Copyright 2003-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Loading and saving nodes.)

  Almost every format is handled by converting it into VRML / X3D nodes graph.
  This allows to use nodes throughout the engine, for all rendering and processing.

  Basic guide for adding a new format:

  @unorderedList(
    @item(
      Particular formats are implemented inside various X3DLoadInternalXxx units.
      Implementation of this unit calls them. In the future,
      a mechanism that allows you to "register" an importer, without modifying
      this unit's implementation, may be done --- report if needed.)

    @item(Scene formats are also listed in the file filters constants:
      see LoadScene_FileFilters .
      Each format has a file filter to specifically choose this format,
      and also is added to the "All Scenes" filter.)

    @item(Enable castle-model-viewer to associate with this file format on freedesktops
      (GNOME, and other following freedesktop.org specs). For this,

      1. Update castle-model-viewer MIME database.
      Simply add appopriate element to ../../../castle-model-viewer/freedesktop/castle-model-viewer.xml.
      Format of that MIME xml file is self-explanatory.
      It's good idea to google first
      to search for standard MIME type for your model format (e.g. wikipedia
      shows mime types for formats).
      If none is found, just use application/x-???, where ??? is some short
      name for your format.

      2. After adding to MIME database, you want to also add format to
      ../../../castle-model-viewer/freedesktop/castle-model-viewer.desktop, to indicate that
      castle-model-viewer handles this MIME type.

      3. Finally, also add this to ../../../castle-model-viewer/freedesktop/install_thumbnailer.sh,
      so that GNOME nautilus thumbnailers for this MIME types can be installed.)

    @item(You probably also want to extend documentation.
      At least https://castle-engine.io/creating_data_model_formats.php ,
      it lists all supported scene formats.)
  )
}
unit X3DLoad;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleUtils, CastleVectors, X3DNodes, CastleStringUtils;

{ Load a scene as X3D node. Guesses scene format based on the URL extension.
  We load a large number of formats, see https://castle-engine.io/creating_data_model_formats.php .

  All the scene formats are loaded as a graph of X3D nodes.

  URL is downloaded using the CastleDownload unit,
  so it supports files, http resources and more.
  See https://castle-engine.io/manual_network.php
  about supported URL schemes.
  If you all you care about is loading normal files, then just pass
  a normal filename (absolute or relative to the current directory)
  as the URL parameter.

  To actually display, animate and do many other things with the loaded
  model, you usually want to load it to TCastleScene, using
  the @link(TCastleSceneCore.Load) method.
  Like this:

@longCode(#
var
  RootNode: TX3DRootNode;
  Scene: TCastleScene;
begin
  RootNode := LoadNode('my_model.x3d');
  Scene := TCastleScene.Create(Application);
  Scene.Load(RootNode, true);
  // The 2nd parameter of Load says that Scene owns RootNode
end;
#)

  Actually, in most cases you don't need to use LoadNode (and this unit, X3DLoad)
  at all, and you can simply load from an URL:

@longCode(#
var
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load('my_model.x3d');
  // you can access Scene.RootNode after loading, if needed
end;
#)

  Note that usually you want to load models from the game data,
  so you would actually use @code('castle-data:/my_model.x3d') URL instead
  of @code('my_model.x3d').
}
function LoadNode(const Url: String): TX3DRootNode; overload;

{ Load a scene as X3D node from TStream.

  Takes a TStream instance with contents to load,
  and MimeType parameter determines the data content (e.g. X3D or glTF or Spine model).

  In most cases, instead of this, you should use a simpler LoadNode overloaded version
  that just takes URL parameter without the explicit Stream or MimeType parameters.
  It will automatically create the stream and guess MIME type.

  The BaseUrl parameter here is used to resolve relative URLs inside the model,
  e.g. references to textures from various 3D and 2D model formats are resolved
  relative to this base URL.
  It generally should be a URL from which you downloaded the model.
  If you don't know it, you can use the current directory (which can be conveniently
  expressed by BaseUrl = '', which is a relative empty URL implying current working dir).
  We @bold(do not) use BaseUrl to determine file contents (e.g. we don't look at filename
  extension here) in this routine.

  Note that some formats (like glTF) require a stream with free seeking capabilities.
  Call @link(Download) with soForceMemoryStream, or wrap the stream in TMemoryStream manually,
  if unsure. The overloaded LoadNode without explicit TStream uses soForceMemoryStream
  when necessary.

  Note that this routine assumes that the stream is not gzip-compressed.
  The overloaded LoadNode without explicit TStream accounts for gzip-compressed streams
  in some cases.
}
function LoadNode(const Stream: TStream; BaseUrl: String; const MimeType: String): TX3DRootNode; overload;

const
  DefaultBakedAnimationSmoothness = 1;

var
  { A smoothness value for "baked" animations loaded from castle-anim-frames
    files. This is multiplied by the scenes_per_time value recorded
    in castle-anim-frames file (30 by default), and determines the number
    of extra frames we add to the baked animation (between key frames). }
  BakedAnimationSmoothness: Single = DefaultBakedAnimationSmoothness;

{ Save model to a file.

  See SaveNode_FileFilters for all model formats that we can save.

  If you provide explicit URL, it determines the output format.
  If you provide a Stream and MimeType, then MimeType determines the output format.
  E.g. use MimeType = 'model/x3d+vrml' to X3D classic encoding,
  or MimeType = 'model/x3d+xml' to X3D XML encoding.

  @param(Generator Optional name, or short description, of the application
    generating this file. This value is not interpreted in any way,
    it is simply a "metadata" information we may store in the resulting file.
  )

  @param(Source Optional name of the original file, if this file is a result of some
    conversion or transformation. This value is not interpreted in any way,
    it is simply a "metadata" information we may store in the resulting file.
  )
}
procedure SaveNode(const Node: TX3DRootNode;
  const Url: String;
  const Generator: String = '';
  const Source: String = ''); overload;
procedure SaveNode(const Node: TX3DRootNode;
  const Stream: TStream; const MimeType: String;
  const Generator: String = '';
  const Source: String = ''); overload;

{ File filters for files loaded by @link(TCastleSceneCore.Load) and @link(LoadNode).
  Suitable for TFileFilterList.AddFiltersFromString and TCastleWindow.FileDialog. }
function LoadScene_FileFilters: String;

{ File filters for files saved by @link(SaveNode).
  Suitable for TFileFilterList.AddFiltersFromString and TCastleWindow.FileDialog. }
function SaveNode_FileFilters: String;

type
  TModelLoadEvent = function (const Stream: TStream; const BaseUrl: String):
    TX3DRootNode;
  TModelSaveEvent = procedure (const Node: TX3DRootNode; const Stream: TStream;
    const Generator: String; const Source: String);

  { Information about a model format, used with @link(RegisterModelFormat). }
  TModelFormat = class
  public
    { How to load given model format (from TStream to TX3DRootNode).
      May be unassigned if we cannot load it. }
    OnLoad: TModelLoadEvent;

    { Does the @link(OnLoad) event require a TStream within which we can
      freely seek. If @true, we will load it with soForceMemoryStream
      for @link(Download). }
    OnLoadForceMemoryStream: Boolean;

    { How to save given model format (from TX3DRootNode to TStream).
      May be unassigned if we cannot save it. }
    OnSave: TModelSaveEvent;

    { List of MIME types. At least one MIME type must be provided here,
      at the time you call @link(RegisterModelFormat). }
    MimeTypes: TCastleStringList;

    { File filter name, like "X3D classic (*.x3dv)".
      Must be non-empty.

      It's your choice whether to show here extensions,
      and if yes -> it's your responsibility
      to make sure they match information in @link(Extensions)
      and map to one of the MIME types specified in @link(MimeTypes).

      To map extensions -> MIME types, add the map to @link(UriMimeExtensions). }
    FileFilterName: String;

    { List of file extensions (including the leading dot) like '.x3dv'.
      Must be non-empty, that is: at least one extension must be provided here.

      If the model filenames contains multiple extensions, you can specify them
      here, e.g. ".x3dv.gz" is a valid extension on this list.
      It is common to pack X3D files in gzip and use ".x3dv.gz" extension,
      so we handle it here.

      It's your responsibility to make sure that all extensions here
      map to one of the MIME types specified in @link(MimeTypes).

      To map extensions -> MIME types, add the map to @link(UriMimeExtensions). }
    Extensions: TCastleStringList;

    constructor Create;
    destructor Destroy; override;
  end;

(*Register given model format, to enable loading and/or saving it
  using @link(LoadNode), @link(SaveNode) and all routines on top of them,
  like @link(TCastleSceneCore.Load).

  The ModelFormat instance given here becomes owned by the internal list
  in this unit. Do not free it, do not modify it after registering it.

  Here's an example how to register a new model format, USD.
  This example assumes you want to register the new model format
  in the @code(initialization) section of a unit, which is the most common place,
  as it ensures that the format is registered for any future use in the application.

  @longCode(#
  function LoadUSD(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
  begin
    Result := TX3DRootNode.Create;
    // TODO: Load USD here
  end;

  var
    ModelFormat: TModelFormat;
  initialization
    ModelFormat := TModelFormat.Create;
    ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadUSD;
    ModelFormat.MimeTypes.Add('model/vnd.usda');
    ModelFormat.MimeTypes.Add('model/vnd.usdz+zip');
    ModelFormat.FileFilterName := 'Universal Scene Description';
    ModelFormat.Extensions.Add('.usd');
    ModelFormat.Extensions.Add('.usda');
    ModelFormat.Extensions.Add('.usdc');
    ModelFormat.Extensions.Add('.usdz');
    RegisterModelFormat(ModelFormat);

    UriMimeExtensions['.usd'] := 'model/vnd.usda';
    UriMimeExtensions['.usda'] := 'model/vnd.usda';
    UriMimeExtensions['.usdc'] := 'model/vnd.usda';
    UriMimeExtensions['.usdz'] := 'model/vnd.usdz+zip';
  end.
  #) *)
procedure RegisterModelFormat(const ModelFormat: TModelFormat);

implementation

uses Generics.Collections,
  CastleClassUtils, CastleImages, CastleUriUtils,
  X3DLoadInternalGEO, X3DLoadInternal3DS, X3DLoadInternalOBJ,
  X3DLoadInternalCollada, X3DLoadInternalSpine, X3DLoadInternalSTL,
  X3DLoadInternalMD3, X3DLoadInternalGLTF, X3DLoadInternalImage,
  X3DLoadInternalCocos2d, CastleInternalNodeInterpolator,
  CastleInternalSpritesheet, CastleDownload, X3DLoadInternalTiledMap;

{ declare FRegisteredModelFormats early ------------------------------------- }

type
  TModelFormatList = class({$ifdef FPC}specialize{$endif} TObjectList<TModelFormat>)
    function FindMimeType(const MimeType: string): TModelFormat;
  end;

var
  FRegisteredModelFormats: TModelFormatList;

{ loading -------------------------------------------------------------------- }

{ Load a sequence of nodes to an animation suitable for TNodeInterpolator.
  Allows to read sequence of static models as an animation,
  e.g. Blender can export Wavefront OBJ like that. }
function LoadSequenceUsingCounter(const Url: String): TX3DRootNode;

  function LoadAnimationUsingCounter(const Url: String): TNodeInterpolator.TAnimationList;
  const
    FramesPerSecond = 30;
  var
    FrameIndex, FirstFrameIndex: Integer;
    Animation: TNodeInterpolator.TAnimation;
    FrameUrl: String;
  begin
    Result := TNodeInterpolator.TAnimationList.Create(true);
    try
      Animation := TNodeInterpolator.TAnimation.Create;
      Result.Add(Animation);
      Animation.Name := TNodeInterpolator.DefaultAnimationName;
      Animation.ScenesPerTime := TNodeInterpolator.DefaultScenesPerTime;
      Animation.Epsilon := TNodeInterpolator.DefaultEpsilon;
      Animation.Loop := false;
      Animation.Backwards := false;

      FrameIndex := 0;
      FrameUrl := FormatNameCounter(Url, FrameIndex, false);
      if not UriFileExists(FrameUrl) then
      begin
        FrameIndex := 1;
        FrameUrl := FormatNameCounter(Url, FrameIndex, false);
        if not UriFileExists(FrameUrl) then
          raise Exception.CreateFmt('First model in a sequence ("%s" or "%s") cannot be found', [
            FormatNameCounter(Url, 0, false),
            FormatNameCounter(Url, 1, false)
          ]);
      end;

      FirstFrameIndex := FrameIndex;

      repeat
        Animation.KeyNodes.Add(LoadNode(FrameUrl));
        Animation.KeyTimes.Add((FrameIndex - FirstFrameIndex) / FramesPerSecond);
        Inc(FrameIndex);
        FrameUrl := FormatNameCounter(Url, FrameIndex, false);
      until not UriFileExists(FrameUrl);
    except
      Result.FreeKeyNodesContents;
      FreeAndNil(Result);
      raise;
    end;
  end;

var
  Animations: TNodeInterpolator.TAnimationList;
begin
  Animations := LoadAnimationUsingCounter(Url);
  try
    Result := TNodeInterpolator.LoadToX3D(Animations);
  finally FreeAndNil(Animations) end;
end;

function LoadNode(const Url: String): TX3DRootNode;
var
  MimeType, UrlWithoutAnchor: string;

  function ForceMemoryStream(const MimeType: String): Boolean;
  var
    ModelFormat: TModelFormat;
  begin
    if FRegisteredModelFormats = nil then
      raise Exception.Create('No model formats registered, you try to LoadNode too early, before initialization of Castle Game Engine units that register model formats');

    ModelFormat := FRegisteredModelFormats.FindMimeType(MimeType);
    if ModelFormat = nil then
      raise Exception.CreateFmt('Unrecognized file type "%s" for scene with base URL "%s"', [
        MimeType,
        UriDisplay(Url)
      ]);

    Result := ModelFormat.OnLoadForceMemoryStream;
  end;

  function DownloadAndLoad(DownloadOptions: TStreamOptions): TX3DRootNode;
  var
    Stream: TStream;
  begin
    { Some formats readers require seeking capability.
      Testcase: e.g. PasGLTF does seeking,
      and without soForceMemoryStream reading glTF from Android assets (TReadAssetStream) would fail. }

    if ForceMemoryStream(MimeType) then
      Include(DownloadOptions, soForceMemoryStream);

    Stream := Download(UrlWithoutAnchor, DownloadOptions);
    try
      Result := LoadNode(Stream, Url, MimeType);
    finally FreeAndNil(Stream) end;
  end;

var
  Gzipped: boolean;
begin
  { We always download stripping anchor.
    Spine, sprite sheets (Starling, Cocos2d), images, Tiled all expect such anchor.
    Other model formats may support it as well in the future. }
  UrlWithoutAnchor := UriDeleteAnchor(Url);

  if HasNameCounter(Url, false) then
  begin
    Result := LoadSequenceUsingCounter(Url)
  end else
  begin
    MimeType := UriMimeType(Url, Gzipped);
    if Gzipped then
    begin
      Result := DownloadAndLoad([soGzip])
    end else
    try
      Result := DownloadAndLoad([])
    except
      { Some readers (only X3D for now, but maybe more will join in future)
        detect that stream is gzip-compressed and raise EGzipCompressed.

        In this case we reopen the file, creating new stream.
        It's a simple and working solution in practice -- it allows us to read X3D files
        compressed by gzip but without indicating this by file extension.

        Other ideas to implement it:

        1. Pipe the output always by gzip decompression.

           But this means additional overhead always,
           even though in 99% cases the input is not gzip-compressed.
           We already filter the input through some streams anyway
           (we already wrap TFileStream in TBufferedReadStream in case of typical X3D).

        2. Rewind the stream and only pipe it conditionally.

           But this forces the stream to be freely "seekable",
           not all stream implementations allow it. }
      on EGzipCompressed do
        Result := DownloadAndLoad([soGzip])
    end;
  end;
end;

function LoadNode(const Stream: TStream;
  BaseUrl: String; const MimeType: String): TX3DRootNode;
var
  ModelFormat: TModelFormat;
begin
  if FRegisteredModelFormats = nil then
    raise Exception.Create('No model formats registered, you try to LoadNode too early, before initialization of Castle Game Engine units that register model formats');

  { All internal loading functions may assume BaseUrl is absolute.

    E.g. from glTF loader:

    Absolute BaseUrl makes the later Document.RootPath calculation correct.
    Otherwise "InclPathDelim(ExtractFilePath(UriToFilenameSafe('my_file.gtlf')))"
    would result in '/' (accidentally making all TPasGLTF.TImage.Uri values
    relative to root directory on Unix). This was reproducible doing
    "castle-model-viewer my_file.gtlf" on the command-line.

    Also castle-model-converter assumes that passing "stdin.x3dv" means that "stdin.x3dv"
    file is in current working dir. Using AbsoluteUri(BaseUrl) correctly
    adds the current working dir to URL. }
  BaseUrl := AbsoluteUri(BaseUrl);

  ModelFormat := FRegisteredModelFormats.FindMimeType(MimeType);
  if ModelFormat = nil then
    raise Exception.CreateFmt('Unrecognized file type "%s" for scene with base URL "%s"', [
      MimeType,
      UriDisplay(BaseUrl)
    ]);

  if not Assigned(ModelFormat.OnLoad) then
    raise Exception.CreateFmt('Cannot load model format "%s"', [MimeType]);

  Result := ModelFormat.OnLoad(Stream, BaseUrl);

  Assert(Result <> nil);

  { Fix names after loading (from any format -- X3D, glTF can have collisions in names),
    to have non-unique names for accessing everything,
    e.g. EXPORT statements should use correct (non-unique) names
    to be IMPORTed.
    Testcase: x3d-tests/gltf_inlined/avocado_and_exports/avocado_imported.x3dv . }
  Result.InternalFixNodeNames;
end;

{ saving -------------------------------------------------------------------- }

procedure SaveNode(const Node: TX3DRootNode;
  const Url: String;
  const Generator: String;
  const Source: String);
var
  Stream: TStream;
begin
  Stream := UrlSaveStream(Url);
  try
    SaveNode(Node, Stream, UriMimeType(Url), Generator, Source);
  finally FreeAndNil(Stream) end;
end;

procedure SaveNode(const Node: TX3DRootNode;
  const Stream: TStream; const MimeType: String;
  const Generator: String;
  const Source: String);
var
  ModelFormat: TModelFormat;
begin
  if FRegisteredModelFormats = nil then
    raise Exception.Create('No model formats registered, you try to SaveNode too early, before initialization of Castle Game Engine units that register model formats');

  ModelFormat := FRegisteredModelFormats.FindMimeType(MimeType);
  if ModelFormat = nil then
    raise Exception.CreateFmt('Unrecognized file type "%s" for saving scene', [
      MimeType
    ]);

  if not Assigned(ModelFormat.OnSave) then
    raise Exception.CreateFmt('Cannot save model format "%s"', [MimeType]);

  ModelFormat.OnSave(Node, Stream, Generator, Source);
end;

{ file filters -------------------------------------------------------------- }

{ Common implementation of LoadScene_FileFilters and SaveNode_FileFilters. }
function SaveLoad_FileFilters(const Load: boolean): String;

  function AllExtensions: String;
  var
    ModelFormat: TModelFormat;
    Ext: string;
  begin
    Result := '';
    for ModelFormat in FRegisteredModelFormats do
      for Ext in ModelFormat.Extensions do
        Result := SAppendPart(Result, ';', '*' + Ext);
  end;

  function FormatExtensions(const ModelFormat: TModelFormat): String;
  var
    Ext: string;
  begin
    Result := '';
    for Ext in ModelFormat.Extensions do
      Result := SAppendPart(Result, ';', '*' + Ext);
  end;

var
  ModelFormat: TModelFormat;
  DefaultMark: String;
begin
  if FRegisteredModelFormats = nil then
    raise Exception.Create('No model formats registered, you try to build filters list too early, before initialization of Castle Game Engine units that register model formats');

  Result := 'All Files|*';

  if Load then
  begin
    { When loading, "All Scenes" is the default filter. }
    Result := Result + '|*All Scenes|' + AllExtensions;
  end;

  for ModelFormat in FRegisteredModelFormats do
  begin
    // exclude ModelFormat that cannot be loaded / saved
    if Load then
    begin
      if not Assigned(ModelFormat.OnLoad) then
        Continue;
    end else
    begin
      if not Assigned(ModelFormat.OnSave) then
        Continue;
    end;

    { When saving, the default filter is X3D XML. }
    DefaultMark := Iff((not Load) and (ModelFormat.MimeTypes[0] = 'model/x3d+xml'), '*', '');

    Result := Result + '|' +
      DefaultMark +
      ModelFormat.FileFilterName + '|' + FormatExtensions(ModelFormat);
  end;
end;

function LoadScene_FileFilters: String;
begin
  Result := SaveLoad_FileFilters(true);
end;

function SaveNode_FileFilters: String;
begin
  Result := SaveLoad_FileFilters(false);
end;

{ TModelFormat and friends -------------------------------------------------- }

constructor TModelFormat.Create;
begin
  inherited;
  MimeTypes := TCastleStringList.Create;
  MimeTypes.CaseSensitive := false; // makes TModelFormatList.FindMimeType ignore case
  Extensions := TCastleStringList.Create;
end;

destructor TModelFormat.Destroy;
begin
  FreeAndNil(MimeTypes);
  FreeAndNil(Extensions);
  inherited;
end;

function TModelFormatList.FindMimeType(const MimeType: string): TModelFormat;
begin
  for Result in Self do
    if Result.MimeTypes.IndexOf(MimeType) <> -1 then
      Exit;
  Result := nil;
end;

procedure RegisterModelFormat(const ModelFormat: TModelFormat);

  procedure CheckMimeTypesNotYetRegistered(const NewMimeTypes: TStrings);
  var
    NewMimeType: String;
    ExistingModelFormat: TModelFormat;
  begin
    for NewMimeType in NewMimeTypes do
    begin
      ExistingModelFormat := FRegisteredModelFormats.FindMimeType(NewMimeType);
      if ExistingModelFormat <> nil then
        raise Exception.CreateFmt('RegisterModelFormat: MIME type "%s" is already registered', [NewMimeType]);
    end;
  end;

var
  Ext: string;
begin
  // validate
  if ModelFormat.MimeTypes.Count = 0 then
    raise Exception.Create('RegisterModelFormat: ModelFormat.MimeTypes must be non-empty at registration');
  if ModelFormat.FileFilterName = '' then
    raise Exception.Create('RegisterModelFormat: ModelFormat.FileFilters must be non-empty at registration');
  if ModelFormat.Extensions.Count = 0 then
    raise Exception.Create('RegisterModelFormat: ModelFormat.Extensions must be non-empty at registration');
  for Ext in ModelFormat.Extensions do
    if not IsPrefix('.', Ext, false) then
      raise Exception.CreateFmt('RegisterModelFormat: ModelFormat.Extensions must start with a dot, but "%s" does not', [Ext]);

  if FRegisteredModelFormats = nil then
    FRegisteredModelFormats := TModelFormatList.Create(true);
  CheckMimeTypesNotYetRegistered(ModelFormat.MimeTypes);
  FRegisteredModelFormats.Add(ModelFormat);
end;

initialization
finalization
  FreeAndNil(FRegisteredModelFormats);
end.
