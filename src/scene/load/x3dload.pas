{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Loading scenes as X3D nodes.)

  Every format except VRML/X3D is handled by converting it into X3D nodes graph.
  This allows to use our great X3D renderer, tools, saving to X3D and such,
  on every model.

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

    @item(Enable view3dscene to associate with this file format on freedesktops
      (GNOME, and other following freedesktop.org specs). For this,

      1. Update view3dscene MIME database.
      Simply add appopriate element to ../../../view3dscene/freedesktop/view3dscene.xml.
      Format of that MIME xml file is self-explanatory.
      It's good idea to google first
      to search for standard MIME type for your model format (e.g. wikipedia
      shows mime types for formats).
      If none is found, just use application/x-???, where ??? is some short
      name for your format.

      2. After adding to MIME database, you want to also add format to
      ../../../view3dscene/freedesktop/view3dscene.desktop, to indicate that
      view3dscene handles this MIME type.

      3. Finally, also add this to ../../../view3dscene/freedesktop/install_thumbnailer.sh,
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
  CastleUtils, CastleVectors, X3DNodes;

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
function LoadNode(const Url: string;
  const NilOnUnrecognizedFormat: boolean = false): TX3DRootNode; overload;

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
function LoadNode(const Stream: TStream; BaseUrl: String; const MimeType: String;
  const NilOnUnrecognizedFormat: boolean = false): TX3DRootNode; overload;

function Load3D(const Url: string;
  const AllowStdIn: boolean = false;
  const NilOnUnrecognizedFormat: boolean = false): TX3DRootNode; deprecated 'use LoadNode, and note it has one less parameter (AllowStdIn is not implemented anymore)';

const
  SaveX3D_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d)|*.x3d|' +
  'X3D XML (compressed) (*.x3dz, *.x3d.gz)|*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv)|*.x3dv|' +
  'X3D classic (compressed) (*.x3dvz, *.x3dv.gz)|*.x3dvz;*.x3dv.gz';

{ File filters for files loaded by @link(TCastleSceneCore.Load) and @link(LoadNode).
  Suitable for TFileFilterList.AddFiltersFromString and TCastleWindow.FileDialog. }
function LoadScene_FileFilters: String;

{ File filters for files loaded by @link(TCastleSceneCore.Load) and @link(LoadNode).
  Suitable for TFileFilterList.AddFiltersFromString and TCastleWindow.FileDialog. }
function Load3D_FileFilters: String; deprecated 'use LoadScene_FileFilters';

{ Load various model formats as animation expressed by VRML/X3D sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single file (like VRML/X3D >= 2.0)
  we load them exactly like LoadNode, adding exactly one item
  to KeyNodes.
  So this function handles @italic(at least) the same model formats as LoadNode.

  Additionally, we load castle-anim-frames and MD3 formats to a sequence of frames.

  @param(KeyNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TX3DNodeList.)

  @param(KeyTimes Sequence of time values.
    Pass here some created and empty instance of TSingleList.)
}
procedure Load3DSequence(
  const Url: string;
  const AllowStdIn: boolean;
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  out ScenesPerTime: Cardinal;
  out Epsilon: Single;
  out TimeLoop, TimeBackwards: boolean); deprecated 'use LoadNode instead of Load3DSequence';

{ File filters for files loaded by Load3DSequence, suitable
  for TFileFilterList.AddFiltersFromString and TCastleWindow.FileDialog. }
function Load3DSequence_FileFilters: String; deprecated 'use LoadScene_FileFilters, and use LoadNode instead of Load3DSequence';

const
  DefaultBakedAnimationSmoothness = 1;

var
  { A smoothness value for "baked" animations loaded from castle-anim-frames
    files. This is multiplied by the scenes_per_time value recorded
    in castle-anim-frames file (30 by default), and determines the number
    of extra frames we add to the baked animation (between key frames). }
  BakedAnimationSmoothness: Single = DefaultBakedAnimationSmoothness;

{ Save model to a file.
  Right now we only support saving to the X3D format,
  in classic (MIME type 'model/x3d+vrml') or XML encoding (MIME type 'model/x3d+xml').

  The overloaded version with explicit URL also automatically detects and handles
  gzip compression of the resulting file.

  @param(Generator Optional name, or short description, of the application
    generating this file. This value is not interpreted in any way,
    it is simply a "metadata" information we store in the resulting file.
  )

  @param(Source Optional name of the original file, if this file is a result of some
    conversion or transformation. This value is not interpreted in any way,
    it is simply a "metadata" information we store in the resulting file.
  )
}
procedure SaveNode(const Node: TX3DNode;
  const URL: String;
  const Generator: String = '';
  const Source: String = ''); overload;
procedure SaveNode(const Node: TX3DNode;
  const Stream: TStream; const MimeType: String;
  const Generator: String = '';
  const Source: String = ''); overload;

implementation

uses CastleClassUtils, CastleImages, CastleURIUtils, CastleStringUtils,
  X3DLoadInternalGEO, X3DLoadInternal3DS, X3DLoadInternalOBJ,
  X3DLoadInternalCollada, X3DLoadInternalSpine, X3DLoadInternalSTL,
  X3DLoadInternalMD3, X3DLoadInternalGLTF, X3DLoadInternalImage,
  X3DLoadInternalCocos2d, CastleInternalNodeInterpolator,
  CastleInternalSpritesheet, CastleDownload, X3DLoadInternalTiledMap;

{ Load a sequence of nodes to an animation suitable for TNodeInterpolator.
  Allows to read sequence of static models as an animation,
  e.g. Blender can export Wavefront OBJ like that. }
function LoadSequenceUsingCounter(const URL: string): TX3DRootNode;

  function LoadAnimationUsingCounter(const URL: string): TNodeInterpolator.TAnimationList;
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
      FrameUrl := FormatNameCounter(URL, FrameIndex, false);
      if not URIFileExists(FrameUrl) then
      begin
        FrameIndex := 1;
        FrameUrl := FormatNameCounter(URL, FrameIndex, false);
        if not URIFileExists(FrameUrl) then
          raise Exception.CreateFmt('First model in a sequence ("%s" or "%s") cannot be found', [
            FormatNameCounter(URL, 0, false),
            FormatNameCounter(URL, 1, false)
          ]);
      end;

      FirstFrameIndex := FrameIndex;

      repeat
        Animation.KeyNodes.Add(LoadNode(FrameUrl));
        Animation.KeyTimes.Add((FrameIndex - FirstFrameIndex) / FramesPerSecond);
        Inc(FrameIndex);
        FrameUrl := FormatNameCounter(URL, FrameIndex, false);
      until not URIFileExists(FrameUrl);
    except
      Result.FreeKeyNodesContents;
      FreeAndNil(Result);
      raise;
    end;
  end;

var
  Animations: TNodeInterpolator.TAnimationList;
begin
  Animations := LoadAnimationUsingCounter(URL);
  try
    Result := TNodeInterpolator.LoadToX3D(Animations);
  finally FreeAndNil(Animations) end;
end;

function LoadNode(const URL: string;
  const NilOnUnrecognizedFormat: boolean): TX3DRootNode;
var
  MimeType, URLWithoutAnchor: string;

  function DownloadAndLoad(DownloadOptions: TStreamOptions): TX3DRootNode;
  var
    Stream: TStream;
  begin
    { Some formats readers require seeking capability.
      Testcase: e.g. PasGLTF does seeking,
      and without soForceMemoryStream reading glTF from Android assets (TReadAssetStream) would fail. }
    if (MimeType = 'model/gltf+json') or
       (MimeType = 'model/gltf-binary') or
       (MimeType = 'application/x-md3') or
       (MimeType = 'image/x-3ds') or
       IsImageMimeType(MimeType, true, false) then
      Include(DownloadOptions, soForceMemoryStream);

    Stream := Download(URLWithoutAnchor, DownloadOptions);
    try
      Result := LoadNode(Stream, URL, MimeType, NilOnUnrecognizedFormat);
    finally FreeAndNil(Stream) end;
  end;

var
  Gzipped: boolean;
begin
  { We always download stripping anchor.
    Spine, sprite sheets (Starling, Cocos2d), images, Tiled all expect such anchor.
    Other model formats may support it as well in the future. }
  URLWithoutAnchor := URIDeleteAnchor(URL, true);

  if HasNameCounter(URL, false) then
  begin
    Result := LoadSequenceUsingCounter(URL)
  end else
  begin
    MimeType := URIMimeType(URL, Gzipped);
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
  BaseUrl: String; const MimeType: String;
  const NilOnUnrecognizedFormat: boolean = false): TX3DRootNode;

  function LoadAnimFrames(const Stream: TStream; const BaseUrl: string): TX3DRootNode;
  var
    Animations: TNodeInterpolator.TAnimationList;
  begin
    Animations := TNodeInterpolator.LoadAnimFramesToKeyNodes(Stream, BaseUrl);
    try
      Result := TNodeInterpolator.LoadToX3D(Animations);
    finally FreeAndNil(Animations) end;
  end;

begin
  { All internal loading functions may assume BaseUrl is absolute.

    E.g. from glTF loader:

    Absolute BaseUrl makes the later Document.RootPath calculation correct.
    Otherwise "InclPathDelim(ExtractFilePath(URIToFilenameSafe('my_file.gtlf')))"
    would result in '/' (accidentally making all TPasGLTF.TImage.URI values
    relative to root directory on Unix). This was reproducible doing
    "view3dscene my_file.gtlf" on the command-line. }
  BaseUrl := AbsoluteURI(BaseUrl);

  if (MimeType = 'application/x-inventor') or
     (MimeType = 'model/vrml') or
     (MimeType = 'model/x3d+vrml') then
    Result := LoadX3DClassicInternal(Stream, BaseUrl)
  else

  if MimeType = 'model/x3d+xml' then
    Result := LoadX3DXmlInternal(Stream, BaseUrl)
  else

  if MimeType = 'application/x-geo' then
    Result := LoadGEO(Stream, BaseUrl)
  else

  if MimeType = 'image/x-3ds' then
    Result := Load3DS(Stream, BaseUrl)
  else

  if MimeType = 'application/x-wavefront-obj' then
    Result := LoadWavefrontOBJ(Stream, BaseUrl)
  else

  if MimeType = 'model/vnd.collada+xml' then
    Result := LoadCollada(Stream, BaseUrl)
  else

  if MimeType = 'application/json' then
    Result := LoadSpine(Stream, BaseUrl)
  else

  if MimeType = 'application/x-castle-anim-frames' then
    Result := LoadAnimFrames(Stream, BaseUrl)
  else

  if MimeType = 'application/x-md3' then
    Result := LoadMD3(Stream, BaseUrl)
  else

  if (MimeType = 'application/x-stl') or
     { try also other STL mime types }
     (MimeType = 'application/wavefront-stl') or
     (MimeType = 'application/vnd.ms-pki.stl') or
     (MimeType = 'application/x-navistyle') then
    Result := LoadSTL(Stream, BaseUrl)
  else

  if (MimeType = 'model/gltf+json') or
     (MimeType = 'model/gltf-binary') then
    Result := LoadGLTF(Stream, BaseUrl)
  else

  if (MimeType = 'application/x-castle-sprite-sheet') or
     (MimeType = 'application/x-starling-sprite-sheet') then
    Result := LoadCastleSpriteSheet(Stream, BaseUrl)
  else

  if (MimeType = 'application/x-plist') or
     (MimeType = 'application/x-cocos2d-sprite-sheet') then
    Result := LoadCocos2d(Stream, BaseUrl)
  else

  if MimeType = 'application/x-tiled-map' then
    Result := LoadTiledMap2d(Stream, BaseUrl)
  else

  { Support for simple graphics images like PNG }
  if IsImageMimeType(MimeType, true, false) then
    Result := LoadImageAsNode(Stream, BaseUrl, MimeType)
  else

  if NilOnUnrecognizedFormat then
    Result := nil
  else
    raise Exception.CreateFmt('Unrecognized file type "%s" for scene with base URL "%s"',
      [MimeType, URIDisplay(BaseUrl)]);

  if Result <> nil then
  begin
    { Fix names after loading (from any format -- X3D, glTF can have collisions in names),
      to have non-unique names for accessing everything,
      e.g. EXPORT statements should use correct (non-unique) names
      to be IMPORTed.
      Testcase: x3d-tests/gltf_inlined/avocado_and_exports/avocado_imported.x3dv . }
    Result.InternalFixNodeNames;
  end;
end;

function LoadScene_FileFilters: String;
var
  ImageExtensions: String;
begin
  ImageExtensions := LoadImage_FileFilters.AllExtensions;

  Result :=   'All Files|*|' +
    '*All Scenes|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.kanim;*.castle-anim-frames;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo;*.json;*.stl;*.glb;*.gltf;*.castle-sprite-sheet;*.starling-xml;*.cocos2d-plist;*.plist;*.tmx;' + ImageExtensions + '|' +
    'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
    { TODO:
      and X3D binary (*.x3db;*.x3db.gz)
    }
    'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
    'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
    'Castle Animation Frames (*.castle-anim-frames, *.kanim)|*.castle-anim-frames;*.kanim|' +
    'glTF (*.glb, *.gltf)|*.glb;*.gltf|' +
    'Collada (*.dae)|*.dae|' +
    'Inventor (*.iv)|*.iv|' +
    '3D Studio (*.3ds)|*.3ds|' +
    'Quake 3 engine models (*.md3)|*.md3|' +
    'Wavefront (*.obj)|*.obj|' +
    'Videoscape (*.geo)|*.geo|' +
    'Spine animation (*.json)|*.json|' +
    'Standard Triangle Language (*.stl)|*.stl|' +
    'Castle Sprite Sheet (*.castle-sprite-sheet)|*.castle-sprite-sheet|' +
    'Starling Sprite Sheet (*.starling-xml)|*.starling-xml|' +
    'Cocos2d Sprite Sheet (*.cocos2d-plist, *.plist)|*.cocos2d-plist;*.plist|' +
    'Tiled Map (*.tmx)|*.tmx|' +
    { Uncomment to see version with extensions - but filter combo is very long then }
    //'Images (' + StringReplace(ImageExtensions, ';', ', ', [rfReplaceAll]) + ')|' + ImageExtensions;
    'Images |' + ImageExtensions;
end;

function Load3D_FileFilters: String;
begin
  Result := LoadScene_FileFilters;
end;

function Load3D(const URL: string;
  const AllowStdIn, NilOnUnrecognizedFormat: boolean): TX3DRootNode;
begin
  Result := LoadNode(URL, NilOnUnrecognizedFormat);
end;

procedure Load3DSequence(const URL: string;
  const AllowStdIn: boolean;
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  out ScenesPerTime: Cardinal;
  out Epsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

  procedure LoadNodeAnimation(Animations: TNodeInterpolator.TAnimationList);
  var
    Animation: TNodeInterpolator.TAnimation;
    I: Integer;
  begin
    { This obsolete routine just reads the 1st animation only.
      There's no way to support multiple animations with this interface. }
    Animation := Animations[0];

    for I := 0 to Animation.KeyNodes.Count - 1 do
      KeyNodes.Add(Animation.KeyNodes[I]);
    for I := 0 to Animation.KeyTimes.Count - 1 do
      KeyTimes.Add(Animation.KeyTimes[I]);
    ScenesPerTime   := Animation.ScenesPerTime;
    Epsilon         := Animation.Epsilon;
    TimeLoop        := Animation.Loop;
    TimeBackwards   := Animation.Backwards;

    FreeAndNil(Animations);
  end;

  procedure LoadSingle(Node: TX3DNode);
  begin
    KeyNodes.Add(Node);
    KeyTimes.Add(0); { One time value }
    ScenesPerTime := 1;      { doesn't matter }
    Epsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }
  end;

var
  MimeType: String;
  AbsoluteBaseUrl: String;
  Stream: TStream;
begin
  Assert(KeyTimes.Count = 0);
  Assert(KeyNodes.Count = 0);

  MimeType := URIMimeType(URL);

  if MimeType = 'application/x-castle-anim-frames' then
  begin
    AbsoluteBaseUrl := AbsoluteURI(URL);
    Stream := Download(URL);
    try
      LoadNodeAnimation(TNodeInterpolator.LoadAnimFramesToKeyNodes(Stream, AbsoluteBaseUrl));
    finally FreeAndNil(Stream) end;
  end else
    LoadSingle(LoadNode(URL));
end;

function Load3DSequence_FileFilters: String;
begin
  Result := LoadScene_FileFilters;
end;

procedure SaveNode(const Node: TX3DNode;
  const URL: String;
  const Generator: String;
  const Source: String);
begin
  {$warnings off} // using deprecated, it will be internal
  Save3D(Node, URL, Generator, Source);
  {$warnings on}
end;

procedure SaveNode(const Node: TX3DNode;
  const Stream: TStream; const MimeType: String;
  const Generator: String;
  const Source: String);
var
  Encoding: TX3DEncoding;
begin
  if (MimeType = 'model/vrml') or
     (MimeType = 'model/x3d+vrml') then
    Encoding := xeClassic
  else
    Encoding := xeXML;
  {$warnings off} // using deprecated, it will be internal
  Save3D(Node, Stream, Generator, Source, Encoding);
  {$warnings on}
end;

end.
