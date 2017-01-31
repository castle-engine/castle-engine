{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Loading 3D models as X3D.)

  Every format except VRML/X3D is handled by converting it into X3D nodes graph.
  This allows to use our great X3D renderer, tools, saving to X3D and such,
  on every 3D model.

  Basic guide for adding a new 3D format:

  @unorderedList(
    @item(
      Particular formats are implemented inside various X3DLoadInternalXxx units.
      Implementation of this unit calls them. In the future,
      a mechanism that allows you to "register" an importer, without modifying
      this unit's implementation, may be done --- report if needed.)

    @item(3D formats are also listed in the file filters constants:
      see Load3D_FileFilters and Load3DSequence_FileFilters.
      Each format has a file filter to specifically choose this format,
      and also is added to the "All 3D models" filter.)

    @item(Enable view3dscene to associate with this file format on freedesktops
      (GNOME, and other following freedesktop.org specs). For this,

      1. Update view3dscene MIME database.
      Simply add appopriate element to ../../../view3dscene/desktop/view3dscene.xml.
      Format of that MIME xml file is self-explanatory.
      It's good idea to google first
      to search for standard MIME type for your model format (e.g. wikipedia
      shows mime types for formats).
      If none is found, just use application/x-???, where ??? is some short
      name for your format.

      2. After adding to MIME database, you want to also add format to
      ../../../view3dscene/desktop/view3dscene.desktop, to indicate that
      view3dscene handles this MIME type.

      3. Finally, also add this to ../../../view3dscene/desktop/install_thumbnailer.sh,
      so that GNOME nautilus thumbnailers for this MIME types can be installed.)

    @item(You probably also want to extend documentation.
      At least ../../../www/htdocs/view3dscene.php, it has a "Features" section that lists
      all supported 3D formats.)
  )
}
unit X3DLoad;

{$I castleconf.inc}

interface

uses CastleVectors, SysUtils, X3DNodes, X3DLoadInternalMD3,
  CastleUtils, Classes;

{ Load 3D model. Guess model format based on URL extension.
  VRML/X3D formats are loaded directly,
  other model formats are converted under the hood to VRML/X3D.

  URL is downloaded using CastleDownload unit.
  If you all you care about is loading normal files, then just pass
  a normal filename (absolute or relative to the current directory)
  as the URL parameter.

  @param(AllowStdIn If AllowStdIn and URL = '-' then it will load
    a VRML/X3D file from StdInStream (using current working directory
    as BaseUrl).) }
function Load3D(const URL: string;
  AllowStdIn: boolean = false;
  NilOnUnrecognizedFormat: boolean = false): TX3DRootNode;

const
  { File filters for files loaded by Load3D, suitable
    for TFileFilterList.AddFiltersFromString and TCastleWindowCustom.FileDialog. }
  Load3D_FileFilters =
  'All Files|*|' +
  '*All 3D models|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.kanim;*.castle-anim-frames;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo;*.json|' +
  'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
  { TODO:
    and X3D binary (*.x3db;*.x3db.gz)
  }
  'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
  'Castle Animation Frames (*.castle-anim-frames, *.kanim)|*.castle-anim-frames;*.kanim|' +
  'Collada (*.dae)|*.dae|' +
  'Inventor (*.iv)|*.iv|' +
  '3D Studio (*.3ds)|*.3ds|' +
  'Quake 3 engine models (*.md3)|*.md3|' +
  'Wavefront (*.obj)|*.obj|' +
  'Videoscape (*.geo)|*.geo|' +
  'Spine animation (*.json)|*.json';

{ Load various model formats as animation expressed by VRML/X3D sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single file (like VRML/X3D >= 2.0)
  we load them exactly like Load3D, adding exactly one item
  to KeyNodes.
  So this function handles @italic(at least) the same model formats as Load3D.

  Additionally, we load castle-anim-frames and MD3 formats to a sequence of frames.

  @param(KeyNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TX3DNodeList.)

  @param(KeyTimes Sequence of time values.
    Pass here some created and empty instance of TSingleList.)
}
procedure Load3DSequence(
  const URL: string;
  const AllowStdIn: boolean;
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean); deprecated 'use Load3D instead of Load3DSequence';

const
  { File filters for files loaded by Load3DSequence, suitable
    for TFileFilterList.AddFiltersFromString and TCastleWindowCustom.FileDialog. }
  Load3DSequence_FileFilters = Load3D_FileFilters deprecated 'use Load3D_FileFilters, and use Load3D instead of Load3DSequence';

const
  DefaultBakedAnimationSmoothness = 1;

var
  { A smoothness value for "baked" animations loaded from castle-anim-frames
    files. This is multiplied by the scenes_per_time value recorded
    in castle-anim-frames file (30 by default), and determines the number
    of extra frames we add to the baked animation (between key frames). }
  BakedAnimationSmoothness: Single = DefaultBakedAnimationSmoothness;

implementation

uses CastleClassUtils, CastleURIUtils,
  X3DLoadInternalGEO, X3DLoadInternal3DS, X3DLoadInternalOBJ,
  X3DLoadInternalCollada, X3DLoadInternalSpine,
  CastleInternalNodeInterpolator;

function Load3D(const URL: string;
  AllowStdIn, NilOnUnrecognizedFormat: boolean): TX3DRootNode;

  function LoadAnimFrames(const URL: string): TX3DRootNode;
  var
    Animations: TNodeInterpolator.TAnimationList;
  begin
    Animations := TNodeInterpolator.LoadAnimFramesToKeyNodes(URL);
    try
      Result := TNodeInterpolator.LoadToX3D(Animations);
    finally FreeAndNil(Animations) end;
  end;

  function LoadMD3(const URL: string): TX3DRootNode;
  var
    Animations: TNodeInterpolator.TAnimationList;
  begin
    Animations := LoadMD3Sequence(URL);
    try
      Result := TNodeInterpolator.LoadToX3D(Animations);
    finally FreeAndNil(Animations) end;
  end;

var
  MimeType: string;
  Gzipped: boolean;
begin
  if AllowStdIn and (URL = '-') then
    result := LoadX3DClassic(URL, true, false) else
  begin
    MimeType := URIMimeType(URL, Gzipped);

    if (MimeType = 'application/x-inventor') or
       (MimeType = 'model/vrml') or
       (MimeType = 'model/x3d+vrml') then
      Result := LoadX3DClassic(URL, false, Gzipped) else

    if MimeType = 'model/x3d+xml' then
      Result := LoadX3DXml(URL, Gzipped) else

    if MimeType = 'application/x-geo' then
      Result := LoadGEO(URL) else

    if MimeType = 'image/x-3ds' then
      Result := Load3DS(URL) else

    if MimeType = 'application/x-wavefront-obj' then
      Result := LoadWavefrontOBJ(URL) else

    if MimeType = 'model/vnd.collada+xml' then
      Result := LoadCollada(URL) else

    if (MimeType = 'application/json') or
       { For Spine, we will strip anchor in LoadSpine, so we can guess MIME
         based on URL without anchor too. Otherwise xxx.json#skinname
         would not be detected as Spine JSON.
         Note that we should not do this in URIMimeType implementation,
         as it depends on reader implementation whether anchor is understood
         (and stripped). }
       (URIMimeType(URIDeleteAnchor(URL, true), Gzipped) = 'application/json') then
      Result := LoadSpine(URL) else

    if MimeType = 'application/x-castle-anim-frames' then
      Result := LoadAnimFrames(URL) else

    if MimeType = 'application/x-md3' then
      Result := LoadMD3(URL) else

    if NilOnUnrecognizedFormat then
      Result := nil else
      raise Exception.CreateFmt(
        'Unrecognized file type "%s" for 3D model file "%s"',
        [MimeType, URIDisplay(URL)]);
  end;
end;

procedure Load3DSequence(const URL: string;
  const AllowStdIn: boolean;
  const KeyNodes: TX3DNodeList;
  const KeyTimes: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
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
    EqualityEpsilon := Animation.EqualityEpsilon;
    TimeLoop        := Animation.Loop;
    TimeBackwards   := Animation.Backwards;

    FreeAndNil(Animations);
  end;

  procedure LoadSingle(Node: TX3DNode);
  begin
    KeyNodes.Add(Node);
    KeyTimes.Add(0); { One time value }
    ScenesPerTime := 1;      { doesn't matter }
    EqualityEpsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }
  end;

var
  MimeType: string;
begin
  Assert(KeyTimes.Count = 0);
  Assert(KeyNodes.Count = 0);

  MimeType := URIMimeType(URL);

  if MimeType = 'application/x-castle-anim-frames' then
    LoadNodeAnimation(TNodeInterpolator.LoadAnimFramesToKeyNodes(URL))
  else
  if MimeType = 'application/x-md3' then
    LoadNodeAnimation(LoadMD3Sequence(URL))
  else
    LoadSingle(Load3D(URL, AllowStdIn));
end;

end.
