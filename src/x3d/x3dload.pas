{
  Copyright 2003-2013 Michalis Kamburelis.

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
      At least view3dscene.php, it has a "Features" section that lists
      all supported 3D formats.)
  )
}
unit X3DLoad;

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
    a VRML/X3D file from StdInStream (using GetCurrentDir as BaseUrl).) }
function Load3D(const URL: string;
  AllowStdIn: boolean = false;
  NilOnUnrecognizedFormat: boolean = false): TX3DRootNode;

const
  { File filters for files loaded by Load3D, suitable
    for TFileFilterList.AddFiltersFromString and
    TCastleWindowBase.FileDialog. }
  Load3D_FileFilters =
  'All Files|*|' +
  '*All 3D models|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo|' +
  'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
  { TODO:
    and X3D binary (*.x3db;*.x3db.gz)
  }
  'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
  'Collada (*.dae)|*.dae|' +
  'Inventor (*.iv)|*.iv|' +
  '3D Studio (*.3ds)|*.3ds|' +
  'Quake 3 engine models (*.md3)|*.md3|' +
  'Wavefront (*.obj)|*.obj|' +
  'Videoscape (*.geo)|*.geo';

{ Load various model formats as animation expressed by VRML/X3D sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single file (like VRML/X3D >= 2.0)
  we load them exactly like Load3D, adding exactly one item
  to RootNodes.
  So this function handles @italic(at least) the same model formats as Load3D.

  Additionally:

  @unorderedList(
    @item(We load kanim format. It can only be loaded to a sequence of files,
      so Load3D cannot deal with it at all.)

    @item(We load MD3 format in a little different (usually more efficient)
      way: to a sequence of frames. In comparison, Load3D loads MD3
      into an interpolated animation.)
  )

  @param(RootNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TX3DNodeList.)

  @param(ATimes Sequence of time values.
    Pass here some created and empty instance of TSingleList.)
}
procedure Load3DSequence(
  const URL: string;
  AllowStdIn: boolean;
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

const
  { File filters for files loaded by Load3DSequence, suitable
    for TFileFilterList.AddFiltersFromString and
    TCastleWindowBase.FileDialog. }
  Load3DSequence_FileFilters =
  'All Files|*|' +
  '*All 3D models|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.kanim;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo|' +
  'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
  { TODO:
    X3D binary (*.x3db;*.x3db.gz)
  }
  'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
  'Castle Game Engine animations (*.kanim)|*.kanim|' +
  'Collada (*.dae)|*.dae|' +
  'Inventor (*.iv)|*.iv|' +
  '3D Studio (*.3ds)|*.3ds|' +
  'Quake 3 engine models (*.md3)|*.md3|' +
  'Wavefront (*.obj)|*.obj|' +
  'Videoscape (*.geo)|*.geo';

implementation

uses CastlePrecalculatedAnimationCore, CastleClassUtils,
  X3DLoadInternalGEO, X3DLoadInternal3DS, X3DLoadInternalOBJ,
  X3DLoadInternalCollada;

function Load3D(const URL: string;
  AllowStdIn, NilOnUnrecognizedFormat: boolean): TX3DRootNode;
const
  GzExt = '.gz';
  Extensions: array [0..14] of string =
  ('.geo', '.3ds', '.obj',
   '.iv',
   '.wrl', '.wrl' + GzExt, '.wrz',
   '.x3dv', '.x3dv' + GzExt, '.x3dvz',
   '.md3', '.dae',
   '.x3d', '.x3dz', '.x3d' + GzExt);
var
  Ext: string;
begin
  if AllowStdIn and (URL = '-') then
    result := LoadX3DClassic(URL, true) else
  begin
    { TODO-net: file operations on URLs }
    Ext := ExtractFileExt(URL);
    if Ext = '.gz' then
      Ext := ExtractFileExt(DeleteFileExt(URL)) + Ext;
    case ArrayPosText(Ext, Extensions) of
      0: result := LoadGEO(URL);
      1: result := Load3DS(URL);
      2: result := LoadWavefrontOBJ(URL);
      3..9: result := LoadX3DClassic(URL, false);
      10: Result := LoadMD3(URL);
      11: Result := LoadCollada(URL);
      12: Result := LoadX3DXml(URL, false);
      13, 14: Result := LoadX3DXml(URL, true);
      else
        if NilOnUnrecognizedFormat then
          Result := nil else
          raise Exception.CreateFmt(
            'Unrecognized file extension "%s" for 3D model file "%s"',
            [Ext, URL]);
    end;
  end;
end;

function LoadAsVRML(const filename: string;
  AllowStdIn: boolean): TX3DRootNode;
begin
  Result := Load3D(FileName, AllowStdIn);
end;

procedure Load3DSequence(const URL: string;
  AllowStdIn: boolean;
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

  procedure LoadKanim;
  var
    ModelURLs: TStringList;
    I, J: Integer;
  begin
    ModelURLs := TStringList.Create;
    try
      TCastlePrecalculatedAnimationCore.LoadFromFileToVars(URL, ModelURLs, Times,
        ScenesPerTime, EqualityEpsilon, TimeLoop, TimeBackwards);

      Assert(ModelURLs.Count = Times.Count);
      Assert(ModelURLs.Count >= 1);

      { Now use ModelURLs to load RootNodes }
      RootNodes.Count := ModelURLs.Count;
      for I := 0 to ModelURLs.Count - 1 do
      try
        RootNodes[I] := Load3D(ModelURLs[I]);
      except
        for J := 0 to I - 1 do
          FPGObjectList_FreeAndNilItem(RootNodes, J);
        raise;
      end;
    finally FreeAndNil(ModelURLs) end;
  end;

  procedure LoadSingle(Node: TX3DNode);
  begin
    RootNodes.Add(Node);
    Times.Add(0); { One time value }
    ScenesPerTime := 1;      { doesn't matter }
    EqualityEpsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }
  end;

var
  Ext: string;
begin
  Assert(Times.Count = 0);
  Assert(RootNodes.Count = 0);

  { TODO-net: file operations on URLs }
  Ext := ExtractFileExt(URL);
  if SameText(Ext, '.kanim') then
    LoadKanim else
  if SameText(Ext, '.md3') then
    LoadMD3Sequence(URL, RootNodes, Times, ScenesPerTime,
      EqualityEpsilon, TimeLoop, TimeBackwards) else
    LoadSingle(Load3D(URL, AllowStdIn));
end;

end.
