{
  Copyright 2003-2011 Michalis Kamburelis.

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
      see LoadVRML_FileFilters and LoadVRMLSequence_FileFilters.
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

uses VectorMath, SysUtils, VRMLNodes, X3DLoadInternalMD3,
  KambiUtils, Classes;

{ This guesses model format basing on ExtractFileExt(filename),
  then loads model converting it to VRML.

  @param(AllowStdIn If AllowStdIn and FileName = '-' then it will load
    a VRML file from StdInStream (using GetCurrentDir as WWWBasePath).) }
function LoadVRML(const filename: string;
  AllowStdIn: boolean = false): TX3DRootNode;

{ Deprecated name for LoadVRML. @deprecated }
function LoadAsVRML(const filename: string;
  AllowStdIn: boolean = false): TX3DRootNode;

const
  { File filters for files loaded by LoadVRML, suitable
    for TFileFilterList.AddFiltersFromString and
    TCastleWindowBase.FileDialog. }
  LoadVRML_FileFilters =
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

{ Load various model formats as animation expressed by VRML sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single VRML file (like VRML >= 2.0)
  this just loads them like LoadVRML, adding exactly one item
  to RootNodes.
  This guarantees that this function handles @italic(at least)
  the same model formats as LoadVRML --- but actually it may
  handle more.

  And indeed, it currently handles kanim, that
  is completely unrecognized by LoadVRML.

  This handles animations in kanim and MD3 formats.

  @param(RootNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TX3DNodeList.)

  @param(ATimes Sequence of time values.
    Pass here some created and empty instance of TSingleList.)
}
procedure LoadVRMLSequence(
  const FileName: string;
  AllowStdIn: boolean;
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

const
  { File filters for files loaded by LoadVRMLSequence, suitable
    for TFileFilterList.AddFiltersFromString and
    TCastleWindowBase.FileDialog. }
  LoadVRMLSequence_FileFilters =
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

uses VRMLAnimation, KambiClassUtils,
  X3DLoadInternalGEO, X3DLoadInternal3DS, X3DLoadInternalOBJ,
  X3DLoadInternalCollada;

function LoadVRML(const filename: string;
  AllowStdIn: boolean): TX3DRootNode;
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
  if AllowStdIn and (FileName = '-') then
    result := LoadVRMLClassic('-', true) else
  begin
    Ext := ExtractFileExt(filename);
    if Ext = '.gz' then
      Ext := ExtractFileExt(DeleteFileExt(FileName)) + Ext;
    case ArrayPosText(Ext, Extensions) of
      0: result := LoadGEO(filename);
      1: result := Load3DS(filename);
      2: result := LoadWavefrontOBJ(filename);
      3..9: result := LoadVRMLClassic(filename, false);
      10: Result := LoadMD3(FileName);
      11: Result := LoadCollada(FileName);
      12: Result := LoadX3DXml(FileName, false);
      13, 14: Result := LoadX3DXml(FileName, true);
      else raise Exception.CreateFmt(
        'Unrecognized file extension "%s" for 3D model file "%s"',
        [Ext, FileName]);
    end;
  end;
end;

function LoadAsVRML(const filename: string;
  AllowStdIn: boolean): TX3DRootNode;
begin
  Result := LoadVRML(FileName, AllowStdIn);
end;

procedure LoadVRMLSequence(const FileName: string;
  AllowStdIn: boolean;
  RootNodes: TX3DNodeList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

  procedure LoadKanim;
  var
    ModelFileNames: TStringList;
    I, J: Integer;
  begin
    ModelFileNames := TStringList.Create;
    try
      T3DPrecalculatedAnimationCore.LoadFromFileToVars(FileName, ModelFileNames, Times,
        ScenesPerTime, EqualityEpsilon, TimeLoop, TimeBackwards);

      Assert(ModelFileNames.Count = Times.Count);
      Assert(ModelFileNames.Count >= 1);

      { Now use ModelFileNames to load RootNodes }
      RootNodes.Count := ModelFileNames.Count;
      for I := 0 to ModelFileNames.Count - 1 do
      try
        RootNodes[I] := LoadVRML(ModelFileNames[I]);
      except
        for J := 0 to I - 1 do
          FPGObjectList_FreeAndNilItem(RootNodes, J);
        raise;
      end;
    finally FreeAndNil(ModelFileNames) end;
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

  Ext := ExtractFileExt(FileName);
  if SameText(Ext, '.kanim') then
    LoadKanim else
  if SameText(Ext, '.md3') then
    LoadMD3Sequence(FileName, RootNodes, Times, ScenesPerTime,
      EqualityEpsilon, TimeLoop, TimeBackwards) else
    LoadSingle(LoadVRML(FileName, AllowStdIn));
end;

end.
