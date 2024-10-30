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
  Loads wit skin and animations.

  References:
  - Format spec: http://icculus.org/homepages/phaethon/q3a/formats/md3format.html
  - Another format spec: https://mino-git.github.io/rtcw-wet-blender-model-tools/md3.html
  - One sample implementation: https://github.com/edems96/MD3-Model---Animation-Viewer/tree/master/src
}
unit X3DLoadInternalMD3;

{$I castleconf.inc}

interface

implementation

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleVectors, X3DNodes, X3DLoad,
  CastleInternalNodeInterpolator, CastleQuaternions,
  CastleFilesUtils, CastleStringUtils, CastleBoxes, X3DLoadInternalUtils,
  X3DCameraUtils, CastleDownload, CastleUriUtils, CastleLog;

type
  EInvalidMD3 = class(Exception);

const
  GoodIdent: AnsiString = 'IDP3';

{$I x3dloadinternalmd3_structs.inc}
{$I x3dloadinternalmd3_surface.inc}
{$I x3dloadinternalmd3_animation.inc}
{$I x3dloadinternalmd3_converter.inc}

function LoadMD3(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Md3: TMD3Converter;
begin
  Md3 := TMD3Converter.Create(Stream, BaseUrl);
  try
    Result := Md3.ToX3D;
  finally FreeAndNil(Md3) end;
end;

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadMD3;
  ModelFormat.OnLoadForceMemoryStream := true;
  ModelFormat.MimeTypes.Add('application/x-md3');
  ModelFormat.FileFilterName := 'Quake 3 engine models (*.md3)';
  ModelFormat.Extensions.Add('.md3');
  RegisterModelFormat(ModelFormat);
end.
