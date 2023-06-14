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
  CastleInternalNodeInterpolator, CastleQuaternions;

{ Load MD3 model, with skin and animations. }
function LoadMD3(const Stream: TStream; const BaseUrl: string): TX3DRootNode;

implementation

uses CastleFilesUtils, CastleStringUtils, CastleBoxes, X3DLoadInternalUtils,
  X3DCameraUtils, CastleDownload, CastleURIUtils, CastleLog;

type
  EInvalidMD3 = class(Exception);

const
  GoodIdent: AnsiString = 'IDP3';

{$I x3dloadinternalmd3_structs.inc}
{$I x3dloadinternalmd3_surface.inc}
{$I x3dloadinternalmd3_animation.inc}
{$I x3dloadinternalmd3_converter.inc}

function LoadMD3(const Stream: TStream; const BaseUrl: string): TX3DRootNode;
var
  Md3: TMD3Converter;
begin
  Md3 := TMD3Converter.Create(Stream, BaseUrl);
  try
    Result := Md3.ToX3D;
  finally FreeAndNil(Md3) end;
end;

end.
