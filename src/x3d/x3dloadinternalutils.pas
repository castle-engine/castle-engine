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

{ Utilities for converting other 3D model formats into VRML/X3D. }
unit X3DLoadInternalUtils;

{$I castleconf.inc}

interface

uses CastleVectors, X3DNodes;

const
  NiceCreaseAngle = DefaultVRML1CreaseAngle;

function ToX3DName(const S: string): string;

{ Calculate best possible ambientIntensity. This is a float that tries to
  satisfy the equation AmbientColor = AmbientIntensity * DiffuseColor.
  Suitable for VRML 2.0/X3D Material.ambientIntensity (as there's no
  Material.ambientColor in VRML 2.0/X3D). }
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3Single): Single;
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4Single): Single;

{ Search harder for file named Base inside directory of BaseUrl.
  BaseUrl must be an absolute URL, we will extract path from it.
  Returns URL relative to BaseUrl.

  Automatically uses FixRelativeUrl on Base, fixing backslashes to slashes,
  so assuming that this is an old 3D format and backslash should be interpreted
  as directory separator (like on Windows).

  We prefer to return just Base, if it exists, or when no alternative exists.
  When Base doesn't exist but some likely alternative exists (e.g. with
  different case), we return it. }
function SearchTextureFile(const BaseUrl: string; Base: string): string;

{ Convert backslashes to slashes. Use for formats where this interpretation
  of backslash (instead of %-encoding actual backslash, see
  https://sourceforge.net/p/castle-engine/tickets/21/ ) seems more common. }
function FixRelativeUrl(const URL: string): string;

implementation

uses SysUtils, CastleStringUtils, CastleFindFiles, CastleLog,
  URIParser, CastleURIUtils;

function ToX3DName(const s: string): string;
const
  { moglibysmy tu uzyc X3DLexer.VRMLNameChars ktore podaje naprawde
    wszystkie dozwolone znaki w nazwie VRMLa. Ale, dla czytelnosci,
    lepiej jednak nie uzywac wszystkich dziwnych znakow z
    X3DLexer.VRMLNameChars i ograniczyc sie do ponizszego zbioru znakow }
  VRMLNameChars = ['a'..'z','A'..'Z','0'..'9'];
  NonVRMLNameChars = AllChars - VRMLNameChars;
begin
  result := SReplaceChars(s, NonVRMLNameChars, '_')
end;

function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3Single): Single;
begin
  Result := 0;
  if not Zero(DiffuseColor[0]) then Result += AmbientColor[0] / DiffuseColor[0];
  if not Zero(DiffuseColor[1]) then Result += AmbientColor[1] / DiffuseColor[1];
  if not Zero(DiffuseColor[2]) then Result += AmbientColor[2] / DiffuseColor[2];
  Result /= 3;
end;

function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4Single): Single;
begin
  Result := AmbientIntensity(
    Vector3SingleCut(AmbientColor),
    Vector3SingleCut(DiffuseColor));
end;

function SearchTextureFile(const BaseUrl: string; Base: string): string;
var
  SomePathDelim: Integer;
  BaseShort, Path: string;
begin
  Path := ExtractURIPath(BaseUrl);
  Base := FixRelativeUrl(Base);

  try
    if SearchFileHard(Path, Base, Result) then
      Exit;

    { According to https://sourceforge.net/tracker/index.php?func=detail&aid=3305661&group_id=200653&atid=974391
      some archives expect search within textures/ subdirectory.
      Example on http://www.gfx-3d-model.com/2008/06/house-07/#more-445
      for Wavefront OBJ. }
    if SearchFileHard(Path + 'textures/', Base, Result) then
    begin
      Result := 'textures/' + Result;
      Exit;
    end;
    if SearchFileHard(Path + 'Textures/', Base, Result) then
    begin
      Result := 'Textures/' + Result;
      Exit;
    end;

    { Some invalid models place full (absolute) path inside texture filename.
      Try to handle it, by stripping path part (from any OS), and trying
      to match new name. }
    SomePathDelim := BackCharsPos(['/', '\'], Base);
    if SomePathDelim <> 0  then
    begin
      BaseShort := SEnding(Base, SomePathDelim + 1);

      if SearchFileHard(Path, BaseShort, Result) then
        Exit;
      if SearchFileHard(Path + 'textures/', BaseShort, Result) then
      begin
        Result := 'textures/' + Result;
        Exit;
      end;
      if SearchFileHard(Path + 'Textures/', BaseShort, Result) then
      begin
        Result := 'Textures/' + Result;
        Exit;
      end;
    end;

  finally
    if Result <> Base then
      { Texture file found, but not under original name }
      WritelnWarning('Texture', Format('Exact texture URL "%s" not found, using instead "%s"',
        [Base, Result]));
  end;

  { default result if nowhere found }
  Result := Base;
end;

function FixRelativeUrl(const URL: string): string;
begin
  Result := SReplaceChars(URL, '\', '/');
end;

end.
