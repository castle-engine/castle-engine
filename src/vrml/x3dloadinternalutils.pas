{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for converting other 3D model formats into VRML/X3D. }
unit X3DLoadInternalUtils;

interface

uses VectorMath, VRMLNodes;

const
  NiceCreaseAngle = DefaultVRML1CreaseAngle;

function ToVRMLName(const S: string): string;

{ Calculate best possible ambientIntensity. This is a float that tries to
  satisfy the equation AmbientColor = AmbientIntensity * DiffuseColor.
  Suitable for VRML 2.0/X3D Material.ambientIntensity (as there's no
  Material.ambientColor in VRML 2.0/X3D). }
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3Single): Single;
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4Single): Single;

{ Search harder for filename Base inside directory Path.
  Path must be absolute and contain the final PathDelim.
  Returns filename relative to Path.

  We prefer to return just Base, if it exists, or when no alternative exists.
  When Base doesn't exist but some likely alternative exists (e.g. with
  different case), we return it. }
function SearchTextureFileName(const Path, Base: string): string;

implementation

uses SysUtils, KambiStringUtils, EnumerateFiles, KambiWarnings;

function ToVRMLName(const s: string): string;
const
  { moglibysmy tu uzyc VRMLLexer.VRMLNameChars ktore podaje naprawde
    wszystkie dozwolone znaki w nazwie VRMLa. Ale, dla czytelnosci,
    lepiej jednak nie uzywac wszystkich dziwnych znakow z
    VRMLLexer.VRMLNameChars i ograniczyc sie do ponizszego zbioru znakow }
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

function SearchTextureFileName(const Path, Base: string): string;
var
  SomePathDelim: Integer;
  BaseShort: string;
begin
  try
    if SearchFileHard(Path, Base, Result) then
      Exit;

    { According to https://sourceforge.net/tracker/index.php?func=detail&aid=3305661&group_id=200653&atid=974391
      some archives expect search within textures/ subdirectory.
      Example on http://www.gfx-3d-model.com/2008/06/house-07/#more-445
      for Wavefront OBJ. }
    if SearchFileHard(Path + 'textures' + PathDelim, Base, Result) then
    begin
      Result := 'textures/' + Result;
      Exit;
    end;
    if SearchFileHard(Path + 'Textures' + PathDelim, Base, Result) then
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
      if SearchFileHard(Path + 'textures' + PathDelim, BaseShort, Result) then
      begin
        Result := 'textures/' + Result;
        Exit;
      end;
      if SearchFileHard(Path + 'Textures' + PathDelim, BaseShort, Result) then
      begin
        Result := 'Textures/' + Result;
        Exit;
      end;
    end;

  finally
    if Result <> Base then
      { Texture file found, but not under original name }
      OnWarning(wtMinor, 'Texture', Format('Exact texture filename "%s" not found, using instead "%s"',
        [Base, Result]));
  end;

  { default result if nowhere found }
  Result := Base;
end;

end.
