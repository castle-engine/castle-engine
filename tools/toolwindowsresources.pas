{
  Copyright 2014-2014 Michalis Kamburelis and FPC team.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on FPC packages/fpmkunit/src/fpmkunit.pp unit,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Windows rc and res stuff. }
unit ToolWindowsResources;

interface

uses CastleUtils, CastleStringUtils;

procedure GenerateWindowsResources(
  const Path, Name, ExecutableName: string; Author: string; const Version: string;
  const Icons: TCastleStringList);

implementation

uses SysUtils,
  CastleURIUtils, CastleWarnings,
  ToolUtils;

{ Make CamelCase with only safe characters (digits and letters). }
function MakeCamelCase(S: string): string;
var
  I: Integer;
begin
  S := SReplaceChars(S, AllChars - ['a'..'z', 'A'..'Z', '0'..'9'], ' ');
  Result := '';
  for I := 1 to Length(S) do
    if S[I] <> ' ' then
      if (I > 1) and (S[I - 1] <> ' ') then
        Result += S[I] else
        Result += UpCase(S[I]);
end;

procedure GenerateWindowsResources(
  const Path, Name, ExecutableName: string; Author: string; const Version: string;
  const Icons: TCastleStringList);
var
  VersionComponents: array [0..3] of Cardinal;

  function ReplaceMacros(const Source: string): string;
  var
    Patterns, Values: TCastleStringList;
    I: Integer;
    P: string;
  begin
    Patterns := nil;
    Values := nil;
    try
      Patterns := TCastleStringList.Create;
      Values := TCastleStringList.Create;
      Patterns.Add('VERSION_MAJOR');   Values.Add(IntToStr(VersionComponents[0]));
      Patterns.Add('VERSION_MINOR');   Values.Add(IntToStr(VersionComponents[1]));
      Patterns.Add('VERSION_RELEASE'); Values.Add(IntToStr(VersionComponents[2]));
      Patterns.Add('VERSION_BUILD');   Values.Add(IntToStr(VersionComponents[3]));
      Patterns.Add('NAME');            Values.Add(Name);
      Patterns.Add('AUTHOR');          Values.Add(Author);
      Patterns.Add('EXECUTABLE_NAME'); Values.Add(ExecutableName);
      // add CamelCase() replacements, add ${} around
      for I := 0 to Patterns.Count - 1 do
      begin
        P := Patterns[I];
        Patterns[I] := '${' + P + '}';
        Patterns.Add('${CamelCase(' + P + ')}');
        Values.Add(MakeCamelCase(Values[I]));
      end;
      Result := SReplacePatterns(Source, Patterns, Values, []);
    finally
      FreeAndNil(Patterns);
      FreeAndNil(Values);
    end;
  end;

const
  RcTemplate = {$I templates/automatic-windows-resources.rc.inc};
  ManifestTemplate = {$I templates/automatic-windows.manifest.inc};
var
  IcoPath, OutputRc, OutputManifest: string;
  VersionComponentsString: TCastleStringList;
  I: Integer;
  WindresOutput: string;
  WindresStatus: Integer;
begin
  { calculate version as 4 numbers, Windows resource/manifest stuff expect this }
  VersionComponentsString := CreateTokens(Version, ['.']);
  try
    for I := 0 to High(VersionComponents) do
      if I < VersionComponentsString.Count then
        VersionComponents[I] := StrToIntDef(Trim(VersionComponentsString[I]), 0) else
        VersionComponents[I] := 0;
  finally FreeAndNil(VersionComponentsString) end;

  if Author = '' then
    Author := 'Unknown Author';

  OutputRc := ReplaceMacros(RcTemplate);

  IcoPath := '';
  for I := 0 to Icons.Count - 1 do
    if ExtractFileExt(Icons[I]) = '.ico' then
    begin
      IcoPath := Icons[I];
      Break;
    end;
  if IcoPath <> '' then
    OutputRc := 'MainIcon ICON "' + IcoPath + '"' + NL + OutputRc else
    OnWarning(wtMinor, 'Windows Resources', 'Icon in format suitable for Windows (.ico) not found. Exe file will not have icon.');

  StringToFile(InclPathDelim(Path) + 'automatic-windows-resources.rc', OutputRc);

  OutputManifest := ReplaceMacros(ManifestTemplate);
  StringToFile(InclPathDelim(Path) + 'automatic-windows.manifest', OutputManifest);

  RunCommandIndirPassthrough(Path, 'windres',
    ['-i', 'automatic-windows-resources.rc', '-o', 'automatic-windows-resources.res'],
    WindresOutput, WindresStatus);
  if WindresStatus <> 0 then
    raise Exception.Create('windres failed, cannot create Windows resource');
end;

end.

