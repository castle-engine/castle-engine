{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Detecting FPC version and some capabilities. }
unit ToolFpcVersion;

interface

type
  TFpcVersion = class
    Major, Minor, Release: Integer;
    IsCodeTyphon: Boolean;
    function AtLeast(const AMajor, AMinor, ARelease: Integer): boolean;
    { Get FPC version by running "fpc -iV". }
    constructor Create;
    function ToString: String; override;
  end;

{ FPC version singleton, automatically created and destroyed in this unit.
  Using this raises exception EExecutableNotFound if FPC cannot be found,
  or any other Exception in case FPC cannot be executed
  or version cannot be parsed.
  Value of this may change when FpcCustomPath changes. }
function FpcVersion: TFpcVersion;

implementation

uses SysUtils,
  CastleFilesUtils, CastleLog, CastleStringUtils,
  ToolCommonUtils, ToolCompilerInfo;

{ TFpcVersion ---------------------------------------------------------------- }

function TFpcVersion.AtLeast(const AMajor, AMinor, ARelease: Integer): boolean;
begin
  Result :=
      (AMajor < Major) or
    ( (AMajor = Major) and (AMinor < Minor) ) or
    ( (AMajor = Major) and (AMinor = Minor) and (ARelease <= Release) );
end;

constructor TFpcVersion.Create;
var
  FpcOutput, FpcExe, Token: string;
  FpcExitStatus, SeekPos: Integer;
begin
  inherited;

  FpcExe := FindExeFpcCompiler;
  MyRunCommandIndir(GetCurrentDir, FpcExe, ['-iV'], FpcOutput, FpcExitStatus,
    // use rcNoConsole to not blink with a console when CGE editor starts
    nil, nil, [rcNoConsole]);
  if FpcExitStatus <> 0 then
    raise Exception.Create('Failed to query FPC version');

  IsCodeTyphon := Pos('codetyphon', LowerCase(FpcExe)) > 0;

  { parse output into 3 numbers }
  FpcOutput := Trim(FpcOutput);
  SeekPos := 1;

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no major version in response "%s"', [FpcOutput]);
  Major := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
    raise Exception.CreateFmt('Failed to query FPC version: no minor version in response "%s"', [FpcOutput]);
  Minor := StrToInt(Token);

  Token := NextToken(FpcOutput, SeekPos, ['.', '-']);
  if Token = '' then
  begin
    WritelnWarning('FPC', 'Invalid FPC version: Failed to query FPC version: no release version in response "%s", assuming 0', [FpcOutput]);
    Release := 0;
  end else
    Release := StrToInt(Token);

  WritelnVerbose('FPC version: ' + ToString);
end;

function TFpcVersion.ToString: String;
begin
  Result := Format('%d.%d.%d', [Major, Minor, Release]);
end;

var
  FpcVersionCached: TFpcVersion;
  FpcVersionCachedFpcPath: String;

function FpcVersion: TFpcVersion;
begin
  { Invalidate cache if FpcCustomPath changed.
    FpcCustomPath is used by FpcPathExe which is used by TFpcVersion.Create. }
  if FpcCustomPath <> FpcVersionCachedFpcPath then
    FreeAndNil(FpcVersionCached);
  if FpcVersionCached = nil then
  begin
    FpcVersionCached := TFpcVersion.Create;
    FpcVersionCachedFpcPath := FpcCustomPath;
  end;
  Result := FpcVersionCached;
end;

finalization
  FreeAndNil(FpcVersionCached);

end.
