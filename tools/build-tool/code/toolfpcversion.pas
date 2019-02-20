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
unit ToolFPCVersion;

interface

type
  TFPCVersion = class
    Major, Minor, Release: Integer;
    IsCodeTyphon: Boolean;
    function AtLeast(const AMajor, AMinor, ARelease: Integer): boolean;
    { Get FPC version by running "fpc -iV". }
    constructor Create;
    function ToString: String; override;
  end;

{ FPC version singleton, automatically created and destroyed in this unit. }
function FPCVersion: TFPCVersion;

implementation

uses SysUtils,
  CastleFilesUtils, CastleLog, CastleStringUtils,
  ToolUtils;

{ TFPCVersion ---------------------------------------------------------------- }

function TFPCVersion.AtLeast(const AMajor, AMinor, ARelease: Integer): boolean;
begin
  Result :=
      (AMajor < Major) or
    ( (AMajor = Major) and (AMinor < Minor) ) or
    ( (AMajor = Major) and (AMinor = Minor) and (ARelease <= Release) );
end;

constructor TFPCVersion.Create;
var
  FpcOutput, FpcExe, Token: string;
  FpcExitStatus, SeekPos: Integer;
begin
  inherited;

  FpcExe := FindExe('fpc');
  if FpcExe = '' then
    raise Exception.Create('Cannot find "fpc" program on $PATH. Make sure it is installed, and available on $PATH');
  MyRunCommandIndir(GetCurrentDir, FpcExe, ['-iV'], FpcOutput, FpcExitStatus);
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

  Writeln('FPC version: ' + ToString);
end;

function TFPCVersion.ToString: String;
begin
  Result := Format('%d.%d.%d', [Major, Minor, Release]);
end;

var
  FPCVersionCached: TFPCVersion;

function FPCVersion: TFPCVersion;
begin
  if FPCVersionCached = nil then
    FPCVersionCached := TFPCVersion.Create;
  Result := FPCVersionCached;
end;

end.
