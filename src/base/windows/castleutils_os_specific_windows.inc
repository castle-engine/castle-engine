{%MainUnit ../castleutils.pas}
{
  Copyright 2000-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some WinAPI helpers }

{$ifdef read_interface}

{ Get exe name through WinAPI GetModuleFileNameW.

  @exclude This is internal.

  Using ParamStr(0) on Windows is buggy in case the directory name contains local characters
  that cannot be expressed in current Windows locale.
  E.g. if parent directory name contains Chinese or Russian characters on Polish Windows.
  It seems it's because FPC uses GetModuleFileNameA for this.

  Using GetModuleFileNameW instead works cool.

  TODO: submit to FPC,
  https://trello.com/c/7Xfek9e4/15-submit-to-fpc-testcase-need-to-use-getmodulefilenamew-instead-of-getmodulefilenamea }
function ExeNameFromGetModule: String;

{$endif read_interface}

{$ifdef read_implementation}

procedure InitializationOSSpecific;
begin
end;

procedure FinalizationOSSpecific;
begin
end;

function ExeNameFromGetModule: String;
var
  S: UnicodeString;
begin
  SetLength(S, MAX_PATH);
  if GetModuleFileNameW(0, PWideChar(@S[1]), MAX_PATH) = 0 then
  begin
    // WritelnWarning('GetModuleFileNameW failed. We will use old method to determine ExeName, which will fail if parent directory contains local characters');
    Exit(ParamStr(0)); // fallback to old method
  end;
  SetLength(S, StrLen(PWideChar(S))); // It's only null-terminated after WinAPI call, set actual length for Pascal UnicodeString
  Result := UTF8Encode(S);
end;

{$endif read_implementation}
