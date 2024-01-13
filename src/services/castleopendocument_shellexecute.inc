{%MainUnit castleopendocument.pas}
{
  Copyright 2012-2024 Michalis Kamburelis and Lazarus developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenUrl and OpenDocument implementations for Windows, using ShellExecute. }

{$ifdef read_uses}
Windows,
{$ifndef FPC} ShellApi, {$endif}
{$endif}

{$ifdef read_implementation}

{ Based on Lazarus LCL: lcl/include/sysenvapis_win.inc }

function OpenUrl(AUrl: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws: WideString;
  ans: AnsiString;
{$ENDIF}
begin
  Result := False;
  if AUrl = '' then Exit;

  {$IFDEF WinCE}
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI;
  Info.lpVerb := 'open';
  Info.lpFile := PWideChar(UTF8Decode(AUrl));
  Result := ShellExecuteEx(@Info);
  {$ELSE}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    {$ifdef FPC}
    ws := UTF8Decode(AUrl);
    {$else}
    ws := AUrl;
    {$endif}
    Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
  end
  else
  begin
    {$ifdef FPC}
    ans := Utf8ToAnsi(AUrl); // utf8 must be converted to Windows Ansi-codepage
    {$else}
    ans := AnsiString(AUrl);
    {$endif}
    Result := ShellExecuteA(0, 'open', PAnsiChar(ans), nil, nil, SW_SHOWNORMAL) > 32;
  end;
  {$ENDIF}
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenUrl(APath);
end;

{$endif read_implementation}
