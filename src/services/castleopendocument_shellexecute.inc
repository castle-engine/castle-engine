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
  ws: WideString;
begin
  Result := False;
  if AUrl = '' then Exit;

  {$ifdef FPC}
  ws := UTF8Decode(AUrl);
  {$else}
  ws := AUrl;
  {$endif}
  Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenUrl(APath);
end;

{$endif read_implementation}
