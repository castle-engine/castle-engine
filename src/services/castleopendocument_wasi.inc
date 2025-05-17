{%MainUnit castleopendocument.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

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

{ OpenUrl and OpenDocument implementation for WebAssembly. }

{$ifdef read_uses}
CastleInternalJobWeb,
{$endif read_uses}

{$ifdef read_implementation}

function OpenUrl(AUrl: String): Boolean;
begin
  { Use JS window.open
    ( https://developer.mozilla.org/en-US/docs/Web/API/Window/open ). }
  Result := JSWindow.Open(AUrl, '_blank') <> nil;
  if not Result then
    WritelnWarning('OpenUrl (used with "%s") failed', [
      AUrl
    ]);
end;

function OpenDocument(APath: String): Boolean;
begin
  WritelnWarning('OpenDocument (used with "%s") not implemented for WebAssembly, only OpenUrl is available', [
    APath
  ]);
  Result := false;
end;

{$endif read_implementation}
