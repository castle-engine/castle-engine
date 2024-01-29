{%MainUnit castleopendocument.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

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

{ OpenUrl and OpenDocument implementations
  that just make a warning that they are not implemented. }

{$ifdef read_implementation}

function OpenUrl(AUrl: String): Boolean;
begin
  WritelnWarning('Not implemented', 'OpenUrl not implemented on this platform');
  Result := false;
end;

function OpenDocument(APath: String): Boolean;
begin
  WritelnWarning('Not implemented', 'OpenDocument not implemented on this platform');
  Result := false;
end;

{$endif read_implementation}
