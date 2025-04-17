{%MainUnit castledownload.pas}
{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

procedure CheckFileAccessSafe(const Url: String);
begin
  if not ApplicationProperties._FileAccessSafe then
    WritelnWarning('Opening file "%s" before the Application.OnInitialize was called. ' +
      'This is not reliable on mobile platforms (Android, iOS). ' +
      'This usually happens if you open a file from the "initialization" section of a unit. ' +
      'You should do it in Application.OnInitialize instead.',
      [Url]);
end;

{ Extract MimeType from HTTP Content-Type.
  Returns empty string if Content-Type is empty (undefined). }
function ContentTypeToMimeType(const ContentType: string): string;
var
  P: Integer;
begin
  P := Pos(';', ContentType);
  if P <> 0 then
    Result := Trim(Copy(ContentType, 1, P - 1))
  else
    Result := Trim(ContentType);
end;

{$endif}
