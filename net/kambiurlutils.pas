{
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ URL utilities. Not much for now, will be much more when handling URLs
  in VRML engine will be really implemented. }
unit KambiURLUtils;

interface

{ This extracts #anchor from URL. On input, URL contains full URL.
  On output, Anchor is removed from URL and saved in Anchor.
  If no #anchor existed, Anchor is set to ''. }
procedure URLExtractAnchor(var URL: string; out Anchor: string);

implementation

uses KambiStringUtils;

procedure URLExtractAnchor(var URL: string; out Anchor: string);
var
  HashPos: Integer;
begin
  HashPos := BackPos('#', URL);
  if HashPos <> 0 then
  begin
    Anchor := SEnding(URL, HashPos + 1);
    SetLength(URL, HashPos - 1);
  end;
end;

end.