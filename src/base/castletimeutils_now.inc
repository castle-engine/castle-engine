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

{ Part of CastleTimeUtils unit: CastleNow. }

{$ifdef read_interface}

{ Just like standard Now functions, but works also on Nintendo Switch. }
function CastleNow: TDateTime;

{$endif read_interface}

{$ifdef read_implementation}

{$ifdef CASTLE_NINTENDO_SWITCH}
function CgeNxGetUnixTime(): QWord; cdecl; external;
{$endif}

function CastleNow: TDateTime;
{$ifdef CASTLE_NINTENDO_SWITCH}
begin
  Result := UnixToDateTime(CgeNxGetUnixTime());
{$else}
begin
  Result := Now;
{$endif}
end;

{$endif read_implementation}
