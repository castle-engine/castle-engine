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

{ Just like standard Now function, but works on all platforms supported by CGE.
  On Nintendo Switch the standard function will crash now. }
function CastleNow: TDateTime;

{ Just like standard Today function, but works on all platforms supported by CGE.
  On Nintendo Switch the standard function will crash now. }
function CastleToday: TDateTime;

{ Just like standard Yesterday function, but works on all platforms supported by CGE.
  On Nintendo Switch the standard function will crash now. }
function CastleYesterday: TDateTime;

{ Just like standard Tomorrow function, but works on all platforms supported by CGE.
  On Nintendo Switch the standard function will crash now. }
function CastleTomorrow: TDateTime;

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

function CastleToday: TDateTime;
begin
  Result := DateOf(CastleNow);
end;

function CastleYesterday: TDateTime;
begin
  Result := DateOf(CastleNow) - 1;
end;

function CastleTomorrow: TDateTime;
begin
  Result := DateOf(CastleNow) + 1;
end;

{$endif read_implementation}
