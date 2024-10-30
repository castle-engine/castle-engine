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

{ Just like standard Randomize function, but works on all platforms supported by CGE.
  On Nintendo Switch the standard function will crash now.
  Note that you seldom need to call this explicitly: CastleUtils calls it already
  in the initialization. }
procedure CastleRandomize;

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

procedure CastleRandomize;
{$ifdef CASTLE_NINTENDO_SWITCH}

  procedure XorShift64(var c64: QWord);
  begin
    c64 := c64 xor (c64 shl 12);
    c64 := c64 xor (c64 shr 25);
    c64 := c64 xor (c64 shl 27);
  end;

var
  NewRandSeed: QWord;
begin
  NewRandSeed := CgeNxGetUnixTime();
  XorShift64(NewRandSeed);
  XorShift64(NewRandSeed);
  XorShift64(NewRandSeed);
  XorShift64(NewRandSeed);
  {$I norqcheckbegin.inc}
  RandSeed := UInt32(NewRandSeed);
  {$I norqcheckend.inc}
{$else}
begin
  // On NX this crashes with "invalid system call", because it accesses clock.
  Randomize;
{$endif}
end;

{$endif read_implementation}
