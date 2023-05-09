{%MainUnit ../castleutils.pas}
{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some UNIX helpers }

{$ifdef read_interface}

{ Return the symlink target path. Like Libc.ReadLink or FpReadLink,
  but more comfortable, it returns a normal Pascal string.
  @raises EOSError In case of any failure (non-existing FileName etc.) }
function CastleReadLink(const FileName: string): string;

{$endif read_interface}

{$ifdef read_implementation}

function CastleReadLink(const FileName: string): string;
var ReadLinkResult, NewLength: Integer;
begin
 NewLength := 100;
 repeat
  SetLength(Result, NewLength);
  ReadLinkResult := {$ifdef FPC} FpReadlink {$else} ReadLink {$endif}
    (PAnsiChar(FileName), Pointer(Result), NewLength);

  OSCheck( ReadLinkResult <> -1, 'readlink' );

  if ReadLinkResult < NewLength then
  begin
   SetLength(Result, ReadLinkResult);
   Exit;
  end;

  NewLength := NewLength * 2;
 until false;
end;

procedure InitializationOSSpecific;
begin
end;

procedure FinalizationOSSpecific;
begin
end;

{$endif read_implementation}