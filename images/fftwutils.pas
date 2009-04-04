{
  Copyright 2009 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ Helper utilities for using fftw unit. }
unit FftwUtils;

interface

{$MACRO on}
{$INLINE on}

{ Fixed versions of fftw_s get/freemem, see
  http://bugs.freepascal.org/view.php?id=13463 }

{ }
procedure kam_fftw_getmem(var p:pointer;size:sizeint);
procedure kam_fftw_freemem(p:pointer);inline;

implementation

procedure kam_fftw_getmem(var p:pointer;size:sizeint);

{$IFDEF align}
var
  originalptr:pointer;
begin
  { We allocate additional "align-1" bytes to be able to align.
    And we allocate additional "SizeOf(Pointer)" to always have space to store
    the value of the original pointer. }
  getmem(originalptr,size + align-1 + SizeOf(Pointer));
  ptruint(p):=(ptruint(originalptr) + SizeOf(Pointer));
  ptruint(p):=(ptruint(p)+align-1) and not (align-1);
  PPointer(ptruint(p) - SizeOf(Pointer))^:=originalptr;
{$ELSE}
begin
  getmem(p,size);
{$ENDIF}
end;

procedure kam_fftw_freemem(p:pointer);inline;

begin
{$IFDEF align}
  freemem(PPointer(ptruint(p) - SizeOf(Pointer))^);
{$ELSE}
  freemem(p);
{$ENDIF}
end;

end.