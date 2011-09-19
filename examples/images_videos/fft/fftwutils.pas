{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helper utilities for using fftw unit. }
unit FftwUtils;

interface

{ Fixed versions of fftw_s get/freemem, see
  http://bugs.freepascal.org/view.php?id=13463 }

{ }
procedure kam_fftw_getmem(var p:pointer;size:sizeint);
procedure kam_fftw_freemem(p:pointer);

implementation

{$macro on}

{$IF defined(cpui386) or defined(cpupowerpc)}
  {$DEFINE align:=16}
{$ENDIF}

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

procedure kam_fftw_freemem(p:pointer);

begin
{$IFDEF align}
  freemem(PPointer(ptruint(p) - SizeOf(Pointer))^);
{$ELSE}
  freemem(p);
{$ENDIF}
end;

end.