{
  Copyright 2002-2004 Michalis Kamburelis.

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

{ Some UNIX helpers }

{$ifdef read_interface}

{$ifdef USE_LIBC}
{ opakowanie na libc.strerror. Zazwyczaj argumentem bedzie ErrNo. }
function KamStrError(ErrNum:integer):string;

procedure checkIsProcInDLL(const dllname,procname:string);
{$endif USE_LIBC}

{ FileName is symlink, returns where FileName points.
  This is equivalent to readlink from libc,
  but it's much more comfortable since it returns result as string.
  Also, it's better than Unix.FpReadLink(pathstr) because Unix.FpReadLink
  1) is not available when USE_LIBC
  2) introduces 255 chars limit (ShortString) on result.

  Raises EKambiOSError in case of failure (non-existing FileName etc.) }
function KamReadLink(const FileName: string): string;

{$endif read_interface}

{$ifdef read_implementation}

{$ifdef USE_LIBC}
function KamStrError(ErrNum:integer):string;
var buf:array[0..79]of char;
    p:PChar;
begin
 {uzywam strerror_p zamiast strerror - ta proc. jest bezpieczna do uzycia
  w watkach. O ile dobrze rozumiem dzialanie strerror_r ona MOZE uzyc
  dostarczonego jej bufora buf (i wtedy zwroci @buf) ale nie musi
  (jesli stwierdzi ze nie ma watkow w programie ?). Wazne jest zeby uznac p
  a nie buf za wynik tej funkcji. }
 p:=strerror_r(ErrNum,buf,High(buf)+1);
 SetString(result,p,strlen(p));
end;

procedure checkIsProcInDLL(const dllname,procname:string);
var dll:pointer;
    err:PChar;
begin
 dll:=dlopen(PChar(dllname),RTLD_GLOBAL or RTLD_LAZY);
 check(dll<>nil, 'Can''t open dll named '+dllname+ ' : '+dlerror);

 dlerror;
 dlsym(dll,PChar(procname));
 err:=dlerror;
 check(err=nil, 'Can''t find function '+procname+' in dll named '+dllname+' : '+err);
 dlclose(dll);
end;
{$endif USE_LIBC}

function KamReadLink(const FileName: string): string;
var ReadLinkResult, NewLength: Integer;
begin
 NewLength := 100;
 repeat
  SetLength(Result, NewLength);
  ReadLinkResult :=
    {$ifdef USE_LIBC} Libc.readlink {$else} FpReadlink {$endif}
      (PChar(FileName), Pointer(Result), NewLength);

  KambiOSCheck( ReadLinkResult <> -1, 'readlink' );

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