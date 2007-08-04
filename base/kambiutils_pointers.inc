{
  Copyright 2002-2005 Michalis Kamburelis.

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

{ Some helpers for pointer arithmetic, Get/FreeMem
  and for "raw memory" scanning (IsMemCharFilled). }

{$ifdef read_interface}

type
  {$define PTRINT_DEFINED}
  {$ifdef DELPHI} {$undef PTRINT_DEFINED} {$endif}
  {$ifdef VER1_0} {$undef PTRINT_DEFINED} {$endif}

  {$ifndef PTRINT_DEFINED}
  { Under Delphi and FPC 1.0.x my code can run only on i386. }
  PtrInt = LongInt;
  PtrUInt = LongWord;
  {$endif}

  { Unsigned integer with the same size as Pointer.
    This was my own name for the type that later was defined by FPC
    as PtrInt. }
  TPointerUInt = PtrUInt;
  { Signed integer with the same size as Pointer. }
  TPointerSInt = PtrInt;

{ Pointer arithmetic. Will wrap over if you add too much. Add may be negative.
  This function is just a shortcut for
    TPointerSInt(result) := TPointerSInt(p)+Add;
  (or with TPointerUInt) that removes any compiler warnings,
  any attempts to convert something to 64bit values and
  any Range/Overflow checks. }
function PointerAdd(p: pointer; add: integer): pointer;

{$ifdef DELPHI}
{ Postac funkcyjna GetMem. Pod Delphi oryginalne GetMem jest w System.
  Pod FPC GetMem funkcyjne JUZ jest zdefiniowane (wiec nie redefiniujemy
  go; nalezy tu na wszelki wypadek zauwazyc ze gdybysmy robili to tez pod FPC
  to GetMem powinnismy wziac z ObjPas) }
function GetMem(Size: integer): pointer; overload;
procedure GetMem(var P: Pointer; Size: Integer); overload;
{$endif}

function GetClearMem(Size: integer; ClearValue: byte {$ifdef DEFPARS}=0{$endif}): pointer; overload;
{ if p <> nil FreeMem(p) and p := nil. Safer version of FreeMem. }
procedure FreeMemNiling(var p: pointer);

{ zwraca -1 jesli cala pamiec Data (o rozmiarze Size) jest wypelniona charem
  AChar, wpp. zwraca 0..Size-1 oznaczajace numer pierwszego znaku roznego
  od AChar.

  IsMemCharFilled sprawdza to samo ale zwraca po prostu true (gdy cala
  pamiec jest = AChar) lub false wpp. }
function CheckIsMemCharFilled(const Data; Size: Integer; AChar: Char): Integer;
function IsMemCharFilled(const Data; Size: Integer; AChar: Char): boolean;

{ These work like @link(IsMemCharFilled).
  But they compare whole Words or DWords.
  Size in each case is the number of Words or DWords (*not* size in bytes),
  this is consequent with parameters to FillWord / FillDWord from FPC's RTL. }
function IsMemWordFilled(const Data; Size: Integer; Value: Word): boolean;
function IsMemDWordFilled(const Data; Size: Integer; Value: DWord): boolean;

{$ifdef DELPHI}
{ Compatibility with FPC }
procedure FillDWord(var X; Count: PtrInt; Value: DWord); 
{$endif}

{$endif read_interface}

{$ifdef read_implementation}

{$Include NoRQCheckBegin.inc}
function PointerAdd(p: pointer; Add: integer): pointer;
{ Ten kod moze powodowac falszywe alarmy co do rangeChecking / arith. overflow,
  dlatego powinien dzialac z R-,Q- }
begin
 TPointerSInt(result) := TPointerSInt(p)+Add;
end;
{$Include NoRQCheckEnd.inc}

{$ifdef DELPHI}
function GetMem(Size: integer): pointer;
begin System.GetMem(result, Size) end;

procedure GetMem(var P: Pointer; Size: Integer);
begin System.GetMem(P, Size) end;
{$endif}

function GetClearMem(Size: integer; clearValue: byte{=0}): pointer;
begin result := GetMem(Size); FillChar(result^,Size, clearvalue) end;

procedure FreeMemNiling(var p: pointer);
begin
 if p <> nil then begin FreeMem(p); p := nil end;
end;

function CheckIsMemCharFilled(const Data; Size: Integer; AChar: Char): Integer;
var i: Integer;
    p: PChar;
begin
 { TODO: hmm, taka optymalizacje jak w IsMemCharFilled moznaby tu zrobic,
   idac po 32 lub 16.}
 p := @Data;
 for i := 0 to Size-1 do
 begin
  if p^<>AChar then begin result := i; Exit end;
  Inc(p);
 end;
 result := -1;
end;

function IsMemCharFilled(const Data; Size: Integer; AChar: Char): boolean;
var p: Pointer;
    p32: PLongWord absolute p;
    p16: PWord absolute p;
    p8: PChar absolute p;
    Good32: LongWord;
    Good16: Word;
begin
 { prosto moznaby ta funkcje zrealizowac jako
   result := CheckIsMemCharFilled(Data, Size, AChar) = -1;
   Ale jest ona dosc niskopoziomowa i w zwiazku z tym chce zeby dzialala
   blyskawicznie nawet dla naprawde duzych Size.
   Patrz time testy w testUnits.Test_KambiUtils - zysk czasowy okolo 4x dla Size
   rzedu 10^2, 3,4 (naturalnie w pesymistycznym przypadku, dla danych
   np. losowych kazdy algorytm bardzo szybko znajduje niezgodnosc...) }

 p := @Data;

 if Size >= 4 then
 begin
  FillChar(Good32, SizeOf(Good32), AChar);
  repeat
   if p32^<>Good32 then begin result := false; Exit end;
   Dec(Size, 4);
   Inc(p32);
  until Size < 4;
 end;

 { teraz Size moze byc tylko 0, 1,2 lub 3. Czyli moze sie tam zmiescic tylko
   raz liczba 2 bajtowa, Word. }
 if Size >= 2 then
 begin
  FillChar(Good16, SizeOf(Good16), AChar);
  if p16^<>Good16 then begin result := false; Exit end;
  Dec(Size, 2);
  Inc(p16);
 end;

 { teraz Size moze byc tylko 0 lub 1 }
 result:=(Size = 0) or (p8^=AChar);
end;

function IsMemWordFilled(const Data; Size: Integer; Value: Word): boolean;
var i: Integer;
    P: PWord;
begin
 { TODO -- optimize this by using IsMemDWordFilled with 
   Value = Value or (Value shl 16). }
 P := @Data;
 for i := 1 to Size do
 begin
  if P^ <> Value then
  begin
   Result := false;
   Exit;
  end;
  Inc(P);
 end;
 Result := true;
end;

function IsMemDWordFilled(const Data; Size: Integer; Value: DWord): boolean;
var i: Integer;
    P: PDWord;
begin
 P := @Data;
 for i := 1 to Size do
 begin
  if P^ <> Value then
  begin
   Result := false;
   Exit;
  end;
  Inc(P);
 end;
 Result := true;
end;

{$ifdef DELPHI}
procedure FillDWord(var X; Count: PtrInt; Value: DWord);
var P: PDWord;
    i: Integer;
begin
 P := @X;
 for i := 1 to Count do
 begin
  P^ := Value;
  Inc(P);
 end;
end;
{$endif}

{$endif read_implementation}
