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

{ Some helpers for pointer arithmetic, Get/FreeMem
  and for "raw memory" scanning (IsMemCharFilled). }

{$ifdef read_interface}

{ Pointer arithmetic. Will wrap over if you add too much. Add may be negative.
  This is just a shortcut for @code(PtrInt(Result) := PtrInt(P)+Add),
  without any range / overflow checks. }
function PointerAdd(p: pointer; add: integer): pointer;

function GetClearMem(Size: integer; ClearValue: byte = 0): pointer; overload;

{ Safer version of FreeMem, checks is parameter @nil, and sets it to @nil
  afterwards. }
procedure FreeMemNiling(var p: pointer);

{ Check is memory filled with the given character.
  Returns -1 is @true, or the number (between 0 .. Size - 1)
  or the first character different than AChar. }
function CheckIsMemCharFilled(const Data; Size: Integer; AChar: Char): Integer;

{ Check is memory filled with the given character.
  Returns simple boolean, use CheckIsMemCharFilled to get more informative
  result. }
function IsMemCharFilled(const Data; Size: Integer; AChar: Char): boolean;

{ Check is memory filled with the Word (2 byte sequence).
  Size if the number of words (not bytes), this is consequent
  with Size parameter from FillWord from FPC's RTL. }
function IsMemWordFilled(const Data; Size: Integer; Value: Word): boolean;

{ Check is memory filled with the DWord (4 byte sequence).
  Size if the number of dwords (not bytes), this is consequent
  with Size parameter from FillDWord from FPC's RTL. }
function IsMemDWordFilled(const Data; Size: Integer; Value: DWord): boolean;

{ Calculate shift between A and B addresses (in bytes).
  Useful to pass as offsets to methods like TGLSLAttribute.EnableArray
  that wrap OpenGL vertex-array API.
  This is simply Result := A - B, except we do some typecasting. }
function OffsetUInt(var A, B): PtrUInt;
function Offset(var A, B): Pointer;

{ Compare two methods. }
function CompareMethods(const AMethod1, AMethod2: TMethod): boolean; deprecated 'use SameMethods';
function SameMethods(const AMethod1, AMethod2: TMethod): boolean;

{$endif read_interface}

{$ifdef read_implementation}

{$include norqcheckbegin.inc}
function PointerAdd(p: pointer; Add: integer): pointer;
{ Ten kod moze powodowac falszywe alarmy co do rangeChecking / arith. overflow,
  dlatego powinien dzialac z R-,Q- }
var
  { PtrInt aliases for pointer types, to allow for
    addition on them with Add (signed) without any warnings. }
  ResultInt: PtrInt absolute Result;
  PInt: PtrInt absolute P;
begin
  ResultInt := PInt + Add;
end;
{$include norqcheckend.inc}

function GetClearMem(Size: integer; clearValue: byte{=0}): pointer;
begin result := GetMem(Size); FillChar(result^,Size, clearvalue) end;

procedure FreeMemNiling(var p: pointer);
begin
  {$ifdef VER3_3} try {$endif}
  if p <> nil then begin FreeMem(p); p := nil end;
  {$ifdef VER3_3}
  except
    on E: EAccessViolation do
    begin
      if IsConsole then
        Writeln(ErrOutput, 'WARNING: FreeMem failed with EAccessViolation, observed bug of FPC 3.3.1 when reading some (but not all) OggVorbis files. TODO: reproduce in simple testcase (right now: play_sounds on Linux/x86_64) and report. As a workaround, we just continue.');
      Exit;
    end;
  end;
  {$endif}
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
   Patrz time testy w TestCastleUtils - zysk czasowy okolo 4x dla Size
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

function OffsetUInt(var A, B): PtrUInt;
begin
  Result := PtrUInt(@A) - PtrUInt(@B);
end;

function Offset(var A, B): Pointer;
begin
  { additional PtrUInt typecast before Pointer, to avoid warning. }
  Result := Pointer(PtrUInt( PtrUInt(@A) - PtrUInt(@B) ));
end;

function CompareMethods(const AMethod1, AMethod2: TMethod): boolean;
begin
  Result := SameMethods(AMethod1, AMethod2);
end;

function SameMethods(const AMethod1, AMethod2: TMethod): boolean;
begin
  Result := (AMethod1.Code = AMethod2.Code) and
            (AMethod1.Data = AMethod2.Data);
end;

{$endif read_implementation}
