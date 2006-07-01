{
  Copyright 2001-2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Very basic operations on numbers.
  Also things related to setting 8087 Control Word. }

{$ifdef read_interface}

{ zamien wartosci a z b }
procedure SwapValues(var a, b: Int64); overload;
procedure SwapValues(var a, b: Integer); overload;
procedure SwapValues(var a, b: Cardinal); overload;
procedure SwapValues(var a, b: Single); overload;
procedure SwapValues(var a, b: Double); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure SwapValues(var a, b: Extended); overload;
{$endif}
procedure SwapValues(var a, b: char); overload;

{ Jesli nie jest mniejsza<=wieksza to zamien wartosciami dwie zmienne.
  Po wywolaniu tych proc. zawsze mniejsza<=wieksza }
procedure OrderUp(var mniejsza, wieksza: Int64   ); overload;
procedure OrderUp(var mniejsza, wieksza: Integer ); overload;
procedure OrderUp(var mniejsza, wieksza: Cardinal); overload;
procedure OrderUp(var mniejsza, wieksza: Single  ); overload;
procedure OrderUp(var mniejsza, wieksza: Double  ); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure OrderUp(var mniejsza, wieksza: Extended); overload;
{$endif}

{ laduje do mniejsza, wieksza wartosci x, y tak zeby mniejsza<=wieksza }
procedure OrderUp(x, y: Integer;  var mniejsza, wieksza: Integer ); overload;
procedure OrderUp(x, y: Cardinal; var mniejsza, wieksza: Cardinal); overload;
procedure OrderUp(x, y: Single;   var mniejsza, wieksza: Single  ); overload;
procedure OrderUp(x, y: Double;   var mniejsza, wieksza: Double  ); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure OrderUp(x, y: Extended; var mniejsza, wieksza: Extended); overload;
{$endif}

{ zwraca min/max z 2/3 elementów
  @noAutoLinkHere }
function min(const a, b: Int64): Int64; overload;
{ @noAutoLinkHere }
function min(const a, b: integer): integer; overload;
{ @noAutoLinkHere }
function min(const a, b: cardinal): cardinal; overload;
{ @noAutoLinkHere }
function min(const a, b: Single): Single; overload;
{ @noAutoLinkHere }
function min(const a, b: Double): Double; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
function min(const a, b: Extended): Extended; overload;
{$endif}

{ @noAutoLinkHere }
function min(const a, b, c: Int64): Int64; overload;
{ @noAutoLinkHere }
function min(const a, b, c: integer): integer; overload;
{ @noAutoLinkHere }
function min(const a, b, c: cardinal): cardinal; overload;
{ @noAutoLinkHere }
function min(const a, b, c: Single): Single; overload;
{ @noAutoLinkHere }
function min(const a, b, c: Double): Double; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
function min(const a, b, c: Extended): Extended; overload;
{$endif}

{ @noAutoLinkHere }
function max(const a, b: Int64): Int64; overload;
{ @noAutoLinkHere }
function max(const a, b: integer): integer; overload;
{ @noAutoLinkHere }
function max(const a, b: cardinal): cardinal; overload;
{ @noAutoLinkHere }
function max(const a, b: Single): Single; overload;
{ @noAutoLinkHere }
function max(const a, b: Double): Double; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
function max(const a, b: Extended): Extended; overload;
{$endif}

{ @noAutoLinkHere }
function max(const a, b, c: Int64): Int64; overload;
{ @noAutoLinkHere }
function max(const a, b, c: integer): integer; overload;
{ @noAutoLinkHere }
function max(const a, b, c: cardinal): cardinal; overload;
{ @noAutoLinkHere }
function max(const a, b, c: Single): Single; overload;
{ @noAutoLinkHere }
function max(const a, b, c: Double): Double; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
function max(const a, b, c: Extended): Extended; overload;
{$endif}

{ These work like @code(A := Min(A, B)).
  But they are a (very little) faster, since they avoid all the mem copying
  work when A is equal to Min(A, B).
  @noAutoLinkHere
  @groupBegin }
procedure MinTo1st(var a: Int64   ; const b: Int64   ); overload;
procedure MinTo1st(var a: Integer ; const b: Integer ); overload;
procedure MinTo1st(var a: Cardinal; const b: Cardinal); overload;
procedure MinTo1st(var a: Single  ; const b: Single  ); overload;
procedure MinTo1st(var a: Double  ; const b: Double  ); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure MinTo1st(var a: Extended; const b: Extended); overload;
{$endif}
{ @groupEnd }

{ These work like @code(A := Max(A, B)).
  But they are a (very little) faster, since they avoid all the mem copying
  work when A is equal to Max(A, B).
  @noAutoLinkHere
  @groupBegin }
procedure MaxTo1st(var a: Int64   ; const b: Int64   ); overload;
procedure MaxTo1st(var a: Integer ; const b: Integer ); overload;
procedure MaxTo1st(var a: Cardinal; const b: Cardinal); overload;
procedure MaxTo1st(var a: Single  ; const b: Single  ); overload;
procedure MaxTo1st(var a: Double  ; const b: Double  ); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure MaxTo1st(var a: Extended; const b: Extended); overload;
{$endif}
{ @groupEnd }

{ zwraca indeks (0, 1 lub 2) maximum / minimum}
function IndexMax(const a0, a1, a2: Double): Integer; overload;
function IndexMin(const a0, a1, a2: Double): Integer; overload;

{ @noAutoLinkHere }
function Between(const a, vBegin, vEnd: Int64): boolean; overload;
{ @noAutoLinkHere }
function Between(const a, vBegin, vEnd: integer): boolean; overload;
{ @noAutoLinkHere }
function Between(const a, vBegin, vEnd: cardinal): boolean; overload;
{ @noAutoLinkHere }
function Between(const a, vBegin, vEnd: Float): boolean; overload;

{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: Int64): Int64; overload;
{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: integer): integer; overload;
{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: cardinal): cardinal; overload;
{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: Single): Single; overload;
{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: Double): Double; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
function Clamped(const a, vBegin, vEnd: Extended): Extended; overload;
{$endif}

{ @noAutoLinkHere }
procedure Clamp(var a: Int64; const vBegin, vEnd: Int64); overload;
{ @noAutoLinkHere }
procedure Clamp(var a: integer; const vBegin, vEnd: integer); overload;
{ @noAutoLinkHere }
procedure Clamp(var a: cardinal; const vBegin, vEnd: cardinal); overload;
{ @noAutoLinkHere }
procedure Clamp(var a: Single; const vBegin, vEnd: Single); overload;
{ @noAutoLinkHere }
procedure Clamp(var a: Double; const vBegin, vEnd: Double); overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
{ @noAutoLinkHere }
procedure Clamp(var a: Extended; const vBegin, vEnd: Extended); overload;
{$endif}

{ RestOf3dCoords : idea jest taka : coord to numer wymiaru 3d, a wiec
  0, 1 lub 2. Chcemy do first i second wygenerowac pozostale dwa wymiary.
  A wiec coord=0 => (first, second) := (1, 2),
         coord=1 => (first, second) := (0, 2),
         coord=2 => (first, second) := (0, 1) }
procedure RestOf3dCoords(coord: integer; out first, second: integer);

{ zwraca value zwiekszone o change. change moze byc takze ujemne.
  Zmiany value beda sie zmieniac cyklicznie i w rezultacie zwrocony
  wynik bedzie zawsze w zakresie 0..maxValue. (nawet jesli poczatkowe
  value nie bedzie w tym zakresie) }
function ChangeIntCycle(value, change, maxValue: integer): integer;

{ (1-a) * l + a*h }
function Lerp(const a: Single; const l, h: Integer): Single; overload;
function Lerp(const a: Single; const l, h: Cardinal): Single; overload;
function Lerp(const a, l, h: Single): Single; overload;

const
  enatural = 2.71828182845905;
  sqrt2 = 1.4142135623730950488016887242097;
  { Pi/2. Taken from FPC sources, file rtl/inc/genmath.inc }
  HalfPi = 1.57079632679489661923;

{ RoundUpToMultiply zaokragla value do najmniejszej wielokrotnosci
  multiplicatora wiekszej lub rownej value. }
function RoundUpToMultiply(value, multiplicator: Integer): Integer;

{ BiggestPowerOf2 zwraca najwieksza potege 2 mniejsza lub rowna Value.
  Jezeli Value = 0 zwroci 0. }
function BiggestPowerOf2(Value: Cardinal): Cardinal;

{ j.w. tyle ze tutaj zwracamy ktora to jest potega 2 zamiast
  od razu ta potege dwojki. Bigget2Exponent(0) = -1. }
function Biggest2Exponent(Value: Cardinal): integer;

{ Smallest2Exponent zwraca najmniejsza liczbe taka ze
  2^Smallest2Exponent(Value) >= Value.

  Smallest2Exponent(0) = -1 (zeby zaznaczyc ze jest to sytuacja inna
  niz gdy Value=1, gdzie zwracamy 0). }
function Smallest2Exponent(Value: Cardinal): Integer;

{ Smallest2Power zwraca od razu 2^Smallest2Exponent(Value)
  (zwraca 0 jezeli Value=0).  }
function Smallest2Power(Value: Cardinal): Cardinal;

{ zwraca true <=> Value=1 or Value=2 or Value=4 itd. (az do max potegi
  dwojki <=High(cardinal) }
function IsPowerOf2(Value: Cardinal): boolean;

function DivRoundUp(Value, Divider: Cardinal): Cardinal; overload;
function DivRoundUp(Value, Divider: Integer): Integer; overload;

{ sourceVal jest wartoscia z zakresu sourceBegin..sourceEnd.

  Niech
@preformatted(
  a = odleglosc sourceVal od sourceBegin (czyli abs(sourceVal-sourceBegin))
  b = old. sourceVal od sourceEnd
)

  Chcemy znalezc liczbe taka zeby jej odleglosc do destBegin miala sie
  do jej odleglosci do destEnd tak samo jak a do b i zeby byla po tej samej
  stronie zakresu, tj.

@preformatted(
  jezeli sourceBegin<sourceEnd<sourceVal lub sourceBegin>sourceEnd>sourceVal
      to destBegin  <destEnd  <result    lub destBegin>  destEnd>  result
  jezeli sourceBegin<sourceVal<sourceEnd lub sourceBegin>sourceVal>sourceEnd
      to destBegin<  result<   destEnd   lub destBegin  >result   >destEnd
  jezeli sourceVal<sourceBegin<sourceEnd lub sourceVal>sourceBegin>sourceEnd
      to result<   destBegin<  destEnd   lub result   >destBegin>  destEnd
)

  Jak widac oba zakresy moga byc rosnace, moga byc oba malejace a moze byc
  jeden malejacy a jeden rosnacy - zawsze zadanie ma 1 wynik dzieki powyzszej
  definicji.

  @groupBegin }
function MapRange(sourceVal, sourceBegin, sourceEnd, destBegin, destEnd: integer): float; overload;
function MapRange(sourceVal, sourceBegin, sourceEnd, destBegin, destEnd: float  ): float; overload;
{ @groupEnd }

{ zwraca liczbe rzeczywista z przedzialu [RangeBegin, RangeEnd).
  Musi byc RangeBegin<RangeEnd. }
function RandomFloatRange(const RangeBegin, RangeEnd: Float): Float;

{ Kat miedzy odcinkiem x1, y1 - x2, y2 a osia OX. Kat 0 oznacza ze y1=y2 i x2>x1,
  potem kat rosnie w kierunku CCW. W radianach. }
function AngleRadPointToPoint(x1, y1, x2, y2: Single): Single;

{ gdy Base, Exponent i result sa wszystkie Cardinalami }
function NatNatPower(Base, Exponent: Cardinal): Cardinal;

{ zwraca losowo -1 lub +1 (oba z rowna szansa 50%) }
function RandomPlusMinus: integer;

{ For FPC 1.9.4 and older (e.g. 1.0.10):

  This function is needed because Math.Power works only for Base>=0.
  But when Exponent is Integer then it is possible to calculate Base^Exponent
  for any Base, even Base<0. This function implements this.

  I submitted this as FPC bug 3005, so this function should be equal
  to Math.Power when it will be fixed in newer FPC.

  For FPC 1.9.5 and newer:

  Bug 3005 is fixed, now this function is roughly equivalent
  to Math.Power. It is not *exactly* equivalent because I do
    if IsZero(Frac(Exponent)) then
     Result := IntPower(Base, Round(Exponent)) ...
  while FPC's Power does sthg like
    if Frac(Exponent) = 0.0 then
     Result := IntPower(Base, Trunc(Exponent)) ...
  so FPC's Power is slightly faster but also slightly more
  "radical" in deciding whether it's worth doing fallback on IntPower.
}
function GeneralPower(const Base, Exponent: Float): Float;

{ @groupBegin

  FPU Control Word :

  na podstawie Jcl8087 unit i JEDI code help - patrz Get8087ControlWord function.

  Control Word :

@preformatted(
  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
   ?  ?  ?| i| r  r| p  p| ?  ?| e  e  e  e  e  e

  i = infinity mode bits,
  r = round mode bits,
  p = precision mode bits,
  e = masked exception bits
      (bit = 1 means "ignore (don't raise) exceptions of that kind")
)

  Default control word in Delphi is $1332. Let's see what that value means :

@preformatted(
  $1332 =
  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
   0  0  0  1  0  0  1  1  0  0  1  1  0  0  1  0
   ?  ?  ?| i| r  r| p  p| ?  ?| e  e  e  e  e  e
  = F8087_Projective_Infinity or
    F8087_NearestOrEven_Round or
    F8087_Extended_Precision or
    F8087_PrecError_Except or
    F8087_Underflow_Except or
    F8087_DenormalizedOperand_Except;
)

  In Borland's help about Set8087CW routine they write "Set8087CW($133F)
  will mask all FPU exceptions" and now we see this is true - $133F it's
  Default8087CW or F8087_Except, so all exceptions get masked.

  FPC 1.0.x: looking at FPC SysWin32.pp unit I found out that FPC default FPU
  control word is $1332 too - look at procedure Exe_entry, they set there FPU
  control word to constant fpucw defined above as $1332.

  FPC 1.9.x: it's implemented in Math unit with fully Delphi-compatible
  Default8087CW constant and Get/Set8087CW functions. I copy&pasted that
  implementation to this file below, so that my units under FPC 1.0.10 can
  use those functions (it's especially required by OpenGL under Win32).

  Niepewne notki : o ile umiem zrozumiec kod Asemblera (a umiem
  bardzo slabo) procedury w RTL'u FPC sa ladnie napisane i
  powinny byc nieczule na zmiany mask exception FPU control word
  przez Set8087CW, za kazdym razem ustawiaja wlasne a potem odtwarzaja
  poprzednie.
  Procedury w RTL'u Borlanda chyba nie - zachowuja sie zgodnie z aktualnie
  ustawionymi flagami exception (chociaz modyfikuja chwilowo flagi precision
  lub rounding i pewnie infinity tez). }
const
  F8087_Except :Word = $3F;
  F8087_InvalidOp_Except :Word = 1;
  F8087_DenormalizedOperand_Except :Word = 1 shl 1;
  F8087_DivZero_Except :Word = 1 shl 2;
  F8087_Overflow_Except :Word = 1 shl 3;
  F8087_Underflow_Except :Word = 1 shl 4;
  F8087_PrecError_Except :Word = 1 shl 5;

  F8087_Precision :Word = 3 shl 8;
  F8087_Single_Precision :Word = 0;
  F8087_Reserved_Precision :Word = 1 shl 8;
  F8087_Double_Precision :Word = 2 shl 8;
  F8087_Extended_Precision :Word = 3 shl 8;

  F8087_Round :Word = 3 shl 10;
  F8087_NearestOrEven_Round :Word = 0;
  F8087_Down_Round :Word = 1 shl 10;
  F8087_Up_Round :Word = 2 shl 10;
  F8087_TowardZero_Round :Word = 3 shl 10;

  F8087_Infinity :Word = 1 shl 12;
  F8087_Projective_Infinity :Word = 0;
  F8087_Affine_Infinity :Word = 1 shl 12;
{ @groupEnd }

{$ifdef FPC}
{ }
function ArcCot(x: Float): Float;
{$endif FPC}

{ Yeah, this is this simplest factorial calculation to Int64.
  Everyone understands that it will quicly break at too large
  n values, right ? }
function SmallFactorial(n: Integer): Int64;

{ Better DivMod version, in case Dividend may be < 0.
  See fpc-devel thread "Math.DivMod results should be signed" on 2006-03-21. }
procedure KamDivMod(Dividend: Integer; Divisor: Word;
  out Result, Remainder: SmallInt);

{ This is a special version of DivMod that always returns Remainder >= 0.

  This is useful in case when Dividend < 0.
  Standard DivMod (and Pascal div and mod operators) return then
  Result rounded toward zero, and Remainder may be < 0.
  This procedure will return Result rounded toward negative infinity,
  and Remainder will always be >= 0. }
procedure DivUnsignedMod(Dividend: Integer; Divisor: Word;
  out Result: Smallint; var Remainder: Word);

{ Returns Ceil(A / B), but calculated faster and more precisely
  (without floating-point help). }
function CeilDiv(const A, B: Cardinal): Cardinal;

{$endif read_interface}

{$ifdef read_implementation}

procedure SwapValues(var a, b: Int64);    type TT = Int64;   {$I kambiutils_implement_swapvalues.inc}
procedure SwapValues(var a, b: Integer);  type TT = integer; {$I kambiutils_implement_swapvalues.inc}
procedure SwapValues(var a, b: Cardinal); type TT = Cardinal;{$I kambiutils_implement_swapvalues.inc}
procedure SwapValues(var a, b: Single);   type TT = Single;  {$I kambiutils_implement_swapvalues.inc}
procedure SwapValues(var a, b: Double);   type TT = Double;  {$I kambiutils_implement_swapvalues.inc}
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure SwapValues(var a, b: Extended); type TT = Extended;{$I kambiutils_implement_swapvalues.inc}
{$endif}
procedure SwapValues(var a, b: char);     type TT = char;    {$I kambiutils_implement_swapvalues.inc}

procedure OrderUp(var mniejsza, wieksza: Int64);    {$I KambiUtils_implement_OrderUp.inc}
procedure OrderUp(var mniejsza, wieksza: Integer);  {$I KambiUtils_implement_OrderUp.inc}
procedure OrderUp(var mniejsza, wieksza: Cardinal); {$I KambiUtils_implement_OrderUp.inc}
procedure OrderUp(var mniejsza, wieksza: Single);   {$I KambiUtils_implement_OrderUp.inc}
procedure OrderUp(var mniejsza, wieksza: Double);   {$I KambiUtils_implement_OrderUp.inc}
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure OrderUp(var mniejsza, wieksza: Extended); {$I KambiUtils_implement_OrderUp.inc}
{$endif}

procedure OrderUp(x, y: Integer; var mniejsza, wieksza: Integer);   {$I KambiUtils_implement_OrderUp4.inc}
procedure OrderUp(x, y: Cardinal; var mniejsza, wieksza: Cardinal); {$I KambiUtils_implement_OrderUp4.inc}
procedure OrderUp(x, y: Single; var mniejsza, wieksza: Single);     {$I KambiUtils_implement_OrderUp4.inc}
procedure OrderUp(x, y: Double; var mniejsza, wieksza: Double);     {$I KambiUtils_implement_OrderUp4.inc}
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure OrderUp(x, y: Extended; var mniejsza, wieksza: Extended); {$I KambiUtils_implement_OrderUp4.inc}
{$endif}

{ Int64 versions }
function min(const a, b: Int64): Int64; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: Int64): Int64; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: Int64): Int64; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: Int64): Int64; {$Include KambiUtils_implement_max3.inc}

{ integer versions }
function min(const a, b: integer): integer; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: integer): integer; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: integer): integer; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: integer): integer; {$Include KambiUtils_implement_max3.inc}

{ cardinal versions }
function min(const a, b: cardinal): cardinal; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: cardinal): cardinal; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: cardinal): cardinal; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: cardinal): cardinal; {$Include KambiUtils_implement_max3.inc}

{ Single, Double, Extended }
function min(const a, b: Single): Single; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: Single): Single; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: Single): Single; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: Single): Single; {$Include KambiUtils_implement_max3.inc}

function min(const a, b: Double): Double; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: Double): Double; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: Double): Double; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: Double): Double; {$Include KambiUtils_implement_max3.inc}

{$ifndef EXTENDED_EQUALS_DOUBLE}
function min(const a, b: Extended): Extended; {$Include KambiUtils_implement_min2.inc}
function max(const a, b: Extended): Extended; {$Include KambiUtils_implement_max2.inc}
function min(const a, b, c: Extended): Extended; {$Include KambiUtils_implement_min3.inc}
function max(const a, b, c: Extended): Extended; {$Include KambiUtils_implement_max3.inc}
{$endif}

procedure MinTo1st(var a: Int64   ; const b: Int64   ); begin if B < A then A := B end;
procedure MinTo1st(var a: Integer ; const b: Integer ); begin if B < A then A := B end;
procedure MinTo1st(var a: Cardinal; const b: Cardinal); begin if B < A then A := B end;
procedure MinTo1st(var a: Single  ; const b: Single  ); begin if B < A then A := B end;
procedure MinTo1st(var a: Double  ; const b: Double  ); begin if B < A then A := B end;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure MinTo1st(var a: Extended; const b: Extended); begin if B < A then A := B end;
{$endif}

procedure MaxTo1st(var a: Int64   ; const b: Int64   ); begin if B > A then A := B end;
procedure MaxTo1st(var a: Integer ; const b: Integer ); begin if B > A then A := B end;
procedure MaxTo1st(var a: Cardinal; const b: Cardinal); begin if B > A then A := B end;
procedure MaxTo1st(var a: Single  ; const b: Single  ); begin if B > A then A := B end;
procedure MaxTo1st(var a: Double  ; const b: Double  ); begin if B > A then A := B end;
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure MaxTo1st(var a: Extended; const b: Extended); begin if B > A then A := B end;
{$endif}

function IndexMax(const a0, a1, a2: Double): Integer;
begin
 if a0>a1 then
 begin
  if a2>a0 then result := 2 else result := 0
 end else
 begin
  if a2>a1 then result := 2 else result := 1
 end;
end;

function IndexMin(const a0, a1, a2: Double): Integer;
begin
 if a0<a1 then
 begin
  if a2<a0 then result := 2 else result := 0
 end else
 begin
  if a2<a1 then result := 2 else result := 1
 end;
end;

function Between(const a, vBegin, vEnd: Int64): boolean; {$I KambiUtils_implement_Between.inc}
function Between(const a, vBegin, vEnd: integer): boolean; {$I KambiUtils_implement_Between.inc}
function Between(const a, vBegin, vEnd: cardinal): boolean; {$I KambiUtils_implement_Between.inc}
function Between(const a, vBegin, vEnd: Float): boolean; {$I KambiUtils_implement_Between.inc}

function Clamped(const a, vBegin, vEnd: Int64): Int64; {$I KambiUtils_implement_Clamped.inc}
function Clamped(const a, vBegin, vEnd: integer): integer; {$I KambiUtils_implement_Clamped.inc}
function Clamped(const a, vBegin, vEnd: cardinal): cardinal; {$I KambiUtils_implement_Clamped.inc}
function Clamped(const a, vBegin, vEnd: Single): Single; {$I KambiUtils_implement_Clamped.inc}
function Clamped(const a, vBegin, vEnd: Double): Double; {$I KambiUtils_implement_Clamped.inc}
{$ifndef EXTENDED_EQUALS_DOUBLE}
function Clamped(const a, vBegin, vEnd: Extended): Extended; {$I KambiUtils_implement_Clamped.inc}
{$endif}

procedure Clamp(var a: Int64;    const vBegin, vEnd: Int64); {$I KambiUtils_implement_Clamp.inc}
procedure Clamp(var a: integer;  const vBegin, vEnd: integer); {$I KambiUtils_implement_Clamp.inc}
procedure Clamp(var a: cardinal; const vBegin, vEnd: cardinal); {$I KambiUtils_implement_Clamp.inc}
procedure Clamp(var a: Single;   const vBegin, vEnd: Single); {$I KambiUtils_implement_Clamp.inc}
procedure Clamp(var a: Double;   const vBegin, vEnd: Double); {$I KambiUtils_implement_Clamp.inc}
{$ifndef EXTENDED_EQUALS_DOUBLE}
procedure Clamp(var a: Extended; const vBegin, vEnd: Extended); {$I KambiUtils_implement_Clamp.inc}
{$endif}

procedure RestOf3dCoords(coord: integer; out first, second: integer);
begin
 if coord=0 then first := 1 else first := 0;
 if coord=2 then second := 1 else second := 2;
end;

function ChangeIntCycle(value, change, maxValue: integer): integer;
begin
 Inc(maxValue);
 result := (value+change) mod maxValue;
 {teraz wynik jest w zakresie -maxValue..maxValue gdzie maxValue to jest to
  oryginalne maxValue jakie dostalismy}
 if result<0 then result := result+maxValue;
end;

{ Lerp : l + (h-l) * a = l + a*h - a*l = (1-a) *l + a*h }
function Lerp(const a: Single; const l, h: Integer): Single;  begin result := l + (h-l) * a; end;
function Lerp(const a: Single; const l, h: Cardinal): Single; begin result := l + (h-l) * a; end;
function Lerp(const a, l, h: Single): Single;                 begin result := l + (h-l) * a; end;

function RoundUpToMultiply(value, multiplicator: Integer): Integer;
begin
 if value mod multiplicator=0 then
  result := value else
  result := ((value div multiplicator)+1)*multiplicator;
end;

function BiggestPowerOf2(Value: Cardinal): Cardinal;
begin
 { szukamy najwiekszej potegi dwojki <= Value.
   To proste na bitach : znajdz pierwszy od lewej (najstarszy) bit rowny 1.
   Reszte bitow na prawo wyzeruj i zwroc calosc jako result. }
 result := Cardinal(1 shl (SizeOf(Cardinal)*8-1));
 while (result<>0) and ((result and Value)=0) do result := result shr 1;
end;

function Biggest2Exponent(Value: Cardinal): integer;
begin
 result := -1;
 while Value<>0 do
  begin Inc(result); Value := Value shr 1 end;
end;

function Smallest2Exponent(Value: Cardinal): Integer;
begin
 if Value=0 then begin result := -1; Exit end;

 for result := SizeOf(Value)*8-1 downto 0 do
  if ((LongWord(1) shl result) and Value) <> 0 then Break;

 { Wiemy ze Value<>0 wiec zawsze z powyzszego for'a wyjdziemy breakiem.
   Teraz result okresla pozycje najbardziej znaczacej 1-ki w liczbie Value.
   Jezeli wszystkie pozostale bity sa rowne 0 (czyli Value=1 shl result)
   to result juz jest dobry. Wpp. zeby 2^result bylo >= Value musimy
   zwiekszyc result o 1.}
 if Value <> Cardinal(1) shl result then Inc(result);
end;

function Smallest2Power(Value: Cardinal): Cardinal;
var Exp: Integer;
begin
 Exp := Smallest2Exponent(Value);
 if Exp=-1 then
  result := 0 else
  result := 1 shl Exp;
end;

function IsPowerOf2(Value: Cardinal): boolean;
var i: Cardinal;
begin
 i := Cardinal(1 shl (SizeOf(Cardinal)*8-1));
 while (i<>0) do
 begin
  if i=Value then begin result := true; exit end;
  i := i shr 1;
 end;
 result := false;
end;

function DivRoundUp(Value, Divider: Cardinal): Cardinal; {$I KambiUtils_implement_DivRoundUp.inc}
function DivRoundUp(Value, Divider: Integer): Integer;   {$I KambiUtils_implement_DivRoundUp.inc}

function MapRange(sourceVal, sourceBegin, sourceEnd, destBegin, destEnd: integer): float;{$I KambiUtils_implement_MapRange.inc}
function MapRange(sourceVal, sourceBegin, sourceEnd, destBegin, destEnd: float  ): float;{$I KambiUtils_implement_MapRange.inc}

function RandomFloatRange(const RangeBegin, RangeEnd: Float): Float;
begin
 result := Random*(RangeEnd-RangeBegin) + RangeBegin;
end;

function AngleRadPointToPoint(x1, y1, x2, y2: Single): Single;
begin
 if y2>y1 then
 begin
  if SameValue(x1, x2) then
   result := Pi/2 else
  begin
   result := ArcTan( (y2-y1) / (x2-x1) );
   if result<0 then result := result +Pi;
  end;
 end else
 if y1>y2 then
 begin
  if SameValue(x1, x2) then
   result := Pi*3/2 else
  begin
   result := ArcTan( (y2-y1) / (x2-x1) );
   if result<0 then result := result +Pi;
   result := result +Pi;
  end;
 end else
 if x2>x1 then result := 0 else result := Pi;
end;

function NatNatPower(Base, Exponent: Cardinal): Cardinal;
var i : Cardinal;
begin
 result := 1;
 i := Exponent;
 while i>0 do
 begin
  while not Odd(i) do
  begin
   i := i shr 1;
   base := sqr(base);
  end;
  i := i-1;
  result := result*base;
 end;
end;

function RandomPlusMinus: integer;
const PlusMinus: array[0..1]of integer = (-1,+1);
begin
 result := PlusMinus[Random(2)];
end;

function GeneralPower(const Base, Exponent: Float): Float;
begin
 if Base<0 then
 begin
  if (Abs(Exponent) <= MaxInt) and IsZero(Frac(Exponent)) then
   Result := IntPower(Base, Round(Exponent)) else
   raise EInvalidArgument.Create('GeneralPower: Exponent must be'+
     ' (very close to) integer if Base<0 to calculate Power(Base, Exponent)');
 end else
  Result := Power(Base, Exponent);
end;

{$ifdef FPC}
function ArcCot(x: Float): Float;
begin
 result := ArcTan(1/x);
end;
{$endif FPC}

function SmallFactorial(n: Integer): Int64;
begin
 Result := 1;
 while n > 1 do begin Result := Result * n; Dec(n) end;
end;

procedure KamDivMod(Dividend: Integer; Divisor: Word;
  out Result, Remainder: SmallInt);
var
  UnsignedResult: Word absolute Result;
  UnsignedRemainder: Word absolute Remainder;
begin
  DivMod(Dividend, Divisor, UnsignedResult, UnsignedRemainder);
end;

procedure DivUnsignedMod(Dividend: Integer; Divisor: Word;
  out Result: Smallint; out Remainder: Word);
var
  SignedRemainder: SmallInt;
begin
  KamDivMod(Dividend, Divisor, Result, SignedRemainder);
  if SignedRemainder < 0 then
  begin
    Dec(Result);
    Remainder := Divisor + SignedRemainder;
  end else
    Remainder := SignedRemainder;
end;

function CeilDiv(const A, B: Cardinal): Cardinal;
var
  DivResult, ModResult: Cardinal;
begin
  { I would like to use DivMod, but it's limited to 16-bit values.
    DivMod(A, B, DivResult, ModResult); }
  DivResult := A div B;
  ModResult := A mod B;
  if ModResult = 0 then
    Result := DivResult else
    Result := DivResult + 1;
end;

{$endif read_implementation}