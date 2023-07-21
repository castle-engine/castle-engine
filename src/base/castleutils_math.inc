{%MainUnit castleutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Basic operations on numbers. }

{$ifdef read_interface}

{ Swap variables values.
  @groupBegin }
procedure SwapValues(var a, b: Byte    ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: QWord   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure SwapValues(var a, b: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
procedure SwapValues(var a, b: char    ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure SwapValues(var a, b: Pointer ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{ @groupEnd }

{ Make sure the Smaller value is <= than the Larger value,
  by eventually swapping them.
  @groupBegin }
procedure OrderUp(var Smaller, Larger: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(var Smaller, Larger: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(var Smaller, Larger: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(var Smaller, Larger: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(var Smaller, Larger: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure OrderUp(var Smaller, Larger: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
{ @groupEnd }

{ Assign the smaller value from X, Y to Smaller variable, the other one
  to Larger variable.
  @groupBegin }
procedure OrderUp(x, y: Int32 ; var Smaller, Larger: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(x, y: UInt32; var Smaller, Larger: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(x, y: Single; var Smaller, Larger: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure OrderUp(x, y: Double; var Smaller, Larger: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure OrderUp(x, y: Extended; var Smaller, Larger: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
{ @groupEnd }

{ Update value of A to be a minimum of A, B.
  Works like @code(A := Min(A, B)), but is marginally faster,
  since you don't have to do anything when A is already smaller.
  @groupBegin }
procedure MinVar(var a: Word    ; const b: Word    ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: Int64   ; const b: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: Int32   ; const b: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: UInt32  ; const b: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: Single  ; const b: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: Double  ; const b: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure MinVar(var a: Extended; const b: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
{ @groupEnd }

{ Update value of A to be a maximum of A, B.
  Works like @code(A := Max(A, B)), but is marginally faster,
  since you don't have to do anything when A is already larger.
  @groupBegin }
procedure MaxVar(var a: Word    ; const b: Word    ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: Int64   ; const b: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: Int32   ; const b: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: UInt32  ; const b: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: Single  ; const b: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: Double  ; const b: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure MaxVar(var a: Extended; const b: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
{ @groupEnd }

{$ifndef FPC}
{$ifndef MSWINDOWS}
// TODO: why is this necessary on Linux with Delphi? What is Integer, Cardinal then?
procedure MaxVar(var a: LongInt ; const b: LongInt ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: LongInt ; const b: LongInt ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MaxVar(var a: LongWord; const b: LongWord); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure MinVar(var a: LongWord; const b: LongWord); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}
{$endif}

type
  { Index of axis in 3D. }
  T3DAxis = 0..2;

{ Index (0, 1 or 2) of maximum / minimum of 3 numbers.
  @groupBegin }
function IndexMax(const a0, a1, a2: Double): T3DAxis; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function IndexMin(const a0, a1, a2: Double): T3DAxis; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{ @groupEnd }

function Between(const a, vBegin, vEnd: Int64   ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Between(const a, vBegin, vEnd: Int32   ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Between(const a, vBegin, vEnd: UInt32  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Between(const a, vBegin, vEnd: Float   ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Between(const a, vBegin, vEnd: Char    ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}

function RoundClamp255(const A: Single): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif}

function Clamped(const a, vBegin, vEnd: Int64   ): Int64   ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Clamped(const a, vBegin, vEnd: QWord   ): QWord   ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Clamped(const a, vBegin, vEnd: Int32   ): Int32   ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Clamped(const a, vBegin, vEnd: UInt32  ): UInt32  ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Clamped(const a, vBegin, vEnd: Single  ): Single  ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Clamped(const a, vBegin, vEnd: Double  ): Double  ; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
function Clamped(const a, vBegin, vEnd: Extended): Extended; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}

procedure ClampVar(var a: Int64   ; const vBegin, vEnd: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure ClampVar(var a: QWord   ; const vBegin, vEnd: QWord   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure ClampVar(var a: Int32   ; const vBegin, vEnd: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure ClampVar(var a: UInt32  ; const vBegin, vEnd: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure ClampVar(var a: Single  ; const vBegin, vEnd: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
procedure ClampVar(var a: Double  ; const vBegin, vEnd: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$ifdef EXTENDED_OVERLOADS}
procedure ClampVar(var a: Extended; const vBegin, vEnd: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{$endif}

procedure Clamp(var a: Int64   ; const vBegin, vEnd: Int64   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
procedure Clamp(var a: Int32   ; const vBegin, vEnd: Int32   ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
procedure Clamp(var a: UInt32  ; const vBegin, vEnd: UInt32  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
procedure Clamp(var a: Single  ; const vBegin, vEnd: Single  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
procedure Clamp(var a: Double  ; const vBegin, vEnd: Double  ); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
{$ifdef EXTENDED_OVERLOADS}
procedure Clamp(var a: Extended; const vBegin, vEnd: Extended); overload; {$ifdef SUPPORTS_INLINE} inline; {$endif} deprecated 'Use ClampVar';
{$endif}

{ Remaining 3D coordinates (0, 1 or 2) except the given coordinate, in the ascending order.

  E.g. Coord = 0, then it sets First = 1 and Second = 2.

  The coordinates are in ascending order,
  so e.g. for Coord = 1 we return First = 0, Second = 2. }
procedure RestOf3DCoords(const Coord: T3DAxis; out First, Second: T3DAxis);

{ Remaining 3D coordinates (0, 1 or 2) except the given coordinate, in order to cycle.

  E.g. Coord = 0, then it sets First = 1 and Second = 2.

  The coordinates are in order to cycle, so First = (Coord + 1) mod 3,
  Second = (Coord + 2) mod 3.
  E.g. for Coord = 1 we return First = 2, Second = 0. }
procedure RestOf3DCoordsCycle(const Coord: T3DAxis; out First, Second: T3DAxis);

{ Increase Value by Change, nicely wrapping in [0..MaxValue], accepting
  also negative Change. The final value is always in the [0..MaxValue] range,
  even when initial Value was outside. }
function ChangeIntCycle(Value, Change, MaxValue: Integer): Integer;

{ Linear interpolation between two values. Returns @code((1-A)*L + A*H).
  @groupBegin }
function Lerp(const a: Single; const l, h: Int32   ): Single; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Lerp(const a: Single; const l, h: UInt32  ): Single; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Lerp(const a, l, h: Single): Single;                 overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Lerp(const a: Double; const l, h: Int32   ): Double; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Lerp(const a: Double; const l, h: UInt32  ): Double; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function Lerp(const a, l, h: Double): Double;                 overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{ @groupEnd }

{ Linear interpolation between 2 angle values, in radians.
  Goes from one value to the other through a shortest path.
  Values are interpreted "modulo 2 * Pi",
  i.e. there's no difference between Angle1 = 0.1,
  Angle1 = 2 * Pi + 0.1, Angle1 = -2 * Pi + 0.1 etc. }
function AngleLerp(T, Angle1, Angle2: Single): Single;

const
  ENatural = 2.71828182845905;
  Sqrt2 = 1.4142135623730950488016887242097;
  Sqrt3 = 1.7320508075688773;
  { Pi / 2. }
  HalfPi = 1.57079632679489661923;
  { Pi * 2. }
  Pi2 = 6.28318530717958647703;

{ Smallest multiple of Multiplicator that is still >= Value. }
function RoundUpToMultiply(value, multiplicator: Integer): Integer;

{ Largest power of 2 still <= Value.
  When Value = 0 returns 0. }
function BiggestPowerOf2(Value: Cardinal): Cardinal;

{ Exponent of the largest power of 2 that it's still <= Value.
  Like BiggestPowerOf2, except that this returns which power of 2 it is,
  while BiggestPowerOf2 just returns exactly this power.
  Biggest2Exponent(0) = -1. }
function Biggest2Exponent(Value: Cardinal): Integer;

{ Smallest exponent such that 2^this exponent is >= Value.
  Smallest2Exponent(0) = -1 (this is different situation than
  Smallest2Exponent(1), when we return 0. }
function Smallest2Exponent(Value: Cardinal): Integer;

{ Smallest power of 2 that is >= Value.
  Like Smallest2Exponent, except here we return 2^Smallest2Exponent(Value).
  Returns 0 when Value = 0. }
function Smallest2Power(Value: Cardinal): Cardinal;

{ Check is Value an exact power of 2. }
function IsPowerOf2(Value: Cardinal): boolean;

function DivRoundUp(const Value, Divider: Cardinal): Cardinal; overload;
function DivRoundUp(const Value, Divider: Integer): Integer; overload;

{ Linearly map value from a one range to another.

  Consider how SourceVal is placed in the [SourceBegin .. SourceEnd]
  (it may be outside of this range, it all still works OK).
  It has some distance to range start (SourceBegin), and some distance
  to range end (SourceEnd).

  We want to find another number, that is identically placed in another range
  [DestBegin .. DestEnd].

  @orderedList(
    @item(Such that @italic(Distance(SourceVal,
      SourceBegin) / Distance(SourceVal, SourceEnd) =
      Distance(Result, DestBegin) / Distance(Result, DestEnd)).)

    @item(And such that it's on the same side of the range.
      So if SourceVal is beyond SourceEnd (that is,
      @code(SourceBegin < SourceEnd < SourceVal) or
      @code(SourceBegin > SourceEnd > SourceVal)) then the result
      must be similarly beyond DestEnd.

      As you can see, this works regardless if one, or both,
      of the ranges increase (range end is larger than range begin).)
  )

  MapRangeClamped additionally at the end clamps the result to be within
  [DestBegin, DestEnd] range (or [DestEnd, DestBegin] range,
  in case DestEnd < DestBegin).

  @groupBegin }
function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Integer): Float; overload;
function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Int64  ): Float; overload;
function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: QWord  ): Float; overload;
function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Float  ): Float; overload;
function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Integer): Float; overload;
function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Int64  ): Float; overload;
function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: QWord  ): Float; overload;
function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Float  ): Float; overload;
{ @groupEnd }

function MapRangeTo01(const SourceVal, SourceBegin, SourceEnd: Single): Single;

{ Random Float value in the given range,
  result is between RangeBegin and RangeEnd.
  Always pass arguments such that RangeBegin < RangeEnd. }
function RandomFloatRange(const RangeBegin, RangeEnd: Single): Single; overload;
function RandomFloatRange(const RangeBegin, RangeEnd: Float): Float; overload;

{ Random integer value in the given range,
  result is always >= RangeBegin and < RangeEnd.
  So @italic(result is never equal to RangeEnd), this is often convenient,
  and makes @code(RandomIntRange(0, N)) consistent with @code(Random(N)).
  Always pass arguments such that RangeBegin < RangeEnd. }
function RandomIntRange(const RangeBegin, RangeEnd: Integer): Integer; overload;
function RandomIntRange(const RangeBegin, RangeEnd: Int64): Int64; overload;

{ Random int value in the given range,
  result is always >= RangeBegin and <= RangeEnd.
  Always pass arguments such that RangeBegin <= RangeEnd. }
function RandomIntRangeInclusive(const RangeBegin, RangeEnd: Integer): Integer; overload;
function RandomIntRangeInclusive(const RangeBegin, RangeEnd: Int64): Int64; overload;

function RandomBoolean: boolean;

{ Random -1 or +1. }
function RandomPlusMinus: Integer;

{ Angle between a 2D line segments and OX axis. Angle 0 means that Y1 = Y2
  and X2 > X1, then the angle increases as you rotate CCW. In radians. }
function AngleRadPointToPoint(x1, y1, x2, y2: Single): Single;

{ Calculate power Base to Exponent, knowing both arguments (and so,
  also the result) are >= 0. }
function NatNatPower(Base, Exponent: Cardinal): Cardinal;

{$ifdef FPC}
{ }
function ArcCot(x: Float): Float;
{$endif FPC}

{ Trivial factorial with Int64 result. Beware, the results very quickly
  stop to fit inside Int64 range. }
function SmallFactorial(n: Integer): Int64;

{ Better DivMod version, in case Dividend may be < 0.

  This fixes lack of DivMod with negative Remainder (for FPC at least
  older than 2.2.4, see fpc-devel thread "Math.DivMod results should be signed"
  on 2006-03-21, http://www.mail-archive.com/fpc-devel@lists.freepascal.org/msg04565.html).

  And workarounds faulty DivMod behavior with FPC 2.4.0,
  see http://bugs.freepascal.org/view.php?id=15453 }
procedure CastleDivMod(Dividend: Integer; Divisor: Word;
  out Result, Remainder: SmallInt);

{ Like DivMod (return result of Integer division and a remainder),
  but always return Remainder >= 0.

  This is useful in case when Dividend < 0.
  Standard DivMod (and Pascal div and mod operators) return then
  Result rounded toward zero, and Remainder may be < 0.
  This procedure will return Result rounded toward negative infinity,
  and Remainder will always be >= 0. }
procedure DivUnsignedMod(Dividend: Integer; Divisor: Word;
  out Result: Smallint; out Remainder: Word);

{ Returns Ceil(A / B), but calculated faster and more precisely
  (without floating-point help). }
function CeilDiv(const A, B: Cardinal): Cardinal; overload;
function CeilDiv(const A, B: Int64): Int64; overload;

{ Calculate Integer division and modulo on two Float arguments.
  Requires that A >= 0 and B > 0, and the Result is always >= 0.

  Tries to secure against floating point imprecision.
  It's always guaranteed that Remainder >= 0 and <= B (and usually should be
  < B, but this cannot be guaranteed). }
procedure FloatDivMod(const A, B: Double;
  out DivResult: Int64; out Remainder: Double);

{ Calculate Float modulo of division on two Float arguments.
  Works for any A (lesser than zero too). Assumes that B > 0.

  Tries to secure against floating point imprecision.
  It's always guaranteed that Remainder >= 0 and <= B (and usually should be
  < B, but this cannot be guaranteed). }
function FloatModulo(const A, B: Double): Double;

procedure MinMax(const x0, x1, x2: Double; out min, max: Double); overload;
procedure MinMax(const x0, x1, x2: Single; out min, max: Single); overload;

{ Our version of CoTan, to workaround @url(http://www.freepascal.org/mantis/view.php?id=9944 old FPC bug). }
function CastleCoTan(const Value: Float): Float;

{ Floor from Sqrt(Value). }
function IntSqrt(const Value: Cardinal): Cardinal;

{ Hermite interpolation between two values.
  For X changing from Edge0 to Edge1, the resulting value changes from 0 to 1,
  smoothly (not with a linear speed).
  This is useful to make nicer transition from one value to another,
  it often looks better than using directly @link(Lerp) or @link(MapRange).

  Just like GLSL smoothstep:
  http://www.khronos.org/opengles/sdk/docs/manglsl/xhtml/smoothstep.xml }
function SmoothStep(const Edge0, Edge1, X: Single): Single;

{ Utility functions to control the endianess of Single and Double values; no
  support for Extended though as such values have yet to be encountered when
  read from a file

  @groupBegin }
function Swap32(Value: LongWord): LongWord;
function Swap64(Value: QWord): QWord;
function SwapEndian(const Value: Single): Single; overload;
function SwapEndian(const Value: Double): Double; overload;

{$ifndef FPC}
function NtoLE(const Value: Word): Word; overload;
function NtoLE(const Value: DWord): DWord; overload;
function NtoLE(const Value: QWord): QWord; overload;
function NtoLE(const Value: Int64): Int64; overload;
{$endif}

function NtoLE(const Value: Single): Single; overload;
function NtoLE(const Value: Double): Double; overload;

{$ifndef FPC}
function NtoBE(const Value: Word): Word; overload;
function NtoBE(const Value: DWord): DWord; overload;
function NtoBE(const Value: QWord): QWord; overload;
function NtoBE(const Value: Int64): Int64; overload;
{$endif}

function NtoBE(const Value: Single): Single; overload;
function NtoBE(const Value: Double): Double; overload;

{$ifndef FPC}
function LEtoN(const Value: Word): Word; overload;
function LEtoN(const Value: DWord): DWord; overload;
function LEtoN(const Value: QWord): QWord; overload;
function LEtoN(const Value: Int64): Int64; overload;
{$endif}

function LEtoN(const Value: Single): Single; overload;
function LEtoN(const Value: Double): Double; overload;

{$ifndef FPC}
function BEtoN(const Value: Word): Word; overload;
function BEtoN(const Value: DWord): DWord; overload;
function BEtoN(const Value: QWord): QWord; overload;
function BEtoN(const Value: Int64): Int64; overload;
{$endif}
function BEtoN(const Value: Single): Single; overload;
function BEtoN(const Value: Double): Double; overload;
{ @groupEnd }

{ Convert degrees to radians, consistent with CastleScript deg()
  function and display of rotations in CGE editor.
  In CGE, all functions take rotations in radians. }
function Deg(const Degrees: Float): Float;

type
  EVectorInvalidOp = class(Exception);
  ETransformedResultInvalid = class(EVectorInvalidOp);

{$endif read_interface}

{$ifdef read_implementation}

procedure SwapValues(var a, b: Byte);   var Tmp: Byte;   begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: Int64);  var Tmp: Int64;  begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: QWord);  var Tmp: QWord;  begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: Int32);  var Tmp: Int32;  begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: UInt32); var Tmp: UInt32; begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: Single); var Tmp: Single; begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: Double); var Tmp: Double; begin Tmp := A; A := B; B := Tmp; end;
{$ifdef EXTENDED_OVERLOADS}
procedure SwapValues(var a, b: Extended); var Tmp: Extended; begin Tmp := A; A := B; B := Tmp; end;
{$endif}
procedure SwapValues(var a, b: char);     var Tmp: char;     begin Tmp := A; A := B; B := Tmp; end;
procedure SwapValues(var a, b: Pointer);  var Tmp: Pointer;  begin Tmp := A; A := B; B := Tmp; end;

procedure OrderUp(var Smaller, Larger: Int64);  begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
procedure OrderUp(var Smaller, Larger: Int32);  begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
procedure OrderUp(var Smaller, Larger: UInt32); begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
procedure OrderUp(var Smaller, Larger: Single); begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
procedure OrderUp(var Smaller, Larger: Double); begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
{$ifdef EXTENDED_OVERLOADS}
procedure OrderUp(var Smaller, Larger: Extended); begin if Smaller > Larger then SwapValues(Smaller, Larger); end;
{$endif}

procedure OrderUp(x, y: Int32;  var Smaller, Larger: Int32);  begin if X <= Y then begin Smaller := X; Larger := Y end else begin Smaller := Y; Larger := X end; end;
procedure OrderUp(x, y: UInt32; var Smaller, Larger: UInt32); begin if X <= Y then begin Smaller := X; Larger := Y end else begin Smaller := Y; Larger := X end; end;
procedure OrderUp(x, y: Single;   var Smaller, Larger: Single);   begin if X <= Y then begin Smaller := X; Larger := Y end else begin Smaller := Y; Larger := X end; end;
procedure OrderUp(x, y: Double;   var Smaller, Larger: Double);   begin if X <= Y then begin Smaller := X; Larger := Y end else begin Smaller := Y; Larger := X end; end;
{$ifdef EXTENDED_OVERLOADS}
procedure OrderUp(x, y: Extended; var Smaller, Larger: Extended); begin if X <= Y then begin Smaller := X; Larger := Y end else begin Smaller := Y; Larger := X end; end;
{$endif}

procedure MinVar(var a: Word   ; const b: Word   ); begin if B < A then A := B end;
procedure MinVar(var a: Int64  ; const b: Int64  ); begin if B < A then A := B end;
procedure MinVar(var a: Int32  ; const b: Int32  ); begin if B < A then A := B end;
procedure MinVar(var a: UInt32 ; const b: UInt32 ); begin if B < A then A := B end;
procedure MinVar(var a: Single ; const b: Single ); begin if B < A then A := B end;
procedure MinVar(var a: Double ; const b: Double ); begin if B < A then A := B end;
{$ifdef EXTENDED_OVERLOADS}
procedure MinVar(var a: Extended; const b: Extended); begin if B < A then A := B end;
{$endif}

procedure MaxVar(var a: Word  ; const b: Word  ); begin if B > A then A := B end;
procedure MaxVar(var a: Int64 ; const b: Int64 ); begin if B > A then A := B end;
procedure MaxVar(var a: Int32 ; const b: Int32 ); begin if B > A then A := B end;
procedure MaxVar(var a: UInt32; const b: UInt32); begin if B > A then A := B end;
procedure MaxVar(var a: Single; const b: Single); begin if B > A then A := B end;
procedure MaxVar(var a: Double; const b: Double); begin if B > A then A := B end;
{$ifdef EXTENDED_OVERLOADS}
procedure MaxVar(var a: Extended; const b: Extended); begin if B > A then A := B end;
{$endif}

{$ifndef FPC}
{$ifndef MSWINDOWS}
procedure MaxVar(var a: LongInt ; const b: LongInt ); begin if B > A then A := B end;
procedure MinVar(var a: LongInt ; const b: LongInt ); begin if B < A then A := B end;
procedure MaxVar(var a: LongWord; const b: LongWord); begin if B > A then A := B end;
procedure MinVar(var a: LongWord; const b: LongWord); begin if B < A then A := B end;
{$endif}
{$endif}

function IndexMax(const a0, a1, a2: Double): T3DAxis;
begin
 if a0 > a1 then
 begin
  if a2 > a0 then result := 2 else result := 0
 end else
 begin
  if a2 > a1 then result := 2 else result := 1
 end;
end;

function IndexMin(const a0, a1, a2: Double): T3DAxis;
begin
 if a0 < a1 then
 begin
  if a2 < a0 then result := 2 else result := 0
 end else
 begin
  if a2 < a1 then result := 2 else result := 1
 end;
end;

function Between(const a, vBegin, vEnd: Int64): boolean;    begin Result := (vBegin <= a) and (a <= vend) end;
function Between(const a, vBegin, vEnd: Int32): boolean;  begin Result := (vBegin <= a) and (a <= vend) end;
function Between(const a, vBegin, vEnd: cardinal): boolean; begin Result := (vBegin <= a) and (a <= vend) end;
function Between(const a, vBegin, vEnd: Float): boolean;    begin Result := (vBegin <= a) and (a <= vend) end;
function Between(const a, vBegin, vEnd: Char): boolean;     begin Result := (vBegin <= a) and (a <= vend) end;

function RoundClamp255(const A: Single): Byte;
var
  I: Int64;
begin
  I := Round(A);
  if I < Low(Byte) then
    Exit(Low(Byte)) else
  if I > High(Byte) then
    Exit(High(Byte)) else
    Exit(I);
end;

function Clamped(const a, vBegin, vEnd: Int64): Int64;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

function Clamped(const a, vBegin, vEnd: QWord): QWord;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

function Clamped(const a, vBegin, vEnd: Int32): Int32;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

function Clamped(const a, vBegin, vEnd: cardinal): cardinal;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

function Clamped(const a, vBegin, vEnd: Single): Single;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

function Clamped(const a, vBegin, vEnd: Double): Double;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;

{$ifdef EXTENDED_OVERLOADS}
function Clamped(const a, vBegin, vEnd: Extended): Extended;
begin
  if a < vBegin then result := vBegin else
  if a > vEnd then result := vEnd else
    result := a;
end;
{$endif}

procedure ClampVar(var a: Int64;    const vBegin, vEnd: Int64);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure ClampVar(var a: QWord;    const vBegin, vEnd: QWord);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure ClampVar(var a: Int32;  const vBegin, vEnd: Int32);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure ClampVar(var a: cardinal; const vBegin, vEnd: cardinal);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure ClampVar(var a: Single;   const vBegin, vEnd: Single);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure ClampVar(var a: Double;   const vBegin, vEnd: Double);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

{$ifdef EXTENDED_OVERLOADS}
procedure ClampVar(var a: Extended; const vBegin, vEnd: Extended);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;
{$endif}

procedure Clamp(var a: Int64;    const vBegin, vEnd: Int64);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure Clamp(var a: Int32;  const vBegin, vEnd: Int32);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure Clamp(var a: cardinal; const vBegin, vEnd: cardinal);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure Clamp(var a: Single;   const vBegin, vEnd: Single);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

procedure Clamp(var a: Double;   const vBegin, vEnd: Double);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;

{$ifdef EXTENDED_OVERLOADS}
procedure Clamp(var a: Extended; const vBegin, vEnd: Extended);
begin
  if a < vBegin then a := vBegin else
  if a > vEnd then a := vEnd;
end;
{$endif}

procedure RestOf3DCoords(const Coord: T3DAxis; out First, Second: T3DAxis);
begin
  if Coord = 0 then First := 1 else First := 0;
  if Coord = 2 then Second := 1 else Second := 2;
end;

procedure RestOf3DCoordsCycle(const Coord: T3DAxis; out First, Second: T3DAxis);
const
  FirstArr: array [T3DAxis] of T3DAxis = (1, 2, 0);
  SecondArr: array [T3DAxis] of T3DAxis = (2, 0, 1);
begin
  First := FirstArr[Coord];
  Second := SecondArr[Coord];
end;

function ChangeIntCycle(Value, Change, MaxValue: Integer): Integer;
begin
  Inc(MaxValue);
  Result := (Value + Change) mod MaxValue;
  { We know Result is now in [-MaxValue .. MaxValue]
    (where MaxValue is the original MaxValue passed to this function). }
  if Result < 0 then
    Result := result + MaxValue;
end;

{ Lerp : l + (h-l) * a = l + a*h - a*l = (1-a) *l + a*h }
function Lerp(const a: Single; const l, h: Int32): Single;  begin result := l + (h-l) * a; end;
function Lerp(const a: Single; const l, h: UInt32): Single; begin result := l + (h-l) * a; end;
function Lerp(const a, l, h: Single): Single;               begin result := l + (h-l) * a; end;
function Lerp(const a: Double; const l, h: Int32): Double;  begin result := l + (h-l) * a; end;
function Lerp(const a: Double; const l, h: UInt32): Double; begin result := l + (h-l) * a; end;
function Lerp(const a, l, h: Double): Double;               begin result := l + (h-l) * a; end;

function AngleLerp(T, Angle1, Angle2: Single): Single;
begin
  Angle1 := FloatModulo(Angle1, Pi2);
  Angle2 := FloatModulo(Angle2, Pi2);

  if Angle1 > Angle2 then
  begin
    T := 1 - T;
    SwapValues(Angle1, Angle2);
  end;
  { rest of the implementation can assume Angle1 <= Angle2 }

  if Angle1 + Pi2 - Angle2 >= Angle2 - Angle1 then
    Result := Lerp(T, Angle1, Angle2)
  else
    Result := FloatModulo(Lerp(T, Angle1 + Pi2, Angle2), Pi2);
end;

function RoundUpToMultiply(value, multiplicator: Integer): Integer;
begin
  if value mod multiplicator = 0 then
    result := value
  else
    result := ((value div multiplicator)+1)*multiplicator;
end;

function BiggestPowerOf2(Value: Cardinal): Cardinal;
begin
  { Find the first from the left ("oldest") bit = 1.
    Set the rest of the bits (all the "younger" bits) to zero. }
  result := Cardinal(1 shl (SizeOf(Cardinal)*8-1));
  while (result <> 0) and ((result and Value) = 0) do result := result shr 1;
end;

function Biggest2Exponent(Value: Cardinal): Integer;
begin
  result := -1;
  while Value <> 0 do
    begin Inc(result); Value := Value shr 1 end;
end;

function Smallest2Exponent(Value: Cardinal): Integer;
begin
  if Value = 0 then begin result := -1; Exit end;

  for result := SizeOf(Value)*8-1 downto 0 do
   if ((LongWord(1) shl result) and Value) <> 0 then Break;

  { since Value <> 0, so the above "for" is always finished by a Break.
    Now Result is the position of the "oldest" bit = 1 in Value.
    If all other bits are = 0 (IOW, "Value = 1 shl Result")
    then Result is already OK. Otherwise increase Result
    to make "2 ^ Result >= Value".}
  if Value <> Cardinal(1) shl result then Inc(result);
end;

function Smallest2Power(Value: Cardinal): Cardinal;
var
  Exp: Integer;
begin
  Exp := Smallest2Exponent(Value);
  if Exp = -1 then
    result := 0
  else
    result := 1 shl Exp;
end;

function IsPowerOf2(Value: Cardinal): boolean;
var
  i: Cardinal;
begin
  i := Cardinal(1 shl (SizeOf(Cardinal)*8-1));
  while (i <> 0) do
  begin
    if i = Value then begin result := true; exit end;
    i := i shr 1;
  end;
  result := false;
end;

function DivRoundUp(const Value, Divider: Cardinal): Cardinal;
begin
  Result := (Value + Divider - 1) div Divider;
end;

function DivRoundUp(const Value, Divider: Integer): Integer;
begin
  Result := (Value + Divider - 1) div Divider;
end;

function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Integer): Float;
begin
  { Map range SourceBegin .. SourceEnd to 0..1, and express SourceVal there. }
  Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
  { Map range 0..1 to DestBegin .. DestEnd }
  Result := Result * (DestEnd - DestBegin) + DestBegin;
end;

function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Int64  ): Float;
begin
  { Map range SourceBegin .. SourceEnd to 0..1, and express SourceVal there. }
  Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
  { Map range 0..1 to DestBegin .. DestEnd }
  Result := Result * (DestEnd - DestBegin) + DestBegin;
end;

function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: QWord  ): Float;
{$I norqcheckbegin.inc}
begin
  { Map range SourceBegin .. SourceEnd to 0..1, and express SourceVal there. }
  Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
  { Map range 0..1 to DestBegin .. DestEnd }
  Result := Result * (DestEnd - DestBegin) + DestBegin;
end;
{$I norqcheckend.inc}

function MapRange(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Float  ): Float;
begin
  { Map range SourceBegin .. SourceEnd to 0..1, and express SourceVal there. }
  Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
  { Map range 0..1 to DestBegin .. DestEnd }
  Result := Result * (DestEnd - DestBegin) + DestBegin;
end;

function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Integer): Float;
begin
  Result := MapRange(SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd);
  if DestEnd < DestBegin then
    Result := Clamped(Result, DestEnd, DestBegin)
  else
    Result := Clamped(Result, DestBegin, DestEnd);
end;

function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Int64  ): Float;
begin
  Result := MapRange(SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd);
  if DestEnd < DestBegin then
    Result := Clamped(Result, DestEnd, DestBegin)
  else
    Result := Clamped(Result, DestBegin, DestEnd);
end;

function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: QWord  ): Float;
begin
  Result := MapRange(SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd);
  if DestEnd < DestBegin then
    Result := Clamped(Result, DestEnd, DestBegin)
  else
    Result := Clamped(Result, DestBegin, DestEnd);
end;

function MapRangeClamped(const SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd: Float  ): Float;
begin
  Result := MapRange(SourceVal, SourceBegin, SourceEnd, DestBegin, DestEnd);
  if DestEnd < DestBegin then
    Result := Clamped(Result, DestEnd, DestBegin)
  else
    Result := Clamped(Result, DestBegin, DestEnd);
end;

function MapRangeTo01(const SourceVal, SourceBegin, SourceEnd: Single): Single;
begin
  Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
end;

function RandomFloatRange(const RangeBegin, RangeEnd: Single): Single;
begin
  Result := Random * (RangeEnd - RangeBegin) + RangeBegin;
end;

function RandomFloatRange(const RangeBegin, RangeEnd: Float): Float;
begin
  Result := Random * (RangeEnd - RangeBegin) + RangeBegin;
end;

function RandomIntRange(const RangeBegin, RangeEnd: Integer): Integer;
begin
  Result := RangeBegin + Random(RangeEnd - RangeBegin);
end;

function RandomIntRange(const RangeBegin, RangeEnd: Int64): Int64;
begin
  Result := RangeBegin + Random(RangeEnd - RangeBegin);
end;

function RandomIntRangeInclusive(const RangeBegin, RangeEnd: Integer): Integer;
begin
  Result := RangeBegin + Random(RangeEnd - RangeBegin + 1);
end;

function RandomIntRangeInclusive(const RangeBegin, RangeEnd: Int64): Int64;
begin
  Result := RangeBegin + Random(RangeEnd - RangeBegin + 1);
end;

function RandomBoolean: boolean;
begin
  Result := Random(2) = 0;
end;

function RandomPlusMinus: Integer;
const
  PlusMinus: array [boolean] of Integer = (-1, +1);
begin
  Result := PlusMinus[RandomBoolean];
end;

function AngleRadPointToPoint(x1, y1, x2, y2: Single): Single;
begin
 if y2 > y1 then
 begin
  if SameValue(x1, x2) then
   result := Pi/2 else
  begin
   result := ArcTan( (y2-y1) / (x2-x1) );
   if result < 0 then result := result +Pi;
  end;
 end else
 if y1 > y2 then
 begin
  if SameValue(x1, x2) then
   result := Pi*3/2 else
  begin
   result := ArcTan( (y2-y1) / (x2-x1) );
   if result < 0 then result := result +Pi;
   result := result +Pi;
  end;
 end else
 if x2 > x1 then result := 0 else result := Pi;
end;

function NatNatPower(Base, Exponent: Cardinal): Cardinal;
var i : Cardinal;
begin
 result := 1;
 i := Exponent;
 while i > 0 do
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

procedure CastleDivMod(Dividend: Integer; Divisor: Word;
  out Result, Remainder: SmallInt);
var
  UnsignedResult, UnsignedRemainder: Word;
begin
  { This should not be necessary for FPC, we support only
    FPC >= 3.2.0 and http://bugs.freepascal.org/view.php?id=15453
    (treatment of negative Dividend) is fixed since FPC 2.4.2.
    So FPC could just call DivMod?

    Still this is necessary for Delphi (that supports only unsigned
    1st param for DivMod).

    For consistency we use this code with both FPC and Delphi. }

  if Dividend < 0 then
  begin
    { Use DivMod with >=0 dividend }
    DivMod(-Dividend, Divisor, UnsignedResult, UnsignedRemainder);
    { The documented behavior of Pascal's div/mod operators and DivMod
      on negative dividends is to return Result closer to zero and
      a negative Remainder. Which means that we can just negate both
      Result and Remainder, and all it's Ok. }
    Result := -UnsignedResult;
    Remainder := -UnsignedRemainder;
  end else
  begin
    DivMod(Dividend, Divisor, UnsignedResult, UnsignedRemainder);
    Result := UnsignedResult;
    Remainder := UnsignedRemainder;
  end;
end;

procedure DivUnsignedMod(Dividend: Integer; Divisor: Word;
  out Result: Smallint; out Remainder: Word);
var
  SignedRemainder: SmallInt;
begin
  CastleDivMod(Dividend, Divisor, Result, SignedRemainder);
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
  { DivMod was limited to 16-bit values in older FPC versions.
    It may be possible to change below to
      DivMod(A, B, DivResult, ModResult);
    on new FPC, after adding auto-test that it works OK even with result
    that doesn't fit in 16-bit range. }
  DivResult := A div B;
  ModResult := A mod B;
  if ModResult = 0 then
    Result := DivResult
  else
    Result := DivResult + 1;
end;

function CeilDiv(const A, B: Int64): Int64;
var
  DivResult, ModResult: Int64;
begin
  { DivMod is limited to 32-bit values, so we cannot use it below. }
  DivResult := A div B;
  ModResult := A mod B;
  if ModResult = 0 then
    Result := DivResult
  else
    Result := DivResult + 1;
end;

procedure FloatDivMod(const A, B: Double;
  out DivResult: Int64; out Remainder: Double);
begin
  DivResult := Trunc(A / B);
  Remainder := A - B * DivResult;
  { Now the tricks to make this (somewhat) more numerically stable,
    i.e. resistant to tricky cases without unprecise floating-point
    calculation. }
  if Remainder < 0 then
    Remainder := 0 else
  if Remainder >= B then
  begin
    Remainder := Remainder - B;
    Inc(DivResult);

    { Mathematically, we could do above two lines in a loop
      "while Remainder >= A", but this could loop (practically)
      forever for very small (or zero) B.

      Instead we just clamp Remainder to [0, B] once again, the hard way. }

    ClampVar(Remainder, 0, B);
  end;
end;

function FloatModulo(const A, B: Double): Double;
var
  DivResult: Int64;
begin
  if A < 0 then
  begin
    { FloatDivMod requires that 1st param be >= 0. }
    FloatDivMod(-A, B, DivResult, Result);
    Result := Clamped(B - Result, 0, B);
  end else
    FloatDivMod(A, B, DivResult, Result);
end;

procedure MinMax(const x0, x1, x2: Double; out min, max: Double);
begin
  min := x0;
  max := x0;
  if   x1 < min then min := x1 else
    if x1 > max then max := x1;
  if   x2 < min then min := x2 else
    if x2 > max then max := x2;
end;

procedure MinMax(const x0, x1, x2: Single; out min, max: Single);
begin
  min := x0;
  max := x0;
  if   x1 < min then min := x1 else
    if x1 > max then max := x1;
  if   x2 < min then min := x2 else
    if x2 > max then max := x2;
end;

function CastleCoTan(const Value: Float): Float;
var
  ASin, ACos: Float;
begin
  SinCos(Value, ASin, ACos);
  Result := Acos / ASin;
end;

function IntSqrt(const Value: Cardinal): Cardinal;
{ See http://en.wikipedia.org/wiki/Integer_square_root.

  This isn't nice impl, as it uses floating-point numbers to calculate
  Integer function... However, doing it on ints, doesn't make this
  process terminate nicely. Hacks like in primes.cpp from
  http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
  don't calculate floor sqrt properly (e.g. for 8 return 3).

  So for now, calculate on floats. Autotests check it's Ok.

  Actually, when calculating on floats, I can just use Trunc(Sqrt())...
  So this is quite pointless exercise. }
(*
var
  Old, New: Float;
begin
  if Value = 0 then Exit(0); { to not fail by division by zero }

  New := Value;
  repeat
    Old := New;
    New := (Old + Value / Old) / 2;
  until Abs(Old - New) < 0.5;
  Result := Trunc(New);
end;
*)
begin
  Result := Trunc(Sqrt(Value));
end;

function SmoothStep(const Edge0, Edge1, X: Single): Single;
begin
  Result := Clamped((X - Edge0) / (Edge1 - Edge0), 0.0, 1.0);
  Result := Result * Result * (3.0 - 2.0 * Result);
end;

{ Use Swap32 / Swap64 names, instead of overloading one name like Swap
  or SwapEndian, to avoid easy mistakes. }

function Swap32(Value: LongWord): LongWord;
begin
  {$ifndef FPC}
  // see
  // https://stackoverflow.com/questions/3065335/how-to-convert-big-endian-numbers-to-native-numbers-delphi
  // https://stackoverflow.com/questions/2882434/how-to-convert-big-endian-and-how-to-flip-the-highest-bit
  Result := Swap(Word(Value)) shl 16 + Swap(Word(Value shr 16));
  {$else}
  Result := SwapEndian(Value);
  {$endif}
end;

function Swap64(Value: QWord): QWord;
begin
  {$ifndef FPC}
  // see
  // https://stackoverflow.com/questions/3065335/how-to-convert-big-endian-numbers-to-native-numbers-delphi
  // https://stackoverflow.com/questions/2882434/how-to-convert-big-endian-and-how-to-flip-the-highest-bit
  Result := Swap32(LongWord(Value));
  Result := (Result shl 32) or Swap32(LongWord(Value shr 32));
  {$else}
  Result := SwapEndian(Value);
  {$endif}
end;

function SwapEndian(const Value: Single): Single;
begin
  PLongWord(@Result)^ := Swap32(PLongWord(@Value)^);
end;

function SwapEndian(const Value: Double): Double;
begin
  PQWord(@Result)^ := Swap64(PQWord(@Value)^);
end;


{$ifndef FPC}
  function NtoLE(const Value: Word): Word;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function NtoLE(const Value: DWord): DWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap32(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function NtoLE(const Value: QWord): QWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap64(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function NtoLE(const Value: Int64): Int64; overload;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap64(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

{$endif}


function NtoLE(const Value: Single): Single;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(Value);
{$else ENDIAN_BIG}
  Result := Value;
{$endif ENDIAN_BIG}
end;

function NtoLE(const Value: Double): Double;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(Value);
{$else ENDIAN_BIG}
  Result := Value;
{$endif ENDIAN_BIG}
end;

{$ifndef FPC}
  function NtoBE(const Value: Word): Word;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap(Value);
  {$endif ENDIAN_BIG}
  end;

  function NtoBE(const Value: DWord): DWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap32(Value);
  {$endif ENDIAN_BIG}
  end;


  function NtoBE(const Value: QWord): QWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap64(Value);
  {$endif ENDIAN_BIG}
  end;

  function NtoBE(const Value: Int64): Int64;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap64(Value);
  {$endif ENDIAN_BIG}
  end;

{$endif}


function NtoBE(const Value: Single): Single;
begin
{$ifdef ENDIAN_BIG}
  Result := Value;
{$else ENDIAN_BIG}
  Result := SwapEndian(Value);
{$endif ENDIAN_BIG}
end;

function NtoBE(const Value: Double): Double;
begin
{$ifdef ENDIAN_BIG}
  Result := Value;
{$else ENDIAN_BIG}
  Result := SwapEndian(Value);
{$endif ENDIAN_BIG}
end;

{$ifndef FPC}
  function LEtoN(const Value: Word): Word;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function LEtoN(const Value: DWord): DWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap32(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function LEtoN(const Value: QWord): QWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap64(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;

  function LEtoN(const Value: Int64): Int64;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Swap64(Value);
  {$else ENDIAN_BIG}
    Result := Value;
  {$endif ENDIAN_BIG}
  end;


{$endif FPC}

function LEtoN(const Value: Single): Single;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(Value);
{$else ENDIAN_BIG}
  Result := Value;
{$endif ENDIAN_BIG}
end;

function LEtoN(const Value: Double): Double;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(Value);
{$else ENDIAN_BIG}
  Result := Value;
{$endif ENDIAN_BIG}
end;

{$ifndef FPC}

  function BEtoN(const Value: Word): Word;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap(Value);
  {$endif ENDIAN_BIG}
  end;

  function BEtoN(const Value: DWord): DWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap32(Value);
  {$endif ENDIAN_BIG}
  end;

  function BEtoN(const Value: QWord): QWord;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap64(Value);
  {$endif ENDIAN_BIG}
  end;

  function BEtoN(const Value: Int64): Int64;
  begin
  {$ifdef ENDIAN_BIG}
    Result := Value;
  {$else ENDIAN_BIG}
    Result := Swap64(Value);
  {$endif ENDIAN_BIG}
  end;

{$endif}


function BEtoN(const Value: Single): Single;
begin
{$ifdef ENDIAN_BIG}
  Result := Value;
{$else ENDIAN_BIG}
  Result := SwapEndian(Value);
{$endif ENDIAN_BIG}
end;

function BEtoN(const Value: Double): Double;
begin
{$ifdef ENDIAN_BIG}
  Result := Value;
{$else ENDIAN_BIG}
  Result := SwapEndian(Value);
{$endif ENDIAN_BIG}
end;

function Deg(const Degrees: Float): Float;
begin
  Result := DegToRad(Degrees);
end;

{$endif read_implementation}
