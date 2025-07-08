{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal definitions to fix Kraft for some compilers/platforms.
  In practice, for now, these are fixes specific for Delphi on non-Windows
  platforms. }
unit CastleInternalKraftOverrides;

{$I castleconf.inc}

interface

{$if (not defined(FPC)) and (not defined(MSWINDOWS))}
type
  { Delphi on non-Windows redefines LongInt/LongWord in a way inconsistent
    with Delphi/Windows or FPC.
    Make Kraft use expected type sizes.
    We don't just search + replace this in Kraft sources, to ease upgrading
    Kraft in the future.
    See https://castle-engine.io/coding_conventions#no_longint_longword .
    This seems already handled better in recent Kraft,
    https://github.com/BeRo1985/kraft/blob/master/src/kraft.pas ,
    that avoids LongInt/LongWord just like CGE. }
  LongInt = Integer;
  LongWord = Cardinal;

const
  { Delphi on Posix defines TThread.Priority as Integer.
    Define tpHigher as 0 (seems to work OK, or should we bump it?)
    to compile. }
  tpHigher = 0;
{$endif}

{ Delphi on Linux doesn't define
  - TFPUPrecisionMode and setter/getter
  - TFPUExceptionMask and setter/getter
  Define dummy "stubs" to compile Kraft. }
{$if defined(FPC) or defined(MSWINDOWS)}
  {$define HAS_FPU_TYPES}
{$endif}

{$ifndef HAS_FPU_TYPES}
type
  TFPUPrecisionMode = (pmSingle, pmReserved, pmDouble, pmExtended);
  TFPUException = (exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision);
  TFPUExceptionMask = set of TFPUException;
var
  GetPrecisionMode: TFPUPrecisionMode;
  GetExceptionMask: TFPUExceptionMask;
procedure SetPrecisionMode(const NewValue: TFPUPrecisionMode);
procedure SetExceptionMask(const NewValue: TFPUExceptionMask);
{$endif}

implementation

{$ifndef HAS_FPU_TYPES}
procedure SetPrecisionMode(const NewValue: TFPUPrecisionMode);
begin
  // Ignored on this compiler/platform, just adjust the GetXxx value
  GetPrecisionMode := NewValue;
end;

procedure SetExceptionMask(const NewValue: TFPUExceptionMask);
begin
  // Ignored on this compiler/platform, just adjust the GetXxx value
  GetExceptionMask := NewValue;
end;
{$endif}

end.
