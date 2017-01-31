{
  Copyright 1999-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various basic utilities. Developed for "Castle Game Engine",
  but generally usable.

  @unorderedList(
    @item(Lists of primitives, using FPC generics.
      Like TIntegerList, TFloatList etc.)

    @item(Basic operations on numbers.)

    @item(Some OS-dependent things.)

    @item(Filenames operations (they somehow complement the standard set
      of routines in SysUtils).)

    @item(Basic algorithms: @link(Sort).)
  )

  This unit is a bag for simple and generally useful things.
  As a rule (to not let myself put too much things here)
  this unit must not depend on the Classes unit
  (see CastleClassUtils for those). Or any higher-level GUI libs
  like LCL, VCL, or CLX.
  The only classes defined and used here are exceptions
  (the base Exception class comes from SysUtils unit)
  and primitives lists classes.

  Initialization of this unit does some generally-useful things:

  @unorderedList(
    @item(Calls Randomize (so that you never forget about it).)

    @item(Sets DecimalSeparator to '.'.

      Delphi and FPC define DecimalSeparator
      based on local settings (like configured user's country).
      But this makes things like StrToFloat and FloatToStr less
      predictable --- they may give different results on different
      systems, which limits their use.
      E.g. FloatToStr(0.9) may output '0,9' on some system.
      And if you write '0,9' to a text file, it may not be understood
      by StrToFloat on some other system.

      Initial (probably localized) value of DecimalSeparator
      is saved in LocaleDecimalSeparator variable.)
  )
}

unit CastleUtils;

{$I castleconf.inc}

interface

uses {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} BaseUnix, Unix, Dl, {$endif}
  Variants, SysUtils, Math, FGL;

{$define read_interface}

{ @section(Very very basic types that are needed by many included files) }

type
  { }
  TIsSmallerFunc = function (const A, B, Data: Pointer): boolean;
  TIsSmallerFuncByObject = function (const A, B: Pointer): boolean of object;

  Float = Math.Float;
  PFloat = Math.PFloat;

  PCardinal = ^Cardinal;
  PLongWord = ^LongWord;
  PShortint = ^Shortint;

  {$ifdef FPC}
  { Pointer to a boolean.
    Defined as ^Byte in some Delphi Windows unit,
    for FPC 1.0.x PBoolean is not available at all. }
  PBoolean = ^Boolean;
  {$endif}

{ include everything }

{$I castleutils_basic_algorithms.inc}
{$I castleutils_miscella.inc}
{$I castleutils_primitive_lists.inc}
{$I castleutils_program_exit.inc}
{$ifdef UNIX}      {$I castleutils_os_specific_unix.inc}    {$endif}
{$ifdef MSWINDOWS} {$I castleutils_os_specific_windows.inc} {$endif}
{$I castleutils_math.inc}
{$I castleutils_filenames.inc}
{$I castleutils_pointers.inc}
{$I castleutils_read_write.inc}

{$undef read_interface}

var
  { }
  LocaleDecimalSeparator: char;

implementation

uses CastleStringUtils, CastleFilesUtils;

{$define read_implementation}

{$I castleutils_basic_algorithms.inc}
{$I castleutils_miscella.inc}
{$I castleutils_primitive_lists.inc}
{$I castleutils_program_exit.inc}
{$I castleutils_math.inc}
{$I castleutils_filenames.inc}

{ We cannot just have
  windows/castleutils_os_specific.inc and
     unix/castleutils_os_specific.inc (same filename on all platforms),
  and depend on paths to choose proper one:
  For Lazarus package this would prevent maintaining single .lpk file,
  see ../packages/README. }
{$ifdef UNIX}      {$I castleutils_os_specific_unix.inc}    {$endif}
{$ifdef MSWINDOWS} {$I castleutils_os_specific_windows.inc} {$endif}

{$I castleutils_pointers.inc}
{$I castleutils_read_write.inc}

{$undef read_implementation}

initialization
 InitializationOSSpecific;

 Randomize; { required by e.g. GetTempFname }

 LocaleDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
 DefaultFormatSettings.DecimalSeparator := '.';

 { FPC includes backslash in AllowDirectorySeparators also on non-Windows,
   so backslash will be considered as directory separator by
   Include/ExcludeTrailingPathDelimiter. This is IMHO very stupid,
   since normal OS routines on Unix *do not* consider backslash to be any
   special character in a filename, it certainly does not separate dirs.
   So Include/ExcludeTrailingPathDelimiter are basically buggy by default.

   Fortunately we can fix it by globally changing AllowDirectorySeparators. }
 {$ifndef MSWINDOWS}
 AllowDirectorySeparators -= ['\'];
 {$endif}
finalization
 FinalizationOSSpecific;
end.
