{
  Copyright 1999-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various basic utilities. Developed for "Kambi VRML game engine",
  but generally usable.

  @unorderedList(
    @item(
      Templates for classes that work like dynamic arrays.
      Base @link(TDynArrayBase) class and many TDyn*Array classes
      implementing TDynArrayBase descendants that can store base Pascal types,
      like TDynIntegerArray, TDynFloatArray etc.

      New TDyn*Xxx classes may be created much like instantiating templates in C++,
      thanks to some tricks with include files and compiler defines,
      see comments at the beginning of dynarray.inc file.)

    @item(Basic operations on numbers.)

    @item(Some OS-dependent things.)

    @item(Filenames operations (they somehow complement the standard set
      of routines in SysUtils).)

    @item(Basic algorithms: @link(Sort).)
  )

  This unit is a bag for simple and generally useful things.
  As a rule (to not let myself put too much things here)
  this unit must not depend on the Classes unit
  (see KambiClassUtils for those). Or any higher-level GUI libs
  like LCL, VCL, or CLX.
  The only classes defined and used here are exceptions
  (the base Exception class comes from SysUtils unit)
  and TDyn*Array classes.

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

    @item(Installs my handler for ExceptProc, see comments at
      @link(HaltCodeOnException).)
  )

  Some of the things from Pascal RTL / FCL get better replacemenets here:

  @unorderedList(
    @item(ChDir with $I+ => better use ChangeDir from this unit
      (because of Delphi bug --- ChDir with $I+ works like with $I-).)

    @item(ParamStr(0), Application.ExeName => better use
      ProgramName or ExeName from this unit. They are portable,
      eliminate the problems under Unixes when ParamStr(0)
      is not necessarily an exe name, unlike Application.ExeName
      they don't need whole VCL / CLX stuff,
      and eliminate Kylix 1 bug in Application.ExeName)

    @item(Reset/Rewrite => it's often better to use SafeReset/SafeRewrite)

    @item(FreeMem => it's often better to use FreeMemNiling)

    @item(FindFirst/FindNext => it's often better to use EnumerateFiles unit)
  )
}

unit KambiUtils;

{$I kambiconf.inc}

interface

uses
  {$ifdef MSWINDOWS}
    Windows, {$ifdef DELPHI} Messages, Types, ShellAPI, {$endif}
  {$endif}
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc, {$else} BaseUnix, Unix, Dl, {$endif}
  {$endif}
  Variants, SysUtils, Math,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

{$define read_interface}

{ @section(Very very basic types that are needed by many included files) }

type
  { }
  TIsSmallerFunc = function (const A, B, Data: Pointer): boolean;
  TIsSmallerFuncByObject = function (const A, B: Pointer): boolean of object;

  Float = {$ifdef DELPHI} Extended {$else} Math.Float {$endif};
  PFloat = {$ifdef DELPHI} PExtended {$else} Math.PFloat {$endif};

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

{$I kambiutils_basic_algorithms.inc}
{$I kambiutils_dyn_arrays.inc}
{$I kambiutils_miscella.inc}
{$I kambiutils_program_exit.inc}
{$ifdef UNIX}      {$I kambiutils_os_specific_unix.inc}    {$endif}
{$ifdef MSWINDOWS} {$I kambiutils_os_specific_windows.inc} {$endif}
{$I kambiutils_math.inc}
{$I kambiutils_filenames.inc}
{$I kambiutils_os_error.inc}
{$I kambiutils_pointers.inc}
{$I kambiutils_read_write.inc}

{$undef read_interface}

var
  { }
  LocaleDecimalSeparator: char;

implementation

uses KambiStringUtils, KambiFilesUtils;

{$define read_implementation}

{$I kambiutils_basic_algorithms.inc}
{$I kambiutils_dyn_arrays.inc}
{$I kambiutils_miscella.inc}
{$I kambiutils_program_exit.inc}
{$I kambiutils_math.inc}
{$I kambiutils_filenames.inc}

{ We cannot just have
  windows/kambiutils_os_specific.inc and
     unix/kambiutils_os_specific.inc (same filename on all platforms),
  and depend on paths to choose proper one:
  For Lazarus package this would prevent maintaining single .lpk file,
  see ../packages/README. }
{$ifdef UNIX}      {$I kambiutils_os_specific_unix.inc}    {$endif}
{$ifdef MSWINDOWS} {$I kambiutils_os_specific_windows.inc} {$endif}

{$I kambiutils_os_error.inc}
{$I kambiutils_pointers.inc}
{$I kambiutils_read_write.inc}

{$undef read_implementation}

initialization
 InitializationProgramExit;
 InitializationOSSpecific;

 Randomize; { required by e.g. GetTempFname }

 LocaleDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
 DefaultFormatSettings.DecimalSeparator := '.';
finalization
 FinalizationOSSpecific;
 FinalizationProgramExit;
end.
