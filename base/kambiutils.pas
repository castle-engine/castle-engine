{
  Copyright 1999-2006 Michalis Kamburelis.

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

{ @abstract(Kambi (Michalis Kamburelis) Utilities.)

  This unit defines many many various basic utilities.

  @unorderedList(
    @item(Base @link(TDynArrayBase) class and many TDyn*Array classes
      implementing TDynArrayBase descendants that can store base Pascal types
      (Integers, strings, floats etc.).)

    @item(They are comfortable replacement for ObjectPascal's dynamic arrays,
      implemented by simulating C++-templates.
      Initially done to achieve in FPC 1.0.6 things that I was doing
      in Delphi using dynamic arrays, but now they are much more comfortable
      and just better (for me) than dynamic arrays (e.g. they have
      methods for seeking and sorting and they can be cleanly extended
      by inheritance).

      See things in kambiutils_dyn_arrays.inc include file.)

    @item(Some basic operations on numbers
      (see things in kambiutils_math.inc).)

    @item(Some simple things that must be implemented differently on
      various OSes (i.e. they are implemented here to hide differences
      between some OSes).)

    @item(They somehow complement standard set of routines in SysUtils.
      See e.g. @link(GetTickCount), @link(ProcessTimerNow) in kambiutils_time.inc.
      See e.g. things in kambiutils_filenames.inc.)

    @item(Things to process command-line options.
      See @link(Parameters) in kambiutils_params.inc.

      They are also used in my unit to parse command-line
      options @link(ParseParametersUnit).)

    @item(Some basic algorithms, e.g. @link(Sort) in kambiutils_basic_algorithms.inc)

    @item(Simple wrappers for OS specific things, like
      for Libc or BaseUnix/Unix units under UNIX
      (see also README.use_libc) or WinAPI under Windows.
      See win32/kambiutils_os_specific.inc and
      unix/kambiutils_os_specific.inc files.)

    @item(@link(TDynLib) class to handle dynamic libraries in a way
      more comfortable (for me) than standard routines available
      in DynLibs, Dl or some OS-specific units like Libc or Windows.)

    @item(Some others...)
  )

  This unit is a bag for simple and generally useful things.
  As a rule (to not let myself put too much things here)
  this unit must not use any objects defined in Classes unit
  (i.e. this unit can't use Classes unit, directly or indirectly).
  So not only GUI libs like VCL, LCL or CLX can't be utilized in this
  unit, but also many other non-visual classes (e.g. streams, lists).
  The only classes that may be defined and used here
  are exceptions (the base Exception class comes from SysUtils unit)
  and TDyn*Array classes.

  Initialization of this unit does some things that you should be aware of
  (for me, these things are just a must-have for my every program,
  reasoning is given below):

  @unorderedList(
    @item(Call Randomize (so that you never forget about it))

    @item(Set DecimalSeparator to '.'.

      Delphi (and, for compatibility, FPC) defines DecimalSeparator
      based on local settings (like configured user's country).
      But this makes things like StrToFloat and FloatToStr work
      unpredictably -- they may give different results on different
      systems. E.g. FloatToStr(0.9) may output '0,9' on some system.
      But if you write '0,9' to a text file, it may not be understood
      by StrToFloat on some other system.

      Moreover (this is rather my personal opinion), '.' as DecimalSeparator
      is really a standard convention on computers. There is no sense
      in localizing this. Instead, I would rather see all users using
      computers to understand that '.' is standard decimal separator.
      This is something that just eases communication -- when everyone
      in the world would use the same DecimalSeparator, we would have
      less communication problems. Just like there is a standard ISO
      date format, also setting a universal standard for DecimalSeparator
      is a good idea.)

    @item(Installs my handler for ExceptProc, see comments at
      @link(HaltCodeOnException))
  )

  Things from Pascal RTL / FCL / etc. that you shouldn't use
  because I have better replacemenets here:
  @unorderedList(
    @item(ChDir with $I+ => better use ChangeDir from this unit
      (because of Delphi bug --- ChDir with $I+ works like with $I-))

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
  {$ifdef WIN32}
    Windows, {$ifdef DELPHI} Messages, Types, ShellAPI, {$endif}
  {$endif}
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc, {$else} BaseUnix, Unix, Dl, {$endif}
  {$endif}
  {$ifdef FPC} DynLibs, {$endif}
  Variants, SysUtils, Math;

{$define read_interface}

{ @section(Very very basic types that are needed by many included files) }

type
  { }
  TIsSmallerFunc = function (const A, B, Data: Pointer): boolean;
  TIsSmallerFuncByObject = function (const A, B: Pointer): boolean of object;

  { @noAutoLinkHere }
  Float = {$ifdef DELPHI} Extended {$else} Math.Float {$endif};
  PFloat = {$ifdef DELPHI} PExtended {$else} Math.PFloat {$endif};

  PCardinal = ^Cardinal;
  PLongWord = ^LongWord;
  PShortint = ^Shortint;

  {$ifdef FPC}
  { I'm overriding PBoolean that is defined as ^Byte in Windows unit.
    For FPC 1.0.x PBoolean is not avail at all. }
  PBoolean = ^Boolean;
  {$endif}

{ include everything }

{$I kambiutils_basic_algorithms.inc}
{$I kambiutils_dyn_arrays.inc}
{$I kambiutils_miscella.inc}
{$I kambiutils_program_exit.inc}
{$I kambiutils_params.inc}
{$I kambiutils_bits.inc}
{$I kambiutils_os_specific.inc}
{$I kambiutils_time.inc}
{$I kambiutils_math.inc}
{$I kambiutils_filenames.inc}
{$I kambiutils_os_error.inc}
{$I kambiutils_pointers.inc}
{$I kambiutils_read_write.inc}
{$I kambiutils_dynlibs.inc}

{$undef read_interface}

implementation

uses KambiStringUtils, KambiFilesUtils;

{$define read_implementation}

{$I kambiutils_basic_algorithms.inc}
{$I kambiutils_dyn_arrays.inc}
{$I kambiutils_miscella.inc}
{$I kambiutils_program_exit.inc}
{$I kambiutils_params.inc}
{$I kambiutils_bits.inc}
{$I kambiutils_time.inc}
{$I kambiutils_math.inc}
{$I kambiutils_filenames.inc}
{$I kambiutils_os_specific.inc}
{$I kambiutils_os_error.inc}
{$I kambiutils_pointers.inc}
{$I kambiutils_read_write.inc}
{$I kambiutils_dynlibs.inc}

{$undef read_implementation}

initialization
 InitializationProgramExit;
 InitializationParams;
 InitializationOSSpecific;

 Randomize; { required by e.g. GetTempFname }
 DecimalSeparator := '.';
finalization
 FinalizationOSSpecific;
 FinalizationParams;
 FinalizationProgramExit;
end.
