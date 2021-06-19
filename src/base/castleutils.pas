{
  Copyright 1999-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various basic utilities.

  @unorderedList(
    @item(Lists, using generics derived from standard Generics.Collections.
      Lists of primitives, like @link(TIntegerList) and @link(TFloatList),
      and a helpful class to define a list of structures @link(TStructList).)

    @item(Basic operations on numbers (compare, randomize).)

    @item(Some OS-dependent things.)

    @item(Some compatibility things, to plug the differences between
      various versions of FPC and Delphi.)

    @item(Filenames operations. They do not touch actual files
      (operations on files are in @link(CastleFilesUtils).))
  )

  This unit is a bag for simple and useful things.
  Let's not overload this unit with too many trivial utilities.
  When adding new utilities, first consider more specific units,
  like @link(CastleFilesUtils), @link(CastleStringUtils) and @link(CastleClassUtils).
  This unit does not depend on the standard @code(Classes) unit (use @link(CastleClassUtils)
  for utilities on top of @code(Classes)).

  Initialization of this unit does some generally-useful things:

  @unorderedList(
    @item(Sets @code(DecimalSeparator) to '.' (dot).

      Bacause Delphi and FPC define DecimalSeparator
      based on local settings (e.g. user's country).
      This makes standard functions like @code(StrToFloat) and @code(FloatToStr)
      less useful for various parsing and printing done by Castle Game Engine,
      where we often must read and write the "dot".
      E.g. @code(FloatToStr(0.9)) may output '0,9' (with a comma) in a some countries.
      Writing it to a file would be usually a mistake --
      as the file may be transferred to another user with a different locale,
      where @code(StrToFloat) could not deal with a comma being used for the separator.

      Initial (localized) value of DecimalSeparator
      is saved in LocaleDecimalSeparator variable.

      @bold(Do not depend on this feature.
      Maybe we had good reasons to do this,
      but in the long run it may be unexpected to new developers
      that the engine overrides a global DecimalSeparator.
      We will remove this feature in the future.)

      Use @link(FormatDot) and FloatToStrDot to reliably output floating point values
      with "dot" as a decimal separator.
      Similarly, use StrToFloatDot, TryStrToFloatDot, StrToFloatDefDot
      to read string with dot to a float.
    )

    @item(Makes AnsiString (which is usually just called "string")
      have UTF-8 encoding.

      This is consistent with Lazarus, that already does the same:
      http://wiki.lazarus.freepascal.org/Unicode_Support_in_Lazarus .
      This way you can just use "string" (not UTF8String or UnicodeString)
      throughout your code, and everything will just work.

      This way your applications will behave the same, whether they use LCL
      (which happens if you use TCastleControlBase on Lazarus form) or not
      (which happens if you use TCastleWindowBase).

      This is also consistent with what our TCastleAbstractFont expects (it's
      rendering assumes UTF-8 encoding of strings) and what some of our
      XML manipulation routines expect.
    )
  )
}

unit CastleUtils;

{$ifdef VER3_0}
  { Almost all CGE code uses ObjFpc mode under FPC,
    but this unit needs Delphi mode for FPC 3.0.x
    to workaround FPC 3.0.0 and 3.0.2 bug:
    they segfault on TStructList definition
    "generic TStructList<T> = class(specialize TList<T>)".

    Fixed in FPC 3.1.1 already, but CGE needs to work with FPC 3.0.0 and 3.0.2 too.

    We still use ObjFpc for FPC 3.1.1 and newer.
    This is consistent with the rest of CGE,
    and makes Lazarus CodeTools working OK in this case,
    since Lazarus CodeTools cannot parse correctly Delphi generics for now:

    https://bugs.freepascal.org/view.php?id=32358
    https://bugs.freepascal.org/view.php?id=32291
    https://bugs.freepascal.org/view.php?id=30271
    https://bugs.freepascal.org/view.php?id=32291
    https://bugs.freepascal.org/view.php?id=30227
  }
  {$mode delphi}
  {$define CASTLE_CONF_DO_NOT_OVERRIDE_MODE}
{$endif}

{$I castleconf.inc}
{$undef CASTLE_CONF_DO_NOT_OVERRIDE_MODE}

interface

uses {$ifdef MSWINDOWS} Windows, {$ifndef FPC} ShlObj, {$endif} {$endif}
  {$ifdef UNIX} BaseUnix, Unix, Dl, {$endif}
  Variants, SysUtils, Math, Generics.Collections;

{$define read_interface}

{ include everything }

{$I castleutils_types.inc}
{$I castleutils_delphi_compatibility.inc}
{$I castleutils_basic_algorithms.inc}
{$I castleutils_platform.inc}
{$I castleutils_miscella.inc}
{$I castleutils_struct_list.inc}
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

{$define read_implementation}

{$I castleutils_types.inc}
{$I castleutils_delphi_compatibility.inc}
{$I castleutils_basic_algorithms.inc}
{$I castleutils_platform.inc}
{$I castleutils_miscella.inc}
{$I castleutils_struct_list.inc}
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

  LocaleDecimalSeparator :=
    {$ifdef FPC} DefaultFormatSettings {$else} FormatSettings {$endif}.DecimalSeparator;
  {$ifdef FPC} DefaultFormatSettings {$else} FormatSettings {$endif}
    .DecimalSeparator := '.';

  { FPC includes backslash in AllowDirectorySeparators also on non-Windows,
    so backslash will be considered as directory separator by
    Include/ExcludeTrailingPathDelimiter. This is IMHO very stupid,
    since normal OS routines on Unix *do not* consider backslash to be any
    special character in a filename, it certainly does not separate dirs.
    So Include/ExcludeTrailingPathDelimiter are basically buggy by default.

    Fortunately we can fix it by globally changing AllowDirectorySeparators. }
  {$ifndef MSWINDOWS}
  AllowDirectorySeparators := AllowDirectorySeparators - ['\'];
  {$endif}

  { Set UTF-8 in AnsiStrings, just like Lazarus
    (see initialization of lazarus/components/lazutils/fpcadds.pas in Lazarus sources) }
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows

{$IFDEF FPC}
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
 {$ENDIF}
finalization
  FinalizationOSSpecific;
end.
