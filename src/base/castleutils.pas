{
  Copyright 1999-2023 Michalis Kamburelis.

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
    @item(Makes AnsiString have UTF-8 encoding when compiled with FPC.

      See @url(https://castle-engine.io/coding_conventions#_most_code_should_use_just_string_and_be_prepared_that_it_is_8_bit_on_fpc_and_16_bit_on_delphi_only_if_writing_to_stream_explicitly_use_8_bit_ansistring_in_usual_case_when_you_write_utf_8
      more docs about our approach to String handling in CGE, for both FPC and Delphi).
      Basically:

      @unorderedList(
        @item With FPC, we follow Lazarus convention, and String = AnsiString (8-bit) = contains UTF-8.

        @item With Delphi, we follow Delphi convention, and String = UnicodeString (16-bit) = contains UTF-16.
      )

      This way you can just use String throughout your code, and everything will just work.

      This way your applications will behave the same, whether they use Delphi,
      or FPC with LCL (which happens if you use TCastleControl on Lazarus form),
      or FPC without LCL (which happens if you use TCastleWindow with FPC).

      Some code in CGE has to assume that String is encoded like this -- e.g. TCastleAbstractFont
      iterates over Unicode characters in a String, so it assumes that the encoding is as above,
      for FPC and Delphi.
    )
  )
}

unit CastleUtils;

{$I castleconf.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$ifndef FPC} ShlObj, {$endif} {$endif}
  {$ifdef UNIX} {$ifdef FPC} BaseUnix, Unix, Dl, {$else} Posix.Unistd, {$endif} {$endif}
  {$ifndef FPC} Classes, {$endif}
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

  {$ifdef CASTLE_TEST_DECIMAL_SEPARATOR_COMMA}
  {$ifdef FPC} DefaultFormatSettings {$else} FormatSettings {$endif}
    .DecimalSeparator := ',';
  {$endif}

  { FPC includes backslash in AllowDirectorySeparators also on non-Windows,
    so backslash will be considered as directory separator by
    Include/ExcludeTrailingPathDelimiter. This is IMHO very stupid,
    since normal OS routines on Unix *do not* consider backslash to be any
    special character in a filename, it certainly does not separate dirs.
    So Include/ExcludeTrailingPathDelimiter are basically buggy by default.

    Fortunately we can fix it by globally changing AllowDirectorySeparators. }
  {$ifdef FPC}
  {$ifndef MSWINDOWS}
  AllowDirectorySeparators := AllowDirectorySeparators - ['\'];
  {$endif}
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
