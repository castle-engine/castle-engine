## Summary

Define `CASTLE_DONT_CHANGE_STRING_ENCODING` to prevent engine from doing `SetMultiByteConversionCodePage(CP_UTF8)` at initialization.

This is only supported (and works flawlessly) with Delphi, or FPC _DelphiUnicode_ mode, for now. If you are brave, you can hack the engine to enable it also for other FPC modes, knowing that some tests will fail.

## Introduction

By default, Castle Game Engine (just like Lazarus LCL) assumes that `AnsiString` contains UTF-8. In `initialization` of CastleUtils, we call:

- `SetMultiByteConversionCodePage(CP_UTF8)` (for both FPC and Delphi)
- `SetMultiByteRTLFileSystemCodePage` (for FPC only, since Delphi RTL uses 16-bit UnicodeString)

This gives us nice guarantee that `String` is equal to either
- `AnsiString`, and it holds UTF-8
- `UnicodeString` (Delphi or FPC with _DelphiUnicode_ RTL), and it holds UTF-16

See https://castle-engine.io/coding_conventions#strings_unicode . We happily process strings in a uniform way using `CastleUnicode` routines, and in most cases just using regular Pascal RTL routines for strings.

Define `CASTLE_DONT_CHANGE_STRING_ENCODING` symbol when building the engine to avoid calling the above. You can do it by
- editing `src/common_includes/castleconf.inc`
- or adding proper `<define>` in [CastleEngineManifest.xml](https://castle-engine.io/project_manifest#_compiler_options_and_paths) if you compile using CGE engine,
- or adding a compilation symbol in Delphi project options if you compile using Delphi IDE,
- or any other place you can define a compilation symbol that affects compilation of Castle Game Engine units.

In effect, 8-bit strings (`AnsiString`) remain with system-specific encoding. This practically matters only on Windows, where `AnsiString` have locale-specific "ANSI encoding" by default, like Windows-1252. (On non-Windows systems, like Linux, the AnsiString uses in practice also UTF-8 since these systems use UTF-8 under the hood for all system APIs.)

## Note about FPC DelphiUnicode mode

We don't support building with FPC's _DelphiUnicode_ mode yet. We need to fix some assumptions to make it work (at this point, some pieces of engine assume that FPC -> implies we have 8-bit `String` equal to `AnsiString`). If you want to use _DelphiUnicode_ mode, please contact us and we will help you.

## Supported only for Delphi, unless you REALLY know what you're doing:)

The `CASTLE_DONT_CHANGE_STRING_ENCODING` option is right now only supported for Delphi.

We made sure everything works in this case, automated tests pass (and they exercise some funny edge-cases, like Spine JSON files with non-ASCII slot names, non-ASCII characters in filenames, in ZIP entries etc.) and we don't need `AnsiString` to contain UTF-8, it can contain platform-specific encoding. 99% of the engine just uses `String` anyway (which equals `UnicodeString` in Delphi). In rare cases where we needed _"8-bit string with UTF-8, not any other, encoding"_, we used `Utf8String` and we have extensive automatic tests (grep for `TTestCompiler.TestAnsiStringUtf8Conversion`) to make sure it all rocks.

We initially planned to support it also for FPC (even without _DelphiUnicode_), and it's actually done "half-way". If you define `CASTLE_DONT_CHANGE_STRING_ENCODING`, and (if you know what you're doing!) you will hack `castleconf.inc` to not complain about it, you can build Castle Game Engine with FPC with this symbol. But then -> we have some things known to be broken (run `tests` to see them). We do not support this configuration. Realistically, it will work, as long as only exchange ASCII text with our engine.

We resigned from full FPC support (that would address all edge-cases) for `CASTLE_DONT_CHANGE_STRING_ENCODING`, because it would mean we would have to define own string type, like

```delphi
  { Preferred String type throughout Castle Game Engine codebase.
    This string type has data encoded as either:

    @unorderedList(
      @item(UTF-8. With compilers that default to String=AnsiString,
        which means FPC (all modes except FPC DelphiUnicode).)
      @item(or UTF-16. With compilers that default to String=UnicodeString,
        which means Delphi or FPC with DelphiUnicode mode.)
    ) }
  CastleString =
    {$if defined(CASTLE_DONT_CHANGE_STRING_ENCODING) and
         defined(FPC) and
         (not defined(FPC_DELPHIUNICODE))}
      Utf8String
    {$else}
      String
    {$endif};
```

and use it *everywhere* throughout the engine. This makes a big complication to contributing to the engine, every newcomer would need to read _"what is `CastleString` type"_. But we want simpler philosophy: _"Castle Game Engine just uses your default `String`, and you don't need to worry about it"_.

We initially wanted to use FPC macro `{$define String:=Utf8String}` in `castleconf.inc` to solve this, but FPC macros cannot redefine keywords.

If you insist, and force engine to use `CASTLE_DONT_CHANGE_STRING_ENCODING` with FPC, be aware that subtle things will break. Our engine assumes UTF-8 when using routines like JSON, XML processing, font display, file opening/saving. We do not guarantee in such case what happens if you try to use non-ASCII characters with our engine.

## Automatic encoding conversions

When `CASTLE_DONT_CHANGE_STRING_ENCODING` is defined, and your codebase uses `AnsiString` with native platform encoding, you rely on automatic encoding conversions between

- `AnsiString` and `UnicodeString`

- `AnsiString` and `Utf8String`. Note that there are some quirks in how this works, and FPC 3.2.2 is not perfectly compatible with Delphi. See the tests in `TTestCompiler.TestAnsiStringUtf8Conversion` for details.

### File names

Throughout the engine, we use URLs, typed as just `String`.

The Unicode characters inside URLs are percent-encoded, following the URL standard. ( Underneath, they always encode UTF-8 bytes, this is following URL standard and independent from compiler / `String` meaning, but you should not be concerned about this. ) See `CastleUriUtils` for various operations, including encoding and decoding.

We also accept regular filenames in most engine routines, and automatically convert them to URLs underneath. Unicode characters in filenames are converted correctly just like in any other `String` usage.

We recommend you also adopt URLs everywhere, as they can just express more things (see https://castle-engine.io/url ) and we support every useful operation on them (see `CastleUriUtils`). But this is not forced.

### Text files contents

Routines that read / write text file contents as 8-bit strings use `Utf8String`, not `AnsiString` to represent these contents:

- `FileToString`, `StringToFile` (in `CastleFilesUtils`)
- `StreamToString`, `ReadGrowingStreamToString`, `MemoryStreamLoadFromString`, `WriteStr/WritelnStr` (in `CastleClassUtils`)
- TODO: `TCastleTextReader` and `TCastleTextWriter` (in `CastleDownload`) should also be changed to use `Utf8String` instead of `AnsiString`.

This way, we assume UTF-8 in all text files, and `Utf8String` makes this explicit. Assigning `Utf8String` to `String` (or passing a `String` to these routines) will do the right thing, in all supported situations. Here is what happens:

- When `String` is 16-bit (`UnicodeString`) (in Delphi or FPC with _DelphiUnicode_ mode), then UTF-8 <-> UTF-16 conversion is done automatically,

- When `String` is 8-bit (`AnsiString`) (in FPC without _DelphiUnicode_ mode), without `CASTLE_DONT_CHANGE_STRING_ENCODING`, then UTF-8 <-> UTF-8 does nothing,

- _Does not work, because of FPC bug, so we don't support this combination_: When `String` is 8-bit (`AnsiString`) (in FPC without _DelphiUnicode_ mode), with `CASTLE_DONT_CHANGE_STRING_ENCODING`, then UTF-8 <-> platform encoding conversion in `AnsiString` is _not_ done automatically by FPC. See `TTestCompiler.TestAnsiStringUtf8Conversion_AnsiDefault`, testing `AnsiString` to/from `Utf8String`: FPC fails doing implicit conversions, only Delphi does them correctly. Use explicit `Utf8ToAnsi` / `AnsiToUtf8` to make it work with both FPC and Delphi.

Testcases in `TTestDownload` check various combinations with various compilers.

## Recommendations and what Lazarus does

In the bigger scheme of things, we recommend you adjust your code to CGE and Lazarus approaches: if you use FPC with `AnsiString`, assume `AnsiString` has UTF-8. Lazarus RTL assumes and does exactly the same thing as Castle Game Engine. See Lazarus sources, in `components/lazutils/fpcadds.pas`, it does:

```
{$ifdef UTF8_RTL}
initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$IFEND}
```

## TODO

- Eliminate in CGE code the remaining `AnsiString` in favor of `Utf8String` when we mean "8-bit, UTF-8 encoded". Use `AnsiString` only when we really mean "8-bit, possible system-specific encoding". This is done for the routines reading / writing text file contents (see "Automatic encoding conversions" above), but other places may remain. Document this, updating https://castle-engine.io/coding_conventions#strings_unicode:

  - We use `Utf8String` when we mean "string with 8-bit characters with UTF-8 encoding".

  - We use `AnsiString` when we mean "string with 8-bi characters with possibly platform-specific encoding". Almost nothing in Castle Game Engine is using this string.

- Possibly just make `CASTLE_DONT_CHANGE_STRING_ENCODING` default, with Delphi and FPC DelphiUnicode? No need for a define.

- Make it also default for Delphi packages. So we don't do `SetMultiByteConversionCodePage(CP_UTF8)` when being installed in Delphi IDE.

- Fix auto-tests.
    - delphi_12 test: without `CASTLE_DONT_CHANGE_STRING_ENCODING`: (check auto-tests win64):
      ```
      Processing: TTestCompiler.TestAnsiStringUtf8Conversion_Ansi1250
      TTestCompiler.TestAnsiStringUtf8Conversion_Ansi1250: Failed: EAssertionFailedError: AssertEquals: Expected Integer 1250, actual 65001
      Exception EAssertionFailedError in module castle-tester.exe at 00000000013003C4.
      AssertEquals: Expected Integer 1250, actual 65001.
      ```

- Test and fix CGE for FPC DelphiUnicode mode.

- `WriteStr/WritelnStr` autotest. `TCastleTextWriter.Write/Writeln` (`castledownload_text.inc`) should also now be good, add autotest. Expect UTF-8, add auto-tests for round-trip with other CGE routines.

- `castledownload_text.inc` — `TCastleTextReader.ReadBuf: AnsiString`, and `Readln/Read` return string. Fix, expect UTF-8, add auto-tests for round-trip with other CGE routines.
