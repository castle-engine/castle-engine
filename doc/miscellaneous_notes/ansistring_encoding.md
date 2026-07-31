## TODO

- Eliminate in CGE code all `AnsiString` in favor of `Utf8String` when we mean "8-bit, UTF-8 encoded". Use `AnsiString` only when we really mean "8-bit, possible system-specific encoding". Document this, updating https://castle-engine.io/coding_conventions#strings_unicode:

  - We use `Utf8String` when we mean "string with 8-bit characters with UTF-8 encoding".

  - We use `AnsiString` when we mean "string with 8-bi characters with possibly platform-specific encoding". Almost nothing in Castle Game Engine is using this string.

- Possibly just make `CASTLE_DONT_CHANGE_STRING_ENCODING` default, with Delphi and FPC DelphiUnicode? No need for a define.

- Make it also default for Delphi packages. So we don't do `SetMultiByteConversionCodePage(CP_UTF8)` when being installed in Delphi IDE.

- Fix auto-tests. Right now `CASTLE_DONT_CHANGE_STRING_ENCODING` + Delphi has one failure remaining, with reading filename with Chinese characters: `TTestDownload.TestLocalCharsCastleData`.

- Test and fix CGE for FPC DelphiUnicode mode.

## Introduction

By default, Castle Game Engine (just like Lazarus LCL) assumes that `AnsiString` contains UTF-8. In `initialization` of CastleUtils, we call:

- `SetMultiByteConversionCodePage(CP_UTF8)` (for both FPC and Delphi)
- `SetMultiByteRTLFileSystemCodePage` (for FPC only, since Delphi RTL uses 16-bit UnicodeString)

This gives us nice assumption. `String` is equal to either
- `AnsiString`, and it holds UTF-8
- `UnicodeString` (Delphi or FPC with _DelphiUnicode_ RTL), and it holds UTF-16

See https://castle-engine.io/coding_conventions#strings_unicode

Define `CASTLE_DONT_CHANGE_STRING_ENCODING` symbol when building the engine to avoid calling the above. You can do it by
- editing `src/common_includes/castleconf.inc`
- or adding proper `<define>` in [CastleEngineManifest.xml](https://castle-engine.io/project_manifest#_compiler_options_and_paths) if you compile using CGE engine,
- or adding a compilation symbol in Delphi project options if you compile using Delphi IDE,
- or any other place you can define a compilation symbol that affects compilation of Castle Game Engine units.

In effect, 8-bit strings (`AnsiString`) remain with system-specific encoding. This practically matters only on Windows, where `AnsiString` have locale-specific "ANSI encoding" by default, like Windows-1252. (On non-Windows systems, like Linux, the AnsiString uses in practice also UTF-8 since these systems use UTF-8 under the hood for all system APIs.)

## Note about FPC DelphiUnicode mode

We don't support building with FPC's _DelphiUnicode_ mode yet. We need to fix some assumptions to make it work (at this point, some pieces of engine assume that FPC -> implies we have 8-bit `String` equal to `AnsiString`). If you want to use _DelphiUnicode_ mode, please contact us and we will help you.

## Supported only for Delphi, unless you REALLY know what you're doing:)

The `CASTLE_DONT_CHANGE_STRING_ENCODING` option is right now only supported for Delphi. We made sure everything works in this case, and we don't need `AnsiString` to contain UTF-8, it can contain platform-specific encoding. 99% of the engine just uses `String` anyway (which equals `UnicodeString` in Delphi). In rare cases where we needed _"8-bit string with UTF-8, not any other, encoding"_, we used `Utf8String` and we have extensive automatic tests (grep for `TTestCompiler.TestAnsiStringUtf8Conversion`) to make sure it all rocks.

We initially planned to support it also for FPC (even without _DelphiUnicode_), and it's actually done "half-way". If you define `CASTLE_DONT_CHANGE_STRING_ENCODING`, and (if you know what you're doing!) you will hack `castleconf.inc` to not complain about it, you can build Castle Game Engine with FPC. But we do not guarantee that it will work correctly, and we do not support this configuration.

We resigned from full FPC support for `CASTLE_DONT_CHANGE_STRING_ENCODING`, because it would mean we would have to define own string type, like

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

and use it *everywhere* throughout the engine. This makes a big complication to contributing to the engine, every newcomer would need to read _"what is our string type"_. But we want simpler philosophy: _"Castle Game Engine just uses your default String, and you don't need to worry about it"_.

We initially wanted to use FPC macro `{$define String:=Utf8String}` to resolve this, but FPC macros cannot redefine keywords.

If you insist, and force engine to use `CASTLE_DONT_CHANGE_STRING_ENCODING` with FPC, be aware that subtle things will break. Our engine assumes UTF-8 when using routines like JSON, XML processing, font display, file opening/saving. We do not guarantee in such case what happens if you try to use non-ASCII characters with our engine.

## Automatic encoding conversions

When `CASTLE_DONT_CHANGE_STRING_ENCODING` is defined, and your codebase uses `AnsiString` with native platform encoding, you rely on automatic encoding conversions between

- `AnsiString` and `UnicodeString`

- `AnsiString` and `Utf8String`. Note that there are some quirks in how this works, and FPC 3.2.2 is not perfectly compatible with Delphi, and FPC 3.2.0 has more issues. See the tests in `TTestCompiler.TestAnsiStringUtf8Conversion` for details.

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