{%MainUnit castlefonts.pas}
{
  Copyright 2001-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Raised by
    @link(TTextureFontData.Create) or @link(TCastleFont.Load) when
    the freetype library cannot be found, and thus font files cannot be read. }
  EFreeTypeLibraryNotFound = CastleTextureFontData.EFreeTypeLibraryNotFound;

const
  { Supported font file formats.
    Use these filters with LCL file dialog (easily set by FileFiltersToDialog)
    or TCastleWindow.FileDialog. }
  LoadFont_FileFilters =
  'All Files|*|' +
  '*All Font Files|*.ttf;*.otf|' +
  'TrueType Fonts (*.ttf)|*.ttf|' +
  'OpenType Fonts (*.otf)|*.otf';

{ Protect characters from being interpreted as special HTML sequences
  by TCastleAbstractFont.Print with Html = @true parameter.
  Replaces '<' with '&lt;' and so on. }
function SimpleHtmlQuote(const S: String): String;

{$endif read_interface}

{$ifdef read_implementation}

const
  { These many glyphs will be allocated always.
    This avoids reallocating memory if you just render strings shorter than this.

    This affects 2 arrays, and SizeOf(TFloatRectangle) should be 4 * 4.
    So it costs 32 bytes per item. }
  MinimumGlyphsAllocated = 100;

{ globals -------------------------------------------------------------------- }

function SimpleHtmlQuote(const S: String): String;
const
  Patterns: array [0..4] of String = ('&amp;', '&lt;', '&gt;', '&apos;', '&quot;');
  Replacements: array [0..4] of String = ('&', '<', '>', '''', '"');
begin
  Result := SReplacePatterns(S, Patterns, Replacements, false);
end;

{$endif read_implementation}
