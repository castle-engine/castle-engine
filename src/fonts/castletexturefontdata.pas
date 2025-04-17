{
  Copyright 2014-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Data for a 2D font initialized from a FreeType font file (TTextureFontData). }
unit CastleTextureFontData;

{$I castleconf.inc}

{ Workaround a convoluted bug in Delphi < 10.4.
  Doesn't affect new Delphi versions, doesn't affect FPC.

  More precisely, we confirmed
  - it occurs in Delphi 10.2.3
  - it is fixed in Delphi 10.4.2 and all newer Delphi versions we tested (11.x, 12.x).

  Details:

  In these older Delphi versions,
  using "inherited", "inherited Create" or "inherited Create(nil)"
  in TTextureFontData.TGlyphDictionary.Create
  leaves the created instance in state when internal FComparer is nil
  (as if TDictionary<TKey,TValue>.Create constructor wasn't called).
  Looks like having own constructor TGlyphDictionary.Create confuses
  these early Delphi versions.

  Workaround is to create and use own comparer, instance of
  TUnicodeCharEqualityComparer.

  This isn't related to whether this constructor has "reintroduce" or not,
  tested.

  In effect,
  - Hash method causes Access Violation,
  - and in effect all other routines (Add, AddOrSetValue, our SetItems)
    cause Access Violation.

  In the past, we even applied this change to any Delphi version, because
  - The TUnicodeCharEqualityComparer makes sense anyway, the default
    comparer from Generics.Collections for Cardinal wouldn't do anything
    substantially different or more optimal.
  - This way TUnicodeCharEqualityComparer will be tested even when we run
    through latest Delphi, like 11.
  - This way we don't care about carefully testing at which Delphi version
    (10.3.x, 10.4.x?) the bug is fixed.

  However, we later found out the workaround is not really applicable
  to all Delphi versions. Freeing our comparer ("FreeAndNil(FComparer)")
  makes a crash with Delphi 12.0, only in RELEASE mode (goes OK in DEBUG...).
  Looks like in newer Delphis, we are not supposed to free the comparer.
  But not freeing it means we most likely have a memory leak in older Delphis.
  So part of the workaround would have to be conditional on Delphi version anyway.

  So we apply the workaround selectively, only to older Delphis.
}
{$ifndef FPC}
  {$if CompilerVersion < 34}
    {$define CASTLE_WORKAROUND_GLYPH_COMPARER}
  {$endif}
{$endif}

interface

uses Generics.Collections, Generics.Defaults,
  CastleVectors, CastleUnicode, CastleStringUtils, CastleImages,
  CastleInternalFreeType, CastleRectangles;

type
  { Raised by
    @link(TTextureFontData.Create) or @link(TCastleFont.Load) when
    the freetype library cannot be found, and thus font files cannot be read. }
  EFreeTypeLibraryNotFound = CastleInternalFreeType.EFreeTypeLibraryNotFound;

  { Additional metadata about TTextureFontData, used when embedding
    and recreating TTextureFontData instances. }
  TTextureFontDataInformation = class
    Size: Cardinal;
    AntiAliased: Boolean;
    FamilyName, StyleName: String;
    Bold, Italic: Boolean;
  end;

  { Data for a 2D font initialized from a FreeType font file, like ttf. }
  TTextureFontData = class
  private
    const
      DistanceFieldPadding = 6;
    var
      FAdditionalPadding: Integer;
      FDistanceField: Boolean;
  public
    type
      { Information about a particular font glyph. }
      TGlyph = class
      public
        { How to shift the glyph with respect
          to the starting position when drawing. }
        X, Y: Integer;

        { How to advance the position for next glyph. }
        AdvanceX, AdvanceY: Integer;

        { Size of the glyph.
          Always Width and Height >= 0 (they are Cardinal type after all),
          but note that it is possible that Width = Height = 0
          (it commonly happens for space ' ' character).

          For rendering, use GlyphDrawImageRect to get the actual size,
          as these fields include an extra padding in case of distance field rendering. }
        Width, Height: Cardinal;

        { Position of the glyph on the image in TTextureFontData.Image.

          For rendering, use GlyphDrawImageRect to get the actual size,
          as these fields include an extra padding in case of distance field rendering. }
        ImageX, ImageY: Cardinal;
      end;

      { Map Unicode code to a TGlyph representation. }
      TGlyphDictionary = class({$ifdef FPC}specialize{$endif} TDictionary<TUnicodeChar, TGlyph>)
      strict private
        {$ifdef CASTLE_WORKAROUND_GLYPH_COMPARER}
        type
          TUnicodeCharEqualityComparer = class(TCustomComparer<TUnicodeChar>)
            function Compare(const Left, Right: TUnicodeChar): Integer; override;
            function Equals(const Left, Right: TUnicodeChar): Boolean; override;
            function GetHashCode(const Value: TUnicodeChar): Integer; override;
          end;
        var
          FComparer: TUnicodeCharEqualityComparer;
        {$endif}

        FOwnsGlyphs: boolean;
        function GetItems(const AKey: TUnicodeChar): TGlyph;
        procedure SetItems(const AKey: TUnicodeChar; const AValue: TGlyph);
      public
        property OwnsGlyphs: boolean read FOwnsGlyphs write FOwnsGlyphs default true;
        { Access dictionary items.
          Setting this is allowed regardless if the key previously existed or not,
          in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
          that only allows setting when the key already exists. }
        property Items [const AKey: TUnicodeChar]: TGlyph read GetItems write SetItems; default;
        constructor Create; reintroduce;
        destructor Destroy; override;
      end;
  private
    type
      TGlyphCharDictionary = array [Byte] of TGlyph;
    const
      MaxFallbackGlyphWarnings = 10;
    var
      FUrl: String;
      FAntiAliased: Boolean;
      FSize: Cardinal;
      { For optimization of rendering normal 8-bit fonts (like standard ASCII
        text), we keep glyphs with index < 256 listed in TGlyphCharDictionary.
        Only the glyphs with index >= 256 are kept on extra TGlyphDictionary. }
      { Non-nil only for filled glyphs. }
      FGlyphsByte: TGlyphCharDictionary;
      FGlyphsExtra: TGlyphDictionary;
      FImage: TGrayscaleImage;
      FFirstExistingGlyph: TGlyph;
      FFirstExistingGlyphChar: TUnicodeChar;
      { If the requested glyph doesn't exit, @link(Glyph) will use this one
        as a fallback. }
      FFallbackGlyph: TGlyph;
      FFallbackGlyphChar: TUnicodeChar;
      FUseFallbackGlyph: Boolean;
      FallbackGlyphWarnings: Integer;
      FFamilyName, FStyleName: String;
      FBold, FItalic: Boolean;

    procedure CalculateFallbackGlyph;
    procedure MakeFallbackWarning(const C: TUnicodeChar);

    { Height of the glyph in the image,
      not counting the additional padding added when rendering with distance field fonts.
      This is a faster shortcut for GlyphDrawImageRect(G).Height. }
    function GlyphDrawHeight(const G: TTextureFontData.TGlyph): Cardinal;

    { Non-zero when distance field rendering is used.
      You need to account for it when rendering the glyph image. }
    property AdditionalPadding: Integer read FAdditionalPadding;
  public
    { Create by reading a FreeType font file, like ttf.

      Providing charaters list as @nil means that we only create glyphs
      for SimpleAsciiCharacters, which includes only the basic ASCII characters.
      The ACharacters instance @italic(does not) become owned by this object,
      so remember to free it after calling this constructor.

      @raises EFreeTypeLibraryNotFound If the freetype library is not installed. }
    constructor Create(const AUrl: String;
      const ASize: Cardinal; const AnAntiAliased: Boolean;
      ACharacters: TUnicodeCharList = nil; const ADistanceField: Boolean = false);

    { Create from a ready data for glyphs and image.
      Useful when font data is embedded inside the Pascal source code.
      AGlyphs instance, and AImage instance, become owned by this class. }
    constructor CreateFromData(const AGlyphs: TGlyphDictionary;
      const AImage: TGrayscaleImage;
      const ASize: Cardinal; const AnAntiAliased: Boolean); overload;
    constructor CreateFromData(const AGlyphs: TGlyphDictionary;
      const AImage: TGrayscaleImage;
      const Information: TTextureFontDataInformation); overload;

    destructor Destroy; override;

    property Url: String read FUrl;

    { Size of the font data (which is the optimal size to display this font,
      without any scaling), in pixels. }
    property Size: Cardinal read FSize;

    { Whether the font data was generated with anti-aliasing. }
    property AntiAliased: Boolean read FAntiAliased;

    { Family name, obtained from the font file. }
    property FamilyName: String read FFamilyName;

    { Style name, obtained from the font file. This should correspond
      to the @link(Bold) and @link(Italic) properties, but e.g. "Italic"
      may be called "Oblique" depending on how it was generated. }
    property StyleName: String read FStyleName;

    { Is the font a bold font (obtained from the font file). }
    property Bold: Boolean read FBold;

    { Is the font an italic font (obtained from the font file). }
    property Italic: Boolean read FItalic;

    { Read-only information about a glyph for given character.

      When AllowUsingFallbackGlyph and UseFallbackGlyph (both are @true
      by default) then we always return non-nil glyph.
      If the desired glyph was not really present, we make a warning
      (using WritelnWarning) and return a fallback glyph.

      When not (AllowUsingFallbackGlyph and UseFallbackGlyph)
      then we return @nil for a missing glyph.
      Glyph may be missing because it was not requested at constructor,
      or because it doesn't exist in the font data. }
    function Glyph(const C: TUnicodeChar;
      const AllowUsingFallbackGlyph: Boolean = true): TGlyph;

    { When a glyph (picture of a particular character) in a font doesn't exist,
      by default we make a warning (using WritelnWarning) and use a fallback
      glyph, like "?". This lets user know that some character is there.

      Set this to @false to just silently omit a missing glyph.
      The @link(Glyph) method will just return (silently) @nil in this case. }
    property UseFallbackGlyph: Boolean
      read FUseFallbackGlyph write FUseFallbackGlyph default true;

    property Image: TGrayscaleImage read FImage;

    { List all characters for which glyphs are actually loaded.
      @link(Glyph) will answer non-nil exactly for these characters.
      The resulting list instance is owned by caller, so take care to free it. }
    function LoadedGlyphs: TUnicodeCharList;

    function TextWidth(const S: string): Integer;
    function TextHeight(const S: string): Integer;
    { The height (above the baseline) of the text.
      This doesn't take into account height of the text below the baseline
      (for example letter "y" has the tail below the baseline in most fonts). }
    function TextHeightBase(const S: string): Integer;
    function TextMove(const S: string): TVector2Integer;

    { Is the font prepared for distance field rendering. }
    property DistanceField: Boolean read FDistanceField;

    { Rect of the glyph in the image,
      without the additional padding added when rendering with distance field fonts.

      To get the full rect of the glyph in the image, with padding,
      use G.ImageX, G.ImageY, G.Width, G.Height. }
    function GlyphDrawImageRect(const G: TTextureFontData.TGlyph): TRectangle;
  end;

const
  { Supported font file formats.
    Use these filters with LCL file dialog (easily set by FileFiltersToDialog)
    or TCastleWindow.FileDialog. }
  LoadFont_FileFilters =
    'All Files|*|' +
    '*All Font Files|*.ttf;*.otf;*.woff;*.woff2|' +
    'TrueType Fonts (*.ttf)|*.ttf|' +
    'OpenType Fonts (*.otf)|*.otf|' +
    'WOFF Fonts (*.woff,*.woff2)|*.woff;*.woff2';

{ TCastleFont.Load should use a given TTextureFontData instance
  instead of loading from given URL.
  This allows to make embedded fonts work seamlessly. }
procedure RegisterEmbeddedFont(const FontData: TTextureFontData;
  const FontUrl: String);

{ Is any font registered for given URL.
  @nil if none. }
function GetEmbeddedFont(const FontUrl: String): TTextureFontData;

implementation

uses Classes, SysUtils, Character,
  CastleLog, CastleUtils, CastleUriUtils, CastleFilesUtils, CastleDownload,
  CastleInternalFreeTypeH;

{ TUnicodeCharEqualityComparer ----------------------------------------------- }

{$ifdef CASTLE_WORKAROUND_GLYPH_COMPARER}

function TTextureFontData.TGlyphDictionary.TUnicodeCharEqualityComparer.
  Compare(const Left, Right: TUnicodeChar): Integer;
begin
  Result := Left - Right;
end;

function TTextureFontData.TGlyphDictionary.TUnicodeCharEqualityComparer.
  Equals(const Left, Right: TUnicodeChar): Boolean;
begin
  Result := Left = Right;
end;

function TTextureFontData.TGlyphDictionary.TUnicodeCharEqualityComparer.
  GetHashCode(const Value: TUnicodeChar): Integer;
begin
  Result := Value;
end;

{$endif}

{ TTextureFontData.TGlyphDictionary ------------------------------------------ }

constructor TTextureFontData.TGlyphDictionary.Create;
begin
  {$ifdef CASTLE_WORKAROUND_GLYPH_COMPARER}
  FComparer := TUnicodeCharEqualityComparer.Create;
  inherited Create(FComparer);
  {$else}
  inherited;
  {$endif}

  FOwnsGlyphs := true;
end;

destructor TTextureFontData.TGlyphDictionary.Destroy;
var
  G: TGlyph;
begin
  if OwnsGlyphs then
    for G in Values do
      G.Free;
  Clear;
  inherited;
  {$ifdef CASTLE_WORKAROUND_GLYPH_COMPARER}
  { Don't do this on new Delphis.
    This makes a crash with Delphi 12 when compiled
    in RELEASE (but not DEBUG) mode.

    Now CASTLE_WORKAROUND_GLYPH_COMPARER is only defined for older Delphis,
    should we free or not?

    Decision: Let eventual memory leaks happen with older Delphis.
    Better memory leaks, than crash.
    Everything is great (no crash, no leak) with Delphis >= 10.4. }
  //FreeAndNil(FComparer);
  {$endif}
end;

function TTextureFontData.TGlyphDictionary.GetItems(const AKey: TUnicodeChar): TGlyph;
begin
  Result := inherited Items[AKey];
end;

procedure TTextureFontData.TGlyphDictionary.SetItems(const AKey: TUnicodeChar; const AValue: TGlyph);
begin
  AddOrSetValue(AKey, AValue);
end;

{ TTextureFontData ----------------------------------------------------------------- }

constructor TTextureFontData.Create(const AUrl: String;
  const ASize: Cardinal; const AnAntiAliased: Boolean;
  ACharacters: TUnicodeCharList; const ADistanceField: Boolean);
var
  FontId: Integer;

  function GetGlyphInfo(const C: TUnicodeChar): TGlyph;
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;
  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeCharToString(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeCharToString(C), Size);

    try
      if Bitmaps.Count = 0 then
      begin
        WritelnWarning('Font', Format('Font "%s" does not contain glyph for character "%s" (index %d)',
          [Url, C, Ord(C)]));
        Exit(nil);
      end;

      Bitmap := Bitmaps.Bitmaps[0];
      if Bitmaps.Count > 1 then
        WritelnWarning('Font', Format('Font "%s" contains a sequence of glyphs (more than a single glyph) for a single character "%s" (index %d)',
          [Url, C, Ord(C)]));
      if (Bitmap^.Width < 0) or (Bitmap^.Height < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Width or Height < 0 for character "%s" (index %d)',
          [Url, C, Ord(C)]));
        Exit(nil);
      end;

      Result := TGlyph.Create;
      Result.Width    := Bitmap^.Width + 2 * AdditionalPadding;
      Result.Height   := Bitmap^.Height + 2 * AdditionalPadding;
      Result.X        := -Bitmap^.X;
      Result.Y        := Bitmap^.Height - 1 + Bitmap^.Y;
      Result.AdvanceX := Bitmap^.AdvanceX shr 10; // 64 * 16, looks like this is just magic for freetype
      Result.AdvanceY := Bitmap^.AdvanceY shr 10; // 64 * 16, looks like this is just magic for freetype
    finally FreeAndNil(Bitmaps) end;
  end;

  { Copy glyph data for character C (assuming it is Ok, that is GetGlyphInfo
    returned non-nil for this) to the Image (at position ImageX, ImageY). }
  procedure GetGlyphData(const C: TUnicodeChar; const ImageX, ImageY: Cardinal);
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;

    { Extracting data from glyph with Pitch, like in TFreeTypeFont.DrawChar. }
    procedure DrawChar;
    var
      B, RX, RY: Integer;
    begin
      B := 0;
      for RY := 0 to Bitmap^.Height - 1 do
      begin
        for RX := 0 to Bitmap^.Width - 1 do
          Image.PixelPtr(ImageX + RX, ImageY + Bitmap^.Height - 1 - RY)^ := Bitmap^.Data^[B + RX];
        Inc(B, Bitmap^.Pitch);
      end;
    end;

    { Generate distance field font texture.

      The algorithm is roughly described at https://libgdx.com/wiki/graphics/2d/fonts/distance-field-fonts
      Here we adjust it for anti-aliased image received in Bitmap^.

      For every pixel we calculate distance to:
      a) (at least partially) opaque pixel
      b) full transparent pixel

      Then we blend those two distances based on opaqueness of the current pixel:
      * for fully opaque pixel it's distance to the nearest transaprent pixel
      * for fully transparent pixel it's distance to the nearest opaque pixel
      * for semi-transparent pixel it's something in-between
        (the formula was invented by trial-and-error and could be improved)

      Notes:

      1.We do not normalize the result as we're supposed to!
        It would force us to use two-pass algorithm, which will slow things down
        significantly. And it seems like the current algorithm is enough
        to get a good quality image.

      2.This algorithm is O(n^4) and as such is rather slow
        (approx. 50 times slower than generating a normal font texture),
        however, absolute time can be considered negligible (far under a second).
        This means distance field font will take longer time to load,
        it's a single-time loading procedure though.
        TCastleFont will also apply a different shader on top of the generated texture
        which also can slow down performance a tiny bit,
        however this effect should be completely negligible.

      3.We also add special padding to each symbol of the font,
        which sometimes results in larger texure generated. }
    procedure DrawCharDistanceField;
    var
      RX, RY, AX, AY: Integer;
      DX, DY: Integer;
      MaxB: Byte;
      DTransparent, DOpaque, TempD: Integer; //6^2 + 6^2 = 72
      Opqaueness: Single;
    begin
      MaxB := 0;
      for RY := 0 to Bitmap^.Height - 1 do
        for RX := 0 to Bitmap^.Width - 1 do
          if Bitmap^.Data^[RY + RY * Bitmap^.Pitch] > MaxB then
            MaxB := Bitmap^.Data^[RY + RY * Bitmap^.Pitch];
      if MaxB = 0 then
        MaxB := 255; //doesn't matter in this case, the glyph doesn't have a single opaque pixel

      for RY := -AdditionalPadding to Bitmap^.Height - 1 + AdditionalPadding do
        for RX := -AdditionalPadding to Bitmap^.Width - 1 + AdditionalPadding do
          begin
            // opaque pixel - calculate distance to nearest transparent pixel
            DTransparent := Sqr(AdditionalPadding);
            DOpaque := Sqr(AdditionalPadding);
            for DY := -AdditionalPadding to AdditionalPadding do
              for DX := -AdditionalPadding to AdditionalPadding do
              begin
                TempD := Sqr(DX) + Sqr(DY);
                AX := RX + DX;
                AY := RY + DY;
                if (AX >= 0) and (AX < Bitmap^.Width) and (AY >= 0) and (AY < Bitmap^.Height) and (Bitmap^.Data^[AX + AY * Bitmap^.Pitch] > 0) then
                begin
                  if DOpaque > TempD then
                    DOpaque := TempD;
                end else
                begin
                  if DTransparent > TempD then
                    DTransparent := TempD;
                end;
              end;
            if (RX >= 0) and (RX < Bitmap^.Width) and (RY >= 0) and (RY < Bitmap^.Height) then
              Opqaueness := Single(Bitmap^.Data^[RX + RY * Bitmap^.Pitch]) / Single(MaxB)
            else
              Opqaueness := 0;
            Image.PixelPtr(ImageX + RX + AdditionalPadding, ImageY + Bitmap^.Height - 1 - RY + AdditionalPadding)^ :=
              Trunc(
                Opqaueness * (128 + 127 * Single(DTransparent) / Sqr(AdditionalPadding)) +
                (1 - Opqaueness) * (127 * (1.0 - Single(DOpaque) / Sqr(AdditionalPadding)))
              );
          end;
    end;

    { Extracting data with Pitch, like in TFreeTypeFont.DrawCharBW. }
    procedure DrawCharBW;
    const
      Bits: array [0..7] of Byte = (128,64,32,16,8,4,2,1);
    var
      RB: Byte;
      RX, RY, B, L: Integer;
    begin
      B := 0;
      for RY := 0 to Bitmap^.Height - 1 do
      begin
        L := 0;
        for RX := 0 to Bitmap^.Width - 1 do
        begin
          RB := RX mod 8;
          if (Bitmap^.Data^[B + L] and Bits[RB]) <> 0 then
            Image.PixelPtr(ImageX + RX, ImageY + Bitmap^.Height - 1 - RY)^ := 255;
          if RB = 7 then
            Inc(L);
        end;
        Inc(B, Bitmap^.Pitch);
      end;
    end;

  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeCharToString(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeCharToString(C), Size);
    try
      Bitmap := Bitmaps.Bitmaps[0];
      if (Bitmap^.Pitch < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Pitch < 0 for character "%s" (index %d)',
          [Url, C, Ord(C)]));
        Exit;
      end;
      if DistanceField then
        DrawCharDistanceField
      else
      if AntiAliased then
        DrawChar
      else
        DrawCharBW;
    finally FreeAndNil(Bitmaps) end;
  end;

  procedure ReadFontMetadata;
  var
    FreeTypeFont: PFT_Face;
  begin
    FreeTypeFont := FontMgr.GetFreeTypeFont(FontId);
    FFamilyName := FreeTypeFont^.family_name;
    FStyleName := FreeTypeFont^.style_name;
    FBold := FreeTypeFont^.style_flags and FT_STYLE_FLAG_BOLD <> 0;
    FItalic := FreeTypeFont^.style_flags and FT_STYLE_FLAG_ITALIC <> 0;
  end;

const
  { Separate the glyphs for safety, to avoid pulling in colors
    from neighboring letters when drawing (floating point errors could in theory
    make small errors moving us outside of the desired pixel). }
  GlyphPadding = 2;

var
  GlyphInfo: TGlyph;
  GlyphsCount, ImageSize: Cardinal;
  MaxWidth, MaxHeight, ImageX, ImageY: Cardinal;
  C: TUnicodeChar;
  TemporaryCharacters: boolean;
begin
  inherited Create;
  FDistanceField := ADistanceField;
  if DistanceField then
    FAdditionalPadding := DistanceFieldPadding
  else
    FAdditionalPadding := 0;
  FUrl := AUrl;
  FSize := ASize;
  FAntiAliased := AnAntiAliased;
  FUseFallbackGlyph := true;

  InitFontMgr;
  FontId := FontMgr.RequestFont(Url);

  TemporaryCharacters := ACharacters = nil;
  if TemporaryCharacters then
  begin
    ACharacters := TUnicodeCharList.Create;
    ACharacters.Add(SimpleAsciiCharacters);
  end;

  ReadFontMetadata;

  {$ifdef WASI}
  // TODO: web: WASI does not support FreeType library, also we fail without exceptions because WASI doesn't have longjmp
  WritelnWarning('TCastleFont', 'Cannot load font "%s", WASI does not support FreeType library', [
    UriDisplay(Url)
  ]);
  Exit;
  {$endif}

  try
    FGlyphsExtra := TGlyphDictionary.Create;

    GlyphsCount := 0;
    MaxWidth    := 0;
    MaxHeight   := 0;
    for C in ACharacters do
    begin
      GlyphInfo := GetGlyphInfo(C);
      if C <= High(FGlyphsByte) then
        FGlyphsByte[C] := GlyphInfo
      else
        FGlyphsExtra[C] := GlyphInfo;
      if GlyphInfo <> nil then
      begin
        if FFirstExistingGlyph = nil then
        begin
          FFirstExistingGlyph := GlyphInfo;
          FFirstExistingGlyphChar := C;
        end;
        Inc(GlyphsCount);
        MaxVar(MaxWidth , GlyphInfo.Width);
        MaxVar(MaxHeight, GlyphInfo.Height);
      end else
        WritelnWarning('Font "%s" does not contain requested character %s (Unicode number %d)',
          [UriDisplay(Url), UnicodeCharToString(C), C]);
    end;

    if GlyphsCount = 0 then
      raise Exception.Create('Cannot create a font with no glyphs');

    MaxWidth := MaxWidth + GlyphPadding;
    MaxHeight := MaxHeight + GlyphPadding;
    if DistanceField then
    begin
      MaxWidth := MaxWidth + 2 * AdditionalPadding;
      MaxHeight := MaxHeight + 2 * AdditionalPadding;
    end;

    ImageSize := 8;
    while (ImageSize div MaxHeight) * (ImageSize div MaxWidth) < GlyphsCount do
      ImageSize := ImageSize * 2;

    WritelnLog('Font', 'Creating image %dx%d to store glyphs of font "%s" (%d glyphs, max glyph size (including %d pixel padding) is %dx%d)',
      [ImageSize, ImageSize, Url, GlyphsCount, GlyphPadding, MaxWidth, MaxHeight]);

    FImage := TGrayscaleImage.Create(ImageSize, ImageSize);
    Image.Clear(0);
    Image.TreatAsAlpha := true;
    // Image.Url doesn't change image contents, it is only information for profiler
    Image.Url := Url + Format('[font converted to a texture, size: %d, anti-aliased: %s]', [
      Size,
      BoolToStr(AntiAliased, true)
    ]);

    ImageX := 0;
    ImageY := 0;
    for C in ACharacters do
    begin
      GlyphInfo := Glyph(C, false);
      if GlyphInfo <> nil then
      begin
        GlyphInfo.ImageX := ImageX;
        GlyphInfo.ImageY := ImageY;

        GetGlyphData(C, ImageX, ImageY);

        ImageX := ImageX + MaxWidth;
        if ImageX + MaxWidth >= ImageSize then
        begin
          ImageX := 0;
          ImageY := ImageY + MaxHeight;
        end;
      end;
    end;

    // Debug: SaveImage(Image, '/tmp/a.png');
  finally
    if TemporaryCharacters then
      FreeAndNil(ACharacters);
  end;

  CalculateFallbackGlyph;
end;

constructor TTextureFontData.CreateFromData(const AGlyphs: TGlyphDictionary;
  const AImage: TGrayscaleImage;
  const ASize: Cardinal; const AnAntiAliased: Boolean);
var
  Information: TTextureFontDataInformation;
begin
  Information := TTextureFontDataInformation.Create;
  try
    Information.Size := ASize;
    Information.AntiAliased := AnAntiAliased;
    CreateFromData(AGlyphs, AImage, Information);
  finally FreeAndNil(Information) end;
end;

constructor TTextureFontData.CreateFromData(const AGlyphs: TGlyphDictionary;
  const AImage: TGrayscaleImage;
  const Information: TTextureFontDataInformation);
var
  C: TUnicodeChar;
  GlyphPair: {$ifdef FPC}TGlyphDictionary.TDictionaryPair{$else}TPair<TUnicodeChar, TGlyph>{$endif};
begin
  inherited Create;
  FUrl := AImage.Url; // this is only for debug purposes now (to potentially display in debug, profiler etc.)

  // restore font information properties from TTextureFontDataInformation
  FSize := Information.Size;
  FAntiAliased := Information.AntiAliased;
  FFamilyName := Information.FamilyName;
  FStyleName := Information.StyleName;
  FBold := Information.Bold;
  FItalic := Information.Italic;

  FUseFallbackGlyph := true;

  // WritelnLog('Creating font from %s with %d glyphs', [
  //   AImage.Url,
  //   AGlyphs.Count
  // ]);

  { split AGlyphs into FGlyphsByte and FGlyphsExtra }
  FGlyphsExtra := TGlyphDictionary.Create;
  for GlyphPair in AGlyphs do
  begin
    C := GlyphPair.Key;
    if C <= High(FGlyphsByte) then
      FGlyphsByte[C] := GlyphPair.Value
    else
      FGlyphsExtra[C] := GlyphPair.Value;

    if FFirstExistingGlyph = nil then
    begin
      FFirstExistingGlyph := GlyphPair.Value;
      FFirstExistingGlyphChar := C;
    end;
  end;
  AGlyphs.OwnsGlyphs := false;
  AGlyphs.Free; // we own AGlyphs, for now we just free them

  FImage := AImage;

  CalculateFallbackGlyph;
end;

destructor TTextureFontData.Destroy;
var
  C: Byte;
begin
  FreeAndNil(FGlyphsExtra);
  for C := Low(Byte) to High(Byte) do
    FreeAndNil(FGlyphsByte[C]);
  FreeAndNil(FImage);
  inherited;
end;

procedure TTextureFontData.CalculateFallbackGlyph;

  function TryFallback(const C: TUnicodeChar): Boolean;
  begin
    FFallbackGlyph := Glyph(C, false);
    FFallbackGlyphChar := C;
    Result := FFallbackGlyph <> nil;
  end;

begin
  if not TryFallback(Ord('?')) then
    if not TryFallback(Ord('_')) then
      if not TryFallback(Ord('.')) then
      begin
        FFallbackGlyph := FFirstExistingGlyph;
        FFallbackGlyphChar := FFirstExistingGlyphChar;
      end;
end;

function TTextureFontData.Glyph(const C: TUnicodeChar;
  const AllowUsingFallbackGlyph: Boolean): TGlyph;
begin
  if C <= High(FGlyphsByte) then
    Result := FGlyphsByte[C]
  else
  if not FGlyphsExtra.TryGetValue(C, Result) then
    Result := nil;

  if (Result = nil) and AllowUsingFallbackGlyph and UseFallbackGlyph then
  begin
    Result := FFallbackGlyph;
    MakeFallbackWarning(C);
  end;
end;

procedure TTextureFontData.MakeFallbackWarning(const C: TUnicodeChar);
begin
  if FallbackGlyphWarnings < MaxFallbackGlyphWarnings then
  begin
    Inc(FallbackGlyphWarnings);
    WritelnWarning('Font is missing glyph for character %s (Unicode number %d)', [
      UnicodeCharToString(C),
      C
    ]);
    if FallbackGlyphWarnings = MaxFallbackGlyphWarnings then
      WritelnWarning('No further warnings about missing glyphs will be reported for this font (to avoid slowing down the application by flooding the log with warnings)');
  end;
end;

function TTextureFontData.LoadedGlyphs: TUnicodeCharList;
var
  C: TUnicodeChar;
begin
  Result := TUnicodeCharList.Create;
  for C := 0 to High(FGlyphsByte) do
    if FGlyphsByte[C] <> nil then
      Result.Add(C);
  for C in FGlyphsExtra.Keys do
    Result.Add(C);
end;

function TTextureFontData.TextWidth(const S: string): Integer;
var
  Iter: TCastleStringIterator;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  Iter.Start(S);
  while Iter.GetNext do
  begin
    G := Glyph(Iter.Current);
    if G <> nil then
      Result := Result + G.AdvanceX;
  end;
end;

function TTextureFontData.GlyphDrawHeight(const G: TTextureFontData.TGlyph): Cardinal;
begin
  Assert(G.Height >= 2 * AdditionalPadding);
  // same as GlyphDrawImageRect calculation
  Result := G.Height - 2 * AdditionalPadding;
end;

function TTextureFontData.GlyphDrawImageRect(const G: TTextureFontData.TGlyph): TRectangle;
begin
  Result.Left   := G.ImageX + AdditionalPadding;
  Result.Bottom := G.ImageY + AdditionalPadding;
  Result.Width  := G.Width  - 2 * AdditionalPadding;
  // same as GlyphDrawHeight calculation
  Result.Height := G.Height - 2 * AdditionalPadding;
end;

function TTextureFontData.TextHeight(const S: string): Integer;
var
  Iter: TCastleStringIterator;
  MinY, MaxY, YOrigin: Integer;
  G: TTextureFontData.TGlyph;
begin
  MinY := 0;
  MaxY := 0;

  Iter.Start(S);
  while Iter.GetNext do
  begin
    G := Glyph(Iter.Current);
    if G <> nil then
    begin
      YOrigin := G.Y;
      MinVar(MinY, -YOrigin);
      MaxVar(MaxY, GlyphDrawHeight(G) - YOrigin);
    end;
  end;
  Result := MaxY - MinY;
end;

function TTextureFontData.TextMove(const S: string): TVector2Integer;
var
  Iter: TCastleStringIterator;
  G: TTextureFontData.TGlyph;
begin
  Result := TVector2Integer.Zero;

  Iter.Start(S);
  while Iter.GetNext do
  begin
    G := Glyph(Iter.Current);
    if G <> nil then
    begin
      Result.X := Result.X + G.AdvanceX;
      Result.Y := Result.Y + G.AdvanceY;
    end;
  end;
end;

function TTextureFontData.TextHeightBase(const S: string): Integer;
var
  Iter: TCastleStringIterator;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  { This is just like TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }
  Iter.Start(S);
  while Iter.GetNext do
  begin
    G := Glyph(Iter.Current);
    if G <> nil then
      MaxVar(Result, GlyphDrawHeight(G) - G.Y);
  end;
end;

{ global routines ----------------------------------------------------------- }

var
  FEmbeddedFonts: TStringList;

procedure RegisterEmbeddedFont(const FontData: TTextureFontData;
  const FontUrl: String);
begin
  if FEmbeddedFonts = nil then
  begin
    FEmbeddedFonts := TStringList.Create;
    // because data URLs *may* ignore case when CastleDataIgnoreCase
    FEmbeddedFonts.CaseSensitive := false;
  end;
  FEmbeddedFonts.AddObject(FontUrl, FontData);
end;

function GetEmbeddedFont(const FontUrl: String): TTextureFontData;
var
  Index: Integer;
begin
  Result := nil;
  if FEmbeddedFonts <> nil then
  begin
    Index := FEmbeddedFonts.IndexOf(FontUrl);
    if Index <> -1 then
      Result := FEmbeddedFonts.Objects[Index] as TTextureFontData;
  end;
end;

end.
