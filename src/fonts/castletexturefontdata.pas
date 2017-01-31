{
  Copyright 2014-2017 Michalis Kamburelis.

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

interface

uses FGL,
  CastleVectors, CastleUnicode, CastleStringUtils, CastleImages;

type
  { Data for a 2D font initialized from a FreeType font file, like ttf. }
  TTextureFontData = class
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
          (it commonly happens for space ' ' character). }
        Width, Height: Cardinal;
        { Position of the glyph on the image in TTextureFontData.Image. }
        ImageX, ImageY: Cardinal;
      end;
      { Map Unicode code to a TGlyph representation. }
      TGlyphDictionary = class(specialize TFPGMap<TUnicodeChar, TGlyph>)
      private
        FOwnsGlyphs: boolean;
      public
        property OwnsGlyphs: boolean read FOwnsGlyphs write FOwnsGlyphs default true;
        constructor Create;
        destructor Destroy; override;
      end;
  private
    type
      TGlyphCharDictionary = array [Byte] of TGlyph;
    var
    FAntiAliased: boolean;
    FSize: Integer;
    { For optimization of rendering normal 8-bit fonts (like standard ASCII
      text), we keep glyphs with index < 256 listed in TGlyphCharDictionary.
      Only the glyphs with index >= 256 are kept on extra TGlyphDictionary. }
    { Non-nil only for filled glyphs. }
    FGlyphsByte: TGlyphCharDictionary;
    FGlyphsExtra: TGlyphDictionary;
    FImage: TGrayscaleImage;
    MeasureDone: boolean;
    FRowHeight, FRowHeightBase, FDescend: Integer;
    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
  public
    {$ifdef HAS_FREE_TYPE}
    { Create by reading a FreeType font file, like ttf.

      Providing charaters list as @nil means that we only create glyphs
      for SimpleAsciiCharacters, which includes only the basic ASCII characters.
      The ACharacters instance @italic(does not) become owned by this object,
      so remember to free it after calling this constructor. }
    constructor Create(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      ACharacters: TUnicodeCharList = nil);
    {$endif}
    { Create from a ready data for glyphs and image.
      Useful when font data is embedded inside the Pascal source code.
      AGlyphs instance, and AImage instance, become owned by this class. }
    constructor CreateFromData(const AGlyphs: TGlyphDictionary;
      const AImage: TGrayscaleImage;
      const ASize: Integer; const AnAntiAliased: boolean);
    destructor Destroy; override;

    property AntiAliased: boolean read FAntiAliased;
    property Size: Integer read FSize;

    { Read-only information about a glyph for given character.
      @nil if given glyph not loaded (because was not requested at constructor,
      or because it doesn't exist in the font). }
    function Glyph(const C: TUnicodeChar): TGlyph;
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

    { Height of a row of text in this font.
      This may be calculated as simply @code(TextHeight('Wy')) for most
      normal fonts. }
    function RowHeight: Integer;

    { Height (above the baseline) of a row of text in this font.
      Similar to TextHeightBase and TextHeight,
      note that RowHeightBase is generally smaller than RowHeight,
      because RowHeightBase doesn't care how low the letter may go below
      the baseline. }
    function RowHeightBase: Integer;

    { How low the text may go below the baseline. }
    function Descend: Integer;
  end;

implementation

uses SysUtils, {$ifdef HAS_FREE_TYPE} CastleFreeType, CastleFtFont, {$endif}
  CastleLog, CastleUtils, CastleURIUtils;

{ TTextureFontData.TGlyphDictionary ------------------------------------------ }

constructor TTextureFontData.TGlyphDictionary.Create;
begin
  inherited;
  FOwnsGlyphs := true;
end;

destructor TTextureFontData.TGlyphDictionary.Destroy;
var
  I: Integer;
begin
  if OwnsGlyphs then
    for I := 0 to Count - 1 do
      Data[I].Free;
  Clear;
  inherited;
end;

{ TTextureFontData ----------------------------------------------------------------- }

{$ifdef HAS_FREE_TYPE}

constructor TTextureFontData.Create(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  ACharacters: TUnicodeCharList);
var
  FontId: Integer;

  function GetGlyphInfo(const C: TUnicodeChar): TGlyph;
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;
  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeToUTF8(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeToUTF8(C), Size);

    try
      if Bitmaps.Count = 0 then
      begin
        WritelnWarning('Font', Format('Font "%s" does not contain glyph for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit(nil);
      end;

      Bitmap := Bitmaps.Bitmaps[0];
      if Bitmaps.Count > 1 then
        WritelnWarning('Font', Format('Font "%s" contains a sequence of glyphs (more than a single glyph) for a single character "%s" (index %d)',
          [URL, C, Ord(C)]));
      if (Bitmap^.Width < 0) or (Bitmap^.Height < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Width or Height < 0 for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit(nil);
      end;

      Result := TGlyph.Create;
      Result.Width    := Bitmap^.Width;
      Result.Height   := Bitmap^.Height;
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
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeToUTF8(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeToUTF8(C), Size);
    try
      Bitmap := Bitmaps.Bitmaps[0];
      if (Bitmap^.Pitch < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Pitch < 0 for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit;
      end;
      if AntiAliased then
        DrawChar else
        DrawCharBW;
    finally FreeAndNil(Bitmaps) end;
  end;

const
  { Separate the glyphs for safety, to avoid pulling in colors
    from neighboring letters when drawing (floating point errors could in theory
    make small errors moving us outside of the desired pixel). }
  GlyphPadding = 2;

var
  FileName: string;
  GlyphInfo: TGlyph;
  GlyphsCount, ImageSize: Cardinal;
  MaxWidth, MaxHeight, ImageX, ImageY: Cardinal;
  C: TUnicodeChar;
  TemporaryCharacters: boolean;
begin
  inherited Create;
  FSize := ASize;
  FAntiAliased := AnAntiAliased;

  CastleFtFont.InitEngine;
  { By default TFontManager uses DefaultResolution that is OS-dependent
    and does not really have any good reasoninig?
    We set 0, letting FreeType library use good default,
    http://www.freetype.org/freetype2/docs/tutorial/step1.html ,
    and in effect Size is in nice pixels by default. }
  FontMgr.Resolution := 0;
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise Exception.CreateFmt('Cannot read font from URL "%s". Note that right now only local file URLs are supported', [URL]);
  FontId := FontMgr.RequestFont(FileName);

  TemporaryCharacters := ACharacters = nil;
  if TemporaryCharacters then
  begin
    ACharacters := TUnicodeCharList.Create;
    ACharacters.Add(SimpleAsciiCharacters);
  end;

  try
    FGlyphsExtra := TGlyphDictionary.Create;

    GlyphsCount := 0;
    MaxWidth    := 0;
    MaxHeight   := 0;
    for C in ACharacters do
    begin
      GlyphInfo := GetGlyphInfo(C);
      if C <= High(FGlyphsByte) then
        FGlyphsByte[C] := GlyphInfo else
        FGlyphsExtra.KeyData[C] := GlyphInfo;
      if GlyphInfo <> nil then
      begin
        Inc(GlyphsCount);
        MaxVar(MaxWidth , GlyphInfo.Width);
        MaxVar(MaxHeight, GlyphInfo.Height);
      end;
    end;

    if GlyphsCount <> 0 then
    begin
      MaxWidth += GlyphPadding;
      MaxHeight += GlyphPadding;

      ImageSize := 8;
      while (ImageSize div MaxHeight) * (ImageSize div MaxWidth) < GlyphsCount do
        ImageSize *= 2;

      WritelnLog('Font', 'Creating image %dx%d to store glyphs of font "%s" (%d glyphs, max glyph size (including %d pixel padding) is %dx%d)',
        [ImageSize, ImageSize, URL, GlyphsCount, GlyphPadding, MaxWidth, MaxHeight]);

      FImage := TGrayscaleImage.Create(ImageSize, ImageSize);
      Image.Clear(0);
      Image.TreatAsAlpha := true;
      Image.URL := URL;

      ImageX := 0;
      ImageY := 0;
      for C in ACharacters do
      begin
        GlyphInfo := Glyph(C);
        if GlyphInfo <> nil then
        begin
          GlyphInfo.ImageX := ImageX;
          GlyphInfo.ImageY := ImageY;

          GetGlyphData(C, ImageX, ImageY);

          ImageX += MaxWidth;
          if ImageX + MaxWidth >= ImageSize then
          begin
            ImageX := 0;
            ImageY += MaxHeight;
          end;
        end;
      end;

      // Debug: SaveImage(Image, '/tmp/a.png');
    end;
  finally
    if TemporaryCharacters then
      FreeAndNil(ACharacters);
  end;
end;

{$endif}

constructor TTextureFontData.CreateFromData(const AGlyphs: TGlyphDictionary;
  const AImage: TGrayscaleImage;
  const ASize: Integer; const AnAntiAliased: boolean);
var
  I: Integer;
  C: TUnicodeChar;
begin
  inherited Create;
  FSize := ASize;
  FAntiAliased := AnAntiAliased;

  { split AGlyphs into FGlyphsByte and FGlyphsExtra }
  FGlyphsExtra := TGlyphDictionary.Create;
  for I := 0 to AGlyphs.Count - 1 do
  begin
    C := AGlyphs.Keys[I];
    if C <= High(FGlyphsByte) then
      FGlyphsByte[C] := AGlyphs.Data[I] else
      FGlyphsExtra.KeyData[C] := AGlyphs.Data[I];
  end;
  AGlyphs.OwnsGlyphs := false;
  AGlyphs.Free; // we own AGlyphs, for now we just free them

  FImage := AImage;
end;

destructor TTextureFontData.Destroy;
var
  C: Byte;
begin
  FreeAndNil(FGlyphsExtra);
  for C in Byte do
    FreeAndNil(FGlyphsByte[C]);
  FreeAndNil(FImage);
  inherited;
end;

function TTextureFontData.Glyph(const C: TUnicodeChar): TGlyph;
var
  I: Integer;
begin
  if C <= High(FGlyphsByte) then
    Result := FGlyphsByte[C] else
  begin
    I := FGlyphsExtra.IndexOf(C);
    if I <> -1 then
      Result := FGlyphsExtra.Data[I] else
      Result := nil;
  end;
end;

function TTextureFontData.LoadedGlyphs: TUnicodeCharList;
var
  I: Integer;
  C: TUnicodeChar;
begin
  Result := TUnicodeCharList.Create;
  for C := 0 to High(FGlyphsByte) do
    if FGlyphsByte[C] <> nil then
      Result.Add(C);
  for I := 0 to FGlyphsExtra.Count - 1 do
    Result.Add(FGlyphsExtra.Keys[I]);
end;

function TTextureFontData.TextWidth(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
      Result += G.AdvanceX;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

function TTextureFontData.TextHeight(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  MinY, MaxY, YOrigin: Integer;
  G: TTextureFontData.TGlyph;
begin
  MinY := 0;
  MaxY := 0;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
    begin
      YOrigin := G.Y;
      MinVar(MinY, -YOrigin);
      MaxVar(MaxY, G.Height - YOrigin);
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
  Result := MaxY - MinY;
end;

function TTextureFontData.TextMove(const S: string): TVector2Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := ZeroVector2Integer;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
    begin
      Result[0] += G.AdvanceX;
      Result[1] += G.AdvanceY;
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

function TTextureFontData.TextHeightBase(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  { This is just like TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
      MaxVar(Result, G.Height - G.Y);

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

procedure TTextureFontData.Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
begin
  ARowHeight := TextHeight('Wy');
  ARowHeightBase := TextHeightBase('W');
  ADescend := TextHeight('y') - TextHeight('a');
end;

function TTextureFontData.RowHeight: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FRowHeight;
end;

function TTextureFontData.RowHeightBase: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FRowHeightBase;
end;

function TTextureFontData.Descend: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FDescend;
end;

end.
