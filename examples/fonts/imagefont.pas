{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Image-based font initialized from a FreeType font file, like ttf (TImageFont). }
unit ImageFont;

interface

uses CastleStringUtils, CastleImages;

type
  { Image-based font initialized from a FreeType font file, like ttf. }
  TImageFont = class
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
        { Position of the glyph on the image in TImageFont.Image. }
        ImageX, ImageY: Cardinal;
      end;
  private
    FAntiAliased: boolean;
    FSize: Integer;
    { Non-nil only for filled glyphs. }
    FGlyphs: array [char] of TGlyph;
    FImage: TGrayscaleImage;
  public
    constructor Create(const URL: string;
      const AnAntiAliased: boolean; const ASize: Integer;
      const ACharacters: TSetOfChars = SimpleAsciiCharacters);
    destructor Destroy; override;

    property AntiAliased: boolean read FAntiAliased;
    property Size: Integer read FSize;

    { Read-only information about a glyph for given character.
      @nil if given glyph not loaded (because was not requested at constructor,
      or because it doesn't exist in the font). }
    function Glyph(const C: char): TGlyph;
    property Image: TGrayscaleImage read FImage;
  end;

implementation

uses SysUtils, FreeType, FtFont, CastleLog,
  CastleUtils, CastleURIUtils, CastleWarnings;

{ TImageFont ----------------------------------------------------------------- }

constructor TImageFont.Create(const URL: string;
  const AnAntiAliased: boolean; const ASize: Integer;
  const ACharacters: TSetOfChars = SimpleAsciiCharacters);
var
  FontId: Integer;

  function GetGlyphInfo(const C: char): TGlyph;
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;
  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, C, Size) else
      Bitmaps := FontMgr.GetString(FontId, C, Size);

    try
      if Bitmaps.Count = 0 then
      begin
        OnWarning(wtMajor, 'Font', Format('Font "%s" does not contain glyph for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit(nil);
      end;

      Bitmap := Bitmaps.Bitmaps[0];
      if Bitmaps.Count > 1 then
        OnWarning(wtMajor, 'Font', Format('Font "%s" contains a sequence of glyphs (more than a single glyph) for a single character "%s" (index %d)',
          [URL, C, Ord(C)]));
      if (Bitmap^.Width < 0) or (Bitmap^.Height < 0) then
      begin
        OnWarning(wtMajor, 'Font', Format('Font "%s" contains a glyphs with Width or Height < 0 for character "%s" (index %d)',
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
  procedure GetGlyphData(const C: char; const ImageX, ImageY: Cardinal);
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
      Bitmaps := FontMgr.GetStringGray(FontId, C, Size) else
      Bitmaps := FontMgr.GetString(FontId, C, Size);
    try
      Bitmap := Bitmaps.Bitmaps[0];
      if (Bitmap^.Pitch < 0) then
      begin
        OnWarning(wtMajor, 'Font', Format('Font "%s" contains a glyphs with Pitch < 0 for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit;
      end;
      if AntiAliased then
        DrawChar else
        DrawCharBW;
    finally FreeAndNil(Bitmaps) end;
  end;

var
  FileName: string;
  C: char;
  GlyphInfo: TGlyph;
  GlyphsCount, ImageSize: Cardinal;
  MaxWidth, MaxHeight, ImageX, ImageY: Cardinal;
begin
  inherited Create;
  FAntiAliased := AnAntiAliased;
  FSize := ASize;

  FtFont.InitEngine;
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

  GlyphsCount := 0;
  MaxWidth    := 0;
  MaxHeight   := 0;
  for C in ACharacters do
  begin
    GlyphInfo := GetGlyphInfo(C);
    FGlyphs[C] := GlyphInfo;
    if GlyphInfo <> nil then
    begin
      Inc(GlyphsCount);
      MaxTo1st(MaxWidth , GlyphInfo.Width);
      MaxTo1st(MaxHeight, GlyphInfo.Height);
    end;
  end;

  if GlyphsCount <> 0 then
  begin
    { Increase the glyph by 1 pixel for safety, to avoid pulling in colors
      from neighboring letters when drawing (floating point errors could in theory
      make small errors moving us outside of the desired pixel). }
    Inc(MaxWidth);
    Inc(MaxHeight);

    ImageSize := 8;
    while (ImageSize div MaxHeight) * (ImageSize div MaxWidth) < GlyphsCount do
      ImageSize *= 2;

    WritelnLog('Font', 'Creating image %dx%d to store glyphs of font "%s" (%d glyphs, max glyph size (with 1 pixel margin) %dx%d)',
      [ImageSize, ImageSize, URL, GlyphsCount, MaxWidth, MaxHeight]);

    FImage := TGrayscaleImage.Create(ImageSize, ImageSize);
    Image.Clear(0);
    Image.TreatAsAlpha := true;

    ImageX := 0;
    ImageY := 0;
    for C in ACharacters do
      if FGlyphs[C] <> nil then
      begin
        FGlyphs[C].ImageX := ImageX;
        FGlyphs[C].ImageY := ImageY;

        GetGlyphData(C, ImageX, ImageY);

        ImageX += MaxWidth;
        if ImageX + MaxWidth >= ImageSize then
        begin
          ImageX := 0;
          ImageY += MaxHeight;
        end;
      end;

    // Debug: SaveImage(Image, '/tmp/a.png');
  end;
end;

destructor TImageFont.Destroy;
var
  C: char;
begin
  for C in char do
    FreeAndNil(FGlyphs[C]);
  FreeAndNil(FImage);
  inherited;
end;

function TImageFont.Glyph(const C: char): TGlyph;
begin
  Result := FGlyphs[C];
end;

end.
