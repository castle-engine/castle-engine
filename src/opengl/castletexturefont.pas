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

{ 2D font using a texture initialized from a FreeType font file (TTextureFont). }
unit CastleTextureFont;

interface

uses CastleGLBitmapFonts, CastleGLImages, CastleStringUtils, CastleColors,
  CastleVectors, CastleTextureFontData, CastleImages;

type
  { 2D font using a texture initialized from a FreeType font file.

    This can load a font file, or it can use ready data in TTextureFontData.
    The latter allows to use this for fonts embedded in a Pascal source code,
    since our texturefont2pascal can convert a font ttf to a unit that defines
    ready TTextureFontData instance. }
  TTextureFont = class(TGLBitmapFontAbstract)
  private
    FFont: TTextureFontData;
    FOwnsFont: boolean;
    Image: TGLImage;
  public
    { Create by reading a FreeType font file, like ttf. }
    constructor Create(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      const ACharacters: TSetOfChars = SimpleAsciiCharacters);
    { Create from a ready TTextureFontData instance.
      Data instance becomes owned by this class if and only if OwnsData. }
    constructor Create(const Data: TTextureFontData; const OwnsData: boolean);
    destructor Destroy; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
    function TextHeightBase(const S: string): Integer; override;
  end;

  { @deprecated Deprecated name, use TTextureFont now. }
  TGLBitmapFont = TTextureFont deprecated;

  { 2D font using a texture to define character images
    with constant width and height.

    This class has some assumptions about how the font image looks like:
    the characters are drawn in ASCII order, starting from space, on an image.
    Derive your own descendants of TGLBitmapFontAbstract to have more flexibility,
    see the implementation of this class --- it is quite simple.
    Or use TTextureFont that can read data from a FreeType (like ttf) font file.

    See e.g. castle_game_engine/examples/fonts/data/sonic_asalga_0.png
    how to prepare an image for use with such font.
    You can find more such fonts on the Internet, see
    e.g. http://opengameart.org/content/sonic-font and
    http://opengameart.org/content/null-terminator. }
  TSimpleTextureFont = class(TGLBitmapFontAbstract)
  private
    Image: TGLImage;
    ImageCols, ImageRows,
      CharMargin, CharDisplayMargin, CharWidth, CharHeight: Integer;
  public
    { Load font from given image.
      @param AImage Image data, becomes owned by this class.
      @param ACharMargin There is a margin in the image between rows and cols.
      @param(ACharDisplayMargin We can display some spacing between characters.
        This is independent from CharMargin and image contents.) }
    constructor Create(AImage: TCastleImage;
      const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
    destructor Destroy; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
    function TextHeightBase(const S: string): Integer; override;
  end;

implementation

uses SysUtils, CastleUtils;

{ TTextureFont --------------------------------------------------------------- }

constructor TTextureFont.Create(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  const ACharacters: TSetOfChars);
begin
  Create(TTextureFontData.Create(URL, ASize, AnAntiAliased, ACharacters), true);
end;

constructor TTextureFont.Create(const Data: TTextureFontData; const OwnsData: boolean);
begin
  inherited Create;
  FOwnsFont := OwnsData;
  FFont := Data;
  Image := TGLImage.Create(FFont.Image, false);
end;

destructor TTextureFont.Destroy;
begin
  FreeAndNil(Image);
  if FOwnsFont then
    FreeAndNil(FFont) else
    FFont := nil;
  inherited;
end;

procedure TTextureFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  C: char;
  ScreenX, ScreenY: Integer;
  G: TTextureFontData.TGlyph;
begin
  Image.Color := Color;
  ScreenX := X;
  ScreenY := Y;
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
    begin
      if (G.Width <> 0) and (G.Height <> 0) then
        Image.Draw(ScreenX - G.X, ScreenY - G.Y, G.Width, G.Height,
          G.ImageX, G.ImageY, G.Width, G.Height);
      ScreenX += G.AdvanceX;
      ScreenY += G.AdvanceY;
    end;
  end;
end;

function TTextureFont.TextWidth(const S: string): Integer;
var
  C: char;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
      Result += G.AdvanceX;
  end;
end;

function TTextureFont.TextHeight(const S: string): Integer;
var
  C: char;
  MinY, MaxY, YOrigin: Integer;
  G: TTextureFontData.TGlyph;
begin
  MinY := 0;
  MaxY := 0;
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
    begin
      YOrigin := G.Y;
      MinTo1st(MinY, -YOrigin);
      MaxTo1st(MaxY, G.Height - YOrigin);
    end;
  end;
  Result := MaxY - MinY;
end;

function TTextureFont.TextHeightBase(const S: string): Integer;
var
  C: char;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  { This is just like TGLBitmapFont.TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
      MaxTo1st(Result, G.Height - G.Y);
  end;
end;

function TTextureFont.TextMove(const S: string): TVector2Integer;
var
  C: char;
  G: TTextureFontData.TGlyph;
begin
  Result := ZeroVector2Integer;
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
    begin
      Result[0] += G.AdvanceX;
      Result[1] += G.AdvanceY;
    end;
  end;
end;

{ TSimpleTextureFont --------------------------------------------------------- }

constructor TSimpleTextureFont.Create(AImage: TCastleImage;
  const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
begin
  inherited Create;
  Image := TGLImage.Create(AImage, false);
  FreeAndNil(AImage); // not needed anymore

  ImageCols := AImageCols;
  ImageRows := AImageRows;
  CharMargin := ACharMargin;
  CharWidth := Image.Width div ImageCols - CharMargin;
  CharHeight := Image.Height div ImageRows - CharMargin;
  CharDisplayMargin := ACharDisplayMargin;
end;

destructor TSimpleTextureFont.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TSimpleTextureFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  ImageX, ImageY: Single;
  I, CharIndex, ScreenX, ScreenY: Integer;
begin
  Image.Color := Color;
  for I := 1 to Length(S) do
  begin
    CharIndex := Ord(S[I]) - Ord(' ');
    ImageX := CharIndex mod ImageCols;
    ImageY := CharIndex div ImageCols;
    if ImageY < ImageRows then
    begin
      ImageX := ImageX * (CharWidth + CharMargin);
      ImageY := Image.Height - (ImageY + 1) * (CharHeight + CharMargin);
      ScreenX := CharDisplayMargin div 2 + X + (I - 1) * (CharWidth + CharDisplayMargin);
      ScreenY := CharDisplayMargin div 2 + Y;
      Image.Draw(ScreenX, ScreenY, CharWidth, CharHeight,
        ImageX, ImageY, CharWidth, CharHeight);
    end;
  end;
end;

function TSimpleTextureFont.TextWidth(const S: string): Integer;
begin
  Result := Length(S) * (CharWidth + CharDisplayMargin);
end;

function TSimpleTextureFont.TextHeight(const S: string): Integer;
begin
  Result := CharHeight + CharDisplayMargin;
end;

function TSimpleTextureFont.TextHeightBase(const S: string): Integer;
begin
  Result := CharHeight + CharDisplayMargin;
end;

function TSimpleTextureFont.TextMove(const S: string): TVector2Integer;
begin
  Result := Vector2Integer(TextWidth(S), TextHeight(S));
end;

end.
