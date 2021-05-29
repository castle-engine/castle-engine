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
  { Bitmap font, where each character is just drawn (and may be multi-color)
    on a raster image. See https://en.wikipedia.org/wiki/Computer_font about
    "bitmap font" or (less common name) "raster font".

    By default this class makes some assumptions about how the font image looks like:
    the characters are drawn in Unicode order, starting from space, on an image.
    Use @link(OnGlyph) event to customize it.

    Examples of such fonts:

    @unorderedList(
      @item(See examples/fonts/data/sonic_asalga_0.png in CGE examples.)
      @item(From OpenGameArt: http://opengameart.org/content/sonic-font,
        http://opengameart.org/content/null-terminator .)
      @item(From itch.io: https://itch.io/game-assets/tag-bitmap-font .)
    ) }
  TCastleBitmapFont = class(TCastleAbstractFont)
  strict private
    DrawableImage: TDrawableImage;
    Image: TCastleImage;
    ImageCols, ImageRows,
      CharMargin, CharDisplayMargin, CharWidth, CharHeight: Integer;
    GlyphsScreenRects, GlyphsImageRects: TFloatRectangleList;
    function ScaledCharWidth: Single;
    function ScaledCharHeight: Single;
    function ScaledCharDisplayMargin: Single;
    function GetSmoothScaling: boolean;
    { Scale applied to the rendered DrawableImage to honor changing the Size property. }
    function Scale: Single;
  strict protected
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Load font from given image.
      @param AImage Image data, becomes owned by this class.
      @param ACharMargin There is a margin in the image between rows and cols.
      @param(ACharDisplayMargin We can display some spacing between characters.
        This is independent from CharMargin and image contents.) }
    procedure Load(AImage: TCastleImage;
      const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
    procedure PrepareResources; override;
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Single; override;
    function TextHeight(const S: string): Single; override;
    function TextHeightBase(const S: string): Single; override;
    function TextMove(const S: string): TVector2; override;
  end;

  TSimpleTextureFont = TCastleBitmapFont deprecated 'use TCastleBitmapFont';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleBitmapFont --------------------------------------------------------- }

constructor TCastleBitmapFont.Create(AOwner: TComponent);
begin
  inherited;
  GlyphsScreenRects := TFloatRectangleList.Create;
  GlyphsImageRects  := TFloatRectangleList.Create;
end;

destructor TCastleBitmapFont.Destroy;
begin
  FreeAndNil(GlyphsScreenRects);
  FreeAndNil(GlyphsImageRects);
  FreeAndNil(Image);
  inherited;
end;

procedure TCastleBitmapFont.Load(AImage: TCastleImage;
  const AImageCols, AImageRows, ACharMargin, ACharDisplayMargin: Integer);
begin
  GLContextClose;
  FreeAndNil(Image);

  Image := AImage;

  ImageCols := AImageCols;
  ImageRows := AImageRows;
  CharMargin := ACharMargin;
  CharWidth := Image.Width div ImageCols - CharMargin;
  CharHeight := Image.Height div ImageRows - CharMargin;
  CharDisplayMargin := ACharDisplayMargin;

  Size := CharHeight;
end;

function TCastleBitmapFont.ScaledCharWidth: Single;
begin
  Result := CharWidth * Scale + Outline * 2;
end;

function TCastleBitmapFont.ScaledCharHeight: Single;
begin
  Result := CharHeight * Scale + Outline * 2;
end;

function TCastleBitmapFont.ScaledCharDisplayMargin: Single;
begin
  Result := CharDisplayMargin * Scale;
end;

function TCastleBitmapFont.Scale: Single;
begin
  Result := Size / CharHeight;
end;

procedure TCastleBitmapFont.SetSize(const Value: Single);
begin
  inherited SetSize(Value);
  if DrawableImage <> nil then
    DrawableImage.SmoothScaling := GetSmoothScaling;
end;

function TCastleBitmapFont.GetSmoothScaling: boolean;
begin
  Result := Size <> CharHeight;
end;

procedure TCastleBitmapFont.PrepareResources;
begin
  inherited;
  if DrawableImage = nil then
    DrawableImage := TDrawableImage.Create(Image, GetSmoothScaling, false);
end;

procedure TCastleBitmapFont.GLContextClose;
begin
  FreeAndNil(DrawableImage);
  inherited;
end;

procedure TCastleBitmapFont.Print(const X, Y: Single; const Color: TCastleColor;
  const S: string);
var
  GlyphsToRender: Integer;

  procedure GlyphDraw(const ScreenRect, ImageRect: TFloatRectangle);
  begin
    if TargetImage = nil then
    begin
      Assert(GlyphsToRender < GlyphsScreenRects.Count);
      GlyphsScreenRects.List^[GlyphsToRender] := ScreenRect;
      GlyphsImageRects .List^[GlyphsToRender] := ImageRect;
      Inc(GlyphsToRender);
    end else
    begin
      TargetImage.DrawFrom(Image,
        Round(ScreenRect.Left),
        Round(ScreenRect.Bottom),
        Round(ImageRect.Left),
        Round(ImageRect.Bottom),
        Round(ImageRect.Width),
        Round(ImageRect.Height));
    end;
  end;

var
  ImageX, ImageY, ScreenX, ScreenY, InitialMargin: Single;
  CharIndex: Integer;
  C: TUnicodeChar;
  TextPtr: PChar;
  I, CharLen: Integer;
begin
  if TargetImage = nil then
  begin
    PrepareResources;
    GlyphsScreenRects.Count := Max(MinimumGlyphsAllocated, Length(S));
    GlyphsImageRects .Count := Max(MinimumGlyphsAllocated, Length(S));
    DrawableImage.Color := Color;
  end;

  GlyphsToRender := 0;

  { Using Floor(ScaledCharDisplayMargin / 2)
    instead of just "ScaledCharDisplayMargin / 2",
    because it looks better in case of rendering the unscaled font
    (Scale = 1), at integer coordinates (e.g. X=10.0, Y=10.0),
    and ScaledCharDisplayMargin is odd ineger (e.g. 1.0).

    You expect then the font to hit pixels exactly,
    while "ScaledCharDisplayMargin / 2" would mean that everything is shifted
    by 0.5 pixel.

    Testcase: font_from_texture.lpr using null_terminator_0.png font. }
  InitialMargin := Floor(ScaledCharDisplayMargin / 2);

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  I := 0;
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    CharIndex := C - Ord(' ');
    ImageX := CharIndex mod ImageCols;
    ImageY := CharIndex div ImageCols;
    if ImageY < ImageRows then
    begin
      ImageX := ImageX * (CharWidth + CharMargin);
      ImageY := Image.Height - (ImageY + 1) * (CharHeight + CharMargin);
      ScreenX := InitialMargin + X + I * (ScaledCharWidth + ScaledCharDisplayMargin);
      ScreenY := InitialMargin + Y;
      Inc(I);

      { TODO: this ignores Outline and related properties now, always renders like Outline = 0. }

      GlyphDraw(
        FloatRectangle(ScreenX, ScreenY, ScaledCharWidth, ScaledCharHeight),
        FloatRectangle(ImageX, ImageY, CharWidth, CharHeight));
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;

  if TargetImage = nil then
  begin
    DrawableImage.Color := Color;
    DrawableImage.Draw(
      PFloatRectangleArray(GlyphsScreenRects.List),
      PFloatRectangleArray(GlyphsImageRects.List), GlyphsToRender);
  end;
end;

function TCastleBitmapFont.TextWidth(const S: string): Single;
begin
  Result := Length(S) * (ScaledCharWidth + ScaledCharDisplayMargin);
end;

function TCastleBitmapFont.TextHeight(const S: string): Single;
begin
  Result := ScaledCharHeight + ScaledCharDisplayMargin;
end;

function TCastleBitmapFont.TextHeightBase(const S: string): Single;
begin
  Result := ScaledCharHeight + ScaledCharDisplayMargin;
end;

function TCastleBitmapFont.TextMove(const S: string): TVector2;
begin
  Result := Vector2(TextWidth(S), TextHeight(S));
end;

{$endif read_implementation}
