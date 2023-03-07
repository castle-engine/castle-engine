{%MainUnit castlefonts.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

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
    "bitmap font" or (less common name, but more valid) "raster font".

    By default this class makes some assumptions about how the font image looks like:
    the characters are placed on the image in their Unicode order,
    starting from space in the upper-left corner.
    TODO: Use OnGlyph event to customize it.

    Examples of such fonts:

    @unorderedList(
      @item(See examples/fonts/data/sonic_asalga_0.png in CGE examples.)
      @item(From OpenGameArt: http://opengameart.org/content/sonic-font,
        http://opengameart.org/content/null-terminator .)
      @item(From itch.io: https://itch.io/game-assets/tag-bitmap-font .)
    ) }
  TCastleBitmapFont = class(TCastleAbstractFont)
  strict private
    FDrawableImage: TDrawableImage;
    FImageUrl: String;
    FImage: TCastleImage;
    FImageColumns, FImageRows,FImageMargin: Cardinal;
    FDisplayMargin: Single;
    FGlyphsScreenRects, FGlyphsImageRects: TFloatRectangleList;
    function ScaledCharWidth: Single;
    function ScaledCharHeight: Single;
    function ScaledDisplayMargin: Single;
    function GetSmoothScaling: boolean;
    { Scale applied to the rendered DrawableImage to honor changing the Size property. }
    function Scale: Single;
    { Free stuff loaded by @link(Load). }
    procedure Unload;
    procedure SetImageUrl(const Value: String);
    procedure SetImageColumns(const Value: Cardinal);
    procedure SetImageRows(const Value: Cardinal);
    procedure SetImageMargin(const Value: Cardinal);
    procedure SetDisplayMargin(const Value: Single);
    function CharWidth: Cardinal;
    function CharHeight: Cardinal;
  strict protected
    procedure SetSize(const Value: Single); override;
    procedure GLContextClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    { Load font from given image.
      @param AImage Image data, becomes owned by this class.
      @param AImageMargin There is a margin in the image between rows and cols.
      @param(ADisplayMargin We can display some spacing between characters.
        This is independent from ImageMargin and image contents.) }
    procedure Load(const AImage: TCastleImage;
      const AImageColumns, AImageRows, AImageMargin: Cardinal; const ADisplayMargin: Single);
      deprecated 'assign properties to load font: ImageUrl, ImageColumns, ImageRows, ImageMargin and (only for display) DisplayMargin';
    procedure PrepareResources; override;
    procedure Print(const X, Y: Single; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Single; override;
    function TextHeight(const S: string): Single; override;
    function TextHeightBase(const S: string): Single; override;
    function TextMove(const S: string): TVector2; override;
    function FontLoaded: Boolean; override;
  published
    { Image that contains the characters.
      The characters are placed on the image in their Unicode order,
      starting from space in the upper-left corner.

      The characters are organized in ImageColumns columns and ImageRows rows,
      and have ImageMargin space between them. }
    property ImageUrl: String read FImageUrl write SetImageUrl;
    { Columns of the characters in the image @link(ImageUrl). }
    property ImageColumns: Cardinal read FImageColumns write SetImageColumns default 8;
    { Rows of the characters in the image @link(ImageUrl). }
    property ImageRows: Cardinal read FImageRows write SetImageRows default 8;
    { Margin between characters in the image @link(ImageUrl). }
    property ImageMargin: Cardinal read FImageMargin write SetImageMargin default 0;
    { Spacing between characters when rendering the font.
      This is independent from @link(ImageMargin). }
    property DisplayMargin: Single read FDisplayMargin write SetDisplayMargin {$ifdef FPC}default 2{$endif};
  end;

  TSimpleTextureFont = TCastleBitmapFont deprecated 'use TCastleBitmapFont';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleBitmapFont --------------------------------------------------------- }

constructor TCastleBitmapFont.Create(AOwner: TComponent);
begin
  inherited;
  FGlyphsScreenRects := TFloatRectangleList.Create;
  FGlyphsImageRects  := TFloatRectangleList.Create;
  FImageColumns := 8;
  FImageRows := 8;
  FImageMargin := 0;
  FDisplayMargin := 2;
end;

destructor TCastleBitmapFont.Destroy;
begin
  FreeAndNil(FGlyphsScreenRects);
  FreeAndNil(FGlyphsImageRects);
  Unload;
  inherited;
end;

procedure TCastleBitmapFont.Unload;
begin
  GLContextClose;
  FreeAndNil(FImage);
end;

procedure TCastleBitmapFont.SetImageUrl(const Value: String);
begin
  if FImageUrl <> Value then
  begin
    FImageUrl := Value;
    Unload;
    if Value <> '' then
    try
      FImage := LoadImage(Value);
      { CharHeight changed, reset Size -- this makes it natural to work,
        as in usual case, when working with TCastleLabel, you don't care about
        TCastleBitmapFont.Size, you only set final size on TCastleLabel. }
      Size := CharHeight;
    except
      on E: Exception do
      begin
        if CastleDesignMode then
        begin
          WritelnWarning('TCastleBitmapFont', 'Failed to load image "%s": %s',
            [URIDisplay(FImageUrl), ExceptMessage(E)]);
          Exit;
        end else
          raise;
      end;
    end;
  end;
end;

procedure TCastleBitmapFont.SetImageColumns(const Value: Cardinal);
begin
  if FImageColumns <> Value then
  begin
    FImageColumns := Value;
    FontSizesChanged; // cause repaint, glyphs changed
  end;
end;

procedure TCastleBitmapFont.SetImageRows(const Value: Cardinal);
begin
  if FImageRows <> Value then
  begin
    FImageRows := Value;
    { CharHeight changed, reset Size -- this makes it natural to work,
      as in usual case, when working with TCastleLabel, you don't care about
      TCastleBitmapFont.Size, you only set final size on TCastleLabel. }
    Size := CharHeight;
    FontSizesChanged; // cause repaint, glyphs changed
  end;
end;

procedure TCastleBitmapFont.SetImageMargin(const Value: Cardinal);
begin
  if FImageMargin <> Value then
  begin
    FImageMargin := Value;
    FontSizesChanged; // cause repaint, glyphs changed
  end;
end;

procedure TCastleBitmapFont.SetDisplayMargin(const Value: Single);
begin
  if FDisplayMargin <> Value then
  begin
    FDisplayMargin := Value;
    FontSizesChanged; // cause repaint
  end;
end;

function TCastleBitmapFont.CharWidth: Cardinal;
begin
  if FImage <> nil then
    Result := Max(0, Integer(FImage.Width div ImageColumns) - ImageMargin)
  else
    Result := 0;
end;

function TCastleBitmapFont.CharHeight: Cardinal;
begin
  if FImage <> nil then
    Result := Max(0, Integer(FImage.Height div ImageRows) - ImageMargin)
  else
    Result := 0;
end;

procedure TCastleBitmapFont.Load(const AImage: TCastleImage;
  const AImageColumns, AImageRows, AImageMargin: Cardinal; const ADisplayMargin: Single);
begin
  Unload;

  FImage := AImage;

  FImageColumns := AImageColumns;
  FImageRows := AImageRows;
  FImageMargin := AImageMargin;
  FDisplayMargin := ADisplayMargin;
  Size := CharHeight;

  FontSizesChanged;
end;

function TCastleBitmapFont.ScaledCharWidth: Single;
begin
  Result := CharWidth * Scale + Outline * 2;
end;

function TCastleBitmapFont.ScaledCharHeight: Single;
begin
  Result := CharHeight * Scale + Outline * 2;
end;

function TCastleBitmapFont.ScaledDisplayMargin: Single;
begin
  Result := FDisplayMargin * Scale;
end;

function TCastleBitmapFont.Scale: Single;
begin
  Result := Size / CharHeight;
end;

procedure TCastleBitmapFont.SetSize(const Value: Single);
begin
  inherited SetSize(Value);
  if FDrawableImage <> nil then
    FDrawableImage.SmoothScaling := GetSmoothScaling;
end;

function TCastleBitmapFont.GetSmoothScaling: boolean;
begin
  Result := Size <> CharHeight;
end;

procedure TCastleBitmapFont.PrepareResources;
begin
  inherited;
  if FDrawableImage = nil then
    FDrawableImage := TDrawableImage.Create(FImage, GetSmoothScaling, false);
end;

procedure TCastleBitmapFont.GLContextClose;
begin
  FreeAndNil(FDrawableImage);
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
      Assert(GlyphsToRender < FGlyphsScreenRects.Count);
      FGlyphsScreenRects.List^[GlyphsToRender] := ScreenRect;
      FGlyphsImageRects .List^[GlyphsToRender] := ImageRect;
      Inc(GlyphsToRender);
    end else
    begin
      TargetImage.DrawFrom(FImage,
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
  I: Integer;
  {$ifdef FPC}
  TextPtr: PChar;
  CharLen: Integer;
  {$else}
  TextIndex: Integer;
  NextTextIndex: Integer;
  TextLength: Integer;
  {$endif}
begin
  if TargetImage = nil then
  begin
    PrepareResources;
    FGlyphsScreenRects.Count := Max(MinimumGlyphsAllocated, Length(S));
    FGlyphsImageRects .Count := Max(MinimumGlyphsAllocated, Length(S));
    FDrawableImage.Color := Color;
  end;

  GlyphsToRender := 0;

  { Using Floor(ScaledDisplayMargin / 2)
    instead of just "ScaledDisplayMargin / 2",
    because it looks better in case of rendering the unscaled font
    (Scale = 1), at integer coordinates (e.g. X=10.0, Y=10.0),
    and ScaledDisplayMargin is odd ineger (e.g. 1.0).

    You expect then the font to hit pixels exactly,
    while "ScaledDisplayMargin / 2" would mean that everything is shifted
    by 0.5 pixel.

    Testcase: font_from_texture.lpr using null_terminator_0.png font. }
  InitialMargin := Floor(ScaledDisplayMargin / 2);

  I := 0;
  {$ifdef FPC}
  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  {$else}
  TextIndex := 1;
  TextLength := Length(S);
  while (TextIndex <= TextLength) do
  {$endif}
  begin
    {$ifdef FPC}
    Inc(TextPtr, CharLen);
    {$else}
    C := UnicodeStringNextChar(S, TextIndex, NextTextIndex);
    TextIndex := NextTextIndex;
    {$endif}
    CharIndex := C - Ord(' ');  // TODO: check that
    ImageX := CharIndex mod FImageColumns;
    ImageY := CharIndex div FImageColumns;
    if ImageY < ImageRows then
    begin
      ImageX := ImageX * (CharWidth + FImageMargin);
      ImageY := FImage.Height - (ImageY + 1) * (CharHeight + FImageMargin);
      ScreenX := InitialMargin + X + I * (ScaledCharWidth + ScaledDisplayMargin);
      ScreenY := InitialMargin + Y;
      Inc(I);

      { TODO: this ignores Outline and related properties now, always renders like Outline = 0. }

      GlyphDraw(
        FloatRectangle(ScreenX, ScreenY, ScaledCharWidth, ScaledCharHeight),
        FloatRectangle(ImageX, ImageY, CharWidth, CharHeight));
    end;

    {$ifdef FPC}
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    {$endif}
  end;

  if TargetImage = nil then
  begin
    FDrawableImage.Color := Color;
    FDrawableImage.Draw(
      PFloatRectangleArray(FGlyphsScreenRects.List),
      PFloatRectangleArray(FGlyphsImageRects.List), GlyphsToRender);
  end;
end;

function TCastleBitmapFont.TextWidth(const S: string): Single;
begin
  Result := Length(S) * (ScaledCharWidth + ScaledDisplayMargin);
end;

function TCastleBitmapFont.TextHeight(const S: string): Single;
begin
  Result := ScaledCharHeight + ScaledDisplayMargin;
end;

function TCastleBitmapFont.TextHeightBase(const S: string): Single;
begin
  Result := ScaledCharHeight + ScaledDisplayMargin;
end;

function TCastleBitmapFont.TextMove(const S: string): TVector2;
begin
  Result := Vector2(TextWidth(S), TextHeight(S));
end;

function TCastleBitmapFont.FontLoaded: Boolean;
begin
  Result := FImage <> nil;
end;

function TCastleBitmapFont.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ImageUrl') or
     (PropertyName = 'ImageColumns') or
     (PropertyName = 'ImageRows') or
     (PropertyName = 'ImageMargin') or
     (PropertyName = 'DisplayMargin') then
      Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
