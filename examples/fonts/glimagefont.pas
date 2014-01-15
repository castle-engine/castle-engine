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

{ Renderable font initialized from a FreeType font file (TGLImageFont). }
unit GLImageFont;

interface

uses CastleGLBitmapFonts, CastleGLImages, CastleStringUtils, CastleColors,
  CastleVectors, ImageFont;

type
  TGLImageFont = class(TGLBitmapFontAbstract)
  private
    FFont: TImageFont;
    Image: TGLImage;
  public
    constructor Create(const URL: string;
      const AnAntiAliased: boolean; const ASize: Integer;
      const ACharacters: TSetOfChars = SimpleAsciiCharacters);
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

constructor TGLImageFont.Create(const URL: string;
  const AnAntiAliased: boolean; const ASize: Integer;
  const ACharacters: TSetOfChars);
begin
  inherited Create;
  FFont := TImageFont.Create(URL, AnAntiAliased, ASize);
  Image := TGLImage.Create(FFont.Image, false);
end;

destructor TGLImageFont.Destroy;
begin
  FreeAndNil(Image);
  FreeAndNil(FFont);
  inherited;
end;

procedure TGLImageFont.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  C: char;
  ScreenX, ScreenY: Integer;
  G: TImageFont.TGlyph;
begin
  // TODO: use font image as alpha, honour color
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

function TGLImageFont.TextWidth(const S: string): Integer;
var
  C: char;
  G: TImageFont.TGlyph;
begin
  Result := 0;
  for C in S do
  begin
    G := FFont.Glyph(C);
    if G <> nil then
      Result += G.AdvanceX;
  end;
end;

function TGLImageFont.TextHeight(const S: string): Integer;
var
  C: char;
  MinY, MaxY, YOrigin: Integer;
  G: TImageFont.TGlyph;
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

function TGLImageFont.TextHeightBase(const S: string): Integer;
var
  C: char;
  G: TImageFont.TGlyph;
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

function TGLImageFont.TextMove(const S: string): TVector2Integer;
var
  C: char;
  G: TImageFont.TGlyph;
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

end.
