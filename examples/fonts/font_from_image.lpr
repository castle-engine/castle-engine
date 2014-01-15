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

{ Font hand-crafted as the image (data/sonic_asalga_0.png,
  null_terminator_0.png).
  This example defines and uses a simple class to render such font.
  You could extend it for more advanced situations (here, we assume that
  each character has constant width and height).
}

{$apptype CONSOLE}

program font_from_image;

uses SysUtils, CastleWindow, CastleControls, CastleGLBitmapFonts, CastleGLImages,
  CastleColors, CastleVectors, CastleFilesUtils, CastleLog, GLImageFont;

{ TFontFromImage ------------------------------------------------------------- }

type
  TFontFromImage = class(TGLBitmapFontAbstract)
  private
    const
      ImageCols = 8;
      ImageRows = 12;
    var
    Image: TGLImage;
    CharMargin, CharDisplayMargin, CharWidth, CharHeight: Integer;
  public
    { Load font from given image.
      @param Url URL inside ApplicationData.
      @param ACharMargin There is a margin in the image between rows and cols.
      @param(ACharDisplayMargin We can display some spacing between characters.
        This is independent from CharMargin and image contents.) }
    constructor Create(const Url: string;
      const ACharMargin, ACharDisplayMargin: Integer);
    destructor Destroy; override;
    procedure Print(const X, Y: Integer; const Color: TCastleColor;
      const S: string); override;
    function TextWidth(const S: string): Integer; override;
    function TextHeight(const S: string): Integer; override;
    function TextMove(const S: string): TVector2Integer; override;
    function TextHeightBase(const S: string): Integer; override;
  end;

constructor TFontFromImage.Create(const Url: string;
  const ACharMargin, ACharDisplayMargin: Integer);
begin
  inherited Create;
  Image := TGLImage.Create(ApplicationData(Url), [], true);

  CharMargin := ACharMargin;
  CharWidth := Image.Width div ImageCols - CharMargin;
  CharHeight := Image.Height div ImageRows - CharMargin;
  CharDisplayMargin := ACharDisplayMargin;
end;

destructor TFontFromImage.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure TFontFromImage.Print(const X, Y: Integer; const Color: TCastleColor;
  const S: string);
var
  ImageX, ImageY: Single;
  I, CharIndex, ScreenX, ScreenY: Integer;
begin
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

function TFontFromImage.TextWidth(const S: string): Integer;
begin
  Result := Length(S) * (CharWidth + CharDisplayMargin);
end;

function TFontFromImage.TextHeight(const S: string): Integer;
begin
  Result := CharHeight + CharDisplayMargin;
end;

function TFontFromImage.TextHeightBase(const S: string): Integer;
begin
  Result := CharHeight + CharDisplayMargin;
end;

function TFontFromImage.TextMove(const S: string): TVector2Integer;
begin
  Result := Vector2Integer(TextWidth(S), TextHeight(S));
end;

{ main program --------------------------------------------------------------- }

var
  Window: TCastleWindow;
  Label1, Label2,
    LabelDeja, LabelDejaBW, LabelDejaLarge,
    LabelStylish, LabelStylishBW, LabelStylishLarge: TCastleLabel;

procedure Open(Window: TCastleWindowBase);
begin
  UIFont := TFontFromImage.Create('sonic_asalga_0.png', 2, 2);

  Label2.CustomFont := TFontFromImage.Create('null_terminator_0.png', 1, 1);
  Label2.OwnsCustomFont := true;

  LabelDeja.CustomFont := TGLImageFont.Create(ApplicationData('DejaVuSans.ttf'), true, 15);
  LabelDeja.OwnsCustomFont := true;
  LabelDejaBW.CustomFont := TGLImageFont.Create(ApplicationData('DejaVuSans.ttf'), false, 15);
  LabelDejaBW.OwnsCustomFont := true;
  LabelDejaLarge.CustomFont := TGLImageFont.Create(ApplicationData('DejaVuSans.ttf'), true, 30);
  LabelDejaLarge.OwnsCustomFont := true;

  LabelStylish.CustomFont := TGLImageFont.Create(ApplicationData('PARPG.ttf'), true, 15);
  LabelStylish.OwnsCustomFont := true;
  LabelStylishBW.CustomFont := TGLImageFont.Create(ApplicationData('PARPG.ttf'), false, 15);
  LabelStylishBW.OwnsCustomFont := true;
  LabelStylishLarge.CustomFont := TGLImageFont.Create(ApplicationData('PARPG.ttf'), true, 30);
  LabelStylishLarge.OwnsCustomFont := true;
end;

procedure Resize(Window: TCastleWindowBase);
const
  Margin = 10;
var
  Y: Integer;

  procedure PositionLabel(L: TCastleLabel);
  begin
    Y -= Integer(L.Rect.Height) + Margin;
    L.CenterHorizontal;
    L.Bottom := Y;
  end;

begin
  { each time Window size changes, reposition labels }
  Y := Window.Height;
  PositionLabel(Label1);
  PositionLabel(Label2);
  PositionLabel(LabelDeja);
  PositionLabel(LabelDejaBw);
  PositionLabel(LabelDejaLarge);
  PositionLabel(LabelStylish);
  PositionLabel(LabelStylishBw);
  PositionLabel(LabelStylishLarge);
end;

var
  Background: TCastleSimpleBackground;
begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertFront(Background);

  Label1 := TCastleLabel.Create(Window);
  Label1.Text.Append('A simple test of a font from an image.');
  Label1.Text.Append('Do cats eat bats?');
  Label1.Text.Append('and sometimes, Do bats eat cats?');
  Label1.Text.Append('1 + 2 + 3 = 6');
  Label1.Padding := 5;
  Window.Controls.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Window);
  Label2.Text.Append('Yet another label.');
  Label2.Text.Append('With different font.');
  Label2.Text.Append('Just because we can :)');
  Label2.Padding := 5;
  Window.Controls.InsertFront(Label2);

  LabelDeja := TCastleLabel.Create(Window);
  LabelDeja.Text.Append('DejaVuSans font');
  LabelDeja.Text.Append('with anti-aliasing.');
  LabelDeja.Padding := 5;
  Window.Controls.InsertFront(LabelDeja);

  LabelDejaBW := TCastleLabel.Create(Window);
  LabelDejaBW.Text.Append('DejaVuSans font');
  LabelDejaBW.Text.Append('without anti-aliasing.');
  LabelDejaBW.Padding := 5;
  Window.Controls.InsertFront(LabelDejaBW);

  LabelDejaLarge := TCastleLabel.Create(Window);
  LabelDejaLarge.Text.Append('DejaVuSans font');
  LabelDejaLarge.Text.Append('with anti-aliasing');
  LabelDejaLarge.Text.Append('and larger size.');
  LabelDejaLarge.Padding := 5;
  Window.Controls.InsertFront(LabelDejaLarge);

  LabelStylish := TCastleLabel.Create(Window);
  LabelStylish.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing.';
  LabelStylish.Padding := 5;
  Window.Controls.InsertFront(LabelStylish);

  LabelStylishBW := TCastleLabel.Create(Window);
  LabelStylishBW.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'without anti-aliasing.';
  LabelStylishBW.Padding := 5;
  Window.Controls.InsertFront(LabelStylishBW);

  LabelStylishLarge := TCastleLabel.Create(Window);
  LabelStylishLarge.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing' +LineEnding+
    'and larger size.';
  LabelStylishLarge.Padding := 5;
  Window.Controls.InsertFront(LabelStylishLarge);

  Window.OnOpen := @Open;
  Window.OnResize := @Resize;
  Window.OpenAndRun;
end.