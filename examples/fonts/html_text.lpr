{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleLabel, with long wrapped text with HTML inside. }

uses SysUtils, Classes,
  CastleTextureFont_DjvSans_20, CastleTextureFont_DjvSansB_20,
  CastleTextureFont_DjvSansO_20, CastleTextureFont_DjvSansBO_20,
  CastleFonts, CastleFontFamily, CastleControls, CastleUIControls, CastleWindow,
  CastleVectors, CastleColors;

const
  Margin = 4;
var
  Window: TCastleWindowBase;
  Label1: TCastleLabel;
  ButtonHtml, ButtonWrap, ButtonAlignLeft, ButtonAlignMiddle, ButtonAlignRight: TCastleButton;
  ScrollView: TCastleScrollView;

procedure UpdateSizes;
var
  WidthInsideScrollArea: Single;
begin
  WidthInsideScrollArea := ScrollView.EffectiveWidthForChildren
    - ScrollView.EffectiveScrollBarWidth;

  // do this first, as it updates Label1.EffectiveHeight
  if ButtonWrap.Pressed then
    Label1.MaxWidth := WidthInsideScrollArea
  else
    Label1.MaxWidth := 0;

  ScrollView.ScrollArea.Width := WidthInsideScrollArea;
  ScrollView.ScrollArea.Height := Label1.EffectiveHeight;
end;

type
  TButtonHandler = class
    class procedure ClickHtml(Sender: TObject);
    class procedure ClickWrap(Sender: TObject);
    class procedure ClickAlignLeft(Sender: TObject);
    class procedure ClickAlignMiddle(Sender: TObject);
    class procedure ClickAlignRight(Sender: TObject);
  end;

class procedure TButtonHandler.ClickHtml(Sender: TObject);
begin
  ButtonHtml.Pressed := not ButtonHtml.Pressed;
  Label1.Html := ButtonHtml.Pressed;
  UpdateSizes;
end;

class procedure TButtonHandler.ClickWrap(Sender: TObject);
begin
  ButtonWrap.Pressed := not ButtonWrap.Pressed;
  UpdateSizes;
end;

class procedure TButtonHandler.ClickAlignLeft(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := true;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := false;
  Label1.Alignment := hpLeft;
  Label1.Anchor(hpLeft);
end;

class procedure TButtonHandler.ClickAlignMiddle(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := true;
  ButtonAlignRight.Pressed := false;
  Label1.Alignment := hpMiddle;
  Label1.Anchor(hpMiddle);
end;

class procedure TButtonHandler.ClickAlignRight(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := true;
  Label1.Alignment := hpRight;
  Label1.Anchor(hpRight);
end;

procedure WindowResize(Container: TUIContainer);
begin
  UpdateSizes;
end;

var
  Background: TCastleRectangleControl;
  Font: TFontFamily;
begin
  Window := TCastleWindowBase.Create(Application);

  { prepare TFontFamily with font varians for bold, italic }
  Font := TFontFamily.Create(Window);
  Font.RegularFont := TTextureFont.Create(Font);
  (Font.RegularFont as TTextureFont).Load(TextureFont_DejaVuSans_20);
  Font.BoldFont := TTextureFont.Create(Font);
  (Font.BoldFont as TTextureFont).Load(TextureFont_DejaVuSansBold_20);
  Font.ItalicFont := TTextureFont.Create(Font);
  (Font.ItalicFont as TTextureFont).Load(TextureFont_DejaVuSansOblique_20);
  Font.BoldItalicFont := TTextureFont.Create(Font);
  (Font.BoldItalicFont as TTextureFont).Load(TextureFont_DejaVuSansBoldOblique_20);

  Background := TCastleRectangleControl.Create(Window);
  Background.Color := White; // Vector4(0.9, 0.9, 0.7, 1.0);
  Background.FullSize := true;
  Window.Controls.InsertFront(Background);

  ButtonHtml := TCastleButton.Create(Window);
  ButtonHtml.Caption := 'HTML';
  ButtonHtml.Toggle := true;
  ButtonHtml.Left := Margin;
  ButtonHtml.Bottom := Margin;
  ButtonHtml.OnClick := @TButtonHandler(nil).ClickHtml;
  ButtonHtml.Pressed := true;
  Window.Controls.InsertFront(ButtonHtml);

  ButtonWrap := TCastleButton.Create(Window);
  ButtonWrap.Caption := 'Wrap';
  ButtonWrap.Toggle := true;
  ButtonWrap.Left := 2 * Margin + ButtonHtml.EffectiveWidth;
  ButtonWrap.Bottom := Margin;
  ButtonWrap.OnClick := @TButtonHandler(nil).ClickWrap;
  ButtonWrap.Pressed := true;
  Window.Controls.InsertFront(ButtonWrap);

  ButtonAlignRight := TCastleButton.Create(Window);
  ButtonAlignRight.Caption := 'Right';
  ButtonAlignRight.Toggle := true;
  ButtonAlignRight.Anchor(hpRight, -Margin);
  ButtonAlignRight.Bottom := Margin;
  ButtonAlignRight.OnClick := @TButtonHandler(nil).ClickAlignRight;
  Window.Controls.InsertFront(ButtonAlignRight);

  ButtonAlignMiddle := TCastleButton.Create(Window);
  ButtonAlignMiddle.Caption := 'Middle';
  ButtonAlignMiddle.Toggle := true;
  ButtonAlignMiddle.Anchor(hpRight, -2 * Margin - ButtonAlignRight.EffectiveWidth);
  ButtonAlignMiddle.Bottom := Margin;
  ButtonAlignMiddle.OnClick := @TButtonHandler(nil).ClickAlignMiddle;
  Window.Controls.InsertFront(ButtonAlignMiddle);

  ButtonAlignLeft := TCastleButton.Create(Window);
  ButtonAlignLeft.Caption := 'Left';
  ButtonAlignLeft.Toggle := true;
  ButtonAlignLeft.Anchor(hpRight, -3 * Margin - ButtonAlignRight.EffectiveWidth - ButtonAlignMiddle.EffectiveWidth);
  ButtonAlignLeft.Bottom := Margin;
  ButtonAlignLeft.OnClick := @TButtonHandler(nil).ClickAlignLeft;
  ButtonAlignLeft.Pressed := true;
  Window.Controls.InsertFront(ButtonAlignLeft);

  ScrollView := TCastleScrollView.Create(Window);
  ScrollView.Anchor(vpTop);
  ScrollView.EnableDragging := true;
  ScrollView.FullSize := true;
  ScrollView.Border.Bottom := 2 * Margin + ButtonHtml.EffectiveHeight;
  ScrollView.Border.Left := Margin;
  ScrollView.Border.Right := Margin;
  ScrollView.Border.Top := Margin;
  Window.Controls.InsertFront(ScrollView);

  Label1 := TCastleLabel.Create(Window);
  Label1.Caption := {$I html_text_demo.html.inc};
  Label1.CustomFont := Font;
  Label1.Html := ButtonHtml.Pressed;
  Label1.Color := Black;
  ScrollView.ScrollArea.InsertFront(Label1);

  Window.OnResize := @WindowResize;
  Window.OpenAndRun;
end.
