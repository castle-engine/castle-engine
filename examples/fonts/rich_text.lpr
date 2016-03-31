{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleLabel, with long wrapped text with HTML tags inside. }

uses SysUtils, Classes,
  CastleTextureFont_DjvSans_20, CastleTextureFont_DjvSansB_20,
  CastleTextureFont_DjvSansO_20, CastleTextureFont_DjvSansBO_20,
  CastleFonts, CastleFontFamily, CastleControls, CastleUIControls, CastleWindow,
  CastleVectors, CastleColors;

const
  Margin = 10;
var
  Label1: TCastleLabel;
  ButtonTags, ButtonWrap, ButtonAlignLeft, ButtonAlignMiddle, ButtonAlignRight: TCastleButton;

type
  TButtonHandler = class
    class procedure ClickTags(Sender: TObject);
    class procedure ClickWrap(Sender: TObject);
    class procedure ClickAlignLeft(Sender: TObject);
    class procedure ClickAlignMiddle(Sender: TObject);
    class procedure ClickAlignRight(Sender: TObject);
  end;

class procedure TButtonHandler.ClickTags(Sender: TObject);
begin
  ButtonTags.Pressed := not ButtonTags.Pressed;
  Label1.Tags := ButtonTags.Pressed;
end;

class procedure TButtonHandler.ClickWrap(Sender: TObject);
begin
  ButtonWrap.Pressed := not ButtonWrap.Pressed;
  if ButtonWrap.Pressed then
    Label1.MaxWidth := Label1.Container.Width - Margin * 2 else
    Label1.MaxWidth := 0;
end;

class procedure TButtonHandler.ClickAlignLeft(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := true;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := false;
  Label1.Alignment := hpLeft;
end;

class procedure TButtonHandler.ClickAlignMiddle(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := true;
  ButtonAlignRight.Pressed := false;
  Label1.Alignment := hpMiddle;
end;

class procedure TButtonHandler.ClickAlignRight(Sender: TObject);
begin
  ButtonAlignLeft.Pressed := false;
  ButtonAlignMiddle.Pressed := false;
  ButtonAlignRight.Pressed := true;
  Label1.Alignment := hpRight;
end;

procedure WindowResize(Container: TUIContainer);
begin
  if ButtonWrap.Pressed then
    Label1.MaxWidth := Container.Width - Margin * 2;
end;

var
  Window: TCastleWindowCustom;
  Background: TCastleSimpleBackground;
  Font: TFontFamily;
begin
  Window := TCastleWindowCustom.Create(Application);

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

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := White; // Vector4Single(0.9, 0.9, 0.7, 1.0);
  Window.Controls.InsertFront(Background);

  ButtonTags := TCastleButton.Create(Window);
  ButtonTags.Caption := 'Tags';
  ButtonTags.Toggle := true;
  ButtonTags.Left := Margin;
  ButtonTags.Bottom := Margin;
  ButtonTags.OnClick := @TButtonHandler(nil).ClickTags;
  ButtonTags.Pressed := true;
  Window.Controls.InsertFront(ButtonTags);

  ButtonWrap := TCastleButton.Create(Window);
  ButtonWrap.Caption := 'Wrap';
  ButtonWrap.Toggle := true;
  ButtonWrap.Left := 2 * Margin + ButtonTags.Width;
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
  ButtonAlignMiddle.Anchor(hpRight, -2 * Margin - ButtonAlignRight.Width);
  ButtonAlignMiddle.Bottom := Margin;
  ButtonAlignMiddle.OnClick := @TButtonHandler(nil).ClickAlignMiddle;
  Window.Controls.InsertFront(ButtonAlignMiddle);

  ButtonAlignLeft := TCastleButton.Create(Window);
  ButtonAlignLeft.Caption := 'Left';
  ButtonAlignLeft.Toggle := true;
  ButtonAlignLeft.Anchor(hpRight, -3 * Margin - ButtonAlignRight.Width - ButtonAlignMiddle.Width);
  ButtonAlignLeft.Bottom := Margin;
  ButtonAlignLeft.OnClick := @TButtonHandler(nil).ClickAlignLeft;
  ButtonAlignLeft.Pressed := true;
  Window.Controls.InsertFront(ButtonAlignLeft);

  Label1 := TCastleLabel.Create(Window);
  Label1.Text.Text := {$I rich_text_demo.html.inc};
  Label1.CustomFont := Font;
  Label1.Left := Margin;
  Label1.Bottom := 2 * Margin + ButtonTags.Height;
  Label1.Tags := ButtonTags.Pressed;
  Label1.Color := Black;
  Window.Controls.InsertFront(Label1);

  Window.OnResize := @WindowResize;
  Window.OpenAndRun;
end.
