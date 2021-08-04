{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

function GetUIFont: TCastleAbstractFont;
procedure SetUIFont(const Value: TCastleAbstractFont);

function GetUIFontSmall: TCastleAbstractFont;
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TCastleUserInterfaceFont.SmallFont';
procedure SetUIFontSmall(const Value: TCastleAbstractFont);
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TCastleUserInterfaceFont.SmallFont';

(*The default font, used throughout the engine as a fallback.
  By default, this is a modern sans-serif font hardcoded into the engine.

  You can assign your own font here, to make this the new default font.

  Although in most cases it is better to leave @link(UIFont) unchanged
  (treat it like "engine default font"), and instead set your default font by
  setting @link(TUIContainer.DefaultFont Window.Container.DefaultFont).
  You can do it by code, or by editing CastleSettings.xml (this way editor
  will be aware of this new default), see https://castle-engine.io/manual_castle_settings.php .

  Note that assigning font here @italic(does not) make it automatically freed.
  Use the ownership mechanism to make sure it is freed,
  i.e. assign some owner to your font instance, like this:

  @longCode(#
    MyFont := TCastleFont.Create({ owner } Application);
    MyFont.Load(TextureFont_Xxxx);
    UIFont := MyFont;
  #)
*)
property UIFont: TCastleAbstractFont read GetUIFont write SetUIFont;
property UIFontSmall: TCastleAbstractFont read GetUIFontSmall write SetUIFontSmall;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

{ UIFont --------------------------------------------------------------------- }

type
  { Component used as owner for fonts created on-demand in UIFont and UIFontSmall. }
  TDefaultUIFont = class(TComponent)
  public
    Normal: TCastleAbstractFont;
    Small: TCastleAbstractFont;
  end;

var
  DefaultUIFont: TDefaultUIFont;
  FUIFont, FUIFontSmall: TCastleAbstractFont;

function GetUIFont: TCastleAbstractFont;
begin
  if FUIFont = nil then
  begin
    if DefaultUIFont = nil then
      DefaultUIFont := TDefaultUIFont.Create(nil);
    if DefaultUIFont.Normal = nil then
    begin
      DefaultUIFont.Normal := TCastleFont.Create(DefaultUIFont);
      (DefaultUIFont.Normal as TCastleFont).Load(TextureFont_DejaVuSans_20);
    end;
    FUIFont := DefaultUIFont.Normal;
  end;

  Result := FUIFont;
end;

procedure SetUIFont(const Value: TCastleAbstractFont);
begin
  FUIFont := Value;
end;

function GetUIFontSmall: TCastleAbstractFont;
begin
  if FUIFontSmall = nil then
  begin
    if DefaultUIFont = nil then
      DefaultUIFont := TDefaultUIFont.Create(nil);
    if DefaultUIFont.Small = nil then
    begin
      DefaultUIFont.Small := TCastleFont.Create(DefaultUIFont);
      (DefaultUIFont.Small as TCastleFont).Load(TextureFont_DejaVuSans_10);
    end;
    FUIFontSmall := DefaultUIFont.Small;
  end;

  Result := FUIFontSmall;
end;

procedure SetUIFontSmall(const Value: TCastleAbstractFont);
begin
  FUIFontSmall := Value;
end;

procedure FinalizationUIFonts;
begin
  if DefaultUIFont <> nil then
  begin
    if (FUIFont <> nil) and
       (FUIFont = DefaultUIFont.Normal) then
      FUIFont := nil;
    if (FUIFontSmall <> nil) and
       (FUIFontSmall = DefaultUIFont.Small) then
      FUIFontSmall := nil;
    FreeAndNil(DefaultUIFont);
  end;
end;

{$endif read_implementation}
