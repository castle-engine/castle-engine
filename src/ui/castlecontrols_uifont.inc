{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ Helper routine to get default font to draw UI.
  @exclude }
function InternalGetUIFont: TCastleAbstractFont;

{ Helper routine to set default font to draw UI.
  @exclude }
procedure InternalSetUIFont(const Value: TCastleAbstractFont);

{$ifdef FPC}
(*The default font, used throughout the engine as a fallback.
  By default, this is a modern sans-serif font hardcoded into the engine.

  Better use FallbackFont now, or use your own font, or render text
  using TCastleLabel.

  You can assign your own font here, to make this the new default font.

  Although in most cases it is better to leave @link(UIFont) unchanged
  (treat it like "engine default font"), and instead set your default font by
  setting @link(TCastleContainer.DefaultFont Window.Container.DefaultFont).
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
property UIFont: TCastleAbstractFont
  read InternalGetUIFont write InternalSetUIFont;
{$endif}
{ @groupEnd }

{ The default font, used throughout the engine as a fallback.
  This is a modern sans-serif font hardcoded into the engine. }
function FallbackFont: TCastleFont;

{ Force using FallbackFont (thus, ignore @link(TCastleContainer.DefaultFont))
  and FallbackTheme (thus, disable any @link(TCastleTheme Theme) customizations) in this UI.
  This guarantees the same UI look, ignoring user's UI font and theme customizations. }
procedure ForceFallbackLook(const Ui: TCastleUserInterface);

{$endif read_interface}

{$ifdef read_implementation}

{ UIFont --------------------------------------------------------------------- }

type
  { Component used as owner for fonts created on-demand in UIFont . }
  TDefaultUIFont = class(TComponent)
  public
    Normal: TCastleFont;
  end;

var
  DefaultUIFont: TDefaultUIFont;
  FUIFont: TCastleAbstractFont;

function FallbackFont: TCastleFont;
begin
  if DefaultUIFont = nil then
    DefaultUIFont := TDefaultUIFont.Create(nil);
  if DefaultUIFont.Normal = nil then
  begin
    DefaultUIFont.Normal := TCastleFont.Create(DefaultUIFont);
    DefaultUIFont.Normal.Load(TextureFont_DejaVuSans_20);
  end;
  Result := DefaultUIFont.Normal;
end;

function InternalGetUIFont: TCastleAbstractFont;
begin
  if FUIFont = nil then
    FUIFont := FallbackFont;
  Result := FUIFont;
end;

procedure InternalSetUIFont(const Value: TCastleAbstractFont);
begin
  FUIFont := Value;
end;

procedure FinalizationUIFonts;
begin
  if DefaultUIFont <> nil then
  begin
    if (FUIFont <> nil) and
       (FUIFont = DefaultUIFont.Normal) then
      FUIFont := nil;
    FreeAndNil(DefaultUIFont);
  end;
end;

procedure ForceFallbackLook(const Ui: TCastleUserInterface);
var
  Child: TCastleUserInterface;
begin
  Ui.CustomTheme := FallbackTheme;
  if Ui is TCastleUserInterfaceFont then
    TCastleUserInterfaceFont(Ui).CustomFont := FallbackFont;
  for Child in Ui do
    ForceFallbackLook(Child);
end;

{$endif read_implementation}
