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

{ The 2D fonts used throughout UI interface.

  By default, this is a modern sans-serif font hardcoded into the engine.
  It will be automatically created and freed if needed. This is comfortable
  for simple applications, you can just start "drawing text"
  without initializing anything.

  You can assign your own font here, to make this the default
  font used by all 2D controls.

  Note that assigning font here @italic(does not)
  make it automatically freed (this would cause more trouble than comfort).
  To make sure your own fonts are always freed, set the font "owner" at creation,
  e.g. the example below sets the @code(Application) as owner:

  @longCode(#
    MyFont := TCastleFont.Create(Application);
    MyFont.Load(TextureFont_Xxxx);
    UIFont := MyFont;
  #)

  @groupBegin }
function GetUIFont: TCastleAbstractFont;
procedure SetUIFont(const Value: TCastleAbstractFont);

function GetUIFontSmall: TCastleAbstractFont;
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TCastleUserInterfaceFont.SmallFont';
procedure SetUIFontSmall(const Value: TCastleAbstractFont);
  deprecated 'use UIFont and temporarily change the size to be smaller, or use TCastleUserInterfaceFont.SmallFont';

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
