{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TViewportNavigationEditor ----------------------------------------------------- }

type
  { For some reason, Viewport.Navigation is displayed as subcomponent
    in editor by default (with properties expanded).
    We don't want that, as then internal navigation instances
    (created when AutoNavigation) could be changed too
    (but they would not be saved). }
  TViewportNavigationEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

function TViewportNavigationEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
end;

function TViewportNavigationEditor.GetValue: String;
var
  V: TCastleNavigation;
begin
  V := GetObjectValue as TCastleNavigation;
  if V = nil then
    Result := '(none)'
  else
  if csTransient in V.ComponentStyle then
    Result := '(internal, because of AutoNavigation)'
  else
    Result := V.Name;
end;
