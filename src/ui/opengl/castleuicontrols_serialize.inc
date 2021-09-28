{%MainUnit castleuicontrols.pas}
{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration of TCastleUserInterface with CastleComponentSerialize. }

{$ifdef read_interface}

{ Save / load TCastleUserInterface (or descendant) to a .castle-user-interface file. }
procedure UserInterfaceSave(const C: TCastleUserInterface; const Url: String);
function UserInterfaceLoad(const Url: String; const Owner: TComponent): TCastleUserInterface;

{$endif read_interface}

{$ifdef read_implementation}

procedure UserInterfaceSave(const C: TCastleUserInterface; const Url: String);
begin
  ComponentSave(C, Url);
end;

function UserInterfaceLoad(const Url: String; const Owner: TComponent): TCastleUserInterface;
begin
  Result := ComponentLoad(Url, Owner) as TCastleUserInterface;
end;

{$endif read_implementation}
