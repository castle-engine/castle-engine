{%MainUnit castlepropedits.pas}
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { Editor for TCastleImageControl. }
  TCastleImageControlComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TCastleImageControlComponentEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 1;
end;

function TCastleImageControlComponentEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Result := 'Reload URL';
  end else
    Result := '';
end;

procedure TCastleImageControlComponentEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  if Index = InheritedCount then
  begin
    (Component as TCastleImageControl).Content.ReloadUrl;
  end else
    WritelnWarning('TCastleImageControlComponentEditor.ExecuteVerb invalid verb index: %d', [Index]);
end;
