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
  { Editor for TCastleTransform. }
  TCastleTransformComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TCastleTransformComponentEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 1;
end;

function TCastleTransformComponentEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Result := 'Reset Transformation';
  end else
    Result := '';
end;

procedure TCastleTransformComponentEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  if Index = InheritedCount then
  begin
    (Component as TCastleTransform).Identity;
    GetDesigner.Modified;
  end else
    WritelnWarning('TCastleTransformComponentEditor.ExecuteVerb invalid verb index: %d', [Index]);
end;
