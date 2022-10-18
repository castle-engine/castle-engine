{%MainUnit castlepropedits.pas}
{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijanski

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { Editor for TCastleHingeJoint. }
  TCastleJointsComponentEditor = class(TComponentEditor)
  private
    function MenuCaption: String;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TCastleJointsComponentEditor.MenuCaption: String;
begin
  if not (Component as TCastleBehavior).InternalCreateGizmosCreated then
    Exit('Show Joint Tools')
  else
    Exit('Hide Joint Tools');
end;

function TCastleJointsComponentEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 1;
end;

function TCastleJointsComponentEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Result := MenuCaption;
  end else
    Result := '';
end;

procedure TCastleJointsComponentEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
  Behavior: TCastleBehavior;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Behavior := Component as TCastleBehavior;
    if Behavior.InternalCreateGizmosCreated then
      Behavior.InternalDestroyGizmos
    else
      Behavior.InternalCreateGizmos;
    GetDesigner.Modified;
  end else
    WritelnWarning(Behavior.ClassName + '.ExecuteVerb invalid verb index: %d', [Index]);
end;
