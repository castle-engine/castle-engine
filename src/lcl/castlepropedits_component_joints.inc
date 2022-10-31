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
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TCastleJointsComponentEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 2;
end;

function TCastleJointsComponentEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount - 1 then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Result := 'Show Joint Tools';
  end else
  if Index = InheritedCount + 1 then
  begin
    Result := 'Hide Joint Tools';
  end else
    Result := '';
end;

procedure TCastleJointsComponentEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
  Joint: TCastleAbstractJoint;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  begin
    Joint := Component as TCastleAbstractJoint;
    if Index = InheritedCount then
    begin
      Joint.InternalCreateGizmos;
      GetDesigner.Modified; // maybe not needed
    end else
    if Index = InheritedCount + 1 then
    begin
      Joint.InternalDestroyGizmos;
      GetDesigner.Modified; // maybe not needed
    end else
      WritelnWarning(Joint.ClassName + '.ExecuteVerb invalid verb index: %d', [Index]);
  end;
end;
