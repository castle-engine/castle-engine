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
var
  ShouldShow: Boolean;
begin
  ShouldShow := not (Component as TCastleBehavior).InternalCreateGizmosCreated;

  if (Component is TCastleHingeJoint)
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
    or (Component is TCastleFixedJoint)
    or (Component is TCastleWorldPlaneDistanceJoint)
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
    or (Component is TCastleBallJoint) then
  begin
    if ShouldShow then
      Exit('Show Joint Anchor')
    else
      Exit('Hide Joint Anchor');
  end;

  if (Component is TCastleRopeJoint)
     or (Component is TCastleDistanceJoint) then
  begin
    if ShouldShow then
      Exit('Show Joint Anchors')
    else
      Exit('Hide Joint Anchors');
  end;

  if (Component is TCastleGrabJoint) then
  begin
    if ShouldShow then
      Exit('Show Joint World Point')
    else
      Exit('Hide Joint World Point');
  end;

  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  if (Component is TCastlePulleyJoint) then
  begin
    if ShouldShow then
      Exit('Show Joint World and World Ground Anchors')
    else
      Exit('Hide Joint World and World Ground Anchors');
  end;

  if (Component is TCastleSliderJoint) then
  begin
    if ShouldShow then
       Exit('Show Joint World Anchor')
     else
       Exit('Hide Joint World Anchor');
  end;
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
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
