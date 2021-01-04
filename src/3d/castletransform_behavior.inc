{%MainUnit castletransform.pas}
{
  Copyright 2020-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ TCastleBehavior. }

{$ifdef read_interface}

  { Behaviors can be attached to TCastleTransform to perform specific logic,
    for example implement creature movement.
    This implements a simple component-system for TCastleTransform.
    Using behaviors allows to attach independent functionality like
    inventory, being alive (because not everything alive needs inventory,
    not everything with inventory needs to be alive), creature logic and more.
    CGE provides implementation for some behaviours, and you can create your own too.

    You implemement a descendant of TCastleBehavior, typically overriding
    its @link(Update) method, and add it to TCastleTransform by @link(TCastleTransform.AddBehavior).
    Inside TCastleBehavior, access the TCastleTransform instances by @link(Parent).

    The API of TCastleBehavior is deliberately a subset of the TCastleTransform API,
    for example both have @link(Update) method.
    In principle, you could achieve the same features by just using TCastleTransform descendants
    instead of TCastleBehavior descendants, accessing the @link(TCastleTransform.UniqueParent)
    from a child. However using TCastleBehavior is more efficient, as TCastleBehavior don't have their
    own transformation and children. }
  TCastleBehavior = class(TCastleComponent)
  private
    FParent: TCastleTransform;
  protected
    { Continuously occuring event, for various tasks.
      @param(RemoveMe Set this to rtRemove or rtRemoveAndFree to remove
        this component from @link(Parent) after this call finished.
        rtRemoveAndFree additionally will free this item.
        Initially it's rtNone when this method is called.) }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); virtual;

    { Called after @link(Parent) changed, at the end of @link(TCastleTransform.AddBehavior),
      @link(TCastleTransform.RemoveBehavior). }
    procedure ParentChanged; virtual;
  public
    { Parent TCastleTransform of this behavior.
      Change it by doing @link(TCastleTransform.AddBehavior),
      @link(TCastleTransform.RemoveBehavior).

      @nil if this behavior is not yet added to a parent. }
    property Parent: TCastleTransform read FParent;
  end;

  TCastleBehaviorClass = class of TCastleBehavior;

{$endif read_interface}

{$ifdef read_implementation}

procedure TCastleBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
end;

procedure TCastleBehavior.ParentChanged;
begin
end;

{$endif read_implementation}
