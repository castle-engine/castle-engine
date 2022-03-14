{%MainUnit castletransform.pas}
{
  Copyright 2020-2022 Michalis Kamburelis.

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
  TCastleBehaviorClass = class of TCastleBehavior;

  { Behaviors can be attached to TCastleTransform to perform specific logic,
    for example implement creature movement.
    This implements a simple component-system for TCastleTransform.
    Using behaviors allows to attach independent functionality like
    TCastleBillboard, TCastleSoundSource, creature AI and more.
    CGE provides implementation for some behaviors, and you can create your own too.

    You implemement a descendant of TCastleBehavior, typically overriding
    its @link(Update) method, and add it to TCastleTransform by @link(TCastleTransform.AddBehavior).
    Inside TCastleBehavior, access the TCastleTransform instances by @link(Parent).

    The API of TCastleBehavior is deliberately a subset of the TCastleTransform API,
    for example both have @link(Update) method.
    Using TCastleBehavior is just simpler and more efficient, as TCastleBehavior doesn't have
    own transformation, children and it cannot render something.
    If this is too limiting, remember you can always implement more powerful behaviors
    by creating TCastleTransform descendants instead of TCastleBehavior descendants,
    and accessing the @link(TCastleTransform.Parent) from a child.
  }
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

    { Check can this behavior be added to NewParent.
      When this returns @false, it has to set also ReasonWhyCannot.
      When overriding this, you can use e.g. this code to make sure we are the only
      behavior of given class:

      @longCode(#
      function TCastleBillboard.CanAttachToParent(const NewParent: TCastleTransform;
        out ReasonWhyCannot: String): Boolean;
      begin
        Result := inherited;
        if not Result then Exit;

        if NewParent.FindBehavior(TCastleBillboard) <> nil then
        begin
          ReasonWhyCannot := 'Only one TCastleBillboard behavior can be added to a given TCastleTransform';
          Result := false;
        end;
      end;
      #)
    }
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; virtual;
  public
    { Parent TCastleTransform of this behavior.
      Change it by doing @link(TCastleTransform.AddBehavior),
      @link(TCastleTransform.RemoveBehavior).

      @nil if this behavior is not yet added to a parent. }
    property Parent: TCastleTransform read FParent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTransform.TCastleBehaviorEnumerator ------------------------------------------------- }

{ TCastleBehaviorEnumerator is optimized to be a record, following
  https://hallvards.blogspot.com/2007/10/more-fun-with-enumerators.html }

constructor TCastleTransform.TCastleBehaviorEnumerator.Create(const AParent: TCastleTransform);
begin
//  inherited Create;
  FParent := AParent;
  FPosition := -1;
end;

function TCastleTransform.TCastleBehaviorEnumerator.GetCurrent: TCastleBehavior;
begin
  Result := FParent.Behaviors[FPosition];
end;

function TCastleTransform.TCastleBehaviorEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FParent.BehaviorsCount;
end;

function TCastleTransform.TCastleBehaviorEnumerator.GetEnumerator: TCastleBehaviorEnumerator;
begin
  // Returns itself. See https://wiki.freepascal.org/for-in_loop
  Result := Self;
end;

{ TCastleBehavior ------------------------------------------------------------ }

procedure TCastleBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
end;

procedure TCastleBehavior.ParentChanged;
begin
end;

function TCastleBehavior.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  ReasonWhyCannot := '';
  Result := true;
end;

{$endif read_implementation}
