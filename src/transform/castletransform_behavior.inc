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
  strict private
    FListenWorldChange: Boolean;
    procedure SetListenWorldChange(const Value: Boolean);
  private
    FParent: TCastleTransform;
  protected
    { Continuously occuring event, for various tasks.
      @param(RemoveMe Set this to rtRemove or rtRemoveAndFree to remove
        this component from @link(Parent) after this call finished.
        rtRemoveAndFree additionally will free this item.
        Initially it's rtNone when this method is called.) }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); virtual;

    { Event called right before rendering the parent TCastleTransform.
      Override this if you have to adjust the parent transformation strictly
      before the rendering.

      Compared to adjusting the parent transformation in e.g. @link(Update),
      adjusting the parent transformation here has advantages:

      @orderedList(
        @item(There's no 1-frame delay between moving the thing that causes
          the change (like camera) and updating the thing that reacts to it
          (like a transformation with a billboard).
        )

        @item(We adjust transformation before rendering
          each occurrence of TCastleTransform in the viewport tree.
          This matters if given TCastleTransform may occur multiple times
          in the viewport tree, directly or through @link(TCastleTransformReference).
        )

        @item(We adjust transformation before each render,
          so things are OK if given TCastleTransform is present in multiple
          viewports (see https://castle-engine.io/multiple_viewports_to_display_one_world).
        )
      )

      For example, adjusting the TCastleTransform done by the TCastleBillboard
      is done using this method. The transformation in this case is determined
      by the camera and parent's parent transformation. The transformation
      has to be applied without any delay (even a 1-frame delay would be visible,
      as artifacts caused by billboard not perfectly aligned with
      the camera would be visible -- blending sort along camera direction in sort3D
      is not perfect if billboard doesn't exactly match camera direction).
      And the transformation with billboard may be present in multiple viewports
      and/or multiple times in the same viewport tree.

      @param(CameraView
        Current camera vectors, in world coordinates, used to render the scene
        now.)

      @param(
        ParentParentWorldTransformation

        World transformation of the parent of the parent of this behavior.
        The name is kind of convoluted, but consistent:
        When the parent of this behavior has exactly one parent TCastleTransform
        (not zero, not more) then ParentParentWorldTransformation.Transform
        is equivalent to Parent.Parent.WorldTransform.
      )
    }
    procedure BeforeRender(
      const CameraView: TViewVectors;
      const ParentParentWorldTransformation: TTransformation); virtual;

    { Called after @link(Parent) changed, at the end of @link(TCastleTransform.AddBehavior),
      @link(TCastleTransform.RemoveBehavior). }
    procedure ParentChanged; virtual;
      deprecated 'Use ParentAfterAttach / ParentBeforeDetach or WorldAfterAttach / WorldBeforeDetach';

    { Called after @link(Parent) changed, e.g. at the end of @link(TCastleTransform.AddBehavior). }
    procedure ParentAfterAttach; virtual;

    { Called before @link(Parent) changed, e.g. at the beginning of @link(TCastleTransform.RemoveBehavior).

      This is also called at the destruction of behavior (more precisely from BeforeDestruction now).
      Even in this case, we guarantee that @link(Parent) is still valid during this call,
      and not in half-destroyed state.
      Before the actual destructor of TCastleBehavior we do BeforeParentDetach and BeforeWorldDetach
      and set Parent to @nil. }
    procedure ParentBeforeDetach; virtual;

    { Called after @link(Parent) became part of @link(World).
      Called only if ListenWorldChange is @true. }
    procedure WorldAfterAttach; virtual;

    { Called before @link(Parent) stops being part of @link(World).
      Called only if ListenWorldChange is @true.

      This is also called at the destruction of behavior (more precisely from BeforeDestruction now).
      Even in this case, we guarantee that @link(Parent) is still valid during this call,
      and not in half-destroyed state.
      Before the actual destructor of TCastleBehavior we do BeforeParentDetach and BeforeWorldDetach
      and set Parent to @nil. }
    procedure WorldBeforeDetach; virtual;

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
    procedure BeforeDestruction; override;

    (*
    // Not used in the end, for now the TransformDesigning and these methods are commented out.

    { Create design-time things that are not displayed in the hierarchy
      so can be created after behavior selection in editor hierarchy }
    procedure DesigningBegin; virtual;

    { Destroy design-time stuff. }
    procedure DesigningEnd; virtual;
    *)

    { Reference to the root transformation (typically matching the
      @link(TCastleViewport.Items) value) corresponding to the parent
      of this behavior.

      May be @nil if the behavior is not yet added to the parent TCastleTransform,
      or when the TCastleTransform is not yet part of the TCastleAbstractRootTransform
      (which means that TCastleTransform is not added to the viewport yet).
      You can use @link(ListenWorldChange) and override
      @link(WorldBeforeDetach), @link(WorldAfterAttach)
      to be notified when this changes. }
    function World: TCastleAbstractRootTransform;

    { Parent TCastleTransform of this behavior.
      Change it by doing @link(TCastleTransform.AddBehavior),
      @link(TCastleTransform.RemoveBehavior).

      @nil if this behavior is not yet added to a parent. }
    property Parent: TCastleTransform read FParent;

    { Set this to @true to receive WorldBeforeDetach, WorldAfterAttach calls. }
    property ListenWorldChange: Boolean
      read FListenWorldChange write SetListenWorldChange;
  end;

  TCastleBehaviorList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleBehavior>;

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

procedure TCastleBehavior.SetListenWorldChange(const Value: Boolean);
begin
  if FListenWorldChange <> Value then
  begin
    FListenWorldChange := Value;
    if Parent <> nil then
    begin
      if Value then
        Parent.AddWorldChangeNotification(Self)
      else
        Parent.RemoveWorldChangeNotification(Self)
    end;
  end;
end;

procedure TCastleBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
end;

procedure TCastleBehavior.BeforeRender(
  const CameraView: TViewVectors;
  const ParentParentWorldTransformation: TTransformation);
begin
end;

procedure TCastleBehavior.ParentChanged;
begin

end;

procedure TCastleBehavior.ParentAfterAttach;
begin

end;

procedure TCastleBehavior.ParentBeforeDetach;
begin

end;

procedure TCastleBehavior.WorldAfterAttach;
begin

end;

procedure TCastleBehavior.WorldBeforeDetach;
begin

end;

function TCastleBehavior.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  ReasonWhyCannot := '';
  Result := true;
end;

procedure TCastleBehavior.BeforeDestruction;
begin
  (*
  // The TransformDesigning mechanism works, but it unused for now.
  DesigningEnd;
  *)

  { Imagine this:

      TMyBehavior = class(TCastleBehavior)
        MyStuff: TObject;
        constructor Create;
        destructor Destroy; override;
        procedure BeforeParentDetach; override;
      end;

      constructor TMyBehavior.Create;
      begin
        inherited;
        MyStuff := TObject.Create;
      end;

      destructor TMyBehavior.Destroy;
      begin
        FreeAndNil(MyStuff);
        inherited;
      end;

      procedure TMyBehavior.BeforeParentDetach;
      begin
        MyStuff.Xxx := ...; // <- this is safe, because BeforeParentDetach is done from BeforeBehaviorDestroy
        inherited;
      end;

      var
        B: TMyBehavior;
      begin
        ..
        FreeAndNil(B);
      end;

    Thanks to calling Parent.BeforeBehaviorDestroy in TCastleBehavior.BeforeDestruction
    (and not TCastleBehavior.Destroy) accessing MyStuff in TMyBehavior.BeforeParentDetach
    is safe.
  }

  if Parent <> nil then
  begin
    Parent.BeforeBehaviorDestroy(Self);
    Assert(Parent = nil);
  end;

  inherited BeforeDestruction;
end;

function TCastleBehavior.World: TCastleAbstractRootTransform;
begin
  if Parent <> nil then
    Result := Parent.World
  else
    Result := nil;
end;

{$endif read_implementation}
