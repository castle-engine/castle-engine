{%MainUnit castleviewport.pas}
{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
//type (continue exising type declaration)

  { Navigation type that determines various navigation properties,
    used by @link(TCastleAutoNavigationViewport.NavigationType). }
  TNavigationType = (
    { Examine mode, comfortable to rotate the scene like an item held in your hand.
      Uses TCastleExamineNavigation. }
    ntExamine,
    { Turntable mode, similar to examine mode, but with a bit different interpretation
      of moves.
      Uses TCastleExamineNavigation. }
    ntTurntable,
    { Walk mode, comfortable to walk around the scene with gravity.
      Uses TCastleWalkNavigation. }
    ntWalk,
    { Fly mode, comfortable to move around around the scene without gravity.
      Uses TCastleWalkNavigation. }
    ntFly,
    { 2D mode, where the camera always looks along with Z axis, and you can pan / zoom in XY. }
    nt2D,
    { Disable user navigation on the scene.
      Uses TCastleWalkNavigation. }
    ntNone
  );

  { Automatically create and switch between various navigation classes on a viewport.

    @deprecated
    We don't advise using this class -- usually it is more straightforward
    to just explicitly create own instances of classes like TCastleExamineNavigation,
    TCastleWalkNavigation,
    add them as TCastleViewport children,
    control which one is used by @link(TCastleUserInterface.Exists).

    This class allows more implicit management of current navigation type.
    Navigation can be adjusted using

    @unorderedList(
      @item(auto-created when you use @link(WalkNavigation), @link(ExamineNavigation),)

      @item(get and set by @link(NavigationType),)

      @item(it can also automatically follow navigation requested in the X3D file
        (using TNavigationInfoNode in MainScene) if @link(AutoNavigation).)
    )
  }
  TCastleAutoNavigationViewport = class(TCastleViewport)
  strict private
    FInternalExamineNavigation: TCastleExamineNavigation;
    FInternalWalkNavigation: TCastleWalkNavigation;
    FInternal2DNavigation: TCastle2DNavigation;
    FAutoNavigation: Boolean;
    FWithinSetNavigationType: Boolean;
    InsideBoundNavigationInfoChanged: Boolean;

    function GetNavigationType: TNavigationType;
    procedure SetNavigationType(const Value: TNavigationType);

    procedure SetAutoNavigation(const Value: Boolean);
    { TNavigationType value determined by current NavigationInfo node. }
    class function NavigationTypeFromNavigationInfo(const Scene: TCastleSceneCore): TNavigationType; static;
  private
    { In addition to ancestor work:
      If AutoNavigation then the @link(Navigation) is automatically created here,
      see @link(AssignDefaultNavigation). }
    procedure ApplyProjection; override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BoundNavigationInfoChanged; override;
  public
    { Return current navigation. Automatically creates it if missing. }
    function RequiredNavigation: TCastleNavigation;

    { Return the currently used navigation as TCastleWalkNavigation, making sure that current
      NavigationType is something using TCastleWalkNavigation.

      @unorderedList(
        @item(
          When SwitchNavigationTypeIfNeeded is @true (the default),
          this method makes sure that the @link(NavigationType) corresponds to a type
          handled by TCastleWalkNavigation, creating and adjusting the navigation if necessary.

          If the current NavigationType does not use TCastleWalkNavigation
          (see @link(TNavigationType) documentation for information which
          navigation types use which TCastleNavigation descendants),
          then it's switched to ntWalk.
        )

        @item(
          When SwitchNavigationTypeIfNeeded is @false,
          then we return @nil if the current navigation is not already
          a TCastleWalkNavigation instance.

          We @italic(never) create a new navigation in this case
          (even if the NavigatinInfo node in MainScene would indicate
          that the new navigation would be a TCastleWalkNavigation).
        )
      )
    }
    function WalkNavigation(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleWalkNavigation;
      deprecated 'instead of this, just get and set "Navigation"; you can check if it is "TCastleWalkNavigation", you can set it to new "TCastleWalkNavigation" instance';

    { Return the currently used navigation as TCastleExamineNavigation, making sure that current
      NavigationType is something using TCastleExamineNavigation (but not TCastle2DNavigation,
      this wants a 3D navigation).

      @unorderedList(
        @item(
          When SwitchNavigationTypeIfNeeded is @true (the default),
          this method makes sure that the @link(NavigationType) corresponds to a type
          handled by TCastleExamineNavigation, creating and adjusting the navigation if necessary.

          If the current NavigationType does not use TCastleExamineNavigation
          (see @link(TNavigationType) documentation for information which
          navigation types use which TCastleNavigation descendants),
          then it's switched to ntExamine.
        )

        @item(
          When SwitchNavigationTypeIfNeeded is @false,
          then we return @nil if the current navigation is not already
          a TCastleExamineNavigation instance.

          We @italic(never) create a new navigation in this case
          (even if the NavigatinInfo node in MainScene would indicate
          that the new navigation would be a TCastleExamineNavigation).
        )
      )
    }
    function ExamineNavigation(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleExamineNavigation;
      deprecated 'instead of this, just get and set "Navigation"; you can check if it is "TCastleExamineNavigation", you can set it to new "TCastleExamineNavigation" instance';

    { Navigation instances internally used by this viewport.
      Using these methods automatically creates these instances
      (so they are never @nil).

      Using these methods @italic(does not) make these
      navigation instances current (in contast to calling @link(ExamineNavigation),
      @link(WalkNavigation) or setting @link(NavigationType)).

      When you switch navigation types by calling @link(ExamineNavigation),
      @link(WalkNavigation) or setting @link(NavigationType)
      the viewport keeps using these instances of navigation,
      instead of creating new navigation instances.
      This way all the navigation properties
      are preserved when you switch
      e.g. NavigationType from ntWalk to ntExamine to ntWalk again.

      @groupBegin }
    function InternalExamineNavigation: TCastleExamineNavigation;
    function InternalWalkNavigation: TCastleWalkNavigation;
    function Internal2DNavigation: TCastle2DNavigation;
    { @groupEnd }

    { Assign @link(Navigation) to a default TCastleNavigation suitable
      for navigating in this scene.

      This is automatically used when @link(Navigation) is @nil
      and @link(AutoNavigation).
      You can also use it explicitly.

      The implementation in base TCastleViewport uses TNavigationInfoNode
      from MainScene. If MainScene is not assigned, we create a simple
      navigation in Examine mode. }
    procedure AssignDefaultNavigation; virtual;

    { Set @link(Navigation) and some of its' parameters
      (like TCastleWalkNavigation.Gravity and so on).

      If @link(AutoNavigation), the initial @link(Navigation)
      as well as initial value of this property are automatically determined
      by the currently bound X3D NavigatinInfo node in the @link(TCastleRootTransform.MainScene MainScene),
      and world bounding box.
      They are also automatically adjusted e.g. when current NavigatinInfo
      node changes.

      But you can set @link(Navigation), or this property,
      manually to override the detected navigation.
      You should set @link(AutoNavigation) to @false to take control
      of @link(Navigation) and this property completely (no auto-detection
      based on @link(TCastleRootTransform.MainScene MainScene) will then take place).

      Note that you can also affect the current NavigationType by directly
      changing the camera properties,
      e.g. you can directly change @link(TCastleWalkNavigation.Gravity) from @false to @true,
      and thus you effectively switch from ntFly to ntWalk navigation types.
      When you read the NavigationType property, we determine the current navigation
      type from current camera properties.

      Setting this sets:
      @unorderedList(
        @itemSpacing compact
        @item @link(TCastleNavigation.Input)
        @item @link(TCastleExamineNavigation.Turntable), only in case of @link(TCastleExamineNavigation)
        @item @link(TCastleWalkNavigation.Gravity), only in case of @link(TCastleWalkNavigation)
        @item @link(TCastleWalkNavigation.PreferGravityUpForRotations), only in case of @link(TCastleWalkNavigation)
        @item @link(TCastleWalkNavigation.PreferGravityUpForMoving), only in case of @link(TCastleWalkNavigation)
      )

      If you write to NavigationType, then you @italic(should not) touch the
      above properties directly. That's because not every combination of
      above properties correspond to some sensible value of NavigationType.
      If you directly set some weird configuration, reading NavigationType will
      try it's best to determine the closest TNavigationType value
      that is similar to your configuration. }
    property NavigationType: TNavigationType
      read GetNavigationType write SetNavigationType
      default ntNone;

    class function InternalGetNavigationType(const V: TCastleViewport): TNavigationType;
  published
    { Assign sensible @link(Navigation) looking
      at the initial world (@link(Items)) if it is not assigned.

      This also allows to later synchronize navigation properties when X3D NavigationInfo
      node changes, or a new NavigationInfo node is bound.

      By default it is @false, which means that you control @link(Navigation) on your own.
    }
    property AutoNavigation: Boolean
      read FAutoNavigation write SetAutoNavigation default false;
  end {$ifdef FPC}deprecated 'use TCastleViewport instead of this; it is more straightforward to create TCastleNavigation explicitly (rather than auto-detecting it) and just add it as TCastleViewport child'{$endif};

{$endif read_interface}

{$ifdef read_implementation}

class function TCastleAutoNavigationViewport.NavigationTypeFromNavigationInfo(const Scene: TCastleSceneCore): TNavigationType;

  function StringToNavigationType(const S: string;
    out NavigationType: TNavigationType): boolean;
  begin
    Result := false;
    if S = 'WALK' then
    begin
      Result := true;
      NavigationType := ntWalk;
    end else
    if S = 'FLY' then
    begin
      Result := true;
      NavigationType := ntFly
    end else
    if S = 'NONE' then
    begin
      Result := true;
      NavigationType := ntNone
    end else
    if (S = 'EXAMINE') or
       (S = 'LOOKAT') then
    begin
      if S = 'LOOKAT' then
        WritelnWarning('X3D', 'TODO: Navigation type "LOOKAT" is not yet supported, treating like "EXAMINE"');
      Result := true;
      NavigationType := ntExamine;
    end else
    if (S = 'ARCHITECTURE') or
       (S = 'TURNTABLE') then
    begin
      Result := true;
      NavigationType := ntTurntable
    end else
    if (S = '2D') or
       { PLANE and PLANE_create3000.github.io names are used by X_ITE,
         see "Online X3D File Format Converter Powered by X_ITE" thread on x3d-public mailing list. }
       (S = 'PLANE') or
       (S = 'PLANE_create3000.github.io') then
    begin
      Result := true;
      NavigationType := nt2D;
    end else
    if S = 'ANY' then
    begin
      { Do nothing, also do not report this NavigationInfo.type as unknown. }
    end else
      WritelnWarning('X3D', 'Unknown NavigationInfo.type "%s"', [S]);
  end;

var
  I: Integer;
  NavigationNode: TNavigationInfoNode;
begin
  NavigationNode := Scene.NavigationInfoStack.Top;
  if NavigationNode <> nil then
    for I := 0 to NavigationNode.FdType.Count - 1 do
      if StringToNavigationType(NavigationNode.FdType.Items[I], Result) then
        Exit;

  { No recognized "type" found, so use default type EXAMINE. }
  Result := ntExamine;
end;

function TCastleAutoNavigationViewport.RequiredNavigation: TCastleNavigation;
begin
  { For backward-compatibility, this also initializes Camera vectors
    (even though the method docs only guarantee that it initializes Navigation,
    but in the past Camera and Navigation were the same thing). }
  EnsureCameraDetected;

  {$warnings off} // using deprecated in deprecated
  if Navigation = nil then
    AssignDefaultNavigation;
  // Since AssignDefaultNavigation may leave Navigation nil, make sure it is assigned now
  if Navigation = nil then
    Result := InternalExamineNavigation;
  Result := Navigation;
  {$warnings on}
end;

function TCastleAutoNavigationViewport.InternalExamineNavigation: TCastleExamineNavigation;
begin
  if FInternalExamineNavigation = nil then
  begin
    FInternalExamineNavigation := TCastleExamineNavigation.Create(Self);
    FInternalExamineNavigation.SetTransient;
  end;
  Result := FInternalExamineNavigation;
end;

function TCastleAutoNavigationViewport.InternalWalkNavigation: TCastleWalkNavigation;
begin
  if FInternalWalkNavigation = nil then
  begin
    FInternalWalkNavigation := TCastleWalkNavigation.Create(Self);
    FInternalWalkNavigation.SetTransient;
  end;
  Result := FInternalWalkNavigation;
end;

function TCastleAutoNavigationViewport.Internal2DNavigation: TCastle2DNavigation;
begin
  if FInternal2DNavigation = nil then
  begin
    FInternal2DNavigation := TCastle2DNavigation.Create(Self);
    FInternal2DNavigation.SetTransient;
  end;
  Result := FInternal2DNavigation;
end;

function TCastleAutoNavigationViewport.ExamineNavigation(const SwitchNavigationTypeIfNeeded: boolean): TCastleExamineNavigation;
var
  NewNavigation: TCastleExamineNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  if (not (Navigation is TCastleExamineNavigation)) or
     // when TCastle2DNavigation, we want to change it 3D examine navigation here
     (Navigation is TCastle2DNavigation) then
  begin
    if not SwitchNavigationTypeIfNeeded then
      Exit(nil);

    { For backward-compatibility, this also initializes Camera vectors
      (even though the method docs only guarantee that it initializes Navigation,
      but in the past Camera and Navigation were the same thing). }
    EnsureCameraDetected;

    NewNavigation := InternalExamineNavigation;
    if Navigation = nil then
      AssignDefaultNavigation; // initialize defaults from MainScene
    // AssignDefaultNavigation could leave Navigation at nil, in which case ignor
    if Navigation <> nil then
      NewNavigation.Assign(Navigation);
    Navigation := NewNavigation;
    { make sure it's in ntExamine mode (as we possibly reuse old navigation,
      by reusing InternalExamineNavigation, so we're not sure what state it's in. }
    NavigationType := ntExamine;
  end;
  Result := Navigation as TCastleExamineNavigation;
  {$warnings on}
end;

function TCastleAutoNavigationViewport.WalkNavigation(const SwitchNavigationTypeIfNeeded: boolean): TCastleWalkNavigation;
var
  NewNavigation: TCastleWalkNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  if not (Navigation is TCastleWalkNavigation) then
  begin
    if not SwitchNavigationTypeIfNeeded then
      Exit(nil);

    { For backward-compatibility, this also initializes Camera vectors
      (even though the method docs only guarantee that it initializes Navigation,
      but in the past Camera and Navigation were the same thing). }
    EnsureCameraDetected;

    NewNavigation := InternalWalkNavigation;
    if Navigation = nil then
      AssignDefaultNavigation; // initialize defaults from MainScene
    // AssignDefaultNavigation could leave Navigation at nil, in which case ignor
    if Navigation <> nil then
      NewNavigation.Assign(Navigation);
    Navigation := NewNavigation;
    { make sure it's in ntWalk mode (as we possibly reuse old navigation,
      by reusing InternalWalkNavigation, so we're not sure what state it's in. }
    NavigationType := ntWalk;
  end;
  Result := Navigation as TCastleWalkNavigation;
  {$warnings on}
end;

class function TCastleAutoNavigationViewport.InternalGetNavigationType(const V: TCastleViewport): TNavigationType;
var
  Nav: TCastleNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Nav := V.Navigation;
  {$warnings on}

  { We are using here Navigation, not RequiredNavigation, as automatically
    creating Navigation could have surprising consequences.
    E.g. it means that SetNavigation(nil) may recreate the navigation,
    as BoundNavigationInfoChanged calls something that checks
    NavigationType. }

  if (Nav <> nil) and (Nav.Input = []) then
    Exit(ntNone);

  if Nav is TCastle2DNavigation then
    Result:= nt2D
  else
  if Nav is TCastleExamineNavigation then
  begin
    if TCastleExamineNavigation(Nav).Turntable then
      Result := ntTurntable
    else
      Result := ntExamine;
  end else
  if Nav is TCastleWalkNavigation then
  begin
    if TCastleWalkNavigation(Nav).Gravity then
      Result := ntWalk
    else
      Result := ntFly;
  end else
    Result := ntNone; // also accounts for Nav = nil case
end;

function TCastleAutoNavigationViewport.GetNavigationType: TNavigationType;
begin
  Result := InternalGetNavigationType(Self);
end;

procedure TCastleAutoNavigationViewport.SetNavigationType(const Value: TNavigationType);
var
  E: TCastleExamineNavigation;
  W: TCastleWalkNavigation;
begin
  { Do this even if "Value = GetNavigationType".
    This makes sense, in case you set some weird values.
    On the other hand, it makes "NavigationType := NavigationType" sometimes
    a sensible operation that changes something.

    It also avoids recursive loop when first assigning navigation
    in AssignDefaultNavigation. }

  { do not change NavigationType when
    SetNavigationType is called from ExamineNavigation or WalkNavigation
    that were already called by NavigationType.
    It's actually harmless, but still useless. }
  if FWithinSetNavigationType then
    Exit;
  FWithinSetNavigationType := true;

  case Value of
    ntExamine:
      begin
        {$warnings off} // TODO: this should be internal
        E := ExamineNavigation;
        {$warnings on}
        E.Input := TCastleNavigation.DefaultInput;
        E.Turntable := false;
      end;
    ntTurntable:
      begin
        {$warnings off} // TODO: this should be internal
        E := ExamineNavigation;
        {$warnings on}
        E.Input := TCastleNavigation.DefaultInput;
        E.Turntable := true;
      end;
    ntWalk:
      begin
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := TCastleNavigation.DefaultInput;
        W.PreferGravityUpForRotations := true;
        W.PreferGravityUpForMoving := true;
        W.Gravity := true;
      end;
    ntFly:
      begin
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := TCastleNavigation.DefaultInput;
        W.PreferGravityUpForRotations := true;
        W.PreferGravityUpForMoving := false;
        W.Gravity := false;
      end;
    nt2D:
      begin
        {$warnings off} // using deprecated Navigation, that should be internal
        if Navigation <> Internal2DNavigation then
        begin
          if Navigation <> nil then // copy properties like Input, Radius from current Navigation
            Internal2DNavigation.Assign(Navigation);
          Navigation := Internal2DNavigation;
        end;
        {$warnings on}
      end;
    ntNone:
      begin
        { Advantage: This way setting NavigationType to ntNone (default NavigationType value)
          will restore Navigation to nil, which is Navigation default value. }
        // Navigation := nil;

        { Advantage: This way of setting NavigationType to ntNone (by making Navigation non-nil)
          explicitly will prevent
          Navigation from being auto-created (in case AutoNavigation remains @true),
          which would make setting "NavigationType := ntNone" moot. }
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := [];
        W.Gravity := false;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleViewport.SetNavigationType: Value?');
    {$endif}
  end;

  { This assertion should be OK. It is commented out only to prevent
    GetNavigationType from accidentally creating something intermediate,
    and thus making debug and release behavior different) }
  // Assert(GetNavigationType = Value);

  FWithinSetNavigationType := false;

  { Call OnBoundNavigationInfoChanged when NavigationType changed.
    Use "inherited" to not do own code that would recursively change NavigationType
    (to something else than Value, possibly). }
  inherited BoundNavigationInfoChanged;
end;

procedure TCastleAutoNavigationViewport.AssignDefaultNavigation;
var
  Scene: TCastleScene;
  C: TCastleExamineNavigation;
  NewNavigationType: TNavigationType;
begin
  {$warnings off} // using deprecated in deprecated
  Scene := Items.MainScene;
  if Scene <> nil then
  begin
    NewNavigationType := NavigationTypeFromNavigationInfo(Scene);

    { Set Navigation explicitly, otherwise SetNavigationType below could call
      ExamineNavigation / WalkNavigation that call AssignDefaultNavigation when Navigation = nil,
      and we would have infinite AssignDefaultNavigation calls loop. }
    if NewNavigationType in [ntExamine, ntTurntable] then
      Navigation := InternalExamineNavigation
    else
      Navigation := InternalWalkNavigation;
    NavigationType := NewNavigationType;
    { Call InternalUpdateNavigation on both internal navigation instances,
      not only on current Navigation, to keep all properties, even the ones specific
      to a class (like TCastleWalkNavigation.PreferredHeight) always up-to-date. }
    Scene.InternalUpdateNavigation(InternalExamineNavigation);
    Scene.InternalUpdateNavigation(InternalWalkNavigation);
  end else
  begin
    C := InternalExamineNavigation;
    C.Radius := DefaultCameraRadius;
    Navigation := C;
  end;
  {$warnings on}
end;

procedure TCastleAutoNavigationViewport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    // Note that we don't register on FInternalExamine/WalkNavigation destruction
    // when they are not current, so they should never be freed in that case.
    if AComponent = FInternalWalkNavigation then
      FInternalWalkNavigation := nil;
    if AComponent = FInternalExamineNavigation then
      FInternalExamineNavigation := nil;
    if AComponent = FInternal2DNavigation then
      FInternal2DNavigation := nil;
  end;
end;

procedure TCastleAutoNavigationViewport.SetAutoNavigation(const Value: Boolean);
begin
  if FAutoNavigation <> Value then
  begin
    FAutoNavigation := Value;
    { Not necessary, and we actually don't have AssignDefaultNavigationDone.
      The navigation will be auto-assigned when it is nil. }
    // if InternalDesignManipulation and Value then
    //   AssignDefaultNavigationDone := false;
  end;
end;

procedure TCastleAutoNavigationViewport.BoundNavigationInfoChanged;
begin
  { Use InsideBoundNavigationInfoChanged to prevent infinite loop
    of BoundNavigationInfoChanged calls, as "NavigationType := " causes
    BoundNavigationInfoChanged. }
  if not InsideBoundNavigationInfoChanged then
  begin
    InsideBoundNavigationInfoChanged := true;
    {$warnings off} // using deprecated to keep it working
    if AutoNavigation and (Navigation <> nil) and (Items.MainScene <> nil) then
    begin
      NavigationType := NavigationTypeFromNavigationInfo(Items.MainScene);
      Items.MainScene.InternalUpdateNavigation(Navigation);
      { Call InternalUpdateNavigation also on both internal navigation instances,
        to keep all properties, even the ones not copied by Assign (because specific
        to a class, like TCastleWalkNavigation.PreferredHeight) always up-to-date. }
      if Navigation <> InternalWalkNavigation then
        Items.MainScene.InternalUpdateNavigation(InternalWalkNavigation);
      if Navigation <> InternalExamineNavigation then
        Items.MainScene.InternalUpdateNavigation(InternalExamineNavigation);
    end;
    {$warnings on}
    InsideBoundNavigationInfoChanged := false;
  end;
  inherited;
end;

procedure TCastleAutoNavigationViewport.ApplyProjection;
begin
  {$warnings off} // using deprecated to keep it working
  if AutoNavigation and (Navigation = nil) then
    AssignDefaultNavigation; // create Navigation if necessary
  {$warnings on}
  inherited;
end;

{$endif read_implementation}
