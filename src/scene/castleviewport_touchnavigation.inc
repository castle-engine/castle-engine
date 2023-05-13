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

type
  { Value for @link(TCastleTouchNavigation.TouchInterface). }
  TTouchInterface = (
    tiNone,
    tiWalk,
    tiWalkRotate,
    tiFlyWalk,
    tiPan
  );

  { Show draggable controls in the corner, to navigate
    in the viewport comfortably on touch devices.

    Depending on @link(AutoTouchInterface) and @link(TouchInterface),
    we will show 0, 1 or 2 controls to navigate at the bottom,
    in the left-bottom and right-bottom.
    It is easiest to just set

    @longCode(#
      MyTouchNavigation.TouchInterface := tiNone; // default
      MyTouchNavigation.AutoTouchInterface := ApplicationProperties.TouchDevice;
    #)

    to activate this automatically on mobile devices, leaving @code(MyTouchNavigation.TouchInterface)
    equal tiNone on non-mobile devices.
    Alternatively, you can do something like this to control it manually on mobile devices:

    @longCode(#
      MyTouchNavigation.AutoTouchInterface := false; // default
      MyTouchNavigation.Exists := ApplicationProperties.TouchDevice;
      MyTouchNavigation.TouchInterface := tiSomething;
    #)

    The size and position of this control determines where the controls appear.
    Typically this control is added as a direct child of TCastleViewport
    and has @link(FullSize) = @true, this way it just fills the TCastleViewport.

    The @link(Viewport) determines the viewport where navigation is affected
    by this control.
    Typically the @link(Viewport) is also our visual parent, but it doesn't have to be.
  }
  TCastleTouchNavigation = class(TCastleUserInterface)
  strict private
    FViewport: TCastleViewport;
    FLastSeenNavigationType: TNavigationType;
    FAutoTouchInterface: Boolean;
    {$warnings off} // TCastleTouchControl should be internal here
    FControl: array [Boolean { right side? }] of TCastleTouchControl;
    {$warnings on}
    FTouchInterface: TTouchInterface;
    FAutoWalkTouchInterface, FAutoExamineTouchInterface: TTouchInterface;
    FControlMouseDragMode: Boolean;
    FScale: Single;
    { Apply current TouchInterface value.
      Call when TouchInterface or ControlMouseDragMode changed. }
    procedure UpdateTouchInterface;
    { Update current TouchInterface if AutoTouchInterface enabled.
      Call when AutoTouchInterface, Viewport,
      AutoExamineTouchInterface, AutoWalkTouchInterface, current navigation changed. }
    procedure UpdateAutoTouchInterface;
    procedure SetViewport(const Value: TCastleViewport);
    procedure SetTouchInterface(const Value: TTouchInterface);
    procedure SetAutoTouchInterface(const Value: Boolean);
    procedure SetAutoWalkTouchInterface(const Value: TTouchInterface);
    procedure SetAutoExamineTouchInterface(const Value: TTouchInterface);
    function TouchInterfaceStored: Boolean;
    procedure SetControlMouseDragMode(const Value: Boolean);
    procedure SetScale(const Value: Single);
  public
    const
      DefaultAutoWalkTouchInterface = tiWalk;
      DefaultAutoExamineTouchInterface = tiPan;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Viewport where navigation is affected by this control.

      You must set this property to something non-nil,
      otherwise this UI control doesn't do much, it will not affect anything. }
    property Viewport: TCastleViewport read FViewport write SetViewport;

    { Configure controls to be visible and available to navigate.
      This automatically manages under the hood 0, 1 or 2
      controls to navigate, placing them at suitable positions
      and handling their operations.

      Note that you can set AutoTouchInterface = @true to have this property
      automatically adjusted. (In which case you should not set this directly.)

      When @link(ControlMouseDragMode) then this property additionally controls
      the @link(TCastleWalkNavigation.MouseDragMode). }
    property TouchInterface: TTouchInterface
      read FTouchInterface write SetTouchInterface stored TouchInterfaceStored
      default tiNone;

    { Automatically adjust @link(TouchInterface) (showing / hiding proper
      touch controls) based on the detected current @link(Viewport) navigation type. }
    property AutoTouchInterface: boolean
      read FAutoTouchInterface write SetAutoTouchInterface
      default false;

    { When using AutoTouchInterface = @true,
      which touch interface should be used when walking
      (since there are multiple sensible choices).
      Select between tiWalkRotate or tiWalk (default).}
    property AutoWalkTouchInterface: TTouchInterface
      read FAutoWalkTouchInterface write SetAutoWalkTouchInterface
      default DefaultAutoWalkTouchInterface;

    { When using AutoTouchInterface = @true,
      which touch interface should be used in examine camera
      (since examine camera can use multi-touch gesture instead).
      Select between tiPan (default) or tiNone.}
    property AutoExamineTouchInterface: TTouchInterface
      read FAutoExamineTouchInterface write SetAutoExamineTouchInterface
      default DefaultAutoExamineTouchInterface;

    { Control also @link(TCastleWalkNavigation.MouseDragMode) by
      the @link(AutoTouchInterface) and @link(TouchInterface) setting.

      We advise to not use this property, and instead set @link(TCastleWalkNavigation.MouseDragMode)
      manually and explicitly. Otherwise this automatic control may be confusing, as it overrides
      what you set in editor as @link(TCastleWalkNavigation.MouseDragMode) value. }
    property ControlMouseDragMode: Boolean
      read FControlMouseDragMode write SetControlMouseDragMode default false;

    { Visual scale of the device. }
    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1.0{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTouchNavigation ----------------------------------------------------- }

constructor TCastleTouchNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAutoWalkTouchInterface := DefaultAutoWalkTouchInterface;
  FAutoExamineTouchInterface := DefaultAutoExamineTouchInterface;
  FScale := 1.0;
end;

procedure TCastleTouchNavigation.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  RightSide: Boolean;
begin
  inherited;

  if (FControl[false] <> nil) or
     (FControl[true] <> nil) then
  begin
    Tx := 0; Ty := 0; Tz := 0; TLength := 0;
    Rx := 0; Ry := 0; Rz := 0; RAngle := 0;

    for RightSide := Low(Boolean) to High(Boolean) do
      if FControl[RightSide] <> nil then
      begin
        FControl[RightSide].GetSensorTranslation(Tx, Ty, Tz, TLength);
        FControl[RightSide].GetSensorRotation(Rx, Ry, Rz, RAngle);
      end;

    {$warnings off} // ignore using deprecated Navigation, TODO: we should fix the way we pass 3D mouse events
    if (Viewport <> nil) and
       (Viewport.Navigation <> nil) then
    begin
      Viewport.Navigation.SensorTranslation(Tx, Ty, Tz, TLength, SecondsPassed);
      Viewport.Navigation.SensorRotation(Rx, Ry, Rz, RAngle, SecondsPassed);
    end;
    {$warnings on}
  end;

  {$warnings off} // ignore using deprecated NavigationType
  if (Viewport <> nil) and
     (FLastSeenNavigationType <> TCastleAutoNavigationViewport.InternalGetNavigationType(Viewport)) then
  begin
    UpdateAutoTouchInterface; // will update FLastSeenNavigationType
  end;
  {$warnings on}
end;

procedure TCastleTouchNavigation.SetTouchInterface(const Value: TTouchInterface);
begin
  if FTouchInterface <> Value then
  begin
    FTouchInterface := Value;
    UpdateTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.UpdateTouchInterface;

  procedure UpdateTouchController(
    const RightSide, CtlVisible: Boolean; const Mode: TCastleTouchCtlMode);
  {$warnings off} // TCastleTouchControl should be internal here
  var
    NewControl: TCastleTouchControl;
  {$warnings on}
  begin
    if FControl[RightSide] <> nil then
    begin
      if CtlVisible then
        FControl[RightSide].TouchMode := Mode
      else
        FreeAndNil(FControl[RightSide]); // this automatically removes FControl[RightSide] from Controls list
    end else
    if CtlVisible then
    begin
      {$warnings off} // TCastleTouchControl should be internal here
      NewControl := TCastleTouchControl.Create(Self);
      {$warnings on}
      NewControl.Scale := Scale;
      NewControl.SetTransient;
      NewControl.TouchMode := Mode;
      if not RightSide then
        NewControl.Position := tpLeft
      else
        NewControl.Position := tpRight;
      InsertFront(NewControl);
      FControl[RightSide] := NewControl;
    end;
  end;

  procedure UpdateTouchControllers(
    const MouseDragMode: TMouseDragMode;
    const LeftVisible, RightVisible: Boolean;
    const LeftMode: TCastleTouchCtlMode = ctcmWalking;
    const RightMode: TCastleTouchCtlMode = ctcmWalking);
  begin
    UpdateTouchController(false, LeftVisible , LeftMode);
    UpdateTouchController(true , RightVisible, RightMode);
    {$warnings off} // TODO: using deprecated for now - this needs to know a "central" navigation
    if ControlMouseDragMode and
       (Viewport <> nil) and
       (Viewport.Navigation is TCastleWalkNavigation) then
      (Viewport.Navigation as TCastleWalkNavigation).MouseDragMode := MouseDragMode;
    {$warnings on}
  end;

begin
  case TouchInterface of
    tiNone:
      UpdateTouchControllers(mdWalk, false, false);
    tiWalkRotate:
      UpdateTouchControllers(mdNone, true, true, ctcmWalking, ctcmHeadRotation);
    tiWalk:
      UpdateTouchControllers(mdRotate, false, true, ctcmWalking, ctcmWalking);
    tiFlyWalk:
      UpdateTouchControllers(mdRotate, true, true, ctcmFlyUpdown, ctcmWalking);
    tiPan:
      UpdateTouchControllers(mdRotate, false, true, ctcmPanXY, ctcmPanXY);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TouchInterface?');
    {$endif}
  end;
end;

function TCastleTouchNavigation.TouchInterfaceStored: Boolean;
begin
  Result := not AutoTouchInterface;
end;

procedure TCastleTouchNavigation.UpdateAutoTouchInterface;
begin
  if (Viewport <> nil) and AutoTouchInterface then
  begin
    {$warnings off} // ignore using deprecated NavigationType
    FLastSeenNavigationType := TCastleAutoNavigationViewport.InternalGetNavigationType(Viewport);
    {$warnings on}
    case FLastSeenNavigationType of
      ntNone:      TouchInterface := tiNone;
      ntWalk:      TouchInterface := FAutoWalkTouchInterface;
      ntFly:       TouchInterface := tiFlyWalk;
      ntExamine:   TouchInterface := FAutoExamineTouchInterface;
      ntTurntable: TouchInterface := FAutoExamineTouchInterface;
      // nt2D: TODO
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTouchNavigation.UpdateAutoTouchInterface not implemented for this NavigationType value');
      {$endif}
    end;
  end;
end;

procedure TCastleTouchNavigation.SetAutoTouchInterface(const Value: Boolean);
begin
  if FAutoTouchInterface <> Value then
  begin
    FAutoTouchInterface := Value;
    { change TouchInterface immediately, in case we just set
      AutoTouchInterface := true }
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetAutoWalkTouchInterface(const Value: TTouchInterface);
begin
  if FAutoWalkTouchInterface <> Value then
  begin
    FAutoWalkTouchInterface := Value;
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetAutoExamineTouchInterface(const Value: TTouchInterface);
begin
  if FAutoExamineTouchInterface <> Value then
  begin
    FAutoExamineTouchInterface := Value;
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetViewport(const Value: TCastleViewport);
begin
  if FViewport <> Value then
  begin
    FViewport := Value;
    UpdateAutoTouchInterface;
  end;
end;

procedure TCastleTouchNavigation.SetControlMouseDragMode(const Value: Boolean);
begin
  if FControlMouseDragMode <> Value then
  begin
    FControlMouseDragMode := Value;
    UpdateTouchInterface;
  end;
end;

function TCastleTouchNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Viewport') or
     (PropertyName = 'TouchInterface') or
     (PropertyName = 'AutoTouchInterface') or
     (PropertyName = 'Scale') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleTouchNavigation.SetScale(const Value: Single);
var
  RightSide: Boolean;
begin
  if FScale <> Value then
  begin
    FScale := Value;
    for RightSide := Low(Boolean) to High(Boolean) do
      if FControl[RightSide] <> nil then
        FControl[RightSide].Scale := FScale;
  end;
end;

{$endif read_implementation}
