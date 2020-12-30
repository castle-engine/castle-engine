{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Window with controls for easy navigation on touch interfaces. }
unit CastleWindowTouch;

{$I castleconf.inc}

interface

uses Classes, CastleWindow, CastleControls, CastleCameras,
  CastleUIControls, CastleViewport;

type
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
    The size and position of this control determines where they appear.

    The @link(Viewport) determines the viewport where navigation is affected
    by this control. In case of @link(AutoTouchInterface), we also look
    at @link(Viewport) value of @link(TCastleViewport.NavigationType)
    to determine the best navigation automatically.
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
  public
    const
      DefaultAutoWalkTouchInterface = tiWalk;
      DefaultAutoExamineTouchInterface = tiPan;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  published
    { Viewport where navigation is affected by this control.
      In case of @link(AutoTouchInterface), we also look
      at @link(Viewport) value of @link(TCastleViewport.NavigationType)
      to determine the best navigation automatically.

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
      touch controls) based on the current @link(Viewport)
      navigation type in @link(TCastleViewport.NavigationType). }
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
  end;

  { Full-featured window for rendering (see @link(TCastleWindow))
    with optional touch controls, to provide a 3D navigation comfortable
    on touch devices (phones, tablets and such).

    In addition to all the goodies of the @link(TCastleWindow) functionality,
    this class can additionally manage one or two TCastleTouchControl instances.
    They will be automatically positioned in the bottom-left
    and bottom-right corners of the screen,
    and will allow the user to navigate using the default @link(TCastleViewport.Navigation SceneManager.Navigation).
    In the simplest case, just set @link(AutomaticTouchInterface) to @true,
    and the touch controls will automatically adjust to the current
    navigation type of the camera (examine, walk, fly...).

    @deprecated Instead of using this automatic composition, it is trivial
    (and much more flexible) to use TCastleWindowBase,
    add there TCastleViewport and TCastleTouchNavigation manually.
  }
  TCastleWindowTouch = class(TCastleWindow)
  strict private
    FTouchNavigation: TCastleTouchNavigation;
    function GetTouchInterface: TTouchInterface;
    function GetAutomaticTouchInterface: Boolean;
    function GetAutomaticWalkTouchCtl: TTouchInterface;
    function GetAutomaticExamineTouchCtl: TTouchInterface;
    procedure SetTouchInterface(const Value: TTouchInterface);
    procedure SetAutomaticTouchInterface(const Value: Boolean);
    procedure SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
    procedure SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
  public
    const
      DefaultAutomaticWalkTouchCtl = TCastleTouchNavigation.DefaultAutoWalkTouchInterface;
      DefaultAutomaticExamineTouchCtl = TCastleTouchNavigation.DefaultAutoExamineTouchInterface;
    constructor Create(AOwner: TComponent); override;
    property TouchInterface: TTouchInterface
      read GetTouchInterface write SetTouchInterface default tiNone;
    property AutomaticTouchInterface: Boolean
      read GetAutomaticTouchInterface write SetAutomaticTouchInterface
      default false;
    property AutomaticWalkTouchCtl: TTouchInterface
      read GetAutomaticWalkTouchCtl write SetAutomaticWalkTouchCtl
      default DefaultAutomaticWalkTouchCtl;
    property AutomaticExamineTouchCtl: TTouchInterface
      read GetAutomaticExamineTouchCtl write SetAutomaticExamineTouchCtl
      default DefaultAutomaticExamineTouchCtl;
  end deprecated 'use TCastleWindowBase and add TCastleTouchNavigation (and maybe set ControlMouseDragMode=true)';

const
  etciNone = tiNone deprecated;
  etciCtlWalkCtlRotate = tiWalkRotate deprecated;
  etciCtlWalkDragRotate = tiWalk deprecated;
  etciCtlFlyCtlWalkDragRotate =  tiFlyWalk deprecated;
  etciCtlPanXYDragRotate = tiPan deprecated;

  tiCtlWalkCtlRotate = tiWalk deprecated;
  tiCtlWalkDragRotate = tiWalkRotate deprecated;
  tiCtlFlyCtlWalkDragRotate = tiFlyWalk deprecated;
  tiCtlPanXYDragRotate = tiPan deprecated;

implementation

uses SysUtils, CastleUtils;

{ TCastleTouchNavigation ----------------------------------------------------- }

constructor TCastleTouchNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAutoWalkTouchInterface := DefaultAutoWalkTouchInterface;
  FAutoExamineTouchInterface := DefaultAutoExamineTouchInterface;
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

    for RightSide in Boolean do
      if FControl[RightSide] <> nil then
      begin
        FControl[RightSide].GetSensorTranslation(Tx, Ty, Tz, TLength);
        FControl[RightSide].GetSensorRotation(Rx, Ry, Rz, RAngle);
      end;

    if (Viewport <> nil) and
       (Viewport.Navigation <> nil) then
    begin
      Viewport.Navigation.SensorTranslation(Tx, Ty, Tz, TLength, SecondsPassed);
      Viewport.Navigation.SensorRotation(Rx, Ry, Rz, RAngle, SecondsPassed);
    end;
  end;

  if (Viewport <> nil) and
     (FLastSeenNavigationType <> Viewport.NavigationType) then
  begin
    FLastSeenNavigationType := Viewport.NavigationType;
    UpdateAutoTouchInterface;
  end;
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
    if ControlMouseDragMode and
      (Viewport <> nil) and
      (Viewport.Navigation is TCastleWalkNavigation) then
      (Viewport.Navigation as TCastleWalkNavigation).MouseDragMode := MouseDragMode;
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
    case Viewport.NavigationType of
      ntNone:      TouchInterface := tiNone;
      ntWalk:      TouchInterface := FAutoWalkTouchInterface;
      ntFly:       TouchInterface := tiFlyWalk;
      ntExamine:   TouchInterface := FAutoExamineTouchInterface;
      ntTurntable: TouchInterface := FAutoExamineTouchInterface;
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

{ TCastleWindowTouch --------------------------------------------------------- }

constructor TCastleWindowTouch.Create(AOwner: TComponent);
begin
  inherited;
  FTouchNavigation := TCastleTouchNavigation.Create(Self);
  FTouchNavigation.Viewport := SceneManager;
  FTouchNavigation.ControlMouseDragMode := true; // for backward compat
  FTouchNavigation.FullSize := true;
  SceneManager.InsertFront(FTouchNavigation);
end;

function TCastleWindowTouch.GetTouchInterface: TTouchInterface;
begin
  Result := FTouchNavigation.TouchInterface;
end;

function TCastleWindowTouch.GetAutomaticTouchInterface: Boolean;
begin
  Result := FTouchNavigation.AutoTouchInterface;
end;

function TCastleWindowTouch.GetAutomaticWalkTouchCtl: TTouchInterface;
begin
  Result := FTouchNavigation.AutoWalkTouchInterface;
end;

function TCastleWindowTouch.GetAutomaticExamineTouchCtl: TTouchInterface;
begin
  Result := FTouchNavigation.AutoExamineTouchInterface;
end;

procedure TCastleWindowTouch.SetTouchInterface(const Value: TTouchInterface);
begin
  FTouchNavigation.TouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticTouchInterface(const Value: Boolean);
begin
  FTouchNavigation.AutoTouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
begin
  FTouchNavigation.AutoWalkTouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
begin
  FTouchNavigation.AutoExamineTouchInterface := Value;
end;

end.
