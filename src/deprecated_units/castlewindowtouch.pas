{
  Copyright 2013-2018 Michalis Kamburelis.

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

uses Classes, CastleWindow, CastleControls, CastleCameras;

type
  TTouchInterface = (
    tiNone,
    tiCtlWalkCtlRotate,
    tiCtlWalkDragRotate,
    tiCtlFlyCtlWalkDragRotate,
    tiCtlPanXYDragRotate);

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
    navigation type of the camera (examine, walk, fly...). }
  TCastleWindowTouch = class(TCastleWindow)
  private
    FAutomaticTouchInterface: boolean;
    FControl: array [boolean { right side? }] of TCastleTouchControl;
    FTouchInterface: TTouchInterface;
    FAutomaticWalkTouchCtl, FAutomaticExamineTouchCtl: TTouchInterface;
    procedure SetTouchInterface(const Value: TTouchInterface);
    procedure SetAutomaticTouchInterface(const Value: boolean);
    procedure SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
    procedure SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
    { Sets touch controls depending on the current navigation mode.
      Should be called each time after navigation mode changed. }
    procedure UpdateAutomaticTouchInterface;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure NavigationInfoChanged; override;
    procedure DoUpdate; override;
  public
    const
      DefaultAutomaticWalkTouchCtl = tiCtlWalkDragRotate;
      DefaultAutomaticExamineTouchCtl = tiCtlPanXYDragRotate;

    { Configure touch controls to be displayed on the window.
      This automatically manages under the hood 0, 1 or 2
      TCastleTouchControl instances, placing them at suitable positions
      and handling their operations.

      Note that you can set AutomaticTouchInterface = @true to have this property
      automatically adjusted. (In which case you should not set this directly.) }
    property TouchInterface: TTouchInterface
      read FTouchInterface write SetTouchInterface;
  published
    { Automatically adjust TouchInterface (showing / hiding proper
      touch controls) based on the current navigation type.
      The navigation type is obtained from the camera of the default viewport,
      see TCastleWindow.NavigationType. }
    property AutomaticTouchInterface: boolean
      read FAutomaticTouchInterface write SetAutomaticTouchInterface
      default false;
    { When using AutomaticTouchInterface = @true,
      which touch interface should be used when walking
      (since there are multiple sensible choices).
      Select between tiCtlWalkCtlRotate or tiCtlWalkDragRotate (default).}
    property AutomaticWalkTouchCtl: TTouchInterface
      read FAutomaticWalkTouchCtl write SetAutomaticWalkTouchCtl
      default DefaultAutomaticWalkTouchCtl;
    { When using AutomaticTouchInterface = @true,
      which touch interface should be used in examine camera
      (since examine camera can use multi-touch gesture instead).
      Select between tiCtlPanXYDragRotate (default) or tiNone.}
    property AutomaticExamineTouchCtl: TTouchInterface
      read FAutomaticExamineTouchCtl write SetAutomaticExamineTouchCtl
      default DefaultAutomaticExamineTouchCtl;
  end deprecated 'use TCastleWindowBase and manually configure TCastleTouchControl; TODO: simpler approach to it, that allows to insert TCastleTouchNavigation to TCastleViewport, will be done';

const
  etciNone = tiNone deprecated;
  etciCtlWalkCtlRotate = tiCtlWalkCtlRotate deprecated;
  etciCtlWalkDragRotate = tiCtlWalkDragRotate deprecated;
  etciCtlFlyCtlWalkDragRotate =  tiCtlFlyCtlWalkDragRotate deprecated;
  etciCtlPanXYDragRotate = tiCtlPanXYDragRotate deprecated;

implementation

uses SysUtils, CastleUIControls, CastleUtils;

constructor TCastleWindowTouch.Create(AOwner: TComponent);
begin
  inherited;
  FAutomaticWalkTouchCtl := DefaultAutomaticWalkTouchCtl;
  FAutomaticExamineTouchCtl := DefaultAutomaticExamineTouchCtl;
end;

procedure TCastleWindowTouch.DoUpdate;
var
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  RightSide: boolean;
begin
  inherited;

  if (FControl[false] <> nil) or
     (FControl[true] <> nil) then
  begin
    Tx := 0; Ty := 0; Tz := 0; TLength := 0;
    Rx := 0; Ry := 0; Rz := 0; RAngle := 0;

    for RightSide in boolean do
      if FControl[RightSide] <> nil then
      begin
        FControl[RightSide].GetSensorTranslation(Tx, Ty, Tz, TLength);
        FControl[RightSide].GetSensorRotation(Rx, Ry, Rz, RAngle);
      end;

    if SceneManager.Navigation <> nil then
    begin
      SceneManager.Navigation.SensorTranslation(Tx, Ty, Tz, TLength, Fps.SecondsPassed);
      SceneManager.Navigation.SensorRotation(Rx, Ry, Rz, RAngle, Fps.SecondsPassed);
    end;
  end;
end;

procedure TCastleWindowTouch.SetTouchInterface(const Value: TTouchInterface);

  procedure UpdateTouchController(
    const RightSide, CtlVisible: boolean; const Mode: TCastleTouchCtlMode);
  var
    NewControl: TCastleTouchControl;
  begin
    if FControl[RightSide] <> nil then
    begin
      if CtlVisible then
        FControl[RightSide].TouchMode := Mode else
        FreeAndNil(FControl[RightSide]); // this automatically removes FControl[RightSide] from Controls list
    end else
    if CtlVisible then
    begin
      NewControl := TCastleTouchControl.Create(self);
      NewControl.TouchMode := Mode;
      if not RightSide then
        NewControl.Position := tpLeft else
        NewControl.Position := tpRight;
      Controls.InsertFront(NewControl);
      FControl[RightSide] := NewControl;
    end;
  end;

  procedure UpdateTouchControllers(
    const MouseDragMode: TMouseDragMode;
    const LeftVisible, RightVisible: boolean;
    const LeftMode: TCastleTouchCtlMode = ctcmWalking;
    const RightMode: TCastleTouchCtlMode = ctcmWalking);
  begin
    UpdateTouchController(false, LeftVisible , LeftMode);
    UpdateTouchController(true , RightVisible, RightMode);
    if SceneManager.Navigation is TCastleWalkNavigation then
      (SceneManager.Navigation as TCastleWalkNavigation).MouseDragMode := MouseDragMode;
  end;

begin
  if FTouchInterface <> Value then
  begin
    FTouchInterface := Value;

    case Value of
      tiNone:
        UpdateTouchControllers(mdWalk, false, false);
      tiCtlWalkCtlRotate:
        UpdateTouchControllers(mdNone, true, true, ctcmWalking, ctcmHeadRotation);
      tiCtlWalkDragRotate:
        UpdateTouchControllers(mdRotate, false, true, ctcmWalking, ctcmWalking);
      tiCtlFlyCtlWalkDragRotate:
        UpdateTouchControllers(mdRotate, true, true, ctcmFlyUpdown, ctcmWalking);
      tiCtlPanXYDragRotate:
        UpdateTouchControllers(mdRotate, false, true, ctcmPanXY, ctcmPanXY);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Value unhandled in SetTouchInterface');
      {$endif}
    end;
  end;
end;

procedure TCastleWindowTouch.UpdateAutomaticTouchInterface;
begin
  if AutomaticTouchInterface then
  begin
    case SceneManager.NavigationType of
      ntNone:      TouchInterface := tiNone;
      ntWalk:      TouchInterface := FAutomaticWalkTouchCtl;
      ntFly:       TouchInterface := tiCtlFlyCtlWalkDragRotate;
      ntExamine:   TouchInterface := FAutomaticExamineTouchCtl;
      ntTurntable: TouchInterface := FAutomaticExamineTouchCtl;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleWindowTouch.UpdateAutomaticTouchInterface not implemented for this NavigationType value');
      {$endif}
    end;
  end;
end;

procedure TCastleWindowTouch.SetAutomaticTouchInterface(const Value: boolean);
begin
  if FAutomaticTouchInterface <> Value then
  begin
    FAutomaticTouchInterface := Value;
    { change TouchInterface immediately, in case we just set
      AutomaticTouchInterface := true }
    UpdateAutomaticTouchInterface;
  end;
end;

procedure TCastleWindowTouch.SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
begin
  if FAutomaticWalkTouchCtl <> Value then
  begin
    FAutomaticWalkTouchCtl := Value;
    UpdateAutomaticTouchInterface;
  end;
end;

procedure TCastleWindowTouch.SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
begin
  if FAutomaticExamineTouchCtl <> Value then
  begin
    FAutomaticExamineTouchCtl := Value;
    UpdateAutomaticTouchInterface;
  end;
end;

procedure TCastleWindowTouch.NavigationInfoChanged;
begin
  inherited;
  UpdateAutomaticTouchInterface;
end;

end.
