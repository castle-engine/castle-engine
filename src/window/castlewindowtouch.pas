{
  Copyright 2013-2014 Michalis Kamburelis.

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

interface

uses Classes, CastleWindow, CastleControls, CastleCameras;

type
  TTouchCtlInterface = (etciNone, etciCtlWalkCtlRotate, etciCtlWalkDragRotate,
                        etciCtlFlyCtlWalkDragRotate, etciCtlPanXYDragRotate);
  TCastleWindowTouch = class(TCastleWindow)
  private
    FAutomaticTouchInterface: boolean;
    LeftTouchCtl, RightTouchCtl: TCastleTouchControl;
    FTouchInterface: TTouchCtlInterface;
    procedure UpdateTouchController(const LeftSide, CtlVisible: boolean;
      const Mode: TCastleTouchCtlMode = ctcmWalking);
    procedure SetTouchInterface(const Value: TTouchCtlInterface);
    procedure SetAutomaticTouchInterface(const Value: boolean);
    { Sets touch controls depending on the current navigation mode.
      Should be called each time after navigation mode changed. }
    procedure UpdateAutomaticTouchInterface;
  protected
    procedure NavigationInfoChanged; override;
    procedure DoUpdate; override;
  public
    { Configure touch controls to be displayed on the window.
      This automatically manages under the hood 0, 1 or 2
      TCastleTouchControl instances, placing them at suitable positions
      and handling their operations.

      Note that you can set AutomaticTouchInterface = @true to have this property
      automatically adjusted. (In which case you should not set this directly.) }
    property TouchInterface: TTouchCtlInterface
      read FTouchInterface write SetTouchInterface;
  published
    { Automatically adjust TouchInterface (showing / hiding proper
      touch controls) based on the current navigation type.
      The navigation type is obtained from the camera of the default viewport,
      see TCastleWindow.NavigationType. }
    property AutomaticTouchInterface: boolean
      read FAutomaticTouchInterface write SetAutomaticTouchInterface
      default false;
  end;

implementation

uses SysUtils, CastleUIControls, CastleUtils;

procedure TCastleWindowTouch.DoUpdate;
var
  I: Integer;
  C: TUIControl;
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
begin
  inherited;

  if (LeftTouchCtl<>nil) or (RightTouchCtl<>nil) then
  begin
    Tx := 0; Ty := 0; Tz := 0; TLength := 0;
    Rx := 0; Ry := 0; Rz := 0; RAngle := 0;

    if LeftTouchCtl <> nil then
    begin
      LeftTouchCtl.GetSensorTranslation(Tx, Ty, Tz, TLength);
      LeftTouchCtl.GetSensorRotation(Rx, Ry, Rz, RAngle);
    end;

    if RightTouchCtl <> nil then
    begin
      RightTouchCtl.GetSensorTranslation(Tx, Ty, Tz, TLength);
      RightTouchCtl.GetSensorRotation(Rx, Ry, Rz, RAngle);
    end;

    { send to all 2D controls, including viewports }
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
      begin
        C.SensorTranslation(Tx, Ty, Tz, TLength, Fps.UpdateSecondsPassed);
        C.SensorRotation(Rx, Ry, Rz, RAngle, Fps.UpdateSecondsPassed);
      end;
    end;
  end;
end;

procedure TCastleWindowTouch.UpdateTouchController(
  const LeftSide, CtlVisible: boolean; const Mode: TCastleTouchCtlMode);
var
  aNewCtl: TCastleTouchControl;
begin
  // left controller
  if LeftSide and (LeftTouchCtl<>nil) then
  begin
    if CtlVisible then
      LeftTouchCtl.TouchMode := Mode else
      FreeAndNil(LeftTouchCtl); // this automatically removes LeftTouchCtl from Controls list
    Exit;
  end;

  // right controller
  if (not LeftSide) and (RightTouchCtl<>nil) then
  begin
    if CtlVisible then
      RightTouchCtl.TouchMode := Mode else
      FreeAndNil(RightTouchCtl); // this automatically removes RightTouchCtl from Controls list
    Exit;
  end;

  if not CtlVisible then Exit;

  aNewCtl := TCastleTouchControl.Create(self);
  aNewCtl.TouchMode := Mode;
  if LeftSide then
    aNewCtl.Position := tpLeft else
    aNewCtl.Position := tpRight;
  Controls.InsertFront(aNewCtl);
  if LeftSide then
    LeftTouchCtl := aNewCtl else
    RightTouchCtl := aNewCtl;
end;

procedure TCastleWindowTouch.SetTouchInterface(const Value: TTouchCtlInterface);
var
  WalkCamera: TWalkCamera;
begin
  if FTouchInterface <> Value then
  begin
    FTouchInterface := Value;

    if SceneManager.Camera <> nil then
    begin
      if SceneManager.Camera is TUniversalCamera then
        WalkCamera := (SceneManager.Camera as TUniversalCamera).Walk else
      if SceneManager.Camera is TWalkCamera then
        WalkCamera := SceneManager.Camera as TWalkCamera else
        WalkCamera := nil;
    end;

    if Value = etciCtlWalkCtlRotate then
    begin
      UpdateTouchController(true, true, ctcmWalking);
      UpdateTouchController(false, true, ctcmHeadRotation);
      if WalkCamera<>nil then
        WalkCamera.MouseDragMode := cwdmNone;
    end else
    if Value = etciCtlWalkDragRotate then
    begin
      UpdateTouchController(true, false);
      UpdateTouchController(false, true, ctcmWalking);
      if WalkCamera<>nil then
        WalkCamera.MouseDragMode := cwdmDragToRotate;
    end else
    if Value = etciCtlFlyCtlWalkDragRotate then
    begin
      UpdateTouchController(true, true, ctcmFlyUpdown);
      UpdateTouchController(false, true, ctcmWalking);
      if WalkCamera<>nil then
        WalkCamera.MouseDragMode := cwdmDragToRotate;
    end else
    if Value = etciCtlPanXYDragRotate then
    begin
      UpdateTouchController(true, false);
      UpdateTouchController(false, true, ctcmPanXY);
      if WalkCamera<>nil then
        WalkCamera.MouseDragMode := cwdmDragToRotate;
    end else
    begin
      UpdateTouchController(true, false);
      UpdateTouchController(false, false);
      if WalkCamera <> nil then
        WalkCamera.MouseDragMode := cwdmDragToWalk;
    end;
  end;
end;

procedure TCastleWindowTouch.UpdateAutomaticTouchInterface;
begin
  if AutomaticTouchInterface then
  begin
    case NavigationType of
      ntNone:      TouchInterface := etciNone;
      ntWalk:      TouchInterface := etciCtlWalkDragRotate;
      ntFly:       TouchInterface := etciCtlFlyCtlWalkDragRotate;
      ntExamine:   TouchInterface := etciCtlPanXYDragRotate;
      ntTurntable: TouchInterface := etciCtlPanXYDragRotate;
      else raise EInternalError.Create('TCastleWindowTouch.UpdateAutomaticTouchInterface not implemented for this NavigationType value');
    end;
  end;
end;

procedure TCastleWindowTouch.SetAutomaticTouchInterface(const Value: boolean);
begin
  if FAutomaticTouchInterface <> Value then
  begin
    FAutomaticTouchInterface := Value;
    { change NavigationType immediately, in case we just set
      AutomaticTouchInterface := true }
    UpdateAutomaticTouchInterface;
  end;
end;

procedure TCastleWindowTouch.NavigationInfoChanged;
begin
  inherited;
  UpdateAutomaticTouchInterface;
end;

end.
