{
  Copyright 2013-2013 Michalis Kamburelis.

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

uses CastleWindow, CastleControls;

type
  TTouchCtlInterface = (etciNone, etciCtlWalkCtlRotate, etciCtlWalkDragRotate);

  TCastleWindowTouch = class(TCastleWindow)
  private
    FDpi: Integer;
    LeftTouchCtl, RightTouchCtl: TCastleTouchControl;
    procedure UpdateTouchController(LeftSide, CtlVisible: boolean;
      Mode: TCastleTouchCtlMode = ctcmWalking);
    procedure UpdateTouchPositions;
  public
    { Called when the navigation type changes. }
    procedure TouchInterface(Mode: TTouchCtlInterface; Dpi: integer);
    procedure EventUpdate; override;
    procedure EventResize; override;
  end;

implementation

uses SysUtils, CastleUIControls, CastleCameras;

procedure TCastleWindowTouch.EventUpdate;
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
      LeftTouchCtl.GetTranslationValues(Tx, Ty, Tz, TLength);
      LeftTouchCtl.GetRotationValues(Rx, Ry, Rz, RAngle);
    end;

    if RightTouchCtl <> nil then
    begin
      RightTouchCtl.GetTranslationValues(Tx, Ty, Tz, TLength);
      RightTouchCtl.GetRotationValues(Rx, Ry, Rz, RAngle);
    end;

    { send to all 2D controls, including viewports }
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
      begin
        C.Mouse3dTranslation(Tx, Ty, Tz, TLength, Fps.UpdateSecondsPassed);
        C.Mouse3dRotation(Rx, Ry, Rz, RAngle, Fps.UpdateSecondsPassed);
      end;
    end;
  end;
end;

procedure TCastleWindowTouch.UpdateTouchPositions;
var
  CtlBorder: Integer;
begin
  CtlBorder := Round(24*FDpi/96);
  if LeftTouchCtl <> nil then
  begin
    LeftTouchCtl.Left := CtlBorder;
    LeftTouchCtl.Bottom := CtlBorder;
  end;
  if RightTouchCtl <> nil then
  begin
    RightTouchCtl.Left := Width - RightTouchCtl.Width - CtlBorder;
    RightTouchCtl.Bottom := CtlBorder;
  end;
end;

procedure TCastleWindowTouch.EventResize;
begin
  inherited;
  UpdateTouchPositions;
end;

procedure TCastleWindowTouch.UpdateTouchController(LeftSide, CtlVisible: boolean; Mode: TCastleTouchCtlMode);
var
  aNewCtl: TCastleTouchControl;
begin
  // left controller
  if LeftSide and (LeftTouchCtl<>nil) then
  begin
    if CtlVisible then
      LeftTouchCtl.TouchMode := Mode
    else begin
      Controls.Remove(LeftTouchCtl);
      FreeAndNil(LeftTouchCtl);
    end;
    Exit;
  end;

  // right controller
  if (not LeftSide) and (RightTouchCtl<>nil) then
  begin
    if CtlVisible then
      RightTouchCtl.TouchMode := Mode
    else begin
      Controls.Remove(RightTouchCtl);
      FreeAndNil(RightTouchCtl);
    end;
    Exit;
  end;

  if not CtlVisible then Exit;

  aNewCtl := TCastleTouchControl.Create(self);
  aNewCtl.TouchMode := Mode;
  aNewCtl.SizeScale := FDpi / 96;
  Controls.InsertFront(aNewCtl);
  if LeftSide then
    LeftTouchCtl := aNewCtl
  else
    RightTouchCtl := aNewCtl;
  UpdateTouchPositions;
end;

procedure TCastleWindowTouch.TouchInterface(Mode: TTouchCtlInterface; Dpi: integer);
var
  WalkCamera: TWalkCamera;
begin
  FDpi := Dpi;

  if SceneManager.Camera <> nil then
  begin
    if SceneManager.Camera is TUniversalCamera then
      WalkCamera := (SceneManager.Camera as TUniversalCamera).Walk else
    if SceneManager.Camera is TWalkCamera then
      WalkCamera := SceneManager.Camera as TWalkCamera else
      WalkCamera := nil;
  end;

  if (Mode = etciNone) or (WalkCamera = nil) then
  begin
    UpdateTouchController(true, false);
    UpdateTouchController(false, false);
    if WalkCamera <> nil then
      WalkCamera.MouseDragMode := cwdmDragToWalk;
  end else
  if Mode = etciCtlWalkCtlRotate then
  begin
    UpdateTouchController(true, true, ctcmWalking);
    UpdateTouchController(false, true, ctcmHeadRotation);
    WalkCamera.MouseDragMode := cwdmNone;
  end else
  if Mode = etciCtlWalkDragRotate then
  begin
    UpdateTouchController(true, false);
    UpdateTouchController(false, true, ctcmWalking);
    WalkCamera.MouseDragMode := cwdmDragToRotate;
  end;
  UpdateTouchPositions;
end;

end.
