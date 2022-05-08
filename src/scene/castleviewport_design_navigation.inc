{%MainUnit castleviewport.pas}
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Design-time navigation descendants. }

{$ifdef read_interface}

var
  { @exclude }
  InternalDesignMouseLook: Boolean;

{$endif read_interface}

{$ifdef read_implementation}

type
  { Descendant of TCastleWalkNavigation configured to work nicely for design-time
    navigation in walk/fly mode. }
  TCastleWalkNavigationDesign = class(TCastleWalkNavigation)
  strict private
    LabelMessage: TCastleLabel;
    LastMoveSpeed: Single;
    MessageStart: TTimerResult;
  public
    constructor Create(AOwner: TComponent); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

constructor TCastleWalkNavigationDesign.Create(AOwner: TComponent);
begin
  inherited;
  Input := [niNormal, ni3dMouse]; // no niMouseDragging, no niGesture

  Gravity := false;

  // use mouse wheel, as alterntive to +/-, to increase/decrease movement speed
  Input_MoveSpeedInc.Assign(keyNumpadPlus , keyNone, '+', false, buttonLeft, mwUp);
  Input_MoveSpeedDec.Assign(keyNumpadMinus, keyNone, '-', false, buttonLeft, mwDown);
  Input_Jump.Assign(keyE);
  Input_Crouch.Assign(keyQ);

  LabelMessage := TCastleLabel.Create(Self);
  LabelMessage.SetTransient;
  LabelMessage.Outline := 1;
  LabelMessage.Color := White;
  LabelMessage.Exists := false;
  LabelMessage.Anchor(hpMiddle);
  LabelMessage.Anchor(vpMiddle);
  LabelMessage.FontSize := 60;
  InsertFront(LabelMessage);

  LastMoveSpeed := MoveSpeed;
end;

function TCastleWalkNavigationDesign.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonRight) then
  begin
    MouseLook := true;
    InternalDesignMouseLook := true;
    Exit(true);
  end;

{
  TODO:
  - still wondering whether toggling this on G is nice for users
    (maybe better menu item with shortcut like Ctrl+N in editor to change design-time navigation
    would be better)
  - doesn't seem to work ok, user never falls down.
    maybe lack of OnInternalMoveAllowed is a problem?

  if Event.IsKey(keyG) then
  begin
    Gravity := not Gravity;
    Exit(true);
  end;
}
end;

function TCastleWalkNavigationDesign.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonRight) then
  begin
    MouseLook := false;
    InternalDesignMouseLook := false;
    Exit(true);
  end;
end;

procedure TCastleWalkNavigationDesign.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
const
  TimeToExist = 1.0;
  TimeToFade = 0.75;
  MessageAlpha = 0.75;
var
  T: TFloatTime;
  Alpha: Single;
begin
  inherited;

  if LastMoveSpeed <> MoveSpeed then
  begin
    LastMoveSpeed := MoveSpeed;
    LabelMessage.Caption := Format('Speed: %f', [MoveSpeed]);
    MessageStart := Timer;
  end;

  if MessageStart.Initialized then
  begin
    T := MessageStart.ElapsedTime;
    LabelMessage.Exists := T < TimeToExist;
    if LabelMessage.Exists then
    begin
      if T < TimeToExist - TimeToFade then
        Alpha := MessageAlpha
      else
        Alpha := MapRange(T, TimeToExist - TimeToFade, TimeToExist, MessageAlpha, 0.0);
      LabelMessage.Color := Vector4(LabelMessage.Color.XYZ, Alpha);
      LabelMessage.OutlineColor := Vector4(LabelMessage.OutlineColor.XYZ, Alpha);
    end;
  end;
end;

{$endif read_implementation}
