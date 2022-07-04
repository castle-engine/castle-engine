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

{ TCastleWalkNavigationDesign ------------------------------------------------ }

type
  { Descendant of TCastleWalkNavigation configured to work nicely for design-time
    navigation in walk/fly mode. }
  TCastleWalkNavigationDesign = class(TCastleWalkNavigation)
  public
    constructor Create(AOwner: TComponent); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  published
    property Input default [niNormal, ni3dMouse];
    property Gravity default false;
    property MoveSpeed {$ifdef FPC}default 10{$endif};
    property MinAngleFromGravityUp {$ifdef FPC}default 0{$endif};
    property CheckCollisions default false;

    property MouseLook stored false;
    { Also adjusted by MouseLook, and we also don't want to persist this. }
    property Cursor stored false;
  end;

constructor TCastleWalkNavigationDesign.Create(AOwner: TComponent);
begin
  inherited;
  Input := [niNormal, ni3dMouse]; // no niMouseDragging, no niGesture
  Gravity := false;
  MoveSpeed := 10; // faster default
  MinAngleFromGravityUp := 0; // allow to rotate smoothly from Top (7) view
  CheckCollisions := false;

  // use mouse wheel, as alterntive to +/-, to increase/decrease movement speed
  Input_MoveSpeedInc.Assign(keyNumpadPlus , keyNone, '+', false, buttonLeft, mwUp);
  Input_MoveSpeedDec.Assign(keyNumpadMinus, keyNone, '-', false, buttonLeft, mwDown);
  Input_Jump.Assign(keyE);
  Input_Crouch.Assign(keyQ);
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
begin
  inherited;
end;

{ TCastleExamineNavigationDesign ------------------------------------------------ }

type
  TCastleExamineNavigationDesign = class(TCastleExamineNavigation)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RotationAccelerate default false;
    property Cursor stored false;
  end;

constructor TCastleExamineNavigationDesign.Create(AOwner: TComponent);
begin
  inherited;
  RotationAccelerate := false;
  { rotate with right mouse button, no modifiers }
  Input_Rotate.MouseButton := buttonRight;
  Input_Rotate.MouseButtonCheckModifiers := [mkShift, mkCtrl];
  Input_Rotate.MouseButtonModifiers := [];
  Input_Rotate.MouseButton2Use := false;
  { move with right mouse button, Shift }
  Input_Move.MouseButton := buttonRight;
  Input_Move.MouseButtonCheckModifiers := [mkShift, mkCtrl];
  Input_Move.MouseButtonModifiers := [mkShift];
  Input_Move.MouseButton2Use := false;
  { no mouse dragging for zoom (but still you can do zoom with mouse wheel) }
  Input_Zoom.MakeClear;
end;

{ TCastle2DNavigationDesign ------------------------------------------------ }

type
  TCastle2DNavigationDesign = class(TCastle2DNavigation)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MouseButtonMove default buttonRight;

    property Cursor stored false;
  end;

constructor TCastle2DNavigationDesign.Create(AOwner: TComponent);
begin
  inherited;
  { move with right mouse button, no modifiers }
  Input_Move.MouseButton := buttonRight;
  Input_Move.MouseButtonCheckModifiers := [mkShift, mkCtrl];
  Input_Move.MouseButtonModifiers := [];
  Input_Move.MouseButton2Use := false;
  { no mouse dragging for zoom (but still you can do zoom with mouse wheel) }
  Input_Zoom.MakeClear;
end;

{$endif read_implementation}
