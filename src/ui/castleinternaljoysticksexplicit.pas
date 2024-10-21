{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Joystick backend getting notified about joystick state by external API. }
unit CastleInternalJoysticksExplicit;

interface

uses CastleJoysticks, CastleVectors;

type
  TExplicitJoystickBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
    procedure SetJoystickCount(const List: TJoystickList; const NewJoystickCount: Integer);
    procedure SetJoystickAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2); deprecated 'use SetJoystickLeftAxis';
    procedure SetJoystickLeftAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2);
    procedure SetJoystickRightAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2);
  end;

implementation

uses CastleUtils, CastleLog;

procedure TExplicitJoystickBackend.Initialize(const List: TJoystickList);
begin
end;

procedure TExplicitJoystickBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
begin
end;

procedure TExplicitJoystickBackend.SetJoystickCount(const List: TJoystickList; const NewJoystickCount: Integer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to NewJoystickCount - 1 do
    List.Add(TJoystick.Create);
end;

procedure TExplicitJoystickBackend.SetJoystickAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2);
begin
  SetJoystickLeftAxis(List, JoystickIndex, Axis);
end;

procedure TExplicitJoystickBackend.SetJoystickLeftAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2);
var
  Joystick: TJoystick;
begin
  if Between(JoystickIndex, 0, List.Count - 1) then
  begin
    Joystick := List[JoystickIndex];
    Joystick.State.Axis[JOY_AXIS_X] := Axis.X;
    Joystick.State.Axis[JOY_AXIS_Y] := Axis.Y;
  end else
    WriteLnWarning('Joystick index %d given to CGEApp_JoystickAxis is incorrect. Current joystick count (given to CGEApp_JoystickCount) is %d.', [
      JoystickIndex,
      List.Count
    ]);
end;

procedure TExplicitJoystickBackend.SetJoystickRightAxis(const List: TJoystickList; const JoystickIndex: Integer; const Axis: TVector2);
var
  Joystick: TJoystick;
begin
  if Between(JoystickIndex, 0, List.Count - 1) then
  begin
    Joystick := List[JoystickIndex];
    Joystick.State.Axis[JOY_AXIS_U] := Axis.X;
    Joystick.State.Axis[JOY_AXIS_R] := -Axis.Y;
  end else
    WriteLnWarning('Joystick index %d given to CGEApp_JoystickAxis is incorrect. Current joystick count (given to CGEApp_JoystickCount) is %d.', [
      JoystickIndex,
      List.Count
    ]);
end;

end.
