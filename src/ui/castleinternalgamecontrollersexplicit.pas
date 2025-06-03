{
  Copyright 2019-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game controllers backend getting notified about controller state
  by external API. }
unit CastleInternalGameControllersExplicit;

interface

uses CastleGameControllers, CastleVectors;

type
  TExplicitGameControllerBackend = class(TGameControllersBackend)
    procedure Initialize(const List: TGameControllerList); override;
    procedure Poll(const List: TGameControllerList;
      const EventContainer: TGameControllers); override;
    procedure SetCount(const List: TGameControllerList; const NewControllerCount: Integer);
    procedure SetAxisLeft(const List: TGameControllerList; const ControllerIndex: Integer; const Axis: TVector2);
    procedure SetAxisRight(const List: TGameControllerList; const ControllerIndex: Integer; const Axis: TVector2);
  end;

implementation

uses CastleUtils, CastleLog;

procedure TExplicitGameControllerBackend.Initialize(const List: TGameControllerList);
begin
end;

procedure TExplicitGameControllerBackend.Poll(const List: TGameControllerList;
  const EventContainer: TGameControllers);
begin
end;

procedure TExplicitGameControllerBackend.SetCount(const List: TGameControllerList; const NewControllerCount: Integer);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to NewControllerCount - 1 do
    List.Add(TGameController.Create);
end;

procedure TExplicitGameControllerBackend.SetAxisLeft(const List: TGameControllerList; const ControllerIndex: Integer; const Axis: TVector2);
var
  Controller: TGameController;
begin
  if Between(ControllerIndex, 0, List.Count - 1) then
  begin
    Controller := List[ControllerIndex];
    Controller.InternalAxis[jaX] := Axis.X;
    // invert it, because TGameController.AxisLeft inverts it too
    Controller.InternalAxis[jaY] := -Axis.Y;
  end else
    WriteLnWarning('Controller index %d given to CGEApp_ControllerAxisLeft is incorrect. Current controller count (given to CGEApp_ControllerCount) is %d.', [
      ControllerIndex,
      List.Count
    ]);
end;

procedure TExplicitGameControllerBackend.SetAxisRight(const List: TGameControllerList; const ControllerIndex: Integer; const Axis: TVector2);
var
  Controller: TGameController;
begin
  if Between(ControllerIndex, 0, List.Count - 1) then
  begin
    Controller := List[ControllerIndex];
    Controller.InternalAxis[jaU] := Axis.X;
    // invert it, because TGameController.AxisRight inverts it too
    Controller.InternalAxis[jaR] := -Axis.Y;
  end else
    WriteLnWarning('Controller index %d given to CGEApp_ControllerAxisRight is incorrect. Current controller count (given to CGEApp_ControllerCount) is %d.', [
      ControllerIndex,
      List.Count
    ]);
end;

end.
