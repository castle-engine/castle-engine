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
  TExplicitControllerManagerBackend = class(TInternalControllerManagerBackend)
    procedure Initialize; override;
    procedure Poll; override;

    procedure SetCount(const NewControllerCount: Integer);
    procedure SetAxisLeft(const ControllerIndex: Integer; const Axis: TVector2);
    procedure SetAxisRight(const ControllerIndex: Integer; const Axis: TVector2);
  end;

implementation

uses CastleUtils, CastleLog, CastleKeysMouse;

{ TExplicitControllerBackend ------------------------------------------------- }

type
  TExplicitControllerBackend = class(TInternalControllerBackend)
    { Last set axis values.
      Explicit backend ignores the InternalAxis[...] values. }
    FAxisLeft, FAxisRight: TVector2;
    function AxisLeft: TVector2; override;
    function AxisRight: TVector2; override;
    function AxisLeftTrigger: Single; override;
    function AxisRightTrigger: Single; override;
    function InternalButtonMap(
      const Button: TInternalGameControllerButton): TGameControllerButton; override;
  end;

function TExplicitControllerBackend.AxisLeft: TVector2;
begin
  Result := FAxisLeft;
end;

function TExplicitControllerBackend.AxisRight: TVector2;
begin
  Result := FAxisRight;
end;

function TExplicitControllerBackend.AxisLeftTrigger: Single;
begin
  { Explicit backend does not support triggers for now, so we return 0. }
  Result := 0;
end;

function TExplicitControllerBackend.AxisRightTrigger: Single;
begin
  { Explicit backend does not support triggers for now, so we return 0. }
  Result := 0;
end;

function TExplicitControllerBackend.InternalButtonMap(
  const Button: TInternalGameControllerButton): TGameControllerButton;
begin
  { Explicit backend just assumes that internal button index
    equals the TGameControllerButton index. }
  Result := TGameControllerButton(Button);
end;

{ TExplicitControllerManagerBackend ----------------------------------------- }

procedure TExplicitControllerManagerBackend.Initialize;
begin
  // Nothing needs to be done here
end;

procedure TExplicitControllerManagerBackend.Poll;
begin
  // Nothing needs to be done here
end;

procedure TExplicitControllerManagerBackend.SetCount(const NewControllerCount: Integer);
var
  I: Integer;
  Controller: TGameController;
begin
  List.Clear;
  for I := 0 to NewControllerCount - 1 do
  begin
    Controller := TGameController.Create;
    // We don't need to assign it anywhere, constructor sets Controller.InternalBackend
    TExplicitControllerBackend.Create(Controller);
    List.Add(Controller);
  end;
  // Call TGameControllers.OnChange
  Controllers.DoChange;
end;

procedure TExplicitControllerManagerBackend.SetAxisLeft(const ControllerIndex: Integer; const Axis: TVector2);
var
  ControllerBackend: TExplicitControllerBackend;
begin
  if Between(ControllerIndex, 0, List.Count - 1) then
  begin
    ControllerBackend := List[ControllerIndex].InternalBackend as TExplicitControllerBackend;
    ControllerBackend.FAxisLeft := Axis;
  end else
    WriteLnWarning('Controller index %d given to CGEApp_ControllerAxisLeft is incorrect. Current controller count (given to CGEApp_ControllerCount) is %d.', [
      ControllerIndex,
      List.Count
    ]);
end;

procedure TExplicitControllerManagerBackend.SetAxisRight(const ControllerIndex: Integer; const Axis: TVector2);
var
  ControllerBackend: TExplicitControllerBackend;
begin
  if Between(ControllerIndex, 0, List.Count - 1) then
  begin
    ControllerBackend := List[ControllerIndex].InternalBackend as TExplicitControllerBackend;
    ControllerBackend.FAxisRight := Axis;
  end else
    WriteLnWarning('Controller index %d given to CGEApp_ControllerAxisRight is incorrect. Current controller count (given to CGEApp_ControllerCount) is %d.', [
      ControllerIndex,
      List.Count
    ]);
end;

end.
