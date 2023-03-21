{
  Copyright 2016-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleNotifications, X3DNodes, X3DFields, X3DTime;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainScene: TCastleScene;
    Notifications: TCastleNotifications;
  private
    TouchSensors: array [1..4] of TTouchSensorNode;
    procedure TouchSensorClick(Sender: TObject);
    procedure TouchSensorTouchTime(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure TouchSensorIsActive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  I: Integer;
begin
  inherited;
  for I := Low(TouchSensors) to High(TouchSensors) do
  begin
    TouchSensors[I] := MainScene.Node('TouchSensorPiece' + IntToStr(I)) as TTouchSensorNode;
    { It is easiest to just handle OnClick.
      It corresponds to EventTouchTime, though OnClick is more standard TNotifyEvent. }
    TouchSensors[I].OnClick := {$ifdef FPC}@{$endif} TouchSensorClick;
    TouchSensors[I].EventTouchTime.AddNotification({$ifdef FPC}@{$endif} TouchSensorTouchTime);
    TouchSensors[I].EventIsActive.AddNotification({$ifdef FPC}@{$endif} TouchSensorIsActive);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.TouchSensorClick(Sender: TObject);
var
  TouchSensor: TTouchSensorNode;
begin
  TouchSensor := Sender as TTouchSensorNode;
  Notifications.Show(Format('TouchSensor OnClick event: piece %s', [
    TouchSensor.X3DName
  ]));
end;

procedure TViewMain.TouchSensorTouchTime(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
  TouchSensor: TTouchSensorNode;
begin
  Val := (Value as TSFTime).Value;
  TouchSensor := Event.ParentNode as TTouchSensorNode;
  Notifications.Show(Format('TouchSensor.touchTime event: piece %s, touch time %f', [
    TouchSensor.X3DName,
    Val
  ]));
end;

procedure TViewMain.TouchSensorIsActive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  Val: Boolean;
  TouchSensor: TTouchSensorNode;
begin
  Val := (Value as TSFBool).Value;
  TouchSensor := Event.ParentNode as TTouchSensorNode;
  Notifications.Show(Format('TouchSensor.isActive event: piece %s, is active: %s', [
    TouchSensor.X3DName,
    BoolToStr(Val, true)
  ]));
end;

end.
