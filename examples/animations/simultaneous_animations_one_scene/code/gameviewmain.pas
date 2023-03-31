{
  Copyright 2019-2023 Michalis Kamburelis.

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

uses SysUtils, Classes,
  CastleScene, CastleControls, CastleLog, CastleVectors,
  CastleFilesUtils, CastleSceneCore, CastleViewport, CastleComponentSerialize,
  CastleUIControls, CastleApplicationProperties, X3DNodes;

type
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonAnimationSqueeze, ButtonAnimationGear: TCastleButton;
    Scene1: TCastleScene;
    Viewport1: TCastleViewport;
  strict private
    procedure ClickAnimationSqueeze(Sender: TObject);
    procedure ClickAnimationGear(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  { Assign OnClick events }
  ButtonAnimationSqueeze.OnClick := {$ifdef FPC}@{$endif}ClickAnimationSqueeze;
  ButtonAnimationGear.OnClick := {$ifdef FPC}@{$endif}ClickAnimationGear;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickAnimationSqueeze(Sender: TObject);
var
  TimeSensor: TTimeSensorNode;
begin
  ButtonAnimationSqueeze.Pressed := not ButtonAnimationSqueeze.Pressed;
  TimeSensor := Scene1.AnimationTimeSensor('squeeze');
  if ButtonAnimationSqueeze.Pressed then
    { setting InitialTime to TimeSensor.ElapsedTimeInCycle below
      means that we start animation from the moment it was stopped
      (so it looks like "unpause"). }
    TimeSensor.Start(true, true, TimeSensor.ElapsedTimeInCycle)
  else
    TimeSensor.Stop;
end;

procedure TViewMain.ClickAnimationGear(Sender: TObject);
var
  TimeSensor: TTimeSensorNode;
begin
  ButtonAnimationGear.Pressed := not ButtonAnimationGear.Pressed;
  TimeSensor := Scene1.AnimationTimeSensor('gear_rotate');
  if ButtonAnimationGear.Pressed then
    { setting InitialTime to TimeSensor.ElapsedTimeInCycle below
      means that we start animation from the moment it was stopped
      (so it looks like "unpause"). }
    TimeSensor.Start(true, true, TimeSensor.ElapsedTimeInCycle)
  else
    TimeSensor.Stop;
end;

end.
