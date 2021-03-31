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

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleVectors,
  CastleFilesUtils, CastleSceneCore, CastleViewport, CastleComponentSerialize,
  CastleUIControls, CastleApplicationProperties, CastleUIState, X3DNodes;

{ TMainState -------------------------------------------------------------- }

type
  TMainState = class(TUIState)
  strict private
    LabelFps: TCastleLabel;
    ButtonAnimationSqueeze, ButtonAnimationGear: TCastleButton;
    Scene1: TCastleScene;
    Viewport1: TCastleViewport;
    procedure ClickAnimationSqueeze(Sender: TObject);
    procedure ClickAnimationGear(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: Boolean); override;
  end;

procedure TMainState.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components created in CGE Editor }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  ButtonAnimationSqueeze := UiOwner.FindRequiredComponent('ButtonAnimationSqueeze') as TCastleButton;
  ButtonAnimationGear := UiOwner.FindRequiredComponent('ButtonAnimationGear') as TCastleButton;
  Scene1 := UiOwner.FindRequiredComponent('Scene1') as TCastleScene;
  Viewport1 := UiOwner.FindRequiredComponent('Viewport1') as TCastleViewport;

  { Assign OnClick events }
  ButtonAnimationSqueeze.OnClick := @ClickAnimationSqueeze;
  ButtonAnimationGear.OnClick := @ClickAnimationGear;
end;

procedure TMainState.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TMainState.ClickAnimationSqueeze(Sender: TObject);
var
  TimeSensor: TTimeSensorNode;
begin
  ButtonAnimationSqueeze.Pressed := not ButtonAnimationSqueeze.Pressed;
  TimeSensor := Scene1.Node('squeeze') as TTimeSensorNode;
  if ButtonAnimationSqueeze.Pressed then
    { setting InitialTime to TimeSensor.ElapsedTimeInCycle below
      means that we start animation from the moment it was stopped
      (so it looks like "unpause"). }
    TimeSensor.Start(true, true, TimeSensor.ElapsedTimeInCycle)
  else
    TimeSensor.Stop;
end;

procedure TMainState.ClickAnimationGear(Sender: TObject);
var
  TimeSensor: TTimeSensorNode;
begin
  ButtonAnimationGear.Pressed := not ButtonAnimationGear.Pressed;
  TimeSensor := Scene1.Node('gear_rotate') as TTimeSensorNode;
  if ButtonAnimationGear.Pressed then
    { setting InitialTime to TimeSensor.ElapsedTimeInCycle below
      means that we start animation from the moment it was stopped
      (so it looks like "unpause"). }
    TimeSensor.Start(true, true, TimeSensor.ElapsedTimeInCycle)
  else
    TimeSensor.Stop;
end;

{ routines ------------------------------------------------------------------- }

var
  Window: TCastleWindowBase;
  MainState: TMainState;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI. }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create instance of TMainState that will create UI, and handle events. }
  MainState := TMainState.Create(Application);
  TUIState.Current := MainState;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'simultaneous_animations_one_scene';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (in case of non-desktop platforms, some necessary may not be prepared yet). }
end.
