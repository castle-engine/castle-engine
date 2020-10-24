{
  Copyright 2017-2018 Michalis Kamburelis.

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

uses CastleWindow;

var
  Window: TCastleWindowBase;

implementation

uses SysUtils, Classes,
  CastleScene, CastleControls, CastleUtils,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors, CastleLog,
  CastleParameters, CastleTiledMap, CastleApplicationProperties,
  CastleUIControls, CastleComponentSerialize;

{ routines ------------------------------------------------------------------- }

type
  TEventsHandler = class(TComponent)
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMapControl;
    ButtonOpen: TCastleButton;
    CheckboxSmoothScaling: TCastleCheckbox;
    CheckboxSmoothScalingSafeBorder: TCastleCheckbox;

    { One-time initialization of resources. }
    procedure Initialize(Sender: TObject);

    { Various events. }
    procedure Update(const Sender: TInputListener;
      const SecondsPassed: Single; var HandleInput: Boolean);
    procedure ClickOpen(Sender: TObject);
    procedure MapMotion(const Sender: TInputListener;
      const Event: TInputMotion; var Handled: Boolean);
    procedure MapPress(const Sender: TInputListener;
      const Event: TInputPressRelease; var Handled: Boolean);
    procedure CheckboxSmoothScalingChange(Sender: TObject);
    procedure CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
  end;

procedure TEventsHandler.Initialize(Sender: TObject);
var
  Ui: TCastleUserInterface;
begin
  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/main.castle-user-interface', Window);
  Window.Controls.InsertFront(Ui);

  { Find necessary controls from main.castle-user-interface }
  LabelFps := Window.FindRequiredComponent('LabelFps') as TCastleLabel;
  TiledMap := Window.FindRequiredComponent('TiledMap') as TCastleTiledMapControl;
  ButtonOpen := Window.FindRequiredComponent('ButtonOpen') as TCastleButton;
  CheckboxSmoothScaling := Window.FindRequiredComponent('CheckboxSmoothScaling') as TCastleCheckbox;
  CheckboxSmoothScalingSafeBorder := Window.FindRequiredComponent('CheckboxSmoothScalingSafeBorder') as TCastleCheckbox;

  { Assign events }
  Ui.OnUpdate := @Update;
  ButtonOpen.OnClick := @ClickOpen;
  TiledMap.OnMotion := @MapMotion;
  TiledMap.OnPress := @MapPress;
  CheckboxSmoothScaling.OnChange := @CheckboxSmoothScalingChange;
  CheckboxSmoothScalingSafeBorder.OnChange := @CheckboxSmoothScalingSafeBorderChange;

  { synchronize initial checkbox state with TiledMap state }
  CheckboxSmoothScaling.Checked := TiledMap.SmoothScaling;
  CheckboxSmoothScalingSafeBorder.Checked := TiledMap.SmoothScalingSafeBorder;

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    TiledMap.URL := Parameters[1]
  else
    TiledMap.URL := 'castle-data:/maps/desert.tmx';
end;

procedure TEventsHandler.Update(const Sender: TInputListener;
  const SecondsPassed: Single; var HandleInput: Boolean);
begin
  LabelFps.Caption := 'FPS: ' + Window.Fps.ToString;
end;

procedure TEventsHandler.ClickOpen(Sender: TObject);
var
  URL: String;
begin
  URL := TiledMap.URL;
  if Window.FileDialog('Open Map', URL, true, 'Tiled Map (*.tmx)|*.tmx|All Files|*') then
    TiledMap.URL := URL;
end;

procedure TEventsHandler.MapMotion(const Sender: TInputListener;
  const Event: TInputMotion; var Handled: Boolean);
begin
  if buttonLeft in Event.Pressed then
  begin
    TiledMap.Origin := TiledMap.Origin -
      (Event.Position - Event.OldPosition) / TiledMap.Scale;
    Handled := true;
  end;
end;

procedure TEventsHandler.MapPress(const Sender: TInputListener;
  const Event: TInputPressRelease; var Handled: Boolean);
const
  MinScale = 0.1;
  MaxScale = 10;
begin
  if Event.IsMouseWheel(mwUp) then
  begin
    TiledMap.Scale := Clamped(TiledMap.Scale * 1.1, MinScale, MaxScale);
    Handled := true;
  end else
  if Event.IsMouseWheel(mwDown) then
  begin
    TiledMap.Scale := Clamped(TiledMap.Scale / 1.1, MinScale, MaxScale);
    Handled := true;
  end;
end;

procedure TEventsHandler.CheckboxSmoothScalingChange(Sender: TObject);
begin
  TiledMap.SmoothScaling := CheckboxSmoothScaling.Checked;
end;

procedure TEventsHandler.CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
begin
  TiledMap.SmoothScalingSafeBorder := CheckboxSmoothScalingSafeBorder.Checked;
end;

var
  EventsHandler: TEventsHandler;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'tiled_demo';

  InitializeLog;

  { initialize Application callbacks }
  EventsHandler := TEventsHandler.Create(Application);
  Application.OnInitializeEvent := @EventsHandler.Initialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
end.
