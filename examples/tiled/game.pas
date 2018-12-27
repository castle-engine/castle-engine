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
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils, Classes,
  CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors, CastleLog,
  CastleParameters, CastleTiledMap, CastleApplicationProperties,
  CastleUIControls, CastleComponentSerialize;

{ routines ------------------------------------------------------------------- }

type
  TEventsHandler = class(TComponent)
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMapControl;
    ButtonOpen: TCastleButton;

    { One-time initialization of resources. }
    procedure Initialize(Sender: TObject);

    { Various events. }
    procedure Update(const Sender: TInputListener;
      const SecondsPassed: Single; var HandleInput: Boolean);
    procedure ClickOpen(Sender: TObject);
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

  { Assign events }
  Ui.OnUpdate := @Update;
  ButtonOpen.OnClick := @ClickOpen;

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    TiledMap.URL := Parameters[1]
  else
    TiledMap.URL := 'castle-data:/desert.tmx';
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
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
end.
