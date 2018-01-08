{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$mode objfpc}{$H+}

{ Game initialization and logic. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors, CastleLog,
  CastleParameters, CastleTiledMap, CastleApplicationProperties;

var
  TiledMap: TCastleTiledMapControl;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  DefaultTmxFile: string;
  FilePath: string;
begin
  { Load the map from given parameter filepath or default. }
  DefaultTmxFile := ApplicationData('desert.tmx');
  if Parameters.High = 1 then
    FilePath := Parameters[1]
  else
    FilePath := DefaultTmxFile;
  TiledMap := TCastleTiledMapControl.Create(Window, FilePath);
  WriteLnLog('filepath', FilePath);
  Window.Controls.InsertFront(TiledMap);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, 'FPS: ' + Container.Fps.ToString);
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  // ... do something every frame
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // ... react to press of key, mouse, touch
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'tiled_demo';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.AutoRedisplay := True;
end.
