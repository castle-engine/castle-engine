{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors, CastleLog,
  CastleParameters, CastleTiledMap;

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
  UIFont.Print(10, 10, Yellow, Format('FPS: %f', [Container.Fps.RealTime]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  // ... do something every frame
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // ... react to press of key, mouse, touch
end;

function MyGetApplicationName: string;
begin
  Result := 'Tiled demo';
end;

initialization
  // ifdef debug
  InitializeLog;
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

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
