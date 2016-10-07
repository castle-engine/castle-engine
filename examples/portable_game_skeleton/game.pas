{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors;

var
  ExampleImage: TCastleImageControl;
  ExampleScene: TCastleScene;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { This is just an example of creating a simple 2D control
    (TCastleImageControl) and 3D object (TCastleScene). }

  ExampleImage := TCastleImageControl.Create(Window);
  ExampleImage.URL := ApplicationData('example_image.png');
  ExampleImage.Bottom := 100;
  ExampleImage.Left := 100;
  Window.Controls.InsertFront(ExampleImage);

  ExampleScene := TCastleScene.Create(Application);
  ExampleScene.Load(ApplicationData('example_scene.x3dv'));
  ExampleScene.Spatial := [ssRendering, ssDynamicCollisions];
  ExampleScene.ProcessEvents := true;
  Window.SceneManager.Items.Add(ExampleScene);
  Window.SceneManager.MainScene := ExampleScene;
end;

procedure WindowRender(Container: TUIContainer);
begin
  // ... custom rendering code
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
  Result := 'my_fantastic_game';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
end.
