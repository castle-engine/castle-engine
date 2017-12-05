{ Game initialization and logic.
  You will probably split larger games into more units,
  and then this main unit will just initialize everything else.

  This code is independent from mobile / standalone platforms.
  It will be used by the appropriate .lpr file for desktop, Android or iOS.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit Game;

{ Compilation options adviced by the Castle Game Engine.
  But actually you can use any options suitable for you (although you will
  most likely want to use ObjFpc or Delphi mode, to have classes support).

  Feel free to remove this line if you know that you configure proper
  compilation options elsewhere (e.g. in the Lazarus project options;
  the CGE build tool automatically defines these options). }
{$mode objfpc}{$H+}{$J-}

interface

implementation

uses SysUtils,
  CastleWindowTouch, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls;

var
  Window: TCastleWindowTouch;
  Status: TCastleLabel;
  ExampleImage: TCastleImageControl;
  ExampleScene: TCastleScene;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Show a label with some text }
  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could use "Vector4(1, 1, 0, 1)" instead of Yellow
  Window.Controls.InsertFront(Status);

  { Show 2D image }
  ExampleImage := TCastleImageControl.Create(Application);
  ExampleImage.URL := ApplicationData('example_image.png');
  ExampleImage.Bottom := 100;
  ExampleImage.Left := 100;
  Window.Controls.InsertFront(ExampleImage);

  { Show a 3D object (TCastleScene) inside a Window.SceneManager
    (which acts as a full-screen viewport by default). }
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
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  // ... do something every frame
  Status.Caption := Format('FPS: %f', [Container.Fps.RealTime]);
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
