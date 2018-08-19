{ Game initialization and logic.
  You will probably split larger games into more units,
  and then this main unit will just initialize everything else.

  This code is independent from mobile / standalone platforms.
  It will be used by the appropriate .lpr file for desktop, Android or iOS.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameInitialize;

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
  CastleUIControls, CastleApplicationProperties;

var
  Window: TCastleWindowTouch;
  Status: TCastleLabel;
  ExampleImage: TCastleImageControl;
  ExampleScene: TCastleScene;

{ routines ------------------------------------------------------------------- }

procedure WindowUpdate(Container: TUIContainer);
begin
  // ... do something every frame
  Status.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // ... react to press of key, mouse, touch
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Assign Window callbacks }
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;

  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { Show a label with frames per second information }
  Status := TCastleLabel.Create(Application);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could also use "Vector4(1, 1, 0, 1)" instead of Yellow
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

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'my_fantastic_game';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (in case of non-desktop platforms, some necessary may not be prepared yet). }
end.
