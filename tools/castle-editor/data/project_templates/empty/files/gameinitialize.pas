{ Game initialization and logic.

  This code is independent from mobile / standalone platforms.
  It will be used by the appropriate .lpr file for desktop, Android or iOS.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleComponentSerialize;

var
  Window: TCastleWindowCustom;
  LabelFps: TCastleLabel;

procedure WindowUpdate(Container: TUIContainer);
begin
  // ... do something every frame
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  // ... react to press of key, mouse, touch
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Ui: TCastleUserInterface;
begin
  { Assign Window callbacks }
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;

  { Adjust container settings,
    e.g. for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/main.castle-user-interface', Window);
  Window.Controls.InsertFront(Ui);

  { Find a label to show frames per second information }
  LabelFps := Window.FindRequiredComponent('LabelFps') as TCastleLabel;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := '${PROJECT_NAME}';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (in case of non-desktop platforms, some necessary may not be prepared yet). }
end.
