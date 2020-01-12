{
  Copyright 2015-2018 Michalis Kamburelis.

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

uses SysUtils,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse,
  CastleLog, CastleGLUtils, CastleColors, CastleWindowProgress,
  CastleUIControls, X3DLoad, CastleUtils, CastleProgress, CastleURIUtils,
  CastleDownload, CastleMessages, CastleApplicationProperties, CastleViewport;

{ routines ------------------------------------------------------------------- }

type
  TPluginWindow = class(TCastleWindowBase)
  protected
    function CreateContainer: TWindowContainer; override;
  end;

  TPluginWindowContainer = class(TWindowContainer)
  private
    procedure MessageClick(Sender: TObject);
    procedure ProgressClick(Sender: TObject);
  public
    Window: TPluginWindow;
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    Status: TCastleLabel;
    MessageButton: TCastleButton;
    ProgressButton: TCastleButton;
    URL, ErrorMessage: string;
    constructor Create(AWindow: TPluginWindow); reintroduce;
    procedure LoadScene(const AURL: string);
    procedure EventOpen(const OpenWindowsCount: Cardinal); override;
    procedure EventUpdate; override;
    procedure EventResize; override;
  end;

{ TPluginWindow -------------------------------------------------------------- }

function TPluginWindow.CreateContainer: TWindowContainer;
begin
  Result := TPluginWindowContainer.Create(Self);
end;

{ TPluginWindowContainer ----------------------------------------------------- }

constructor TPluginWindowContainer.Create(AWindow: TPluginWindow);
begin
  inherited Create(AWindow);
  Window := AWindow;
end;

procedure TPluginWindowContainer.LoadScene(const AURL: string);
begin
  try
    { change URL even in case of Scene.Load errors, as Scene.Load clears
      the scene anyway }
    URL := AURL;
    { used for progress bar }
    Application.MainWindow := Window;
    Scene.Load(AURL);
    Application.MainWindow := nil;
    ErrorMessage := '';
  except
    on E: TObject do ErrorMessage := ExceptMessage(E);
  end;
end;

procedure TPluginWindowContainer.EventOpen(const OpenWindowsCount: Cardinal);
begin
  inherited;

  { used for progress bar }
  Application.MainWindow := Window;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building shape octree';
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  if Window.NamedParameters.Values['cge_scene'] <> '' then
    LoadScene(Window.NamedParameters.Values['cge_scene']);

  Application.MainWindow := nil;

  Status := TCastleLabel.Create(Self);
  Status.Frame := true;
  Status.Color := White;
  Window.Controls.InsertFront(Status);

  MessageButton := TCastleButton.Create(Self);
  MessageButton.Caption := 'Test Modal Message';
  MessageButton.OnClick := @MessageClick;
  MessageButton.Bottom := 100;
  MessageButton.Left := 10;
  Window.Controls.InsertFront(MessageButton);

  ProgressButton := TCastleButton.Create(Self);
  ProgressButton.Caption := 'Test Progress Bar';
  ProgressButton.OnClick := @ProgressClick;
  ProgressButton.Bottom := 150;
  ProgressButton.Left := 10;
  Window.Controls.InsertFront(ProgressButton);
end;

procedure TPluginWindowContainer.EventUpdate;
var
  S: String;
begin
  inherited;
  S :=
    'Model: ' + URICaption(URL) + NL +
    'Browser: ' + Application.UserAgent + NL +
    'FPS: ' +  Fps.ToString + NL;
  // Note that ErrorMessage may contain newlines too, e.g. SSL error message
  if ErrorMessage <> '' then
    S := S + 'Error when loading: ' + ErrorMessage;
  Status.Caption := S;
end;

procedure TPluginWindowContainer.EventResize;
begin
  inherited;
  Status.Align(hpLeft, hpLeft, 10);
  Status.Align(vpBottom, vpBottom, 10);
end;

procedure TPluginWindowContainer.MessageClick(Sender: TObject);
begin
  if MessageYesNo(Window, 'Test of a yes/no message. Click one of the buttons!') then
    MessageOK(Window, 'You clicked "Yes".') else
    MessageOK(Window, 'You clicked "No".');
end;

procedure TPluginWindowContainer.ProgressClick(Sender: TObject);
const
  TestProgressSteps = 100;
var
  I: Integer;
begin
  { used for progress bar }
  Application.MainWindow := Window;

  Progress.Init(TestProgressSteps, 'Please wait...');
  try
    for I := 1 to TestProgressSteps do
    begin
      Sleep(10);
      Progress.Step;
      WritelnLog('Progress', 'Step %d', [I]);
    end;
  finally Progress.Fini end;

  Application.MainWindow := nil;
end;

{ global --------------------------------------------------------------------- }

procedure ApplicationInitialize;
begin
  EnableNetwork := true;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'cge_3d_viewer';

  InitializeLog; // start logging early, to test plugin startup

  { initialize Application }
  Application.OnInitialize := @ApplicationInitialize;
  Application.DefaultWindowClass := TPluginWindow;

  Progress.UserInterface := WindowProgressInterface;
end.
