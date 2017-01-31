{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic, independent from mobile / standalone / plugin. }
unit Game;

interface

implementation

uses SysUtils, CastleWindow, CastleScene, CastleControls,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleWindowTouch,
  CastleLog, CastleGLUtils, CastleColors, CastleWindowProgress,
  CastleUIControls, X3DLoad, CastleUtils, CastleProgress, CastleURIUtils,
  CastleDownload, CastleMessages;

{ routines ------------------------------------------------------------------- }

type
  TPluginWindow = class(TCastleWindowTouch)
  protected
    function CreateContainer: TWindowContainer; override;
  end;

  TPluginWindowContainer = class(TWindowContainer)
  private
    procedure MessageClick(Sender: TObject);
    procedure ProgressClick(Sender: TObject);
  public
    Parent: TPluginWindow;
    Scene: TCastleScene;
    Status: TCastleLabel;
    MessageButton: TCastleButton;
    ProgressButton: TCastleButton;
    URL, ErrorMessage: string;
    constructor Create(AParent: TPluginWindow); reintroduce;
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

constructor TPluginWindowContainer.Create(AParent: TPluginWindow);
begin
  inherited Create(AParent);
  Parent := AParent;
end;

procedure TPluginWindowContainer.LoadScene(const AURL: string);
begin
  try
    { change URL even in case of Scene.Load errors, as Scene.Load clears
      the scene anyway }
    URL := AURL;
    { used for progress bar }
    Application.MainWindow := Parent;
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
  Application.MainWindow := Parent;

  Scene := TCastleScene.Create(Application);
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building shape octree';
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Parent.SceneManager.Items.Add(Scene);
  Parent.SceneManager.MainScene := Scene;

  if Parent.NamedParameters.Values['cge_scene'] <> '' then
    LoadScene(Parent.NamedParameters.Values['cge_scene']);

  Application.MainWindow := nil;

  Status := TCastleLabel.Create(Self);
  Status.Frame := true;
  Parent.Controls.InsertFront(Status);

  MessageButton := TCastleButton.Create(Self);
  MessageButton.Caption := 'Test Modal Message';
  MessageButton.OnClick := @MessageClick;
  MessageButton.Bottom := 100;
  MessageButton.Left := 10;
  Parent.Controls.InsertFront(MessageButton);

  ProgressButton := TCastleButton.Create(Self);
  ProgressButton.Caption := 'Test Progress Bar';
  ProgressButton.OnClick := @ProgressClick;
  ProgressButton.Bottom := 150;
  ProgressButton.Left := 10;
  Parent.Controls.InsertFront(ProgressButton);
end;

procedure TPluginWindowContainer.EventUpdate;
begin
  inherited;
  Status.Caption := 'Model: ' + URICaption(URL) + NL +
    'Browser: ' + Application.UserAgent + NL +
    Format('FPS: %f real : %f',  [Fps.FrameTime, Fps.RealTime]);
  if ErrorMessage <> '' then
    Status.Text.Append('Erorr when loading: ' + ErrorMessage);
end;

procedure TPluginWindowContainer.EventResize;
begin
  inherited;
  Status.Align(hpLeft, hpLeft, 10);
  Status.Align(vpBottom, vpBottom, 10);
end;

procedure TPluginWindowContainer.MessageClick(Sender: TObject);
begin
  if MessageYesNo(Parent, 'Test of a yes/no message. Click one of the buttons!') then
    MessageOK(Parent, 'You clicked "Yes".') else
    MessageOK(Parent, 'You clicked "No".');
end;

procedure TPluginWindowContainer.ProgressClick(Sender: TObject);
const
  TestProgressSteps = 100;
var
  I: Integer;
begin
  { used for progress bar }
  Application.MainWindow := Parent;

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

function MyGetApplicationName: string;
begin
  Result := 'cge_3d_viewer';
end;

initialization
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog; // start logging early, to test plugin startup

  { initialize Application }
  Application.OnInitialize := @ApplicationInitialize;
  Application.DefaultWindowClass := TPluginWindow;

  Progress.UserInterface := WindowProgressInterface;
end.
