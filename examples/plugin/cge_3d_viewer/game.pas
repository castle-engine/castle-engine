{
  Copyright 2015-2015 Michalis Kamburelis.

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
  CastleDownload;

{ routines ------------------------------------------------------------------- }

type
  TPluginWindow = class(TCastleWindowTouch)
  protected
    function CreateContainer: TWindowContainer; override;
  end;

  TPluginWindowContainer = class(TWindowContainer)
  public
    Parent: TPluginWindow;
    Scene: TCastleScene;
    Status: TCastleLabel;
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

  Status := TCastleLabel.Create(Application);
  Parent.Controls.InsertFront(Status);
end;

procedure TPluginWindowContainer.EventUpdate;
begin
  inherited;
  Status.Text.Text := 'Model: ' + URICaption(URL) + NL +
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

  // TODO: do not use WindowProgressInterface, it makes it's own loop
  // that could hang browser (or at least plugin container).
  //Progress.UserInterface := WindowProgressInterface;
end.
