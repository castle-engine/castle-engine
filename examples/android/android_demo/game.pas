{ -*- compile-command: "sh compile.sh" -*- }
{
  Copyright 2013-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple 3D game. This unit implements the game logic,
  actually independent from Android. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils, CastleWindow, CastleControls, CastleUIControls, CastleRectangles,
  CastleGLUtils, CastleColors, X3DNodes, CastleFilesUtils, CastleLog,
  CastleSceneCore, CastleFindFiles, CastleStringUtils, CastleMessages,
  CastleProgress, CastleWindowProgress;

var
  {$ifdef SOLID_BACKGROUND}
  Background: TCastleSimpleBackground;
  {$endif}
  Image: TCastleImageControl;

  ToggleShaderButton: TCastleButton;
  ToggleScreenEffectButton: TCastleButton;
  ToggleSSAOButton: TCastleButton;
  TouchUIButton: TCastleButton;
  MessageButton: TCastleButton;
  ProgressButton: TCastleButton;
  ReopenContextButton: TCastleButton;

  MyShaderEffect: TEffectNode;
  MyScreenEffect: TScreenEffectNode;

type
  TDummy = class
    procedure ToggleShaderClick(Sender: TObject);
    procedure ToggleScreenEffectClick(Sender: TObject);
    procedure ToggleSSAOClick(Sender: TObject);
    procedure TouchUIClick(Sender: TObject);
    procedure MessageClick(Sender: TObject);
    procedure ProgressClick(Sender: TObject);
    procedure ReopenContextClick(Sender: TObject);
  end;

procedure TDummy.ToggleShaderClick(Sender: TObject);
begin
  if MyShaderEffect <> nil then
  begin
    MyShaderEffect.Enabled := not MyShaderEffect.Enabled;
    ToggleShaderButton.Pressed := MyShaderEffect.Enabled;
  end;
end;

procedure TDummy.ToggleScreenEffectClick(Sender: TObject);
begin
  if MyScreenEffect <> nil then
  begin
    MyScreenEffect.Enabled := not MyScreenEffect.Enabled;
    ToggleScreenEffectButton.Pressed := MyScreenEffect.Enabled;
  end;
end;

procedure TDummy.ToggleSSAOClick(Sender: TObject);
begin
  Window.SceneManager.ScreenSpaceAmbientOcclusion :=
    not Window.SceneManager.ScreenSpaceAmbientOcclusion;
  ToggleSSAOButton.Pressed := Window.SceneManager.ScreenSpaceAmbientOcclusion;
end;

procedure TDummy.TouchUIClick(Sender: TObject);
begin
  if Window.TouchInterface = High(TTouchCtlInterface) then
    Window.TouchInterface := Low(TTouchCtlInterface) else
    Window.TouchInterface := Succ(Window.TouchInterface);
end;

procedure TDummy.MessageClick(Sender: TObject);
begin
  if MessageYesNo(Window, 'Test of a yes/no message. Click one of the buttons!') then
    MessageOK(Window, 'You clicked "Yes".') else
    MessageOK(Window, 'You clicked "No".');
end;

procedure TDummy.ProgressClick(Sender: TObject);
var
  I: Integer;
begin
  Progress.Init(100, 'Please wait...');
  try
    for I := 1 to 100 do
    begin
      Sleep(100);
      Progress.Step;
      { On Android, it is possible that our window was closed at any time.
        Prepare for this, to not have progress UI still active when window
        is open again. }
      if Window.Closed then
        Exit;
    end;
  finally Progress.Fini end;
end;

procedure TDummy.ReopenContextClick(Sender: TObject);
begin
  Window.Close(false);
  Window.Open;
end;

procedure FindFilesCallback(const FileInfo: TFileInfo; Data: Pointer);
begin
  WritelnLog('FindFiles', 'Found URL:%s, Name:%s, AbsoluteName:%s, Directory:%s',
    [FileInfo.URL, FileInfo.Name, FileInfo.AbsoluteName, BoolToStr[FileInfo.Directory]]);
end;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  InitializeLog('1.0.0');

  Progress.UserInterface := WindowProgressInterface;

{$ifdef SOLID_BACKGROUND}
  { Show other controls under SceneManager, this way our Background
    is visible. Otherwise, Background defined in main 3D scene is used. }
  Window.SceneManager.Transparent := true;

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertBack(Background);
{$endif}

  Image := TCastleImageControl.Create(Window);
  Image.URL := ApplicationData('sample_image_with_alpha.png' {'sample_texture.ppm'});
  Window.Controls.InsertFront(Image);

  Window.Load(ApplicationData('castle_with_lights_and_camera.wrl'));
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  ToggleShaderButton := TCastleButton.Create(Window);
  ToggleShaderButton.Caption := 'Toggle Shader Effect';
  ToggleShaderButton.OnClick := @TDummy(nil).ToggleShaderClick;
  ToggleShaderButton.Toggle := true;
  Window.Controls.InsertFront(ToggleShaderButton);

  ToggleScreenEffectButton := TCastleButton.Create(Window);
  ToggleScreenEffectButton.Caption := 'Toggle Screen Effect';
  ToggleScreenEffectButton.OnClick := @TDummy(nil).ToggleScreenEffectClick;
  ToggleScreenEffectButton.Toggle := true;
  Window.Controls.InsertFront(ToggleScreenEffectButton);

  ToggleSSAOButton := TCastleButton.Create(Window);
  ToggleSSAOButton.Caption := 'Toggle SSAO';
  ToggleSSAOButton.OnClick := @TDummy(nil).ToggleSSAOClick;
  ToggleSSAOButton.Toggle := true;
  Window.Controls.InsertFront(ToggleSSAOButton);

  TouchUIButton := TCastleButton.Create(Window);
  TouchUIButton.Caption := 'Next Touch UI';
  TouchUIButton.OnClick := @TDummy(nil).TouchUIClick;
  Window.Controls.InsertFront(TouchUIButton);

  MessageButton := TCastleButton.Create(Window);
  MessageButton.Caption := 'Test Modal Message';
  MessageButton.OnClick := @TDummy(nil).MessageClick;
  Window.Controls.InsertFront(MessageButton);

  ProgressButton := TCastleButton.Create(Window);
  ProgressButton.Caption := 'Test Progress Bar';
  ProgressButton.OnClick := @TDummy(nil).ProgressClick;
  Window.Controls.InsertFront(ProgressButton);

  ReopenContextButton := TCastleButton.Create(Window);
  ReopenContextButton.Caption := 'Test Reopening OpenGL Context';
  ReopenContextButton.OnClick := @TDummy(nil).ReopenContextClick;
  Window.Controls.InsertFront(ReopenContextButton);

  MyShaderEffect := Window.SceneManager.MainScene.RootNode.TryFindNodeByName(
    TEffectNode, 'MyShaderEffect', false) as TEffectNode;
  ToggleShaderButton.Pressed := (MyShaderEffect <> nil) and MyShaderEffect.Enabled;

  MyScreenEffect := Window.SceneManager.MainScene.RootNode.TryFindNodeByName(
    TScreenEffectNode, 'MyScreenEffect', false) as TScreenEffectNode;
  ToggleScreenEffectButton.Pressed := (MyScreenEffect <> nil) and MyScreenEffect.Enabled;

  Window.TouchInterface := etciCtlWalkDragRotate;

  { Test that FindFiles works also on Android asset filesystem. }
  FindFiles(ApplicationData(''), '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'skies', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle/', '*', true, @FindFilesCallback, nil, [ffRecursive]);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, Format('FPS : %f (real : %f). Shapes : %d / %d',
   [Window.Fps.FrameTime,
    Window.Fps.RealTime,
    Window.SceneManager.Statistics.ShapesRendered,
    Window.SceneManager.Statistics.ShapesVisible]));
end;

procedure WindowResize(Container: TUIContainer);
const
  Margin = 10;
var
  Bottom: Integer;
begin
  Image.AlignHorizontal(prLow, prLow, Margin);
  Image.AlignVertical(prHigh, prHigh, -Margin);

  Bottom := Window.Height;

  Bottom -= ToggleShaderButton.Height + Margin;
  ToggleShaderButton.AlignHorizontal(prMiddle, prMiddle);
  ToggleShaderButton.Bottom := Bottom;

  Bottom -= ToggleScreenEffectButton.Height + Margin;
  ToggleScreenEffectButton.AlignHorizontal(prMiddle, prMiddle);
  ToggleScreenEffectButton.Bottom := Bottom;

  Bottom -= ToggleSSAOButton.Height + Margin;
  ToggleSSAOButton.AlignHorizontal(prMiddle, prMiddle);
  ToggleSSAOButton.Bottom := Bottom;

  Bottom := Window.Height;

  Bottom -= TouchUIButton.Height + Margin;
  TouchUIButton.AlignHorizontal(prHigh, prHigh, -Margin);
  TouchUIButton.Bottom := Bottom;

  Bottom -= MessageButton.Height + Margin;
  MessageButton.AlignHorizontal(prHigh, prHigh, -Margin);
  MessageButton.Bottom := Bottom;

  Bottom -= ProgressButton.Height + Margin;
  ProgressButton.AlignHorizontal(prHigh, prHigh, -Margin);
  ProgressButton.Bottom := Bottom;

  Bottom -= ReopenContextButton.Height + Margin;
  ReopenContextButton.AlignHorizontal(prHigh, prHigh, -Margin);
  ReopenContextButton.Bottom := Bottom;
end;

function MyGetApplicationName: string;
begin
  Result := 'androiddemo';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnResize := @WindowResize;
end.
