{
  Copyright 2013-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple 3D application. This unit implements the application logic,
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
  CastleProgress, CastleWindowProgress, CastleUtils, CastleSoundEngine;

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
  ToggleTextureUpdatesButton: TCastleButton;
  PlaySoundButton: TCastleButton;

  MyShaderEffect: TEffectNode;
  MyScreenEffect: TScreenEffectNode;

  SoundBuffer1, SoundBuffer2: TSoundBuffer;

type
  TDummy = class
    class procedure ToggleShaderClick(Sender: TObject);
    class procedure ToggleScreenEffectClick(Sender: TObject);
    class procedure ToggleSSAOClick(Sender: TObject);
    class procedure TouchUIClick(Sender: TObject);
    class procedure MessageClick(Sender: TObject);
    class procedure ProgressClick(Sender: TObject);
    class procedure ReopenContextClick(Sender: TObject);
    class procedure ToggleTextureUpdates(Sender: TObject);
    class procedure ToggleTextureUpdatesCallback(Node: TX3DNode);
    class procedure PlaySound(Sender: TObject);
  end;

class procedure TDummy.ToggleShaderClick(Sender: TObject);
begin
  if MyShaderEffect <> nil then
  begin
    MyShaderEffect.Enabled := not MyShaderEffect.Enabled;
    ToggleShaderButton.Pressed := MyShaderEffect.Enabled;
  end;
end;

class procedure TDummy.ToggleScreenEffectClick(Sender: TObject);
begin
  if MyScreenEffect <> nil then
  begin
    MyScreenEffect.Enabled := not MyScreenEffect.Enabled;
    ToggleScreenEffectButton.Pressed := MyScreenEffect.Enabled;
  end;
end;

class procedure TDummy.ToggleSSAOClick(Sender: TObject);
begin
  Window.SceneManager.ScreenSpaceAmbientOcclusion :=
    not Window.SceneManager.ScreenSpaceAmbientOcclusion;
  ToggleSSAOButton.Pressed := Window.SceneManager.ScreenSpaceAmbientOcclusion;
end;

class procedure TDummy.TouchUIClick(Sender: TObject);
begin
  if Window.TouchInterface = High(TTouchInterface) then
    Window.TouchInterface := Low(TTouchInterface) else
    Window.TouchInterface := Succ(Window.TouchInterface);
end;

class procedure TDummy.MessageClick(Sender: TObject);
begin
  { On Android, a nice test is to switch to desktop (home)
    when one of these modal MessageXxx is working. The application loop
    (done inside MessageXxx, they call Application.ProcessMessage in a loop)
    will still work, even though the window is closed.
    When user gets back to our app, she/he will see the message box again. }
  if MessageYesNo(Window, 'Test of a yes/no message test.' + NL + NL +' Do you want to deliberately cause an exception (to test our CastleWindow.HandleException method)?') then
  begin
    MessageOK(Window, 'You clicked "Yes". Raising an exception, get ready!');
    raise Exception.Create('Test exception');
  end else
    MessageOK(Window, 'You clicked "No".');
end;

class procedure TDummy.ProgressClick(Sender: TObject);
const
  TestProgressSteps = 100;
var
  I: Integer;
begin
  Progress.Init(TestProgressSteps, 'Please wait');
  try
    for I := 1 to TestProgressSteps do
    begin
      Sleep(100);
      Progress.Step;
      { Note that on Android, Window may get closed (OpenGL context lost)
        at any time, also during such progress operation.
        For example when user switches to desktop (home) on Android.

        Progress.Step processes events (Application.ProcessMessage),
        so it will correctly react to it, closing the Window.
        This "for" loop will still continue, even though the window
        is closed (so no redraw will happen). It will actually get to the end
        of progress quickier (because without redraw, our speed is not throttled;
        you can see this by commenting Sleep call above. With window open,
        we're throttled by redraw speed. With window closed, we're not,
        and even long progress finishes quickly.)
        When the progress finishes, the main loop (from Application.Run)
        will allow to wait for next event (without doing busy waiting and wasting
        CPU), so we do not drain your battery power at all.

        If user will get back to our application before the progress finished,
        she/he will even correctly see the progress continuing at correct point.
        So everything just works. Just do not assume that Window stays
        open when processing events, and you're fine. }
      WritelnLog('Progress', 'Step %d', [I]);
    end;
  finally Progress.Fini end;
end;

class procedure TDummy.ReopenContextClick(Sender: TObject);
begin
  Window.Close(false);
  Window.Open;
end;

class procedure TDummy.ToggleTextureUpdatesCallback(Node: TX3DNode);
var
  CubeMap: TGeneratedCubeMapTextureNode;
  LogStr: string;
begin
  CubeMap := Node as TGeneratedCubeMapTextureNode;
  if CubeMap.Update = upNone then
    CubeMap.Update := upAlways else
    CubeMap.Update := upNone;
  WriteStr(LogStr, 'Toggled updates on ' + CubeMap.NiceName + ' to ', CubeMap.Update);
  WritelnLog('CubeMap', LogStr);
end;

class procedure TDummy.ToggleTextureUpdates(Sender: TObject);
begin
  Window.SceneManager.MainScene.RootNode.EnumerateNodes(
    TGeneratedCubeMapTextureNode, @ToggleTextureUpdatesCallback, false);
end;

class procedure TDummy.PlaySound(Sender: TObject);
begin
  if Random < 0.5 then
    SoundEngine.PlaySound(SoundBuffer1) else
    SoundEngine.PlaySound(SoundBuffer2);
end;

procedure FindFilesCallback(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  WritelnLog('FindFiles', 'Found URL:%s, Name:%s, AbsoluteName:%s, Directory:%s',
    [FileInfo.URL, FileInfo.Name, FileInfo.AbsoluteName, BoolToStr(FileInfo.Directory, true)]);
end;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  Margin = 10;

var
  AnchorY: Integer;

  { Anchor next button under the previous one. }
  procedure AnchorNextButton(const B: TCastleButton);
  begin
    B.Anchor(vpTop, AnchorY);
    AnchorY -= Margin + Round(B.Rect.Height / B.UIScale);
  end;

begin
  Progress.UserInterface := WindowProgressInterface;

  Window.Container.UIScaling := usEncloseReferenceSize;
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;

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
  Image.Anchor(hpLeft, Margin);
  Image.Anchor(vpTop, -Margin);
  Window.Controls.InsertFront(Image);

  Window.Load(ApplicationData('castle_with_lights_and_camera.wrl'));
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  { buttons in middle-top, from top to bottom }

  AnchorY := -Margin;

  ToggleShaderButton := TCastleButton.Create(Window);
  ToggleShaderButton.Caption := 'Toggle Shader Effect';
  ToggleShaderButton.OnClick := @TDummy(nil).ToggleShaderClick;
  ToggleShaderButton.Toggle := true;
  ToggleShaderButton.Anchor(hpMiddle);
  Window.Controls.InsertFront(ToggleShaderButton);
  AnchorNextButton(ToggleShaderButton);

  ToggleScreenEffectButton := TCastleButton.Create(Window);
  ToggleScreenEffectButton.Caption := 'Toggle Screen Effect';
  ToggleScreenEffectButton.OnClick := @TDummy(nil).ToggleScreenEffectClick;
  ToggleScreenEffectButton.Toggle := true;
  ToggleScreenEffectButton.Anchor(hpMiddle);
  Window.Controls.InsertFront(ToggleScreenEffectButton);
  AnchorNextButton(ToggleScreenEffectButton);

  ToggleSSAOButton := TCastleButton.Create(Window);
  ToggleSSAOButton.Caption := 'Toggle SSAO';
  ToggleSSAOButton.OnClick := @TDummy(nil).ToggleSSAOClick;
  ToggleSSAOButton.Toggle := true;
  ToggleSSAOButton.Anchor(hpMiddle);
  Window.Controls.InsertFront(ToggleSSAOButton);
  AnchorNextButton(ToggleSSAOButton);

  { buttons in right-top, from top to bottom }

  AnchorY := -Margin;

  TouchUIButton := TCastleButton.Create(Window);
  TouchUIButton.Caption := 'Next Touch UI';
  TouchUIButton.OnClick := @TDummy(nil).TouchUIClick;
  TouchUIButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(TouchUIButton);
  AnchorNextButton(TouchUIButton);

  MessageButton := TCastleButton.Create(Window);
  MessageButton.Caption := 'Test Modal Message';
  MessageButton.OnClick := @TDummy(nil).MessageClick;
  MessageButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(MessageButton);
  AnchorNextButton(MessageButton);

  ProgressButton := TCastleButton.Create(Window);
  ProgressButton.Caption := 'Test Progress Bar';
  ProgressButton.OnClick := @TDummy(nil).ProgressClick;
  ProgressButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(ProgressButton);
  AnchorNextButton(ProgressButton);

  ReopenContextButton := TCastleButton.Create(Window);
  ReopenContextButton.Caption := 'Test Reopening OpenGL Context';
  ReopenContextButton.OnClick := @TDummy(nil).ReopenContextClick;
  ReopenContextButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(ReopenContextButton);
  AnchorNextButton(ReopenContextButton);

  ToggleTextureUpdatesButton := TCastleButton.Create(Window);
  ToggleTextureUpdatesButton.Caption := 'Toggle CubeMap Texture Updates';
  ToggleTextureUpdatesButton.OnClick := @TDummy(nil).ToggleTextureUpdates;
  ToggleTextureUpdatesButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(ToggleTextureUpdatesButton);
  AnchorNextButton(ToggleTextureUpdatesButton);

  PlaySoundButton := TCastleButton.Create(Window);
  PlaySoundButton.Caption := 'Play Sound';
  PlaySoundButton.OnClick := @TDummy(nil).PlaySound;
  PlaySoundButton.Anchor(hpRight, -Margin);
  Window.Controls.InsertFront(PlaySoundButton);
  AnchorNextButton(PlaySoundButton);

  MyShaderEffect := Window.SceneManager.MainScene.RootNode.TryFindNodeByName(
    TEffectNode, 'MyShaderEffect', false) as TEffectNode;
  ToggleShaderButton.Pressed := (MyShaderEffect <> nil) and MyShaderEffect.Enabled;

  MyScreenEffect := Window.SceneManager.MainScene.RootNode.TryFindNodeByName(
    TScreenEffectNode, 'MyScreenEffect', false) as TScreenEffectNode;
  ToggleScreenEffectButton.Pressed := (MyScreenEffect <> nil) and MyScreenEffect.Enabled;

  Window.TouchInterface := tiCtlWalkDragRotate;

  { Test that FindFiles works also on Android asset filesystem. }
  FindFiles(ApplicationData(''), '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'skies', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle/', '*', true, @FindFilesCallback, nil, [ffRecursive]);

  SoundBuffer1 := SoundEngine.LoadBuffer(ApplicationData('sounds/werewolf_howling.wav'));
  SoundBuffer2 := SoundEngine.LoadBuffer(ApplicationData('sounds/player_potion_drink.wav'));
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, Format('FPS : %f (real : %f). Shapes : %d / %d',
   [Window.Fps.FrameTime,
    Window.Fps.RealTime,
    Window.SceneManager.Statistics.ShapesRendered,
    Window.SceneManager.Statistics.ShapesVisible]));
end;

function MyGetApplicationName: string;
begin
  Result := 'androiddemo';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { test: this is forbidden on Android.
    You cannot open files before Application.OnInitialize happened.
  LoadImage(ApplicationData('sample_texture.ppm')).Free;
  }

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
end.
