{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleNotifications, CastleViewport, X3DNodes, CastleScene,
  CastleSoundEngine;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
    ButtonToggleShader: TCastleButton;
    ButtonToggleScreenEffect: TCastleButton;
    ButtonToggleSSAO: TCastleButton;
    ButtonTouchNavigation: TCastleButton;
    ButtonMessage: TCastleButton;
    ButtonProgress: TCastleButton;
    ButtonReopenContext: TCastleButton;
    ButtonToggleTextureUpdates: TCastleButton;
    ButtonPlaySoundWav: TCastleButton;
    ButtonPlaySoundOgg: TCastleButton;
    ButtonVibrate: TCastleButton;
    ButtonTerminate: TCastleButton;
    StatusText: TCastleLabel;
    TouchNavigation: TCastleTouchNavigation;
    MainViewport: TCastleViewport;
    MainScene: TCastleScene;

    { Other fields, initialized in Start }
    MyShaderEffect: TEffectNode;
    MyScreenEffect: TScreenEffectNode;
    SoundBufferWav, SoundBufferOgg: TSoundBuffer;

    procedure ClickToggleShader(Sender: TObject);
    procedure ClickToggleScreenEffect(Sender: TObject);
    procedure ClickToggleSSAO(Sender: TObject);
    procedure ClickTouchNavigation(Sender: TObject);
    procedure ClickMessage(Sender: TObject);
    procedure ClickProgress(Sender: TObject);
    procedure ClickReopenContext(Sender: TObject);
    procedure ClickToggleTextureUpdates(Sender: TObject);
    procedure ToggleTextureUpdatesCallback(Node: TX3DNode);
    procedure ClickPlaySoundWav(Sender: TObject);
    procedure ClickPlaySoundOgg(Sender: TObject);
    procedure ClickVibrate(Sender: TObject);
    procedure ClickTerminate(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleProgress, CastleWindow, CastleFilesUtils, CastleFindFiles,
  CastleOpenDocument, CastleMessages, CastleLog, CastleApplicationProperties, CastleUtils;

procedure FindFilesCallback(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  WritelnLog('FindFiles', 'Found URL:%s, Name:%s, AbsoluteName:%s, Directory:%s',
    [FileInfo.URL, FileInfo.Name, FileInfo.AbsoluteName, BoolToStr(FileInfo.Directory, true)]);
end;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  StatusText := UiOwner.FindRequiredComponent('StatusText') as TCastleLabel;
  ButtonToggleShader := UiOwner.FindRequiredComponent('ButtonToggleShader') as TCastleButton;
  ButtonToggleScreenEffect := UiOwner.FindRequiredComponent('ButtonToggleScreenEffect') as TCastleButton;
  ButtonToggleSSAO := UiOwner.FindRequiredComponent('ButtonToggleSSAO') as TCastleButton;
  ButtonTouchNavigation := UiOwner.FindRequiredComponent('ButtonTouchNavigation') as TCastleButton;
  ButtonMessage := UiOwner.FindRequiredComponent('ButtonMessage') as TCastleButton;
  ButtonProgress := UiOwner.FindRequiredComponent('ButtonProgress') as TCastleButton;
  ButtonReopenContext := UiOwner.FindRequiredComponent('ButtonReopenContext') as TCastleButton;
  ButtonToggleTextureUpdates := UiOwner.FindRequiredComponent('ButtonToggleTextureUpdates') as TCastleButton;
  ButtonPlaySoundWav := UiOwner.FindRequiredComponent('ButtonPlaySoundWav') as TCastleButton;
  ButtonPlaySoundOgg := UiOwner.FindRequiredComponent('ButtonPlaySoundOgg') as TCastleButton;
  ButtonVibrate := UiOwner.FindRequiredComponent('ButtonVibrate') as TCastleButton;
  ButtonTerminate := UiOwner.FindRequiredComponent('ButtonTerminate') as TCastleButton;
  TouchNavigation := UiOwner.FindRequiredComponent('TouchNavigation') as TCastleTouchNavigation;
  MainViewport := UiOwner.FindRequiredComponent('MainViewport') as TCastleViewport;
  MainScene := UiOwner.FindRequiredComponent('MainScene') as TCastleScene;

  { assign events }
  ButtonToggleShader.OnClick := @ClickToggleShader;
  ButtonToggleScreenEffect.OnClick := @ClickToggleScreenEffect;
  ButtonToggleSSAO.OnClick := @ClickToggleSSAO;
  ButtonTouchNavigation.OnClick := @ClickTouchNavigation;
  ButtonMessage.OnClick := @ClickMessage;
  ButtonProgress.OnClick := @ClickProgress;
  ButtonReopenContext.OnClick := @ClickReopenContext;
  ButtonToggleTextureUpdates.OnClick := @ClickToggleTextureUpdates;
  ButtonPlaySoundWav.OnClick := @ClickPlaySoundWav;
  ButtonPlaySoundOgg.OnClick := @ClickPlaySoundOgg;
  ButtonVibrate.OnClick := @ClickVibrate;
  ButtonTerminate.OnClick := @ClickTerminate;

  { configure components }
  ButtonMessage.Exists := ApplicationProperties.PlatformAllowsModalRoutines;
  ButtonProgress.Exists := ApplicationProperties.PlatformAllowsModalRoutines;
  ButtonTerminate.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  TouchNavigation.TouchInterface := tiWalk;

  { initialize other fields }

  MyShaderEffect := MainScene.RootNode.TryFindNodeByName(
    TEffectNode, 'MyShaderEffect', false) as TEffectNode;
  ButtonToggleShader.Pressed := (MyShaderEffect <> nil) and MyShaderEffect.Enabled;

  MyScreenEffect := MainScene.RootNode.TryFindNodeByName(
    TScreenEffectNode, 'MyScreenEffect', false) as TScreenEffectNode;
  ButtonToggleScreenEffect.Pressed := (MyScreenEffect <> nil) and MyScreenEffect.Enabled;

  SoundBufferWav := SoundEngine.LoadBuffer('castle-data:/sounds/player_potion_drink.wav');

  try
    SoundBufferOgg := SoundEngine.LoadBuffer('castle-data:/sounds/werewolf_howling.ogg');
  except
    on E: ESoundFileError do
      WritelnWarning('OggVorbis loading failed: ' + E.Message);
  end;

  { Test that FindFiles works also on Android asset filesystem.
    These calls don't do anything (they merely output some log messages about found files). }
  FindFiles('castle-data:/', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/skies', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/textures/castle', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/textures/castle/', '*', true, @FindFilesCallback, nil, [ffRecursive]);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  TouchInterfaceStr: String;
begin
  inherited;
  WriteStr(TouchInterfaceStr, TouchNavigation.TouchInterface);

  StatusText.Caption := Format('FPS : %s' + NL +
    'Shapes : %d / %d' + NL +
    'Touch Navigation: %s', [
    Container.Fps.ToString,
    MainViewport.Statistics.ShapesRendered,
    MainViewport.Statistics.ShapesVisible,
    TouchInterfaceStr
  ]);
end;

procedure TStateMain.ClickToggleShader(Sender: TObject);
begin
  if MyShaderEffect <> nil then
  begin
    MyShaderEffect.Enabled := not MyShaderEffect.Enabled;
    ButtonToggleShader.Pressed := MyShaderEffect.Enabled;
  end;
end;

procedure TStateMain.ClickToggleScreenEffect(Sender: TObject);
begin
  if MyScreenEffect <> nil then
  begin
    MyScreenEffect.Enabled := not MyScreenEffect.Enabled;
    ButtonToggleScreenEffect.Pressed := MyScreenEffect.Enabled;
  end;
end;

procedure TStateMain.ClickToggleSSAO(Sender: TObject);
begin
  MainViewport.ScreenSpaceAmbientOcclusion :=
    not MainViewport.ScreenSpaceAmbientOcclusion;
  ButtonToggleSSAO.Pressed := MainViewport.ScreenSpaceAmbientOcclusion;
end;

procedure TStateMain.ClickTouchNavigation(Sender: TObject);
begin
  if TouchNavigation.TouchInterface = High(TTouchInterface) then
    TouchNavigation.TouchInterface := Low(TTouchInterface)
  else
    TouchNavigation.TouchInterface := Succ(TouchNavigation.TouchInterface);
end;

procedure TStateMain.ClickMessage(Sender: TObject);
begin
  { On Android, a nice test is to switch to desktop (home)
    when one of these modal MessageXxx is working. The application loop
    (done inside MessageXxx, they call Application.ProcessMessage in a loop)
    will still work, even though the window is closed.
    When user gets back to our app, she/he will see the message box again. }
  if MessageYesNo(Application.MainWindow, 'Test of a yes/no message test.' + NL + NL +' Do you want to deliberately cause an exception (to test our CastleWindow.HandleException method)?') then
  begin
    MessageOK(Application.MainWindow, 'You clicked "Yes". Raising an exception, get ready!');
    raise Exception.Create('Test exception');
  end else
    MessageOK(Application.MainWindow, 'You clicked "No".');
end;

procedure TStateMain.ClickProgress(Sender: TObject);
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

procedure TStateMain.ClickReopenContext(Sender: TObject);
begin
  Application.MainWindow.Close(false);
  Application.MainWindow.Open;
end;

procedure TStateMain.ToggleTextureUpdatesCallback(Node: TX3DNode);
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

procedure TStateMain.ClickToggleTextureUpdates(Sender: TObject);
begin
  MainScene.RootNode.EnumerateNodes(
    TGeneratedCubeMapTextureNode, @ToggleTextureUpdatesCallback, false);
end;

procedure TStateMain.ClickPlaySoundWav(Sender: TObject);
begin
  SoundEngine.PlaySound(SoundBufferWav);
end;

procedure TStateMain.ClickPlaySoundOgg(Sender: TObject);
begin
  SoundEngine.PlaySound(SoundBufferOgg);
end;

procedure TStateMain.ClickVibrate(Sender: TObject);
begin
  Vibrate(200);
end;

procedure TStateMain.ClickTerminate(Sender: TObject);
begin
  Application.Terminate;
end;

end.
