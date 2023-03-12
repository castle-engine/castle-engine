{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleNotifications, CastleViewport, X3DNodes, CastleScene,
  CastleSoundEngine;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonToggleShader: TCastleButton;
    ButtonToggleScreenEffect: TCastleButton;
    ButtonToggleSSAO: TCastleButton;
    ButtonTouchNavigation: TCastleButton;
    ButtonMessage: TCastleButton;
    ButtonReopenContext: TCastleButton;
    ButtonToggleTextureUpdates: TCastleButton;
    ButtonPlaySoundWav: TCastleButton;
    ButtonPlaySoundOgg: TCastleButton;
    ButtonVibrate: TCastleButton;
    ButtonTerminate: TCastleButton;
    StatusText: TCastleLabel;
    TouchNavigation: TCastleTouchNavigation;
    MainViewport: TCastleViewport;
    SceneCastle, SceneTeapots: TCastleScene;
    SoundWav, SoundOgg: TCastleSound;
  private
    { Other fields, initialized in Start }
    MyShaderEffect: TEffectNode;
    MyScreenEffect: TScreenEffectNode;

    procedure ClickToggleShader(Sender: TObject);
    procedure ClickToggleScreenEffect(Sender: TObject);
    procedure ClickToggleSSAO(Sender: TObject);
    procedure ClickTouchNavigation(Sender: TObject);
    procedure ClickMessage(Sender: TObject);
    procedure ClickReopenContext(Sender: TObject);
    procedure ClickToggleTextureUpdates(Sender: TObject);
    procedure ToggleTextureUpdatesCallback(Node: TX3DNode);
    procedure ClickPlaySoundWav(Sender: TObject);
    procedure ClickPlaySoundOgg(Sender: TObject);
    procedure ClickVibrate(Sender: TObject);
    procedure ClickTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, TypInfo,
  CastleWindow, CastleFilesUtils, CastleFindFiles,
  CastleOpenDocument, CastleMessages, CastleLog, CastleApplicationProperties, CastleUtils;

procedure FindFilesCallback(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  WritelnLog('FindFiles', 'Found URL:%s, Name:%s, AbsoluteName:%s, Directory:%s',
    [FileInfo.URL, FileInfo.Name, FileInfo.AbsoluteName, BoolToStr(FileInfo.Directory, true)]);
end;

function TouchInterfaceToStr(const Value: TTouchInterface): String;
begin
  Result := GetEnumName(TypeInfo(TTouchInterface), Ord(Value));
end;

function TextureUpdateToStr(const Value: TTextureUpdate): String;
begin
  Result := GetEnumName(TypeInfo(TTextureUpdate), Ord(Value));
end;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { assign events }
  ButtonToggleShader.OnClick := {$ifdef FPC}@{$endif}ClickToggleShader;
  ButtonToggleScreenEffect.OnClick := {$ifdef FPC}@{$endif}ClickToggleScreenEffect;
  ButtonToggleSSAO.OnClick := {$ifdef FPC}@{$endif}ClickToggleSSAO;
  ButtonTouchNavigation.OnClick := {$ifdef FPC}@{$endif}ClickTouchNavigation;
  ButtonMessage.OnClick := {$ifdef FPC}@{$endif}ClickMessage;
  ButtonReopenContext.OnClick := {$ifdef FPC}@{$endif}ClickReopenContext;
  ButtonToggleTextureUpdates.OnClick := {$ifdef FPC}@{$endif}ClickToggleTextureUpdates;
  ButtonPlaySoundWav.OnClick := {$ifdef FPC}@{$endif}ClickPlaySoundWav;
  ButtonPlaySoundOgg.OnClick := {$ifdef FPC}@{$endif}ClickPlaySoundOgg;
  ButtonVibrate.OnClick := {$ifdef FPC}@{$endif}ClickVibrate;
  ButtonTerminate.OnClick := {$ifdef FPC}@{$endif}ClickTerminate;

  { configure components }
  ButtonMessage.Exists := ApplicationProperties.PlatformAllowsModalRoutines;
  ButtonTerminate.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  TouchNavigation.TouchInterface := tiWalk;

  { initialize other fields }

  MyShaderEffect := SceneCastle.Node('MyShaderEffect') as TEffectNode;
  ButtonToggleShader.Pressed := (MyShaderEffect <> nil) and MyShaderEffect.Enabled;

  MyScreenEffect := SceneCastle.Node('MyScreenEffect') as TScreenEffectNode;
  ButtonToggleScreenEffect.Pressed := (MyScreenEffect <> nil) and MyScreenEffect.Enabled;

  { Test that FindFiles works also on Android asset filesystem.
    These calls don't do anything (they merely output some log messages about found files). }
  FindFiles('castle-data:/', '*', true, {$ifdef FPC}@{$endif}FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/skies', '*', true, {$ifdef FPC}@{$endif}FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/textures/castle', '*', true, {$ifdef FPC}@{$endif}FindFilesCallback, nil, [ffRecursive]);
  FindFiles('castle-data:/textures/castle/', '*', true, {$ifdef FPC}@{$endif}FindFilesCallback, nil, [ffRecursive]);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  StatusText.Caption := Format('FPS : %s' + NL +
    'Shapes : %d / %d' + NL +
    'Touch Navigation: %s', [
    Container.Fps.ToString,
    MainViewport.Statistics.ShapesRendered,
    MainViewport.Statistics.ShapesVisible,
    TouchInterfaceToStr(TouchNavigation.TouchInterface)
  ]);
end;

procedure TViewMain.ClickToggleShader(Sender: TObject);
begin
  if MyShaderEffect <> nil then
  begin
    MyShaderEffect.Enabled := not MyShaderEffect.Enabled;
    ButtonToggleShader.Pressed := MyShaderEffect.Enabled;
  end;
end;

procedure TViewMain.ClickToggleScreenEffect(Sender: TObject);
begin
  if MyScreenEffect <> nil then
  begin
    MyScreenEffect.Enabled := not MyScreenEffect.Enabled;
    ButtonToggleScreenEffect.Pressed := MyScreenEffect.Enabled;
  end;
end;

procedure TViewMain.ClickToggleSSAO(Sender: TObject);
begin
  MainViewport.ScreenSpaceAmbientOcclusion :=
    not MainViewport.ScreenSpaceAmbientOcclusion;
  ButtonToggleSSAO.Pressed := MainViewport.ScreenSpaceAmbientOcclusion;
end;

procedure TViewMain.ClickTouchNavigation(Sender: TObject);
begin
  if TouchNavigation.TouchInterface = High(TTouchInterface) then
    TouchNavigation.TouchInterface := Low(TTouchInterface)
  else
    TouchNavigation.TouchInterface := Succ(TouchNavigation.TouchInterface);
end;

procedure TViewMain.ClickMessage(Sender: TObject);
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

procedure TViewMain.ClickReopenContext(Sender: TObject);
begin
  Application.MainWindow.Close(false);
  Application.MainWindow.Open;
end;

procedure TViewMain.ToggleTextureUpdatesCallback(Node: TX3DNode);
var
  CubeMap: TGeneratedCubeMapTextureNode;
begin
  CubeMap := Node as TGeneratedCubeMapTextureNode;
  if CubeMap.Update = upNone then
    CubeMap.Update := upAlways else
    CubeMap.Update := upNone;
  WritelnLog('CubeMap', 'Toggled updates on ' + CubeMap.NiceName +
    ' to ' + TextureUpdateToStr(CubeMap.Update));
end;

procedure TViewMain.ClickToggleTextureUpdates(Sender: TObject);
begin
  SceneTeapots.RootNode.EnumerateNodes(
    TGeneratedCubeMapTextureNode, {$ifdef FPC}@{$endif}ToggleTextureUpdatesCallback, false);
end;

procedure TViewMain.ClickPlaySoundWav(Sender: TObject);
begin
  SoundEngine.Play(SoundWav);
end;

procedure TViewMain.ClickPlaySoundOgg(Sender: TObject);
begin
  SoundEngine.Play(SoundOgg);
end;

procedure TViewMain.ClickVibrate(Sender: TObject);
begin
  Vibrate(200);
end;

procedure TViewMain.ClickTerminate(Sender: TObject);
begin
  Application.Terminate;
end;

end.
