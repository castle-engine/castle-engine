{ -*- compile-command: "sh compile.sh" -*- }
{
  Copyright 2013 Michalis Kamburelis.

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
  CastleSceneCore, CastleFindFiles, CastleStringUtils;

var
  {$ifdef SOLID_BACKGROUND}
  Background: TCastleSimpleBackground;
  {$endif}
  Image: TCastleImageControl;
  ToggleShaderButton: TCastleButton;
  TouchUIButton: TCastleButton;
  MyShaderEffect: TEffectNode;

type
  TDummy = class
    procedure ToggleShaderClick(Sender: TObject);
    procedure TouchUIClick(Sender: TObject);
  end;

procedure TDummy.ToggleShaderClick(Sender: TObject);
begin
  if MyShaderEffect <> nil then
    MyShaderEffect.FdEnabled.Send(not MyShaderEffect.FdEnabled.Value);
end;

procedure TDummy.TouchUIClick(Sender: TObject);
begin
  if Window.TouchInterface = High(TTouchCtlInterface) then
    Window.TouchInterface := Low(TTouchCtlInterface) else
    Window.TouchInterface := Succ(Window.TouchInterface);
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

{$ifdef SOLID_BACKGROUND}
  { Show other controls under SceneManager, this way our Background
    is visible. Otherwise, Background defined in main 3D scene is used. }
  Window.SceneManager.Transparent := true;

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertBack(Background);
{$endif}

  Image := TCastleImageControl.Create(Window);
  // TODO: png support for Android
  // TODO: read files using Anroid assets:
  // http://stackoverflow.com/questions/13317387/how-to-get-file-in-assets-from-android-ndk
//    Image.Image := TouchCtlOuter.MakeCopy;
  Image.URL := ApplicationData('sample_image_with_alpha.png' {'sample_texture.ppm'});
  Window.Controls.InsertFront(Image);

  Window.Load(ApplicationData('castle_with_lights_and_camera.wrl'));
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  ToggleShaderButton := TCastleButton.Create(Window);
  ToggleShaderButton.Caption := 'Toggle Shader Effect';
  ToggleShaderButton.OnClick := @TDummy(nil).ToggleShaderClick;
  Window.Controls.InsertFront(ToggleShaderButton);

  TouchUIButton := TCastleButton.Create(Window);
  TouchUIButton.Caption := 'Next Touch UI';
  TouchUIButton.OnClick := @TDummy(nil).TouchUIClick;
  Window.Controls.InsertFront(TouchUIButton);

  MyShaderEffect := Window.SceneManager.MainScene.RootNode.TryFindNodeByName(
    TEffectNode, 'MyShaderEffect', false) as TEffectNode;

  Window.TouchInterface := etciCtlWalkDragRotate;

  { Test that FindFiles works also on Android asset filesystem. }
  FindFiles(ApplicationData(''), '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'skies', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle', '*', true, @FindFilesCallback, nil, [ffRecursive]);
  FindFiles(ApplicationData('') + 'textures/castle/', '*', true, @FindFilesCallback, nil, [ffRecursive]);
end;

procedure WindowResize(Sender: TCastleWindowBase);
const
  Margin = 10;
begin
  Image.Left := Margin;
  Image.Bottom := Window.Height - Image.Height - Margin;

  ToggleShaderButton.Left := (Window.Width - ToggleShaderButton.Width) div 2;
  ToggleShaderButton.Bottom := Window.Height - ToggleShaderButton.Height - Margin;

  TouchUIButton.Left := Window.Width - TouchUIButton.Width - Margin;
  TouchUIButton.Bottom := Window.Height - TouchUIButton.Height - Margin;
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
  Window.OnResize := @WindowResize;
end.
