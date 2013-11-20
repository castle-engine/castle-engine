{ -*- compile-command: "sh compile.sh" -*- }
{
  Copyright 2004-2013 Michalis Kamburelis.

  This file is part of "interpolated_curves".

  "interpolated_curves" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "interpolated_curves" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "interpolated_curves"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Simple 3D game. This unit implements the game logic,
  actually independent from Android. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils, CastleControls, CastleUIControls, CastleRectangles,
  CastleGLUtils, CastleColors;

type
  { Define 2D control with custom drawing code, just to show that we can. }
  T2DControls = class(TUIControl)
  public
    procedure Draw; override;
    function DrawStyle: TUIControlDrawStyle; override;
  end;

function T2DControls.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure T2DControls.Draw;
begin
  DrawRectangle(Rectangle(
    Application.ScreenWidth - 100,
    Application.ScreenHeight - 100, 80, 80), Blue);
end;

var
  Background: TCastleSimpleBackground;
  MyControl: T2DControls;
  Image: TCastleImageControl;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  { Show other controls under SceneManager, this way our Background
    is visible. Otherwise, Background defined in main 3D scene is used. }
  Window.SceneManager.Transparent := true;

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertBack(Background);

  MyControl := T2DControls.Create(Window);
  Window.Controls.InsertFront(MyControl);

  Image := TCastleImageControl.Create(Window);
  // TODO: png support for Android
  // TODO: read files using Anroid assets:
  // http://stackoverflow.com/questions/13317387/how-to-get-file-in-assets-from-android-ndk
//    Image.Image := TouchCtlOuter.MakeCopy;
  Image.URL := 'file:///sdcard/kambitest/sample_texture.ppm';
  Window.Controls.InsertFront(Image);

  Window.Load('file:///sdcard/kambitest/castle_with_lights_and_camera.wrl');
end;

procedure WindowResize(Sender: TCastleWindowBase);
begin
  Image.Left := 10;
  Image.Bottom := Application.ScreenHeight - 300;
end;

function MyGetApplicationName: string;
begin
  Result := 'cge_android_lib';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Window.OnResize := @WindowResize;
end.
