{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test doing various stuff from TCastleWindow.OnOpen. }
unit TestCastleWindowOpen;

interface

uses Classes, SysUtils, fpcunit, testutils, testregistry, CastleWindow;

type
  TTestContainer = class(TTestCase)
  published
    procedure TestProgressFromOpen;
    procedure TestSaveScreenFromOpen;
    procedure TestLoadLevelFromOpen;
  end;

implementation

uses CastleControls, CastleProgress, CastleWindowProgress, CastleImages,
  CastleUIControls;

type
  TControl1 = class(TUIControl)
    procedure GLContextOpen; override;
  end;

procedure TControl1.GLContextOpen;
var
  I: Integer;
begin
  { We do a progress bar from GLContextOpen, before all GLContextOpen
    calls on other controls (Button, SceneManager).
    And progress will do a SaveScreen, forcing rendering of all controls. }
  Progress.Init(100, 'Please wait...');
  try
    for I := 1 to 100 do
      Progress.Step;
  finally Progress.Fini end;
end;

procedure WindowOpen1(Container: TUIContainer);
var
  I: Integer;
begin
  { We do a progress bar from OnOpen, it will do a SaveScreen. }
  Progress.Init(100, 'Please wait...');
  try
    for I := 1 to 100 do
      Progress.Step;
  finally Progress.Fini end;
end;

procedure TTestContainer.TestProgressFromOpen;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(nil);
  try
    Window.Controls.InsertFront(TControl1.Create(Window));
    Window.Controls.InsertFront(TCastleButton.Create(Window));
    Window.Controls.InsertFront(TControl1.Create(Window));
    Window.OnOpen := @WindowOpen1;
    Application.MainWindow := Window;
    Progress.UserInterface := WindowProgressInterface;

    Window.Open;
    Window.Close;
  finally
    FreeAndNil(Window);
    Application.MainWindow := nil;
    Progress.UserInterface := ProgressNullInterface;
  end;
end;

type
  TControl2 = class(TUIControl)
    procedure GLContextOpen; override;
  end;

procedure TControl2.GLContextOpen;
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure WindowOpen2(Container: TUIContainer);
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure TTestContainer.TestSaveScreenFromOpen;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(nil);
  try
    Window.Controls.InsertFront(TControl2.Create(Window));
    Window.Controls.InsertFront(TCastleButton.Create(Window));
    Window.Controls.InsertFront(TControl2.Create(Window));
    Window.OnOpen := @WindowOpen2;
    Application.MainWindow := Window;
    Progress.UserInterface := WindowProgressInterface;

    Window.Open;
    Window.Close;
  finally
    FreeAndNil(Window);
    Application.MainWindow := nil;
    Progress.UserInterface := ProgressNullInterface;
  end;
end;

type
  TControl3 = class(TUIControl)
    procedure GLContextOpen; override;
  end;

procedure TControl3.GLContextOpen;
begin
  (Application.MainWindow as TCastleWindow).SceneManager.LoadLevel('level_without_loading_image');
end;

procedure WindowOpen3(Container: TUIContainer);
begin
  (Application.MainWindow as TCastleWindow).SceneManager.LoadLevel('level_without_loading_image');
end;

procedure TTestContainer.TestLoadLevelFromOpen;

  procedure DoTest(const WithButton: boolean);
  var
    Window: TCastleWindow;
  begin
    Window := TCastleWindow.Create(nil);
    try
      Window.Controls.InsertFront(TControl3.Create(Window));
      if WithButton then
        Window.Controls.InsertFront(TCastleButton.Create(Window));
      Window.Controls.InsertFront(TControl3.Create(Window));
      Window.OnOpen := @WindowOpen3;
      Application.MainWindow := Window;
      Progress.UserInterface := WindowProgressInterface;

      Window.Open;
      Window.Close;
    finally
      FreeAndNil(Window);
      Application.MainWindow := nil;
      Progress.UserInterface := ProgressNullInterface;
    end;
  end;

begin
  DoTest(false);
  DoTest(true);
end;

initialization
  RegisterTest(TTestContainer);
end.
