// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleWindowOpen" -*-
{
  Copyright 2014-2021 Michalis Kamburelis.

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

uses Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  {$else}CastleTester{$endif}, CastleWindow;

type
  TTestCastleWindowOpen = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestProgressFromOpen;
    procedure TestSaveScreenFromOpen;
    procedure TestLoadLevelFromOpen;
  end;

implementation

uses CastleControls, CastleProgress, CastleWindowProgress, CastleImages,
  CastleUIControls, CastleViewport, CastleLevels;

type
  TControl1 = class(TCastleUserInterface)
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

procedure WindowOpen1(Container: TCastleContainer);
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

procedure TTestCastleWindowOpen.TestProgressFromOpen;
var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(nil);
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
  TControl2 = class(TCastleUserInterface)
    procedure GLContextOpen; override;
  end;

procedure TControl2.GLContextOpen;
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure WindowOpen2(Container: TCastleContainer);
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure TTestCastleWindowOpen.TestSaveScreenFromOpen;
var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(nil);
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
  TCastleWindowWithSceneManager = class(TCastleWindowBase)
    SceneManager: TGameSceneManager;
    constructor Create(AOwner: TComponent); override;
  end;

constructor TCastleWindowWithSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  SceneManager := TGameSceneManager.Create(Self);
  SceneManager.FullSize := true;
  Controls.InsertFront(SceneManager);
end;

type
  TControl3 = class(TCastleUserInterface)
    procedure GLContextOpen; override;
  end;

procedure TControl3.GLContextOpen;
begin
  (Application.MainWindow as TCastleWindowWithSceneManager).SceneManager.LoadLevel('level_without_loading_image');
end;

procedure WindowOpen3(Container: TCastleContainer);
begin
  (Application.MainWindow as TCastleWindowWithSceneManager).SceneManager.LoadLevel('level_without_loading_image');
end;

procedure TTestCastleWindowOpen.TestLoadLevelFromOpen;

  procedure DoTest(const WithButton: boolean);
  var
    Window: TCastleWindowWithSceneManager;
  begin
    Window := TCastleWindowWithSceneManager.Create(nil);
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

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleWindowOpen);
{$endif}
end.
