// -*- compile-command: "./test_single_testcase.sh TTestCastleWindowOpen" -*-
{
  Copyright 2014-2022 Michalis Kamburelis.

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

uses Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry
  {$else}CastleTester{$endif}, CastleWindow;

type
  TTestCastleWindowOpen = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestSaveScreenFromOpen;
    procedure TestLoadLevelFromOpen;
  end;

implementation

uses CastleControls, CastleImages,
  CastleUIControls, CastleViewport, CastleLevels;

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
  Window: TCastleWindow;
begin
  {$ifdef CASTLE_TESTER}
  if not IsConsoleMode then
    Exit; // TODO: We can test window progress only in console mode
  {$endif}

  Window := TCastleWindow.Create(nil);
  try
    Window.Controls.InsertFront(TControl2.Create(Window));
    Window.Controls.InsertFront(TCastleButton.Create(Window));
    Window.Controls.InsertFront(TControl2.Create(Window));
    Window.OnOpen := @WindowOpen2;
    Application.MainWindow := Window;

    Window.Open;
    Window.Close;
  finally
    FreeAndNil(Window);
    Application.MainWindow := nil;
  end;
end;

type
  TCastleWindowWithSceneManager = class(TCastleWindow)
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

      Window.Open;
      Window.Close;
    finally
      FreeAndNil(Window);
      Application.MainWindow := nil;
    end;
  end;

begin
  {$ifdef CASTLE_TESTER}
  if not IsConsoleMode then
    Exit; // TODO: We can test window progress only in console mode
  {$endif}

  Levels.LoadFromFiles('castle-data:/game/level_without_loading_image');
  DoTest(false);
  DoTest(true);
end;

initialization
  RegisterTest(TTestCastleWindowOpen);
end.
