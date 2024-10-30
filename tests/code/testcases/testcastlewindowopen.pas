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

{ Test doing various stuff when GL context is open. }
unit TestCastleWindowOpen;

interface

uses Classes, SysUtils,
  CastleTester, CastleWindow;

type
  TTestCastleWindowOpen = class(TCastleTestCase)
  private
    procedure WindowOpen2(Sender: TObject);
    procedure WindowOpen3(Sender: TObject);
  published
    procedure TestSaveScreenFromOpen;
    procedure TestLoadLevelFromOpen;
  end;

implementation

uses CastleControls, CastleImages,
  CastleUIControls, CastleViewport, CastleLevels;

type
  TEventControl = class(TCastleUserInterface)
  public
    OnGLContextOpen: TNotifyEvent;
    procedure GLContextOpen; override;
  end;

  TControl2 = class(TCastleUserInterface)
    procedure GLContextOpen; override;
  end;

procedure TEventControl.GLContextOpen;
begin
  if Assigned(OnGLContextOpen) then
    OnGLContextOpen(Self);
end;

procedure TControl2.GLContextOpen;
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure TTestCastleWindowOpen.WindowOpen2(Sender: TObject);
var
  Image: TCastleImage;
begin
  Image := Application.MainWindow.SaveScreen;
  FreeAndNil(Image);
end;

procedure TTestCastleWindowOpen.TestSaveScreenFromOpen;
var
  Window: TCastleWindow;
  EventControl: TEventControl;
begin
  if not CanCreateWindowForTest then
    Exit;

  Window := CreateWindowForTest;
  try
    EventControl := TEventControl.Create(Window);
    EventControl.OnGLContextOpen := {$ifdef FPC}@{$endif} WindowOpen2;
    Window.Controls.InsertFront(EventControl);

    Window.Controls.InsertFront(TControl2.Create(Window));
    Window.Controls.InsertFront(TCastleButton.Create(Window));
    Window.Controls.InsertFront(TControl2.Create(Window));
    Application.MainWindow := Window;

    Window.Open;
    Window.Close;
  finally
    DestroyWindowForTest(Window);
  end;
end;

var
  // Avalable during TTestCastleWindowOpen.TestLoadLevelFromOpen
  SceneManagerForLoadLevel: TGameSceneManager;

type
  TControl3 = class(TCastleUserInterface)
    procedure GLContextOpen; override;
  end;

procedure TControl3.GLContextOpen;
begin
  SceneManagerForLoadLevel.LoadLevel('level_without_loading_image');
end;

procedure TTestCastleWindowOpen.WindowOpen3(Sender: TObject);
begin
  SceneManagerForLoadLevel.LoadLevel('level_without_loading_image');
end;

procedure TTestCastleWindowOpen.TestLoadLevelFromOpen;

  procedure DoTest(const WithButton: boolean);
  var
    Window: TCastleWindow;
    EventControl: TEventControl;
  begin
    Window := CreateWindowForTest;
    try
      EventControl := TEventControl.Create(Window);
      EventControl.OnGLContextOpen := {$ifdef FPC}@{$endif} WindowOpen3;
      Window.Controls.InsertFront(EventControl);

      SceneManagerForLoadLevel := TGameSceneManager.Create(Window);
      SceneManagerForLoadLevel.FullSize := true;
      Window.Controls.InsertFront(SceneManagerForLoadLevel);

      Window.Controls.InsertFront(TControl3.Create(Window));
      if WithButton then
        Window.Controls.InsertFront(TCastleButton.Create(Window));
      Window.Controls.InsertFront(TControl3.Create(Window));

      Window.Open;
      Window.Close;
    finally
      DestroyWindowForTest(Window);
    end;
  end;

begin
  if not CanCreateWindowForTest then
    Exit;

  Levels.LoadFromFiles('castle-data:/game/level_without_loading_image');
  DoTest(false);
  DoTest(true);
end;

initialization
  RegisterTest(TTestCastleWindowOpen);
end.
