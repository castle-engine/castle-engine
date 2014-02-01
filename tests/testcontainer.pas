{
  Copyright 2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test TUIContainer. }
unit TestContainer;

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

uses CastleControls, CastleProgress, CastleWindowProgress, CastleImages;

procedure WindowOpen1(Container: TUIContainer);
var
  I: Integer;
begin
  { Note that we do a progress bar from OnOpen, before Window.SceneManager
    or Button had a chance to receive ContainerResize or even GLContextOpen
    calls. And progress will do a SaveScreen. }
  Progress.Init(100, 'Please wait...');
  try
    for I := 1 to 100 do
      Progress.Step;
  finally Progress.Fini end;
end;

procedure TTestContainer.TestProgressFromOpen;
var
  Button: TCastleButton;
  Window: TCastleWindow;
begin
  Window := TCastleWindow.Create(nil);
  try
    Button := TCastleButton.Create(Window);
    Window.Controls.InsertFront(Button);
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

var
  Window2: TCastleWindow;

procedure WindowOpen2(Container: TUIContainer);
var
  Image: TCastleImage;
begin
  Image := Window2.SaveScreen;
  FreeAndNil(Image);
end;

procedure TTestContainer.TestSaveScreenFromOpen;
var
  Button: TCastleButton;
begin
  Window2 := TCastleWindow.Create(nil);
  try
    Button := TCastleButton.Create(Window2);
    Window2.Controls.InsertFront(Button);
    Window2.OnOpen := @WindowOpen2;
    Application.MainWindow := Window2;
    Progress.UserInterface := WindowProgressInterface;

    Window2.Open;
    Window2.Close;
  finally
    FreeAndNil(Window2);
    Application.MainWindow := nil;
    Progress.UserInterface := ProgressNullInterface;
  end;
end;

var
  Window3: TCastleWindow;

procedure WindowOpen3(Container: TUIContainer);
begin
  Window3.SceneManager.LoadLevel('level_without_loading_image');
end;

procedure TTestContainer.TestLoadLevelFromOpen;

  procedure DoTest(const WithButton: boolean);
  var
    Button: TCastleButton;
  begin
    Window3 := TCastleWindow.Create(nil);
    try
      if WithButton then
      begin
        Button := TCastleButton.Create(Window3);
        Window3.Controls.InsertFront(Button);
      end;
      Window3.OnOpen := @WindowOpen3;
      Application.MainWindow := Window3;
      Progress.UserInterface := WindowProgressInterface;

      Window3.Open;
      Window3.Close;
    finally
      FreeAndNil(Window3);
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
