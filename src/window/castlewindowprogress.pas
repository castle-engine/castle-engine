{
  Copyright 2002-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Progress bar displayed in a TCastleWindowBase.

  Simply set @code(WindowProgressInterface.Window) to your TCastleWindowBase
  instance, and assign

@longCode(#  Progress.UserInterface := WindowProgressInterface;#)

  Between Progress.Init and Fini you shouldn't do anything with
  window set as @code(WindowProgressInterface.Window).
  It's callbacks will be temporarily swapped and it will be used
  to render progress bar.

  As usual, remember to always call Progress.Fini if you called
  Progress.Init. Progress.Fini restores original callbacks and OpenGL
  state of your window. Usually it's best and safest to use try..finally
  block like

@longCode(#  Progress.Init; try.....finally Progress.Fini; end; #) }


unit CastleWindowProgress;

{$I castleconf.inc}

interface

uses CastleWindow, CastleProgress, CastleWindowModes,
  CastleImages, CastleGLImages, CastleUIControls, CastleControls;

type
  TWindowProgressInterface = class(TProgressUserInterface)
  private
    Bar: TCastleProgressBar;
    FWindow: TCastleWindowCustom;
    SavedMode: TGLMode;
  public
    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TCastleWindowCustom read FWindow write FWindow;

    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use progress bar
    drawn on TCastleWindow.
    This instance is created in initialization, freed in finalization. }
  WindowProgressInterface: TWindowProgressInterface;

implementation

uses SysUtils, CastleUtils, CastleKeysMouse;

{ TWindowProgressInterface  ------------------------------------------------ }

procedure TWindowProgressInterface.Init(Progress: TProgress);
begin
  Check(Window <> nil,
    'TWindowProgressInterface: You must assign Window before doing Init');

  Bar := TCastleProgressBar.Create(nil);
  Bar.Progress := Progress;

  if Image <> nil then
    Bar.Background := Image.MakeCopy else
    Bar.Background := Window.SaveScreen;
  Bar.YPosition := BarYPosition;

  SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);

  Window.Controls.InsertFront(Bar);

  { init our window state }
  Window.AutoRedisplay := true;
  Window.Cursor := mcWait;
  { To actually draw progress start. }
  Window.PostRedisplay;
  Window.FlushRedisplay;
  Application.ProcessMessage(false, false);
end;

procedure TWindowProgressInterface.Update(Progress: TProgress);
begin
  Application.ProcessAllMessages;
end;

procedure TWindowProgressInterface.Fini(Progress: TProgress);
begin
  FreeAndNil(Bar);
  FreeAndNil(SavedMode);
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
