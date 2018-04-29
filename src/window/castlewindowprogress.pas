{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Progress bar displayed in a TCastleWindowCustom.

  Simply set @code(WindowProgressInterface.Window) to your TCastleWindowCustom
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
  { Progress bar rendered on OpenGL context (TCastleWindow).
    Uses Application.MainWindow to render the progress bar,
    so be sure to assign it.
    If the Application.MainWindow is not assigned, or not open,
    when progress bar starts --- we gracefully avoid showing any progress. }
  TWindowProgressInterface = class(TProgressUserInterface)
  private
    Bar: TCastleProgressBar;
    { Window used to render the progress bar, or nil if none.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    UsedWindow: TCastleWindowCustom;
    SavedMode: TGLMode;
    function GetWindow: TCastleWindowCustom;
    procedure SetWindow(const Value: TCastleWindowCustom);
  public
    { @deprecated Using this is deprecated, you should rather assign to
      Application.MainWindow. }
    property Window: TCastleWindowCustom read GetWindow write SetWindow; deprecated;

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

{$warnings off}
// TODO: This unit temporarily uses RenderingCamera singleton.
uses SysUtils, CastleUtils, CastleKeysMouse, CastleRenderingCamera;
{$warnings on}

{ TWindowProgressInterface  ------------------------------------------------ }

function TWindowProgressInterface.GetWindow: TCastleWindowCustom;
begin
  Result := Application.MainWindow;
end;

procedure TWindowProgressInterface.SetWindow(const Value: TCastleWindowCustom);
begin
  Application.MainWindow := Value;
end;

procedure TWindowProgressInterface.Init(Progress: TProgress);
begin
  if (Application.MainWindow <> nil) and
      Application.MainWindow.GLInitialized and
     { Do not initialize progress if we're in the middle of rendering off-screen.
       This can happen if you have a GeneratedCubeMapTexure on an animated scene,
       and progress bar appears because after animating a transform the shape octree
       needs to be rebuild for frustum culling (which can happen on larger scenes,
       to easily reproduce use TESTING_PROGRESS_DELAY with android_demo 2 teapots scene). }
     (RenderingCamera.Target = rtScreen) then
    UsedWindow := Application.MainWindow else
    UsedWindow := nil;

  if UsedWindow = nil then Exit;

  Bar := TCastleProgressBar.Create(nil);
  Bar.Progress := Progress;

  if Image <> nil then
    Bar.Background := (Image as TRGBImage).MakeCopy else
    Bar.Background := UsedWindow.SaveScreen;
  Bar.YPosition := BarYPosition;

  SavedMode := TGLMode.CreateReset(UsedWindow, nil, nil, @NoClose);

  UsedWindow.Controls.InsertFront(Bar);

  { init our window state }
  UsedWindow.AutoRedisplay := true;
  UsedWindow.InternalCursor := mcWait;
  { To actually draw progress start. }
  UsedWindow.Invalidate;
  Application.ProcessAllMessages;
end;

procedure TWindowProgressInterface.Update(Progress: TProgress);
begin
  if UsedWindow = nil then Exit;
  Application.ProcessAllMessages;
end;

procedure TWindowProgressInterface.Fini(Progress: TProgress);
begin
  if UsedWindow = nil then Exit;
  FreeAndNil(Bar);
  FreeAndNil(SavedMode);
  UsedWindow := nil;
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
