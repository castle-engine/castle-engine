{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @deprecated This displays nothing now.
  It used to be: Progress bar displayed in a TCastleWindow.

  Simply set @code(WindowProgressInterface.Window) to your TCastleWindow
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


unit CastleWindowProgress
  deprecated 'use TCastleView and WaitForRenderAndCall to display progress of loading operations';

{$I castleconf.inc}

interface

{$warnings off} // using deprecated CastleProgress in deprecated
uses CastleWindow, CastleProgress, CastleInternalWindowModes,
  CastleImages, CastleGLImages, CastleUIControls, CastleControls;
{$warnings on}

type
  { Progress bar rendered on OpenGL context (TCastleWindow).
    Uses Application.MainWindow to render the progress bar,
    so be sure to assign it.
    If the Application.MainWindow is not assigned, or not open,
    when progress bar starts --- we gracefully avoid showing any progress. }
  TWindowProgressInterface = class(TProgressUserInterface)
  private
    { Window used to render the progress bar, or nil if none.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    {$ifdef FPC}
    function GetWindow: TCastleWindow;
    procedure SetWindow(const Value: TCastleWindow);
    {$endif}
  public
    {$ifdef FPC}
    { @deprecated Using this is deprecated, you should rather assign to
      Application.MainWindow. }
    property Window: TCastleWindow read GetWindow write SetWindow; deprecated;
    {$endif}

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

{$ifdef FPC}
function TWindowProgressInterface.GetWindow: TCastleWindow;
begin
  Result := Application.MainWindow;
end;

procedure TWindowProgressInterface.SetWindow(const Value: TCastleWindow);
begin
  Application.MainWindow := Value;
end;
{$endif}

procedure TWindowProgressInterface.Init(Progress: TProgress);
begin
end;

procedure TWindowProgressInterface.Update(Progress: TProgress);
begin
end;

procedure TWindowProgressInterface.Fini(Progress: TProgress);
begin
end;

initialization
  WindowProgressInterface := TWindowProgressInterface.Create;
finalization
  FreeAndNil(WindowProgressInterface);
end.
