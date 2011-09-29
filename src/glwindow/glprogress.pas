{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Progress bar displayed in a TCastleWindowBase.

  Simply set @code(GLProgressInterface.Window) to your TCastleWindowBase
  instance, and assign

@longCode(#  Progress.UserInterface := GLProgressInterface;#)

  Between Progress.Init and Fini you shouldn't do anything with
  window set as @code(GLProgressInterface.Window).
  It's callbacks will be temporarily swapped and it will be used
  to render progress bar.

  As usual, remember to always call Progress.Fini if you called
  Progress.Init. Progress.Fini restores original callbacks and OpenGL
  state of your window. Usually it's best and safest to use try..finally
  block like

@longCode(#  Progress.Init; try.....finally Progress.Fini; end; #) }


unit GLProgress;

{$I kambiconf.inc}

interface

uses GL, OpenGLFonts, OpenGLBmpFonts, GLWindow, ProgressUnit,
  GLWinModes, KambiGLUtils;

const
  DefaultBarYPosition = 0.5;

type
  TGLProgressInterface = class(TProgressUserInterface)
  private
    { Background image (screen captured at the moment of Init call) }
    list_drawProgressBG: TGLuint;
    ProgressFont: TGLBitmapFont_Abstract;
    FWindow: TCastleWindowBase;
    SavedMode: TGLMode;
    FBarYPosition: Single;
  public
    constructor Create;

    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TCastleWindowBase read FWindow write FWindow;

    { Vertical position of the displayed bar. 0 means that
      the middle of bar is on the bottom of the screen, 1 means it's
      at the top. 0.5 is the default, and natural, value: it says
      to put bar in the middle of the screen. }
    property BarYPosition: Single
      read FBarYPosition write FBarYPosition default DefaultBarYPosition;

    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { Assign this to Progress.UserInterface to use OpenGL progress bar.
    This instance is created in initialization, freed in finalization. }
  GLProgressInterface: TGLProgressInterface;

implementation

uses SysUtils, KambiUtils,  BFNT_BitstreamVeraSans_Unit, Images, KeysMouse;

{ display -------------------------------------------------------------------- }

procedure DisplayProgress(Window: TCastleWindowBase);
var
  Margin: integer;
  BarHeight, y1, y2, YMiddle: TGLfloat;
  Progress: TProgress;
  ProgressInterface: TGLProgressInterface;
begin
  Progress := TProgress(Window.UserData);
  ProgressInterface := Progress.UserInterface as TGLProgressInterface;

  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(ProgressInterface.list_drawProgressBG);

  Margin := 100 * Window.width div 800;
  BarHeight := 50 * Window.height div 600;
  YMiddle := Window.Height * ProgressInterface.BarYPosition;
  y1 := YMiddle + BarHeight/2;
  y2 := YMiddle - BarHeight/2;

  glColor3ub(192, 192, 192);
  glRectf(Margin, y1, Window.width-Margin, y2);
  glColor3f(0.2, 0.5, 0);
  glRectf(Margin, y1,
    Margin + (Cardinal(Window.width)-2*Margin) * Progress.Position/Progress.Max, y2);

  glColor3f(0, 0,0);
  glRasterPos2f(Margin + 20,
    YMiddle - ProgressInterface.ProgressFont.TextHeight('M') div 2);
  ProgressInterface.ProgressFont.Print(Progress.TitleWithPosition(true));
end;

{ TGLProgressInterface  ------------------------------------------------ }

constructor TGLProgressInterface.Create;
begin
  inherited;
  FBarYPosition := DefaultBarYPosition;
end;

procedure TGLProgressInterface.Init(Progress: TProgress);
begin
  Check(Window <> nil,
    'TGLProgressInterface: You must assign Window before doing Init');

  {catch screen}
  list_drawProgressBG := Window.SaveScreen_ToDisplayList;

  SavedMode := TGLMode.CreateReset(Window,
    GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT, false,
    {$ifdef FPC_OBJFPC} @ {$endif} DisplayProgress, nil,
    {$ifdef FPC_OBJFPC} @ {$endif} NoClose, Window.Fps.Active);

  {init our state}
  Window.UserData := Progress;
  Window.AutoRedisplay := true;
  ProgressFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

  Window.Cursor := mcWait;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

  ProjectionGLOrtho(0, Window.Width, 0, Window.Height);

  { To actually draw progress start. }
  Window.PostRedisplay;
  Window.FlushRedisplay;

  Application.ProcessMessage(false);
end;

procedure TGLProgressInterface.Update(Progress: TProgress);
begin
  Application.ProcessAllMessages;
end;

procedure TGLProgressInterface.Fini(Progress: TProgress);
begin
  FreeAndNil(ProgressFont);
  glDeleteLists(list_drawProgressBG, 1);

  SavedMode.Free;
end;

initialization
  GLProgressInterface := TGLProgressInterface.Create;
finalization
  FreeAndNil(GLProgressInterface);
end.
