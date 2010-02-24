{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Progress bar displayed in OpenGL inside GLWindow window.)

  Simply set @code(ProgressGLInterface.Window) to your TGLWindow
  instance, and assign

@longCode(#  Progress.UserInterface := ProgressGLInterface;#)

  Between Progress.Init and Fini you shouldn't do anything with
  window set as @code(ProgressGLInterface.Window).
  It's callbacks will be temporarily swapped and it will be used
  to render progress bar.

  As usual, remember to always call Progress.Fini if you called
  Progress.Init. Progress.Fini restores original callbacks and OpenGL
  state of your window. Usually it's best and safest to use try..finally
  block like

@longCode(#  Progress.Init; try.....finally Progress.Fini; end; #) }


unit ProgressGL;

{$I kambiconf.inc}

interface

uses GL, GLU, GLExt, OpenGLFonts, OpenGLBmpFonts, GLWindow, ProgressUnit,
  GLWinModes, KambiGLUtils;

const
  DefaultBarYPosition = 0.5;

type
  TProgressGLInterface = class(TProgressUserInterface)
  private
    { Background image (screen captured at the moment of Init call) }
    list_drawProgressBG: TGLuint;
    ProgressFont: TGLBitmapFont_Abstract;
    FWindow: TGLWindow;
    SavedMode: TGLMode;
    FBarYPosition: Single;
  public
    constructor Create;

    { Window used to render the progress bar.
      Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TGLWindow read FWindow write FWindow;

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
  ProgressGLInterface: TProgressGLInterface;

implementation

uses SysUtils, KambiUtils,  BFNT_BitstreamVeraSans_Unit, Images, KeysMouse;

{ display proc. --------------------------------------------------------------}

procedure DisplayProgress(glwin: TGLWindow);
var margines: integer;
    BarHeight, y1, y2, YMiddle: TGLfloat;
    Progress: TProgress;
    ProgressInterface: TProgressGLInterface;
begin
 Progress := TProgress(glwin.UserData);
 ProgressInterface := Progress.UserInterface as TProgressGLInterface;

 glLoadIdentity;
 glRasterPos2i(0, 0);
 glCallList(ProgressInterface.list_drawProgressBG);

 margines := 100 * glwin.width div 800;
 BarHeight := 50 * glwin.height div 600;
 YMiddle := Glwin.Height * ProgressInterface.BarYPosition;
 y1 := YMiddle + BarHeight/2;
 y2 := YMiddle - BarHeight/2;

 glColor3ub(192, 192, 192);
 glRectf(margines, y1, glwin.width-margines, y2);
 glColor3f(0.2, 0.5, 0);
 glRectf(margines, y1,
   margines + (Cardinal(glwin.width)-2*margines) * Progress.Position/Progress.Max, y2);

 glColor3f(0, 0,0);
 glRasterPos2f(margines + 20,
   YMiddle - ProgressInterface.ProgressFont.TextHeight('M') div 2);
 ProgressInterface.ProgressFont.Print(Progress.TitleWithPosition(true));
end;

{ TProgressGLInterface  ------------------------------------------------ }

constructor TProgressGLInterface.Create;
begin
  inherited;
  FBarYPosition := DefaultBarYPosition;
end;

procedure TProgressGLInterface.Init(Progress: TProgress);
begin
 Check(Window <> nil,
   'TProgressGLInterface: You must assign Window before doing Init');

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

procedure TProgressGLInterface.Update(Progress: TProgress);
begin
 Application.ProcessAllMessages;
end;

procedure TProgressGLInterface.Fini(Progress: TProgress);
begin
 FreeAndNil(ProgressFont);
 glDeleteLists(list_drawProgressBG, 1);

 SavedMode.Free;
end;

initialization
 ProgressGLInterface := TProgressGLInterface.Create;
finalization
 FreeAndNil(ProgressGLInterface);
end.
