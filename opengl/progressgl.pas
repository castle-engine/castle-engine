{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Progress bar displayed in OpenGL inside GLWindow window.)

  Rejestruje odpowiednie funkcje modulu ProgressUnit w RegisterProgresGL.

  Pomiedzy Progress.Init a Fini nie powinienes nic robic z okienkiem
  ktore przekazesz jako parametr dla RegisterProgressGL. Jego callbacki
  beda zmienione, stan kontekstu OpenGL'a tez bedzie zmieniony.
  Wiec nic z nimi nie rob.

  Pamietaj aby po Progress.Init wywolac zawsze Progress.Fini (najlepiej
  w konstrukcji
    Progress.Init; try.....finally Progress.Fini end
  ) a wszystko (callbacki, stan OpenGL'a) bedzie przywrocone po
  Progress.Fini. }


unit ProgressGL;

{$I kambiconf.inc}
{$I openglmac.inc}

interface

uses GL, GLU, GLExt, OpenGLFonts, OpenGLBmpFonts, GLWindow, ProgressUnit,
  GLWinModes, KambiGLUtils;

const
  DefaultBarYPosition = 0.5;

type
  TProgressGLInterface = class(TProgressUserInterface)
  private
    { obraz tla (ekran w momencie Init) }
    list_drawProgressBG: TGLuint;
    ProgressFont: TGLBitmapFont_Abstract;
    FWindow: TGLWindow;
    SavedMode: TGLMode;
    FBarYPosition: Single;
  public
    constructor Create;

    { Assign this before doing Init. Don't change this when we are
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
  { created in initialization, freed in finalization }
  ProgressGLInterface :TProgressGLInterface;

implementation

uses SysUtils, KambiUtils,  BFNT_BitstreamVeraSans_Unit, Images;

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
   margines + (glwin.width-2*margines) * Progress.Position/Progress.Max, y2);

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

 SavedMode := TGLMode.Create(Window,
   GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT, false);

 {catch screen}
 list_drawProgressBG := Window.SaveScreenToDisplayList;

 {init our state}
 SetStdNoCloseGLWindowState(Window,
   {$ifdef FPC_OBJFPC} @ {$endif} DisplayProgress, nil, Progress, true,
   Window.FPSActive, false, K_None, false, false);
 ProgressFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

 Window.Cursor := gcWait;

 glDisable(GL_TEXTURE_2D);
 glDisable(GL_LIGHTING);
 glDisable(GL_DEPTH_TEST);

 ProjectionGLOrtho(0, Window.Width, 0, Window.Height);

 { To actually draw progress start. }
 Window.PostRedisplay;
 Window.FlushRedisplay;

 glwm.ProcessMessage(false);
end;

procedure TProgressGLInterface.Update(Progress: TProgress);
begin
 glwm.ProcessAllMessages;
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
