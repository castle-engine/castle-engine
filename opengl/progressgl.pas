{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
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

{
  TODO: make sure docs look good in pasdoc
  TODO: translate docs to English
}

{$I kambiconf.inc}
{$I openglmac.inc}

interface

uses OpenGLh, OpenGLFonts, OpenGLBmpFonts, GLWindow, ProgressUnit,
  GLWinModes, KambiGLUtils;

type
  TProgressGLInterface = class(TProgressUserInterface)
  private
    { obraz tla (ekran w momencie Init) }
    list_drawProgressBG: TGLuint;
    ProgressFont: TGLBitmapFont_Abstract;
    FWindow: TGLWindow;
    SavedMode: TGLMode;
  public
    { Assign this before doing Init. Don't change this when we are
      between Init and Fini. }
    property Window: TGLWindow read FWindow write FWindow;

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
    BarHeight, y1, y2: TGLfloat;
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
 y1 := glwin.height div 2 + BarHeight/2;
 y2 := glwin.height div 2 - BarHeight/2;

 glColor3ub(192, 192, 192);
 glRectf(margines, y1, glwin.width-margines, y2);
 glColor3f(0.2, 0.5, 0);
 glRectf(margines, y1,
   margines + (glwin.width-2*margines) * Progress.Position/Progress.Max, y2);

 glColor3f(0, 0,0);
 glRasterPos2i(margines + 20,
   (glwin.Height - ProgressInterface.ProgressFont.TextHeight('M')) div 2);
 ProgressInterface.ProgressFont.Print(
   Progress.Title +' '+ Progress.DescribePosition);
end;

{ TProgressGLInterface  ------------------------------------------------ }

procedure TProgressGLInterface.Init(Progress: TProgress);
begin
 Check(Window <> nil,
   'TProgressGLInterface: You must assign Window before doing Init');

 SavedMode := TGLMode.Create(Window,
   GL_CURRENT_BIT or GL_ENABLE_BIT or GL_TRANSFORM_BIT);

 {catch screen}
 list_drawProgressBG := Window.SaveScreenToDispList;

 {init our state}
 SetStdNoCloseGLWindowState(Window, DisplayProgress, nil, Progress, true,
   Window.FPSActive, false, #0, false, false);
 ProgressFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

 glDisable(GL_TEXTURE_2D);
 glDisable(GL_LIGHTING);
 glDisable(GL_DEPTH_TEST);

 ProjectionGLOrtho(0, Window.Width, 0, Window.Height);

 glwm.ProcessMessage(false);
end;

procedure TProgressGLInterface.Update(Progress: TProgress);
begin
 glwm.ProcessMessage(false);
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
