{
  Copyright 2004-2005 Michalis Kamburelis.

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

{ Demo of Font.BreakLines method.
  Resize the window and watch how the text lines are automatically broken.
}

program test_font_break;

uses GLWindow, GLW_Demo, OpenGLh, KambiGLUtils, OpenGLFonts, SysUtils, Classes,
  KambiUtils, OpenGLBmpFonts, BFNT_BitstreamVeraSans_Unit, VectorMath;

var
  Font: TGLBitmapFont_Abstract;
  BoxWidth: Integer;

procedure Draw(glwin: TGLWindow);
var x1, x2: Integer;
begin
 glClear(GL_COLOR_BUFFER_BIT);
 glLoadIdentity;
 x1 := (glwin.Width - BoxWidth) div 2;
 x2 := x1 + BoxWidth;
 glColorv(Yellow3Single);
 VerticalGLLine(x1, 0, glwin.Height);
 VerticalGLLine(x2, 0, glwin.Height);
 glColorv(White3Single);
 Font.PrintBrokenString(
   'blah blah blah, I''m a long long long text and'
   +' I''m very curious how I will be broken to fit nicely between those'
   +' two yellow lines on the screen.' +nl+
   'BTW: Note that line breaks will be correctly preserved in the broken'
   +' text.' +nl+
   nl+
   'You can resize this window and watch how this text breaks at'
   +' various line widths.', BoxWidth, x1,
   { Using Font.Descend instead of 0, so that lower parts of the lowest line
     are visible } Font.Descend,
   false, 0);
end;

procedure Resize(glwin: TGLWindow);
begin
 glViewport(0, 0, glwin.Width, glwin.Height);
 ProjectionGLOrtho(0, glwin.Width, 0, glwin.Height);
 BoxWidth := glwin.Width * 2 div 3;
end;

procedure Init(glwin: TGLWindow);
begin
 Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure Close(glwin: TGLWindow);
begin
 FreeAndNil(Font);
end;

begin
 glw.OnInit := @Init;
 glw.OnClose := @Close;
 glw.OnResize := @Resize;
 glw.DepthBufferBits := 0;
 glw.InitLoop('Font.BreakLines demo', @Draw);
end.