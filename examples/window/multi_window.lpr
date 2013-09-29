{
  Copyright 2004-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of
  - using multiple windows with CastleWindow unit
  - instead of registering OnXxx callbacks overriding EventXxx methods
    (more OOP approach)
  - TGLOutlineFont rendered with depth (3d letters)
  - press c to change cursor in the window that has focus, this is to demo
    that TCastleWindowBase.Cursor indeed works as appropriate, i.e. changes the cursor
    only for the given window.
}

program multi_window;

{$I castleconf.inc}
{$apptype GUI}

uses CastleGL, CastleWindow, SysUtils, CastleUtils, CastleGLOutlineFonts,
  CastleOutlineFont_BVSans, CastleGLUtils, CastleKeysMouse, CastleVectors,
  CastleStringUtils, CastleColors;

type
  TMyWindow = class(TCastleWindowDemo)
  public
    FontGL, FontGLLines: TGLOutlineFontAbstract;
    RotX, RotY, RotZ, MoveX, MoveY, MoveZ :TGLfloat;
    OnlyLines: boolean;
    Text: string;
    LightCol3f, DarkCol3f: TVector3f;
    Rotating: boolean;

    procedure EventDraw; override;
    procedure EventUpdate; override;
    procedure EventResize; override;
    procedure EventOpen; override;
    procedure EventClose; override;
    procedure EventPress(const Event: TInputPressRelease); override;
  end;

procedure TMyWindow.EventDraw;
begin
 inherited;

 GLClear([cbColor, cbDepth], Black);
 glLoadIdentity();
 glTranslatef(MoveX, MoveY, MoveZ);

 glRotatef(RotX, 1, 0, 0);
 glRotatef(RotY, 0, 1, 0);
 glRotatef(RotZ ,0, 0, 1);

 // Pulsing Colors Based On The Rotation
//  glColor3f(cos(rot/20.0), sin(rot/25.0), 0.5*cos(rot/17.0));
//  glColor3f(0.5+cos(rot/20.0)/2, 0.5+sin(rot/25.0)/2, 0.5+0.5*cos(rot/17.0)/2);

 glScalef(0.08, 0.08, 0.08);
 glColorv(LightCol3f);
 if OnlyLines then
 begin
  FontGLLines.Print(Text);
 end else
 begin
  glPushMatrix;
  FontGL.Print(Text);
  glPopMatrix;
  glColorv(DarkCol3f);
  FontGLLines.Print(Text);
 end;
end;

procedure TMyWindow.EventUpdate;

  function B(val: boolean): integer;
  begin if val then result := 1 else result := -1 end;

  procedure GLChange(var a: TGLfloat; change: TGLFloat);
  begin
   a += change;
   PostRedisplay;
  end;

begin
 inherited;

 if Pressed[K_X] then GLChange(RotX, B(mkShift in Pressed.Modifiers)*0.6);
 if Pressed[K_Y] then GLChange(RotY, B(mkShift in Pressed.Modifiers)*0.6);
 if Pressed[K_Z] then GLChange(RotZ, B(mkShift in Pressed.Modifiers)*0.6);

 if Pressed[K_Left] then GLChange(MoveX, -0.1);
 if Pressed[K_Right] then GLChange(MoveX, 0.1);
 if Pressed[K_Down] then GLChange(MoveY, -0.1);
 if Pressed[K_Up] then GLChange(MoveY, 0.1);
 if Pressed[K_PageUp] then GLChange(MoveZ, 0.1);
 if Pressed[K_PageDown] then GLChange(MoveZ, -0.1);

 if Rotating then
 begin
  GLChange(Roty, -0.1);
  GLChange(Rotz, 0.1);
 end;
end;

procedure TMyWindow.EventResize;
begin
 inherited;
 glViewport(Rect);
 PerspectiveProjection(45.0, Width/Height, 0.1, 100.0);
end;

procedure TMyWindow.EventOpen;
begin
 inherited;

 glEnable(GL_DEPTH_TEST);
 //fontgl := TGLOutlineFont.Create(0, 255, 0, 2, WGL_FONT_POLYGONS, 'Comic Sans MS');
 FontGL := TGLOutlineFont.Create(OutlineFont_BVSans, 40);
 FontGLLines := TGLOutlineFont.Create(OutlineFont_BVSans, 40, true);
end;

procedure TMyWindow.EventClose;
begin
 FreeAndNil(FontGL);
 FreeAndNil(FontGLLines);

 inherited;
end;

procedure TMyWindow.EventPress(const Event: TInputPressRelease);
var
  URL: string;
begin
 inherited;
 case Event.KeyCharacter of
  'l':begin
       OnlyLines := not OnlyLines;
       PostRedisplay;
      end;
  'c':if Cursor = High(Cursor) then Cursor := Low(Cursor) else Cursor := Succ(Cursor);
  'o':begin
        URL := '';
        { when file dialog is open, note that the other windows
          are still active as they should. }
        FileDialog('Test open file dialog', URL, true);
      end;
 end;
end;

var i: integer;
    Windws: array[0..4]of TMyWindow;
begin
 for i := 0 to High(Windws) do
 begin
  Windws[i] := TMyWindow.Create(nil);

  Windws[i].MoveZ := -10;
  Windws[i].Text := 'Window number '+IntToStr(i);
  Windws[i].LightCol3f := Vector3Single(Random*1.5, Random*1.5, Random*1.5);
  Windws[i].DarkCol3f := Vector3Single(Random*0.7, Random*0.7, Random*0.7);
  Windws[i].Rotating := Odd(i);

  Windws[i].Caption := IntToStr(i)+
    ' : outline font + multiple windows under CastleWindow demo';
  Windws[i].Width := 200;
  Windws[i].Height := 200;
  Windws[i].Left := 30 + 250 * (i mod 3);
  Windws[i].Top := 30 + 250 * (i div 3);
 end;
 try
  for i := 0 to High(Windws) do
  begin
    Windws[i].Open;
    Windws[i].SetDemoOptions(K_F11, CharEscape, true);
  end;
  Application.Run; { Loop will end when the last window is closed by the user }
 finally
  for i := 0 to High(Windws) do FreeAndNil(Windws[i]);
 end;
end.
