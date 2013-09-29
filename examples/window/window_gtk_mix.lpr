{
  Copyright 2005-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a demo how one can use TCastleWindowBase.MakeGLAreaContainer
  to put some GTK widgets in window that is still managed
  as simple TCastleWindowBase object from CastleWindow unit.
  CastleWindow must be implemented on top of GTK
  (CASTLE_WINDOW_GTK_2) to be able to compile this.

  This demo inserts GtkEntry widget into TCastleWindowBase window.
  When user changes text of this entry, OpenGL area displays it.

  This demo also demonstrates some simple use of TGLOutlineFont
  class from CastleGLOutlineFonts.
}

program window_gtk_mix;

{$I castleconf.inc}

uses CastleWindow, CastleGL, CastleGLUtils, SysUtils, CastleColors,
  CastleVectors, CastleUtils, CastleGLOutlineFonts, CastleOutlineFont_BVSans,
  Gtk2, Glib2, CastleStringUtils, CastleKeysMouse;

type
  TMyWindow = class(TCastleWindowDemo)
  protected
    function MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget; override;
  end;

var
  Window: TMyWindow;
  Font: TGLOutlineFontAbstract;
  Entry: PGtkWidget;

procedure EntryTextChanged(editable: PGtkEditable; user_data: GPointer); cdecl;
begin
 Window.PostRedisplay;
end;

function TMyWindow.MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget;
begin
 Entry := gtk_entry_new;
 gtk_entry_set_text(GTK_ENTRY(Entry),
   'Text entered in GtkEntry is rendered in OpenGL');
 gtk_signal_connect(PGtkObject(Entry), 'changed',
   GTK_SIGNAL_FUNC(@EntryTextChanged), nil);
 gtk_widget_show(Entry);

 Result := gtk_vbox_new(false, 2);

 gtk_box_pack_end(GTK_BOX(Result), GTK_WIDGET(GLArea), true, true, 0);
 gtk_box_pack_end(GTK_BOX(Result), Entry, false, false, 0);
 gtk_widget_show(Result);
end;

procedure Draw(Window: TCastleWindowBase);
begin
 GLClear([cbColor, cbDepth], Black);
 glLoadIdentity;
 glTranslatef(-13, 0, -20);
 glScalef(0.1, 0.1, 0.1);
 glRotatef(40, 0, 1, 0);
 glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,
   Vector4Single(1, 1, 0, 1));
 Font.Print(gtk_entry_get_text(GTK_ENTRY(Entry)));
end;

procedure Open(Window: TCastleWindowBase);
begin
 Font := TGLOutlineFont.Create(OutlineFont_BVSans, 10);
 glEnable(GL_DEPTH_TEST);
 glEnable(GL_LIGHT0);
 glEnable(GL_LIGHTING);
 glEnable(GL_NORMALIZE);
 glShadeModel(GL_FLAT);
 glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
end;

procedure Close(Window: TCastleWindowBase);
begin
 FreeAndNil(Font);
end;

procedure Resize(Window: TCastleWindowBase);
begin
  glViewport(Window.Rect);
  PerspectiveProjection(45, Window.Width / Window.Height, 0.1, 100);
end;

begin
 Window := TMyWindow.Create(nil);
 try
  Window.Width := 600;
  Window.Height := 400;
  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OnResize := @Resize;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun('CastleWindow with some GTK widgets', @Draw);
 finally Window.Free end;
end.