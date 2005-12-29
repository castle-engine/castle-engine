{
  Copyright 2005 Michalis Kamburelis.

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

{ This is a demo how one can use TGLWindow.MakeGLAreaContainer
  to put some GTK widgets in window that is still managed
  as simple TGLWindow object from GLWindow unit.
  GLWindow must be implemented on top of GTK
  (GLWINDOW_GTK_1 or GLWINDOW_GTK_2) to be able to compile this.
  You must also define GTK_1 when compiling this unit
  if GLWindow is implemented on top of GTK 1.x (GLWINDOW_GTK_1).

  This demo inserts GtkEntry widget into TGLWindow window.
  When user changes text of this entry, OpenGL area displays it.

  This demo also demonstrates some simple use of TGLOutlineFont
  class from OpenGLTTFonts.
}

{ $define GTK_1}

program test_glwindow_gtk_mix;

uses GLWindow, OpenGLh, KambiGLUtils, OpenGLFonts, SysUtils,
  VectorMath, KambiUtils, OpenGLTTFonts, TTF_BitstreamVeraSans_Unit,
  {$ifdef GTK_1} Gtk, Gdk, Glib {$else} Gtk2, Gdk2, Glib2 {$endif};

type
  TMyGLWindow = class(TGLWindowDemo)
  protected
    function MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget; override;
  end;

var
  Glw: TMyGLWindow;
  Font: TGLOutlineFont_Abstract;
  Entry: PGtkWidget;

procedure EntryTextChanged(editable: PGtkEditable; user_data: GPointer); cdecl;
begin
 glw.PostRedisplay;
end;

function TMyGLWindow.MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget;
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

procedure Draw(glwin: TGLWindow);
begin
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 glLoadIdentity;
 glTranslatef(-13, 0, -20);
 glScalef(0.1, 0.1, 0.1);
 glRotatef(40, 0, 1, 0);
 glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,
   Vector4Single(0.1, 0.1, 0.0, 1.0));
 Font.Print(gtk_entry_get_text(GTK_ENTRY(Entry)));
end;

procedure Init(glwin: TGLWindow);
begin
 Font := TGLOutlineFont.Create(@TTF_BitstreamVeraSans, 1);
 glEnable(GL_DEPTH_TEST);
 glEnable(GL_LIGHT0);
 glEnable(GL_LIGHTING);
end;

procedure Close(glwin: TGLWindow);
begin
 FreeAndNil(Font);
end;

procedure Resize(glwin: TGLWindow);
begin
 glViewport(0, 0, glwin.Width, glwin.Height);
 ProjectionGLPerspective(45, glwin.Width / glwin.Height, 0.1, 100);
end;

begin
 glw := TMyGLWindow.Create;
 try
  glw.Width := 600;
  glw.Height := 400;
  glw.OnInit := Init;
  glw.OnClose := Close;
  glw.OnResize := Resize;
  glw.InitLoop('GLWindow with some GTK widgets', Draw);
 finally glw.Free end;
end.