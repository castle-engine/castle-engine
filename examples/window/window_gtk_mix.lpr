{
  Copyright 2005-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a demo how one can use TCastleWindowCustom.MakeGLAreaContainer
  to put some GTK widgets in window that is still managed
  as simple TCastleWindowCustom object from CastleWindow unit.
  CastleWindow must be implemented on top of GTK
  (CASTLE_WINDOW_GTK_2) to be able to compile this.

  This demo inserts GtkEntry widget into TCastleWindowCustom window.
  When user changes text of this entry, OpenGL area displays it.

  This demo also demonstrates some simple use of TGLOutlineFont
  class from CastleGLOutlineFonts.
}

program window_gtk_mix;

uses CastleWindow, SysUtils, CastleColors,
  CastleVectors, CastleUtils, Gtk2, Glib2, CastleStringUtils,
  CastleKeysMouse, CastleScene, X3DNodes;

type
  TMyWindow = class(TCastleWindow)
  protected
    function MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget; override;
  end;

var
  Window: TMyWindow;
  Scene: TCastleScene;
  Text: TTextNode;
  Entry: PGtkWidget;
  TextString: string = 'Text entered in GtkEntry is rendered in OpenGL';

procedure EntryTextChanged(editable: PGtkEditable; user_data: GPointer); cdecl;
begin
  TextString := gtk_entry_get_text(GTK_ENTRY(Entry));
  Text.FdString.Send([TextString]);
end;

function TMyWindow.MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget;
begin
  Entry := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(Entry), PChar(TextString));
  gtk_signal_connect(PGtkObject(Entry), 'changed',
    GTK_SIGNAL_FUNC(@EntryTextChanged), nil);
  gtk_widget_show(Entry);

  Result := gtk_vbox_new(false, 2);

  gtk_box_pack_end(GTK_BOX(Result), GTK_WIDGET(GLArea), true, true, 0);
  gtk_box_pack_end(GTK_BOX(Result), Entry, false, false, 0);
  gtk_widget_show(Result);
end;

procedure CreateScene;
var
  Appearance: TAppearanceNode;
  Material: TMaterialNode;
  Root: TX3DRootNode;
  Shape: TShapeNode;
begin
  Text := TTextNode.Create;
  Text.FdString.Send([TextString]);

  Material := TMaterialNode.Create;
  Material.FdDiffuseColor.Value := Vector3Single(1, 1, 0);

  Appearance := TAppearanceNode.Create;
  Appearance.FdMaterial.Value := Material;

  Shape := TShapeNode.Create;
  Shape.FdGeometry.Value := Text;
  Shape.Appearance := Appearance;

  Root := TX3DRootNode.Create;
  Root.FdChildren.Add(Shape);

  Scene := TCastleScene.Create(Application);
  Scene.Load(Root, true);
end;

begin
  Window := TMyWindow.Create(Application);

  CreateScene;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.Width := 600;
  Window.Height := 400;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.Caption := 'CastleWindow with some GTK widgets';
  Window.OpenAndRun;
end.