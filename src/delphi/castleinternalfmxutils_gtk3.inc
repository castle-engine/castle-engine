{%MainUnit castleinternalfmxutils.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Minimal GTK 3 API definition we need.
  Along with friends: GDK 3, Glib 2, GObject 2 etc. }

{ ---------------------------------------------------------------------------- }
{ Minimal GLIB, see https://docs.gtk.org/glib/ }

type
  { Some type aliases, just to make below definitions clearer.
    Note: For many other types below, we've been lazy and just used "Pointer",
    instead of proper PGtkWidget etc. }
  GType = CULong;
  GBoolean  = LongBool;
  GInt = CInt;
  PGList = Pointer;
  PGChar = PAnsiChar;

function g_list_first(List: PGList): Pointer; cdecl; external 'libglib-2.0.so';

{ ---------------------------------------------------------------------------- }
{ Minimal GOBJECT (friend of glib), see
  https://docs.gtk.org/gobject/index.html  }

type
  { We pass only pointers, but we define records
    to access some fields that macros need to access.
    Yeah, these are effectively trivial records and we could
    instead just cast to PGType, but thanks to these definitions
    we're consistent with equivalent macros in C and in FPC RTL. }
  PGTypeClass = ^TGTypeClass;
  TGTypeClass = record
    g_type: GType;
  end;

  PGTypeInstance = ^TGTypeInstance;
  TGTypeInstance = record
    g_class: PGTypeClass;
  end;

{ This is used underneath by casting macros like GTK_CONTAINER.
  x:=GTK_CONTAINER(y) is actually
  x:=g_type_check_instance_cast(y, gtk_container_get_type) }
function g_type_check_instance_cast(instance: Pointer; iface_type: GType): Pointer; cdecl; external 'libgobject-2.0.so';

{ Get the name of an object's type.
  https://developer-old.gnome.org/gobject/stable/gobject-The-Base-Object-Type.html#G-OBJECT-TYPE-NAME:CAPS }
function g_type_name(AnObjectType: GType): Pgchar; cdecl; external 'libgobject-2.0.so';

{ Macros following code in FPC GTK 2 bindings
  (FPC RTL license same as CGE). }

function G_TYPE_FROM_CLASS(g_class: Pointer): GType;
begin
  Result := (PGTypeClass(g_class))^.g_type;
end;

function G_TYPE_FROM_INSTANCE(Instance: Pointer): GType;
begin
  Result := G_TYPE_FROM_CLASS((PGTypeInstance(instance))^.g_class);
end;

{ This is just alias for G_TYPE_FROM_INSTANCE. }
function G_OBJECT_TYPE(anObject: pointer): GType;
begin
  Result := G_TYPE_FROM_INSTANCE(anObject);
end;

{ Return a name of type of given instance, like 'GtkWindow'.
  Very useful for debugging.
  Following GTK docs, caller is *not* resposible for freeing the returned
  Pgchar pointer. }
function G_OBJECT_TYPE_NAME(anObject: pointer): Pgchar;
begin
  Result := g_type_name(G_OBJECT_TYPE(anObject));
end;

{ ---------------------------------------------------------------------------- }
{ Minimal GDK 3 API definition we need. }

function gdk_x11_window_get_xid(widget: Pointer): Pointer; cdecl; external 'libgdk-3.so.0';

{ ---------------------------------------------------------------------------- }
{ Minimal GTK 3 API definition we need. }

function gtk_widget_get_window(widget: Pointer): Pointer; cdecl; external 'libgtk-3.so.0';
procedure gtk_widget_show(Widget: Pointer); cdecl; external 'libgtk-3.so.0';
procedure gtk_widget_show_all(Widget: Pointer); cdecl; external 'libgtk-3.so.0';
procedure gtk_widget_realize(Widget: Pointer); cdecl; external 'libgtk-3.so.0';
procedure gtk_widget_set_size_request(Widget: Pointer; Width, Height: GInt); cdecl; external 'libgtk-3.so.0';
function gtk_widget_get_parent(Widget: Pointer): Pointer; cdecl; external 'libgtk-3.so.0';
 // deprecated and not necessary it seems:
 //procedure gtk_widget_set_double_buffered(Widget: Pointer; double_buffered: gboolean); cdecl; external 'libgtk-3.so.0';

function gtk_drawing_area_new: Pointer; cdecl; external 'libgtk-3.so.0';

procedure gtk_container_add(Container: Pointer; Widget: Pointer); cdecl; external 'libgtk-3.so.0';
procedure gtk_container_remove(Container: Pointer; Widget: Pointer); cdecl; external 'libgtk-3.so.0';
function gtk_container_get_type: GType; cdecl; external 'libgtk-3.so.0';
function gtk_container_get_children(Container: Pointer): PGList; cdecl; external 'libgtk-3.so.0';

function gtk_fixed_get_type: GType; cdecl; external 'libgtk-3.so.0';
{ Adds a widget to a GtkFixed container at the given position.
  https://docs.gtk.org/gtk3/method.Fixed.put.html }
procedure gtk_fixed_put(Fixed: Pointer; Widget: Pointer; x, y: GInt); cdecl; external 'libgtk-3.so.0';
{ Moves a child of a GtkFixed container to the given position.
  https://docs.gtk.org/gtk3/method.Fixed.move.html }
procedure gtk_fixed_move(Fixed: Pointer; Widget: Pointer; x, y: GInt); cdecl; external 'libgtk-3.so.0';

function gtk_bin_get_child(Bin: Pointer): Pointer; cdecl; external 'libgtk-3.so.0';
