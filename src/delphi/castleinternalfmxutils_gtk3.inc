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
  GULong = CULong;
  PGList = Pointer;
  PGChar = PAnsiChar;
  GInt8 = CInt8;
  Guint32 = Cuint32;
  TGQuark = Guint32;

const
  G_TYPE_FUNDAMENTAL_SHIFT = 2;
  G_TYPE_FUNDAMENTAL_MAX = 255 shl G_TYPE_FUNDAMENTAL_SHIFT;

  { Constant fundamental types,
    introduced by g_type_init().
    Copied from FPC GTK2 packages (glib is the same). }
  G_TYPE_INVALID = GType(0 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_NONE = GType(1 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_INTERFACE = GType(2 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_CHAR = GType(3 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_UCHAR = GType(4 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_BOOLEAN = GType(5 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_INT = GType(6 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_UINT = GType(7 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_LONG = GType(8 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_ULONG = GType(9 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_INT64 = GType(10 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_UINT64 = GType(11 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_ENUM = GType(12 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_FLAGS = GType(13 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_FLOAT = GType(14 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_DOUBLE = GType(15 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_STRING = GType(16 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_POINTER = GType(17 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_BOXED = GType(18 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_PARAM = GType(19 shl G_TYPE_FUNDAMENTAL_SHIFT);
  G_TYPE_OBJECT = GType(20 shl G_TYPE_FUNDAMENTAL_SHIFT);

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

  TGClosure = record
    // We only ever pass pointers to it
  end;
  PGClosure = ^TGClosure;

  { Used to pass pointers to routines.
    Parameters differ based on particular signal, so we just declare a pointer.
    See https://docs.gtk.org/gobject/callback.Callback.html }
  TGCallback = Pointer; // procedure; cdecl;

  TGClosureNotify = procedure (data: Pointer; closure: PGClosure); cdecl;

  TGConnectFlags = CInt;

  { Generic value with dynamic type used in Glib and friends.
    Note: GValue should be trated as opaque type by everything except
    g_value_xxx routines, see https://docs.gtk.org/gobject/struct.Value.html . }
  TGValue = record
    g_type : GType;
    data : array[0..1] of record
      case longint of
        0 : ( v_int : Cint );
        1 : ( v_uint : Cuint );
        2 : ( v_long : Clong );
        3 : ( v_ulong : Culong );
        4 : ( v_int64 : Cint64 );
        5 : ( v_uint64 : Cuint64 );
        6 : ( v_float : Single );
        7 : ( v_double : Double );
        8 : ( v_pointer : Pointer );
      end;
  end;
  PGValue = ^TGValue;

{ This is used underneath by casting macros like GTK_CONTAINER.
  x:=GTK_CONTAINER(y) is actually
  x:=g_type_check_instance_cast(y, gtk_container_get_type) }
function g_type_check_instance_cast(instance: Pointer; iface_type: GType): Pointer; cdecl; external 'libgobject-2.0.so';

{ Get the name of an object's type.
  https://developer-old.gnome.org/gobject/stable/gobject-The-Base-Object-Type.html#G-OBJECT-TYPE-NAME:CAPS }
function g_type_name(AnObjectType: GType): Pgchar; cdecl; external 'libgobject-2.0.so';

function g_signal_lookup(name: Pgchar; itype:GType): Cuint; cdecl; external 'libgobject-2.0.so';
function g_signal_connect_data(instance: Pointer; detailed_signal: Pgchar;
  c_handler: TGCallback; data: Pointer; destroy_data: TGClosureNotify;
  connect_flags: TGConnectFlags): GULong; cdecl; external 'libgobject-2.0.so';
procedure g_signal_emitv(instance_and_params: PGValue; signal_id: Cuint;
  detail: TGQuark; return_value: PGValue); cdecl; external 'libgobject-2.0.so';

function g_value_init(value: PGValue; g_type: GType):PGValue; cdecl; external 'libgobject-2.0.so';
function g_value_reset(value: PGValue): PGValue; cdecl; external 'libgobject-2.0.so';
procedure g_value_unset(value: PGValue); cdecl; external 'libgobject-2.0.so';
procedure g_value_set_instance(value: PGValue; instance: Pointer); cdecl; external 'libgobject-2.0.so';
procedure g_value_set_pointer(value: PGValue; ptr: Pointer); cdecl; external 'libgobject-2.0.so';
procedure g_value_set_boolean(value: PGValue; v_boolean: gboolean); cdecl; external 'libgobject-2.0.so';

function g_object_ref(Obj: Pointer): Pointer; cdecl; external 'libgobject-2.0.so';
procedure g_object_unref(Obj: Pointer); cdecl; external 'libgobject-2.0.so';

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

function g_signal_connect(instance: Pointer; detailed_signal: Pgchar;
  c_handler: TGCallback; data: Pointer) : gulong;
begin
  Result := g_signal_connect_data(instance, detailed_signal, c_handler,
    data, nil, 0);
end;

{ ---------------------------------------------------------------------------- }
{ Minimal GDK 3 API definition we need. }

const
  GDK_EXPOSURE_MASK            = 1 shl 1;
  GDK_POINTER_MOTION_MASK      = 1 shl 2;
  GDK_POINTER_MOTION_HINT_MASK = 1 shl 3;
  GDK_BUTTON_MOTION_MASK       = 1 shl 4;
  GDK_BUTTON1_MOTION_MASK      = 1 shl 5;
  GDK_BUTTON2_MOTION_MASK      = 1 shl 6;
  GDK_BUTTON3_MOTION_MASK      = 1 shl 7;
  GDK_BUTTON_PRESS_MASK        = 1 shl 8;
  GDK_BUTTON_RELEASE_MASK      = 1 shl 9;
  GDK_KEY_PRESS_MASK           = 1 shl 10;
  GDK_KEY_RELEASE_MASK         = 1 shl 11;
  GDK_ENTER_NOTIFY_MASK        = 1 shl 12;
  GDK_LEAVE_NOTIFY_MASK        = 1 shl 13;
  GDK_FOCUS_CHANGE_MASK        = 1 shl 14;
  GDK_STRUCTURE_MASK           = 1 shl 15;
  GDK_PROPERTY_CHANGE_MASK     = 1 shl 16;
  GDK_VISIBILITY_NOTIFY_MASK   = 1 shl 17;
  GDK_PROXIMITY_IN_MASK        = 1 shl 18;
  GDK_PROXIMITY_OUT_MASK       = 1 shl 19;
  GDK_SUBSTRUCTURE_MASK        = 1 shl 20;
  GDK_SCROLL_MASK              = 1 shl 21;
  GDK_ALL_EVENTS_MASK          = $3FFFFE;

type
  TGdkEventType = GInt;

  PGdkEventAny = ^TGdkEventAny;
  TGdkEventAny = record
    &type: TGdkEventType;
    window: Pointer; // PGdkWindow;
    send_event: gint8;
  end;

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
procedure gtk_widget_set_events(Widget: Pointer; events: Gint); cdecl; external 'libgtk-3.so.0';
procedure gtk_widget_destroy(Widget: Pointer); cdecl; external 'libgtk-3.so.0';

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
