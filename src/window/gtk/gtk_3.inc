{%MainUnit ../castlewindow.pas}

{ Definitions specific to GTK 3 (and not used with GTK 2).
  These make GTK 3 usage more similar to GTK 2. }

const
  { Define gfalse, gtrue just like GTK 2 does.
    In case of GTK 3 bindings, gboolean is just Boolean32. }
  gfalse = gboolean(0);
  gtrue = gboolean(1);

type
  { These are functions that check type in GTK 2.
    In GTK 3, we just cast.
    TODO: use g_type_check_instance_cast to perform check and cast? }
  GTK_WIDGET = PGtkWidget;
  GTK_OBJECT = PGObject;
  GTK_WINDOW = PGtkWindow;
  GTK_CONTAINER = PGtkContainer;
  GTK_SIGNAL_FUNC = TGCallback;
  G_OBJECT = PGObject;
  GTK_DIALOG = PGtkDialog;

{ Deprecated in GTK 3, but still available and useful to us:
  https://docs.gtk.org/gtk3/method.Widget.set_double_buffered.html

  The https://stackoverflow.com/questions/31557168/gtkwidget-alternative-for-deprecated-double-buffered-property
  advises that the other solution is using
  https://docs.gtk.org/gtk3/class.GLArea.html , but we don't want to:
  1. It would mean that GTK 2 backend (which for now
     we want to maintain) would differ more from the GTK 3 backend.
  2. We prefer to choose on our side EGL vs glX, and communicate with them
     on our side, this gives us more flexibility and we need code for EGL / glX
     anyway to work on other platforms. So it's actually simpler for us to
     use EGL / glX directly. }
procedure gtk_widget_set_double_buffered(Widget: PGtkWidget; DoubleBuffered: gboolean);
  cdecl; external LazGtk3_library name 'gtk_widget_set_double_buffered';

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
{ Define gdk_x11_xxx routines we need, as units in
  src/window/gtk/gtk3/gtk3bindings/ don't define them. }
function gdk_x11_window_get_xid(Window: PGdkWindow): TWindow;
  cdecl; external LazGdk3_library;
function gdk_x11_display_get_xdisplay(Display: PGdkDisplay): Xlib.PDisplay;
  cdecl; external LazGdk3_library;
function gdk_x11_screen_get_screen_number(Screen: PGdkScreen): Integer;
  cdecl; external LazGdk3_library;
function gdk_x11_screen_lookup_visual(Screen: PGdkScreen;
  XVisualid: TVisualID): PGdkVisual;
  cdecl; external LazGdk3_library;
{$endif}

{ Implement a few methods that match GTK 2 names and signature
  using straightforward counterparts from GTK 3. }

function gtk_signal_connect(const Obj: PGObject; const SignalName: String;
  const CHandler: TGCallback; const Data: Pointer): GULong;
begin
  Result := g_signal_connect_data(Obj, PAnsiChar(SignalName), CHandler, Data, nil, G_CONNECT_DEFAULT);
end;

procedure gtk_signal_disconnect(const Obj: PGObject; const HandlerId: GULong);
begin
  g_signal_handler_disconnect(Obj, HandlerId);
end;

procedure g_signal_handlers_block_by_func(const Obj: PGObject; const Func: Pointer; const Data: Pointer);
begin
  if g_signal_handlers_block_matched(Obj, [G_SIGNAL_MATCH_FUNC, G_SIGNAL_MATCH_DATA], 0, 0, nil, Func, Data) = 0 then
    WritelnWarning('g_signal_handlers_block_matched did not match any handlers');
end;

procedure g_signal_handlers_unblock_by_func(const Obj: PGObject; const Func: Pointer; const Data: Pointer);
begin
  if g_signal_handlers_unblock_matched(Obj, [G_SIGNAL_MATCH_FUNC, G_SIGNAL_MATCH_DATA], 0, 0, nil, Func, Data) = 0 then
    WritelnWarning('g_signal_handlers_unblock_matched did not match any handlers');
end;

procedure gtk_object_set_user_data(const Obj: PGObject; const Data: Pointer);
begin
  g_object_set_data(Obj, 'castle_user_data', Data);
end;

function gtk_object_get_user_data(const Obj: PGObject): Pointer;
begin
  Result := g_object_get_data(Obj, 'castle_user_data');
end;

function gtk_events_pending_bool: Boolean;
begin
  Result := gtk_events_pending();
end;

{ Get pointer (mouse) as GdkDevice from GdkDisplay. }
function GdkPointerDevice(const Display: PGdkDisplay): PGdkDevice;
var
  DeviceManager: PGdkDeviceManager;
begin
  DeviceManager := gdk_display_get_device_manager(Display);
  Result := gdk_device_manager_get_client_pointer(DeviceManager);
end;

procedure gdk_display_warp_pointer(const Display: PGdkDisplay;
  const Screen: PGdkScreen; const X, Y: GInt);
var
  Device: PGdkDevice;
begin
  Device := GdkPointerDevice(Display);
  gdk_device_warp(Device, Screen, X, Y);
end;

function gtk_vbox_new(homogeneous: gboolean; spacing: gint): PGtkBox;
begin
  Result := PGtkBox(gtk_box_new(GTK_ORIENTATION_VERTICAL, spacing));
end;

procedure gtk_box_pack_end_defaults(const Box: PGtkBox; const Child: PGtkWidget);
begin
  gtk_box_pack_end(Box, Child, gtrue, gtrue, 0);
end;
