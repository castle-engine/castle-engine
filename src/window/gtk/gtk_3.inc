{%MainUnit ../castlewindow.pas}

{ Definitions specific to GTK 3 (and not used with GTK 2). }

const
  { Define gfalse, gtrue just like GTK 2 does.
    In case of GTK 3 bindings, gboolean is just Boolean32. }
  gfalse = gboolean(0);
  gtrue = gboolean(1);

  gdklib = LazGdk3_library;

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

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
function gdk_x11_window_get_xid(Window: PGdkWindow): TWindow;
  cdecl; external LazGdk3_library;
function gdk_x11_display_get_xdisplay(Display: PGdkDisplay): Xlib.PDisplay;
  cdecl; external LazGdk3_library;
function gdk_x11_screen_get_screen_number(Screen: PGdkScreen): Integer;
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
