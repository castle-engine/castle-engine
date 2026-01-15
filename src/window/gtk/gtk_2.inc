{%MainUnit ../castlewindow.pas}

{ Definitions specific to GTK 2 (and not used with GTK 3). }

// Various functions missing from FPC GTK 2 / GDK 2 units that we need
procedure gtk_check_menu_item_set_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem; draw_as_radio:gboolean);
  cdecl; external gtklib;

function gtk_check_menu_item_get_draw_as_radio(
  check_menu_item:PGtkCheckMenuItem):gboolean;
  cdecl; external gtklib;

{ This is mispelled in FPC gtk bindings, see
  [http://www.freepascal.org/bugs/showrec.php3?ID=5112] }
function gdk_display_open(display_name:Pgchar):PGdkDisplay;
  cdecl; external gdklib;

function gdk_cursor_new_for_display(Display: PGdkDisplay;
  cursor_type: TGdkCursorType): PGdkCursor;
  cdecl; external gdklib name 'gdk_cursor_new_for_display';
function gdk_display_get_default: PGdkDisplay;
  cdecl; external gdklib name 'gdk_display_get_default';
function gdk_screen_get_primary_monitor(Screen: PGdkScreen): GInt;
  cdecl; external gdklib name 'gdk_screen_get_primary_monitor';
function gdk_screen_get_resolution(screen: PGdkScreen): gdouble;
  cdecl; external gdklib;

function gtk_events_pending_bool: Boolean;
begin
  Result := gtk_events_pending() <> 0;
end;

{ Get GDK PGdkWindow from GTK PGtkWidget. }
function gtk_widget_get_window(const Widget: PGtkWidget): PGdkWindow;
begin
  Result := Widget^.Window;
end;
