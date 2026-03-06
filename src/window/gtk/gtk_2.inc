{%MainUnit ../castlewindow.pas}

{ Definitions specific to GTK 2 (and not used with GTK 3).
  These make GTK 2 usage more similar to GTK 3. }

type
  TGdkRGBA = record
    Red: GDouble;
    Green: GDouble;
    Blue: GDouble;
    Alpha: GDouble;
  end;
  PGdkRGBA = ^TGdkRGBA;

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
function gdk_screen_get_resolution(screen: PGdkScreen): GDouble;
  cdecl; external gdklib;

{$ifndef CASTLE_SetMousePosition_USE_XWarpPointer}
// Since GDK >= 2.8
procedure gdk_display_warp_pointer(Display: PGdkDisplay;
  Screen: PGdkScreen; X, Y: GInt); cdecl; external gdklib;
// Since GTK >= 2.18
procedure gdk_window_get_root_coords(Window: PGdkWindow;
  X, Y: GInt; RootX, RootY: PGInt); cdecl; external gtklib;
{$endif}

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
// Available since GDK >= 2.2 .
// Missing in FPC Gdk2x unit, so we define it here.
function gdk_x11_screen_lookup_visual(Screen: PGdkScreen;
  XVisualid: TVisualID): PGdkVisual; cdecl; external gdklib;
{$endif}

function gtk_events_pending_bool: Boolean;
begin
  Result := gtk_events_pending() <> 0;
end;

{ Get GDK PGdkWindow from GTK PGtkWidget. }
function gtk_widget_get_window(const Widget: PGtkWidget): PGdkWindow;
begin
  Result := Widget^.Window;
end;

{ Check is GTK widget realized. }
function gtk_widget_get_realized(const Widget: PGtkWidget): Boolean;
begin
  Result := GTK_REALIZED and GTK_WIDGET_FLAGS(Widget) <> 0;
end;

{ Get PGtkColorSelection inside PGtkColorSelectionDialog.
  Result is always PGtkColorSelection, not just PGtkWidget,
  but we define it as returning PGtkWidget to match GTK 3 function.
  https://docs.gtk.org/gtk3/method.ColorSelectionDialog.get_color_selection.html }
function gtk_color_selection_dialog_get_color_selection(
  const ColorSelectionDialog: PGtkColorSelectionDialog): PGtkWidget;
begin
  Result := ColorSelectionDialog^.colorsel;
end;

procedure gtk_color_selection_set_current_rgba(const ColorSelection: PGtkColorSelection;
  const Color: PGdkRGBA);
begin
  // We can typecast PGdkRGBA to PGDouble, because it's a record of 4 GDouble-s.
  gtk_color_selection_set_color(ColorSelection, PGDouble(Color));
end;

procedure gtk_color_selection_get_current_rgba(const ColorSelection: PGtkColorSelection;
  const Color: PGdkRGBA);
begin
  gtk_color_selection_get_color(ColorSelection, PGDouble(Color));
end;

function gdk_event_get_scroll_direction(const Event: PGdkEvent; const Direction: PGdkScrollDirection): gboolean;
begin
  { Note: Declaring "Event: PGdkEventScroll" would be obviously more natural,
    as this is the type we actually need, and is actually passed.
    But we declare "Event: PGdkEvent" for compatibility with how GTK 3
    bindings declare this, so that GTK 2 and 3 calls look the same. }
  Direction^ := PGdkEventScroll(Event)^.Direction;
  if (Direction^ = GDK_SCROLL_UP) or
     (Direction^ = GDK_SCROLL_DOWN) or
     (Direction^ = GDK_SCROLL_LEFT) or
     (Direction^ = GDK_SCROLL_RIGHT) then
    Result := gtrue
  else
    Result := gfalse;
end;
