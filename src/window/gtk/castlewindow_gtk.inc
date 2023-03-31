{%MainUnit ../castlewindow.pas}
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleWindow backend based on GTK 2.

  TODO:
  - In OpenBackend implement MaxWidth/Height
    (Or maybe these properties should be removed?
    They are made for symmetry with MinWidth/Height. Are they really useful?)
  - Value of propery FullScreen could change at runtime.
    Observe window-state-event, change FullScreen at such times.
    This way I should be able to react to fullscreen changes
    forced by user (using window manager, not F11) really cleanly.
}

{$ifdef OpenGLES}
  {$I castlewindow_egl.inc}
{$else}
  {$I castlewindow_glx.inc}
{$endif}

{$ifdef read_interface_uses}
Glib2, Gdk2, Gtk2, CastleDynLib,
{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB} Gdk2X, X, Xlib, XUtil, {$endif}
{$endif}

{$ifdef read_implementation_uses}
CastleInternalWindowModes, CastleFindFiles,
{$endif}

{$ifdef read_interface_types}
  { @exclude }
  { For now I use GtkDrawingArea when CASTLE_WINDOW_GTK_2.
    But, really, GLAreaGtk could be any gtk widget with CASTLE_WINDOW_GTK_2. }
  PGtkGLArea = PGtkWidget;
{$endif read_interface_types}

{$ifdef read_window_interface}
private
  WindowGtk: PGtkWindow;
  GLAreaGtk: PGtkGLArea;

  { These things are initialized only when (MainMenu <> nil) and (not Closed). }
  window_vbox: PGtkVBox;
  window_accel_group: PGtkAccelGroup;

  { Run ADialog as a modal window.
    ADialog will be made modal (and "transient" for us in the GtkWindow variant).

    Then we will wait (while in TModeGLFrozenImage) for dialog to finish.
    PGtkDialog version just returns gint response, like gtk_dialog_run
    --- possible values like gtk_dialog_run, depending on dialog buttons.
    PGtkWindow version returns @true if closed by Ok button,
    otherwise (when closed by Cancel button, or "delete" event) returns @false.

    Overloaded version with PGtkDialog is a close analogy of
    gtk_dialog_run. In fact, it was written looking at implementation
    of gtk_dialog_run (http://git.gnome.org/browse/gtk+/tree/gtk/gtkdialog.c?h=gtk-2-18),
    although it's also very similar (even shares some internal helper data)
    with the GtkWindow variant.

    Never use GTK's gtk_dialog_run! (Reason: We have to run event loop by our
    Application.ProcessMessage, not by GTK's g_main_loop_run,
    to properly run Application.ProcessUpdates, to properly redraw /
    call Update etc.)

    @groupBegin }
  function GtkDialogRun(ADialog: PGtkWindow;
    ok_button, cancel_button: PGtkWidget): boolean;
  function GtkDialogRun(ADialog: PGtkDialog): gint;
  { @groupEnd }
  procedure GetInitialCursorPos;
  {$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
  { GLAreaGtk X11 identifier.
    TODO: Name is confusing, this is about GLAreaGtk, not from WindowGtk.
    It is used by castlewindow_glx.inc functions. }
  function WindowXID: TWindow;
  {$endif}
private
  FBorderWidth: Cardinal;

  { If > 0, you cannot recreate menu. }
  MenuRecreateForbidden: Integer;

  procedure UpdateCursor;
public
  { Change this only when Closed.
    This is the width of border of main GtkWindow that will be created
    in Open, set with gtk_container_set_border_width. }
  property BorderWidth: Cardinal
    read FBorderWidth write FBorderWidth default 0;
protected
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  { Find in OpenWindows window with WindowGtk = given seekWindowGtk.
    Return nil if no such window is found. }
  function TryFindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindow;
  { Find in OpenWindows window with GLAreaGtk = given seekGLAreaGtk.
    Return nil if no such window is found. }
  function TryFindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindow;

  { Same as TryFindWindowGtk, but exception EInternalError when
    no such window is found. }
  function FindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindow;
  { Same as TryFindGLAreaGtk, but exception EInternalError when
    no such window is found. }
  function FindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindow;

  { This must be constantly called.
    We use it for two tasks currently:
    1. calling Update and times events of Application and all our windows
    2. taking care of redraw }
  procedure ProcessUpdates;
private
  { Used by SetCursor, each item may be nil if not initialized. }
  Cursors: array [TMouseCursor] of PGdkCursor;

  FXDisplayName: string;

  InitializeXDisplayDone: boolean;

  XDisplayGdk: PGdkDisplay;
  XScreenGdk: PGdkScreen;

  { Initialize X display, calling gtk_init, gtk_gl_init, calculating
    XDisplayGdk and XScreenGdk.

    Call this only once you're sure that XDisplayName is set to
    a value desired by used (so, after parsing command-line parameters).

    Also, call this only when necessary, when you need to open a window
    (or get the screen parameters, like Application.ScreenWidth / ScreenHeight).
    This way, our GUI tools can perform a subset of their functionality
    (e.g. respond to --help or --version) even in a headless mode
    (without X server running). }
  procedure InitializeXDisplay;

  {$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
  { Used by castlewindow_glx.inc functions, used by XF86VMODE functions. }
  function XDisplay: Xlib.PDisplay;
  function XScreen: Integer;
  {$endif}
public
  { Set XDisplay name, this will be used for all TCastleWindow
    that will be subsequently initialized by TCastleWindow.Open.

    Note that this is exposed by GTK even for non-XWindows platforms,
    but I don't know what it does there. }
  property XDisplayName: string read FXDisplayName write FXDisplayName;
{$endif read_application_interface}

{$ifdef read_implementation}

{ various helpers ------------------------------------------------------------ }

{$I castlewindow_gtk_menu.inc}

{ This is mispelled in FPC gtk bindings, see
  [http://www.freepascal.org/bugs/showrec.php3?ID=5112] }
function gdk_display_open(display_name:Pgchar):PGdkDisplay;
  cdecl; external gdklib;

{ Some functions missing from FPC gtk bindings }
function gdk_cursor_new_for_display(Display: PGdkDisplay;
  cursor_type: TGdkCursorType): PGdkCursor;
  cdecl; external gdklib name 'gdk_cursor_new_for_display';
function gdk_display_get_default: PGdkDisplay;
  cdecl; external gdklib name 'gdk_display_get_default';
function gdk_screen_get_primary_monitor(Screen: PGdkScreen): GInt;
  cdecl; external gdklib name 'gdk_screen_get_primary_monitor';
function gdk_screen_get_resolution(screen: PGdkScreen): gdouble;
  cdecl; external gdklib;

{ Converts GDK keysym (from gdkkeysyms.pp, or gdk/gdkkeysym.h) to
  keyXxx constant (from Keys unit). Returns keyNone if no conversion is possible
  (gdk keysym has no corresponding TKey value). }
function GdkKeysymToKey(GdkKeysym: guint): TKey;
begin
  case GdkKeysym of
    GDK_KEY_BackSpace: Result := keyBackSpace;
    GDK_KEY_Tab: Result := keyTab;
    GDK_KEY_Return: Result := keyEnter;

    GDK_KEY_Escape: Result := keyEscape;
    GDK_KEY_Space: Result := keySpace;
    GDK_KEY_Page_Up:  Result := keyPageUp;
    GDK_KEY_Page_Down: Result := keyPageDown;
    GDK_KEY_End: Result := keyEnd;
    GDK_KEY_Home: Result := keyHome;
    GDK_KEY_Left: Result := keyArrowLeft;
    GDK_KEY_Up: Result := keyArrowUp;
    GDK_KEY_Right: Result := keyArrowRight;
    GDK_KEY_Down: Result := keyArrowDown;
    GDK_KEY_Insert: Result := keyInsert;
    GDK_KEY_Delete: Result := keyDelete;
    GDK_KEY_BracketLeft: Result := keyLeftBracket;
    GDK_KEY_BracketRight: Result := keyRightBracket;

    GDK_KEY_KP_Add: Result := keyNumpadPlus;
    GDK_KEY_KP_Subtract: Result := keyNumpadMinus;

    GDK_KEY_0..GDK_KEY_9: Result := TKey(Ord(key0) + GdkKeysym - GDK_KEY_0);

    GDK_KEY_Capital_A..GDK_KEY_Capital_Z: Result := TKey(Ord(keyA) + GdkKeysym - GDK_KEY_Capital_A);
    GDK_KEY_A        ..GDK_KEY_Z        : Result := TKey(Ord(keyA) + GdkKeysym - GDK_KEY_A);

    GDK_KEY_F1..GDK_KEY_F12: Result := TKey(Ord(keyF1) + GdkKeysym - GDK_KEY_F1);

    GDK_KEY_Comma: Result := keyComma;
    GDK_KEY_Period: Result := keyPeriod;
    GDK_KEY_Print: Result := keyPrintScreen;
    GDK_KEY_Caps_Lock: Result := keyCapsLock;
    GDK_KEY_Scroll_Lock: Result := keyScrollLock;
    GDK_KEY_Num_Lock: Result := keyNumLock;
    GDK_KEY_Pause: Result := keyPause;

    GDK_KEY_KP_0: Result := keyNumpad0;
    GDK_KEY_KP_1: Result := keyNumpad1;
    GDK_KEY_KP_2: Result := keyNumpad2;
    GDK_KEY_KP_3: Result := keyNumpad3;
    GDK_KEY_KP_4: Result := keyNumpad4;
    GDK_KEY_KP_5: Result := keyNumpad5;
    GDK_KEY_KP_6: Result := keyNumpad6;
    GDK_KEY_KP_7: Result := keyNumpad7;
    GDK_KEY_KP_8: Result := keyNumpad8;
    GDK_KEY_KP_9: Result := keyNumpad9;
    GDK_KEY_KP_End: Result := keyNumpadEnd;
    GDK_KEY_KP_Down: Result := keyNumpadDown;
    GDK_KEY_KP_Next: Result := keyNumpadPageDown;
    GDK_KEY_KP_Left: Result := keyNumpadLeft;
    GDK_KEY_KP_Begin: Result := keyNumpadBegin;
    GDK_KEY_KP_Right: Result := keyNumpadRight;
    GDK_KEY_KP_Home: Result := keyNumpadHome;
    GDK_KEY_KP_Up: Result := keyNumpadUp;
    GDK_KEY_KP_Prior: Result := keyNumpadPageUp;
    GDK_KEY_KP_Insert: Result := keyNumpadInsert;
    GDK_KEY_KP_Delete: Result := keyNumpadDelete;
    GDK_KEY_KP_Enter: Result := keyNumpadEnter;
    GDK_KEY_KP_Multiply: Result := keyNumpadMultiply;
    GDK_KEY_KP_Divide: Result := keyNumpadDivide;
    GDK_KEY_Apostrophe: Result := keyApostrophe;
    GDK_KEY_Semicolon: Result := keySemicolon;
    GDK_KEY_Slash: Result := keySlash;
    GDK_KEY_Grave: Result := keyBackQuote;
    GDK_KEY_Minus: Result := keyMinus;
    GDK_KEY_Plus: Result := keyPlus;
    GDK_KEY_Equal: Result := keyEqual;
    GDK_KEY_BackSlash: Result := keyBackSlash;

    else Result := keyNone;
  end;
end;

{ Converts GDK mouse button number (from GdkEventButton.button)
  to TCastleMouseButton. Returns false (and does not change mb) if such
  button number cannot be represented as TCastleMouseButton. }
function ButtonNumberToMouseButton(bn: guint; out mb: TCastleMouseButton): boolean;
begin
  { No need to implement this using dirty typecasts, it's simple enough
    to do it type-safe. }
  case bn of
    1: mb := buttonLeft;
    2: mb := buttonMiddle;
    3: mb := buttonRight;
    8: mb := buttonExtra1;
    9: mb := buttonExtra2;
    else Exit(false);
  end;
  Result := true;
end;

{ callbacks for gtk -----------------------------------------------------

  All gtk callback names are named signal_xxx where xxx is the gtk signal name.
  So all events have names like signal_yyy_event.

  Callbacks that expect as 1st parameter GLAreaGtk of some OpenWindows window
  have 1st parameter AGLAreaGtk: PGtkAreaGtk.
  Similarly, callbackas that expect as 1st parameter WindowGtk of some
  OpenWindows window have 1st parameter AWindowGtk: PGtkWindow.

  Note that we capture and handle exceptions here, we don't depend on
  Application.DoRun to catch exceptions from our ProcessMessage.
  This avoids problems with GTK: we *cannot* let any exception go out
  from the GTK callback, it causes further SEGFAULTs,
  probably because GTK is simply not prepared for a callback to raise exception.
  A simple test is to remove these try..except clauses, and instead do

    try
      gtk_main_iteration_do(WaitForMessage);
    except
      on E: TObject do
        HandleException(E);
    end;

  instead of direct

    gtk_main_iteration_do(WaitForMessage);

  in ProcessMessage. The result? At the end of HandleException, you get
  SEGFAULT at the end of TCastleWindow.MessageOK...
  (debugger doesn't show sensible stuff, it's like GtkDialogRun is Ok,
  and then any instruction just crashes.)
  I found no other explanation other than GTK memory got corrupted.
  Catching exception inside the callback, and handling it here, works OK.
}

{$define SIGNAL_GENERAL_BEGIN:=
try}
{$define SIGNAL_GENERAL_END:=
except
  on E: TObject do
    // sender is just Application, like TCustomApplication.Run does
    Application.HandleException(Application);
end}

{$define SIGNAL_GLAREA_BEGIN:=
begin
  with Application.FindGLAreaGtk(AGLAreaGtk) do
  begin
    SIGNAL_GENERAL_BEGIN}
{$define SIGNAL_GLAREA_END:=
    SIGNAL_GENERAL_END;
  end;
end}

{$define SIGNAL_WINDOW_BEGIN:=
begin
  with Application.FindWindowGtk(AWindowGtk) do
  begin
    SIGNAL_GENERAL_BEGIN}
{$define SIGNAL_WINDOW_END:=
    SIGNAL_GENERAL_END;
  end;
end}

function signal_expose_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventExpose;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
  { About Event^.Count:

    Event^.Count should be >= 0.

    If it's > 0 it means that more EXPOSE events were present in event queue
    when this expose event was sent.

    So (since we are here not redrawing a portion of our window, instead
    we're redrawing the whole window) one can think that we can safely ignore
    this expose event when Event^.Count > 0.
    We will redraw ourselves next time, when the event queue will not contain
    any more expose events.

    But: when AutoRedisplay := true (or a similiar situation,
    we're constantly doing Invalidate) it is not so safe to do this
    because it is possible then that EVERY event will have Event^.Count > 0.
    I.e. we may not process expose events fast enough, and there always
    will be some expose event in queue when another expose event arrives,
    so always Event^.Count > 0.

    Actually, GDK does some exposure events compression itself.
    I don't know what exactly this means. But I guess that
    stupid Invalidate calls will be ignored, so new expose events will be
    added to queue only when no other expose events exist there.
    So every event will have Event^.Count = 0. But this STILL means that
    checking here is (Event^.Count > 0) is useless !

    Test below confirms that:
      if Event.Count > 0 then
       Writeln('Have Expose event with Event.Count = ', Event.Count);
  }

  Invalidate;

  Result := gtrue;
SIGNAL_GLAREA_END;

function signal_window_configure_event(AWindowGtk: PGtkWindow;
  event: PGdkEventConfigure; data: gpointer): gboolean; cdecl;
{ This signal is registered for WindowGtk, not GLAreaGtk.
  That's because we want to update here Left/Top.
  And position of GLAreaGtk is constant, only WindowGtk position changes. }
var
  LeftGtk, TopGtk: gint;
SIGNAL_WINDOW_BEGIN
  { I could ask here for GLAreaGtk global position.
    But as of now, Left and Top are coordinates of whole WindowGtk
    (with WindowManager decorations, that's how
    gdk_window_get_root_origin works).
    Reasons -- the same as in castlewindow_xlib.inc. }
  gdk_window_get_root_origin(GTK_WIDGET(WindowGtk)^.window, @LeftGtk, @TopGtk);
  FLeft := LeftGtk;
  FTop := TopGtk;
  { for the sake of gtk, I can't tell that I "handled" this in any way }
  Result := gfalse;
SIGNAL_WINDOW_END;

function signal_glarea_configure_event(AGLAreaGtk: PGtkGLArea;
  event: PGdkEventConfigure; data: gpointer): gboolean; cdecl;
{ We could probably do this in signal_window_configure_event,
  since GLAreaGtk size changes only when WindowGtk size changes.
  But I was simply unable to find a function in gtk that does it in a clean
  way (getting GTK_WIDGET(GLAreaGtk).Allocation.Width does not work as it
  should). This signal ensures that Event.Width/Height are exactly what
  I want.

  Moreover, this way ensures that after only changing the window position
  no DoResize will be called. And that's good -- there's no point in calling
  DoResize when only Left/Top changed. }
SIGNAL_GLAREA_BEGIN
  DoResize(Event^.Width, Event^.Height, false);
  { for the sake of gtk, I can't tell that I "handled" this in any way }
  Result := gfalse;
SIGNAL_GLAREA_END;

function signal_delete_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
SIGNAL_WINDOW_BEGIN
  DoCloseQuery;
  Result := gtrue;
SIGNAL_WINDOW_END;

function signal_key_press_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
var
  Key: TKey;
  KeyString: string;
SIGNAL_WINDOW_BEGIN
  case Event^.KeyVal of
    GDK_KEY_Shift_L:   SetPrivateModifiersDown(mkShift, false, true);
    GDK_KEY_Shift_R:   SetPrivateModifiersDown(mkShift, true,  true);
    GDK_KEY_Control_L: SetPrivateModifiersDown(mkCtrl,  false, true);
    GDK_KEY_Control_R: SetPrivateModifiersDown(mkCtrl,  true,  true);
    GDK_KEY_Alt_L:     SetPrivateModifiersDown(mkAlt,   false, true);
    GDK_KEY_Alt_R:     SetPrivateModifiersDown(mkAlt,   true,  true);
    else
    begin
      Key := GdkKeysymToKey(Event^.KeyVal);
      KeyString := Event^._string;

      if (Key <> keyNone) or (KeyString <> '') then
      begin
        { Fix some cases when KeyString should better correspond to Key. }
        if KeyString = '' then
          case Key of
            {$ifdef MSWINDOWS}
            { It seems that GTK 1.3 for Windows cannot translate GDK_KEY_Escape and
              GDK_KEY_Return to standard chars (#13 and #27). So I'm fixing it here. }
            keyEscape   : KeyString := CharEscape;
            keyEnter    : KeyString := CharEnter;
            {$endif}
            { It seems that GTK 2 doesn't translate backspace and tab to
              appropriate chars. So I'm fixing it here. }
            keyTab      : KeyString := CharTab;
            keyBackSpace: KeyString := CharBackSpace;
            keyDelete   : KeyString := CharDelete;
            else ;
          end;

        DoKeyDown(Key, KeyString);
      end;
    end;
  end;

  { It would be nice to say here Result := true if key was handled
    in DoKeyDown. But I don't have any indicator whether key was handled
    in EventPress or OnPress (because such thing would complicate
    implementation of OnPress callback in every program using
    CastleWindow). So I must assume that either (1) always or (2) never
    when I called DoKeyDown key is handled.
    But (1) is actually not acceptable, because it disallows user
    to activate menu using F10 key. So I must set Result := false here.

    I'm also setting Result := false in signal_key_release_event,
    to be consequent.
  }
  Result := gfalse;
SIGNAL_WINDOW_END;

function signal_key_release_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
var
  Key: TKey;
SIGNAL_WINDOW_BEGIN
  case Event^.KeyVal of
    GDK_KEY_Shift_L:   SetPrivateModifiersDown(mkShift, false, false);
    GDK_KEY_Shift_R:   SetPrivateModifiersDown(mkShift, true,  false);
    GDK_KEY_Control_L: SetPrivateModifiersDown(mkCtrl,  false, false);
    GDK_KEY_Control_R: SetPrivateModifiersDown(mkCtrl,  true,  false);
    GDK_KEY_Alt_L:     SetPrivateModifiersDown(mkAlt,   false, false);
    GDK_KEY_Alt_R:     SetPrivateModifiersDown(mkAlt,   true,  false);
    else
    begin
      Key := GdkKeysymToKey(Event^.KeyVal);
      if Key <> keyNone then DoKeyUp(Key);
    end;
  end;

  { Why false, not true ? See comments at the end of
    signal_key_press_event. }
  Result := gfalse;
SIGNAL_WINDOW_END;

function signal_button_press_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TCastleMouseButton;
SIGNAL_GLAREA_BEGIN
  Result := (Event^._type = GDK_BUTTON_PRESS) and
    ButtonNumberToMouseButton(Event^.button, mb);
  if Result then
    DoMouseDown(LeftTopToCastle(Event^.x, Event^.y), mb, 0);
SIGNAL_GLAREA_END;

function signal_button_release_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TCastleMouseButton;
SIGNAL_GLAREA_BEGIN
  Result := (Event^._type = GDK_BUTTON_RELEASE) and
    ButtonNumberToMouseButton(Event^.button, mb);
  if Result then
    DoMouseUp(LeftTopToCastle(Event^.x, Event^.y), mb, 0);
SIGNAL_GLAREA_END;

function signal_motion_notify_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventMotion;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
  DoMotion(InputMotion(MousePosition,
    LeftTopToCastle(Event^.x, Event^.y), MousePressed, 0));
  Result := gtrue;
SIGNAL_GLAREA_END;

function signal_scroll_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventScroll;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
  case Event^.Direction of
    GDK_SCROLL_UP:   DoMouseWheel( 1, true);
    GDK_SCROLL_DOWN: DoMouseWheel(-1, true);
    GDK_SCROLL_LEFT: DoMouseWheel( 1, false);
    GDK_SCROLL_RIGHT:DoMouseWheel(-1, false);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else WritelnWarning('GTK', 'Invalid GdkEventScroll.direction');
    {$endif}
  end;
  Result := gtrue;
SIGNAL_GLAREA_END;

procedure signal_menu_item_activate(AMenuItemGtk: PGtkMenuItem;
  data: gpointer); cdecl;
var
  MenuItem: TMenuItem;
  Window: TCastleWindow;
begin
  SIGNAL_GENERAL_BEGIN
    MenuItem := MenuItemFromSmallId( PtrInt(Data) );
    Window := TCastleWindow( gtk_object_get_user_data(GTK_OBJECT(AMenuItemGtk)) );

    if MenuItem is TMenuItemChecked then
    begin
      { GTK checked menu items do always something like our AutoCheckedToggle.
        This assert confirms that: }

      if not (MenuItem is TMenuItemRadio) then
        Assert( gtk_check_menu_item_get_active(PGtkCheckMenuItem(AMenuItemGtk)) <>
                TMenuItemChecked(MenuItem).Checked);

      { I don't want this (because, first of all, not always my
        TMenuItemChecked will have AutoCheckedToggle = true.
        Second, when not MenuActive, I do not want such behavior, even for
        AutoCheckedToggle = true.)

        So below I'm simply returning such GTK menu item to it's previous state.

        Note that we CAN'T do here simple
        1. gtk_check_menu_item_set_active(PGtkCheckMenuItem(AMenuItemGtk),
            not (gtk_check_menu_item_get_active(AMenuItemGtk)));
        or (equivalent)
        2. gtk_check_menu_item_set_active(PGtkCheckMenuItem(AMenuItemGtk),
            TMenuItemChecked(MenuItem).Checked);
        because gtk_check_menu_item_set_active
        would call THIS SIGNAL ONCE AGAIN when we change active state !
        This means that inside gtk_check_menu_item_set_active this
        procedure would be called once again (if we were using 1.,
        this would already be a bug, because this would lead to recursive
        call. If we were using 2., the bug will be
        because we would call Window.DoMenuClick(MenuItem) TWICE.

        So I must temporary block this signal before doing
        gtk_check_menu_item_set_active. }
      MenuItemBlock(AMenuItemGtk, MenuItem);
      try
        gtk_check_menu_item_set_active(PGtkCheckMenuItem(AMenuItemGtk),
          TMenuItemChecked(MenuItem).Checked);
      finally
        MenuItemUnblock(AMenuItemGtk, MenuItem);
      end;
    end;

    Window.DoMenuClick(MenuItem);
  SIGNAL_GENERAL_END;
end;

function signal_window_focus_out_event(AWindowGtk: PGtkWindow;
  Event: PGdkEventfocus; data: gpointer): gboolean; cdecl;
{ This is called when user switches to another window. }
SIGNAL_WINDOW_BEGIN
  ReleaseAllKeysAndMouse;
  { for the sake of gtk, I can't tell that I "handled" this in any way }
  FFocused := false;
  Result := gfalse;
SIGNAL_WINDOW_END;

function signal_window_focus_in_event(AWindowGtk: PGtkWindow;
  Event: PGdkEventfocus; data: gpointer): gboolean; cdecl;
SIGNAL_WINDOW_BEGIN
  { Reset FMousePosition, since mouse position is unknown now,
    and do not activate mouse look at next frame.
    Note: resetting with FMousePosition := Vector2(-1, -1);
    is not enough, testcase: wrong 1st movement in Unholy after alt+tab to fight. }
  GetInitialCursorPos;
  Container.MouseLookIgnoreNextMotion;
  FFocused := true;
  Result := gfalse;
SIGNAL_WINDOW_END;

{ TCastleWindow ------------------------------------------------------------ }

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      UpdateCursor;
  end;
end;

procedure TCastleWindow.UpdateCursor;

  function CreateNoneCursor: PGdkCursor;
  const
    Bits: array[0..0] of Guchar = (0);
    Color: TGdkColor = (Pixel: 0; Red: 0; Green: 0; Blue: 0);
  var
    Pixmap: PGdkPixmap;
  begin
    { Based on [http://mail.gnome.org/archives/gtk-app-devel-list/2005-January/msg00370.html] }
    Pixmap := gdk_bitmap_create_from_data(nil, @Bits, 1, 1);
    try
      Result := gdk_cursor_new_from_pixmap(Pixmap, Pixmap, @Color, @Color, 0, 0);
    finally gdk_pixmap_unref(Pixmap); end;
  end;

const
  { GDK cursors.
    See https://developer.gnome.org/gdk3/stable/gdk3-Cursors.html for the list. }
  GdkCursorTypeFromCastle: array [mcStandard .. High(TMouseCursor)] of
    TGdkCursorType =
  (
    GDK_LEFT_PTR, //< mcStandard.
                  // Note that GDK_ARROW seems standard under XWindows
                  // (it is a right-top arrow),
                  // but actually in modern applications it is more standard
                  // to use left-top arrow in GDK_LEFT_PTR.
    GDK_WATCH,                // mcWait
    GDK_XTERM,                // mcText
    GDK_HAND2,                // mcHand
    GDK_SB_V_DOUBLE_ARROW,    // mcResizeVertical
    GDK_SB_H_DOUBLE_ARROW,    // mcResizeHorizontal
    GDK_TOP_LEFT_CORNER,      // mcResizeTopLeft
    GDK_TOP_SIDE,             // mcResizeTop
    GDK_TOP_RIGHT_CORNER,     // mcResizeTopRight
    GDK_LEFT_SIDE,            // mcResizeLeft
    GDK_RIGHT_SIDE,           // mcResizeRight
    GDK_BOTTOM_LEFT_CORNER,   // mcResizeBottomLeft
    GDK_BOTTOM_SIDE,          // mcResizeBottom
    GDK_BOTTOM_RIGHT_CORNER   // mcResizeBottomRight
  );
begin
  if Application.Cursors[InternalCursor] = nil then
  begin
    { initialize Application.Cursors[Cursor] }
    case InternalCursor of
      mcDefault:
        { don't do anything, Application.Cursors[mcDefault] should be nil,
          gdk_window_set_cursor will understand nil as "use parent"
          and that's exactly what we want for mcDefault. };
      mcNone, mcForceNone: Application.Cursors[InternalCursor] := CreateNoneCursor;
      else
        Application.Cursors[InternalCursor] := gdk_cursor_new_for_display(
          Application.XDisplayGdk, GdkCursorTypeFromCastle[InternalCursor]);
    end;
  end;

  gdk_window_set_cursor(GTK_WIDGET(GLAreaGtk)^.Window, Application.Cursors[InternalCursor]);
end;

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
// Since GDK >= 2.2
function gdk_x11_screen_lookup_visual(Screen: PGdkScreen; XVisualid: TVisualID): PGdkVisual;
  cdecl; external gdklib;
{$else}
// Since GDK >= 2.8
procedure gdk_display_warp_pointer(Display: PGdkDisplay;
  Screen: PGdkScreen; X, Y: GInt); cdecl; external gdklib;
// Since GTK >= 2.18
procedure gdk_window_get_root_coords(Window: PGdkWindow;
  X, Y: GInt; RootX, RootY: PGInt); cdecl; external gtklib;
{$endif}

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
function TCastleWindow.WindowXID: TWindow;
begin
  Result := gdk_x11_drawable_get_xid(GTK_WIDGET(GLAreaGtk)^.Window);
end;
{$endif}

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
var
  P: TVector2Integer;
{$ifndef CASTLE_WINDOW_GTK_WITH_XLIB}
  RootX, RootY: GInt;
{$endif}
begin
  if not Closed then
  begin
    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    P := CastleToLeftTopInt(Value);

    {$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
    { With older GDK/GTK, you had to use XWarpPointer to position mouse.
      We still use it, for now, for GTK under Xlib --- this way we work
      even with ancient GTK versions. }
    XWarpPointer(
      gdk_x11_drawable_get_xdisplay(GTK_WIDGET(GLAreaGtk)^.Window),
      X.None,
      WindowXID,
      0, 0, 0, 0, P.X, P.Y);
    {$else}
      gdk_window_get_root_coords(GTK_WIDGET(GLAreaGtk)^.Window,
        P.X, P.Y, @RootX, @RootY);
      gdk_display_warp_pointer(Application.XDisplayGdk,
        Application.XScreenGdk, RootX, RootY);
    {$endif}
  end;
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then
    gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(GetWholeCaption));
end;

procedure TCastleWindow.GetInitialCursorPos;
var
  X, Y: GInt;
begin
  gdk_window_get_pointer(GTK_WIDGET(GLAreaGtk)^.Window, @X, @Y, nil);
  FMousePosition := LeftTopToCastle(X, Y);
end;

procedure TCastleWindow.OpenBackend;

{ Notes about FullScreen implementation for GTK_2:

  I'm doing fullscreen using gtk_window_fullscreen,
  this tells window manager that I want to be fullscreen
  using XAtom _NET_WM_STATE_FULLSCREEN.
  This avoids using override_redirect (so gtk menu is usable,
  and user can use window-manager key shortcuts like Alt+Tab
  with our fullscreen window) and at the same time window is on top
  (it should not be covered by things like gnome-panel).
}

const
  VBoxSpacing = 2;

  procedure set_default_glarea_size(Width, Height: Integer);
  { This requires MainMenu.Handle to be initialized (only if MainMenu <> nil,
    of course ) }
  var
    WindowWidth, WindowHeight: Integer;
    WindowReq: TGtkRequisition;
  begin
    { Implementation of this is really non-elegant.
      But I do not know how to implement this in GTK+ (even if I would use
      GTK+ 2.x). I would like something like
        gtk_widget_set_default_size(GLAreaGtk, Width, Height)
      but there is no gtk_widget_set_default_size function, there is only
      gtk_window_set_default_size. I do NOT want to set minimal area of a widget,
      I want default size (that could be later made smaller by a user),
      and I do not want to give non-standard configuration values to
      gtk_window_set_policy. So I do the only solution that I know of:
      I'm simply doing gtk_window_set_default_size, enlarging my Height
      by menu height and VBoxSpacing. }
    WindowWidth := Width;
    WindowHeight := Height;
    if (MainMenu <> nil) and MainMenuVisible then
    begin
      gtk_widget_size_request(GTK_WIDGET(MainMenu.Handle), @WindowReq);
      WindowHeight += WindowReq.Height + VBoxSpacing;
    end;
    gtk_window_set_default_size(WindowGtk, WindowWidth, WindowHeight);
  end;

  procedure ContextNotPossible;
  begin
   raise EGLContextNotPossible.CreateFmt(
     'Cannot initialize OpenGL context with requested attributes (%s)',
     [ RequestedBufferAttributes ]);
  end;

  procedure InitializeDpi;
  var
    ScreenWidthMM: Single;
    NewDpi: Single;
  begin
    NewDpi := gdk_screen_get_resolution(Application.XScreenGdk);
    if NewDpi <= 0 then
    begin
      ScreenWidthMM := gdk_screen_width_mm();
      if ScreenWidthMM <= 0 then
      begin
        { Lazarus LCL also secures from this case, with comment:
          "some TV-out screens don't know there size" (GetScreenWidthMM). }
        NewDpi := DefaultDpi;
      end else
        NewDpi := gdk_screen_width() / (ScreenWidthMM / 25.4);
    end;
    Container.Dpi := NewDpi;
    // Too verbose by default, UpdateUIScale will show it if UIScaling is usDpiScale.
    // WritelnLog('GTK', 'Dots (pixels) per inch is %f (%f * default %f)',
    //   [NewDpi, NewDpi / DefaultDpi, DefaultDpi]);
  end;

{$ifndef OpenGLES}
var
  GdkVisual: PGdkVisual;
  GdkColormap: PGdkColormap;
{$endif}
begin
  Application.InitializeXDisplay;

  WindowGtk := PGtkWindow(gtk_window_new(GTK_WINDOW_TOPLEVEL));

  if (GtkIconName <> '') and
     { gtk_window_set_icon_name exists since GTK 2.6.
       (Although I officially require GTK 2.6, this function is really
       not critical, so we check it dynamically.) }
     Assigned(@gtk_window_set_icon_name) then
    gtk_window_set_icon_name(WindowGtk, PCharOrNil(GtkIconName));

  gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(GetWholeCaption));
  gtk_container_set_border_width(GTK_CONTAINER(WindowGtk), BorderWidth);

  { Create XVisual }
  ContextCreateBegin;

  {$ifndef OpenGLES}
  { Create GdkColormap using XVisual }
  GdkVisual := gdk_x11_screen_lookup_visual(Application.XScreenGdk, XVisual^.VisualId);
  GdkColormap := gdk_colormap_new(GdkVisual, gfalse);
  gtk_widget_push_colormap(GdkColormap);
  {$endif}

  { Create now GLAreaGtk, which will use GdkColormap }
  GLAreaGtk := gtk_drawing_area_new;
  gtk_widget_set_double_buffered(GLAreaGtk, gfalse); // following Lazarus GTK OpenGL, not sure if needed

  // Cleanup after gtk_widget_push_colormap called above
  gtk_widget_pop_colormap();

  {$ifndef OpenGLES}
  { We should pass WindowXID to be correct.
    But:
    - it is not ready yet (widget is not realized, and calling WindowXID
      would just cause warning
      "... Gdk-WARNING **: ....: /build/gtk+2.0-ABXlu1/gtk+2.0-2.24.32/gdk/x11/gdkdrawable-x11.c:952 drawable is not a pixmap or window"
    - it is actually not used by castlewindow_glx.inc now.

    So, as a quick solution, we pass 0.
  }
  ContextCreateEnd({ WindowXID } 0);
  {$endif}

  gtk_widget_show(GTK_WIDGET(GLAreaGtk));

  { connect signal handlers to GLAreaGtk }
  { What events to catch ? It must cover all signal_yyy_event functions that we
    will connect. This must be called before X Window is created. }
  gtk_widget_set_events(GTK_WIDGET(GLAreaGtk),
      GDK_EXPOSURE_MASK {for expose_event} or
      GDK_BUTTON_PRESS_MASK {for button_press_event} or
      GDK_BUTTON_RELEASE_MASK {for button_release_event} or
      GDK_POINTER_MOTION_MASK {for motion_notify_event}
      { we don't have to tell here that we want configure_event,
        it is always called });

  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'configure_event',
    GTK_SIGNAL_FUNC(@signal_glarea_configure_event), nil);
  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'expose_event',
    GTK_SIGNAL_FUNC(@signal_expose_event), nil);
  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'button_press_event',
    GTK_SIGNAL_FUNC(@signal_button_press_event), nil);
  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'button_release_event',
    GTK_SIGNAL_FUNC(@signal_button_release_event), nil);
  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'motion_notify_event',
    GTK_SIGNAL_FUNC(@signal_motion_notify_event), nil);
  gtk_signal_connect(GTK_OBJECT(GLAreaGtk), 'scroll_event',
    GTK_SIGNAL_FUNC(@signal_scroll_event), nil);

  { connect signal handlers to WindowGtk }
  gtk_widget_set_events(GTK_WIDGET(WindowGtk),
      GDK_KEY_PRESS_MASK {for key_press} or
      GDK_KEY_RELEASE_MASK {for key_release}
      { we don't have to tell here that we want configure_event,
        it is always called });

  gtk_signal_connect(GTK_OBJECT(WindowGtk), 'configure_event',
    GTK_SIGNAL_FUNC(@signal_window_configure_event), nil);
  gtk_signal_connect (GTK_OBJECT(WindowGtk), 'delete_event',
    GTK_SIGNAL_FUNC(@signal_delete_event), nil);
  gtk_signal_connect(GTK_OBJECT(WindowGtk), 'key_press_event',
    GTK_SIGNAL_FUNC(@signal_key_press_event), nil);
  gtk_signal_connect(GTK_OBJECT(WindowGtk), 'key_release_event',
    GTK_SIGNAL_FUNC(@signal_key_release_event), nil);
  gtk_signal_connect(GTK_OBJECT(WindowGtk), 'focus_out_event',
    GTK_SIGNAL_FUNC(@signal_window_focus_out_event), nil);
  gtk_signal_connect(GTK_OBJECT(WindowGtk), 'focus_in_event',
    GTK_SIGNAL_FUNC(@signal_window_focus_in_event), nil);

  { Add us to OpenWindows windows to ensure that all callbacks that are
    eventually called by procedures below will be able to use
    Application.FindGLAreaGtk and Application.FindWindowGtk. }
  Application.OpenWindowsAdd(Self);

  { setup window position from Left, Top }
  gtk_widget_set_uposition(GTK_WIDGET(WindowGtk), Left, Top);

  if (MainMenu <> nil) and MainMenuVisible then
  begin
    window_accel_group := gtk_accel_group_new;
    gtk_window_add_accel_group(GTK_WINDOW(WindowGtk), window_accel_group);

    window_vbox := PGtkVBox(gtk_vbox_new(gfalse, VBoxSpacing));
    gtk_widget_show(GTK_WIDGET(window_vbox));
    MenuInitialize;
    gtk_box_pack_end_defaults(PGtkBox(window_vbox), GTK_WIDGET(GLAreaGtk));
    gtk_container_add(GTK_CONTAINER(WindowGtk), GTK_WIDGET(window_vbox));
  end else
    gtk_container_add(GTK_CONTAINER(WindowGtk), GTK_WIDGET(GLAreaGtk));

  { setup window/glarea size and resize policy from
    MinWidth, MinHeight, Width, Height, ResizeAllowed }
  if FullScreen then
  begin
    { Applying FullScreen changes to the Left / Top / Width / Height.
      Call DoResize (not perfect, since it doesn't account for menu bar
      height, but better than nothing), in case GTK will not send
      us proper resize event (but it will send up proper resize event,
      tests show). }
    FLeft := 0;
    FTop := 0;
    DoResize(Application.ScreenWidth, Application.ScreenHeight, false);

    { When in FullScreen we set whole window size, not just GLArea size, to screen size.
      This is the ultimate sense of FullScreen,
      so there is no discussion, no matter what value has ResizeAllowed.
      So I'm doing gtk_widget_set_usize(WindowGtk, ...) and
                   gtk_window_set_default_size
      instead of
                   gtk_widget_set_usize(GLAreaGtk, ...) and
                   set_default_glarea_size
    }
    gtk_widget_set_usize(GTK_WIDGET(WindowGtk), MinWidth, MinHeight);
    gtk_window_set_default_size(WindowGtk, Application.ScreenWidth, Application.ScreenHeight);

    gtk_window_fullscreen(WindowGtk);
  end else
  begin
    if ResizeAllowed <> raAllowed then
      gtk_widget_set_usize(GTK_WIDGET(GLAreaGtk), Width, Height)
    else
      gtk_widget_set_usize(GTK_WIDGET(GLAreaGtk), MinWidth, MinHeight);
    set_default_glarea_size(Width, Height);
    gtk_window_set_resizable(WindowGtk, ResizeAllowed = raAllowed);
  end;

  { Honour display (XScreenGdk is default screen on chosen display)
    chosen by user. }
  gtk_window_set_screen(WindowGtk, Application.XScreenGdk);

  if Visible then
  begin
    { Show WindowGtk, it will also make visible all other widgets.
      (Their "gtk_widget_show" calls only scheduled them to be shown
      when this top-level window is shown.)

      gtk_widget_show also realizes (allocates X resources) the widget
      and all it's children before returning (but does not necessarily
      map the Gdk window *now*, one can use gtk_widget_show_now for this;
      but I don't need it) }
    gtk_widget_show(GTK_WIDGET(WindowGtk));
  end else
  begin
    { We want to realize all our GTK resources now, to finish
      our initialization. So we can call gtk_widget_realize.
      This automatically realizes parents of passed object, so if I pass here
      GLAreaGtk, then also WindowGtk will get realized, and that's all I need.

      See [http://library.gnome.org/devel/gtk/stable/GtkWidget.html#gtk-widget-realize]
      and [http://developer.gnome.org/doc/GGAD/faqs.html]
      ("When do I need to call gtk_widget_realize() vs. gtk_widget_show()?"
      question). }
    gtk_widget_realize(GTK_WIDGET(GLAreaGtk));
  end;

  {$ifdef OpenGLES}
  { In case of EGL (OpenGLES) we really need WindowXID.
    So we cannot use previous workaround, instead we wait for a good time
    to get WindowXID, when widget is realized.
    TODO: Should we just do it with glX version too? }
  ContextCreateEnd(WindowXID);
  {$endif}

  { From this point I require that WindowGtk is realized (has associated
    GdkWindow, i.e. I can use GTK_WIDGET(WindowGtk).Window).
    It also means that I can do MakeCurrent (and call OpenGL commands)
    from now. In particular, we have valid gl context when we leave OpenBackend. }
  if (GTK_REALIZED and GTK_WIDGET_FLAGS( GTK_WIDGET(WindowGtk) )) = 0 then
    raise EInternalError.Create('CastleWindow.OpenBackend: GdkWindow not realized yet');

  GetInitialCursorPos;
  UpdateCursor;
  InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
begin
  { Do not unref here GLAreaGtk.
    GLAreaGtk is placed inside WindowGtk when we do
    gtk_container_add(WindowGtk, GLAreaGtk). During gtk_container_add
    WindowGtk calls sink on GLAreaGtk, so GLAreaGtk has reference 1 and it
    means that it is referenced from WindowGtk. So WindowGtk is responsible
    for calling unref on GLAreaGtk. }
  GLAreaGtk := nil;

  if (MainMenu <> nil) and MainMenuVisible then
    MenuFinalize;

  window_vbox := nil;

  if window_accel_group <> nil then
  begin
    g_object_unref(G_OBJECT(window_accel_group));
    window_accel_group := nil;
  end;

  { Note: we could use
    gtk_quit_add_destroy(1, GTK_OBJECT(WindowGtk));
    in OpenBackend instead of doing gtk_widget_unref below.
    But doing gtk_widget_unref below is a cleaner way (we're freeing WindowGtk
    as soon as it can be freed). }
  if WindowGtk <> nil then
  begin
    if Visible then
      gtk_widget_hide(GTK_WIDGET(WindowGtk));

    { Doing here
        gtk_widget_unref(GTK_WIDGET(WindowGtk));
      instead of gtk_widget_destroy causes
        Gtk-CRITICAL **: file gtkobject.c: line 1179 (gtk_object_unref):
        assertion `object->ref_count > 0' failed.

      If I understand some comments (e.g. at GtkWidget.gtk_widget_destroy)
      properly GTK+ holds a reference to toplevel windows. Again, if I understand
      it properly, this means that my program is NOT treated as the owner
      of a reference to WindowGtk. So my program should not do unref on
      WindowGtk. Instead my program should call gtk_widget_destroy and
      gtk_widget_destroy will automatically do unref to release the
      reference GTK+ has to toplevel WindowGtk window (and gtk_widget_destroy
      will also tell GTK+ to remove WindowGtk from it's internal list of
      toplevel windows and do some other needed by GTK+ things). }
    gtk_widget_destroy(GTK_WIDGET(WindowGtk));
    WindowGtk := nil;
  end;
end;

procedure TCastleWindow.CreateBackend;
begin
  { We don't do here anything, and thus it doesn't require calling
    Application.InitializeXDisplay. This way, merely calling
    TCastleWindow.Create doesn't require X server runnning. }
end;

{ TCastleWindow.*Dialog --------------------------------------------------- }

type
  { Data type for GtkDialogRun (both variants).
    PDialogData will be passed as a 2nd argument to signals
    (and 3rd argument to events) associated with dialog boxes. }
  TDialogData = record
    Answered: boolean; { = false }
    Answer: gint;
  end;
  PDialogData = ^TDialogData;

procedure signal_dialog_ok_clicked(AWidget: PGtkWidget; Data: PDialogData); cdecl;
begin
  Data^.Answered := true;
  Data^.Answer := GTK_RESPONSE_ACCEPT;
end;

procedure signal_dialog_cancel_clicked(AWidget: PGtkWidget;
  Data: PDialogData); cdecl;
begin
  Data^.Answered := true;
  Data^.Answer := GTK_RESPONSE_REJECT;
end;

function signal_dialog_delete_event(AWidget: PGtkWidget; Event: PGdkEventAny;
  Data: PDialogData): gboolean; cdecl;
begin
  Data^.Answered := true;
  Data^.Answer := GTK_RESPONSE_DELETE_EVENT;
  { don't allow to call gtk_widget_destroy, we will call it ourselves
    in BackendFileDialog/ColorDialog. }
  Result := gtrue;
end;

procedure signal_dialog_unmap(ADialog: PGtkDialog; Data: PDialogData); cdecl;
begin
  Data^.Answered := true;
  Data^.Answer := GTK_RESPONSE_NONE;
end;

procedure signal_dialog_response(ADialog: PGtkDialog; ResponseId: GInt;
  Data: PDialogData); cdecl;
begin
  Data^.Answered := true;
  Data^.Answer := ResponseId;
end;

function TCastleWindow.GtkDialogRun(ADialog: PGtkWindow;
  ok_button, cancel_button: PGtkWidget): boolean;
var
  Data: TDialogData;
  Mode: TGLModeFrozenScreen;
begin
  gtk_signal_connect(GTK_OBJECT(ok_button),
    'clicked',      GTK_SIGNAL_FUNC(@signal_dialog_ok_clicked),     @Data);
  gtk_signal_connect(GTK_OBJECT(cancel_button),
    'clicked',      GTK_SIGNAL_FUNC(@signal_dialog_cancel_clicked), @Data);
  gtk_signal_connect(GTK_OBJECT(ADialog),
    'delete_event', GTK_SIGNAL_FUNC(@signal_dialog_delete_event),   @Data);

  gtk_window_set_modal(ADialog, gtrue);
  gtk_window_set_transient_for(ADialog, WindowGtk);

  { prepare Data }
  Data.Answered := false;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    gtk_widget_show(GTK_WIDGET(ADialog));
    while not Data.Answered do Application.ProcessMessage(true, true);
  finally Mode.Free end;

  Result := Data.Answer = GTK_RESPONSE_ACCEPT;
end;

function TCastleWindow.GtkDialogRun(ADialog: PGtkDialog): gint;
var
  Data: TDialogData;
  Mode: TGLModeFrozenScreen;
  SigHandle1, SigHandle2, SigHandle3: GULong;
begin
  SigHandle1 := gtk_signal_connect(GTK_OBJECT(ADialog),
    'response',    GTK_SIGNAL_FUNC(@signal_dialog_response),     @Data);
  SigHandle2 := gtk_signal_connect(GTK_OBJECT(ADialog),
    'unmap',       GTK_SIGNAL_FUNC(@signal_dialog_unmap),        @Data);
  SigHandle3 := gtk_signal_connect(GTK_OBJECT(ADialog),
    'delete_event',GTK_SIGNAL_FUNC(@signal_dialog_delete_event), @Data);

  { gtk_dialog_run contains here also safeguards against destroying
    the dialog when it runs, by using g_object_ref/unref around and
    registering signal on destroy of ADialog. We do not need this,
    nothing can destroy the dialog AFAIK. }

  gtk_window_set_modal(GTK_WINDOW(ADialog), gtrue);

  Data.Answered := false;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    gtk_widget_show(GTK_WIDGET(ADialog));
    while not Data.Answered do Application.ProcessMessage(true, true);
  finally Mode.Free end;

  { Disconnect these signal handlers. gtk_dialog_run also does this.

    Otherwise under x86_64/Linux we get segfaults at gtk_widget_destroy
    (usually done by the caller of GtkDialogRun).
    Seen both with FPC 2.2.4 and 2.4.0, probably FPC version doesn't matter.
    These are probably caused by gtk_widget_destroy calling some signal
    (probably 'unmap') when destroying, that gets invalid @Data pointer
    and messes with memory. }
  gtk_signal_disconnect(GTK_OBJECT(ADialog), SigHandle1);
  gtk_signal_disconnect(GTK_OBJECT(ADialog), SigHandle2);
  gtk_signal_disconnect(GTK_OBJECT(ADialog), SigHandle3);

  Result := Data.Answer;
end;

function GtkVersionAtLeast(const Major, Minor, Micro: Cardinal): boolean;
begin
  Result :=
     (gtk_major_version > Major) or
    ((gtk_major_version = Major) and
       (gtk_minor_version > Minor) or
      ((gtk_minor_version = Minor) and
         (gtk_micro_version >= Micro) ));
end;

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;

{ FileChooser is available only in GTK >= 2.4.
  We assume that everyone with GTK 2 has also newer GTK 2, >= 2.4.
  So we just always use GtkFileChooser, old implementation using
  GtkFileSelection is removed.  }

var
  HasDefaultFilter: boolean;

  procedure DialogAddFilters(Dialog: PGtkFileChooserDialog;
    FileFilters: TFileFilterList);
  var
    I, J: Integer;
    GtkFilter: PGtkFileFilter;
    Filter: TFileFilter;
  begin
    for I := 0 to FileFilters.Count - 1 do
    begin
      GtkFilter := gtk_file_filter_new();
      Filter := FileFilters[I];

      gtk_file_filter_set_name(GtkFilter, PChar(Filter.Name));
      for J := 0 to Filter.Patterns.Count - 1 do
        gtk_file_filter_add_pattern(GtkFilter, PChar(Filter.Patterns[J]));
      gtk_file_chooser_add_filter(Dialog, GtkFilter);

      if I = FileFilters.DefaultFilter then
      begin
        gtk_file_chooser_set_filter(Dialog, GtkFilter);
        HasDefaultFilter := true;
      end;
    end;
  end;

var
  Action: TGtkFileChooserAction;
  OkButtonText: PChar;
  Dialog: PGtkFileChooserDialog;
  CFileName: PChar;
  ExpandedFileName, Dir: string;
begin
  if OpenDialog then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_OPEN;
    OkButtonText := PChar(GTK_STOCK_OPEN);
  end else
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SAVE;
    OkButtonText := PChar(GTK_STOCK_SAVE);
  end;

  Dialog := PGtkFileChooserDialog(gtk_file_chooser_dialog_new(
    PChar(Title), WindowGtk, Action,
    PChar(GTK_STOCK_CANCEL), [ GTK_RESPONSE_CANCEL,
    OkButtonText, GTK_RESPONSE_ACCEPT,
    nil ]));

  HasDefaultFilter := false;

  if FileFilters <> nil then
    DialogAddFilters(Dialog, FileFilters);

  if not OpenDialog then
  begin
    { The overwrite_confirmation stuff is only in GTK >= 2.8.
      For example fink (stable) has only GTK 2.6 for now,
      so we use them only if they can be loaded from GTK lib. }
    if Assigned(@gtk_file_chooser_set_do_overwrite_confirmation) then
      gtk_file_chooser_set_do_overwrite_confirmation(Dialog, gtrue);
  end;

  { We don't want to simply call gtk_file_chooser_set_filename,
    as it doesn't behave like we want in all cases.

    gtk_file_chooser_set_filename changes to the dir with file.
    If the file exists, _set_filename will select it.
    _set_filename will do nothing if file is not on the list,
    so in this case it's better to _set_current_name. }

  if FileName = '' then
  begin
    { We explicitly allow FileName = '' in the interface of BackendFileDialog,
      it's equal to FileName = GetCurrentDir (with PathDelim at the end) }
    gtk_file_chooser_set_current_folder(Dialog, PChar(GetCurrentDir));
  end else
  begin
    { GTK likes to get full pathnames, otherwise things like

        (bump_mapping:5494): libgnomevfs-CRITICAL **:
        gnome_vfs_get_uri_from_local_path:
        assertion `g_path_is_absolute (local_full_path)' failed

      happen. }

    ExpandedFileName := ExpandFileName(FileName);

    { Test in one go whether file exists, and is a regular file or dir. }
    if FileExists(ExpandedFileName) then
    begin
      if DirectoryExists(ExpandedFileName) then
        gtk_file_chooser_set_current_folder(Dialog, PChar(ExpandedFileName)) else
      begin
        if HasDefaultFilter and not GtkVersionAtLeast(2, 24, 4) then
        begin
          { Unfortunately, the default filter will be ignored after
            gtk_file_chooser_set_filename
            (even if your FileName matches with the default filter).
            See [http://www.mail-archive.com/gtk-list@gnome.org/msg29044.html].
            This makes ugly dialog (with by default all files shown, and no
            filter selected in combo box), so avoid it:
            workaround is to set only directory.

            Note that adding gtk_file_chooser_set_current_name
            with ExtractFileName(ExpandedFileName)
            (this would be a little nicer workaround)
            after setting dir has no effect, no idea why.

            This problem is present in GTK 2.18 (Debian lenny),
            not present in GTK 2.24.4 (Debian wheezy on 2011-07, also Ubuntu 11.4). }
          ExpandedFileName := ExtractFilePath(ExpandedFileName);
          gtk_file_chooser_set_current_folder(Dialog, PChar(ExpandedFileName));
        end else
          gtk_file_chooser_set_filename(Dialog, PChar(ExpandedFileName));
      end;
    end else
    begin
      Dir := ExtractFileDir(ExpandedFileName);
      if Dir = '' then
        Dir := GetCurrentDir;
      gtk_file_chooser_set_current_folder(Dialog, PChar(Dir));

      { GTK 2 makes warnings

          (bezier_curves:32196): Gtk-CRITICAL **:
          gtk_file_chooser_default_set_current_name:
          assertion `impl->action == GTK_FILE_CHOOSER_ACTION_SAVE ||
          impl->action == GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER' failed

        when trying to set non-existing base filename for an "open" dialog.
        That's Ok, I mean non-existing base filename actually doesn't make sense
        for open dialog...
        So we don't do it, simply ignoring ExtractFileName(FileName). }
      if not OpenDialog then
      begin
        gtk_file_chooser_set_current_name(Dialog,
          PChar(ExtractFileName(ExpandedFileName)));
      end;
    end;
  end;

  { The Dialog is set to modal, it's shown,
    buttons cause exit with proper response. }
  Result := GtkDialogRun(GTK_DIALOG(Dialog)) = GTK_RESPONSE_ACCEPT;

  if Result then
  begin
    CFileName := gtk_file_chooser_get_filename(Dialog);
    { This copies string contents, so we can later free CFileName pointer. }
    FileName := CFileName;
    g_free(CFileName);
  end;

  gtk_widget_destroy(GTK_WIDGET(Dialog));
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): Boolean;
var
  Dialog: PGtkColorSelectionDialog;
  GtkColor: array [0..3] of GDouble;
  i: Integer;
begin
  Dialog := PGtkColorSelectionDialog(gtk_color_selection_dialog_new('Choose color'));
  try
    for i := 0 to 3 do GtkColor[i] := Color[i];
    gtk_color_selection_set_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);

    Result := GtkDialogRun(PGtkWindow(Dialog),
      Dialog^.ok_button, Dialog^.cancel_button);

    if Result then
    begin
      gtk_color_selection_get_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);
      { Note: Do not touch Color.InternalData[3], leave it unchanged. Reason:
        While https://developer-old.gnome.org/gtk2/2.24/GtkColorSelection.html#gtk-color-selection-set-color
        says it wants a pointer to 4 components, including alpha, but actually alpha treatment is useless:
        - user cannot edit alpha
        - on return, it seems always 65535.0 (why this, max of Word? it's in Double...)
      }
      for i := 0 to 2 do
        Color.InternalData[i] := GtkColor[i];
    end;
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

{ We would like to map to TGtkMessageType,
  but value GTK_MESSAGE_OTHER is missing from FPC bindings
  and it prevents compilation in FPC >= 2.7.1. }

const
  MessageTypeToGtk: array [TWindowMessageType] of Integer =
  ( Ord(GTK_MESSAGE_INFO),
    Ord(GTK_MESSAGE_WARNING),
    Ord(GTK_MESSAGE_QUESTION),
    Ord(GTK_MESSAGE_ERROR),
    { GTK_MESSAGE_OTHER, missing from FPC bindings } 4 );

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
var
  Dialog: PGtkMessageDialog;
  SNew: string;
begin
  SNew := SUnformattable(S); // gtk_message_dialog_new formats argument using printf, prevent it
  Dialog := PGtkMessageDialog(gtk_message_dialog_new(WindowGtk, 0,
    TGtkMessageType(MessageTypeToGtk[MessageType]), GTK_BUTTONS_OK, PChar(SNew)));
  try
    GtkDialogRun(GTK_DIALOG(Dialog));
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

function TCastleWindow.MessageYesNo(const S: string; const MessageType: TWindowMessageType): boolean;
var
  Dialog: PGtkMessageDialog;
  SNew: string;
begin
  SNew := SUnformattable(S); // gtk_message_dialog_new formats argument using printf, prevent it
  Dialog := PGtkMessageDialog(gtk_message_dialog_new(WindowGtk, 0,
    TGtkMessageType(MessageTypeToGtk[MessageType]), GTK_BUTTONS_YES_NO, PChar(SNew)));
  try
    Result := GtkDialogRun(GTK_DIALOG(Dialog)) = GTK_RESPONSE_YES;
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  if FFullScreenBackend <> FFullScreenWanted then
  begin
    FFullScreenBackend := FFullScreenWanted;
    if not Closed then
    begin
      if FFullScreenBackend then
        gtk_window_fullscreen(WindowGtk)
      else
        gtk_window_unfullscreen(WindowGtk);
    end;
  end;
end;

{ TGTKClipboard ----------------------------------------------------------- }

{ TODO: Shouldn't we convert here UTF8<->system, like in LCL backend?
  GTK always uses UTF8, which may not be our system encoding.

  TODO: gtk_clipboard_wait_for_text runs the main loop.
  We don't really want it (it would require blocking the main UI usually,
  which makes the whole asynchronous fun useless --- if we want to block,
  then why not just wait for the clipboard, instead of running app loop?).

  Hm, although LCL also does here waiting by Application.ProcessMessages
  inside lcl/interfaces/gtk/gtkproc.inc inside WaitForClipboardAnswer.
}

type
  TGTKClipboard = class(TCastleClipboard)
  protected
    function GetAsText: string; override;
    procedure SetAsText(const Value: string); override;
  end;

function TGTKClipboard.GetAsText: string;
var
  C: PGtkClipboard;
begin
  C := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  Result := gtk_clipboard_wait_for_text(C);
end;

procedure TGTKClipboard.SetAsText(const Value: string);
var
  C: PGtkClipboard;
begin
  C := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  gtk_clipboard_set_text(C, PChar(Value), Length(Value));
end;

{ TCastleApplication ------------------------------------------------------------- }

procedure TCastleApplication.ProcessUpdates;
begin
  { Render the invalidated windows.
    And call Update and Timer events of Application and all windows.

    Note: Right now we do not use normal GTK mechanism for redraws
    (that is, gtk_widget_queue_draw for posting and handling signal "expose"
    to call DoRender). In the past, this caused problems, and in particular
    failed totally when in newer GTK/glib expose signal couldn't be
    delivered when we were already inside one expose signal handler.

    Also, we use our own timer mechanism, without depending on GTK timers.
    This makes our code consistent across platforms. }

  UpdateAndRenderEverything;
end;

function TCastleApplication.TryFindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindow;
var
  i: Integer;
begin
  for i := 0 to OpenWindowsCount - 1 do
    if OpenWindows[i].WindowGtk = seekWindowGtk then
      Exit(OpenWindows[i]);
  Result := nil;
end;

function TCastleApplication.TryFindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindow;
var
  i: Integer;
begin
  for i := 0 to OpenWindowsCount-1 do
    if OpenWindows[i].GLAreaGtk = seekGLAreaGtk then
      Exit(OpenWindows[i]);
  Result := nil;
end;

function TCastleApplication.FindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindow;
begin
 Result := TryFindWindowGtk(seekWindowGtk);
 if Result = nil then raise EInternalError.Create(
   'No open window with such WindowGtk found');
end;

function TCastleApplication.FindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindow;
begin
 Result := TryFindGLAreaGtk(seekGLAreaGtk);
 if Result = nil then raise EInternalError.Create(
   'No open window with such GLAreaGtk found');
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;
  inherited Run; // calls DoRun, surrounded by HandleException, in a loop

  { Do not replace this with gtk_main().
    We need to use our ProcessMessage to call our ProcessUpdates. }
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { Basically, call gtk_main_iteration (equivalent to
    gtk_main_iteration_do with parameter blocking = always true),
    only while gtk_events_pending (so we know it will not actually block).

    Moreover, call ProcessUpdates in the middle.

    Summing it up, just call ProcessMessage(true) while gtk_events_pending. }

  Result := not Terminated;

  while Result and (gtk_events_pending() <> 0) do
  begin
    { Since we know that gtk_events_pending() <> 0, the 1st parameter WaitForMessage
      below for ProcessMessage doesn't matter: we know we will never wait for
      message, since we just made sure that we have some message.
      Only 2nd parameter WaitToLimitFPS matters. }
    Result := ProcessMessage(false, false);
  end;

  if Result then
  begin
    { We know now gtk_events_pending() = 0. So call ProcessUpdates now
      (otherwise ProcessAllMessages would never do ProcessUpdates,
      since we call ProcessMessage only when gtk_events_pending() <> 0,
      which then avoids ProcessUpdates.) }
    ProcessUpdates;
    Result := not Terminated;
  end;
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
var
  WasAnyMessage: boolean;
begin
  (* Note that gtk_main_iteration_do result is not useful to us.
     That is, we can't just call here

       Result := not GBoolToPas(gtk_main_iteration_do(WaitForMessage))

     because gtk_main_iteration_do always returns true if the program
     doesn't use gtk_main call at all (i.e. does it's whole loop manually,
     by calling Application.ProcessMessage).

     Confirmed by a look at gtk source code
     (http://git.gnome.org/browse/gtk+/tree/gtk/gtkmain.c?h=gtk-2-18),
     gtk_main_iteration_do ends like this:

           if (main_loops)
             return !g_main_is_running (main_loops->data);
           else
             return TRUE;

     and note that main_loops is set only by gtk_main.
     So a program that doesn't call gtk_main at all has
     main_loops = nil always.

     This means that I should trace the Terminated value myself,
     just like I do for X11 or WinAPI CastleWindow implementations.
     And the result of gtk_main_iteration_do call should be ignored. *)

  if Terminated then Exit(false);

  { Only allow yourself a ProcessUpdates call when no events are pending.
    This is done regardless of WaitForMessage mechanism.

    This follows castlewindow_winsystem.inc approach, and works very good
    to prevent doing Update / Render when we're clogged with events
    (typically happens when walking with mouse look, then we're clogged
    with mouse move events). }
  if gtk_events_pending() = 0 then
  begin
    ProcessUpdates;
    if Terminated then Exit(false);
  end;

  { Once we had below condition "and (not Terminated)",
    but it's pointless, we know Terminated is false now. }
  WaitForMessage := WaitForMessage and Application.AllowSuspendForInput;
  if WaitForMessage then
    MarkSleeping;

  { we could probably not call gtk_main_iteration_do when WasAnyMessage = false,
    and simplify this code a little. But let's keep it this way, to be similar
    to castlewindow_winsystem.inc, that should do the same approach. }
  WasAnyMessage := (gtk_events_pending() <> 0) or WaitForMessage;

  gtk_main_iteration_do(WaitForMessage);
  Result := not Terminated;

  if (not WasAnyMessage) and
     (not Terminated) and
     (not WaitForMessage) and
     WaitToLimitFPS then
    DoLimitFPS;
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
 if gtk_main_level() > 0 then gtk_main_quit();

 { Tracking "Terminated" is needed, gtk_main_quit is not enough --- see
   comments in ProcessMessage implementation.

   Actually, we call gtk_main_quit
   only to exit from gtk_main --- if we would not use gtk_main
   (but e.g. implement Loop simply by calling ProcessMessage,
   which is entirely possible), then calling gtk_main_quit
   would not be needed at all. }

 Terminate; // set Terminated := true
end;

procedure TCastleApplication.CreateBackend;
begin
  inherited Initialize; // set Terminated := false

  RegisterClipboard(TGTKClipboard.Create);
end;

procedure TCastleApplication.DestroyBackend;
var
  C: TMouseCursor;
begin
  { unref cursors allocated in Cursors array }
  for C := Low(C) to High(C) do
    if Cursors[C] <> nil then
    begin
      gdk_cursor_unref(Cursors[C]);
      Cursors[C] := nil;
    end;
end;

{ When CASTLE_WINDOW_USE_XF86VMODE, then castlewindow_xf86vmode.inc
  will provide implementation for ScreenWidth and ScreenHeight. }
{$ifndef CASTLE_WINDOW_USE_XF86VMODE}
function TCastleApplication.ScreenWidth: integer;
var
  Monitor: GInt;
  Rect: TGdkRectangle;
begin
  InitializeXDisplay;
  //Result := gdk_screen_get_width(XScreenGdk);
  // Below is better for dual monitors.
  // TODO: add to docs that Left/Top are relative to global coords, not this monitor
  // TODO: this is actually still bad, different window may be on different monitors with different resolutions
  Monitor := gdk_screen_get_primary_monitor(XScreenGdk);
  gdk_screen_get_monitor_geometry(XScreenGdk, Monitor, @Rect);
  Result := Rect.Width;
end;

function TCastleApplication.ScreenHeight: integer;
var
  Monitor: GInt;
  Rect: TGdkRectangle;
begin
  InitializeXDisplay;
  //Result := gdk_screen_get_height(XScreenGdk);
  // Below is better for dual monitors.
  // TODO: add to docs that Left/Top are relative to global coords, not this monitor
  // TODO: this is actually still bad, different window may be on different monitors with different resolutions
  Monitor := gdk_screen_get_primary_monitor(XScreenGdk);
  gdk_screen_get_monitor_geometry(XScreenGdk, Monitor, @Rect);
  Result := Rect.Height;
end;
{$endif CASTLE_WINDOW_USE_XF86VMODE}

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'GTK+';
end;

{$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
function TCastleApplication.XDisplay: Xlib.PDisplay;
begin
  InitializeXDisplay;
  Result := gdk_x11_display_get_xdisplay(XDisplayGdk);
end;

function TCastleApplication.XScreen: Integer;
begin
  InitializeXDisplay;
  Result := gdk_x11_screen_get_screen_number(XScreenGdk);
end;
{$endif}

procedure TCastleApplication.InitializeXDisplay;

  {$ifdef DARWIN}
  { Replacement for gtk_init on Darwin, to try harder to attach to a running
    X server on macOS. }
  procedure DarwinGtkInit;
  const
    AltXDisplayName = ':0';
  var
    AltDisplay: PGdkDisplay;
  begin
    if not gtk_init_check(@argc, @argv) then
    begin
      AltDisplay := gdk_display_open(PChar(AltXDisplayName));

      if AltDisplay <> nil then
      begin
        Writeln(ErrOutput, 'Opening the default X display failed, retrying with "',
           AltXDisplayName, '" to try to attach to running X server on macOS.');
        gdk_display_manager_set_default_display(gdk_display_manager_get(), AltDisplay);
        gtk_init(@argc, @argv);

        { if success, update XDisplayName to the one actually used }
        XDisplayName := AltXDisplayName;
      end else
      begin
        Writeln(ErrOutput, 'Opening X display failed (tried both the default, and "', AltXDisplayName, '"). Start X server first.');
        Halt(1);
      end;
    end;
  end;
  {$endif}

begin
  if not InitializeXDisplayDone then
  begin
    { TODO: Old comment suggested that GTK initializes X display at gtk_init,
      parsing --display on it's own. Confirm?
      It should work anyway. }

    {$ifdef DARWIN}
    DarwinGtkInit;
    {$else}
    gtk_init(@argc, @argv);
    {$endif}

    if XDisplayName <> '' then
      XDisplayGdk := gdk_display_open(PChar(XDisplayName))
    else
      XDisplayGdk := gdk_display_get_default();

    XScreenGdk := gdk_display_get_default_screen(XDisplayGdk);

    InitializeXDisplayDone := true;
  end;
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { Confirmed using window_events example that it is "true" for GTK
    (press "5", OnMotion is generated). }
  Result := true;
end;

{$endif read_implementation}
