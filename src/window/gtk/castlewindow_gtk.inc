{
  Copyright 2004-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef DARWIN}
  {$linklib gdkglext-x11-1.0}
  {$linklib gtkglext-x11-1.0}
{$endif DARWIN}

{ This contains two very similar CastleWindow backends, both based on GTK.
  One based on GTK 1.x and GtkGLArea,
  the other one based on GTK 2.x and GtkGLExt. }

{$ifdef read_interface_types}
  { @exclude }
  { For now I use GtkDrawingArea when CASTLE_WINDOW_GTK_2.
    But, really, GLAreaGtk could be any gtk widget with CASTLE_WINDOW_GTK_2. }
  PGtkGLArea = PGtkWidget;
{$endif read_interface_types}

{$ifdef read_window_interface}
private
  RedisplayPosted: boolean;

  WindowGtk: PGtkWindow;
  GLAreaGtk: PGtkGLArea;

  { These should be always equivalent to
    gtk_widget_get_gl_config(GLAreaGtk),
    gtk_widget_get_gl_context(GLAreaGtk),
    gtk_widget_get_gl_drawable(GLAreaGtk).
    Or nil when GLAreaGtk = nil. }
  GLConfig: PGdkGLConfig;
  GLContext: PGdkGLContext;
  GLDrawable: PGdkGLDrawable;

  { These things are inited only when (MainMenu <> nil) and (not Closed). }
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
    receive idle etc.)

    @groupBegin }
  function GtkDialogRun(ADialog: PGtkWindow;
    ok_button, cancel_button: PGtkWidget): boolean;
  function GtkDialogRun(ADialog: PGtkDialog): gint;
  { @groupEnd }
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

  { Implementation of MakeGLAreaContainer in this class simply
    returns Result := GLArea.
    When creating gtk window in Open, I'm 1st creating GLArea widget.
    Then I'm inserting MakeGLAreaContainer(GLArea) inside my window.

    This means that you can override MakeGLAreaContainer if you want
    your window to contain something more than just OpenGL drawing area
    (and some menu, derived from MainMenu property).
    You can e.g. create in MakeGLAreaContainer some complex container,
    put there many various GTK widgets, connect your signals to them,
    and then put GLArea inside it.
    This way using this function you can extend window TCastleWindowBase
    with whatever GTK GUI you like.

    The only requirement is that GLArea widget must be placed
    somewhere inside widget that you return in this function.

    Note: if you create here some widgets, remember to show them
    using gtk_widget_show.

    Some notes about using this function:
    Using this function kills some part of portability of CastleWindow unit,
    as this function exists only when CastleWindow is implemented on top
    of GTK, so you will have to always use CastleWindow implemented on top GTK
    on every OS, e.g. with Windows version of your program you will have
    to distribute GTK and GtkGLExt/GLArea for Windows.
    This is not really a problem, since GTK 2.x and GtkGLExt work
    excellent also under Windows (you know, GTK itself is a library
    that provides great portability). However this raises a question:
    what are advantages of using in such situation CastleWindow unit
    (as opposed to just using directly Gtk and GtkGLExt API
    without any need for CastleWindow unit): well, my advice is that
    if your program uses it's OpenGL area as it's central and crucial
    part (and using some GUI only to e.g. get some input from user
    about what to display in OpenGL area), than using CastleWindow unit
    may be more straightforward as you have to write much less code in Gtk.

    Example of using this can be found in examples/window/test_window_gtk_mix }
  function MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget; virtual;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  { used in ProcessUpdates }
  LastDoTimerTime: TMilisecTime;

  QuitPosted: boolean;

  { Find in OpenWindows window with WindowGtk = given seekWindowGtk.
    Return nil if no such window is found. }
  function TryFindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindowBase;
  { Find in OpenWindows window with GLAreaGtk = given seekGLAreaGtk.
    Return nil if no such window is found. }
  function TryFindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindowBase;

  { Same as TryFindWindowGtk, but exception EInternalError when
    no such window is found. }
  function FindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindowBase;
  { Same as TryFindGLAreaGtk, but exception EInternalError when
    no such window is found. }
  function FindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindowBase;

  { This must be constantly called.
    We use it for two tasks currently:
    1. calling idle and times events of Application and all our windows
    2. taking care of redraw }
  procedure ProcessUpdates;
private
  { Used by SetCursor, each item may be nil if not initialized. }
  Cursors: array [TMouseCursor] of PGdkCursor;

  FXDisplayName: string;

  InitializeXDisplayDone: boolean;

  XDisplayGdk: PGdkDisplay;
  XScreenGdk: PGdkScreen;

  { Check XDisplayGdk. Call this once you're sure that XDisplayName is set to
    value desired by used now. See castlewindow_xlib.inc for comments about
    this, it's used here for analogous reasons.

    This initializes XDisplayGdk, XScreenGdk.

    You should call this when you know that XDisplayName is finally set.
    Good choice is before initializing your window.

    TODO: actually, gtk initializes x display at gtk_init time already.
    It looks like --display works only because GTK parses this command-line option too? }
  procedure InitializeXDisplay;

  {$ifdef CASTLE_WINDOW_USE_XF86VMODE}
  function XDisplay: Xlib.PDisplay;
  function XScreen: Integer;
  {$endif}
private
  { These functions are only in GTK >= 2.8. For example fink (stable) has only
    GTK 2.6 for now, so we have to be careful and use them only if they can be
    loaded from GTK lib.

    Normal definitions submitted as http://bugs.freepascal.org/view.php?id=11246 }

  gtk_file_chooser_get_do_overwrite_confirmation: function (
    chooser: PGtkFileChooser): gboolean; cdecl;
  gtk_file_chooser_set_do_overwrite_confirmation: procedure (
    chooser: PGtkFileChooser; do_overwrite_confirmation: gboolean); cdecl;

  { Since GTK 2.6.
    (Although I officially require GTK 2.6, this function is really
    not critical, so I load it dynamically.) }
  gtk_window_set_icon_name: procedure (
    window: PGtkWindow; const name: PGChar); cdecl;
  gtk_window_set_default_icon_name: procedure (
    const name: PGChar); cdecl;

  LibGtk: TDynLib;
public
  { Set XDisplay name, this will be used for all TCastleWindowBase
    that will be subsequently initialized by TCastleWindowBase.Open.

    Note that this is exposed by GTK even for non-XWindows platforms,
    but I don't know what it does there. }
  property XDisplayName: string read FXDisplayName write FXDisplayName;
{$endif read_application_interface}

{$ifdef read_implementation}

{ gboolean type stuff.

  Remember that under FPC >= 2.4.0 Pascal's "true" is $FFFFFFFF,
  not equal to GTK's "true" (equal to int 1). Some GTK routines treat
  $FFFFFFFF as true (since it's <> 0), but some not.
  For example, passing "true" instead of "LongBool(gtrue)"
  to gtk_widget_set_gl_capability (as "direct" parameter)
  causes troubles, gtkglext understood Pascal's "true" as false
  and returned indirect OpenGL contexts.
  See http://lists.freepascal.org/lists/fpc-devel/2009-April/016668.html

  That's why you need to use Glib's constants, not Pascal's false/true.

  Also, GTK's unit constants gfalse/gtrue are ints, while gboolean is LongBool,
  so they are not assigment-compatible. So use our g_false/g_true
  (with underscore) for best results. }
const
  g_true = LongBool(gtrue);
  g_false = LongBool(gfalse);

function PasToGbool(const B: boolean): gboolean;
const
  T: array [boolean] of GBoolean = (g_false, g_true);
begin
  Result := T[B];
end;

function GboolToPas(const B: gboolean): boolean;
begin
  Result := B <> g_false;
end;

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

{ gtk, gdk helpers ------------------------------------------------------------ }

{ Converts GDK keysym (from gdkkeysyms.pp, or gdk/gdkkeysym.h) to
  K_Xxx constant (from Keys unit). Returns K_None if no conversion is possible
  (gdk keysym has no corresponding TKey value). }
function GdkKeysymToKey(GdkKeysym: guint): TKey;
begin
 case GdkKeysym of
  GDK_KEY_BackSpace: Result := K_BackSpace;
  GDK_KEY_Tab: Result := K_Tab;
  GDK_KEY_Return: Result := K_Enter;

  GDK_KEY_Escape: Result := K_Escape;
  GDK_KEY_Space: Result := K_Space;
  GDK_KEY_Page_Up:  Result := K_PageUp;
  GDK_KEY_Page_Down: Result := K_PageDown;
  GDK_KEY_End: Result := K_End;
  GDK_KEY_Home: Result := K_Home;
  GDK_KEY_Left: Result := K_Left;
  GDK_KEY_Up: Result := K_Up;
  GDK_KEY_Right: Result := K_Right;
  GDK_KEY_Down: Result := K_Down;
  GDK_KEY_Insert: Result := K_Insert;
  GDK_KEY_Delete: Result := K_Delete;
  GDK_KEY_BracketLeft: Result := K_LeftBracket;
  GDK_KEY_BracketRight: Result := K_RightBracket;

  GDK_KEY_KP_Add: Result := K_Numpad_Plus;
  GDK_KEY_KP_Subtract: Result := K_Numpad_Minus;

  GDK_KEY_0..GDK_KEY_9: Result := K_0 + GdkKeysym - GDK_KEY_0;

  GDK_KEY_Capital_A..GDK_KEY_Capital_Z: Result := K_A + GdkKeysym - GDK_KEY_Capital_A;
  GDK_KEY_A        ..GDK_KEY_Z        : Result := K_A + GdkKeysym - GDK_KEY_A;

  GDK_KEY_F1..GDK_KEY_F12: Result := K_F1 + GdkKeysym - GDK_KEY_F1;

  GDK_KEY_Comma: Result := K_Comma;
  GDK_KEY_Period: Result := K_Period;
  GDK_KEY_Print: Result := K_PrintScreen;
  GDK_KEY_Caps_Lock: Result := K_CapsLock;
  GDK_KEY_Scroll_Lock: Result := K_ScrollLock;
  GDK_KEY_Num_Lock: Result := K_NumLock;
  GDK_KEY_Pause: Result := K_Pause;

  GDK_KEY_KP_0: Result := K_Numpad_0;
  GDK_KEY_KP_1: Result := K_Numpad_1;
  GDK_KEY_KP_2: Result := K_Numpad_2;
  GDK_KEY_KP_3: Result := K_Numpad_3;
  GDK_KEY_KP_4: Result := K_Numpad_4;
  GDK_KEY_KP_5: Result := K_Numpad_5;
  GDK_KEY_KP_6: Result := K_Numpad_6;
  GDK_KEY_KP_7: Result := K_Numpad_7;
  GDK_KEY_KP_8: Result := K_Numpad_8;
  GDK_KEY_KP_9: Result := K_Numpad_9;
  GDK_KEY_KP_End: Result := K_Numpad_End;
  GDK_KEY_KP_Down: Result := K_Numpad_Down;
  GDK_KEY_KP_Next: Result := K_Numpad_PageDown;
  GDK_KEY_KP_Left: Result := K_Numpad_Left;
  GDK_KEY_KP_Begin: Result := K_Numpad_Begin;
  GDK_KEY_KP_Right: Result := K_Numpad_Right;
  GDK_KEY_KP_Home: Result := K_Numpad_Home;
  GDK_KEY_KP_Up: Result := K_Numpad_Up;
  GDK_KEY_KP_Prior: Result := K_Numpad_PageUp;
  GDK_KEY_KP_Insert: Result := K_Numpad_Insert;
  GDK_KEY_KP_Delete: Result := K_Numpad_Delete;
  GDK_KEY_KP_Enter: Result := K_Numpad_Enter;
  GDK_KEY_KP_Multiply: Result := K_Numpad_Multiply;
  GDK_KEY_KP_Divide: Result := K_Numpad_Divide;
  GDK_KEY_Apostrophe: Result := K_Apostrophe;
  GDK_KEY_Semicolon: Result := K_Semicolon;
  GDK_KEY_Slash: Result := K_Slash;
  GDK_KEY_Grave: Result := K_BackQuote;
  GDK_KEY_Minus: Result := K_Minus;
  GDK_KEY_Plus: Result := K_Plus;
  GDK_KEY_Equal: Result := K_Equal;
  GDK_KEY_BackSlash: Result := K_BackSlash;

  else Result := K_None;
 end;
end;

{ Converts GDK mouse button number (from GdkEventButton.button)
  to TMouseButton. Returns false (and does not change mb) if such
  button number cannot be represented as TMouseButton. }
function ButtonNumberToMouseButton(bn: guint; out mb: TMouseButton): boolean;
begin
 { No need to implement this using dirty typecasts, it's simple enough
   to do it type-safe. }
 case bn of
  1: mb := mbLeft;
  2: mb := mbMiddle;
  3: mb := mbRight;
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
}

{$define SIGNAL_GLAREA_BEGIN:=
begin
 with Application.FindGLAreaGtk(AGLAreaGtk) do begin}
{$define SIGNAL_GLAREA_END:=
 end; end}

{$define SIGNAL_WINDOW_BEGIN:=
begin
 with Application.FindWindowGtk(AWindowGtk) do begin}
{$define SIGNAL_WINDOW_END:=
 end; end}

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
    we're constantly doing PostRedisplay) it is not so safe to do this
    because it is possible then that EVERY event will have Event^.Count > 0.
    I.e. we may not process expose events fast enough, and there always
    will be some expose event in queue when another expose event arrives,
    so always Event^.Count > 0.

    Actually, GDK does some exposure events compression itself.
    I don't know what exactly this means. But I guess that
    stupid PostRedisplay calls will be ignored, so new expose events will be
    added to queue only when no other expose events exist there.
    So every event will have Event^.Count = 0. But this STILL means that
    checking here is (Event^.Count > 0) is useless !

    Test below confirms that:
      if Event.Count > 0 then
       Writeln('Have Expose event with Event.Count = ', Event.Count);
  }

  PostRedisplay;

  Result := g_true;
SIGNAL_GLAREA_END;

function signal_window_configure_event(AWindowGtk: PGtkWindow;
  event: PGdkEventConfigure; data: gpointer): gboolean; cdecl;
{ This signal is registered for WindowGtk, not GLAreaGtk.
  That's because we want to update here Left/Top.
  And position of GLAreaGtk is constant, only WindowGtk position changes. }
var LeftGtk, TopGtk: gint;
SIGNAL_WINDOW_BEGIN
 { I could ask here for GLAreaGtk global position.
   But as for now Left and Top are coordinates of whole WindowGtk
   (with WindowManager decorations, that's how
   gdk_window_get_root_origin works).
   Reasons -- the same as in castlewindow_xlib.inc. }
 gdk_window_get_root_origin(GTK_WIDGET(WindowGtk)^.window, @LeftGtk, @TopGtk);
 Left := LeftGtk;
 Top := TopGtk;
 { for the sake of gtk, I can't tell that I "handled" this in any way }
 Result := g_false;
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
 Result := g_false;
SIGNAL_GLAREA_END;

function signal_delete_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
SIGNAL_WINDOW_BEGIN
 DoCloseQuery;
 Result := g_true;
SIGNAL_WINDOW_END;

function signal_key_press_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
var Key: TKey;
    CharKey: char;
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

   {$ifdef MSWINDOWS}
   { It seems that GTK 1.3 for Windows cannot translate GDK_KEY_Escape and
     GDK_KEY_Return to standard chars (#13 and #27). So I'm fixing it here. }
   if Key = K_Escape then
    CharKey := CharEscape else
   if Key = K_Enter then
    CharKey := CharEnter else
   {$endif}
   { It seems that GTK 2 doesn't translate backspace and tab to
     appropriate chars. So I'm fixing it here. }
   if Key = K_Tab then
    CharKey := CharTab else
   if Key = K_BackSpace then
    CharKey := CharBackSpace else
   { note: testing is Event.Length = 1 not only makes sure that we can
     safely take Event.TheString[0] as a whole CharKey,
     but it also makes sure that this is not something MBCS encoded. }
   if Event^.Length = 1 then
    CharKey := Event^._string[0] else
    CharKey := #0;

   if (Key <> K_None) or (CharKey <> #0) then DoKeyDown(Key, CharKey);
  end;
 end;

 { It would be nice to say here Result := true if key was handled
   in DoKeyDown. But I don't have any indicator whether key was handled
   in EventKeyDown or OnKeyDown (because such thing would complicate
   implementation of OnKeyDown callback in every program using
   CastleWindow). So I must assume that either (1) always or (2) never
   when I called DoKeyDown key is handled.
   But (1) is actually not acceptable, because it disallows user
   to activate menu using F10 key. So I must set Result := false here.

   I'm also setting Result := false in signal_key_release_event,
   to be consequent.
 }
 Result := g_false;
SIGNAL_WINDOW_END;

function signal_key_release_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
var Key: TKey;
SIGNAL_WINDOW_BEGIN
 case Event^.KeyVal of
  GDK_KEY_Shift_L:   SetPrivateModifiersDown(mkShift, false, false);
  GDK_KEY_Shift_R:   SetPrivateModifiersDown(mkShift, true,  false);
  GDK_KEY_Control_L: SetPrivateModifiersDown(mkCtrl,  false, false);
  GDK_KEY_Control_R: SetPrivateModifiersDown(mkCtrl,  true,  false);
  GDK_KEY_Alt_L:     SetPrivateModifiersDown(mkAlt,   false, false);
  GDK_KEY_Alt_R:     SetPrivateModifiersDown(mkAlt,   true,  false);
  else begin
   Key := GdkKeysymToKey(Event^.KeyVal);
   if Key <> K_None then DoKeyUp(Key);
  end;
 end;

 { Why false, not true ? See comments at the end of
   signal_key_press_event. }
 Result := g_false;
SIGNAL_WINDOW_END;

function signal_button_press_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TMouseButton;
SIGNAL_GLAREA_BEGIN
  Result := PasToGbool((Event^._type = GDK_BUTTON_PRESS) and
    ButtonNumberToMouseButton(Event^.button, mb));
  if GboolToPas(Result) then
    DoMouseDown(Round(Event^.x), Round(Event^.y), mb);
SIGNAL_GLAREA_END;

function signal_button_release_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TMouseButton;
SIGNAL_GLAREA_BEGIN
  Result := PasToGbool((Event^._type = GDK_BUTTON_RELEASE) and
    ButtonNumberToMouseButton(Event^.button, mb));
  if GboolToPas(Result) then
    DoMouseUp(Round(Event^.x), Round(Event^.y), mb);
SIGNAL_GLAREA_END;

function signal_motion_notify_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventMotion;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
  DoMouseMove(Round(Event^.x), Round(Event^.y));
  Result := g_true;
SIGNAL_GLAREA_END;

function signal_scroll_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventScroll;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
  case Event^.Direction of
    GDK_SCROLL_UP:   DoMouseWheel( 1, true);
    GDK_SCROLL_DOWN: DoMouseWheel(-1, true);
    GDK_SCROLL_LEFT: DoMouseWheel( 1, false);
    GDK_SCROLL_RIGHT:DoMouseWheel(-1, false);
    else if Log then WritelnLog('GTK', 'Invalid GdkEventScroll.direction');
  end;
  Result := g_true;
SIGNAL_GLAREA_END;

procedure signal_menu_item_activate(AMenuItemGtk: PGtkMenuItem;
  data: gpointer); cdecl;
var MenuItem: TMenuItem;
    Window: TCastleWindowBase;
begin
 MenuItem := MenuItemFromSmallId( PtrInt(Data) );
 Window := TCastleWindowBase( gtk_object_get_user_data(GTK_OBJECT(AMenuItemGtk)) );

 if MenuItem is TMenuItemChecked then
 begin
  { GTK checked menu items do always something like our AutoCheckedToggle.
    This assert confirms that: }

  if not (MenuItem is TMenuItemRadio) then
    Assert( gtk_check_menu_item_get_active(PGtkCheckMenuItem(AMenuItemGtk)) <>
            TMenuItemChecked(MenuItem).Checked);

  { I don't want this (because, first of all, not always my
    TMenuItemChecked will have AutoCheckedToggle = true.
    Second, when not MenuActive, I do not want such behaviour, even for
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
    because we would call Window.DoMenuCommand(MenuItem) TWICE.

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

 Window.DoMenuCommand(MenuItem);
end;

function signal_window_focus_out_event(AWindowGtk: PGtkWindow;
  Event: PGdkEventfocus; data: gpointer): gboolean; cdecl;
{ This is called when user switches to another window. }
SIGNAL_WINDOW_BEGIN
  ReleaseAllKeysAndMouse;
  { for the sake of gtk, I can't tell that I "handled" this in any way }
  Result := g_false;
SIGNAL_WINDOW_END;

{ TCastleWindowBase ------------------------------------------------------------ }

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      UpdateCursor;
  end;
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  FCustomCursor := Value;
  { TODO }
end;

procedure TCastleWindowBase.UpdateCursor;

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
      Result := gdk_cursor_new_from_pixmap(
        Pixmap, Pixmap, @Color, @Color, 0, 0);
    finally gdk_pixmap_unref(Pixmap); end;
  end;

const
  GdkCursorTypeFromMy: array [mcStandard .. High(TMouseCursor)] of
    TGdkCursorType =
  ( { I think that GDK_ARROW was traditionally standard under XWindows
      (right-top arrow), but then gtk changed standard to much more standard
      left-top arrow in GDK_LEFT_PTR.

      See for example
      http://library.gnome.org/devel/gdk/stable/gdk-Cursors.html
      for the list.
    }
    GDK_LEFT_PTR, GDK_WATCH, GDK_XTERM, GDK_HAND2 );
begin
  if Application.Cursors[Cursor] = nil then
  begin
    { initialize Application.Cursors[Cursor] }
    case Cursor of
      mcDefault:
        { don't do anything, Application.Cursors[mcDefault] should be nil,
          gdk_window_set_cursor will understand nil as "use parent"
          and that's exactly what we want for mcDefault. };
      mcNone: Application.Cursors[Cursor] := CreateNoneCursor;
      mcCustom: { TODO: temporary, behave like mcDefault };
      else
        Application.Cursors[Cursor] := gdk_cursor_new_for_display(
          Application.XDisplayGdk, GdkCursorTypeFromMy[Cursor]);
    end;
  end;

  gdk_window_set_cursor(GTK_WIDGET(GLAreaGtk)^.Window, Application.Cursors[Cursor]);
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  {$ifdef CASTLE_WINDOW_GTK_WITH_XLIB}
  if not Closed then
    XWarpPointer(
      gdk_x11_drawable_get_xdisplay(GTK_WIDGET(GLAreaGtk)^.Window),
      X.None,
      gdk_x11_drawable_get_xid(GTK_WIDGET(GLAreaGtk)^.Window),
      0, 0, 0, 0, NewMouseX, NewMouseY);
  {$else}
  raise Exception.Create('TCastleWindowBase.SetMousePosition not implemented with GTK 1');
  {$endif}
end;

procedure TCastleWindowBase.SwapBuffers;
begin
 gdk_gl_drawable_swap_buffers(GLDrawable);
end;

procedure TCastleWindowBase.PostRedisplay;
begin
  { We track RedisplayPosted ourselves, without GTK gtk_widget_queue_draw.
    This makes things simpler. }
  RedisplayPosted := true;
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  if RedisplayPosted then
  begin
    RedisplayPosted := false;
    DoDraw;
  end;

  { Without our own RedisplayPosted tracking:

    - under GTK 2 this would be easy:
      gdk_window_process_updates(GTK_WIDGET(WindowGtk)^.Window, g_true);

    - but under GTK 1 this isn't possible AFAIK, I would have to just
      always call DoDraw here (which is valid but non-optimal behavior
      of FlushRedisplay)

  }
end;

procedure TCastleWindowBase.SetCaption(const Value: string);
begin
 FCaption := Value;
 if not Closed then
  gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(Caption));
end;

procedure TCastleWindowBase.BackendMakeCurrent;
begin
 Assert(not Closed);

 { When GLAreaGtk is implemented by gdkglwindow-x11 or
   gdkglpixmap-x11 or gdkglwindow-win32:
   gdk_gl_drawable_gl_begin simply calls
   gdk_gl_window_impl_x11_make_context_current
   and gdk_gl_drawable_gl_end doesn't do anything.
   So I can simply forget about gdk_gl_drawable_gl
   and call gdk_gl_drawable_gl_begin on BackendMakeCurrent.

   In case GLAreaGtk is implemented by gdkglpixmap-win32
   (this is not possible to achieve now in CastleWindow, but maybe
   in the future will be) or just in case it will be implemented
   on some system differently, I will have to only slghtly
   improve this procedure: I will keep track of currently current
   GLAreaGtk, and I will do something like
     gdk_gl_drawable_gl_end(currently active);
     currently active := newly active;
     gdk_gl_drawable_gl_begin(currently active);
 }

 gdk_gl_drawable_gl_begin(GLDrawable, GLContext);
end;

function TCastleWindowBase.MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget;
begin
 Result := GTK_WIDGET(GLArea);
end;

procedure TCastleWindowBase.OpenBackend;

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
  var WindowWidth, WindowHeight: Integer;
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
   if MainMenu <> nil then
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

var
  VisualAttr: TLongIntList;
  GLAreaContainer: PGtkWidget;
begin
 RedisplayPosted := false; { reset to default value }

 Application.InitializeXDisplay;

 WindowGtk := PGtkWindow(gtk_window_new(GTK_WINDOW_TOPLEVEL));

 if (GtkIconName <> '') and
    Assigned(Application.gtk_window_set_icon_name) then
   Application.gtk_window_set_icon_name(WindowGtk, PCharOrNil(GtkIconName));

 gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(Caption));
 gtk_container_set_border_width(GTK_CONTAINER(WindowGtk), BorderWidth);

 { Create VisualAttr and then create appropriate GLAreaGtk
   (and GLConfig in case of GTK 2) honouring this VisualAttr.

   The code for initializing VisualAttr is quite the same
   as the code from castlewindow_xlib.inc because the meaning of GDK_GL_
   constants is the same as GLX_ constants. }
 VisualAttr := TLongIntList.Create;
 try
  if DoubleBuffer then
    VisualAttr.Add(GDK_GL_DOUBLEBUFFER);
  VisualAttr.AddArray([
    GDK_GL_RGBA,
    { Note that sizes of buffers in VisualAttr must be given in bits
      (not in e.g. bytes), so this is exactly what I have in my variables. }
    GDK_GL_DEPTH_SIZE, DepthBufferBits,

    { This is a workaround for gtkglext Windows bug,
      one must specify GDK_GL_BUFFER_SIZE or GTK_GL_RED/GREEN/BLUE_SIZE otherwise
      gtkglext will request 32-bits sized color buffer (instead of just accepting
      any bit depth), that may not be available.

      I know how to fix it in gtkglext, I submitted this to gtkglext list
      (mail "Bug at creation of GdkGLConfig on Win32" from 2005-03-01),
      unfortunately with no response to this day. }
    GDK_GL_BUFFER_SIZE, 1,
    GDK_GL_STENCIL_SIZE, StencilBufferBits,
    GDK_GL_ALPHA_SIZE, AlphaBits,
    GDK_GL_ACCUM_RED_SIZE, AccumBufferBits[0],
    GDK_GL_ACCUM_GREEN_SIZE, AccumBufferBits[1],
    GDK_GL_ACCUM_BLUE_SIZE, AccumBufferBits[2],
    GDK_GL_ACCUM_ALPHA_SIZE, AccumBufferBits[3] ]);

  if MultiSampling > 1 then
    { I guess that gtkglext should internally check for
      glXQueryExtensionsString and GLX_ARB_multisample availability. }
    VisualAttr.AddArray([
      GDK_GL_SAMPLE_BUFFERS, 1,
      GDK_GL_SAMPLES, MultiSampling ]);

  { end of VisualAttr list }
  VisualAttr.Add(GDK_GL_ATTRIB_LIST_NONE);

  GLConfig := gdk_gl_config_new(PInteger(VisualAttr.List));
  { Looking at gears demo of GtkGLExt, one should check GLConfig = nil
    to know whether such gl configuration was possible. }
  if GLConfig = nil then
   ContextNotPossible;

  GLAreaGtk := gtk_drawing_area_new;
  if not GboolToPas(gtk_widget_set_gl_capability(GLAreaGtk, GLConfig, nil,
    g_true, GDK_GL_RGBA_TYPE)) then
   ContextNotPossible;
 finally FreeAndNil(VisualAttr) end;

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

 { Add us to OpenWindows windows to ensure that all callbacks that are
   eventually called by procedures below will be able to use
   Application.FindGLAreaGtk and Application.FindWindowGtk. }
 Application.OpenWindowsAdd(Self);

 { setup window position from Left, Top }
 gtk_widget_set_uposition(GTK_WIDGET(WindowGtk), Left, Top);

 GLAreaContainer := MakeGLAreaContainer(GLAreaGtk);

 if MainMenu <> nil then
 begin
  window_accel_group := gtk_accel_group_new;
  gtk_window_add_accel_group(GTK_WINDOW(WindowGtk), window_accel_group);

  window_vbox := PGtkVBox(gtk_vbox_new(g_false, VBoxSpacing));
  gtk_widget_show(GTK_WIDGET(window_vbox));
  MenuInitialize;
  gtk_box_pack_end_defaults(PGtkBox(window_vbox), GLAreaContainer);
  gtk_container_add(GTK_CONTAINER(WindowGtk), GTK_WIDGET(window_vbox));
 end else
  gtk_container_add(GTK_CONTAINER(WindowGtk), GLAreaContainer);

 { setup window/glarea size and resize policy from
   MinWidth, MinHeight, Width, Height, ResizeAllowed }
 if FullScreen then
 begin

  { When I'm in FullScreen I'm setting whole window size to screen size,
    not only GLArea size to Width/Height. This is the ultimate sense of FullScreen,
    so there is no discussion, no matter what value has ResizeAllowed.
    So I'm doing gtk_widget_set_usize(WindowGtk, ...) and
                 gtk_window_set_default_size
    instead of
                 gtk_widget_set_usize(GLAreaGtk, ...) and
                 set_default_glarea_size

    If FullScreen, Width/Height here should be equal to
    Application.ScreenWidth/Height. But maybe later I will change this
    in TCastleWindowBase.Open implementation, so I do not want to depend on this. }

  gtk_widget_set_usize(GTK_WIDGET(WindowGtk), MinWidth, MinHeight);
  gtk_window_set_default_size(WindowGtk, Application.ScreenWidth, Application.ScreenHeight);

  gtk_window_fullscreen(WindowGtk);
 end else
 begin
  if ResizeAllowed <> raAllowed then
   gtk_widget_set_usize(GTK_WIDGET(GLAreaGtk), Width, Height) else
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

 { From this point I require that WindowGtk is realized (has associated
   GdkWindow, i.e. I can use GTK_WIDGET(WindowGtk).Window).
   It also means that I can do MakeCurrent (and call OpenGL commands)
   from now. In particular, we have valid gl context when we leave OpenBackend. }
 if (GTK_REALIZED and GTK_WIDGET_FLAGS( GTK_WIDGET(WindowGtk) )) = 0 then
  raise EInternalError.Create('CastleWindow.OpenBackend: GdkWindow not realized yet');

 { These must be inited only when I'm sure that window is realized.
   I checked this even in gtkglext sources:
   yes, gtk_widget_get_gl_drawable and gtk_widget_get_gl_context check
   and always return nil if their widget is not realized. }
 GLDrawable := gtk_widget_get_gl_drawable(GLAreaGtk);
 GLContext := gtk_widget_get_gl_context(GLAreaGtk);

 UpdateCursor;
end;

procedure TCastleWindowBase.CloseBackend;
begin
 { Do not unref here GLAreaGtk.
   GLAreaGtk is placed inside WindowGtk when we do
   gtk_container_add(WindowGtk, GLAreaGtk). During gtk_container_add
   WindowGtk calls sink on GLAreaGtk, so GLAreaGtk has reference 1 and it
   means that it is referenced from WindowGtk. So WindowGtk is responsible
   for calling unref on GLAreaGtk. }
 GLAreaGtk := nil;

 if MainMenu <> nil then MenuFinalize;

 window_vbox := nil;

 GLDrawable := nil; { TODO -- unref it ? }
 GLConfig := nil; { TODO -- unref it ? }

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

procedure TCastleWindowBase.CreateBackend;
begin
end;

{ TCastleWindowBase.*Dialog --------------------------------------------------- }

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
    in FileDialog/ColorDialog. }
  Result := g_true;
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

function TCastleWindowBase.GtkDialogRun(ADialog: PGtkWindow;
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

  gtk_window_set_modal(ADialog, g_true);
  gtk_window_set_transient_for(ADialog, WindowGtk);

  { prepare Data }
  Data.Answered := false;

  { enter glwin mode }
  { Once I used here @HalfToneStipple to show user that glwin window is
    temporary disabled. But then I added lines with "gtk_window_set_modal"
    and "gtk_window_set_transient_for" above and it's no longer necessary.
    So I don't use it. }
  Mode := TGLModeFrozenScreen.Create(Self, 0, false, nil);
  try
    gtk_widget_show(GTK_WIDGET(ADialog));
    while not Data.Answered do Application.ProcessMessage(true, true);
  finally Mode.Free end;

  Result := Data.Answer = GTK_RESPONSE_ACCEPT;
end;

function TCastleWindowBase.GtkDialogRun(ADialog: PGtkDialog): gint;
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

  gtk_window_set_modal(GTK_WINDOW(ADialog), g_true);

  Data.Answered := false;

  Mode := TGLModeFrozenScreen.Create(Self, 0, false, nil);
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

function TCastleWindowBase.FileDialog(const Title: string; var FileName: string;
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
  SearchResult: Integer;
  SearchRecord: TSearchRec;
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
    if Assigned(Application.gtk_file_chooser_set_do_overwrite_confirmation) then
      Application.gtk_file_chooser_set_do_overwrite_confirmation(Dialog, g_true);
  end;

  { We don't want to simply call gtk_file_chooser_set_filename,
    as it doesn't behave like we want in all cases.

    gtk_file_chooser_set_filename changes to the dir with file.
    If the file exists, _set_filename will select it.
    _set_filename will do nothing if file is not on the list,
    so in this case it's better to _set_current_name. }

  if FileName = '' then
  begin
    { We explicitly allow FileName = '' in the interface of FileDialog,
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
    SearchResult := FindFirst(ExpandedFileName, faReallyAnyFile, SearchRecord);

    if SearchResult = 0 then
    begin
      if (faDirectory and SearchRecord.Attr) <> 0 then
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
      FindClose(SearchRecord);
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

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
var
  Dialog: PGtkColorSelectionDialog;
  GtkColor: array [0..3] of GDouble;
  i: Integer;
begin
  Dialog := PGtkColorSelectionDialog(gtk_color_selection_dialog_new('Choose color'));
  try
    for i := 0 to 2 do GtkColor[i] := Color[i];
    gtk_color_selection_set_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);

    Result := GtkDialogRun(PGtkWindow(Dialog),
      Dialog^.ok_button, Dialog^.cancel_button);

    if Result then
    begin
      gtk_color_selection_get_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);
      for i := 0 to 2 do Color[i] := GtkColor[i];
    end;
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

const
  MessageTypeToGtk: array [TWindowMessageType] of TGtkMessageType =
  ( GTK_MESSAGE_INFO,
    GTK_MESSAGE_WARNING,
    GTK_MESSAGE_QUESTION,
    GTK_MESSAGE_ERROR,
    { GTK_MESSAGE_OTHER, missing from FPC bindings } TGtkMessageType(4) );

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
var
  Dialog: PGtkMessageDialog;
begin
  Dialog := PGtkMessageDialog(gtk_message_dialog_new(
    WindowGtk, 0, MessageTypeToGtk[MessageType], GTK_BUTTONS_OK, PChar(S)));
  try
    GtkDialogRun(GTK_DIALOG(Dialog));
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

function TCastleWindowBase.MessageYesNo(const S: string; const MessageType: TWindowMessageType): boolean;
var
  Dialog: PGtkMessageDialog;
begin
  Dialog := PGtkMessageDialog(gtk_message_dialog_new(
    WindowGtk, 0, MessageTypeToGtk[MessageType], GTK_BUTTONS_YES_NO, PChar(S)));
  try
    Result := GtkDialogRun(GTK_DIALOG(Dialog)) = GTK_RESPONSE_YES;
  finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

{ TGLApplication ------------------------------------------------------------- }

procedure TGLApplication.ProcessUpdates;

  { If some window has RedisplayPosted = @true, redraw it.

    Note: Right now we do not use normal GTK mechanism for redraws
    (that is, gtk_widget_queue_draw for posting and handling signal "expose"
    to call DoDraw). In the past, this caused problems, and in particular
    failed totally when in newer GTK/glib expose signal couldn't be
    delivered when we were already inside one expose signal handler. }
  procedure WindowsFlushRedisplay;
  var
    I: Integer;
    Window: TCastleWindowBase;
  begin
    I := 0;
    while I < OpenWindowsCount do
    begin
      Window := OpenWindows[I];
      Window.FlushRedisplay;
      if Window.Closed then Continue {don't Inc(I)};
      Inc(I);
    end;
  end;

  { Call idle and timer events of Application and all windows.

    Idle events are handled by DoSelfIdle and OpenWindows.DoIdle.

    Timer events are handled by MaybeDoTimer, that in turn will call
    (when needed) DoSelfTimer and OpenWindows.DoTimer. }
  procedure WindowsIdleAndTimer;
  begin
    DoSelfIdle;
    FOpenWindows.DoIdle;

    { For simplicity, we're implementing here our own timer mechanism,
      without using gtk timers. }
    MaybeDoTimer(LastDoTimerTime);
  end;

begin
  WindowsFlushRedisplay;
  WindowsIdleAndTimer;
end;

function TGLApplication.TryFindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindowBase;
var
  i: Integer;
begin
  for i := 0 to OpenWindowsCount - 1 do
    if OpenWindows[i].WindowGtk = seekWindowGtk then
      Exit(OpenWindows[i]);
  Result := nil;
end;

function TGLApplication.TryFindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindowBase;
var
  i: Integer;
begin
  for i := 0 to OpenWindowsCount-1 do
    if OpenWindows[i].GLAreaGtk = seekGLAreaGtk then
      Exit(OpenWindows[i]);
  Result := nil;
end;

function TGLApplication.FindWindowGtk(seekWindowGtk: PGtkWindow): TCastleWindowBase;
begin
 Result := TryFindWindowGtk(seekWindowGtk);
 if Result = nil then raise EInternalError.Create(
   'No open window with such WindowGtk found');
end;

function TGLApplication.FindGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TCastleWindowBase;
begin
 Result := TryFindGLAreaGtk(seekGLAreaGtk);
 if Result = nil then raise EInternalError.Create(
   'No open window with such GLAreaGtk found');
end;

procedure TGLApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  while ProcessMessage(true, true) do ;

  { Do not replace this with gtk_main().
    We need to use our ProcessMessage to call our ProcessUpdates. }
end;

function TGLApplication.ProcessAllMessages: boolean;
begin
  { Basically, call gtk_main_iteration (equivalent to
    gtk_main_iteration_do with parameter blocking = always true),
    only while gtk_events_pending (so we know it will not actually block).

    Moreover, call ProcessUpdates in the middle.

    Summing it up, just call ProcessMessage(true) while gtk_events_pending. }

  Result := not QuitPosted;

  while Result and (gtk_events_pending() <> 0) do
  begin
    Result := ProcessMessage(true, true);
  end;

  if Result then
  begin
    { We know now gtk_events_pending() = 0. So call ProcessUpdates now
      (otherwise ProcessAllMessages would never do ProcessUpdates,
      since we call ProcessMessage only when gtk_events_pending() <> 0,
      which then avoids ProcessUpdates.) }
    ProcessUpdates;
    Result := not QuitPosted;
  end;
end;

function TGLApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;

  function SomeRedisplayPosted: boolean;
  var
    I: Integer;
  begin
    for I := 0 to OpenWindowsCount - 1 do
      if OpenWindows[I].RedisplayPosted then Exit(true);

    Result := false;
  end;

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

     This means that I should trace the QuitPosted value myself,
     just like I do for X11 or WinAPI CastleWindow implementations.
     And the result of gtk_main_iteration_do call should be ignored. *)

  if QuitPosted then Exit(false);

  { Only allow yourself a ProcessUpdates call when no events are pending.
    This is done regardless of WaitForMessage mechanism.

    This follows castlewindow_winsystem.inc approach, and works very good
    to prevent doing Idle / Draw when we're clogged with events
    (typically happens when walking with mouse look, then we're clogged
    with mouse move events). }
  if gtk_events_pending() = 0 then
  begin
    ProcessUpdates;
    if QuitPosted then Exit(false);
  end;

  { Once we had below condition "and (not QuitPosted)",
    but it's pointless, we know QuitPosted is false now. }
  WaitForMessage := WaitForMessage and Application.AllowSuspendForInput and
    (not SomeRedisplayPosted);

  { we could probably not call gtk_main_iteration_do when WasAnyMessage = false,
    and simplify this code a little. But let's keep it this way, to be similar
    to castlewindow_winsystem.inc, that should do the same approach. }
  WasAnyMessage := (gtk_events_pending() <> 0) or WaitForMessage;

  gtk_main_iteration_do(WaitForMessage);
  Result := not QuitPosted;

  if (not WasAnyMessage) and
     (not QuitPosted) and
     (not WaitForMessage) and
     WaitToLimitFPS then
    DoLimitFPS;
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
 if gtk_main_level() > 0 then gtk_main_quit();

 { QuitPosted is needed, gtk_main_quit is not enough --- see
   comments in ProcessMessage implementation.

   Actually, we call gtk_main_quit
   only to exit from gtk_main --- if we would not use gtk_main
   (but e.g. implement Loop simply by calling ProcessMessage,
   which is entirely possible), then calling gtk_main_quit
   would not be needed at all. }

 QuitPosted := true;
end;

procedure TGLApplication.CreateBackend;
{$ifdef DARWIN}
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
         AltXDisplayName, '" to try to attach to running X server on Mac OS X.');
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
  {$else}
begin
  gtk_init(@argc, @argv);
  {$endif}

  gtk_gl_init(@argc, @argv);

  QuitPosted := false;

  { These are available only in GTK 2.8.
    We want to cooperate even with GTK 2.6 (in Mac OS X fink stable),
    so we have to load them dynamically. }
  gtk_file_chooser_get_do_overwrite_confirmation := nil;
  gtk_file_chooser_set_do_overwrite_confirmation := nil;
  gtk_window_set_icon_name := nil;
  gtk_window_set_default_icon_name := nil;

  LibGtk := TDynLib.Load(gtklib, false);
  if LibGtk <> nil then
  begin
    LibGtk.SymbolErrorBehaviour := seReturnNil;
    Pointer(gtk_file_chooser_get_do_overwrite_confirmation) := LibGtk.Symbol('gtk_file_chooser_get_do_overwrite_confirmation');
    Pointer(gtk_file_chooser_set_do_overwrite_confirmation) := LibGtk.Symbol('gtk_file_chooser_set_do_overwrite_confirmation');
    Pointer(gtk_window_set_icon_name) := LibGtk.Symbol('gtk_window_set_icon_name');
    Pointer(gtk_window_set_default_icon_name) := LibGtk.Symbol('gtk_window_set_default_icon_name');
  end;
end;

procedure TGLApplication.DestroyBackend;
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

  FreeAndNil(LibGtk);
  gtk_file_chooser_get_do_overwrite_confirmation := nil;
  gtk_file_chooser_set_do_overwrite_confirmation := nil;
  gtk_window_set_icon_name := nil;
  gtk_window_set_default_icon_name := nil;
end;

{ When CASTLE_WINDOW_USE_XF86VMODE, then castlewindow_xf86vmode.inc
  will provide implementation for ScreenWidth and ScreenHeight. }
{$ifndef CASTLE_WINDOW_USE_XF86VMODE}
function TGLApplication.ScreenWidth: integer;
begin
  InitializeXDisplay;
  Result := gdk_screen_get_width(XScreenGdk);
end;

function TGLApplication.ScreenHeight: integer;
begin
  InitializeXDisplay;
  Result := gdk_screen_get_height(XScreenGdk);
end;
{$endif CASTLE_WINDOW_USE_XF86VMODE}

function TGLApplication.BackendName: string;
begin
  Result := 'GTK+';
end;

{$ifdef CASTLE_WINDOW_USE_XF86VMODE}
function TGLApplication.XDisplay: Xlib.PDisplay;
begin
  InitializeXDisplay;
  Result := gdk_x11_display_get_xdisplay(XDisplayGdk);
end;

function TGLApplication.XScreen: Integer;
begin
  InitializeXDisplay;
  Result := gdk_x11_screen_get_screen_number(XScreenGdk);
end;
{$endif}

procedure TGLApplication.InitializeXDisplay;
begin
  if not InitializeXDisplayDone then
  begin
    if XDisplayName <> '' then
      XDisplayGdk := gdk_display_open(PChar(XDisplayName)) else
      XDisplayGdk := gdk_display_get_default();

    XScreenGdk := gdk_display_get_default_screen(XDisplayGdk);

    InitializeXDisplayDone := true;
  end;
end;

{$endif read_implementation}