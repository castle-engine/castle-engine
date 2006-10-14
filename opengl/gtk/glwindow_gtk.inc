{
  Copyright 2004-2006 Michalis Kamburelis.

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

{ This contains two very similar GLWindow backends, both based on GTK.
  One based on GTK 1.x and GtkGLArea,
  the other one based on GTK 2.x and GtkGLExt. }

{$ifdef read_interface_types}
  PGtkGLArea =
    {$ifdef GLWINDOW_GTK_2}
    { For now I use GtkDrawingArea when GLWINDOW_GTK_2.
      But, really, GLAreaGtk could be any gtk widget with GLWINDOW_GTK_2. }
    PGtkWidget
    {$else}
    GtkGLArea.PGtkGLArea
    {$endif};
{$endif read_interface_types}

{$ifdef read_tglwindow_interface}
private
  WindowGtk: PGtkWindow;
  GLAreaGtk: PGtkGLArea;

  {$ifdef GLWINDOW_GTK_2}
  { These should be always equivalent to
    gtk_widget_get_gl_config(GLAreaGtk),
    gtk_widget_get_gl_context(GLAreaGtk),
    gtk_widget_get_gl_drawable(GLAreaGtk).
    Or nil when GLAreaGtk = nil. }
  GLConfig: PGdkGLConfig;
  GLContext: PGdkGLContext;
  GLDrawable: PGdkGLDrawable;
  {$endif}

  {$ifdef GLWINDOW_GTK_1}
  { look at implementation of InitImplDepend.
    Value of this is meaningfull only if FullScreen = true. }
  FullScreenUsingOverrideRedirect: boolean;
  {$endif}

  { These things are inited only when (MainMenu <> nil) and (not Closed). }
  window_vbox: PGtkVBox;
  window_menu_bar: PGtkMenuBar;
  window_accel_group: PGtkAccelGroup;

  { This will initialize window_menu_bar and insert it to window_vbox.
    It will not clear the previous value of window_menu_bar,
    it does not try to remove previous window_menu_bar from window_vbox
    etc. - you have to take care of that yourself. This simply assumes
    that window_vbox is empty and current value of window_menu_bar
    has no meaning. }
  procedure init_window_menu_bar;

  { ADialog will be made modal and transient for us.
    Then we will wait (while in TModeGLFrozenImage) for
    clicking on ok_button (this will make us exit with true)
    or cancel_button or by delete_event (these will make us exit with
    false). }
  function RunGtkDialog(ADialog: PGtkWindow;
    ok_button, cancel_button: PGtkWidget): boolean;

  FBorderWidth: Cardinal;

  procedure UpdateMouseVisible;
public

  { Change this only when Closed.
    This is the width of border of main GtkWindow that will be created
    in Init, set with gtk_container_set_border_width. }
  property BorderWidth: Cardinal
    read FBorderWidth write FBorderWidth default 0;
protected

  { Implementation of MakeGLAreaContainer in this class simply
    returns Result := GLArea.
    When creating gtk window in Init, I'm 1st creating GLArea widget.
    Then I'm inserting MakeGLAreaContainer(GLArea) inside my window.

    This means that you can override MakeGLAreaContainer if you want
    your window to contain something more than just OpenGL drawing area
    (and some menu, derived from MainMenu property).
    You can e.g. create in MakeGLAreaContainer some complex container,
    put there many various GTK widgets, connect your signals to them,
    and then put GLArea inside it.
    This way using this function you can extend window TGLWindow
    with whatever GTK GUI you like.

    The only requirement is that GLArea widget must be placed
    somewhere inside widget that you return in this function.

    Note: if you create here some widgets, remember to show them
    using gtk_widget_show.

    Some notes about using this function:
    Using this function kills some part of portability of GLWindow unit,
    as this function exists only when GLWindow is implemented on top
    of GTK, so you will have to always use GLWindow implemented on top GTK
    on every OS, e.g. with Win32 version of your program you will have
    to distribute GTK and GtkGLExt/GLArea for Windows.
    This is not really a problem, since GTK 2.x and GtkGLExt work
    excellent also under Windows (you know, GTK itself is a library
    that provides great portability). However this raises a question:
    what are advantages of using in such situation GLWindow unit
    (as opposed to just using directly Gtk and GtkGLExt API
    without any need for GLWindow unit): well, my advice is that
    if your program uses it's OpenGL area as it's central and crucial
    part (and using some GUI only to e.g. get some input from user
    about what to display in OpenGL area), than using GLWindow unit
    may be more straightforward as you have to write much less code in Gtk.

    Example of using this can be found in ../examples/test_glwindow_gtk_mix }
  function MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget; virtual;
{$endif read_tglwindow_interface}

{$ifdef read_tglwindowmanager_interface}
private
  { used in GlobalIdle }
  LastDoTimerTime: TMilisecTime;

  QuitPosted: boolean;

  { used in SetGlobalIdleInstalled }
  GlobalIdleHandle: guint;
  FGlobalIdleInstalled: boolean;
  procedure SetGlobalIdleInstalled(value: boolean);
  { This controls whether GlobalIdle is installed (i.e. registered to gtk).
    It is initialized to true in CreateImplDepend, finalized to false
    in DestroyImplDepend and for the most part stays as true.
    BUT it must be temporarily disabled in ProcessAllMesages. }
  property GlobalIdleInstalled: boolean
    read FGlobalIdleInstalled write SetGlobalIdleInstalled;

  { Find in Active[] window with WindowGtk = given seekWindowGtk.
    Return nil if no such window is found. }
  function TryActiveWindowGtk(seekWindowGtk: PGtkWindow): TGLWindow;
  { Find in Active[] window with GLAreaGtk = given seekGLAreaGtk.
    Return nil if no such window is found. }
  function TryActiveGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TGLWindow;

  { Same as TryActiveWindowGtk, but exception EInternalError when
    no such window is found. }
  function ActiveWindowGtk(seekWindowGtk: PGtkWindow): TGLWindow;
  { Same as TryActiveGLAreaGtk, but exception EInternalError when
    no such window is found. }
  function ActiveGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TGLWindow;

  { Will be used by SetMouseVisible. }
  InvisibleCursor: PGdkCursor;

  {$ifdef GLWINDOW_GTK_2}
  FXDisplayName: string;
  {$endif}

  {$ifdef GLWINDOW_USE_XF86VMODE}
  function XDisplay: Xlib.PDisplay;
  function XScreen: Integer;
  {$endif}
public
  {$ifdef GLWINDOW_GTK_2}
  { Set XDisplay name, this will be used for all TGLWindow
    that will be subsequently initialized by TGLWindow.Init.

    Note that this is exposed by GTK even for non-XWindows platforms,
    but I don't know what it does there. }
  property XDisplayName: string read FXDisplayName write FXDisplayName;
  {$endif}
{$endif read_tglwindowmanager_interface}

{$ifdef read_implementation}

{$ifndef GLWINDOW_GTK_2} {$I gtk_1_keysyms.inc} {$endif}
{$I glwindow_gtk_menu.inc}

{$ifdef WIN32}
  {$ifdef GLWINDOW_GTK_1}
    {$define GLWINDOW_GTK_1_WIN32}
  {$endif}
{$endif}

{$ifdef GLWINDOW_GTK_WITH_XLIB}
function gdk_x11_drawable_get_xid(Drawable: PGdkDrawable): X.TXID;
  cdecl; external gdklib name 'gdk_x11_drawable_get_xid';

function gdk_x11_drawable_get_xdisplay(Drawable: PGdkDrawable): Xlib.PDisplay;
  cdecl; external gdklib name 'gdk_x11_drawable_get_xdisplay';

function gdk_x11_get_default_screen(): Gint;
  cdecl; external gdklib name 'gdk_x11_get_default_screen';

function gdk_x11_get_default_xdisplay(): Xlib.PDisplay;
  cdecl; external gdklib name 'gdk_x11_get_default_xdisplay';
{$endif}

{ gtk, gdk helpers ------------------------------------------------------------ }

{$ifndef GLWINDOW_GTK_2}
const
  { missing from FPC's gdktypes.pp (present in
    /usr/include/gtk-1.2/gdk/gdktypes.h) }
  GDK_CURRENT_TIME = 0;
{$endif}

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

  (*
    It seems that win32 gtk (at least GTK 1.3) uses other consts for
    numpad plus and minus.

    Note: I *cannot* convert here two GDK_xxx codes to one K_xxx
    code (e.g. write
      GDK_plus_key, GDK_KP_Add: Result := K_Numpad_Plus;
    ), this would lead to similiar problems as those described in
    glwindow_private_modifiers_down.inc.

    For some time I had here this code:
      {$ifdef GLWINDOW_GTK_1_WIN32} GDK_KEY_plus_key {$else} GDK_KEY_KP_Add {$endif}:
         Result := K_Numpad_Plus;
      {$ifdef GLWINDOW_GTK_1_WIN32} GDK_KEY_minus {$else} GDK_KEY_KP_Subtract {$endif}:
         Result := K_Numpad_Minus;
    but that's a hack only for GTK 1 under Windows
    (and it collides with checking for K_Minus below),
    so I removed it.
  *)
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

  Callbacks that expect as 1st parameter GLAreaGtk of some Active[] window
  have 1st parameter AGLAreaGtk: PGtkAreaGtk.
  Similarly, callbackas that expect as 1st parameter WindowGtk of some
  Active[] window have 1st parameter AWindowGtk: PGtkWindow.
}

{$define SIGNAL_GLAREA_BEGIN:=
begin
 with glwm.ActiveGLAreaGtk(AGLAreaGtk) do begin}
{$define SIGNAL_GLAREA_END:=
 end; end}

{$define SIGNAL_WINDOW_BEGIN:=
begin
 with glwm.ActiveWindowGtk(AWindowGtk) do begin}
{$define SIGNAL_WINDOW_END:=
 end; end}

function GlobalIdle(data: gpointer):
  {$ifdef GLWINDOW_GTK_2} gboolean {$else} gint {$endif}; cdecl; forward;

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

   But: when AutoRedisplay := true (on some similiar situation is present,
   i.e. we're constantly doing PostRedisplay) it is not so safe to do this
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

 DoDraw;

 { This is needed, GlobalIdle must be called even if you continuously
   call PostRedisplay (of just have AutoRedisplay := true).
   See comments at SetGlobalIdleInstalled to know why I can't
   do this differently (by setting GlobalIdle's priority == redraw priority). }
 GlobalIdle(nil);

 Result := true;
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
   Reasons -- the same as in GLWindow_xlib.inc. }
 gdk_window_get_root_origin(GTK_WIDGET(WindowGtk)^.window, @LeftGtk, @TopGtk);
 Left := LeftGtk;
 Top := TopGtk;
 { for the sake of gtk, I can't tell that I "handled" this in any way }
 Result := false;
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
 Result := false;
SIGNAL_GLAREA_END;

function signal_delete_event(AWindowGtk: PGtkWindow; Event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
SIGNAL_WINDOW_BEGIN
 DoCloseQuery;
 Result := true;
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

   {$ifdef WIN32}
   { It seems that GTK 1.3 for win32 cannot translate GDK_KEY_Escape and
     GDK_KEY_Return to standard chars (#13 and #27). So I'm fixing it here. }
   if Key = K_Escape then
    CharKey := CharEscape else
   if Key = K_Enter then
    CharKey := CharEnter else
   {$endif}
   {$ifdef GLWINDOW_GTK_2}
   { It seems that GTK 2 doesn't translate backspace and tab to
     appropriate chars. So I'm fixing it here. }
   if Key = K_Tab then
    CharKey := CharTab else
   if Key = K_BackSpace then
    CharKey := CharBackSpace else
   {$endif}
   { note: testing is Event.Length = 1 not only makes sure that we can
     safely take Event.TheString[0] as a whole CharKey,
     but it also makes sure that this is not something MBCS encoded. }
   if Event^.Length = 1 then
    CharKey := Event^.{$ifdef GLWINDOW_GTK_2} _string {$else} TheString {$endif} [0] else
    CharKey := #0;

   if (Key <> K_None) or (CharKey <> #0) then DoKeyDown(Key, CharKey);
  end;
 end;

 { It would be nice to say here Result := true if key was handled
   in DoKeyDown. But I don't have any indicator whether key was handled
   in EventKeyDown or OnKeyDown (because such thing would complicate
   implementation of OnKeyDown callback in every program using
   GLWindow). So I must assume that either (1) always or (2) never
   when I called DoKeyDown key is handled.
   But (1) is actually not acceptable, because it disallows user
   to activate menu using F10 key. So I must set Result := false here.

   I'm also setting Result := false in signal_key_release_event,
   to be consequent.
 }
 Result := false;
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
 Result := false;
SIGNAL_WINDOW_END;

function signal_button_press_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TMouseButton;
SIGNAL_GLAREA_BEGIN
 Result := (Event^.{$ifdef GLWINDOW_GTK_2} _type {$else} TheType {$endif}
   = GDK_BUTTON_PRESS) and
   ButtonNumberToMouseButton(Event^.button, mb);
 if Result then
  DoMouseDown(Round(Event^.x), Round(Event^.y), mb);
SIGNAL_GLAREA_END;

function signal_button_release_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventButton;
  data: gpointer): gboolean; cdecl;
var mb: TMouseButton;
SIGNAL_GLAREA_BEGIN
 Result := (Event^.{$ifdef GLWINDOW_GTK_2} _type {$else} TheType {$endif}
   = GDK_BUTTON_RELEASE) and
   ButtonNumberToMouseButton(Event^.button, mb);
 if Result then
  DoMouseUp(Round(Event^.x), Round(Event^.y), mb);
SIGNAL_GLAREA_END;

function signal_motion_notify_event(AGLAreaGtk: PGtkGLArea; Event: PGdkEventMotion;
  data: gpointer): gboolean; cdecl;
SIGNAL_GLAREA_BEGIN
 DoMouseMove(Round(Event^.x), Round(Event^.y));
 Result := true;
SIGNAL_GLAREA_END;

procedure signal_menu_item_activate(AMenuItemGtk: PGtkMenuItem;
  data: gpointer); cdecl;

  {$ifndef GLWINDOW_GTK_2}
  { GTK 2 already defines this function. }
  function gtk_check_menu_item_get_active(
    check_menu_item: PGtkCheckMenuItem): gboolean;
  begin
   Result := active(PGtkCheckMenuItem(check_menu_item)^) <> 0;
  end;
  {$endif}

var MenuItem: TMenuItem;
    glwin: TGLWindow;
begin
 MenuItem := MenuItemFromSmallId( TPointerSInt(Data) );
 glwin := TGLWindow( gtk_object_get_user_data(GTK_OBJECT(AMenuItemGtk)) );

 if MenuItem is TMenuItemChecked then
 begin
  { GTK checked menu items do always something like our AutoCheckedToggle.
    This assert confirms that: }

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
    because we would call glwin.DoMenuCommand(MenuItem) TWICE.

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

 glwin.DoMenuCommand(MenuItem);
end;

function signal_window_focus_out_event(AWindowGtk: PGtkWindow;
  Event: PGdkEventfocus; data: gpointer): gboolean; cdecl;
{ This is called when user switches to another window. }
SIGNAL_WINDOW_BEGIN
 ReleaseAllKeysAndMouse;
 { for the sake of gtk, I can't tell that I "handled" this in any way }
 Result := false;
SIGNAL_WINDOW_END;

{ TGLWindow ------------------------------------------------------------ }

procedure TGLWindow.SetMouseVisible(const Value: boolean);
begin
  if FMouseVisible <> Value then
  begin
    FMouseVisible := Value;
    if not Closed then
      UpdateMouseVisible;
  end;
end;

procedure TGLWindow.UpdateMouseVisible;
begin
  if MouseVisible then
    gdk_window_set_cursor(GTK_WIDGET(GLAreaGtk).Window, nil) else
    gdk_window_set_cursor(GTK_WIDGET(GLAreaGtk).Window, Glwm.InvisibleCursor);
end;

procedure TGLWindow.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  {$ifdef GLWINDOW_GTK_WITH_XLIB}
  XWarpPointer(
    gdk_x11_drawable_get_xdisplay(GTK_WIDGET(GLAreaGtk).Window),
    X.None,
    gdk_x11_drawable_get_xid(GTK_WIDGET(GLAreaGtk).Window),
    0, 0, 0, 0, NewMouseX, NewMouseY);
  {$else}
  raise Exception.Create('TODO: TGLWindow.SetMousePosition not implemented');
  {$endif}
end;

procedure TGLWindow.MainMenuChangedImplDepend;
begin
 { According to GTK docs (at gtk_container_remove),
   the simplest way to remove window_menu_bar from window_vbox is to simply
   do gtk_widget_destroy on window_menu_bar.
   This will take care of removing window_menu_bar appropriately. }
 gtk_widget_destroy(GTK_WIDGET(window_menu_bar));

 init_window_menu_bar;
end;

procedure TGLWindow.SwapBuffers;
begin
 {$ifdef GLWINDOW_GTK_2}
 gdk_gl_drawable_swap_buffers(GLDrawable);
 {$else}
 gtk_gl_area_swapbuffers(GLAreaGtk);
 {$endif}
end;

procedure TGLWindow.PostRedisplay;
begin
 if not Closed then gtk_widget_queue_draw(GTK_WIDGET(GLAreaGtk));
end;

procedure TGLWindow.FlushRedisplay;
begin
 {$ifdef GLWINDOW_GTK_2}
 gdk_window_process_updates(GTK_WIDGET(WindowGtk)^.Window, true);
 {$else}
 { I don't know how to implement it efficiently under GTK 1.x
   -- maybe it's not possible ?
   So I'm simply always calling DoDraw here. }
 DoDraw;
 {$endif}
end;

procedure TGLWindow.SetCaption(const Value: string);
begin
 FCaption := Value;
 if not Closed then
  gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(Caption));
end;

procedure TGLWindow.MakeCurrent;
begin
 Assert(not Closed);

 {$ifdef GLWINDOW_GTK_2}

 { When GLAreaGtk is implemented by gdkglwindow-x11 or
   gdkglpixmap-x11 or gdkglwindow-win32:
   gdk_gl_drawable_gl_begin simply calls
   gdk_gl_window_impl_x11_make_context_current
   and gdk_gl_drawable_gl_end doesn't do anything.
   So I can simply forget about gdk_gl_drawable_gl
   and call gdk_gl_drawable_gl_begin on MakeCurrent.

   In case GLAreaGtk is implemented by gdkglpixmap-win32
   (this is not possible to achieve now in GLWindow, but maybe
   in the future will be) or just in case it will be implemented
   on some system differently, I will have to only slghtly
   improve this procedure: I will keep track of currently current
   GLAreaGtk, and I will do something like
     gdk_gl_drawable_gl_end(currently active);
     currently active := newly active;
     gdk_gl_drawable_gl_begin(currently active);
 }

 gdk_gl_drawable_gl_begin(GLDrawable, GLContext);
 {$else}
 gtk_gl_area_make_current(GLAreaGtk);
 {$endif}
end;

function TGLWindow.MakeGLAreaContainer(GLArea: PGtkGLArea): PGtkWidget;
begin
 Result := GTK_WIDGET(GLArea);
end;

{$ifdef GLWINDOW_GTK_2}
{ This is mispelled in FPC gtk bindings, see
  [http://www.freepascal.org/bugs/showrec.php3?ID=5112] }
function gdk_display_open(display_name:Pgchar):PGdkDisplay;
  cdecl; external gdklib;
{$endif}

procedure TGLWindow.InitImplDepend;

{ Notes about FullScreen implementation with GTK_1:
  1) Note that Left/Top and Width/Height will already be initialized before
     InitImplDepend to appropriate values (i.e., the window will cover
     whole screen).

  2) When FullScreenUsingOverrideRedirect = true
     I'm implementing FullScreen window the same way as I do it in XLIB
     implementation: I'm doing override_redirect (using
     gtk_window_new(GTK_WINDOW_POPUP)) and I'm grabbing keyboard (after
     gtk_widget_show(WindowGtk), because this requires GDK window
     "GTK_WIDGET(WindowGtk).Window" initialized ).

     Unfortunately this makes implementing MainMenu impossible.
     Activating menu bar switches user to new window in X and makes impossible
     to go back to our window. Our program must be then killed.
     This is a typical problem with using override_redirect.
     So we simply cannot give user any menu when we're in FullScreen
     and FullScreenUsingOverrideRedirect.

  3) If FullScreenUsingOverrideRedirect = false I will use
       gdk_window_set_decorations(GTK_WIDGET(WindowGtk).Window, 0);
     instead of using GTK_WINDOW_POPUP and gdk_keyboard_grab.
     But that is not producing satisfactory results -- things like
     gnome-panel are covering our fullscreen window. So our "fullscreen window"
     is not really fullscreen.

     On the other hand,
     advantage of this is that we're avoiding using override_redirect
     and so we can show MainMenu.

  So I have two possible implementations for FullScreen. And both of them
  wrong. In InitImplDepend I'm deciding which one to use:
    FullScreenUsingOverrideRedirect := MainMenu = nil

  How about GTK_2 ?

  I'm doing fullscreen using gtk_window_fullscreen,
  this tells window manager that I want to be fullscreen
  using XAtom _NET_WM_STATE_FULLSCREEN.
  This avoids using override_redirect (so gtk menu is usable,
  and user can use window-manager key shortcuts like Alt+Tab
  with our fullscreen window) and at the same time window is on top
  (it should not be covered by things like gnome-panel).
}

  {$ifndef GLWINDOW_GTK_2}
  { This is already present in GTK 2.x }
  procedure gtk_window_set_resizable(Window: PGtkWindow; Resizable: gboolean);
  begin
   if Resizable then
    gtk_window_set_policy(Window, 0, 1, 0) else
    gtk_window_set_policy(Window, 0, 0, 1);
  end;

  const
    GDK_GL_ATTRIB_LIST_NONE = GDK_GL_NONE;
  {$endif}

const
  VBoxSpacing = 2;

  procedure set_default_glarea_size(Width, Height: Integer);
  { This requires window_menu_bar to be initialized (only if MainMenu <> nil,
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
    gtk_widget_size_request(GTK_WIDGET(window_menu_bar), @WindowReq);
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
  VisualAttr: TDynLongIntArray;
  GLAreaContainer: PGtkWidget;
  {$ifdef GLWINDOW_GTK_2}
  WindowXDisplay: PGdkDisplay;
  WindowXScreen: PGdkScreen;
  {$endif}
begin
 {$ifdef GLWINDOW_GTK_1}
 FullScreenUsingOverrideRedirect := MainMenu = nil;
 WindowGtk := PGtkWindow(gtk_window_new(
   Iff(FullScreen and FullScreenUsingOverrideRedirect,
     GTK_WINDOW_POPUP, GTK_WINDOW_TOPLEVEL)
   ) );
 {$else}
 WindowGtk := PGtkWindow(gtk_window_new(GTK_WINDOW_TOPLEVEL));
 {$endif}

 gtk_window_set_title(GTK_WINDOW(WindowGtk), PChar(Caption));
 gtk_container_set_border_width(GTK_CONTAINER(WindowGtk), BorderWidth);

 { Create VisualAttr and then create appropriate GLAreaGtk
   (and GLConfig in case of GTK 2) honouring this VisualAttr.

   The code for initializing VisualAttr is quite the same
   as the code from glwindow_xlib.inc because the meaning of GDK_GL_
   constants is the same as GLX_ constants. }
 VisualAttr := TDynLongIntArray.Create;
 try
  VisualAttr.AllowedCapacityOverflow := 20;
  if DoubleBuffer then VisualAttr.AppendItem(GDK_GL_DOUBLEBUFFER);
  VisualAttr.AppendArray([
    GDK_GL_RGBA,
    { Note that sizes of buffers in VisualAttr must be given in bits
      (not in e.g. bytes), so this is exactly what I have in my variables. }
    GDK_GL_DEPTH_SIZE,
      {$ifdef WORKAROUND_DEPTH_BITS_0_BUG} Max(DepthBufferBits, 1)
      {$else} DepthBufferBits
      {$endif},

    { This is a workaround for gtkglext win32 bug,
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
    GDK_GL_ACCUM_ALPHA_SIZE, AccumBufferBits[3],
    GDK_GL_ATTRIB_LIST_NONE ]);

  {$ifdef GLWINDOW_GTK_2}

  GLConfig := gdk_gl_config_new(@VisualAttr.Items[0]);
  { Looking at gears demo of GtkGLExt, one should check GLConfig = nil
    to know whether such gl configuration was possible. }
  if GLConfig = nil then
   ContextNotPossible;

  GLAreaGtk := gtk_drawing_area_new;
  if not gtk_widget_set_gl_capability(GLAreaGtk, GLConfig, nil, true,
    GDK_GL_RGBA_TYPE) then
   ContextNotPossible;

  {$else}

  GLAreaGtk := PGtkGLArea( gtk_gl_area_new(@VisualAttr.Items[0]) );
  if GLAreaGtk = nil then
   ContextNotPossible;

  {$endif}
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

 { Add us to Active[] windows to ensure that all callbacks that are
   eventually called by procedures below will be able to use
   glwm.ActiveGLAreaGtk and glwm.ActiveWindowGtk. }
 glwm.ActiveAdd(Self);

 { setup window position from Left, Top }
 gtk_widget_set_uposition(GTK_WIDGET(WindowGtk), Left, Top);

 GLAreaContainer := MakeGLAreaContainer(GLAreaGtk);

 if MainMenu <> nil then
 begin
  window_accel_group := gtk_accel_group_new;
  gtk_window_add_accel_group(GTK_WINDOW(WindowGtk), window_accel_group);

  window_vbox := PGtkVBox(gtk_vbox_new(false, VBoxSpacing));
  gtk_widget_show(GTK_WIDGET(window_vbox));
  init_window_menu_bar;
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
    glwm.ScreenWidth/Height. But maybe later I will change this in TGLWindow.Init
    implementation, so I do not want to depend on this. }

  gtk_widget_set_usize(GTK_WIDGET(WindowGtk),
    { The correct things to say here are MinWidth, MinHeight.
      After all, it's gtk_window_set_default_size that should specify
      full window size. Also, I should take here into account
      that the fullscreen state of my window may be changed by user
      using window-manager keys that add or remove decorations from
      my window and resize it. It means that I can't think of
      a fullscreen window like "a window that can't be resized".
      A fulscreen window is a window that merely
      "initially has fullscreen size and no decorations",
      but it CAN be changed at runtime by user.

      Unfortunately, I can't express this with GTK_1, with GTK_1
      I must say here glwm.ScreenWidth, glwm.ScreenHeight, otherwise
      GTK_1 seems to just ignore gtk_window_set_default_size and
      opens fullscreen window with no decorations but
      with MinWidth, MinHeight size (instead of screen size). }
    {$ifdef GLWINDOW_GTK_2} MinWidth, MinHeight
    {$else} glwm.ScreenWidth, glwm.ScreenHeight
    {$endif} );
  gtk_window_set_default_size(WindowGtk, glwm.ScreenWidth, glwm.ScreenHeight);

  {$ifdef GLWINDOW_GTK_2}
  gtk_window_fullscreen(WindowGtk);
  {$else}

  { I'm trying to make window non-resizeable when FullScreen
    (again, no matter what value has ResizeAllowed). This is (probably) not
    important when I'm implementing FullScreen using override_redirect
    (it is important when I'm using gdk_window_set_decorations
    or gtk_window_fullscreen),
    since override_redirect blocks any interactions between our window
    and window manager.

    I'm not setting whether window is resizeable
    when I'm doing FullScreen by gtk_window_fullscreen.
    In this case window manager should anyway treat my window like that.
    I should not interfere with this, as such behaviour would
    make my program beave poorly when user switches fullscreen
    at runtime *using window manager* (I'm not talking about
    switching fullscreen using Ctrl+F from my programs but using
    window manager specific functions, like Alt+BackSpace on my metacity
    configuration). }
  gtk_window_set_resizable(WindowGtk, false);

  {$endif}
 end else
 begin
  if ResizeAllowed <> raAllowed then
   gtk_widget_set_usize(GTK_WIDGET(GLAreaGtk), Width, Height) else
   gtk_widget_set_usize(GTK_WIDGET(GLAreaGtk), MinWidth, MinHeight);
  set_default_glarea_size(Width, Height);
  gtk_window_set_resizable(WindowGtk, ResizeAllowed = raAllowed);
 end;

 {$ifdef GLWINDOW_GTK_2}
 if Glwm.XDisplayName <> '' then
 begin
   WindowXDisplay := gdk_display_open(PChar(Glwm.XDisplayName));
   WindowXScreen := gdk_display_get_default_screen(WindowXDisplay);
   gtk_window_set_screen(WindowGtk, WindowXScreen);
 end;
 {$endif}

 { Show WindowGtk, it will also make visible all other widgets.
   If I understand everything right, gtk_widget_show realizes the widget
   before returning (but does not necessarily map the Gdk window,
   one can use gtk_widget_show_now for this; but I don't need it) }
 gtk_widget_show(GTK_WIDGET(WindowGtk));

 { From this point I require that WindowGtk is realized (has associated
   GdkWindow, i.e. I can use GTK_WIDGET(WindowGtk).Window).
   It also means that I can do MakeCurrent (and call OpenGL commands)
   from now. In particular, we have valid gl context when we leave InitImplDepend. }
 if (GTK_REALIZED and GTK_WIDGET_FLAGS( GTK_WIDGET(WindowGtk) )) = 0 then
  raise EInternalError.Create('GLWindow.InitImplDepend: GdkWindow not realized yet');

 {$ifdef GLWINDOW_GTK_2}
 { These must be inited only when I'm sure that window is realized.
   I checked this even in gtkglext sources:
   yes, gtk_widget_get_gl_drawable and gtk_widget_get_gl_context check
   and always return nil if their widget is not realized. }
 GLDrawable := gtk_widget_get_gl_drawable(GLAreaGtk);
 GLContext := gtk_widget_get_gl_context(GLAreaGtk);
 {$endif}

 if FullScreen then
 begin
  { Note: gtk_window_fullscreen may actually not make our window fullscreen,
    just like gdk_window_set_decorations is not guaranteed to work
    under GTK 1, as they both assume that window manager will honour our
    request. Dummy window managers, or smart window managers that are
    forced by user to do something, may ignore our requests, or may
    change our window's decorations/fullscreen state while our program works.

    But that's not a problem for us.
    We will anyway behave like we are fullscreen,
    and our DoResize is already prepared for cases where window may
    be forced to have different size than requested. }

  {$ifdef GLWINDOW_GTK_1}
  if FullScreenUsingOverrideRedirect then
   Check(gdk_keyboard_grab(GTK_WIDGET(WindowGtk).Window, 0, GDK_CURRENT_TIME) = 0,
     'gdk_keyboard_grab was not successful. Try to run this program in windowed '+
     '(not fullscreen) mode.') else
   gdk_window_set_decorations(GTK_WIDGET(WindowGtk).Window, 0);
  {$endif}
 end;

 UpdateMouseVisible;
end;

procedure TGLWindow.CloseImplDepend;
begin
 Glwm.AutoRedisplayList.Delete(Self);

 {$ifdef GLWINDOW_GTK_1}
 if FullScreen and FullScreenUsingOverrideRedirect then
  gdk_keyboard_ungrab(GDK_CURRENT_TIME);
 {$endif}

 { Do not unref here GLAreaGtk.
   GLAreaGtk is placed inside WindowGtk when we do
   gtk_container_add(WindowGtk, GLAreaGtk). During gtk_container_add
   WindowGtk calls sink on GLAreaGtk, so GLAreaGtk has reference 1 and it
   means that it is referenced from WindowGtk. So WindowGtk is responsible
   for calling unref on GLAreaGtk. }
 GLAreaGtk := nil;

 window_menu_bar := nil;
 window_vbox := nil;

 {$ifdef GLWINDOW_GTK_2}
 GLDrawable := nil; { TODO -- unref it ? }
 GLConfig := nil; { TODO -- unref it ? }
 {$endif}

 if window_accel_group <> nil then
 begin
  {$ifdef GLWINDOW_GTK_2}
  g_object_unref(G_OBJECT(window_accel_group));
  {$else}
  gtk_accel_group_unref(window_accel_group);
  {$endif}
  window_accel_group := nil;
 end;

 { Note: we could use
   gtk_quit_add_destroy(1, GTK_OBJECT(WindowGtk));
   in InitImplDepend instead of doing gtk_widget_unref below.
   But doing gtk_widget_unref below is a cleaner way (we're freeing WindowGtk
   as soon as it can be freed). }
 if WindowGtk <> nil then
 begin
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

procedure TGLWindow.CreateImplDepend;
begin
end;

{ TGLWindow.*Dialog --------------------------------------------------- }

type
  { Data type for RunGtkDialog.
    PDialogData will be passed as a 2nd argument to signals
    (and 3rd argument to events) associated with dialog boxes. }
  TDialogData = record
    Answered: boolean; { = false }
    Answer: boolean;
    Dialog: PGtkWindow;
  end;
  PDialogData = ^TDialogData;

procedure signal_dialog_ok_clicked(AWidget: PGtkWidget; Data: PDialogData); cdecl;
begin
 Data^.Answered := true;
 Data^.Answer := true;
end;

procedure signal_dialog_cancel_clicked(AWidget: PGtkWidget;
  Data: PDialogData); cdecl;
begin
 Data^.Answered := true;
 Data^.Answer := false;
end;

function signal_dialog_delete_event(AWidget: PGtkWidget; Event: PGdkEventKey;
  Data: PDialogData): gboolean; cdecl;
begin
 signal_dialog_cancel_clicked(nil, Data);
 { don't allow to call gtk_widget_destroy, we will call it ourselves
   in FileDialog/ColorDialog. }
 Result := true;
end;

function TGLWindow.RunGtkDialog(ADialog: PGtkWindow;
  ok_button, cancel_button: PGtkWidget): boolean;
var Data: TDialogData;
    Mode: TGLModeFrozenScreen;
begin
 Data.Dialog := ADialog;

 gtk_signal_connect(GTK_OBJECT(ok_button),
   'clicked',      GTK_SIGNAL_FUNC(@signal_dialog_ok_clicked),     @Data);
 gtk_signal_connect(GTK_OBJECT(cancel_button),
   'clicked',      GTK_SIGNAL_FUNC(@signal_dialog_cancel_clicked), @Data);
 gtk_signal_connect(GTK_OBJECT(Data.Dialog),
   'delete_event', GTK_SIGNAL_FUNC(@signal_dialog_delete_event),   @Data);

 gtk_window_set_modal(ADialog, true);
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
  while not Data.Answered do glwm.ProcessMessage(true);
 finally Mode.Free end;

 Result := Data.Answer;
end;

function TGLWindow.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean): boolean;
var Dialog: PGtkFileSelection;
begin
 Dialog := PGtkFileSelection(gtk_file_selection_new(PChar(Title)));
 try
  gtk_file_selection_set_filename(Dialog, PChar(FileName));
  Result := RunGtkDialog(PGtkWindow(Dialog),
    Dialog^.ok_button, Dialog^.cancel_button);
  if Result then
   FileName := gtk_file_selection_get_filename(Dialog);
 finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

function TGLWindow.ColorDialog(var Color: TVector3Single): boolean;
var Dialog: PGtkColorSelectionDialog;
    GtkColor: array[0..3]of GDouble;
    i: Integer;
begin
 Dialog := PGtkColorSelectionDialog(gtk_color_selection_dialog_new('Choose color'));
 try
  for i := 0 to 2 do GtkColor[i] := Color[i];
  gtk_color_selection_set_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);

  Result := RunGtkDialog(PGtkWindow(Dialog),
    Dialog^.ok_button, Dialog^.cancel_button);

  if Result then
  begin
   gtk_color_selection_get_color(PGtkColorSelection(Dialog^.colorsel), @GtkColor);
   for i := 0 to 2 do Color[i] := GtkColor[i];
  end;
 finally gtk_widget_destroy(GTK_WIDGET(Dialog)); end;
end;

{ TGLWindowsManager: global idle ------------------------------------------- }

function GlobalIdle(data: gpointer):
  {$ifdef GLWINDOW_GTK_2} gboolean {$else} gint {$endif}; cdecl;
{ This function simply calls glwm.DoSelfIdle and glwm.DoActiveWindowsIdle
  and takes care of calling glwm.DoSelfTimer and glwm.DoActiveWindowsTimer. }
begin
 with glwm do
 begin
  DoSelfIdle;
  DoActiveWindowsIdle;

  { For simplicity, we're implementing here our own timer mechanism,
    without using gtk timers. }
  MaybeDoTimer(LastDoTimerTime);
 end;

 { Returning false here means to remove this idle function.
   So I always return true here. }
 Result := {$ifndef GLWINDOW_GTK_2} gint {$endif} (true);
end;

procedure TGLWindowsManager.SetGlobalIdleInstalled(value: boolean);
begin
 if FGlobalIdleInstalled <> value then
 begin
  FGlobalIdleInstalled := value;
  if FGlobalIdleInstalled then
  begin
   { Notes for GTK_1:
     This is important: priority of Idles must be the same as the priority of
     redraws. That's because OnIdle must be constantly called, even when
     AutoRedisplay = true (i.e. when we always have some redraw pending).
     The default low priority of gtk_idle_add would make OnIdle call impossible
     while AutoRedisplay = true.
     So GTK_PRIORITY_REDRAW is needed.

     Notes for GTK_2:
     While idle with priority GTK_PRIORITY_REDRAW works perfectly
     with GTK_1, it does cause problems with GTK_2 :
     After switching to alternative menu,
     key shortcuts to menu items do not work.
     I don't know what's the exact reason of this, I guess that I'm just
     blocking some other internal gtk idles and so I'm causing some
     mysterious problems.

     Simple test case with some comments is in
     gtk.testy/test_changing_menu/test_changing_menu.dpr.

     Experiments show that giving priority G_PRIORITY_DEFAULT_IDLE is OK.

     What to do with the problem
     "OnIdle must be constantly called, even when AutoRedisplay = true" ?
     Well, this is simple: I'm just calling GlobalIdle(nil) manually
     after calling DoDraw in signal_expose_event.
   }
   GlobalIdleHandle :=
     gtk_idle_add_priority(G_PRIORITY_DEFAULT_IDLE,
       {$ifdef FPC_OBJFPC} @ {$endif} GlobalIdle, nil);
  end else
  begin
   gtk_idle_remove(GlobalIdleHandle);
  end;
 end;
end;

{ TGLWindowsManager ------------------------------------------- }

function TGLWindowsManager.TryActiveWindowGtk(seekWindowGtk: PGtkWindow):
  TGLWindow;
var i: Integer;
begin
 for i := 0 to ActiveCount-1 do
  if Active[i].WindowGtk = seekWindowGtk then
   Exit(Active[i]);
 Result := nil;
end;

function TGLWindowsManager.TryActiveGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TGLWindow;
var i: Integer;
begin
 for i := 0 to ActiveCount-1 do
  if Active[i].GLAreaGtk = seekGLAreaGtk then
   Exit(Active[i]);
 Result := nil;
end;

function TGLWindowsManager.ActiveWindowGtk(seekWindowGtk: PGtkWindow): TGLWindow;
begin
 Result := TryActiveWindowGtk(seekWindowGtk);
 if Result = nil then raise EInternalError.Create(
   'No Active[] window with such WindowGtk found');
end;

function TGLWindowsManager.ActiveGLAreaGtk(seekGLAreaGtk: PGtkGLArea): TGLWindow;
begin
 Result := TryActiveGLAreaGtk(seekGLAreaGtk);
 if Result = nil then raise EInternalError.Create(
   'No Active[] window with such GLAreaGtk found');
end;

procedure TGLWindowsManager.Loop;
begin
  { Alternative impl: while ProcessMessage(true) do ; }
  gtk_main();
end;

function TGLWindowsManager.ProcessAllMessages: boolean;
begin
 { See comments at TGLWindow.DoDraw to understand this mess
   with AutoRedisplayAddToList here. }
 if AutoRedisplayAddToList = 0 then AutoRedisplayList.Clear;
 Inc(AutoRedisplayAddToList);
 try

  { GlobalIdleInstalled must be temporarily turned off.
    That's because when GlobalIdleInstalled = true gtk_events_pending()
    always returns true (because it thinks that it should call GlobalIdle
    in a loop).

    To make sure that glwm.DoSelfIdle and DoIdle of Active[] windows
    is executed we just manually execute at the end GlobalIdle. }
  GlobalIdleInstalled := false;
  try

   while gtk_events_pending() <> 0 do
    Result := not {$ifndef GLWINDOW_GTK_2} gboolean {$endif}
      (gtk_main_iteration());

  finally GlobalIdleInstalled := true end;

  { Without call below glwm.DoSelfIdle and DoIdle of Active[] windows
    would not be executed in any ProcessAllMessages.
    (It is possible that setting GlobalIdleInstalled := true; will also
    execute GlobalIdle as a side effect. But we must not depend on that.) }
  GlobalIdle(nil);

 finally Dec(AutoRedisplayAddToList) end;
 if AutoRedisplayAddToList = 0 then AutoRedisplayList.PostRedisplay;
end;

function TGLWindowsManager.ProcessMessage(AllowSuspend: boolean): boolean;
begin
  (* I would like to do here
    Result := not {$ifndef GLWINDOW_GTK_2} gboolean {$endif}
      (gtk_main_iteration_do(AllowSuspend))

    I.e. I want to use the result of gtk_main_iteration_do to return it.
    Unfortunately, it turns out that gtk_main_iteration_do always returns
    true if the program doesn't use gtk_main call at all
    (i.e. does it's whole loop manually, by calling Glwm.ProcessMessage).
    This is confirmed by
    1. practice (just change impl of Loop to
         while ProcessMessage(true) do ;
       instead of relaying on gtk_main
    2. gtk source code:
         gint
         gtk_main_iteration_do (gboolean blocking)
         {
           GDK_THREADS_LEAVE ();
           g_main_iteration (blocking);
           GDK_THREADS_ENTER ();

           if (main_loops)
             return !g_main_is_running (main_loops->data);
           else
             return TRUE;
         }


       and note that main_loops is set only by gtk_main.
       So a program that doesn't call gtk_main at all has
       main_loops = nil always.

    This means that I should trace the QuitPosted value myself,
    just like I do for X11 or WinAPI GLWindow implementations.
    And the result of gtk_main_iteration_do call should be ignored. *)

  gtk_main_iteration_do(AllowSuspend);

  Result := not QuitPosted;
end;

procedure TGLWindowsManager.QuitWhenNoWindowsActive;
begin
 if gtk_main_level() > 0 then gtk_main_quit();

 { QuitPosted is needed, gtk_main_quit is not enough --- see
   comments in ProcessMessage impl.

   Actually, we call gtk_main_quit
   only to exit from gtk_main --- if we would not use gtk_main
   (but e.g. implement Loop simply by calling ProcessMessage,
   which is entirely possible), then calling gtk_main_quit
   would not be needed at all. }

 QuitPosted := true;
end;

procedure TGLWindowsManager.CreateImplDependent;

  procedure InitializeInvisibleCursor;
  const
    Bits: array[0..0] of Guchar = ({$ifdef GLWINDOW_GTK_2} 0 {$else} #0 {$endif});
    Color: TGdkColor = (Pixel: 0; Red: 0; Green: 0; Blue: 0);
  var
    Pixmap: PGdkPixmap;
  begin
    { Based on [http://mail.gnome.org/archives/gtk-app-devel-list/2005-January/msg00370.html] }
    Pixmap := gdk_bitmap_create_from_data(nil, @Bits, 1, 1);
    try
      InvisibleCursor := gdk_cursor_new_from_pixmap(
        Pixmap, Pixmap, @Color, @Color, 0, 0);
    finally gdk_pixmap_unref(Pixmap); end;
  end;

begin
  gtk_init(@argc, @argv);

  {$ifdef GLWINDOW_GTK_2}
  gtk_gl_init(@argc, @argv);
  {$endif}

  AutoRedisplayList := TGLWindowsList.Create;

  FGlobalIdleInstalled := false;
  GlobalIdleInstalled := true;

  InitializeInvisibleCursor;

  QuitPosted := false;
end;

procedure TGLWindowsManager.DestroyImplDependent;
begin
  GlobalIdleInstalled := false;

  FreeAndNil(AutoRedisplayList);

  if InvisibleCursor <> nil then
  begin
    {$ifdef GLWINDOW_GTK_2}
    gdk_cursor_unref(InvisibleCursor);
    {$endif}
    InvisibleCursor := nil;
  end;
end;

{ When GLWINDOW_USE_XF86VMODE, then glwindow_xf86vmode.inc
  will provide implementation for ScreenWidth and ScreenHeight. }
{$ifndef GLWINDOW_USE_XF86VMODE}
function TGLWindowsManager.ScreenWidth: integer;
begin
 Result := gdk_screen_width();
end;

function TGLWindowsManager.ScreenHeight: integer;
begin
 Result := gdk_screen_height();
end;
{$endif GLWINDOW_USE_XF86VMODE}

function TGLWindowsManager.ImplementationName: string;
begin
 Result := {$ifdef GLWINDOW_GTK_2} 'GTK+' {$else} 'GTK+ 1.x' {$endif};
end;

{$ifdef GLWINDOW_USE_XF86VMODE}
function TGLWindowsManager.XDisplay: Xlib.PDisplay;
begin
  { TODO: should be synched with XDisplayName property }
  Result := gdk_x11_get_default_xdisplay();
end;

function TGLWindowsManager.XScreen: Integer;
begin
  Result := gdk_x11_get_default_screen();
end;
{$endif}

{$endif read_implementation}