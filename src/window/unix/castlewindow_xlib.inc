{%MainUnit ../castlewindow.pas}
{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleWindow backend using Xlib
  and glX (for desktop OpenGL) or EGL (for OpenGL ES). }

{$I castlewindow_dialogs_by_messages.inc}

{$ifdef OpenGLES}
  {$I castlewindow_egl.inc}
{$else}
  {$I castlewindow_glx.inc}
{$endif}

{$ifdef read_interface_uses}
Xlib, CastleInternalXlib, XUtil, X, KeySym, CursorFont,
{$endif}

{$ifdef read_window_interface}
private
  windowXID: TWindow;
  parentwinXID: TWindow; { refreshed on ReparentNotify, needed for X_Configureevent }
  {$ifndef OpenGLES}
  Colormap: TColormap;
  {$endif}

  function HandleXEvent(const Event: TXEvent): boolean;
  procedure UpdateCursor;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  FXDisplayName: string;
  FXScreen: Integer;
  InitializeXDisplayDone: boolean;
  XCursors: array [TMouseCursor] of TCursor;
  FXDisplay: PDisplay;

  { Used to capture "quit window" message. Initialized in InitializeXDisplay. }
  WMDeleteWindow: TAtom;
  { Arrays for fast conversion between X keysyms (cut off to least significant
    byte) to our TKey values. The most significant byte of X keysym determines
    which array to use. }
  KeySym0ToKey, KeySymFFToKey: TKeysBytes;
  { KeySymToKey; zamienia ksym na TKey, zwraca keyNone jezeli
    nie dalo sie skonwertowac (bo moze ten keysym nie ma odpowiednika
    w naszym malym TKey ? a moze ksym byl = NoSymbol ?) }
  function KeySymToKey(ksym: TKeySym): TKey;
  { Seek on OpenWindows window with WindowXID = seekWindowXID.
    Returns nil if not found. }
  function FindWindowXID(seekWindowXID: TWindow): TCastleWindow;

  { Initialize and check X display.
    Call this once you're sure that XDisplayName is set to
    a value desired by user (so, after processing --display command-line
    option), and only when you're sure you need to connect to the X server
    (to open a window, or query Application.ScreenWidth / ScreenHeight).

    This initializes and checks some things determined by the X display:

    @unorderedList(
      @item initializes XScreen,
      @item checks for glX extension,
      @item gets WM_DELETE_WINDOW.
    ) }
  procedure InitializeXDisplay;

public
  function HandleXEvent(Event: TXEvent): boolean;

  { X display name used to communicate with X server (to open windows,
    query screen size and so on). }
  property XDisplayName: string read FXDisplayName write FXDisplayName;
  property XDisplay: PDisplay read FXDisplay;
  property XScreen: Integer read FXScreen;
{$endif read_application_interface}

{$ifdef read_implementation}

const
  AcceptedEventsMask = ExposureMask or StructureNotifyMask or
    KeyPressMask or KeyReleaseMask {key events} or
    ButtonPressMask or ButtonReleaseMask or PointerMotionMask {mouse events} or
    FocusChangeMask;

{ TCastleWindow --------------------------------------------------------  }

procedure TCastleWindow.OpenBackend;

  procedure InitializeDpi;
  var
    DW, DWMM: Integer;
    NewDpi: Single;
  begin
    DW := XDisplayWidth(Application.XDisplay, Application.XScreen);
    DWMM := XDisplayWidthMM(Application.XDisplay, Application.XScreen);
    NewDpi := DW / (DWMM / 25.4);

    // Too verbose by default, UpdateUIScale will show it if UIScaling is usDpiScale.
    // WritelnLog('X11', 'Dots (pixels) per inch is %f (%f * default %f)',
    //   [NewDpi, NewDpi / DefaultDpi, DefaultDpi]);
    Container.Dpi := NewDpi;
  end;

var
  AttrValueMask: Cardinal;
  Attr: TXSetWindowAttributes;
  CorrectLeft, CorrectTop, CorrectWidth, CorrectHeight: Integer;
  sizeHints: TXSizeHints;
  WindTitleProp: TXTextProperty;
  ClassHint: TXClassHint;
  WMHints: TXWMHints;
  PWindTitle: PChar;
const
  AllPointerEventsMask = ButtonPressMask or ButtonReleaseMask or
    EnterWindowMask or LeaveWindowMask or PointerMotionMask;
begin
  Application.InitializeXDisplay;

  Attr.event_mask := AcceptedEventsMask;
  Attr.border_pixel := 0;
  AttrValueMask := CWBorderPixel or CWEventMask;

  parentwinXID := XRootWindow(Application.XDisplay, Application.XScreen);

  { Creates XVisual }
  ContextCreateBegin;

  {$ifndef OpenGLES}
  Colormap := XCreateColormap(Application.XDisplay, ParentWinXID,
    XVisual^.Visual, AllocNone);
  Attr.Colormap := Colormap;
  AttrValueMask := AttrValueMask or CWColormap;
  {$endif}

  if FullScreen then
  begin
    Attr.override_redirect := XBool_true;
    AttrValueMask := AttrValueMask or CWOverrideRedirect;
    CorrectLeft := 0;
    CorrectTop := 0;
    CorrectWidth := Application.ScreenWidth;
    CorrectHeight := Application.ScreenHeight;
  end else
  begin
    CorrectLeft := Left;
    CorrectTop := Top;
    CorrectWidth := Width;
    CorrectHeight := Height;
  end;

  windowXID := XCreateWindow(Application.XDisplay, parentwinXID,
    CorrectLeft, CorrectTop, CorrectWidth, CorrectHeight, 0,
    {$ifndef OpenGLES} XVisual^.depth {$else} CopyFromParent {$endif},
    InputOutput,
    {$ifndef OpenGLES} XVisual^.visual {$else} PVisual(CopyFromParent) {$endif},
    AttrValueMask, @Attr);

  if FullScreen and Visible then
  begin
    { XMapWindow must be called before doing all the "grab" operations.
      This also means that this whole piece of code cannot be done
      when Visible = @false. }
    XMapWindow(Application.XDisplay, windowXID);
    check( XGrabKeyboard(Application.XDisplay, windowXID, XBool_True, GrabModeAsync, GrabModeAsync,
       CurrentTime) = GrabSuccess, 'cannot grab the keyboard');
    check( XGrabPointer(Application.XDisplay, windowXID, XBool_True,
       AcceptedEventsMask and AllPointerEventsMask,
       GrabModeAsync, GrabModeAsync, windowXID, None, CurrentTime) = GrabSuccess,
       'cannot grab the pointer');

    { In FullScreen we don't use SizeHints.
      We don't need them anyway, as in fullscreen the window can never by resized.

      Also, in FullScreen we do not use WMDeleteWindow that allows to implement
      OnCloseQuery. But in FullScreen (with override_redirect) user cannot
      tell the window manager "I want to close this window",
      there are no window decorations and all keys (including Alt+F4) are grabbed.
      So we don't need WMDeleteWindow anyway.

      TODO:
      - Implement FullScreen without grabbing the keys and override_redirect.
      - When this is done, WMDeleteWindow should be used with fullscreen too. }
  end else
  begin
    { only set window title and handle wm_delete_events if in windowed mode }
    XSetWMProtocols(Application.XDisplay, windowXID, @Application.WMDeleteWindow, 1);

    { ready sizeHints structure }
    sizeHints.x := CorrectLeft;
    sizeHints.y := CorrectTop;
    sizeHints.width := CorrectWidth;
    sizeHints.height := CorrectHeight;
    sizeHints.flags := USSize or USPosition;
    if ResizeAllowed = raAllowed then
    begin
      sizeHints.min_width := minWidth;
      sizeHints.min_height := minHeight;
      sizeHints.max_width := maxWidth;
      sizeHints.max_height := maxHeight;
    end else
    begin
      sizeHints.min_width := CorrectWidth;
      sizeHints.min_height := CorrectHeight;
      sizeHints.max_width := CorrectWidth;
      sizeHints.max_height := CorrectHeight;
    end;
    sizeHints.flags := sizeHints.flags or PMinSize or PMaxSize;

    { ready WMHints, ClassHints }
    WMHints.flags := InputHint;
    WMHints.input := XBool_True;
    ClassHint.res_name := StrNew(PChar(ApplicationName));
    ClassHint.res_class := StrNew(PChar(ApplicationName));

    { ready WindTitle }
    PWindTitle := PChar(GetWholeCaption);
    Check( XStringListToTextProperty(@PWindTitle, 1, @WindTitleProp) <> 0, 'not enough memory for XStringListToTextProperty');

    { use prepared WindTitleProp, Size/WM/ClassHints to give a lot of
      hints to WindowManager (that will hopefully honour them...) }
    XSetWMProperties_Pascal(Application.XDisplay, windowXID, @WindTitleProp, @WindTitleProp,
       @sizeHints, @WMHints, @ClassHint);

    { realease allocated memory for structs for XSetWMProperties }
    XFree(WindTitleProp.value);
    StrDispose(ClassHint.res_name);
    StrDispose(ClassHint.res_class);

    if Visible then
      XMapWindow(Application.XDisplay, windowXID);
  end;

  ContextCreateEnd(windowXID);

  { applying FullScreen changes to the Left / Top / Width / Height }
  if FullScreen then
  begin
    FLeft := 0;
    FTop := 0;
    DoResize(Application.ScreenWidth, Application.ScreenHeight, false);
  end;

  UpdateCursor;

  Application.OpenWindowsAdd(Self);

  MakeCurrent;
  InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
var
  dummy_event: TXEvent;
begin
  ContextDestroyBegin;

  if FFullScreenBackend and Visible then
  begin
    XUnGrabPointer(Application.XDisplay, CurrentTime);
    XUnGrabKeyboard(Application.XDisplay, CurrentTime);
  end;

  { czyscimy kolejke komunikatow dla tego okienka.
    To nie daje nam absolutnej gwarancji ze do naszej kolejki nie trafia
    nigdy komunikaty juz zamknietych okienek bo przeciez messsage queue
    jest wypelniana w innym procesie wiec my mozemy wyczyscic a tu
    zaraz potem cos nam wpadnie. Wiec i tak w Application.ProcessMessage
    musimy sprawdzic czy dostalismy message dla istniejacego okienka.
    ALE ponizsze moze nam zaoszczedzic sporo czasu w ProcessMessage
    bo mimo wszystko zapewne zazwyczaj oczywsci kolejke z wiekszosci
    smieci dla tego okienka. }
  while XBool(XCheckWindowEvent(
    Application.XDisplay, windowXID, Longint($FFFFFFFF), @dummy_event)) <> XBool_False do ;

  if windowXID <> None then
  begin
    XDestroyWindow(Application.XDisplay, windowXID);
    windowXID := None
  end;

  {$ifndef OpenGLES}
  if Colormap  <> None then
  begin
    XFreeColormap(Application.XDisplay, Colormap);
    Colormap := None;
  end;
  {$endif}

  ContextDestroyEnd;
end;

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

  function CreateNoneCursor: TCursor;
  var
    bm_no: TPixmap;
    cmap: TColormap;
    black, dummy: TXColor;
  const
    bm_no_data: array[0..7] of Byte = (0, 0, 0, 0, 0, 0, 0, 0);
  begin
    { Based on [http://www.linuxforums.org/forum/linux-programming-scripting/59012-xlib-hide-mouse-pointer.html] }
    cmap := DefaultColormap(Application.XDisplay, DefaultScreen(Application.XDisplay));
    XAllocNamedColor(Application.XDisplay, cmap, 'black', @black, @dummy);
    try
      bm_no := XCreateBitmapFromData(Application.XDisplay, WindowXID, @bm_no_data, 8, 8);
      try
        Result := XCreatePixmapCursor(Application.XDisplay, bm_no, bm_no, @black, @black, 0, 0);
      finally
        if bm_no <> None then
          XFreePixmap(Application.XDisplay, bm_no);
      end;
    finally
      XFreeColors(Application.XDisplay, cmap, @black.pixel, 1, 0);
    end;
  end;

const
  XCursorShapeFromCastle: array [mcStandard .. High(TMouseCursor)] of LongWord =
  ( XC_left_ptr,             // mcStandard
    XC_watch,                // mcWait
    XC_xterm,                // mcText
    XC_hand2,                // mcHand
    XC_sb_v_double_arrow,    // mcResizeVertical
    XC_sb_h_double_arrow,    // mcResizeHorizontal
    XC_top_left_corner,      // mcResizeTopLeft
    XC_top_side,             // mcResizeTop
    XC_top_right_corner,     // mcResizeTopRight
    XC_left_side,            // mcResizeLeft
    XC_right_side,           // mcResizeRight
    XC_bottom_left_corner,   // mcResizeBottomLeft
    XC_bottom_side,          // mcResizeBottom
    XC_bottom_right_corner   // mcResizeBottomRight
  );
begin
  if InternalCursor = mcDefault then
  begin
    XUndefineCursor(Application.XDisplay, WindowXID);
  end else
  begin
    if Application.XCursors[InternalCursor] = X.None then
    begin
      { initialize XCursors[InternalCursor] }
      if InternalCursor in [mcNone, mcForceNone] then
        Application.XCursors[InternalCursor] := CreateNoneCursor
      else
        Application.XCursors[InternalCursor] := XCreateFontCursor(
          Application.XDisplay, XCursorShapeFromCastle[InternalCursor]);
    end;

    XDefineCursor(Application.XDisplay, WindowXID, Application.XCursors[InternalCursor]);
  end;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
var
  P: TVector2Integer;
begin
  if not Closed then
  begin

    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    P := CastleToLeftTopInt(Value);
    XWarpPointer(Application.XDisplay, X.None, WindowXID, 0, 0, 0, 0, P.X, P.Y);
  end;
end;

procedure TCastleWindow.CreateBackend;
begin
  windowXID := None;

  { We don't do here anything depending on X server,
    and thus this doesn't require calling Application.InitializeXDisplay.
    This way, merely calling TCastleWindow.Create
    doesn't require X server runnning. }
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
var
  PValue: PChar;
  WindTitleProp: TXTextProperty;
begin
  FCaption[Part] := Value;

  if windowXID <> None then
  begin
    { create, use, free WindTitleProp }
    PValue := PChar(GetWholeCaption);
    Check( XStringListToTextProperty(@PValue, 1, @WindTitleProp) <> 0,
      'not enough memory for XStringListToTextProperty');
    XSetTextProperty(Application.XDisplay, windowXID, @WindTitleProp, XInternAtom(Application.XDisplay, 'WM_NAME', XBool_True) );
    XFree(WindTitleProp.value);
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.BackendMenuInitialize;
begin
  { does nothing }
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  { does nothing }
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { does nothing }
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { does nothing }
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { does nothing }
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := true; // MenuUpdateChecked does nothing, so yes, it's fast :)
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { does nothing }
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { does nothing }
end;

function TCastleWindow.HandleXEvent(const Event: TXEvent): boolean;

  procedure X_KeyPress(const xEvent: TXKeyEvent);
  var
    k: TKeySym;
    CharKey: char;
    EventString: string;
    Key: TKey;
  begin
    { calculate k, CharKey }
    if XLookupString(@xEvent, @CharKey, 1, @k, nil) = 0 then
      EventString := ''
    else
      EventString := CharKey;

    { maybe do DoKeyDown }
    case k of
      XK_Shift_L:   SetPrivateModifiersDown(mkShift, false, true);
      XK_Shift_R:   SetPrivateModifiersDown(mkShift, true,  true);
      XK_Control_L: SetPrivateModifiersDown(mkCtrl,  false, true);
      XK_Control_R: SetPrivateModifiersDown(mkCtrl,  true,  true);
      XK_Alt_L:     SetPrivateModifiersDown(mkAlt,   false, true);
      XK_Alt_R:     SetPrivateModifiersDown(mkAlt,   true,  true);
      else begin
        Key := Application.KeySymToKey(k);
        if (Key <> keyNone) or (EventString <> '') then DoKeyDown(Key, EventString);
      end;
    end;
  end;

  procedure X_KeyRelease(const xEvent: TXKeyEvent);
  var
    k: TKeySym;
    CharKey: char; //we don't use it further anywhere
    Key: TKey;
  begin
    { Note: "XLib Progr Manual" warns that KeyRelease is not guaranteed on
      "some very old systems". I didn't ever see such system...
      Anyway, in such case we can't make DoKeyUp, OnKeyUp, all Keys will remain
      pressed forever until window unfocused... }

    { calculate k, CharKey }
    XLookupString(@xEvent, @CharKey, 1, @k, nil);

    { maybe do DoKeyUp }
    case k of
      XK_Shift_L:   SetPrivateModifiersDown(mkShift, false, false);
      XK_Shift_R:   SetPrivateModifiersDown(mkShift, true,  false);
      XK_Control_L: SetPrivateModifiersDown(mkCtrl,  false, false);
      XK_Control_R: SetPrivateModifiersDown(mkCtrl,  true,  false);
      XK_Alt_L:     SetPrivateModifiersDown(mkAlt,   false, false);
      XK_Alt_R:     SetPrivateModifiersDown(mkAlt,   true,  false);
      else begin
        Key := Application.KeySymToKey(k);
        if Key <> keyNone then DoKeyUp(Key);
      end;
    end;
  end;

  procedure X_ConfigureNotify(const xEvent: TXConfigureEvent);
  var
    dummy_win: TWindow;
  begin
    { event.x , event.y are relative to our parent.
      We always create our window giving XRootWindow(Application.XDisplay, Application.XScreen)
      as parent and we want our fLeft, fTop to be relitive to that
      window. BUT window managers may redirect our window and put
      it in some thick border, for example. Than this border is our parent
      and our position is given relative to that border. But we want it
      to be relative to XRootWindow(...) to be useful for future window
      creation !! So we translate coords from our parentwinXID to
      XRootWindow(...) }
    XTranslateCoordinates(Application.XDisplay,
      parentwinXID, XRootWindow(Application.XDisplay, Application.XScreen),
      {event.x, event.y,}0, 0, @fLeft, @fTop, @dummy_win);

    { TODO: for now, Left and Top indicate corner of the window *with frame*
      (while they should indicate corner of GL area only, according to interface). }
    DoResize(xEvent.width, xEvent.height, false);
  end;

  function xbtnToMouseButton(button: Cardinal; var mbtn: TCastleMouseButton): boolean;
  begin
    result := true;
    case button of
      Button1 : mbtn := buttonLeft;
      Button2 : mbtn := buttonMiddle;
      Button3 : mbtn := buttonRight;
      else Result := false;
    end;
  end;

  procedure X_ButtonPress(const xEvent: TXButtonPressedEvent);
  var
    btn: TCastleMouseButton;
  begin
    if xbtnToMouseButton(xEvent.button, btn) then
      DoMouseDown(LeftTopToCastle(xEvent.x, xEvent.y), btn, 0) else
    case xEvent.Button of
      Button4: DoMouseWheel( 1, true);
      Button5: DoMouseWheel(-1, true);
      6: DoMouseWheel( 1, false); { not sure if this is possible }
      7: DoMouseWheel(-1, false); { not sure if this is possible }
    end;
  end;

  procedure X_ButtonRelease(const xEvent: TXButtonReleasedEvent);
  var
    btn: TCastleMouseButton;
  begin
    if xbtnToMouseButton(xEvent.button, btn) then
      DoMouseUp(LeftTopToCastle(xEvent.x, xEvent.y), btn, 0)
  end;

  procedure X_MotionNotify(const xEvent: TXMotionEvent);
  begin
    { We could refresh MousePressed now. But no need for now?
    mousePressed:=[];
    if (Button1Mask and event.state) <> 0 then Include(mousePressed, buttonLeft);
    if (Button2Mask and event.state) <> 0 then Include(mousePressed, buttonMiddle);
    if (Button3Mask and event.state) <> 0 then Include(mousePressed, buttonRight);
    }
    DoMotion(InputMotion(MousePosition,
      LeftTopToCastle(xEvent.x, xEvent.y),
      MousePressed, 0));
  end;

  procedure X_FocusOut(const xEvent: TXFocusOutEvent);
  begin
    { Clear KeysDown and MousePressed information.
      Otherwise, we could miss the keyup or mouseup event, if user pressed
      the key/mouse with our window in focus, then (holding the key/mouse)
      switched to other window. }
    ReleaseAllKeysAndMouse;
  end;

begin
  Result := true;
  case Event._type of
    Expose: { XExposeEvent }
       if Event.xexpose.count = 0 then Invalidate;
    (*MapNotify: needRedisplay := true; { XMapEvent } <- unused *)
    KeyPress: X_KeyPress(Event.xkey);  { XKeyEvent }
    KeyRelease: X_KeyRelease(Event.xkey);
    ButtonPress: X_ButtonPress(Event.xbutton); { XButtonPressEvent }
    ButtonRelease: X_ButtonRelease(Event.xbutton); { XButtonReleaseEvent }
    MotionNotify: X_MotionNotify(Event.xmotion); { XPointerMovedEvent }
    ConfigureNotify: X_ConfigureNotify(Event.xconfigure); { XConfigureEvent }
    ClientMessage : { XClientMessageEvent - no corresponding XXxxEvent mask }
      if (event.xclient.data.l[0] = Integer(Application.WMDeleteWindow)) then
       DoCloseQuery;
    ReparentNotify : parentwinXID := Event.xreparent.parent;
    FocusOut: X_FocusOut(Event.xfocus);
    else Result := false;
  end;
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  SimpleUpdateFullScreenBackend;
end;

{ TCastleApplication ------------------------------------------------------- }

function TCastleApplication.FindWindowXID(seekWindowXID: TWindow): TCastleWindow;
var
  i: integer;
begin
  for i := 0 to OpenWindowsCount-1 do
    if OpenWindows[i].WindowXID = seekWindowXID then
      Exit(OpenWindows[i]);
  Result := nil;
end;

function TCastleApplication.KeySymToKey(KSym: TKeySym): TKey;
begin
  Result := keyNone;
  if ksym <> NoSymbol then
    case ksym and $FF00 of
      { Note that KeySym*ToKey values may be keyNone, and so we may return keyNone. }
      $0000: Exit(KeySym0ToKey [ksym and $FF]);
      $FF00: Exit(KeySymFFToKey[ksym and $FF]);
   end;
end;

procedure TCastleApplication.InitializeXDisplay;
{$ifdef DARWIN}
const
  AltXDisplayName = ':0';
{$endif}
begin
  if not InitializeXDisplayDone then
  begin
    { process FXDisplayName by Xlib.XDisplayName, to make it look nicer }
    FXDisplayName := Xlib.XDisplayName(PCharOrNil(FXDisplayName));
    FXDisplay := XOpenDisplay(PCharOrNil(FXDisplayName));

    if XDisplay = nil then
    begin
      {$ifdef DARWIN}
      Writeln(ErrOutput, Format('Opening the default X display "%s" failed, retrying with "%s" to try to attach to running X server on macOS.',
        [XDisplayName, AltXDisplayName]));

      FXDisplayName := Xlib.XDisplayName(PCharOrNil(AltXDisplayName));
      FXDisplay := XOpenDisplay(PCharOrNil(FXDisplayName));

      if XDisplay = nil then
      {$endif}
        raise Exception.CreateFmt('XOpenDisplay could not open display "%s"', [XDisplayName]);
    end;

    // TODO: Is it OK for a plugin to just get XDefaultScreen below?
    FXScreen := XDefaultScreen(XDisplay);
    ContextApplicationCheck;
    WMDeleteWindow := XInternAtom(XDisplay, 'WM_DELETE_WINDOW', XBool_True);
    InitializeXDisplayDone := true;
  end;
end;

procedure TCastleApplication.CreateBackend;
const
  { Under Kylix, capital letters are XK_?  and lower are XK_l?.
    Under FPC,           ...     are XKc_?    ...    are XK_?. }
  XK_a_lower = {$ifdef DCC} XK_la {$endif} {$ifdef FPC} XK_a {$endif};
  XK_z_lower = {$ifdef DCC} XK_lz {$endif} {$ifdef FPC} XK_z {$endif};
  XK_a_upper = {$ifdef DCC} XK_a {$endif} {$ifdef FPC} XKc_a {$endif};
  XK_z_upper = {$ifdef DCC} XK_z {$endif} {$ifdef FPC} XKc_z {$endif};
var
  b: byte;
begin
  { See ~/sources/fpc/trunk/packages/x11/src/keysym.pp }

  { initialize KeySym*ToKey }
  for b := Low(b) to High(b) do KeySym0ToKey[b] := keyNone;
  for b := Low(b) to High(b) do KeySymFFToKey[b] := keyNone;

  KeySymFFToKey[XK_Page_Up and $FF] := keyPageUp;
  KeySymFFToKey[XK_Page_Down and $FF] := keyPageDown;
  KeySymFFToKey[XK_Home and $FF] := keyHome;
  KeySymFFToKey[XK_End and $FF] := keyEnd;
  KeySymFFToKey[XK_Left and $FF] := keyArrowLeft;
  KeySymFFToKey[XK_Right and $FF] := keyArrowRight;
  KeySymFFToKey[XK_Up and $FF] := keyArrowUp;
  KeySymFFToKey[XK_Down and $FF] := keyArrowDown;
  KeySymFFToKey[XK_Delete and $FF] := keyDelete;
  KeySymFFToKey[XK_Insert and $FF] := keyInsert;
  KeySymFFToKey[XK_BackSpace and $FF] := keyBackSpace;
  KeySymFFToKey[XK_Tab and $FF] := keyTab;
  KeySymFFToKey[XK_KP_Add and $FF] := keyNumpadPlus;
  KeySymFFToKey[XK_KP_Subtract and $FF] := keyNumpadMinus;
  KeySymFFToKey[XK_Escape and $FF] := keyEscape;
  KeySymFFToKey[XK_Return and $FF] := keyEnter;
  KeySymFFToKey[XK_Print and $FF] := keyPrintScreen;
  KeySymFFToKey[XK_Caps_Lock and $FF] := keyCapsLock;
  KeySymFFToKey[XK_Scroll_Lock and $FF] := keyScrollLock;
  KeySymFFToKey[XK_Num_Lock and $FF] := keyNumLock;
  KeySymFFToKey[XK_Pause and $FF] := keyPause;

  KeySymFFToKey[XK_KP_0 and $FF] := keyNumpad0;
  KeySymFFToKey[XK_KP_1 and $FF] := keyNumpad1;
  KeySymFFToKey[XK_KP_2 and $FF] := keyNumpad2;
  KeySymFFToKey[XK_KP_3 and $FF] := keyNumpad3;
  KeySymFFToKey[XK_KP_4 and $FF] := keyNumpad4;
  KeySymFFToKey[XK_KP_5 and $FF] := keyNumpad5;
  KeySymFFToKey[XK_KP_6 and $FF] := keyNumpad6;
  KeySymFFToKey[XK_KP_7 and $FF] := keyNumpad7;
  KeySymFFToKey[XK_KP_8 and $FF] := keyNumpad8;
  KeySymFFToKey[XK_KP_9 and $FF] := keyNumpad9;
  KeySymFFToKey[XK_KP_End and $FF] := keyNumpadEnd;
  KeySymFFToKey[XK_KP_Down and $FF] := keyNumpadDown;
  KeySymFFToKey[XK_KP_Next and $FF] := keyNumpadPageDown;
  KeySymFFToKey[XK_KP_Left and $FF] := keyNumpadLeft;
  KeySymFFToKey[XK_KP_Begin and $FF] := keyNumpadBegin;
  KeySymFFToKey[XK_KP_Right and $FF] := keyNumpadRight;
  KeySymFFToKey[XK_KP_Home and $FF] := keyNumpadHome;
  KeySymFFToKey[XK_KP_Up and $FF] := keyNumpadUp;
  KeySymFFToKey[XK_KP_Prior and $FF] := keyNumpadPageUp;
  KeySymFFToKey[XK_KP_Insert and $FF] := keyNumpadInsert;
  KeySymFFToKey[XK_KP_Delete and $FF] := keyNumpadDelete;
  KeySymFFToKey[XK_KP_Enter and $FF] := keyNumpadEnter;
  KeySymFFToKey[XK_KP_Multiply and $FF] := keyNumpadMultiply;
  KeySymFFToKey[XK_KP_Divide and $FF] := keyNumpadDivide;

  for b := 0 to 11 do KeySymFFToKey[(XK_F1+b) and $FF] := TKey(Ord(keyF1) + b);

  for b := XK_0 to XK_9 do KeySym0ToKey[b] := TKey(b);
  for b := XK_a_lower to XK_z_lower do KeySym0ToKey[b] := TKey(Ord(UpCase(Chr(b))));
  for b := XK_a_upper to XK_z_upper do KeySym0ToKey[b] := TKey(b);
  KeySym0ToKey[XK_Space] := keySpace;
  KeySym0ToKey[XK_comma] := keyComma;
  KeySym0ToKey[XK_period] := keyPeriod;
  KeySym0ToKey[XK_bracketleft] := keyLeftBracket;
  KeySym0ToKey[XK_bracketright] := keyRightBracket;
  KeySym0ToKey[XK_apostrophe] := keyApostrophe;
  KeySym0ToKey[XK_semicolon] := keySemicolon;
  KeySym0ToKey[XK_slash] := keySlash;
  KeySym0ToKey[XK_grave] := keyBackQuote;
  KeySym0ToKey[XK_minus] := keyMinus;
  KeySym0ToKey[XK_plus] := keyPlus;
  KeySym0ToKey[XK_equal] := keyEqual;
  KeySym0ToKey[XK_backslash] := keyBackSlash;
end;

procedure TCastleApplication.DestroyBackend;
var
  C: TMouseCursor;
begin
  if XDisplay <> nil then
  begin
    for C := Low(C) to High(C) do
      if XCursors[C] <> X.None then
      begin
        XFreeCursor(XDisplay, XCursors[C]);
        XCursors[C] := X.None;
      end;

    XCloseDisplay(XDisplay);
  end;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Xlib+' + {$ifndef OpenGLES} 'glX' {$else} 'EGL' {$endif};
end;

function TCastleApplication.HandleXEvent(Event: TXEvent): boolean;
var
  Window: TCastleWindow;
begin
  Result := false;
  if event._type = MappingNotify then
  begin
    XRefreshKeyboardMapping(@event);
    Result := true;
  end else
  begin
    { Events for a particular window.
      We check is the window present on Application.OpenWindows,
      finding window instance based on windowXID }
    Window := Application.FindWindowXID(event.xany.window);
    if Window <> nil then
      Result := Window.HandleXEvent(event);
  end;
end;

{$ifndef CASTLE_WINDOW_USE_XF86VMODE}
function TCastleApplication.ScreenWidth: integer;
begin
  InitializeXDisplay;
  Result := XDisplayWidth(XDisplay, XScreen);
end;

function TCastleApplication.ScreenHeight: integer;
begin
  InitializeXDisplay;
  Result := XDisplayHeight(XDisplay, XScreen);
end;

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

{$endif}

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  Result := true;
end;

{$endif read_implementation}
