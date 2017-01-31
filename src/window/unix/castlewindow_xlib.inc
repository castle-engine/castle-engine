{
  Copyright 2001-2017 Michalis Kamburelis.

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
Xlib, CastleXlib, XUtil, X, KeySym, CursorFont,
{$endif}

{$ifdef read_window_interface}
private
  windowXID: TWindow;
  parentwinXID: TWindow; { refreshed on ReparentNotify, needed for X_Configureevent }

  {$ifdef CASTLE_ENGINE_PLUGIN}
  PluginWindow: TNPWindow;
  PluginStruct: TNPSetWindowCallbackStruct;
  {$endif}

  function HandleXEvent(const Event: TXEvent): boolean;
  procedure UpdateCursor;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  {$ifndef CASTLE_ENGINE_PLUGIN}
  FXDisplayName: string;
  {$endif}
  FXScreen: Integer;
  InitializeXDisplayDone: boolean;
  XCursors: array [TMouseCursor] of TCursor;
  FXDisplay: PDisplay;

  { uzywamy tego aby wylapac quit message. Inicjowane w SetXDisplayName }
  WMDeleteWindow: TAtom;
  { Arrays for fast conversion between X keysyms (cut off to least significant
    byte) to our TKey values. The most significant byte of X keysym determines
    which array to use. }
  KeySym0ToKey, KeySymFFToKey: TKeysBytes;
  { KeySymToKey; zamienia ksym na TKey, zwraca K_None jezeli
    nie dalo sie skonwertowac (bo moze ten keysym nie ma odpowiednika
    w naszym malym TKey ? a moze ksym byl = NoSymbol ?) }
  function KeySymToKey(ksym: TKeySym): TKey;
  { Seek on OpenWindows window with WindowXID = seekWindowXID.
    Returns nil if not found. }
  function FindWindowXID(seekWindowXID: TWindow): TCastleWindowCustom;
  {$ifndef CASTLE_ENGINE_PLUGIN}
  procedure SetXDisplayName(const Value: string);
  {$endif}

  { Check XDisplay. Call this once you're sure that XDisplay is set to
    value desired by user now.

    And since we know now that XDisplay is OK, we initialize some
    things: XScreen, check for glX extension, get WM_DELETE_WINDOW
    (we do it actually only if this was run for the 1st time).
    These things actually have be done only once, after XDisplay selection.
    But note that this can't be called in SetXDisplayName automatically.

    So you should call this when you know that last SetXDisplayName was
    called --- good choice is before initializing your window. }
  procedure InitializeXDisplay;

public
  function HandleXEvent(Event: TXEvent): boolean;

  {$ifndef CASTLE_ENGINE_PLUGIN}
  { XDisplayName used by all Window.Open calls.
    We assume that once we open some window, this will stay constant,
    so don't change it later!
    When this set, it's immediately processed by XDisplayName, so it may change by itself.
    By default this is XDisplayName(''). }
  property XDisplayName: string read FXDisplayName write SetXDisplayName;
  {$endif}
  property XDisplay: PDisplay read FXDisplay;
  property XScreen: Integer read FXScreen;
{$endif read_application_interface}

{$ifdef read_implementation}

const
  AcceptedEventsMask = ExposureMask or StructureNotifyMask or
    KeyPressMask or KeyReleaseMask {key events} or
    ButtonPressMask or ButtonReleaseMask or PointerMotionMask {mouse events} or
    FocusChangeMask;

{ TCastleWindowCustom --------------------------------------------------------  }

procedure TCastleWindowCustom.OpenBackend;
var
  AttrValueMask: Cardinal;
  Attr: TXSetWindowAttributes;
  {$ifndef CASTLE_ENGINE_PLUGIN}
  CorrectLeft, CorrectTop, CorrectWidth, CorrectHeight: Integer;
  sizeHints: TXSizeHints;
  WindTitleProp: TXTextProperty;
  ClassHint: TXClassHint;
  WMHints: TXWMHints;
  PWindTitle: PChar;
  {$endif}
const
  AllPointerEventsMask = ButtonPressMask or ButtonReleaseMask or
    EnterWindowMask or LeaveWindowMask or PointerMotionMask;
begin
  {$ifdef CASTLE_ENGINE_PLUGIN}
  Application.FXDisplay := PluginStruct.Display;

  FLeft := PluginWindow.X;
  FTop := PluginWindow.Y;
  FWidth := PluginWindow.Width;
  FHeight := PluginWindow.Height;
  Fullscreen := false;
  {$endif}

  Application.InitializeXDisplay;

  Attr.event_mask := AcceptedEventsMask;
  Attr.border_pixel := 0;
  AttrValueMask := CWBorderPixel or CWEventMask;

  parentwinXID := XRootWindow(Application.XDisplay, Application.XScreen);

  ContextCreateBegin(Attr, AttrValueMask);

  if fullscreen then
  begin
    Attr.override_redirect := XBool_true;
    AttrValueMask := AttrValueMask or CWOverrideRedirect;
    {$ifndef CASTLE_ENGINE_PLUGIN}
    CorrectLeft := 0;
    CorrectTop := 0;
    CorrectWidth := Application.ScreenWidth;
    CorrectHeight := Application.ScreenHeight;
    {$endif}
  end else
  begin
    {$ifndef CASTLE_ENGINE_PLUGIN}
    CorrectLeft := Left;
    CorrectTop := Top;
    CorrectWidth := Width;
    CorrectHeight := Height;
    {$endif}
  end;

  {$ifdef CASTLE_ENGINE_PLUGIN}
  windowXID := TWindow(PluginWindow.Window);
  { below is crucial, otherwise we'll not listed to events on windowXID }
  XChangeWindowAttributes(Application.XDisplay, windowXID, CWEventMask, @Attr);
  {$else}
  windowXID := XCreateWindow(Application.XDisplay, parentwinXID,
    CorrectLeft, CorrectTop, CorrectWidth, CorrectHeight, 0,
    {$ifndef OpenGLES} XVisual^.depth {$else} CopyFromParent {$endif},
    InputOutput,
    {$ifndef OpenGLES} XVisual^.visual {$else} PVisual(CopyFromParent) {$endif},
    AttrValueMask, @Attr);

  if FFullscreen and Visible then
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

    { W fullscreen nie realizujemy sizeHints (ktore zapewniaja realizacje
      funkcji ResizeAllowed <> raAllowed). ALE w fullscreen nigdy okno nie moze
      byc resized (wiec nie potrzeba nam sizeHints zeby to powiedziec...)
      wiec ok.

      Podobnie, w fullscreen nie realizujemy wmDeleteWindow ktore zapewnia
      realizacje uslugi OnCloseQuery. ALE w fullscreen user nie moze
      powiedziec window-managerowi ze chce zamknac to okno skoro window
      manager niemal nie wie o istnieniu naszego okna (grabbed klawisze,
      mysz, nasze okno bez ramki) wiec ok. }
    { TODO: kiedy juz zrobisz fullscreen bez Grabow i override_redirecta
      to chyba WM_DELETE_WINDOW pojdzie do fullscreen ?}
    { TODO: zrobic fullscreen bez Grabow (i ew. override_redirect) }
  end else
  begin
    { only set window title and handle wm_delete_events if in windowed mode }
    XSetWMProtocols(Application.XDisplay, windowXID, @Application.wmDeleteWindow, 1);

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
  {$endif}

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
end;

procedure TCastleWindowCustom.CloseBackend;
var
  dummy_event: TXEvent;
begin
  ContextDestroy;

  if FFullscreen and Visible then
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
    {$ifndef CASTLE_ENGINE_PLUGIN}
    XDestroyWindow(Application.XDisplay, windowXID);
    {$endif}
    windowXID := None
  end;

  {$ifndef OpenGLES}
  if colormap  <> None then begin XFreeColormap(Application.XDisplay, colormap);   colormap := None  end;
  if XVisual   <> nil  then begin XFree(XVisual);                                  XVisual := nil    end;
  {$endif}
end;

procedure TCastleWindowCustom.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      UpdateCursor;
  end;
end;

procedure TCastleWindowCustom.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  FCustomCursor := Value;
  { TODO }
end;

procedure TCastleWindowCustom.UpdateCursor;

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
  XCursorShapeFromMy: array [mcStandard .. High(TMouseCursor)] of LongWord =
  ( XC_left_ptr, XC_watch, XC_xterm, XC_hand2 );
begin
  { TODO: for now mcCustom is treated like mcDefault }
  if InternalCursor in [mcDefault, mcCustom] then
  begin
    XUndefineCursor(Application.XDisplay, WindowXID);
  end else
  begin
    if Application.XCursors[InternalCursor] = X.None then
    begin
      { initialize XCursors[InternalCursor] }
      if InternalCursor in [mcNone, mcForceNone] then
        Application.XCursors[InternalCursor] := CreateNoneCursor else
        Application.XCursors[InternalCursor] := XCreateFontCursor(Application.XDisplay,
          XCursorShapeFromMy[InternalCursor]);
    end;

    XDefineCursor(Application.XDisplay, WindowXID, Application.XCursors[InternalCursor]);
  end;
end;

procedure TCastleWindowCustom.SetMousePosition(const Value: TVector2Single);
begin
  if not Closed then
    XWarpPointer(Application.XDisplay, X.None, WindowXID,
      0, 0, 0, 0, Floor(Value[0]), Height - 1 - Floor(Value[1]));
end;

procedure TCastleWindowCustom.CreateBackend;
begin
  windowXID := None;
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
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

function TCastleWindowCustom.RedirectKeyDownToMenuClick: boolean;
begin
 Result := true;
end;

procedure TCastleWindowCustom.BackendMenuInitialize;
begin
  { does nothing }
end;

procedure TCastleWindowCustom.BackendMenuFinalize;
begin
  { does nothing }
end;

procedure TCastleWindowCustom.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { does nothing }
end;

procedure TCastleWindowCustom.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { does nothing }
end;

procedure TCastleWindowCustom.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { does nothing }
end;

function TCastleWindowCustom.MenuUpdateCheckedFast: boolean;
begin
  Result := true; // MenuUpdateChecked does nothing, so yes, it's fast :)
end;

procedure TCastleWindowCustom.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { does nothing }
end;

procedure TCastleWindowCustom.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { does nothing }
end;

function TCastleWindowCustom.HandleXEvent(const Event: TXEvent): boolean;

  procedure X_KeyPress(const event: TXKeyEvent);
  var
    k: TKeySym;
    CharKey: char;
    Key: TKey;
  begin
    { calculate k, CharKey }
    if XLookupString(@event, @CharKey, 1, @k, nil) = 0 then
      CharKey := #0;

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
        if (Key <> K_None) or (CharKey <> #0) then DoKeyDown(Key, CharKey);
      end;
    end;
  end;

  procedure X_KeyRelease(const event: TXKeyEvent);
  var
    k: TKeySym;
    CharKey: char;
    Key: TKey;
  begin
    { Note: "XLib Progr Manual" warns that KeyRelease is not guaranteed on
      "some very old systems". I didn't ever see such system...
      Anyway, in such case we can't make DoKeyUp, OnKeyUp, all Keys will remain
      pressed forever until window unfocused... }

    { calculate k, CharKey }
    XLookupString(@event, @CharKey, 1, @k, nil);

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
        if Key <> K_None then DoKeyUp(Key);
      end;
    end;
  end;

  procedure X_ConfigureNotify(const event: TXConfigureEvent);
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
    DoResize(event.width, event.height, false);
  end;

  function xbtnToMouseButton(button: Cardinal; var mbtn: TMouseButton): boolean;
  begin
    result := true;
    case button of
      Button1 : mbtn := mbLeft;
      Button2 : mbtn := mbMiddle;
      Button3 : mbtn := mbRight;
      else result := false;
    end;
  end;

  procedure X_ButtonPress(const event: TXButtonPressedEvent);
  var
    btn: TMouseButton;
  begin
    if xbtnToMouseButton(event.button, btn) then
      DoMouseDown(Vector2Single(Event.x, Height - 1 - Event.y), btn, 0) else
    case Event.Button of
      Button4: DoMouseWheel( 1, true);
      Button5: DoMouseWheel(-1, true);
      6: DoMouseWheel( 1, false); { not sure if this is possible }
      7: DoMouseWheel(-1, false); { not sure if this is possible }
    end;
  end;

  procedure X_ButtonRelease(const event: TXButtonReleasedEvent);
  var
    btn: TMouseButton;
  begin
    if xbtnToMouseButton(event.button, btn) then
      DoMouseUp(Vector2Single(Event.x, Height - 1 - Event.y), btn, 0)
  end;

  procedure X_MotionNotify(const event: TXMotionEvent);
  begin
    { We could refresh MousePressed now. But no need for now?
    mousePressed:=[];
    if (Button1Mask and event.state) <> 0 then Include(mousePressed, mbLeft);
    if (Button2Mask and event.state) <> 0 then Include(mousePressed, mbMiddle);
    if (Button3Mask and event.state) <> 0 then Include(mousePressed, mbRight);
    }
    DoMotion(InputMotion(MousePosition,
      Vector2Single(Event.x, Height - 1 - Event.y),
      MousePressed, 0));
  end;

  procedure X_FocusOut(const event: TXFocusOutEvent);
  begin
    { Clear KeysDown and MousePressed information.
      Otherwise, we could miss the keyup or mouseup event, if user pressed
      the key/mouse with our window in focus, then (holding the key/mouse)
      switched to other window. }
    ReleaseAllKeysAndMouse;
  end;

begin
  Result := true;
  case event._type of
    Expose: { XExposeEvent }
       if event.xexpose.count = 0 then Invalidate;
    (*MapNotify: needRedisplay := true; { XMapEvent } <- unused *)
    KeyPress: X_KeyPress(event.xkey);  { XKeyEvent }
    KeyRelease: X_KeyRelease(event.xkey);
    ButtonPress: X_ButtonPress(event.xbutton); { XButtonPressEvent }
    ButtonRelease: X_ButtonRelease(event.xbutton); { XButtonReleaseEvent }
    MotionNotify: X_MotionNotify(event.xmotion); { XPointerMovedEvent }
    ConfigureNotify: X_ConfigureNotify(event.xconfigure); { XConfigureEvent }
    ClientMessage : { XClientMessageEvent - no corresponding XXxxEvent mask }
      if (event.xclient.data.l[0] = Integer(Application.wmDeleteWindow)) then
       DoCloseQuery;
    ReparentNotify : parentwinXID := event.xreparent.parent;
    FocusOut: X_FocusOut(event.xfocus);
    else Result := false;
  end;
end;

procedure TCastleWindowCustom.SetFullScreen(const Value: boolean);
begin
  SimpleSetFullScreen(Value);
end;

{ TCastleApplication ------------------------------------------------------- }

function TCastleApplication.FindWindowXID(seekWindowXID: TWindow): TCastleWindowCustom;
var
  i: integer;
begin
  for i := 0 to OpenWindowsCount-1 do
    if OpenWindows[i].WindowXID = seekWindowXID then
      Exit(OpenWindows[i]);
  result := nil;
end;

{$ifndef CASTLE_ENGINE_PLUGIN}
procedure TCastleApplication.SetXDisplayName(const Value: string);
begin
  if XDisplay <> nil then XCloseDisplay(XDisplay);

  FXDisplayName := Xlib.XDisplayName(PCharOrNil(Value));

  FXDisplay := XOpenDisplay(PCharOrNil(Value));

  { Tests: Writeln('Setting XDisplayName to "', Value, '" success: ', FXDisplay <> nil); }

  { I used to check this, if XDisplay = nil then raise ECheckFailed....;
    but actually that's bad: at the beginning we call SetXDisplayName('')
    to set to default display, and that doesn't have to succeed since
    a default display may
    not be available (e.g. you run from a Linux console, or on Mac OS X
    not from X11 xterm) --- then you have to specify display name, the default
    display name '' will not be OK.

    So the check for XDisplay = nil will be done at window initialization. }
end;
{$endif}

function TCastleApplication.KeySymToKey(KSym: TKeySym): TKey;
begin
  result := K_None;
  if ksym <> NoSymbol then
    case ksym and $FF00 of
      { Note that KeySym*ToKey values may be K_None, and so we may return K_None. }
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
  if XDisplay = nil then
  begin
    {$ifdef CASTLE_ENGINE_PLUGIN}
    raise Exception.Create('Plugin caller did not set X display');
    {$else}
      {$ifdef DARWIN}
      Writeln(ErrOutput, Format('Opening the default X display "%s" failed, retrying with "%s" to try to attach to running X server on Mac OS X.',
        [XDisplayName, AltXDisplayName]));
      XDisplayName := AltXDisplayName;
      if XDisplay = nil then
      {$endif}
        raise Exception.CreateFmt('XOpenDisplay could not open display "%s"', [XDisplayName]);
    {$endif}
  end;

  if not InitializeXDisplayDone then
  begin
    // TODO: ok for plugin to just get XDefaultScreen?
    FXScreen := XDefaultScreen(XDisplay);
    ContextApplicationCheck;
    wmDeleteWindow := XInternAtom(XDisplay, 'WM_DELETE_WINDOW', XBool_True);
    InitializeXDisplayDone := true;
  end;
end;

procedure TCastleApplication.CreateBackend;
const
  { Under Kylix, capital letters are XK_?  and lower are XK_l?.
    Under FPC,           ...     are XKc_?    ...    are XK_?. }
  XK_a_lower = {$ifdef DELPHI} XK_la {$endif} {$ifdef FPC} XK_a {$endif};
  XK_z_lower = {$ifdef DELPHI} XK_lz {$endif} {$ifdef FPC} XK_z {$endif};
  XK_a_upper = {$ifdef DELPHI} XK_a {$endif} {$ifdef FPC} XKc_a {$endif};
  XK_z_upper = {$ifdef DELPHI} XK_z {$endif} {$ifdef FPC} XKc_z {$endif};
var
  b: byte;
begin
  { for a plugin, this is unknown until NPP_New }
  {$ifndef CASTLE_ENGINE_PLUGIN}
  SetXDisplayName(''); { zainicjuj XDisplayName }
  {$endif}

  { See ~/sources/fpc/trunk/packages/x11/src/keysym.pp }

  { initialize KeySym*ToKey }
  for b := Low(b) to High(b) do KeySym0ToKey[b] := K_None;
  for b := Low(b) to High(b) do KeySymFFToKey[b] := K_None;

  KeySymFFToKey[XK_Page_Up and $FF] := K_PageUp;
  KeySymFFToKey[XK_Page_Down and $FF] := K_PageDown;
  KeySymFFToKey[XK_Home and $FF] := K_Home;
  KeySymFFToKey[XK_End and $FF] := K_End;
  KeySymFFToKey[XK_Left and $FF] := K_Left;
  KeySymFFToKey[XK_Right and $FF] := K_Right;
  KeySymFFToKey[XK_Up and $FF] := K_Up;
  KeySymFFToKey[XK_Down and $FF] := K_Down;
  KeySymFFToKey[XK_Delete and $FF] := K_Delete;
  KeySymFFToKey[XK_Insert and $FF] := K_Insert;
  KeySymFFToKey[XK_BackSpace and $FF] := K_BackSpace;
  KeySymFFToKey[XK_Tab and $FF] := K_Tab;
  KeySymFFToKey[XK_KP_Add and $FF] := K_Numpad_Plus;
  KeySymFFToKey[XK_KP_Subtract and $FF] := K_Numpad_Minus;
  KeySymFFToKey[XK_Escape and $FF] := K_Escape;
  KeySymFFToKey[XK_Return and $FF] := K_Enter;
  KeySymFFToKey[XK_Print and $FF] := K_PrintScreen;
  KeySymFFToKey[XK_Caps_Lock and $FF] := K_CapsLock;
  KeySymFFToKey[XK_Scroll_Lock and $FF] := K_ScrollLock;
  KeySymFFToKey[XK_Num_Lock and $FF] := K_NumLock;
  KeySymFFToKey[XK_Pause and $FF] := K_Pause;

  KeySymFFToKey[XK_KP_0 and $FF] := K_Numpad_0;
  KeySymFFToKey[XK_KP_1 and $FF] := K_Numpad_1;
  KeySymFFToKey[XK_KP_2 and $FF] := K_Numpad_2;
  KeySymFFToKey[XK_KP_3 and $FF] := K_Numpad_3;
  KeySymFFToKey[XK_KP_4 and $FF] := K_Numpad_4;
  KeySymFFToKey[XK_KP_5 and $FF] := K_Numpad_5;
  KeySymFFToKey[XK_KP_6 and $FF] := K_Numpad_6;
  KeySymFFToKey[XK_KP_7 and $FF] := K_Numpad_7;
  KeySymFFToKey[XK_KP_8 and $FF] := K_Numpad_8;
  KeySymFFToKey[XK_KP_9 and $FF] := K_Numpad_9;
  KeySymFFToKey[XK_KP_End and $FF] := K_Numpad_End;
  KeySymFFToKey[XK_KP_Down and $FF] := K_Numpad_Down;
  KeySymFFToKey[XK_KP_Next and $FF] := K_Numpad_PageDown;
  KeySymFFToKey[XK_KP_Left and $FF] := K_Numpad_Left;
  KeySymFFToKey[XK_KP_Begin and $FF] := K_Numpad_Begin;
  KeySymFFToKey[XK_KP_Right and $FF] := K_Numpad_Right;
  KeySymFFToKey[XK_KP_Home and $FF] := K_Numpad_Home;
  KeySymFFToKey[XK_KP_Up and $FF] := K_Numpad_Up;
  KeySymFFToKey[XK_KP_Prior and $FF] := K_Numpad_PageUp;
  KeySymFFToKey[XK_KP_Insert and $FF] := K_Numpad_Insert;
  KeySymFFToKey[XK_KP_Delete and $FF] := K_Numpad_Delete;
  KeySymFFToKey[XK_KP_Enter and $FF] := K_Numpad_Enter;
  KeySymFFToKey[XK_KP_Multiply and $FF] := K_Numpad_Multiply;
  KeySymFFToKey[XK_KP_Divide and $FF] := K_Numpad_Divide;

  for b := 0 to 11 do KeySymFFToKey[(XK_F1+b) and $FF] := TKey(Ord(K_F1) + b);

  for b := XK_0 to XK_9 do KeySym0ToKey[b] := TKey(b);
  for b := XK_a_lower to XK_z_lower do KeySym0ToKey[b] := TKey(Ord(UpCase(Chr(b))));
  for b := XK_a_upper to XK_z_upper do KeySym0ToKey[b] := TKey(b);
  KeySym0ToKey[XK_Space] := K_Space;
  KeySym0ToKey[XK_comma] := K_comma;
  KeySym0ToKey[XK_period] := K_period;
  KeySym0ToKey[XK_bracketleft] := K_LeftBracket;
  KeySym0ToKey[XK_bracketright] := K_RightBracket;
  KeySym0ToKey[XK_apostrophe] := K_Apostrophe;
  KeySym0ToKey[XK_semicolon] := K_Semicolon;
  KeySym0ToKey[XK_slash] := K_Slash;
  KeySym0ToKey[XK_grave] := K_BackQuote;
  KeySym0ToKey[XK_minus] := K_Minus;
  KeySym0ToKey[XK_plus] := K_Plus;
  KeySym0ToKey[XK_equal] := K_Equal;
  KeySym0ToKey[XK_backslash] := K_BackSlash;
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
  Window: TCastleWindowCustom;
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

{ TCastleClipboard ----------------------------------------------------------- }

function TCastleClipboard.GetAsText: string;
begin
  { TODO }
  Result := '';
end;

procedure TCastleClipboard.SetAsText(const Value: string);
begin
  { TODO }
end;

{ TCastleWindow -------------------------------------------------------------- }

procedure TCastleWindow.NavigationInfoChanged;
begin
end;

{$endif read_implementation}
