{
  Copyright 2001-2006 Michalis Kamburelis.

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

{$ifdef read_tglwindow_interface}
private
  glcontext: GLXContext;
  windowXID: TWindow;
  parentwinXID: TWindow; { refreshed on ReparentNotify, needed for X_Configureevent }
  viinfo: PXVisualInfo;
  colormap: TColormap;
  procedure X_KeyPress(const event: TXKeyEvent);
  procedure X_KeyRelease(const event: TXKeyEvent);
  procedure X_ConfigureNotify(const event: TXConfigureEvent);
  procedure X_ButtonPress(const event: TXButtonPressedEvent);
  procedure X_ButtonRelease(const event: TXButtonReleasedEvent);
  procedure X_MotionNotify(const event: TXMotionEvent);
  procedure X_FocusOut(const event: TXFocusOutEvent);

  procedure UpdateMouseVisible;
{$endif read_tglwindow_interface}

{$ifdef read_tglwindowmanager_interface}
private
  FDisplayName: string;
  Display: PDisplay;
  Screen: integer;
  { uzywamy tego aby wylapac quit message. Inicjowane w SetDisplayName }
  wmDeleteWindow: TAtom;
  { tablice przejscia z KeySym okrojonego do mniej znaczacego bajtu a TKey
   (moje stale K_x). Ktorej tablicy (0 czy FF) uzyc do konwersji - zalezy
   od bardziej znaczacego bajtu. Tablice sa konstruowane w CreateImplDepend,
   powinienes z nich korzystac tylko przez KeySymToKlawisz. }
  keySym_to_TKey0,
  keySym_to_TKeyFF: TKeysBytes;
  { KeySymToKey; zamienia ksym na TKey, zwraca K_None jezeli
    nie dalo sie skonwertowac (bo moze ten keysym nie ma odpowiednika
    w naszym malym TKey ? a moze ksym byl = NoSymbol ?) }
  function KeySymToKey(ksym: TKeySym): TKey;
  { Seek on Active[] window with WindowXID = seekWindowXID.
    Returns nil if not found. }
  function ActiveWindowXID(seekWindowXID: TWindow): TGLWindow;
  procedure SetDisplayName(const Value: string);

  {$ifdef GLWINDOW_HAS_VIDEO_CHANGE}
  VideoModeChanged: boolean;
  { *ModeInfos are valid only if VideoModeChanged }
  CurrentModeInfo, OriginalModeInfo: TXF86VidModeModeInfo;
  {$endif}
public
  { jezeli UseVirtualXScreen to pod X-ami zmiana rozmiarow Video przez
    VideoChange nie spowoduje zmiany ScreenWidth/H i bedziemy miec
    okno np. 800x600 na ekranie 640x480. To jest wlasnie virtual screen
    X-ow. W sumie moze to byc fajna rzecz dla jakichs programow uzytkowych ktore
    pozwalaja userowi na operowanie na ekranie jakimis kontrolkami
    ale nie dla typowych pelnoekranowych gier/dem : dla nas zmiana
    rozmiaru ekranu ma na celu zmniejszenie/zwiekszenie dokladnosci
    renderowanego obrazu. Wiec domyslnie UseVirtualXScreen = false. }
  UseVirtualXScreen: boolean; { = false }

  { DisplayName uzywane przez wszystkie nastepne wywolania glwin.Init.
    glwm nie jest przygotowany na zarzadzanie oknami na wielu displayach i
    implementacja TGLWindow zaklada ze po zrobieniu Init glwm.Display bedzie
    caly czas takie samo - wiec NIE ZMIENIAJ DisplayName po zrobieniu jakiegos Init !
    DisplayName bezposrednio po przypisaniu moze ulec zmianie - bo
    SetDisplayName powoduje cos jak FDisplayName := XDisplayName(FDiaplayName),
    a wiec X serwer moze cos od razu powiedziec o tym jak on rozumie ta
    DisplayName.

    Default = '', zinterpretowane przez X serwer. }
  property DisplayName: string read FDisplayName write SetDisplayName;

  property DisplayPtr: PDisplay read Display;
{$endif read_tglwindowmanager_interface}

{$ifdef read_implementation}

{ Czyli implementacja GLWindow za pomoca XWindows poprzez
  Xlib + glX.

  Punktem wyjscia byl znacznie przerobiony, rozszerzony, poprawiony prosty
  programik testgl jaki dostalem razem z modulem opengl12 pod Kylixa.
  Byl tam prosciutki schemat "openGL poprzez glX" i na nim sie opieralem
  na poczatku. Potem znaczna pomoca okazal sie glxdino w "OpenGL and X"
  by Mark Kilgard i Lesson1 z Nehe w wersji glX.
  No a wreszcie mam wszystko z "Xlib Programming Manual".

  Note about potential problem: my code depends on the fact that
  Xlib informs me about key releases with KeyRelease. However
  XLibProgrManual states that this is not guaranteed on "some
  very old systems". In practice I haven't seen such systems
  and I am not sure whether such systems even exist in nowadays
  so I wrote my code assuming that I can depend on KeyRelease.
}

const
  AcceptedEventsMask = ExposureMask or StructureNotifyMask or
    KeyPressMask or KeyReleaseMask {key events} or
    ButtonPressMask or ButtonReleaseMask or PointerMotionMask {mouse events} or
    FocusChangeMask;

{ TGLWindow --------------------------------------------------------  }

procedure TGLWindow.InitWinSystemDepend;
var sizeHints: TXSizeHints;
    AttrValueMask: Cardinal;
    attr: TXSetWindowAttributes;
    WindTitleProp: TXTextProperty;
    ClassHint: TXClassHint;
    WMHints: TXWMHints;
    PWindTitle: PChar;
    VisualAttr: TDynLongIntArray;
const
  AllPointerEventsMask = ButtonPressMask or ButtonReleaseMask or
    EnterWindowMask or LeaveWindowMask or PointerMotionMask;
begin
 VisualAttr := TDynLongIntArray.Create;
 try
  VisualAttr.AllowedCapacityOverflow := 20;
  if DoubleBuffer then VisualAttr.AppendItem(GLX_DOUBLEBUFFER);
  VisualAttr.AppendArray([
    GLX_RGBA,
    { Ponizsze rozmiary buforow sa zawsze w bitach }
    GLX_DEPTH_SIZE,
      {$ifdef WORKAROUND_DEPTH_BITS_0_BUG} Max(DepthBufferBits, 1)
      {$else} DepthBufferBits
      {$endif},
    GLX_STENCIL_SIZE, StencilBufferBits,
    GLX_ALPHA_SIZE, AlphaBits,
    GLX_ACCUM_RED_SIZE, AccumBufferBits[0],
    GLX_ACCUM_GREEN_SIZE, AccumBufferBits[1],
    GLX_ACCUM_BLUE_SIZE, AccumBufferBits[2],
    GLX_ACCUM_ALPHA_SIZE, AccumBufferBits[3],
    None ]);
  viinfo := glXChooseVisual(glwm.display, glwm.screen, @VisualAttr.Items[0]);
 finally FreeAndNil(VisualAttr) end;

 if viinfo = nil then
  raise EGLContextNotPossible.CreateFmt(
    'glXChooseVisual: visual with requested attributes (%s) not found',
    [ RequestedBufferAttributes ]);
 {zawsze w tym miejscu bedzie Assert(glwm.screen = viinfo.screen);}

 parentwinXID := XRootWindow(glwm.display, glwm.screen);

 colormap := XCreateColormap(glwm.display, parentwinXID,
                             viinfo.visual, AllocNone);
  { bardziej speszial metoda brania Colormap jest w glxdino }
 attr.colormap := colormap;
 attr.border_pixel := 0;
 attr.event_mask := AcceptedEventsMask;
 AttrValueMask := CWBorderPixel or CWColormap or CWEventMask;

 if fullscreen then
 begin
  attr.override_redirect := XBool_true;
  AttrValueMask := AttrValueMask or CWOverrideRedirect;
 end;

 windowXID := XCreateWindow(glwm.display, parentwinXID,
   left, top, width, height,
   0, viinfo.depth, InputOutput, viinfo.visual,
   AttrValueMask, @attr);
 if FFullscreen then
 begin
  XMapWindow(glwm.display, windowXID); { MapWindow musi byc przed Grabami ponizej }
  check( XGrabKeyboard(glwm.display, windowXID, XBool_True, GrabModeAsync, GrabModeAsync,
     CurrentTime) = GrabSuccess, 'cannot grab the keyboard');
  check( XGrabPointer(glwm.display, windowXID, XBool_True,
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
  XSetWMProtocols(glwm.display, windowXID, @glwm.wmDeleteWindow, 1);

  { ready sizeHints structure }
  sizeHints.x := left;
  sizeHints.y := top;
  sizeHints.width := width;
  sizeHints.height := height;
  sizeHints.flags := USSize or USPosition;
  if ResizeAllowed = raAllowed then
  begin
   sizeHints.min_width := minWidth;
   sizeHints.min_height := minHeight;
   sizeHints.max_width := maxWidth;
   sizeHints.max_height := maxHeight;
  end else
  begin
   sizeHints.min_width := width;
   sizeHints.min_height := height;
   sizeHints.max_width := width;
   sizeHints.max_height := height;
  end;
  sizeHints.flags := sizeHints.flags or PMinSize or PMaxSize;

  { ready WMHints, ClassHints }
  WMHints.flags := InputHint;
  WMHints.input := XBool_True;
  ClassHint.res_name := StrNew(PChar(ProgramName));
  ClassHint.res_class := StrNew(PChar(ProgramName));

  { ready WindTitle }
  PWindTitle := PChar(FCaption);
  Check( XStringListToTextProperty(@PWindTitle, 1, @WindTitleProp) <> 0, 'not enough memory for XStringListToTextProperty');

  { use prepared WindTitleProp, Size/WM/ClassHints to give a lot of
    hints to WindowManager (that will hopefully honour them...) }
  XSetWMProperties_Pascal(glwm.display, windowXID, @WindTitleProp, @WindTitleProp,
     @sizeHints, @WMHints, @ClassHint);

  { realease allocated memory for structs for XSetWMProperties }
  XFree(WindTitleProp.value);
  StrDispose(ClassHint.res_name);
  StrDispose(ClassHint.res_class);

  XMapWindow(glwm.display, windowXID);
 end;
 glcontext := glXCreateContext(glwm.display, viinfo, nil, true);
 check( glcontext <> nil, 'could not create rendering context');

 UpdateMouseVisible;

 glwm.ActiveAdd(Self);

 MakeCurrent;
end;

procedure TGLWindow.CloseImplDepend;
var dummy_event: TXEvent;
begin
 if glcontext <> nil then
 begin
  glXDestroyContext(glwm.display, glcontext);
  glcontext := nil;
 end;

 if FFullscreen then
 begin
  XUnGrabPointer(glwm.display, CurrentTime);
  XUnGrabKeyboard(glwm.display, CurrentTime);
 end;

 { czyscimy kolejke komunikatow dla tego okienka.
   To nie daje nam absolutnej gwarancji ze do naszej kolejki nie trafia
   nigdy komunikaty juz zamknietych okienek bo przeciez messsage queue
   jest wypelniana w innym procesie wiec my mozemy wyczyscic a tu
   zaraz potem cos nam wpadnie. Wiec i tak w glwm.ProcessMessage
   musimy sprawdzic czy dostalismy message dla istniejacego okienka.
   ALE ponizsze moze nam zaoszczedzic sporo czasu w ProcessMessage
   bo mimo wszystko zapewne zazwyczaj oczywsci kolejke z wiekszosci
   smieci dla tego okienka. }
 while XBool(XCheckWindowEvent(glwm.display, windowXID, Longint($FFFFFFFF), @dummy_event)) do ;

 if windowXID <> None then begin XDestroyWindow(glwm.display, windowXID); windowXID := None end;
 if colormap  <> None then begin XFreeColormap(glwm.display, colormap);   colormap := None  end;
 if viinfo    <> nil  then begin XFree(viinfo);                           viinfo := nil     end;
end;

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
var
  bm_no: TPixmap;
  cmap: TColormap;
  no_ptr: TCursor;
  black, dummy: TXColor;
const
  bm_no_data: array[0..7] of Byte = (0, 0, 0, 0, 0, 0, 0, 0);
begin
  if MouseVisible then
  begin
    XUndefineCursor(Glwm.DisplayPtr, WindowXID);
  end else
  begin
    { Based on [http://www.linuxforums.org/forum/linux-programming-scripting/59012-xlib-hide-mouse-pointer.html] }
    cmap := DefaultColormap(Glwm.DisplayPtr, DefaultScreen(Glwm.DisplayPtr));
    XAllocNamedColor(Glwm.DisplayPtr, cmap, 'black', @black, @dummy);
    try
      bm_no := XCreateBitmapFromData(Glwm.DisplayPtr, WindowXID, @bm_no_data, 8, 8);
      try
        no_ptr := XCreatePixmapCursor(Glwm.DisplayPtr, bm_no, bm_no, @black, @black, 0, 0);
        try
          XDefineCursor(Glwm.DisplayPtr, WindowXID, no_ptr);
        finally
          XFreeCursor(Glwm.DisplayPtr, no_ptr);
        end;
      finally
        if bm_no <> None then
          XFreePixmap(Glwm.DisplayPtr, bm_no);
      end;
    finally
      XFreeColors(Glwm.DisplayPtr, cmap, @black.pixel, 1, 0);
    end;
  end;
end;

procedure TGLWindow.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  Assert(not Closed);
  XWarpPointer(Glwm.DisplayPtr, X.None, WindowXID,
    0, 0, 0, 0, NewMouseX, NewMouseY);
end;

procedure TGLWindow.MakeCurrent;
begin
  Assert(not Closed);
  Check( glXMakeCurrent(glwm.display, windowXID, glcontext), 'glXMakeCurrent');
end;

procedure TGLWindow.CreateImplDepend;
begin
 windowXID := None;
end;

procedure TGLWindow.SetCaption(const Value: string);
var PValue: PChar;
    WindTitleProp: TXTextProperty;
begin
 fCaption := Value;

 if windowXID <> None then
 begin
  {skonstruuj WindTitleProp}
  PValue := PChar(Value);
  Check( XStringListToTextProperty(@PValue, 1, @WindTitleProp) <> 0,
    'not enough memory for XStringListToTextProperty');

  {uzyj WindTitleProp}
  XSetTextProperty(glwm.display, windowXID, @WindTitleProp, XInternAtom(glwm.display, 'WM_NAME', XBool_True) );

  {zwolnij WindTitleProp}
  XFree(WindTitleProp.value);
 end;
end;

function TGLWindow.RedirectKeyDownToMenuCommand: boolean;
begin
 Result := true;
end;

procedure TGLWindow.MainMenuChangedImplDepend;
begin
 { no operation }
end;

procedure TGLWindow.SwapBuffers;
begin
 glXSwapBuffers(glwm.display, windowXID);
end;

procedure TGLWindow.X_KeyPress(const event: TXKeyEvent);
var k: TKeySym;
    CharKey: char;
    Key: TKey;
begin
 { evaluate k, CharKey }
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
   Key := glwm.KeySymToKey(k);
   if (Key <> K_None) or (CharKey <> #0) then DoKeyDown(Key, CharKey);
  end;
 end;
end;

procedure TGLWindow.X_KeyRelease(const event: TXKeyEvent);
var k: TKeySym;
    CharKey: char;
    Key: TKey;
begin
 { evaluate k, CharKey }
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
   Key := glwm.KeySymToKey(k);
   if Key <> K_None then DoKeyUp(Key);
  end;
 end;
end;

procedure TGLWindow.X_ConfigureNotify(const event: TXConfigureEvent);
var dummy_win: TWindow;
begin
 { event.x , event.y are realtive to out parent.
   We always create our window giving XRootWindow(glwm.display, glwm.screen)
   as parent and we want our fLeft, fTop to be relitive to that
   window. BUT window managers may redirect our window and put
   it in some thick border, for example. Than this border is our parent
   and our position is given relative to that border. But we want it
   to be relative to XRootWindow(...) to be useful for future window
   creation !! So we translate coords from our parentwinXID to
   XRootWindow(...) }
 XTranslateCoordinates(glwm.display,
   parentwinXID, XRootWindow(glwm.display, glwm.screen),
   {event.x, event.y,}0, 0, @fLeft, @fTop, @dummy_win);

   { TODO: chwilowo, po prostu left i top pod GLWINDOW_XLIB oznaczaja rog okienka
     RAZEM z ramka, wbrew temu co napisalem w interfejsie... }
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

procedure TGLWindow.X_ButtonPress(const event: TXButtonPressedEvent);
var btn: TMouseButton;
begin
 if xbtnToMouseButton(event.button, btn) then
  DoMouseDown(event.x, event.y, btn)
end;

procedure TGLWindow.X_ButtonRelease(const event: TXButtonReleasedEvent);
var btn: TMouseButton;
begin
 if xbtnToMouseButton(event.button, btn) then
  DoMouseUp(event.x, event.y, btn)
end;

procedure TGLWindow.X_MotionNotify(const event: TXMotionEvent);
begin
{ moglibysmy tu odswiezyc mousePressed, ale po co ?
 mousePressed:=[];
 if (Button1Mask and event.state) <> 0 then Include(mousePressed, mbLeft);
 if (Button2Mask and event.state) <> 0 then Include(mousePressed, mbMiddle);
 if (Button3Mask and event.state) <> 0 then Include(mousePressed, mbRight);
}
 DoMouseMove(event.x, event.y);
end;

procedure TGLWindow.X_FocusOut(const event: TXFocusOutEvent);
begin
 { wyczysc tablice KeysDown i MousePressed.
   (bo jezeli user przycisnal klawisz w naszym okienku,
   a puscil go gdy inne okienko ma focus to nigdy nie dowiemy sie o zmianie
   i bedziemy mieli nieprawidlowo klawisz ciagle zaznaczony jako wcisniety.
   Lepiej zaznaczac sobie wszystkie na puszczone w momencie gdy tracimy
   keyboard focus.) }
 ReleaseAllKeysAndMouse;
end;

function TGLWindow.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean): boolean;
begin
 Result := MessageInputQuery(Self, Title, FileName, taLeft);
end;

function TGLWindow.ColorDialog(var Color: TVector3Single): boolean;
var NewColor: TVector3Single;
begin
 Result := false;
 if not MessageInputQuerySingle(Self, 'Input Red value:'  , NewColor[0], taLeft) then Exit;
 if not MessageInputQuerySingle(Self, 'Input Green value:', NewColor[1], taLeft) then Exit;
 if not MessageInputQuerySingle(Self, 'Input Blue value:' , NewColor[2], taLeft) then Exit;

 Result := true;
 Color := NewColor;
end;

{ TGLWindowsManager ------------------------------------------------------- }

function TGLWindowsManager.ActiveWindowXID(seekWindowXID: TWindow): TGLWindow;
var i: integer;
begin
 for i := 0 to ActiveCount-1 do
  if Active[i].WindowXID = seekWindowXID then
   Exit(Active[i]);
 result := nil;
end;

procedure TGLWindowsManager.SetDisplayName(const Value: string);
begin
 if Display <> nil then XCloseDisplay(Display);

 FDisplayName := XDisplayName(PCharOrNil(Value));

 Display := XOpenDisplay(PCharOrNil(Value));
 if display = nil then
  raise ECheckFailed.Create('XOpenDisplay failed - could not open display '+DisplayName);
 screen := XDefaultScreen(display);

 Check( glXQueryExtension(display, nil, nil),
   'X server has no glX extension (needed to run OpenGL-based programs)');
 wmDeleteWindow := XInternAtom(display, 'WM_DELETE_WINDOW', XBool_True);
end;

function TGLWindowsManager.KeySymToKey(ksym: TKeySym): TKey;
begin
 result := K_None;
 if ksym <> NoSymbol then
  case ksym and $FF00 of
   {wartosci w tablicach keySym_To_TKey* tez moga byc rowne K_None, wiec
    ponizsze exit'y moga ciagle zwracac K_None.}
   $0000: exit(keySym_To_TKey0[ksym and $FF]);
   $FF00: exit(keySym_To_TKeyFF[ksym and $FF]);
  end;
end;

procedure TGLWindowsManager.CreateImplDependent;
const
  { Under Kylix, capital letters are XK_?  and lower are XK_l?.
    Under FPC,           ...     are XKc_?    ...    are XK_?. }
  XK_a_lower = {$ifdef DELPHI} XK_la {$endif} {$ifdef FPC} XK_a {$endif};
  XK_z_lower = {$ifdef DELPHI} XK_lz {$endif} {$ifdef FPC} XK_z {$endif};
  XK_a_upper = {$ifdef DELPHI} XK_a {$endif} {$ifdef FPC} XKc_a {$endif};
  XK_z_upper = {$ifdef DELPHI} XK_z {$endif} {$ifdef FPC} XKc_z {$endif};

var b: byte;
begin
  SetDisplayName(''); { zainicjuj DisplayName }

  { See ~/sources/fpc/latest/trunk/packages/extra/x11/keysym.pp }

  { konstruuje tablice keySym_To_TKey0 i FF : }
  for b := Low(b) to High(b) do keySym_to_TKey0[b] := K_None;
  for b := Low(b) to High(b) do keySym_to_TKeyFF[b] := K_None;

  keySym_to_TKeyFF[XK_Page_Up and $FF] := K_PageUp;
  keySym_to_TKeyFF[XK_Page_Down and $FF] := K_PageDown;
  keySym_to_TKeyFF[XK_Home and $FF] := K_Home;
  keySym_to_TKeyFF[XK_End and $FF] := K_End;
  keySym_to_TKeyFF[XK_Left and $FF] := K_Left;
  keySym_to_TKeyFF[XK_Right and $FF] := K_Right;
  keySym_to_TKeyFF[XK_Up and $FF] := K_Up;
  keySym_to_TKeyFF[XK_Down and $FF] := K_Down;
  keySym_to_TKeyFF[XK_Delete and $FF] := K_Delete;
  keySym_to_TKeyFF[XK_Insert and $FF] := K_Insert;
  keySym_to_TKeyFF[XK_BackSpace and $FF] := K_BackSpace;
  keySym_to_TKeyFF[XK_Tab and $FF] := K_Tab;
  keySym_to_TKeyFF[XK_KP_Add and $FF] := K_Numpad_Plus;
  keySym_to_TKeyFF[XK_KP_Subtract and $FF] := K_Numpad_Minus;
  keySym_to_TKeyFF[XK_Escape and $FF] := K_Escape;
  keySym_to_TKeyFF[XK_Return and $FF] := K_Enter;
  keySym_to_TKeyFF[XK_Print and $FF] := K_PrintScreen;
  keySym_to_TKeyFF[XK_Caps_Lock and $FF] := K_CapsLock;
  keySym_to_TKeyFF[XK_Scroll_Lock and $FF] := K_ScrollLock;
  keySym_to_TKeyFF[XK_Num_Lock and $FF] := K_NumLock;
  keySym_to_TKeyFF[XK_Pause and $FF] := K_Pause;

  KeySym_To_TKeyFF[XK_KP_0 and $FF] := K_Numpad_0;
  KeySym_To_TKeyFF[XK_KP_1 and $FF] := K_Numpad_1;
  KeySym_To_TKeyFF[XK_KP_2 and $FF] := K_Numpad_2;
  KeySym_To_TKeyFF[XK_KP_3 and $FF] := K_Numpad_3;
  KeySym_To_TKeyFF[XK_KP_4 and $FF] := K_Numpad_4;
  KeySym_To_TKeyFF[XK_KP_5 and $FF] := K_Numpad_5;
  KeySym_To_TKeyFF[XK_KP_6 and $FF] := K_Numpad_6;
  KeySym_To_TKeyFF[XK_KP_7 and $FF] := K_Numpad_7;
  KeySym_To_TKeyFF[XK_KP_8 and $FF] := K_Numpad_8;
  KeySym_To_TKeyFF[XK_KP_9 and $FF] := K_Numpad_9;
  KeySym_To_TKeyFF[XK_KP_End and $FF] := K_Numpad_End;
  KeySym_To_TKeyFF[XK_KP_Down and $FF] := K_Numpad_Down;
  KeySym_To_TKeyFF[XK_KP_Next and $FF] := K_Numpad_PageDown;
  KeySym_To_TKeyFF[XK_KP_Left and $FF] := K_Numpad_Left;
  KeySym_To_TKeyFF[XK_KP_Begin and $FF] := K_Numpad_Begin;
  KeySym_To_TKeyFF[XK_KP_Right and $FF] := K_Numpad_Right;
  KeySym_To_TKeyFF[XK_KP_Home and $FF] := K_Numpad_Home;
  KeySym_To_TKeyFF[XK_KP_Up and $FF] := K_Numpad_Up;
  KeySym_To_TKeyFF[XK_KP_Prior and $FF] := K_Numpad_PageUp;
  KeySym_To_TKeyFF[XK_KP_Insert and $FF] := K_Numpad_Insert;
  KeySym_To_TKeyFF[XK_KP_Delete and $FF] := K_Numpad_Delete;
  KeySym_To_TKeyFF[XK_KP_Enter and $FF] := K_Numpad_Enter;
  KeySym_To_TKeyFF[XK_KP_Multiply and $FF] := K_Numpad_Multiply;
  KeySym_To_TKeyFF[XK_KP_Divide and $FF] := K_Numpad_Divide;

  for b := 0 to 11 do keySym_to_TKeyFF[(XK_F1+b) and $FF] := K_F1+b;

  for b := XK_0 to XK_9 do keySym_to_TKey0[b] := ord(b);
  for b := XK_a_lower to XK_z_lower do keySym_to_TKey0[b] := ord(UpCase(chr(b)));
  for b := XK_a_upper to XK_z_upper do keySym_to_TKey0[b] := b;
  keySym_to_TKey0[XK_Space] := K_Space;
  keySym_to_TKey0[XK_comma] := K_comma;
  keySym_to_TKey0[XK_period] := K_period;
  keySym_to_TKey0[XK_bracketleft] := K_LeftBracket;
  keySym_to_TKey0[XK_bracketright] := K_RightBracket;
  KeySym_To_TKey0[XK_apostrophe] := K_Apostrophe;
  KeySym_To_TKey0[XK_semicolon] := K_Semicolon;
  KeySym_To_TKey0[XK_slash] := K_Slash;
  KeySym_To_TKey0[XK_grave] := K_BackQuote;
  KeySym_To_TKey0[XK_minus] := K_Minus;
  KeySym_To_TKey0[XK_equal] := K_Equal;
  KeySym_To_TKey0[XK_backslash] := K_BackSlash;
end;

procedure TGLWindowsManager.DestroyImplDependent;
begin
 if Display <> nil then XCloseDisplay(Display);
end;

function TGLWindowsManager.ScreenWidth: integer;
begin
 { jezeli VideoModeChanged to nie chcemy uzywac rozmiarow screenu (bo wtedy
   po utworzeniu okienka fullscreen mielibysmy okno o wymiarach np. 800x600
   na ekranie 640x480. Po tym oknie musielibysmy sie przewijac myszka.
   Zdecydowanie, to nie jest nasz cel : zazwyczaj my zmieniajac rozdzielczosc
   chcemy miec po prostu wieksza/mniejsza dokladnosc obrazu zmieniajac ilosc pixeli
   na ktorych ten obraz ma sie miescic. }
 {$ifdef GLWINDOW_HAS_VIDEO_CHANGE}
 if VideoModeChanged and (not UseVirtualXScreen) then
  result := CurrentModeInfo.hdisplay else
 {$endif}
  result := XDisplayWidth(display, screen);
end;

function TGLWindowsManager.ScreenHeight: integer;
begin
 {$ifdef GLWINDOW_HAS_VIDEO_CHANGE}
 if VideoModeChanged and (not UseVirtualXScreen) then
  result := CurrentModeInfo.vdisplay else
 {$endif}
  result := XDisplayHeight(display, screen);
end;

function TGLWindowsManager.ImplementationName: string;
begin
 Result := 'Xlib';
end;

{ --------------------------------------------------------------------------------
  TGLWindowsManager video resizing with XF86VMode }

{$ifdef GLWINDOW_HAS_VIDEO_CHANGE}

function XF86VidModeModeLineToModeInfo(const ModeLine: TXF86VidModeModeLine;
  DotClock: Longint): TXF86VidModeModeInfo;
begin
 result.dotclock := DotClock;
 result.hdisplay := ModeLine.hdisplay;
 result.hsyncstart := ModeLine.hsyncstart;
 result.hsyncend := ModeLine.hsyncend;
 result.htotal := ModeLine.htotal;
 result.hskew := ModeLine.hskew;
 result.vdisplay := ModeLine.vdisplay;
 result.vsyncstart := ModeLine.vsyncstart;
 result.vsyncend := ModeLine.vsyncend;
 result.vtotal := ModeLine.vtotal;
 result.flags := ModeLine.flags;
 result.privsize := 0;
 { TODO: should I try to copy privsize here somehow ?
   TODO: should we in TryVideoChange and VideoReset and here do something
   with c_private fields ? Manual is unclear to me... }
end;

function TGLWindowsManager.TryVideoChange: boolean;
var {vidModeMajorVersion, vidModeMinorVersion: integer;}
    i, bestmode: integer;

    modesCount: integer;
    modes : PArray_PXF86VidModeModeInfo;

    OriginalMode: TXF86VidModeModeLine;
    OriginalModeDotClock: Longint;
begin
 {w tej chwili nie zmieniamy BitsPerPixel ekranu wiec
  robimy faktyczne VideoChange tylko jezeli VideoResize. }
 if not VideoResize then exit(true);

 {TESTOWE:}
{ XF86VidModeQueryVersion(DisplayPtr, @vidModeMajorVersion,
   @vidModeMinorVersion);
 Writeln(Format('XF86VidModeExtension-Version %d.%d', [vidModeMajorVersion,
   vidModeMinorVersion]));}

 { if this is first VideoChange then save desktop resolution before
   switching modes }
 if not VideoModeChanged then
 begin
  XF86VidModeGetModeLine(DisplayPtr, Screen, @OriginalModeDotClock,
    @OriginalMode);
  OriginalModeInfo := XF86VidModeModeLineToModeInfo(OriginalMode, OriginalModeDotClock);
 end;

 XF86VidModeGetAllModeLines(DisplayPtr, Screen, @modesCount, @modes);
 try
  { look for mode with requested resolution }
  bestmode := -1;
  for i := 0 to modesCount-1 do
   if (not VideoResize) or
      ((modes[i]^.hdisplay = VideoResizeWidth) and
       (modes[i]^.vdisplay = VideoResizeHeight)) then
       { TODO: jak tu dopasowac BitsPerPixel }
    bestMode := i;

  if bestmode < 0 then exit(false);

  CurrentModeInfo := modes[bestMode]^;
  XF86VidModeSwitchToMode(DisplayPtr, Screen, @CurrentModeInfo);

  {SetViewport jest przydatne gdy zmienimy sie z duzego ekranu na mniejszy i
   nie chcemy uzywac VirtualXScreen. Wtedy czasem nasz maly ekranik jest
   nieco z boku i user musi nakierowac myszka aby byl on w srodku.
   Ponizsze SetViewport zrobi to za niego. }
  XF86VidModeSetViewPort(DisplayPtr, Screen, 0, 0);

  VideoModeChanged := true;
  result := true;
 finally XFree(modes) end;
end;

procedure TGLWindowsManager.VideoReset;
begin
 if VideoModeChanged then
 begin
  { switch back to original desktop resolution }
  XF86VidModeSwitchToMode(DisplayPtr, Screen, @OriginalModeInfo);
  VideoModeChanged := false;
 end;
end;

{
 stare testy:
 XSetInputFocus(glwm.display, windowXID, RevertToParent, CurrentTime);
}

{$endif GLWINDOW_HAS_VIDEO_CHANGE}

{$endif read_implementation}
