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

{ CastleWindow backend using WinAPI, Windows-only.

  TODO:
  - Alt key presses are not perfectly detected.
    Is it even possible to cleanly catch K_Alt key press in WinAPI?
    We would have to catch sys_keydown message but then we also
    block using standard Alt+F4 or Alt+Space? Another trouble:
    if you enter system menu by Alt+Down, we will not get Alt+Up?
  - Implement MainMenu.Enabled.
}

{$ifdef OpenGLES}
  {$I castlewindow_egl.inc}
{$else}
  {$I castlewindow_wgl.inc}
{$endif}

{$ifdef read_interface_uses}
Windows, CommDlg,
{$endif}

{$ifdef read_implementation_uses}
CastleWindowModes,
{$endif}

{$ifdef read_window_interface}
private
  h_Dc: HDC;
  h_Wnd: HWND;
  { InCloseBackend = true means we are currently inside CloseBackend call.
    Useful -- see at WM_ACTIVATE. }
  InCloseBackend: boolean;

  EnableUpdatesFromWindowProc, DisableUpdatesFromWindowProc: Cardinal;

  VK_LeftBracket_Exists,
  VK_RightBracket_Exists,
  VK_Apostrophe_Exists,
  VK_Semicolon_Exists,
  VK_Slash_Exists,
  VK_BackQuote_Exists,
  VK_BackSlash_Exists: boolean;
  VK_LeftBracket,
  VK_RightBracket,
  VK_Apostrophe,
  VK_Semicolon,
  VK_Slash,
  VK_BackQuote,
  VK_BackSlash: Byte;

  {$ifdef CASTLE_ENGINE_PLUGIN}
  PluginWindow: TNPWindow;
  {$endif}

  function WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

  { convert virtual key code (VK_xxx) to TKey. Returns K_None if no conversion
    available. }
  function VirtualKeyToKey(VirtualKey: Byte): TKey;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  { Should we do ChangeDisplaySettings(nil, 0) when Destroy? }
  DisplaySettingsChanged: boolean;
  wndClassName: string;

  { A private array, initialized in CreateBackend.
    NameForLoadCursor are actually constant, taken from Windows unit,
    but CursorHandle are loaded each time in CreateBackend.
    Having them all loaded once makes sure that WM_SETCURSOR is able to
    use predefined HCURSOR values and so is fast. SetCursor is guaranteed
    by WinAPI to return fast if called with already used cursor.

    For mcDefault, and mcCustom, values in this array are undefined.
    For mcNone, CursorHandle is 0 (this is what SetCursor always accepts
    as invisible cursor). }
  CursorHandles: array [TMouseCursor] of record
    NameForLoadCursor: LPCTSTR;
    Handle: HCURSOR;
  end;

  fappActive: boolean;
  procedure SetAppActive(value: boolean);
  property appActive: boolean read fappActive write SetAppActive;
{$endif read_application_interface}

{$ifdef read_implementation}

{$I castlewindow_winapi_menu.inc}

{ TCastleWindowCustom ------------------------------------------------- }

const
  { Some constants missing from FPC's Windows unit. }
  VK_OEM_1 = $BA;
  VK_OEM_PLUS = $BB;
  VK_OEM_COMMA = $BC;
  VK_OEM_MINUS = $BD;
  VK_OEM_PERIOD = $BE;
  VK_OEM_2 = $BF;
  VK_OEM_3 = $C0;
  VK_OEM_4 = $DB;
  VK_OEM_5 = $DC;
  VK_OEM_6 = $DD;
  VK_OEM_7 = $DE;
  VK_OEM_8 = $DF;
  VK_OEM_102 = $E2;

{ convert virtual key code (VK_xxx) to TKey. Returns K_None if no conversion
  available. }
function TCastleWindowCustom.VirtualKeyToKey(VirtualKey: Byte): TKey;
var
  VirtualKeyToKeyResult: TKey absolute Result;

  function VK_Stored(VK_Exists: boolean; VK_Value: Byte; K_Value: TKey): boolean;
  begin
    Result := VK_Exists and (VirtualKey = VK_Value);
    if Result then
      VirtualKeyToKeyResult := K_Value;
  end;

begin
  { Tests: InfoWrite(Format('key %d', [VirtualKey])); }

  case VirtualKey of
    VK_BACK: Result := K_BackSpace;
    VK_TAB: Result := K_Tab;
    VK_RETURN: Result := K_Enter;
    VK_SHIFT: Result := K_Shift;
    VK_CONTROL: Result := K_Ctrl;
    VK_MENU: Result := K_Alt;
    VK_ESCAPE: Result := K_Escape;
    VK_SPACE: Result := K_Space;
    VK_PRIOR: Result := K_PageUp;
    VK_NEXT: Result := K_PageDown;
    VK_END: Result := K_End;
    VK_HOME: Result := K_Home;
    VK_LEFT: Result := K_Left;
    VK_UP: Result := K_Up;
    VK_RIGHT: Result := K_Right;
    VK_DOWN: Result := K_Down;
    VK_INSERT: Result := K_Insert;
    VK_DELETE: Result := K_Delete;
    VK_ADD: Result := K_Numpad_Plus;
    VK_SUBTRACT: Result := K_Numpad_Minus;
    VK_SNAPSHOT: Result := K_PrintScreen;
    VK_NUMLOCK: Result := K_NumLock;
    VK_SCROLL: Result := K_ScrollLock;
    VK_CAPITAL: Result := K_CapsLock;
    VK_PAUSE: Result := K_Pause;
    VK_OEM_COMMA: Result := K_Comma;
    VK_OEM_PERIOD: Result := K_Period;
    VK_NUMPAD0: Result := K_Numpad_0;
    VK_NUMPAD1: Result := K_Numpad_1;
    VK_NUMPAD2: Result := K_Numpad_2;
    VK_NUMPAD3: Result := K_Numpad_3;
    VK_NUMPAD4: Result := K_Numpad_4;
    VK_NUMPAD5: Result := K_Numpad_5;
    VK_NUMPAD6: Result := K_Numpad_6;
    VK_NUMPAD7: Result := K_Numpad_7;
    VK_NUMPAD8: Result := K_Numpad_8;
    VK_NUMPAD9: Result := K_Numpad_9;
    VK_CLEAR: Result := K_Numpad_Begin;
    VK_MULTIPLY: Result := K_Numpad_Multiply;
    VK_DIVIDE: Result := K_Numpad_Divide;
    VK_OEM_MINUS: Result := K_Minus;
    VK_OEM_PLUS: Result := K_Plus;

    Ord('0') .. Ord('9'): Result := TKey(Ord(K_0)  + Ord(VirtualKey) - Ord('0'));
    Ord('A') .. Ord('Z'): Result := TKey(Ord(K_A)  + Ord(VirtualKey) - Ord('A'));
    VK_F1    .. VK_F12  : Result := TKey(Ord(K_F1) + Ord(VirtualKey) - Ord(VK_F1));
    else
      if not VK_Stored(VK_LeftBracket_Exists , VK_LeftBracket , K_LeftBracket ) then
      if not VK_Stored(VK_RightBracket_Exists, VK_RightBracket, K_RightBracket) then
      if not VK_Stored(VK_Apostrophe_Exists  , VK_Apostrophe  , K_Apostrophe  ) then
      if not VK_Stored(VK_Semicolon_Exists   , VK_Semicolon   , K_Semicolon   ) then
      if not VK_Stored(VK_Slash_Exists       , VK_Slash       , K_Slash       ) then
      if not VK_Stored(VK_BackQuote_Exists   , VK_BackQuote   , K_BackQuote   ) then
      if not VK_Stored(VK_BackSlash_Exists   , VK_BackSlash   , K_BackSlash   ) then
        Result := K_None;
  end;

  { Note that CastleWindow WinAPI will never generate Press/Release with these keys:
      K_Numpad_End;
      K_Numpad_Down;
      K_Numpad_PageDown;
      K_Numpad_Left;
      K_Numpad_Right;
      K_Numpad_Home;
      K_Numpad_Up;
      K_Numpad_PageUp;
      K_Numpad_Insert;
      K_Numpad_Delete;
      K_Numpad_Enter;
    because (as far as I can see) they are undistinguishable from normal,
    non-numpad key codes under WinAPI.

    Note that it seems that VK_SNAPSHOT (K_PrintScreen) is never passed to
    WM_KEYDOWN (although it's passed to WM_KEYUP; but there we ignore it,
    because in our Keys[] table it's already up).
    So you will never get KeyDown/Up with K_PrintScreen. }
end;

function TryHandlingMessage(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM): boolean;
var
  Window: TCastleWindowCustom;
begin
  Window := TCastleWindowCustom(GetWindowLongPtr(hWnd, GWL_USERDATA));
  Result := Application.FindWindow(Window) >= 0;
  if Result then
    Window.WindowProc(uMsg, wParm, lParm);
end;

function WndProc(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM) :LRESULT; stdcall;
var
  Window: TCastleWindowCustom;
begin
  Window := TCastleWindowCustom(GetWindowLongPtr(hWnd, GWL_USERDATA));
  { Wszystkie hwnd jakie tu beda wpadac to beda nasze okna
    (it's only different for a plugin, but right now the plugin uses only TryHandlingMessage,
    not this). Ale zanim
    ustawimy oknu SetWindowLongPtr (GetWindowLongPtr bedzie do tego czasu zwracac 0)
    ono juz dostaje jakies messagy - WM_CREATE i inne.
    Mozemy je spokojnie zignorowac.

    Co wiecej jednak, moze tak byc ze Window <> nil ale wskazuje na okienko
    ktore juz jest zamkniete (a moze nawet na obiekt ktory juz zostal
    zwolniony z pamieci !). Dlaczego tak jest, patrz komentarz na poczatku
    castlewindow_winsystem.inc. Wiec sprawdzamy tutaj czy Window jest na liscie
    Application.OpenWindows .

    Moreover, temporary windows created by CreateTemporaryWindow in
    SetPixelFormat_WGLChoose also get here (as they use our
    Application.wndClassName). They don't set Get/SetWindowLongPtr, so Window here
    will be @nil in this case. }
  if Application.FindWindow(Window) >= 0 then
    result := Window.WindowProc(uMsg, wParm, lParm) else
    result := DefWindowProc(hWnd, uMsg, wParm, lParm);
end;

function TCastleWindowCustom.WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

{ Note: the following Windows events may be called from CloseBackend
  (so they probably should check InCloseBackend):
     WM_WINDOWPOSCHANGING
     WM_WINDOWPOSCHANGED
     WM_NCACTIVATE
     WM_ACTIVATE
     WM_ACTIVATEAPP
     WM_KILLFOCUS
     WM_DESTROY
     WM_NCDESTROY

     WM_SIZE (yes, tested on Windows 2000 Prof, when running my progs
       with --fullscreen-custom, e.g. glinformation --fullscreen-custom 640x480
       or glplotter --fullscreen-custom 640x480)
}

  procedure MaybeCapture;
  { mouse down occurred; so capture the mouse if it's not captured already }
  begin
    if GetCapture <> H_Wnd then SetCapture(H_Wnd);
  end;

  procedure MaybeRelease;
  { mouse up occurred; release capture if all mouse buttons are now up }
  begin
    if (mousePressed=[]) and (GetCapture = H_Wnd) then ReleaseCapture;
  end;

  type
    { This is useful to deconstruct WParam and LParam. It will work faster
      than any LoWord/HiWord functions in Windows unit, and it allows you
      to specify signedness of values as you wish. }
    TWinParam = packed record
      case Integer of
        0:(Signed: LongInt {$ifdef CPUX86_64}; Dummy: LongWord {$endif});
        1:(LoWord, HiWord: Word);
        2:(LoSmallint, HiSmallint: Smallint);
        3:(LowestByte, LowerByte, HigherByte, HighestByte: byte);
    end;

  function InterceptedSystemKey(key: integer): boolean;
  begin
    Result := { TODO-alt: (key = K_Alt) or }(key = VK_F10)
  end;

  procedure Handle_WM_KEYDOWN;
  { Notka:
    Windows wysyla nam WM_KEYDOWN co jakis czas dopoki user trzyma dany klawisz.
    Czestotliwosc wysylania WM_KEYDOWN zalezy od ustawien autorepeat klawiatury.
    To czy klawisz zostal wcisniety po raz pierwszy czy tez jest to jego auto-repeat
    mozna ustalic badajac bit KEY_ALREADY_DOWN w lParm; ale nie robie tego, bo to
    dziala ok. EventKeyDown jest wywolywane wielokrotnie gdy ktos trzyma wcisniety
    klawisz. Mialem tutaj kiedys implementacje wlasnego autorepeata z wlasnym
    KeyDownDelay ale byla niepotrzebna, przeciez Windows automatycznie robi
    wszystko ok. }
  var
    Key: TKey;
    CharMsg: TMsg;
    CharKey: char;
  begin
    { WM_CHAR is placed in our message queue by TranslateMessage.
      Always in WM_KEYDOWN we do this PeekMessage(..., WM_CHAR, WM_CHAR, PM_REMOVE).
      This means that now we have
      1. no WM_CHAR messages in our message queue (which means that this
         WM_KEYDOWN message is not translatable as WM_CHAR) OR
      2. we have exactly one WM_CHAR message in our queue, and this is WM_CHAR
         message corresponding to current WM_KEYDOWN message. }
    if PeekMessage(@CharMsg, h_Wnd, WM_CHAR, WM_CHAR, PM_REMOVE) then
      CharKey := Chr(CharMsg.wParam) else
      CharKey := #0;

    Key := VirtualKeyToKey(wParm);

    { We cannot depend that VK_OEM_4 is always "[" and VK_OEM_6 is always "]",
      see http://www.tronan.com/macromachine/scripthelp/VK.htm.
      On non-US keyboards, other keys may correspond to them.

      So below we do a little trick: we check which virtual key corresponds to
      left/right brackets, and we remember it. If my VirtualKeyToKey returned
      K_None for this virtual key, then we can safely assume that in the future
      (e.g. for next WM_KEYUP for this key) this virtual key always corresponds
      to left/right bracket. }
    if Key = K_None then
    begin
      case CharKey of
        '[' : begin VK_LeftBracket_Exists  := true; VK_LeftBracket  := wParm; Key := K_LeftBracket ; end;
        ']' : begin VK_RightBracket_Exists := true; VK_RightBracket := wParm; Key := K_RightBracket; end;
        '''': begin VK_Apostrophe_Exists   := true; VK_Apostrophe   := wParm; Key := K_Apostrophe  ; end;
        ';' : begin VK_Semicolon_Exists    := true; VK_Semicolon    := wParm; Key := K_Semicolon   ; end;
        '/' : begin VK_Slash_Exists        := true; VK_Slash        := wParm; Key := K_Slash       ; end;
        '`' : begin VK_BackQuote_Exists    := true; VK_BackQuote    := wParm; Key := K_BackQuote   ; end;
        '\' : begin VK_BackSlash_Exists    := true; VK_BackSlash    := wParm; Key := K_BackSlash   ; end;
      end;
    end;

    if (Key <> K_None) or (CharKey <> #0) then DoKeyDown(Key, CharKey);
  end;

  procedure Handle_WM_KEYUP;
  var
    Key: TKey;
  begin
    Key := VirtualKeyToKey(wParm);
    if Key <> K_None then DoKeyUp(Key);
  end;

  procedure HandleMouseDown(const Button: TMouseButton);
  begin
    MaybeCapture;
    DoMouseDown(Vector2Single(
                   TWinParam(lParm).LoSmallint,
      Height - 1 - TWinParam(lParm).HiSmallint), Button, 0);
  end;

  procedure HandleMouseUp(const Button: TMouseButton);
  begin
    DoMouseUp(Vector2Single(
                   TWinParam(lParm).LoSmallint,
      Height - 1 - TWinParam(lParm).HiSmallint), Button, 0);
    MaybeRelease;
  end;

{ FPC <= 2.2.4 doesn't define WM_MOUSEHWHEEL in rtl/win/wininc/messages.inc }
{$ifdef VER2_0} const WM_MOUSEHWHEEL= $020E; {$endif}
{$ifdef VER2_2} const WM_MOUSEHWHEEL= $020E; {$endif}

var
  DoEnableUpdatesFromWindowProc, DoDisableUpdatesFromWindowProc: boolean;
begin
  Result := 0;

  { generalnie chcemy przepuszczac SYSKEY* do DefaultWndProc ktore moze
    wtedy np. na Alt+F4 zrobic nam close albo na Alt+spacja otworzyc
    nam system menu itp. To wszystko sa fajne rzeczy i nie chce ich blokowac
    przechwytujac zawsze zdarzenia SYSKEY*, tym samym rezygnuje z przetwarzania
    kiedywkolwiek sekwencji klawiszy w rodzaju Alt+F4.

    Ale jednak czasem chce przechwycic zdarzenia SYSKEY* zeby obsluzyc wlasne
    klawisze. Wiec niektore klawisze przechwytujemy od systemu operacyjnego.
    (normalnie Alt i F10 powoduja wejscie w menu). Wiec wtedy zmieniam uMsg z
    SYSKEY* na KEY* (korzystajac z tego ze te zdarzenia maja taki sam format
    dla wParm i lParm). }
  if (uMsg = WM_SYSKEYDOWN) and InterceptedSystemKey(wParm) then uMsg := WM_KEYDOWN;
  if (uMsg = WM_SYSKEYUP) and InterceptedSystemKey(wParm) then uMsg := WM_KEYUP;

  case uMsg of
    WM_ACTIVATE:
      if not InCloseBackend then
        if LoWord(wParm) = WA_INACTIVE then
          { When user switches to another program, fake that we release all keys/mouse.
            Otherwise we could miss some key up / mouse up, when user releases it over
            another program/window.
            Do not do this when InCloseBackend, as ReleaseAllKeysAndMouse causes
            some DoKeyUp/DoMouseUp (and these even cause BackendMakeCurrent), and no DoXxx
            should be done when the window is closing and releasing it's resources. }
          ReleaseAllKeysAndMouse;
      { Don't Exit, let control flow to DefWindowProc. }
    WM_ACTIVATEAPP:
      begin
        Application.appActive := longbool(wParm);
        {exit; <- allow DefWindowProc handle WM_ACTIVETAPP too,
         since you can't say "we surely handled everything that should be done in
         reaction for this message"}
      end;
    WM_CLOSE: begin DoCloseQuery; Exit end;

    WM_KEYDOWN: begin Handle_WM_KEYDOWN; Exit end;
    WM_KEYUP: begin Handle_WM_KEYUP; Exit end;

    { The only way we can get WM_CHAR message is when we get WM_KEYDOWN
      message and TranslateMessage (called from castlewindow_winsystem.inc)
      translates it to WM_CHAR message.
      But always in WM_KEYDOWN handler (a few lines above) we do
      PeekMessage(..., WM_CHAR, WM_CHAR, PM_REMOVE) so all WM_CHAR messages
      are removed from queue immediately after they are put there by
      TranslateMessage.

      So no WM_CHAR message should be ever dispatched by DispatchMessage.
      So no WM_CHAR message should ever get here.

      Except: It seems wine-1.5.30-1 doesn't implement something correctly,
      and let's WM_CHAR arrive sometimes (when pressing escape to unfocus
      menu bar). So this is just commented out now.

    WM_CHAR:
       raise EInternalError.Create(
         'All WM_CHAR messages should be handled while handling WM_KEYDOWN');
    }

    WM_SIZE:
      if not InCloseBackend then
      begin
        { Note: Window class has (VREDRAW or HREDRAW), so Windows will automatically
          take care of requesting window redraw after window resize. }
        if (wParm = SIZE_MAXIMIZED) or (wParm = SIZE_RESTORED) then
          DoResize(LoWord(lParm), HiWord(lParm), false);
        Exit;
      end;
    WM_MOVE:
       {WM_MOVE zachowuje sie jakby bylo resize, bo po przesunieciu okna
        takze jest wymagane glViewport. Nie jestem pewien czy jest to niedorobka
        w implementacji openGL'a pod Windowsem czy jest to dopuszczalne - w kazdym
        razie musimy tak robic, co latwo sprawdzic wywolujac OpenGL_windowed, przesuwajac
        okno mysza a potem robiac glRedPixels (odczyta pixele z miejsca w ktorym BYLO
        nasza okienko, czyli odczyta jakis kawalek desktopu, innych aplikacji itp.) }
      begin
        fLeft := TWinParam(lParm).LoSmallint;
        fTop := TWinParam(lParm).HiSmallint;

        { You know it, and I know it: WinAPI is so damn stupid and inconsistent.
          What's up this time ? Well, I want to update Left and Top below to the position
          on the screen of our client area. "Client area" means tha place where OpenGL
          context is --- it's the window without the border, title bar and menu bar
          (if MainMenu <> nil). I need this, because

          1. Left and Top are said in unit's interface to work like that
          2. SetMousePosition requires this. Otherwise positions set by SetMousePosition
             and read by MousePosition properties would be slightly different.
             And there are real cases when this is extremely important that they use
             exactly the same coordinate systems --- see e.g. mouse look handling in
             TCastleWindowCustom.EventMotion.

          So what's the problem ? Well, if the window is not FullScreen, Left and Top
          as recorded in lParm are exactly what I want. But when FullScreen, Left and Top
          are always 0, 0 --- which means that they don't take menu bar height into
          account. The effect: mouse look + menu bar + fullscreen mode don't work
          (i.e. user is unable to do mouse look, TCastleWindowCustom.EventMotion always
          ignores it). Tested on view3dscene.

          So I workaround this by adding CYMENU. This is not perfect, as this assumes
          that menu bar will always be a single line.
          Note that this is non-perfect also because WM_MOVE message may not get to
          FullScreen window at all, so in OpenBackend I also do this trick. }
        if FullScreen and (MainMenu <> nil) and MainMenuVisible then
          FTop += GetSystemMetrics(SM_CYMENU);

        { TODO: przerobic to na glViewport ? po co wywolywac tu OnResize ktore
          moze robic tu mnostwo rzeczy ktore przeciez nie sa potrzebne, bo tak
          naprawde nie zmienil sie size tylko position ? }
        DoResize(fWidth, fHeight, false);
        Exit;
      end;
    WM_SIZING:
      begin
        result := WMSizingHandler(lParm, wParm, minWidth, minHeight, maxWidth, maxHeight);
        Exit;
      end;
     { We do not capture WM_PAINT, let WM_PAINT fallback to DefWindowProc.
       WM_PAINT occurs also when there's a need to redraw window frame
       (which we don't care about, we don't draw custom window frame).

       If there's a need to actually redraw window contents,
       then WM_PAINT will cause (immediately, it seems that DefWindowProc
       will just call WndProc) WM_ERASEBKGND, and we want to capture WM_ERASEBKGND. }
    WM_ERASEBKGND:
      begin
        if IsWindowEnabled(H_Wnd) then
          Invalidate else
          { If not IsWindowEnabled then we have a modal window blocking our window.
            Like a Windows.MessageBox. In this case, our Application.ProcessMessage
            may not be continously called, so we cannot use normal Invalidate
            and depend that window will be repainted soon.

            IOW, right now we're in WindowProc but not because of calling
            DispatchMessage from Application.ProcessMessage. Instead Windows
            modal boxes makes artificial call to cause our redraw,
            e.g. because user moves the modal window over our window.
            So just redraw right now. }
          DoRender;
        Result := 1;
        Exit;
      end;

    { This is needed so that our window properly redraws itself when user
      activates menu from menu bar. If this is not present then when user
      activates our menu bar for the FIRST time (I don't know why only the
      first ?) and then opens some drop-drop menu 1, then switches to
      another drop-down menu 2, then we have problem: drop-down menu 1 contents
      are still partially visible (but only drop-down menu 2 contents should
      be visible now !). So our program must come and draw itself, so that
      drop-down menu 1 contents are not visible anymore.

      Note that this occurs when the user activates our menu bar for the first time.
      If he will close the menu and then open it again then Windows will properly
      handle everything and drop-down menu 1 contents will not be visible when
      drop-down menu 2 is open. (Windows can do this using saved image of our
      window, because activating menu under Windows essentially blocks our program
      anyway (look at comments at WM_INITMENU)).

      I don't know why Windows can't handle this itself when the menu is open
      for the first time. So maybe I'm actually fixing a Windows bug by handling
      this WM_ENTERIDLE below ? All of this tested on Windows 2000 Prof. }
    WM_ENTERIDLE: begin DoRender; Exit end;

    WM_INITMENU:
      begin
        { We must call ReleaseAllKeysAndMouse when menu is activated.
          Even though the application message
          loop is blocked while we are in menu (menu under Windows work like modal
          windows that block activity of main window, the only way we can do some
          action when the menu is open is to handle some messages here like
          WM_ENTERIDLE or WM_INITMENU; those messages do NOT come from
          castlewindow_winsystem.inc DispatchMessage call, our WndProc is instead
          called directly by WinAPI).

          Still the problem remains: when someone presses a key (we get DoKeyDown)
          and then enters menu and then releases a key (we don't get DoKeyUp) and then
          leaves menu -- we have KeysDown[SomeKey] = true where it should be false.

          That's why we need this ReleaseAllKeysAndMouse below. }
        ReleaseAllKeysAndMouse;

        { Windows blocks our program when user enters a menu bar.
          Essentially, this means that window will not receive normal events,
          will not process OnUpdate and such during the time menu bar is open.
          So menu bar is like a modal dialog box.
          So Fps.SecondsPassed at next Update is irrelevant, just like at the end of TGLMode.
          Fixes e.g. CameraTransitions when choosing viewpoint from view3dscene menu.

          We would like to call this always when user leaves the menu, but it seems
          there's no WinAPI event for this. But we can as well call this when user enters
          the menu, because of blocking above --- it will have the same effect, since
          DoUpdate is not called inside the menu. }
        Fps.ZeroNextSecondsPassed;

        Exit;
      end;

    WM_MOUSEMOVE:
      begin
        { We could refresh FMousePressed now:
            FmousePressed:=[];
            if (MK_LBUTTON and wParm) <> 0 then Include(fmousePressed, mbLeft);
            if (MK_MBUTTON and wParm) <> 0 then Include(fmousePressed, mbMiddle);
            if (MK_RBUTTON and wParm) <> 0 then Include(fmousePressed, mbRight);
          but it's not needed (we keep it current anyway in mousedown/up events). }
        DoMotion(InputMotion(MousePosition, Vector2Single(
                         TWinParam(lParm).LoSmallint,
            Height - 1 - TWinParam(lParm).HiSmallint),
          MousePressed, 0));
        Exit;
      end;
    WM_LBUTTONDOWN: begin HandleMouseDown(mbLeft  ); Exit end;
    WM_MBUTTONDOWN: begin HandleMouseDown(mbMiddle); Exit end;
    WM_RBUTTONDOWN: begin HandleMouseDown(mbRight ); Exit end;
    WM_LBUTTONUP  : begin HandleMouseUp(mbLeft  ); Exit end;
    WM_MBUTTONUP  : begin HandleMouseUp(mbMiddle); Exit end;
    WM_RBUTTONUP  : begin HandleMouseUp(mbRight ); Exit end;
    WM_COMMAND    :
      { If this comes from a menu item, call DoMenuClick }
      if HiWord(wParm) = 0 then
      begin
        DoMenuClick(MenuItemFromSmallId(LoWord(wParm)));
        Exit;
      end;

    { On Windows, we have to change cursor in each WM_SETCURSOR (called on each
      mouse move (when mouse not captured), we also manually call it after each
      Cursor property change).

      That's because Windows internally doesn't have any "cursor" property associated
      with h_Wnd --- instead, it has only SetCursor that changes cursor globally,
      for everyone. Changing cursor on each WM_SETCURSOR seems to be the solution to
      enable / disable our private Cursor value when appropriate, so as not to change
      cursor look for other programs. This is also what glut for win32 does, so thanks
      go to glut win32 author, Nate Robins, for showing how it's done.

      There's some uncertainty whether doing SetCursor calls so often (each WM_SETCURSOR,
      each mouse move !) will not decrease performance --- but, well, there seems to be
      no other way, and SetCursor docs guarantee that SetCursor checks whether cursor
      changes. In no change, SetCursor returns immediately. So, we feel a little safer
      that we use CursorHandles[Cursor].Handle, so when Cursor stays the same, SetCursor
      is called with exactly the same arguments. }
    WM_SETCURSOR:
      begin
        { Falling back on DefWindowProc means that it will install
          our window class cursor (or parent cursor) if in client area,
          or just do the right thing if outside client area.
          So it's suitable when wParm <> h_Wnd or we want mcDefault. }
        if wParm = h_Wnd then
          case InternalCursor of
            mcDefault: { Fallback on DefWindowProc };
            mcCustom: { TODO, for now behave like mcDefault };
            else
              begin
                Windows.SetCursor(Application.CursorHandles[InternalCursor].Handle);
                Exit(1);
              end;
          end;
      end;

    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      begin
        if HiWord(WParm) <> 0 then
          DoMouseWheel(SmallInt(HiWord(WParm)) / 120, uMsg = WM_MOUSEWHEEL);
        Exit;
      end;
  end;

  { The DefWindowProc may enter a modal loop in some cases, e.g. when
    WM_NCLBUTTONDOWN initializes window resizing operation.

    This makes our normal message loop not working (although application
    is working, but methods like ProcessMessage are not called in a loop anymore,
    instead we "hang" in a DispatchMessage call in top-level ProcessMessage).
    WinAPI will directly call WindowProc, creating a recursion.
    See See http://stackoverflow.com/questions/3102074/win32-my-application-freezes-while-the-user-resizes-the-window

    So to keep receiving update/timer/redraws, we simply call
    UpdateAndRenderEverything manually here.

    A simple solution was to just call it always when recursion here is detected.
    But this seems unexpected to WinAPI in many situations, causing

    1. crashes, when this is done from some messages.
    2. slowdown at initialization (view3dscene main menu blinks for some
      time, with "busy" cursor, before it becomes responsive).

    So the solution now is to activate it only for certain commands,
    and moreover deactivate inside certain other commands. }
  //Writeln('Got message ', uMsg,
  //  ', UpdatesFromWindowProc: Enable: ', EnableUpdatesFromWindowProc,
  //  ', Disable: ', DisableUpdatesFromWindowProc);

  if (EnableUpdatesFromWindowProc <> 0) and
     (DisableUpdatesFromWindowProc = 0) and
     (not Closed) then
    Application.UpdateAndRenderEverything;

  DoEnableUpdatesFromWindowProc := uMsg = WM_NCLBUTTONDOWN;
  DoDisableUpdatesFromWindowProc :=
    // user closes window by clicking on close button
    ((uMsg = WM_SYSCOMMAND) and (wParm = SC_CLOSE)) or
    // we close the window by code
    (uMsg = WM_NCACTIVATE) or
    // we recreate the window as fullscreen
    (uMsg = WM_IME_SETCONTEXT) or
    // we recreate the window as windowed?
    (uMsg = WM_ACTIVATE);

  if DoEnableUpdatesFromWindowProc then Inc(EnableUpdatesFromWindowProc);
  if DoDisableUpdatesFromWindowProc then Inc(DisableUpdatesFromWindowProc);
  try
    Result := DefWindowProc(h_Wnd, uMsg, wParm, lParm);
  finally
    if DoEnableUpdatesFromWindowProc then Dec(EnableUpdatesFromWindowProc);
    if DoDisableUpdatesFromWindowProc then Dec(DisableUpdatesFromWindowProc);
  end;
end;

function TCastleWindowCustom.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

procedure TCastleWindowCustom.BackendMenuFinalize;
var
  Old_H_Menu: HMenu;
begin
  Old_H_Menu := GetMenu(H_Wnd);
  OSCheck( DestroyMenu(Old_H_Menu), 'DestroyMenu(Old_H_Menu)');
  { for now, Handle of our menu items are always zero.
    But do this for the future. }
  MainMenu.ClearHandles;
end;

procedure TCastleWindowCustom.BackendMenuInitialize;
begin
  OSCheck( SetMenu(H_Wnd, MakeWinapiMenu(MainMenu, true)), 'SetMenu');
end;

procedure TCastleWindowCustom.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowCustom.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowCustom.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.OpenBackend;
var
  {$ifndef CASTLE_ENGINE_PLUGIN}
  dwExStyle, dwStyle: dword;
  WindowRect: TRect;
  H_Menu: HMenu;
  {$endif}
  ClientRect: TRect;
begin
  {$ifdef CASTLE_ENGINE_PLUGIN}
  FLeft := PluginWindow.X;
  FTop := PluginWindow.Y;
  FWidth := PluginWindow.Width;
  FHeight := PluginWindow.Height;
  Fullscreen := false;

  H_wnd := HWND(PluginWindow.Window);
  {$else}
  { optionally hide mouse in FullScreem,
    set dwStyle and dwExStyle for our window }
  if FFullScreen then
  begin
    WindowRect := Bounds(0, 0, Application.ScreenWidth, Application.ScreenHeight);
    dwExStyle := WS_EX_APPWINDOW;
    dwStyle := DWord(WS_POPUP); { glut daje tutaj tez WS_MAXIMIZE; bez znaczenia, jak sadze; }
  end else
  begin
    WindowRect := Bounds(fleft, ftop, fwidth, fheight);
    dwExStyle := WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
    if ResizeAllowed = raAllowed then
    begin
      { jezeli rozmiary ekranu nie spelniaja wymagan min/max Width/Height to nie dajemy
        przycisku MAXIMIZE dla okienka. Ponizszy test nie jest precyzyjny bo okienko
        zmaksymalizowane ma wewnetrzna szerokosc / wysokosc troche mniejsze od
        ScreenW/ScreemH. Ale szczerze mowiac nie mam ochoty sie babrac w badaniu
        szerokosci ramki zmaksymalizowanego okienka. Po prostu - jezeli ponizszy check
        spowoduje ze nie bedzie przycisku MAXIMIZE mimo ze moglby byc to trudno, to tylko
        drobna niewygoda dla usera. Jezeli spowoduje ze przycisk MAXIMIZE bedzie mimo ze
        nie powinno go byc to obwarowane checkami DoResize zignoruje nieprawidlowy rozmiar
        i juz. }
      if (not between(Application.ScreenWidth, minWidth, maxWidth)) or
         (not between(Application.ScreenHeight, minHeight, maxHeight)) then
        dwStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_THICKFRAME else
        dwStyle := WS_OVERLAPPEDWINDOW;
    end else
      { dwStyle jest tu dokladnien taki sam jak WS_OVERLAPPEDWINDOW ale bez
        WS_THICKFRAME i WS_MAXIMIZEBOX czyli nie mozna resizowac okna. }
      dwStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX;

    { Adjust WindowRect - so that ClientRect will hold the requested size and
      position. Whole WindowRect will be a bit larger (depends on dwStyle,
      dwExStyle).

      Note that when FullScreen = true there is no need to do this - the intention
      of FullScreen is that whole window (with menu) covers whole screen and
      window has no frame. So WindowRect should be (0, 0, Application.ScreenWidth,
      Application.ScreenHeight) and this is done set. I could write it like that:
        AdjustWindowRectEx(WindowRect, dwStyle,
          (not FullScreen) and (MainMenu <> nil), dwExStyle);
      but it's more clear to just omit AdjustWindowRectEx call when FullScreen.

      Note the (documented) WinAPI bug here: AdjustWindowRectEx cannot take
      into account if the menu bar wraps (in two or more rows). This wrapping
      happens, depending on menu contents and user fonts. But AdjustWindowRectEx
      has to way to account for it: it only knows if the menu exists at all.
      Documented in MSDN explicitly: "The AdjustWindowRectEx function does not add
      extra space when a menu bar wraps to two or more rows.", with no workaround
      pointed...

      This means that actual OpenGL area size may be different than
      requested (OnResize will report it corectly, eventually lying, honoring
      ResizeAllowed properrty). Happened e.g. when glViewImage allowed minimal
      window size = 200, and default welcome image had 256x256 pixels.
      When you opened glViewImage with no params in directory with no images,
      then OpenGL area of size 256x256 was requested, which was so narrow that
      menu bar wrapped. Which made OpenGL area not tall enough for 256 pixels,
      and glViewImage had to add a vertical scrollbar to display welcome image.
    }
    AdjustWindowRectEx(WindowRect, dwStyle,
      (MainMenu <> nil) and MainMenuVisible, dwExStyle);
  end;
  dwStyle := dwStyle or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;

  { calculate H_Menu }
  if (MainMenu <> nil) and MainMenuVisible then
    H_Menu := MakeWinapiMenu(MainMenu, true) else
    H_Menu := 0;

  { main point : create the window }
  H_wnd := CreateWindowEx(dwExStyle,
    PChar(Application.wndClassName), PChar(GetWholeCaption), dwStyle,
    windowRect.left, windowRect.top,
    windowRect.right-windowRect.left, windowRect.Bottom-windowRect.top,
    0 { no parent window }, H_Menu, hInstance,
    nil { don't pass anything to WM_CREATE } );
  {$endif}

  Check(h_Wnd <> 0, 'Creating window (H_wnd) failed');

  { calculate correct Left and Top, required e.g. by SetCursorPos }
  if FullScreen then
  begin
    FLeft := 0;
    FTop := 0;
  end;
  { See WM_MOVE handling comments why this is needed to make Top correct. }
  if FullScreen and (MainMenu <> nil) and MainMenuVisible then
    FTop += GetSystemMetrics(SM_CYMENU);

  SetWindowLongPtr(h_Wnd, GWL_USERDATA, PtrUInt(Self));

  { get h_DC and set pixel format }

  h_Dc := GetDC(h_Wnd);
  Check ( h_Dc <> 0, 'GetDC failed');

  ContextCreateEnd(h_Wnd);

  MakeCurrent;

  { robimy OpenWindowsAdd PRZED ShowWindow bo ShowWindow wywoluje WinProc
    z message'm kazacym nam sie odmalowac. Wiec zeby przetworzyc tego messaga
    (ktory powoduje po prostu Invalidate) nasze okienko musi byc
    zarejestrowane w Application.OpenWindows. Dlaczego nie zrobic po prostu Invalidate ?
    Tak jest porzadniej, no i w ten sposob wiem DLACZEGO to dziala, a nie tylko
    ze "dziala kiedy wywolamy Invalidate". }
  Application.OpenWindowsAdd(Self);

  {$ifndef CASTLE_ENGINE_PLUGIN}
  if Visible then
  begin
    ShowWindow(h_Wnd, SW_SHOW);
    SetForegroundWindow(h_Wnd);
    SetFocus(h_Wnd);
  end;
  {$endif}

  { Windows does not send us WM_SIZE messages when FullScreen (that's probably
    caused by our special dwStyle and dwStyleEx for FullScreen, probably
    by the lack of WS_OVERLAPPED in dwStyle). So we should simulate it here,
    because we actually "manualy" changed the size (to screen size) by applying FullScreen.
    Also, MainMenu may take some space, so our size is not even precisely
    Application.ScreenWidth / Application.ScreenHeight.
    So we check client rect size explicitly, with GetClientRect, and call DoResize
    (we don't care here about ResizeAllowed, DoResize will take care of averything). }
  if FullScreen then
  begin
    OSCheck( GetClientRect(h_Wnd, ClientRect), 'GetClientRect');
    DoResize(ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top, false);
  end;
end;

procedure TCastleWindowCustom.SetCursor(const Value: TMouseCursor);
var
  Point: Windows.TPoint;
begin
  if FCursor <> Value then
  begin
    FCursor := Value;

    { We reinstall the cursor on each WM_SETCURSOR call, that's the proper
      place to do it. Right now, we want to force this reinstallation,
      since FCursor changed (normally WM_SETCURSOR will be called only when
      mouse will be moved). So we call PostMessage, if current position is
      within the window.

      This is similar to what glut for Win32 does in glutSetCursor. }
    GetCursorPos(@Point);
    if h_Wnd = WindowFromPoint(Point) then
      PostMessage(h_Wnd, WM_SETCURSOR, h_Wnd, 0 { not important});
  end;
end;

procedure TCastleWindowCustom.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  FCustomCursor := Value;
  { TODO }
end;

procedure TCastleWindowCustom.SetMousePosition(const Value: TVector2Single);
begin
  if not Closed then
    SetCursorPos(Left + Floor(Value[0]), Top + Height - 1 - Floor(Value[1]));
end;

procedure TCastleWindowCustom.CloseBackend;
begin
  InCloseBackend := true;
  try
    ContextDestroy;

    if (h_Dc <> 0) and (ReleaseDC(h_Wnd, h_Dc) = 0) then
      WritelnWarning('WinAPI', 'Releasing device context (ReleaseDC) failed.');
    h_Dc := 0;
    {$ifndef CASTLE_ENGINE_PLUGIN}
    if (h_Wnd <> 0) and (not DestroyWindow(h_Wnd))then
      WritelnWarning('WinAPI', 'Destroying window (DestroyWindow) failed.');
    {$endif}
    h_Wnd := 0;
  finally InCloseBackend := false; end;
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if h_Wnd <> 0 then SetWindowText(h_Wnd, PChar(GetWholeCaption));
end;

procedure TCastleWindowCustom.CreateBackend;
begin
end;

function TCastleWindowCustom.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;

  { Make path absolute. Same as ExpandFileName, but knows that
    FilePath contains only a path and will always work as it should.
    (Delphi's and FPC's do not guarantee correct ExpandFileName result
    for paths ended with PathDelim.)

    FilePath may, but doesn't have to, end with PathDelim.
    Result will always end with PathDelim.

    Note: '' is treated as current dir (as it always should be, because
    ExtractFilePath('file') = '' and 'file' is in current dir). }
  function ExpandFilePath(const FilePath: string): string;
  begin
   if FilePath = '' then
    Result := InclPathDelim(GetCurrentDir) else
    Result := ExtractFilePath( ExpandFileName(InclPathDelim(FilePath) +
      'dummy_file_name' ));

   { note : doing here something like
       ExtractFilePath( ExpandFileName(ExclPathDelim(FilePath)) )
     (i.e., pretending that FilePath points to a file, not directory)
     would not be safe, because we don't know what ExpandFileName
     will do with 'c:' }
  end;

  { Prepare string with encoded filter names and patterns, as specified
    in WinAPI OPENFILENAME reference.

    Note that Windows encoding of OpenFileName has limitations here, since all filters
    are passed in a string: pattern cannot contain ';', and WinAPI reference
    says that it also cannot contain a space. }
  function MakeStrFilter(FileFilters: TFileFilterList): string;
  var
    I, J: Integer;
    Filter: TFileFilter;
  begin
    Result := '';
    for I := 0 to FileFilters.Count - 1 do
    begin
      Filter := FileFilters[I];

      Result += Filter.Name + #0;

      for J := 0 to Filter.Patterns.Count - 1 do
      begin
        if J <> 0 then Result += ';';
        Result += Filter.Patterns[J];
      end;

      Result += #0;
    end;

    Result += #0;
  end;

  function SearchFirstPatternWithExtension(const Ext: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;

    { Look only at extensions '.' + something non-empty.
      Don't even bother looking at special extensions '' and '.',
      or things wihout a leading '.', they will not
      work with our trick anyway, see comments where I use
      SearchFirstPatternWithExtension. }
    if (Length(Ext) >= 2) and (Ext[1] = '.') then
    begin
      for I := 0 to FileFilters.Count - 1 do
        if SameText(FileFilters[I].Patterns[0], '*' + Ext) then
        begin
          if (Result = -1) or
             { If we're lucky enough to have more than one match (this may happen
               e.g. for our "all images" filters that list the sum of other filters)
               then choose more specific (with less patterns) filter. }
             (FileFilters[I].Patterns.Count < FileFilters[Result].Patterns.Count) then
            Result := I;
        end;
    end;
  end;

var
  Struct: TOpenFileName;
  FileNameBuffer: array [0..6000] of char;
  FilePath: string;
  Mode: TGLModeFrozenScreen;
  CurrentFileNameExt: string;
  CurrentFormatIndex: Integer;
begin
  FillChar(Struct, SizeOf(Struct), 0);
  Struct.lStructSize := SizeOf(Struct);
  Struct.hwndOwner := H_Wnd;

  FileNameBuffer := ExtractFileName(FileName);
  Struct.lpstrFile := @FileNameBuffer;

  { As for FileNameBuffer size:
    we just hope that FileNameBuffer will be ALWAYS sufficiently large.
    This seems to be the only reasonable way to deal with this.
    Checking CommDlgExtendedError for BUFFER_TOO_SMALL and then decoding
    required length from lpstrFile is pretty pointless (after all, you will know
    the required length, but then how can you ask GetXxxFileName to return again
    the same filename, without displaying dialog once again?). }
  Struct.nMaxFile := SizeOf(FileNameBuffer);

  { We're storing result of ExtractFilePath in FilePath, because pointer
    Struct.lpstrInitialDir must be valid when I call GetOpenFileName }
  FilePath := ExtractFilePath(FileName);
  { In WinAPI help they say that when lpstrInitialDir = nil it uses current dir.
    But that's not true, it uses some other dir (last dir of open dialog box ?).
    So I have to fix this here, correcting FilePath from '' to GetCurrentDir.
    In fact, it seems that GetOpen/SaveFileName can't understand relative paths,
    in the form 'subdir1/file' or './subdir1/file' (maybe it does not understand
    that those paths are relative to current dir ??), so I'm always expanding paths. }
  FilePath := ExpandFilePath(FilePath);
  Struct.lpstrInitialDir := PCharOrNil(FilePath);

 { tests:
   InfoWrite('current dir : "' + GetCurrentDir + '"');
   InfoWrite('dialog box InitialDir : "' + FilePath + '"');}

  Struct.lpstrTitle := PCharOrNil(Title);
  Struct.Flags := OFN_NOCHANGEDIR or OFN_HIDEREADONLY;
  if OpenDialog then
    Struct.Flags := Struct.Flags or OFN_FILEMUSTEXIST else
    Struct.Flags := Struct.Flags or OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST;

  if FileFilters <> nil then
  begin
    Struct.lpstrFilter := PChar(MakeStrFilter(FileFilters));
    Struct.nFilterIndex := FileFilters.DefaultFilter + 1;

    if not OpenDialog then
    begin
      { Note that GetSaveFileName is brain-dead and
        works really correct only if you pass filename without
        extension, and user doesn't type any extension. Then GetSaveFileName appends
        to the output filename the extension from current filter, or lpstrDefExt
        if current filter doesn't contain any extension (e.g. if current filter has '*'
        as all files). And then it works Ok.

        It fails tragically if user types some extension. Basically, GetSaveFileName
        will not change filename extension then, but standard behavior expects that you
        should save the file with format chosen in filters, that is "save as type" label.
        So you probably save a file with data format and filename extension totally
        uncorelated.

        Yes, this stupidity actually happens --- e.g. in M$ Outlook (it's made my M$,
        so it's supposed to reflect some "standard" usage, right?) saving the email
        (there are two filters, one is Email (*.eml) and the other HTML (*.htm, *.html)):
        if you type a.html, and press Enter (leaving data format as Email)...
        the data format is Email but it's saved to filename named a.html.

        This means that if user types an extension or application proposes an extension
        then the bad things happen.
        (Like FireFox: it proposes filename with extension a.htm, so when user changes
        data format to "text file (*.txt)" the file gets saved to a.htm with text (not direct
        HTML tags) format)) . Program must (by standard)
        use data format chosen in the dialog and filename as given --- so they are totally
        unrelated.

        This also means that it's difficult for user to sensibly save file with extension
        that is not the 1st pattern of some filter. GetSaveFileName appends only the 1st pattern
        of current filter, and if you just type the extension then you probably forget to
        change combo box "save as type".

        What can I do? I want to avoid WinAPI stupidity, and I do not want to return
        a separate parameter specifying which filter was chosen (to keep interface simple),
        so I want the filename to be related with data format.
        I can do one thing: if there exists a filter
        with current filename extension as it's 1st pattern then I can remove it from
        filename and set this filter as default. So in typical case things will work Ok,
        as much as they can, assumig that one of your filters matches your current extension.

        In other cases, "save as type" of my
        WinAPI dialogs will just be useless, but there's nothing I can do: I have to
        keep filename with extension in lpstrFile then, and GetSaveFileName will
        not correlate this with format chosen by combo box "save as type".
        So I will simply ignore combo box "save as type" (while Windows standard says
        to ignore extension then; either way, both exits are wrong, since user can very
        easily save a file with data format and extension not related).
      }
      CurrentFileNameExt := ExtractFileExt(FileNameBuffer);
      CurrentFormatIndex := SearchFirstPatternWithExtension(CurrentFileNameExt);
      if CurrentFormatIndex <> -1 then
      begin
        { We can use workaround described above. }
        FileNameBuffer := DeleteFileExt(FileNameBuffer);
        Struct.nFilterIndex := CurrentFormatIndex + 1;

        { If user selects something like "all files" then also append this extension.

          I pass here CurrentFileNameExt without leading dot (since
          SearchFirstPatternWithExtension returned something <> -1, I know there's
          a leading dot).

          Note: only first three characters of lpstrDefExt extension are taken
          into account,  according to WinAPI reference. (and WTF can I do about it?)

          Also note that unregistered extensions are treated somewhat differently.
          Unfortunately, lpstrDefExt will append then, instead of leaving filename alone. }
        Struct.lpstrDefExt := PChar(SEnding(CurrentFileNameExt, 2));
      end;
    end;
  end;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    if OpenDialog then
      Result := GetOpenFileName(@Struct) else
      Result := GetSaveFileName(@Struct);
  finally Mode.Free end;

  if Result then
    FileName := FileNameBuffer else
  begin
    if CommDlgExtendedError <> 0 then
      raise Exception.CreateFmt('GetOpen/SaveFileName failed with error %d',
        [CommDlgExtendedError]);
  end;
end;

function TCastleWindowCustom.ColorDialog(var Color: TVector3Single): boolean;

  function Vector3SingleToColorRef(const Value: TVector3Single): TColorRef;
  begin
   Result := Windows.RGB(Clamped(Round(Value[0]*High(Byte)), 0, High(Byte)),
                         Clamped(Round(Value[1]*High(Byte)), 0, High(Byte)),
                         Clamped(Round(Value[2]*High(Byte)), 0, High(Byte)) );
  end;

  function ColorRefToVector3Single(const Value: TColorRef): TVector3Single;
  begin
   Result[0] := GetRValue(Value)/High(Byte);
   Result[1] := GetGValue(Value)/High(Byte);
   Result[2] := GetBValue(Value)/High(Byte);
  end;

var
  Struct: TChooseColor;
  CustomColors: array [0..15] of TColorRef;
  Mode: TGLModeFrozenScreen;
begin
  { init CustomColors to all blacks; TODO: I should use a static table for this
    (and put a comment in CastleWindow to say that custom colors are preserved in win dialog) }
  FillChar(CustomColors, SizeOf(CustomColors), 0);

  { init Struct }
  FillChar(Struct, SizeOf(Struct), 0);
  Struct.lStructSize := SizeOf(Struct);
  Struct.hwndOwner := H_Wnd;
  Struct.rgbResult := Vector3SingleToColorRef(Color);
  Struct.lpCustColors := @CustomColors;
  { Give CC_FULLOPEN since this is intended to give user a free choice from RGB
    colors. So we suggest user to choose any RGB color freely, instead of using
    only the predefined ones. }
  Struct.Flags := CC_FULLOPEN or CC_RGBINIT;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    Result := ChooseColor(@Struct);
  finally Mode.Free end;

  if Result then
    Color := ColorRefToVector3Single(Struct.rgbResult);
end;

const
  MessageTypeToWin: array [TWindowMessageType] of LongWord =
  ( MB_ICONINFORMATION,
    MB_ICONWARNING,
    MB_ICONQUESTION,
    MB_ICONERROR,
    0 );

  MessageTypeToCaption: array [TWindowMessageType] of string =
  ( 'Information',
    'Warning',
    'Question',
    'Error',
    'Information');

procedure TCastleWindowCustom.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  MessageBox(H_Wnd, PChar(S),
    { Always pass meaningfull caption. Passing nil here causes Windows to
      always display localized "Error" on caption, which looks nonsensible
      on boxes with warnings or questions. }
    PChar(MessageTypeToCaption[MessageType]),
    MB_OK or MessageTypeToWin[MessageType] or
    { MB_APPLMODAL is actually the default. MB_TASKMODAL would also be suitable
      probably, as we always pass non-nil H_Wnd. }
    MB_APPLMODAL);
end;

function TCastleWindowCustom.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := MessageBox(H_Wnd, PChar(S),
    PChar(MessageTypeToCaption[MessageType]),
    MB_YESNO or MessageTypeToWin[MessageType] or
    { MB_APPLMODAL is actually the default. MB_TASKMODAL would also be suitable
      probably, as we always pass non-nil H_Wnd. }
    MB_APPLMODAL) = IDYES;
end;

procedure TCastleWindowCustom.SetFullScreen(const Value: boolean);
begin
  SimpleSetFullScreen(Value);
end;

{ TCastleApplication ----------------------------------------------- }

procedure TCastleApplication.CreateBackend;
var
  WindowClass: TWndClass;
  C: TMouseCursor;
begin
  appActive := true;

  { initialize NameForLoadCursor for CursorHandles }
  CursorHandles[mcStandard].NameForLoadCursor := IDC_ARROW;
  CursorHandles[mcWait].NameForLoadCursor := IDC_WAIT;
  CursorHandles[mcText].NameForLoadCursor := IDC_IBEAM;
  CursorHandles[mcHand].NameForLoadCursor := IDC_HAND;

  { initialize Handle for CursorHandles }

  CursorHandles[mcNone].Handle := 0;
  CursorHandles[mcForceNone].Handle := 0;

  for C := Low(C) to High(C) do
    if not (C in [mcDefault, mcNone, mcForceNone, mcCustom]) then
      CursorHandles[C].Handle := LoadCursor(0, CursorHandles[C].NameForLoadCursor);

  { create window class (WindowClass) }

  { no need to worry about multiple applications
    registering same WndClassName because window classes are
    process-specific (so, if other process registers another
    CastleWindow class, it will be just a different class -
    distinguished by the hInstance parameter of TWndClass and
    CreateWindowEx). }
  WndClassName := 'CastleWindow';

  FillChar(WindowClass, SizeOf(WindowClass), 0);
  { style : redraw on size,  own DC for window }
  WindowClass.style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
  WindowClass.lpfnWndProc := @WndProc;
  WindowClass.hInstance := hInstance;
  { sprobuj zaladowac ikone z pliku EXE, tzn. z naszego resource;
    w ten sposob mozesz ja latwo zmieniac w IDE delphi a mozesz sam stworzyc
    odpowiedni resource przez windres, no i twoja ikona bedzie widoczna dla innych
    programow.
    Jesli nie znajdzie ikonki w resource to zaladuj ikone ze zbioru Windowsa
    idi_application. }
  WindowClass.hIcon := LoadIcon(hInstance, 'MAINICON');
  if WindowClass.hIcon = 0 then WindowClass.hIcon := LoadIcon(0, IDI_APPLICATION);
  WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
  WindowClass.lpszClassName := PChar(WndClassName);
  OSCheck( Windows.RegisterClass(WindowClass) <> 0, 'RegisterClass');
end;

procedure TCastleApplication.DestroyBackend;
begin
  OSCheck( Windows.UnregisterClass(PChar(wndClassName), hInstance), 'UnregisterClass');
end;

procedure TCastleApplication.VideoReset;
begin
  { switch back to the desktop mode }
  if DisplaySettingsChanged then ChangeDisplaySettings(nil, 0);
end;

function TCastleApplication.TryVideoChange: boolean;

{$ifdef VER1_9_6}
{ This is to workaround a bug in FPC 1.9.6.
  Already fixed in FPC 1.9.7. }
type
  { Copied from FPC 1.9.7 from 2005-01-18 }
  DEVMODE = record
       dmDeviceName : array[0..(CCHDEVICENAME)-1] of BCHAR;
       dmSpecVersion : WORD;
       dmDriverVersion : WORD;
       dmSize : WORD;
       dmDriverExtra : WORD;
       dmFields : DWORD;
       case byte of
         1: (dmOrientation : SmallInt;
             dmPaperSize : SmallInt;
             dmPaperLength : SmallInt;
             dmPaperWidth : SmallInt;
             dmScale : SmallInt;
             dmCopies : SmallInt;
             dmDefaultSource : SmallInt;
             dmPrintQuality : SmallInt;
             dmColor : SmallInt;
             dmDuplex : SmallInt;
             dmYResolution : SmallInt;
             dmTTOption : SmallInt;
             dmCollate : SmallInt;
             dmFormName : array[0..(CCHFORMNAME)-1] of BCHAR;
             dmLogPixels : WORD;
             dmBitsPerPel : DWORD;
             dmPelsWidth : DWORD;
             dmPelsHeight : DWORD;
             dmDisplayFlags : DWORD;
             dmDisplayFrequency : DWORD;
             dmICMMethod : DWORD;
             dmICMIntent : DWORD;
             dmMediaType : DWORD;
             dmDitherType : DWORD;
             dmICCManufacturer : DWORD;
             dmICCModel : DWORD
            );
         2: (dmPosition: POINTL;
             dmDisplayOrientation: DWORD;
             dmDisplayFixedOutput: DWORD;
            );
    end;
{$endif}

var
  dmScreenSettings: Devmode;
  ChangeResult: Windows.LONG;
begin
  VideoReset; { reset normal screen settings if we called TryVideoChange more than once }

  { optionally adjust screen to our fullscreen mode (do ChangeDisplaySettings) }
  with dmScreenSettings do
  begin
    dmFields := 0;

    if VideoColorBits <> 0 then
    begin
      dmBitsPerPel := VideoColorBits;
      dmFields     := dmFields or DM_BITSPERPEL;
    end;

    if VideoResize then
    begin
      dmPelsWidth  := VideoResizeWidth;
      dmPelsHeight := VideoResizeHeight;
      dmFields     := dmFields or DM_PELSWIDTH or DM_PELSHEIGHT;
    end;

    if VideoFrequency <> 0 then
    begin
      dmDisplayFrequency := VideoFrequency;
      dmFields    := dmFields or DM_DISPLAYFREQUENCY;
    end;

    if dmFields <> 0 then { then TRY to change screen size and/or resolution }
    begin
      dmSize := sizeof(dmScreenSettings);

      { Reading ChangeDisplaySettings docs
        (http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/devcons_7gz7.asp):
        The dmDriverExtra member of DEVMODE must be initialized to indicate
        the number of bytes of private driver data following the
        DEVMODE structure.

        So I guess that I should just set this to 0 ? }
      dmDriverExtra := 0;

      ChangeResult := ChangeDisplaySettings(@dmScreenSettings, CDS_FullScreen);
      Result := ChangeResult = DISP_CHANGE_SUCCESSFUL;
      { tests: InfoWrite('ChangeResult = '+ DispChangeToStr(ChangeResult)); }
      if Result then DisplaySettingsChanged := true;
    end else
      Result := true; { we didn't do anything - well, then we succeded, right ? }
  end;
end;

procedure TCastleApplication.SetAppActive(value: boolean);
begin
  { ponizej : gdy fappActive zmienia sie z true na false musimy
    wykonac ChangeDisplaySettings(nil, 0) bez wzgledu na to czy
    DisplaySettingsChanged. W ten sposob unikamy bledu Windowsa
    z zostawaniem przycisku po oknie fullscreen na pasku zadan.
    W rezultacie musimy tez odnawiac nasz Video mode jesli potem
    wracamy do naszej aplikacji. }
  if value and not appActive then
  begin
    if DisplaySettingsChanged then VideoChange(true);
  end else
  if appActive and not value then
    ChangeDisplaySettings(nil, 0);

  fappActive := value;
end;

function TCastleApplication.ScreenWidth: integer;
begin
  if DisplaySettingsChanged and VideoResize then
    result := VideoResizeWidth else
    result := GetSystemMetrics(SM_CXSCREEN);
end;

function TCastleApplication.ScreenHeight: integer;
begin
  if DisplaySettingsChanged and VideoResize then
    result := VideoResizeHeight else
    result := GetSystemMetrics(SM_CYSCREEN);
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'WinAPI';
end;

{ TCastleClipboard ----------------------------------------------------------- }

{ Clipboard operations using WinAPI based on:
  - http://msdn.microsoft.com/en-us/library/windows/desktop/ms649016%28v=vs.85%29.aspx
  - LCL implementation: TWin32WidgetSet.ClipboardGetOwnerShip (set)
    and ClipboardGetData (get) in lcl/interfaces/win32/win32winapi.inc
}

function TCastleClipboard.GetAsText: string;
var
  DataHandle: Windows.HGLOBAL;
  Data: Pointer;
  Size: Integer;
begin
  Result := '';
  if IsClipboardFormatAvailable(CF_TEXT) then
  begin
    if not Windows.OpenClipboard(Windows.HWND(nil)) then Exit;
    try
      DataHandle := Windows.GetClipboardData(CF_TEXT);
      if DataHandle = 0 then Exit;

      Size := Windows.GlobalSize(DataHandle);
      if Size > 1 then
      begin
        Dec(Size); // do not copy trailing #0
        Assert(Size > 0);
        Data := Windows.GlobalLock(DataHandle);
        try
          SetLength(Result, Size);
          Move(Data^, Result[1], Size);
        finally Windows.GlobalUnlock(DataHandle) end;
      end;
    finally Windows.CloseClipboard end;
  end;
end;

procedure TCastleClipboard.SetAsText(const Value: string);
var
  DataHandle : Windows.HGLOBAL;
  Data: Pointer;
  Size: Integer;
begin
  if not Windows.OpenClipboard(Windows.HWND(nil)) then Exit;
  try
    if not Windows.EmptyClipboard then Exit;

    if Value <> '' then
    begin
      Size := Length(Value) + 1; // copy to Windows with trailing #0
      DataHandle := Windows.GlobalAlloc(Windows.GMEM_MOVEABLE, Size);
      if DataHandle = 0 then Exit;

      Data := GlobalLock(DataHandle);
      try
        Move(Value[1], Data^, Size);
      finally Windows.GlobalUnlock(DataHandle) end;
      Windows.SetClipboardData(CF_TEXT, DataHandle);
    end;
  finally Windows.CloseClipboard end;
end;

{ TCastleWindow -------------------------------------------------------------- }

procedure TCastleWindow.NavigationInfoChanged;
begin
end;

{$endif read_implementation}
