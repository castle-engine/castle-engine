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
  h_GLRc: HGLRC;
  h_Dc: HDC;
  h_Wnd: HWND;
  { InCloseImplDepend = true means we are currently inside CloseImplDepend call.
    Useful -- see at WM_ACTIVATE. }
  InCloseImplDepend: boolean;
  function WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

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

  { convert virtual key code (VK_xxx) to TKey. Returns K_None if no conversion
    available. }
  function VirtualKeyToKey(VirtualKey: Byte): TKey;
{$endif read_tglwindow_interface}

{$ifdef read_tglwindowmanager_interface}
private
  { czy robic ChangeDisplaySettings(nil, 0) przy Destroy ? }
  DisplaySettingsChanged: boolean;
  wndClassName: string;

  fappActive: boolean;
  procedure SetAppActive(value: boolean);
  property appActive: boolean read fappActive write SetAppActive;

  FMouseVisible: boolean;
  procedure SetMouseVisible(const Value: boolean);
  property MouseVisible: boolean
    read FMouseVisible write SetMouseVisible default true;
{$endif read_tglwindowmanager_interface}

{$ifdef read_implementation}

{ TGLWindow ------------------------------------------------- }

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
function TGLWindow.VirtualKeyToKey(VirtualKey: Byte): TKey;
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
  VK_OEM_PLUS: Result := K_Equal;

  Ord('0') .. Ord('9'): Result := K_0  + VirtualKey - Ord('0');
  Ord('A') .. Ord('Z'): Result := K_A  + VirtualKey - Ord('A');
  VK_F1    .. VK_F12  : Result := K_F1 + VirtualKey - VK_F1;
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

 { Note that GLWindow WinAPI will never generate KeyDown/Up with these keys:
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

function WndProc(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM) :LRESULT; stdcall;
var glwin: TGLWindow;
begin
 glwin := TGLWindow(GetWindowLong(hWnd, GWL_USERDATA));
 { wszystkie hwnd jakie tu beda wpadac to beda nasze okna. Ale zanim
   ustawimy oknu SetWindowLong (GetWindowLong bedzie do tego czasu zwracac 0)
   ono juz dostaje jakies messagy - WM_CREATE i inne.
   Mozemy je spokojnie zignorowac.

   Co wiecej jednak, moze tak byc ze glwin <> nil ale wskazuje na okienko
   ktore juz jest zamkniete (a moze nawet na obiekt ktory juz zostal
   zwolniony z pamieci !). Dlaczego tak jest, patrz komentarz na poczatku
   GLWindow_winsystem.inc. Wiec sprawdzamy tutaj czy glwin jest na liscie
   glwm.Active .}
 if glwm.FindWindow(glwin) >= 0 then
  result := glwin.WindowProc(uMsg, wParm, lParm) else
  result := DefWindowProc(hWnd, uMsg, wParm, lParm);
end;

function TGLWindow.WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

{ Note: the following Windows events may be called from CloseImplDepend
  (so they probably should check InCloseImplDepend):
     WM_WINDOWPOSCHANGING
     WM_WINDOWPOSCHANGED
     WM_NCACTIVATE
     WM_ACTIVATE
     WM_ACTIVATEAPP
     WM_KILLFOCUS
     WM_DESTROY
     WM_NCDESTROY

     WM_SIZE (yes, tested on Windows 2000 Prof, when running my progs
       with --fullscreen-custom, e.g. glcaps --fullscreen-custom 640x480
       or glplotter --fullscreen-custom 640x480)
}

  procedure MaybeCapture;
  { mouse down occured; so capture the mouse if it's not captured already }
  begin
   if GetCapture <> H_Wnd then SetCapture(H_Wnd);
  end;

  procedure MaybeRelease;
  { mouse up occured; release capture if all mouse buttons are now up }
  begin
   if (mousePressed=[]) and (GetCapture = H_Wnd) then ReleaseCapture;
  end;

  type
    LongWord_rec = packed record
      case integer of
       0:(Signed: LongInt);
       1:(LoWord, HiWord: Word);
       2:(LoSmallint, HiSmallint: Smallint);
       3:(LowestByte, LowerByte, HigherByte, HighestByte: byte);
       4:(LowestShortint, LowerShortint, HigherShortint, HighestShortint: byte);
    end;

  function InterceptedSystemKey(key: integer): boolean;
  begin result := { TODO-alt: (key = K_Alt) or }(key = K_F10) end;

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
  var Key: TKey;
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
  var Key: TKey;
  begin
   Key := VirtualKeyToKey(wParm);
   if Key <> K_None then DoKeyUp(Key);
  end;

begin
 Result := 0;

 {tests: LogWrite(MessageDescr(h_Wnd, uMsg, wParm, lParm));}

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
  { Let control flow to DefWindowProc.
    Jezeli not WindowActive to ReleaseAllKeysAndMouse (bo mozemy
    wychodzac miec jakis klawisz true, a potem user go zwolni nad innym oknem
    i odpowiednie WM_KEYUP do nas nie dojdzie)/

    We do not want to do any ReleaseAllKeysAndMouse if InCloseImplDepend
    (ReleaseAllKeysAndMouse causes some DoKeyUp/DoMouseUp and these cause MakeCurrent
    -- do not do that ! More generally, no DoXxx may be called from CloseImplDepend). }
  WM_ACTIVATE:
    if not InCloseImplDepend then
     if LoWord(wParm) = WA_INACTIVE then ReleaseAllKeysAndMouse;
  WM_ACTIVATEAPP:
    begin
     glwm.appActive := longbool(wParm);
     {exit; <- allow DefWindowProc handle WM_ACTIVETAPP too,
      since you can't say "we surely handled everything that should be done in
      reaction for this message"}
    end;
  WM_CLOSE: begin DoCloseQuery; Exit end;

  WM_KEYDOWN: begin Handle_WM_KEYDOWN; Exit end;
  WM_KEYUP: begin Handle_WM_KEYUP; Exit end;

  { The only way we can get WM_CHAR message is when we get WM_KEYDOWN
    message and TranslateMessage (called from glwindow_winsystem.inc)
    translates it to WM_CHAR message.
    But always in WM_KEYDOWN handler (a few lines above) we do
    PeekMessage(..., WM_CHAR, WM_CHAR, PM_REMOVE) so all WM_CHAR messages
    are removed from queue immediately after they are put there by
    TranslateMessage.

    So no WM_CHAR message should be ever dispatched by DispatchMessage.
    So no WM_CHAR message should ever get here. }
  WM_CHAR:
     raise EInternalError.Create(
       'All WM_CHAR messages should be handled while handling WM_KEYDOWN');

  WM_SIZE:
    if not InCloseImplDepend then
    begin
     { Note: Window class has (VREDRAW or HREDRAW), so Windows will automatically
       take care of requesting window redraw after window resize. }
     if (wParm = SIZE_MAXIMIZED) or (wParm = SIZE_RESTORED) then
      DoResize(LOWORD(lParm), HIWORD(lParm), false);
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
     fLeft := LongWord_rec(lParm).LoSmallint;
     fTop := LongWord_rec(lParm).HiSmallint;
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
   { nie chwytamy WM_PAINTa, pozwalamy aby WM_PAINT przelecial do DefWindowProc.
     WM_PAINT moze zajsc takze gdy np. trzeba tylko odmalowac ramke okna (a w to
     ja sie nie mieszam). WM_PAINT przesle (synchronicznie, tzn. praktycznie wprost
     do WndProc) WM_ERASEBKGND jesli trzeba bedzie odmalowac tlo i to jest to co ja chce
     chwycic. }
  WM_ERASEBKGND: begin
     if IsWindowEnabled(H_Wnd) then
      redisplayPosted := true else
      { TODO:  ponizsze ulegnie zmianie jezeli wprowadze wlasne okienka modalne }
      { jezeli nie jestesmy Enabled to moze znaczyc tylko ze zostalo
        uruchomione okienko modalne blokujace nasze okienko (np. Windows.MessageBox).
        W takiej sytuacji nie mozemy polegac na mechanizmie ustawiania
        redisplayPosted na true bo glwm.ProcessMessage moze nie dzialac
        (byc zawieszone, do czasu az wywolana gdziestam procedura MessageBox
        powroci). Innymi slowy, jestesmy w WindowProc ale nie na skutek wywolania
        DispatchMessage z glwm.ProcessMessage - lecz Windowsowe okienko modalne
        sztucznie udaje nasza petle komunikatow abysmy sie odmalowali, bo np.
        user przesuwa to okienko modalne nad naszym oknem.
        Musimy wiec w tym momencie przerysowac sie natychmiast. }
      begin
       DoDraw;
       redisplayPosted := false;
      end;
     result := 1;
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
  WM_ENTERIDLE: begin DoDraw; Exit end;

  { We must call ReleaseAllKeysAndMouse when menu is activated.
    Even though the application message
    loop is blocked while we are in menu (menu under Windows work like modal
    windows that block activity of main window, the only way we can do some
    action when the menu is open is to handle some messages here like
    WM_ENTERIDLE or WM_INITMENU; those messages do NOT come from
    GLWindow_winsystem.inc DispatchMessage call, our WndProc is instead
    called directly by WinAPI).

    Still the problem remains: when someone presses a key (we get DoKeyDown)
    and then enters menu and then releases a key (we don't get DoKeyUp) and then
    leaves menu -- we have KeysDown[SomeKey] = true where it should be false.

    That's why we need this ReleaseAllKeysAndMouse below. }
  WM_INITMENU: begin ReleaseAllKeysAndMouse; Exit; end;

  WM_MOUSEMOVE:
    begin
     { moglibysmy tu odswiezac fMousePressed :
     fmousePressed:=[];
     if (MK_LBUTTON and wParm) <> 0 then Include(fmousePressed, mbLeft);
     if (MK_MBUTTON and wParm) <> 0 then Include(fmousePressed, mbMiddle);
     if (MK_RBUTTON and wParm) <> 0 then Include(fmousePressed, mbRight);
       ... ale nie ma po temu zadnej potrzeby. }
     DoMouseMove( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint);
     exit;
    end;
  WM_LBUTTONDOWN: begin MaybeCapture; DoMouseDown( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbLeft);   Exit end;
  WM_MBUTTONDOWN: begin MaybeCapture; DoMouseDown( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbMiddle); Exit end;
  WM_RBUTTONDOWN: begin MaybeCapture; DoMouseDown( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbRight);  Exit end;
  WM_LBUTTONUP  : begin DoMouseUp( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbLeft);   MaybeRelease; Exit end;
  WM_MBUTTONUP  : begin DoMouseUp( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbMiddle); MaybeRelease; Exit end;
  WM_RBUTTONUP  : begin DoMouseUp( LongWord_rec(lParm).LoSmallint, LongWord_rec(lParm).HiSmallint, mbRight);  MaybeRelease; Exit end;
  WM_COMMAND    :
    begin
     { If this comes from a menu item, call DoMenuCommand }
     if HiWord(wParm) = 0 then
     begin
      DoMenuCommand(MenuItemFromSmallId(LoWord(wParm)));
      Exit;
     end;
    end;
 end;
 Result := DefWindowProc(h_Wnd, uMsg, wParm, lParm);
end;

function TGLWindow.RedirectKeyDownToMenuCommand: boolean;
begin
 Result := true;
end;

procedure TGLWindow.MainMenuChangedImplDepend;
var Old_H_Menu: HMenu;
begin
 Old_H_Menu := GetMenu(H_Wnd);
 KambiOSCheck( SetMenu(H_Wnd, WindowsMenuFromGLWindowMenu(MainMenu, true)),
   'SetMenu');
 KambiOSCheck( DestroyMenu(Old_H_Menu), 'DestroyMenu(Old_H_Menu)');
end;

procedure TGLWindow.InitWinSystemDepend;
var
  PixelFormat: integer;
  dwExStyle,
  dwStyle: dword;
  pfd: Tpixelformatdescriptor;
  WindowRect, ClientRect: TRect;
  H_Menu: HMenu;
begin
 if ColorBits = 0 then ColorBits := Glwm.VideoColorBits;

 { optionally hide mouse in FullScreem,
   set dwStyle and dwExStyle for our window }
 if FFullScreen then
 begin
  dwExStyle := WS_EX_APPWINDOW;
  dwStyle := DWord(WS_POPUP); { glut daje tutaj tez WS_MAXIMIZE; bez znaczenia, jak sadze; }
 end else
 begin
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
   if (not between(glwm.ScreenWidth, minWidth, maxWidth)) or
      (not between(glwm.ScreenHeight, minHeight, maxHeight)) then
    dwStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_THICKFRAME else
    dwStyle := WS_OVERLAPPEDWINDOW;
  end else
   { dwStyle jest tu dokladnien taki sam jak WS_OVERLAPPEDWINDOW ale bez
     WS_THICKFRAME i WS_MAXIMIZEBOX czyli nie mozna resizowac okna. }
   dwStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX;
 end;
 dwStyle := dwStyle or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;

 WindowRect := Bounds(fleft, ftop, fwidth, fheight);
 { Adjust WindowRect - so that ClientRect will hold the requested size and
   position. Whole WindowRect will be a bit larger (depends on dwStyle,
   dwExStyle).

   Note that when FullScreen = true there is no need to do this - the intention
   of FullScreen is that whole window (with menu) covers whole screen and
   window has no frame. So WindowRect should be (0, 0, glwm.ScreenWidth,
   glwm.ScreenHeight) and this is already set. I could write it like that:
     AdjustWindowRectEx(WindowRect, dwStyle,
       (not FullScreen) and (MainMenu <> nil), dwExStyle);
   but it's more clear to just omit AdjustWindowRectEx call when FullScreen. }
 if not FullScreen then
  AdjustWindowRectEx(WindowRect, dwStyle, MainMenu <> nil, dwExStyle);

 { evaluate H_Menu }
 if MainMenu <> nil then
  H_Menu := WindowsMenuFromGLWindowMenu(MainMenu, true) else
  H_Menu := 0;

 { main point : create the window }
 H_wnd := CreateWindowEx(dwExStyle,
   PChar(glwm.wndClassName), PChar(fCaption),
   dwStyle,
   windowRect.left, windowRect.top,
   windowRect.right-windowRect.left, windowRect.Bottom-windowRect.top,
   0 { no parent window }, H_Menu, hInstance,
   nil { don't pass anything to WM_CREATE } );
 Check( h_Wnd <> 0, 'CreateWindowEx failed');

 SetWindowLong(h_Wnd, GWL_USERDATA, Longint(Self));

 { get h_DC and set pixel format }

 h_Dc := GetDC(h_Wnd);
 Check ( h_Dc <> 0, 'GetDC failed');

 FillChar(pfd, SizeOf(pfd), 0);
 with pfd do begin
  nSize := SizeOf(TPIXELFORMATDESCRIPTOR);
  nVersion := 1;
  dwFlags := PFD_DRAW_TO_WINDOW              // Format Must Support Window
    or PFD_SUPPORT_OPENGL;                  // Format Must Support OpenGL
  if doubleBuffer then dwFlags := dwFlags or PFD_DOUBLEBUFFER;
  iPixelType := PFD_TYPE_RGBA;              // Request An RGBA Format
  cColorBits := ColorBits; // WinAPI accepts here 0 as "default"
  cAlphaBits := AlphaBits;
  cDepthBits := DepthBufferBits;
  cStencilBits := StencilBufferBits;
  { niestety cAccumRed/Green/Blue/AlphaBits sa ignorowane - musimy probowac poradzic sobie
    przy pomocy cAccumBits. }
  cAccumBits := RoundUpToMultiply(AccumBufferBits[0], 8) +
                RoundUpToMultiply(AccumBufferBits[1], 8) +
                RoundUpToMultiply(AccumBufferBits[2], 8) +
                RoundUpToMultiply(AccumBufferBits[3], 8);
  iLayerType := PFD_MAIN_PLANE;             // Main Drawing Layer
 end;
 PixelFormat := ChoosePixelFormat(h_Dc, pfd);
 KambiOSCheck( PixelFormat <> 0, 'ChoosePixelFormat');

 { sprawdzamy czy dostalismy wymagane AlphaBits, DepthBufferBits, StencilBufferBits i
   AccumBufferBits - to dlatego ze ChoosePixelFormat nie gwarantowalo nam ze dostaniemy
   rzeczywiscie to co chcemy, a w przypadku AccumBufferBits nie bylismy nawet w stanie
   powiedziec tego co trzeba ChoosePixelFormat.

   TODO:  na dluzsza mete dobrym rozwiazaniem bedzie tu zaimplementowanie samemu
   przegladania dostepnych pixel formats i wybierania samemu najlepszego pixel format. }
 DescribePixelFormat(h_Dc, PixelFormat, SizeOf(pfd), pfd);
 CheckRequestedBufferAttributes('ChoosePixelFormat',
   pfd.cStencilBits, pfd.cDepthBits, pfd.cAlphaBits,
   pfd.cAccumRedBits, pfd.cAccumGreenBits, pfd.cAccumBlueBits, pfd.cAccumAlphaBits);

 { skoro PixelFormat jest akceptowalny to wybierz go. Przekazywanie pfd w ponizszej
   funkcji nie jest wazne i wlasciwie moznaby z niego zrezygnowac, o ile dobrze rozumiem
   help do winapi. Wazne jest zeby przekazac wybrany PixelFormat. }
 KambiOSCheck( SetPixelFormat(h_Dc, PixelFormat, @pfd), 'SetPixelFormat');

 if (GetDeviceCaps(h_Dc, RASTERCAPS) and RC_PALETTE) <> 0 then
  raise EGLContextNotPossible.Create('This device is paletted ! Bad display settings !');

 { jezeli ColorBits bylo = 0 to teraz dowiedz sie co dostalismy }
 if ColorBits = 0 then
  ColorBits := GetDeviceCaps(h_DC, BITSPIXEL);

 { create gl context and make it current }
 h_GLRc := wglCreateContext(h_Dc);
 KambiOSCheck(h_GLRc <> 0, 'wglCreateContext');
 MakeCurrent;

 { robimy ActiveAdd PRZED ShowWindow bo ShowWindow wywoluje WinProc
   z message'm kazacym nam sie odmalowac. Wiec zeby przetworzyc tego messaga
   (ktory powoduje po prostu PostRedisplay) nasze okienko musi byc
   zarejestrowane w glwm.Active. Dlaczego nie zrobic po prostu PostRedisplay ?
   Tak jest porzadniej, no i w ten sposob wiem DLACZEGO to dziala, a nie tylko
   ze "dziala kiedy wywolamy PostRedisplay". }
 glwm.ActiveAdd(Self);

 ShowWindow(h_Wnd, SW_SHOW);
 SetForegroundWindow(h_Wnd);
 SetFocus(h_Wnd);

 { Windows does not send us WM_SIZE messages when FullScreen (that's probably
   caused by our special dwStyle and dwStyleEx for FullScreen, probably
   by the lack of WS_OVERLAPPED in dwStyle). But we should check it here,
   because it's possible that the real Width/Height of the OpenGL window is now
   NOT glwm.ScreenWidth/Height - that's because of MainMenu, it takes some space.
   So we check client rect size explicitly, with GetClientRect, and call DoResize
   (we don't care here about ResizeAllowed, DoResize will take care of averything). }
 if FullScreen then
 begin
  KambiOSCheck( GetClientRect(h_Wnd, ClientRect), 'GetClientRect');
  DoResize(ClientRect.Right-ClientRect.Left, ClientRect.Bottom-ClientRect.Top, false);
 end;
end;

procedure TGLWindow.SetMouseVisible(const Value: boolean);
begin
  { Note that this completely ignores the fact whether current window
    is Closed or not. This just changes cursor visiblity globally for all
    windows of this program.

    I don't know of any easy way to tell WinAPI that I want
    particular cursor hidden over some particular window.
    It seems that the only way to do it (it seems that Delphi
    does it like this) is just to intercept mouse moves and
    call SetCursor (this changes cursor globally, which can also
    make the cursor hidden --- quite like under GTK and Xlib
    where you can set the cursor to empty transparent image)
    whenever the control over which the cursor is changes it's Cursor
    property.

    For now I will definitely go with a simpler approach
    and just change cursor globally. }
  FMouseVisible := Value;
  Glwm.MouseVisible := MouseVisible;
end;

procedure TGLWindow.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  Assert(not Closed);
  SetCursorPos(Left + NewMouseX, Top + NewMouseY);
end;

procedure TGLWindow.CloseImplDepend;
begin
 InCloseImplDepend := true;
 try
  if h_GLRc <> 0 then
  begin
   if (not wglMakeCurrent(h_Dc, 0)) then
    CloseError('Deactivating current OpenGL rendering context (wglMakeCurrent(..., NULL)) failed.');
   if (not wglDeleteContext(h_GLRc)) then
    CloseError('Releasing current OpenGL rendering context (wglDeleteContext) failed.');
   h_GLRc := 0;
  end;
  if (h_Dc <> 0) and (ReleaseDC(h_Wnd, h_Dc) = 0) then
   CloseError('Releasing device context (ReleaseDC) failed.');
  h_Dc := 0;
  if (h_Wnd <> 0) and (not DestroyWindow(h_Wnd))then
   CloseError('Destroying window (DestroyWindow) failed.');
  h_Wnd := 0;
 finally InCloseImplDepend := false; end;
end;

procedure TGLWindow.MakeCurrent;
begin
  Assert(not Closed);
  KambiOSCheck( wglMakeCurrent(h_Dc, h_GLRc), 'wglMakeCurrent');
end;

procedure TGLWindow.SetCaption(const Value: string);
begin
 fCaption := Value;
 if h_Wnd <> 0 then SetWindowText(h_Wnd, PChar(Value));
end;

procedure TGLWindow.CreateImplDepend;
begin
end;

procedure TGLWindow.SwapBuffers;
begin
 Windows.SwapBuffers(h_Dc);
end;

function TGLWindow.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean): boolean;
var Struct: TOpenFileName;
    FileNameBuffer: array[0..6000]of char;
    FilePath: string;
    Mode: TGLModeFrozenScreen;
begin
 FillChar(Struct, SizeOf(Struct), 0);
 Struct.lStructSize := SizeOf(Struct);
 Struct.hwndOwner := H_Wnd;
 { We just hope that FileNameBuffer will be ALWAYS sufficiently large.
   I don't know, this seems to be the only reasonable way to deal with this.
   Checking CommDlgExtendedError for BUFFER_TOO_SMALL and then decoding
   required length from lpstrFile is pretty pointless. }
 FileNameBuffer := ExtractFileName(FileName);
 Struct.lpstrFile := @FileNameBuffer;
 Struct.nMaxFile := SizeOf(FileNameBuffer);

 { We're storing result of ExtractFilePath in FilePath, because pointer
   Struct.lpstrInitialDir must obviously be valid when I call GetOpenFileName }
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

 { enter glwin mode }
 Mode := TGLModeFrozenScreen.Create(Self, 0, false, nil);
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

function TGLWindow.ColorDialog(var Color: TVector3Single): boolean;

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

var Struct: TChooseColor;
    CustomColors: array[0..15]of TColorRef;
begin
 { init CustomColors to all blacks; TODO: I should use a static table for this
   (and put a comment in GLWindow to say that custom colors are preserved in win dialog) }
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

 Result := ChooseColor(@Struct);

 if Result then
  Color := ColorRefToVector3Single(Struct.rgbResult);
end;

{ TGLWindowsManager ----------------------------------------------- }

procedure TGLWindowsManager.CreateImplDependent;
var WindowClass: TWndClass;
begin
 appActive := true;

 FMouseVisible := true;

 { create window class (WindowClass) }

 { no need to worry about multiple applications
   registering same WndClassName because window classes are
   process-specific (so, if other process registers another
   GLWindow class, it will be just a different class -
   distinguished by the hInstance parameter of TWndClass and
   CreateWindowEx). }
 WndClassName := 'GLWindow';

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
 KambiOSCheck( RegisterClass(WindowClass) <> 0, 'RegisterClass');
end;

procedure TGLWindowsManager.DestroyImplDependent;
begin
 KambiOSCheck( UnregisterClass(PChar(wndClassName), hInstance), 'UnregisterClass');
end;

procedure TGLWindowsManager.VideoReset;
begin
 { switch back to the desktop mode }
 if DisplaySettingsChanged then ChangeDisplaySettings(nil, 0);
end;

function TGLWindowsManager.TryVideoChange: boolean;

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

var dmScreenSettings: Devmode;
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

procedure TGLWindowsManager.SetAppActive(value: boolean);
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

function TGLWindowsManager.ScreenWidth: integer;
begin
 if DisplaySettingsChanged and VideoResize then
  result := VideoResizeWidth else
  result := GetSystemMetrics(SM_CXSCREEN);
end;

function TGLWindowsManager.ScreenHeight: integer;
begin
 if DisplaySettingsChanged and VideoResize then
  result := VideoResizeHeight else
  result := GetSystemMetrics(SM_CYSCREEN);
end;

function TGLWindowsManager.ImplementationName: string;
begin
 Result := 'WinAPI';
end;

procedure TGLWindowsManager.SetMouseVisible(const Value: boolean);
begin
  if FMouseVisible <> Value then
  begin
    FMouseVisible := Value;
    { We must call ShowCursor only when MouseVisible value changes.
      That's because ShowCursor has internal counter of calls
      (inc of calling with true, dec on calling with false),
      and we want to avoid this counter (because calling
        MouseVisible := true;
        MouseVisible := true;
        MouseVisible := false;
      should hide the cursor. In other words, calling
      MouseVisible := true more than once doesn't change anything.) }
    ShowCursor(MouseVisible);
  end;
end;

{$endif read_implementation}