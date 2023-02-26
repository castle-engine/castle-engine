{%MainUnit ../castlewindow.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

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
    Is it even possible to cleanly catch VK_Alt key press in WinAPI?
    We would have to catch sys_keydown message but then we also
    block using standard Alt+F4 or Alt+Space? Another trouble:
    if you enter system menu by Alt+Down, we will not get Alt+Up?

  - Implement MainMenu.Enabled.

  Note: We use Unicode version of WinAPI (W suffix) throughout the code.

  - Why?
    Following https://stackoverflow.com/questions/33714546/winapi-unicode-and-ansi-functions ,
    this is available on all Windowses after Windows 95, 98 and ME,
    and it is advised to use now (as the "A" routines just call "W" routines doing conversion underneath),
    and using it is easier on Delphi (where String = UnicodeString),
    and using it with both FPC and Delphi makes code simpler.

  - Note: LCL win32 widgetset (used on 64-bit Windows too) still uses ANSI.
}

{$ifdef OpenGLES}
  {$I castlewindow_egl.inc}
{$else}
  {$I castlewindow_wgl.inc}
{$endif}

{$ifdef read_interface_uses}
{ note: this unit's definitions conflict with other definitions used in this file
  But we need it for RegisterDeviceNotification, therefore adding it to the very
  first position in the uses list to be overwritten by subsequent used units }
{$ifdef FPC}
JwaWinUser, JwaDbt,
{$else}
Winapi.Messages, Character,
{$endif}
Imm,
{ /note }
Windows, CommDlg,
{$endif}

{$ifdef read_implementation_uses}
CastleInternalWindowModes, CastleMessages, CastleJoysticks, CastleUnicode,
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

  function WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

  { convert virtual key code (VK_xxx) to TKey. Returns keyNone if no conversion
    available. }
  function VirtualKeyToKey(VirtualKey: Byte): TKey;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  { Should we do ChangeDisplaySettings(nil, 0) when Destroy? }
  DisplaySettingsChanged: boolean;
  WndClassName: UnicodeString;

  { A private array, initialized in CreateBackend.
    Having all cursor handles loaded makes sure that WM_SETCURSOR is able to
    use predefined HCURSOR values and so is fast. SetCursor is guaranteed
    by WinAPI to return fast if called with already used cursor.

    For mcDefault, value in this array is undefined.
    (but 0 in practice, for now).

    For mcNone and CursorHandles, CursorHandle is guaranteed 0
    (this is what SetCursor always accepts as invisible cursor). }
  CursorHandles: array [TMouseCursor] of record
    Handle: HCURSOR;
  end;

  fappActive: boolean;
  procedure SetAppActive(value: boolean);
  property appActive: boolean read fappActive write SetAppActive;
{$endif read_application_interface}

{$ifdef read_implementation}

{$I castlewindow_winapi_menu.inc}
{$I castlewindow_winapi_sendinput.inc}

{ Describe ChangeDisplaySettings result. }
function DispChangeToStr(L: LongInt): string;
begin
 case L of
  DISP_CHANGE_SUCCESSFUL: Result := 'DISP_CHANGE_SUCCESSFUL';
  DISP_CHANGE_RESTART: Result := 'DISP_CHANGE_RESTART';
  DISP_CHANGE_BADFLAGS: Result := 'DISP_CHANGE_BADFLAGS';
  DISP_CHANGE_FAILED: Result := 'DISP_CHANGE_FAILED';
  DISP_CHANGE_BADMODE: Result := 'DISP_CHANGE_BADMODE';
  DISP_CHANGE_NOTUPDATED: Result := 'DISP_CHANGE_NOTUPDATED';
  else Result := IntToStr(L);
 end;
end;

{ WMSizing ------------------------------------------------------------------- }

type
  { Handling WMSizing message is a way to force minimum/maximum form
    sizes under WinAPI. }
  TWMSizing = record
    Msg: Cardinal;    {< wm_SIZING }
    fwSide: Longint;  {< const WMSZ_xxx }
    lprc: PRect;      {< rectangle with window sizes }
    Result: LongBool; {< should return longbool(true) }
  end;

procedure WMSizingHandler(var Msg:TWMSizing;
  const minWidth, minHeight, maxWidth, maxHeight:integer); overload;
var
  w, h:integer;
begin
  assert(minWidth<=maxWidth);
  assert(minHeight<=maxHeight);
  with msg.lprc^ do
  begin
    w := right-left;
    if w<minWidth then right := left+minWidth else
      if w>maxWidth then right := left+maxWidth;
    h := bottom-top;
    if h<minHeight then bottom := top+minHeight else
      if h>maxHeight then bottom := top+maxHeight;
  end;
  msg.result := true;
end;

function WMSizingHandler(const lParm:LPARAM; const wParm:WPARAM;
  const minWidth, minHeight, maxWidth, maxHeight:integer):LRESULT; overload;
var
  msg: TWMSizing;
begin
  msg.Msg := WM_SIZING;
  msg.fwSide := wParm;
  msg.lprc := PRect(lParm);
  WMSizingHandler(msg, minWidth, minHeight, maxWidth, maxHeight);
  result := LRESULT(msg.Result);
end;

{ TCastleWindow ------------------------------------------------- }

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

{ convert virtual key code (VK_xxx) to TKey. Returns keyNone if no conversion
  available. }
function TCastleWindow.VirtualKeyToKey(VirtualKey: Byte): TKey;
var
  VirtualKeyToKeyResult: TKey absolute Result;

  function VK_Stored(VK_Exists: boolean; VK_Value: Byte; KeyValue: TKey): boolean;
  begin
    Result := VK_Exists and (VirtualKey = VK_Value);
    if Result then
      VirtualKeyToKeyResult := KeyValue;
  end;

begin
  { Tests: InfoWrite(Format('key %d', [VirtualKey])); }

  case VirtualKey of
    VK_BACK: Result := keyBackSpace;
    VK_TAB: Result := keyTab;
    VK_RETURN: Result := keyEnter;
    VK_SHIFT: Result := keyShift;
    VK_CONTROL: Result := keyCtrl;
    VK_MENU: Result := keyAlt;
    VK_ESCAPE: Result := keyEscape;
    VK_SPACE: Result := keySpace;
    VK_PRIOR: Result := keyPageUp;
    VK_NEXT: Result := keyPageDown;
    VK_END: Result := keyEnd;
    VK_HOME: Result := keyHome;
    VK_LEFT: Result := keyArrowLeft;
    VK_UP: Result := keyArrowUp;
    VK_RIGHT: Result := keyArrowRight;
    VK_DOWN: Result := keyArrowDown;
    VK_INSERT: Result := keyInsert;
    VK_DELETE: Result := keyDelete;
    VK_ADD: Result := keyNumpadPlus;
    VK_SUBTRACT: Result := keyNumpadMinus;
    VK_SNAPSHOT: Result := keyPrintScreen;
    VK_NUMLOCK: Result := keyNumLock;
    VK_SCROLL: Result := keyScrollLock;
    VK_CAPITAL: Result := keyCapsLock;
    VK_PAUSE: Result := keyPause;
    VK_OEM_COMMA: Result := keyComma;
    VK_OEM_PERIOD: Result := keyPeriod;
    VK_NUMPAD0: Result := keyNumpad0;
    VK_NUMPAD1: Result := keyNumpad1;
    VK_NUMPAD2: Result := keyNumpad2;
    VK_NUMPAD3: Result := keyNumpad3;
    VK_NUMPAD4: Result := keyNumpad4;
    VK_NUMPAD5: Result := keyNumpad5;
    VK_NUMPAD6: Result := keyNumpad6;
    VK_NUMPAD7: Result := keyNumpad7;
    VK_NUMPAD8: Result := keyNumpad8;
    VK_NUMPAD9: Result := keyNumpad9;
    VK_CLEAR: Result := keyNumpadBegin;
    VK_MULTIPLY: Result := keyNumpadMultiply;
    VK_DIVIDE: Result := keyNumpadDivide;
    VK_OEM_MINUS: Result := keyMinus;
    VK_OEM_PLUS: Result := keyPlus;

    Ord('0') .. Ord('9'): Result := TKey(Ord(key0)  + Ord(VirtualKey) - Ord('0'));
    Ord('A') .. Ord('Z'): Result := TKey(Ord(keyA)  + Ord(VirtualKey) - Ord('A'));
    VK_F1    .. VK_F12  : Result := TKey(Ord(keyF1) + Ord(VirtualKey) - Ord(VK_F1));
    else
      if not VK_Stored(VK_LeftBracket_Exists , VK_LeftBracket , keyLeftBracket ) then
      if not VK_Stored(VK_RightBracket_Exists, VK_RightBracket, keyRightBracket) then
      if not VK_Stored(VK_Apostrophe_Exists  , VK_Apostrophe  , keyApostrophe  ) then
      if not VK_Stored(VK_Semicolon_Exists   , VK_Semicolon   , keySemicolon   ) then
      if not VK_Stored(VK_Slash_Exists       , VK_Slash       , keySlash       ) then
      if not VK_Stored(VK_BackQuote_Exists   , VK_BackQuote   , keyBackQuote   ) then
      if not VK_Stored(VK_BackSlash_Exists   , VK_BackSlash   , keyBackSlash   ) then
        Result := keyNone;
  end;

  { Note that CastleWindow WinAPI will never generate Press/Release with these keys:
      keyNumpadEnd;
      keyNumpadDown;
      keyNumpadPageDown;
      keyNumpadLeft;
      keyNumpadRight;
      keyNumpadHome;
      keyNumpadUp;
      keyNumpadPageUp;
      keyNumpadInsert;
      keyNumpadDelete;
      keyNumpadEnter;
    because (as far as I can see) they are undistinguishable from normal,
    non-numpad key codes under WinAPI.

    Note that it seems that VK_SNAPSHOT (keyPrintScreen) is never passed to
    WM_KEYDOWN (although it's passed to WM_KEYUP; but there we ignore it,
    because in our Keys[] table it's already up).
    So you will never get KeyDown/Up with keyPrintScreen. }
end;

function TryHandlingMessage(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM): boolean;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow(GetWindowLongPtr(hWnd, GWL_USERDATA));
  Result := Application.FindWindow(Window) >= 0;
  if Result then
    Window.WindowProc(uMsg, wParm, lParm);
end;

function WndProc(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM) :LRESULT; stdcall;
var
  Window: TCastleWindow;
begin
  Window := TCastleWindow(GetWindowLongPtr(hWnd, GWL_USERDATA));
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
    result := Window.WindowProc(uMsg, wParm, lParm)
  else
    result := DefWindowProcW(hWnd, uMsg, wParm, lParm);
end;

function TCastleWindow.WindowProc(uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT;

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
    { This is useful to deconstruct WParam and LParam.
      It will work faster than any LoWord/HiWord functions in Windows unit,
      it avoids any range-check errors,
      it allows to specify signedness of values as you wish. }
    TWinParam = packed record
      case Integer of
        0:(Signed: LongInt {$if defined(cpu64)}; Dummy: LongWord {$endif});
        1:(LoWord, HiWord: Word);
        2:(LoSmallint, HiSmallint: Smallint);
        3:(LowestByte, LowerByte, HigherByte, HighestByte: byte);
    end;

  function InterceptedSystemKey(key: integer): boolean;
  begin
    Result := { TODO-alt: (key = VK_Alt) or }(key = VK_F10)
  end;

  { Convert WM_CHAR's wParam value to a String in UTF-8 encoding,
    corresponding to key pressed. Empty if none. }
  function ConvertWmCharToString(const CharMsgWParam: WPARAM): String;
  begin
    { According to WM_CHAR docs, wParam is already a single character in UTF-16 encoding.
      This is true if we peeked it using PeekMessageW, not PeekMessage.
      So just convert it using UTF8Encode.

      Notes:

      - Doing "Exit(UnicodeToUTF8(WParm))" also seems to work for Polish chars.
        But it's not necessarily correct, I think: UTF-16 character number
        is not necessarily equal to just Unicode number.

      - At one point we did here

          MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @CharMsgWParam, SizeOf(CharMsgWParam), ...)

        But this was a hack: we treated CharMsgWParam as string
        (4 bytes, so 2 characters) in local Windows ANSI code page.
        We then hackishly ignored MultiByteToWideChar result,
        and used first returned WideChar.
    }
    {$ifdef FPC}
    Exit(UTF8Encode(WideChar(CharMsgWParam)));
    {$else}
    Exit(WideChar(CharMsgWParam));
    {$endif}
  end;

  { Handle WinAPI WM_KEYDOWN message by calling DoKeyDown method.
    We also handle (peek and remove) WM_CHAR message here. }
  procedure Handle_WM_KEYDOWN;

  { Note that WM_KEYDOWN is repeated when the user holds the key down.
    The frequency of "key repeat" depends on user settings.

    And... that's completely OK. We still pass each WM_KEYDOWN to DoKeyDown.
    This way our API (TCastleUserInterface.Press, TCastleWindow.OnPress)
    will generate repeated key presses.
    This is documented and desired (e.g. for implementing UI like edit box).
    User can check TInputPressRelease.KeyRepeated that is calculated
    elsewhere. (We could also calculate TInputPressRelease.KeyRepeated here,
    looking at KEY_ALREADY_DOWN bit in lParm, but it's not needed for now.)
  }

  var
    Key: TKey;
    CharMsg: TMsg;
    KeyString: string;
  begin
    { WM_CHAR is placed in our message queue by TranslateMessage.
      Always in WM_KEYDOWN we do this PeekMessageW(..., WM_CHAR, WM_CHAR, PM_REMOVE).
      This means that now we have
      1. no WM_CHAR messages in our message queue (which means that this
         WM_KEYDOWN message is not translatable as WM_CHAR) OR
      2. we have exactly one WM_CHAR message in our queue, and this is WM_CHAR
         message corresponding to current WM_KEYDOWN message.

      Note PeekMessageW, not PeekMessage.
      This is necessary to receive UTF-16 for e.g. Polish chars.
      Otherwise WM_CHAR gets characters in ANSI encoding.
    }

    if PeekMessageW({$ifdef FPC}@{$endif}CharMsg, h_Wnd, WM_CHAR, WM_CHAR, PM_REMOVE) then
      KeyString := ConvertWmCharToString(CharMsg.wParam)
    else
      KeyString := '';

    Key := VirtualKeyToKey(wParm);

    { We cannot depend that VK_OEM_4 is always "[" and VK_OEM_6 is always "]",
      see http://www.tronan.com/macromachine/scripthelp/VK.htm.
      On non-US keyboards, other keys may correspond to them.

      So below we do a little trick: we check which virtual key corresponds to
      left/right brackets, and we remember it. If my VirtualKeyToKey returned
      keyNone for this virtual key, then we can safely assume that in the future
      (e.g. for next WM_KEYUP for this key) this virtual key always corresponds
      to left/right bracket. }
    if Key = keyNone then
    begin
      if KeyString = '[' then
        begin VK_LeftBracket_Exists  := true; VK_LeftBracket  := wParm; Key := keyLeftBracket ; end
      else if KeyString = ']' then
        begin VK_RightBracket_Exists := true; VK_RightBracket := wParm; Key := keyRightBracket; end
      else if KeyString = '''' then
        begin VK_Apostrophe_Exists   := true; VK_Apostrophe   := wParm; Key := keyApostrophe  ; end
      else if KeyString = ';' then
        begin VK_Semicolon_Exists    := true; VK_Semicolon    := wParm; Key := keySemicolon   ; end
      else if KeyString = '/' then
        begin VK_Slash_Exists        := true; VK_Slash        := wParm; Key := keySlash       ; end
      else if KeyString = '`' then
        begin VK_BackQuote_Exists    := true; VK_BackQuote    := wParm; Key := keyBackQuote   ; end
      else if KeyString = '\' then
        begin VK_BackSlash_Exists    := true; VK_BackSlash    := wParm; Key := keyBackSlash   ; end;
    end;

    { Fix some cases when KeyString should better correspond to Key }
    if (KeyString = '') and (Key = keyDelete) then
      KeyString := CharDelete;

    if (Key <> keyNone) or (KeyString <> '') then DoKeyDown(Key, KeyString);
  end;

  { Handle WinAPI WM_KEYUP message by calling DoKeyUp method. }
  procedure Handle_WM_KEYUP;
  var
    Key: TKey;
  begin
    Key := VirtualKeyToKey(wParm);
    if Key <> keyNone then DoKeyUp(Key);
  end;

  { Handle WM_CHAR that arrived without WM_KEYDOWN.

    - In case of input in Western languages (English, Polish etc.):

      This should never happen.

      The only way we can get WM_CHAR message is when we get WM_KEYDOWN
      message and TranslateMessage (called from castlewindow_winsystem.inc)
      translates it to WM_CHAR message.
      But actually always in WM_KEYDOWN handler we do
      PeekMessageW(..., WM_CHAR, WM_CHAR, PM_REMOVE) so all WM_CHAR messages
      are removed from queue immediately after they are put there by TranslateMessage.

      So no WM_CHAR message should be ever dispatched by DispatchMessage.
      So no WM_CHAR message should ever get here.

    - In case of Chinese, where input arrives only through WM_CHAR,
      this can definitely happen.

      For Chinese you have a special window that allows to compose Chinese characters.
      Looks like they don't result in WM_KEYDOWN, only WM_CHAR.
      Tested with simplified Chinese input on Windows 10.
      Note: It does *not* make WM_UNICHAR (regardless if we register using RegisterClassW or ANSI version).

    - A minor thing about Wine compatibility:
      It seems wine-1.5.30-1 lets WM_CHAR arrive without WM_KEYDOWN
      in one special case too: when pressing escape to unfocus menu bar.
  }
  procedure Handle_WM_CHAR;
  var
    KeyString: String;
  begin
    KeyString := ConvertWmCharToString(WParm);
    if KeyString <> '' then
      DoKeyDown(keyNone, KeyString);
    WritelnLog('Received WM_CHAR. Test information for https://github.com/castle-engine/castle-engine/issues/159 : WParam ' + IntToStr(WParm) + ' string: ' + {$ifdef FPC}UTF8ToHtmlEntities({$endif}KeyString{$ifdef FPC}){$endif});
  end;

  { Handle WM_UNICHAR that reports a key press as 32-bit Unicode character.

    WM_UNICHAR seems to never happen in practice.

    See also
    https://stackoverflow.com/questions/378296/why-is-my-wm-unichar-handler-never-called
    https://stackoverflow.com/questions/5844689/winapi-how-to-process-keyboard-input-in-custom-edit-control

    """
    - WM_KEYDOWN is sent to the window with the focus when a non-system key has been pressed. When the message is translated by the TranslateMessage function, a WM_CHAR message is sent to the window.
    - WM_CHAR uses UTF-16.
    - WM_UNICHAR is similat to WM_CHAR, except that it uses UTF-32.

      It's purpose is to send/post Unicode characters to ANSI windows.
      If the window is ANSI (created with CreateWindowA), when WM_CHAR is generated.
      If it is Unicode (created with CreateWindowW) then WM_UNICHAR is generated.

      So your control should probably handle both.
      Other applications can send us WM_UNICHAR, according to WinAPI docs.
    """

    Testing in practice: I didn't find a way to cause WM_UNICHAR in either
    ANSI or Unicode window (created with CreateWindowExW).
  }
  procedure Handle_WM_UNICHAR;
  var
    KeyString: String;
  begin
    WritelnLog('Received WM_UNICHAR. This may work but is UNTESTED in CGE (please post a report if you know how to cause this message). Test information for https://github.com/castle-engine/castle-engine/issues/159 : WParam ' + IntToStr(WParm));
    if wParm <> UNICODE_NOCHAR then // ignore UNICODE_NOCHAR, which is used only to test do we handle WM_UNICHAR
    begin
      { Treat UTF-32 character code as just Unicode code.
        This is the same: https://en.wikipedia.org/wiki/UTF-32,
        """Each 32-bit value in UTF-32 represents one Unicode code point and is exactly equal to that code point's numerical value. """
      }
      KeyString := {$ifdef FPC}UnicodeToUTF8(WParm){$else}ConvertFromUtf32(WParm){$endif};
      if KeyString <> '' then
        DoKeyDown(keyNone, KeyString);
    end;
  end;

  function Handle_IME: Boolean;
    function ToPoint(const V: TVector2): TPoint;
    begin
      Result.X := Floor(V.X);
      Result.Y := Floor(Container.Height - V.Y);
    end;
    function GetCaretPos(var CaretPos: TPoint): Boolean;
    var
      CastleEdit: TCastleEdit;
      CaretShift: Single;
      S: string;
    begin
      Result := false;
      { TODO: Handles all input controls, not just TCastleEdit }
      if (Container.Focus.Count <> 0) and
         (Container.Focus.Last is TCastleEdit) then
      begin
        CastleEdit := Container.Focus.Last as TCastleEdit;
        S := CastleEdit.Text;
        if CastleEdit.PasswordChar <> #0 then
          S := StringOfChar(CastleEdit.PasswordChar, Length(S));
        CaretShift := CastleEdit.Font.TextWidth(S);
        CaretPos := ToPoint(CastleEdit.LocalToContainerPosition(Vector2(CaretShift / CastleEdit.UIScale, 0)));
        Result := true;
      end;
    end;

  var
    vhIMC: HIMC;
    vSize: Integer;
    vBuffer: TBytes;
    KeyString: UnicodeString;
    //vLogFont: TLogFont;
    vCF: TCompositionForm;
    Pt: TPoint;
  begin
    Result := false;
    case uMsg of
      WM_IME_STARTCOMPOSITION, WM_IME_ENDCOMPOSITION:
        begin
          { adjusted composition window's position and font/caret size }
          vhIMC := ImmGetContext(h_Wnd);
          if vhIMC = 0 then Exit;
          try
            ImmSetOpenStatus(vhIMC, true);
            { TODO: Set the LogFont size }
            {
            ImmGetCompositionFont(vhIMC, @vLogFont);
            vLogFont.lfHeight := Font.Height;
            ImmSetCompositionFont(vhIMC, @vLogFont);
            }
            if not GetCaretPos(Pt) then
            begin
              GetCursorPos(Pt);
              ScreenToClient(h_Wnd, Pt);
              Pt.Offset(0, 25);
            end;
            vCF.ptCurrentPos := Pt;
            vCF.dwStyle := CFS_RECT;
            vCF.rcArea := TRect.Create(0, 0, Width, Height);
            ImmSetCompositionWindow(vhIMC, @vCF);
          finally
            ImmReleaseContext(h_Wnd, vhIMC);
          end;
        end;
      WM_IME_COMPOSITION:
        begin
          if (LParm and GCS_RESULTSTR) <> 0 then
          begin
            vhIMC := ImmGetContext(h_Wnd);
            if vhIMC = 0 then Exit;
            try
              vSize := ImmGetCompositionStringW(vhIMC, GCS_RESULTSTR, nil, 0);
              if vSize > 0 then
              begin
                SetLength(vBuffer, vSize);
                ImmGetCompositionStringW(vhIMC, GCS_RESULTSTR, @vBuffer[0], vSize);
                KeyString := WideStringOf(vBuffer);
                if KeyString <> '' then
                begin
                  DoKeyDown(keyNone, KeyString);
                end;
              end;
            finally
              ImmReleaseContext(h_Wnd, vhIMC);
            end;
            Result := true;
          end;
        end;
    end;
  end;

  procedure HandleMouseDown(const Button: TCastleMouseButton);
  begin
    MaybeCapture;
    DoMouseDown(LeftTopToCastle(
      TWinParam(lParm).LoSmallint,
      TWinParam(lParm).HiSmallint), Button, 0);
  end;

  procedure HandleMouseUp(const Button: TCastleMouseButton);
  begin
    DoMouseUp(LeftTopToCastle(
      TWinParam(lParm).LoSmallint,
      TWinParam(lParm).HiSmallint), Button, 0);
    MaybeRelease;
  end;

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
        if TWinParam(wParm).LoWord = WA_INACTIVE then
          { When user switches to another program, fake that we release all keys/mouse.
            Otherwise we could miss some key up / mouse up, when user releases it over
            another program/window.
            Do not do this when InCloseBackend, as ReleaseAllKeysAndMouse causes
            some DoKeyUp/DoMouseUp (and these even cause BackendMakeCurrent), and no DoXxx
            should be done when the window is closing and releasing it's resources. }
          ReleaseAllKeysAndMouse;
      { Don't Exit, let control flow to DefWindowProcW. }
    WM_ACTIVATEAPP:
      begin
        Application.appActive := longbool(wParm);
        {exit; <- allow DefWindowProcW handle WM_ACTIVETAPP too,
         since you can't say "we surely handled everything that should be done in
         reaction for this message"}
      end;
    WM_CLOSE: begin DoCloseQuery; Exit end;

    WM_KEYDOWN: begin Handle_WM_KEYDOWN; Exit end;
    WM_KEYUP: begin Handle_WM_KEYUP; Exit end;
    WM_CHAR: begin Handle_WM_CHAR; Exit end;
    WM_IME_STARTCOMPOSITION,
    WM_IME_ENDCOMPOSITION,
    WM_IME_COMPOSITION:
      begin
        if Handle_IME then Exit(0);
      end;
    // WM_UNICHAR answers true, and doesn't call DefWindowProcW, because we handled the message
    WM_UNICHAR: begin Handle_WM_UNICHAR; Exit(1); end;

    WM_SIZE:
      if not InCloseBackend then
      begin
        { Note: Window class has (VREDRAW or HREDRAW), so Windows will automatically
          take care of requesting window redraw after window resize. }
        if (wParm = SIZE_MAXIMIZED) or (wParm = SIZE_RESTORED) then
          DoResize(TWinParam(lParm).LoWord, TWinParam(lParm).HiWord, false);
        Exit;
      end;
    WM_MOVE:
       { For some reason, you need to call glViewport at WM_MOVE
         (when window moved) on Windows.
         Otherwise moving a window, and doing glRedPixels, reads pixels
         at *old window placement*. }
      begin
        fLeft := TWinParam(lParm).LoSmallint;
        fTop := TWinParam(lParm).HiSmallint;

        { We need a hack in case of FullScreen and menu.

          We want to update Left and Top below to the position
          on the screen of our client area. "Client area" means tha place where OpenGL
          context is --- it's the window without the border, title bar and menu bar
          (if MainMenu <> nil). We need this, because

          1. Left and Top are said in unit's interface to work like that

          2. SetMousePosition requires this. Otherwise positions
             set by SetMousePosition and read by MousePosition properties
             would be slightly different.
             And there are cases when it is important that they use
             exactly the same coordinate systems --- e.g. mouse look handling.

          When the window is not FullScreen, WinAPI behaves nice,
          and Left and Top as recorded in lParm are exactly what I want.

          But when the window is FullScreen, Left and Top
          are always 0, 0 --- which means that they don't take menu bar height into
          account. The effect: mouse look + menu bar + fullscreen mode don't work.

          We workaround this by adding CYMENU. This is not perfect, as this assumes
          that menu bar will always be a single line.
          (This is also non-perfect anyway, because WM_MOVE message may not get to
          FullScreen window at all, so in OpenBackend I also do this trick.) }
        if FullScreen and (MainMenu <> nil) and MainMenuVisible then
          FTop := FTop + GetSystemMetrics(SM_CYMENU);

        { TODO: Only position actually changed, not size.
          Do we really need to call DoResize, OnResize?
          Maybe we should just call only glViewport,
          using current RenderContext.Viewport? }
        DoResize(fRealWidth, fRealHeight, false);
        Exit;
      end;
    WM_SIZING:
      begin
        result := WMSizingHandler(lParm, wParm, minWidth, minHeight, maxWidth, maxHeight);
        Exit;
      end;
    { We do not capture WM_PAINT, let WM_PAINT fallback to DefWindowProcW.
      WM_PAINT occurs also when there's a need to redraw window frame
      (which we don't care about, we don't draw custom window frame).

      If there's a need to actually redraw window contents,
      then WM_PAINT will cause (immediately, it seems that DefWindowProcW
      will just call WndProc) WM_ERASEBKGND, and we want to capture WM_ERASEBKGND. }
    WM_ERASEBKGND:
      begin
        if IsWindowEnabled(H_Wnd) then
          Invalidate else
          { If not IsWindowEnabled then we have a modal window blocking our window.
            Like a Windows.MessageBox. In this case, our Application.ProcessMessage
            may not be continuously called, so we cannot use normal Invalidate
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
            FMousePressed := [];
            if (MK_LBUTTON and wParm) <> 0 then Include(FMousePressed, buttonLeft);
            if (MK_MBUTTON and wParm) <> 0 then Include(FMousePressed, buttonMiddle);
            if (MK_RBUTTON and wParm) <> 0 then Include(FMousePressed, buttonRight);
          but it's not needed (we keep it current anyway in mousedown/up events). }
        DoMotion(InputMotion(MousePosition, LeftTopToCastle(
          TWinParam(lParm).LoSmallint,
          TWinParam(lParm).HiSmallint),
          MousePressed, 0));
        Exit;
      end;
    WM_LBUTTONDOWN: begin HandleMouseDown(buttonLeft  ); Exit end;
    WM_MBUTTONDOWN: begin HandleMouseDown(buttonMiddle); Exit end;
    WM_RBUTTONDOWN: begin HandleMouseDown(buttonRight ); Exit end;
    WM_LBUTTONUP  : begin HandleMouseUp(buttonLeft  ); Exit end;
    WM_MBUTTONUP  : begin HandleMouseUp(buttonMiddle); Exit end;
    WM_RBUTTONUP  : begin HandleMouseUp(buttonRight ); Exit end;
    WM_COMMAND    :
      { If this comes from a menu item, call DoMenuClick }
      if TWinParam(wParm).HiWord = 0 then
      begin
        DoMenuClick(MenuItemFromSmallId(TWinParam(wParm).LoWord));
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
      changes. If no change, SetCursor returns immediately. So, we feel a little safer
      that we use CursorHandles[Cursor].Handle, so when Cursor stays the same, SetCursor
      is called with exactly the same arguments. }
    WM_SETCURSOR:
      begin
        if (WParm = h_Wnd) and
           ( (InternalCursor in [mcNone, mcForceNone]) or
             (Application.CursorHandles[InternalCursor].Handle <> 0) ) then
        begin
          Windows.SetCursor(Application.CursorHandles[InternalCursor].Handle);
          Exit(1);
        end;
        { Otherwise we fall back on DefWindowProcW.
          It will install our window class cursor (or parent cursor)
          if in client area, or just do the right thing if outside client area.
          So it's suitable when wParm <> h_Wnd,
          or we want mcDefault. }
      end;

    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      begin
        if TWinParam(WParm).HiSmallInt <> 0 then
          DoMouseWheel(TWinParam(WParm).HiSmallInt / 120, uMsg = WM_MOUSEWHEEL);
        Exit;
      end;

    { Event received when something in Windows configuration had been changed,
      most often this means connecting/disconnecting plug-n-play devices.
      Here we use it to detect DBT_DEVICEARRIVAL and DBT_DEVICEREMOVECOMPLETE events
      which correspond to adding or removing devices and suggest those are
      caused by connecting/disconnecting a Joystick from the system. }
    WM_DEVICECHANGE:
      begin
        case WParm of
          { A device has been sucessfully added to the system }
          DBT_DEVICEARRIVAL:
            begin
              WriteLnLog('Received a WM_DEVICECHANGE event with DBT_DEVICEARRIVAL');
              //it might be a joystick, therefore
              Joysticks.InternalConnected;
            end;
          { A device has been successfully removed from the system.
            Currently we more reliably handle this case in Joystick backend Poll
            because DBT_DEVICEREMOVECOMPLETE is sent by many different devices
            e.g. such as USB flash drive and it isn't right to treat all of these
            as disconnected joysticks. E.g. according to Steam recommendations
            the game should be set to pause when the joystick is disconnected,
            but forcing pause mode every time some arbitrary device has disconnected
            may be inconsistent and look like a bug to the user.
            The backend does this job better:
            1. The InternalDisconnected is sent immediately after the joystick has
               been disconnected, while WinAPI message comes with a delay ~0.5 seconds.
            2. Backend Poll guarantees that disconnect event comes from a previously
               connected joystick, not from any device.
            3. Backend Poll guarantees to send the InternalDisconnected event when
               joystick stopped working properly, while WinAPI behavior may depend
               on joystick driver implementation. }
          {DBT_DEVICEREMOVECOMPLETE:
            begin
              WriteLnLog('Received a WM_DEVICECHANGE event with DBT_DEVICEREMOVECOMPLETE');
              //it might be a joystick, therefore
              Joysticks.InternalDisconnected;
            end;}
          { Something unspecified has changed in the system devices configuration
            We don't need to handle this case because we are subscribed to more specific
            joystick connect/disconnect events through RegisterDeviceNotification. }
          //DBT_DEVNODES_CHANGED: WriteLnLog('Received a WM_DEVICECHANGE event with DBT_DEVNODES_CHANGED');
        end;
        Exit(1);
      end;
  end;

  { The DefWindowProcW may enter a modal loop in some cases, e.g. when
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
    { Note: It is extremely important to use here DefWindowProcW.
      If you use (ANSI, in FPC) DefWindowProc, then Windows will detect that
      we want to use ANSI API (with 8-bit strings, not 16-bit)
      which will make caption provided to CreateWindowExW expected to be encoded
      as 8-bit (passing there a pointer to UTF-16 will be interpreted wrong,
      as just 1 single letter). }
    Result := DefWindowProcW(h_Wnd, uMsg, wParm, lParm);
  finally
    if DoEnableUpdatesFromWindowProc then Dec(EnableUpdatesFromWindowProc);
    if DoDisableUpdatesFromWindowProc then Dec(DisableUpdatesFromWindowProc);
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.BackendMenuFinalize;
var
  Old_H_Menu: HMenu;
begin
  Old_H_Menu := GetMenu(H_Wnd);
  OSCheck( DestroyMenu(Old_H_Menu), 'DestroyMenu(Old_H_Menu)');
  { for now, Handle of our menu items are always zero.
    But do this for the future. }
  MainMenu.ClearHandles;
end;

procedure TCastleWindow.BackendMenuInitialize;
begin
  OSCheck( SetMenu(H_Wnd, MakeWinapiMenu(MainMenu, true)), 'SetMenu');
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: unoptimal }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.OpenBackend;

  // Get initial MousePosition, even before the first mouse move/down event.
  procedure GetInitialCursorPos(const ClientRect: TRect);
  var
    InitialCursorPos: Windows.TPoint;
  begin
    if GetCursorPos({$ifdef FPC}@{$endif}InitialCursorPos) then
    begin
      FMousePosition := LeftTopToCastle(
        InitialCursorPos.X - ClientRect.Left,
        InitialCursorPos.Y - ClientRect.Top);
    end;
  end;

  procedure InitializeDpi;
  begin
    FDpi := GetDeviceCaps(h_Dc, LOGPIXELSX);
    // Too verbose by default, UpdateUIScale will show it if UIScaling is usDpiScale.
    // WritelnLog('WinAPI', 'Dots (pixels) per inch is %f (%f * default %f)',
    //   [FDpi, FDpi / DefaultDpi, DefaultDpi]);
  end;

  { Request WinAPI to send DBT_DEVICEARRIVAL and DBT_DEVICEREMOVECOMPLETE events
    when new Joysticks have been connected to the system }
  procedure RegisterForJoystickEvents;
  var
    BroadcastDeviceInterface: DEV_BROADCAST_DEVICEINTERFACE;
    GUID_DEVINTERFACE_HID: TGuid;
    DeviceNotification: Pointer; // HDEVNOTIFY;
  begin
    { Construct GUID that corresponds to Joysticks/controllers devices
      The values have been taken from SDL2 SDL_windowsjoystick.c }
    GUID_DEVINTERFACE_HID.D1 := $4D1E55B2;
    GUID_DEVINTERFACE_HID.D2 := $F16F;
    GUID_DEVINTERFACE_HID.D3 := $11CF;
    GUID_DEVINTERFACE_HID.D4[0] := $88;
    GUID_DEVINTERFACE_HID.D4[1] := $CB;
    GUID_DEVINTERFACE_HID.D4[2] := 0;
    GUID_DEVINTERFACE_HID.D4[3] := $11;
    GUID_DEVINTERFACE_HID.D4[4] := $11;
    GUID_DEVINTERFACE_HID.D4[5] := 0;
    GUID_DEVINTERFACE_HID.D4[6] := 0;
    GUID_DEVINTERFACE_HID.D4[7] := $30;

    //construct BroadcastDeviceInterface record
    FillChar(BroadcastDeviceInterface, SizeOf(BroadcastDeviceInterface), 0);
    BroadcastDeviceInterface.dbcc_size := sizeof(DEV_BROADCAST_DEVICEINTERFACE);
    BroadcastDeviceInterface.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
    BroadcastDeviceInterface.dbcc_classguid := GUID_DEVINTERFACE_HID;
    { Ask WinAPI to send DBT_DEVICEARRIVAL and DBT_DEVICEREMOVECOMPLETE events
      for the requested GUID }
    DeviceNotification := RegisterDeviceNotification(h_Wnd, @BroadcastDeviceInterface, DEVICE_NOTIFY_WINDOW_HANDLE);
    if not Assigned(DeviceNotification) then
      WriteLnWarning('Unable to RegisterDeviceNotification. Connecting/disconnecting joysticks will not be reported.');
  end;

var
  dwExStyle, dwStyle: dword;
  WindowRect: TRect;
  H_Menu: HMenu;
  ClientRect: TRect;
begin
  { optionally hide mouse in FullScreem,
    set dwStyle and dwExStyle for our window }
  if FullScreen then
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
      dwStyle := WS_OVERLAPPEDWINDOW;
    end else
      { Just like WS_OVERLAPPEDWINDOW, but without flags that would allow to resize the window). }
      dwStyle := WS_OVERLAPPEDWINDOW and not (WS_THICKFRAME or WS_MAXIMIZEBOX);

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
      ResizeAllowed properrty). Happened e.g. when castle-view-image allowed minimal
      window size = 200, and default welcome image had 256x256 pixels.
      When you opened castle-view-image with no params in directory with no images,
      then OpenGL area of size 256x256 was requested, which was so narrow that
      menu bar wrapped. Which made OpenGL area not tall enough for 256 pixels,
      and castle-view-image had to add a vertical scrollbar to display welcome image.
    }
    AdjustWindowRectEx(WindowRect, dwStyle,
      (MainMenu <> nil) and MainMenuVisible, dwExStyle);
  end;
  dwStyle := dwStyle or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;

  { calculate H_Menu }
  if (MainMenu <> nil) and MainMenuVisible then
    H_Menu := MakeWinapiMenu(MainMenu, true)
  else
    H_Menu := 0;

  { main point : create the window }
  H_wnd := CreateWindowExW(dwExStyle,
    PWideChar(Application.WndClassName),
    PWideChar(StringToUtf16(GetWholeCaption)),
    dwStyle,
    windowRect.left, windowRect.top,
    windowRect.right-windowRect.left, windowRect.Bottom-windowRect.top,
    0 { no parent window }, H_Menu, hInstance,
    nil { don't pass anything to WM_CREATE } );

  Check(h_Wnd <> 0, 'Creating window (CreateWindowExW) failed');

  { calculate correct Left and Top, required e.g. by SetCursorPos }
  if FullScreen then
  begin
    FLeft := 0;
    FTop := 0;
  end;
  { See WM_MOVE handling comments why this is needed to make Top correct. }
  if FullScreen and (MainMenu <> nil) and MainMenuVisible then
    FTop := FTop + GetSystemMetrics(SM_CYMENU);

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

  if Visible then
  begin
    ShowWindow(h_Wnd, SW_SHOW);
    SetForegroundWindow(h_Wnd);
    SetFocus(h_Wnd);
  end;

  OSCheck(GetClientRect(h_Wnd, ClientRect), 'GetClientRect');

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
    DoResize(ClientRect.Right - ClientRect.Left,
             ClientRect.Bottom - ClientRect.Top, false);
  end;

  GetInitialCursorPos(ClientRect);
  InitializeDpi;

  RegisterForJoystickEvents;
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
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
    GetCursorPos({$ifdef FPC}@{$endif}Point);
    if h_Wnd = WindowFromPoint(Point) then
      PostMessage(h_Wnd, WM_SETCURSOR, h_Wnd, 0 { not important});
  end;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);

  { Perform similar function as WinAPI SetCursorPos (set cursor position,
    given absolute screen coordinates), but using SendInput WinAPI call.
    This is more reliable, which can be observed when using MouseLook.

    Testcase: dragging_test, if we would use SetCursorPos then:

    - TCastleContainer.MouseLookDelta doesn't set FMouseLookWaitingForMiddle to true:
      then we have problems due to occasional rect jump, when moving mouse slowly
      (because we need to set FMouseLookWaitingForMiddle to true!).

    - TCastleContainer.MouseLookDelta sets FMouseLookWaitingForMiddle to true:
      then we have occasional "blocks" when moving mouse very fast
      (like from edge to edge of the screen). Experimenting, we just
      cannot reliably capture "the end of FMouseLookWaitingForMiddle",
      no matter how small/large margin in ValidArea we use.

    Info about SendInput:

    - https://social.msdn.microsoft.com/Forums/sqlserver/en-US/72d2c30e-0689-4c80-bf02-dcbc33b7b7de/problems-with-getcursorpos-and-setcursorpos-when-running-directx11-on-windows-10
      https://docs.microsoft.com/pl-pl/windows/win32/api/winuser/ns-winuser-mouseinput

    - Possibly related:
      https://github.com/gottebp/windows10-setcursorpos-bug-demonstration
      http://www.cplusplus.com/forum/general/101920/
  }
  procedure SetCursorPosBySendInput(const X, Y: Integer);
  var
    I: TInput;
  begin
    FillChar(I, SizeOf(I), 0);
    I.InputType := INPUT_MOUSE;
    I.Mi.Flags := MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE;
    I.Mi.Dx := (X * 65535) div Application.ScreenWidth;
    I.Mi.Dy := (Y * 65535) div Application.ScreenHeight;
    if SendInput(1, @I, SizeOf(I)) <> 1 then
      WritelnWarning('Could not set mouse position with SendInput, WinAPI error');
  end;

var
  P: TVector2Integer;
begin
  if not Closed then
  begin

    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    P := CastleToLeftTopInt(Value);
    SetCursorPosBySendInput(Left + P.X, Top + P.Y);
  end;
end;

procedure TCastleWindow.CloseBackend;
begin
  InCloseBackend := true;
  try
    ContextDestroy;

    if (h_Dc <> 0) and (ReleaseDC(h_Wnd, h_Dc) = 0) then
      WritelnWarning('WinAPI', 'Releasing device context (ReleaseDC) failed.');
    h_Dc := 0;
    if (h_Wnd <> 0) and (not DestroyWindow(h_Wnd))then
      WritelnWarning('WinAPI', 'Destroying window (DestroyWindow) failed.');
    h_Wnd := 0;
  finally InCloseBackend := false; end;
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if h_Wnd <> 0 then
  begin
    SetWindowTextW(h_Wnd, PWideChar(StringToUtf16(GetWholeCaption)));
  end;
end;

procedure TCastleWindow.CreateBackend;
begin
end;

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
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

      Result := Result + Filter.Name + #0;

      for J := 0 to Filter.Patterns.Count - 1 do
      begin
        if J <> 0 then Result := Result + ';';
        Result := Result + Filter.Patterns[J];
      end;

      Result := Result + #0;
    end;

    Result := Result + #0;
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
  Struct: TOpenFileNameW;
  FileNameBuffer: array [0..6000] of widechar;
  {$ifndef FPC}FileNameString: String;{$endif}
  FilePath: string;
  Mode: TGLModeFrozenScreen;
  CurrentFileNameExt: string;
  CurrentFormatIndex: Integer;
begin
  FillChar(Struct, SizeOf(Struct), 0);
  Struct.lStructSize := SizeOf(Struct);
  Struct.hwndOwner := H_Wnd;

  {$ifdef FPC}
  FileNameBuffer := ExtractFileName(FileName);
  {$else}
  StrPLCopy(FileNameBuffer, ExtractFileName(FileName), High(FileNameBuffer));
  {$endif}
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
  Struct.lpstrInitialDir := PWideCharOrNil(StringToUtf16(FilePath));

 { tests:
   InfoWrite('current dir : "' + GetCurrentDir + '"');
   InfoWrite('dialog box InitialDir : "' + FilePath + '"');}

  Struct.lpstrTitle := PWideCharOrNil(StringToUtf16(Title));
  Struct.Flags := OFN_NOCHANGEDIR or OFN_HIDEREADONLY;
  if OpenDialog then
    Struct.Flags := Struct.Flags or OFN_FILEMUSTEXIST else
    Struct.Flags := Struct.Flags or OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST;

  if FileFilters <> nil then
  begin
    Struct.lpstrFilter := PWideCharOrNil(StringToUtf16(MakeStrFilter(FileFilters)));
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
      CurrentFileNameExt := ExtractFileExt(Utf16ToString(FileNameBuffer));
      CurrentFormatIndex := SearchFirstPatternWithExtension(CurrentFileNameExt);
      if CurrentFormatIndex <> -1 then
      begin
        { We can use workaround described above. }
        {$ifdef FPC}
        FileNameBuffer := DeleteFileExt(FileNameBuffer);
        {$else}
        if StrLen(FileNameBuffer) > 0 then
          SetString(FileNameString, PWideChar(@FileNameBuffer[0]), StrLen(FileNameBuffer))
        else
          FileNameString := '';

        FileNameString := DeleteFileExt(FileNameString);
        StrPLCopy(FileNameBuffer, ExtractFileName(FileNameString), High(FileNameBuffer));
        {$endif}
        Struct.nFilterIndex := CurrentFormatIndex + 1;

        { If user selects something like "all files" then also append this extension.

          I pass here CurrentFileNameExt without leading dot (since
          SearchFirstPatternWithExtension returned something <> -1, I know there's
          a leading dot).

          Note: only first three characters of lpstrDefExt extension are taken
          into account,  according to WinAPI reference. (and WTF can I do about it?)

          Also note that unregistered extensions are treated somewhat differently.
          Unfortunately, lpstrDefExt will append then, instead of leaving filename alone. }
        Struct.lpstrDefExt := PWideCharOrNil(StringToUtf16(SEnding(CurrentFileNameExt, 2)));
      end;
    end;
  end;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    if OpenDialog then
      Result := GetOpenFileNameW({$ifdef FPC}@{$endif}Struct) else
      Result := GetSaveFileNameW({$ifdef FPC}@{$endif}Struct);
  finally Mode.Free end;

  if Result then
    FileName := Utf16ToString(FileNameBuffer) else
  begin
    if CommDlgExtendedError <> 0 then
      raise Exception.CreateFmt('GetOpen/SaveFileName failed with error %d',
        [CommDlgExtendedError]);
  end;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): Boolean;

  function Vector3ToColorRef(const Value: TVector3): TColorRef;
  begin
   Result := Windows.RGB(Clamped(Round(Value.X*High(Byte)), 0, High(Byte)),
                         Clamped(Round(Value.Y*High(Byte)), 0, High(Byte)),
                         Clamped(Round(Value.Z*High(Byte)), 0, High(Byte)) );
  end;

  function ColorRefToVector3(const Value: TColorRef): TVector3;
  begin
   Result.X := GetRValue(Value)/High(Byte);
   Result.Y := GetGValue(Value)/High(Byte);
   Result.Z := GetBValue(Value)/High(Byte);
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
  Struct.rgbResult := Vector3ToColorRef(Color.RGB);
  Struct.lpCustColors := @CustomColors;
  { Give CC_FULLOPEN since this is intended to give user a free choice from RGB
    colors. So we suggest user to choose any RGB color freely, instead of using
    only the predefined ones. }
  Struct.Flags := CC_FULLOPEN or CC_RGBINIT;

  Mode := TGLModeFrozenScreen.Create(Self);
  try
    Result := ChooseColor({$ifdef FPC}@{$endif}Struct);
  finally Mode.Free end;

  if Result then
    Color := Vector4(ColorRefToVector3(Struct.rgbResult), Color.W);
end;

(*
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
*)

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  CastleMessages.MessageOK(Self, S);

  // WinAPI MessageBox is unreliable to display over a fullscreen window.
  // See also e.g. https://gamedev.stackexchange.com/questions/892/display-a-message-box-over-a-full-screen-directx-application
  (*
  MessageBox(H_Wnd, PChar(S),
    { Always pass meaningfull caption. Passing nil here causes Windows to
      always display localized "Error" on caption, which looks nonsensible
      on boxes with warnings or questions. }
    PChar(MessageTypeToCaption[MessageType]),
    MB_OK or MessageTypeToWin[MessageType] or
    { MB_APPLMODAL is actually the default. MB_TASKMODAL would also be suitable
      probably, as we always pass non-nil H_Wnd. }
    MB_APPLMODAL);
  *)
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := CastleMessages.MessageYesNo(Self, S);

  // WinAPI MessageBox is unreliable to display over a fullscreen window.
  // See also e.g. https://gamedev.stackexchange.com/questions/892/display-a-message-box-over-a-full-screen-directx-application
  (*
  Result := MessageBox(H_Wnd, PChar(S),
    PChar(MessageTypeToCaption[MessageType]),
    MB_YESNO or MessageTypeToWin[MessageType] or
    { MB_APPLMODAL is actually the default. MB_TASKMODAL would also be suitable
      probably, as we always pass non-nil H_Wnd. }
    MB_APPLMODAL) = IDYES;
  *)
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  SimpleUpdateFullScreenBackend;
end;

{ TWinApiClipboard ----------------------------------------------------------- }

{ Clipboard operations using WinAPI based on:
  - http://msdn.microsoft.com/en-us/library/windows/desktop/ms649016%28v=vs.85%29.aspx
  - LCL implementation: TWin32WidgetSet.ClipboardGetOwnerShip (set)
    and ClipboardGetData (get) in lcl/interfaces/win32/win32winapi.inc
}

type
  TWinApiClipboard = class(TCastleClipboard)
  protected
    function GetAsText: string; override;
    procedure SetAsText(const Value: string); override;
  end;

function TWinApiClipboard.GetAsText: string;
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

procedure TWinApiClipboard.SetAsText(const Value: string);
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

{ TCastleApplication ----------------------------------------------- }

procedure TCastleApplication.CreateBackend;
const
  WinCursorNameFromCastle: array [mcStandard .. High(TMouseCursor)] of LPCWSTR = (
    PWideChar(IDC_ARROW),  // mcStandard
    PWideChar(IDC_WAIT),   // mcWait
    PWideChar(IDC_IBEAM),  // mcText
    PWideChar(IDC_HAND),   // mcHand
    PWideChar(IDC_SIZENS), // mcResizeVertical
    PWideChar(IDC_SIZEWE), // mcResizeHorizontal
    PWideChar(IDC_SIZENWSE), // no IDC_SIZENW in Windows, // mcResizeTopLeft
    PWideChar(IDC_SIZENS),   // no IDC_SIZEN  in Windows, // mcResizeTop
    PWideChar(IDC_SIZENESW), // no IDC_SIZENE in Windows, // mcResizeTopRight
    PWideChar(IDC_SIZEWE),   // no IDC_SIZEW  in Windows, // mcResizeLeft
    PWideChar(IDC_SIZEWE),   // no IDC_SIZEE  in Windows, // mcResizeRight
    PWideChar(IDC_SIZENESW), // no IDC_SIZESW in Windows, // mcResizeBottomLeft
    PWideChar(IDC_SIZENS),   // no IDC_SIZES  in Windows, // mcResizeBottom
    PWideChar(IDC_SIZENWSE)  // no IDC_SIZESE in Windows, // mcResizeBottomRight
  );
var
  WindowClass: TWndClassW;
  C: TMouseCursor;
begin
  RegisterClipboard(TWinApiClipboard.Create);

  appActive := true;

  { Initialize CursorHandles.
    We depend on the fact that CursorHandles is initially filled with zero,
    so Handle remains nil for the rest of TMouseCursor values
    (not in WinCursorNameFromCastle). }
  for C := Low(WinCursorNameFromCastle) to High(WinCursorNameFromCastle) do
    CursorHandles[C].Handle := LoadCursorW(0, WinCursorNameFromCastle[C]);

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
  WindowClass.hIcon := LoadIconW(hInstance, 'MAINICON');
  if WindowClass.hIcon = 0 then
    WindowClass.hIcon := LoadIconW(0, PWideChar(IDI_APPLICATION));
  WindowClass.hCursor := CursorHandles[mcStandard].Handle;
  WindowClass.lpszClassName := PWideChar(WndClassName);
  OSCheck( Windows.RegisterClassW(WindowClass) <> 0, 'RegisterClassW');
end;

procedure TCastleApplication.DestroyBackend;
begin
  OSCheck( Windows.UnregisterClassW(PWideChar(WndClassName), hInstance), 'UnregisterClassW');
end;

procedure TCastleApplication.VideoReset;
begin
  { switch back to the desktop mode }
  if DisplaySettingsChanged then
    ChangeDisplaySettings({$ifdef FPC}nil{$else}TDeviceMode(nil^){$endif}, 0);
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

      ChangeResult := ChangeDisplaySettings({$ifdef FPC}@{$endif}dmScreenSettings, CDS_FullScreen);
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
    ChangeDisplaySettings({$ifdef FPC}nil{$else}TDeviceMode(nil^){$endif} , 0);

  fappActive := value;

  { in this simple case, this can just control Container.Focused }
  if OpenWindowsCount = 1 then
  begin
    OpenWindows[0].FFocused := Value;
    { right after changing Focused, it's safest to consider mouse position
      not suitable for mouse look }
    OpenWindows[0].Container.MouseLookIgnoreNextMotion;
  end;
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

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'WinAPI';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { Confirmed using window_events example that it is "true" for GTK
    (press "5", OnMotion is generated). }
  Result := true;
end;

{$endif read_implementation}
