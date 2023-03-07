{%MainUnit castlewindow.pas}
{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleWindow backend that depends on external code to initialize
  OpenGL context.
  The external code must notify us about events by calling LibraryXxx methods,
  like TCastleWindow.LibraryKeyDown or TCastleApplication.LibraryUpdate.
  The external code will get notified about our events using LibraryCallbackProc. }

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_implementation_uses}
CastleJoysticks,
{$endif}

{$ifdef read_interface_types}
  TLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32; szParam: pcchar): cInt32; cdecl;
{$endif read_interface_types}

{$ifdef read_window_interface}
public
  { Only the library in src/deprecated_library/castleengine.lpr uses this,
    to notify our window instance about various events. }
  procedure LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  FScreenWidth, FScreenHeight: Integer;
  FScreenStatusBarHeight: Cardinal;
  procedure WarningToCallback(const Category, S: string);
{$endif read_application_interface}

{$ifdef read_global_interface}

{ Initialize / Finalize set up some global things.
  Call CGEApp_Initialize / CGEApp_Finalize once and only once during the entire application. }
procedure CGEApp_Initialize(ApplicationConfigDirectory: PChar); cdecl;
procedure CGEApp_Finalize; cdecl;

{ Used by the external process to ask for context properties from CGE
  (whether to use alpha channel, depth buffer etc.).
  CGE answers looking at Application.MainWindow properties. }
procedure CGEApp_ContextProperties(RedBits, GreenBits, BlueBits, AlphaBits, DepthBits, StencilBits, MultiSampling: PCInt); cdecl; export;

{ Notifications from the external process about various events:
  rendering context was opened, closed, resized etc. }
procedure CGEApp_Open(InitialWidth, InitialHeight, InitialStatusBarHeight, ADpi: cUInt32); cdecl;
procedure CGEApp_Close(QuitWhenLastWindowClosed: CBool); cdecl;
procedure CGEApp_Resize(uiViewWidth, uiViewHeight, uiStatusBarHeight: cUInt32); cdecl;
procedure CGEApp_Render; cdecl;
procedure CGEApp_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl;
procedure CGEApp_Update; cdecl;
procedure CGEApp_MouseDown(X, Y: CInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl;
procedure CGEApp_Motion(X, Y: CInt32; FingerIndex: CInt32); cdecl;
procedure CGEApp_MouseUp(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32; TrackReleased: cBool); cdecl;
procedure CGEApp_KeyDown(eKey: CInt32); cdecl;
procedure CGEApp_KeyUp(eKey: CInt32); cdecl;
procedure CGEApp_JoystickAxis(JoystickIndex: CInt; X, Y: Single); cdecl;
procedure CGEApp_JoystickCount(JoystickCount: CInt); cdecl;
function CGEApp_HandleOpenUrl(szUrl: PChar): CInt32; cdecl;

const
  { Constants used for callbacks.
    Must be synchronized with constants in
    ../library/castlelib_dynloader.pas and
    ../library/castleengine.h }

  // library callback codes
  ecgelibNeedsDisplay          = 0;
  ecgelibSetMouseCursor        = 1;  // sends mouse cursor code in iParam1
  ecgelibNavigationTypeChanged = 2;  // sends ECgeNavigationType in iParam1 (see castleengine.h)
  ecgelibSetMousePosition      = 3;  // sends x in iParam1 and y in iParam2
  ecgelibWarning               = 4;  // sends message in szParam

  // navigation types (ECgeNavigationType enum)
  ecgenavWalk      = 0;
  ecgenavFly       = 1;
  ecgenavExamine   = 2;
  ecgenavTurntable = 3;
  ecgenavNone      = 4;

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

var
  LibraryCallbackProc: TLibraryCallbackProc;

{$endif read_global_interface}

{$ifdef read_implementation}

{ TCastleWindow ---------------------------------------------------------- }

procedure TCastleWindow.CreateBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindow.OpenBackend;
begin
  { We just assume that we already have OpenGL context initialized.
    Nothing to do. }
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindow.SwapBuffers;
begin
  { The external code must take care of swapping buffers or doing glFlush. }
end;

procedure TCastleWindow.CloseBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindow.Invalidate;
begin
  if not Closed then Invalidated := true;
  if Assigned(LibraryCallbackProc) then  // tell the parent window to repaint
    LibraryCallbackProc(ecgelibNeedsDisplay, 0, 0, nil);
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  { Nothing to do, we assume external code makes sure this is always
    the current OpenGL context. }
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
var
  CursorCode: cInt32;
begin
  if FCursor <> Value then
  begin
    FCursor := Value;

    if Assigned(LibraryCallbackProc) then
    begin
      // send to client
      case Value of
        mcNone, mcForceNone: CursorCode := ecgecursorNone;
        mcWait: CursorCode := ecgecursorWait;
        mcHand: CursorCode := ecgecursorHand;
        mcText: CursorCode := ecgecursorText;
        else CursorCode := ecgecursorDefault;
      end;
      LibraryCallbackProc(ecgelibSetMouseCursor, CursorCode, 0, nil);
    end;
  end;
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then
    { TODO: call LibraryCallbackProc to change caption to GetWholeCaption };
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
begin
  if (not Closed) and Assigned(LibraryCallbackProc) then
  begin

    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    LibraryCallbackProc(ecgelibSetMousePosition, Floor(Value[0]), Floor(Value[1]), nil);
  end;
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  { TODO: call LibraryCallbackProc to switch fullscreen mode }
  FFullScreenBackend := FFullScreenWanted;
end;

{ LibraryXxx methods --------------------------------------------------------- }

procedure TCastleWindow.LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
begin
  DoMouseWheel(Scroll, Vertical);
end;

{ TCastleWindow menu ----------------------------------------------------
  Does not do anything, this backend cannot manage the menu bar. }

procedure TCastleWindow.BackendMenuInitialize;
begin
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

{ TCastleWindow dialogs ---------------------------------------------------------- }

{ Methods below should make native-looking dialog boxes. }

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO } Result := false;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): Boolean;
begin
  { TODO } Result := false;
end;

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  { TODO } Result := true;
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
end;

procedure TCastleApplication.DestroyBackend;
begin
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  Result := true;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  Result := ProcessMessage(false, false);
end;

procedure TCastleApplication.Run;
begin
  { for this CastleWindow backend, Run makes no sense, the external code
    must perform event loop }
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  Terminate; // set Terminated := true
end;

function TCastleApplication.ScreenWidth: integer;
begin
  if FScreenWidth = 0 then
    WritelnWarning('Window', 'Application.ScreenWidth is queried before window is open, returns zero');
  Result := FScreenWidth;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  if FScreenHeight = 0 then
    WritelnWarning('Window', 'Application.ScreenHeight is queried before window is open, returns zero');
  Result := FScreenHeight;
end;

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := FScreenStatusBarHeight;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Library (Using Existing OpenGL Context)';
end;

procedure TCastleApplication.WarningToCallback(const Category, S: string);
var
  sMsg: string;
  szBuffer: PChar;
  nBufSize: cardinal;
begin
  if Assigned(LibraryCallbackProc) then
  begin
    sMsg := Category + ': ' + S;
    nBufSize := Length(sMsg);
    szBuffer := StrAlloc(nBufSize+1);
    StrPLCopy(szBuffer, sMsg, nBufSize);
    LibraryCallbackProc(4 {ecgelibWarning}, 0, 0, pcchar(szBuffer));
    StrDispose(szBuffer);
  end;
end;

{$define read_implementation}

procedure CheckMainWindow;
begin
  if Application.MainWindow = nil then
    raise Exception.Create('Application.MainWindow not set. One of the game units *must* set Application.MainWindow value in the "initialization" section');
end;

procedure CGEApp_Initialize(ApplicationConfigDirectory: PChar); cdecl;
begin
  try
    // use given ApplicationConfigDirectory
    ApplicationConfigOverride := FilenameToURISafe(InclPathDelim(ApplicationConfigDirectory));
    WritelnLog('Library initialized, set application config directory to URL: %s', [ApplicationConfigOverride]);

    ApplicationProperties.OnWarning.Add(@Application.WarningToCallback);
  except
    on E: TObject do WritelnWarning('CGEApp_Initialize', ExceptMessage(E));
  end;
end;

procedure CGEApp_Finalize; cdecl;
begin
  try
    ApplicationProperties.OnWarning.Remove(@Application.WarningToCallback);
  except
    on E: TObject do WritelnWarning('CGEApp_Finalize', ExceptMessage(E));
  end;
end;

procedure CGEApp_ContextProperties(RedBits, GreenBits, BlueBits, AlphaBits, DepthBits, StencilBits, MultiSampling: PCInt); cdecl;
var
  W: TCastleWindow;
begin
  try
    CheckMainWindow;
    W := Application.MainWindow;
    RedBits      ^ := Max(1, W.RedBits);
    GreenBits    ^ := Max(1, W.GreenBits);
    BlueBits     ^ := Max(1, W.BlueBits);
    AlphaBits    ^ := W.AlphaBits;
    DepthBits    ^ := W.DepthBits;
    StencilBits  ^ := W.StencilBits;
    MultiSampling^ := W.MultiSampling;
    WritelnLog('Rendering context will be initialized with these (minimal) buffer sizes: red %d, green %d, blue %d, alpha %d, depth %d, stencil %d, multi-sampling %d', [
      RedBits      ^,
      GreenBits    ^,
      BlueBits     ^,
      AlphaBits    ^,
      DepthBits    ^,
      StencilBits  ^,
      MultiSampling^
    ]);
  except
    on E: TObject do WritelnWarning('CGEApp_ContextProperties', ExceptMessage(E));
  end;
end;

procedure CGEApp_Open(InitialWidth, InitialHeight, InitialStatusBarHeight, ADpi: cUInt32); cdecl;
begin
  try
    CheckMainWindow;

    { We have to set window sizes, and Application.Screen* sizes,
      before calling MainWindow.Open. That's because EventOpen may already
      look at these values.
      Yes, other CastleWindow backends also make sure that these are set early. }

    WritelnLog('Library received CGEApp_Open (window size: %d %d)', [InitialWidth, InitialHeight]);
    Application.FScreenWidth := InitialWidth;
    Application.FScreenHeight := InitialHeight;
    Application.FScreenStatusBarHeight := InitialStatusBarHeight;
    //Window.FullScreen := true; // TODO: setting fullscreen should work like that 4 lines below. Also, should be default?
    Application.MainWindow.FLeft := 0;
    Application.MainWindow.FTop := 0;
    Application.MainWindow.FWidth := InitialWidth;
    Application.MainWindow.FHeight := InitialHeight;
    Application.MainWindow.Container.Dpi := ADpi;
    Application.MainWindow.Open;
  except
    on E: TObject do WritelnWarning('CGEApp_Open', ExceptMessage(E));
  end;
end;

procedure CGEApp_Close(QuitWhenLastWindowClosed: CBool); cdecl;
begin
  try
    CheckMainWindow;
    Application.MainWindow.Close(QuitWhenLastWindowClosed);
  except
    on E: TObject do WritelnWarning('CGEApp_Close', ExceptMessage(E));
  end;
end;

procedure CGEApp_Resize(uiViewWidth, uiViewHeight, uiStatusBarHeight: cUInt32); cdecl;
begin
  try
    CheckMainWindow;
    Application.FScreenWidth  := uiViewWidth;
    Application.FScreenHeight := uiViewHeight;
    Application.FScreenStatusBarHeight := uiStatusBarHeight;
    Application.MainWindow.DoResize(uiViewWidth, uiViewHeight, false);
  except
    on E: TObject do WritelnWarning('CGEApp_Resize', ExceptMessage(E));
  end;
end;

procedure CGEApp_Render; cdecl;
begin
  try
    CheckMainWindow;
    Application.MainWindow.DoRender;
  except
    on E: TObject do WritelnWarning('CGEApp_Render', ExceptMessage(E));
  end;
end;

procedure CGEApp_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl;
begin
  try
    LibraryCallbackProc := aProc;
  except
    on E: TObject do WritelnWarning('CGEApp_SetLibraryCallbackProc', ExceptMessage(E));
  end;
end;

procedure CGEApp_Update; cdecl;
begin
  try
    Application.DoApplicationUpdate;
    Application.FOpenWindows.DoUpdate;
    Application.MaybeDoTimer;
  except
    on E: TObject do WritelnWarning('CGEApp_Update', ExceptMessage(E));
  end;
end;

procedure CGEApp_MouseDown(X, Y: CInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl;
var
  MyButton: TCastleMouseButton;
begin
  try
    CheckMainWindow;
    if (bLeftBtn) then MyButton := buttonLeft else MyButton := buttonRight;
    Application.MainWindow.DoMouseDown(Vector2(X, Y), MyButton, FingerIndex);
  except
    on E: TObject do WritelnWarning('CGEApp_MouseDown', ExceptMessage(E));
  end;
end;

procedure CGEApp_Motion(X, Y: CInt32; FingerIndex: CInt32); cdecl;
var
  W: TCastleWindow;
begin
  try
    CheckMainWindow;
    { TODO: using MousePressed as Pressed is only OK without multi-touch. }
    W := Application.MainWindow;
    W.DoMotion(InputMotion(W.FTouches.FingerIndexPosition[FingerIndex],
      Vector2(X, Y), W.MousePressed, FingerIndex));
  except
    on E: TObject do WritelnWarning('CGEApp_Motion', ExceptMessage(E));
  end;
end;

procedure CGEApp_MouseUp(X, Y: cInt32; bLeftBtn: cBool;
  FingerIndex: CInt32; TrackReleased: cBool); cdecl;
var
  MyButton: TCastleMouseButton;
begin
  try
    CheckMainWindow;
    if bLeftBtn then
      MyButton := buttonLeft
    else
      MyButton := buttonRight;
    Application.MainWindow.DoMouseUp(Vector2(X, Y), MyButton, FingerIndex, TrackReleased);
  except
    on E: TObject do WritelnWarning('CGEApp_MouseUp', ExceptMessage(E));
  end;
end;

procedure CGEApp_KeyDown(eKey: CInt32); cdecl;

  { Guess key as String, used e.g. by X3D KeySensor processing.
    Without this, demo-models/navigation/navigation_info_bind.x3dv would not react to keys 1,2,3. }
  function GuessKeyStringFromKey(const Key: TKey): String;
  begin
    Result := '';
    case Key of
      key0..key9: Result := Chr(Ord(Key));
      keyA..keyZ: Result := LoCase(Chr(Ord(Key)));
    end;
  end;

var
  Key: TKey;
begin
  try
    CheckMainWindow;
    Key := TKey(eKey);
    if Key <> keyNone then
    begin
      // Writeln(Format('Library received key down %s "%s"', [
      //   KeyToStr(Key),
      //   GuessKeyStringFromKey(Key)
      // ]));
      Application.MainWindow.DoKeyDown(Key, GuessKeyStringFromKey(Key));
    end;
  except
    on E: TObject do WritelnWarning('CGEApp_KeyDown', ExceptMessage(E));
  end;
end;

procedure CGEApp_KeyUp(eKey: CInt32); cdecl;
var
  Key: TKey;
begin
  try
    CheckMainWindow;
    Key := TKey(eKey);
    if Key <> keyNone then
    begin
      // Writeln(Format('Library received key up %s', [
      //   KeyToStr(Key)
      // ]));
      Application.MainWindow.DoKeyUp(Key);
    end;
  except
    on E: TObject do WritelnWarning('CGEApp_KeyUp', ExceptMessage(E));
  end;
end;

procedure CGEApp_JoystickCount(JoystickCount: CInt); cdecl;
begin
  try
    CheckMainWindow;
    Joysticks.InternalSetJoystickCount(JoystickCount);
  except
    on E: TObject do WritelnWarning('CGEApp_JoystickCount', ExceptMessage(E));
  end;
end;

procedure CGEApp_JoystickAxis(JoystickIndex: CInt; X, Y: Single); cdecl;
begin
  try
    CheckMainWindow;
    Joysticks.InternalSetJoystickAxis(JoystickIndex, Vector2(X, Y));
    // if (X <> 0) or (Y <> 0) then
    //   WritelnLog('Joy axis %d %f %f %f', [JoystickIndex, X, Y]);
  except
    on E: TObject do WritelnWarning('CGEApp_JoystickAxis', ExceptMessage(E));
  end;
end;

function CGEApp_HandleOpenUrl(szUrl: PChar): CInt32; cdecl;
begin
  Result := 0;
  try
    CheckMainWindow;
    Application.MainWindow.DoDropFiles([StrPas(PChar(szUrl))]);
    Result := 1;
  except
    on E: TObject do WritelnWarning('CGEApp_HandleOpenUrl', ExceptMessage(E));
  end;
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  Result := true;
end;

{$endif read_implementation}
