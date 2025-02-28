{%MainUnit castlewindow.pas}
{
  Copyright 2004-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_uses}
CastleInternalJobWeb,
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
strict private
  Canvas: IJSHTMLCanvasElement;
  LastClientRectHeight, LastClientRectWidth, LastDevicePixelRatio: Double;
  { Get mouse position from JS event, convert it to CGE coordinates. }
  function JsMousePositionToCastle(const Event: IJSPointerEvent): TVector2;
  function PointerDown(BaseEvent: IJSEvent): Boolean;
  function PointerUp(BaseEvent: IJSEvent): Boolean;
  function PointerMove(BaseEvent: IJSEvent): Boolean;
  function LostPointerCapture(BaseEvent: IJSEvent): Boolean;
  function KeyDown(BaseEvent: IJSEvent): Boolean;
  function KeyUp(BaseEvent: IJSEvent): Boolean;
{$endif read_window_interface}

{$ifdef read_application_interface}
strict private
  CanvasAnimationHandler: Integer;
  WasFrameRendered: Boolean;
  { Callback for requestAnimationFrame
    ( https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame ) }
  procedure AnimationFrame(Time: TDOMHighResTimeStamp);
{$endif read_application_interface}

{$ifdef read_implementation}

{ TCastleWindow ------------------------------------------------------------------ }

procedure TCastleWindow.CreateBackend;
begin
end;

procedure TCastleWindow.BackendMenuInitialize;
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.SwapBuffers;
begin
  { No need to do anything, according to MDN we don't even need to do glFlush
    when using RequestAnimationFrame. }
end;

procedure TCastleWindow.OpenBackend;

  procedure DebugLog;
  begin
    // LogGLInformationVerbose := true;
    WritelnLog(Format('Initial WebGL context Width x Height: %d x %d', [
      FRealWidth,
      FRealHeight
    ]));
  end;

var
  GLBase, GL2Base: IJSnsISupports;
  CanvasBase: IJSElement;
  ContextOptions: IJSWebGLContextAttributes;
begin
  { Note that we first check if something is nil, only later we use Cast.
    Because the Cast seems to fail if original is nil, unlike "as" operator. }
  CanvasBase := JSDocument.getElementById('castle-canvas');
  if CanvasBase = nil then
    raise Exception.Create('Failed to find canvas element with id="castle-canvas"');
  Canvas := TJSHTMLCanvasElement.Cast(CanvasBase);

  ContextOptions := TJSWebGLContextAttributes.Create;
  { Seems to have no effect. }
  ContextOptions.Antialias := false;
  { This is critical to have correct text rendering.
    By default premultipliedAlpha is true, we don't want this,
    we assume not premultiplied in all our operations, including when delivering
    pixels with blending.
    See
    https://registry.khronos.org/webgl/specs/latest/1.0/#DOM-WebGLRenderingContext-drawingBufferWidth
    https://registry.khronos.org/webgl/specs/latest/1.0/#PREMULTIPLIED_ALPHA }
  ContextOptions.PremultipliedAlpha := false;

  GL2Base := Canvas.getContext('webgl2', ContextOptions);
  if GL2Base <> nil then
  begin
    GL2 := TJSWebGL2RenderingContext.Cast(GL2Base);
    // WebGL 2.0 is a superset of WebGL 1.0
    GL := TJSWebGLRenderingContext.Cast(GL2);
    WritelnLog('WebGL 2.0 context initialized from WebAssembly');
  end else
  begin
    GLBase := Canvas.getContext('webgl', ContextOptions);
    if GLBase = nil then
      raise Exception.Create('Failed to load WebGL context (2.0 or 1.0) from WebAssembly');
    GL := TJSWebGLRenderingContext.Cast(GLBase);
    WritelnLog('WebGL 1.0 context initialized from WebAssembly');
  end;

  // Tell TCastleWindow about the real window size.
  // Use GL.DrawingBufferWidth/Height, just like in TCastleWindow.BackendInsideUpdate.
  DoResize(GL.DrawingBufferWidth, GL.DrawingBufferHeight, false);

  DebugLog;

  Application.OpenWindowsAdd(Self);

  Canvas.addEventListener('pointerdown', @PointerDown);
  Canvas.addEventListener('pointerup', @PointerUp);
  Canvas.addEventListener('lostpointercapture', @LostPointerCapture);
  Canvas.addEventListener('pointermove', @PointerMove);

  { Catching key events on canvas doesn't work, as the canvas needs to be focused,
    see https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event .
    For now, we catch key events on the whole window. }
  JSWindow.addEventListener('keydown', @KeyDown);
  JSWindow.addEventListener('keyup', @KeyUp);

  // TODO: web: initialize initial cursor position, set initial cursor, initialize DPI if possible
  // GetInitialCursorPos;
  // UpdateCursor;
  // InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
begin
  { One doesn't close WebGL context. }
end;

function JsMouseButtonToCastle(const JsButton: Integer;
  out Button: TCastleMouseButton): Boolean;
begin
  Result := true;
  case JsButton of
    0: Button := buttonLeft;
    1: Button := buttonMiddle;
    2: Button := buttonRight;
    else
      begin
        WritelnWarning('Web', 'Ignoring PointerDown with unhandled button %d', [
          JsButton
        ]);
        Exit(false);
      end;
  end;
end;

function TCastleWindow.JsMousePositionToCastle(const Event: IJSPointerEvent): TVector2;
begin
  { Some info about various positions:
    https://stephendoddtech.com/blog/game-design/mouse-event-listener-input-html-canvas

    Note that we need to Canvas.OffsetWidth/Height to get the correct position,
    because we adjust Canvas.Width/Height to match the actual pixels on the screen.
    See https://www.khronos.org/webgl/wiki/HandlingHighDPI . }
  Result := Vector2(
                             Event.OffsetX * Canvas.Width / Canvas.OffsetWidth,
    GL.DrawingBufferHeight - Event.OffsetY * Canvas.Height / Canvas.OffsetHeight);
end;

function TCastleWindow.PointerDown(BaseEvent: IJSEvent): Boolean;
var
  Event: IJSPointerEvent;
  Button: TCastleMouseButton;
  FingerIndex: TFingerIndex;
  Position: TVector2;
begin
  Result := true; // assume not handled, in case of early exit
  Event := TJSPointerEvent.Cast(BaseEvent);

  if not JsMouseButtonToCastle(Event.Button, Button) then
    Exit;
  { TODO: web: Get FingerIndex from pointer event
    ( https://developer.mozilla.org/en-US/docs/Web/API/Element/pointerdown_event ),
    maybe use Event.PointerId,
    to support multi-touch on mobiles. }
  FingerIndex := 0;
  Position := JsMousePositionToCastle(Event);
  DoMouseDown(Position, Button, FingerIndex);

  { Prevent default action, like selecting text.
    See https://stackoverflow.com/questions/128923/whats-the-effect-of-adding-return-false-to-a-click-event-listener }
  Result := false;
end;

function TCastleWindow.PointerUp(BaseEvent: IJSEvent): Boolean;
var
  Event: IJSPointerEvent;
  Button: TCastleMouseButton;
  FingerIndex: TFingerIndex;
  Position: TVector2;
begin
  Result := true; // assume not handled, in case of early exit
  Event := TJSPointerEvent.Cast(BaseEvent);

  if not JsMouseButtonToCastle(Event.Button, Button) then
    Exit;
  FingerIndex := 0;
  Position := JsMousePositionToCastle(Event);
  DoMouseUp(Position, Button, FingerIndex);

  Result := false;
end;

function TCastleWindow.LostPointerCapture(BaseEvent: IJSEvent): Boolean;
var
  B: TCastleMouseButton;
begin
  Result := true; // assume not handled

  if MousePressed <> [] then
  begin
    WritelnLog('LostPointerCapture while some buttons pressed, simulating MouseUp for all buttons');
    for B in MousePressed do
      DoMouseUp(Container.MousePosition, B, 0);
  end;
end;

function TCastleWindow.PointerMove(BaseEvent: IJSEvent): Boolean;
var
  Event: IJSPointerEvent;
  FingerIndex: TFingerIndex;
  Position: TVector2;
begin
  Result := true; // assume not handled, in case of early exit
  Event := TJSPointerEvent.Cast(BaseEvent);

  FingerIndex := 0;
  Position := JsMousePositionToCastle(Event);
  DoMotion(InputMotion(Container.MousePosition, Position, Container.MousePressed, FingerIndex));

  Result := false;
end;

{ Convert JS key codes to our (TKey) codes.
  Also, sometimes override KeyString to what we expect.
  https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_code_values }
procedure JsKeyCodeToCastle(const JsKeyCode: String; out Key: TKey; var KeyString: String);
begin
  case JsKeyCode of
    'Escape': begin Key := keyEscape; KeyString := CharEscape; end;
    'Digit1': Key := key1;
    'Digit2': Key := key2;
    'Digit3': Key := key3;
    'Digit4': Key := key4;
    'Digit5': Key := key5;
    'Digit6': Key := key6;
    'Digit7': Key := key7;
    'Digit8': Key := key8;
    'Digit9': Key := key9;
    'Digit0': Key := key0;
    'Minus': Key := keyMinus;
    'Equal': Key := keyEqual;
    'Backspace': begin Key := keyBackSpace; KeyString := CharBackSpace; end;
    'Tab': begin Key := keyTab; KeyString := CharTab; end;
    'KeyQ': Key := keyQ;
    'KeyW': Key := keyW;
    'KeyE': Key := keyE;
    'KeyR': Key := keyR;
    'KeyT': Key := keyT;
    'KeyY': Key := keyY;
    'KeyU': Key := keyU;
    'KeyI': Key := keyI;
    'KeyO': Key := keyO;
    'KeyP': Key := keyP;
    'BracketLeft': Key := keyLeftBracket;
    'BracketRight': Key := keyRightBracket;
    'Enter': begin Key := keyEnter; KeyString := CharEnter; end;
    'ControlLeft': Key := keyCtrl;
    'KeyA': Key := keyA;
    'KeyS': Key := keyS;
    'KeyD': Key := keyD;
    'KeyF': Key := keyF;
    'KeyG': Key := keyG;
    'KeyH': Key := keyH;
    'KeyJ': Key := keyJ;
    'KeyK': Key := keyK;
    'KeyL': Key := keyL;
    'Semicolon': Key := keySemicolon;
    // 'Quote': Key := keyQuote;
    'Backquote': Key := keyBackQuote;
    'ShiftLeft': Key := keyShift;
    'Backslash': Key := keyBackSlash;
    'KeyZ': Key := keyZ;
    'KeyX': Key := keyX;
    'KeyC': Key := keyC;
    'KeyV': Key := keyV;
    'KeyB': Key := keyB;
    'KeyN': Key := keyN;
    'KeyM': Key := keyM;
    'Comma': Key := keyComma;
    'Period': Key := keyPeriod;
    'Slash': Key := keySlash;
    'ShiftRight': Key := keyShift;
    'NumpadMultiply': Key := keyNumpadMultiply;
    'AltLeft': Key := keyAlt;
    'Space': Key := keySpace;
    //'CapsLock': Key := keyCapsLock;
    'F1': Key := keyF1;
    'F2': Key := keyF2;
    'F3': Key := keyF3;
    'F4': Key := keyF4;
    'F5': Key := keyF5;
    'F6': Key := keyF6;
    'F7': Key := keyF7;
    'F8': Key := keyF8;
    'F9': Key := keyF9;
    'F10': Key := keyF10;
    'Pause': Key := keyPause;
    //'ScrollLock': Key := keyScrollLock;
    'Numpad7': Key := keyNumpad7;
    'Numpad8': Key := keyNumpad8;
    'Numpad9': Key := keyNumpad9;
    'NumpadSubtract': Key := keyNumpadMinus;
    'Numpad4': Key := keyNumpad4;
    'Numpad5': Key := keyNumpad5;
    'Numpad6': Key := keyNumpad6;
    'NumpadAdd': Key := keyNumpadPlus;
    'Numpad1': Key := keyNumpad1;
    'Numpad2': Key := keyNumpad2;
    'Numpad3': Key := keyNumpad3;
    'Numpad0': Key := keyNumpad0;
    //'NumpadDecimal': Key :=
    'F11': Key := keyF11;
    'F12': Key := keyF12;
    'NumpadEnter': Key := keyNumpadEnter;
    'ControlRight': Key := keyCtrl;
    'NumpadDivide': Key := keyNumpadDivide;
    'PrintScreen': Key := keyPrintScreen;
    'AltRight': Key := keyAlt;
    'Home': Key := keyHome;
    'ArrowUp': Key := keyArrowUp;
    'PageUp': Key := keyPageUp;
    'ArrowLeft': Key := keyArrowLeft;
    'ArrowRight': Key := keyArrowRight;
    'End': Key := keyEnd;
    'ArrowDown': Key := keyArrowDown;
    'PageDown': Key := keyPageDown;
    'Insert': Key := keyInsert;
    'Delete': Key := keyDelete;
    //'MetaLeft': Key := keyMeta;
    //'MetaRight': Key := keyMeta;
    //'ContextMenu': Key := keyContextMenu;
    else
      begin
        WritelnWarning('Web', 'Ignoring KeyDown with unhandled code %s', [
          JsKeyCode
        ]);
        Key := keyNone;
      end;
  end;
end;

function TCastleWindow.KeyDown(BaseEvent: IJSEvent): Boolean;
var
  Event: IJSKeyboardEvent;
  Key: TKey;
  KeyString: String;
begin
  Event := TJSKeyboardEvent.Cast(BaseEvent);
  KeyString := String(Event.Key);
  JsKeyCodeToCastle(String(Event.Code), Key, KeyString);
  if (Key <> keyNone) or (KeyString <> '') then
    DoKeyDown(Key, KeyString);
  Result := false;
end;

function TCastleWindow.KeyUp(BaseEvent: IJSEvent): Boolean;
var
  Event: IJSKeyboardEvent;
  Key: TKey;
  KeyString: String;
begin
  Event := TJSKeyboardEvent.Cast(BaseEvent);
  KeyString := String(Event.Key);
  JsKeyCodeToCastle(String(Event.Code), Key, KeyString);
  // actually KeyString is not used in KeyUp, but we had it calculated anyway
  if Key <> keyNone then
    DoKeyUp(Key);
  Result := false;
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  // No typical way to use Window.Caption from WASM
  // if not Closed then Something := GetWholeCaption
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  // No need for this, we always call WebGL through GL/GL2 which are context
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO: web: UpdateCursor - is this possible on web at all? };
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO: web: what is correct here } true;
end;

procedure TCastleWindow.SystemSetMousePosition(const Value: TVector2);
begin
  { TODO: web: is this possible? we need it for MouseLook. }
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  { This is the simplest implementation of this method: }
  SimpleUpdateFullScreenBackend;
  { Some backends may provide a better way of doing this.
    Remove the above line then, and put better implementation here. }
  // TODO: web: go to fullscreen in WebGL initiated by application (call requestFullscreen on canvas)
end;

procedure TCastleWindow.BackendInsideUpdate;

  (*
  This causes "infinitely growing canvas" problem, for unknown reasons.
  We use JS solution in index.html for this now.

  procedure UpdateCanvasSize;
  var
    ClientRect: IJSDOMRect;
    NewWidth, NewHeight: Integer;
    DevicePixelRatio: Double;
  const
    Epsilon = 0.001;
  begin
    { Adjust the Canvas.Width/Height (the rendering resolution we use,
      we render to it) to what is available (how many pixels on the screen
      we have). Get the "pixels on the screen size":

      - from Canvas.ClientWidth/Height
      - or (even better implementation) from Canvas.GetBoundingClientRect *
        JSWindow.DevicePixelRatio

      This adjustment is needed:

      - e.g. after entering fullscreen, or otherwise resizing the canvas
        (e.g. by resizing developer tools panel in fullscreen mode,
        changes the available area)
        to change the rendering resolution to the new fullscreen resolution.

      - It is also needed to have really "crisp" rendering, by having
        our resolution match the pixels 1-1 and avoid the effect that
        "we render to a smaller texture, and then upscale".
        For this we need to use DevicePixelRatio.

      See:
      - Official:
        https://www.khronos.org/webgl/wiki/HandlingHighDPI
        https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver
      - (excellent overview of sizing of canvas and WebGL):
        https://webglfundamentals.org/webgl/lessons/webgl-resizing-the-canvas.html
      - Also:
        https://github.com/KhronosGroup/WebGL/issues/2460
        https://stackoverflow.com/questions/56107172/resize-canvas-without-enlarge-the-elements-webgl
    }
    {$define CASTLE_CANVAS_ACCOUNT_FOR_PIXEL_RATIO}

    {$ifdef CASTLE_CANVAS_ACCOUNT_FOR_PIXEL_RATIO}
    DevicePixelRatio := JSWindow.DevicePixelRatio;
    ClientRect := Canvas.GetBoundingClientRect();
    NewWidth := Round(ClientRect.Width * DevicePixelRatio);
    NewHeight := Round(ClientRect.Height * DevicePixelRatio);
    if (not SameValue(LastClientRectWidth, ClientRect.Width, Epsilon)) or
       (not SameValue(LastClientRectHeight, ClientRect.Height, Epsilon)) or
       (not SameValue(LastDevicePixelRatio, DevicePixelRatio, Epsilon)) then
    begin

    {$else}
    NewWidth := Canvas.ClientWidth;
    NewHeight := Canvas.ClientHeight;
    if (Canvas.Width <> NewWidth) or
       (Canvas.Height <> NewHeight) then
    begin

    {$endif}
      Canvas.Width := NewWidth;
      Canvas.Height := NewHeight;
      WritelnLog('Canvas width/height changed to %d x %d (device pixel ratio: %f)', [
        NewWidth,
        NewHeight,
        DevicePixelRatio
      ]);
    end;
  end;
  *)

  procedure UpdateCallResize;
  begin
    { Watch for changes of GL.DrawingBufferWidth/Height
      (they are practically caused by our own change of Canvas.Width/Height
      by code above) and fire DoResize when needed.

      Note that we compare with window FRealWidth/Heigth, not Width/Height.
      Otherwise the window with ResizeAllowed = raOnlyAtOpen would behave
      like it never received previous DoResize.
      The FRealWidth/Heigth are "last values reported to DoResize".
    }
    if (FRealWidth <> GL.DrawingBufferWidth) or
       (FRealHeight <> GL.DrawingBufferHeight) then
    begin
      WritelnLog('Drawing buffer width/height changed to %d x %d', [
        GL.DrawingBufferWidth,
        GL.DrawingBufferHeight
      ]);
      DoResize(GL.DrawingBufferWidth, GL.DrawingBufferHeight, false);
    end;
  end;

begin
  //UpdateCanvasSize;
  UpdateCallResize;
end;

{ TCastleWindow dialogs -------------------------------------------------- }

{ Methods below should make native-looking dialog boxes.
  Alternatively, you can include castlewindow_dialogs_by_messages.inc
  (outside of $ifdef read_implementation, usually at the beginning of
  castlewindow_xxx.inc file), to have simple dialog boxes drawn on
  OpenGL context using our CastleMessages unit. }

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO: web: browser dialog } Result := false;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): Boolean;
begin
  { TODO: web: browser dialog, if possible? } Result := false;
end;

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO: web: just alert? }
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  { TODO: web: browser dialog, if possible? } Result := true;
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
end;

procedure TCastleApplication.DestroyBackend;
begin
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;

  { Hide HTML element with given id, ignore if it doesn't exist in DOM. }
  procedure HideHtmlElement(const ElementId: String);
  var
    Element: IJSElement;
    HtmlElement: IJSHTMLElement;
  begin
    Element := JSDocument.getElementById(ElementId);
    if Element = nil then
      Exit;
    HtmlElement := TJSHTMLDivElement.Cast(Element);
    HtmlElement.Style.CssText := 'display: none';
  end;

begin
  { Simplest implementation. This will be done in a loop using RequestAnimationFrame. }
  UpdateAndRenderEverything;
  if not WasFrameRendered then
  begin
    WasFrameRendered := true;
    HideHtmlElement('castle-loading-info');
    HideHtmlElement('castle-loading-progress-container');
  end;
  Result := not Terminated;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { Simplest implementation. }
  Result := ProcessMessage(false, false);
end;

procedure TCastleApplication.AnimationFrame(Time: TDOMHighResTimeStamp);
begin
  { TODO: web: recreate context if necessary.
    See https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/isContextLost }
	if GL.isContextLost then
    raise Exception.Create('Context has been lost');

  // This is completely not necessary anymore:
  // CGE gets time by FPC GetTickCount64, we don't need this Time parameter
  (*
  // calculate DeltaTime, update HasPreviousTime and PreviousTime
  if HasPreviousTime then
    DeltaTime := (Time - PreviousTime) / 1000
  else
  begin
    DeltaTime := 1 / 60; // arbitrary initial value
    HasPreviousTime := True;
  end;
  PreviousTime := Time;
  *)

  // Call Update, Render on all TCastleWindow
  ProcessAllMessages;

  // schedule next frame
  CanvasAnimationHandler := JSWindow.RequestAnimationFrame(@AnimationFrame);
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then
    Exit;

  // Do first Render/Update, and schedule next frame using RequestAnimationFrame
  AnimationFrame(0);
end;

procedure TCastleApplication.BackendTerminate;
begin
end;

function TCastleApplication.ScreenWidth: integer;
begin
  if GL <> nil then
    Result := GL.DrawingBufferWidth
  else
  begin
    WritelnWarning('Querying Application.ScreenWidth before GL context is initialized returns arbitrary value 64');
    Result := 64;
  end;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  if GL <> nil then
    Result := GL.DrawingBufferHeight
  else
  begin
    WritelnWarning('Querying Application.ScreenHeight before GL context is initialized returns arbitrary value 64');
    Result := 64;
  end;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'WebAssembly';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { TODO: web: check, using window_events example, what is the correct value
    (press "5", see if OnMotion is generated). }
  Result := true;
end;

{$endif read_implementation}
