{%MainUnit castlewindow.pas}
{
  Copyright 2004-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_uses}
Job.Js, CastleInternalJobWeb,
{$endif}

{$ifdef read_implementation_uses}
CastleInternalWebUtils,
{$endif}

{$ifdef read_window_interface}
strict private
  FCanvas: IJSHTMLCanvasElement;
  LastClientRectHeight, LastClientRectWidth, LastDevicePixelRatio: Double;
  DoneWarningCursor: Boolean;

  { Get mouse position from JS event, convert it to CGE coordinates. }
  function JsMousePositionToCastle(const Event: IJSMouseEvent): TVector2;

  function PointerDown(BaseEvent: IJSEvent): Boolean;
  function PointerUp(BaseEvent: IJSEvent): Boolean;
  function PointerMove(BaseEvent: IJSEvent): Boolean;

  { Handlers for legacy mouse events, used only while pointer lock is effective,
    because Chromium does not deliver pointerdown / pointerup during pointer lock. }
  function MouseDown(BaseEvent: IJSEvent): Boolean;
  function MouseUp(BaseEvent: IJSEvent): Boolean;

  function MouseOrPointerDown(const Event: IJSMouseEvent): Boolean;
  function MouseOrPointerUp(const Event: IJSMouseEvent): Boolean;
  function LostPointerCapture(BaseEvent: IJSEvent): Boolean;
  function KeyDown(BaseEvent: IJSEvent): Boolean;
  function KeyUp(BaseEvent: IJSEvent): Boolean;
  function ContextMenu(BaseEvent: IJSEvent): Boolean;

  { Called when the window loses focus, e.g. because the user switched
    to another application (Alt+Tab) or browser tab (Ctrl+Tab).
    We will not receive keyup events then, and we don't want to keep
    considering keys like "Ctrl" or "Alt" being pressed forever,
    this would also block navigation.

    Testcase: TCastleWalkNavigation in examples/terrain will not move player
    by AWSD after Ctrl+Tab (and go back to tab by mouse click)
    if it thinks Ctrl/Alt are pressed.

    See https://developer.mozilla.org/en-US/docs/Web/API/Window/blur_event .

    We could alternatively (or additionally) watch
    https://developer.mozilla.org/en-US/docs/Web/API/Document/visibilitychange_event }
  function Blur(BaseEvent: IJSEvent): Boolean;

  { Shortcut to query our PointerLock implementation, casted to TCastleWebPointerLock. }
  function PointerLockEffective: Boolean;
private
  property Canvas: IJSHTMLCanvasElement read FCanvas;
{$endif read_window_interface}

{$ifdef read_application_interface}
strict private
  CanvasAnimationHandler: Integer;
  WasFrameRendered: Boolean;
  FPageUrlParametersInitialized: Boolean;
  { Callback for requestAnimationFrame
    ( https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame ) }
  procedure AnimationFrame(Time: TDOMHighResTimeStamp);
{$endif read_application_interface}

{$ifdef read_implementation}

const
  CanvasElementId = 'castle-canvas';

{ Declare TWindowWebContainer and TCastleWebPointerLock ---------------------- }

type
  TWindowWebContainer = class(TWindowContainer)
  protected
    function CreatePointerLock: TCastleAbstractPointerLock; override;
  end;

  { Pointer lock implementation on web.

    Request "pointer lock" from the browser, which means that
    1. the mouse cursor is hidden and 2. mouse movement is unbounded
    and 3. mouse move is only reported as a delta.

    This is what we need to implement @link(TCastleContainer.PointerLock)
    on the web.
    On the web, SetCursor and setting MousePosition don't work (and it would
    be confusing if we would automatically start pointer lock e.g.
    on setting cursor to none), so pointer lock has to be requested explicitly
    by @code(Container.PointerLock.Active:=true)
    (and it works on all platforms).

    The request is not applied immediately, user may reject it.
    User may also exit pointer lock at any time, e.g. by pressing Escape key.
    We expose @link(Effective) to communicate this.
    So treat PointerLock.Active only as "wants to have pointer lock",
    while @link(Effective) is "actually has pointer lock now".

    See @url(https://developer.mozilla.org/en-US/docs/Web/API/Pointer_Lock_API
    pointer lock API docs). }
  TCastleWebPointerLock = class(TCastleAbstractPointerLock)
  strict private
    FEffective: Boolean;

    { Did the browser reject the UnadjustedMovement pointer lock option
      with NotSupportedError. Once we learn it's unsupported,
      RequestPointerLockNow does not request it again.

      Note that this can only be set @true on browsers that return
      Promise from requestPointerLock. This includes Chrome and Vivaldi on Linux,
      but not Firefox on Linux. Luckily, Firefox on Linux supports
      UnadjustedMovement. }
    FUnadjustedMovementUnsupported: Boolean;

    { Promise of the last RequestPointerLock call, so we can observe its
      rejection (LockPromiseRejected) and unregister callbacks when no longer relevant.

      Note that this is only assigned (i.e. non-nil; check with "= nil")
      on browsers that return
      Promise from requestPointerLock. This includes Chrome and Vivaldi on Linux,
      but not Firefox on Linux. }
    FRequestPromise: IJSPromise;

    { We detected that browser doesn't return Promise from requestPointerLock
      (like Firefox on Linux now). }
    FPromiseUnsupported: Boolean;

    { Listener for document.pointerlockchange event. }
    function PointerLockChange(BaseEvent: IJSEvent): Boolean;

    { Listener for document.pointerlockerror event, used only
      with browsers that don't return Promise from requestPointerLock
      (like Firefox on Linux now). }
    function PointerLockError(BaseEvent: IJSEvent): Boolean;

    { Rejection handler (OnRejected for RegisterPromiseCallbacks)
      of the RequestPointerLock promise. }
    function LockPromiseRejected(const aValue: Variant): Variant;

    { Request pointer lock from the browser now.
      Includes the UnadjustedMovement option, unless we already learned
      (FUnadjustedMovementUnsupported) that this browser rejects it. }
    procedure RequestPointerLockNow;
  protected
    procedure ActiveChanged(const NewValue: Boolean); override;
    function Delta(const Event: TInputMotion): TVector2; override;
  public
    procedure RegisterListeners;

    { Is the pointer lock, requested or cancelled by setting @link(Active),
      currently effective.

      This can only be @true if @link(Active) is also @true.

      The @link(TInputMotion.InternalWebDelta) is useful (non-zero)
      only when this is true.

      Moreover, do not depend on @link(TInputMotion.Position) and
      @link(TInputMotion.OldPosition) when this is true. Right now,
      browsers will just report them as if mouse never moved at all. }
    property Effective: Boolean read FEffective;

    { Calculate proper Delta from IJSPointerEvent, at each PointerMove. }
    procedure CalculateDelta(var SendMotion: TInputMotion;
      const Event: IJSPointerEvent);
  end;

{ TCastleWindow -------------------------------------------------------------- }

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
  CanvasBase := JSDocument.getElementById(CanvasElementId);
  if CanvasBase = nil then
    raise Exception.CreateFmt('Failed to find canvas element with id="%s"', [
      CanvasElementId
    ]);
  FCanvas := TJSHTMLCanvasElement.Cast(CanvasBase);

  ContextOptions := TJSWebGLContextAttributes.Create;

  { We need alpha = false to avoid subtle issues.
    Default value alpha = true causes blending to be applied to the whole canvas,
    so it's blended with the final page color (like white) when the pixel
    written to screen happens to have alpha < 1.0.

    Testcases where we have issues without this fix:
    - if a screen effect writes fragment alpha < 1.0
    - when rendering 3D shapes with blending, like:
      - cat hair in castle-model-viewer-mobile
      - trees' leaves in walking_adventure
    - when rendering text and 2D images with blending, like:
      - outline of "credits" text in platformer
      - outline of assets in platformer in fullscreen
      - heart that is invisible (once you lose health) in platformer
      - around cge logo in buttons on component_gallery
      - around "The Unholy Society" assets (when no screen effects active) }
  ContextOptions.Alpha := false;

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
  Canvas.addEventListener('contextmenu', @ContextMenu);

  { Pointer events for down/up are not delivered during pointer lock in Chromium.

    The Pointer Lock spec only guarantees MouseEvents in lock mode, see
    https://www.w3.org/TR/pointerlock-2/ ,
    """The pointer-lock target receives all relevant user generated MouseEvent
    events: namely, all user-generated mousemove, mousedown..."""
    And see issue https://github.com/w3c/pointerlock/issues/97 .

    So we additionally listen to
    legacy mousedown / mouseup, but use them only while pointer lock is effective.
    Conversely, PointerDown / PointerUp ignore events while pointer lock is
    effective, so we never report a single click twice (e.g. in Firefox, which
    delivers both pointer and mouse events during lock). }
  Canvas.addEventListener('mousedown', @MouseDown);
  Canvas.addEventListener('mouseup', @MouseUp);

  { Catching key events on canvas doesn't work, as the canvas needs to be focused,
    see https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event .
    For now, we catch key events on the whole window. }
  JSWindow.addEventListener('keydown', @KeyDown);
  JSWindow.addEventListener('keyup', @KeyUp);
  JSWindow.addEventListener('blur', @Blur);

  (Container.PointerLock as TCastleWebPointerLock).RegisterListeners;

  // TODO: web: initialize initial cursor position, set initial cursor, initialize DPI if possible
  // GetInitialCursorPos;
  // UpdateCursor;
  // InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
begin
  { One doesn't close WebGL context. }
end;

function TCastleWindow.PointerLockEffective: Boolean;
begin
  Result := (Container.PointerLock as TCastleWebPointerLock).Effective;
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

function TCastleWindow.JsMousePositionToCastle(const Event: IJSMouseEvent): TVector2;
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

function TCastleWindow.MouseOrPointerDown(const Event: IJSMouseEvent): Boolean;
var
  Button: TCastleMouseButton;
  FingerIndex: TFingerIndex;
  Position: TVector2;
begin
  Result := true; // assume not handled, in case of early exit

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

function TCastleWindow.MouseOrPointerUp(const Event: IJSMouseEvent): Boolean;
var
  Button: TCastleMouseButton;
  FingerIndex: TFingerIndex;
  Position: TVector2;
begin
  Result := true; // assume not handled, in case of early exit

  if not JsMouseButtonToCastle(Event.Button, Button) then
    Exit;
  FingerIndex := 0;
  Position := JsMousePositionToCastle(Event);
  DoMouseUp(Position, Button, FingerIndex);

  Result := false;
end;

function TCastleWindow.PointerDown(BaseEvent: IJSEvent): Boolean;
begin
  Result := true; // assume not handled, in case of early exit
  if PointerLockEffective then
    Exit; // clicks are handled by MouseDown
  Result := MouseOrPointerDown(TJSPointerEvent.Cast(BaseEvent));
end;

function TCastleWindow.PointerUp(BaseEvent: IJSEvent): Boolean;
begin
  Result := true; // assume not handled, in case of early exit
  if PointerLockEffective then
    Exit; // clicks are handled by MouseUp
  Result := MouseOrPointerUp(TJSPointerEvent.Cast(BaseEvent));
end;

function TCastleWindow.MouseDown(BaseEvent: IJSEvent): Boolean;
begin
  Result := true; // assume not handled, in case of early exit
  if not PointerLockEffective then
    Exit; // clicks are handled by PointerDown
  Result := MouseOrPointerDown(TJSMouseEvent.Cast(BaseEvent));
end;

function TCastleWindow.MouseUp(BaseEvent: IJSEvent): Boolean;
begin
  Result := true; // assume not handled, in case of early exit
  if not PointerLockEffective then
    Exit; // clicks are handled by PointerUp
  Result := MouseOrPointerUp(TJSMouseEvent.Cast(BaseEvent));
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
  SendMotion: TInputMotion;
begin
  Result := true; // assume not handled, in case of early exit
  Event := TJSPointerEvent.Cast(BaseEvent);

  { TODO: Report FPC/WebAssembly bug: Format %f hangs when given integer
    value. Below, it is critical we use %d for MovementX/MovementY.

  WritelnLog('TCastleWindow.PointerMove: offset %f %f, movement %d %d, screen %f %f, button: %d', [
    Event.OffsetX, Event.OffsetY,
    Event.MovementX, Event.MovementY,
    Event.ScreenX, Event.ScreenY,
    Event.Button
  ]);
  }

  { Firefox (tested on Linux) sends wrong PointerMove events during pointer lock,
    right *before* sending PointerDown/Up events. It seems it synthesizes
    "mouse moved to center of the screen" and sends them before sending
    the actual PointerDown/Up events.
    Since they are send before (not after) we cannot detect them by easy flag
    like FIgnoreNextMotion.

    Luckily, they contain Button <> -1 (which normal move events never do).
    So we just reject them in this case.

    Note that Chromium-based browsers (Vivaldi tested, also on Linux) also seem
    to send such events with Button=0, but they always contain MovementX=MovementY=0,
    so they would be harmless even if not ignored.

    Effectively, when either Firefox or Chromium-based browsers send PointerMove
    with Button <> -1 -> it's useless and we ignore it.
    Note that normal movement during mouse look, even with mouse button pressed,
    has Button=-1. }
  if Event.Button <> -1 then
    Exit(false);

  FingerIndex := 0;
  Position := JsMousePositionToCastle(Event);
  SendMotion := InputMotion(Container.MousePosition, Position,
    Container.MousePressed, FingerIndex);
  (Container.PointerLock as TCastleWebPointerLock).CalculateDelta(SendMotion, Event);
  DoMotion(SendMotion);

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

function TCastleWindow.Blur(BaseEvent: IJSEvent): Boolean;
begin
  ReleaseAllKeysAndMouse;
  Result := false;
end;

function TCastleWindow.ContextMenu(BaseEvent: IJSEvent): Boolean;
begin
  { Prevent browsers from showing their context menu,
    which makes using right mouse button impossible in applications.
    See https://developer.mozilla.org/en-US/docs/Web/API/Element/contextmenu_event
    (which also notes that user can avoid this by holding Shift key when
    right-clicking) and
    https://stackoverflow.com/questions/381795/how-to-disable-right-click-context-menu-in-javascript#16987577
    https://stackoverflow.com/questions/51958065/prevent-context-menu-from-opening-on-a-right-click-over-element
  }
  BaseEvent.PreventDefault;
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
    begin
      WritelnWarningOnce(DoneWarningCursor, 'Web',
        'Changing mouse cursor on the web is not possible. If you want to hide the cursor for pointer lock, the "Container.PointerLock.Active := true" is sufficient.');
    end;
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO: web: what is correct here } true;
end;

procedure TCastleWindow.SystemSetMousePosition(const Value: TVector2);
begin
  { Repositioning the mouse on the web is not possible,
    so this will never be implemented.
    See PointerLock, which addresses a similar need, but in a different way. }
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

function TCastleApplication.MultipleWindowsPossible: Boolean;
begin
  Result := false;
end;

function TCastleApplication.PageUrl: String;
begin
  Result := JSWindow.Location.Href;
end;

function TCastleApplication.PageUrlQuery: String;
begin
  Result := JSWindow.Location.Search;
end;

function TCastleApplication.PageUrlParameters: TStringStringMap;
begin
  if not FPageUrlParametersInitialized then
  begin
    // for now, TCastleApplication.Create creates a useless instance that we'll discard here
    FreeAndNil(FPageUrlParameters);
    FPageUrlParameters := UrlQueryParameters(PageUrlQuery);
    FPageUrlParametersInitialized := true;
  end;

  Result := FPageUrlParameters;
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { On the web, setting MousePosition does nothing, in particular it doesn't
    generate Motion events. }
  Result := false;
end;

{ TWindowWebContainer -------------------------------------------------------- }

function TWindowWebContainer.CreatePointerLock: TCastleAbstractPointerLock;
begin
  Result := TCastleWebPointerLock.Create(Self);
end;

{ TCastleWebPointerLock ------------------------------------------------------ }

procedure TCastleWebPointerLock.RegisterListeners;
begin
  { Pointer Lock API: observe the pointer lock state
    https://developer.mozilla.org/en-US/docs/Web/API/Pointer_Lock_API }
  JSDocument.addEventListener('pointerlockchange', @PointerLockChange);

  { Note: We do not listen for the 'pointerlockerror' event.

    - When FPromiseUnsupported = @false (so requestPointerLock returns a Promise):

        A failed RequestPointerLock also rejects the returned promise, and only
        the promise rejection carries the reason (error.name), which we need to
        distinguish "UnadjustedMovement unsupported" from other failures.
        So we handle all failures in LockPromiseRejected instead.

    - When FPromiseUnsupported = @true (so requestPointerLock does not return a Promise):

        We will register 'pointerlockerror' listener in
        RequestPointerLockNow, when we detect that FPromiseUnsupported
        should be true.
  }
end;

function TCastleWebPointerLock.PointerLockChange(BaseEvent: IJSEvent): Boolean;
var
  NewEffective: Boolean;
begin
  NewEffective :=
    (JSDocument.PointerLockElement <> nil) and
    (JSDocument.PointerLockElement.Id = CanvasElementId);

  // log NewEffective and JSDocument.PointerLockElement
  if JSDocument.PointerLockElement <> nil then
    WritelnLog('Pointer locked to element: tag=%s id=%s class=%s. This is our canvas: %s', [
      JSDocument.PointerLockElement.TagName,
      JSDocument.PointerLockElement.Id,
      JSDocument.PointerLockElement.ClassName_,
      BoolToStr(NewEffective, true)
    ])
  else
    WritelnLog('Pointer not locked to any element.');

  if Active then
  begin
    FEffective := NewEffective;
    if not NewEffective then
      CancelActive;
    Assert(Active = Effective);
  end else
  begin
    if NewEffective then
      WritelnWarning('Web', 'Browser reports pointer lock became effective, but it was not requested by setting PointerLock.Active. Ignoring.');
  end;

  Result := true;
end;

function TCastleWebPointerLock.PointerLockError(BaseEvent: IJSEvent): Boolean;
begin
  WritelnWarning('Web', 'Pointer lock error');
  FEffective := false;
  if Active then
    CancelActive;
  Result := true;
end;

function TCastleWebPointerLock.LockPromiseRejected(const aValue: Variant): Variant;
var
  Error: IJSDOMException;
  ErrorName: String;
begin
  Result := aValue;

  { If user no longer wants pointer lock, ignore this rejection. }
  if not Active then
    Exit;

  Error := TJSDOMException.Cast(aValue);
  ErrorName := Error.Name;

  if (ErrorName = 'NotSupportedError') and (not FUnadjustedMovementUnsupported) then
  begin
    { The UnadjustedMovement option is not supported on this browser/OS.
      Remember it (to not request it again) and retry now without it. }
    WritelnLog('Web', 'Pointer lock UnadjustedMovement option not supported, retrying without it');
    FUnadjustedMovementUnsupported := true;
    RequestPointerLockNow;
  end else
  begin
    { Pointer lock request failed for some other reason,
      e.g. request was not within / right after a user gesture.
      We cannot recover, so reset the "wants pointer lock" intent and notify
      listeners (e.g. to turn off MouseLook). }
    WritelnWarning('Web', 'Pointer lock request failed: %s (%s)', [
      ErrorName,
      Error.Message
    ]);
    FEffective := false;
    CancelActive;
  end;
end;

procedure TCastleWebPointerLock.RequestPointerLockNow;
var
  Window: TCastleWindow;
  Options: IJSPointerLockOptions;
begin
  Window := (Container as TWindowContainer).Parent;

  { Use UnadjustedMovement to avoid OS mouse acceleration,
    get raw precise movement.

    Not all browsers/platforms support it. Google Chrome and Vivaldi
    (also based on Chromium) on Linux reject such request with NotSupportedError:

      """Uncaught (in promise) NotSupportedError: The options asked for in this
      request are not supported on this platform."""

    Tested with fail:
    - Vivaldi 8.0.4033.46 (latest on 2026-06-13), Linux/x86_64
    - Google Chrome 149.0.7827.114 (latest on 2026-06-13), Linux/x86_64

    Firefox on Linux supports it. Tested with success:
    - Firefox 151.0.4 (latest on 2026-06-13), Linux/x86_64

    See https://web.dev/articles/disable-mouse-acceleration
    where it's confirmed that UnadjustedMovement is not supported on Linux
    by Chromium-based browsers.
  }
  if not FUnadjustedMovementUnsupported then
  begin
    Options := TJSPointerLockOptions.Create;
    Options.UnadjustedMovement := true;
    FRequestPromise := Window.Canvas.RequestPointerLock(Options);
  end else
    FRequestPromise := Window.Canvas.RequestPointerLock;

  { Did JS return a Promise or null/undefined?

    requestPointerLock is declared to return IJSPromise (a Pascal interface),
    so when the browser returns no promise (JS null/undefined), Job.Js gives us
    a nil interface (see TJSObject.InvokeJSObjectResult) -- so we check "= nil".

    Note: do *not* use VarIsNull here. The VarIsNull / Variants.Null advice
    from https://wiki.lazarus.freepascal.org/WebAssembly/DOM applies only
    to JS values fetched as Variant, where JS null becomes Variants.Null.
    Checking here VarIsNull(FRequestPromise) would be useless
    (always return false). }
  if FRequestPromise = nil then
  begin
    if not FPromiseUnsupported then
    begin
      FPromiseUnsupported := true;
      WritelnLog('Web', 'Browser does not return a promise from requestPointerLock, using pointerlockerror for error handling.');
      { We assign PointerLockError listener then.
        We don't use it with browsers that return a promise, to avoid
        double-calling the error handler. }
      JSDocument.addEventListener('pointerlockerror', @PointerLockError);
    end;
  end else
  begin
    { Observe the rejection. We don't need an accepted callback: a successful
      lock is reported by the pointerlockchange event (PointerLockChange). }
    RegisterPromiseCallbacks(FRequestPromise, nil, @LockPromiseRejected);
  end;
end;

procedure TCastleWebPointerLock.ActiveChanged(const NewValue: Boolean);
begin
  if NewValue then
  begin
    { Note: We considered a mechanism to "schedule" pointer lock request
      in case this is called outside of user gesture (like
      "WalkNavigation.MouseLook := buttonRight in Container.MousePressed"
      in Update of examples/creature_behaviors/ ).
      But

      - This scheduling is not really a solution. In the above example,
        Update happens after mouse click, so the "scheduled activation"
        would not happen anytime soon.

      - It seems it's also not necessary, as browsers allow pointer lock
        requests also a bit later after user gesture, not necessarily
        in the same event handler.
        See https://developer.mozilla.org/en-US/docs/Glossary/Transient_activation
        "Transient activation expires after a timeout (if not renewed by
        further interaction)".
    }
    RequestPointerLockNow;
  end else
  begin
    { Stop observing any pending request promise, so a late rejection
      (e.g. NotSupportedError) won't trigger a retry after we no longer
      want the lock. }
    UnregisterPromiseAndNil(FRequestPromise);
    JSDocument.ExitPointerLock;
    FEffective := false;
  end;
end;

function TCastleWebPointerLock.Delta(const Event: TInputMotion): TVector2;
begin
  if Effective then
    Result := Event.InternalWebDelta
  else
    Result := TVector2.Zero;
end;

procedure TCastleWebPointerLock.CalculateDelta(var SendMotion: TInputMotion;
  const Event: IJSPointerEvent);
begin
  { What to use for deltas?

    - movementX/Y are designed for this

    - experiment rejected: Since
      https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX
      https://github.com/w3c/pointerlock/issues/42
      says their coordinates are unreliable and suggests to calculate deltas
      ourselves, by comparing screenX/Y.
      But that's wrong advise, as of 2026-06: screenX/Y
      - in Firefox 151.0.4 is mostly zero,
      - and reports horizontal-only deltas in Vivaldi 8.0.4033.46 .

    - experiment rejected: we tried scaling them just like we scale position
      in JsMousePositionToCastle,
        SendMotion.InternalWebDelta := Vector2(
           Event.MovementX * Canvas.Width / Canvas.OffsetWidth,
          -Event.MovementY * Canvas.Height / Canvas.OffsetHeight);
      but this resulted in speed being *less* consistent with how MousePosition
      changes in Firefox in dragging_test, when zoomed in/out.

    All in all, just using movementX/Y is most reliable now.
  }
  if FEffective then
    SendMotion.InternalWebDelta := Vector2(Event.MovementX, -Event.MovementY);
end;

{$endif read_implementation}
