{%MainUnit castlewindow.pas}
{
  Copyright 2022-2023 Michalis Kamburelis and GLPT, LCL contributors.

  The Cocoa integration code is based on

  - GLPT

      The base code to create window with OpenGL context
      comes from GLPT original, from https://github.com/daar/GLPT .
      The fork from https://github.com/genericptr/GLPT was also useful.

      Thank you go to all GLPT contributors, in particular:
      - Darius Blaszyk (daar)
      - Ryan Joseph (genericptr)

  - LCL Cocoa widgetset

      The code for alerts and file/color dialogs is based
      on LCL Cocoa widgetset.
      In many other cases we consulted LCL Cocoa widgetset code
      to see "how to do something properly with Cocoa".

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO p1:
  - fullscreen: should be on top of dock / menu

  TODO p2:
  - use filters in FileDialog to highlight correct files
  - update caption at runtime
  - avoid deprecated warnings at assembling
    Assembling castlewindow
    /Users/administrator/sources/castle-engine/castle-engine/examples/user_interface/test_all_state_events/castle-engine-output/compilation/x86_64-darwin/castlewindow.s:263336:10: warning: section "__datacoal_nt" is deprecated
    .section __DATA, __datacoal_nt, coalesced
             ^      ~~~~~~~~~~~~~~
    /Users/administrator/sources/castle-engine/castle-engine/examples/user_interface/test_all_state_events/castle-engine-output/compilation/x86_64-darwin/castlewindow.s:263336:10: note: change section name to "__data"
    .section __DATA, __datacoal_nt, coalesced
             ^      ~~~~~~~~~~~~~~
    /Users/administrator/sources/castle-engine/castle-engine/examples/user_interface/test_all_state_events/castle-engine-output/compilation/x86_64-darwin/castlewindow.s:263484:10: warning: section "__datacoal_nt" is deprecated
    .section __DATA, __datacoal_nt, coalesced
             ^      ~~~~~~~~~~~~~~
    /Users/administrator/sources/castle-engine/castle-engine/examples/user_interface/test_all_state_events/castle-engine-output/compilation/x86_64-darwin/castlewindow.s:263484:10: note: change section name to "__data"
    .section __DATA, __datacoal_nt, coalesced
             ^      ~~~~~~~~~~~~~~
    Unknown how to deal with it.
    https://wiki.lazarus.freepascal.org/Mac_Installation_FAQ#Lazarus_reports_success.2C_but_there_are_errors.21
    mentions it, without any answer except "ignore them"..
  - entering modal message leaves the key pressed down, testcase: test_all_state_events, press Q
  - hiding cursor doesn't work for unknown reason
}

{$ifdef read_interface_uses}
CocoaAll,
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
private
  ref: NSWindow;
  GLcontext: NSOpenGLContext;
  FMenuForcefullyDisabled: Boolean;
  procedure FullScreenBegin;
  procedure FullScreenEnd;
  procedure SetMenuForcefullyDisabled(const Value: Boolean);
  property MenuForcefullyDisabled: Boolean read FMenuForcefullyDisabled write SetMenuForcefullyDisabled;

  { Show Cocoa modal message box.

    Use Cocoa Sheet called NSAlert.
    See https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Sheets/Tasks/UsingAlertSheets.html#//apple_ref/doc/uid/20001045-BABFIBIA .
    Based on Lazarus LCL lcl/interfaces/cocoa/cocoalclintf.inc ,
    but greatly simplified.

    @param(AlertStyle Pass NSCriticalAlertStyle or NSInformationalAlertStyle.
      You can also use NSWarningAlertStyle, but Apple docs openly say it is
      equivalent to NSInformationalAlertStyle, see
      https://developer.apple.com/documentation/appkit/nsalertstyle.)

    @param(ButtonCaptions Array of button captions.)

    @param(ButtonResults Array of button result values. May have equal length as ButtonCaptions.)

    @returns(The value from ButtonResults corresponding to the button that was pressed.
      Returns -1 if application ended while the message was active.) }
  function CocoaPromptUser(const DialogCaption: String;
    const DialogMessage: String;
    const AlertStyle: NSAlertStyle;
    const ButtonCaptions: array of String;
    const ButtonResults: array of NSInteger): NSInteger;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  CursorHidden: Boolean;
{$endif read_application_interface}

{$ifdef read_implementation}

{$I castlewindow_cocoa_menu.inc}
{$I castlewindow_cocoa_message.inc}
{$I castlewindow_cocoa_filedialog.inc}
{$I castlewindow_cocoa_colordialog.inc}

{ Integration code from GLPT ------------------------------------------------- }

var
  KeyCodeCocoaToCastle: array [0..127] of TKey = (
    {   0 }   keyA,
    {   1 }   keyS,
    {   2 }   keyD,
    {   3 }   keyF,
    {   4 }   keyH,
    {   5 }   keyG,
    {   6 }   keyZ,
    {   7 }   keyX,
    {   8 }   keyC,
    {   9 }   keyV,
    {  10 }   keyNone, // TODO, GLPT_SCANCODE_NONUSBACKSLASH, { GLPT_SCANCODE_NONUSBACKSLASH on ANSI and JIS keyboards (if this key would exist there), GLPT_SCANCODE_GRAVE on ISO. (The USB keyboard driver actually translates these usage codes to different virtual key codes depending on whether the keyboard is ISO/ANSI/JIS. That's why you have to help it identify the keyboard type when you plug in a PC USB keyboard. It's a historical thing - ADB keyboards are wired this way.) }
    {  11 }   keyB,
    {  12 }   keyQ,
    {  13 }   keyW,
    {  14 }   keyE,
    {  15 }   keyR,
    {  16 }   keyY,
    {  17 }   keyT,
    {  18 }   key1,
    {  19 }   key2,
    {  20 }   key3,
    {  21 }   key4,
    {  22 }   key6,
    {  23 }   key5,
    {  24 }   keyEqual,
    {  25 }   key9,
    {  26 }   key7,
    {  27 }   keyMinus,
    {  28 }   key8,
    {  29 }   key0,
    {  30 }   keyRightBracket,
    {  31 }   keyO,
    {  32 }   keyU,
    {  33 }   keyLeftBracket,
    {  34 }   keyI,
    {  35 }   keyP,
    {  36 }   keyEnter,
    {  37 }   keyL,
    {  38 }   keyJ,
    {  39 }   keyApostrophe,
    {  40 }   keyK,
    {  41 }   keySemicolon,
    {  42 }   keyBackslash,
    {  43 }   keyComma,
    {  44 }   keySlash,
    {  45 }   keyN,
    {  46 }   keyM,
    {  47 }   keyPeriod,
    {  48 }   keyTab,
    {  49 }   keySpace,
    {  50 }   keyNone, // TODO, GLPT_SCANCODE_GRAVE, { GLPT_SCANCODE_GRAVE on ANSI and JIS keyboards, GLPT_SCANCODE_NONUSBACKSLASH on ISO (see comment about virtual key code 10 above) }
    {  51 }   keyBackspace,
    {  52 }   keyNumpadEnter, { keyboard enter on portables }
    {  53 }   keyEscape,
    {  54 }   keyNone, // TODO, GLPT_SCANCODE_RGUI,
    {  55 }   keyNone, // TODO, GLPT_SCANCODE_GUI,
    {  56 }   keyShift, // left shift
    {  57 }   keyCapsLock,
    {  58 }   keyAlt, // left alt
    {  59 }   keyCtrl, // left ctrl
    {  60 }   keyShift, // right shift
    {  61 }   keyAlt, // right alt
    {  62 }   keyCtrl, // right ctrl
    {  63 }   keyNone, // TODO, GLPT_SCANCODE_RGUI, { fn on portables, acts as a hardware-level modifier already, so we don't generate events for it, also XK_Meta_R }
    {  64 }   keyNone, // TODO, GLPT_SCANCODE_F17,
    {  65 }   keyPeriod, // keyNumpadPeriod, but we don't have such specific key
    {  66 }   keyNone, { unknown (unused?) }
    {  67 }   keyNumpadMultiply,
    {  68 }   keyNone, { unknown (unused?) }
    {  69 }   keyNumpadPlus,
    {  70 }   keyNone, { unknown (unused?) }
    {  71 }   keyNone, // TODO, GLPT_SCANCODE_NUMLOCKCLEAR,
    {  72 }   keyNone, // TODO, GLPT_SCANCODE_VOLUMEUP,
    {  73 }   keyNone, // TODO, GLPT_SCANCODE_VOLUMEDOWN,
    {  74 }   keyNone, // TODO, GLPT_SCANCODE_MUTE,
    {  75 }   keyNumpadDivide,
    {  76 }   keyNumpadEnter, { keypad enter on external keyboards, fn-return on portables }
    {  77 }   keyNone, { unknown (unused?) }
    {  78 }   keyNumpadMinus,
    {  79 }   keyNone, // TODO, GLPT_SCANCODE_F18,
    {  80 }   keyNone, // TODO, GLPT_SCANCODE_F19,
    {  81 }   keyEqual, // keyNumpadEqual, but we don't have such specific key
    {  82 }   keyNumpad0,
    {  83 }   keyNumpad1,
    {  84 }   keyNumpad2,
    {  85 }   keyNumpad3,
    {  86 }   keyNumpad4,
    {  87 }   keyNumpad5,
    {  88 }   keyNumpad6,
    {  89 }   keyNumpad7,
    {  90 }   keyNone, { unknown (unused?) }
    {  91 }   keyNumpad8,
    {  92 }   keyNumpad9,
    {  93 }   keyNone, // TODO, GLPT_SCANCODE_INTERNATIONAL3, { Cosmo_USB2ADB.c says "Yen (JIS)" }
    {  94 }   keyNone, // TODO, GLPT_SCANCODE_INTERNATIONAL1, { Cosmo_USB2ADB.c says "Ro (JIS)" }
    {  95 }   keyComma, { Cosmo_USB2ADB.c says ", JIS only" }  // keyNumpadComma, but we don't have such specific key
    {  96 }   keyF5,
    {  97 }   keyF6,
    {  98 }   keyF7,
    {  99 }   keyF3,
    { 100 }   keyF8,
    { 101 }   keyF9,
    { 102 }   keyNone, // TODO, GLPT_SCANCODE_LANG2, { Cosmo_USB2ADB.c says "Eisu" }
    { 103 }   keyF11,
    { 104 }   keyNone, // TODO, GLPT_SCANCODE_LANG1, { Cosmo_USB2ADB.c says "Kana" }
    { 105 }   keyPrintScreen, { On ADB keyboards, this key is labeled "F13/print screen". Problem: USB has different usage codes for these two functions. On Apple USB keyboards, the key is labeled "F13" and sends the F13 usage code (GLPT_SCANCODE_F13). I decided to use GLPT_SCANCODE_PRINTSCREEN here nevertheless since SDL applications are more likely to assume the presence of a print screen key than an F13 key. }
    { 106 }   keyNone, // TODO, GLPT_SCANCODE_F16,
    { 107 }   keyNone, // TODO, GLPT_SCANCODE_SCROLLLOCK, { F14/scroll lock, see comment about F13/print screen above }
    { 108 }   keyNone, { unknown (unused?) }
    { 109 }   keyF10,
    { 110 }   keyNone, // TODO, GLPT_SCANCODE_APPLICATION, { windows contextual menu key, fn-enter on portables }
    { 111 }   keyF12,
    { 112 }   keyNone, { unknown (unused?) }
    { 113 }   keyNone, // TODO, GLPT_SCANCODE_PAUSE, { F15/pause, see comment about F13/print screen above }
    { 114 }   keyInsert, { the key is actually labeled "help" on Apple keyboards, and works as such in Mac OS, but it sends the "insert" usage code even on Apple USB keyboards }
    { 115 }   keyHome,
    { 116 }   keyPageUp,
    { 117 }   keyDelete,
    { 118 }   keyF4,
    { 119 }   keyEnd,
    { 120 }   keyF2,
    { 121 }   keyPageDown,
    { 122 }   keyF1,
    { 123 }   keyArrowLeft,
    { 124 }   keyArrowRight,
    { 125 }   keyArrowDown,
    { 126 }   keyArrowUp,
    { 127 }   keyNone // TODO, GLPT_SCANCODE_POWER
);

{=============================================}
{@! ___BORDERLESS WINDOW___ }
{=============================================}

type
  TBorderlessWindow = objcclass (NSWindow)
  public
    function initWithContentRect_styleMask_backing_defer (contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBool): id; override;
    function initWithContentRect(contentRect: NSRect): id; message 'initWithContentRect:';
    function canBecomeKeyWindow: ObjCBool; override;
    function canBecomeMainWindow: ObjCBool; override;
    procedure setKeepFullScreenAlways (newValue: boolean); message 'setKeepFullScreenAlways:';
    procedure dealloc; override;
  private
    keepFullScreenAlways: boolean;
    procedure screenParametersChanged (notification: NSNotification); message 'screenParametersChanged:';
  end;

function TBorderlessWindow.canBecomeKeyWindow: ObjCBool;
begin
  result := true;
end;

function TBorderlessWindow.canBecomeMainWindow: ObjCBool;
begin
  result := true;
end;

procedure TBorderlessWindow.setKeepFullScreenAlways (newValue: boolean);
begin
  orderFrontRegardless;
  keepFullScreenAlways := newValue;
  if keepFullScreenAlways then
    setFrame_display(screen.frame, true);
end;

procedure TBorderlessWindow.screenParametersChanged (notification: NSNotification);
begin
  if keepFullScreenAlways then
    setFrame_display(NSScreen.mainScreen.frame, true);
end;

procedure TBorderlessWindow.dealloc;
begin
  NSNotificationCenter.defaultCenter.removeObserver(self);
  inherited dealloc;
end;

function TBorderlessWindow.initWithContentRect_styleMask_backing_defer (contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBool): id;
begin
  result := inherited initWithContentRect_styleMask_backing_defer(contentRect, aStyle, bufferingType, flag);
  if result <> nil then
    NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result, objcselector('screenParametersChanged:'), NSApplicationDidChangeScreenParametersNotification, nil);
end;

function TBorderlessWindow.initWithContentRect(contentRect: NSRect): id;
begin
  result := initWithContentRect_styleMask_backing_defer(contentRect, NSBorderlessWindowMask, NSBackingStoreBuffered, false);
  if result <> nil then
    begin
      result.setMovableByWindowBackground(false);
      //result.setBackgroundColor(NSColor.clearColor);
      //result.setOpaque(false);
      result.setHasShadow(false);
      result.setExcludedFromWindowsMenu(true);
      self := result;
    end;
end;

{=============================================}
{@! ___COCOA WINDOW___ }
{=============================================}

type
  TCocoaWindow = objcclass (TBorderlessWindow)
  private
    ref: TCastleWindow;
    WithinCloseBackend: Cardinal;
  public
    procedure close; override;
    procedure becomeKeyWindow; override;
    procedure resignKeyWindow; override;
    procedure doCommandBySelector(aSelector: SEL); override;
  end;

procedure TCocoaWindow.close;
begin
  if WithinCloseBackend = 0 then
    ref.DoCloseQuery;
  inherited;
end;

procedure TCocoaWindow.doCommandBySelector(aSelector: SEL);
begin
  // do nothing to prevent beeping
end;

procedure TCocoaWindow.becomeKeyWindow;
begin
  inherited;
  // ref.FKeyWindow := true; // we don't track such property now
end;

procedure TCocoaWindow.resignKeyWindow;
begin
  inherited;
  // ref.FKeyWindow := false;
end;

{=============================================}
{@! ___COCOA APP___ }
{=============================================}

type
  TCocoaApp = objcclass (NSApplication)
    procedure poll; message 'poll';
  end;

type
  TCocoaAppDelegate = objcclass (NSObject, NSApplicationDelegateProtocol)
    procedure application_openFiles(sender: NSApplication; filenames: NSArray); message 'application:openFiles:';
  end;

procedure TCocoaApp.poll;
var
  win: TCastleWindow = nil;

  procedure CheckMotionDragged(const MouseButton: TCastleMouseButton);
  begin
    if not (MouseButton in win.MousePressed) then
    begin
      WritelnWarning('Cocoa event indicated that %s should be pressed, but not registered as pressed yet', [
        MouseButtonStr[MouseButton]
      ]);
      win.DoMouseDown(win.MousePosition, MouseButton, 0);
    end;
  end;

var
  event: NSEvent;
  pool: NSAutoreleasePool;
  CastleMouseButton: TCastleMouseButton;
begin
  pool := NSAutoreleasePool.alloc.init;
  event := nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, {NSDate.distantPast}nil, NSDefaultRunLoopMode, true);
  if event <> nil then
    begin
      //WritelnLog('Cocoa event ' + event.description.UTF8String);

      if (event.window <> nil) and
         { We need to check the class of event.window, as when clicking "minimize" / "zoom" in menu
           we get events not related to this window.

           Note: It seems we cannot use
             (event.window is TCocoaWindow)
           or even
             (event.window.ClassType = TCocoaWindow)
           with objcclass. Comparing ClassName.UTF8String seems like the only solution
           (report if you know a cleaner approach).
         }
         (event.window.ClassName.UTF8String = 'TCocoaWindow') then
        win := TCocoaWindow(event.window).ref;

      if win <> nil then
      begin
        case event.type_ of
          NSMouseMoved,
          NSLeftMouseDragged,
          NSRightMouseDragged,
          NSOtherMouseDragged:
            begin
              case event.type_ of
                NSLeftMouseDragged : CheckMotionDragged(buttonLeft);
                NSRightMouseDragged: CheckMotionDragged(buttonRight);
                NSOtherMouseDragged: CheckMotionDragged(buttonMiddle);
              end;
              win.DoMotion(InputMotion(win.MousePosition,
                Vector2(event.locationInWindow.x, event.locationInWindow.y), win.MousePressed, 0));
            end;
          NSLeftMouseDown, NSRightMouseDown, NSOtherMouseDown:
            begin
              case event.type_ of
                NSLeftMouseDown:  CastleMouseButton := buttonLeft;
                NSRightMouseDown: CastleMouseButton := buttonRight;
                NSOtherMouseDown: CastleMouseButton := buttonMiddle;
                else raise EInternalError.Create('Unexpected event.type_');
              end;
              win.DoMouseDown(Vector2(event.locationInWindow.x, event.locationInWindow.y),
                CastleMouseButton, 0);
            end;
          NSLeftMouseUp, NSRightMouseUp, NSOtherMouseUp:
            begin
              case event.type_ of
                NSLeftMouseUp:  CastleMouseButton := buttonLeft;
                NSRightMouseUp: CastleMouseButton := buttonRight;
                NSOtherMouseUp: CastleMouseButton := buttonMiddle;
                else raise EInternalError.Create('Unexpected event.type_');
              end;
              win.DoMouseUp(Vector2(event.locationInWindow.x, event.locationInWindow.y),
                CastleMouseButton, 0);
            end;
          NSKeyDown:
            win.DoKeyDown(KeyCodeCocoaToCastle[event.keycode], event.characters.UTF8String);
            // TODO we could use event.isARepeat for our KeyRepeated, instead we always calculate it manually
          NSKeyUp:
            win.DoKeyUp(KeyCodeCocoaToCastle[event.keycode]);
          NSScrollWheel:
            begin
              if event.deltaX <> 0 then
                win.DoMouseWheel(event.deltaX, false);
              if event.deltaY <> 0 then
                win.DoMouseWheel(event.deltaY, true);
            end;
          otherwise
            ;
        end;
      end else
      begin
        { This is normal:
          - at start Cocoa sends message
            NSEvent: type=Kitdefined loc=(0,1080) time=608959.3 flags=0x40 win=0x0 winNum=0 ctxt=0x0 subtype=1 data1=627 data2=0
          - when doing minimize / zoom, we get messages for window that is not TCocoaWindow
        }
        //WritelnWarning('Ignoring Cocoa event %s, window does not exist', [event.description.UTF8String]);
      end;

      sendEvent(event);
      updateWindows;
    end;
  pool.release;
end;

procedure TCocoaAppDelegate.application_openFiles(sender: NSApplication; filenames: NSArray);
var
  lFiles: array of string;
  lNSStr: NSString;
  I: Integer;
begin
  SetLength(lFiles, filenames.count);
  for I := 0 to filenames.count-1 do
  begin
    lNSStr := NSString(filenames.objectAtIndex(I));
    lFiles[I] := lNSStr.UTF8String;
  end;
  if Application.MainWindow <> nil then
    Application.MainWindow.DoDropFiles(lFiles);
end;

{=============================================}
{@! ___OPENGL VIEW___ }
{=============================================}

type
  TOpenGLView = objcclass (NSView)
    public
      function initWithFrame(frameRect: NSRect): id; override;
      function isOpaque: ObjCBool; override;
      procedure viewDidMoveToWindow; override;
      procedure mouseEntered(theEvent: NSEvent); override;
      procedure mouseExited(theEvent: NSEvent); override;
      procedure doCommandBySelector(aSelector: SEL); override;
      procedure updateTrackingAreas; override;
      procedure keyDown(theEvent: NSEvent); override;
      procedure drawRect(dirtyRect: NSRect); override;

      { Realizes TCastleWindow.OnDropFiles, allows to handle dropping files. }
      function performDragOperation(sender: NSDraggingInfoProtocol): ObjCBOOL; override;
      function prepareForDragOperation(sender: NSDraggingInfoProtocol): ObjCBool; override;
      function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation; override;
    private
      openGLContext: NSOpenGLContext;
      trackingArea: NSTrackingArea;

      function defaultPixelFormat: NSOpenGLPixelFormat; message 'defaultPixelFormat';
      function windowRef: TCastleWindow; message 'windowRef';
      procedure frameChanged (sender: NSNotification); message 'frameChanged:';
      procedure reshape; message 'reshape';
  end;

// note: setValues_forParameter in RTL headers is parsed wrong
type
  NSOpenGLContext_Fixed = objccategory external (NSOpenGLContext)
    procedure setValues_forParameter_fixed (vals: pointer; param: NSOpenGLContextParameter); overload; message 'setValues:forParameter:';
  end;

function TOpenGLView.windowRef: TCastleWindow;
begin
  result := TCocoaWindow(window).ref;
end;

procedure TOpenGLView.updateTrackingAreas;
begin
  if trackingArea <> nil then
    removeTrackingArea(trackingArea);
  trackingArea := NSTrackingArea.alloc.initWithRect_options_owner_userInfo(bounds, NSTrackingMouseEnteredAndExited + NSTrackingActiveAlways, self, nil).autorelease;
  addTrackingArea(trackingArea);
end;

procedure TOpenGLView.keyDown(theEvent: NSEvent);
begin
  // do nothing to prevent beeping
end;

procedure TOpenGLView.doCommandBySelector(aSelector: SEL);
begin
  // do nothing to prevent beeping
end;

procedure TOpenGLView.mouseEntered(theEvent: NSEvent);
begin
  windowRef.FFocused := true;
end;

procedure TOpenGLView.mouseExited(theEvent: NSEvent);
begin
  windowRef.FFocused := false;
end;

procedure TOpenGLView.viewDidMoveToWindow;
var
  swapInterval: integer = 1;
  opacity: integer = 0;
begin
  inherited viewDidMoveToWindow;

  if openGLContext = nil then
    begin
      openGLContext := NSOpenGLContext.alloc.initWithFormat_shareContext(defaultPixelFormat, nil);
      if openGLContext = nil then
        raise EInternalError.Create('invalid NSOpenGLContext');
      openGLContext.makeCurrentContext;
      openGLContext.setView(self);

      openGLContext.setValues_forParameter_fixed(@swapInterval, NSOpenGLCPSwapInterval);

      if not isOpaque then
        openGLContext.setValues_forParameter_fixed(@opacity, NSOpenGLCPSurfaceOpacity);
    end;
  if window = nil then
    openGLContext.clearDrawable;
end;

procedure TOpenGLView.frameChanged (sender: NSNotification);
begin
  if openGLContext <> nil then
    reshape;
end;

procedure TOpenGLView.drawRect(dirtyRect: NSRect);
begin
  // This has to be done in Cocoa_SwapBuffers, at least on macOS 12
  // openGLContext.flushBuffer;
end;

procedure TOpenGLView.reshape;
var
  ScaleFactor: Single;
begin
  openGLContext.update;

  { To be secure, check both window and windowRef.
    When window=nil, accessing windowRef would be an error. }
  if (window <> nil) and (windowRef <> nil) then
  begin
    { On high-resolution displays, the bounds.size needs to be scaled by
      https://developer.apple.com/documentation/appkit/nswindow/1419459-backingscalefactor?language=objc
      See also
      https://developer.apple.com/documentation/appkit/nsviewlayercontentscaledelegate?language=objc
      https://developer.apple.com/documentation/quartzcore/calayer/1410746-contentsscale?language=objc
      (possibly we could use NSViewLayerContentScaleDelegate
      to avoid such scaling on OpenGL control; for now this solution seems easier). }
    if window.respondsToSelector( ObjCSelector('backingScaleFactor')) then
      ScaleFactor := window.backingScaleFactor
    else
    if window.respondsToSelector( ObjCSelector('userSpaceScaleFactor')) then // for older OSX
      {$warnings off}
      // do not warn about userSpaceScaleFactor being deprecated, we only use it for older OS
      ScaleFactor := window.userSpaceScaleFactor
      {$warnings on}
    else
      ScaleFactor := 1;

    windowRef.DoResize(
      trunc(ScaleFactor * bounds.size.width),
      trunc(ScaleFactor * bounds.size.height), false);
  end;
end;

function TOpenGLView.isOpaque: ObjCBool;
begin
  // return false to make the view transparent
  result := window.backgroundColor.alphaComponent > 0;
end;

function TOpenGLView.defaultPixelFormat: NSOpenGLPixelFormat;
function Inc (var i: integer): integer;
begin
  i += 1;
  result := i;
end;
const
  NSOpenGLPFAOpenGLProfile = 99 { available in 10_7 };
const
  NSOpenGLProfileVersionLegacy = $1000 { available in 10_7 };
  NSOpenGLProfileVersion3_2Core = $3200 { available in 10_7 };
  NSOpenGLProfileVersion4_1Core = $4100 { available in 10_10 };
var
  attributes: array[0..32] of NSOpenGLPixelFormatAttribute;
  i: integer = -1;
  context: TCastleWindow;
begin
  context := TCocoaWindow(window).ref;

  { Request accelerated?
    Does not seem necessary,
    https://developer.apple.com/documentation/appkit/nsopenglpixelformat/1436213-opengl_pixel_format_attributes
    says that accelerated is preferred anyway. }
  // attributes[Inc(i)] := NSOpenGLPFAAccelerated;

  if context.doubleBuffer then
    attributes[Inc(i)] := NSOpenGLPFADoubleBuffer;
  attributes[Inc(i)] := NSOpenGLPFAColorSize;
  attributes[Inc(i)] := context.ColorBits;
  attributes[Inc(i)] := NSOpenGLPFADepthSize;
  attributes[Inc(i)] := context.DepthBits;
  attributes[Inc(i)] := NSOpenGLPFAStencilSize;
  attributes[Inc(i)] := context.StencilBits;
  attributes[Inc(i)] := NSOpenGLPFAOpenGLProfile;
  attributes[Inc(i)] := NSOpenGLProfileVersion3_2Core;
  attributes[Inc(i)] := 0;

  result := NSOpenGLPixelFormat.alloc.initWithAttributes(@attributes).autorelease;
  if result = nil then
    raise EInternalError.Create('invalid NSOpenGLPixelFormat');

  { Make TGLFeatures.Create use a path that avoids accessing deprecated glGetString(GL_EXTENSIONS).

    TODO: This should not be necessary, we should not change RequestCapabilities here.
    But FPC GLExt unit will by default access deprecated glGetString(GL_EXTENSIONS),
    this is for now the simplest way to prevent it. }
  TGLFeatures.RequestCapabilities := rcForceModern;
end;

function TOpenGLView.initWithFrame(frameRect: NSRect): id;
begin
  result := inherited initWithFrame(frameRect);
  if result <> nil then
    NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result, objcselector('frameChanged:'), NSViewGlobalFrameDidChangeNotification, nil);
end;

function TOpenGLView.draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
begin
  // Result := NSDragOperationNone;
  // Necessary to support drag-and-drop files on the application.
  Result := sender.draggingSourceOperationMask();
end;

function TOpenGLView.prepareForDragOperation(sender: NSDraggingInfoProtocol): ObjCBool;
begin
  { According to docs https://developer.apple.com/documentation/appkit/nsdraggingdestination/1416066-preparefordragoperation?language=objc ,
    it is necessary to return true to support drag-and-drop files on the application.

    In practice overriding prepareForDragOperation doesn't seem to do anything
    - it is not necessary (overriding  draggingEntered is enough),
    - it doesn't make drag-and-drop possible without draggingEntered.

    Still we override it, following the docs. }

  Result := true;
end;

function TOpenGLView.performDragOperation(sender: NSDraggingInfoProtocol): ObjCBOOL;
{ Based on LCL Cocoa widgetset }
var
  draggedURLs: NSArray;
  lFiles: array of string;
  i: Integer;
  pboard: NSPasteboard;
  lNSStr: NSString;
  CastleWindow: TCastleWindow;
begin
  Result := False;
  pboard := sender.draggingPasteboard();

  // Multiple strings
  draggedURLs := pboard.propertyListForType(NSFilenamesPboardType);
  SetLength(lFiles, draggedURLs.count);
  for i := 0 to draggedURLs.count-1 do
  begin
    lNSStr := NSString(draggedURLs.objectAtIndex(i));
    lFiles[i] := lNSStr.UTF8String;
  end;

  CastleWindow := TCocoaWindow(window).ref;

  if Length(lFiles) > 0 then
    CastleWindow.DoDropFiles(lFiles);

  Result := True;
end;

{=============================================}
{@! ___GLPT___ }
{=============================================}

{ unused
procedure Cocoa_GetFrameBufferSize(win: TCastleWindow; out width, height: integer);
begin
  width := trunc(win^.ref.contentView.bounds.size.width);
  height:= trunc(win^.ref.contentView.bounds.size.height);
end;
}

{ TCastleWindow ------------------------------------------------------------------ }

procedure TCastleWindow.CreateBackend;
begin
end;

procedure TCastleWindow.SwapBuffers;
begin
  ref.contentView.display;
  glcontext.flushBuffer;
end;

{ TODO:
  This causes error from all examples:
  EOpenGLError: End of TCastleWindow.DoRender
  OpenGL error (1286): The command is trying to render to or read from the framebuffer while the currently bound framebuffer is not framebuffer complete (i.e. the return value from glCheckFramebufferStatus is not GL_FRAMEBUFFER_COMPLETE).
}
{.$define COCOA_FULL_SCREEN}

procedure TCastleWindow.FullScreenBegin;
{$ifdef COCOA_FULL_SCREEN}
var
  FullScreenOptions: NSDictionary;
begin
  { TODO: alternative:

    if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
    begin
      if ref.collectionBehavior and (
           NSWindowCollectionBehaviorFullScreenPrimary or
           NSWindowCollectionBehaviorFullScreenAuxiliary
         ) = 0 then
        ref.setCollectionBehavior(ref.collectionBehavior or NSWindowCollectionBehaviorFullScreenPrimary);
      ref.toggleFullScreen(nil);
    end
  }

  { In Objective-C:
      NSDictionary fullScreenOptions = [[NSDictionary dictionaryWithObject: [NSNumber numberWithBool: YES] forKey: NSFullScreenModeSetting] retain];
    See https://thinkstack.wordpress.com/2015/12/19/full-screen-mode-for-cocoa-applications/
  }
  FullScreenOptions := NSDictionary.dictionaryWithObjectsAndKeys(
    NSNumber.numberWithBool(true), NSFullScreenModeSetting,
    nil);
  FFullScreenBackend := ref.contentView.EnterFullScreenMode_withOptions(ref.screen, FullScreenOptions);
  if not FFullScreenBackend then
    WritelnWarning('Cannot enter full-screen mode');
{$else}
begin
{$endif COCOA_FULL_SCREEN}
end;

procedure TCastleWindow.FullScreenEnd;
begin
{$ifdef COCOA_FULL_SCREEN}
  ref.contentView.ExitFullScreenModeWithOptions(nil);
{$endif COCOA_FULL_SCREEN}
end;

procedure TCastleWindow.OpenBackend;
const
  NSWindowCollectionBehaviorFullScreenPrimary = 1 shl 7 { available in 10_7 };
  NSWindowCollectionBehaviorFullScreenAuxiliary = 1 shl 8 { available in 10_7 };
var
  window: TCocoaWindow;
  openGLView: TOpenGLView;
  windowFlags: NSUInteger = 0;
begin
  if not FullScreen then
    begin
      windowFlags := NSTitledWindowMask + NSClosableWindowMask + NSMiniaturizableWindowMask + NSResizableWindowMask;
      window := TCocoaWindow.alloc.initWithContentRect_styleMask_backing_defer(NSMakeRect(FLeft, FTop, FWidth, FHeight), windowFlags, NSBackingStoreBuffered, false);
      window.setTitle(NSSTR(GetWholeCaption));
    end
  else
    begin
      { Update position/size in case window is fullscreen.
        If the backend always creates a fullscreen window for some reason
        (e.g. typical on mobile), then you can remove the condition
        "if FullScreen then" and just do this always, regardless of FullScreen value. }
      FLeft := 0;
      FTop := 0;
      DoResize(Application.ScreenWidth, Application.ScreenHeight, false);

      windowFlags := NSBorderlessWindowMask;
      window := TCocoaWindow.alloc.initWithContentRect(NSMakeRect(FLeft, FTop, FWidth, FHeight));
      window.setKeepFullScreenAlways(true);
    end;

  window.ref := Self;

  openGLView := TOpenGLView.alloc.initWithFrame(window.contentView.bounds);
  // makes drag-and-drop files on window work
  openGLView.registerForDraggedTypes(NSArray.arrayWithObjects_count(@NSFilenamesPboardType, 1));
  window.setContentView(openGLView);
  openGLView.release;

  if FullScreen then
    FullScreenBegin;

  window.makeFirstResponder(openGLView);

  window.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
  window.setAcceptsMouseMovedEvents(true);
  window.makeKeyAndOrderFront(nil);

  glcontext := openGLView.openGLContext;
  ref := window;

  Application.OpenWindowsAdd(Self);

  // TODO
  // GetInitialCursorPos;
  // UpdateCursor;
  // InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
var
  CocoaWindow: TCocoaWindow;
begin
  if ref <> nil then
  begin
    CocoaWindow := TCocoaWindow(ref);
    Inc(CocoaWindow.WithinCloseBackend); // avoids infinite loop at closing
    CocoaWindow.close;
    Dec(CocoaWindow.WithinCloseBackend);
    ref := nil;
  end;
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: String);
begin
  FCaption[Part] := Value;
  if not Closed then
    ref.setTitle(NSSTR(GetWholeCaption));
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  glcontext.makeCurrentContext;
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
var
  WantsCursorHidden: Boolean;
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
    begin
      { To implement mcNone, mcForceNone we need to hide/unhide the cursor.
        The calls to hide/unhide in Cocoa must be balanced, following docs,
        so we use CursorHidden to know when they are actually necessary to call. }
      WantsCursorHidden := Value in [mcNone, mcForceNone];
      if Application.CursorHidden <> WantsCursorHidden then
      begin
        Application.CursorHidden := WantsCursorHidden;
        // TODO: hiding cursor doesn't work for unknown reason
        if Application.CursorHidden then
          NSCursor.hide()
        else
          NSCursor.unhide();
      end;

      // Set Cocoa (global) cursor, see https://developer.apple.com/documentation/appkit/nscursor
      case Value of
        mcDefault:
          NSCursor.arrowCursor.set_;
        mcNone, mcForceNone:
          { do nothing here, without any warning };
        mcText:
          NSCursor.IBeamCursor.set_;
        mcHand:
          NSCursor.openHandCursor.set_;
        mcResizeHorizontal:
          NSCursor.resizeLeftRightCursor.set_;
        mcResizeVertical:
          NSCursor.resizeUpDownCursor.set_;
        mcResizeLeft:
          NSCursor.resizeLeftCursor.set_;
        mcResizeRight:
          NSCursor.resizeRightCursor.set_;
        mcResizeTop:
          NSCursor.resizeUpCursor.set_;
        mcResizeBottom:
          NSCursor.resizeDownCursor.set_;
        else
          begin
            NSCursor.arrowCursor.set_;
            WritelnWarning('Cursor %d not implemented on Cocoa', [Ord(Value)]);
          end;
      end;
    end;
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO } true;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
begin
  if not Closed then
    { TODO };
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  if FFullScreenBackend <> FFullScreenWanted then
  begin
    if not Closed then
    begin
      if FFullScreenWanted then
        FullScreenBegin
      else
        FullScreenEnd;
    end else
      FFullScreenBackend := FFullScreenWanted;
  end;
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;

  procedure SetupMainMenu;

    procedure AddMenu(menu: NSMenu);
    var
      menuItem: NSMenuItem;
    begin
      menuItem := NSMenuItem.alloc.initWithTitle_action_keyEquivalent(menu.title, nil, NSSTR('')).autorelease;
      menuItem.setSubmenu(menu);
      NSApp.mainMenu.addItem(menuItem);
    end;

  var
    mainMenu: NSMenu;
    appleMenu: NSMenu;
    //windowMenu: NSMenu;
  begin
    // main menu
    mainMenu := NSMenu.alloc.init.autorelease;
    NSApp.setMainMenu(mainMenu);

    // apple menu
    appleMenu := NSMenu.alloc.initWithTitle(NSSTR('')).autorelease;
    appleMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Quit ' + ApplicationName), objcselector('terminate:'), NSSTR('q'));

    AddMenu(appleMenu);

    { This is consistent with some Apple applications,
      but it would mix with TCastleWindow.MainMenu and be surprisingly uneditable for developer.

    // window menu
    windowMenu := NSMenu.alloc.initWithTitle(NSSTR('Window')).autorelease;
    windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Minimize'), objcselector('performMiniaturize:'), NSSTR('m'));
    windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Zoom'), objcselector('performZoom:'), NSSTR(''));

    AddMenu(windowMenu);
    }
  end;

var
  pool: NSAutoreleasePool;
  app: TCocoaApp;
  delegate: TCocoaAppDelegate;
begin
  // https://hero.handmade.network/forums/code-discussion/t/1409-main_game_loop_on_os_x

  pool := NSAutoreleasePool.alloc.init;

  app := TCocoaApp(TCocoaApp.sharedApplication);

  delegate := TCocoaAppDelegate.alloc.init;
  app.setDelegate(delegate);

  NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular);
  NSApp.activateIgnoringOtherApps(true);

  if NSApp.mainMenu = nil then
    SetupMainMenu;
  app.finishLaunching;

  pool.release;
end;

procedure TCastleApplication.DestroyBackend;
begin
  // This is deprecated, and seems to do nothing for us, we call this when windows are already closed.
  //  NSApp.terminate(nil);
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  if Terminated then Exit(false);

  { TODO: honor WaitForMessage }

  // note: do we really need to call on main thread? isn't the first thread "main"?
  //TCocoaApp(TCocoaApp.sharedApplication).poll;
  NSApp.performSelectorOnMainThread_withObject_waitUntilDone(objcselector('poll'), nil, true);
  if Terminated then Exit(false);

  { TODO: UpdateAndRenderEverything should be called only if no message was in the queue.

    This follows castlewindow_winsystem.inc approach, and works very good
    to prevent doing Update / Render when we're clogged with events
    (typically happens when walking with mouse look, then we're clogged
    with mouse move events). }

  UpdateAndRenderEverything;
  Result := not Terminated;

  { TODO:
  if (not WasAnyMessage) and
     (not Terminated) and
     (not WaitForMessage) and
     WaitToLimitFPS then
    DoLimitFPS;
  }
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { This implementation is valid for start: }
  Result := ProcessMessage(false, false);

  { In general, ProcessAllMessages should make sure that all messages
    are handled, calling "ProcessMessage(false, false)" as long as some
    message exists in window system queue.
    Then it should call UpdateAndRenderEverything.
    See the GTK backend for example. }
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  { Implementing Run by calling ProcessMessage in a loop (like below)
    is a valid and good implementation. Make sure your ProcessMessage
    honours WaitForMessage and WaitToLimitFPS = true,
    to avoid wasting CPU on "busy waiting". }
  while ProcessMessage(true, true) do ;
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  Terminate; // set Terminated := true
end;

function TCastleApplication.ScreenWidth: integer;
var
  screenFrame: NSRect;
begin
  screenFrame := NSScreen.mainScreen.frame;
  Result := trunc(NSMaxX(screenFrame)) - trunc(NSMinX(screenFrame));
end;

function TCastleApplication.ScreenHeight: integer;
var
  screenFrame: NSRect;
begin
  screenFrame := NSScreen.mainScreen.frame;
  Result := trunc(NSMaxY(screenFrame)) - trunc(NSMinY(screenFrame));
end;

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

function TCastleApplication.BackendName: String;
begin
  Result := 'Cocoa';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { You should check, using window_events example, what is the correct value
    (press "5", see if OnMotion is generated). }
  Result := true;
end;

{$endif read_implementation}
