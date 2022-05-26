{
  Copyright 2022-2022 Michalis Kamburelis and GLPT authors.

  The Cocoa integration code is based on
  - (mostly) GLPT original from https://github.com/daar/GLPT
  - fork from https://github.com/genericptr/GLPT
  Thank you go to all GLPT contributors, in particular:
  - Darius Blaszyk (daar)
  - Ryan Joseph (genericptr)

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO p1:
  - restore main menu - why does it crash?
    Somehow Lazarus runs the application with same menu OK.
  - fullscreen: should be on top of dock / menu

  TODO p2:
  - use filters in FileDialog to highlight correct files
  - update caption at runtime
  - menu implement, with key shortcuts
  - submit PR for GLPT:
    - wglpt -> want
    - attributes size must be 32, 4 is too small to sometimes fit!
  - use AppBundle for "castle-engine package" too
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
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  CursorHidden: Boolean;
{$endif read_application_interface}

{$ifdef read_implementation}

{ Integration code from GLPT ------------------------------------------------- }

{=============================================}
{@! ___SCANECODE TABLE___ }
{=============================================}

var
  darwin_scancode_table: array[0..127] of TKey = (
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
{@! ___COCOA UTILS___ }
{=============================================}
type
  NSFileManager_Additions = objccategory external (NSFileManager)
    function createDirectoryAtURL_withIntermediateDirectories_attributes_error (url: NSURL; createIntermediates: boolean; attributes: NSDictionary; error: NSErrorPtr): boolean; message 'createDirectoryAtURL:withIntermediateDirectories:attributes:error:'; { available in 10_7, 5_0 }
  end;

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
            win.DoKeyDown(darwin_scancode_table[event.keycode], event.characters.UTF8String);
            // TODO we could use event.isARepeat for our KeyRepeated, instead we always calculate it manually
          NSKeyUp:
            win.DoKeyUp(darwin_scancode_table[event.keycode]);
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
begin
  openGLContext.update;

  if window <> nil then // TODO should be windowRef?
    begin
      windowRef.DoResize(trunc(bounds.size.width), trunc(bounds.size.height), false);
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

  // note: implement this?
  //if (_this->gl_config.accelerated >= 0) {
  //    if (_this->gl_config.accelerated) {
  //        attr[i++] = NSOpenGLPFAAccelerated;
  //    } else {
  //        attr[i++] = NSOpenGLPFARendererID;
  //        attr[i++] = kCGLRendererGenericFloatID;
  //    }
  //}

  if context.doubleBuffer then
    attributes[Inc(i)] := NSOpenGLPFADoubleBuffer;
  attributes[Inc(i)] := NSOpenGLPFAColorSize;
  attributes[Inc(i)] := context.ColorBits;
  attributes[Inc(i)] := NSOpenGLPFADepthSize;
  attributes[Inc(i)] := context.DepthBits;
  attributes[Inc(i)] := NSOpenGLPFAStencilSize;
  attributes[Inc(i)] := context.StencilBits;
  attributes[Inc(i)] := NSOpenGLPFAOpenGLProfile;
  // note: we can only specify "legacy" or "core" on mac and the system will decide what version we actually get

  // TODO: CGE for now we always request "legacy"
  //if context.profile = GLPT_CONTEXT_PROFILE_LEGACY then
    attributes[Inc(i)] := NSOpenGLProfileVersionLegacy;
  {
  else if context.profile = GLPT_CONTEXT_PROFILE_CORE then
    begin
      if context.majorVersion = 3 then
        attributes[Inc(i)] := NSOpenGLProfileVersion3_2Core
      else if context.majorVersion = 4 then
        attributes[Inc(i)] := NSOpenGLProfileVersion4_1Core
      else
        glptError(GLPT_ERROR_PLATFORM, 'invalid core profile major version');
    end
  else
    glptError(GLPT_ERROR_PLATFORM, 'invalid context profile');
  }
  attributes[Inc(i)] := 0;

  result := NSOpenGLPixelFormat.alloc.initWithAttributes(@attributes).autorelease;
  if result = nil then
    raise EInternalError.Create('invalid NSOpenGLPixelFormat');
end;

function TOpenGLView.initWithFrame(frameRect: NSRect): id;
begin
  result := inherited initWithFrame(frameRect);
  if result <> nil then
    NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(result, objcselector('frameChanged:'), NSViewGlobalFrameDidChangeNotification, nil);
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

procedure TCastleWindow.BackendMenuInitialize;
begin
  { TODO: implement menu on Cocoa }
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  { TODO: implement menu on Cocoa }
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
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
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO: implement menu on Cocoa faster, without reinitialize on update }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindow.SwapBuffers;
begin
  ref.contentView.display;
  glcontext.flushBuffer;
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
  window.setContentView(openGLView);
  openGLView.release;

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

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
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
  { This is the simplest implementation of this method: }
  SimpleUpdateFullScreenBackend;
  { Some backends may provide a better way of doing this.
    Remove the above line then, and put better implementation here. }
end;

{ ----------------------------------------------------------------------------
  TCastleWindow.MessageOK, MessageYesNo (Cocoa alerts)
}

var
  AlertEnded: Boolean;
  AlertResultCode: NSModalResponse;

procedure AlertCompletionHandler(ResultCode: NSModalResponse);
  {cdecl; No need to change calling convention, looking at examples}
begin
  AlertEnded := true;
  AlertResultCode := ResultCode;
end;

type
  TAlertCompletionHandler = reference to procedure(ResultCode: NSModalResponse); cdecl; cblock;

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

  @param(ParentWindow Shows the prompt as a sheet to the specified NSWindow.)

  @returns(The value from ButtonResults corresponding to the button that was pressed.
    Returns -1 if application ended while the message was active.) }
function CocoaPromptUser(const DialogCaption : string;
  const DialogMessage : string;
  const AlertStyle: NSAlertStyle;
  const ButtonCaptions: array of String;
  const ButtonResults: array of NSInteger;
  const ParentWindow : NSWindow): NSInteger;
var
  anAlert: NSAlert;
  informativeText: NSString;
  messageText: NSString;
  I: Integer;
  aButton: NSButton;
begin
  Assert(ParentWindow <> nil);
  Assert(High(ButtonCaptions) = High(ButtonResults));
  Assert(High(ButtonCaptions) >= 0);

  Result := -1;
  AnAlert := NSAlert.alloc.init;
  try
    informativeText := NSSTR(DialogMessage);
    messageText := NSSTR(DialogCaption);
    anAlert.setAlertStyle(AlertStyle);
    anAlert.setInformativeText(informativeText);
    anAlert.setMessageText(messageText);

    for I := Low(ButtonCaptions) to High(ButtonCaptions) do
    begin
      aButton := anAlert.addButtonWithTitle(NSSTR(ButtonCaptions[I]));
      aButton.setKeyEquivalentModifierMask(0);

      // call aButton.setKeyEquivalent
      if (ButtonCaptions[I] = 'OK') or
         (ButtonCaptions[I] = 'Yes') then
        aButton.setKeyEquivalent(NSSTR(#13))
      else
      if ButtonCaptions[I] = 'No' then
        aButton.setKeyEquivalent(NSSTR(#27))
      else
        // By default, the first button is the default button. If in our
        // case this should not be the case, remove the default status
        // from the first button.
        aButton.setKeyEquivalent(NSSTR(''));

      aButton.setTag(ButtonResults[I]);
    end;

    AlertResultCode := -1;
    AlertEnded := false;

    { Using beginSheetModalForWindow_completionHandler with callback
      based on https://wiki.freepascal.org/macOS_NSAlert

      This requires casting AlertCompletionHandler to "cblock", see
      https://forum.lazarus.freepascal.org/index.php?topic=57724.0
        The OpaqueCBlocks predated FPC support for cblocks. So they're basically untyped parameters used as placeholders for cblock parameters.
      https://wiki.freepascal.org/FPC_New_Features_3.2.0#Support_for_interfacing_with_C_blocks_functionality
      https://gitlab.com/freepascal.org/fpc/source/-/blob/main/tests/test/tblock1.pp
      https://gitlab.com/freepascal.org/fpc/source/-/blob/main/tests/test/tblock2.pp
    }

    anAlert.beginSheetModalForWindow_completionHandler(ParentWindow,
      TAlertCompletionHandler(@AlertCompletionHandler));

    while not AlertEnded do
      if not Application.ProcessMessage(true, true) then
        Break;

    if AlertEnded then
      Result := AlertResultCode;
  finally
    AnAlert.release;
  end;

  { Make the ParentWindow again receive keys, otherwise closing the dialog
    with key makes weird experience -- further key presses will not be send
    to the application window you're looking at. }
  ParentWindow.makeKeyAndOrderFront(nil);
end;

const
  CaptionFromMessageType: array [TWindowMessageType] of String = (
    'Information',
    'Warning',
    'Question',
    'Error',
    'Message' // mtOther
  );
  AlertStyleFromMessageType: array [TWindowMessageType] of NSAlertStyle = (
    NSInformationalAlertStyle, // mtInfo
    NSWarningAlertStyle, // mtWarning
    NSInformationalAlertStyle, // mtQuestion
    NSCriticalAlertStyle, // mtError
    NSInformationalAlertStyle // mtOther
  );

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  CocoaPromptUser(CaptionFromMessageType[MessageType], S,
    AlertStyleFromMessageType[MessageType], ['OK'], [0], ref);
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := CocoaPromptUser(CaptionFromMessageType[MessageType], S,
    AlertStyleFromMessageType[MessageType], ['Yes', 'No'], [0, 1], ref) = 0;
end;

{ TCastleWindow.BackendFileDialog -------------------------------------------- }

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;

{ Implementation based on LCL TCocoaWSFileDialog.ShowModal }

var
  openDlg: NSOpenPanel;
  saveDlg: NSSavePanel;
  InitName, InitDir: string;

  // filter accessory view
  // accessoryView: NSView;
  // lFilter: TCocoaFilterComboBox;

  // isMenuOn: Boolean;

(*
  // setup panel and its accessory view
  procedure CreateAccessoryView(AOpenOwner: NSOpenPanel; ASaveOwner: NSSavePanel);
  const
    INT_MIN_ACCESSORYVIEW_WIDTH = 300;
    OFS_HRZ = 10;
  var
    lRect: NSRect;
    lText: NSTextField;
    lTextStr: NSString;
    lDialogView: NSView;
    lAccessoryWidth: Integer = INT_MIN_ACCESSORYVIEW_WIDTH;
    w: Integer;
    nw: Integer;
    fw: Integer;
  begin
    // check if the accessory is necessary
    if FileDialog.Filter = '' then Exit;

    // try to obtain the dialog size
    lDialogView := NSView(ASaveOwner.contentView);
    if lDialogView <> nil then
    begin
      if (NSAppkitVersionNumber >= NSAppKitVersionNumber11_0) then
        // starting with Big Sur, the dialog retains the last openned size
        // causing the width to be increased on every openning of the dialog
        // we'd simply force the lAccessoryWidth to start with the minimum width
        lAccessoryWidth := INT_MIN_ACCESSORYVIEW_WIDTH
      else if lDialogView.frame.size.width > INT_MIN_ACCESSORYVIEW_WIDTH then
        lAccessoryWidth := Round(lDialogView.frame.size.width);
    end;
    lRect := GetNSRect(0, 0, lAccessoryWidth, 30);
    accessoryView := NSView.alloc.initWithFrame(lRect);

    // "Format:" label
    lText := NSTextField.alloc.initWithFrame(NSNullRect);
    {$ifdef BOOLFIX}
    lText.setBezeled_(Ord(False));
    lText.setDrawsBackground_(Ord(False));
    lText.setEditable_(Ord(False));
    lText.setSelectable_(Ord(False));
    {$else}
    lText.setBezeled(False);
    lText.setDrawsBackground(False);
    lText.setEditable(False);
    lText.setSelectable(False);
    {$endif}
    lTextStr := NSStringUTF8(rsMacOSFileFormat);
    lText.setStringValue(lTextStr);
    lText.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(NSRegularControlSize)));
    lText.sizeToFit;

    // Combobox
    lFilter := TCocoaFilterComboBox.alloc.initWithFrame(NSNullRect);
    lFilter.IsOpenDialog := AOpenOwner <> nil;
    if lFilter.IsOpenDialog then
      lFilter.DialogHandle := AOpenOwner
    else
      lFilter.DialogHandle := ASaveOwner;
    lFilter.Owner := FileDialog;
    lFilter.setTarget(lFilter);
    lFilter.setAction(objcselector('comboboxAction:'));
    lFilter.updateFilterList();
    if FileDialog.FilterIndex <= 0 then
      lFilter.lastSelectedItemIndex := 0
    else
      lFilter.lastSelectedItemIndex := FileDialog.FilterIndex-1;
    lFilter.lastSelectedItemIndex := lFilter.setDialogFilter(lFilter.lastSelectedItemIndex);
    lFilter.sizeToFit;
    lFilter.setAutoresizingMask(NSViewWidthSizable);
    if FileDialog.FilterIndex>0 then
      lFilter.selectItemAtIndex(FileDialog.FilterIndex-1);

    // Trying to put controls into the center of the Acc-view
    //  Label must fit in full. Whatever is left is for filter
    w:=lAccessoryWidth - OFS_HRZ - OFS_HRZ;
    fw:=Round(lFilter.frame.size.width);
    nw:=Round(lText.frame.size.width + fw + OFS_HRZ);
    if nw>w then begin
      dec(fw, nw-w);
      nw:=w;
    end;

    lText.setFrame(  NSMakeRect(
       Round((w-nw) / 2+OFS_HRZ)
       , 0
       , lText.frame.size.width
       , lFilter.frame.size.height
    ));

    lFilter.setFrame( NSMakeRect(
       lText.frame.origin.x+lText.frame.size.width+OFS_HRZ
       ,4
       ,fw
       ,lFilter.frame.size.height
      ));

    accessoryView.addSubview(lText.autorelease);
    accessoryView.addSubview(lFilter.autorelease);

    lFilter.setAutoresizingMask(NSViewWidthSizable);

    lFilter.DialogHandle.setAccessoryView(accessoryView.autorelease);
    lFilter.DialogHandle.setDelegate(lFilter);
  end;
*)

begin
  InitName := ExtractFileName(FileName);
  InitDir := ExtractFileDir(FileName);

  // TODO: FileFilters
  // Adapt LCL code? Or resign -- Cocoa doesn't have such filters natively.
  // lFilter := nil;

  if OpenDialog then
  begin
    openDlg := NSOpenPanel.openPanel;
    openDlg.setAllowsMultipleSelection(false);

    { to choose only directory we would do this:
    openDlg.setCanChooseDirectories(true);
    openDlg.setCanChooseFiles(false);
    }
    openDlg.setCanChooseFiles(true);
    openDlg.setCanChooseDirectories(false);

    // that's correct, NSOpenPanel descends from NSSavePanel and we use it
    saveDlg := openDlg;
  end else
  begin
    saveDlg := NSSavePanel.savePanel;
    saveDlg.setCanCreateDirectories(true);
    saveDlg.setNameFieldStringValue(NSSTR(InitName));
    openDlg := nil;
  end;

  saveDlg.setAccessoryView(nil);
  saveDlg.retain; // LCL comment: this is for OSX 10.6 (and we don't use ARC either)
  saveDlg.setTitle(NSSTR(Title));
  saveDlg.setDirectoryURL(NSURL.fileURLWithPath(NSSTR(InitDir)));

  // TODO: isMenuOn := ToggleAppMenu(false);
  try
    Result := saveDlg.runModal = NSModalResponseOK;
    if Result then
      FileName := saveDlg.URL.path.UTF8String;

    // release everything
    saveDlg.release;
  finally
    // TODO: ToggleAppMenu(isMenuOn);
  end;

  ref.makeKeyAndOrderFront(nil);
end;

type
  TColorPanelDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  public
    colorPanel: NSColorPanel;
    didPickColor: Boolean;
    // NSWindowDelegateProtocol
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    //
    procedure pickColor; message 'pickColor'; // button action
    procedure exit; message 'exit'; // button action
  end;

{ TColorPanelDelegate }

procedure TColorPanelDelegate.windowWillClose(notification: NSNotification);
begin
  NSApp.stopModal();
end;

procedure TColorPanelDelegate.pickColor();
begin
  didPickColor := true;
  exit();
end;

procedure TColorPanelDelegate.exit();
begin
  colorPanel.close();
end;

function TCastleWindow.ColorDialog(var Color: TVector3): boolean;

  function GetNSRect(x, y, width, height: Integer): NSRect;
  begin
    Result.origin.x := x;
    Result.origin.y := y;
    Result.size.width := width;
    Result.size.height := height;
  end;

  function NSColorFromCastle(const Color: TCastleColor): NSColor;
  begin
    Result := NSColor.colorWithDeviceRed_green_blue_alpha(
      Color.X,
      Color.Y,
      Color.Z,
      Color.W
    );
  end;

  function NSColorToCastle(Color: NSColor): TCastleColor;
  begin
    // We must convert color to device RGB to be able to read it,
    // otherwise reading e.g. grayscale results in crash.
    if Color.colorSpace <> NSColorSpace.deviceRGBColorSpace then
    begin
      Color := Color.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace);
      if Color = nil then
      begin
        WritelnWarning('Cannot convert color to RGB');
        Exit(Black);
      end;
    end;

    Result := Vector4(
      Color.redComponent,
      Color.greenComponent,
      Color.blueComponent,
      Color.alphaComponent
    );
  end;

{ Implementation based on LCL TCocoaWSColorDialog.ShowModal }

var
  colorDelegate: TColorPanelDelegate;
  colorPanel: NSColorPanel;
  // accessory view
  accessoryView: NSView;
  lRect: NSRect;
  okButton, cancelButton: NSButton;
  //isMenuOn: Boolean;
begin
  colorPanel := NSColorPanel.sharedColorPanel();
  if (colorPanel.respondsToSelector(ObjCSelector('setRestorable:'))) then
    colorPanel.setRestorable(false);
  // TODO: pass color with alpha
  colorPanel.setColor(NSColorFromCastle(Vector4(Color, 1)));

  colorDelegate := TColorPanelDelegate.alloc.init();
  colorDelegate.colorPanel := colorPanel;

  // setup panel and its accessory view
  lRect := GetNSRect(0, 0, 220, 30);
  accessoryView := NSView.alloc.initWithFrame(lRect);

  lRect := GetNSRect(110, 4, 110-8, 24);
  okButton := NSButton.alloc.initWithFrame(lRect);
  okButton.setButtonType(NSMomentaryPushInButton);
  okButton.setBezelStyle(NSRoundedBezelStyle);
  okButton.setTitle(NSSTR('Pick'));
  okButton.setAction(objcselector('pickColor'));
  okButton.setTarget(colorDelegate);

  lRect := GetNSRect(8, 4, 110-8, 24);
  cancelButton := NSButton.alloc.initWithFrame(lRect);
  cancelButton.setButtonType(NSMomentaryPushInButton);
  cancelButton.setBezelStyle(NSRoundedBezelStyle);
  cancelButton.setTitle(NSSTR('Cancel'));
  cancelButton.SetAction(objcselector('exit'));
  cancelButton.setTarget(colorDelegate);

  accessoryView.addSubview(okButton.autorelease);
  accessoryView.addSubview(cancelButton.autorelease);

  colorPanel.setDelegate(colorDelegate);
  colorPanel.setAccessoryView(accessoryView.autorelease);
  colorPanel.setShowsAlpha(False);
  colorPanel.setDefaultButtonCell(okButton.cell);

  // show panel
  // TODO: isMenuOn := ToggleAppMenu(false);
  try
    colorPanel.makeKeyAndOrderFront(colorDelegate);
    NSApp.runModalForWindow(colorPanel);

    Result := colorDelegate.didPickColor;
    if Result then
      // TODO: pass color with alpha
      Color := NSColorToCastle(colorPanel.color).XYZ;
  finally
    // TODO: ToggleAppMenu(isMenuOn);
  end;

  ref.makeKeyAndOrderFront(nil);
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;

  procedure SetupMainMenu;

    procedure AddMenu (menu: NSMenu);
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
    windowMenu: NSMenu;
  begin
    Exit; // TODO: this causes a crash when running through "castle-engine run"

    // main menu
    mainMenu := NSMenu.alloc.init.autorelease;
    NSApp.setMainMenu(mainMenu);

    // apple menu
    appleMenu := NSMenu.alloc.initWithTitle(NSSTR('')).autorelease;
    appleMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Quit '+NSProcessInfo.processInfo.processName.UTF8String), objcselector('terminate:'), NSSTR('q'));

    AddMenu(appleMenu);

    // window menu
    windowMenu := NSMenu.alloc.initWithTitle(NSSTR('Window')).autorelease;
    windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Minimize'), objcselector('performMiniaturize:'), NSSTR('m'));
    windowMenu.addItemWithTitle_action_keyEquivalent(NSSTR('Zoom'), objcselector('performZoom:'), NSSTR(''));

    // TODO: add CGE menu

    AddMenu(windowMenu);
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

function TCastleApplication.BackendName: string;
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
