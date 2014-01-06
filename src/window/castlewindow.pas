{
  Copyright 2001-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Window with OpenGL context suitable for 2D and 3D rendering
  of "Castle Game Engine". The base TCastleWindowBase provides a simple
  window with OpenGL context, and is suitable for any OpenGL program.
  More advanced TCastleWindow extends it to comfortably
  render 2D controls and 3D objects defined by our engine.

  @link(Application) object (instance of class @link(TCastleApplication))
  is a central manager of all open @link(TCastleWindowBase) windows.

  Using this unit:

  @orderedList(
    @item(Declare and create @link(TCastleWindowBase) instance. (Or a descendant
      like @link(TCastleWindow).))

    @item(Assign Glw properties and callbacks like
      @link(TCastleWindowBase.OnDraw OnDraw),
      @link(TCastleWindowBase.OnResize OnResize),
      @link(TCastleWindowBase.Width Width),
      @link(TCastleWindowBase.Height Height),
      @link(TCastleWindowBase.Caption Caption).)

    @item(Call @link(TCastleWindowBase.Open Window.Open),
      this will actually show the window and it's
      associated OpenGL context. It also calls
      @link(TCastleWindowBase.EventOpen EventOpen)
      (@link(TCastleWindowBase.OnOpen OnOpen) callback)
      and @link(TCastleWindowBase.EventResize EventResize)
      (@link(TCastleWindowBase.OnResize OnResize) callback).)

    @item(Call @link(TCastleApplication.Run Application.Run).
      This will enter message loop that will call
      appropriate windows' callbacks at appropriate times
      (OnDraw, OnPress, OnRelease, OnResize, OnUpdate and many more).
      There are also some Application callbacks, like
      @link(TCastleApplication.OnUpdate Application.OnUpdate).

      For more advanced needs you can use something like
        @longCode(#  while Application.ProcessMessage do <something>;#)
      instead of Application.Run.

      You can also call @link(TCastleWindowBase.OpenAndRun Window.OpenAndRun),
      this is just a shortcut for Window.Open + Application.Run.)

    @item(Application.Run ends when you call @link(TCastleApplication.Quit Application.Quit)
      or when you close last visible window using @link(TCastleWindowBase.Close Close(true)).

      User is also allowed to close a window using WindowManager facilities
      (clicking on "X" button in the frame corner, pressing Alt+F4 or something
      like that). By default, such user action will make window close
      (but you can freely customize what your program does when user
      tries to close the window using callback
      @link(TCastleWindowBase.OnCloseQuery OnCloseQuery)).)
  )

  So the simplest example of using this unit can look like this:

@longcode(#
  uses CastleWindow;

  var
    Window: TCastleWindowCustom;

  procedure Draw(Window: TCastleWindowBase);
  begin  ...  end;

  procedure Resize(Window: TCastleWindowBase);
  begin  ...  end;

  begin
    Window := TCastleWindowCustom.Create(Application);
    Window.OnResize := @Resize;
    Window.OnDraw := @Draw;
    Window.Caption := 'Simplest CastleWindow example';
    Window.OpenAndRun;
  end.
#)

  @italic(More object-oriented approach):
  Instead of assigning callbacks (OnDraw, OnResize etc.) you can
  also derive a new class from TCastleWindowBase and override some of virtual
  methods, like EventDraw, EventResize etc. Every callback OnXxx
  has a corresponding EventXxx method. In TCastleWindowBase class,
  all EventXxx methods simply call appropriate OnXxx callbacks
  (this way you can use whatever approach you like -- OOP or not-OOP).

  This is a second version of the "simplest example" program above,
  this time using OOP approach:

@longcode(#
  uses CastleWindow;

  type
    TMyWindow = class(TCastleWindowCustom)
      procedure EventDraw; override;
      procedure EventResize; override;
    end;

  procedure TMyWindow.EventDraw;
  begin  ...  end;

  procedure TMyWindow.EventResize;
  begin  ...  end;

  var
    Window: TMyWindow;
  begin
    Window := TMyWindow.Create(Application);
    Window.Caption := 'Simplest CastleWindow example using more OOP';
    Window.OpenAndRun;
  end.
#)

  The non-OOP approach has one advantage: you can easily switch all callbacks
  to some other set of callbacks. This allows you to implement modal behaviors,
  where a function suspends normal callbacks to display some dialog.
  See @link(CastleWindowModes) unit and, build on top of it,
  dialog boxes in @link(CastleMessages) and progress bar in @link(CastleWindowProgress).
  These units give you some typical GUI capabilities, and they are in pure OpenGL.

  Using OOP approach (overriding EventXxx methods instead of registering OnXxx
  callbacks) you can not do such things so easily -- in general, you have
  to define something to turn off special EventXxx functionality
  (like SetDemoOptions in TCastleWindowDemo and UseControls in TCastleWindowCustom)
  and you have to turn them off/on when using CastleWindowModes
  (mentioned TCastleWindowDemo and TCastleWindowCustom are already handled in
  CastleWindowModes). TODO: I shall do some virtual methods in TCastleWindowBase
  to make this easy.

  Random features list:

  @unorderedList(

    @item(TCastleApplication.ProcessMessage method.
      This allows you to reimplement
      event loop handling, which is crucial for implementing things
      like @link(MessageInputQuery) function that does modal GUI dialog box.)

    @item(TCastleWindowBase.Pressed to easily and reliably check which keys
      are pressed.)

    @item(Frames per second measuring, see @link(TCastleWindowBase.Fps),)

    @item(A menu bar under WinAPI and GTK backends.

      You can attach a menu to a window. Menu structure is constructed using
      various descendants of TMenuEntry class.
      Then you have to assign such menu structure
      to TCastleWindowBase.MainMenu property. When CastleWindow is implemented on top
      of GTK_2 or WINAPI or LCL we will show this menu and call
      TCastleWindowBase.EventMenuClick (TCastleWindowBase.OnMenuClick) when user clicks some menu item.
      Other backends (XLIB for now) ignore MainMenu.

      See @code(castle_game_engine/examples/window/window_menu.lpr)
      for an example how to use the menu.)

    @item(Changing screen resolution and bit depth,
      see TCastleApplication.VideoChange.)

    @item(You can request OpenGL context properties:
      @unorderedList(
        @item color buffer
        @item with alpha channel (@link(TCastleWindowBase.AlphaBits AlphaBits)),
        @item stencil buffer (@link(TCastleWindowBase.StencilBits StencilBits)),
        @item double buffer (@link(TCastleWindowBase.DoubleBuffer DoubleBuffer)),
        @item(multisampling (full-screen antialiasing) buffers (by
          @link(TCastleWindowBase.MultiSampling MultiSampling) or higher-level
          @link(TCastleWindowBase.AntiAliasing AntiAliasing)))
      )
    )

    @item(You can use native modal dialogs for things such as file selection.
      GTK backend will use GTK dialogs, WinAPI backend
      will use Windows dialog boxes, XLib backend will fall back
      on CastleMessages text input.

      See TCastleWindowBase.FileDialog (for opening and saving files) and
      TCastleWindowBase.ColorDialog (for choosing RGB colors).)

    @item(TCastleWindowBase.ParseParameters method allows you to easily initialize TCastleWindowBase
      properties like initial size and position using command-line
      parameters like @code(@--geometry WIDTHxHEIGHT), @code(@--display) etc.)
  )
}

unit CastleWindow;

{$I castleconf.inc}

{ Choose CastleWindow backend ------------------------------------------ }

{ You can define one of the symbols CASTLE_WINDOW_xxx mentioned below to include
  a specific CastleWindow backend. If you don't, there is a code lower that
  chooses the best backend for given OS.

  Available backends:

  CASTLE_WINDOW_GTK_2
    Based on GTK 2.x, using GtkGLExt extension. Made 2005-02.
    MainMenu is implemented as a nice-looking GTK menu bar.
    Dialog windows implemented using GTK dialog windows.
    Generally, has a nice native look of GTK application.

    Should work under any OS where GTK works.
    Currently tested under Linux, FreeBSD, Mac OS X and Windows.

    FullScreen is cleanly implemented in GTK_2, never using override_redirect,
    so Alt+Tab always works (even when your window is fullscreen),
    and things like gnome-panel will never cover your fullscreen window.
    Also changing FullScreen is nice, GTK will instruct the window manager
    to fullscreen/unfullscreen the window, which means it will happen
    without recreating the OpenGL context.

    Known problems:
    - Tab key cannot work as menu item shortcut (it's always only
      for switching focus). This is an issue with GTK 1/2,
      that simply can't be fixed in CastleWindow.
    - TryVideoChange is not finished, i.e. always returns false.
      See TODOs near CASTLE_WINDOW_USE_XF86VMODE definition.
    - Under Windows, window will be always resizeable by user, even if
      you set ResizeAllowed <> raAllowed.
      This is masked in our unit (so your OnResize callback will not get
      to know such thing), so it's harmless for correctness of your programs,
      but user can do it.

    Historically, we also had CASTLE_WINDOW_GTK_1, based on GTK 1.x (>= 1.2)
    using GtkGLArea widget. Was made around beginning of march 2004.
    Removed 2011-12 (commit r10674), ancient and obsoleted by GTK 2.

  CASTLE_WINDOW_WINAPI
    Based on Windows API. Of course only for Windows.

    MainMenu is implemented as WinAPI menu bar. So it looks nice.
    Dialog windows are implemented as common Windows dialog boxes.
    Has a nice native look on Windows.

  CASTLE_WINDOW_XLIB
    Based on XLib units. No X toolkit is used.
    Only for OSes where X is used, which in practice means Unix.

    For desktop OpenGL (when OpenGLES is not defined) this is implemented
    on top of glX.
    For OpenGL ES (when OpenGLES is defined) this is implemented
    on top of EGL, Embedded-System Graphics Library.
    This is the only backend capable of initializing OpenGL ES contexts
    for now.

    MainMenu is not implemented (it's ignored).
    That's not easy to implement when you don't want to use any X toolkit.
    And it's not a good idea to implement it yourself (without any standard
    GUI toolkit) --- this makes many Xlib programs ugly, because every single one
    uses his own GUI. In other words:
    if you want to have MainMenu then just use CASTLE_WINDOW_GTK_2.

    Dialog boxes are implemented using CastleMessages.MessageXxx.
    So they are not very comfortable to user, but they work.

    On Unix platforms, whether you should use CASTLE_WINDOW_GTK_2 or
    this CASTLE_WINDOW_XLIB depends on your program.

    - For utility programs, usually CASTLE_WINDOW_GTK_2.
      You want the menu bar and native (GTK-themed) look of dialog boxes.

    - For fullscreen games, usually CASTLE_WINDOW_XLIB.
      You usually do not use the menu bar in fullscreen games,
      and do not want popup dialog boxes. Instead you draw everything
      inside your OpenGL context, which makes your game look the same
      regardless of the platform and GUI can be styled to your game theme.
      For example, menu may be done by TCastleOnScreenMenu, and dialog boxes
      by CastleMessages.

      As a bonus, XLIB allows you to change screen resolution when
      starting the game, which may be useful. And has one dependency less
      (GTK is commonly installed, but gtkglext is not, and CASTLE_WINDOW_GTK_2
      requires gtkglext).

  CASTLE_WINDOW_LCL
    Use Lazarus TForm (with menu, dialogs and so on) and TOpenGLControl.
    This wraps Lazarus form and TOpenGLControl inside a TCastleWindowBase
    instance.
    It's cross-platform, it has a native look --- all thanks to Lazarus LCL.
    It misses some things that Lazarus misses ---- like screen resizing,
    and event loop may have problems in case of mouse look.

    To use this:
    - You have to add castle_components package
      to the requirements of the castle_window Lazarus package.

      It will also automatically add LazOpenGLContext package as dependency
      of castle_window, which is good.
      We need castle_components package for LCL helpers
      (like converting mouse/keys between LCL and CastleKeysMouse),
      and we need LazOpenGLContext package for TOpenGLControl.
      Note that we do *not* use TCastleControl component in this case.

      This is a necessary manual step, as Lazarus packages do not have (yet)
      any mechanism to express a package dependency that is OS-specific.

    - And usually you should compile programs only using Lazarus
      (IDE or lazbuild), to automatically have correct LCL paths used.

  CASTLE_WINDOW_ANDROID
    Initialize OpenGL context using EGL on Android.

  CASTLE_WINDOW_LIBRARY
    Use existing OpenGL context.
    This is useful when the engine is used as a library (see src/library/),
    and an external code already initialized OpenGL context.

    Note that the external code must take care to initialize
    context following our TCastleWindow properties like
    TCastleWindow.DepthBits, TCastleWindow.StencilBits and such.
    It also must take care of calling TCastleWindowBase.LibraryXxx
    methods to notify us about events like key/mouse press.

  CASTLE_WINDOW_TEMPLATE
    This is a special dummy backend, useful only as an example
    for programmers that want to implement another CastleWindow backend
    (e.g. based on Mac OS X Carbon).
    It compiles, but actually nothing works.
    See file CASTLE_WINDOW_backend_template.inc.
}

{ Define CASTLE_WINDOW_BEST_NOGUI to choose the best backend for programs
  that do not use native gui (like native dialog boxes in TCastleWindow.MessageOK
  or native menu bar in TCastleWindow.Menu). On Unix, this will choose Xlib,
  that allows you to resize the screen and has less dependencies than GtkGlExt
  backend. }
{$ifdef CASTLE_WINDOW_BEST_NOGUI}
  {$ifdef UNIX}
    {$ifdef ANDROID}
      {$define CASTLE_WINDOW_ANDROID}
    {$else}
      {$define CASTLE_WINDOW_XLIB}
    {$endif}
  {$else}
    {$ifdef MSWINDOWS}
      {$define CASTLE_WINDOW_WINAPI}
    {$else}
      {$fatal CASTLE_WINDOW_BEST_NOGUI is unknown for this operating system.}
    {$endif}
  {$endif}
{$endif}

{ If CastleWindow backend is not choosen at this point, choose
  default (best, most functional and stable) for a given OS.

  This way you can override configuration below by compiling CastleWindow
  with some CASTLE_WINDOW_xxx symbol already defined. }
{$ifndef CASTLE_WINDOW_WINAPI}
 {$ifndef CASTLE_WINDOW_XLIB}
  {$ifndef CASTLE_WINDOW_GTK_2}
   {$ifndef CASTLE_WINDOW_TEMPLATE}
    {$ifndef CASTLE_WINDOW_LCL}
     {$ifndef CASTLE_WINDOW_ANDROID}
      {$ifndef CASTLE_WINDOW_LIBRARY}

       {$ifdef MSWINDOWS}
         {$define CASTLE_WINDOW_WINAPI} // best (looks native and most functional) on Windows
         { $define CASTLE_WINDOW_GTK_2}
         { $define CASTLE_WINDOW_LCL}
         { $define CASTLE_WINDOW_LIBRARY}
         { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
       {$endif}
       {$ifdef UNIX}
         {$ifdef ANDROID}
           {$define CASTLE_WINDOW_ANDROID}
         {$else}
           {$ifdef DARWIN}
             {$define CASTLE_WINDOW_XLIB} // easiest to compile
             { $define CASTLE_WINDOW_LCL} // best (looks native and most functional) on Mac OS X, but requires LCL
             { $define CASTLE_WINDOW_GTK_2}
             { $define CASTLE_WINDOW_LIBRARY}
             { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
           {$else}
             {$ifndef OpenGLES}
               {$define CASTLE_WINDOW_GTK_2} // best (looks native and most functional) on Unix (except Mac OS X)
             {$else}
               {$define CASTLE_WINDOW_XLIB}
             {$endif}
             { $define CASTLE_WINDOW_XLIB}
             { $define CASTLE_WINDOW_LCL}
             { $define CASTLE_WINDOW_LIBRARY}
             { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
           {$endif}
         {$endif}
       {$endif}

      {$endif}
     {$endif}
    {$endif}
   {$endif}
  {$endif}
 {$endif}
{$endif}

{ To make new GL Window backend -------------------------------------

  - Define a symbol like CASTLE_WINDOW_FOO for a new backend,
    document it in the "available backends list" above.
  - Create a file castlewindow_foo.inc with contents from
    castlewindow_backend_template.inc
    and conditionally include it from castlewindow_backend.inc.
  - Adjust defining
    CASTLE_WINDOW_HAS_VIDEO_CHANGE and CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN
    for your backend.
  - Implement all methods in castlewindow_foo.inc. You wil find the specification
    what each method should do in the specification of the interface of this
    module.
  - Call all TCastleWindowBase.DoXxx functions at appropriate places from your
    backend.
    You can call all DoUpdate and DoTimer for all Application.OpenWindows
    using Application.FOpenWindows.DoUpdate/Timer (this will give usually
    inefficient but working backend)
  - Call TCastleApplication.DoSelfUpdate and DoSelfTimer when appropriate.
    Remember that you can always assume that the ONLY existing instance of
    TCastleApplication is Application.
  Some important things that can be easily forgotten:
  - Remember that probably you will have to call ReleaseAllKeysAndMouse
    when user switches to another window or activates MainMenu.
}

{ Configure some debugging options of CastleWindow ------------------------------- }

{ When CASTLE_WINDOW_LOG_EVENTS is defined, TCastleWindowBase events will be logged.
  This means logging (using CastleLog) at begin, end, and at exception exit
  inside all TCastleWindowBase events (EventXxx methods).
  Very useful, although floods your log with incredible amount of messages
  very quickly.

  Actually, CASTLE_WINDOW_LOG_EVENTS by itself turns logging for @italic(almost)
  all events. For the really really often events (draw, update, timer,
  mouse move for now), you'll need to define also CASTLE_WINDOW_LOG_EVENTS_ALL
  (relevant only if CASTLE_WINDOW_EVENTS_LOG).
}
{ $define CASTLE_WINDOW_EVENTS_LOG}
{ $define CASTLE_WINDOW_EVENTS_LOG_ALL}
{$ifndef CASTLE_WINDOW_EVENTS_LOG}
  {$undef CASTLE_WINDOW_EVENTS_LOG_ALL}
{$endif}

{ Define CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW to check OpenGL errors
  after TCastleWindowBase.EventDraw (TCastleWindowBase.OnDraw callback) calls.
  This is done by DoDraw, that is: when a backend initiates the drawing.
  The check is done by CastleGLUtils.CheckGLErrors, checks glGetError
  and eventually raises an exception. }
{$ifdef DEBUG}
  {$define CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW}
{$endif}

{ Configure internal things -------------------------------------------------- }

{$ifdef CASTLE_WINDOW_GTK_2} {$define CASTLE_WINDOW_GTK_ANY} {$endif}

{ Sometimes GTK backend needs to call some X-specific things:
  1. Implementing TCastleWindowBase.SetMousePosition.
     Older GDK/GTK versions didn't have any function for this (see here
     [http://mail.gnome.org/archives/gtk-list/2001-January/msg00035.html]),
     although newer GDK has gdk_display_warp_pointer.
     So we had to bypass GTK and use Xlib's XWarpPointer.
  2. Screen resizing. We have to use for this XF86VidMode extension,
     just like for CASTLE_WINDOW_XLIB backend. }
{$ifdef CASTLE_WINDOW_GTK_2}
  {$ifdef UNIX}
    {$define CASTLE_WINDOW_GTK_WITH_XLIB}
  {$endif}
{$endif}

{ Does backend implement TryVideoChange and VideoReset methods?
  (if this will not be defined, we will use TryVideoChange that always
  returns false and VideoReset that is NOOP). }
{$undef CASTLE_WINDOW_HAS_VIDEO_CHANGE}
{$ifdef CASTLE_WINDOW_WINAPI}
  {$define CASTLE_WINDOW_HAS_VIDEO_CHANGE}
{$endif}
{$ifdef CASTLE_WINDOW_XLIB}
  {$define CASTLE_WINDOW_HAS_VIDEO_CHANGE}
  {$define CASTLE_WINDOW_USE_XF86VMODE}
{$endif}
{$ifdef CASTLE_WINDOW_GTK_ANY}
  {$ifdef UNIX}
    { Hmm. This compiles and basically works, but the new screen is still
      virtual. For now this is disabled. TODO. }
    { $define CASTLE_WINDOW_HAS_VIDEO_CHANGE}
    { $define CASTLE_WINDOW_USE_XF86VMODE}
  {$endif}
{$endif}

{ See castlewindow_private_modifiers_down.inc for description in what
  situations you want to define CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN. }
{$ifdef CASTLE_WINDOW_GTK_ANY} {$define CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN} {$endif}
{$ifdef CASTLE_WINDOW_XLIB}    {$define CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN} {$endif}

{ TODO:

  General:
  - TCastleWindowBase.Width, Height, Left, Top: allow to change them
    after the window is opened.
  - Use EnumDisplaySettings instead of such variables as
    VideoColorBits / VideoScreenWidth / VideoFrequency,
    do some proc DisplayExists and EnumDisplays.
  - Allow passing VideoColorBits, VideoFrequency for --fullscreen-custom
    param.

  See also backend-specific TODOs in castlewindow_xxx.inc files.
}

{$I castleconf.inc}

interface

uses {$define read_interface_uses}
  {$I castlewindow_backend.inc}
  {$undef read_interface_uses}
  SysUtils, Classes, CastleVectors, CastleGL, CastleRectangles, CastleColors,
  CastleUtils, CastleClassUtils, CastleGLUtils, CastleImages, CastleGLImages,
  CastleKeysMouse, CastleStringUtils, CastleFilesUtils, CastleTimeUtils,
  CastleFileFilters, CastleUIControls, CastleCameras, FGL, pk3DConnexion,
  { VRML/X3D stuff }
  X3DNodes, CastleScene, CastleSceneManager, CastleLevels;

{$define read_interface}

const
  { }
  WindowPositionCenter = -1000000;
  WindowDefaultSize = -1000000;

type
  TWindowParseOption = (poGeometry, poScreenGeometry, poDisplay, poMacOsXProcessSerialNumber);
  TWindowParseOptions = set of TWindowParseOption;
  PWindowParseOptions = ^TWindowParseOptions;

const
  { All "normal" command-line options,
    that most programs using CastleWindow should be able to handle
    without any problems.

    In other words, most programs calling @link(TCastleWindowBase.ParseParameters)
    method can safely pass as the 1st parameter this constant,
    StandardParseOptions.
    Or they can simply call overloaded version of TCastleWindowBase.ParseParameters
    that doesn't take any parameters, it is always equivalent to
    calling TCastleWindowBase.ParseParameters(StandardParseOptions). }
  StandardParseOptions = [poGeometry, poScreenGeometry, poDisplay, poMacOsXProcessSerialNumber];

  DefaultDepthBits = 16;

  DefaultFpsCaptionUpdateInterval = 5000;

  DefaultTooltipDelay = 1000;
  DefaultTooltipDistance = 10;

  DefaultLimitFPS = 100.0;

type
  { Development notes:
    When extending TAntiAliasing, remember to also
    update ScreenEffectLibrary implementation to be able to handle them
    (and screen_effect_library.glsl to handle them in GLSL). }
  { Anti-aliasing values for TCastleWindowBase.AntiAliasing. }
  TAntiAliasing = (aaNone,
    aa2SamplesFaster, //< 2 samples, "don't care" hint.
    aa2SamplesNicer,  //< 2 samples, "nicest" hint (quincunx (5 taps) for NVidia).
    aa4SamplesFaster, //< 4 samples, "don't care" hint.
    aa4SamplesNicer,  //< 4 samples, "nicest" hint (9 taps for NVidia).
    aa8SamplesFaster, //< 8 samples, "don't care" hint.
    aa8SamplesNicer,  //< 8 samples, "nicest" hint.
    aa16SamplesFaster, //< 16 samples, "don't care" hint.
    aa16SamplesNicer   //< 16 samples, "nicest" hint.
  );

const
  DefaultAntiAliasing = aaNone;

  AntiAliasingNames: array [TAntiAliasing] of string =
  ( 'None',
    '2 samples (faster)',
    '2 samples (nicer)',
    '4 samples (faster)',
    '4 samples (nicer)',
    '8 samples (faster) (only latest GPUs)',
    '8 samples (nicer) (only latest GPUs)',
    '16 samples (faster) (only latest GPUs)',
    '16 samples (nicer) (only latest GPUs)'
  );

type
  TCastleWindowBase = class;

  {$I castlewindowmenu.inc}

  { Type of message box, for TCastleWindowBase.MessageOK and TCastleWindowBase.MessageYesNo. }
  TWindowMessageType = (mtInfo, mtWarning, mtQuestion, mtError, mtOther);

  TUpdateFunc = procedure;
  TWindowFunc = procedure (Window: TCastleWindowBase);
  TDrawFunc = TWindowFunc;
  TMouseMoveFunc = procedure (Window: TCastleWindowBase; NewX, NewY: Integer);
  TInputPressReleaseFunc = procedure (Window: TCastleWindowBase; const Event: TInputPressRelease);
  TMenuClickFunc = procedure (Window: TCastleWindowBase; Item: TMenuItem);
  TGLContextRetryOpenFunc = function (Window: TCastleWindowBase): boolean;
  TDropFilesFunc = procedure (Window: TCastleWindowBase; const FileNames: array of string);
  { }
  TResizeAllowed = (raNotAllowed, raOnlyAtOpen, raAllowed);

  EGLContextNotPossible = class(Exception);

  {$define read_interface_types}
  {$I castlewindow_backend.inc}
  {$undef read_interface_types}

  { Window with an OpenGL context.
    See CastleWindow unit description for more info and examples of use. }
  TCastleWindowBase = class(TComponent)

  { Include CastleWindow-backend-specific parts of TCastleWindowBase class.
    Remember to explicitly specify the scope
    (usually "private") of things that you add to TCastleWindowBase class in backends,
    this is safest. Some backends may expose some protected or even public
    things that are specific for them. }

  {$define read_window_interface}
  {$I castlewindow_backend.inc}
  {$undef read_window_interface}

  private
    type
      TCaptionPart = (cpPublic, cpFps);
    var
    FWidth, FHeight, FLeft, FTop: Integer;
    FOnOpen: TWindowFunc;
    FOnBeforeDraw, FOnDraw: TDrawFunc;
    FOnResize: TWindowFunc;
    FOnClose: TWindowFunc;
    FOnCloseQuery: TWindowFunc;
    FOnPress, FOnRelease: TInputPressReleaseFunc;
    FMouseMove: TMouseMoveFunc;
    FOnUpdate, FOnTimer: TWindowFunc;
    FOnDropFiles: TDropFilesFunc;
    FFullScreen, FDoubleBuffer: boolean;
    FResizeAllowed: TResizeAllowed;
    FMousePressed: TMouseButtons;
    FMouseX, FMouseY: integer;
    FRedBits, FGreenBits, FBlueBits: Cardinal;
    FCursor: TMouseCursor;
    FCustomCursor: TRGBAlphaImage;
    FAutoRedisplay: boolean;
    FCaption: array [TCaptionPart] of string;
    BeforeFullScreenGeometryKnown: boolean;
    BeforeFullScreenLeft, BeforeFullScreenTop, BeforeFullScreenWidth, BeforeFullScreenHeight: Integer;

    { FClosed = are we outside of Open..Close }
    FClosed: boolean;

    { EventOpenCalled = has OnOpen been called from Open? }
    EventOpenCalled: boolean;
    CloseErrors: string; { Used by CloseBackend. }

    MenuUpdateInside: Cardinal;
    MenuUpdateNeedsInitialize: boolean;
    MenuInitialized: boolean;

    FFps: TFramesPerSecond;

    FDepthBits: Cardinal;
    FStencilBits: Cardinal;
    FAccumBits: TVector4Cardinal;
    FAlphaBits: Cardinal;
    FMultiSampling: Cardinal;
    FAntiAliasing: TAntiAliasing;
    FGtkIconName: string;
    FVisible: boolean;
    FPressed: TKeysPressed;
    FMinWidth: Integer;
    FMinHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FDpi: Integer;

    function GetColorBits: Cardinal;
    procedure SetColorBits(const Value: Cardinal);
    procedure SetAntiAliasing(const Value: TAntiAliasing);
    procedure SetCursor(const Value: TMouseCursor);
    procedure SetCustomCursor(const Value: TRGBAlphaImage);
    procedure SetAutoRedisplay(value: boolean);
    function GetPublicCaption: string;
    procedure SetPublicCaption(const Value: string);
    procedure SetCaption(const Part: TCaptionPart; const Value: string);
    function GetWholeCaption: string;
    procedure SetFullScreen(const Value: boolean);

    { Set FullScreen value in a dumb (but always reliable) way:
      when it changes, just close, negate FFullScreen and reopen the window.
      This will work, as long as OpenBackend honors the FullScreen setting. }
    procedure SimpleSetFullScreen(const Value: boolean);

    { Used in particular backend, open OpenGL context and do
      Application.OpenWindowsAdd(Self) there.

      Here's a list of properties that should be made "visible" to the user
      in OpenBackend:

        Width, Height, Left, Top
        Cursor, CustomCursor, FullScreen (remember that changes to this after OpenBackend
          should also be allowed)
        ResizeAllowed (DoResize already implements appropriate
          checks, but implementation should provide user with visual clues that
          the window may / may not be resized)
        MainMenu (display MainMenu and provide way to call DoMenuClick)

      OpenGL context must be initialized honouring these properties:
        DoubleBuffer, StencilBits, DepthBits, AlphaBits,
        AccumBits, MultiSampling }
    procedure OpenBackend;

    { Close OpenGL context, for particular backend.

      No need to call OpenWindowsRemove here, it's done by universal Close already.
      It's advised (although not totally required) that all errors during
      CloseBackend should be caught and cause only CloseError.
      Reasoning: Close should, regardless of trouble, try to finalize as much
      as possible. }
    procedure CloseBackend;

    { Make the OpenGL context of this window current (active for following
      OpenGL commands). }
    procedure BackendMakeCurrent;

    procedure CloseError(const error: string);

    { Swap OpenGL buffers.
      Call this method only when DoubleBuffered and if you already did
      MakeCurrent. (Implicit glFlush is guaranteed.) }
    procedure SwapBuffers;

    { BackendMenuInitialize should cause backend to build whole menu resources
      for MainMenu, BackendMenuFinalize to free them.

      BackendMenuFinalize is called before changing MainMenu structure
      in arbitrary way, BackendMenuInitialize is called after.
      Backend should just free / initialize resources related to menu.
      BackendMenuInitialize may assume that BackendMenuFinalize was already called
      (so no need to try to free in BackendMenuInitialize again).

      They are never called directly: always call MenuInitialize / Finalize.
      These make sure to care about eventual MenuUpdateBegin / MenuUpdateEnd
      around, and make sure BackendMenuFinalize is called only when menu is
      already initialized (by BackendMenuInitialize), and
      BackendMenuInitialize is called only when menu is not initialized yet.

      Implementation of these can assume that MainMenu <> nil now,
      and that MainMenuVisible = @true.
      Also it may assume that Closed = false.

      Note: if backend wants, it may itself call these from
      OpenBackend / CloseBackend. Of course, when you call them
      yourself, you have to make sure on your own that all assumptions
      are satisfied. In practice, BackendMenuFinalize should clear all the variables
      to the state right after constructor (zero, nil etc.),
      and BackendMenuInitialize expect them as such, and then everything will work Ok.

      @groupBegin }
    procedure BackendMenuInitialize;
    procedure BackendMenuFinalize;
    { @groupEnd }

    { These call corresponding BackendMenuInitialize or BackendMenuFinalize,
      unless we're inside MenuUpdateBegin / MenuUpdateEnd,
      and take care to only initialize when finalized,
      and finalize only when initialized.
      They also take care of checking Closed and MainMenuVisible properties.
      @groupBegin }
    procedure MenuInitialize;
    procedure MenuFinalize;
    { @groupEnd }

    { For optimization purposes, you may surround many menu changes
      inside MenuUpdateBegin + MenuUpdateEnd calls.
      Make sure window is not closed / opened between them.
      @groupBegin }
    procedure MenuUpdateBegin;
    procedure MenuUpdateEnd;
    { @groupEnd }

    { Notification called immediately after menu entry properties changed.
      This is called only when MainMenu <> nil and Entry is contained
      inside our MainMenu. Also, this is called only when not Closed.

      In case of MenuUpdateXxx calls: You can be sure that
      only appropriate local Entry properties changed, no other menu entry
      (even child menu entry for submenus) was changed.

      Sloppy backend may simply do here MenuFinalize + MenuInitialize,
      but a better backend may do something more efficient,
      like updating only this specific Entry resources.

      In case of MenuInsert: remember that this is called
      after Entry was added to Parent. So if your MenuInsert simply
      calls MenuFinalize + MenuInitialize, make sure your MenuFinalize
      handles the case that some TMenuEntry may have uninitialized (zero)
      TMenuEntry.Handle.

      In case of MenuDelete: remember that this is called
      after Entry was removed from Parent. So if your MenuDelete simply
      calls MenuFinalize + MenuInitialize, make sure your MenuFinalize
      finalizes (releases backend-specific resources) also for Entry
      that isn't in the MainMenu hierarchy anymore.

      Both MenuInsert and MenuDelete problems disappear if your
      MenuFinalize doesn't look at MainMenu.Entries hierarchy, and releases
      in one go whole backend-specific hierarchy under MainMenu.Handle.
      Or if you implement MenuInsert and MenuDelete in a more optimal way,
      without MenuFinalize + MenuInitialize.

      @groupBegin }
    procedure MenuUpdateCaption(Entry: TMenuEntryWithCaption);
    procedure MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
    procedure MenuUpdateChecked(Entry: TMenuItemChecked);
    procedure MenuInsert(const Parent: TMenu; const ParentPosition: Integer; const Entry: TMenuEntry);
    procedure MenuDelete(const Parent: TMenu; const ParentPosition: Integer; const Entry: TMenuEntry);
    function MenuUpdateCheckedFast: boolean;
    { @groupEnd }

    procedure CreateBackend;

    { Simulate that all the keys and mouse buttons were released.
      For all keys that are down (Pressed[k]) calls DoKeyUp(k).
      For all mouse buttons that are down (mb in MousePressed) calls DoMouseUp(mb).
      If CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN is defined,
      this calls at the beginning SetPrivateModifiersDown(..., ..., false)
      to say that all keys are up.

      Useful when user somehow switches to another window / control
      (e.g. when he opens our MainMenu), such that we are then unable to catch
      following KeyUps and MouseUps. So user simply STOPS controlling our
      program with mouse and keyboard, so we have to assume that he releases
      all keys and mouse buttons.

      Nie widze chwilowo zastosowania dla MousePressed[*] := false,
      ale jest to konsekwentne. Z punktu widzenia programu w momencie wejscia
      usera do menu user przestaje kontrolowac program (tzn. okienko OpenGLa)
      przy pomocy myszy i klawiatury. }
    procedure ReleaseAllKeysAndMouse;

    { Should DoKeyDown be able to call DoMenuClick, that is should
      we handle menu key shortcuts ourselves.

      This is implemented in backend-specific CastleWindow parts.
      When in DoKeyDown we get some key event that specifies that
      some menu item should be called -- if RedirectKeyDownToMenuClick,
      DoKeyDown will do DoMenuClick. Else DoKeyDown will do nothing.

      This should be implemened as "Result := true" if we have to process
      keypresses in CastleWindow to pass them as menu commands, e.g. when CastleWindow
      works on top of Xlib.
      When CastleWindow works on top of GTK or WinAPI that allow us to do a "real"
      menu, this should be implemented as "Result := false". }
    function RedirectKeyDownToMenuClick: boolean;

    { DoXxx methods ------------------------------------------------------------

      DoXxx method should be called by CastleWindow backend when an event
      Xxx happens. DoXxx methods take care of various backend-independent
      stuff, and take care of calling EventXxx (that calls OnXxx in turn).
      CastleWindow backend should never call EventXxx directly.
      (And nothing should call OnXxx directly except EventXxx.)

      Remember that no DoXxx may be called from CloseBackend.

      Below is the detailed specification, but summing some things up
      you don't have to worry about these things when you use DoXxx methods
      (these things are fully handled by DoXxx methods):
      - updating state of MousePressed
      - updating state of Pressed (Pressed.Keys, Pressed.Characters etc.)
      - updating state of MouseX, MouseY
      - calling MakeCurrent before every EventXxx
      - flushing gl commands (and swapping gl buffers when DoubleBuffer'ing)
      - taking care of AutoRedisplay
      - updating Width, Height (and updating it with accordance to
        Min/MaxWidth/Height and ResizeAllowed)
      - checking MainMenu.Enabled
    }

    { DoResize with FirstResizeAfterOpen = true is called only once
      (and exactly once) from TCastleWindowBase.Open implementation.
      So all CastleWindow-backend code should always
      pass FirstResizeAfterOpen = false (EVEN if it may be called from
      OpenBackend (that is called before DoResize in Open) !).

      Some more notes about calling DoResize from OpenBackend:
      in this case DoResize will NOT call EventResize (since the first callback
      that should be called for a window is EventOpen). We will always after
      EventOpen call DoResize(..., true), so this should not be a problem
      anywhere. You can simply call DoResize from OpenBackend to tell us what
      real Width/Height we have, and the real EventResize will be called
      just at a later time.

      You can pass here ANY AWidth, AHeight. It will be automatically clipped
      here to fit in Min/MaxWidth/Height and to satisfy ResizeAllowed.

      Also MakeCurrent + EventResize will be called (probably; sometimes
      DoResize may decide to not call EventResize - e.g. when ResizeAllowed
      =raNotAllowed and FirstTime = false then it's useless to call here
      EventResize).

      Remember: this function does not automatically call PostRedisplay.
      You must make sure that soon after changing size of the window
      you will call DoDraw (e.g. you can call PostRedisplay after
      calling DoResize; but usually (under WinAPI, Xlib, GTK)
      it's not needed, i.e. WinAPI, Xlib, and GTK all take care of this
      automatically). }
    procedure DoResize(AWidth, AHeight: integer; FirstResizeAfterOpen: boolean);
    { Wywoluj kiedy user kliknie na przycisku "Zamknij" itp.
      Wywola EventCloseQuery i ew. Close (and Close will execute EventClose,
      CloseBackend etc.). Note that there is no DoClose method and there
      should not be such method : always use DoCloseQuery. }
    procedure DoCloseQuery;

    { Do MakeCurrent,
         EventBeforeDraw,
         EventDraw (inside Fps._RenderBegin/End)
         flush gl command pipeline (and swap gl buffers if DoubleBuffer)

      - Take care of AutoRedisplay, like

          @code(if AutoRedisplay then PostRedisplay;)

        So specific CastleWindow backends need not to worry about
        AutoRedisplay. They only have to implement PostRedisplay. }
    procedure DoDraw;

    { DoKeyDown/Up: pass here key that is pressed down or released up.

      Only DoKeyDown: pass also CharKey. Pass Key = K_None if this is not
      representable as TKey, pass CharKey =#0 if this is not representable
      as char. But never pass both Key = K_None and CharKey = #0
      (this would have no meaning).

      Only DoKeyUp: never pass Key = K_None.

      If you call DoKeyUp while (not Pressed[Key]) it will be ignored
      (will not do any EventRelease etc. - just NOOP).

      This will
         update Pressed (Pressed.Keys, Pressed.Characters, etc.) accordingly,
         DoKeyDown: may here call DoMenuClick
           (and then it will NOT call MakeCurrent and EventKeyDown)
         MakeCurrent,
         EventKeyDown/Up.
    }
    procedure DoKeyDown(Key: TKey; CharKey: char);
    procedure DoKeyUp(key: TKey);
    { Do MakeCurrent,
         EventMouseMove,
         update MouseX, Y }
    procedure DoMouseMove(x, y: integer);
    { DoMouseDown/Up:
        update FMouseX, FMouseY (so that before EventMouseDown/Up position
          of the mouse is set to the current, precise, position)
        update MousePressed
        MakeCurrent
        EventMouseDown/Up }
    procedure DoMouseDown(x, y: integer; btn: CastleKeysMouse.TMouseButton);
    procedure DoMouseUp(x, y: integer; btn: CastleKeysMouse.TMouseButton);
    procedure DoMouseWheel(const Scroll: Single; const Vertical: boolean);
    procedure DoUpdate;
    procedure DoTimer;
    { Just call it when user presses some MenuItem.
      This takes care of MainMenu.Enabled,
        MakeCurent,
        Item.DoClick,
        optional EventMenuClick or EventKeyDown }
    procedure DoMenuClick(Item: TMenuItem);

    procedure DoDropFiles(const FileNames: array of string);

    { Just like FileDialog, but these always get and should set FileName,
      not an URL. Also, for OpenDialog, we make sure that initial FileName
      contains only a path (not the final file name), since this is
      good behaviour for users (even if some API allow to set proposed
      file name). }
    function BackendFileDialog(const Title: string; var FileName: string;
      OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean; overload;

    procedure OpenCore;
    { Current OpenGL buffers configuration required.
      Stuff like DoubleBuffer, AlphaBits, DepthBits,
      StencilBits, AccumBits etc.
      This simply returns a text description of these properties.

      It does not describe the current OpenGL context parameters.
      (It doesn't even need an OpenGL context open.)

      Useful for constructing messages e.g. for EGLContextNotPossible exceptions. }
    function RequestedBufferAttributes: string;
    { Check do given OpenGL buffers configuration satisfies the
      requested configuration.

      So it checks do

@preformatted(
  ProvidedStencilBits >= StencilBits and
  ProvidedDepthBits >= DepthBits ...
)
      and so on. If not, EGLContextNotPossible is raised with detailed
      description (which buffer constraint is not satisfied -- e.g. maybe
      the stencil buffer is not available).

      Note that ProvidedMultiSampling is not checked if MultiSampling is <= 1.
      In other words, if multisampling was not required, ProvidedMultiSampling
      doesn't matter --- it's Ok even ProvidedMultiSampling = 0 and
      MultiSampling = 1, which happens commonly (since our MultiSampling = 1 means
      "no multisampling" and is default, but most backends returns num_samples
      (or something equivalent) as = 0 when multisampling not supported). }
    procedure CheckRequestedBufferAttributes(const ProviderName: string;
      ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
      ProvidedAccumRedBits, ProvidedAccumGreenBits, ProvidedAccumBlueBits,
      ProvidedAccumAlphaBits, ProvidedMultiSampling: Cardinal);

    { For IUIContainer interface. Private, since when you have a class
      instance, you just use class properties (that read directly from a field,
      without the overhead of a function call). }
    function GetMouseX: Integer;
    function GetMouseY: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetDpi: Integer;
    function GetMousePressed: TMouseButtons;
    function GetPressed: TKeysPressed;
  public

    { Handle appropriate event.

      In the TCastleWindowBase class, these methods simply call appropriate OnXxx
      callbacks (if assigned). Also CastleUIControls.OnGLContextOpen,
      CastleUIControls.OnGLContextClose lists are called here too.

      You can override them to do anything you want.

      You can also call these methods directly for some tricks.
      You may want to do MakeCurrent before calling them directly,
      if your application may have many OpenGL windows.
      When EventXxx are called internally from this unit, they are always
      preceded by MakeCurrent call.

      Notes for overriding OnUpdate and OnTimer: you will usually also
      want to override then AllowSuspendForInput, to disallow suspending
      when you want to keep receiving update/timer calls.

      Notes for overriding OnCloseQuery: you have to return @true
      to allow closing of the window.
      @groupBegin }
    procedure EventResize; virtual;
    procedure EventOpen; virtual;
    procedure EventClose; virtual;
    function EventCloseQuery: boolean; virtual;
    procedure EventDraw; virtual;
    procedure EventBeforeDraw; virtual;
    procedure EventPress(const Event: TInputPressRelease); virtual;
    procedure EventRelease(const Event: TInputPressRelease); virtual;
    procedure EventMouseMove(newX, newY: integer); virtual;
    procedure EventUpdate; virtual;
    procedure EventTimer; virtual;
    procedure EventMenuClick(Item: TMenuItem); virtual;
    procedure EventDropFiles(const FileNames: array of string); virtual;
    { @groupEnd }

    { Is it allowed to suspend (for an indefinite amount of time) waiting
      for user input.

      Allowing this is a good thing, as it means our process doesn't eat
      your CPU when it simply waits, doing nothing, for user input.
      On the other hand, you cannot allow this if you want to do some
      things continously, regardless of user input.

      In this class, this simply checks if OnUpdate or OnTimer events
      are assigned. If one of them is, we do not allow to suspend.
      In descendants, you typically want to override this if there's
      a chance you may do something in overridden EventUpdate or EventTimer. }
    function AllowSuspendForInput: boolean; virtual;

    { ------------------------------------------------------------------------
      Stuff that may be initialized only when the window is not open yet.
      When the window is open, these are read-only (may only change
      through internal methods, e.g. we'll update @link(Width), @link(Height),
      @link(Left), @link(Top) to reflect current size and position).  }

    { Size of the window OpenGL area. Together with frame and border
      sizes, and eventually menu bar size, this determines the final
      window size.

      MinWidth / MaxWidth / MinHeight / MaxHeight place constraints
      on these values (rigorously honored when window is open):
      always @code(MinWidth <= Width <= MaxWidth) and
      @code(MinHeight <= Height <= MaxHeight).

      ResizeAllowed places constrains when window manager and user may change
      window size. In particular, when ResizeAllowed <> raAllowed then
      window sizes cannot change when window is open.

      Note that for some window managers, we cannot always reliably
      force the size constraints and block resizing on the desktop.
      If you set rigorous size constraints, or ResizeAllowed <> raAllowed,
      you may find that window manager still resizes the window.
      In such cases, we may fake our size a little ---
      @link(Width) and @link(Height) values may not correspond to actual
      size as seen on the desktop. This is comfortable, as in such cases
      you usually want to just ignore window managers limits and just
      proceed as if your size requirements are satisfied.

      Special WindowDefaultSize value of these properties
      means: at @link(Open), use some comfortable size slightly
      smaller than desktop size.
      @groupBegin }
    property Width: integer read FWidth write FWidth default WindowDefaultSize;
    property Height: integer read FHeight write FHeight default WindowDefaultSize;
    { @groupEnd }

    { Rectangle representing the inside of this container.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to window
      sizes. }
    function Rect: TRectangle;

    { Window DPI. Defaults to 96, so it is recommended to set to correct value
      on higher density displays. }
    property Dpi: integer read FDpi write FDpi default DefaultDpi;

    { Window position on the screen. If one (or both) of them is equal
      to WindowPositionCenter at the initialization (Open) time,
      then it will be set to position the window at the screen center.
      @groupBegin }
    property Left: integer read FLeft write FLeft default WindowPositionCenter;
    property Top :integer read FTop write FTop default WindowPositionCenter;
    { @groupEnd }

    { Whether the window is fullscreen.
      This forces @link(Width) and @link(Height) and position and window style
      to fill the whole screen.

      Note that we always set fullscreen UI (no border, size matching
      Application.ScreenWidth and Application.ScreenHeight), ignoring
      the window size constraints like MinWidth, MaxWidth, MinHeight, MaxHeight
      and ResizeAllowed. For example we do not check does Application.ScreenWidth
      fit inside MinWidth and MaxWidth. It's assumed that you really want
      to set/unset the fullscreen UI if you change this property.
      However, the sizes known to your application (stored in @link(Width), @link(Height))
      are still constrained by MinWidth, MaxWidth, MinHeight, MaxHeight
      and ResizeAllowed.

      You can change this always, even on already open window.
      Just note that some backends don't allow to switch this without
      destroying / recreating the OpenGL context. So be prepared that changing FullScreen
      may result in OnClose + OnOpen sequence, so make sure that closing and opening
      recreates the whole necessary OpenGL state exactly as it was. This is usually
      natural, all our TUIControl automatically work with this,
      so this is only a concern if you do some direct OpenGL tricks. }
    property FullScreen: boolean read FFullScreen write SetFullScreen default false;

    { Deprecated, instead just do @code(FullScreen := not FullScreen). }
    procedure SwapFullScreen; deprecated;

    { Should we request and use the double buffer.
      After every draw, we automatically swap buffers (if DoubleBuffer)
      or call glFlush (if not DoubleBuffer). }
    property DoubleBuffer: boolean read FDoubleBuffer write FDoubleBuffer default true;

    { Required red / green / blue color buffer precision for this window.
      When 0, the default window system color precision will be used.

      You can either set them by separate red / green / blue properties.
      Or you can use ColorBits that reads / writes all three channels bits.
      Reading ColorBits simply returns the sum of
      @code(RedBits + GreenBits + BlueBits).
      Writing ColorBits simply set RedBits and BlueBits to
      @code(ColorBits div 3), and sets GreenBits to the remainder.
      This way green channel has always the best resolution (as is usual,
      since it's perceived most), and the sum is always as requested.
      This way setting ColorBits to values like 16 or 24 works as expected.

      Note that it's also possible to change color resolution by
      changing the whole screen settings.
      See TCastleApplication.VideoColorBits and TCastleApplication.VideoChange for this.
      These properties only request the color
      resolution for this window, which is less intrusive (you don't change
      the whole screen) but also may have a smaller chance of success.

      @groupBegin }
    property RedBits: Cardinal read FRedBits write FRedBits default 0;
    property GreenBits: Cardinal read FGreenBits write FGreenBits default 0;
    property BlueBits: Cardinal read FBlueBits write FBlueBits default 0;
    property ColorBits: Cardinal read GetColorBits write SetColorBits stored false default 0;
    { @groupEnd }

    { Sets mouse cursor appearance over this window.
      See TMouseCursor for a list of possible values and their meanings.

      TODO: for now, mcCustom is not handled anywhere. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Image for cursor, used only when @link(Cursor) = mcCustom.
      We will try hard to use any cursor image as appropriate, but on some platforms
      cursor size may be limited (16 x 16 seems standard for GTK) and cursor
      may be forced to monochrome.

      Note that you still own the TRGBAlphaImage instance passed here --- you're
      responsible for freeing it etc. If this is @nil, and @link(Cursor) = mcCustom,
      then it will be treated like @link(Cursor) = mcDefault. (I don't raise error
      in such case, as that would make changing both Cursor and CustomCursor values
      unnecessarily tricky for the programmer.)

      TODO: for now, this is not implemented. @link(Cursor) ignores mcCustom value,
      under every CastleWindow backend... sorry, CustomCursor is only a plan. }
    property CustomCursor: TRGBAlphaImage read FCustomCursor
      write SetCustomCursor;

    { Place mouse cursor at NewMouseX and NewMouseY.
      Position is specified relative to this window's upper-top corner
      (more specifically, OpenGL area upper-top corner),
      just like MouseX and MouseY properties.

      Note that the actually set position may be different than requested,
      for example if part of the window is offscreen then
      window manager will probably refuse to move mouse cursor offscreen.

      This @italic(may) generate normal OnMouseMove event, just as if the
      user moved the mouse. But it's also allowed to not do this.

      Ignored when window is closed. }
    procedure SetMousePosition(const NewMouseX, NewMouseY: Integer);

    { When (if at all) window size may be changed.

      @unorderedList(
        @item(raNotAllowed

          @link(Width) and @link(Height) can only change
          to honor MinWidth / MaxWidth / MinHeight / MaxHeight constraints.
          Absolutely nothing else may cause them to change,
          user cannot resize the window.

          Note that setting the @link(FullScreen) property to @true
          works regardless of this. Just don't change @link(FullScreen) property
          if you don't want to. However, @link(Width) / @link(Height)
          values will be kept unchanged (they will not be updated to Application.ScreenWidth
          and Application.ScreenHeight), so from the point of view of you code
          the raNotAllowed works always.

          You can be sure that EventResize (OnResize) will be called only
          once, when window is opened (right after initial EventOpen (OnOpen)).)

        @item(raOnlyAtOpen

          @link(Width) and @link(Height) may be adjusted when the window
          is opened, by @link(Open) call. For example window manager
          may decide that the size is too large for the current screen.
          Or when you request FullScreen window and window size has to be
          adjusted to match current screen size. Also they will always be
          adjusted to fit in MinWidth / MaxWidth / MinHeight / MaxHeight constraints.

          After opening, window size cannot change anymore.
          In particular user cannot resize the window (by dragging border
          or such). After the first EventOpen (OnOpen) call,
          the window size becomes constant. From the first EventResize
          (OnResize) the window size is constant, as long as the window
          remains open.

          You can be sure that EventResize (OnResize) will be called only
          once, when window is opened (right after initial EventOpen (OnOpen)).)

        @item(raAllowed

          @link(Width) and @link(Height) may be adjusted at open time,
          and later user can resize the window too.
          This is the default value, giving user and window manager
          the most flexibility.

          You have to be prepared for this, handling OnResize and adjusting
          stuff like OpenGL viewport and projection matrix.)
      )

      Note that the we call the first glViewport automatically in @link(Open).
      So in typical cases, you don't have to call glViewport ever yourself,
      when ResizeAllowed <> raAllowed. }
    property ResizeAllowed: TResizeAllowed
      read FResizeAllowed write FResizeAllowed default raAllowed;

    { Event called when OpenGL context is initialized.

      It's guaranteed that every newly opened window will get
      EventOpen (OnOpen) first, and then EventResize (OnResize),
      and only then --- the other callbacks, as the user uses the window.
      This is consistent EventOpen (OnOpen)
      is always the first executed callback and EventClose (OnClose)
      is always the last. This allows you to cleanly initialize / finalize
      OpenGL resources.

      During EventOpen (OnOpen) you already have valid
      Width / Height values, that is those values were already adjusted
      if ResizeAllowed <> raNotAllowed. }
    property OnOpen: TWindowFunc read FOnOpen write FOnOpen;

    { Minimum and maximum window sizes. Always

@preformatted(
  0 < MinWidth <= MaxWidth and
  0 < MinHeight <= MaxHeight
)

      We do not allow user to resize the window outside of these constraints.

      We also fix window @link(Width) and @link(Height) to fit within
      these constraints when you @link(Open) the window. We do it regardless
      of ResizeAllowed (even when it's raNotAllowed).

      In other words, these constraints have a higher priority than
      ResizeAllowed and your desired @link(Width) and @link(Height)
      and even @link(FullScreen) (setting @link(FullScreen) property
      will stil change the visible sizes, but your perceived sizes
      will be constrained by this min/max).
      So you can be sure that (as long as window
      is open) @link(Width) / @link(Height) will always fit in these constraints.
      @groupBegin }
    property MinWidth: Integer read FMinWidth write FMinWidth default 100;
    property MinHeight: Integer read FMinHeight write FMinHeight default 100;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 4000;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 4000;
    { @groupEnd }

    { Required depth buffer precision. Zero means that we don't need
      depth buffer at all. We may get depth buffer with more precision
      than requested (we may even get depth buffer when we set
      DepthBits = 0), this all depends on graphic card.

      Default value is 16 (DefaultDepthBits),
      which is a reasonable default for 3D programs
      that want to work with depth test enabled.

      @italic(Design notes:) One may ask why default value is not 0?

      @orderedList(
        @item(
          Most programs using OpenGL use depth testing, so many programs
          would have to call something like @code(Window.DepthBits := 16).)

        @item(
          Often graphic cards / window systems / OSes give you an OpenGL
          context with depth buffer @italic(even if you don't need depth buffer).
          I don't say that it's bad. But it makes very easy to forget about
          doing @code(DepthBits := something-non-zero;).
          If you're writing 3d program and sitting on some
          system that always gives you depth buffer (even if DepthBits = 0)
          then it may happen that you forget to write in your program
          @longCode(#  Window.DepthBits := 16;#)

          And while on your system everything will work, you will
          receive errors on other systems because you forgot to request a
          depth buffer.)
      )

      Of course, if you are writing a program that does not need depth buffer
      you should set Window.DepthBits := 0. The only advantage of having
      default DepthBits = 16 is that if you forget to set
      Window.DepthBits := 0 your programs will still work (most graphic cards
      will give you some depth buffer anyway).
      They will just use more resources than they should.
    }
    property DepthBits: Cardinal
      read FDepthBits write FDepthBits default DefaultDepthBits;

    { Required stencil buffer precision, zero means that stencil buffer is
      not needed.

      Just like with other XxxBits property, we may get more
      bits than we requested. But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      @link(Open) will raise an error.

      Note that after initializing OpenGL context (when opening the window),
      StencilBits is @italic(not) updated to the current (provided)
      stencil buffer bit size. For example, if you requested StencilBits := 8,
      and you got 16-bits buffer: StencilBits value will still remain 8.
      This is sensible in case you close the window, tweak some settings
      and try to open it again. Use @code(glGetInteger(GL_STENCIL_BITS))
      when window is open to query current (actual) buffer size. }
    property StencilBits: Cardinal
      read FStencilBits write FStencilBits default 0;

    { How many samples are required for multi-sampling (anti-aliasing).
      Use @link(AntiAliasing) instead of this for more comfortable
      (higher-level) way to turn on multi-sampling (anti-aliasing).

      1 means that no multi-sampling is required.
      Values larger than 1 mean that we require OpenGL context with
      multi-sampling capabilities. Various GPUs may support various
      values (it's a trade-off between quality and speed),
      try typical values 2 or 4.

      You can enable/disable anti-aliasing in your program by code like
      @longCode(#
        if GLFeatures.Multisample then glEnable(GL_MULTISAMPLE_ARB);
        if GLFeatures.Multisample then glDisable(GL_MULTISAMPLE_ARB);
      #)
      But usually that's not needed, as it is "on" by default
      (GL_ARB_multisample spec says so) if you requested multi-sampling context
      (that is, if this property is > 1). See GL_ARB_multisample spec for details:
      [http://opengl.org/registry/specs/ARB/multisample.txt].

      Just like with other XxxBits property, we may get more
      samples than we requested (e.g. if you request 3, you will most probably
      get 4). But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      @link(Open) will raise an error.
      TODO: actually, this may change to be similar to Lazarus
      TOpenGLControl.MultiSampling, and also be more comfortable --- to retry
      initialization with no multi-sampling. In this case this property will
      not be changed, to be nice.

      You can always read OpenGL GL_SAMPLE_BUFFERS_ARB and GL_SAMPLES_ARB
      values after initializing OpenGL context, to know exactly
      how many samples did you actually get, and did you get multi-sampling at all.
      Actually, we already initialize global CastleGLUtils.GLCurrentMultiSampling
      for you, you can use this. }
    property MultiSampling: Cardinal
      read FMultiSampling write FMultiSampling default 1;

    { Comfortably turn on/off anti-aliasing.

      Setting this property automatically sets also the @link(MultiSampling)
      property. Although it's easy to request multi-sampling by using the
      @link(MultiSampling) property directly, using AntiAliasing is a little
      more comfortable. You don't have to wonder what are the sensible
      values of @link(MultiSampling) for common GPUs, and we also
      automatically use NV_multisample_filter_hint for nicer anti-aliasing
      when possible. }
    property AntiAliasing: TAntiAliasing
      read FAntiAliasing write SetAntiAliasing default DefaultAntiAliasing;

    { Required number of bits in alpha channel of color buffer.
      Zero means that alpha channel is not needed.

      Just like with other XxxBits property, we may get more
      bits than we requested. But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      @link(Open) will raise an error.

      It's undefined how I'll treat this variable when indexed color mode
      will be possible in TCastleWindowBase. }
    property AlphaBits: Cardinal
      read FAlphaBits write FAlphaBits default 0;

    { Required number of bits in color channels of accumulation buffer.
      Color channel is 0..3: red, green, blue, alpha.
      Zero means that given channel of accumulation buffer is not needed,
      so when the vector is all zeros (default value) this means that
      accumulation buffer is not needed at all.

      Just like with other XxxBits property, we may get more
      bits than we requested. But we will never get less --- if window system
      will not be able to provide GL context with requested number of bits,
      @link(Open) will raise an error.

      @deprecated
      This property is deprecated, since modern OpenGL deprecated accumulation
      buffer. It may not be supported by some backends (e.g. now LCL backend,
      the default backend on Mac OS X, doesn't support it). }
    property AccumBits: TVector4Cardinal read FAccumBits write FAccumBits; deprecated;

    { Name of the icon for this window used by GTK 2 backend.

      This is simply passed to @code(gtk_window_set_icon_name),
      see [http://library.gnome.org/devel/gtk/stable/GtkWindow.html#gtk-window-set-icon-name].
      This allows you to use an installed icon (in /usr/share/icons/
      or ~/.local/share/icons/) for your program. See
      [http://library.gnome.org/devel/integration-guide/stable/icons.html.en]
      for short information how and where to install your icons.

      It's ignored on non-GTK 2 backends. }
    property GtkIconName: string read FGtkIconName write FGtkIconName;

    (*Should this window be actually displayed on the desktop.
      In all normal programs you want to leave this as @true, as the
      main purpose of the window is to actually be visible and interactive
      on the desktop.

      Setting this to @false allows you to get an OpenGL context without
      showing anything on the desktop. This can be used for rendering
      and capturing OpenGL stuff without showing it on the desktop.
      One example is the @--screenshot option of view3dscene, see
      [http://castle-engine.sourceforge.net/view3dscene.php#section_screenshot].

      If you implement such thing, remember that you should not render
      and capture the normal front or back buffer contents.
      OpenGL makes no guarantee that a hidden window will have any allocated
      memory, so capturing hidden window contents isn't useful (you may
      get something valid, or you may get random / blank screen, depending
      on OS and GPU). However, you can create Framebuffer Object
      on modern GPUs, and capture it's contents. An example code snippet:

@longCode(#
{ add CastleGLImages, CastleImages to your uses clause }

var
  ScreenshotRender: TGLRenderToTexture;
  Image: TRGBImage;
begin
  ScreenshotRender := TGLRenderToTexture.Create(Width, Height);
  try
    ScreenshotRender.Buffer := tbNone;
    ScreenshotRender.GLContextOpen;
    ScreenshotRender.RenderBegin;

    { render your stuff here }

    { capture the screen }
    Image := SaveScreen_NoFlush(Rectangle(0, 0, Width, Height),
      ScreenshotRender.ColorBuffer);
    try
      SaveImage(Image, 'aaa.png');
    finally FreeAndNil(Image) end;

    ScreenshotRender.RenderEnd;
  finally FreeAndNil(ScreenshotRender) end;
end;
#)
       *)
    property Visible: boolean read FVisible write FVisible default true;

    { Caption of the window. By default it's initialized to ApplicationName.
      May be changed even when the window is already open. }
    property Caption: string read GetPublicCaption write SetPublicCaption;

    { Draw your window contents here.

      Called when your window contents must be redrawn,
      e.g. after creating a window, after resizing a window, after uncovering
      the window etc. You can also request yourself a redraw of the window
      by the PostRedisplay method, which will cause this event to be called
      at nearest good time.

      Note that calling PostRedisplay while in EventDraw (OnDraw) is not ignored.
      It means that in a short time next EventDraw (OnDraw) will be called. }
    property OnDraw: TDrawFunc read FOnDraw write FOnDraw;

    { Always called right before EventDraw (OnDraw).
      These two events, EventBeforeDraw (OnBeforeDraw) and EventDraw (OnDraw),
      will be always called sequentially as a pair.

      The only difference between these two events is that
      time spent in EventBeforeDraw (OnBeforeDraw)
      is NOT counted as "frame time"
      by Fps.FrameTime. This is useful when you have something that needs
      to be done from time to time right before OnDraw and that is very
      time-consuming. It such cases it is not desirable to put such time-consuming
      task inside OnDraw because this would cause a sudden big change in
      Fps.FrameTime value. So you can avoid this by putting
      this in OnBeforeDraw. }
    property OnBeforeDraw: TDrawFunc read FOnBeforeDraw write FOnBeforeDraw;

    { Called when the window size (@link(Width), @link(Height)) changes.
      It's also guaranteed to be called during @link(Open),
      right after the EventOpen (OnOpen) event.

      Our OpenGL context is already "current" when this event is called
      (MakeCurrent is done right before), like for other events.
      This is a good place to set OpenGL viewport and projection matrix.

      See also ResizeAllowed.

      Simple 2D OpenGL programs may want to register here simple
      @link(Resize2D). }
    property OnResize: TWindowFunc read FOnResize write FOnResize;

    { Called when the window is closed, right before the OpenGL context
      is destroyed. This is your last chance to release OpenGL resources,
      like textures, shaders, display lists etc. This is a counterpart
      to OnOpen event. }
    property OnClose: TWindowFunc read FOnClose write FOnClose;

    { Called when user presses a key or mouse button or moves mouse wheel. }
    property OnPress: TInputPressReleaseFunc read FOnPress write FOnPress;

    { Called when user releases a pressed key or mouse button.

      Details about key up events:
      It's called right after
      Pressed[Key] changed from true to false.

      Key is never K_None.

      C may be #0 is no representable character is released.
      When C is <> #0, we detected that some character is released.
      This is connected with setting Characters[C] from @true to @false.

      Note that reporting characters for "key release" messages is not
      perfect, as various key combinations (sometimes more than one?) may lead
      to generating given character. We have some intelligent algorithm
      for this, used to make Characters table and to detect
      this C for OnRelease callback. The idea is that a character is released
      when the key that initially caused the press of this character is
      also released.

      This solves in a determined way problems like
      "what happens if I press Shift, then X,
      then release Shift, then release X". (will "X" be correctly
      released as pressed and then released? yes.
      will small "x" be reported as released at the end? no, as it was never
      pressed.) }
    property OnRelease: TInputPressReleaseFunc read FOnRelease write FOnRelease;

    { Called when user tries to close the window.
      This is called when you use window manager features to close the window,
      like clicking on the "close" icon on the window frame or using Alt+F4
      on most desktops. This is @italic(not) called when you explicitly
      close the window by calling the @link(Close) method.

      When this callback is not assigned, we will
      just let the window be closed. When it's assigned,
      the window will not closed --- you should call here @link(Close)
      explicitly if you want to (for example, after asking user for
      confirmation "do you really want to quit?").

      When handling this event, you must remember that user
      may try to close our window at any time.
      E.g. if you're implementing here somehing like showing user
      text "You cannot quit now" or asking user "Do you really want to quit"
      remember that while you display such message to user and you're
      processing events (e.g. looking for keypress "Yes" or "No"),
      user may try to close your window again.

      CastleMessages unit offers some nice routines that you can safely
      use here, e.g. you can use it inside OnCloseQuery like

        if MessageYesNo(Window, 'Are you sure you want to quit?') then
         Close;

      Inside MessageYesNo, when we're processing events,
      and waiting for user's answer (yes or no),
      futher OnCloseQuery events will be ignored, so everything will work OK.

      This event is also useful if you want to call Close(false)
      on closing the window (i.e. QuitWhenLastWindowClosed = false).
      By default, if this event is undefined, we call Close(true)
      when user tries to close the window. }
    property OnCloseQuery: TWindowFunc read FOnCloseQuery write FOnCloseQuery; { = nil }

    { Called when mouse is moved. Remember you always have the currently
      pressed mouse buttons in MousePressed. When this is called,
      the MouseX, MouseY properties describe the @italic(previous)
      mouse position, while callback parameters NewX, NewY describe
      the @italic(new) mouse position. }
    property OnMouseMove :TMouseMoveFunc read FMouseMove write FMouseMove;

    { Continously occuring event, called for all open windows.
      This event is called at least as regularly as redraw,
      so it is continously called even when your game
      is overwhelmed by messages (like mouse moves) and redraws.

      Called at the same time when
      @link(TCastleApplication.OnUpdate Application.OnUpdate) is called.

      You should add code to this window's OnUpdate event
      (not to TCastleApplication.OnUpdate) when you do something related
      to this window. For example when you check this window's
      @link(Pressed) keys state, or animate something displayed on this window.
      This allows various "modal boxes" and such (see CastleMessages)
      to nicely "pause" such processing by temporarily replacing
      OnUpdate and other events of a window that displays a modal box. }
    property OnUpdate: TWindowFunc read FOnUpdate write FOnUpdate;

    { @deprecated Deprecated name for OnUpdate. }
    property OnIdle: TWindowFunc read FOnUpdate write FOnUpdate; deprecated;

    { Timer event is called approximately after each
      @link(TCastleApplication.TimerMilisec Application.TimerMilisec)
      miliseconds passed. See also
      @link(TCastleApplication.OnTimer Application.OnTimer).

      This is a very simple timer mechanism, as all timers (timers for all windows
      and the global @link(Application) timer) use the same delay:
      @link(TCastleApplication.TimerMilisec Application.TimerMilisec).
      We consciously decided to not implement anything more involved here.
      If you need really flexible timer mechanism, do not use this.
      Instead use @link(OnUpdate)
      (or TUIControl.Update, or T3D.Update) and look at it's @code(SecondsPassed)
      value to perform actions (one time or repeated) with a specified delay.
      The engine source is full of examples of this.

      Under Lazarus, you can of course also use LCL timers. }
    property OnTimer: TWindowFunc read FOnTimer write FOnTimer;

    { Called when user drag and drops file(s) on the window.
      In case of Mac OS X bundle, this is also called when user opens a document
      associated with our application by double-clicking.
      Note: this is currently supported only by CASTLE_WINDOW_LCL backend. }
    property OnDropFiles: TDropFilesFunc read FOnDropFiles write FOnDropFiles;

    { Should we automatically redraw the window all the time,
      without a need for PostRedisplay call.
      If @true, window will behave like a redraw is always needed,
      and EventDraw (OnDraw) will be always called as often as posible.
      This may be a waste of OS resources, so don't use it, unless
      you know that you really have some animation displayed
      all the time. }
    property AutoRedisplay: boolean read fAutoRedisplay write SetAutoRedisplay
      default false;

    { -------------------------------------------------------------------------
      Menu things (menu may be modified at runtime, everything will be
      automatically properly redisplayed etc.) }

  private
    FMainMenu: TMenu;
    FMainMenuVisible: boolean;
    FOwnsMainMenu: boolean;
    FOnMenuClick: TMenuClickFunc;
    FUserData: Pointer;
    procedure SetMainMenu(Value: TMenu);
  public
    { Menu bar of this window.
      When not assigned, we have no menu bar.

      Note that MainMenu.Caption will be ignored.

      You can change this freely while Closed.

      You can change this almost freely while not Closed: you can use
      various properties of TMenuEntry descendants (adding, deleting items
      from TMenu, changing Caption, Key, CharKey, Checked properties --
      anything) and you can change value of MainMenu BUT you must not
      change MainMenu <> nil state when the window is not Closed.
      I.e. if you called Open with MainMenu = nil, then MainMenu must stay
      nil unit Close. If you called Open with MainMenu <> nil, then you
      can assign other MainMenu values while not Closed, but only values
      <>nil. I.e. you can't set MainMenu to nil if you called Open
      with MainMenu <> nil.
      See @code(castle_game_engine/examples/window/window_menu.lpr)
      for demo of changing value of MainMenu while window is not Closed.

      Note that MainMenu.Enabled is honoured (as well as Enabled
      for all menu items inside, of course).
      You can use this to disallow user from clicking on the whole
      menu. When MainMenu.Enabled = @false then
      no MenuItem.DoClick, no EventMenuClick
      will be called when user presses some menu item.
      When user presses some keyboard shortcut for some menu item,
      no MenuItem.DoClick and no EventMenuClick will be called,
      but instead normal EventPress (OnPress) will be called.

      When it is useful to set this to false?
      For example hen using CastleWindowModes. When you're changing modes (e.g. at the
      beginning of CastleMessages.MessageOk) you're temporary setting
      OnMenuClick to nil, but this doesn't block TMenuItem.DoClick
      functions. The only way to block menu from triggering ANY event is to
      set this to MainMenu.Enabled to @false. }
    property MainMenu: TMenu read FMainMenu write SetMainMenu;

    { Is MainMenu visible. @false means that we do not show main menu bar,
      but menu key shortcuts should still work.
      Right now, you can reliably change this only before window is open. }
    property MainMenuVisible: boolean
      read FMainMenuVisible write FMainMenuVisible default true;

    { If true then in TCastleWindowBase destructor MainMenu will be destroyed too
      (if not nil, od course). Usually this is something useful. }
    property OwnsMainMenu: boolean read FOwnsMainMenu write FOwnsMainMenu default true;

    { Called each time user chooses some menu item and it's not handled
      in TMenuItem.DoClick. By default, menu item handling is passed
      to TMenuItem.DoClick. Only when it return @false (not handled) then
      we call this window's event. }
    property OnMenuClick: TMenuClickFunc read FOnMenuClick write FOnMenuClick;

    { Deprecated name for OnMenuClick. }
    property OnMenuCommand: TMenuClickFunc read FOnMenuClick write FOnMenuClick; deprecated;

    { @section(Mouse state) -------------------------------------------------- }

    { Currently pressed mouse buttons. When this changes, you're always
      notified by OnMouseDown or OnMouseUp calls.

      This value is always current, in particular it's already updated
      when we call events OnMouseDown and OnMouseUp. }
    property MousePressed: TMouseButtons read FMousePressed;

    { Mouse position. This is the mouse position relative to this window,
      more precisely relative to the OpenGL control of this window.

      Left-top corner is (0, 0), and right-bottom is (Width - 1, Height - 1).
      This is consistent with most window libraries (GTK, LCL etc.).
      Plese note that Y coordinate is reversed with respect to the typical OpenGL
      Ortho2D projection, if needed you'll have to adjust it (by using
      @code(Height - MouseY)).

      Note that we have mouse capturing (when user presses and holds
      the mouse button, all the following mouse events are reported to this
      window, even when user moves the mouse outside of the window).
      This is typical of all window libraries (GTK, LCL etc.).
      This implicates that mouse positions are sometimes tracked also
      when mouse is outside the window, which means that mouse position
      may be outside the rectangle (0, 0) - (Width - 1, Height - 1),
      so it may even be negative.

      In all situations the MouseX, MouseY is the latest known mouse position.
      The only exception is within EventMouseMove (and so, also in OnMouseMove
      callback): MouseX, MouseY is then the previous known mouse position,
      while new mouse position is provided as NewX, NewY arguments to
      EventMouseMove (and OnMouseMove).

      @groupBegin }
    property MouseX: integer read FMouseX;
    property MouseY: integer read FMouseY;
    { @groupEnd }

    { Place for your pointer, for any purposes.
      No code in this unit touches the value of this field.
      This is similar to TComponent.Tag property. }
    property UserData: Pointer read FUserData write FUserData;

    property Closed: boolean read FClosed default true;

    { Create the window with associated OpenGL context and show it.

      @unorderedList(
        @item(Create window, it's OpenGL area, optionally it's menu.)
        @item(Create OpenGL context associated with it's OpenGL area.)
        @item(Show the window.)
        @item(Call LoadAllExtensions.
          This way every information initialized by this
          is ready, like GLVersion, GLUVersion, extensions are checked
          and initialized.)

        @item(Initial events called:
          @unorderedList(
            @itemSpacing Compact
            @item Call MakeCurrent, EventOpen (OnOpen)
            @item Call MakeCurrent, EventResize (OnResize)
            @item(Call MakeCurrent once again, to be sure that after Open
              active OpenGL context is the one associated with newly created
              window (in case you would change active OpenGL context inside
              EventResize (OnResize), which is allowed).)
          )
        )
      )

      Call to this method is ignored if the window is already open
      (if @link(Closed) = @false).

      @raises(EGLContextNotPossible
        If it's not possible to obtain
        OpenGL context with specified attributes.
        For example, maybe you set AlphaBits, DepthBits, StencilBits, AccumBits
        properties too high?

        It's guaranteed that when EGLContextNotPossible
        is raised, the window remains in correct (closed) state.
        This means that you can catch EGLContextNotPossible
        and lower some OpenGL buffer requirements and try to open once again.
        Although it's usually more comfortable to use the overloaded
        version of @link(Open) with Retry callback for this purpose.

        This parameterless version of Open automatically
        turns off multi-sampling (AntiAliasing and MultiSampling
        properties), and then we turn off
        stencil buffer (StencilBits), if OpenGL context cannot be initialized.
        But if it still cannot be initialized, we raise EGLContextNotPossible.
        You can use overloaded Open version with Retry callback to customize
        this fallback mechanism, to code which OpenGL context features
        may be turned off.)
    }
    procedure Open;

    { Open the window with OpenGL context, allowing you to lower
      the OpenGL context requirements and retry.

      If the OpenGL context cannot be initialized,
      then @code(Retry) callback is called. Inside this callback you should
      either:

      @unorderedList(
        @item(lower some context requirements (like set MultiSampling to 1
          if it was > 1) if possible, and return @true to retry, or)
        @item(do not change context requirements and return @false to give up.)
      )

      Note that the parameterless version of @link(Open) method
      actually calls this version, with a default retry callback
      that turns off AntiAliasing and MultiSampling, and then StencilBits
      (since all our engine code should be ready that multi-sampling
      or stencil buffers may not be available).
      Using your own Retry callback, with this version,
      allows you to decide which context parameters may be lowered
      to allow creating a window.

      @raises(EGLContextNotPossible If it's not possible to obtain
        requested OpenGL context, and the @code(Retry) callback
        returned @false.) }
    procedure Open(const Retry: TGLContextRetryOpenFunc);

    { Close window.

      @unorderedList(
        @item(Calls EventClose (and OnClose).)
        @item(Hides window, destroys it.)
        @item(
          if this was the only open TCastleWindowBase window
          and QuitWhenLastWindowClosed = true then
          this calls Application.Quit.)
      )

      Note that often there's no need to call Close explicitly in your program,
      because in destructor of this object we call Close, to be sure
      that window is closed.

      TODO: zrobic param boolean CloseFromDestroyQuitWhenLastWindowClosed?
      As for now Close from destructor is called always with
      QuitWhenLastWindowClosed = true.

      Call to Close is ignored if window is already Closed. }
    procedure Close(QuitWhenLastWindowClosed: boolean = true);

    { Make contents of OpenGL area of this window
      redrawn, at the nearest good time. The redraw will not happen
      immediately, we will only "make a note" that we should do it soon.
      Redraw means that we call EventBeforeDraw (OnBeforeDraw), EventDraw
      (OnDraw), then we flush OpenGL commands, swap buffers etc.

      Calling this on a closed window is allowed and ignored. }
    procedure PostRedisplay;

    { Force redraw of OpenGL area @italic(right now),
      only if any redraw is needed.

      If we know we should redraw a window (for example, because window
      manager just said that window is brought to front of the desktop,
      or because you called PostRedisplay) then we will redraw
      the window @italic(right now). This method will directly
      call EventBeforeDraw (OnBeforeDraw), EventDraw
      (OnDraw), flush OpenGL commands, swap buffers and such.

      You really should not use this method too often. It's best to leave
      to this unit's internals decision when the redraw should happen,
      and allow us to redraw only once even if you called PostRedisplay
      many times in a short time.

      The one valid reason for using this function is when you need
      to read back the drawn window contents (e.g. by glReadPixels).
      Then you want to make sure first that any pending redraws are
      actually done --- this method allows you to do this. }
    procedure FlushRedisplay;

    { Make the OpenGL context of this window "current" (following OpenGL
      commands will apply to this). When the window is opened, and right
      before calling any window callback, we always automatically call
      this, so you should not need to call this method yourself
      in normal circumstances. }
    procedure MakeCurrent;

    { Capture the current window contents to an image (file).

      These functions take care of flushing any pending redraw operations
      (like FlushRedisplay: they will redraw the window if necessary)
      and then capture the screen contents correctly.

      Note that only capturing the double-buffered windows (the default)
      is reliable.
      @groupBegin }
    procedure SaveScreen(const fname: string); overload;
    function SaveScreen: TRGBImage; overload;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload;
    function SaveScreenToGL(const ScalingPossible: boolean = false): TGLImage; overload;
    function SaveScreenToGL(const SaveRect: TRectangle;
      const ScalingPossible: boolean = false): TGLImage; overload;
    { @groupEnd }

    { Color buffer where we draw, and from which it makes sense to grab pixels.
      Use with SaveScreen_NoFlush. }
    function SaveScreenBuffer: TColorBuffer;

    { Asks and saves current screenshot.
      Asks user where to save the file (using @link(FileDialog),
      as default URL taking ProposedURL).
      If user accepts calls Window.SaveScreen.
      In case of problems with saving, shows a dialog (doesn't raise exception). }
    procedure SaveScreenDialog(ProposedURL: string);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    { Tracks which keys, characters, modifiers are pressed. }
    property Pressed: TKeysPressed read FPressed;

    { Fps -------------------------------------------------------------------- }

    { Frames per second measuring. }
    property Fps: TFramesPerSecond read FFps;

    { OpenAndRun stuff --------------------------------------------------------- }

    { Shortcut for Open (create and show the window with GL contex)
      and Application.Run (run the event loop). }
    procedure OpenAndRun; overload;

    { Shortcut for setting Caption, OnDraw,
      then calling Open (create and show the window with GL contex)
      and Application.Run (run the event loop). }
    procedure OpenAndRun(const ACaption: string; AOnDraw: TDrawFunc); overload;

    { Parsing parameters ------------------------------------------------------- }

    { Parse some command-line options and remove them from @link(Parameters)
      list. AllowedOptions specify which command-line options are handled.
      See [http://castle-engine.sourceforge.net/opengl_options.php] for
      documentaion what these options actually do from user's point of view.

      @definitionList(
        @itemLabel poGeometry
        @item(Handle these command-line options:
          @unorderedList(
            @itemSpacing Compact
            @item(@--fullscreen: sets FullScreen to @true.)
            @item(@--geometry: sets FullScreen to @false
              and changes @link(Width), @link(Height), @link(Left), @link(Top)
              as user wants.)
          )
        )

        @itemLabel poScreenGeometry
        @item(Handle @--fullscreen-custom: sets FullScreen and VideoResize
          to @true, initializes VideResizeWidth and VideResizeHeight
          and actually tries to change your desktop resolution by VideoChange.)

        @itemLabel poDisplay
        @item(Handle @--display: sets Application.XDisplayName under Unix.)

        @itemLabel poMacOsXProcessSerialNumber
        @item(
          (Only relevant on Mac OS X) A special parameter -psvn_x_xxx will be found
          and removed from the @link(Parameters) list. See
          http://forums.macrumors.com/showthread.php?t=207344 and
          http://stackoverflow.com/questions/10242115/os-x-strange-psn-command-line-parameter-when-launched-from-finder .
        )
      )

      Multiple options of the same kind are allowed, for example two options
      @code(--fullscreen --geometry 100x100+0+0) are allowed. Each of them will
      have appropriate effect, in the above example, @--fullscreen param
      will be overridden by following @--geometry param. Such overridding
      is sometimes useful from shell scripts.

      Overloaded version with SpecifiedOptions says which command-line
      options were found and handled. For example, if poGeometry, then
      you know that user requested some window size.

      @raises(EInvalidParams When some of our options have invalid arguments.) }
    procedure ParseParameters(
      const AllowedOptions: TWindowParseOptions = StandardParseOptions); overload;
    procedure ParseParameters(
      const AllowedOptions: TWindowParseOptions;
      out SpecifiedOptions: TWindowParseOptions); overload;

    { Help text for options in AllowedOptions.
      The idea is that if you call @code(ParseParameters(AllowedOptions))
      in your program then you should also show your users somwhere
      (e.g. in response to "--help" option) the list of allowed
      options obtained by @code(ParseParametersHelp(AllowedOptions))
      (i.e. with the same value of AllowedOptions).

      Returned string may be multiline, but it does not contain
      the trailing newline (newline char after the last line).

      Returned help text conforms to rules in
      @code(castle_game_engine/doc/kambi_command_line_params.txt).

      If AddHeader then it adds line saying @code('Window options:')
      (and showing backend name, for debug purposes)
      at the beginning. This allows you to comfortably use
      the output of this function as a whole
      paragraph (separated from the rest of your "--help" text
      by e.g. empty lines around). }
    class function ParseParametersHelp(
      const AllowedOptions: TWindowParseOptions;
      AddHeader: boolean): string;

    { dialog boxes using GUI ------------------------------------------------ }

    { About all dialogs:
      - Behaviour of callbacks:
        callbacks of Application and callbacks of other TCastleWindowBase MAY be called while
        the dialog is open. Callbacks of THIS object (OnXxx) will not be
        called. You should treat XxxDialog like
          TGLMode.Create(Self, ...)
          TCastleWindowBaseState.SetStandardState
          ....
          TGLMode.Free
      - How does these dialogs look like?
        Under GTK and WinAPI backends we use native dialogs of these.
        Under Xlib backend we simply fallback on CastleMessages.Message*.
    }

    { Select a file to open or save.
      Accepts and returns argument as an URL.
      Passing a filename as an URL is also allowed (as everywhere),
      it may be changed into an URL on return.

      This dialog may also allow user for some typical file-management
      operations by the way (create some directories, rename some files etc.).

      Returns @true and sets URL accordingly if user chooses some
      file and accepts it. Returns @false if user cancels.

      @param(Title A dialog title.)

      @param(URL Specifies default file as an URL (or simple filename).

        In short, things are designed such that for normal file viewers,
        you can give here the URL of last opened file, or '' if none.

        This URL can be absolute or relative, may include a path, may include a name.
        If you specify only a path (remember to end it with the slash),
        then it's the default path where to save the file.
        If you specify the name (component after final slash), then it's the
        proposed file name for saving (for OpenDialog, this proposed file name
        is ignored, since that's more natural for open dialogs).

        Empty value ('') always means the same as "current directory", guaranteed.
        So it's equivalent to @code(URICurrentPath).

        Note that the path must end with a slash. Otherwise '/tmp/blah' would be
        ambigous (it could mean either file name 'blah' in the dir '/tmp/' dir,
        or dir '/tmp/blah' without a proposed file name).)

      @param(OpenDialog Is this an open (@true) or save (@false) file dialog.

        @unorderedList(
          @item(
            If OpenDialog = @true: force the user to only choose existing
            (and readable) file. The intention is that you can open
            file indicated by URL, at least for reading. There is no guarantee
            about it though (it's not possible to guarantee it on a multi-process OS),
            the only 100% sure way to know that the file can be opened and read
            is to actually try to do it.

            To directly read a file (as a stream) from the obtained URL
            you should usually use our CastleDownload.Download function.
            This way you get a readable stream and you
            automatically support loading data from the other protocols (http,
            data etc.) too.)

          @item(
            If OpenDialog = @false: a save dialog.
            Allows user to select a non-existing file.
            If user chooses an existing file, some backends may show
            a warning like @italic("Are you sure you want to overwrite this file?").

            The intention is that directory of the returned file should exist,
            and you should be able to write files there.
            But, again, there is no 100% guarantee about it.
            The only way to be sure whether you can save a file is to actually
            try to do it.

            To directly write to a file (as a stream) to the obtained URL
            you should usually use our URLSaveStream.)
        )
      )

      @param(FileFilters A set of file filters to present to user.
        Pass @nil (default) if you do not want to use file file filters,
        so user will just always see everything. An overloaded version
        allows you to pass file filters encoded in a single string,
        this may be slightly more comfortable for call, see
        TFileFilterList.AddFiltersFromString
        for explanation how to encode filters in a string.)

      @groupBegin }
    function FileDialog(const Title: string; var URL: string;
      OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean; overload;
    function FileDialog(const Title: string; var URL: string;
      OpenDialog: boolean; const FileFilters: string): boolean; overload;
    { @groupEnd }

    { Shows a dialog window allowing user to choose an RGB color.
      Initial value of Color specifies initial RGB values proposed to the user.
      If user accepts, returns true and sets Color accordingly, else
      returns false (and does not modify Color).

      @groupBegin }
    function ColorDialog(var Color: TCastleColor): boolean;
    function ColorDialog(var Color: TVector3Single): boolean;
    function ColorDialog(var Color: TVector3Byte): boolean;
    { @groupEnd }

    { Simple "OK" dialog box. }
    procedure MessageOK(const S: string; const MessageType: TWindowMessageType);

    { Simple yes/no question dialog box. }
    function MessageYesNo(const S: string;
      const MessageType: TWindowMessageType = mtQuestion): boolean;
  end;

  { Window with OpenGL context and some helper functionality, useful
    for demo programs. This includes key shortcuts to close the window,
    switch FullScreen, and to display FPS on window caption. }
  TCastleWindowDemo = class(TCastleWindowBase)
  private
    LastFpsOutputTick: DWORD;
    FFpsShowOnCaption: boolean;
    FSwapFullScreen_Key: TKey;
    FClose_CharKey: char;
    FFpsCaptionUpdateInterval: TMilisecTime;
  public
    { Show current frames per second on window caption.
      You can modify this property only @italic(before calling @link(Open).) }
    property FpsShowOnCaption: boolean
      read FFpsShowOnCaption write FFpsShowOnCaption default false;

    { Key to use to switch between FullScreen and not FullScreen.
      Set to K_None (default) to disable this functionality.
      Suggested value to enable this functionality is K_F11, this is consistent
      will fullscreen key in other programs.
      You can freely modify it at any time, even after calling @link(Open).

      The fullscreen is switched by closing it, changing @link(FullScreen)
      property and opening it again. So be sure to have good OnOpen / OnClose
      implementations: you have to be able to recreate in OnOpen everything
      that was released in OnClose. }
    property SwapFullScreen_Key: TKey
      read FSwapFullScreen_Key write FSwapFullScreen_Key default K_None;

    { Key to use to close the window.
      Set to #0 (default) to disable this functionality.
      Suggested value to enable this functionality is CharEscape.
      You can freely modify it at any time, even after calling @link(Open). }
    property Close_CharKey: char
      read FClose_CharKey write FClose_CharKey default #0;

    { The amount of time (in miliseconds) between updating Caption
      with current FPS value. Used when FpsShowOnCaption.

      Note that updating Caption of the window too often @italic(may) cause
      a significant FPS dropdown, in other words: don't set this to too small value.
      I once used here value 200. It's 5 times per second,
      this didn't seem too often, until once I checked my program
      with this turned off and found that my program runs now
      much faster (you can see that looking at FpsRealTime
      (FpsFrameTime does not change)).

      That's why I use here quite big value by default,
      DefaultFpsCaptionUpdateInterval.

      If you really want to show FPS counts updated more constantly,
      you should display them each frame as a text in OpenGL
      (like I do in view3dscene). }
    property FpsCaptionUpdateInterval: TMilisecTime
      read FFpsCaptionUpdateInterval write FFpsCaptionUpdateInterval
      default DefaultFpsCaptionUpdateInterval;

    procedure EventPress(const Event: TInputPressRelease); override;
    procedure EventUpdate; override;
    function AllowSuspendForInput: boolean; override;

    procedure SetDemoOptions(ASwapFullScreen_Key: TKey;
      AClose_CharKey: char;
      AFpsShowOnCaption: boolean);

    constructor Create(AOwner: TComponent); override;
  end;

  { OpenGL window keeping a @link(Controls) list. This allows you to
    trivially add to the window any TUIControl descendants.

    We pass our inputs (mouse / key events) to the top-most
    (that is, first on the @link(Controls) list) control under the current mouse position
    (we check control's PositionInside method for this).
    As long as the event is not handled,
    we look for next controls under the mouse position.
    Only if no control handled the event, we pass it to the inherited
    EventXxx method, which calls normal window callbacks like OnPress.

    We also call other methods on every control,
    like TUIControl.Update, TUIControl.Draw2D, TUIControl.WindowResize.

    We use OnVisibleChange event of our controls to make
    PostRedisplay when something visible changed. If you want to use
    OnVisibleChange for other purposes, you can reassign OnVisibleChange
    yourself. This window will only change OnVisibleChange from @nil
    to it's own internal callback (when adding a control),
    and from it's own internal callback to @nil (when removing a control).
    This means that if you assign OnVisibleChange callback to your own
    method --- window will not touch it anymore.

    TCamera descendants can be treated like any other TUIControl,
    that is you can add them directly to the @link(Controls) list.
    Note that usually, when using TCastleSceneManager, you should only
    assign camera to TCastleSceneManager.Camera, so most programs
    @italic(should not) add their TCamera intances directly to the
    Controls list. }
  TCastleWindowCustom = class(TCastleWindowDemo, IUIContainer)
  private
    FControls: TUIControlList;
    FUseControls: boolean;
    FOnDrawStyle: TUIControlDrawStyle;
    FFocus: TUIControl;
    FCaptureInput: TUIControl;
    FTooltipDelay: TMilisecTime;
    FTooltipDistance: Cardinal;
    FTooltipVisible: boolean;
    FTooltipX, FTooltipY: Integer;
    LastPositionForTooltip: boolean;
    LastPositionForTooltipX, LastPositionForTooltipY: Integer;
    LastPositionForTooltipTime: TTimerResult;
    Mouse3d: T3DConnexionDevice;
    Mouse3dPollTimer: Single;
    procedure ControlsVisibleChange(Sender: TObject);
    procedure SetUseControls(const Value: boolean);
    procedure UpdateFocusAndMouseCursor;
    function GetTooltipX: Integer;
    function GetTooltipY: Integer;
    { Called when the control C is destroyed or just removed from Controls list. }
    procedure DetachNotification(const C: TUIControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Enable @link(Controls) list processing.

      @italic(Messing with this is very dangerous), that's why it's
      visibility is only protected (although could be even pubilshed, technically).
      This makes all controls miss all their events, including some critical
      notification events like TUIControl.GLContextOpen, TUIControl.GLContextClose,
      TUIControl.ContainerResize.

      You can reliably only turn this off temporarily, when you know that
      no events (or at least no meaningful events, like resize or control
      add/remove) will reach the window during this time. }
    property UseControls: boolean
      read FUseControls write SetUseControls default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Controls listening for user input (keyboard / mouse) to this window.

      Usually you explicitly add / delete controls to this list.
      Also, freeing the control that is on this list
      automatically removes it from this list (using the TComponent.Notification
      mechanism).

      Controls on the list should be specified in front-to-back order.
      That is, controls at the beginning of this list are first to catch
      some events, and are rendered as the last ones (to cover controls
      beneath them). }
    property Controls: TUIControlList read FControls;

    { Returns the control that should receive input events first,
      or @nil if none. More precisely, this is the first on Controls
      list that is enabled and under the mouse cursor.
      @nil is returned when there's no enabled control under the mouse cursor,
      or when UseControls = @false. }
    property Focus: TUIControl read FFocus;

    { How OnDraw callback fits within various Draw methods of our
      @link(Controls).

      @unorderedList(
        @item(dsNone means that OnDraw is called at the end,
          after all our @link(Controls) are drawn.

          OpenGL projection matrix is not modified (so projection
          is whatever you set yourself, by EventResize, OnResize,
          or whatever TCastleSceneManager set for you).

          Note that the interpretation of dsNone is different than for
          TUIControl.DrawStyle: for TUIControl.DrawStyle, dsNone
          means "do not draw". For OnDrawStyle property,
          dsNone means "draw at the end without any tricks".

          This is suitable if you want to draw something over other
          controls, and you want to set projection yourself (or use
          the current projection, whatever it is).)

        @item(ds2D means that OnDraw is also called at the end,
          after all our @link(Controls) are drawn. But this time
          we're called within 2D orthographic projection,
          the same as set for TUIControl.DrawStyle = ds2D.

          This is suitable if you want to draw 2D contents,
          and our simple 2D orthographic projection suits you.)

        @item(ds3D means that OnDraw is called after all other
          @link(Controls) with ds3D draw style, but before any 2D
          controls.

          OpenGL projection matrix is not modified (so projection
          is whatever you set yourself, by EventResize, OnResize,
          or whatever TCastleSceneManager set for you).

          This is suitable if you want to draw something 3D,
          that may be later covered by 2D controls.)
      )
    }
    property OnDrawStyle: TUIControlDrawStyle
      read FOnDrawStyle write FOnDrawStyle default dsNone;

    property TooltipDelay: TMilisecTime read FTooltipDelay write FTooltipDelay
      default DefaultTooltipDelay;
    property TooltipDistance: Cardinal read FTooltipDistance write FTooltipDistance
      default DefaultTooltipDistance;

    { When the tooltip should be shown (mouse hovers over a control
      with a tooltip) then the TooltipVisible is set to @true,
      and TooltipX, TooltipY indicate left-bottom suggested position
      of the tooltip.

      The tooltip is only detected when TUIControl.TooltipStyle <> dsNone.
      See TUIControl.TooltipStyle and TUIControl.DrawTooltip.
      For simple purposes just set TUIControlFont.Tooltip to something
      non-empty.
      @groupBegin }
    property TooltipVisible: boolean read FTooltipVisible;
    property TooltipX: Integer read FTooltipX;
    property TooltipY: Integer read FTooltipY;
    { @groupEnd }

    procedure EventOpen; override;
    procedure EventClose; override;
    procedure EventPress(const Event: TInputPressRelease); override;
    procedure EventRelease(const Event: TInputPressRelease); override;
    procedure EventUpdate; override;
    procedure EventMouseMove(NewX, NewY: Integer); override;
    function AllowSuspendForInput: boolean; override;
    procedure EventBeforeDraw; override;
    procedure EventDraw; override;
    procedure EventResize; override;
  end;

  { Window with an OpenGL context, most comfortable to render 3D worlds
    with 2D controls above. Add your 3D stuff to the scene manager
    available in @link(SceneManager) property. Add your 2D stuff
    to the @link(TCastleWindowCustom.Controls) property (from ancestor TCastleWindowCustom).

    You can directly access the SceneManager and configure it however you like.

    You have comfortable @link(Load) method that simply loads a single 3D model
    to your world.

    If you're looking for analogous Lazarus component
    (that does basically the same, but can be placed on a Lazarus form)
    see TCastleControl component. }
  TCastleWindow = class(TCastleWindowCustom)
  private
    FSceneManager: TGameSceneManager;

    function GetShadowVolumes: boolean;
    function GetShadowVolumesDraw: boolean;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesDraw(const Value: boolean);
    function GetNavigationType: TNavigationType;
    procedure SetNavigationType(const Value: TNavigationType);
  protected
    procedure NavigationInfoChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    { Load a single 3D model to your world
      (removing other models, and resetting the camera).

      This is nice for simple 3D model browsers, but usually for games you
      don't want to use this method --- it's more flexible to create TCastleScene
      yourself, and add it to scene manager yourself, see engine examples like
      scene_manager_basic.lpr. }
    procedure Load(const SceneURL: string);
    procedure Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);

    function MainScene: TCastleScene;
    property SceneManager: TGameSceneManager read FSceneManager;

    { See TCastleAbstractViewport.ShadowVolumes. }
    property ShadowVolumes: boolean
      read GetShadowVolumes write SetShadowVolumes
      default TCastleAbstractViewport.DefaultShadowVolumes;

    { See TCastleAbstractViewport.ShadowVolumesDraw. }
    property ShadowVolumesDraw: boolean
      read GetShadowVolumesDraw write SetShadowVolumesDraw default false;

    { Navigation type of the main camera associated with the default SceneManager.
      Note that this may not be the only camera used for rendering,
      it may not even be used at all (you can do all rendering using
      @link(TCastleAbstractViewport)s.
      So use this property only if you use only a single default viewport. }
    property NavigationType: TNavigationType
      read GetNavigationType write SetNavigationType;
  end;

  TWindowList = class(specialize TFPGObjectList<TCastleWindowBase>)
  private
    { Call wszystkie OnUpdate / OnTimer for all windows on this list.
      Using Application.OpenWindows.DoUpdate / DoTimer  is a simplest
      way for CastleWindow backend to handle these events.
      @groupBegin }
    procedure DoUpdate;
    procedure DoTimer;
    { @groupEnd }
  public
    { Simply calls PostRedisplay on all items. }
    procedure PostRedisplay;
  end;

  { Application, managing all open TCastleWindowBase (OpenGL windows).
    This tracks all open instances of TCastleWindowBase
    and implements message loop. It also handles some global tasks
    like managing the screen (changing current screen resolution and/or
    bit depth etc.)

    The only instance of this class should be in @link(Application) variable.
    Don't create any other instances of class TCastleApplication, there's no
    point in doing that. }
  TCastleApplication = class(TComponent)

  { Include CastleWindow-backend-specific parts of
    TCastleApplication class. Rules and comments that apply here are
    the same as in analogous place at TCastleWindowBase class,
    when read_window_interface is defined. }

  {$define read_application_interface}
  {$I castlewindow_backend.inc}
  {$undef read_application_interface}

  private
    FOnInitialize: TProcedure;
    Initialized: boolean;
    FOnUpdate :TUpdateFunc;
    FOnTimer :TProcedure;
    FTimerMilisec :Cardinal;
    FVideoColorBits: integer;
    FVideoFrequency: Cardinal;
    { Current window with OpenGL context active.
      Update in TCastleWindowBase.MakeCurrent, also TCastleWindowBase.Close. }
    Current: TCastleWindowBase;
    LastLimitFPSTime: TTimerResult;
    FLimitFPS: Single;
    FMainWindow: TCastleWindowBase;

    FOpenWindows: TWindowList;
    function GetOpenWindows(Index: integer): TCastleWindowBase;

    { Add new item to OpenWindows.
      Windows must not be already on OpenWindows list. }
    procedure OpenWindowsAdd(Window: TCastleWindowBase);

    { Delete window from OpenWindows.

      Given Window doesn't have to be on the OpenWindows list. If it is not, this
      method is NOOP. This is useful when this is called from TCastleWindowBase.Close
      because TCastleWindowBase.Close should work even for partially constructed
      Windows.

      If Window was present on OpenWindows and after removing Window
      OpenWindowsCount = 0 and QuitWhenLastWindowClosed then it calls Quit. }
    procedure OpenWindowsRemove(Window: TCastleWindowBase; QuitWhenLastWindowClosed: boolean);

    { Find window on the OpenWindows list. Returns index, or -1 if not found. }
    function FindWindow(Window: TCastleWindowBase): integer;

    procedure CreateBackend;
    procedure DestroyBackend;

    { The CastleWindow-backend specific part of Quit method implementation.
      In non-backend-specific part of Quit we already closed all windows,
      so this will be called only when OpenWindowsCount = 0.
      So the only things you have to do here is:
      - make ProcessMessage to return false
      - terminate Run method, if it works (if Run is implemented using
        "while ProcessMessage do ;" then the first condition is all that is
        really needed)

        Note: it is NOT guaranteed that we are inside Run method
        when calling this function, i.e. it may be the case that noone ever
        called Application.Run (e.g. in @code(kambi_lines) game, where everything is done
        using while ProcessMessages do ...), but still it must be valid to call
        Quit and QuitWhenNoOpenWindows in such situation.
        Also it must be valid to call Quit and QuitWhenNoOpenWindows more
        then once. }
    procedure QuitWhenNoOpenWindows;

    { This simply checks Assigned(FOnUpdate) and only then calls FOnUpdate.
      ALWAYS use this method instead of directly calling FOnUpdate. }
    procedure DoSelfUpdate;

    { Same as DoSelfUpdate, but here with FOnTimer. }
    procedure DoSelfTimer;

    { Something useful for some CastleWindow backends. This will implement
      (in a simple way) calling of DoSelfOpen and OpenWindows.DoTimer.

      Declare in TCastleApplication some variable like
        LastDoTimerTime: TMilisecTime
      initialized to 0. Then just call very often (probably at the same time
      you're calling DoSelfUpdate)
        MaybeDoTimer(LastDoTimerTime);
      This will take care of calling DoSelfTimer and OpenWindows.DoTimer
      at the appropriate times. It will use and update LastDoTimerTime,
      you shouldn't read or write LastDoTimerTime yourself. }
    procedure MaybeDoTimer(var ALastDoTimerTime: TMilisecTime);

    { Just like TCastleWindowBase.AllowSuspendForInput, except this is for
      the whole Application. Returns @true only if all open
      windows allow it, and we do not have OnUpdate and OnTimer. }
    function AllowSuspendForInput: boolean;

    procedure DoLimitFPS;
    procedure SetMainWindow(const Value: TCastleWindowBase);
  public
    { If VideoResize, then next VideoChange call will
      try to resize the screen to given VideoResizeWidth /
      VideoResizeHeight. Otherwise, next TryVideoChange and VideoChange will
      use default screen size.
      @groupBegin }
    VideoResize : boolean;
    VideoResizeWidth,
    VideoResizeheight : integer;
    { @groupEnd }

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Color bits per pixel that will be set by next VideoChange call,
      and that are tried to be used at TCastleWindowBase.Open.
      Zero means that system default is used. }
    property VideoColorBits: integer read FVideoColorBits write FVideoColorBits default 0;

    { Video frequency to set in next VideoChange call.
      Leave as 0 to use system default. }
    property VideoFrequency: Cardinal read FVideoFrequency write FVideoFrequency default 0;

    { Describe the changes recorded in variables VideoXxx,
      used by VideoChange and TryVideoChange.
      This is a multiline string, each line is indented by 2 spaces,
      always ends with CastleUtils.NL. }
    function VideoSettingsDescribe: string;

    { Change the screen size, color bits and such, following the directions
      you set in VideoColorBits, VideoResize,
      VideoResizeWidth / VideoResizeHeight, and VideoFrequency variables.
      Returns @true if success. }
    function TryVideoChange: boolean;

    { Change the screen size, color bits and such, following the directions
      you set in VideoColorBits, VideoResize,
      VideoResizeWidth / VideoResizeHeight, and VideoFrequency variables.
      This actually just calls TryVideoChange and checks the result.

      If not success: if OnErrorWarnUserAndContinue then we'll display
      a warning and continue. If not OnErrorWarnUserAndContinue then
      we'll raise an Exception.

      @raises(Exception If video mode change failed,
        and OnErrorWarnUserAndContinue = false.) }
    procedure VideoChange(OnErrorWarnUserAndContinue: boolean);

    { Return default screen video mode.
      If you never called TryVideoChange (with success), then this does nothing.
      This is automatically called in Application.Destroy,
      so at finalization of this unit. This way your game nicely restores
      screen resolution for user. }
    procedure VideoReset;

    function ScreenHeight: integer;
    function ScreenWidth: integer;

    { List of all open windows.
      @groupBegin }
    function OpenWindowsCount: integer;
    property OpenWindows[Index: integer]: TCastleWindowBase read GetOpenWindows;
    { @groupEnd }

    { The backend is initialized. Called only once, at the very beginning
      of the game.

      For standalone games, this is not called automatically.
      You can call it from the main program file, to work consistently with
      "library" targets (see below). Or you can ignore this,
      if you're not interested in Android and similar targets.
      For standalone games, you may as well just put
      your initialization code into the main program code.

      For targets like Android or iOS or browser plugin,
      where the engine has to run as a library, this callback is useful.
      In such case, we really should not do anything (even reading files)
      before an external process tells us "Ok, you're ready".
      So you should put all the game initialization in an Application.OnInitialize
      callback. It will be automatically called by CastleWindow backend
      when suitable. }
    property OnInitialize: TProcedure read FOnInitialize write FOnInitialize;

    procedure Initialize;

    { Continously occuring event.
      @seealso TCastleWindowBase.OnUpdate. }
    property OnUpdate: TUpdateFunc read FOnUpdate write FOnUpdate;

    { @deprecated Deprecated name for OnUpdate. }
    property OnIdle: TUpdateFunc read FOnUpdate write FOnUpdate; deprecated;

    { Event called approximately after each TimerMilisec miliseconds.
      The actual delay may be larger than TimerMilisec miliseconds,
      depending on how the program (and OS) is busy.

      You can of course change TimerMilisec (and OnTimer) even
      when some windows are already open.
      @groupBegin }
    property OnTimer: TProcedure read FOnTimer write FOnTimer;
    property TimerMilisec: Cardinal read FTimerMilisec write FTimerMilisec default 1000;
    { @groupEnd }

    { On targets when only one TCastleWindowBase instance makes sense
      (like Android), set this to the reference of that window. }
    property MainWindow: TCastleWindowBase read FMainWindow write SetMainWindow;

    { Process messages from the window system.
      You have to call this repeatedly to process key presses,
      mouse events, redraws and everything else.
      Messages are processed and appropriate window callbacks are called,
      like TCastleWindowBase.OnDraw,
      TCastleWindowBase.OnUpdate,
      TCastleWindowBase.OnKeyPress and many others.

      For simple programs calling the @link(Run) method is usually
      the best solution, @link(Run) just calls ProcessMessage in a loop.
      Manually using the ProcessMessage method allows you to implement
      modal dialog boxes (generally any kind of "display something
      until something happens" behavior). Make your own event loop like this:

@longCode(#
  while not SomethingHappened do
    Application.ProcessMessages(...);
#)

      Often this is used together with TGLMode, TGLModeFrozenScreen
      and similar utilities from CastleWindowModes unit.
      They allow you to temporarily replace window callbacks with new ones,
      and later restore the original ones.
      This is useful for behavior similar to modal dialog boxes.

      Returns @true if we should continue, that is
      if @link(Quit) method was not called (directly or by closing
      the last window). If you want to check it (if you
      allow the user at all to close the application during modal box or such)
      you can do:

@longCode(#
  while not SomethingHappened do
    if not Application.ProcessMessage(...) then
      Break;
#)

      Do not assume too much about message processing internals.
      For example, not all ProcessMessage calls cause redraw, even if redraw
      is requested by PostRedisplay. When we have messages to process,
      we generally don't call redraw or even OnUpdate.

      @param(WaitForMessage If @true (and some other conditions are met,
        for example we do not have to call OnUpdate continuosly)
        then we can block, waiting for an event to process.

        Set this to @true whenever you can, that is whenever your program
        only responds to user inputs (as opposed to making some operations,
        like animation or loading or ray-tracing something).
        Generally, when @code(SomethingHappened) from the example pseudo-code
        above can only be changed by user events (e.g. user has to click
        something; nothing happens if user doesn't click for 5 minutes or 5 hours).
        This allows to let OS and CPU have some rest, and spend time
        on other applications, or just sleep and conserve laptop battery power.
      )

      @param(WaitToLimitFPS If @true, then we have backup mechanism for
        limiting CPU usage. When WaitForMessage mechanism cannot be used
        (becasue WaitForMessage is @false or some other conditions disallow it),
        and user doesn't throw events at us (we don't want to sleep when user
        produces many events e.g. by mouse move),
        then we can do a small sleep to stabilize number of ProcessMessage
        calls at LimitFPS per second.

        Set this to @true whenever you can, that is whenever you don't need
        ProcessMessage to return as fast as it can. For example,
        when you're displaying some animation, then displaying LimitFPS frames
        should be enough. OTOH, if you really do something that should be done
        as fast as possible (like loading some file or ray-tracing) you probably
        have to set this to @false.
      )
    }
    function ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;

    { Processes @italic(all) pending messages.

      Contrast this with ProcessMessage method, that processes only a single
      event. Or no event at all (when no events were pending and
      AllowSuspend = @false). This means that after calling ProcessMessage
      once, you may have many messages left in the queue (especially
      mouse move together with key presses typically makes a lot of
      events). So it's not good to use if you want to react timely to
      some user requests, e.g. when you do something time-consuming
      and allow user to break the task with Escape key.

      ProcessAllMessages is like
      calling in a loop something like ProcessMessage(false), ends when
      ProcessMessage didn't process any message (it's internally returned
      by ProcessMessage2) or when quit was called (or last window closed).

      So ProcessAllMessages makes sure we have processed all pending events,
      thus we are up-to-date with window system requests. }
    function ProcessAllMessages: boolean;

    { Close all open windows, make ProcessMessage return @false,
      finish the @link(Run) method (if working), and thus finish the
      application work. }
    procedure Quit;

    { Run the program using TCastleWindowBase, by doing the event loop.
      Think of it as just a shortcut for "while ProcessMessage do ;".

      Note that this does nothing if OpenWindowsCount = 0, that is there
      are no open windows. Besides the obvious reason (you didn't call
      TCastleWindowBase.Open on any window...) this may also happen if you called
      Close (or Application.Quit) from your window OnOpen / OnResize callback.
      In such case no event would probably reach
      our program, and user would have no chance to quit, so Run just refuses
      to work and exits immediately without any error. }
    procedure Run;

    function BackendName: string;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Limit the number of (real) frames per second, to not hog the CPU.
      Set to zero to not limit.

      To be more precise, this limits the number of TCastleApplication.ProcessMessage
      calls per second, in situations when we do not have to process any user input.
      So we limit not only rendering (TCastleWindowBase.OnDraw)
      but also other animation processing (TCastleWindowBase.OnUpdate) calls per second.
      See TCastleApplication.ProcessMessage.

      In case of CastleWindow backends when we have to fight with event clogging
      (right now only LCL backend, used by default only on Mac OS X)
      this is also the "desired number of FPS": we make sure that even
      when application is clogged with events (like when dragging with mouse),
      we call update (TCastleWindowBase.OnUpdate) and (if necessary)
      draw (TCastleWindowBase.OnDraw and related) at least as often.
      When LimitFPS is used for this purpose ("desired number of FPS"),
      it is also capped (by MaxDesiredFPS = 100.0). }
    property LimitFPS: Single read FLimitFPS write FLimitFPS default DefaultLimitFPS;
  end;

  { @deprecated Deprecated name for TCastleApplication. }
  TGLApplication = TCastleApplication deprecated;

  { Clipboard for cut / copy / paste of text.
    You usually use this by the single global instance @link(Clipboard).
    Interface is mostly compatible with LCL clipboard. }
  TCastleClipboard = class
  private
    function GetAsText: string;
    procedure SetAsText(const Value: string);
  public
    property AsText: string read GetAsText write SetAsText;
  end;

{ Single global instance of TCastleApplication.
  Automatically created / destroyed by CastleWindow unit. }
function Application: TCastleApplication;

{ Single global instance of TCastleClipboard.
  Automatically created / destroyed by CastleWindow unit. }
function Clipboard: TCastleClipboard;

{ A simple TCastleWindowBase.OnResize callback implementation, that sets 2D projection.
  You can use it like @code(Window.OnResize := Resize2D;) or just by calling
  it directly from your OnResize callback.

  It does
@longCode(#
  glViewport(Window.Rect);
  OrthoProjection(0, Window.Width, 0, Window.Height);
#) }
procedure Resize2D(Window: TCastleWindowBase);

{ Describe given key. Key is given as combination of character code (may be #0)
  and Key code (may be K_None), and additional required @code(Modifiers)
  (although some modifiers may be already implied by CharKey).
  See @link(TMenuItem.Key) and @link(TMenuItem.CharKey) and @link(TMenuItem.Modifiers).

  Only when both CharKey = #0 and Key = K_None
  then this combination doesn't describe any key, and we return @false.
  Otherwise we return @true and set S. }
function KeyString(const CharKey: char; const Key: TKey; const Modifiers: TModifierKeys;
  out S: string): boolean;

{$undef read_interface}

{$define read_interface_2}
{$i castlewindowmenu.inc}
{$undef read_interface_2}

implementation

uses CastleParameters, CastleLog, CastleGLVersion, CastleURIUtils, CastleWarnings,
  {$define read_implementation_uses}
  {$I castlewindow_backend.inc}
  {$undef read_implementation_uses}
  X3DLoad;

{$define read_implementation}

{$I castlewindowmenu.inc}
{$I castlewindow_backend.inc}

{ TCastleWindowBase ---------------------------------------------------------- }

constructor TCastleWindowBase.Create(AOwner: TComponent);
begin
  inherited;
  FClosed := true;
  FWidth  := WindowDefaultSize;
  FHeight := WindowDefaultSize;
  FLeft  := WindowPositionCenter;
  FTop   := WindowPositionCenter;
  FDoubleBuffer := true;
  FCaption[cpPublic] := ApplicationName;
  FResizeAllowed := raAllowed;
  minWidth := 100;  maxWidth := 4000;
  minHeight := 100; maxHeight := 4000;
  DepthBits := DefaultDepthBits;
  FCursor := mcDefault;
  FMultiSampling := 1;
  FVisible := true;
  OwnsMainMenu := true;
  FDpi := DefaultDpi;
  FPressed := TKeysPressed.Create;
  FFps := TFramesPerSecond.Create;
  FMainMenuVisible := true;

  CreateBackend;
end;

destructor TCastleWindowBase.Destroy;
begin
  Close; { <- This will be ignored if already Closed }

  if OwnsMainMenu then
    FreeAndNil(FMainMenu) else
  if FMainMenu <> nil then
  begin
    FMainMenu.ParentWindow := nil; { clear Self from FMainMenu.ParentWindow }
    FMainMenu := nil;
  end;

  FreeAndNil(FFps);
  FreeAndNil(FPressed);
  inherited;
end;

procedure TCastleWindowBase.OpenCore;
begin
  if not FClosed then Exit;

  try
    { Adjust Left/Top/Width/Height as needed.
      Note: calculations below try to correct window geometry but they
      can fail to foresee some things. In particular, they do not take
      into account a potential menu bar that may be visible when MainMenu <> nil.
      E.g., when MainMenu <> nil and implementation supports MainMenu as
      menu bar (GTK and WINAPI implementations) and FullScreen then
      the actual OpenGL window size will NOT match ScreenWidth/Height,
      it will be slightly smaller (menu bar takes some space).
    }
    if Width  = WindowDefaultSize then FWidth  := Application.ScreenWidth  * 4 div 5;
    if Height = WindowDefaultSize then FHeight := Application.ScreenHeight * 4 div 5;
    Clamp(fwidth, minWidth, maxWidth);
    Clamp(fheight, minHeight, maxHeight);
    if left = WindowPositionCenter then fleft := (Application.ScreenWidth-width) div 2;
    if top  = WindowPositionCenter then ftop := (Application.ScreenHeight-height) div 2;

    { reset some window state variables }
    Pressed.Clear;
    fmousePressed := [];
    EventOpenCalled := false;

    { Set Closed to false. Before OpenBackend and EventOpen + EventResize,
      since they can raise exceptions, and in reaction to it we will cleanly
      Close the window. }
    FClosed := false;

    { Call OpenBackend. Note that OpenBackend can call DoResize,
      it will still be correctly understood. }
    OpenBackend;

    { Do MakeCurrent before glViewport and EventOpen. }
    MakeCurrent;

    LoadAllExtensions;

    if Log then
      WritelnLogMultiline('OpenGL context initialization', GLInformationString);

    if GLVersion.BuggyDepth32 and
      (glGetInteger(GL_DEPTH_BITS) >= 32) and
      (StencilBits = 0) then
    begin
      if Log then
        WritelnLog('OpenGL context initialization',
          'Got >= 32-bit depth buffer, unfortunately it is known to be buggy on this OpenGL implementation. We will try to force 24-bit depth buffer by forcing stencil buffer.');
      { Close the window, increase StencilBits to try to force 24-bit
        depth buffer, and call ourselves again.
        Checking "StencilBits = 0" above prevents from getting into
        an infinite loop here. }
      Close({ QuitWhenLastWindowClosed } false);
      StencilBits := 8;
      OpenCore;
      Exit;
    end;

    { synchronize glViewport with our Width/Height (note that, because
      of ResizeAllowed and MinWidth etc. that can be different than actual window
      sizes). }
    glViewport(Rect);

    {$ifndef OpenGLES}
    if ( (AntiAliasing = aa2SamplesNicer) or
         (AntiAliasing = aa4SamplesNicer) ) and
       GLFeatures.NV_multisample_filter_hint then
      glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
    {$endif}

    { call first EventOpen and EventResize. Zwroc uwage ze te DoResize i DoOpen
      MUSZA byc wykonane na samym koncu procedury Open - jak juz wszystko inne
      zostalo wykonane. Wszystko po to ze juz w pierwszym OnOpen lub OnResize
      moze zostac wywolane Application.ProcessMessages np. w wyniku wywolania w OnOpen
      CastleMessages.MessageOk. }
    EventOpenCalled := true;
    EventOpen;

    { Check Closed here, in case OnOpen closed the window
      (by calling Application.Quit (that calls Close on all windows) or direct Close
      on this window). Note that Close calls
      CloseBackend and generally has *immediate* effect --- that's why
      doing anything more with window now (like MakeCurrent) would be wrong. }
    if Closed then Exit;

    DoResize(FWidth, FHeight, true);

    { Check Closed here, in case OnResize closed the window. }
    if Closed then Exit;

    { to be SURE that current window's gl context is active,
      even if someone in EventOpen changed current gl context }
    MakeCurrent;
  except
    Close; raise;
  end;
end;

procedure TCastleWindowBase.Open(const Retry: TGLContextRetryOpenFunc);
begin
  try
    OpenCore;
  except
    on E: EGLContextNotPossible do
    begin
      if Retry(Self) then
        Open(Retry) { recursive call } else
        raise;
    end;
  end;
end;

{ Try to lower anti-aliasing (multi-sampling) and shadows (stencil buffer)
  requirements and initialize worse GL context. }
function DefaultRetryOpen(Window: TCastleWindowBase): boolean;
begin
  if Window.AntiAliasing <> aaNone then
  begin
    Window.AntiAliasing := aaNone;
    if Log then WritelnLog('OpenGL context', 'OpenGL context cannot be initialized. Multi-sampling (anti-aliasing) turned off, trying to initialize once again.');
    Result := true;
  end else
  if Window.StencilBits > 0 then
  begin
    Window.StencilBits := 0;
    if Log then WritelnLog('OpenGL context', 'OpenGL context cannot be initialized. Stencil buffer (shadow volumes) turned off, trying to initialize once again.');
    Result := true;
  end else
    Result := false;
end;

procedure TCastleWindowBase.Open;
begin
  Open(@DefaultRetryOpen);
end;

procedure TCastleWindowBase.CloseError(const error: string);
begin
  if CloseErrors <> '' then
    CloseErrors := CloseErrors + nl + Error else
    CloseErrors := Error
end;

procedure TCastleWindowBase.Close(QuitWhenLastWindowClosed: boolean);
begin
  if FClosed then Exit;

  try
    if EventOpenCalled then
    begin
      MakeCurrent;
      EventClose;
    end;
  finally
    CloseErrors := '';
    CloseBackend;

    FClosed := true;

    Application.Current := nil;

    { Note: it is important here that OpenWindowsRemove will not raise any error
      if Self is not on OpenWindows list. This is useful if the window
      was partially constructed.

      E.g. when StencilBits was too high and OpenBackend
      method raised an exception EGLContextNotPossible. Then this method, Close,
      is called, but Self is not on OpenWindows list. And this fact should not be
      reported as an error -- error is EGLContextNotPossible ! }
    Application.OpenWindowsRemove(Self, QuitWhenLastWindowClosed);

    { Only raise at the end. This way we always go fully through CloseBackend,
      regardless of errors.}
    if CloseErrors <> '' then
      raise Exception.Create('Error while trying to close window ' + CloseErrors);
  end;
end;

procedure TCastleWindowBase.MakeCurrent;
begin
  { Calling BackendMakeCurrent is done very often (before every event,
    so a couple of times for every frame). And usually it's useless,
    as most games have only 1 open window. }
  if Application.Current <> Self then
  begin
    BackendMakeCurrent;
    Application.Current := Self;
  end;
end;

procedure TCastleWindowBase.SetAutoRedisplay(value: boolean);
begin
  fAutoRedisplay := value;
  if value and (not Closed) then PostRedisplay;
end;

procedure TCastleWindowBase.ReleaseAllKeysAndMouse;
var
  k: TKey;
  mb: CastleKeysMouse.TMouseButton;
  {$ifdef CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  mk: TModifierKey;
  b: boolean;
  {$endif}
begin
  {$ifdef CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  { When CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN, I *HAVE* to use below
    SetPrivateModifiersDown. It would be an error to do DoKeyUp(K_Ctrl)
    directly when CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN, instead we have to
    use SetPrivateModifiersDown(mkCtrl, ...).
    This is the only way to make values in PrivateModifiersDown[]
    and Pressed[] arrays consistent. }
  for mk := Low(mk) to High(mk) do
    for b := Low(b) to High(b) do
      SetPrivateModifiersDown(mk, b, false);
  {$endif CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}

  { Since we do DoKeyUp, this should also take care of Characters. }

  for k := Low(k) to High(k) do
    if Pressed[k] then DoKeyUp(k);

  for mb := Low(mb) to High(mb) do if mb in MousePressed then
    DoMouseUp(MouseX, MouseY, mb);
end;

function TCastleWindowBase.GetColorBits: Cardinal;
begin
  Result := RedBits + GreenBits + BlueBits;
end;

procedure TCastleWindowBase.SetColorBits(const Value: Cardinal);
begin
  RedBits := Value div 3;
  BlueBits := Value div 3;
  GreenBits := Value - RedBits - BlueBits;
  Assert(Value = ColorBits);
end;

procedure TCastleWindowBase.SetAntiAliasing(const Value: TAntiAliasing);
const
  AntiAliasingToMultiSampling: array [TAntiAliasing] of Cardinal =
  ( 1,
    2, 2,
    4, 4,
    8, 8,
    16, 16 );
  { Note: when new GPUs appear that support more samples,
    - extend the TAntiAliasing type and related arrays (just recompile to see
      where you need to change),
    - extend the src/x3d/opengl/glsl/screen_effect_library.glsl
    - extend the check for samples in ScreenEffectLibrary in CastleRenderer
      (this must be synchronized with screen_effect_library.glsl implementation).
  }
begin
  FAntiAliasing := Value;
  MultiSampling := AntiAliasingToMultiSampling[Value];
end;

{ wszystkie zdarzenia TCastleWindowBase - opakowujace je procedury DoXxx ktore
  robia wszystkie rzeczy niezalezne od implementacji dla danego zdarzenia
  (m.in. wywoluja EventXxx ktore m.in. wywoluje OnXxx jesli jest assigned).
  Implementacje CastleWindow powinny wywolywac te funkcje, NIE wywolywac
  bezposrednio EventXxx ani tym bardziej OnXxx !
  ------------------------------------------------------------------------------------ }

procedure TCastleWindowBase.DoResize(AWidth, AHeight: integer; FirstResizeAfterOpen: boolean);
begin
  { zabezpiecz sie przed
    1) backends where we can't express ResizeAllowed <> raAllowed
    2) Windowsem, ktory moze zresizowac nasze okno np. gdy sie nie miescimy na ekranie
    3) XWindow-Managerem ktory zawsze moze nas zresizowac, mimo ze prosimy go
       zeby tego nie robil.
    wiec pod wszystkimi trzema implementacjami musimy sprawdzic warunek ze
      albo ResizeAllowed = raAllowed albo naprawde fwidth = w itd.
    Sprawdzamy tez czy w i h sa w odpowiednim zakresie minXxx .. maxXxx.
      Oczywiscie implementacje powinny starac sie zeby nic spoza tego zakresu do nas
      nie dotarlo, ale nigdy nie ma pewnosci. Zwracam uwage, ze wymagamy aby zawsze
      minWidth > 0 i minHeight > 0 wiec jednoczesnie ponizej gwarantujemy sobie ze nie
      zachodzi sytuacja w = 0 lub h = 0.

    Apropos wywolywania DoResize(.., false) z OpenBackend:
    zabezpieczamy sie przed tym zawsze. Ale mozna tu odnotowac ze z pewnoscia
    OpenBackend moze wywolywac DoResize(.., false) w przypadku
    implementacji WINAPI i GTK.
  }

  { update FWidth, FHeight.
    Below we are explicitly forcing assertions about ResizeAllowed:
    when ResizeAllowed
      = raNotAllowed: FWidth and FHeight cannot change
      = raOnlyAtOpen: FWidth and FHeight can change only at first EventResize
        (with FirstResizeAfterOpen = true), at least from the point of view of outside.
        Internally, every call to DoResize upto and including FirstResizeAfterOpen call
        changes FWidth and FHeight. This allows to accumulate e.g. size changes
        caused by FullScreen=true, in case OpenBackend does them by explicitly
        calling DoResize, like unix/castlewindow_xlib.inc .
      = raAllowed: FWidth and FHeight can change freely
  }
  if (ResizeAllowed = raAllowed) or
     ((ResizeAllowed = raOnlyAtOpen) and
      (FirstResizeAfterOpen or not EventOpenCalled)) then
  begin
    FWidth := Clamped(AWidth,  MinWidth,  MaxWidth);
    FHeight := Clamped(AHeight, MinHeight, MaxHeight);
  end;

  { do not call EventResize before EventOpen (this check is needed
    because OpenBackend is allowed to call DoResize) }
  if not EventOpenCalled then Exit;

  { jezeli ResizeAllowed <> raAllowed to nie powinnismy wywolywac EventResize
    poza pierwszym razem (gdy FirstResizeAfterOpen).
    Kazdy nastepny raz i tak bylby pozbawiony
    znaczenia, bo przeciez Width i Height i tak nie ulegly zmianie. }
  if (not FirstResizeAfterOpen) and (ResizeAllowed <> raAllowed) then Exit;

  MakeCurrent;
  EventResize;
end;

procedure TCastleWindowBase.DoCloseQuery;
begin
  MakeCurrent;
  if EventCloseQuery then Close;
end;

procedure TCastleWindowBase.DoDraw;
begin
  MakeCurrent;

  EventBeforeDraw;
  if Closed then Exit; { check, in case window got closed in the event }

  Fps._RenderBegin;
  try
    EventDraw;
    if Closed then Exit; { check, in case window got closed in the event }

    if GLVersion.BuggySwapNonStandardViewport then
      glViewport(Rect);

    if DoubleBuffer then SwapBuffers else glFlush;
    if AutoRedisplay then PostRedisplay;
  finally Fps._RenderEnd end;

  {$ifdef CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW} CheckGLErrors('End of TCastleWindowBase.DoDraw'); {$endif}
end;

procedure TCastleWindowBase.DoKeyDown(Key: TKey; CharKey: char);

  function SeekMatchingMenuItem: TMenuItem;

    function SeekMe(Entry: TMenuEntry): TMenuItem;
    var
      i: Integer;
    begin
      Result := nil;
      if Entry is TMenu then
      begin
        for i := 0 to TMenu(Entry).Count - 1 do
        begin
          Result := SeekMe(TMenu(Entry).Entries[i]);
          if Result <> nil then Break;
        end;
      end else
      if (Entry is TMenuItem) and
         TMenuItem(Entry).KeyMatches(Key, CharKey, Pressed.Modifiers) then
        Result := TMenuItem(Entry);
    end;

  begin
    if MainMenu <> nil then
      Result := SeekMe(MainMenu) else
      Result := nil;
  end;

var
  MatchingMI: TMenuItem;
begin
  Pressed.KeyDown(Key, CharKey);

  MatchingMI := SeekMatchingMenuItem;
  if (MainMenu <> nil) and
     MainMenu.Enabled and
     (MatchingMI <> nil) then
  begin
    if (not MainMenuVisible) or RedirectKeyDownToMenuClick then
      DoMenuClick(MatchingMI);
  end else
  begin
    MakeCurrent;
    EventPress(InputKey(Key, CharKey));
  end;
end;

procedure TCastleWindowBase.DoKeyUp(key: TKey);
var
  C: char;
begin
  if Pressed[Key] then
  begin
    { K_None key is never pressed, DoKeyDown guarentees this }
    Assert(Key <> K_None);
    Pressed.KeyUp(Key, C);
    MakeCurrent;
    EventRelease(InputKey(key, C));
  end;
end;

procedure TCastleWindowBase.DoMouseMove(x, y: integer);
begin
 MakeCurrent;
 EventMouseMove(x, y);
 FMouseX := x; { odswiezamy FMouseXY dopiero PO wywolaniu EventMouseMove }
 FMouseY := y;
end;

procedure TCastleWindowBase.DoMouseDown(x, y: integer; btn: CastleKeysMouse.TMouseButton);
begin
 FMouseX := x;
 FMouseY := y;
 Include(FMousePressed, btn);
 MakeCurrent;
 EventPress(InputMouseButton(btn));
end;

procedure TCastleWindowBase.DoMouseUp(x, y: integer; btn: CastleKeysMouse.TMouseButton);
begin
 FMouseX := x;
 FMouseY := y;
 Exclude(FMousePressed, btn);
 MakeCurrent;
 EventRelease(InputMouseButton(btn));
end;

procedure TCastleWindowBase.DoMouseWheel(const Scroll: Single; const Vertical: boolean);
begin
  MakeCurrent;
  EventPress(InputMouseWheel(Scroll, Vertical));
end;

procedure TCastleWindowBase.DoUpdate;
begin
  Fps._UpdateBegin;
  MakeCurrent;
  EventUpdate;
end;

procedure TCastleWindowBase.DoTimer; begin  MakeCurrent; EventTimer end;

procedure TCastleWindowBase.DoMenuClick(Item: TMenuItem);
begin
  if (MainMenu <> nil) and (not MainMenu.Enabled) then Exit;

  MakeCurrent;
  if Item.DoClick then Exit;

  { Maybe Item.DoClick changed current OpenGL context and returned false?
    We want to be safe, so we do here MakeCurrent again. }
  MakeCurrent;
  EventMenuClick(Item);
end;

procedure TCastleWindowBase.DoDropFiles(const FileNames: array of string);
begin
  MakeCurrent;
  EventDropFiles(FileNames);
end;

{ funkcje EventXxx ktore sa wirtualne i sa GWARANTOWANE ze w klasie bazowej
  wywoluja po prostu OnXxx. Te funkcje moga byc pokrywane w podklasach.
  ---------------------------------------------------------------------------- }

function TCastleWindowBase.EventCloseQuery: boolean;
const EventName = 'CloseQuery';
begin
 result := not Assigned(OnCloseQuery);
 {$I castlewindow_eventbegin.inc}
 if Assigned(OnCloseQuery) then
   OnCloseQuery(Self);
 {$I castlewindow_eventend.inc}
end;

procedure TCastleWindowBase.EventOpen;                              const EventName = 'Open';       begin {$I castlewindow_eventbegin.inc} if Assigned(OnOpen)        then begin OnOpen(Self);              end;   {$I castlewindow_eventend.inc} end;
procedure TCastleWindowBase.EventClose;                             const EventName = 'Close';      begin {$I castlewindow_eventbegin.inc} if Assigned(OnClose)       then begin OnClose(Self);             end;   {$I castlewindow_eventend.inc} end;
{$define BONUS_LOG_STRING := Format('%d, %d', [Width, Height])}
procedure TCastleWindowBase.EventResize;                            const EventName = 'Resize';     begin {$I castlewindow_eventbegin.inc} if Assigned(OnResize)      then begin OnResize(Self);            end;   {$I castlewindow_eventend.inc} end;
{$undef BONUS_LOG_STRING}
{$define BONUS_LOG_STRING := Event.Description}
procedure TCastleWindowBase.EventPress(const Event: TInputPressRelease);       const EventName = 'Press';    begin {$I castlewindow_eventbegin.inc} if Assigned(OnPress)   then begin OnPress  (Self, Event); end; {$I castlewindow_eventend.inc} end;
procedure TCastleWindowBase.EventRelease(const Event: TInputPressRelease);     const EventName = 'Release';  begin {$I castlewindow_eventbegin.inc} if Assigned(OnRelease) then begin OnRelease(Self, Event); end; {$I castlewindow_eventend.inc} end;
{$undef BONUS_LOG_STRING}
procedure TCastleWindowBase.EventMenuClick(Item: TMenuItem);                   const EventName = 'MenuClick';begin {$I castlewindow_eventbegin.inc} if Assigned(OnMenuClick) then begin OnMenuClick(Self, Item);      end; {$I castlewindow_eventend.inc} end;
procedure TCastleWindowBase.EventDropFiles(const FileNames: array of string);  const EventName = 'DropFiles';begin {$I castlewindow_eventbegin.inc} if Assigned(OnDropFiles) then begin OnDropFiles(Self, FileNames); end; {$I castlewindow_eventend.inc} end;

{ Events below happen so often, that they are logged only when
  CASTLE_WINDOW_EVENTS_LOG_ALL is defined.

  For CASTLE_WINDOW_eventbegin/end.inc to work, we do here a little trick
  with CASTLE_WINDOW_EVENTS_LOG symbol: undefine CASTLE_WINDOW_EVENTS_LOG temporarily if
  CASTLE_WINDOW_EVENTS_LOG_ALL not defined. }
{$ifndef CASTLE_WINDOW_EVENTS_LOG_ALL}
  {$ifdef CASTLE_WINDOW_EVENTS_LOG}
    {$define WAS_CASTLE_WINDOW_EVENTS_LOG}
    {$undef CASTLE_WINDOW_EVENTS_LOG}
  {$endif}
{$endif}

  {$define BONUS_LOG_STRING := Format('%d, %d', [newX, newY])}
  procedure TCastleWindowBase.EventMouseMove(newX, newY: integer);const EventName = 'MouseMove'; begin {$I castlewindow_eventbegin.inc} if Assigned(OnMouseMove) then begin OnMouseMove(Self, newX, newY); end;   {$I castlewindow_eventend.inc} end;
  {$undef BONUS_LOG_STRING}

  procedure TCastleWindowBase.EventBeforeDraw;                    const EventName = 'BeforeDraw';begin {$I castlewindow_eventbegin.inc} if Assigned(OnBeforeDraw)then begin OnBeforeDraw(Self);            end;   {$I castlewindow_eventend.inc} end;
  procedure TCastleWindowBase.EventDraw;                          const EventName = 'Draw';      begin {$I castlewindow_eventbegin.inc} if Assigned(OnDraw)      then begin OnDraw(Self);                  end;   {$I castlewindow_eventend.inc} end;
  procedure TCastleWindowBase.EventUpdate;                        const EventName = 'Update';    begin {$I castlewindow_eventbegin.inc} if Assigned(OnUpdate)    then begin OnUpdate(Self);                end;   {$I castlewindow_eventend.inc} end;
  procedure TCastleWindowBase.EventTimer;                         const EventName = 'Timer';     begin {$I castlewindow_eventbegin.inc} if Assigned(OnTimer)     then begin OnTimer(Self);                 end;   {$I castlewindow_eventend.inc} end;

{$ifndef CASTLE_WINDOW_EVENTS_LOG_ALL}
  {$ifdef WAS_CASTLE_WINDOW_EVENTS_LOG}
    {$define CASTLE_WINDOW_EVENTS_LOG}
  {$endif}
{$endif}

function TCastleWindowBase.AllowSuspendForInput: boolean;
begin
 result := not (Assigned(OnUpdate) or Assigned(OnTimer));
end;

{ Menu things ------------------------------------------------------------ }

procedure TCastleWindowBase.SetMainMenu(Value: TMenu);
begin
 if MainMenu <> Value then
 begin
  if (not Closed) and ((MainMenu <> nil) <> (Value <> nil)) then
   raise EInternalError.Create('While TCastleWindowBase is not Closed, '+
     'you can''t set MainMenu from nil to non-nil or from non-nil to nil');

  if FMainMenu <> nil then
  begin
    MenuFinalize;
    FMainMenu.ParentWindow := nil;
  end;

  FMainMenu := Value;

  if FMainMenu <> nil then
  begin
    FMainMenu.ParentWindow := Self;
    MenuInitialize;
  end;
 end;
end;

{ SaveScreen wykonane na CastleWindow (robimy najpierw FlushRedisplay)
  -------------------------------------------------------------------------- }

function TCastleWindowBase.SaveScreenBuffer: TColorBuffer;
begin
  if DoubleBuffer then
    Result := cbBack else
    Result := cbFront;
end;

procedure TCastleWindowBase.SaveScreen(const fname: string);
var
  Image: TRGBImage;
begin
  Image := SaveScreen;
  try
    SaveImage(Image, fname);
  finally FreeAndNil(Image) end;
end;

function TCastleWindowBase.SaveScreen: TRGBImage;
begin
  Result := SaveScreen(Rect);
end;

function TCastleWindowBase.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  if DoubleBuffer then
  begin
    EventBeforeDraw;
    EventDraw;
  end else
    FlushRedisplay;
  Result := SaveScreen_NoFlush(SaveRect, SaveScreenBuffer);
end;

function TCastleWindowBase.SaveScreenToGL(const ScalingPossible: boolean): TGLImage;
begin
  Result := SaveScreenToGL(Rect, ScalingPossible);
end;

function TCastleWindowBase.SaveScreenToGL(
  const SaveRect: TRectangle;
  const ScalingPossible: boolean): TGLImage;
begin
  if DoubleBuffer then
  begin
    EventBeforeDraw;
    EventDraw;
  end else
    FlushRedisplay;
  Result := SaveScreenToGL_NoFlush(SaveRect, SaveScreenBuffer, ScalingPossible);
end;

procedure TCastleWindowBase.SaveScreenDialog(ProposedURL: string);
begin
  if FileDialog('Save screen to file', ProposedURL, false, SaveImage_FileFilters) then
  try
    SaveScreen(ProposedURL);
  except
    on E: Exception do MessageOK('Unable to save screen: ' + E.Message, mtError);
  end;
end;

function TCastleWindowBase.FileDialog(const Title: string; var URL: string;
  OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean;
var
  FileName: string;
begin
  { calculate FileName from URL }
  FileName := URIToFilenameSafe(URL);
  if OpenDialog then
    FileName := ExtractFilePath(FileName);
  Result := BackendFileDialog(Title, FileName, OpenDialog, FileFilters);
  if Result then
    URL := FilenameToURISafe(FileName);
end;

function TCastleWindowBase.FileDialog(const Title: string; var URL: string;
  OpenDialog: boolean; const FileFilters: string): boolean;
var
  FFList: TFileFilterList;
begin
  FFList := TFileFilterList.Create(true);
  try
    FFList.AddFiltersFromString(FileFilters);
    Result := FileDialog(Title, URL, OpenDialog, FFList);
  finally FreeAndNil(FFList) end;
end;

function TCastleWindowBase.ColorDialog(var Color: TCastleColor): boolean;
var
  Color3: TVector3Single;
begin
  Color3 := Vector3SingleCut(Color);
  Result := ColorDialog(Color3);
  if Result then
    Color := Vector4Single(Color3, 1.0);
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Byte): boolean;
var
  ColorSingle: TVector3Single;
begin
  ColorSingle[0] := Color[0] / High(Byte);
  ColorSingle[1] := Color[1] / High(Byte);
  ColorSingle[2] := Color[2] / High(Byte);
  Result := ColorDialog(ColorSingle);
  if Result then
    Color := Vector3Byte(ColorSingle);
end;

{ OpenAndRun ----------------------------------------------------------------- }

procedure TCastleWindowBase.OpenAndRun(const ACaption: string; AOnDraw: TDrawFunc);
begin
  SetPublicCaption(ACaption);
  OnDraw := AOnDraw;
  OpenAndRun;
end;

procedure TCastleWindowBase.OpenAndRun;
begin
  Open;
  Application.Run;
end;

{ TCastleWindowBase ParseParameters -------------------------------------------------- }

type
  TOptionProcData = record
    SpecifiedOptions: TWindowParseOptions;
    Window: TCastleWindowBase;
  end;
  POptionProcData = ^TOptionProcData;

procedure GeometryOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var ProcData: POptionProcData absolute Data;

  procedure ApplyGeometryParam(const geom: string);
  var p: integer;
      parWidth, parHeight, parXoff, parYoff: integer;
      xoffPlus, yoffPlus, sizeSpecified, positionSpecified: boolean;
      { p to znak w stringu geom ktory teraz chcemy czytac.
        parWidth i parHeight sa valid tylko o ile sizeSpecified.
        parXoff, parYoff, xoffPlus, yoffPlus sa valid tylko o ile positionSpecified.
      }

    procedure ParseSize;
    { parsuje width i height }
    var startp: integer;
    begin
     sizeSpecified := true;

     {width}
     startp := p;
     while SCharIs(geom, p, ['0'..'9']) do Inc(p);
     parWidth := StrToInt(CopyPos(geom, startp, p-1));

     {height}
     if not SCharIs(geom, p, ['x','X']) then
      raise EInvalidParams.Create(
        'Invalid --geometry parameter : expected "x" between WIDTH and HEIGHT');
     Inc(p);
     startp := p;
     while SCharIs(geom, p, ['0'..'9']) do Inc(p);
     parHeight := StrToInt(CopyPos(geom, startp, p-1));
    end;

    procedure ParsePosition;
    { parsuje xoff, yoff i koniec stringa. }
    var startp: integer;
    begin
     positionSpecified := true;

     {xoff}
     if not SCharIs(geom, p, ['-','+']) then
      raise EInvalidParams.Create(
        'Invalid --geometry parameter : expected "-" or "+" before XOFF');
     xoffPlus := geom[p] = '+';
     Inc(p);
     startp := p;
     if SCharIs(geom, p, ['-','+']) then Inc(p);
     while SCharIs(geom, p, ['0'..'9']) do Inc(p);
     parXoff := StrToInt(CopyPos(geom, startp, p-1));

     {yoff}
     if not SCharIs(geom, p, ['-','+']) then
      raise EInvalidParams.Create(
        'Invalid --geometry parameter : expected "-" or "+" before YOFF');
     yoffPlus := geom[p] = '+';
     Inc(p);
     startp := p;
     if SCharIs(geom, p, ['-','+']) then Inc(p);
     while SCharIs(geom, p, ['0'..'9']) do Inc(p);
     parYoff := StrToInt(CopyPos(geom, startp, p-1));

     {end of string}
     if not (p = Length(geom)+1) then
      raise EInvalidParams.Create(
        'Invalid --geometry parameter : expected end of parameter');
    end;

  begin
   ProcData^.Window.FullScreen := false;
   try
    sizeSpecified := false;
    positionSpecified := false;
    p := 1;

    if SCharIs(geom, p,['+','-']) then
     ParsePosition else
    begin
     ParseSize;
     if p <= Length(geom) then ParsePosition;
    end;

    {ok, now we can apply what we have}
    if sizeSpecified then
    begin
     ProcData^.Window.Width := parWidth;
     ProcData^.Window.Height := parHeight;
    end;
    if positionSpecified then
    begin
     if xoffPlus then
      ProcData^.Window.Left := parXoff else
      ProcData^.Window.Left := Application.ScreenWidth-parXoff-parWidth;
     if yoffPlus then
      ProcData^.Window.Top := parYoff else
      ProcData^.Window.Top := Application.ScreenHeight-parYoff-parHeight;
    end;

   except
    on E: EConvertError do
     raise EInvalidParams.Create('Invalid --geometry parameter : '+E.Message);
   end;
  end;

begin
 Include(ProcData^.SpecifiedOptions, poGeometry);
 case OptionNum of
  0: ProcData^.Window.FullScreen := true;
  1: ApplyGeometryParam(Argument);
 end;
end;

procedure ScreenGeometryOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var ProcData: POptionProcData absolute Data;

  procedure ApplyFullScreenCustomParam(const option: string);
  var p: integer;
  begin
   ProcData^.Window.FullScreen := true;
   try
    p := CharsPos(['x','X'], option);
    if p = 0 then
     raise EInvalidParams.Create(
       'Invalid --fullscreen-custom parameter - format is not WIDTHxHEIGHT');
    Application.VideoResizeWidth := StrToInt(Copy(option, 1, p-1));
    Application.VideoResizeHeight := StrToInt(SEnding(option, p+1));
    Application.VideoResize := true;
    Application.VideoChange(true);
   except
    on E: EConvertError do
     raise EInvalidParams.Create('Invalid --fullscreen-custom parameter : '+E.Message);
   end;
  end;

begin
 Include(ProcData^.SpecifiedOptions, poScreenGeometry);
 case OptionNum of
  0: ApplyFullScreenCustomParam(Argument);
 end;
end;

procedure DisplayOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  ProcData: POptionProcData absolute Data;
begin
  Include(ProcData^.SpecifiedOptions, poDisplay);
  case OptionNum of
    0: {$ifdef CASTLE_WINDOW_XLIB}
       if Application.FOpenWindows.Count <> 0 then
         WarningWrite(ApplicationName + ': some windows are already open ' +
           'so --display option is ignored.') else
         Application.XDisplayName := Argument;
       {$else}
         {$ifdef CASTLE_WINDOW_GTK_2}
         Application.XDisplayName := Argument;
         {$else}
         WarningWrite(ApplicationName + ': warning: --display option is ignored ' +
           'when we don''t use directly Xlib');
         {$endif}
       {$endif}
  end;
end;

procedure TCastleWindowBase.ParseParameters(const AllowedOptions: TWindowParseOptions;
  out SpecifiedOptions: TWindowParseOptions);

const
  GeometryOptions: array[0..1]of TOption =
  ( (Short:#0; Long:'fullscreen'; Argument: oaNone),
    (short:#0; Long:'geometry'; Argument: oaRequired) );

  ScreenGeometryOptions: array[0..0]of TOption =
  ( (Short:#0; Long:'fullscreen-custom'; Argument: oaRequired) );

  DisplayOptions: array[0..0]of TOption =
  ( (Short:#0; Long:'display'; Argument: oaRequired) );

  OptionsForParam: array[TWindowParseOption] of
    record
      pOptions: POption_Array;
      Count: Integer;
      OptionProc: TOptionProc;
    end =
  ( ( pOptions: @GeometryOptions;
      Count: High(GeometryOptions)+1;
      OptionProc: {$ifdef FPC_OBJFPC} @ {$endif} GeometryOptionProc),
    ( pOptions: @ScreenGeometryOptions;
      Count: High(ScreenGeometryOptions) + 1;
      OptionProc: {$ifdef FPC_OBJFPC} @ {$endif} ScreenGeometryOptionProc),
    ( pOptions: @DisplayOptions;
      Count: High(DisplayOptions) + 1;
      OptionProc: {$ifdef FPC_OBJFPC} @ {$endif} DisplayOptionProc),
    ( pOptions: nil;
      Count: 0;
      OptionProc: nil)
  );

var
  Data: TOptionProcData;

  procedure HandleMacOsXProcessSerialNumber;
  {$ifdef DARWIN}
  var
    I: Integer;
  begin
    for I := 1 to Parameters.Count - 1 do
      if IsPrefix('-psn_', Parameters[I], false) then
      begin
        Parameters.Delete(I);
        Include(Data.SpecifiedOptions, poMacOsXProcessSerialNumber);
        Exit;
      end;
  {$else}
  begin
  {$endif}
  end;

var
  ParamKind: TWindowParseOption;
begin
 Data.SpecifiedOptions := [];
 Data.Window := Self;

 for ParamKind := Low(ParamKind) to High(ParamKind) do
   if ParamKind in AllowedOptions then
   begin
     if ParamKind = poMacOsXProcessSerialNumber then
       HandleMacOsXProcessSerialNumber else
       Parameters.Parse(OptionsForParam[ParamKind].pOptions,
         OptionsForParam[ParamKind].Count,
         OptionsForParam[ParamKind].OptionProc, @Data, true);
   end;

 SpecifiedOptions := Data.SpecifiedOptions;
end;

procedure TCastleWindowBase.ParseParameters(const AllowedOptions: TWindowParseOptions);
var
  dummy: TWindowParseOptions;
begin
  ParseParameters(AllowedOptions, dummy);
end;

class function TCastleWindowBase.ParseParametersHelp(
  const AllowedOptions: TWindowParseOptions;
  AddHeader: boolean): string;
const
  HelpForParam: array[TWindowParseOption] of string =
  ('  --geometry WIDTHxHEIGHT<sign>XOFF<sign>YOFF' +nl+
   '                        Set initial window size and/or position' +nl+
   '  --fullscreen          Set initial window size to cover whole screen',
   '  --fullscreen-custom WIDTHxHEIGHT' +nl+
   '                        Try to resize the screen to WIDTHxHEIGHT and' +nl+
   '                        then set initial window size to cover whole screen',
   '  --display DISPLAY-NAME' +nl+
   '                        Use given X display name.',
   ''
   );
var
  ParamKind: TWindowParseOption;
begin
  if AddHeader then
    result := 'Window options (backend ' + Application.BackendName + '):' else
    result := '';

  for ParamKind := Low(ParamKind) to High(ParamKind) do
    if (ParamKind in AllowedOptions) and
       (ParamKind <> poMacOsXProcessSerialNumber) then
    begin
      if result <> '' then result += nl;
      result += HelpForParam[ParamKind];
    end;
end;

{ TCastleWindowBase miscellaneous -------------------------------------------- }

function TCastleWindowBase.RequestedBufferAttributes: string;
begin
 if DoubleBuffer then
   Result := 'double buffered' else
   Result := 'single buffered';
 if ColorBits > 0 then
   Result += Format(', with RGB colors bits (%d, %d, %d) (total %d color bits)', [RedBits, GreenBits, BlueBits, ColorBits]);
 if DepthBits > 0 then
   Result += Format(', with %d-bits sized depth buffer', [DepthBits]);
 if StencilBits > 0 then
   Result += Format(', with %d-bits sized stencil buffer', [StencilBits]);
 if AlphaBits > 0 then
   Result += Format(', with %d-bits sized alpha channel', [AlphaBits]);
 if not ZeroVector(FAccumBits) then
   Result += Format(', with (%d,%d,%d,%d)-bits sized accumulation buffer',
    [FAccumBits[0], FAccumBits[1], FAccumBits[2], FAccumBits[3]]);
 if MultiSampling > 1 then
   Result += Format(', with multisampling (%d samples)', [MultiSampling]);
end;

procedure TCastleWindowBase.CheckRequestedBufferAttributes(
  const ProviderName: string;
  ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
  ProvidedAccumRedBits, ProvidedAccumGreenBits, ProvidedAccumBlueBits,
  ProvidedAccumAlphaBits, ProvidedMultiSampling: Cardinal);

  procedure CheckRequestedBits(const Name: string; RequestedBits, ProvidedBits: Cardinal);
  begin
   if ProvidedBits < RequestedBits then
    raise EGLContextNotPossible.CreateFmt('%s provided OpenGL context with %s'
      +' %d-bits sized but at least %d-bits sized is required',
      [ ProviderName, Name, ProvidedBits, RequestedBits ]);
  end;

begin
 CheckRequestedBits('stencil buffer', StencilBits, ProvidedStencilBits);
 CheckRequestedBits('depth buffer', DepthBits, ProvidedDepthBits);
 CheckRequestedBits('alpha channel', AlphaBits, ProvidedAlphaBits);
 CheckRequestedBits('accumulation buffer''s red channel'  , FAccumBits[0], ProvidedAccumRedBits);
 CheckRequestedBits('accumulation buffer''s green channel', FAccumBits[1], ProvidedAccumGreenBits);
 CheckRequestedBits('accumulation buffer''s blue channel' , FAccumBits[2], ProvidedAccumBlueBits);
 CheckRequestedBits('accumulation buffer''s alpha channel', FAccumBits[3], ProvidedAccumAlphaBits);

 { If MultiSampling <= 1, this means that multisampling not required,
   so don't check it. Even if MultiSampling = 1 and ProvidedMultiSampling = 0
   (as most backends report no multisampling as num samples = 0), it's all Ok. }

 if MultiSampling > 1 then
 begin
   if ProvidedMultiSampling < MultiSampling then
    raise EGLContextNotPossible.CreateFmt('%s provided OpenGL context with %d ' +
      'samples for multisampling (<= 1 means that no multisampling was provided) ' +
      'but at last %d samples for multisampling is required',
      [ ProviderName, ProvidedMultiSampling, MultiSampling ]);
 end;
end;

function TCastleWindowBase.GetMouseX: Integer;
begin
  Result := FMouseX;
end;

function TCastleWindowBase.GetMouseY: Integer;
begin
  Result := FMouseY;
end;

function TCastleWindowBase.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TCastleWindowBase.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCastleWindowBase.GetDpi: Integer;
begin
  Result := FDpi;
end;

function TCastleWindowBase.GetMousePressed: TMouseButtons;
begin
  Result := FMousePressed;
end;

function TCastleWindowBase.GetPressed: TKeysPressed;
begin
  Result := FPressed;
end;

procedure TCastleWindowBase.MenuUpdateBegin;
begin
  { MenuUpdateNeedsInitialize = false always when MenuUpdateInside = 0. }
  Assert((MenuUpdateInside <> 0) or (not MenuUpdateNeedsInitialize));

  Inc(MenuUpdateInside);
end;

procedure TCastleWindowBase.MenuUpdateEnd;
begin
  Dec(MenuUpdateInside);
  if (MenuUpdateInside = 0) and MenuUpdateNeedsInitialize then
  begin
    MenuUpdateNeedsInitialize := false;
    { We could also manually call BackendMenuInitialize now,
      as we know MenuUpdateInside = 0. But MenuInitialize takes
      care also about some checks and updating MenuInitialized variable. }
    MenuInitialize;
  end;
end;

procedure TCastleWindowBase.MenuInitialize;
begin
  if MenuUpdateInside = 0 then
  begin
    if (not MenuInitialized) and (not Closed) and MainMenuVisible then
    begin
      BackendMenuInitialize;
      MenuInitialized := true;
    end;
  end else
    MenuUpdateNeedsInitialize := true;
end;

procedure TCastleWindowBase.MenuFinalize;
begin
  { MenuFinalize ignores MenuUpdateInside state, not needed. }
  if MenuInitialized and (not Closed) and MainMenuVisible then
  begin
    MenuInitialized := false;
    BackendMenuFinalize;
  end;
end;

procedure TCastleWindowBase.SimpleSetFullScreen(const Value: boolean);
begin
  if FFullScreen <> Value then
  begin
    FFullScreen := Value;
    if not Closed then
    begin
      Close(false);
      if Value then
      begin
        { Value is true, so we change FFullScreen from false to true.
          Save BeforeFullScreen* now. }
        BeforeFullScreenGeometryKnown := true;
        BeforeFullScreenLeft := Left;
        BeforeFullScreenTop := Top;
        BeforeFullScreenWidth := Width;
        BeforeFullScreenHeight := Height;
      end else
      begin
        { We change FFullScreen from true to false.
          Set window geometry. Note that BeforeFullScreenGeometryKnown may be false,
          if the window was initially opened in FullScreen mode, in which case
          we just set default sensible geometry. }
        if BeforeFullScreenGeometryKnown then
        begin
          Left := BeforeFullScreenLeft;
          Top := BeforeFullScreenTop;
          Width := BeforeFullScreenWidth;
          Height := BeforeFullScreenHeight;
        end else
        begin
          Left := WindowPositionCenter;
          Top := WindowPositionCenter;
          Width := WindowDefaultSize;
          Height := WindowDefaultSize;
        end;
      end;
      Open;
    end;
  end;
end;

procedure TCastleWindowBase.SwapFullScreen;
begin
  FullScreen := not FullScreen;
end;

function TCastleWindowBase.GetPublicCaption: string;
begin
  Result := FCaption[cpPublic];
end;

procedure TCastleWindowBase.SetPublicCaption(const Value: string);
begin
  SetCaption(cpPublic, Value);
end;

function TCastleWindowBase.GetWholeCaption: string;
begin
  Result := FCaption[cpPublic] + FCaption[cpFps];
end;

function TCastleWindowBase.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

{ TCastleWindowDemo ---------------------------------------------------------------- }

procedure TCastleWindowDemo.EventUpdate;
begin
  inherited;
  { show FPS on caption once FpsCaptionUpdateInterval passed }
  if FpsShowOnCaption and
     ((lastFpsOutputTick = 0) or
      (TimeTickDiff(lastFpsOutputTick, GetTickCount) >= FpsCaptionUpdateInterval)) then
  begin
    LastFpsOutputTick := GetTickCount;
    SetCaption(cpFps, Format(' - FPS : %f (real : %f)', [Fps.FrameTime, Fps.RealTime]));
  end;
end;

function TCastleWindowDemo.AllowSuspendForInput: boolean;
begin
  result := (inherited AllowSuspendForInput) and (not FpsShowOnCaption);
end;

procedure TCastleWindowDemo.EventPress(const Event: TInputPressRelease);
begin
  if Event.IsKey(Close_CharKey) then
    Close else
  if Event.IsKey(SwapFullScreen_Key) then
    FullScreen := not FullScreen else
    inherited;
    { nie wywoluj inherited jesli to byl klawisz Close_CharKey lub
      SwapFullScreen_Key bo te klawisze zmienily okienko na tyle ze mozna
      podejrzewac ze wcisniecie klawisza mozna juz uznac za nieaktualne. }
end;

procedure TCastleWindowDemo.SetDemoOptions(ASwapFullScreen_Key: TKey;
  AClose_CharKey: char;
  AFpsShowOnCaption: boolean);
begin
  SwapFullScreen_Key := ASwapFullScreen_Key;
  Close_CharKey := AClose_CharKey;
  FpsShowOnCaption := AFpsShowOnCaption;
end;

constructor TCastleWindowDemo.Create(AOwner: TComponent);
begin
  inherited;
  Close_CharKey := #0;
  SwapFullScreen_Key := K_None;
  FpsShowOnCaption := false;
  FFpsCaptionUpdateInterval := DefaultFpsCaptionUpdateInterval;
end;

{ TControlledUIControlList ----------------------------------------------------- }

type
  { TUIControlList descendant that takes care to react to list add/remove
    notifications, doing appropriate operations with parent Container. }
  TControlledUIControlList = class(TUIControlList)
  private
    Container: TCastleWindowCustom;
  public
    constructor Create(const FreeObjects: boolean; const AContainer: TCastleWindowCustom);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

constructor TControlledUIControlList.Create(const FreeObjects: boolean;
  const AContainer: TCastleWindowCustom);
begin
  inherited Create(FreeObjects);
  Container := AContainer;
end;

procedure TControlledUIControlList.Notify(Ptr: Pointer; Action: TListNotification);
var
  C: TUIControl absolute Ptr;
begin
  inherited;

  C := TUIControl(Ptr);
  case Action of
    lnAdded:
      begin
        { Make sure Container.ControlsVisibleChange will be called
          when a control calls OnVisibleChange. }
        if C.OnVisibleChange = nil then
          C.OnVisibleChange := @Container.ControlsVisibleChange;

        { Register Container to be notified of control destruction. }
        C.FreeNotification(Container);

        C.Container := Container;

        if not Container.Closed then
        begin
          if C.DisableContextOpenClose = 0 then
            C.GLContextOpen;
          { Call initial ContainerResize for control.
            If window OpenGL context is not yet initialized, defer it to
            the Open time, then our initial EventResize will be called
            that will do ContainerResize on every control. }
          C.ContainerResize(Container.Width, Container.Height);
        end;
      end;
    lnExtracted, lnDeleted:
      begin
        if (not Container.Closed) and
           (C.DisableContextOpenClose = 0) then
          C.GLContextClose;

        if C.OnVisibleChange = @Container.ControlsVisibleChange then
          C.OnVisibleChange := nil;

        C.RemoveFreeNotification(Container);
        Container.DetachNotification(C);

        C.Container := nil;
      end;
    else raise EInternalError.Create('TControlledUIControlList.Notify action?');
  end;

  { This notification may get called during FreeAndNil(FControls)
    in TCastleWindowCustom.Destroy. Then FControls is already nil, and we're
    getting remove notification for all items (as FreeAndNil first sets
    object to nil). Testcase: lets_take_a_walk exit. }
  if Container.FControls <> nil then
    Container.UpdateFocusAndMouseCursor;
end;

{ TCastleWindowCustom --------------------------------------------------------- }

constructor TCastleWindowCustom.Create(AOwner: TComponent);
begin
  inherited;
  FControls := TControlledUIControlList.Create(false, Self);
  FUseControls := true;
  FOnDrawStyle := dsNone;
  FTooltipDelay := DefaultTooltipDelay;
  FTooltipDistance := DefaultTooltipDistance;

  { connect 3D device - 3Dconnexion device }
  Mouse3dPollTimer := 0;
  try
    Mouse3d := T3DConnexionDevice.Create('Castle Control');
  except
    on E: Exception do
      if Log then WritelnLog('3D Mouse', 'Exception %s when initializing T3DConnexionDevice: %s',
        [E.ClassName, E.Message]);
  end;
end;

destructor TCastleWindowCustom.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(Mouse3d);
  inherited;
end;

procedure TCastleWindowCustom.Notification(AComponent: TComponent; Operation: TOperation);
begin
  { We have to remove a reference to the object from Controls list.
    This is crucial: TControlledUIControlList.Notify,
    and some Controls.MakeSingle calls, assume that all objects on
    the Controls list are always valid objects (no invalid references,
    even for a short time).

    Check "Controls <> nil" is not needed here, it's just in case
    this code will be moved to TUIControl.Notification some day.
    See T3D.Notification for explanation. }

  if (Operation = opRemove) and (AComponent is TUIControl) {and (Controls <> nil)} then
  begin
    Controls.DeleteAll(AComponent);
    DetachNotification(TUIControl(AComponent));
  end;
end;

procedure TCastleWindowCustom.DetachNotification(const C: TUIControl);
begin
  if C = FFocus        then FFocus := nil;
  if C = FCaptureInput then FCaptureInput := nil;
end;

procedure TCastleWindowCustom.UpdateFocusAndMouseCursor;

  function CalculateFocus: TUIControl;
  var
    I: Integer;
  begin
    if not UseControls then Exit(nil);

    for I := 0 to Controls.Count - 1 do
    begin
      Result := Controls[I];
      if Result.PositionInside(MouseX, MouseY) then
        Exit;
    end;

    Result := nil;
  end;

  function CalculateMouseCursor: TMouseCursor;
  begin
    if Focus <> nil then
      Result := Focus.Cursor else
      Result := mcDefault;
  end;

var
  NewFocus: TUIControl;
begin
  if FCaptureInput <> nil then
    NewFocus := FCaptureInput else
    NewFocus := CalculateFocus;

  if NewFocus <> Focus then
  begin
    if (Focus <> nil) and UseControls then Focus.Focused := false;
    FFocus := NewFocus;
    { No need to check UseControls above: if Focus <> nil then we know
      UseControls was true during CalculateFocus. }
    if (Focus <> nil) then Focus.Focused := true;
  end;

  Cursor := CalculateMouseCursor;
end;

procedure TCastleWindowCustom.EventUpdate;

  procedure UpdateTooltip;
  var
    T: TTimerResult;
    NewTooltipVisible: boolean;
  begin
    { Update TooltipVisible and LastPositionForTooltip*.
      Idea is that user must move the mouse very slowly to activate tooltip. }

    T := Fps.UpdateStartTime;
    if (not LastPositionForTooltip) or
       (Sqr(LastPositionForTooltipX - MouseX) +
        Sqr(LastPositionForTooltipY - MouseY) > Sqr(TooltipDistance)) then
    begin
      LastPositionForTooltip := true;
      LastPositionForTooltipX := MouseX;
      LastPositionForTooltipY := MouseY;
      LastPositionForTooltipTime := T;
      NewTooltipVisible := false;
    end else
      NewTooltipVisible :=
        { make TooltipVisible only when we're over a control that has
          focus. This avoids unnecessary changing of TooltipVisible
          (and related PostRedisplay) when there's no tooltip possible. }
        (Focus <> nil) and
        (Focus.TooltipStyle <> dsNone) and
        ( (1000 * (T - LastPositionForTooltipTime)) div
          TimerFrequency > TooltipDelay );

    if FTooltipVisible <> NewTooltipVisible then
    begin
      FTooltipVisible := NewTooltipVisible;

      if TooltipVisible then
      begin
        { when setting TooltipVisible from false to true,
          update LastPositionForTooltipX/Y. We don't want to hide the tooltip
          at the slightest jiggle of the mouse :) On the other hand,
          we don't want to update LastPositionForTooltipX/Y more often,
          as it would disable the purpose of TooltipDistance: faster
          mouse movement should hide the tooltip. }
        LastPositionForTooltipX := MouseX;
        LastPositionForTooltipY := MouseY;
        { also update TooltipX/Y }
        FTooltipX := MouseX;
        FTooltipY := MouseY;
      end;

      PostRedisplay;
    end;
  end;

var
  I: Integer;
  C: TUIControl;
  HandleInput: boolean;
  Dummy: boolean;
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  Mouse3dPollSpeed: Single;
const
  Mouse3dPollDelay = 0.05;
begin
  if UseControls then
  begin
    UpdateTooltip;

    { 3D Mouse }
    if Assigned(Mouse3D) and Mouse3D.Loaded then
    begin
      Mouse3dPollTimer -= Fps.UpdateSecondsPassed;
      if Mouse3dPollTimer < 0 then
      begin
        { get values from sensor }
        Mouse3dPollSpeed := -Mouse3dPollTimer + Mouse3dPollDelay;
        Mouse3D.GetSensorTranslation(Tx, Ty, Tz, TLength);
        Mouse3D.GetSensorRotation(Rx, Ry, Rz, RAngle);

        { send to all 2D controls, including viewports }
        for I := 0 to Controls.Count - 1 do
        begin
          C := Controls[I];
          if C.PositionInside(MouseX, MouseY) then
          begin
            C.SensorTranslation(Tx, Ty, Tz, TLength, Mouse3dPollSpeed);
            C.SensorRotation(Rx, Ry, Rz, RAngle, Mouse3dPollSpeed);
          end;
        end;

        { set timer.
          The "repeat ... until" below should not be necessary under normal
          circumstances, as Mouse3dPollDelay should be much larger than typical
          frequency of how often this is checked. But we do it for safety
          (in case something else, like AI or collision detection,
          slows us down *a lot*). }
        repeat Mouse3dPollTimer += Mouse3dPollDelay until Mouse3dPollTimer > 0;
      end;
    end;

    { Although we call Update for all the controls, we look
      at PositionInside and track HandleInput values.
      See TUIControl.Update for explanation. }

    HandleInput := true;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
      begin
        C.Update(Fps.UpdateSecondsPassed, HandleInput);
      end else
      begin
        Dummy := false;
        C.Update(Fps.UpdateSecondsPassed, Dummy);
      end;
    end;
  end;

  inherited;
end;

procedure TCastleWindowCustom.EventPress(const Event: TInputPressRelease);
var
  C: TUIControl;
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Press(Event) then
        begin
          { We have to check whether C.Container = Self. That is because
            the implementation of control's Press method could remove itself
            from our Controls list. Consider e.g. TCastleOnScreenMenu.Press
            that may remove itself from the Window.Controls list when clicking
            "close menu" item. We cannot, in such case, save a reference to
            this control in FCaptureInput, because we should not speak with it
            anymore (we don't know when it's destroyed, we cannot call it's
            Release method because it has Container = nil, and so on). }
          if (Event.EventType = itMouseButton) and
             (C.Container = Self as IUIContainer) then
            FCaptureInput := C;
          Exit;
        end;
    end;
  end;

  inherited;
end;

procedure TCastleWindowCustom.EventRelease(const Event: TInputPressRelease);
var
  C, Capture: TUIControl;
  I: Integer;
begin
  Capture := FCaptureInput;
  if FMousePressed = [] then
    FCaptureInput := nil;

  if UseControls then
  begin
    if Capture <> nil then
    begin
      Capture.Release(Event);
      Exit;
    end;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.Release(Event) then Exit;
    end;
  end;

  inherited;
end;

procedure TCastleWindowCustom.EventOpen;
var
  I: Integer;
begin
  inherited;
  if Application.OpenWindowsCount = 1 then
    OnGLContextOpen.ExecuteAll;

  { call GLContextOpen on controls after inherited (OnOpen). }
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextOpen;
  end;
end;

procedure TCastleWindowCustom.EventClose;
var
  I: Integer;
begin
  { call GLContextClose on controls before inherited (OnClose).
    This may be called from Close, which may be called from TCastleWindowBase destructor,
    so prepare for Controls being possibly nil now. }
  if UseControls and (Controls <> nil) then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextClose;
  end;

  if Application.OpenWindowsCount = 1 then
    OnGLContextClose.ExecuteAll;
  inherited;
end;

function TCastleWindowCustom.AllowSuspendForInput: boolean;
var
  I: Integer;
begin
  Result := inherited;
  if not Result then Exit;

  if UseControls then
  begin
    { Do not suspend when you're over a control that may have a tooltip,
      as EventUpdate must track and eventually show tooltip. }
    if (Focus <> nil) and (Focus.TooltipStyle <> dsNone) then
      Exit(false);

    for I := 0 to Controls.Count - 1 do
    begin
      Result := Controls[I].AllowSuspendForInput;
      if not Result then Exit;
    end;
  end;
end;

procedure TCastleWindowCustom.SetUseControls(const Value: boolean);
begin
  if Value <> UseControls then
  begin
    FUseControls := Value;
    { Focus must always be @nil when UseControls = false }
    UpdateFocusAndMouseCursor;
  end;
end;

procedure TCastleWindowCustom.EventMouseMove(NewX, NewY: Integer);
var
  C: TUIControl;
  I: Integer;
begin
  UpdateFocusAndMouseCursor;

  if UseControls then
  begin
    if FCaptureInput <> nil then
    begin
      FCaptureInput.MouseMove(MouseX, MouseY, NewX, NewY);
      Exit;
    end;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseMove(MouseX, MouseY, NewX, NewY) then Exit;
    end;
  end;

  inherited;
end;

procedure TCastleWindowCustom.ControlsVisibleChange(Sender: TObject);
begin
  PostRedisplay;
end;

procedure TCastleWindowCustom.EventBeforeDraw;
var
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].BeforeDraw;
  end;

  inherited;
end;

procedure TCastleWindowCustom.EventDraw;

  { Call Draw for all controls having DrawStyle = ds3D.

    Also (since we call DrawStyle for everything anyway)
    calculates AnythingWants2D = if any control returned DrawStyle = ds2D.
    If not, you can later avoid even changing projection to 2D. }
  procedure Draw3D(out AnythingWants2D: boolean);
  var
    I: Integer;
    C: TUIControl;
  begin
    AnythingWants2D := false;

    if UseControls then
    begin
      { draw controls in "downto" order, back to front }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];
        case C.DrawStyle of
          ds2D: AnythingWants2D := true;
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for TUIControl.Draw calls.
            For now, just glLoadIdentity. }
          ds3D: begin {$ifndef OpenGLES} glLoadIdentity; {$endif} C.Draw; end;
        end;
      end;

      if TooltipVisible and (Focus <> nil) then
        case Focus.TooltipStyle of
          ds2D: AnythingWants2D := true;
          ds3D: begin {$ifndef OpenGLES} glLoadIdentity; {$endif} Focus.DrawTooltip; end;
        end;
    end;

    case OnDrawStyle of
      ds2D: AnythingWants2D := true;
      ds3D: begin {$ifndef OpenGLES} glLoadIdentity; {$endif} inherited EventDraw; end;
    end;
  end;

  procedure Draw2D;
  var
    C: TUIControl;
    I: Integer;
  begin
    { Set state that is guaranteed for Draw2D calls,
      but TUIControl.Draw cannot change it carelessly. }
    {$ifndef OpenGLES}
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    {$endif}
    glDisable(GL_DEPTH_TEST);
    ScissorDisable;
    GLEnableTexture(CastleGLUtils.etNone);
    glViewport(Rect);

    OrthoProjection(0, Width, 0, Height);

    if UseControls then
    begin
      { draw controls in "downto" order, back to front }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];

        if C.DrawStyle = ds2D then
        begin
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for Draw2d calls. }
          {$ifndef OpenGLES} glLoadIdentity; {$endif}
          CastleGLUtils.WindowPos := Vector2LongInt(0, 0);
          C.Draw;
        end;
      end;

      if TooltipVisible and (Focus <> nil) and (Focus.TooltipStyle = ds2D) then
      begin
        {$ifndef OpenGLES} glLoadIdentity; {$endif}
        CastleGLUtils.WindowPos := Vector2LongInt(0, 0);
        Focus.DrawTooltip;
      end;
    end;

    if OnDrawStyle = ds2D then
    begin
      {$ifndef OpenGLES} glLoadIdentity; {$endif}
      CastleGLUtils.WindowPos := Vector2LongInt(0, 0);
      inherited EventDraw;
    end;
  end;

var
  AnythingWants2D: boolean;
begin
  Draw3D(AnythingWants2D);

  if AnythingWants2D then
    Draw2D;

  if OnDrawStyle = dsNone then
    inherited;
end;

procedure TCastleWindowCustom.EventResize;
var
  I: Integer;
begin
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].ContainerResize(Width, Height);
  end;

  { This way control's get ContainerResize
    (so have ContainerWidth / ContainerHeight set) before OnResize,
    useful to position them in OnResize. }
  inherited;
end;

function TCastleWindowCustom.GetTooltipX: Integer;
begin
  Result := FTooltipX;
end;

function TCastleWindowCustom.GetTooltipY: Integer;
begin
  Result := FTooltipY;
end;

{ TCastleWindow ------------------------------------------------------- }

constructor TCastleWindow.Create(AOwner: TComponent);
begin
  inherited;

  FSceneManager := TGameSceneManager.Create(Self);
  { SetSubComponent and Name setting below are not really necessary,
    but since TCastleWindow is a TComponent descendant, *maybe* in the future
    we'll make use of it. }
  FSceneManager.SetSubComponent(true);
  FSceneManager.Name := 'SceneManager';
  FSceneManager.OnBoundNavigationInfoChanged := @NavigationInfoChanged;
  Controls.Add(SceneManager);
end;

procedure TCastleWindow.Load(const SceneURL: string);
begin
  Load(Load3D(SceneURL, false), true);
end;

procedure TCastleWindow.Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);
begin
  { destroy MainScene and Camera, we will recreate them }
  SceneManager.MainScene.Free;
  SceneManager.MainScene := nil;
  SceneManager.Items.Clear;
  SceneManager.Camera.Free;

  SceneManager.MainScene := TCastleScene.Create(Self);
  SceneManager.MainScene.Load(ARootNode, OwnsRootNode);
  SceneManager.Items.Add(SceneManager.MainScene);

  { initialize octrees titles }
  MainScene.TriangleOctreeProgressTitle := 'Building triangle octree';
  MainScene.ShapeOctreeProgressTitle := 'Building shape octree';

  { just to make our Camera always non-nil }
  SceneManager.Camera := SceneManager.CreateDefaultCamera;
end;

function TCastleWindow.MainScene: TCastleScene;
begin
  Result := SceneManager.MainScene;
end;

function TCastleWindow.GetShadowVolumes: boolean;
begin
  Result := SceneManager.ShadowVolumes;
end;

procedure TCastleWindow.SetShadowVolumes(const Value: boolean);
begin
  SceneManager.ShadowVolumes := Value;
end;

function TCastleWindow.GetShadowVolumesDraw: boolean;
begin
  Result := SceneManager.ShadowVolumesDraw;
end;

procedure TCastleWindow.SetShadowVolumesDraw(const Value: boolean);
begin
  SceneManager.ShadowVolumesDraw := Value;
end;

function TCastleWindow.GetNavigationType: TNavigationType;
begin
  if SceneManager.Camera <> nil then
    Result := SceneManager.Camera.GetNavigationType else
    Result := ntNone;
end;

procedure TCastleWindow.SetNavigationType(const Value: TNavigationType);
begin
  if (SceneManager.Camera <> nil) and
     (SceneManager.Camera is TUniversalCamera) then
  begin
    (SceneManager.Camera as TUniversalCamera).NavigationType := Value;
    if Assigned(SceneManager.OnBoundNavigationInfoChanged) then
      SceneManager.OnBoundNavigationInfoChanged(SceneManager);
  end;
end;

{ TWindowList ------------------------------------------------------------ }

procedure TWindowList.PostRedisplay;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].PostRedisplay;
end;

procedure TWindowList.DoUpdate;
var
  i: integer;
begin
  for i := 0 to Count - 1 do Items[i].DoUpdate;
end;

procedure TWindowList.DoTimer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do Items[i].DoTimer;
end;

{ --------------------------------------------------------------------------
  Generic part of implementation of TCastleApplication,
  that does not depend what CASTLE_WINDOW_xxx backend you want. }

var
  FApplication: TCastleApplication;

constructor TCastleApplication.Create(AOwner: TComponent);
begin
  inherited;
  FOpenWindows := TWindowList.Create(false);
  FTimerMilisec := 1000;
  FLimitFPS := DefaultLimitFPS;

  CreateBackend;
end;

destructor TCastleApplication.Destroy;
begin
  { Close any windows possibly open now.
    This is necessary --- after destroying Application there would be really
    no way for them to close properly (that is, TCastleWindowBase.CloseBackend
    may, and usually will, fail with very strange errors when called
    after freeing central Application). }
  Quit;

  { unregister free notification from these objects }
  MainWindow := nil;

  { nil now the Application variable. For reasoning, see this units
    finalization. }
  FApplication := nil;

  VideoReset;
  DestroyBackend;
  FreeAndNil(FOpenWindows);
  inherited;
end;

procedure TCastleApplication.Initialize;
begin
  if not Initialized then
  begin
    Initialized := true;
    if Assigned(OnInitialize) then
      OnInitialize();
  end;
end;

procedure TCastleApplication.SetMainWindow(const Value: TCastleWindowBase);
begin
  if FMainWindow <> Value then
  begin
    if FMainWindow <> nil then
      FMainWindow.RemoveFreeNotification(Self);
    FMainWindow := Value;
    if FMainWindow <> nil then
      FMainWindow.FreeNotification(Self);
  end;
end;

procedure TCastleApplication.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = MainWindow) then
    MainWindow := nil;
end;

function TCastleApplication.GetOpenWindows(Index: integer): TCastleWindowBase;
begin
  result := FOpenWindows[Index];
end;

function TCastleApplication.OpenWindowsCount: integer;
begin
  result := FOpenWindows.Count;
end;

procedure TCastleApplication.OpenWindowsAdd(Window: TCastleWindowBase);
begin
  FOpenWindows.Add(Window);
end;

procedure TCastleApplication.OpenWindowsRemove(Window: TCastleWindowBase;
  QuitWhenLastWindowClosed: boolean);
begin
  if (FOpenWindows.Remove(Window) <> -1) and
     (OpenWindowsCount = 0) and QuitWhenLastWindowClosed then Quit;
end;

function TCastleApplication.FindWindow(Window: TCastleWindowBase): integer;
begin
  for result := 0 to OpenWindowsCount-1 do
    if OpenWindows[result] = Window then exit;
  result := -1;
end;

procedure TCastleApplication.Quit;
var
  OldOpenWindowsCount: Integer;
begin
  { We're calling here Close(false) so we will not cause infinite recursive
    Quit calls.

    Remember that calling Close actually calls Application.OpenWindowsRemove.
    In fact, it's guaranteed that calling Close on open
    window will remove it from OpenWindows list (we even check it by assert,
    otherwise our "while" could never finish).
    So the number of open windows will drop during while
    (that's why "for I := 0 to OpenWindowsCount - 1 do ..." would be stupid
    code here, but "while OpenWindowsCount > 0 ..." is Ok). }

  while OpenWindowsCount > 0 do
  begin
    OldOpenWindowsCount := OpenWindowsCount;
    OpenWindows[0].Close(false);
    Assert(OpenWindowsCount = OldOpenWindowsCount - 1);
  end;

  QuitWhenNoOpenWindows;
end;

procedure TCastleApplication.DoSelfUpdate;
begin
  if Assigned(FOnUpdate) then FOnUpdate;
end;

procedure TCastleApplication.DoSelfTimer;
begin
  if Assigned(FOnTimer) then FOnTimer;
end;

procedure TCastleApplication.MaybeDoTimer(var ALastDoTimerTime: TMilisecTime);
var
  Now: TMilisecTime;
begin
  Now := GetTickCount;
  if ((ALastDoTimerTime = 0) or
      (MilisecTimesSubtract(Now, ALastDoTimerTime) >= FTimerMilisec)) then
  begin
    ALastDoTimerTime := Now;
    DoSelfTimer;
    FOpenWindows.DoTimer;
  end;
end;

function TCastleApplication.AllowSuspendForInput: boolean;
var
  I: Integer;
begin
  Result := not (Assigned(OnUpdate) or Assigned(OnTimer));
  if not Result then Exit;

  for I := 0 to OpenWindowsCount - 1 do
  begin
    Result := OpenWindows[I].AllowSuspendForInput;
    if not Result then Exit;
  end;
end;

{ TCastleApplication.Video* things ---------------------------------------- }

{$ifndef CASTLE_WINDOW_HAS_VIDEO_CHANGE}
function TCastleApplication.TryVideoChange: boolean;
begin
 Result := false;
end;

procedure TCastleApplication.VideoReset;
begin
end;
{$endif not CASTLE_WINDOW_HAS_VIDEO_CHANGE}

function TCastleApplication.VideoSettingsDescribe: string;
begin
  Result := '';
  if VideoResize then
    Result += Format('  Screen size :  %dx%d', [VideoResizeWidth, VideoResizeHeight]) + nl;
  if VideoColorBits <> 0 then
    Result += Format('  Color bits per pixel : %d', [VideoColorBits]) + nl;
  if VideoFrequency <> 0 then
    Result += Format('  Display frequency : %d', [VideoFrequency]) + nl;

  if Result = '' then
    Result := '  No display settings change' + nl;
end;

procedure TCastleApplication.VideoChange(OnErrorWarnUserAndContinue: boolean);
var s: string;
begin
 if not TryVideoChange then
 begin
  s := 'Can''t change display settings to : ' + nl + VideoSettingsDescribe;

  {$ifndef CASTLE_WINDOW_HAS_VIDEO_CHANGE}
    s += ' (changing Video properties not implemented when CastleWindow is '+
      'made on top of ' +BackendName +')';
  {$endif}

  if OnErrorWarnUserAndContinue then
   WarningWrite(s+'. Trying to continue anyway.') else
   raise Exception.Create(s);
 end;
end;

procedure TCastleApplication.DoLimitFPS;
var
  NowTime: TTimerResult;
  TimeRemainingFloat: Single;
begin
  if LimitFPS > 0 then
  begin
    NowTime := Timer;

    { When this is run for the 1st time, LastLimitFPSTime is zero,
      so NowTime - LastLimitFPSTime is huge, so we will not do any Sleep
      and only update LastLimitFPSTime.

      For the same reason, it is not a problem if you do not call DoLimitFPS
      often enough (for example, you do a couple of ProcessMessage calls
      without DoLimitFPS for some reason), or when user temporarily sets
      LimitFPS to zero and then back to 100.0.
      In every case, NowTime - LastLimitFPSTime will be large, and no sleep
      will happen. IOW, in the worst case --- we will not limit FPS,
      but we will *never* slow down the program when it's not really necessary. }

    TimeRemainingFloat :=
      { how long I should wait between _LimitFPS calls }
      1 / LimitFPS -
      { how long I actually waited between _LimitFPS calls }
      (NowTime - LastLimitFPSTime) / TimerFrequency;
    { Don't do Sleep with too small values.
      It's better to have larger FPS values than limit,
      than to have them too small. }
    if TimeRemainingFloat > 0.001 then
    begin
      Sleep(Round(1000 * TimeRemainingFloat));
      LastLimitFPSTime := Timer;
    end else
      LastLimitFPSTime := NowTime;
  end;
end;

{ global --------------------------------------------------------------------- }

procedure Resize2D(Window: TCastleWindowBase);
begin
  glViewport(Window.Rect);
  OrthoProjection(0, Window.Width, 0, Window.Height);
end;

function KeyString(const CharKey: char; const Key: TKey;
  const Modifiers: TModifierKeys; out S: string): boolean;
begin
  if CharKey <> #0 then
  begin
    S := CharToNiceStr(CharKey, Modifiers, false
      {$ifdef CASTLE_WINDOW_LCL} {$ifdef LCLCarbon}, true {$endif} {$endif} );
    Result := true;
  end else
  if Key <> K_None then
  begin
    S := KeyToStr(Key, Modifiers
      {$ifdef CASTLE_WINDOW_LCL} {$ifdef LCLCarbon}, true {$endif} {$endif});
    Result := true;
  end else
  Result := false;
end;

function Application: TCastleApplication;
begin
  Result := FApplication;
end;

var
  FClipboard: TCastleClipboard;

function Clipboard: TCastleClipboard;
begin
  Result := FClipboard;
end;

{ init/fini --------------------------------------------------------------- }

initialization
  CastleWindowMenu_Init;
  FApplication := TCastleApplication.Create(nil);
  FClipboard := TCastleClipboard.Create;
finalization
  { Instead of using FreeAndNil, just call Free.
    In our destructor we take care of setting Application variable to @nil,
    when it becomes really useless.

    Otherwise FreeAndNil first nils, then frees Application, and we really
    want to keep Application during first stage of TCastleApplication destruction:
    when calling Quit, which may close windows, which may use Application
    variable in their Close or CloseBackend implementations. }
  Application.Free;
  Assert(Application = nil);

  FreeAndNil(FClipboard);

  { Order is important: Castlewindowmenu_Fini frees MenuItems, which is needed
    by TMenu destructor. And some TCastleWindowBase instances may be freed
    only by Application destructor (when they are owned by Application). }
  CastleWindowMenu_Fini;
end.
