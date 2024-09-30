{
  Copyright 2001-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Defines @link(TCastleWindow) class: a window for rendering,
  processing of inputs, and generally the primary way to display your
  "Castle Game Engine" application.

  Technically speaking, most of the @link(TCastleWindow) job
  is to initialize rendering context (like OpenGL, OpenGLES context)
  and to manage a hierarchy of user interface controls (@link(TCastleUserInterface)).
  When necessary, it calls the proper methods of these controls to render,
  process inputs, and so on.

  @bold(Usage:)

  You usually do not need to write code to create an instance of this class yourself.
  Just create a new project (using @italic("New Project") in CGE editor, or
  using @code(castle-engine create NEW-PROJECT-NAME) command-line).
  The recommended code to create and initialize a single @link(TCastleWindow)
  instance will be already there. It basically

  @unorderedList(
    @itemSpacing Compact
    @item(Creates @link(TCastleWindow) instance.)
    @item(Calls @link(TCastleWindow.Open Window.Open)
      to initialize rendering context and display the window.)
    @item(Calls @link(TCastleApplication.Run Application.Run)
      to process system events in a loop.
      This loop will run until you call
      @link(TCastleApplication.Terminate Application.Terminate)
      or close last visible window using @link(TCastleWindow.Close Window.Close).
      User can also close the window (using Alt+F4 or clicking on "X" button
      on the titlebar).)
  )

  Most of the code you write will be inside a "view", see
  https://castle-engine.io/view_events . This view is just
  a @link(TCastleUserInterface) instance inside a @link(TCastleWindow),
  and that's where you can override methods like @link(TCastleUserInterface.Render),
  @link(TCastleUserInterface.Press), @link(TCastleUserInterface.Update).

  @bold(Application singleton:)

  This unit also defines a singleton @link(Application) object
  (instance of class @link(TCastleApplication)),
  which is a central manager of all open @link(TCastleWindow) windows.

  You don't need to use the @link(Application) object directly in most cases.
  The new projects will contain a unit @code(GameInitialize) that performs
  the initialization of your application in a routine called @code(ApplicationInitialize),
  assigned to the
  @link(TCastleApplication.OnInitialize Application.OnInitialize).
  This is the place to do a global initialization (that doesn't fit to be
  put in any view's Start method).

  @bold(Multiple windows:)

  Desktop applications are generally free to create and open
  as many @link(TCastleWindow) instances as they want.
  Mobile and console applications are limited to a single
  @link(TCastleWindow) instance which shall be assigned to the
  @link(TCastleApplication.MainWindow Application.MainWindow) property.

  @bold(Window features:)

  Features of @link(TCastleWindow) and @link(TCastleApplication) include:

  @unorderedList(
    @item(Each window has its own rendering context.
      OpenGL and OpenGLES context can be initialized by each window.)

    @item(Window handles system events like key presses,
      mouse presses and movement, touch events on mobile.)

    @item(Window sends events when it needs to be redrawn
      and when it has been resized.)

    @item(TCastleWindow.Pressed information is available to
      always check which keys re pressed.)

    @item(Application speed is automatically measured, see @link(TCastleWindow.Fps),)

    @item(On desktops you can create multiple windows,
      each with its own rendering context.
      All the windows share rendering resources (like textures).)

    @item(On desktops (and Android) you can use
      @link(TCastleApplication.ProcessMessage) to implement modal dialog boxes
      as routines that block the event loop until the dialog is closed.
      To do this, you can run a loop like this:

      @longCode(#
        while Application.ProcessMessage do <something>;
      #)
    )

    @item(Multiple backends are available (WinAPI, GTK 2, Cocoa, LCL, FMX, Xlib...),
      see @url(https://castle-engine.io/castlewindow_backends CastleWindow Backends).
      There's also a special backend CASTLE_WINDOW_LIBRARY that allows to use
      existing OpenGL(ES) context created by some other code
      (e.g. Objective-C code on iOS).)

    @item(Some backends support a menu bar.
      Consult @url(https://castle-engine.io/castlewindow_backends CastleWindow Backends)
      to learn which backends support menus.

      You can attach a menu to a window. Menu structure is constructed using
      various descendants of TMenuEntry class.
      Then you have to assign such menu structure
      to TCastleWindow.MainMenu property.
      Assign @link(TCastleWindow.OnMenuItemClick)
      to handle what happens when the user clicks some menu item.

      See @code(examples/window/window_menu/)
      for an example how to use the menu.)

    @item(Changing screen resolution and bit depth is possible on some platforms,
      see TCastleApplication.VideoChange.
      Consult @url(https://castle-engine.io/castlewindow_backends CastleWindow Backends)
      to learn which backends support this feature.)

    @item(You can request OpenGL(ES) context properties:
      @unorderedList(
        @item color buffer
        @item with alpha channel (@link(TCastleWindow.AlphaBits AlphaBits)),
        @item stencil buffer (@link(TCastleWindow.StencilBits StencilBits)),
        @item double buffer (@link(TCastleWindow.DoubleBuffer DoubleBuffer)),
        @item(multisampling (full-screen antialiasing) buffers (by
          @link(TCastleWindow.MultiSampling MultiSampling) or higher-level
          @link(TCastleWindow.AntiAliasing AntiAliasing)))
      )
    )

    @item(You can use native modal dialogs for things such as file selection.
      Consult @url(https://castle-engine.io/castlewindow_backends CastleWindow Backends)
      to learn which backends support this feature.

      We will use native dialogs,like GTK or WinAPI dialogs, if available.
      So these dialogs look and behave good on all platforms.

      See TCastleWindow.FileDialog (for opening and saving files) and
      TCastleWindow.ColorDialog (for choosing RGB colors).)

    @item(TCastleWindow.ParseParameters method allows you to easily initialize TCastleWindow
      properties like initial size and position using command-line
      parameters like @code(@--geometry WIDTHxHEIGHT), @code(@--display) etc.)
  )
}

unit CastleWindow;

{$I castleconf.inc}

{ Choose CastleWindow backend ------------------------------------------ }

{ You can define one of the CASTLE_WINDOW_xxx symbols to use
  a specific CastleWindow backend.
  See https://castle-engine.io/castlewindow_backends
  for the documentation of available backends.

  If you don't define any such symbol,
  below we automatically choose the best backend for given OS. }
{$ifndef CASTLE_WINDOW_WINAPI}
 {$ifndef CASTLE_WINDOW_XLIB}
  {$ifndef CASTLE_WINDOW_GTK_2}
   {$ifndef CASTLE_WINDOW_TEMPLATE}
    {$ifndef CASTLE_WINDOW_FORM}
     {$ifndef CASTLE_WINDOW_ANDROID}
      {$ifndef CASTLE_WINDOW_LIBRARY}

       // PasDoc cannot handle "$if defined(xxx)" for now, workaround below
       {$ifdef PASDOC}
         {$define CASTLE_WINDOW_GTK_2}
       {$else}

         {$if defined(MSWINDOWS)}
           // various possible backends on Windows:
           {$define CASTLE_WINDOW_WINAPI} // best (looks native and most functional) on Windows
           { $define CASTLE_WINDOW_GTK_2}
           { $define CASTLE_WINDOW_FORM}
           { $define CASTLE_WINDOW_LIBRARY}
           { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
         {$elseif defined(UNIX)}
           {$if not defined(FPC)}
             // Delphi on non-Windows supports now only this backend
             {$define CASTLE_WINDOW_FORM}
           {$elseif defined(ANDROID)}
             {$define CASTLE_WINDOW_ANDROID}
           {$elseif defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)}
             {$define CASTLE_WINDOW_LIBRARY}
           {$elseif defined(DARWIN)}
             // various possible backends on macOS (desktop):
             {$define CASTLE_WINDOW_COCOA} // best (looks native) on macOS
             { $define CASTLE_WINDOW_XLIB} // requires Xlib to compile and to work
             { $define CASTLE_WINDOW_FORM} // looks native, requires LCL (from Lazarus) or FMX (from Delphi) to compile
             { $define CASTLE_WINDOW_GTK_2}
             { $define CASTLE_WINDOW_LIBRARY}
             { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
           {$else}
             // various possible backends on traditional Unix (Linux, FreeBSD) desktop:
             {$define CASTLE_WINDOW_GTK_2} // best (looks native and most functional), supports both OpenGL and OpenGLES
             { $define CASTLE_WINDOW_XLIB} // supports both OpenGL and OpenGLES
             { $define CASTLE_WINDOW_FORM}
             { $define CASTLE_WINDOW_LIBRARY}
             { $define CASTLE_WINDOW_TEMPLATE} // only useful for developers
           {$endif}
         {$endif} // end of UNIX possibilities

       {$endif} // end of "not PasDoc"

      {$endif}
     {$endif}
    {$endif}
   {$endif}
  {$endif}
 {$endif}
{$endif}

{ Configure some debugging options of CastleWindow ------------------------------- }

{ Define CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW to check OpenGL errors
  after TCastleContainer.EventRender calls.
  This is done by DoRender, that is: when a backend initiates the drawing.
  The check is done by CastleGLUtils.CheckGLErrors, checks glGetError
  and eventually raises an exception. }
{$ifdef DEBUG}
  {$define CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW}
{$endif}

{ Configure internal things -------------------------------------------------- }

{$ifdef CASTLE_WINDOW_GTK_2} {$define CASTLE_WINDOW_GTK_ANY} {$endif}

{ Sometimes GTK backend needs to call some X-specific things:
  1. Implementing TCastleWindow.SystemSetMousePosition.
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

{ Define EGL to use EGL, cross-platform library to initialize OpenGL or OpenGLES
  context.
  It can work with WinAPI, Xlib, GTK backends of CastleWindow.

  By default, we define USE_EGL only when OpenGLES is defined.

  With regular OpenGL (not ES), it's better to use glX (Unix) / wgl (Windows)
  instead of EGL, that are practically guaranteed to be installed on Linux
  (if it has OpenGL at all) or Windows.

  - Unix: Though EGL is also practically almost guaranteed,
    with both OpenGL and OpenGLES support.
  - Windows, EGL can be found, but note: some implementations will
    not support OpenGL, will only support OpenGLES.
    Trying to use OpenGL will result in
    EGLContextNotPossible exception with message like "EGL: Cannot bind OpenGL API...".

  If you want, define below USE_EGL to always use EGL,
  with OpenGL or OpenGLES. }
{.$define USE_EGL}
{$ifdef OpenGLES}
  {$define USE_EGL}
{$endif}
// By default CASTLE_WINDOW_GTK_2 uses glX to initialize OpenGL context,
// but it's not available on Windows.
{$if defined(MSWINDOWS) and defined(CASTLE_WINDOW_GTK_2)}
  {$define USE_EGL}
{$endif}

{ Does backend implement TryVideoChange and VideoReset methods?
  (if this will not be defined, we will use TryVideoChange that always
  returns false and VideoReset that is NOOP). }
{$undef CASTLE_WINDOW_HAS_VIDEO_CHANGE}
{$ifdef CASTLE_WINDOW_WINAPI}
  {$define CASTLE_WINDOW_HAS_VIDEO_CHANGE}
{$endif}
{$ifdef CASTLE_WINDOW_XLIB}
  { Disable using XF86VMODE on macOS, because it seems that newer XQuarts
    does not provide it. }
  {$ifndef DARWIN}
    {$define CASTLE_WINDOW_HAS_VIDEO_CHANGE}
    {$define CASTLE_WINDOW_USE_XF86VMODE}
  {$endif}
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

{$ifdef CASTLE_WINDOW_COCOA}
  {$modeswitch objectivec1}
  {$modeswitch cblocks}
{$endif}

interface

uses {$define read_interface_uses}
  {$I castlewindow_backend.inc}
  {$undef read_interface_uses}
  { FPC units }
  SysUtils, Classes, Generics.Collections, CustApp, CTypes,
  { Castle Game Engine units }
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleVectors, CastleRectangles, CastleColors, CastleRenderOptions,
  CastleUtils, CastleClassUtils, CastleGLUtils, CastleImages, CastleGLImages,
  CastleKeysMouse, CastleStringUtils, CastleFilesUtils, CastleTimeUtils,
  CastleFileFilters, CastleUIControls, CastleInternalContextBase,
  CastleInternalPk3DConnexion, CastleParameters, CastleSoundEngine,
  CastleApplicationProperties;

{$define read_interface}

const
  { }
  WindowPositionCenter = -1000000;
  WindowDefaultSize = -1000000;

const
  DefaultDepthBits = 24;
  DepthBitsFallback = 16;

  DefaultFpsCaptionUpdateDelay = 1.0;

  DefaultLimitFPS = TCastleApplicationProperties.DefaultLimitFPS
    deprecated 'use TCastleApplicationProperties.DefaultLimitFPS';

type
  { Development notes:
    When extending TAntiAliasing, remember to also
    update ScreenEffectLibrary implementation to be able to handle them
    (and screen_effect_library.glsl to handle them in GLSL). }
  { Anti-aliasing values for TCastleWindow.AntiAliasing. }
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
  TCastleWindow = class;

  { Expose TCastleContainer type from CastleWindow unit, since almost all code using
    CastleWindow will need to use TCastleContainer type for callback parameter type. }

  { }
  TUIContainer = CastleUIControls.TCastleContainer deprecated 'use TCastleContainer';
  TCastleContainer = CastleUIControls.TCastleContainer;

  {$I castlewindowmenu.inc}

  { Type of message box, for TCastleWindow.MessageOK and TCastleWindow.MessageYesNo. }
  TWindowMessageType = (mtInfo, mtWarning, mtQuestion, mtError, mtOther);

  TUpdateFunc = procedure;
  TMenuClickFunc = procedure (Container: TCastleContainer; Item: TMenuItem);
  TMenuItemClickEvent = procedure (const Item: TMenuItem) of object;
  TDropFilesFunc = procedure (Container: TCastleContainer; const FileNames: array of string);
  TGLContextRetryOpenFunc = function (Window: TCastleWindow): boolean;

  TResizeAllowed = (raNotAllowed, raOnlyAtOpen, raAllowed);

  EGLContextNotPossible = class(Exception);

  TCaptionPart = (cpPublic, cpFps);

  { Container suitable to be used in TCastleWindow.
    Never create instances of this, just create TCastleWindow,
    and use container of it (in @link(TCastleWindow.Container)). }
  TWindowContainer = class(TCastleContainer)
  private
    Parent: TCastleWindow;
  public
    constructor Create(AParent: TCastleWindow); reintroduce;

    procedure Invalidate; override;
    function GLInitialized: boolean; override;
    function PixelsWidth: Integer; override;
    function PixelsHeight: Integer; override;
    function PixelsRect: TRectangle; override;
    function Focused: boolean; override;
    procedure SetInternalCursor(const Value: TMouseCursor); override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; override;
    procedure SystemSetMousePosition(const Value: TVector2); override;
    function SettingMousePositionCausesMotion: Boolean; override;
  end deprecated 'do not descend from this, instead use custom TCastleView descendants';

  {$define read_interface_types}
  {$I castlewindow_backend.inc}
  {$undef read_interface_types}

  { Window to render everything (3D or 2D) with Castle Game Engine.

    You should use this with TCastleView, following https://castle-engine.io/views
    and the rest of CGE manual.
    All user interface creation and event handling should be inside some view.

    By default, the window is filled with simple color from
    @link(TCastleContainer.BackgroundColor Container.BackgroundColor).

    If you're looking for an analogous Lazarus / Delphi component
    (that can be placed on a Lazarus / Delphi form)
    see @link(TCastleControl) component.
    Note that you cannot use both TCastleControl and TCastleWindow
    within the same application.
    See https://castle-engine.io/control_on_form .

    Deprecated functionality:
    You can actually add any @link(TCastleUserInterface) controls to
    the @link(Controls) property, not only TCastleView descendants. }
  TCastleWindow = class(TComponent)

  { Include CastleWindow-backend-specific parts of TCastleWindow class.
    Remember to explicitly specify the scope
    (usually "private") of things that you add to TCastleWindow class in backends,
    this is safest. Some backends may expose some protected or even public
    things that are specific for them. }

  {$define read_window_interface}
  {$I castlewindow_backend.inc}
  {$undef read_window_interface}

  protected
    {$warnings off} // using deprecated class in deprecated method
    { Create a container class for this window.
      Override this to use a custom container class, e.g. to override
      some container methods. }
    function CreateContainer: TWindowContainer; virtual; deprecated 'instead of custom TWindowContainer descendants, use custom TCastleView descendants';
    {$warnings on}
  private
    FWidth, FHeight, FLeft, FTop: Integer;
    { Window size reported last to DoResize,
      and not clamped with some internal constaints like ResizeAllowed
      of MaxWidth etc. }
    FRealWidth, FRealHeight: Integer;
    FOnCloseQuery: TContainerObjectEvent;
    FOnDropFiles: TDropFilesFunc;
    { FFullScreenWanted is the value set by FullScreen property by the user.
      FFullScreenBackend is the last value of FullScreen known to the backend
      (GTK, WinAPI etc.) }
    FFullScreenWanted, FFullScreenBackend: boolean;
    FDoubleBuffer: boolean;
    FDuringOpen: boolean;
    FResizeAllowed: TResizeAllowed;
    FFocused: boolean;
    FRedBits, FGreenBits, FBlueBits: Cardinal;
    FAutoRedisplay: boolean;
    FCaption: array [TCaptionPart] of string;
    BeforeFullScreenGeometryKnown: boolean;
    BeforeFullScreenLeft, BeforeFullScreenTop, BeforeFullScreenWidth, BeforeFullScreenHeight: Integer;
    { Track if some Invalidate was called (and not realized yet by
      DoRender call). Invalidate only sets this, leaving actually
      calling of DoRender for later. Call DoRender only of Invalidated = @true.

      Note: Xlib program should always wait for the first Expose event before
      redrawing anything. Similar for WinAPI. }
    Invalidated: boolean;

    { FClosed = are we outside of Open..Close }
    FClosed: boolean;

    { EventOpenCalled = has OnOpen been called from Open? }
    EventOpenCalled: boolean;

    MenuUpdateInside: Cardinal;
    MenuUpdateNeedsInitialize: boolean;
    MenuInitialized: boolean;

    FDepthBits: Cardinal;
    FStencilBits: Cardinal;
    FAlphaBits: Cardinal;
    FMultiSampling: Cardinal;
    FAntiAliasing: TAntiAliasing;
    FGtkIconName: String;
    FVisible: boolean;
    FMinWidth: Integer;
    FMinHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    // Using deprecated TWindowContainer - should be internal in the future
    {$warnings off}
    FContainer: TWindowContainer;
    {$warnings on}
    FCursor: TMouseCursor;
    FNamedParameters: TCastleStringList;
    { When Open, this says if the window actually has double-buffer. }
    HasDoubleBuffer: Boolean;
    { Ready TGLContextRequirements instance.
      Call UpdateRequirements to initialize its fields correctly. }
    FRequirements: TGLContextRequirements;
    function GetColorBits: Cardinal;
    procedure SetColorBits(const Value: Cardinal);
    procedure SetAntiAliasing(const Value: TAntiAliasing);
    procedure SetAutoRedisplay(const Value: boolean);
    function GetPublicCaption: String;
    procedure SetPublicCaption(const Value: String);
    procedure SetCaption(const Part: TCaptionPart; const Value: String);
    function GetWholeCaption: String;
    procedure SetCursor(const Value: TMouseCursor);
    function GetTouches(const Index: Integer): TTouch;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetResizeAllowed(const Value: TResizeAllowed);

    { Convert window position from the usual window system convention,
      where (0,0) is left-top, and the window has size FRealWidth/FRealHeight,
      to CGE convention where (0,0) is left-bottom. }
    function LeftTopToCastle(const V: TVector2): TVector2; overload;
    function LeftTopToCastle(const X, Y: Single): TVector2; overload;

    { Convert window position from the CGE convention to window system
      convention where (0,0) is left-top, also rounding it.
      This reverses LeftTopToCastle, and rounds. }
    function CastleToLeftTopInt(const V: TVector2): TVector2Integer;

    { Update FFullScreenBackend to FFullScreenWanted by closing and reopening
      the window (if needed, i.e. if FFullScreenBackend <> FFullScreenWanted). }
    procedure SimpleUpdateFullScreenBackend;

    { Set FFullScreenBackend to FFullScreenWanted.
      Every backend must implement this. In the simplest case,
      just call SimpleUpdateFullScreenBackend. }
    procedure UpdateFullScreenBackend;

    function GetMousePosition: TVector2;
    procedure SetMousePosition(const Value: TVector2);
    procedure SetFullScreenWanted(const Value: Boolean);

    { Implement in backends to react to requests of mouse position
      by code. See @link(TCastleContainer.SystemSetMousePosition). }
    procedure SystemSetMousePosition(const Value: TVector2);

    { Used in particular backend, open OpenGL context and do
      Application.OpenWindowsAdd(Self) there.

      Properties that should be used in OpenBackend implementation to determine
      the window:

      - Width, Height, Left, Top
      - Cursor,
      - FullScreen
        (Note that FFullScreenWanted and FFullScreenBackend are always
        equal at this point, so you can read any of these fields.
        You can also just read FullScreen property.)
      - ResizeAllowed
        (DoResize already implements appropriate
        checks, but implementation should provide user with visual clues that
        the window may / may not be resized)
      - MainMenu (display MainMenu and provide way to call DoMenuClick)

      OpenGL context must be initialized honouring these properties:

      - DoubleBuffer,
      - StencilBits,
      - DepthBits,
      - AlphaBits,
      - MultiSampling }
    procedure OpenBackend;

    { Close OpenGL context, for particular backend.

      No need to call OpenWindowsRemove here, it's done by universal Close already.
      It's advised (although not totally required) that all errors during
      CloseBackend should be caught and cause only WritelnWarning.
      Reasoning: Close should, regardless of trouble, try to finalize as much
      as possible. }
    procedure CloseBackend;

    { Make the OpenGL context of this window current (active for following
      OpenGL commands). }
    procedure BackendMakeCurrent;

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

    { Called from DoUpdate. Backends may fill this with code to be done
      on each update, regardless of how update is performed,
      e.g. this will be called also if something calls FOpenWindows.DoUpdate in
      TCastleApplication. }
    procedure BackendInsideUpdate;

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

      The implementation (usually just hardcoded true or false result)
      is backend-specific.

      - If @true, and in DoKeyDown we get some key event that specifies menu item,
        then DoKeyDown calls DoMenuClick (and not calls EventKeyDown).

      - If @false, then DoKeyDown doesn't check whether key corresponds to menu item.
        It just calls EventKeyDown. }
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
      - updating state of Container.MousePressed
      - updating state of Container.Pressed (Pressed.Keys, Pressed.Characters etc.)
      - updating state of Container.MousePosition
      - calling MakeCurrent before every EventXxx
      - flushing gl commands (and swapping gl buffers when DoubleBuffer'ing)
      - taking care of AutoRedisplay
      - updating Width, Height (and updating it with accordance to
        Min/MaxWidth/Height and ResizeAllowed)
      - checking MainMenu.Enabled
    }

    { DoResize with FirstResizeAfterOpen = true is called only once
      (and exactly once) from TCastleWindow.Open implementation.
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

      Remember: this function does not automatically call Invalidate.
      You must make sure that soon after changing size of the window
      you will call DoRender (e.g. you can call Invalidate after
      calling DoResize; but usually (under WinAPI, Xlib, GTK)
      it's not needed, i.e. WinAPI, Xlib, and GTK all take care of this
      automatically). }
    procedure DoResize(AWidth, AHeight: integer; FirstResizeAfterOpen: boolean);

    { Called by a backend when user wants to close the window
      (e.g. by pressing the special "close" button on the window manager border).
      It is possible to ignore it (see OnCloseQuery docs).
      This calls OnCloseQuery and then eventually Close
      (and Close will execute OnClose, CloseBackend etc.).

      Note that there is no "DoClose" method defined.
      You should always call DoCloseQuery. }
    procedure DoCloseQuery;

    { Do MakeCurrent,
         EventBeforeRender,
         EventRender (inside Fps.InternalRenderBegin/End)
         flush gl command pipeline (and swap gl buffers if DoubleBuffer)

      - Take care of AutoRedisplay, like

          @code(if AutoRedisplay then Invalidate;)

        So specific CastleWindow backends need not to worry about
        AutoRedisplay. They only have to call this when Invalidated = @true. }
    procedure DoRender;

    { DoKeyDown/Up: pass here key that is pressed down or released up.

      Only DoKeyDown: pass also KeyString. Pass Key = keyNone if this is not
      representable as TKey, pass KeyString = '' if this is not representable
      as char. But never pass both Key = keyNone and KeyString = ''
      (this would mean that nothing is pressed, at least nothing that can be represented
      in CGE).

      Only DoKeyUp: never pass Key = keyNone.

      If you call DoKeyUp while (not Pressed[Key]) it will be ignored
      (will not do any EventRelease etc. - just NOOP).

      This will
         update Pressed (Pressed.Keys, Pressed.Characters, etc.) accordingly,
         DoKeyDown: may here call DoMenuClick
           (and then it will NOT call MakeCurrent and EventKeyDown)
         MakeCurrent,
         EventKeyDown/Up.
    }
    procedure DoKeyDown(const Key: TKey; const KeyString: String);
    procedure DoKeyUp(const key: TKey);
    procedure DoMotion(const Event: TInputMotion);
    procedure DoMouseDown(const Position: TVector2;
      const AButton: TCastleMouseButton; const FingerIndex: TFingerIndex);
    procedure DoMouseUp(const Position: TVector2;
      const AButton: TCastleMouseButton; const FingerIndex: TFingerIndex);
    procedure DoMouseWheel(const Scroll: Single; const Vertical: Boolean);
    { Just call it when user presses some MenuItem.
      This takes care of
      - MainMenu.Enabled (and if menu item was activated using key shortcut,
        but the menu is disabled, we pass key to EventKeyDown)
      - MakeCurrent,
      - Item.DoClick,
      - OnMenuClick,
      - OnMenuItemClick }
    procedure DoMenuClick(Item: TMenuItem);

    procedure DoDropFiles(const FileNames: array of string);
    function MessageReceived(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;

    { Just like FileDialog, but these always get and should set FileName,
      not an URL. Also, for OpenDialog, we make sure that initial FileName
      contains only a path (not the final file name), since this is
      good behavior for users (even if some API allow to set proposed
      file name). }
    function BackendFileDialog(const Title: String; var FileName: String;
      OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean; overload;

    procedure OpenCore;
    { Current OpenGL buffers configuration required.
      Stuff like DoubleBuffer, AlphaBits, DepthBits,
      StencilBits etc.
      This simply returns a text description of these properties.

      It does not describe the current OpenGL context parameters.
      (It doesn't even need an OpenGL context open.)

      Useful for constructing messages e.g. for EGLContextNotPossible exceptions. }
    function RequestedBufferAttributes: String;

    { Check do given OpenGL buffers configuration satisfies the
      requested configuration.

      So it checks does

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
    procedure CheckRequestedBufferAttributes(const ProviderName: String;
      ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
      ProvidedMultiSampling: Cardinal);

    { Update FRequirements to reflect current window properties. }
    procedure UpdateRequirements;
  protected
    procedure DoUpdate; virtual;
  public
    property Container: TWindowContainer read FContainer;

    { Is it allowed to suspend (for an indefinite amount of time) waiting
      for user input.

      Allowing this is a good thing, as it means our process doesn't eat
      your CPU when it simply waits, doing nothing, for user input.
      On the other hand, you cannot allow this if you want to do some
      things continuously, regardless of user input.

      The default implementation plays it safe, and does not allow suspending
      if we have OnUpdate callback defined. }
    function AllowSuspendForInput: boolean; virtual;

    { Size of the window OpenGL area. Together with frame and border
      sizes, and eventually menu bar size, this determines the final
      window size.

      When the window is open, these are read-only (may only change
      through internal methods, that is: we'll update @link(Width), @link(Height),
      @link(Left), @link(Top) to reflect current size and position).

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
      means: at @link(Open), calculate and use some comfortable window size.

      TODO: Make it possible to set these properties while the window is open.

      @groupBegin }
    property Width: integer read FWidth write SetWidth default WindowDefaultSize;
    property Height: integer read FHeight write SetHeight default WindowDefaultSize;
    { @groupEnd }

    { Rectangle representing the inside of this container.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to window
      sizes. }
    function Rect: TRectangle;

    { Window position on the screen. If one (or both) of them is equal
      to WindowPositionCenter at the initialization (Open) time,
      then it will be set to position the window at the screen center.

      TODO: Make it possible to set these properties while the window is open.

      @groupBegin }
    property Left: integer read FLeft write SetLeft default WindowPositionCenter;
    property Top :integer read FTop write SetTop default WindowPositionCenter;
    { @groupEnd }

    { Is the window fullscreen.

      A fullscreen window has @link(Width), @link(Height), @link(Left), @link(Top)
      set to fill the whole screen.
      The window style is also, if possible, borderless.

      The window size constraints (like MinWidth, MaxWidth, MinHeight, MaxHeight
      and ResizeAllowed) are ignored, that is: we do not check whether
      screen size fits inside MinWidth and MaxWidth. If this is @true,
      the window is always fullscreen.
      However, the sizes visible to your application
      (exposed in properties @link(Width), @link(Height))
      are still constrained by MinWidth, MaxWidth, MinHeight, MaxHeight
      and ResizeAllowed.

      It is best to adjust this property before the window is open,
      this way window will be immediately open in the full-screen size.
      In the standard case (see https://castle-engine.io/manual_cross_platform.php
      about a typical initialization) you should place
      @code(Window.FullScreen := true) inside the unit initialization section.

      You can also change this property after the window is open.
      But note that:

      @unorderedList(
        @item(Some backends require closing + reopening the window to make it
          fullscreen. So be prepared that changing FullScreen
          may result in EventClose + EventOpen events, and all OpenGL resources
          are reloaded. In most cases, engine takes care of everything
          automatically (all TCastleScene, TCastleUserInterface, TDrawableImage and other
          resources are automatically reloaded), just be aware that
          this operation may take a bit of time.
        )

        @item(For safety (because of the above limitation), changing
          this property does not have an immediate effect on the window.
          The actual fullscreen change will happen a bit later, from the message loop.
        )
      )
    }
    property FullScreen: boolean
      read FFullScreenWanted write SetFullScreenWanted default false;

    { Deprecated, instead just do @code(FullScreen := not FullScreen). }
    procedure SwapFullScreen; deprecated 'use "FullScreen := not FullScreen"';

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

    { Current mouse position.
      In case of touch devices, this reports the last known position
      for FingerIndex = 0, and setting this has no effect.

      See @link(TTouch.Position) for a documentaion how this is expressed.

      In all situations the MousePosition is the latest known mouse position.
      The only exception is within EventMotion (and so, also in OnMotion
      callback): MousePosition is then the previous known mouse position,
      while new mouse position is provided as NewMousePosition argument to
      EventMotion (and OnMotion).

      @italic(About setting the mouse position:)

      @unorderedList(
        @item(There is no guarantee that the position was set
          exactly to what was requested. Various backends have their
          limitations, and position may be rounded, and almost everywhere
          position will be clamped to the current screen space.)

        @item(It is undefined whether setting mouse position
          will generate an OnMotion event (just as if the
          user moved the mouse). Some backends do it, some don't,
          and there is no way to make it consistent (because
          backend may report the motion event with delay, so we really
          don't know whether user moved the mouse or was it caused
          by code).)

        @item(Setting mouse position is always ignored when
          the window is closed.)
      ) }
    property MousePosition: TVector2
      read GetMousePosition write SetMousePosition;
      {$ifdef FPC} deprecated 'use Container.MousePressed'; {$endif}

    { Currently active touches on the screen.
      This tracks currently pressed fingers, in case of touch devices (mobile, like Android and iOS).
      In case of desktops, it tracks the current mouse position, regardless if any mouse button is
      currently pressed.

      Indexed from 0 to TouchesCount - 1.
      @seealso TouchesCount
      @seealso TTouch }
    property Touches[const Index: Integer]: TTouch read GetTouches;
      {$ifdef FPC} deprecated 'use Container.Touches'; {$endif}

    { Count of currently active touches (mouse or fingers pressed) on the screen.
      @seealso Touches }
    function TouchesCount: Integer;
      deprecated 'use Container.TouchesCount';

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
      ) }
    property ResizeAllowed: TResizeAllowed
      read FResizeAllowed write FResizeAllowed default raAllowed;

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

      Default value is @link(DefaultDepthBits),
      which is non-zero and a reasonable default for 3D programs
      that want to work with depth test enabled.

      Note that we have a fallback mechanism in case @link(DepthBits)
      is too large: we fallback on @link(DepthBitsFallback) then.

      @italic(Design notes:) Why default value is not 0?

      @orderedList(
        @item(
          Most programs using OpenGL use 3D and so use depth testing.
          So many programs
          would have to call something like @code(Window.DepthBits := DefaultDepthBits).
        )

        @item(
          Often graphic cards / window systems / OSes give you an OpenGL
          context with depth buffer @italic(even if you don't need depth buffer).
          This makes it easy to forget about setting DepthBits to something
          non-zero, because on @italic(your) system you may happen
          to always get some depth buffer.
        )
      )

      If you are writing a program that does not need depth buffer
      you can set Window.DepthBits := 0, to inform OpenGL it doesn't need
      to allocate any depth buffer.
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
      and you got 16-bits stencil buffer: StencilBits value will still remain 8.
      This is sensible in case you close the window, tweak some settings
      and try to open it again. Use @code(glGetInteger(GL_STENCIL_BITS))
      when window is open to query current (actual) buffer size. }
    property StencilBits: Cardinal
      read FStencilBits write FStencilBits default DefaultStencilBits;

    { How many samples are required for multi-sampling (anti-aliasing).

      Set @link(AntiAliasing) instead of this for more comfortable
      (higher-level) way to turn on multi-sampling (anti-aliasing).
      Setting @link(AntiAliasing) will also set this property.

      1 means that no multi-sampling is required.
      Values larger than 1 mean that we require OpenGL context with
      multi-sampling capabilities. Various GPUs may support various
      values (it's a trade-off between quality and speed),
      try typical values 2 or 4.

      You can enable/disable anti-aliasing in your program by code like

      @longCode(#
        if GLFeatures.Multisample then glEnable(GL_MULTISAMPLE);
        if GLFeatures.Multisample then glDisable(GL_MULTISAMPLE);
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
      @link(Open) will raise an error. }
    property AlphaBits: Cardinal
      read FAlphaBits write FAlphaBits default 0;

    { Name of the icon for this window used by GTK 2 backend.

      This is simply passed to @code(gtk_window_set_icon_name),
      see [http://library.gnome.org/devel/gtk/stable/GtkWindow.html#gtk-window-set-icon-name].
      This allows you to use an installed icon (in /usr/share/icons/
      or ~/.local/share/icons/) for your program. See
      [http://library.gnome.org/devel/integration-guide/stable/icons.html.en]
      for short information how and where to install your icons.

      It's ignored on non-GTK 2 backends. }
    property GtkIconName: String read FGtkIconName write FGtkIconName;

    (*Should this window be actually displayed on the desktop.
      In all normal programs you want to leave this as @true, as the
      main purpose of the window is to actually be visible and interactive
      on the desktop.

      Setting this to @false allows you to get an OpenGL context without
      showing anything on the desktop. This can be used for rendering
      and capturing OpenGL stuff without showing it on the desktop.
      One example is the @--screenshot option of castle-model-viewer, see
      [https://castle-engine.io/castle-model-viewer].

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

    { Caption of the window.
      By default it's initialized from ApplicationProperties.Caption or (if empty)
      ApplicationName.
      May be changed even when the window is already open. }
    property Caption: String read GetPublicCaption write SetPublicCaption;

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
    property OnCloseQuery: TContainerObjectEvent read FOnCloseQuery write FOnCloseQuery;

    { Send fake motion event, without actually moving the mouse through the backend.
      This is useful only for automatic tests.
      @exclude }
    procedure InternalFakeMotion(const Event: TInputMotion);

    { Called when user drag and drops file(s) on the window.
      In case of macOS bundle, this is also called when user opens a document
      associated with our application by double-clicking.

      Note: this is currently supported only by Form and Cocoa backends
      of TCastleWindow, see https://castle-engine.io/castlewindow_backends . }
    property OnDropFiles: TDropFilesFunc read FOnDropFiles write FOnDropFiles;

    { Should we automatically redraw the window all the time,
      without the need for an @link(Invalidate) call.
      If @true (the default), render events will be called as often as reasonable
      on this system, usually 60 times per second.

      If your game may have a still screen (nothing animates),
      then this approach is a little unoptimal, as we use CPU and GPU
      for drawing, when it's not needed. In such case, you can set this
      property to @false, and make sure that you call
      @link(Invalidate) always when you need to redraw the screen.
      Note that the engine components always call @link(Invalidate) when
      necessary, so you really only need to call @link(Invalidate) yourself
      if you make custom rendering in some @link(TCastleUserInterface.OnRender). }
    property AutoRedisplay: boolean read FAutoRedisplay write SetAutoRedisplay
      default true;

    { -------------------------------------------------------------------------
      Menu things (menu may be modified at runtime, everything will be
      automatically properly redisplayed etc.) }

  private
    FMainMenu: TMenu;
    FMainMenuVisible: boolean;
    FOwnsMainMenu: boolean;
    FOnMenuClick: TMenuClickFunc;
    FOnMenuItemClick: TMenuItemClickEvent;
    FUserData: Pointer;
    procedure SetMainMenu(Value: TMenu);
  public
    { Menu bar of this window.
      When not assigned, we have no menu bar.

      Note that MainMenu.Caption will be ignored.

      You can change this freely while Closed.

      You can change this almost freely while not Closed: you can use
      various properties of TMenuEntry descendants (adding, deleting items
      from TMenu, changing Caption, Key, KeyString, Checked properties --
      anything) and you can change value of MainMenu BUT you must not
      change MainMenu <> nil state when the window is not Closed.
      I.e. if you called Open with MainMenu = nil, then MainMenu must stay
      nil unit Close. If you called Open with MainMenu <> nil, then you
      can assign other MainMenu values while not Closed, but only values
      <>nil. I.e. you can't set MainMenu to nil if you called Open
      with MainMenu <> nil.
      See @code(examples/window/window_menu/)
      for demo of changing value of MainMenu while window is not Closed.

      Note that MainMenu.Enabled is honoured (as well as Enabled
      for all menu items inside, of course).
      You can use this to disallow user from clicking on the whole
      menu. When MainMenu.Enabled = @false then
      menu click is not called
      (no MenuItem.DoClick, no OnMenuClick, no OnMenuItemClick)
      when user presses some menu item.
      When user presses some keyboard shortcut for some menu item,
      also no menu click is not called
      (no MenuItem.DoClick, no OnMenuClick, no OnMenuItemClick)
      and we make normal EventPress.

      Disabling MainMenu is useful e.g. during modal dialog box, like @link(MessageOk).
      This way you can force use to interact with the modal box. }
    property MainMenu: TMenu read FMainMenu write SetMainMenu;

    { Is MainMenu visible. @false means that we do not show main menu bar,
      but menu key shortcuts should still work.
      Right now, you can reliably change this only before window is open. }
    property MainMenuVisible: boolean
      read FMainMenuVisible write FMainMenuVisible default true;

    { If true then the @link(MainMenu) will automatically freed when this
      TCastleWindow instance is freed. }
    property OwnsMainMenu: boolean read FOwnsMainMenu write FOwnsMainMenu default true;

    { User clicked (enabled) menu item
      (and it wasn't already handled by TMenuItem.DoClick).

      By default, menu item handling is passed to TMenuItem.DoClick.
      Only when it returns @false (not handled) then we call this event. }
    property OnMenuClick: TMenuClickFunc read FOnMenuClick write FOnMenuClick;
      {$ifdef FPC} deprecated 'use OnMenuItemClick'; {$endif}

    { User clicked (enabled) menu item
      (and it wasn't already handled by TMenuItem.DoClick).

      By default, menu item handling is passed to TMenuItem.DoClick.
      Only when it returns @false (not handled) then we call this event. }
    property OnMenuItemClick: TMenuItemClickEvent read FOnMenuItemClick write FOnMenuItemClick;

    { @section(Mouse state) -------------------------------------------------- }

    { Mouse buttons currently pressed.
      See @link(TCastleContainer.MousePressed) for details. }
    function MousePressed: TCastleMouseButtons;

    { Is the window focused now, which means that keys/mouse events
      are directed to this window. }
    property Focused: boolean read FFocused;

    { Place for your pointer, for any purposes.
      No code in this unit touches the value of this field.
      This is similar to TComponent.Tag property. }
    property UserData: Pointer read FUserData write FUserData;

    property Closed: boolean read FClosed default true;

    { Mouse cursor appearance over this window.
      See TMouseCursor for a list of possible values and their meanings.

      Note that this is for internal usage in the engine. In your applications,
      you should set TCastleUserInterface.Cursor on any UI control (including on TCastleView),
      never set this property directly. }
    property InternalCursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { List of user-interface controls currently active.
      See @link(TCastleContainer.Controls) for details. }
    function Controls: TInternalChildrenControls;

    { Is the OpenGL context initialized. This is equivalent to @code(not Closed),
      which means we are between an @link(Open) and @link(Close) calls. }
    function GLInitialized: boolean;

    { Create the window with associated rendering context and show it.

      @unorderedList(
        @item(Create window with a rendering area, optionally with a menu bar.)
        @item(Create rendering context.)
        @item(Show the window.)
        @item(Call @link(GLInformationInitialize) to initialize @link(GLVersion), @link(GLFeatures),
          show them in log (https://castle-engine.io/manual_log.php).)

        @item(Initial events called:
          @unorderedList(
            @itemSpacing Compact
            @item Call MakeCurrent, EventOpen (OnOpen)
            @item Call MakeCurrent, EventResize (OnResize)
            @item(Call MakeCurrent once again, to be sure that after Open
              active rendering context is the one associated with newly created
              window (in case you would change active rendering context inside
              EventResize (OnResize), which is allowed).)
          )
        )
      )

      Call to this method is ignored if the window is already open
      (if @link(Closed) = @false).

      @raises(EGLContextNotPossible
        If it's not possible to obtain
        OpenGL context with specified attributes.
        For example, maybe you set AlphaBits, DepthBits, StencilBits
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
    procedure Open; overload;

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
    procedure Open(const Retry: TGLContextRetryOpenFunc); overload;

    { Close window.

      @unorderedList(
        @item(Calls OnClose.)
        @item(Hides window, destroys it.)
        @item(
          if this was the only open TCastleWindow window
          and QuitWhenLastWindowClosed = true then
          this calls Application.Terminate.)
      )

      Note that often there's no need to call Close explicitly in your program,
      because in destructor of this object we call Close, to be sure
      that window is closed.

      TODO: zrobic param boolean CloseFromDestroyQuitWhenLastWindowClosed?
      As for now Close from destructor is called always with
      QuitWhenLastWindowClosed = true.

      Call to Close is ignored if window is already Closed. }
    procedure Close(const QuitWhenLastWindowClosed: boolean = true);

    { @deprecated Deprecated name for @link(Invalidate). }
    procedure PostRedisplay; deprecated;

    { See TCastleContainer.Invalidate. }
    procedure Invalidate;

    { Make the OpenGL context of this window @italic(current). Following OpenGL
      commands will apply to this context, and the @link(CastleRenderContext.RenderContext RenderContext)
      will also refer to this.
      When the window is opened, and right
      before calling any window callback, we always automatically call
      this, so you should not need to call this method yourself
      in normal circumstances. }
    procedure MakeCurrent;

    { Capture the current window contents to an image (file).

      These functions take care of making a redraw before capturing screen
      contents. That's because you can only reliably capture the screen contents
      of the back buffer (before swap) using OpenGL. In theory, the single-buffer
      case could be optimized (do not redraw if not needed, that is:
      if not invalidated), but it's not worth the complication since noone uses
      single-buffer for normal applications... And also, there is no reliable
      way to capture screen contents in case of single-buffer.

      Note that only capturing the double-buffered windows (the default)
      is reliable.
      @groupBegin }
    procedure SaveScreen(const Url: String); overload;
    function SaveScreen: TRGBImage; overload;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload;
    function SaveScreenToGL(const SmoothScaling: boolean = false): TDrawableImage; overload;
    function SaveScreenToGL(const SaveRect: TRectangle;
      const SmoothScaling: boolean = false): TDrawableImage; overload;
    { @groupEnd }

    { Color buffer where we draw, and from which it makes sense to grab pixels.
      Use only if you save the screen using low-level SaveScreen_NoFlush function.
      Usually, you should save the screen using the simpler @link(SaveScreen) method,
      and then the @name is not useful. }
    function SaveScreenBuffer: TColorBuffer;

    { Asks and saves current screenshot.
      Asks user where to save the file (using @link(FileDialog),
      as default URL taking ProposedUrl).
      If user accepts calls Window.SaveScreen.
      In case of problems with saving, shows a dialog (doesn't raise exception). }
    procedure SaveScreenDialog(ProposedUrl: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    { Keys currently pressed. }
    function Pressed: TKeysPressed; deprecated 'use Container.Pressed';

    { Measures application speed. }
    function Fps: TFramesPerSecond; deprecated 'use Container.Fps';

    { OpenAndRun stuff --------------------------------------------------------- }

    { Do @link(Open) (initialize rendering context, show the window)
      followed by @link(TCastleApplication.Run Application.Run) (run the event loop). }
    procedure OpenAndRun;

    { Parsing parameters ------------------------------------------------------- }

    { Parse some command-line options and remove them from @link(Parameters)
      list. See https://castle-engine.io/opengl_options.php for
      documentaion what these options actually do from user's point of view.

      @definitionList(
        @itemLabel @--fullscreen
        @item(Sets FullScreen to @true.)

        @itemLabel @--window
        @item(Sets FullScreen to @false.)

        @itemLabel @--geometry WIDTHxHEIGHT<sign>XOFF<sign>YOFF
        @item(Sets FullScreen to @false and sets window position and size:
          @link(Width), @link(Height), @link(Left), @link(Top).)

        @itemLabel @--fullscreen-custom WIDTHxHEIGHT
        @item(Change desktop resolution by VideoChange and sets FullScreen to @true.
          Changing desktop resolution is not implemented on all platforms.)

        @itemLabel @--pretend-touch-device
        @item(Set @link(TCastleApplicationProperties.TouchDevice
          ApplicationProperties.TouchDevice) to true.
          See @url(https://castle-engine.io/touch_input touch input documentation).)
      )

      @raises(EInvalidParams When some of our options have invalid arguments.) }
    procedure ParseParameters;

    { Help text for options handled by ParseParameters.

      Returned string may be multiline, but it does not contain
      the trailing newline (newline char after the last line). }
    class function ParseParametersHelp: String;

    { Select a file to open or save, using native (looks familiar on a given system) dialog box.
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
            you should usually use our UrlSaveStream.)
        )
      )

      @param(FileFilters A set of file filters to present to user.
        Pass @nil (default) if you do not want to use file file filters,
        so user will just always see everything. An overloaded version
        allows you to pass file filters encoded in a single string,
        this may be slightly more comfortable for call, see
        TFileFilterList.AddFiltersFromString
        for explanation how to encode filters in a string.)

      As with all the model dialog methods here, like FileDialog,
      ColorDialog, MessageOK, MessageYesNo:

      @unorderedList(
        @item(
          The events of this TCastleWindow will not happen
          while we are inside a modal dialog box.

          We have a special code that temporarily
          disables all UI controls on the @link(Controls) list
          (so your TCastleUserInterface, TCastleTransform, TCastleView etc.
          instances will @italic(not) have their methods,
          like @code(Update), called).
        )

        @item(
          The events of @link(Application) and (in case you have multiple
          windows open) the events of other TCastleWindow @italic(may)
          (but do not have to) happen, while the dialog box is open.

          So be prepared there to handle the situation that this window may be "stuck"
          in a modal dialog box.
        )
      )

      @groupBegin }
    function FileDialog(const Title: String; var Url: String;
      OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean; overload;
    function FileDialog(const Title: String; var Url: String;
      OpenDialog: boolean; const FileFilters: String): boolean; overload;
    { @groupEnd }

    { Choose a color, using native (looks familiar on a given system) dialog box.

      Initial value of Color specifies initial color proposed to the user.
      If user accepts, we return true and set Color accordingly, else
      we return false (and do not modify Color).

      Overloaded version with TCastleColor specifies a color with alpha.
      But note that only some backends actually allow user to adjust alpha
      (others leave alpha unchanged).
      Backends that allow alpha editing now are: Cocoa, Xlib.

      @groupBegin }
    function ColorDialog(var Color: TCastleColor): Boolean; overload;
    function ColorDialog(var Color: TCastleColorRGB): Boolean; overload;
    function ColorDialog(var Color: TVector3Byte): Boolean; overload;
    { @groupEnd }

    { Show some information and just ask to press "OK",
      using native (looks familiar on a given system) dialog box. }
    procedure MessageOK(const S: String; const MessageType: TWindowMessageType);

    { Ask a yes/no question, using native (looks familiar on a given system) dialog box. }
    function MessageYesNo(const S: String;
      const MessageType: TWindowMessageType = mtQuestion): boolean;

    { Named parameters used to initialize this window.
      Right now not used (were used by NPAPI plugin, may be useful to new web target). }
    property NamedParameters: TCastleStringList read FNamedParameters;
  private
    LastFpsOutputTime: TTimerResult;
    FFpsShowOnCaption: boolean;
    FSwapFullScreen_Key: TKey;
    FClose_KeyString: String;
    FFpsCaptionUpdateDelay: Single;
  public
    { Show current frames per second on window caption.
      You can modify this property only @italic(before calling @link(Open).) }
    property FpsShowOnCaption: boolean
      read FFpsShowOnCaption write FFpsShowOnCaption default false;

    { Key to use to switch between FullScreen and not FullScreen.
      Set to keyNone (default) to disable this functionality.
      Suggested value to enable this functionality is keyF11, this is consistent
      will fullscreen key in other programs.
      You can freely modify it at any time, even after calling @link(Open).

      The fullscreen is switched by closing it, changing @link(FullScreen)
      property and opening it again. So be sure to have good OnOpen / OnClose
      implementations: you have to be able to recreate in OnOpen everything
      that was released in OnClose. }
    property SwapFullScreen_Key: TKey
      read FSwapFullScreen_Key write FSwapFullScreen_Key default keyNone;

    { Key to use to close the window.
      Set to '' (default) to disable this functionality.
      Suggested value to enable this functionality is CharEscape.
      You can freely modify it at any time, even after calling @link(Open). }
    property Close_KeyString: String
      read FClose_KeyString write FClose_KeyString;

    { The amount of time (in seconds) between updating Caption
      with current FPS value. Used when FpsShowOnCaption.

      You probably don't want to change this -- the default value
      is synchronized with how often the @link(TFramesPerSecond) actually
      change. So there's no point in making this smaller (you may cause
      slowdowns, and you will not see anything better). }
    property FpsCaptionUpdateDelay: Single
      read FFpsCaptionUpdateDelay write FFpsCaptionUpdateDelay
      {$ifdef FPC}default DefaultFpsCaptionUpdateDelay{$endif};

    { Configure some options typically used by "demo" applications. }
    procedure SetDemoOptions(const ASwapFullScreen_Key: TKey;
      AClose_KeyString: String;
      const AFpsShowOnCaption: boolean);
  end;

  TCastleWindowCustom = TCastleWindow deprecated 'use TCastleWindow';
  TCastleWindowBase = TCastleWindow deprecated 'use TCastleWindow';

  TWindowList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleWindow>)
  private
    { Call OnUpdate for all windows on this list. }
    procedure DoUpdate;
  public
    { Simply calls Invalidate on all items. }
    procedure Invalidate;
  end;

  TCastleWindowClass = class of TCastleWindow;

  { Application, managing all open TCastleWindow (OpenGL windows).
    This tracks all open instances of TCastleWindow
    and implements message loop. It also handles some global tasks
    like managing the screen (changing current screen resolution and/or
    bit depth etc.)

    The only instance of this class should be in @link(Application) variable.
    Don't create any other instances of class TCastleApplication, there's no
    point in doing that. }
  TCastleApplication = class(TCustomApplication)

  { Include CastleWindow-backend-specific parts of
    TCastleApplication class. Rules and comments that apply here are
    the same as in analogous place at TCastleWindow class,
    when read_window_interface is defined. }

  {$define read_application_interface}
  {$I castlewindow_backend.inc}
  {$undef read_application_interface}

  private
    FOnInitialize{, FOnInitializeJavaActivity}: TProcedure;
    FOnInitializeEvent: TNotifyEvent;
    Initialized, InitializedJavaActivity: boolean;
    FVideoColorBits: integer;
    FVideoFrequency: Cardinal;
    { Current window with OpenGL context active.
      Update in TCastleWindow.MakeCurrent, also TCastleWindow.Close. }
    Current: TCastleWindow;
    LastLimitFPSTime: TTimerResult;
    FMainWindow: TCastleWindow;
    //FUserAgent: String;

    FOpenWindows: TWindowList;
    function GetOpenWindows(Index: integer): TCastleWindow;
    { Run @link(OnInitialize) and
      @link(TCastleApplicationProperties.OnInitializeJavaActivity) callbacks,
      if not run yet.

      Called CastleEngineInitialize, not just @code(Initialize),
      because this is something entirely different from inherited
      TCustomApplication.Initialize. In particular, this should not
      be ever called by user code --- TCastleWindow implementation
      takes care to call it automatically. }
    procedure CastleEngineInitialize;

    { Use MainWindow, or a guessed window supposed to be the main. }
    function GuessedMainWindow: TCastleWindow;

    { Add new item to OpenWindows.
      Windows must not be already on OpenWindows list. }
    procedure OpenWindowsAdd(Window: TCastleWindow);

    { Delete window from OpenWindows.

      Given Window doesn't have to be on the OpenWindows list. If it is not, this
      method is NOOP. This is useful when this is called from TCastleWindow.Close
      because TCastleWindow.Close should work even for partially constructed
      Windows.

      If Window was present on OpenWindows and after removing Window
      OpenWindowsCount = 0 and QuitWhenLastWindowClosed then it calls Quit. }
    procedure OpenWindowsRemove(Window: TCastleWindow; QuitWhenLastWindowClosed: boolean);

    { Find window on the OpenWindows list. Returns index, or -1 if not found. }
    function FindWindow(Window: TCastleWindow): integer;

    procedure CreateBackend;
    procedure DestroyBackend;

    { Make sure that event loop exits as soon as possible.

      At this point, we already set "Terminated" (from ancestor
      TCustomApplication) to @true. And your methods like "ProcessMessage"
      should check it, so no need to do anything special in BackendTerminate
      to make them exit.

      However, some backends execute some native code to run a loop,
      like CASTLE_WINDOW_FORM may run FormApplication.Run
      or CASTLE_WINDOW_GTK may run (though doesn't now) gtk_main.
      You need to break these loops in this method. }
    procedure BackendTerminate;

    { Call OnUpdate on all open windows,
      call ApplicationProperties.OnUpdate,
      and call OnRender on all necessary windows.
      This allows some backends to easily do everything that typically needs
      to be done continuosly (without the need for any message from the outside). }
    procedure UpdateAndRenderEverything(out WasAnyRendering: boolean); overload;
    procedure UpdateAndRenderEverything; overload;

    procedure MarkSleeping;

    { Can we wait (hang) for next message.
      See TCastleWindow.AllowSuspendForInput, this is similar but for
      the whole Application. Returns @true only if all open
      windows allow it and we don't have any
      @link(TCastleApplicationProperties.OnUpdate ApplicationProperties.OnUpdate). }
    function AllowSuspendForInput: boolean;

    procedure DoLimitFPS;
    procedure SetMainWindow(const Value: TCastleWindow);

    { Close all open windows, make ProcessMessage return @false,
      finish the @link(Run) method (if working), and thus finish the
      application work. }
    procedure CloseAllOpenWindows;

    {$ifdef FPC}
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    function GetTouchDevice: boolean;
    procedure SetTouchDevice(const Value: boolean);
    function GetLimitFPS: Single;
    procedure SetLimitFPS(const Value: Single);
    {$endif}
    function GetMainContainer: TCastleContainer;
  protected
    { Override TCustomApplication to pass TCustomApplication.Log
      to CastleLog logger. }
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    { Every backend must override this. TCustomApplication will
      automatically catch exceptions occuring inside DoRun. }
    procedure DoRun; override;
  public
    { If VideoResize, then next VideoChange call will
      try to resize the screen to given VideoResizeWidth /
      VideoResizeHeight. Otherwise, next TryVideoChange and VideoChange will
      use default screen size.
      @groupBegin }
    VideoResize: Boolean;
    VideoResizeWidth,
    VideoResizeHeight: Integer;
    { @groupEnd }

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Color bits per pixel that will be set by next VideoChange call,
      and that are tried to be used at TCastleWindow.Open.
      Zero means that system default is used. }
    property VideoColorBits: integer read FVideoColorBits write FVideoColorBits default 0;

    { Video frequency to set in next VideoChange call.
      Leave as 0 to use system default. }
    property VideoFrequency: Cardinal read FVideoFrequency write FVideoFrequency default 0;

    { Describe the changes recorded in variables VideoXxx,
      used by VideoChange and TryVideoChange.
      This is a multiline string, each line is indented by 2 spaces,
      always ends with CastleUtils.NL. }
    function VideoSettingsDescribe: String;

    (*Change the screen size, color bits and such, following the directions
      you set in VideoColorBits, VideoResize,
      VideoResizeWidth / VideoResizeHeight, and VideoFrequency variables.
      Returns @true if success.

      See example code
      @code(examples/user_interface/screen_resolution_change) for a working
      example.

      Simple example usage:

@longCode(#
unit GameChangeVideoResolution;

interface

var
  // TODO: Allow user to configure it somehow
  UserWantsToChangeScreenResolution: Boolean = true;

{ Change screen resolution, if desired by UserWantsToChangeScreenResolution. }
procedure ChangeResolution;

implementation

uses CastleWindow, CastleLog, CastleConfig;

procedure ChangeResolution;
begin
  if UserWantsToChangeScreenResolution then
  begin
    Application.VideoResize := true;
    // TODO: Allow user to choose the desired resolution
    Application.VideoResizeWidth := 1024;
    Application.VideoResizeHeight := 768;
    if not Application.TryVideoChange then
      WritelnWarning('Cannot change screen resolution, continuing with current settings');
  end;
end;

initialization
  UserWantsToChangeScreenResolution := UserConfig.GetValue('video/change_resolution', false);
  ChangeResolution;
finalization
  Application.VideoReset;
end.
#)

      TODO: Expose methods like EnumeratePossibleVideoConfigurations to predict
      what video settings are possible.

      TODO: Prefix "Video" for the family of these functions is not clear.
      Something like "Screen" would be better.
    *)
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
    property OpenWindows[Index: integer]: TCastleWindow read GetOpenWindows;
    { @groupEnd }

    { The application and CastleWindow backend is initialized.
      Called only once, at the very beginning
      of the game, when we're ready to load everything
      and the first OpenGL context is initialized (right before
      calling TCastleUserInterface.GLContextOpen on all UIs).

      For targets like Android or iOS,
      you should not do anything (even reading files) before this callback occurs.
      Only when this occurs, we know that external process told us
      "Ok, you're ready".
      So you should put all the game initialization in an Application.OnInitialize
      callback. It will be automatically called by CastleWindow backend
      when we're really ready (actually, a little later ---
      when OpenGL context is active, to allow you to display progress
      bars etc. when loading). }
    property OnInitialize: TProcedure read FOnInitialize write FOnInitialize;
    property OnInitializeEvent: TNotifyEvent read FOnInitializeEvent write FOnInitializeEvent;

    { Used on platforms that can only show a single window (TCastleWindow) at a time,
      like mobile or web applications.

      In other exceptional situations when we need a single window
      (e.g. to display CGE uncaught exception) we may also use this window,
      if set (otherwise we may use the 1st currently open window). }
    property MainWindow: TCastleWindow read FMainWindow write SetMainWindow;

    { User agent string, when running inside a browser.
      TODO: Right now never set (was used by NPAPI plugin, may be useful to new web target). }
    //property UserAgent: String read FUserAgent;

    { Process messages from the window system.
      You have to call this repeatedly to process key presses,
      mouse events, redraws and everything else.
      Messages are processed and appropriate methods of TCastleUserInterface
      instances (including TCastleView) are called.

      For simple programs calling the @link(Run) method is usually
      the best solution, @link(Run) just calls ProcessMessage in a loop.
      Manually using the ProcessMessage method allows you to implement
      modal dialog boxes (generally any kind of "display something
      until something happens" behavior). Make your own event loop like this:

      @longCode(#
        while not SomethingHappened do
          Application.ProcessMessages(...);
      #)

      This can used to implement routines that wait until a modal dialog box
      returns, like @link(MessageOK) or @link(MessageYesNo).

      For comfort, returns @code(not Terminated).
      So it returns @true if we should continue, that is
      if @code(Terminate) method was not called (directly or by closing
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
      is requested by Invalidate. When we have messages to process,
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

    { Processes @italic(all) pending messages. Do not wait for anything.

      Contrast this with ProcessMessage method, that processes only a single
      event. Or no event at all (when no events were pending and
      AllowSuspend = @false). This means that after calling ProcessMessage
      once, you may have many messages left in the queue (especially
      mouse move together with key presses typically makes a lot of
      events). So it's not good to use if you want to react timely to
      some user requests, e.g. when you do something time-consuming
      and allow user to break the task with Escape key.

      ProcessAllMessages is like
      calling in a loop ProcessMessage(false, false), ends when
      ProcessMessage(false, false) didn't process any message
      or when quit was called (or last window closed).

      So ProcessAllMessages makes sure we have processed all pending events,
      thus we are up-to-date with window system requests. }
    function ProcessAllMessages: boolean;

    procedure Terminate; override;

    { Run the program using TCastleWindow, by doing the event loop.
      Think of it as just a shortcut for "while ProcessMessage do ;".

      Note that this does nothing if OpenWindowsCount = 0, that is there
      are no open windows. Besides the obvious reason (you didn't call
      TCastleWindow.Open on any window...) this may also happen if you called
      Close (or Application.Terminate) from your window OnOpen / OnResize callback.
      In such case no event would probably reach
      our program, and user would have no chance to quit, so Run just refuses
      to work and exits immediately without any error. }
    procedure Run;

    function BackendName: String;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HandleException(Sender: TObject); override;

    { Parse some command-line options and remove them from @link(Parameters)
      list. These are standard command-line parameters of Castle Game Engine programs.

      See TCastleWindow.ParseParameters for more options specific to each window.

      @definitionList(
        @itemLabel -h / @--help
        @item(Output help, exit.)

        @itemLabel -v / @--version
        @item(Output version, exit,
          Uses @link(TCastleApplicationProperties.Version ApplicationProperties.Version).)

        @itemLabel @--log-file FILE-NAME
        @item(Force log to given file, sets @link(LogFileName).)

        @itemLabel @--display X-DISPLAY-NAME
        @item(Sets Application.XDisplayName under Unix.)

        @itemLabel -psvn_x_xxx
        @item(
          (Only relevant on macOS.) A special parameter -psvn_x_xxx will be found
          and removed from the @link(Parameters) list. See
          http://forums.macrumors.com/showthread.php?t=207344 and
          http://stackoverflow.com/questions/10242115/os-x-strange-psn-command-line-parameter-when-launched-from-finder .
        )

        @itemLabel @--no-limit-fps
        @item(Allows to disable
          @link(TCastleApplicationProperties.LimitFps ApplicationProperties.LimitFps),
          to allows to observe maximum FPS, see
          http://castle-engine.io/manual_optimization.php )

        @itemLabel @--capabilities automatic|force-fixed-function|force-modern
        @item(Force OpenGL context to have specific capabilities, to test rendering on modern or ancient GPUs.)
      )

      Moreover this also handles parameters from @link(TCastleWindow.ParseParameters),
      if @link(MainWindow) is set already.
      But note that our templates do not set @link(MainWindow) so early.
      We recommend to explicitly set window size / Window.FullScreen from code and call
      @link(TCastleWindow.ParseParameters) right after, to allow user to override.

      Moreover this also handles parameters from @link(TSoundEngine.ParseParameters).
    }
    procedure ParseStandardParameters;
    function ParseStandardParametersHelp: String;

    { Are we using OpenGLES for rendering. }
    function OpenGLES: Boolean;

    {$ifdef FPC}
    property LimitFPS: Single read GetLimitFPS write SetLimitFPS;
      deprecated 'use ApplicationProperties.LimitFps';
    property Version: String read GetVersion write SetVersion;
      deprecated 'use ApplicationProperties.Version';
    property TouchDevice: boolean read GetTouchDevice write SetTouchDevice;
      deprecated 'use ApplicationProperties.TouchDevice';
    {$endif FPC}
  end;

  { @deprecated Deprecated name for TCastleApplication. }
  TGLApplication = TCastleApplication deprecated;

{ Single global instance of TCastleApplication.
  Automatically created / destroyed by CastleWindow unit. }
function Application: TCastleApplication;

{ Describe given key. Key is given as combination of character (UTF-8 character as String, may be '')
  and Key code (may be keyNone), and additional required @code(Modifiers)
  (although some modifiers may be already implied by KeyString, e.g. when it is CtrlA).
  See @link(TMenuItem.Key) and @link(TMenuItem.KeyString) and @link(TMenuItem.Modifiers).

  Only when Key = keyNone and KeyString = ''
  then this combination doesn't describe any key, and we return @false.
  Otherwise we return @true and set S. }
function KeyToString(const KeyString: String; const Key: TKey; const Modifiers: TModifierKeys;
  out S: String): boolean;
function KeyString(const AKeyString: String; const Key: TKey; const Modifiers: TModifierKeys;
  out S: String): boolean; deprecated 'use KeyToString';

{$undef read_interface}

{$define read_global_interface}
{$I castlewindow_backend.inc}
{$undef read_global_interface}

{$define read_interface_2}
{$i castlewindowmenu.inc}
{$undef read_interface_2}

implementation

uses
  CastleLog, CastleGLVersion, CastleUriUtils, CastleControls, CastleMessaging,
  CastleRenderContext, CastleInternalGLUtils,
  {$define read_implementation_uses}
  {$I castlewindow_backend.inc}
  {$undef read_implementation_uses}
  Math;

{$define read_implementation}

var
  UnitFinalization: Boolean;

{$I castlewindowmenu.inc}
{$I castlewindow_backend.inc}

{ TWindowContainer ----------------------------------------------------------- }

constructor TWindowContainer.Create(AParent: TCastleWindow);
begin
  inherited Create(nil);
  Parent := AParent;
end;

procedure TWindowContainer.Invalidate;
begin
  Parent.Invalidate;
end;

function TWindowContainer.GLInitialized: boolean;
begin
  Result := Parent.GLInitialized;
end;

function TWindowContainer.PixelsWidth: Integer;
begin
  Result := Parent.Width;
end;

function TWindowContainer.PixelsHeight: Integer;
begin
  Result := Parent.Height;
end;

function TWindowContainer.PixelsRect: TRectangle;
begin
  Result := Parent.Rect;
end;

function TWindowContainer.Focused: boolean;
begin
  Result := Parent.Focused;
end;

procedure TWindowContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  Parent.InternalCursor := Value;
end;

function TWindowContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  { In theory, the single-buffer
    case could be optimized (do not redraw if not needed, that is:
    if not invalidated), but it's not worth the complication since noone uses
    single-buffer for normal applications... And also, there is no reliable
    way to capture screen contents in case of single-buffer. }

  EventBeforeRender;
  EventRender;
  Result := SaveScreen_NoFlush(SaveRect, Parent.SaveScreenBuffer);
end;

procedure TWindowContainer.SystemSetMousePosition(const Value: TVector2);
begin
  Parent.SystemSetMousePosition(Value);
end;

{ TCastleWindow ---------------------------------------------------------- }

constructor TCastleWindow.Create(AOwner: TComponent);
begin
  inherited;
  FClosed := true;
  FWidth  := WindowDefaultSize;
  FHeight := WindowDefaultSize;
  FLeft  := WindowPositionCenter;
  FTop   := WindowPositionCenter;
  FDoubleBuffer := true;
  if ApplicationProperties.Caption <> '' then
    FCaption[cpPublic] := ApplicationProperties.Caption
  else
    FCaption[cpPublic] := ApplicationName;
  FResizeAllowed := raAllowed;
  minWidth := 100;  maxWidth := 4000;
  minHeight := 100; maxHeight := 4000;
  DepthBits := DefaultDepthBits;
  StencilBits := DefaultStencilBits;
  FCursor := mcDefault;
  FMultiSampling := 1;
  FVisible := true;
  FAutoRedisplay := true;
  OwnsMainMenu := true;
  FMainMenuVisible := true;
  // Using deprecated CreateContainer - should be internal in the future
  {$warnings off}
  FContainer := CreateContainer;
  {$warnings on}
  Close_KeyString := '';
  SwapFullScreen_Key := keyNone;
  FpsShowOnCaption := false;
  FFpsCaptionUpdateDelay := DefaultFpsCaptionUpdateDelay;
  FFocused := true;
  FNamedParameters := TCastleStringList.Create;
  FRequirements := TGLContextRequirements.Create(nil);

  CreateBackend;

  if Messaging <> nil then
    Messaging.OnReceive.Add({$ifdef FPC}@{$endif} MessageReceived);
end;

destructor TCastleWindow.Destroy;
begin
  Close; { <- This will be ignored if already Closed }

  if OwnsMainMenu then
    FreeAndNil(FMainMenu) else
  if FMainMenu <> nil then
  begin
    FMainMenu.ParentWindow := nil; { clear Self from FMainMenu.ParentWindow }
    FMainMenu := nil;
  end;

  if Messaging <> nil then
    Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} MessageReceived);

  FreeAndNil(FContainer);
  FreeAndNil(FNamedParameters);
  FreeAndNil(FRequirements);
  inherited;
end;

{$warnings off} // using deprecated class in deprecated method
function TCastleWindow.CreateContainer: TWindowContainer;
begin
  Result := TWindowContainer.Create(Self);
end;
{$warnings on}

procedure TCastleWindow.OpenCore;

  procedure RenderLoadingBackground;
  var
    WindowRect, TextRect: TRectangle;
    UIScale: Single;
  begin
    WindowRect := Rect;

    RenderContext.Viewport := WindowRect;
    OrthoProjection(FloatRectangle(WindowRect));

    { Not only is RenderContext.Clear faster than DrawRectangle(WindowRect,...).
      In this case, it is also more reliable: in case of Android immersive
      mode, we may not have yet our desired size (our width or height is smaller
      than device screen). For some reason, RenderContext.Clear manages to clear
      the whole screen area anyway. }
    RenderContext.Clear([cbColor], Theme.LoadingBackgroundColor);

    UIScale := TCastleContainer.InternalCalculateUIScale(
      Theme.LoadingUIScaling, Theme.LoadingUIReferenceWidth, Theme.LoadingUIReferenceHeight, Theme.LoadingUIExplicitScale,
      Container.Dpi, FRealWidth, FRealHeight);
    TextRect := Theme.ImagesPersistent[tiLoading].Image.Rect.
      ScaleAroundCenter(UIScale).
      Align(hpMiddle, WindowRect, hpMiddle).
      Align(vpMiddle, WindowRect, vpMiddle);
    Theme.Draw(TextRect, tiLoading, UIScale, Theme.LoadingColor);

    // just like TCastleWindow.DoRender
    if HasDoubleBuffer then SwapBuffers else glFlush;

    {$IFDEF android}
    { Workaround an ARM64 Android-specific bug which manifests on some devices
      (reproduced on Xiaomi MI 9 SE) that creates a dangling EInvalidOp after calling Theme.Draw.
      It doesn't seem to interfere with normal porgram workflow,
      neither it causes any visible graphic glitches in the image rendered by Theme.Draw,
      but causes ClearExceptions(true) to raise unrelated dangling exceptions,
      which in turn makes e.g. CastleScript misbehave.
      Also seems to be reproduced outside of Castle Game Engine:
      https://forum.lazarus.freepascal.org/index.php/topic,42933.msg318965.html?#msg318965
      It seems that clearing them once per app run is perfectly enough,
      all subsequent calls to Theme.Draw do not raise the exception. }
    ClearExceptions(false);
    {$ENDIF}
  end;

  { Do the job of OpenCore, do not protect from possible exceptions raised inside. }
  procedure OpenUnprotected;
  begin
    { Once context is initialized, then Android activity is initialized,
      or iOS called CGEApp_Initialize -> so it's safe to access files. }
    ApplicationProperties._FileAccessSafe := true;

    { Adjust Left/Top/Width/Height as needed.

      Note: calculations below try to correct window geometry but they
      can fail to foresee some things. In particular, they do not take
      into account a potential menu bar that may be visible when MainMenu <> nil.
      E.g., when MainMenu <> nil and implementation supports MainMenu as
      menu bar (GTK and WINAPI implementations) and FullScreen then
      the actual OpenGL window size will NOT match ScreenWidth/Height,
      it will be slightly smaller (menu bar takes some space). }
    if Width  = WindowDefaultSize then FWidth  := Application.ScreenWidth  * 4 div 5;
    if Height = WindowDefaultSize then FHeight := Application.ScreenHeight * 4 div 5;
    ClampVar(FWidth , MinWidth , MaxWidth);
    ClampVar(FHeight, MinHeight, MaxHeight);
    if Left = WindowPositionCenter then FLeft := (Application.ScreenWidth  - Width ) div 2;
    if Top  = WindowPositionCenter then FTop  := (Application.ScreenHeight - Height) div 2;
    FRealWidth  := FWidth;
    FRealHeight := FHeight;

    { reset some window state variables }
    EventOpenCalled := false;

    { Set Closed to false. Before OpenBackend and EventOpen + EventResize,
      since they can raise exceptions, and in reaction to it we will cleanly
      Close the window. }
    FClosed := false;
    Invalidated := false;

    UpdateRequirements; // OpenBackend may use it

    // initialize HasDoubleBuffer
    HasDoubleBuffer := DoubleBuffer;
    Assert(HasDoubleBuffer = FRequirements.DoubleBuffer);

    { Call OpenBackend. Note that OpenBackend can call DoResize,
      it will still be correctly understood. }
    OpenBackend;

    { Do MakeCurrent before setting RenderContext.Viewport and EventOpen. }
    MakeCurrent;

    GLInformationInitialize;

    if GLVersion.BuggyDepth32 and
      (glGetInteger(GL_DEPTH_BITS) >= 32) and
      (StencilBits = 0) then
    begin
      WritelnLog('Rendering Context Initialization',
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

    { synchronize RenderContext.Viewport with our Width/Height (note that, because
      of ResizeAllowed and MinWidth etc. that can be different than actual window
      sizes). }
    RenderContext.Viewport := Rect;

    {$ifndef OpenGLES}
    if ( (AntiAliasing = aa2SamplesNicer) or
         (AntiAliasing = aa4SamplesNicer) ) and
       GLFeatures.NV_multisample_filter_hint then
      glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
    {$endif}

    try
      { Make ApplicationProperties.IsGLContextOpen true now, to allow creating
        TDrawableImage from Application.OnInitialize work Ok. }
      ApplicationProperties._GLContextEarlyOpen;

      RenderLoadingBackground;

      Application.CastleEngineInitialize;
      if Closed then Exit;

      { Call first EventOpen and then EventResize.
        Note that DoOpen and DoResize must be done after the OpenGL context
        is initialized and everything is ready.
        Even the 1st OnOpen / OnResize event may call Application.ProcessMessages,
        e.g. because user calls CastleMessages.MessageOk. }
      EventOpenCalled := true;
      Container.EventOpen(Application.OpenWindowsCount);

      { Check Closed here, in case OnOpen closed the window
        (by calling Application.Terminate (that calls Close on all windows) or direct Close
        on this window). Note that Close calls
        CloseBackend and generally has *immediate* effect --- that's why
        doing anything more with window now (like MakeCurrent) would be wrong. }
      if Closed then Exit;

      DoResize(FRealWidth, FRealHeight, true);
    except
      { capture exceptions from Application.OnInitialize,
        Container.EventOpen, Container.EventResize }
      Application.HandleException(Self);
    end;

    { Check Closed here, in case OnResize closed the window. }
    if Closed then Exit;

    { to be SURE that current window's gl context is active,
      even if someone in EventOpen changed current gl context }
    MakeCurrent;

    UpdateFullScreenBackend;
  end;

begin
  if not FClosed then Exit;

  FDuringOpen := true;
  try
    try
      OpenUnprotected;
    except
      Close; raise;
    end;
  finally FDuringOpen := false end;
end;

procedure TCastleWindow.Open(const Retry: TGLContextRetryOpenFunc);
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

{ Try to lower
  - anti-aliasing (multi-sampling)
  - shadows (stencil buffer)
  - depth size
  requirements and initialize worse GL context. }
function DefaultRetryOpen(Window: TCastleWindow): boolean;
begin
  if Window.AntiAliasing <> aaNone then
  begin
    Window.AntiAliasing := aaNone;
    WritelnLog('OpenGL context', 'OpenGL context cannot be initialized. Multi-sampling (anti-aliasing) turned off, trying to initialize once again.');
    Result := true;
  end else
  if Window.StencilBits > 0 then
  begin
    Window.StencilBits := 0;
    WritelnLog('OpenGL context', 'OpenGL context cannot be initialized. Stencil buffer (shadow volumes) turned off, trying to initialize once again.');
    Result := true;
  end else
  if Window.DepthBits > DepthBitsFallback then
  begin
    Window.DepthBits := DepthBitsFallback;
    WritelnLog('OpenGL context', 'OpenGL context cannot be initialized. Depth bits decreased to %d, trying to initialize once again.', [
      DepthBitsFallback
    ]);
    Result := true;
  end else
    Result := false;
end;

procedure TCastleWindow.Open;
begin
  Open(@DefaultRetryOpen);
end;

procedure TCastleWindow.Close(const QuitWhenLastWindowClosed: boolean);
begin
  if FClosed then Exit;

  try
    if EventOpenCalled then
    begin
      MakeCurrent;
      Container.EventClose(Application.OpenWindowsCount);
    end;
  finally
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
  end;
end;

procedure TCastleWindow.MakeCurrent;
begin
  { Calling BackendMakeCurrent is done very often (before every event,
    so a couple of times for every frame). And usually it's useless,
    as most games have only 1 open window. }
  if Application.Current <> Self then
  begin
    BackendMakeCurrent;
    RenderContext := Container.Context;
    Application.Current := Self;
  end;
end;

procedure TCastleWindow.SetAutoRedisplay(const Value: boolean);
begin
  FAutoRedisplay := value;
  if Value then Invalidate;
end;

procedure TCastleWindow.ReleaseAllKeysAndMouse;
var
  Key: TKey;
  MouseButton: TCastleMouseButton;
  {$ifdef CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  ModKey: TModifierKey;
  B: boolean;
  {$endif}
begin
  {$ifdef CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}
  { When CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN, I *HAVE* to use below
    SetPrivateModifiersDown. It would be an error to do DoKeyUp(keyCtrl)
    directly when CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN, instead we have to
    use SetPrivateModifiersDown(mkCtrl, ...).
    This is the only way to make values in PrivateModifiersDown[]
    and Pressed[] arrays consistent. }
  for ModKey := Low(ModKey) to High(ModKey) do
    for B := Low(B) to High(B) do
      SetPrivateModifiersDown(ModKey, B, false);
  {$endif CASTLE_WINDOW_USE_PRIVATE_MODIFIERS_DOWN}

  { Since we do DoKeyUp, this should also take care of releasing Characters. }
  for Key := Low(Key) to High(Key) do
    if Container.Pressed[Key] then
      DoKeyUp(Key);

  for MouseButton := Low(MouseButton) to High(MouseButton) do
    if MouseButton in Container.MousePressed then
      DoMouseUp(Container.MousePosition, MouseButton, 0);

  Container.MouseLookIgnoreNextMotion;
end;

function TCastleWindow.GetColorBits: Cardinal;
begin
  Result := RedBits + GreenBits + BlueBits;
end;

procedure TCastleWindow.SetColorBits(const Value: Cardinal);
begin
  RedBits := Value div 3;
  BlueBits := Value div 3;
  GreenBits := Value - RedBits - BlueBits;
  Assert(Value = ColorBits);
end;

procedure TCastleWindow.SetAntiAliasing(const Value: TAntiAliasing);
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
    - extend the src/x3d/opengl/glsl/source/screen_effect_library.glsl
    - extend the check for samples in ScreenEffectLibrary in CastleScreenEffects
      (this must be synchronized with screen_effect_library.glsl implementation).
  }
begin
  FAntiAliasing := Value;
  MultiSampling := AntiAliasingToMultiSampling[Value];
end;

{ ----------------------------------------------------------------------------

  TCastleWindow events DoXxx methods,
  implementations independent from the backend.
  Backends should always call DoXxx, never call directly EventXxx or OnXxx. }

procedure TCastleWindow.DoResize(AWidth, AHeight: integer; FirstResizeAfterOpen: boolean);
begin
  { Set FRealWidth/Height unconditionally to AWidth/AHeight. }
  FRealWidth := AWidth;
  FRealHeight := AHeight;

  { When setting FWidth/FHeight (which is used to report size to user,
    and to create OpenGL area size) we make sure some constaints are honored:

    - You should not be able to resize the window when ResizeAllowed <> raAllowed.
      When ResizeAllowed = raNotAllowed, we want to guarantee that
      FWidth and FHeight stay constant, just as developer set them.

      And some backends do not allow to express,
      or do not reliably honour, all possible ResizeAllowed values.
      E.g. Windows will always resize the window to fit on desktop.
      Window Manager on X can also do this.
      So we need to "forcefully" apply the ResizeAllowed logic here:

      * raNotAllowed: FWidth and FHeight cannot change

      * raOnlyAtOpen: FWidth and FHeight can change only at first EventResize
        (with FirstResizeAfterOpen = true), at least from the point of view of outside.
        Internally, every call to DoResize upto and including FirstResizeAfterOpen call
        changes FWidth and FHeight. This allows to accumulate e.g. size changes
        caused by FullScreen=true, in case OpenBackend does them by explicitly
        calling DoResize, like unix/castlewindow_xlib.inc .

      * raAllowed: FWidth and FHeight can change freely

    - Same thing for Min/MaxWidth/Height.

      BTW, always MinWidth > 0 and MinHeight > 0,
      so the Clamp below will also make sure FWidth and FHeight are > 0.

    We also secure from calls with FirstResizeAfterOpen=false
    from OpenBackend, which may happen from e.g. WinAPI and GTK backends.
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
  Container.EventResize;
end;

procedure TCastleWindow.DoCloseQuery;
begin
  MakeCurrent;
  if Assigned(OnCloseQuery) then
    OnCloseQuery(Container) else
    Close;
end;

procedure TCastleWindow.DoRender;
begin
  FrameProfiler.Start(fmBeforeRender);
  { We set Invalidated := false before EventRender (that calls OnRender),
    because we guarantee that calling Invalidate within OnRender will
    cause the redraw in next frame. }
  Invalidated := false;
  MakeCurrent;
  Container.EventBeforeRender;
  FrameProfiler.Stop(fmBeforeRender);

  if Closed then Exit; { check, in case window got closed in the event }

  FrameProfiler.Start(fmRender);
  Container.Fps.InternalRenderBegin;
  try
    Container.EventRender;
    if Closed then Exit; { check, in case window got closed in the event }

    if GLVersion.BuggySwapNonStandardViewport then
      RenderContext.Viewport := Rect;

    FrameProfiler.Start(fmRenderSwapFlush);
    if HasDoubleBuffer then SwapBuffers else glFlush;

    { Keep this check inside fmRenderSwapFlush measurement.

      Naive explanation (seems wrong, see below):
      On Windows, SwapBuffers may execute immediately,
      but then the next OpenGL call actually waits for it to finish
      (see https://www.freelists.org/post/visionegg/swap-buffers-returns-immediately-for-2D-Textures-in-OpenGL-20,1 ).
      So if this is outside fmRenderSwapFlush, fmRenderSwapFlush would be usually
      zero, and most of the frame time would not be covered by anything we measure
      in FrameProfiler.

      The above explanation is however proven wrong by:

      in -dRELEASE builds, without CheckGLErrors call at all,
      fmRenderSwapFlush times are OK, nicely non-zero reasonable.
      I.e. we don't have the problem of fmRenderSwapFlush = zero in -dRELEASE
      builds, and we should -> if the above explanation was all that happens.
      If the above explanation was right, then we should also measure
      the next GL command (like RenderContext.Clear at the start
      of TCastleContainer.EventRender), but we don't need to.

      Tested (both debug and release observations above) on
        Version string: 4.6.0 NVIDIA 425.46
        Vendor: NVIDIA Corporation
        Renderer: GeForce GTX 1050 Ti/PCIe/SSE2
    }
    {$ifdef CASTLE_WINDOW_CHECK_GL_ERRORS_AFTER_DRAW}
    CheckGLErrors('End of TCastleWindow.DoRender');
    {$endif}

    FrameProfiler.Stop(fmRenderSwapFlush);

    if AutoRedisplay then Invalidate;
  finally
    Container.Fps.InternalRenderEnd;
    FrameProfiler.Stop(fmRender);
  end;
end;

procedure TCastleWindow.DoKeyDown(const Key: TKey; const KeyString: String);
var
  Event: TInputPressRelease;

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
         TMenuItem(Entry).KeyMatches(Key, Event.KeyString, Container.Pressed.Modifiers) then
        Result := TMenuItem(Entry);
    end;

  begin
    if MainMenu <> nil then
      Result := SeekMe(MainMenu) else
      Result := nil;
  end;

var
  MatchingMI: TMenuItem;
  KeyRepeated: boolean;
begin
  Event := InputKey(Container.MousePosition, Key, KeyString, ModifiersDown(Container.Pressed));

  KeyRepeated :=
    // Key or KeyString non-empty
    ((Key <> keyNone) or (KeyString <> '')) and
    // Key already pressed
    ((Key = keyNone) or Container.Pressed.Keys[Key]) and
    // KeyString already pressed
    ((KeyString = '') or Container.Pressed.Strings[KeyString]);

  Container.Pressed.KeyDown(Key, KeyString);

  { This implementation guarantees that

    A. we call some menu item
    B. *or* we call Container.EventPress.

    There is *no* situation possible in which we don't handle key
    (because it matches menu item) but we also don't pass it to DoMenuClick.
    This also avoids some inconsistencies between SeekMatchingMenuItem
    and what GUI toolkit actually does, in case RedirectKeyDownToMenuClick = false
    (like on GTK): e.g. we had situation when SeekMatchingMenuItem was finding a menu
    item (for Key=keyNumPad7, KeyString='7') but for GTK it didn't match corresponding
    menu item.

    It also means it is undefined (backend-specific) whether Container.EventPress
    *does* receive keys that triggered some menu items,
    when RedirectKeyDownToMenuClick = false.
    Some backends may hide such keys, some don't, and in case of
    RedirectKeyDownToMenuClick = false we dont' control it at all.

    Only when RedirectKeyDownToMenuClick = true we guarantee that only *one*
    of A or B fires.

    So:
    - We always guarantee that A or B occurs.
    - Only when RedirectKeyDownToMenuClick = true we guarantee that exactly one
      of A or B occurs.
  }

  MatchingMI := nil;
  if (RedirectKeyDownToMenuClick or (not MainMenuVisible)) and
     (MainMenu <> nil) and
     MainMenu.Enabled then
    MatchingMI := SeekMatchingMenuItem;
  if MatchingMI <> nil then
  begin
    DoMenuClick(MatchingMI);
  end else
  begin
    MakeCurrent;
    Event.KeyRepeated := KeyRepeated;
    Container.EventPress(Event);

    if Event.IsKey(Close_KeyString) then
      Close
    else
    if Event.IsKey(SwapFullScreen_Key) then
      FullScreen := not FullScreen;
  end;
end;

procedure TCastleWindow.DoKeyUp(const key: TKey);
var
  KeyString: String;
begin
  if Container.Pressed[Key] then
  begin
    { keyNone key is never pressed, DoKeyDown guarantees this }
    Assert(Key <> keyNone);
    Container.Pressed.KeyUp(Key, KeyString);
    MakeCurrent;
    Container.EventRelease(InputKey(Container.MousePosition, Key, KeyString,
      ModifiersDown(Container.Pressed)));
  end;
end;

procedure TCastleWindow.InternalFakeMotion(const Event: TInputMotion);
begin
  DoMotion(Event);
end;

procedure TCastleWindow.DoMotion(const Event: TInputMotion);
begin
  MakeCurrent;
  Container.EventMotion(Event);
end;

procedure TCastleWindow.DoMouseDown(const Position: TVector2;
  const AButton: TCastleMouseButton; const FingerIndex: TFingerIndex);
var
  Event: TInputPressRelease;
begin
  MakeCurrent;
  Event := InputMouseButton(Position, AButton, FingerIndex,
    ModifiersDown(Container.Pressed));
  Container.EventPress(Event);
end;

procedure TCastleWindow.DoMouseUp(const Position: TVector2;
  const AButton: TCastleMouseButton; const FingerIndex: TFingerIndex);
var
  Event: TInputPressRelease;
begin
  MakeCurrent;
  Event := InputMouseButton(Position, AButton, FingerIndex,
    ModifiersDown(Container.Pressed));
  Container.EventRelease(Event);
end;

procedure TCastleWindow.DoMouseWheel(const Scroll: Single; const Vertical: Boolean);
var
  Event: TInputPressRelease;
begin
  MakeCurrent;
  Event := InputMouseWheel(Container.MousePosition, Scroll, Vertical,
    ModifiersDown(Container.Pressed));
  Container.EventPress(Event);
end;

procedure TCastleWindow.DoUpdate;
begin
  FrameProfiler.StartFrame;
  FrameProfiler.Start(fmUpdate);

  BackendInsideUpdate;

  MakeCurrent;
  Container.EventUpdate;

  { show FPS on caption once FpsCaptionUpdateDelay passed }
  if FpsShowOnCaption and
     (TimerSeconds(Timer, LastFpsOutputTime) >= FpsCaptionUpdateDelay) then
  begin
    LastFpsOutputTime := Timer;
    SetCaption(cpFps, ' - FPS: ' + Container.Fps.ToString);
  end;

  UpdateFullScreenBackend;

  FrameProfiler.Stop(fmUpdate);
end;

procedure TCastleWindow.DoMenuClick(Item: TMenuItem);
begin
  if (MainMenu <> nil) and (not MainMenu.Enabled) then Exit;

  MakeCurrent;
  if Item.DoClick then Exit;

  { Maybe Item.DoClick changed current OpenGL context and returned false?
    We want to be safe, so we do here MakeCurrent again. }
  MakeCurrent;
  {$warnings off} // keep deprecated working
  if Assigned(OnMenuClick) then
    OnMenuClick(Container, Item);
  {$warnings on}
  if Assigned(OnMenuItemClick) then
    OnMenuItemClick(Item);
end;

procedure TCastleWindow.DoDropFiles(const FileNames: array of string);
begin
  MakeCurrent;
  if Assigned(OnDropFiles) then
    OnDropFiles(Container, FileNames);
end;

function TCastleWindow.MessageReceived(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;
var
  Url: String;
begin
  Result := false;
  if (Received.Count = 2) and
     (Received[0] = 'open-associated-url') then
  begin
    Url := Received[1];
    DoDropFiles([Url]);
    Result := true;
  end;
end;

function TCastleWindow.AllowSuspendForInput: boolean;
begin
  Result := Container.AllowSuspendForInput and not (Invalidated or FpsShowOnCaption);
end;

{ Menu things ------------------------------------------------------------ }

procedure TCastleWindow.SetMainMenu(Value: TMenu);
begin
 if MainMenu <> Value then
 begin
  if (not Closed) and ((MainMenu <> nil) <> (Value <> nil)) then
   raise EInternalError.Create('While TCastleWindow is not Closed, '+
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

{ SaveScreenXxx --------------------------------------------------------------- }

function TCastleWindow.SaveScreenBuffer: TColorBuffer;
begin
  if HasDoubleBuffer then
    Result := cbBack
  else
    Result := cbFront;
end;

procedure TCastleWindow.SaveScreen(const Url: String);
begin
  Container.SaveScreen(Url);
end;

function TCastleWindow.SaveScreen: TRGBImage;
begin
  if Closed then
    raise Exception.Create('Cannot save the screen when the TCastleWindow is closed');
  Result := Container.SaveScreen;
end;

function TCastleWindow.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  if Closed then
    raise Exception.Create('Cannot save the screen when the TCastleWindow is closed');
  Result := Container.SaveScreen(SaveRect);
end;

function TCastleWindow.SaveScreenToGL(const SmoothScaling: boolean): TDrawableImage;
begin
  Result := SaveScreenToGL(Rect, SmoothScaling);
end;

function TCastleWindow.SaveScreenToGL(
  const SaveRect: TRectangle;
  const SmoothScaling: boolean): TDrawableImage;
begin
  if Closed then
    raise Exception.Create('Cannot save the screen when the TCastleWindow is closed');
  Container.EventBeforeRender;
  Container.EventRender;
  Result := SaveScreenToGL_NoFlush(SaveRect, SaveScreenBuffer, SmoothScaling);
end;

procedure TCastleWindow.SaveScreenDialog(ProposedUrl: String);
begin
  if FileDialog('Save screen to file', ProposedUrl, false, SaveImage_FileFilters) then
  try
    SaveScreen(ProposedUrl);
  except
    on E: Exception do MessageOK('Unable to save screen: ' + E.Message, mtError);
  end;
end;

function TCastleWindow.FileDialog(const Title: String; var Url: String;
  OpenDialog: boolean; FileFilters: TFileFilterList = nil): boolean;
var
  FileName: String;
begin
  { calculate FileName from URL }
  FileName := UriToFilenameSafe(Url);
  if OpenDialog then
    FileName := ExtractFilePath(FileName);
  Result := BackendFileDialog(Title, FileName, OpenDialog, FileFilters);
  if Result then
    Url := FilenameToUriSafe(FileName);
end;

function TCastleWindow.FileDialog(const Title: String; var Url: String;
  OpenDialog: boolean; const FileFilters: String): boolean;
var
  FFList: TFileFilterList;
begin
  FFList := TFileFilterList.Create(true);
  try
    FFList.AddFiltersFromString(FileFilters);
    Result := FileDialog(Title, Url, OpenDialog, FFList);
  finally FreeAndNil(FFList) end;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColorRGB): boolean;
var
  Color4: TCastleColor;
begin
  Color4 := Vector4(Color, 1);
  Result := ColorDialog(Color4);
  if Result then
    Color := Color4.RGB;
end;

function TCastleWindow.ColorDialog(var Color: TVector3Byte): boolean;
var
  ColorSingle: TVector4;
begin
  ColorSingle.X := Color.X / High(Byte);
  ColorSingle.Y := Color.Y / High(Byte);
  ColorSingle.Z := Color.Z / High(Byte);
  ColorSingle.W := 1;
  Result := ColorDialog(ColorSingle);
  if Result then
    Color := Vector3Byte(ColorSingle.XYZ);
end;

{ OpenAndRun ----------------------------------------------------------------- }

procedure TCastleWindow.OpenAndRun;
begin
  Open;
  Application.Run;
end;

{ TCastleWindow ParseParameters -------------------------------------------------- }

procedure WindowOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: String; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Window: TCastleWindow absolute Data;

  procedure ApplyGeometryParam(const geom: String);
  var
    p: integer;
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
   Window.FullScreen := false;
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
     Window.Width := parWidth;
     Window.Height := parHeight;
    end;
    if positionSpecified then
    begin
     if xoffPlus then
      Window.Left := parXoff else
      Window.Left := Application.ScreenWidth-parXoff-parWidth;
     if yoffPlus then
      Window.Top := parYoff else
      Window.Top := Application.ScreenHeight-parYoff-parHeight;
    end;

   except
    on E: EConvertError do
     raise EInvalidParams.Create('Invalid --geometry parameter : '+E.Message);
   end;
  end;

  procedure ApplyFullScreenCustomParam(const option: String);
  var p: integer;
  begin
   Window.FullScreen := true;
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
  case OptionNum of
    0: Window.FullScreen := true;
    1: Window.FullScreen := false;
    2: ApplyGeometryParam(Argument);
    3: ApplyFullScreenCustomParam(Argument);
    4: ApplicationProperties.TouchDevice := true;
    else raise EInternalError.CreateFmt('WindowOptionProc: unhandled OptionNum %d', [OptionNum]);
  end;
end;

procedure TCastleWindow.ParseParameters;
const
  Options: array [0..4] of TOption = (
    (Short: #0; Long: 'fullscreen'; Argument: oaNone),
    (Short: #0; Long: 'window'; Argument: oaNone),
    (short: #0; Long: 'geometry'; Argument: oaRequired),
    (Short: #0; Long: 'fullscreen-custom'; Argument: oaRequired),
    (Short: #0; Long: 'pretend-touch-device'; Argument: oaNone)
  );
begin
  Parameters.Parse(Options, {$ifdef FPC}@{$endif} WindowOptionProc, Self, true);
end;

class function TCastleWindow.ParseParametersHelp: String;
begin
  Result :=
    OptionDescription('--fullscreen', 'Set window to full-screen (cover whole screen).') + NL +
    OptionDescription('--window', 'Set window to not be full-screen.') + NL +
    OptionDescription('--geometry WIDTHxHEIGHT<sign>XOFF<sign>YOFF', 'Set window to not be full-screen, and set initial size and/or position.') + NL +
    OptionDescription('--fullscreen-custom WIDTHxHEIGHT', 'Change desktop resolution and set window to full-screen.') + NL +
    OptionDescription('--pretend-touch-device', 'Pretend this is a device with a touch screen, for debugging purposes.');
end;

{ TCastleWindow miscellaneous -------------------------------------------- }

procedure TCastleWindow.UpdateRequirements;
begin
  FRequirements.DoubleBuffer := DoubleBuffer;
  FRequirements.ColorBits := ColorBits;
  FRequirements.RedBits := RedBits;
  FRequirements.GreenBits := GreenBits;
  FRequirements.BlueBits := BlueBits;
  FRequirements.DepthBits := DepthBits;
  FRequirements.AlphaBits := AlphaBits;
  FRequirements.StencilBits := StencilBits;
  FRequirements.MultiSampling := MultiSampling;
end;

function TCastleWindow.RequestedBufferAttributes: String;
begin
  UpdateRequirements;
  Result := FRequirements.RequestedBufferAttributes;
end;

procedure TCastleWindow.CheckRequestedBufferAttributes(
  const ProviderName: String;
  ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
  ProvidedMultiSampling: Cardinal);
begin
  UpdateRequirements;
  FRequirements.CheckRequestedBufferAttributes(ProviderName,
    ProvidedStencilBits, ProvidedDepthBits, ProvidedAlphaBits,
    ProvidedMultiSampling);
end;

procedure TCastleWindow.MenuUpdateBegin;
begin
  { MenuUpdateNeedsInitialize = false always when MenuUpdateInside = 0. }
  Assert((MenuUpdateInside <> 0) or (not MenuUpdateNeedsInitialize));

  Inc(MenuUpdateInside);
end;

procedure TCastleWindow.MenuUpdateEnd;
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

procedure TCastleWindow.MenuInitialize;
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

procedure TCastleWindow.MenuFinalize;
begin
  { MenuFinalize ignores MenuUpdateInside state, not needed. }
  if MenuInitialized and (not Closed) and MainMenuVisible then
  begin
    MenuInitialized := false;
    BackendMenuFinalize;
  end;
end;

procedure TCastleWindow.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if not Closed then
      WritelnWarning('Window', 'Changing TCastleWindow.Width when the window is open is not supported now');
  end;
end;

procedure TCastleWindow.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if not Closed then
      WritelnWarning('Window', 'Changing TCastleWindow.Height when the window is open is not supported now');
  end;
end;

procedure TCastleWindow.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if not Closed then
      WritelnWarning('Window', 'Changing TCastleWindow.Left when the window is open is not supported now');
  end;
end;

procedure TCastleWindow.SetTop(const Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    if not Closed then
      WritelnWarning('Window', 'Changing TCastleWindow.Top when the window is open is not supported now');
  end;
end;

procedure TCastleWindow.SetResizeAllowed(const Value: TResizeAllowed);
begin
  if FResizeAllowed <> Value then
  begin
    FResizeAllowed := Value;
    if not Closed then
      WritelnWarning('Window', 'Changing TCastleWindow.ResizeAllowed when the window is open is not supported now');
  end;
end;

procedure TCastleWindow.SetFullScreenWanted(const Value: Boolean);
begin
  if FFullScreenWanted <> Value then
  begin
    FFullScreenWanted := Value;

    if FDuringOpen and Value then
      WriteLnWarning('Window', 'TCastleWindow.FullScreen is changed during the initial Application.OnInitialize / OnOpen / OnResize events. This works, but is unoptimal (the window will start non-fullscreen and will only change to fullscreen later). ' + 'Usually you should rather initialize "Window.FullScreen" in the main unit "initialization" section.');

    { When the window is Closed, updating FFullScreenBackend is trivial
      and can be done automatically. Otherwise, it is delated
      to UpdateFullScreenBackend call. }
    if Closed then
      FFullScreenBackend := Value;
  end;
end;

procedure TCastleWindow.SimpleUpdateFullScreenBackend;
begin
  if FFullScreenBackend <> FFullScreenWanted then
  begin
    FFullScreenBackend := FFullScreenWanted;
    if not Closed then
    begin
      Close(false);
      if FFullScreenBackend then
      begin
        { FFullScreenBackend is true, so we changed it from false to true.
          Save BeforeFullScreen* now. }
        BeforeFullScreenGeometryKnown := true;
        BeforeFullScreenLeft := Left;
        BeforeFullScreenTop := Top;
        BeforeFullScreenWidth := Width;
        BeforeFullScreenHeight := Height;
      end else
      begin
        { We change FFullScreenBackend from true to false.
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

procedure TCastleWindow.SwapFullScreen;
begin
  FullScreen := not FullScreen;
end;

function TCastleWindow.GetPublicCaption: String;
begin
  Result := FCaption[cpPublic];
end;

procedure TCastleWindow.SetPublicCaption(const Value: String);
begin
  SetCaption(cpPublic, Value);
end;

function TCastleWindow.GetWholeCaption: String;
begin
  Result := FCaption[cpPublic] + FCaption[cpFps];
end;

function TCastleWindow.Rect: TRectangle;
begin
  if Closed then
    Result := TRectangle.Empty else
    Result := Rectangle(0, 0, Width, Height);
end;

function TCastleWindow.GLInitialized: boolean;
begin
  Result := not Closed;
end;

procedure TCastleWindow.PostRedisplay;
begin
  Invalidate;
end;

{$ifndef CASTLE_WINDOW_LIBRARY}
{$ifndef CASTLE_WINDOW_FORM}
procedure TCastleWindow.Invalidate;
begin
  if not Closed then
    Invalidated := true;
end;
{$endif}
{$endif}

function TCastleWindow.Controls: TInternalChildrenControls;
begin
  Result := Container.Controls;
end;

procedure TCastleWindow.SetDemoOptions(const ASwapFullScreen_Key: TKey;
  AClose_KeyString: String;
  const AFpsShowOnCaption: boolean);
begin
  // only for backward compatibility (when this parameter was Char) convert #0 to ''
  if AClose_KeyString = #0 then
    AClose_KeyString := '';

  SwapFullScreen_Key := ASwapFullScreen_Key;
  Close_KeyString := AClose_KeyString;
  FpsShowOnCaption := AFpsShowOnCaption;
end;

function TCastleWindow.MousePressed: TCastleMouseButtons;
begin
  Result := Container.MousePressed;
end;

function TCastleWindow.Pressed: TKeysPressed;
begin
  Result := Container.Pressed;
end;

function TCastleWindow.Fps: TFramesPerSecond;
begin
  Result := Container.Fps;
end;

function TCastleWindow.GetTouches(const Index: Integer): TTouch;
begin
  Result := Container.Touches[Index];
end;

function TCastleWindow.TouchesCount: Integer;
begin
  Result := Container.TouchesCount;
end;

function TCastleWindow.GetMousePosition: TVector2;
begin
  Result := Container.MousePosition;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
begin
  Container.MousePosition := Value;
end;

function TCastleWindow.LeftTopToCastle(const V: TVector2): TVector2;
begin
  Result := LeftTopToCastle(V.X, V.Y);
end;

function TCastleWindow.LeftTopToCastle(const X, Y: Single): TVector2;
begin
  Result.X := X;
  Result.Y := FRealHeight - 1 - Y;
end;

function TCastleWindow.CastleToLeftTopInt(const V: TVector2): TVector2Integer;
begin
  Result.X := Floor(V.X);
  Result.Y := FRealHeight - 1 - Floor(V.Y);
end;

{ TWindowList ------------------------------------------------------------ }

procedure TWindowList.Invalidate;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].Invalidate;
end;

procedure TWindowList.DoUpdate;
var
  i: integer;
begin
  for i := 0 to Count - 1 do Items[i].DoUpdate;
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
  CreateBackend;
  OnMainContainer := {$ifdef FPC}@{$endif}GetMainContainer;
end;

destructor TCastleApplication.Destroy;
begin
  OnMainContainer := nil;

  { Close any windows possibly open now.
    This is necessary --- after destroying Application there would be really
    no way for them to close properly (that is, TCastleWindow.CloseBackend
    may, and usually will, fail with very strange errors when called
    after freeing central Application). }
  CloseAllOpenWindows;

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

function TCastleApplication.GetMainContainer: TCastleContainer;
begin
  if MainWindow <> nil then
    Result := MainWindow.Container
  else
    Result := nil;
end;

procedure TCastleApplication.CastleEngineInitialize;
var
  TimeStart, TimeStart2: TCastleProfilerTime;
begin
  if Initialized and not InitializedJavaActivity then
    WritelnLog('Android', 'Android Java activity was killed (and now got created from stratch), but native thread survived. Calling only OnInitializeJavaActivity.');

  if not InitializedJavaActivity then
  begin
    InitializedJavaActivity := true;
    {if Assigned(OnInitializeJavaActivity) then
      OnInitializeJavaActivity();}
    ApplicationProperties._InitializeJavaActivity;
  end;

  if not Initialized then
  begin
    Initialized := true;

    // Call OnInitialize and OnInitializeEvent, watched by Profiler

    if Assigned(OnInitialize) or Assigned(OnInitializeEvent) then
    begin
      TimeStart := Profiler.Start('TCastleApplication Initialization');
      try
        if Assigned(OnInitialize) then
        begin
          TimeStart2 := Profiler.Start('TCastleApplication.OnInitialize');
          try
            OnInitialize();
          finally Profiler.Stop(TimeStart2) end;
        end;

        if Assigned(OnInitializeEvent) then
        begin
          TimeStart2 := Profiler.Start('TCastleApplication.OnInitializeEvent');
          try
            OnInitializeEvent(Self);
          finally Profiler.Stop(TimeStart2) end;
        end;
      finally Profiler.Stop(TimeStart, true) end;
    end;
  end;
end;

procedure TCastleApplication.SetMainWindow(const Value: TCastleWindow);
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

function TCastleApplication.GetOpenWindows(Index: integer): TCastleWindow;
begin
  result := FOpenWindows[Index];
end;

function TCastleApplication.OpenWindowsCount: integer;
begin
  result := FOpenWindows.Count;
end;

procedure TCastleApplication.OpenWindowsAdd(Window: TCastleWindow);
begin
  FOpenWindows.Add(Window);
end;

procedure TCastleApplication.OpenWindowsRemove(Window: TCastleWindow;
  QuitWhenLastWindowClosed: boolean);
begin
  if (FOpenWindows.Remove(Window) <> -1) and
     (OpenWindowsCount = 0) and
     QuitWhenLastWindowClosed then
    CloseAllOpenWindows;
end;

function TCastleApplication.FindWindow(Window: TCastleWindow): integer;
begin
  for result := 0 to OpenWindowsCount-1 do
    if OpenWindows[result] = Window then exit;
  result := -1;
end;

{$ifdef CASTLE_NINTENDO_SWITCH}
procedure CgeNxApplicationTerminate; cdecl; external;
{$endif}

procedure TCastleApplication.Terminate;
begin
  inherited;

  {$ifdef CASTLE_NINTENDO_SWITCH}
  CgeNxApplicationTerminate;
  {$endif}

  BackendTerminate;
end;

procedure TCastleApplication.CloseAllOpenWindows;
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

  Terminate;
end;

procedure TCastleApplication.UpdateAndRenderEverything(out WasAnyRendering: boolean);
var
  I: integer;
  Window: TCastleWindow;
begin
  WasAnyRendering := false;

  { We call ApplicationProperties._Update *right before rendering*, because:

     - This makes ApplicationProperties.OnUpdate callbacks have similar frequency
       as calls to window's OnRender callbacks, when the application
       is under a lot of stress (many messages).

       Otherwise, if we would move this call outside of
       "if .. not CheckMessage then ..." in castlewindow_winsystem.inc,
       then doing something
       that generates a lot of events (like moving the mouse)
       would make us generate a lot OnUpdate events,
       without many OnRender between. Which isn't actually prohibited...
       but it seems useless.

     - Calling OnUpdate later, only if "not WasAnyRendering", would
       also be bad, because then intensive redrawing (e.g. if you redraw
       every frame, with AutoRedisplay) would make OnUpdate called less
       often.

     In effect, we like to have OnUpdate called roughly as often as OnRender,
     even if we don't really guarantee it. }
  ApplicationProperties._Update;
  if Terminated then Exit;

  { Redraw some windows, and call window's OnUpdate.

    Gathering OnUpdate and OnRender of the same window together,
    we minimize the amount of needed MakeContextCurrent calls in case
    of applications with multiple windows.

    Remember that every callback may close the window, which also modifies
    the OpenWindows and OpenWindowsCount. The code below secures from
    all such changes, e.g. by copying "OpenWindows[I]" at the beginning,
    and checking "Window.Closed" all the time. }
  I := 0;
  while I < OpenWindowsCount do
  begin
    Window := OpenWindows[I];
    Window.DoUpdate;
    if Window.Closed then Continue {don't Inc(I)};
    if Terminated then Exit;
    Inc(I);
  end;

  ApplicationProperties._UpdateEnd;
  if Terminated then Exit;

  I := 0;
  while I < OpenWindowsCount do
  begin
    Window := OpenWindows[I];
    if Window.Invalidated then
    begin
      WasAnyRendering := true;
      Window.DoRender;
      if Window.Closed then Continue {don't Inc(I)};
      if Terminated then Exit;
    end else
      Window.Container.Fps.InternalSleeping;

    Inc(I);
  end;
end;

procedure TCastleApplication.UpdateAndRenderEverything;
var
  IgnoreWasAnyRendering: boolean;
begin
  UpdateAndRenderEverything(IgnoreWasAnyRendering);
end;

procedure TCastleApplication.MarkSleeping;
var
  I: Integer;
begin
  for I := 0 to OpenWindowsCount - 1 do
    OpenWindows[I].Container.Fps.InternalSleeping;
end;

function TCastleApplication.AllowSuspendForInput: boolean;
var
  I: Integer;
begin
  Result := ApplicationProperties.OnUpdate.Count = 0;
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

function TCastleApplication.VideoSettingsDescribe: String;
begin
  Result := '';
  if VideoResize then
    Result := Result + Format('  Screen size :  %dx%d', [VideoResizeWidth, VideoResizeHeight]) + nl;
  if VideoColorBits <> 0 then
    Result := Result + Format('  Color bits per pixel : %d', [VideoColorBits]) + nl;
  if VideoFrequency <> 0 then
    Result := Result + Format('  Display frequency : %d', [VideoFrequency]) + nl;

  if Result = '' then
    Result := '  No display settings change' + nl;
end;

procedure TCastleApplication.VideoChange(OnErrorWarnUserAndContinue: boolean);
var s: String;
begin
 if not TryVideoChange then
 begin
  s := 'Can''t change display settings to : ' + nl + VideoSettingsDescribe;

  {$ifndef CASTLE_WINDOW_HAS_VIDEO_CHANGE}
    S := S + ' (changing Video properties not implemented when CastleWindow is '+
      'made on top of ' + BackendName + ')';
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
  if ApplicationProperties.LimitFPS > 0 then
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
      { how long we should wait between _LimitFPS calls }
      1 / ApplicationProperties.LimitFPS -
      { how long we actually waited between _LimitFPS calls }
      TimerSeconds(NowTime, LastLimitFPSTime);
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

function TCastleApplication.GuessedMainWindow: TCastleWindow;
begin
  if MainWindow <> nil then
    Result := MainWindow else
  if OpenWindowsCount = 1 then
    Result := OpenWindows[0] else
    Result := nil; // no open window, or unknown which window is the main one
end;

{ Similar to TCustomApplication.HandleException, but not entirely.

  - We don't want to fallback on SysUtils.ShowException, that is rather
    useless because when IsConsole, it shows only a summary of exception,
    without any stacktrace.

  - Also we prefer our own ExceptMessage.

  - If we cannot show the exception, we prefer to simply reraise it,
    closing the program, with default FPC exception handler (that shows
    the stacktrace). }
procedure TCastleApplication.HandleException(Sender: TObject);

  {$ifndef FPC}
  function ExceptionBackTrace(const E: TObject): String;
  begin
    if E is Exception then
      Result := Exception(E).StackTrace
    else
      Result := '';
  end;
  {$endif}

  procedure DefaultShowException(ExceptObject: TObject; ExceptAddr: Pointer);
  var
    OriginalObj: TObject;
    OriginalAddr: Pointer;
    {$ifdef FPC}
    OriginalFrameCount: Longint;
    OriginalFrame: Pointer;
    {$endif}
    ErrMessage: String;
    ContinueApp: Boolean;
  begin
    ErrMessage := ExceptMessage(ExceptObject) + NL + NL +
      {$ifdef FPC} DumpExceptionBackTraceToString
      {$else} ExceptionBackTrace(ExceptObject)
      {$endif};
    { in case the following code, trying to handle the exception with nice GUI,
      will fail and crash horribly -- make sure to log the exception. }
    WritelnLog('Exception', ErrMessage);

    if (GuessedMainWindow <> nil) and
       (not GuessedMainWindow.Closed) and
       { for some weird reason (even though we try to protect from it in code)
         handling of GuessedMainWindow.MessageOK causes another exception
         that resulted in recursive call to HandleException.
         Prevent the loop with just crash in this case. }
       (not Theme.InternalMessageFallbackLook) then
    begin
      OriginalObj := ExceptObject;
      OriginalAddr := ExceptAddr;
      {$ifdef FPC}
      OriginalFrameCount := ExceptFrameCount;
      OriginalFrame := ExceptFrames;
      {$endif}
      ContinueApp := false; // initialize, in case MessageYesNo will make exception
      try
        Theme.InternalMessageFallbackLook := true;
        ContinueApp := GuessedMainWindow.MessageYesNo(
          'An error occurred. Try to continue the application?' + NL + NL +
          'Error details:' + NL + ErrMessage, mtError);
        Theme.InternalMessageFallbackLook := false;
      except
        on E: TObject do
        begin
          WritelnWarning('Exception', 'Exception ' + E.ClassName + ' occurred in the error handler itself. This means we cannot report the exception by a nice dialog box. The *original* exception report follows.');
          // TODO: ExceptProc on Delphi:
          {$ifdef FPC}
          ExceptProc(OriginalObj, OriginalAddr {$ifdef FPC}, OriginalFrameCount, OriginalFrame{$endif});
          WritelnWarning('Exception', 'And below is a report about the exception within exception handler.');
          ExceptProc(SysUtils.ExceptObject, SysUtils.ExceptAddr, SysUtils.ExceptFrameCount, SysUtils.ExceptFrames);
          { Setting ErrorAddr avoids HeapTrc outputting looong useless output
            (since memory leaks are normal when you exit with Halt(...)
            and no finalization blocks are run). }
          {$endif}
          ErrorAddr := OriginalAddr;
          Halt(1);
        end;
      end;

      if not ContinueApp then
      begin
        { Setting ErrorAddr avoids HeapTrc outputting looong useless output
          (since memory leaks are normal when you exit with Halt(...)
          and no finalization blocks are run). }
        ErrorAddr := OriginalAddr;
        Halt(1);
      end;
    end else
    begin
      { reraise, causing the app to exit with default FPC messsage and stacktrace }
      { not nice, as the stacktrace becomes overridden by this line in CastleWindow.pas:
      raise Exception.Create('Unhandled exception ' + ExceptMessage(ExceptObject, ExceptAddr));
      }
      { not correct, causes errors probably because ExceptObject is already freed:
      raise ExceptObject at ExceptAddr;
      }
      { this works best: }
      // TODO: ExceptProc
      {$ifdef FPC}
      ExceptProc(ExceptObject, ExceptAddr, ExceptFrameCount, ExceptFrames);
      {$endif}
      Halt(1);
    end;
  end;

begin
  if (not (ExceptObject is Exception)) or
     (not Assigned(OnException)) then
    DefaultShowException(ExceptObject, ExceptAddr)
  else
    OnException(Sender, Exception(ExceptObject));

  if StopOnException then
    Terminate;
end;

procedure TCastleApplication.DoLog(EventType : TEventType; const Msg : String);
begin
  WritelnLog('CastleWindow', Msg);
end;

procedure TCastleApplication.DoRun;
begin
  ProcessMessage(true, true);
end;

function TCastleApplication.ParseStandardParametersHelp: String;
begin
  Result :=
    OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
    OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
    OptionDescription('--log-file FILE-NAME', 'Write log to given file.') + NL +
    OptionDescription('--display X-DISPLAY-NAME', '(Unix) Display window on given X display.') + NL +
    OptionDescription('--no-limit-fps', 'Disable FPS limit. (We cap FPS by default, to save CPU and laptop battery.) Use this along with disabled V-Sync to see the maximum possible FPS.') + NL +
    OptionDescription('--capabilities automatic|force-fixed-function|force-modern', 'Force OpenGL context to have specific capabilities, to test rendering on modern or ancient GPUs.');
end;

// TODO: why this doesn't work as static TCastleApplication.OptionProc ?
procedure ApplicationOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: String; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  HelpString: String;
begin
  case OptionNum of
    0: begin
         HelpString :=
           ApplicationName + NL+
           NL+
           'Available command-line options:' + NL +
           Application.ParseStandardParametersHelp + NL +
           SoundEngine.ParseParametersHelp + NL+
           // do this regardless of MainWindow <> nil, as MainWindow may be assigned later
           TCastleWindow.ParseParametersHelp + NL +
           NL +
           'TCastleWindow backend: ' + Application.BackendName + '.' + NL +
           NL +
           ApplicationProperties.Description;
         InfoWrite(HelpString);
         Halt;
       end;
    1: begin
         // include ApplicationName in --version output, this is good for help2man
         Writeln(ApplicationName + ' ' + ApplicationProperties.Version);
         Halt;
       end;
    2: LogFileName := Argument;
    3: begin
         {$ifdef CASTLE_WINDOW_XLIB}
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
    4: ApplicationProperties.LimitFps := 0;
    5: TGLFeatures.RequestCapabilities := StrToCapabilities(Argument);
    else raise EInternalError.Create('ApplicationOptionProc: unhandled OptionNum');
  end;
end;

procedure TCastleApplication.ParseStandardParameters;
const
  Options: array [0..5] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'log-file'; Argument: oaRequired),
    (Short: #0 ; Long: 'display'; Argument: oaRequired),
    (Short: #0 ; Long: 'no-limit-fps'; Argument: oaNone),
    (Short: #0 ; Long: 'capabilities'; Argument: oaRequired)
  );

  {$ifdef DARWIN}
  procedure RemoveMacOsProcessSerialNumber;
  var
    I: Integer;
  begin
    for I := 1 to Parameters.Count - 1 do
      if IsPrefix('-psn_', Parameters[I], false) then
      begin
        Parameters.Delete(I);
        Exit;
      end;
  end;
  {$endif}

begin
  {$ifndef FPC}
  try
  {$endif}

    {$ifdef DARWIN}
    RemoveMacOsProcessSerialNumber;
    {$endif}

    SoundEngine.ParseParameters;

    if MainWindow <> nil then
      MainWindow.ParseParameters;

    Parameters.Parse(Options, @ApplicationOptionProc, Self, true);

  { With FPC, if something here raises an exception,
    we just let it be unhandled and stop the application.
    FPC makes by default a nice error box, on Windows too.
    However Delphi crashes with SEGFAULT on Windows,
    testcase:

    - cd examples/research_special_rendering_methods/test_rendering_opengl_capabilities
    - ./test_rendering_opengl_capabilities_standalone.exe --capabilities=something-invalid
    - or
      ./test_rendering_opengl_capabilities_standalone.exe --no-sound=excessive-argument
    - reproducible also in Delphi debugger, however without any useful backtrace.

    Workaround for now is just to capture exception here,
    and display a nice error box ourselves. }

  {$ifndef FPC}
  except
    on E: TObject do
    begin
      ErrorWrite(ExceptMessage(E));
      Halt(1);
    end;
  end;
  {$endif}
end;

function TCastleApplication.OpenGLES: Boolean;
begin
  (* Note that CGE own code can just use
       {$ifdef OpenGLES}
     instead of runtime check
       if Application.OpenGLES

     However, for user code, we don't want to expose this as a define.
     - Because user code can be compiled in various ways.
       Our build tool can define some CGE-specific defines,
       but user can also compile without our build tool.
     - Some day, renderer selection may be possible at runtime.

     So user code must use
       if Application.OpenGLES
     (or test with $ifdefs for specific platforms, like ANDROID or CASTLE_IOS.)
  *)

  Result := {$ifdef OpenGLES} true {$else} false {$endif};
end;

{$ifdef FPC}

function TCastleApplication.GetLimitFPS: Single;
begin
  Result := ApplicationProperties.LimitFPS;
end;

procedure TCastleApplication.SetLimitFPS(const Value: Single);
begin
  ApplicationProperties.LimitFPS := Value;
end;

function TCastleApplication.GetVersion: String;
begin
  Result := ApplicationProperties.Version;
end;

procedure TCastleApplication.SetVersion(const Value: String);
begin
  ApplicationProperties.Version := Value;
end;

function TCastleApplication.GetTouchDevice: boolean;
begin
  Result := ApplicationProperties.TouchDevice;
end;

procedure TCastleApplication.SetTouchDevice(const Value: boolean);
begin
  ApplicationProperties.TouchDevice := Value;
end;

{$endif}

{ global --------------------------------------------------------------------- }

function KeyToString(const KeyString: String; const Key: TKey;
  const Modifiers: TModifierKeys; out S: String): boolean;
const
  {$ifdef CASTLE_WINDOW_FORM}
    {$ifdef LCLCarbon}
      {$define CASTLE_CtrlIsCommand}
    {$endif}
  {$endif}
  // Our Cocoa backend always reports Command modifier as Ctrl.
  {$ifdef CASTLE_WINDOW_COCOA}
    {$define CASTLE_CtrlIsCommand}
  {$endif}
  CtrlIsCommand = {$ifdef CASTLE_CtrlIsCommand} true {$else} false {$endif};
begin
  if KeyString <> '' then
  begin
    S := KeyStringToNiceStr(KeyString, Modifiers, false, CtrlIsCommand);
    Result := true;
  end else
  if Key <> keyNone then
  begin
    S := KeyToStr(Key, Modifiers, CtrlIsCommand);
    Result := true;
  end else
  Result := false;
end;

function KeyString(const AKeyString: String; const Key: TKey; const Modifiers: TModifierKeys;
  out S: String): boolean;
begin
  Result := KeyToString(AKeyString, Key, Modifiers, S);
end;

function Application: TCastleApplication;
begin
  { In case of UnitFinalization,
    return nil (and acccessing it should cause an error),
    since we wouldn't free it if we create a new one. }
  if (FApplication = nil) and not UnitFinalization then
    FApplication := TCastleApplication.Create(nil);
  Result := FApplication;
end;

{ init/fini --------------------------------------------------------------- }

initialization
  ApplicationProperties._FileAccessSafe := false;
finalization
  UnitFinalization := true;

  { Instead of using FreeAndNil, just call Free.
    In our destructor we take care of setting Application variable to @nil,
    when it becomes really useless.

    Otherwise FreeAndNil first nils, then frees Application, and we really
    want to keep Application during first stage of TCastleApplication destruction:
    when calling Quit, which may close windows, which may use Application
    variable in their Close or CloseBackend implementations. }
  Application.Free;
  Assert(Application = nil);

  { Order is important: we free MenuItems, which are needed
    by TMenu destructor. And some TCastleWindow instances may be freed
    only by Application destructor (when they are owned by Application). }
  FreeAndNil(FMenuItems);
end.
