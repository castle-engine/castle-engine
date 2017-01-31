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

{ NPAPI (Mozilla API for plugins).

  Parts based on npapi.h from NPAPI SDK, license:
  Version: MPL 1.1/GPL 2.0/LGPL 2.1.

  @exclude Not ready for PasDoc. Also, internal for the engine.
}
unit CastleNPAPI;

{$I castleconf.inc}

{$PACKRECORDS C}
{$PACKENUM 4} // is default in ObjPas anyway, see http://www.freepascal.org/docs-html/prog/progsu59.html

{$I castle_npapi.inc}

interface

uses CTypes {$ifdef MOZ_X11}, Xlib, X {$endif};

{*----------------------------------------------------------------------*/
/*                        Plugin Version Constants                      */
/*----------------------------------------------------------------------*}

const
  NP_VERSION_MAJOR = 0;
  NP_VERSION_MINOR = 28;


  // The OS/2 version of Netscape uses RC_DATA to define the
  // mime types, file extensions, etc that are required.
  // Use a vertical bar to separate types, end types with \0.
  // FileVersion and ProductVersion are 32bit ints, all other
  // entries are strings that MUST be terminated with a \0.

  //  AN EXAMPLE:

  //  RCDATA NP_INFO_ProductVersion { 1,0,0,1,}

  //  RCDATA NP_INFO_MIMEType    { "video/x-video|",
  //                               "video/x-flick\0" }
  //  RCDATA NP_INFO_FileExtents { "avi|",
  //                               "flc\0" }
  //  RCDATA NP_INFO_FileOpenName{ "MMOS2 video player(*.avi)|",
  //                               "MMOS2 Flc/Fli player(*.flc)\0" }

  //  RCDATA NP_INFO_FileVersion       { 1,0,0,1 }
  //  RCDATA NP_INFO_CompanyName       { "Netscape Communications\0" }
  //  RCDATA NP_INFO_FileDescription   { "NPAVI32 Extension DLL\0"
  //  RCDATA NP_INFO_InternalName      { "NPAVI32\0" )
  //  RCDATA NP_INFO_LegalCopyright    { "Copyright Netscape Communications \251 1996\0"
  //  RCDATA NP_INFO_OriginalFilename  { "NVAPI32.DLL" }
  //  RCDATA NP_INFO_ProductName       { "NPAVI32 Dynamic Link Library\0" }

  { RC_DATA types for version info - required }
  NP_INFO_ProductVersion      = 1;
  NP_INFO_MIMEType            = 2;
  NP_INFO_FileOpenName        = 3;
  NP_INFO_FileExtents         = 4;
  { RC_DATA types for version info - used if found }
  NP_INFO_FileDescription     = 5;
  NP_INFO_ProductName         = 6;
  { RC_DATA types for version info - optional }
  NP_INFO_CompanyName         = 7;
  NP_INFO_FileVersion         = 8;
  NP_INFO_InternalName        = 9;
  NP_INFO_LegalCopyright      = 10;
  NP_INFO_OriginalFilename    = 11;

{*----------------------------------------------------------------------*/
/*                       Definition of Basic Types                      */
/*----------------------------------------------------------------------*}

type
  { NPAPI boolean. Allowed values: bFalse = 0, bTrue = 1. }
  TNPBool = Byte;
  PNPBool = ^TNPBool;

  { PRBool from XulRunner.
    Careful: this has 4 byte size, while TNPBool is 1 byte. }
  TPRBool = (PR_FALSE = 0, PR_TRUE = 1);
  PPRBool = ^TPRBool;

  TNPError = CInt16;
  TNPReason = CInt16;
  TNPMIMEType = PChar;

{*----------------------------------------------------------------------*/
/*                       Structures and definitions                     */
/*----------------------------------------------------------------------*}

  {*
   *  NPP is a plug-in's opaque instance handle
   *}
  TNPP = record
    pdata: Pointer;      //< plug-in private data
    ndata: Pointer;      //< netscape private data
  end;
  PNPP = ^TNPP;

  TNPStream = record
    pdata: Pointer; //< plug-in private data
    ndata: Pointer; //< netscape private data
    url: PChar;
    streamEnd: CUInt32;
    lastmodified: CUInt32;
    notifyData: Pointer;
    headers: PChar; {< Response headers from host.
                             * Exists only for >= NPVERS_HAS_RESPONSE_HEADERS.
                             * Used for HTTP only; NULL for non-HTTP.
                             * Available from NPP_NewStream onwards.
                             * Plugin should copy this data before storing it.
                             * Includes HTTP status line and all headers,
                             * preferably verbatim as received from server,
                             * headers formatted as in HTTP ("Header: Value"),
                             * and newlines (\n, NOT \r\n) separating lines.
                             * Terminated by \n\0 (NOT \n\n\0). *}
  end;
  PNPStream = ^TNPStream;
  PPNPStream = ^PNPStream;

  PNPByteRange = ^TNPByteRange;
  TNPByteRange = record
    offset: CInt32; //< negative offset means from the end
    length: CUInt32;
    next: PNPByteRange;
  end;

  TNPSavedData = record
    len: CInt32;
    buf: Pointer;
  end;
  PNPSavedData = ^TNPSavedData;
  PPNPSavedData = ^PNPSavedData;

  TNPRect = record
    top: CUInt16;
    left: CUInt16;
    bottom: CUInt16;
    right: CUInt16;
  end;
  PNPRect = ^TNPRect;

  TNPSize = record
    width: CInt32;
    height: CInt32;
  end;

  TNPFocusDirection = (
    NPFocusNext = 0,
    NPFocusPrevious = 1
  );

const
 { Return values for NPP_HandleEvent }
  kNPEventNotHandled = 0;
  kNPEventHandled = 1;
  { Exact meaning must be spec'd in event model. }
  kNPEventStartIME = 2;

{$ifdef UNIX}
{*
 * Unix specific structures and definitions
 *}

{*
 * Callback Structures.
 *
 * These are used to pass additional platform specific information.
 *}
const
  NP_SETWINDOW = 1;
  NP_PRINT = 2;

type
  TNPAnyCallbackStruct = record
    aType: CInt32;
  end;

  TNPSetWindowCallbackStruct = record
    aType: CInt32;
    {$ifdef MOZ_X11}
    display: Xlib.PDisplay;
    visual: Xlib.PVisual;
    colormap: X.TColormap;
    depth: CUint;
    {$endif}
  end;
  PNPSetWindowCallbackStruct = ^TNPSetWindowCallbackStruct;

  TNPPrintCallbackStruct = record
    aType: CInt32;
    fp: Pointer; //< In C, this is actually a FILE* pointer, useless from Pascal.
  end;

{$endif UNIX}

type
  TNPDrawingModel = (
    NPDrawingModelDUMMY = 0

{$ifdef DARWIN}
{$ifndef NP_NO_QUICKDRAW}
  , NPDrawingModelQuickDraw = 0
{$endif}
  , NPDrawingModelCoreGraphics = 1
  , NPDrawingModelOpenGL = 2
  , NPDrawingModelCoreAnimation = 3
  , NPDrawingModelInvalidatingCoreAnimation = 4
{$endif}

{$ifdef MSWINDOWS}
  , NPDrawingModelSyncWin = 5
{$endif}

{$ifdef MOZ_X11}
  , NPDrawingModelSyncX = 6
{$endif}

  //#if 0 /* OBSOLETE */
  //  , NPDrawingModelAsyncBitmapSurfaceOBSOLETE = 7
  //#if defined(XP_WIN)
  //  , NPDrawingModelAsyncWindowsDXGISurfaceOBSOLETE = 8
  //{$endif}
  //{$endif}
);

{$ifdef DARWIN}
type
  TNPEventModel = (
{$ifndef NP_NO_CARBON}
  NPEventModelCarbon = 0,
{$endif}
  NPEventModelCocoa = 1
} ;
{$endif}

{*
 *   The following masks are applied on certain platforms to NPNV and
 *   NPPV selectors that pass around pointers to COM interfaces. Newer
 *   compilers on some platforms may generate vtables that are not
 *   compatible with older compilers. To prevent older plugins from
 *   not understanding a new browser's ABI, these masks change the
 *   values of those selectors on those platforms. To remain backwards
 *   compatible with different versions of the browser, plugins can
 *   use these masks to dynamically determine and use the correct C++
 *   ABI that the browser is expecting. This does not apply to Windows
 *   as Microsoft's COM ABI will likely not change.
 *}
const
  NP_ABI_GCC3_MASK = $10000000;
  _NP_ABI_MIXIN_FOR_GCC3 = 0;

  {$ifdef DARWIN}
  TODO test this.
  NP_ABI_MACHO_MASK = $01000000;
  _NP_ABI_MIXIN_FOR_MACHO = NP_ABI_MACHO_MASK;
  {$else}
  _NP_ABI_MIXIN_FOR_MACHO = 0;
  {$endif}

  NP_ABI_MASK = _NP_ABI_MIXIN_FOR_GCC3 or _NP_ABI_MIXIN_FOR_MACHO;

{*
 * List of variable names for which NPP_GetValue shall be implemented
 *}
type
  TNPPVariable = (
    NPPVpluginNameString = 1,
    NPPVpluginDescriptionString = 2,
    NPPVpluginWindowBool = 3,
    NPPVpluginTransparentBool = 4,
    NPPVjavaClass = 5,
    NPPVpluginWindowSize = 6,
    NPPVpluginTimerInterval = 7,
    NPPVpluginScriptableInstance = 10 or NP_ABI_MASK,
    NPPVpluginScriptableIID = 11,
    NPPVjavascriptPushCallerBool = 12,
    NPPVpluginKeepLibraryInMemory = 13,
    NPPVpluginNeedsXEmbed         = 14,

    { Get the NPObject for scripting the plugin. Introduced in NPAPI minor version 14. }
    NPPVpluginScriptableNPObject  = 15,

    { Get the plugin value (as \0-terminated UTF-8 string data) for
      form submission if the plugin is part of a form. Use
      NPN_MemAlloc() to allocate memory for the string data. Introduced
      in NPAPI minor version 15. }
    NPPVformValue = 16,

    NPPVpluginUrlRequestsDisplayedBool = 17,

    { Checks if the plugin is interested in receiving the http body of
      all http requests (including failed ones, http status != 200). }
    NPPVpluginWantsAllNetworkStreams = 18,

    { Browsers can retrieve a native ATK accessibility plug ID via this variable. }
    NPPVpluginNativeAccessibleAtkPlugId = 19,

    { Checks to see if the plug-in would like the browser to load the "src" attribute. }
    NPPVpluginCancelSrcStream = 20,

    NPPVsupportsAdvancedKeyHandling = 21,

    NPPVpluginUsesDOMForCursorBool = 22,

    { Used for negotiating drawing models }
    NPPVpluginDrawingModel = 1000

    {$ifdef DARWIN}
    { Used for negotiating event models }
    , NPPVpluginEventModel = 1001
    { In the NPDrawingModelCoreAnimation drawing model, the browser asks the plug-in for a Core Animation layer. }
    , NPPVpluginCoreAnimationLayer = 1003
    {$endif}

    // #if defined(MOZ_PLATFORM_MAEMO) && ((MOZ_PLATFORM_MAEMO == 5) || (MOZ_PLATFORM_MAEMO == 6))
    //   , NPPVpluginWindowlessLocalBool = 2002
    // {$endif}

    { Notification that the plugin just started or stopped playing audio }
    , NPPVpluginIsPlayingAudio = 4000
  );

  {*
   * List of variable names for which NPN_GetValue should be implemented.
   *}
  TNPNVariable = (
    NPNVxDisplay = 1,
    NPNVxtAppContext = 2,
    NPNVnetscapeWindow = 3,
    NPNVjavascriptEnabledBool = 4,
    NPNVasdEnabledBool = 5,
    NPNVisOfflineBool = 6,

    NPNVserviceManager = (10 or NP_ABI_MASK),
    NPNVDOMElement     = (11 or NP_ABI_MASK),
    NPNVDOMWindow      = (12 or NP_ABI_MASK),
    NPNVToolkit        = (13 or NP_ABI_MASK),
    NPNVSupportsXEmbedBool = 14,

    { Get the NPObject wrapper for the browser window. }
    NPNVWindowNPObject = 15,

    { Get the NPObject wrapper for the plugins DOM element. }
    NPNVPluginElementNPObject = 16,

    NPNVSupportsWindowless = 17,

    NPNVprivateModeBool = 18,

    NPNVsupportsAdvancedKeyHandling = 21,

    NPNVdocumentOrigin = 22,

    NPNVpluginDrawingModel = 1000 //< Get the current drawing model (NPDrawingModel)
  {$ifdef DARWIN}
    , NPNVcontentsScaleFactor = 1001
  {$ifndef NP_NO_QUICKDRAW}
    , NPNVsupportsQuickDrawBool = 2000
  {$endif}
    , NPNVsupportsCoreGraphicsBool = 2001
    , NPNVsupportsOpenGLBool = 2002
    , NPNVsupportsCoreAnimationBool = 2003
    , NPNVsupportsInvalidatingCoreAnimationBool = 2004
  {$endif}
  // #if 0 /* OBSOLETE */
  //   , NPNVsupportsAsyncBitmapSurfaceBoolOBSOLETE = 2007
  // #if defined(XP_WIN)
  //   , NPNVsupportsAsyncWindowsDXGISurfaceBoolOBSOLETE = 2008
  // {$endif}
  // {$endif}
  {$ifdef DARWIN}
  {$ifndef NP_NO_CARBON}
    , NPNVsupportsCarbonBool = 3000 //< TRUE if the browser supports the Carbon event model
  {$endif}
    , NPNVsupportsCocoaBool = 3001 //< TRUE if the browser supports the Cocoa event model
    , NPNVsupportsUpdatedCocoaTextInputBool = 3002 //< TRUE if the browser supports the updated Cocoa text input specification.
    , NPNVmuteAudioBool = 4000 //< Request that the browser wants to mute or unmute the plugin
    , NPNVsupportsCompositingCoreAnimationPluginsBool = 74656 //< TRUE if the browser supports CA model compositing
  {$endif}
  // #if defined(MOZ_PLATFORM_MAEMO) && ((MOZ_PLATFORM_MAEMO == 5) || (MOZ_PLATFORM_MAEMO == 6))
  //   , NPNVSupportsWindowlessLocal = 2002
  // {$endif}
  );

  TNPNURLVariable = (
    NPNURLVCookie = 501,
    NPNURLVProxy = 502
  );

  {*
   * The type of Toolkit the widgets use
   *}
  TNPNToolkitType = (
    NPNVGtk12 = 1,
    NPNVGtk2 = 2
  );

  {*
   * The type of a NPWindow - it specifies the type of the data structure
   * returned in the window field.
   *}
  TNPWindowType = (
    NPWindowTypeWindow = 1,
    NPWindowTypeDrawable = 2
  );

  TNPWindow = record
    window: Pointer;  {< Platform specific window handle }
    { Position of top left corner relative to a netscape page.
      OS/2:  Position of bottom left corner relative to visible netscape window.
      @groupBegin }
    x: CInt32;
    y: CInt32;
    { @groupEnd }
    { Maximum window size }
    width: CUInt32;
    height: CUInt32;
    clipRect: TNPRect; //< Clipping rectangle in port coordinates
    {$ifdef UNIX}
    {$ifndef DARWIN}
    ws_info: Pointer; //< Platform-dependent additional data
    {$endif}
    {$endif}
    aType: TNPWindowType ; //< Is this a window or a drawable?
  end;
  PNPWindow = ^TNPWindow;

  TNPImageExpose = record
    data: PChar;       //< image pointer
    stride: CInt32;     //< Stride of data image pointer
    depth: CInt32;      //< Depth of image pointer
    x: CInt32;          //< Expose x
    y: CInt32;          //< Expose y
    width: CUInt32;      //< Expose width
    height: CUInt32;     //< Expose height
    dataSize: TNPSize;   //< Data buffer size
    translateX: Single; //< translate X matrix value
    translateY: Single; //< translate Y matrix value
    scaleX: Single;     //< scale X matrix value
    scaleY: Single;     //< scale Y matrix value
  end;

  TNPFullPrint = record
    pluginPrinted: TNPBool; //< Set TRUE if plugin handled fullscreen printing
    printOne: TNPBool;      //< TRUE if plugin should print one copy to default printer
    platformPrint: Pointer; //< Platform-specific printing info
  end;

  TNPEmbedPrint = record
    window: TNPWindow;
    platformPrint: Pointer; //< Platform-specific printing info
  end;

  TNPPrint = record
    mode: CUInt16;             //< NP_FULL or NP_EMBED
    case Integer of
      0: (fullPrint: TNPFullPrint);   //< if mode is NP_FULL
      1: (embedPrint: TNPEmbedPrint); //< if mode is NP_EMBED
  end;
  PNPPrint = ^TNPPrint;

  TNPRegion = Pointer;

  TNPMenu = Pointer;
  PNPMenu = ^TNPMenu;

(* TODO: section below not translated yet:

{$ifdef DARWIN}
{$ifndef NP_NO_CARBON}
typedef EventRecord NPEvent;
{$endif}
#elif defined(XP_SYMBIAN)
typedef QEvent NPEvent;
#elif defined(XP_WIN)
typedef struct _NPEvent
{
  CUInt16 event;
  uintptr_t wParam;
  uintptr_t lParam;
} NPEvent;
#elif defined(XP_OS2)
typedef struct _NPEvent
{
  CUInt32 event;
  CUInt32 wParam;
  CUInt32 lParam;
} NPEvent;
#elif defined(XP_UNIX) && defined(MOZ_X11)
typedef XEvent NPEvent;
{$else}
typedef Pointer  NPEvent;
{$endif}

{$ifdef DARWIN}
typedef Pointer NPRegion;
{$ifndef NP_NO_QUICKDRAW}
typedef RgnHandle NPQDRegion;
{$endif}
typedef CGPathRef NPCGRegion;
#elif defined(XP_WIN)
typedef HRGN NPRegion;
#elif defined(XP_UNIX) && defined(MOZ_X11)
typedef Region NPRegion;
#elif defined(XP_SYMBIAN)
typedef QRegion* NPRegion;
{$else}
typedef void *NPRegion;
{$endif}

typedef struct _NPNSString NPNSString;
typedef struct _NPNSWindow NPNSWindow;
typedef struct _NPNSMenu   NPNSMenu;

{$ifdef DARWIN}
typedef NPNSMenu NPMenu;
{$else}
typedef void *NPMenu;
{$endif}
*)

type
  TNPCoordinateSpace = (
    NPCoordinateSpacePlugin = 1,
    NPCoordinateSpaceWindow = 2,
    NPCoordinateSpaceFlippedWindow = 3,
    NPCoordinateSpaceScreen = 4,
    NPCoordinateSpaceFlippedScreen = 5
  );

(* TODO: section below not translated yet:

{$ifdef DARWIN}

{$ifndef NP_NO_QUICKDRAW}
typedef struct NP_Port
{
  CGrafPtr port;
  CInt32 portx; //< position inside the topmost window
  CInt32 porty;
} NP_Port;
{$endif} /* NP_NO_QUICKDRAW */

/*
 * NP_CGContext is the type of the NPWindow's 'window' when the plugin specifies NPDrawingModelCoreGraphics
 * as its drawing model.
 */

typedef struct NP_CGContext
{
  CGContextRef context;
  void *window; /* A WindowRef under the Carbon event model. */
} NP_CGContext;

/*
 * NP_GLContext is the type of the NPWindow's 'window' when the plugin specifies NPDrawingModelOpenGL as its
 * drawing model.
 */

#if !NP_NO_OPENGL
typedef struct NP_GLContext
{
  CGLContextObj context;
#ifdef NP_NO_CARBON
  NPNSWindow *window;
{$else}
  void *window; /* Can be either an NSWindow or a WindowRef depending on the event model */
{$endif}
} NP_GLContext;
{$endif} /* !NP_NO_OPENGL */

typedef enum {
  NPCocoaEventDrawRect = 1,
  NPCocoaEventMouseDown,
  NPCocoaEventMouseUp,
  NPCocoaEventMouseMoved,
  NPCocoaEventMouseEntered,
  NPCocoaEventMouseExited,
  NPCocoaEventMouseDragged,
  NPCocoaEventKeyDown,
  NPCocoaEventKeyUp,
  NPCocoaEventFlagsChanged,
  NPCocoaEventFocusChanged,
  NPCocoaEventWindowFocusChanged,
  NPCocoaEventScrollWheel,
  NPCocoaEventTextInput
} NPCocoaEventType;

typedef struct _NPCocoaEvent {
  NPCocoaEventType type;
  CUInt32 version;
  union {
    struct {
      CUInt32 modifierFlags;
      double   pluginX;
      double   pluginY;
      CInt32  buttonNumber;
      CInt32  clickCount;
      double   deltaX;
      double   deltaY;
      double   deltaZ;
    } mouse;
    struct {
      CUInt32    modifierFlags;
      NPNSString *characters;
      NPNSString *charactersIgnoringModifiers;
      NPBool      isARepeat;
      CUInt16    keyCode;
    } key;
    struct {
      CGContextRef context;
      double x;
      double y;
      double width;
      double height;
    } draw;
    struct {
      NPBool hasFocus;
    } focus;
    struct {
      NPNSString *text;
    } text;
  } data;
} NPCocoaEvent;

{$ifndef NP_NO_CARBON}
/* Non-standard event types that can be passed to HandleEvent */
enum NPEventType {
  NPEventType_GetFocusEvent = (osEvt + 16),
  NPEventType_LoseFocusEvent,
  NPEventType_AdjustCursorEvent,
  NPEventType_MenuCommandEvent,
  NPEventType_ClippingChangedEvent,
  NPEventType_ScrollingBeginsEvent = 1000,
  NPEventType_ScrollingEndsEvent
};
{$endif} /* NP_NO_CARBON */

{$endif} /* XP_MACOSX */
*)

const
  {*
   * Values for mode passed to NPP_New:
   *}
  NP_EMBED = 1;
  NP_FULL  = 2;

  {*
   * Values for stream type passed to NPP_NewStream:
   *}
  NP_NORMAL     = 1;
  NP_SEEK       = 2;
  NP_ASFILE     = 3;
  NP_ASFILEONLY = 4;

  //NP_MAXREADY = (((unsigned)(~0)<<1)>>1)

  {*
   * Flags for NPP_ClearSiteData.
   *}
  NP_CLEAR_ALL   = 0;
  NP_CLEAR_CACHE = (1 << 0);

  {*----------------------------------------------------------------------*/
  /*       Error and Reason Code definitions                              */
  /*----------------------------------------------------------------------*}

  {*
   * Values of type TNPError:
   *}
  NPERR_BASE                        = 0;
  NPERR_NO_ERROR                    = (NPERR_BASE + 0);
  NPERR_GENERIC_ERROR               = (NPERR_BASE + 1);
  NPERR_INVALID_INSTANCE_ERROR      = (NPERR_BASE + 2);
  NPERR_INVALID_FUNCTABLE_ERROR     = (NPERR_BASE + 3);
  NPERR_MODULE_LOAD_FAILED_ERROR    = (NPERR_BASE + 4);
  NPERR_OUT_OF_MEMORY_ERROR         = (NPERR_BASE + 5);
  NPERR_INVALID_PLUGIN_ERROR        = (NPERR_BASE + 6);
  NPERR_INVALID_PLUGIN_DIR_ERROR    = (NPERR_BASE + 7);
  NPERR_INCOMPATIBLE_VERSION_ERROR  = (NPERR_BASE + 8);
  NPERR_INVALID_PARAM               = (NPERR_BASE + 9);
  NPERR_INVALID_URL                 = (NPERR_BASE + 10);
  NPERR_FILE_NOT_FOUND              = (NPERR_BASE + 11);
  NPERR_NO_DATA                     = (NPERR_BASE + 12);
  NPERR_STREAM_NOT_SEEKABLE         = (NPERR_BASE + 13);
  NPERR_TIME_RANGE_NOT_SUPPORTED    = (NPERR_BASE + 14);
  NPERR_MALFORMED_SITE              = (NPERR_BASE + 15);

  {*
   * Values of type NPReason:
   *}
  NPRES_BASE         = 0;
  NPRES_DONE         = (NPRES_BASE + 0);
  NPRES_NETWORK_ERR  = (NPRES_BASE + 1);
  NPRES_USER_BREAK   = (NPRES_BASE + 2);

  {*
   * Don't use these obsolete error codes any more.
   *}
  // NP_NOERR  NP_NOERR_is_obsolete_use_NPERR_NO_ERROR
  // NP_EINVAL NP_EINVAL_is_obsolete_use_NPERR_GENERIC_ERROR
  // NP_EABORT NP_EABORT_is_obsolete_use_NPRES_USER_BREAK

  {*
   * Version feature information
   *}
  NPVERS_HAS_STREAMOUTPUT             = 8;
  NPVERS_HAS_NOTIFICATION             = 9;
  NPVERS_HAS_LIVECONNECT              = 9;
  NPVERS_68K_HAS_LIVECONNECT          = 11;
  NPVERS_HAS_WINDOWLESS               = 11;
  NPVERS_HAS_XPCONNECT_SCRIPTING      = 13;
  NPVERS_HAS_NPRUNTIME_SCRIPTING      = 14;
  NPVERS_HAS_FORM_VALUES              = 15;
  NPVERS_HAS_POPUPS_ENABLED_STATE     = 16;
  NPVERS_HAS_RESPONSE_HEADERS         = 17;
  NPVERS_HAS_NPOBJECT_ENUM            = 18;
  NPVERS_HAS_PLUGIN_THREAD_ASYNC_CALL = 19;
  NPVERS_HAS_ALL_NETWORK_STREAMS      = 20;
  NPVERS_HAS_URL_AND_AUTH_INFO        = 21;
  NPVERS_HAS_PRIVATE_MODE             = 22;
  NPVERS_MACOSX_HAS_COCOA_EVENTS      = 23;
  NPVERS_HAS_ADVANCED_KEY_HANDLING    = 25;
  NPVERS_HAS_URL_REDIRECT_HANDLING    = 26;
  NPVERS_HAS_CLEAR_SITE_DATA          = 27;

{*----------------------------------------------------------------------*/
/*                        Function Prototypes                           */
/*----------------------------------------------------------------------*}

type
  TNPN_ScheduleTimerCallback = procedure(npp: PNPP; timerID: CUInt32); extdecl;
  TNPN_PluginThreadAsyncCallCallback = procedure(userData: Pointer); extdecl;

  { NPP_* functions are provided by the plugin and called by the navigator. }
  {$ifdef UNIX}
  TNPP_GetMIMEDescription = function (): PChar; extdecl;
  {$endif}
  TNPP_NewProcPtr = function (pluginType: TNPMIMEType; instance: PNPP; mode: CUInt16; argc: CInt16; argn: PPChar; argv: PPChar; saved: PNPSavedData): TNPError; extdecl;
  TNPP_DestroyProcPtr = function (instance: PNPP; save: PPNPSavedData): TNPError; extdecl;
  TNPP_SetWindowProcPtr = function (instance: PNPP; window: PNPWindow): TNPError; extdecl;
  TNPP_NewStreamProcPtr = function (instance: PNPP; aType: TNPMIMEType; stream: PNPStream; seekable: TNPBool; stype: PCUInt16): TNPError; extdecl;
  TNPP_DestroyStreamProcPtr = function (instance: PNPP; stream: PNPStream; reason: TNPReason): TNPError; extdecl;
  TNPP_WriteReadyProcPtr = function (instance: PNPP; stream: PNPStream): CInt32; extdecl;
  TNPP_WriteProcPtr = function (instance: PNPP; stream: PNPStream; offset: CInt32; len: CInt32; buffer: Pointer): CInt32; extdecl;
  TNPP_StreamAsFileProcPtr = procedure (instance: PNPP; stream: PNPStream; fname: PChar); extdecl;
  TNPP_PrintProcPtr = procedure (instance: PNPP; platformPrint: PNPPrint); extdecl;
  TNPP_HandleEventProcPtr = function (instance: PNPP; event: Pointer): CInt16; extdecl;
  TNPP_URLNotifyProcPtr = procedure (instance: PNPP; url: PChar; reason: TNPReason; notifyData: Pointer); extdecl;
  {* Any NPObjects returned to the browser via NPP_GetValue should be retained
     by the plugin on the way out. The browser is responsible for releasing. *}
  TNPP_GetValueProcPtr = function (instance: PNPP; variable: TNPPVariable; value: Pointer): TNPError; extdecl;
  TNPP_SetValueProcPtr = function (instance: PNPP; variable: TNPNVariable; value: Pointer): TNPError; extdecl;
  TNPP_GotFocusProcPtr = function (instance: PNPP; direction: TNPFocusDirection): TNPBool; extdecl;
  TNPP_LostFocusProcPtr = procedure (instance: PNPP); extdecl;
  TNPP_URLRedirectNotifyProcPtr = procedure (instance: PNPP; url: PChar; status: CInt32; notifyData: Pointer); extdecl;
  TNPP_ClearSiteDataProcPtr = function (site: PChar; flags: CUInt64; maxAge: CUInt64): TNPError; extdecl;
  TNPP_GetSitesWithDataProcPtr = function (): PPChar; extdecl;
  TNPP_DidCompositeProcPtr = procedure (instance: PNPP); extdecl;

  { NPN_* functions are provided by the navigator and called by the plugin. }
  TNPN_VersionProcPtr = procedure (plugin_major: PCInt; plugin_minor: PCInt; netscape_major: PCInt; netscape_minor: PCInt); extdecl;
  TNPN_GetURLNotifyProcPtr = function (instance: PNPP; url: PChar; target: PChar; notifyData: Pointer): TNPError; extdecl;
  TNPN_GetURLProcPtr = function (instance: PNPP; url: PChar; target: PChar): TNPError; extdecl;
  TNPN_PostURLNotifyProcPtr = function (instance: PNPP; url: PChar; target: PChar; len: CUInt32; buf: PChar; aFile: TNPBool; notifyData: Pointer): TNPError; extdecl;
  TNPN_PostURLProcPtr = function (instance: PNPP; url: PChar; target: PChar; len: CUInt32; buf: PChar; aFile: TNPBool): TNPError; extdecl;
  TNPN_RequestReadProcPtr = function (stream: PNPStream; rangeList: PNPByteRange): TNPError; extdecl;
  TNPN_NewStreamProcPtr = function (instance: PNPP; aType: TNPMIMEType; target: PChar; stream: PPNPStream): TNPError; extdecl;
  TNPN_WriteProcPtr = function (instance: PNPP; stream: PNPStream; len: CInt32; buffer: Pointer): CInt32; extdecl;
  TNPN_DestroyStreamProcPtr = function (instance: PNPP; stream: PNPStream; reason: TNPReason): TNPError; extdecl;
  TNPN_StatusProcPtr = procedure (instance: PNPP; message: PChar); extdecl;
  {* Browser manages the lifetime of the buffer returned by NPN_UserAgent, don't
     depend on it sticking around and don't free it. *}
  TNPN_UserAgentProcPtr = function (instance: PNPP): PChar; extdecl;
  TNPN_MemAllocProcPtr = function (size: CUInt32): Pointer; extdecl;
  TNPN_MemFreeProcPtr = procedure (ptr: Pointer); extdecl;
  TNPN_MemFlushProcPtr = function (size: CUInt32): CUInt32; extdecl;
  TNPN_ReloadPluginsProcPtr = procedure (reloadPages: TNPBool); extdecl;
  TNPN_GetValueProcPtr = function (instance: PNPP; variable: TNPNVariable; value: Pointer): TNPError; extdecl;
  TNPN_SetValueProcPtr = function (instance: PNPP; variable: TNPPVariable; value: Pointer): TNPError; extdecl;
  TNPN_InvalidateRectProcPtr = procedure (instance: PNPP; invalidRect: PNPRect); extdecl;
  TNPN_InvalidateRegionProcPtr = procedure (instance: PNPP; invalidRegion: TNPRegion); extdecl;
  TNPN_ForceRedrawProcPtr = procedure (instance: PNPP); extdecl;
  TNPN_PushPopupsEnabledStateProcPtr = procedure (instance: PNPP; enabled: TNPBool); extdecl;
  TNPN_PopPopupsEnabledStateProcPtr = procedure (instance: PNPP); extdecl;
  TNPN_PluginThreadAsyncCallProcPtr = procedure (instance: PNPP; func: TNPN_PluginThreadAsyncCallCallback ; userData: Pointer); extdecl;
  TNPN_GetValueForURLProcPtr = function (instance: PNPP; variable: TNPNURLVariable; url: PChar; value: PPChar; len: PCUInt32): TNPError; extdecl;
  TNPN_SetValueForURLProcPtr = function (instance: PNPP; variable: TNPNURLVariable; url: PChar; value: PChar; len: CUInt32): TNPError; extdecl;
  TNPN_GetAuthenticationInfoProcPtr = function (instance: PNPP; protocol: PChar; host: PChar; port: CInt32; scheme: PChar; realm: PChar; username: PPChar; ulen: PCUInt32; password: PPChar; plen: PCUInt32): TNPError; extdecl;
  TNPN_ScheduleTimerProcPtr = function (instance: PNPP; interval: CUInt32; aRepeat: TNPBool; timerFunc: TNPN_ScheduleTimerCallback): CUInt32; extdecl;
  TNPN_UnscheduleTimerProcPtr = procedure (instance: PNPP; timerID: CUInt32); extdecl;
  TNPN_PopUpContextMenuProcPtr = function (instance: PNPP; menu: PNPMenu): TNPError; extdecl;
  TNPN_ConvertPointProcPtr = function (instance: PNPP; sourceX: double; sourceY: double; sourceSpace: TNPCoordinateSpace; destX: PDouble; destY: PDouble; destSpace: TNPCoordinateSpace): TNPBool; extdecl;
  TNPN_HandleEventProcPtr = function (instance: PNPP; event: Pointer; handled: TNPBool): TNPBool; extdecl;
  TNPN_UnfocusInstanceProcPtr = function (instance: PNPP; direction: TNPFocusDirection): TNPBool; extdecl;
  TNPN_URLRedirectResponseProcPtr = procedure (instance: PNPP; notifyData: Pointer; allow: TNPBool); extdecl;
  TNPN_DummyPtr = Pointer; //< used for obsolete function pointers

// typedef NPBool       (* NP_LOADDS NPP_GotFocusPtr)(NPP instance, NPFocusDirection direction);
// typedef void         (* NP_LOADDS NPP_LostFocusPtr)(NPP instance);
// typedef void         (* NP_LOADDS NPP_URLRedirectNotifyPtr)(NPP instance, const char* url, int32_t status, void* notifyData);
// typedef NPError      (* NP_LOADDS NPP_ClearSiteDataPtr)(const char* site, uint64_t flags, uint64_t maxAge);
// typedef char**       (* NP_LOADDS NPP_GetSitesWithDataPtr)(void);
// typedef void         (* NP_LOADDS NPP_DidCompositePtr)(NPP instance);

  { TODO }
  TNPP_GotFocusPtr = Pointer;
  TNPP_LostFocusPtr = Pointer; //< TODO
  TNPP_URLRedirectNotifyPtr = Pointer; //< TODO
  TNPP_ClearSiteDataPtr = Pointer; //< TODO
  TNPP_GetSitesWithDataPtr = Pointer; //< TODO
  TNPP_DidCompositePtr = Pointer; //< TODO

  TNPN_GetJavaEnvProcPtr = Pointer; //< TODO
  TNPN_GetJavaPeerProcPtr = Pointer; //< TODO
  TNPN_GetStringIdentifierProcPtr = Pointer; //< TODO
  TNPN_GetStringIdentifiersProcPtr = Pointer; //< TODO
  TNPN_GetIntIdentifierProcPtr = Pointer; //< TODO
  TNPN_IdentifierIsStringProcPtr = Pointer; //< TODO
  TNPN_UTF8FromIdentifierProcPtr = Pointer; //< TODO
  TNPN_IntFromIdentifierProcPtr = Pointer; //< TODO
  TNPN_CreateObjectProcPtr = Pointer; //< TODO
  TNPN_RetainObjectProcPtr = Pointer; //< TODO
  TNPN_ReleaseObjectProcPtr = Pointer; //< TODO
  TNPN_InvokeProcPtr = Pointer; //< TODO
  TNPN_InvokeDefaultProcPtr = Pointer; //< TODO
  TNPN_EvaluateProcPtr = Pointer; //< TODO
  TNPN_GetPropertyProcPtr = Pointer; //< TODO
  TNPN_SetPropertyProcPtr = Pointer; //< TODO
  TNPN_RemovePropertyProcPtr = Pointer; //< TODO
  TNPN_HasPropertyProcPtr = Pointer; //< TODO
  TNPN_HasMethodProcPtr = Pointer; //< TODO
  TNPN_ReleaseVariantValueProcPtr = Pointer; //< TODO
  TNPN_SetExceptionProcPtr = Pointer; //< TODO
  TNPN_EnumerateProcPtr = Pointer; //< TODO
  TNPN_ConstructProcPtr = Pointer; //< TODO
  TNPN_GetValueForURLPtr = Pointer; //< TODO
  TNPN_SetValueForURLPtr = Pointer; //< TODO
  TNPN_GetAuthenticationInfoPtr = Pointer; //< TODO
  TNPN_PopUpContextMenuPtr = Pointer; //< TODO
  TNPN_ConvertPointPtr = Pointer; //< TODO
  TNPN_HandleEventPtr = Pointer; //< TODO
  TNPN_UnfocusInstancePtr = Pointer; //< TODO
  TNPN_URLRedirectResponsePtr = Pointer; //< TODO

  TNPPluginFuncs = record
    size: CUInt16;
    version: CUInt16;
    newp: TNPP_NewProcPtr;
    destroy: TNPP_DestroyProcPtr;
    setwindow: TNPP_SetWindowProcPtr;
    newstream: TNPP_NewStreamProcPtr;
    destroystream: TNPP_DestroyStreamProcPtr;
    asfile: TNPP_StreamAsFileProcPtr;
    writeready: TNPP_WriteReadyProcPtr;
    aWrite: TNPP_WriteProcPtr;
    print: TNPP_PrintProcPtr;
    event: TNPP_HandleEventProcPtr;
    urlnotify: TNPP_URLNotifyProcPtr;
    javaClass: Pointer;
    getvalue: TNPP_GetValueProcPtr;
    setvalue: TNPP_SetValueProcPtr;
    // rest unused by CGE (when moving this, update check in NP_GetEntryPoints)
    UNUSED_gotfocus: TNPP_GotFocusPtr;
    UNUSED_lostfocus: TNPP_LostFocusPtr;
    UNUSED_urlredirectnotify: TNPP_URLRedirectNotifyPtr;
    UNUSED_clearsitedata: TNPP_ClearSiteDataPtr;
    UNUSED_getsiteswithdata: TNPP_GetSitesWithDataPtr;
    UNUSED_didComposite: TNPP_DidCompositePtr;
  end;
  PNPPluginFuncs = ^TNPPluginFuncs;

  TNPNetscapeFuncs = record
    size: CUInt16;
    version: CUInt16;
    geturl: TNPN_GetURLProcPtr;
    posturl: TNPN_PostURLProcPtr;
    requestread: TNPN_RequestReadProcPtr;
    newstream: TNPN_NewStreamProcPtr;
    aWrite: TNPN_WriteProcPtr;
    destroystream: TNPN_DestroyStreamProcPtr;
    status: TNPN_StatusProcPtr;
    uagent: TNPN_UserAgentProcPtr;
    memalloc: TNPN_MemAllocProcPtr;
    memfree: TNPN_MemFreeProcPtr;
    memflush: TNPN_MemFlushProcPtr;
    reloadplugins: TNPN_ReloadPluginsProcPtr;
    getJavaEnv: TNPN_GetJavaEnvProcPtr;
    getJavaPeer: TNPN_GetJavaPeerProcPtr;
    geturlnotify: TNPN_GetURLNotifyProcPtr;
    posturlnotify: TNPN_PostURLNotifyProcPtr;
    getvalue: TNPN_GetValueProcPtr;
    setvalue: TNPN_SetValueProcPtr;
    invalidaterect: TNPN_InvalidateRectProcPtr;
    invalidateregion: TNPN_InvalidateRegionProcPtr;
    forceredraw: TNPN_ForceRedrawProcPtr;
    getstringidentifier: TNPN_GetStringIdentifierProcPtr;
    getstringidentifiers: TNPN_GetStringIdentifiersProcPtr;
    getintidentifier: TNPN_GetIntIdentifierProcPtr;
    identifierisstring: TNPN_IdentifierIsStringProcPtr;
    utf8fromidentifier: TNPN_UTF8FromIdentifierProcPtr;
    intfromidentifier: TNPN_IntFromIdentifierProcPtr;
    createobject: TNPN_CreateObjectProcPtr;
    retainobject: TNPN_RetainObjectProcPtr;
    releaseobject: TNPN_ReleaseObjectProcPtr;
    invoke: TNPN_InvokeProcPtr;
    invokeDefault: TNPN_InvokeDefaultProcPtr;
    evaluate: TNPN_EvaluateProcPtr;
    getproperty: TNPN_GetPropertyProcPtr;
    setproperty: TNPN_SetPropertyProcPtr;
    removeproperty: TNPN_RemovePropertyProcPtr;
    hasproperty: TNPN_HasPropertyProcPtr;
    hasmethod: TNPN_HasMethodProcPtr;
    releasevariantvalue: TNPN_ReleaseVariantValueProcPtr;
    setexception: TNPN_SetExceptionProcPtr;
    pushpopupsenabledstate: TNPN_PushPopupsEnabledStateProcPtr;
    poppopupsenabledstate: TNPN_PopPopupsEnabledStateProcPtr;
    enumerate: TNPN_EnumerateProcPtr;
    pluginthreadasynccall: TNPN_PluginThreadAsyncCallProcPtr;
    construct: TNPN_ConstructProcPtr;
    getvalueforurl: TNPN_GetValueForURLPtr;
    setvalueforurl: TNPN_SetValueForURLPtr;
    getauthenticationinfo: TNPN_GetAuthenticationInfoPtr;
    scheduletimer: TNPN_ScheduleTimerProcPtr;
    unscheduletimer: TNPN_UnscheduleTimerProcPtr;
    // rest unused by CGE (when moving this, update check in NP_Initialize)
    UNUSED_popupcontextmenu: TNPN_PopUpContextMenuPtr;
    UNUSED_convertpoint: TNPN_ConvertPointPtr;
    UNUSED_handleevent: TNPN_HandleEventPtr;
    UNUSED_unfocusinstance: TNPN_UnfocusInstancePtr;
    UNUSED_urlredirectresponse: TNPN_URLRedirectResponsePtr;
    UNUSED_initasyncsurfaceOBSOLETE: TNPN_DummyPtr;
    UNUSED_finalizeasyncsurfaceOBSOLETE: TNPN_DummyPtr;
    UNUSED_setcurrentasyncsurfaceOBSOLETE: TNPN_DummyPtr;
  end;
  PNPNetscapeFuncs = ^TNPNetscapeFuncs;

{$ifndef UNIX}
(*
// declarations below not really needed,
// you can (should?) use pointers from TNPNetscapeFuncs on Windows too
function NPN_GetURL(instance: PNPP; url: PChar; target: PChar): TNPError; extdecl; external;
function NPN_PostURL(instance: PNPP; url: PChar; target: PChar; len: CUInt32; buf: PChar; aFile: TNPBool): TNPError; extdecl; external;
function NPN_RequestRead(stream: PNPStream; rangeList: PNPByteRange): TNPError; extdecl; external;
function NPN_NewStream(instance: PNPP; aType: TNPMIMEType; target: PChar; stream: PPNPStream): TNPError; extdecl; external;
function NPN_Write(instance: PNPP; stream: PNPStream; len: CInt32; buffer: Pointer): CInt32; extdecl; external;
function NPN_DestroyStream(instance: PNPP; stream: PNPStream; reason: TNPReason): TNPError; extdecl; external;
procedure NPN_Status(instance: PNPP; message: PChar); extdecl; external;
function NPN_UserAgent(instance: PNPP): PChar; extdecl; external;
function NPN_MemAlloc(size: CUInt32): Pointer; extdecl; external;
procedure NPN_MemFree(ptr: Pointer); extdecl; external;
function NPN_MemFlush(size: CUInt32): CUInt32; extdecl; external;
procedure NPN_ReloadPlugins(reloadPages: TNPBool); extdecl; external;
function NPN_GetValue(instance: PNPP; variable: TNPNVariable; value: Pointer): TNPError; extdecl; external;
function NPN_SetValue(instance: PNPP; variable: TNPPVariable; value: Pointer): TNPError; extdecl; external;
procedure NPN_InvalidateRect(instance: PNPP; invalidRect: PNPRect); extdecl; external;
procedure NPN_InvalidateRegion(instance: PNPP; invalidRegion: TNPRegion); extdecl; external;
procedure NPN_ForceRedraw(instance: PNPP); extdecl; external;
procedure NPN_PushPopupsEnabledState(instance: PNPP; enabled: TNPBool); extdecl; external;
procedure NPN_PopPopupsEnabledState(instance: PNPP); extdecl; external;
procedure NPN_PluginThreadAsyncCall(instance: PNPP; func: TNPN_PluginThreadAsyncCallCallback ; userData: Pointer); extdecl; external;
function NPN_GetValueForURL(instance: PNPP; variable: TNPNURLVariable; url: PChar; value: PPChar; len: PCUInt32): TNPError; extdecl; external;
function NPN_SetValueForURL(instance: PNPP; variable: TNPNURLVariable; url: PChar; value: PChar; len: CUInt32): TNPError; extdecl; external;
function NPN_GetAuthenticationInfo(instance: PNPP; protocol: PChar; host: PChar; port: CInt32; scheme: PChar; realm: PChar; username: PPChar; ulen: PCUInt32; password: PPChar; plen: PCUInt32): TNPError; extdecl; external;
function NPN_ScheduleTimer(instance: PNPP; interval: CUInt32; aRepeat: TNPBool; timerFunc: TNPN_ScheduleTimerCallback): CUInt32; extdecl; external;
procedure NPN_UnscheduleTimer(instance: PNPP; timerID: CUInt32); extdecl; external;
function NPN_PopUpContextMenu(instance: PNPP; menu: PNPMenu): TNPError; extdecl; external;
function NPN_ConvertPoint(instance: PNPP; sourceX: double; sourceY: double; sourceSpace: TNPCoordinateSpace; destX: PDouble; destY: PDouble; destSpace: TNPCoordinateSpace): TNPBool; extdecl; external;
function NPN_HandleEvent(instance: PNPP; event: Pointer; handled: TNPBool): TNPBool; extdecl; external;
function NPN_UnfocusInstance(instance: PNPP; direction: TNPFocusDirection): TNPBool; extdecl; external;
procedure NPN_URLRedirectResponse(instance: PNPP; notifyData: Pointer; allow: TNPBool); extdecl; external;
*)
{$endif}

implementation

end.
