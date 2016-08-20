{ FPC GLX unit with my patch to bring it to 1.4 level:
  http://bugs.freepascal.org/view.php?id=19953
  Also modified to work in objfpc mode, to ease compilation along
  with the rest of the engine. }

{

  Translation of the Mesa GLX headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther


  Mesa 3-D graphics library
  Version:  3.0
  Copyright (C) 1995-1998  Brian Paul

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

{ @exclude (This is only a C header translation --- no nice PasDoc docs.) }
unit CastleGlx;

{$I castleconf.inc}

interface

{$MACRO ON}

{$IFDEF Unix}
  uses
    X, XLib, XUtil;
  {$DEFINE HasGLX}  // Activate GLX stuff
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}

{$IFNDEF HasGLX}
  {$MESSAGE GLX not present on this platform.}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

// Note: Requires that the GL library has already been initialized
function InitGLX: Boolean;

var
  GLXDumpUnresolvedFunctions,
  GLXInitialized: Boolean;


// =======================================================
//   GLX consts, types and functions
// =======================================================

// Tokens for glXChooseVisual and glXGetConfig:
const
  GLX_USE_GL                            = 1;
  GLX_BUFFER_SIZE                       = 2;
  GLX_LEVEL                             = 3;
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_STEREO                            = 6;
  GLX_AUX_BUFFERS                       = 7;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_ACCUM_RED_SIZE                    = 14;
  GLX_ACCUM_GREEN_SIZE                  = 15;
  GLX_ACCUM_BLUE_SIZE                   = 16;
  GLX_ACCUM_ALPHA_SIZE                  = 17;

  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                        = 1;
  GLX_BAD_ATTRIBUTE                     = 2;
  GLX_NO_EXTENSION                      = 3;
  GLX_BAD_VISUAL                        = 4;
  GLX_BAD_CONTEXT                       = 5;
  GLX_BAD_VALUE                         = 6;
  GLX_BAD_ENUM                          = 7;

  // GLX 1.1 and later:
  GLX_VENDOR                            = 1;
  GLX_VERSION                           = 2;
  GLX_EXTENSIONS                        = 3;

  // GLX 1.3 and later:
  GLX_CONFIG_CAVEAT               = $20;
  GLX_DONT_CARE                   = $FFFFFFFF;
  GLX_X_VISUAL_TYPE               = $22;
  GLX_TRANSPARENT_TYPE            = $23;
  GLX_TRANSPARENT_INDEX_VALUE     = $24;
  GLX_TRANSPARENT_RED_VALUE       = $25;
  GLX_TRANSPARENT_GREEN_VALUE     = $26;
  GLX_TRANSPARENT_BLUE_VALUE      = $27;
  GLX_TRANSPARENT_ALPHA_VALUE     = $28;
  GLX_WINDOW_BIT                  = $00000001;
  GLX_PIXMAP_BIT                  = $00000002;
  GLX_PBUFFER_BIT                 = $00000004;
  GLX_AUX_BUFFERS_BIT             = $00000010;
  GLX_FRONT_LEFT_BUFFER_BIT       = $00000001;
  GLX_FRONT_RIGHT_BUFFER_BIT      = $00000002;
  GLX_BACK_LEFT_BUFFER_BIT        = $00000004;
  GLX_BACK_RIGHT_BUFFER_BIT       = $00000008;
  GLX_DEPTH_BUFFER_BIT            = $00000020;
  GLX_STENCIL_BUFFER_BIT          = $00000040;
  GLX_ACCUM_BUFFER_BIT            = $00000080;
  GLX_NONE                        = $8000;
  GLX_SLOW_CONFIG                 = $8001;
  GLX_TRUE_COLOR                  = $8002;
  GLX_DIRECT_COLOR                = $8003;
  GLX_PSEUDO_COLOR                = $8004;
  GLX_STATIC_COLOR                = $8005;
  GLX_GRAY_SCALE                  = $8006;
  GLX_STATIC_GRAY                 = $8007;
  GLX_TRANSPARENT_RGB             = $8008;
  GLX_TRANSPARENT_INDEX           = $8009;
  GLX_VISUAL_ID                   = $800B;
  GLX_SCREEN                      = $800C;
  GLX_NON_CONFORMANT_CONFIG       = $800D;
  GLX_DRAWABLE_TYPE               = $8010;
  GLX_RENDER_TYPE                 = $8011;
  GLX_X_RENDERABLE                = $8012;
  GLX_FBCONFIG_ID                 = $8013;
  GLX_RGBA_TYPE                   = $8014;
  GLX_COLOR_INDEX_TYPE            = $8015;
  GLX_MAX_PBUFFER_WIDTH           = $8016;
  GLX_MAX_PBUFFER_HEIGHT          = $8017;
  GLX_MAX_PBUFFER_PIXELS          = $8018;
  GLX_PRESERVED_CONTENTS          = $801B;
  GLX_LARGEST_PBUFFER             = $801C;
  GLX_WIDTH                       = $801D;
  GLX_HEIGHT                      = $801E;
  GLX_EVENT_MASK                  = $801F;
  GLX_DAMAGED                     = $8020;
  GLX_SAVED                       = $8021;
  GLX_WINDOW                      = $8022;
  GLX_PBUFFER                     = $8023;
  GLX_PBUFFER_HEIGHT              = $8040;
  GLX_PBUFFER_WIDTH               = $8041;
  GLX_RGBA_BIT                    = $00000001;
  GLX_COLOR_INDEX_BIT             = $00000002;
  GLX_PBUFFER_CLOBBER_MASK        = $08000000;

  // GLX 1.4 and later:
  GLX_SAMPLE_BUFFERS              = $186a0; // 100000
  GLX_SAMPLES                     = $186a1; // 100001

  // Extensions:

  // GLX_ARB_multisample
  GLX_SAMPLE_BUFFERS_ARB             = 100000;
  GLX_SAMPLES_ARB                    = 100001;

  // GLX_ARB_create_context (http://www.opengl.org/registry/specs/ARB/glx_create_context.txt)
  GLX_CONTEXT_DEBUG_BIT_ARB          = $00000001;
  GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $00000002;
  GLX_CONTEXT_MAJOR_VERSION_ARB      = $2091;
  GLX_CONTEXT_MINOR_VERSION_ARB      = $2092;
  GLX_CONTEXT_FLAGS_ARB              = $2094;

  // GLX_ARB_create_context_profile
  GLX_CONTEXT_CORE_PROFILE_BIT_ARB   = $00000001;
  GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  GLX_CONTEXT_PROFILE_MASK_ARB       = $9126;

  // GLX_ARB_create_context_robustness
  GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB  = $00000004;
  GLX_LOSE_CONTEXT_ON_RESET_ARB      = $8252;
  GLX_CONTEXT_RESET_NOTIFICATION_STRATEGY_ARB = $8256;
  GLX_NO_RESET_NOTIFICATION_ARB      = $8261;

  // GLX_SGIS_multisample
  GLX_SAMPLE_BUFFERS_SGIS            = 100000;
  GLX_SAMPLES_SGIS                   = 100001;

  // GLX_EXT_visual_info
  GLX_X_VISUAL_TYPE_EXT                 = $22;
  GLX_TRANSPARENT_TYPE_EXT              = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT       = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT         = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT       = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT        = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT       = $28;

  GLX_TRUE_COLOR_EXT                    = $8002;
  GLX_DIRECT_COLOR_EXT                  = $8003;
  GLX_PSEUDO_COLOR_EXT                  = $8004;
  GLX_STATIC_COLOR_EXT                  = $8005;
  GLX_GRAY_SCALE_EXT                    = $8006;
  GLX_STATIC_GRAY_EXT                   = $8007;
  GLX_NONE_EXT                          = $8000;
  GLX_TRANSPARENT_RGB_EXT               = $8008;
  GLX_TRANSPARENT_INDEX_EXT             = $8009;

type
  // From XLib:
  XPixmap = TXID;
  XFont = TXID;
  XColormap = TXID;

  GLXContext = Pointer;
  GLXPixmap = TXID;
  GLXDrawable = TXID;
  GLXContextID = TXID;

  TXPixmap = XPixmap;
  TXFont = XFont;
  TXColormap = XColormap;

  TGLXContext = GLXContext;
  TGLXPixmap = GLXPixmap;
  TGLXDrawable = GLXDrawable;
  // GLX 1.3 and later
  TGLXFBConfigRec = record { internal, use only a pointer to this } end;
  TGLXFBConfig = ^TGLXFBConfigRec;
  { PGLXFBConfig is a pointer to TGLXFBConfig (which is also a pointer).
    glX uses this to represent an array of FB configs. }
  PGLXFBConfig = ^TGLXFBConfig;
  TGLXFBConfigID = TXID;
  TGLXContextID = GLXContextID;
  TGLXWindow = TXID;
  TGLXPbuffer = TXID;

var
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; attribList: PInteger): PXVisualInfo; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl;
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl;
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  glXWaitGL: procedure; cdecl;
  glXWaitX: procedure; cdecl;
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl;

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl;

  // GLX 1.2 and later
  glXGetCurrentDisplay: function: PDisplay; cdecl;

  // GLX 1.3 and later
  glXChooseFBConfig: function(dpy: PDisplay; screen: Integer; attribList: PInteger; var nitems: Integer): PGLXFBConfig; cdecl;
  glXGetFBConfigAttrib: function(dpy: PDisplay; config: TGLXFBConfig; attribute: Integer; var value: Integer): Integer; cdecl;
  glXGetFBConfigs: function(dpy: PDisplay; screen: Integer; var nelements: Integer): PGLXFBConfig; cdecl;
  glXGetVisualFromFBConfig: function(dpy: PDisplay; config: TGLXFBConfig): PXVisualInfo; cdecl;
  glXCreateWindow: function(dpy: PDisplay; config: TGLXFBConfig; win: X.TWindow; attribList: PInteger): TGLXWindow; cdecl;
  glXDestroyWindow: procedure (dpy: PDisplay; window: TGLXWindow); cdecl;
  glXCreatePixmap: function(dpy: PDisplay; config: TGLXFBConfig; pixmap: TXPixmap; attribList: PInteger): TGLXPixmap; cdecl;
  glXDestroyPixmap: procedure (dpy: PDisplay; pixmap: TGLXPixmap); cdecl;
  glXCreatePbuffer: function(dpy: PDisplay; config: TGLXFBConfig; attribList: PInteger): TGLXPbuffer; cdecl;
  glXDestroyPbuffer: procedure (dpy: PDisplay; pbuf: TGLXPbuffer); cdecl;
  glXQueryDrawable: procedure (dpy: PDisplay; draw: TGLXDrawable; attribute: Integer; value: PLongWord); cdecl;
  glXCreateNewContext: function(dpy: PDisplay; config: TGLXFBConfig; renderType: Integer; shareList: TGLXContext; direct: boolean): TGLXContext; cdecl;
  glXMakeContextCurrent: function(dpy: PDisplay; draw: TGLXDrawable; read: GLXDrawable; ctx: TGLXContext): boolean; cdecl;
  glXGetCurrentReadDrawable: function: TGLXDrawable; cdecl;
  glXQueryContext: function(dpy: PDisplay; ctx: TGLXContext; attribute: Integer; var value: Integer): Integer; cdecl;
  glXSelectEvent: procedure (dpy: PDisplay; drawable: TGLXDrawable; mask: LongWord); cdecl;
  glXGetSelectedEvent: procedure (dpy: PDisplay; drawable: TGLXDrawable; mask: PLongWord); cdecl;

  // GLX 1.4 and later
  glXGetProcAddress: function(procname: PChar): Pointer; cdecl;

  // Extensions:

  // GLX_ARB_get_proc_address
  glXGetProcAddressARB: function(procname: PChar): Pointer; cdecl;

  // GLX_ARB_create_context
  glXCreateContextAttribsARB: function (dpy: PDisplay; config: TGLXFBConfig; share_context: TGLXContext; direct: boolean; attrib_list: PInteger): TGLXContext; cdecl;

  // GLX_MESA_pixmap_colormap
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;

  // Unknown Mesa GLX extension (undocumented in current GLX C headers?)
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl;

  // GLX_SGI_video_sync
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl;

// =======================================================
//
// =======================================================

{
  Safe checking of glX version and extension presence.

  For each glX version, these functions check that
  - glXQueryExtension indicates that glX extension is present at all on this display.
  - glXQueryVersion indicates that (at least) this version is supported.
  - all the entry points defined for this glX version were
    successfully loaded from the library.
    For now, all glX versions are fully backward-compatible,
    so e.g. if GLX_version_1_3 is true, you know that also GLX_version_1_2
    and GLX_version_1_1 etc. are true,
    and all entry points up to glX 1.3 are assigned.

  For each extension, these functions check that
  - it is declared within glXQueryExtensionsString (which in turn means
    that glXQueryExtensionsString must be available, which requires glX 1.1)
  - all it's entry points were successfully loaded from library

  As such, these functions are the safest way to check if given
  extension/glX version is available.

  Note that the availability of glX version and extension may depend
  on the X display and (in case of extension) even screen number.
}

function GLX_version_1_0(Display: PDisplay): boolean;
function GLX_version_1_1(Display: PDisplay): boolean;
function GLX_version_1_2(Display: PDisplay): boolean;
function GLX_version_1_3(Display: PDisplay): boolean;
function GLX_version_1_4(Display: PDisplay): boolean;

function GLX_ARB_get_proc_address(Display: PDisplay; Screen: Integer): boolean;
function GLX_ARB_create_context(Display: PDisplay; Screen: Integer): boolean;
function GLX_ARB_create_context_profile(Display: PDisplay; Screen: Integer): boolean;
function GLX_ARB_create_context_robustness(Display: PDisplay; Screen: Integer): boolean;
function GLX_ARB_multisample(Display: PDisplay; Screen: Integer): boolean;
function GLX_EXT_visual_info(Display: PDisplay; Screen: Integer): boolean;
function GLX_MESA_pixmap_colormap(Display: PDisplay; Screen: Integer): boolean;
function GLX_SGI_video_sync(Display: PDisplay; Screen: Integer): boolean;
function GLX_SGIS_multisample(Display: PDisplay; Screen: Integer): boolean;

implementation

uses GL, dynlibs, GLExt { for glext_ExtensionSupported utility }, CastleLog;

{$LINKLIB m}

function GLX_version_1_0(Display: PDisplay): boolean;
var
  IgnoredErrorb, IgnoredEvent, Major, Minor: Integer;
begin
  Result :=
    { check is glX present at all for this display }
    Assigned(glXQueryExtension) and
    glXQueryExtension(Display, IgnoredErrorb, IgnoredEvent) and
    { check glXQueryVersion, although there is no glX with version < 1 }
    Assigned(glXQueryVersion) and
    glXQueryVersion(Display, Major, Minor) and
    (Major >= 1) and
    { check entry points assigned }
    Assigned(glXChooseVisual) and
    Assigned(glXCreateContext) and
    Assigned(glXDestroyContext) and
    Assigned(glXMakeCurrent) and
    Assigned(glXCopyContext) and
    Assigned(glXSwapBuffers) and
    Assigned(glXCreateGLXPixmap) and
    Assigned(glXDestroyGLXPixmap) and
    { Assigned(glXQueryExtension) and } { (already checked) }
    { Assigned(glXQueryVersion) and } { (already checked) }
    Assigned(glXIsDirect) and
    Assigned(glXGetConfig) and
    Assigned(glXGetCurrentContext) and
    Assigned(glXGetCurrentDrawable) and
    Assigned(glXWaitGL) and
    Assigned(glXWaitX) and
    Assigned(glXUseXFont)
end;

function GLX_version_1_1(Display: PDisplay): boolean;
var
  Major, Minor: Integer;
begin
  Result :=
    { check previous version Ok }
    GLX_version_1_0(Display) and
    { check glXQueryVersion }
    glXQueryVersion(Display, Major, Minor) and
    ( (Major > 1) or ((Major = 1) and (Minor >= 1)) ) and
    { check entry points assigned }
    Assigned(glXQueryExtensionsString) and
    Assigned(glXQueryServerString) and
    Assigned(glXGetClientString);
end;

function GLX_version_1_2(Display: PDisplay): boolean;
var
  Major, Minor: Integer;
begin
  Result :=
    { check previous version Ok }
    GLX_version_1_1(Display) and
    { check glXQueryVersion }
    glXQueryVersion(Display, Major, Minor) and
    ( (Major > 1) or ((Major = 1) and (Minor >= 2)) ) and
    { check entry points assigned }
    Assigned(glXGetCurrentDisplay);
end;

function GLX_version_1_3(Display: PDisplay): boolean;
var
  Major, Minor: Integer;
begin
  Result :=
    { check previous version Ok }
    GLX_version_1_2(Display) and
    { check glXQueryVersion }
    glXQueryVersion(Display, Major, Minor) and
    ( (Major > 1) or ((Major = 1) and (Minor >= 3)) ) and
    { check entry points assigned }
    Assigned(glXChooseFBConfig) and
    Assigned(glXGetFBConfigAttrib) and
    Assigned(glXGetFBConfigs) and
    Assigned(glXGetVisualFromFBConfig) and
    Assigned(glXCreateWindow) and
    Assigned(glXDestroyWindow) and
    Assigned(glXCreatePixmap) and
    Assigned(glXDestroyPixmap) and
    Assigned(glXCreatePbuffer) and
    Assigned(glXDestroyPbuffer) and
    Assigned(glXQueryDrawable) and
    Assigned(glXCreateNewContext) and
    Assigned(glXMakeContextCurrent) and
    Assigned(glXGetCurrentReadDrawable) and
    Assigned(glXQueryContext) and
    Assigned(glXSelectEvent) and
    Assigned(glXGetSelectedEvent);
end;

function GLX_version_1_4(Display: PDisplay): boolean;
var
  Major, Minor: Integer;
begin
  Result :=
    { check previous version Ok }
    GLX_version_1_3(Display) and
    { check glXQueryVersion }
    glXQueryVersion(Display, Major, Minor) and
    ( (Major > 1) or ((Major = 1) and (Minor >= 4)) ) and
    { check entry points assigned }
    Assigned(glXGetProcAddress);
end;

function GLX_ARB_get_proc_address(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_ARB_get_proc_address', GlxExtensions) and
      Assigned(glXGetProcAddressARB);
  end;
end;

function GLX_ARB_create_context(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_ARB_create_context', GlxExtensions) and
      Assigned(glXCreateContextAttribsARB);
  end;
end;

function GLX_ARB_create_context_profile(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_ARB_create_context_profile', GlxExtensions);
  end;
end;

function GLX_ARB_create_context_robustness(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_ARB_create_context_robustness', GlxExtensions);
  end;
end;

function GLX_ARB_multisample(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_ARB_multisample', GlxExtensions);
  end;
end;

function GLX_EXT_visual_info(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_EXT_visual_info', GlxExtensions);
  end;
end;

function GLX_MESA_pixmap_colormap(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_MESA_pixmap_colormap', GlxExtensions) and
      Assigned(glXCreateGLXPixmapMESA);
  end;
end;

function GLX_SGI_video_sync(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_SGI_video_sync', GlxExtensions) and
      Assigned(glXGetVideoSyncSGI) and
      Assigned(glXWaitVideoSyncSGI);
  end;
end;

function GLX_SGIS_multisample(Display: PDisplay; Screen: Integer): boolean;
var
  GlxExtensions: PChar;
begin
  Result := GLX_version_1_1(Display);
  if Result then
  begin
    GlxExtensions := glXQueryExtensionsString(Display, Screen);
    Result := glext_ExtensionSupported('GLX_SGIS_multisample', GlxExtensions);
  end;
end;

function GetProc(handle: PtrInt; name: PChar): Pointer;
begin
  Result := GetProcAddress(handle, name);
  if (Result = nil) and GLXDumpUnresolvedFunctions then
    WritelnWarning('GLX', 'Unresolved: ' + name);
end;

function InitGLX: Boolean;
var
  OurLibGL: TLibHandle;
begin
  Result := False;

{$ifndef darwin}
  OurLibGL := libGl;
{$else darwin}
  // Do not load glX stuff from GL library on Mac OS X, it is not there.
  // Try standard Apple X11:
  OurLibGL := LoadLibrary('/usr/X11R6/lib/libGL.dylib');
  // Try X11 from MacPorts:
  if OurLibGL = 0 then
    OurLibGL := LoadLibrary('/opt/local/lib/libGL.dylib');
{$endif darwin}

  if OurLibGL = 0 then
    exit;

  Pointer(glXChooseVisual) := GetProc(OurLibGL, 'glXChooseVisual');
  Pointer(glXCreateContext) := GetProc(OurLibGL, 'glXCreateContext');
  Pointer(glXDestroyContext) := GetProc(OurLibGL, 'glXDestroyContext');
  Pointer(glXMakeCurrent) := GetProc(OurLibGL, 'glXMakeCurrent');
  Pointer(glXCopyContext) := GetProc(OurLibGL, 'glXCopyContext');
  Pointer(glXSwapBuffers) := GetProc(OurLibGL, 'glXSwapBuffers');
  Pointer(glXCreateGLXPixmap) := GetProc(OurLibGL, 'glXCreateGLXPixmap');
  Pointer(glXDestroyGLXPixmap) := GetProc(OurLibGL, 'glXDestroyGLXPixmap');
  Pointer(glXQueryExtension) := GetProc(OurLibGL, 'glXQueryExtension');
  Pointer(glXQueryVersion) := GetProc(OurLibGL, 'glXQueryVersion');
  Pointer(glXIsDirect) := GetProc(OurLibGL, 'glXIsDirect');
  Pointer(glXGetConfig) := GetProc(OurLibGL, 'glXGetConfig');
  Pointer(glXGetCurrentContext) := GetProc(OurLibGL, 'glXGetCurrentContext');
  Pointer(glXGetCurrentDrawable) := GetProc(OurLibGL, 'glXGetCurrentDrawable');
  Pointer(glXWaitGL) := GetProc(OurLibGL, 'glXWaitGL');
  Pointer(glXWaitX) := GetProc(OurLibGL, 'glXWaitX');
  Pointer(glXUseXFont) := GetProc(OurLibGL, 'glXUseXFont');
  // GLX 1.1 and later
  Pointer(glXQueryExtensionsString) := GetProc(OurLibGL, 'glXQueryExtensionsString');
  Pointer(glXQueryServerString) := GetProc(OurLibGL, 'glXQueryServerString');
  Pointer(glXGetClientString) := GetProc(OurLibGL, 'glXGetClientString');
  // GLX 1.2 and later
  Pointer(glXGetCurrentDisplay) := GetProc(OurLibGL, 'glXGetCurrentDisplay');
  // GLX 1.3 and later
  Pointer(glXChooseFBConfig) := GetProc(OurLibGL, 'glXChooseFBConfig');
  Pointer(glXGetFBConfigAttrib) := GetProc(OurLibGL, 'glXGetFBConfigAttrib');
  Pointer(glXGetFBConfigs) := GetProc(OurLibGL, 'glXGetFBConfigs');
  Pointer(glXGetVisualFromFBConfig) := GetProc(OurLibGL, 'glXGetVisualFromFBConfig');
  Pointer(glXCreateWindow) := GetProc(OurLibGL, 'glXCreateWindow');
  Pointer(glXDestroyWindow) := GetProc(OurLibGL, 'glXDestroyWindow');
  Pointer(glXCreatePixmap) := GetProc(OurLibGL, 'glXCreatePixmap');
  Pointer(glXDestroyPixmap) := GetProc(OurLibGL, 'glXDestroyPixmap');
  Pointer(glXCreatePbuffer) := GetProc(OurLibGL, 'glXCreatePbuffer');
  Pointer(glXDestroyPbuffer) := GetProc(OurLibGL, 'glXDestroyPbuffer');
  Pointer(glXQueryDrawable) := GetProc(OurLibGL, 'glXQueryDrawable');
  Pointer(glXCreateNewContext) := GetProc(OurLibGL, 'glXCreateNewContext');
  Pointer(glXMakeContextCurrent) := GetProc(OurLibGL, 'glXMakeContextCurrent');
  Pointer(glXGetCurrentReadDrawable) := GetProc(OurLibGL, 'glXGetCurrentReadDrawable');
  Pointer(glXQueryContext) := GetProc(OurLibGL, 'glXQueryContext');
  Pointer(glXSelectEvent) := GetProc(OurLibGL, 'glXSelectEvent');
  Pointer(glXGetSelectedEvent) := GetProc(OurLibGL, 'glXGetSelectedEvent');
  // GLX 1.4 and later
  Pointer(glXGetProcAddress) := GetProc(OurLibGL, 'glXGetProcAddress');
  // Extensions
  // GLX_ARB_get_proc_address
  Pointer(glXGetProcAddressARB) := GetProc(OurLibGL, 'glXGetProcAddressARB');
  // GLX_ARB_create_context
  Pointer(glXCreateContextAttribsARB) := GetProc(OurLibGL, 'glXCreateContextAttribsARB');
  // GLX_MESA_pixmap_colormap
  Pointer(glXCreateGLXPixmapMESA) := GetProc(OurLibGL, 'glXCreateGLXPixmapMESA');
  // Unknown Mesa GLX extension
  Pointer(glXReleaseBufferMESA) := GetProc(OurLibGL, 'glXReleaseBufferMESA');
  Pointer(glXCopySubBufferMESA) := GetProc(OurLibGL, 'glXCopySubBufferMESA');
  // GLX_SGI_video_sync
  Pointer(glXGetVideoSyncSGI) := GetProc(OurLibGL, 'glXGetVideoSyncSGI');
  Pointer(glXWaitVideoSyncSGI) := GetProc(OurLibGL, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

initialization
  InitGLX;
end.
