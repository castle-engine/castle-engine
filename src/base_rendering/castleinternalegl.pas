{
  Copyright 2023-2023 Michalis Kamburelis, Benjamin 'BeRo' Rosseaux, The Khronos Group.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
  The original code from BeRo has GLES 2.0 + EGL combined,
  we split EGL into a separate unit in Castle Game Engine, as
  - EGL makes sense also for OpenGL (not ES).
  - it is conceptually a different library - EGL initializes context,
    like glX or wgl. It is separate from OpenGLES concepts.
  - Along with it, we made a few improvements in CGE:
    compatibility with FreeBSD,
    compatibility with Delphi,
    usage of our cross-platform and cross-compiler TDynLib.

  ----------------------------------------------------------------------------
  Original headers:

  Copyright (c) 2007-2009 The Khronos Group Inc.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and/or associated documentation files (the
  "Materials"), to deal in the Materials without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Materials, and to
  permit persons to whom the Materials are furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Materials.

  THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

  ----------------------------------------------------------------------------
}

{ EGL, a cross-platform library to initialize OpenGL and OpenGLES contexts. }
unit CastleInternalEgl;

{$i castleconf.inc}

{$ifdef FPC}
  {$packrecords C}
{$endif}

interface

uses CTypes, SysUtils
  {$ifdef UNIX_WITH_X}, X, XLib {$endif}
  {$ifdef MSWINDOWS}, Windows {$endif};

type
  PEGLConfig  = ^EGLConfig;
  PEGLint  = ^EGLint;
  EGLint = CInt32;
  EGLConfig = pointer;

{$if defined(UNIX_WITH_X)}
  { Xlib types that are needed by EGL.
    Match EGL/eglplatform.h from Khronos:

      typedef Display *EGLNativeDisplayType;
      typedef Pixmap   EGLNativePixmapType;
      typedef Window   EGLNativeWindowType;

    Note that TWindow and TPixmap sizes match also Pointer type,
    i.e. X headers declare them as 32-bit on 32-bit CPUs,
    and 64-bit on 64-bit CPUs.
    See tests/egl_sizes_test .
    See TTestCastleInternalEgl.TestEglTypeSizes .

    This is nice, it makes things safe -- at means that at binary
    (ABI) level, EGL calls accept the same sizes for these arguments,
    whether they are compiled to be aliases to X types or to be aliases to
    just pointers.
  }
  EGLNativeDisplayType = PDisplay;
  EGLNativeWindowType = TWindow;
  EGLNativePixmapType = TPixmap;
{$elseif defined(MSWINDOWS)}
  { WinAPI types that are needed by EGL.
    Match EGL/eglplatform.h from Khronos:

      typedef HDC     EGLNativeDisplayType;
      typedef HBITMAP EGLNativePixmapType;
      typedef HWND    EGLNativeWindowType;

    Note that all these native Windows handles are aliases to System.THandle,
    which is 32-bit on 32-bit CPUs, and 64-bit on 64-bit CPUs.
    This is nice, it makes things safe -- at means that at binary
    (ABI) level, EGL calls accept the same sizes for these arguments,
    whether they are compiled to be aliases to WinAPI types or to be aliases to
    just pointers.
  }
  EGLNativeDisplayType = HDC;
  EGLNativeWindowType = HWND;
  EGLNativePixmapType = HBITMAP;
{$else}
  { Cross-platform generic definition of EGL types.
    Match EGL/eglplatform.h from Khronos:

      #if defined(EGL_NO_PLATFORM_SPECIFIC_TYPES)
      typedef void *EGLNativeDisplayType;
      typedef void *EGLNativePixmapType;
      typedef void *EGLNativeWindowType;
  }
  EGLNativeDisplayType = Pointer;
  EGLNativeWindowType = Pointer;
  EGLNativePixmapType = Pointer;
{$endif}

  EGLBoolean = CUint32;
  EGLenum = CUint32;
  EGLContext = pointer;
  EGLDisplay = pointer;
  EGLSurface = pointer;
  EGLClientBuffer = pointer;

{ EGL Versioning  }
const
    EGL_VERSION_1_0 = 1;
    EGL_VERSION_1_1 = 1;
    EGL_VERSION_1_2 = 1;
    EGL_VERSION_1_3 = 1;
    EGL_VERSION_1_4 = 1;
    { EGL Enumerants. Bitmasks and other exceptional cases aside, most
      * enums are assigned unique values starting at 0x3000.
      }
    { EGL aliases  }
    EGL_FALSE = 0;
    EGL_TRUE = 1;

{ Out-of-band handle values  }
{ was #define dname def_expr }
function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;

{ was #define dname def_expr }
function EGL_NO_CONTEXT : EGLContext;

{ was #define dname def_expr }
function EGL_NO_DISPLAY : EGLDisplay;

{ was #define dname def_expr }
function EGL_NO_SURFACE : EGLSurface;

{ Out-of-band attribute value  }
{ was #define dname def_expr }
function EGL_DONT_CARE : EGLint;

{ Errors / GetError return values  }
const
  EGL_SUCCESS = $3000;
  EGL_NOT_INITIALIZED = $3001;
  EGL_BAD_ACCESS = $3002;
  EGL_BAD_ALLOC = $3003;
  EGL_BAD_ATTRIBUTE = $3004;
  EGL_BAD_CONFIG = $3005;
  EGL_BAD_CONTEXT = $3006;
  EGL_BAD_CURRENT_SURFACE = $3007;
  EGL_BAD_DISPLAY = $3008;
  EGL_BAD_MATCH = $3009;
  EGL_BAD_NATIVE_PIXMAP = $300A;
  EGL_BAD_NATIVE_WINDOW = $300B;
  EGL_BAD_PARAMETER = $300C;
  EGL_BAD_SURFACE = $300D;
  { EGL 1.1 - IMG_power_management  }
  EGL_CONTEXT_LOST = $300E;
  { Reserved 0x300F-0x301F for additional errors  }
  { Config attributes  }
  EGL_BUFFER_SIZE = $3020;
  EGL_ALPHA_SIZE = $3021;
  EGL_BLUE_SIZE = $3022;
  EGL_GREEN_SIZE = $3023;
  EGL_RED_SIZE = $3024;
  EGL_DEPTH_SIZE = $3025;
  EGL_STENCIL_SIZE = $3026;
  EGL_CONFIG_CAVEAT = $3027;
  EGL_CONFIG_ID = $3028;
  EGL_LEVEL = $3029;
  EGL_MAX_PBUFFER_HEIGHT = $302A;
  EGL_MAX_PBUFFER_PIXELS = $302B;
  EGL_MAX_PBUFFER_WIDTH = $302C;
  EGL_NATIVE_RENDERABLE = $302D;
  EGL_NATIVE_VISUAL_ID = $302E;
  EGL_NATIVE_VISUAL_TYPE = $302F;
  EGL_PRESERVED_RESOURCES = $3030;
  EGL_SAMPLES = $3031;
  EGL_SAMPLE_BUFFERS = $3032;
  EGL_SURFACE_TYPE = $3033;
  EGL_TRANSPARENT_TYPE = $3034;
  EGL_TRANSPARENT_BLUE_VALUE = $3035;
  EGL_TRANSPARENT_GREEN_VALUE = $3036;
  EGL_TRANSPARENT_RED_VALUE = $3037;
  { Attrib list terminator  }
  EGL_NONE = $3038;
  EGL_BIND_TO_TEXTURE_RGB = $3039;
  EGL_BIND_TO_TEXTURE_RGBA = $303A;
  EGL_MIN_SWAP_INTERVAL = $303B;
  EGL_MAX_SWAP_INTERVAL = $303C;
  EGL_LUMINANCE_SIZE = $303D;
  EGL_ALPHA_MASK_SIZE = $303E;
  EGL_COLOR_BUFFER_TYPE = $303F;
  EGL_RENDERABLE_TYPE = $3040;
  { Pseudo-attribute (not queryable)  }
  EGL_MATCH_NATIVE_PIXMAP = $3041;
  EGL_CONFORMANT = $3042;
  { Reserved 0x3041-0x304F for additional config attributes  }
  { Config attribute values  }
  { EGL_CONFIG_CAVEAT value  }
  EGL_SLOW_CONFIG = $3050;
  { EGL_CONFIG_CAVEAT value  }
  EGL_NON_CONFORMANT_CONFIG = $3051;
  { EGL_TRANSPARENT_TYPE value  }
  EGL_TRANSPARENT_RGB = $3052;
  { EGL_COLOR_BUFFER_TYPE value  }
  EGL_RGB_BUFFER = $308E;
  { EGL_COLOR_BUFFER_TYPE value  }
  EGL_LUMINANCE_BUFFER = $308F;
  { More config attribute values, for EGL_TEXTURE_FORMAT  }
  EGL_NO_TEXTURE = $305C;
  EGL_TEXTURE_RGB = $305D;
  EGL_TEXTURE_RGBA = $305E;
  EGL_TEXTURE_2D = $305F;
  { Config attribute mask bits  }
  { EGL_SURFACE_TYPE mask bits  }
  EGL_PBUFFER_BIT = $0001;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_PIXMAP_BIT = $0002;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_WINDOW_BIT = $0004;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_VG_COLORSPACE_LINEAR_BIT = $0020;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_ES_BIT = $0001;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENVG_BIT = $0002;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_ES2_BIT = $0004;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_BIT = $0008;
  { QueryString targets  }
  EGL_VENDOR = $3053;
  EGL_VERSION = $3054;
  EGL_EXTENSIONS = $3055;
  EGL_CLIENT_APIS = $308D;
  { QuerySurface / SurfaceAttrib / CreatePbufferSurface targets  }
  EGL_HEIGHT = $3056;
  EGL_WIDTH = $3057;
  EGL_LARGEST_PBUFFER = $3058;
  EGL_TEXTURE_FORMAT = $3080;
  EGL_TEXTURE_TARGET = $3081;
  EGL_MIPMAP_TEXTURE = $3082;
  EGL_MIPMAP_LEVEL = $3083;
  EGL_RENDER_BUFFER = $3086;
  EGL_VG_COLORSPACE = $3087;
  EGL_VG_ALPHA_FORMAT = $3088;
  EGL_HORIZONTAL_RESOLUTION = $3090;
  EGL_VERTICAL_RESOLUTION = $3091;
  EGL_PIXEL_ASPECT_RATIO = $3092;
  EGL_SWAP_BEHAVIOR = $3093;
  EGL_MULTISAMPLE_RESOLVE = $3099;
  { EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets  }
  EGL_BACK_BUFFER = $3084;
  EGL_SINGLE_BUFFER = $3085;
  { OpenVG color spaces  }
  { EGL_VG_COLORSPACE value  }
  EGL_VG_COLORSPACE_sRGB = $3089;
  { EGL_VG_COLORSPACE value  }
  EGL_VG_COLORSPACE_LINEAR = $308A;
  { OpenVG alpha formats  }
  { EGL_ALPHA_FORMAT value  }
  EGL_VG_ALPHA_FORMAT_NONPRE = $308B;
  { EGL_ALPHA_FORMAT value  }
  EGL_VG_ALPHA_FORMAT_PRE = $308C;
  { Constant scale factor by which fractional display resolutions &
    aspect ratio are scaled when queried as integer values.
  }
  EGL_DISPLAY_SCALING = 10000;

  { Constants added in EGL 1.5 (check at runtime EGL version to make sure
    these are safe to use!) }
  EGL_CONTEXT_MAJOR_VERSION         = $3098;
  EGL_CONTEXT_MINOR_VERSION         = $30FB;
  EGL_CONTEXT_OPENGL_PROFILE_MASK   = $30FD;
  EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY = $31BD;
  EGL_NO_RESET_NOTIFICATION         = $31BE;
  EGL_LOSE_CONTEXT_ON_RESET         = $31BF;
  EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT = $00000001;
  EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT = $00000002;
  EGL_CONTEXT_OPENGL_DEBUG          = $31B0;
  EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE = $31B1;
  EGL_CONTEXT_OPENGL_ROBUST_ACCESS  = $31B2;
  EGL_OPENGL_ES3_BIT                = $00000040;
  EGL_CL_EVENT_HANDLE               = $309C;
  EGL_SYNC_CL_EVENT                 = $30FE;
  EGL_SYNC_CL_EVENT_COMPLETE        = $30FF;
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE  = $30F0;
  EGL_SYNC_TYPE                     = $30F7;
  EGL_SYNC_STATUS                   = $30F1;
  EGL_SYNC_CONDITION                = $30F8;
  EGL_SIGNALED                      = $30F2;
  EGL_UNSIGNALED                    = $30F3;
  EGL_SYNC_FLUSH_COMMANDS_BIT       = $0001;
  //EGL_FOREVER                       = $FFFFFFFFFFFFFFFFull;
  EGL_TIMEOUT_EXPIRED               = $30F5;
  EGL_CONDITION_SATISFIED           = $30F6;
  //EGL_NO_SYNC                       = EGL_CAST(EGLSync,0= );
  EGL_SYNC_FENCE                    = $30F9;
  EGL_GL_COLORSPACE                 = $309D;
  EGL_GL_COLORSPACE_SRGB            = $3089;
  EGL_GL_COLORSPACE_LINEAR          = $308A;
  EGL_GL_RENDERBUFFER               = $30B9;
  EGL_GL_TEXTURE_2D                 = $30B1;
  EGL_GL_TEXTURE_LEVEL              = $30BC;
  EGL_GL_TEXTURE_3D                 = $30B2;
  EGL_GL_TEXTURE_ZOFFSET            = $30BD;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X = $30B3;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $30B4;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $30B5;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $30B6;
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $30B7;
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $30B8;
  EGL_IMAGE_PRESERVED               = $30D2;
  //EGL_NO_IMAGE                      = EGL_CAST(EGLImage,0);

{ Unknown display resolution/aspect ratio  }
{ was #define dname def_expr }
function EGL_UNKNOWN : EGLint;

{ Back buffer swap behaviors  }
{ EGL_SWAP_BEHAVIOR value  }

const
  EGL_BUFFER_PRESERVED = $3094;
  { EGL_SWAP_BEHAVIOR value  }
  EGL_BUFFER_DESTROYED = $3095;
  { CreatePbufferFromClientBuffer buffer types  }
  EGL_OPENVG_IMAGE = $3096;
  { QueryContext targets  }
  EGL_CONTEXT_CLIENT_TYPE = $3097;
  { CreateContext attributes  }
  EGL_CONTEXT_CLIENT_VERSION = $3098;
  { Multisample resolution behaviors  }
  { EGL_MULTISAMPLE_RESOLVE value  }
  EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;
  { EGL_MULTISAMPLE_RESOLVE value  }
  EGL_MULTISAMPLE_RESOLVE_BOX = $309B;
  { BindAPI/QueryAPI targets  }
  EGL_OPENGL_ES_API = $30A0;
  EGL_OPENVG_API = $30A1;
  EGL_OPENGL_API = $30A2;
  { GetCurrentSurface targets  }
  EGL_DRAW = $3059;
  EGL_READ = $305A;
  { WaitNative engines  }
  EGL_CORE_NATIVE_ENGINE = $305B;
  { EGL 1.2 tokens renamed for consistency in EGL 1.3  }
  EGL_COLORSPACE = EGL_VG_COLORSPACE;
  EGL_ALPHA_FORMAT = EGL_VG_ALPHA_FORMAT;
  EGL_COLORSPACE_sRGB = EGL_VG_COLORSPACE_sRGB;
  EGL_COLORSPACE_LINEAR = EGL_VG_COLORSPACE_LINEAR;
  EGL_ALPHA_FORMAT_NONPRE = EGL_VG_ALPHA_FORMAT_NONPRE;
  EGL_ALPHA_FORMAT_PRE = EGL_VG_ALPHA_FORMAT_PRE;
  { EGL extensions must request enum blocks from the Khronos
    API Registrar, who maintains the enumerant registry. Submit
    a bug in Khronos Bugzilla against task "Registry".
  }

{ EGL Functions  }
var
  eglGetError : function:EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglTerminate : function(dpy:EGLDisplay):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglQueryString : function(dpy:EGLDisplay; name:EGLint):PAnsiChar;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglCreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglCreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglCreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglBindAPI : function(api:EGLenum):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglQueryAPI : function:EGLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglWaitClient : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglReleaseThread : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglCreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglSurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglSwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  (* Const before type ignored *)
  eglCreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetCurrentContext : function:EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetCurrentSurface : function(readdraw:EGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetCurrentDisplay : function:EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglWaitGL : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglWaitNative : function(engine:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglSwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglCopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}

(* Const before type ignored *)
var
  eglGetProcAddress : function(procname:PAnsiChar):Pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}

const
  { Header file version number  }
  { Current version at http://www.khronos.org/registry/egl/  }
  EGL_EGLEXT_VERSION = 3;
  EGL_KHR_config_attribs = 1;
  { EGLConfig attribute  }
  EGL_CONFORMANT_KHR = $3042;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_VG_COLORSPACE_LINEAR_BIT_KHR = $0020;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_VG_ALPHA_FORMAT_PRE_BIT_KHR = $0040;
  EGL_KHR_lock_surface = 1;
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
  EGL_READ_SURFACE_BIT_KHR = $0001;
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
  EGL_WRITE_SURFACE_BIT_KHR = $0002;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_LOCK_SURFACE_BIT_KHR = $0080;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_OPTIMAL_FORMAT_BIT_KHR = $0100;
  { EGLConfig attribute  }
  EGL_MATCH_FORMAT_KHR = $3043;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGB_565_EXACT_KHR = $30C0;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGB_565_KHR = $30C1;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGBA_8888_EXACT_KHR = $30C2;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGBA_8888_KHR = $30C3;
  { eglLockSurfaceKHR attribute  }
  EGL_MAP_PRESERVE_PIXELS_KHR = $30C4;
  { eglLockSurfaceKHR attribute  }
  EGL_LOCK_USAGE_HINT_KHR = $30C5;
  { eglQuerySurface attribute  }
  EGL_BITMAP_POINTER_KHR = $30C6;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PITCH_KHR = $30C7;
  { eglQuerySurface attribute  }
  EGL_BITMAP_ORIGIN_KHR = $30C8;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_RED_OFFSET_KHR = $30C9;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_GREEN_OFFSET_KHR = $30CA;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_BLUE_OFFSET_KHR = $30CB;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_ALPHA_OFFSET_KHR = $30CC;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_LUMINANCE_OFFSET_KHR = $30CD;
  { EGL_BITMAP_ORIGIN_KHR value  }
  EGL_LOWER_LEFT_KHR = $30CE;
  { EGL_BITMAP_ORIGIN_KHR value  }
  EGL_UPPER_LEFT_KHR = $30CF;

  (* Const before type ignored *)
  EGL_KHR_image = 1;

  { eglCreateImageKHR target  }
  EGL_NATIVE_PIXMAP_KHR = $30B0;

type
  EGLImageKHR = pointer;

{ was #define dname def_expr }
function EGL_NO_IMAGE_KHR : EGLImageKHR;

const
  (* Const before type ignored *)
  EGL_KHR_vg_parent_image = 1;
  { eglCreateImageKHR target  }
  EGL_VG_PARENT_IMAGE_KHR = $30BA;
  EGL_KHR_gl_texture_2D_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_2D_KHR = $30B1;
  { eglCreateImageKHR attribute  }
  EGL_GL_TEXTURE_LEVEL_KHR = $30BC;
  EGL_KHR_gl_texture_cubemap_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X_KHR = $30B3;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X_KHR = $30B4;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y_KHR = $30B5;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_KHR = $30B6;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z_KHR = $30B7;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_KHR = $30B8;
  EGL_KHR_gl_texture_3D_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_3D_KHR = $30B2;
  { eglCreateImageKHR attribute  }
  EGL_GL_TEXTURE_ZOFFSET_KHR = $30BD;
  EGL_KHR_gl_renderbuffer_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_RENDERBUFFER_KHR = $30B9;
  EGL_KHR_image_base = 1;
  { Most interfaces defined by EGL_KHR_image_pixmap above  }
  { eglCreateImageKHR attribute  }
  EGL_IMAGE_PRESERVED_KHR = $30D2;
  EGL_KHR_image_pixmap = 1;
  { Interfaces defined by EGL_KHR_image above  }

{ Try to load dynamic library with EGL functions. }
procedure LoadEgl;

{ Returns @true if LoadEgl was run and the EGL library was successfully loaded.
  This implies that all function pointers here are non-nil. }
function EglAvailable: Boolean;

implementation

uses CastleDynLib;

{ Load from given library or using eglGetProcAddress. }
function glGetProcAddress(ahlib: TDynLib;ProcName: String): pointer;
var
  ProcNameAnsi: AnsiString;
begin
  result := ahlib.Symbol(PChar(ProcName));
  if Assigned(eglGetProcAddress) and not Assigned(result) then
  begin
    ProcNameAnsi := ProcName;
    result := eglGetProcAddress(PAnsiChar(ProcNameAnsi));
  end;
end;

{ was #define dname def_expr }
function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;
begin
  EGL_DEFAULT_DISPLAY := EGLNativeDisplayType(0);
end;

{ was #define dname def_expr }
function EGL_NO_CONTEXT : EGLContext;
begin
  EGL_NO_CONTEXT := EGLContext(0);
end;

{ was #define dname def_expr }
function EGL_NO_DISPLAY : EGLDisplay;
begin
  EGL_NO_DISPLAY := EGLDisplay(0);
end;

{ was #define dname def_expr }
function EGL_NO_SURFACE : EGLSurface;
begin
  EGL_NO_SURFACE := EGLSurface(0);
end;

{ was #define dname def_expr }
function EGL_DONT_CARE : EGLint;
begin
  EGL_DONT_CARE := EGLint(-(1));
end;

{ was #define dname def_expr }
function EGL_UNKNOWN : EGLint;
begin
  EGL_UNKNOWN := EGLint(-(1));
end;

{ was #define dname def_expr }
function EGL_NO_IMAGE_KHR : EGLImageKHR;
begin
  EGL_NO_IMAGE_KHR := EGLImageKHR(0);
end;

var
  EGLLib : TDynLib;

procedure FreeEGL;
begin
  FreeAndNil(EGLLib);

  eglGetError := nil;
  eglGetDisplay := nil;
  eglInitialize := nil;
  eglTerminate := nil;
  eglQueryString := nil;
  eglGetConfigs := nil;
  eglChooseConfig := nil;
  eglGetConfigAttrib := nil;
  eglCreateWindowSurface := nil;
  eglCreatePbufferSurface := nil;
  eglCreatePixmapSurface := nil;
  eglDestroySurface := nil;
  eglQuerySurface := nil;
  eglBindAPI := nil;
  eglQueryAPI := nil;
  eglWaitClient := nil;
  eglReleaseThread := nil;
  eglCreatePbufferFromClientBuffer := nil;
  eglSurfaceAttrib := nil;
  eglBindTexImage := nil;
  eglReleaseTexImage := nil;
  eglSwapInterval := nil;
  eglCreateContext := nil;
  eglDestroyContext := nil;
  eglMakeCurrent := nil;
  eglGetCurrentContext := nil;
  eglGetCurrentSurface := nil;
  eglGetCurrentDisplay := nil;
  eglQueryContext := nil;
  eglWaitGL := nil;
  eglWaitNative := nil;
  eglSwapBuffers := nil;
  eglCopyBuffers := nil;
  eglGetProcAddress := nil;
end;

function EglAvailable: Boolean;
begin
  Result := EGLLib <> nil;
end;

procedure LoadEgl;
const
  {$ifdef MSWINDOWS}
  LibName = 'libEGL.dll';
  LibName2 = '';
  {$else}
  { First try to access libEGL.so.1 (from libegl1-mesa package on Debian).
    The name libEGL.so is only available in -dev package. }
  LibName = 'libEGL.so.1';
  LibName2 = 'libEGL.so';
  {$endif}
begin
  FreeEGL;

  EGLLib := TDynLib.Load(LibName, false);
  {$warnings off} // ignore warning "unreachable code" on Windows, where LibName2 = ''
  if (EGLLib = nil) and (LibName2 <> '') then
    EGLLib := TDynLib.Load(LibName2, false);
  {$warnings on}

  if EGLLib <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} eglGetProcAddress) := EGLLib.Symbol('eglGetProcAddress');

    Pointer({$ifndef FPC}@{$endif} eglGetError) := glGetProcAddress(EGLLib,'eglGetError');
    Pointer({$ifndef FPC}@{$endif} eglGetDisplay) := glGetProcAddress(EGLLib,'eglGetDisplay');
    Pointer({$ifndef FPC}@{$endif} eglInitialize) := glGetProcAddress(EGLLib,'eglInitialize');
    Pointer({$ifndef FPC}@{$endif} eglTerminate) := glGetProcAddress(EGLLib,'eglTerminate');
    Pointer({$ifndef FPC}@{$endif} eglQueryString) := glGetProcAddress(EGLLib,'eglQueryString');
    Pointer({$ifndef FPC}@{$endif} eglGetConfigs) := glGetProcAddress(EGLLib,'eglGetConfigs');
    Pointer({$ifndef FPC}@{$endif} eglChooseConfig) := glGetProcAddress(EGLLib,'eglChooseConfig');
    Pointer({$ifndef FPC}@{$endif} eglGetConfigAttrib) := glGetProcAddress(EGLLib,'eglGetConfigAttrib');
    Pointer({$ifndef FPC}@{$endif} eglCreateWindowSurface) := glGetProcAddress(EGLLib,'eglCreateWindowSurface');
    Pointer({$ifndef FPC}@{$endif} eglCreatePbufferSurface) := glGetProcAddress(EGLLib,'eglCreatePbufferSurface');
    Pointer({$ifndef FPC}@{$endif} eglCreatePixmapSurface) := glGetProcAddress(EGLLib,'eglCreatePixmapSurface');
    Pointer({$ifndef FPC}@{$endif} eglDestroySurface) := glGetProcAddress(EGLLib,'eglDestroySurface');
    Pointer({$ifndef FPC}@{$endif} eglQuerySurface) := glGetProcAddress(EGLLib,'eglQuerySurface');
    Pointer({$ifndef FPC}@{$endif} eglBindAPI) := glGetProcAddress(EGLLib,'eglBindAPI');
    Pointer({$ifndef FPC}@{$endif} eglQueryAPI) := glGetProcAddress(EGLLib,'eglQueryAPI');
    Pointer({$ifndef FPC}@{$endif} eglWaitClient) := glGetProcAddress(EGLLib,'eglWaitClient');
    Pointer({$ifndef FPC}@{$endif} eglReleaseThread) := glGetProcAddress(EGLLib,'eglReleaseThread');
    Pointer({$ifndef FPC}@{$endif} eglCreatePbufferFromClientBuffer) := glGetProcAddress(EGLLib,'eglCreatePbufferFromClientBuffer');
    Pointer({$ifndef FPC}@{$endif} eglSurfaceAttrib) := glGetProcAddress(EGLLib,'eglSurfaceAttrib');
    Pointer({$ifndef FPC}@{$endif} eglBindTexImage) := glGetProcAddress(EGLLib,'eglBindTexImage');
    Pointer({$ifndef FPC}@{$endif} eglReleaseTexImage) := glGetProcAddress(EGLLib,'eglReleaseTexImage');
    Pointer({$ifndef FPC}@{$endif} eglSwapInterval) := glGetProcAddress(EGLLib,'eglSwapInterval');
    Pointer({$ifndef FPC}@{$endif} eglCreateContext) := glGetProcAddress(EGLLib,'eglCreateContext');
    Pointer({$ifndef FPC}@{$endif} eglDestroyContext) := glGetProcAddress(EGLLib,'eglDestroyContext');
    Pointer({$ifndef FPC}@{$endif} eglMakeCurrent) := glGetProcAddress(EGLLib,'eglMakeCurrent');
    Pointer({$ifndef FPC}@{$endif} eglGetCurrentContext) := glGetProcAddress(EGLLib,'eglGetCurrentContext');
    Pointer({$ifndef FPC}@{$endif} eglGetCurrentSurface) := glGetProcAddress(EGLLib,'eglGetCurrentSurface');
    Pointer({$ifndef FPC}@{$endif} eglGetCurrentDisplay) := glGetProcAddress(EGLLib,'eglGetCurrentDisplay');
    Pointer({$ifndef FPC}@{$endif} eglQueryContext) := glGetProcAddress(EGLLib,'eglQueryContext');
    Pointer({$ifndef FPC}@{$endif} eglWaitGL) := glGetProcAddress(EGLLib,'eglWaitGL');
    Pointer({$ifndef FPC}@{$endif} eglWaitNative) := glGetProcAddress(EGLLib,'eglWaitNative');
    Pointer({$ifndef FPC}@{$endif} eglSwapBuffers) := glGetProcAddress(EGLLib,'eglSwapBuffers');
    Pointer({$ifndef FPC}@{$endif} eglCopyBuffers) := glGetProcAddress(EGLLib,'eglCopyBuffers');
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  LoadEgl;
  {$endif}
finalization
  FreeEGL;
end.