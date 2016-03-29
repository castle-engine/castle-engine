{ OpenGL ES 2.0 API.

  Copied from fpc/trunk/packages/opengles/src/gles20.pas.
  For some reason, this unit is not compiled by default for Android
  (FPC 2.7.1 includes it for normal Linux). }

{** OpenGL ES 2.0 headers
 **
 ** Ported/Translated for FreePascal by Benjamin 'BeRo' Rosseaux
 ** benjamin@rosseaux.com - http://www.rosseaux.com
 **
 ** EGL part:
 **
 ** Copyright (c) 2007-2009 The Khronos Group Inc.
 **
 ** Permission is hereby granted, free of charge, to any person obtaining a
 ** copy of this software and/or associated documentation files (the
 ** "Materials"), to deal in the Materials without restriction, including
 ** without limitation the rights to use, copy, modify, merge, publish,
 ** distribute, sublicense, and/or sell copies of the Materials, and to
 ** permit persons to whom the Materials are furnished to do so, subject to
 ** the following conditions:
 **
 ** The above copyright notice and this permission notice shall be included
 ** in all copies or substantial portions of the Materials.
 **
 ** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 ** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 ** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 ** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 ** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 ** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 ** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 **
 ** GLESv2 part:
 **
 ** This document is licensed under the SGI Free Software B License Version
 ** 2.0. For details, see http://oss.sgi.com/projects/FreeB/
 **}
unit CastleGLES20;
{$i castleconf.inc}

{$ifdef linux}
  {$define EGL}
{$endif}
{$ifdef windows}
  {$define EGL}
{$endif}
{$ifdef ANDROID}
  {$define EGL}
{$endif}

interface

uses SysUtils,dynlibs{$ifdef linux},x,xlib{$endif}{$ifdef windows},Windows{$endif};

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Type
  Pchar  = ^char;

{$ifdef EGL}
type
  PEGLConfig  = ^EGLConfig;
  PEGLint  = ^EGLint;
     EGLint = {$ifdef win64}int64{$else}longint{$endif}; // Why int64 only on win64 and not even on 64-bit linux???

     EGLConfig = pointer;

  { EGL Types  }
  { EGLint is defined in eglplatform.h  }

  type

{$ifdef linux}
     EGLNativeDisplayType = PDisplay;

     EGLNativeWindowType = TWindow;

     EGLNativePixmapType = TPixmap;
{$else linux}
{$ifdef windows}
     EGLNativeDisplayType = HDC;

     EGLNativeWindowType = HWND;

     EGLNativePixmapType = HBITMAP;
{$else windows}
     EGLNativeDisplayType = ptrint;

     EGLNativeWindowType = pointer;

     EGLNativePixmapType = pointer;
{$endif windows}
{$endif linux}

     EGLBoolean = dword;

     EGLenum = dword;


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
   * aspect ratio are scaled when queried as integer values.
    }
     EGL_DISPLAY_SCALING = 10000;
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
   * API Registrar, who maintains the enumerant registry. Submit
   * a bug in Khronos Bugzilla against task "Registry".
    }
  { EGL Functions  }

  var
    eglGetError : function:EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglTerminate : function(dpy:EGLDisplay):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    eglQueryString : function(dpy:EGLDisplay; name:EGLint):pchar;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
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
  { This is a generic function pointer type, whose name indicates it must
   * be cast to the proper type *and calling convention* before use.
    }

  type

     __eglMustCastToProperFunctionPointerType = procedure (_para1:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { Now, define eglGetProcAddress using the generic function ptr. type  }
(* Const before type ignored *)

  var
    eglGetProcAddress : function(procname:pchar):__eglMustCastToProperFunctionPointerType;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { Header file version number  }
  { Current version at http://www.khronos.org/registry/egl/  }

  const
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

  const
     EGL_KHR_image = 1;
  { eglCreateImageKHR target  }
     EGL_NATIVE_PIXMAP_KHR = $30B0;

type
     EGLImageKHR = pointer;
  { was #define dname def_expr }
  function EGL_NO_IMAGE_KHR : EGLImageKHR;

(* Const before type ignored *)

  const
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

{$endif EGL}


type
  PGLubyte = ^GLubyte;
  PGLboolean  = ^GLboolean;
  PGLenum  = ^GLenum;
  PGLfloat  = ^GLfloat;
  PGLint  = ^GLint;
  PGLsizei  = ^GLsizei;
  PGLuint  = ^GLuint;

  {-------------------------------------------------------------------------
   * Data type definitions
   *----------------------------------------------------------------------- }

     GLvoid = pointer;
     TGLvoid = GLvoid;

     GLenum = dword;
     TGLenum = GLenum;

     GLboolean = byte;
     TGLboolean = GLboolean;

     GLbitfield = dword;
     TGLbitfield = GLbitfield;

     GLbyte = shortint;
     TGLbyte = GLbyte;

     GLshort = smallint;
     TGLshort = GLshort;

     GLint = longint;
     TGLint = GLint;

     GLsizei = longint;
     TGLsizei = GLsizei;

     GLubyte = byte;
     TGLubyte = GLubyte;

     GLushort = word;
     TGLushort = GLushort;

     GLuint = longword;
     TGLuint = GLuint;

     GLfloat = single;
     TGLfloat = GLfloat;

     GLclampf = single;
     TGLclampf = GLclampf;

     GLfixed = longint;
     TGLfixed = GLfixed;
  { GL types for handling large vertex buffer objects  }

     GLintptr = ptrint;

     GLsizeiptr = ptrint;
  { OpenGL ES core versions  }

  const
     GL_ES_VERSION_2_0 = 1;
  { ClearBufferMask  }
     GL_DEPTH_BUFFER_BIT = $00000100;
     GL_STENCIL_BUFFER_BIT = $00000400;
     GL_COLOR_BUFFER_BIT = $00004000;
  { Boolean  }
     GL_FALSE = 0;
     GL_TRUE = 1;
  { BeginMode  }
     GL_POINTS = $0000;
     GL_LINES = $0001;
     GL_LINE_LOOP = $0002;
     GL_LINE_STRIP = $0003;
     GL_TRIANGLES = $0004;
     GL_TRIANGLE_STRIP = $0005;
     GL_TRIANGLE_FAN = $0006;
  { AlphaFunction (not supported in ES20)  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { BlendingFactorDest  }
     GL_ZERO = 0;
     GL_ONE = 1;
     GL_SRC_COLOR = $0300;
     GL_ONE_MINUS_SRC_COLOR = $0301;
     GL_SRC_ALPHA = $0302;
     GL_ONE_MINUS_SRC_ALPHA = $0303;
     GL_DST_ALPHA = $0304;
     GL_ONE_MINUS_DST_ALPHA = $0305;
  { BlendingFactorSrc  }
  {      GL_ZERO  }
  {      GL_ONE  }
     GL_DST_COLOR = $0306;
     GL_ONE_MINUS_DST_COLOR = $0307;
     GL_SRC_ALPHA_SATURATE = $0308;
  {      GL_SRC_ALPHA  }
  {      GL_ONE_MINUS_SRC_ALPHA  }
  {      GL_DST_ALPHA  }
  {      GL_ONE_MINUS_DST_ALPHA  }
  { BlendEquationSeparate  }
     GL_FUNC_ADD = $8006;
     GL_BLEND_EQUATION = $8009;
  { same as BLEND_EQUATION  }
     GL_BLEND_EQUATION_RGB = $8009;
     GL_BLEND_EQUATION_ALPHA = $883D;
  { BlendSubtract  }
     GL_FUNC_SUBTRACT = $800A;
     GL_FUNC_REVERSE_SUBTRACT = $800B;
  { Separate Blend Functions  }
     GL_BLEND_DST_RGB = $80C8;
     GL_BLEND_SRC_RGB = $80C9;
     GL_BLEND_DST_ALPHA = $80CA;
     GL_BLEND_SRC_ALPHA = $80CB;
     GL_CONSTANT_COLOR = $8001;
     GL_ONE_MINUS_CONSTANT_COLOR = $8002;
     GL_CONSTANT_ALPHA = $8003;
     GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
     GL_BLEND_COLOR = $8005;
  { Buffer Objects  }
     GL_ARRAY_BUFFER = $8892;
     GL_ELEMENT_ARRAY_BUFFER = $8893;
     GL_ARRAY_BUFFER_BINDING = $8894;
     GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
     GL_STREAM_DRAW = $88E0;
     GL_STATIC_DRAW = $88E4;
     GL_DYNAMIC_DRAW = $88E8;
     GL_BUFFER_SIZE = $8764;
     GL_BUFFER_USAGE = $8765;
     GL_CURRENT_VERTEX_ATTRIB = $8626;
  { CullFaceMode  }
     GL_FRONT = $0404;
     GL_BACK = $0405;
     GL_FRONT_AND_BACK = $0408;
  { DepthFunction  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { EnableCap  }
     GL_TEXTURE_2D = $0DE1;
     GL_CULL_FACE = $0B44;
     GL_BLEND = $0BE2;
     GL_DITHER = $0BD0;
     GL_STENCIL_TEST = $0B90;
     GL_DEPTH_TEST = $0B71;
     GL_SCISSOR_TEST = $0C11;
     GL_POLYGON_OFFSET_FILL = $8037;
     GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
     GL_SAMPLE_COVERAGE = $80A0;
  { ErrorCode  }
     GL_NO_ERROR = 0;
     GL_INVALID_ENUM = $0500;
     GL_INVALID_VALUE = $0501;
     GL_INVALID_OPERATION = $0502;
     GL_OUT_OF_MEMORY = $0505;
  { FrontFaceDirection  }
     GL_CW = $0900;
     GL_CCW = $0901;
  { GetPName  }
     GL_LINE_WIDTH = $0B21;
     GL_ALIASED_POINT_SIZE_RANGE = $846D;
     GL_ALIASED_LINE_WIDTH_RANGE = $846E;
     GL_CULL_FACE_MODE = $0B45;
     GL_FRONT_FACE = $0B46;
     GL_DEPTH_RANGE = $0B70;
     GL_DEPTH_WRITEMASK = $0B72;
     GL_DEPTH_CLEAR_VALUE = $0B73;
     GL_DEPTH_FUNC = $0B74;
     GL_STENCIL_CLEAR_VALUE = $0B91;
     GL_STENCIL_FUNC = $0B92;
     GL_STENCIL_FAIL = $0B94;
     GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
     GL_STENCIL_PASS_DEPTH_PASS = $0B96;
     GL_STENCIL_REF = $0B97;
     GL_STENCIL_VALUE_MASK = $0B93;
     GL_STENCIL_WRITEMASK = $0B98;
     GL_STENCIL_BACK_FUNC = $8800;
     GL_STENCIL_BACK_FAIL = $8801;
     GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
     GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
     GL_STENCIL_BACK_REF = $8CA3;
     GL_STENCIL_BACK_VALUE_MASK = $8CA4;
     GL_STENCIL_BACK_WRITEMASK = $8CA5;
     GL_VIEWPORT = $0BA2;
     GL_SCISSOR_BOX = $0C10;
  {      GL_SCISSOR_TEST  }
     GL_COLOR_CLEAR_VALUE = $0C22;
     GL_COLOR_WRITEMASK = $0C23;
     GL_UNPACK_ALIGNMENT = $0CF5;
     GL_PACK_ALIGNMENT = $0D05;
     GL_MAX_TEXTURE_SIZE = $0D33;
     GL_MAX_VIEWPORT_DIMS = $0D3A;
     GL_SUBPIXEL_BITS = $0D50;
     GL_RED_BITS = $0D52;
     GL_GREEN_BITS = $0D53;
     GL_BLUE_BITS = $0D54;
     GL_ALPHA_BITS = $0D55;
     GL_DEPTH_BITS = $0D56;
     GL_STENCIL_BITS = $0D57;
     GL_POLYGON_OFFSET_UNITS = $2A00;
  {      GL_POLYGON_OFFSET_FILL  }
     GL_POLYGON_OFFSET_FACTOR = $8038;
     GL_TEXTURE_BINDING_2D = $8069;
     GL_SAMPLE_BUFFERS = $80A8;
     GL_SAMPLES = $80A9;
     GL_SAMPLE_COVERAGE_VALUE = $80AA;
     GL_SAMPLE_COVERAGE_INVERT = $80AB;
  { GetTextureParameter  }
  {      GL_TEXTURE_MAG_FILTER  }
  {      GL_TEXTURE_MIN_FILTER  }
  {      GL_TEXTURE_WRAP_S  }
  {      GL_TEXTURE_WRAP_T  }
     GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
     GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  { HintMode  }
     GL_DONT_CARE = $1100;
     GL_FASTEST = $1101;
     GL_NICEST = $1102;
  { HintTarget  }
     GL_GENERATE_MIPMAP_HINT = $8192;
  { DataType  }
     GL_BYTE = $1400;
     GL_UNSIGNED_BYTE = $1401;
     GL_SHORT = $1402;
     GL_UNSIGNED_SHORT = $1403;
     GL_INT = $1404;
     GL_UNSIGNED_INT = $1405;
     GL_FLOAT = $1406;
     GL_FIXED = $140C;
  { PixelFormat  }
     GL_DEPTH_COMPONENT = $1902;
     GL_ALPHA = $1906;
     GL_RGB = $1907;
     GL_RGBA = $1908;
     GL_LUMINANCE = $1909;
     GL_LUMINANCE_ALPHA = $190A;
  { PixelType  }
  {      GL_UNSIGNED_BYTE  }
     GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
     GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
     GL_UNSIGNED_SHORT_5_6_5 = $8363;
  { Shaders  }
     GL_FRAGMENT_SHADER = $8B30;
     GL_VERTEX_SHADER = $8B31;
     GL_MAX_VERTEX_ATTRIBS = $8869;
     GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
     GL_MAX_VARYING_VECTORS = $8DFC;
     GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
     GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
     GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
     GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
     GL_SHADER_TYPE = $8B4F;
     GL_DELETE_STATUS = $8B80;
     GL_LINK_STATUS = $8B82;
     GL_VALIDATE_STATUS = $8B83;
     GL_ATTACHED_SHADERS = $8B85;
     GL_ACTIVE_UNIFORMS = $8B86;
     GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
     GL_ACTIVE_ATTRIBUTES = $8B89;
     GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
     GL_SHADING_LANGUAGE_VERSION = $8B8C;
     GL_CURRENT_PROGRAM = $8B8D;
  { StencilFunction  }
     GL_NEVER = $0200;
     GL_LESS = $0201;
     GL_EQUAL = $0202;
     GL_LEQUAL = $0203;
     GL_GREATER = $0204;
     GL_NOTEQUAL = $0205;
     GL_GEQUAL = $0206;
     GL_ALWAYS = $0207;
  { StencilOp  }
  {      GL_ZERO  }
     GL_KEEP = $1E00;
     GL_REPLACE = $1E01;
     GL_INCR = $1E02;
     GL_DECR = $1E03;
     GL_INVERT = $150A;
     GL_INCR_WRAP = $8507;
     GL_DECR_WRAP = $8508;
  { StringName  }
     GL_VENDOR = $1F00;
     GL_RENDERER = $1F01;
     GL_VERSION = $1F02;
     GL_EXTENSIONS = $1F03;
  { TextureMagFilter  }
     GL_NEAREST = $2600;
     GL_LINEAR = $2601;
  { TextureMinFilter  }
  {      GL_NEAREST  }
  {      GL_LINEAR  }
     GL_NEAREST_MIPMAP_NEAREST = $2700;
     GL_LINEAR_MIPMAP_NEAREST = $2701;
     GL_NEAREST_MIPMAP_LINEAR = $2702;
     GL_LINEAR_MIPMAP_LINEAR = $2703;
  { TextureParameterName  }
     GL_TEXTURE_MAG_FILTER = $2800;
     GL_TEXTURE_MIN_FILTER = $2801;
     GL_TEXTURE_WRAP_S = $2802;
     GL_TEXTURE_WRAP_T = $2803;
  { TextureTarget  }
  {      GL_TEXTURE_2D  }
     GL_TEXTURE = $1702;
     GL_TEXTURE_CUBE_MAP = $8513;
     GL_TEXTURE_BINDING_CUBE_MAP = $8514;
     GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
     GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  { TextureUnit  }
     GL_TEXTURE0 = $84C0;
     GL_TEXTURE1 = $84C1;
     GL_TEXTURE2 = $84C2;
     GL_TEXTURE3 = $84C3;
     GL_TEXTURE4 = $84C4;
     GL_TEXTURE5 = $84C5;
     GL_TEXTURE6 = $84C6;
     GL_TEXTURE7 = $84C7;
     GL_TEXTURE8 = $84C8;
     GL_TEXTURE9 = $84C9;
     GL_TEXTURE10 = $84CA;
     GL_TEXTURE11 = $84CB;
     GL_TEXTURE12 = $84CC;
     GL_TEXTURE13 = $84CD;
     GL_TEXTURE14 = $84CE;
     GL_TEXTURE15 = $84CF;
     GL_TEXTURE16 = $84D0;
     GL_TEXTURE17 = $84D1;
     GL_TEXTURE18 = $84D2;
     GL_TEXTURE19 = $84D3;
     GL_TEXTURE20 = $84D4;
     GL_TEXTURE21 = $84D5;
     GL_TEXTURE22 = $84D6;
     GL_TEXTURE23 = $84D7;
     GL_TEXTURE24 = $84D8;
     GL_TEXTURE25 = $84D9;
     GL_TEXTURE26 = $84DA;
     GL_TEXTURE27 = $84DB;
     GL_TEXTURE28 = $84DC;
     GL_TEXTURE29 = $84DD;
     GL_TEXTURE30 = $84DE;
     GL_TEXTURE31 = $84DF;
     GL_ACTIVE_TEXTURE = $84E0;
  { TextureWrapMode  }
     GL_REPEAT = $2901;
     GL_CLAMP_TO_EDGE = $812F;
     GL_MIRRORED_REPEAT = $8370;
  { Uniform Types  }
     GL_FLOAT_VEC2 = $8B50;
     GL_FLOAT_VEC3 = $8B51;
     GL_FLOAT_VEC4 = $8B52;
     GL_INT_VEC2 = $8B53;
     GL_INT_VEC3 = $8B54;
     GL_INT_VEC4 = $8B55;
     GL_BOOL = $8B56;
     GL_BOOL_VEC2 = $8B57;
     GL_BOOL_VEC3 = $8B58;
     GL_BOOL_VEC4 = $8B59;
     GL_FLOAT_MAT2 = $8B5A;
     GL_FLOAT_MAT3 = $8B5B;
     GL_FLOAT_MAT4 = $8B5C;
     GL_SAMPLER_2D = $8B5E;
     GL_SAMPLER_CUBE = $8B60;
  { Vertex Arrays  }
     GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
     GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
     GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
     GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
     GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
     GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
     GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  { Read Format  }
     GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
     GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  { Shader Source  }
     GL_COMPILE_STATUS = $8B81;
     GL_INFO_LOG_LENGTH = $8B84;
     GL_SHADER_SOURCE_LENGTH = $8B88;
     GL_SHADER_COMPILER = $8DFA;
  { Shader Binary  }
     GL_SHADER_BINARY_FORMATS = $8DF8;
     GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  { Shader Precision-Specified Types  }
     GL_LOW_FLOAT = $8DF0;
     GL_MEDIUM_FLOAT = $8DF1;
     GL_HIGH_FLOAT = $8DF2;
     GL_LOW_INT = $8DF3;
     GL_MEDIUM_INT = $8DF4;
     GL_HIGH_INT = $8DF5;
  { Framebuffer Object.  }
     GL_FRAMEBUFFER = $8D40;
     GL_RENDERBUFFER = $8D41;
     GL_RGBA4 = $8056;
     GL_RGB5_A1 = $8057;
     GL_RGB565 = $8D62;
     GL_DEPTH_COMPONENT16 = $81A5;
     GL_STENCIL_INDEX = $1901;
     GL_STENCIL_INDEX8 = $8D48;
     GL_RENDERBUFFER_WIDTH = $8D42;
     GL_RENDERBUFFER_HEIGHT = $8D43;
     GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
     GL_RENDERBUFFER_RED_SIZE = $8D50;
     GL_RENDERBUFFER_GREEN_SIZE = $8D51;
     GL_RENDERBUFFER_BLUE_SIZE = $8D52;
     GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
     GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
     GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
     GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
     GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
     GL_COLOR_ATTACHMENT0 = $8CE0;
     GL_DEPTH_ATTACHMENT = $8D00;
     GL_STENCIL_ATTACHMENT = $8D20;
     GL_NONE = 0;
     GL_FRAMEBUFFER_COMPLETE = $8CD5;
     GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
     GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
     GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
     GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
     GL_FRAMEBUFFER_BINDING = $8CA6;
     GL_RENDERBUFFER_BINDING = $8CA7;
     GL_MAX_RENDERBUFFER_SIZE = $84E8;
     GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  {-------------------------------------------------------------------------
   * GL core functions.
   *----------------------------------------------------------------------- }

  var
    glActiveTexture : procedure(texture:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glAttachShader : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBindAttribLocation : procedure(_program:GLuint; index:GLuint; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindBuffer : procedure(target:GLenum; buffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindFramebuffer : procedure(target:GLenum; framebuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindRenderbuffer : procedure(target:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindTexture : procedure(target:GLenum; texture:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendColor : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendEquation : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendEquationSeparate : procedure(modeRGB:GLenum; modeAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendFunc : procedure(sfactor:GLenum; dfactor:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlendFuncSeparate : procedure(srcRGB:GLenum; dstRGB:GLenum; srcAlpha:GLenum; dstAlpha:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBufferData : procedure(target:GLenum; size:GLsizeiptr; data:pointer; usage:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glBufferSubData : procedure(target:GLenum; offset:GLintptr; size:GLsizeiptr; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCheckFramebufferStatus : function(target:GLenum):GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClear : procedure(mask:GLbitfield);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearColor : procedure(red:GLclampf; green:GLclampf; blue:GLclampf; alpha:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearDepthf : procedure(depth:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearStencil : procedure(s:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glColorMask : procedure(red:GLboolean; green:GLboolean; blue:GLboolean; alpha:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCompileShader : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;
      height:GLsizei; format:GLenum; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; x:GLint; y:GLint;
      width:GLsizei; height:GLsizei; border:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; x:GLint;
      y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCreateProgram : function:GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCreateShader : function(_type:GLenum):GLuint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCullFace : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteBuffers : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteFramebuffers : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteRenderbuffers : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteShader : procedure(shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDeleteTextures : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthFunc : procedure(func:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthMask : procedure(flag:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDepthRangef : procedure(zNear:GLclampf; zFar:GLclampf);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDetachShader : procedure(_program:GLuint; shader:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDisable : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDisableVertexAttribArray : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawArrays : procedure(mode:GLenum; first:GLint; count:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glDrawElements : procedure(mode:GLenum; count:GLsizei; _type:GLenum; indices:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEnable : procedure(cap:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEnableVertexAttribArray : procedure(index:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFinish : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFlush : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferRenderbuffer : procedure(target:GLenum; attachment:GLenum; renderbuffertarget:GLenum; renderbuffer:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferTexture2D : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFrontFace : procedure(mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenBuffers : procedure(n:GLsizei; buffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenerateMipmap : procedure(target:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenFramebuffers : procedure(n:GLsizei; framebuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenRenderbuffers : procedure(n:GLsizei; renderbuffers:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenTextures : procedure(n:GLsizei; textures:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveAttrib : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;
      _type:pGLenum; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniform : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;
      _type:pGLenum; name:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetAttachedShaders : procedure(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetAttribLocation : function(_program:GLuint; name:pchar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBooleanv : procedure(pname:GLenum; params:pGLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetError : function:GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFloatv : procedure(pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFramebufferAttachmentParameteriv : procedure(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetIntegerv : procedure(pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramiv : procedure(_program:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramInfoLog : procedure(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetRenderbufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderiv : procedure(shader:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderInfoLog : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderPrecisionFormat : procedure(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderSource : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetString : function(name:GLenum):PGLubyte;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameterfv : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformfv : procedure(_program:GLuint; location:GLint; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformiv : procedure(_program:GLuint; location:GLint; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetUniformLocation : function(_program:GLuint; name:pchar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribfv : procedure(index:GLuint; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribiv : procedure(index:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribPointerv : procedure(index:GLuint; pname:GLenum; pointer:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glHint : procedure(target:GLenum; mode:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsBuffer : function(buffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsEnabled : function(cap:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsFramebuffer : function(framebuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsProgram : function(_program:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsRenderbuffer : function(renderbuffer:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsShader : function(shader:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsTexture : function(texture:GLuint):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glLineWidth : procedure(width:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glLinkProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glPixelStorei : procedure(pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glPolygonOffset : procedure(factor:GLfloat; units:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glReadPixels : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei; format:GLenum;
      _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glReleaseShaderCompiler : procedure;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glRenderbufferStorage : procedure(target:GLenum; internalformat:GLenum; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSampleCoverage : procedure(value:GLclampf; invert:GLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glScissor : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
    glShaderBinary : procedure(n:GLsizei; shaders:pGLuint; binaryformat:GLenum; binary:pointer; length:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
(* Const before type ignored *)
    glShaderSource : procedure(shader:GLuint; count:GLsizei; _string:Ppchar; length:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilFunc : procedure(func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilFuncSeparate : procedure(face:GLenum; func:GLenum; ref:GLint; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilMask : procedure(mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilMaskSeparate : procedure(face:GLenum; mask:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilOp : procedure(fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glStencilOpSeparate : procedure(face:GLenum; fail:GLenum; zfail:GLenum; zpass:GLenum);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexImage2D : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexParameterf : procedure(target:GLenum; pname:GLenum; param:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexParameterfv : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexParameteri : procedure(target:GLenum; pname:GLenum; param:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexSubImage2D : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; width:GLsizei;
      height:GLsizei; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1f : procedure(location:GLint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform1fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1i : procedure(location:GLint; x:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform1iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2f : procedure(location:GLint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform2fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2i : procedure(location:GLint; x:GLint; y:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform2iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3f : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform3fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3i : procedure(location:GLint; x:GLint; y:GLint; z:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform3iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4f : procedure(location:GLint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform4fv : procedure(location:GLint; count:GLsizei; v:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4i : procedure(location:GLint; x:GLint; y:GLint; z:GLint; w:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniform4iv : procedure(location:GLint; count:GLsizei; v:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix2fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix3fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glUniformMatrix4fv : procedure(location:GLint; count:GLsizei; transpose:GLboolean; value:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUseProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glValidateProgram : procedure(_program:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib1f : procedure(indx:GLuint; x:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib1fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib2f : procedure(indx:GLuint; x:GLfloat; y:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib2fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib3f : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib3fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttrib4f : procedure(indx:GLuint; x:GLfloat; y:GLfloat; z:GLfloat; w:GLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttrib4fv : procedure(indx:GLuint; values:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glVertexAttribPointer : procedure(indx:GLuint; size:GLint; _type:GLenum; normalized:GLboolean; stride:GLsizei;
      ptr:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glViewport : procedure(x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  {------------------------------------------------------------------------*
   * IMG extension tokens
   *------------------------------------------------------------------------ }
  { GL_IMG_binary_shader  }

  const
     GL_SGX_BINARY_IMG = $8C0A;
  { GL_IMG_texture_compression_pvrtc  }
     GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = $8C00;
     GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = $8C01;
     GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
     GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;
     GL_BGRA = $80E1;
  {------------------------------------------------------------------------*
   * IMG extension functions
   *------------------------------------------------------------------------ }
  { GL_IMG_binary_shader  }
     GL_IMG_binary_shader = 1;
  { GL_IMG_texture_compression_pvrtc  }
     GL_IMG_texture_compression_pvrtc = 1;
  {
   * This document is licensed under the SGI Free Software B License Version
   * 2.0. For details, see http://oss.sgi.com/projects/FreeB/ .
    }
  {------------------------------------------------------------------------*
   * OES extension tokens
   *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
     GL_ETC1_RGB8_OES = $8D64;
  { GL_OES_compressed_paletted_texture  }
     GL_PALETTE4_RGB8_OES = $8B90;
     GL_PALETTE4_RGBA8_OES = $8B91;
     GL_PALETTE4_R5_G6_B5_OES = $8B92;
     GL_PALETTE4_RGBA4_OES = $8B93;
     GL_PALETTE4_RGB5_A1_OES = $8B94;
     GL_PALETTE8_RGB8_OES = $8B95;
     GL_PALETTE8_RGBA8_OES = $8B96;
     GL_PALETTE8_R5_G6_B5_OES = $8B97;
     GL_PALETTE8_RGBA4_OES = $8B98;
     GL_PALETTE8_RGB5_A1_OES = $8B99;
  { GL_OES_depth24  }
     GL_DEPTH_COMPONENT24_OES = $81A6;
  { GL_OES_depth32  }
     GL_DEPTH_COMPONENT32_OES = $81A7;
  { GL_OES_depth_texture  }
  { No new tokens introduced by this extension.  }
  { GL_OES_EGL_image  }

  type

     GLeglImageOES = pointer;
  { GL_OES_get_program_binary  }

  const
     GL_PROGRAM_BINARY_LENGTH_OES = $8741;
     GL_NUM_PROGRAM_BINARY_FORMATS_OES = $87FE;
     GL_PROGRAM_BINARY_FORMATS_OES = $87FF;
  { GL_OES_mapbuffer  }
     GL_WRITE_ONLY_OES = $88B9;
     GL_BUFFER_ACCESS_OES = $88BB;
     GL_BUFFER_MAPPED_OES = $88BC;
     GL_BUFFER_MAP_POINTER_OES = $88BD;
  { GL_OES_packed_depth_stencil  }
     GL_DEPTH_STENCIL_OES = $84F9;
     GL_UNSIGNED_INT_24_8_OES = $84FA;
     GL_DEPTH24_STENCIL8_OES = $88F0;
  { GL_OES_rgb8_rgba8  }
     GL_RGB8_OES = $8051;
     GL_RGBA8_OES = $8058;
  { GL_OES_standard_derivatives  }
     GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES = $8B8B;
  { GL_OES_stencil1  }
     GL_STENCIL_INDEX1_OES = $8D46;
  { GL_OES_stencil4  }
     GL_STENCIL_INDEX4_OES = $8D47;
  { GL_OES_texture3D  }
     GL_TEXTURE_WRAP_R_OES = $8072;
     GL_TEXTURE_3D_OES = $806F;
     GL_TEXTURE_BINDING_3D_OES = $806A;
     GL_MAX_3D_TEXTURE_SIZE_OES = $8073;
     GL_SAMPLER_3D_OES = $8B5F;
     GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_OES = $8CD4;
  { GL_OES_texture_half_float  }
     GL_HALF_FLOAT_OES = $8D61;
  { GL_OES_vertex_half_float  }
  { GL_HALF_FLOAT_OES defined in GL_OES_texture_half_float already.  }
  { GL_OES_vertex_type_10_10_10_2  }
     GL_UNSIGNED_INT_10_10_10_2_OES = $8DF6;
     GL_INT_10_10_10_2_OES = $8DF7;
  {------------------------------------------------------------------------*
   * AMD extension tokens
   *------------------------------------------------------------------------ }
  { GL_AMD_compressed_3DC_texture  }
     GL_3DC_X_AMD = $87F9;
     GL_3DC_XY_AMD = $87FA;
  { GL_AMD_compressed_ATC_texture  }
     GL_ATC_RGB_AMD = $8C92;
     GL_ATC_RGBA_EXPLICIT_ALPHA_AMD = $8C93;
     GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD = $87EE;
  { GL_AMD_program_binary_Z400  }
     GL_Z400_BINARY_AMD = $8740;
  { GL_AMD_performance_monitor  }
{$define GL_AMD_performance_monitor}
     GL_COUNTER_TYPE_AMD = $8BC0;
     GL_COUNTER_RANGE_AMD = $8BC1;
     GL_UNSIGNED_INT64_AMD = $8BC2;
     GL_PERCENTAGE_AMD = $8BC3;
     GL_PERFMON_RESULT_AVAILABLE_AMD = $8BC4;
     GL_PERFMON_RESULT_SIZE_AMD = $8BC5;
     GL_PERFMON_RESULT_AMD = $8BC6;
  {------------------------------------------------------------------------*
   * EXT extension tokens
   *------------------------------------------------------------------------ }
  { GL_EXT_texture_filter_anisotropic  }
     GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
     GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  { GL_EXT_texture_type_2_10_10_10_REV  }
     GL_UNSIGNED_INT_2_10_10_10_REV_EXT = $8368;
  {------------------------------------------------------------------------*
   * OES extension functions
   *------------------------------------------------------------------------ }
  { GL_OES_compressed_ETC1_RGB8_texture  }
     GL_OES_compressed_ETC1_RGB8_texture = 1;
  { GL_OES_compressed_paletted_texture  }
     GL_OES_compressed_paletted_texture = 1;
  { GL_OES_EGL_image  }

  var
    glEGLImageTargetTexture2DOES : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEGLImageTargetRenderbufferStorageOES : procedure(target:GLenum; image:GLeglImageOES);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  { GL_OES_depth24  }

  const
     GL_OES_depth24 = 1;
  { GL_OES_depth32  }
     GL_OES_depth32 = 1;
  { GL_OES_depth_texture  }
     GL_OES_depth_texture = 1;
  { GL_OES_element_index_uint  }
     GL_OES_element_index_uint = 1;
  { GL_OES_fbo_render_mipmap  }
     GL_OES_fbo_render_mipmap = 1;
  { GL_OES_fragment_precision_high  }
     GL_OES_fragment_precision_high = 1;
  { GL_OES_get_program_binary  }

  var
    glGetProgramBinaryOES : procedure(_program:GLuint; bufSize:GLsizei; length:pGLsizei; binaryFormat:pGLenum; binary:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glProgramBinaryOES : procedure(_program:GLuint; binaryFormat:GLenum; binary:pointer; length:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

(* Const before type ignored *)
  { GL_OES_mapbuffer  }

  const
     GL_OES_mapbuffer = 1;

  var
    glMapBufferOES : function(target:GLenum; access:GLenum):pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUnmapBufferOES : function(target:GLenum):GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferPointervOES : procedure(target:GLenum; pname:GLenum; params:Ppointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  type

     PFNGLMAPBUFFEROESPROC = pointer;
  { GL_OES_packed_depth_stencil  }

  const
     GL_OES_packed_depth_stencil = 1;
  { GL_OES_rgb8_rgba8  }
     GL_OES_rgb8_rgba8 = 1;
  { GL_OES_standard_derivatives  }
     GL_OES_standard_derivatives = 1;
  { GL_OES_stencil1  }
     GL_OES_stencil1 = 1;
  { GL_OES_stencil4  }
     GL_OES_stencil4 = 1;
  { GL_OES_texture_3D  }
     GL_OES_texture_3D = 1;
(* Const before type ignored *)

  var
    glTexImage3DOES : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      depth:GLsizei; border:GLint; format:GLenum; _type:GLenum; pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; _type:GLenum;
      pixels:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      x:GLint; y:GLint; width:GLsizei; height:GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexImage3DOES : procedure(target:GLenum; level:GLint; internalformat:GLenum; width:GLsizei; height:GLsizei;
      depth:GLsizei; border:GLint; imageSize:GLsizei; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glCompressedTexSubImage3DOES : procedure(target:GLenum; level:GLint; xoffset:GLint; yoffset:GLint; zoffset:GLint;
      width:GLsizei; height:GLsizei; depth:GLsizei; format:GLenum; imageSize:GLsizei;
      data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferTexture3DOES : procedure(target:GLenum; attachment:GLenum; textarget:GLenum; texture:GLuint; level:GLint;
      zoffset:GLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  { GL_OES_texture_float_linear  }

  const
     GL_OES_texture_float_linear = 1;
  { GL_OES_texture_half_float_linear  }
     GL_OES_texture_half_float_linear = 1;
  { GL_OES_texture_float  }
     GL_OES_texture_float = 1;
  { GL_OES_texture_half_float  }
     GL_OES_texture_half_float = 1;
  { GL_OES_texture_npot  }
     GL_OES_texture_npot = 1;
  { GL_OES_vertex_half_float  }
     GL_OES_vertex_half_float = 1;
  { GL_OES_vertex_type_10_10_10_2  }
     GL_OES_vertex_type_10_10_10_2 = 1;
  {------------------------------------------------------------------------*
   * AMD extension functions
   *------------------------------------------------------------------------ }
  { GL_AMD_compressed_3DC_texture  }
     GL_AMD_compressed_3DC_texture = 1;
  { GL_AMD_compressed_ATC_texture  }
     GL_AMD_compressed_ATC_texture = 1;
  { GL_AMD_program_binary_Z400  }
     GL_AMD_program_binary_Z400 = 1;
  { AMD_performance_monitor  }
     GL_AMD_performance_monitor = 1;

  var
    glGetPerfMonitorGroupsAMD : procedure(numGroups:pGLint; groupsSize:GLsizei; groups:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCountersAMD : procedure(group:GLuint; numCounters:pGLint; maxActiveCounters:pGLint; counterSize:GLsizei; counters:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorGroupStringAMD : procedure(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterStringAMD : procedure(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:pchar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterInfoAMD : procedure(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenPerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeletePerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSelectPerfMonitorCountersAMD : procedure(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBeginPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEndPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterDataAMD : procedure(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  {------------------------------------------------------------------------*
   * EXT extension functions
   *------------------------------------------------------------------------ }
  { GL_EXT_texture_filter_anisotropic  }

  const
     GL_EXT_texture_filter_anisotropic = 1;
  { GL_EXT_texture_type_2_10_10_10_REV  }
     GL_EXT_texture_type_2_10_10_10_REV = 1;

function glGetProcAddress(ahlib:tlibhandle;ProcName:pchar):pointer;

procedure GLES20Initialization;

implementation

  function glGetProcAddress(ahlib:tlibhandle;ProcName:pchar):pointer;
    begin
      result:=dynlibs.GetProcAddress(ahlib,ProcName);
{$ifdef EGL}
      if assigned(eglGetProcAddress) and not assigned(result) then
        result:=eglGetProcAddress(ProcName);
{$endif}
    end;

{$ifdef EGL}
  { was #define dname def_expr }
  function EGL_DEFAULT_DISPLAY : EGLNativeDisplayType;
      begin
         EGL_DEFAULT_DISPLAY:=EGLNativeDisplayType(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_CONTEXT : EGLContext;
      begin
         EGL_NO_CONTEXT:=EGLContext(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_DISPLAY : EGLDisplay;
      begin
         EGL_NO_DISPLAY:=EGLDisplay(0);
      end;

  { was #define dname def_expr }
  function EGL_NO_SURFACE : EGLSurface;
      begin
         EGL_NO_SURFACE:=EGLSurface(0);
      end;

  { was #define dname def_expr }
  function EGL_DONT_CARE : EGLint;
      begin
         EGL_DONT_CARE:=EGLint(-(1));
      end;

  { was #define dname def_expr }
  function EGL_UNKNOWN : EGLint;
      begin
         EGL_UNKNOWN:=EGLint(-(1));
      end;

  { was #define dname def_expr }
  function EGL_NO_IMAGE_KHR : EGLImageKHR;
      begin
         EGL_NO_IMAGE_KHR:=EGLImageKHR(0);
      end;


  var
    EGLLib : tlibhandle;


  procedure FreeEGL;
    begin
      if EGLLib<>0 then
        FreeLibrary(EGLLib);

      eglGetError:=nil;
      eglGetDisplay:=nil;
      eglInitialize:=nil;
      eglTerminate:=nil;
      eglQueryString:=nil;
      eglGetConfigs:=nil;
      eglChooseConfig:=nil;
      eglGetConfigAttrib:=nil;
      eglCreateWindowSurface:=nil;
      eglCreatePbufferSurface:=nil;
      eglCreatePixmapSurface:=nil;
      eglDestroySurface:=nil;
      eglQuerySurface:=nil;
      eglBindAPI:=nil;
      eglQueryAPI:=nil;
      eglWaitClient:=nil;
      eglReleaseThread:=nil;
      eglCreatePbufferFromClientBuffer:=nil;
      eglSurfaceAttrib:=nil;
      eglBindTexImage:=nil;
      eglReleaseTexImage:=nil;
      eglSwapInterval:=nil;
      eglCreateContext:=nil;
      eglDestroyContext:=nil;
      eglMakeCurrent:=nil;
      eglGetCurrentContext:=nil;
      eglGetCurrentSurface:=nil;
      eglGetCurrentDisplay:=nil;
      eglQueryContext:=nil;
      eglWaitGL:=nil;
      eglWaitNative:=nil;
      eglSwapBuffers:=nil;
      eglCopyBuffers:=nil;
      eglGetProcAddress:=nil;
    end;


  procedure LoadEGL(const Lib: string; const AltLibName: string = '');
    begin
      FreeEGL;
{$ifdef OpenGLES}
      EGLLib:=dynlibs.LoadLibrary(Lib);
      if (EGLLib=0) and (AltLibName <> '') then
        EGLLib:=dynlibs.LoadLibrary(AltLibName);
      if EGLLib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));
{$else}
      Exit;
{$endif}

      pointer(eglGetProcAddress):=GetProcAddress(EGLLib,'glGetProcAddress');

      pointer(eglGetError):=glGetProcAddress(EGLLib,'eglGetError');
      pointer(eglGetDisplay):=glGetProcAddress(EGLLib,'eglGetDisplay');
      pointer(eglInitialize):=glGetProcAddress(EGLLib,'eglInitialize');
      pointer(eglTerminate):=glGetProcAddress(EGLLib,'eglTerminate');
      pointer(eglQueryString):=glGetProcAddress(EGLLib,'eglQueryString');
      pointer(eglGetConfigs):=glGetProcAddress(EGLLib,'eglGetConfigs');
      pointer(eglChooseConfig):=glGetProcAddress(EGLLib,'eglChooseConfig');
      pointer(eglGetConfigAttrib):=glGetProcAddress(EGLLib,'eglGetConfigAttrib');
      pointer(eglCreateWindowSurface):=glGetProcAddress(EGLLib,'eglCreateWindowSurface');
      pointer(eglCreatePbufferSurface):=glGetProcAddress(EGLLib,'eglCreatePbufferSurface');
      pointer(eglCreatePixmapSurface):=glGetProcAddress(EGLLib,'eglCreatePixmapSurface');
      pointer(eglDestroySurface):=glGetProcAddress(EGLLib,'eglDestroySurface');
      pointer(eglQuerySurface):=glGetProcAddress(EGLLib,'eglQuerySurface');
      pointer(eglBindAPI):=glGetProcAddress(EGLLib,'eglBindAPI');
      pointer(eglQueryAPI):=glGetProcAddress(EGLLib,'eglQueryAPI');
      pointer(eglWaitClient):=glGetProcAddress(EGLLib,'eglWaitClient');
      pointer(eglReleaseThread):=glGetProcAddress(EGLLib,'eglReleaseThread');
      pointer(eglCreatePbufferFromClientBuffer):=glGetProcAddress(EGLLib,'eglCreatePbufferFromClientBuffer');
      pointer(eglSurfaceAttrib):=glGetProcAddress(EGLLib,'eglSurfaceAttrib');
      pointer(eglBindTexImage):=glGetProcAddress(EGLLib,'eglBindTexImage');
      pointer(eglReleaseTexImage):=glGetProcAddress(EGLLib,'eglReleaseTexImage');
      pointer(eglSwapInterval):=glGetProcAddress(EGLLib,'eglSwapInterval');
      pointer(eglCreateContext):=glGetProcAddress(EGLLib,'eglCreateContext');
      pointer(eglDestroyContext):=glGetProcAddress(EGLLib,'eglDestroyContext');
      pointer(eglMakeCurrent):=glGetProcAddress(EGLLib,'eglMakeCurrent');
      pointer(eglGetCurrentContext):=glGetProcAddress(EGLLib,'eglGetCurrentContext');
      pointer(eglGetCurrentSurface):=glGetProcAddress(EGLLib,'eglGetCurrentSurface');
      pointer(eglGetCurrentDisplay):=glGetProcAddress(EGLLib,'eglGetCurrentDisplay');
      pointer(eglQueryContext):=glGetProcAddress(EGLLib,'eglQueryContext');
      pointer(eglWaitGL):=glGetProcAddress(EGLLib,'eglWaitGL');
      pointer(eglWaitNative):=glGetProcAddress(EGLLib,'eglWaitNative');
      pointer(eglSwapBuffers):=glGetProcAddress(EGLLib,'eglSwapBuffers');
      pointer(eglCopyBuffers):=glGetProcAddress(EGLLib,'eglCopyBuffers');
    end;
{$endif EGL}

  var
    GLESv2Lib : tlibhandle;

  procedure FreeGLESv2;
    begin
      if GLESv2Lib<>0 then
        FreeLibrary(GLESv2Lib);

      glActiveTexture:=nil;
      glAttachShader:=nil;
      glBindAttribLocation:=nil;
      glBindBuffer:=nil;
      glBindFramebuffer:=nil;
      glBindRenderbuffer:=nil;
      glBindTexture:=nil;
      glBlendColor:=nil;
      glBlendEquation:=nil;
      glBlendEquationSeparate:=nil;
      glBlendFunc:=nil;
      glBlendFuncSeparate:=nil;
      glBufferData:=nil;
      glBufferSubData:=nil;
      glCheckFramebufferStatus:=nil;
      glClear:=nil;
      glClearColor:=nil;
      glClearDepthf:=nil;
      glClearStencil:=nil;
      glColorMask:=nil;
      glCompileShader:=nil;
      glCompressedTexImage2D:=nil;
      glCompressedTexSubImage2D:=nil;
      glCopyTexImage2D:=nil;
      glCopyTexSubImage2D:=nil;
      glCreateProgram:=nil;
      glCreateShader:=nil;
      glCullFace:=nil;
      glDeleteBuffers:=nil;
      glDeleteFramebuffers:=nil;
      glDeleteProgram:=nil;
      glDeleteRenderbuffers:=nil;
      glDeleteShader:=nil;
      glDeleteTextures:=nil;
      glDepthFunc:=nil;
      glDepthMask:=nil;
      glDepthRangef:=nil;
      glDetachShader:=nil;
      glDisable:=nil;
      glDisableVertexAttribArray:=nil;
      glDrawArrays:=nil;
      glDrawElements:=nil;
      glEnable:=nil;
      glEnableVertexAttribArray:=nil;
      glFinish:=nil;
      glFlush:=nil;
      glFramebufferRenderbuffer:=nil;
      glFramebufferTexture2D:=nil;
      glFrontFace:=nil;
      glGenBuffers:=nil;
      glGenerateMipmap:=nil;
      glGenFramebuffers:=nil;
      glGenRenderbuffers:=nil;
      glGenTextures:=nil;
      glGetActiveAttrib:=nil;
      glGetActiveUniform:=nil;
      glGetAttachedShaders:=nil;
      glGetAttribLocation:=nil;
      glGetBooleanv:=nil;
      glGetBufferParameteriv:=nil;
      glGetError:=nil;
      glGetFloatv:=nil;
      glGetFramebufferAttachmentParameteriv:=nil;
      glGetIntegerv:=nil;
      glGetProgramiv:=nil;
      glGetProgramInfoLog:=nil;
      glGetRenderbufferParameteriv:=nil;
      glGetShaderiv:=nil;
      glGetShaderInfoLog:=nil;
      glGetShaderPrecisionFormat:=nil;
      glGetShaderSource:=nil;
      glGetString:=nil;
      glGetTexParameterfv:=nil;
      glGetTexParameteriv:=nil;
      glGetUniformfv:=nil;
      glGetUniformiv:=nil;
      glGetUniformLocation:=nil;
      glGetVertexAttribfv:=nil;
      glGetVertexAttribiv:=nil;
      glGetVertexAttribPointerv:=nil;
      glHint:=nil;
      glIsBuffer:=nil;
      glIsEnabled:=nil;
      glIsFramebuffer:=nil;
      glIsProgram:=nil;
      glIsRenderbuffer:=nil;
      glIsShader:=nil;
      glIsTexture:=nil;
      glLineWidth:=nil;
      glLinkProgram:=nil;
      glPixelStorei:=nil;
      glPolygonOffset:=nil;
      glReadPixels:=nil;
      glReleaseShaderCompiler:=nil;
      glRenderbufferStorage:=nil;
      glSampleCoverage:=nil;
      glScissor:=nil;
      glShaderBinary:=nil;
      glShaderSource:=nil;
      glStencilFunc:=nil;
      glStencilFuncSeparate:=nil;
      glStencilMask:=nil;
      glStencilMaskSeparate:=nil;
      glStencilOp:=nil;
      glStencilOpSeparate:=nil;
      glTexImage2D:=nil;
      glTexParameterf:=nil;
      glTexParameterfv:=nil;
      glTexParameteri:=nil;
      glTexParameteriv:=nil;
      glTexSubImage2D:=nil;
      glUniform1f:=nil;
      glUniform1fv:=nil;
      glUniform1i:=nil;
      glUniform1iv:=nil;
      glUniform2f:=nil;
      glUniform2fv:=nil;
      glUniform2i:=nil;
      glUniform2iv:=nil;
      glUniform3f:=nil;
      glUniform3fv:=nil;
      glUniform3i:=nil;
      glUniform3iv:=nil;
      glUniform4f:=nil;
      glUniform4fv:=nil;
      glUniform4i:=nil;
      glUniform4iv:=nil;
      glUniformMatrix2fv:=nil;
      glUniformMatrix3fv:=nil;
      glUniformMatrix4fv:=nil;
      glUseProgram:=nil;
      glValidateProgram:=nil;
      glVertexAttrib1f:=nil;
      glVertexAttrib1fv:=nil;
      glVertexAttrib2f:=nil;
      glVertexAttrib2fv:=nil;
      glVertexAttrib3f:=nil;
      glVertexAttrib3fv:=nil;
      glVertexAttrib4f:=nil;
      glVertexAttrib4fv:=nil;
      glVertexAttribPointer:=nil;
      glViewport:=nil;
      glEGLImageTargetTexture2DOES:=nil;
      glEGLImageTargetRenderbufferStorageOES:=nil;
      glGetProgramBinaryOES:=nil;
      glProgramBinaryOES:=nil;
      glMapBufferOES:=nil;
      glUnmapBufferOES:=nil;
      glGetBufferPointervOES:=nil;
      glTexImage3DOES:=nil;
      glTexSubImage3DOES:=nil;
      glCopyTexSubImage3DOES:=nil;
      glCompressedTexImage3DOES:=nil;
      glCompressedTexSubImage3DOES:=nil;
      glFramebufferTexture3DOES:=nil;
      glGetPerfMonitorGroupsAMD:=nil;
      glGetPerfMonitorCountersAMD:=nil;
      glGetPerfMonitorGroupStringAMD:=nil;
      glGetPerfMonitorCounterStringAMD:=nil;
      glGetPerfMonitorCounterInfoAMD:=nil;
      glGenPerfMonitorsAMD:=nil;
      glDeletePerfMonitorsAMD:=nil;
      glSelectPerfMonitorCountersAMD:=nil;
      glBeginPerfMonitorAMD:=nil;
      glEndPerfMonitorAMD:=nil;
      glGetPerfMonitorCounterDataAMD:=nil;
    end;


  procedure LoadGLESv2(const Lib: string; const AltLibName: string = '');
    begin
      FreeGLESv2;
{$ifdef OpenGLES}
      GLESv2Lib:=dynlibs.LoadLibrary(Lib);
      if (GLESv2Lib=0) and (AltLibName <> '') then
        GLESv2Lib:=dynlibs.LoadLibrary(AltLibName);
      if GLESv2Lib=0 then
        raise Exception.Create(format('Could not load library: %s',[Lib]));
{$else}
      Exit;
{$endif}

      pointer(glActiveTexture):=glGetProcAddress(GLESv2Lib,'glActiveTexture');
      pointer(glAttachShader):=glGetProcAddress(GLESv2Lib,'glAttachShader');
      pointer(glBindAttribLocation):=glGetProcAddress(GLESv2Lib,'glBindAttribLocation');
      pointer(glBindBuffer):=glGetProcAddress(GLESv2Lib,'glBindBuffer');
      pointer(glBindFramebuffer):=glGetProcAddress(GLESv2Lib,'glBindFramebuffer');
      pointer(glBindRenderbuffer):=glGetProcAddress(GLESv2Lib,'glBindRenderbuffer');
      pointer(glBindTexture):=glGetProcAddress(GLESv2Lib,'glBindTexture');
      pointer(glBlendColor):=glGetProcAddress(GLESv2Lib,'glBlendColor');
      pointer(glBlendEquation):=glGetProcAddress(GLESv2Lib,'glBlendEquation');
      pointer(glBlendEquationSeparate):=glGetProcAddress(GLESv2Lib,'glBlendEquationSeparate');
      pointer(glBlendFunc):=glGetProcAddress(GLESv2Lib,'glBlendFunc');
      pointer(glBlendFuncSeparate):=glGetProcAddress(GLESv2Lib,'glBlendFuncSeparate');
      pointer(glBufferData):=glGetProcAddress(GLESv2Lib,'glBufferData');
      pointer(glBufferSubData):=glGetProcAddress(GLESv2Lib,'glBufferSubData');
      pointer(glCheckFramebufferStatus):=glGetProcAddress(GLESv2Lib,'glCheckFramebufferStatus');
      pointer(glClear):=glGetProcAddress(GLESv2Lib,'glClear');
      pointer(glClearColor):=glGetProcAddress(GLESv2Lib,'glClearColor');
      pointer(glClearDepthf):=glGetProcAddress(GLESv2Lib,'glClearDepthf');
      pointer(glClearStencil):=glGetProcAddress(GLESv2Lib,'glClearStencil');
      pointer(glColorMask):=glGetProcAddress(GLESv2Lib,'glColorMask');
      pointer(glCompileShader):=glGetProcAddress(GLESv2Lib,'glCompileShader');
      pointer(glCompressedTexImage2D):=glGetProcAddress(GLESv2Lib,'glCompressedTexImage2D');
      pointer(glCompressedTexSubImage2D):=glGetProcAddress(GLESv2Lib,'glCompressedTexSubImage2D');
      pointer(glCopyTexImage2D):=glGetProcAddress(GLESv2Lib,'glCopyTexImage2D');
      pointer(glCopyTexSubImage2D):=glGetProcAddress(GLESv2Lib,'glCopyTexSubImage2D');
      pointer(glCreateProgram):=glGetProcAddress(GLESv2Lib,'glCreateProgram');
      pointer(glCreateShader):=glGetProcAddress(GLESv2Lib,'glCreateShader');
      pointer(glCullFace):=glGetProcAddress(GLESv2Lib,'glCullFace');
      pointer(glDeleteBuffers):=glGetProcAddress(GLESv2Lib,'glDeleteBuffers');
      pointer(glDeleteFramebuffers):=glGetProcAddress(GLESv2Lib,'glDeleteFramebuffers');
      pointer(glDeleteProgram):=glGetProcAddress(GLESv2Lib,'glDeleteProgram');
      pointer(glDeleteRenderbuffers):=glGetProcAddress(GLESv2Lib,'glDeleteRenderbuffers');
      pointer(glDeleteShader):=glGetProcAddress(GLESv2Lib,'glDeleteShader');
      pointer(glDeleteTextures):=glGetProcAddress(GLESv2Lib,'glDeleteTextures');
      pointer(glDepthFunc):=glGetProcAddress(GLESv2Lib,'glDepthFunc');
      pointer(glDepthMask):=glGetProcAddress(GLESv2Lib,'glDepthMask');
      pointer(glDepthRangef):=glGetProcAddress(GLESv2Lib,'glDepthRangef');
      pointer(glDetachShader):=glGetProcAddress(GLESv2Lib,'glDetachShader');
      pointer(glDisable):=glGetProcAddress(GLESv2Lib,'glDisable');
      pointer(glDisableVertexAttribArray):=glGetProcAddress(GLESv2Lib,'glDisableVertexAttribArray');
      pointer(glDrawArrays):=glGetProcAddress(GLESv2Lib,'glDrawArrays');
      pointer(glDrawElements):=glGetProcAddress(GLESv2Lib,'glDrawElements');
      pointer(glEnable):=glGetProcAddress(GLESv2Lib,'glEnable');
      pointer(glEnableVertexAttribArray):=glGetProcAddress(GLESv2Lib,'glEnableVertexAttribArray');
      pointer(glFinish):=glGetProcAddress(GLESv2Lib,'glFinish');
      pointer(glFlush):=glGetProcAddress(GLESv2Lib,'glFlush');
      pointer(glFramebufferRenderbuffer):=glGetProcAddress(GLESv2Lib,'glFramebufferRenderbuffer');
      pointer(glFramebufferTexture2D):=glGetProcAddress(GLESv2Lib,'glFramebufferTexture2D');
      pointer(glFrontFace):=glGetProcAddress(GLESv2Lib,'glFrontFace');
      pointer(glGenBuffers):=glGetProcAddress(GLESv2Lib,'glGenBuffers');
      pointer(glGenerateMipmap):=glGetProcAddress(GLESv2Lib,'glGenerateMipmap');
      pointer(glGenFramebuffers):=glGetProcAddress(GLESv2Lib,'glGenFramebuffers');
      pointer(glGenRenderbuffers):=glGetProcAddress(GLESv2Lib,'glGenRenderbuffers');
      pointer(glGenTextures):=glGetProcAddress(GLESv2Lib,'glGenTextures');
      pointer(glGetActiveAttrib):=glGetProcAddress(GLESv2Lib,'glGetActiveAttrib');
      pointer(glGetActiveUniform):=glGetProcAddress(GLESv2Lib,'glGetActiveUniform');
      pointer(glGetAttachedShaders):=glGetProcAddress(GLESv2Lib,'glGetAttachedShaders');
      pointer(glGetAttribLocation):=glGetProcAddress(GLESv2Lib,'glGetAttribLocation');
      pointer(glGetBooleanv):=glGetProcAddress(GLESv2Lib,'glGetBooleanv');
      pointer(glGetBufferParameteriv):=glGetProcAddress(GLESv2Lib,'glGetBufferParameteriv');
      pointer(glGetError):=glGetProcAddress(GLESv2Lib,'glGetError');
      pointer(glGetFloatv):=glGetProcAddress(GLESv2Lib,'glGetFloatv');
      pointer(glGetFramebufferAttachmentParameteriv):=glGetProcAddress(GLESv2Lib,'glGetFramebufferAttachmentParameteriv');
      pointer(glGetIntegerv):=glGetProcAddress(GLESv2Lib,'glGetIntegerv');
      pointer(glGetProgramiv):=glGetProcAddress(GLESv2Lib,'glGetProgramiv');
      pointer(glGetProgramInfoLog):=glGetProcAddress(GLESv2Lib,'glGetProgramInfoLog');
      pointer(glGetRenderbufferParameteriv):=glGetProcAddress(GLESv2Lib,'glGetRenderbufferParameteriv');
      pointer(glGetShaderiv):=glGetProcAddress(GLESv2Lib,'glGetShaderiv');
      pointer(glGetShaderInfoLog):=glGetProcAddress(GLESv2Lib,'glGetShaderInfoLog');
      pointer(glGetShaderPrecisionFormat):=glGetProcAddress(GLESv2Lib,'glGetShaderPrecisionFormat');
      pointer(glGetShaderSource):=glGetProcAddress(GLESv2Lib,'glGetShaderSource');
      pointer(glGetString):=glGetProcAddress(GLESv2Lib,'glGetString');
      pointer(glGetTexParameterfv):=glGetProcAddress(GLESv2Lib,'glGetTexParameterfv');
      pointer(glGetTexParameteriv):=glGetProcAddress(GLESv2Lib,'glGetTexParameteriv');
      pointer(glGetUniformfv):=glGetProcAddress(GLESv2Lib,'glGetUniformfv');
      pointer(glGetUniformiv):=glGetProcAddress(GLESv2Lib,'glGetUniformiv');
      pointer(glGetUniformLocation):=glGetProcAddress(GLESv2Lib,'glGetUniformLocation');
      pointer(glGetVertexAttribfv):=glGetProcAddress(GLESv2Lib,'glGetVertexAttribfv');
      pointer(glGetVertexAttribiv):=glGetProcAddress(GLESv2Lib,'glGetVertexAttribiv');
      pointer(glGetVertexAttribPointerv):=glGetProcAddress(GLESv2Lib,'glGetVertexAttribPointerv');
      pointer(glHint):=glGetProcAddress(GLESv2Lib,'glHint');
      pointer(glIsBuffer):=glGetProcAddress(GLESv2Lib,'glIsBuffer');
      pointer(glIsEnabled):=glGetProcAddress(GLESv2Lib,'glIsEnabled');
      pointer(glIsFramebuffer):=glGetProcAddress(GLESv2Lib,'glIsFramebuffer');
      pointer(glIsProgram):=glGetProcAddress(GLESv2Lib,'glIsProgram');
      pointer(glIsRenderbuffer):=glGetProcAddress(GLESv2Lib,'glIsRenderbuffer');
      pointer(glIsShader):=glGetProcAddress(GLESv2Lib,'glIsShader');
      pointer(glIsTexture):=glGetProcAddress(GLESv2Lib,'glIsTexture');
      pointer(glLineWidth):=glGetProcAddress(GLESv2Lib,'glLineWidth');
      pointer(glLinkProgram):=glGetProcAddress(GLESv2Lib,'glLinkProgram');
      pointer(glPixelStorei):=glGetProcAddress(GLESv2Lib,'glPixelStorei');
      pointer(glPolygonOffset):=glGetProcAddress(GLESv2Lib,'glPolygonOffset');
      pointer(glReadPixels):=glGetProcAddress(GLESv2Lib,'glReadPixels');
      pointer(glReleaseShaderCompiler):=glGetProcAddress(GLESv2Lib,'glReleaseShaderCompiler');
      pointer(glRenderbufferStorage):=glGetProcAddress(GLESv2Lib,'glRenderbufferStorage');
      pointer(glSampleCoverage):=glGetProcAddress(GLESv2Lib,'glSampleCoverage');
      pointer(glScissor):=glGetProcAddress(GLESv2Lib,'glScissor');
      pointer(glShaderBinary):=glGetProcAddress(GLESv2Lib,'glShaderBinary');
      pointer(glShaderSource):=glGetProcAddress(GLESv2Lib,'glShaderSource');
      pointer(glStencilFunc):=glGetProcAddress(GLESv2Lib,'glStencilFunc');
      pointer(glStencilFuncSeparate):=glGetProcAddress(GLESv2Lib,'glStencilFuncSeparate');
      pointer(glStencilMask):=glGetProcAddress(GLESv2Lib,'glStencilMask');
      pointer(glStencilMaskSeparate):=glGetProcAddress(GLESv2Lib,'glStencilMaskSeparate');
      pointer(glStencilOp):=glGetProcAddress(GLESv2Lib,'glStencilOp');
      pointer(glStencilOpSeparate):=glGetProcAddress(GLESv2Lib,'glStencilOpSeparate');
      pointer(glTexImage2D):=glGetProcAddress(GLESv2Lib,'glTexImage2D');
      pointer(glTexParameterf):=glGetProcAddress(GLESv2Lib,'glTexParameterf');
      pointer(glTexParameterfv):=glGetProcAddress(GLESv2Lib,'glTexParameterfv');
      pointer(glTexParameteri):=glGetProcAddress(GLESv2Lib,'glTexParameteri');
      pointer(glTexParameteriv):=glGetProcAddress(GLESv2Lib,'glTexParameteriv');
      pointer(glTexSubImage2D):=glGetProcAddress(GLESv2Lib,'glTexSubImage2D');
      pointer(glUniform1f):=glGetProcAddress(GLESv2Lib,'glUniform1f');
      pointer(glUniform1fv):=glGetProcAddress(GLESv2Lib,'glUniform1fv');
      pointer(glUniform1i):=glGetProcAddress(GLESv2Lib,'glUniform1i');
      pointer(glUniform1iv):=glGetProcAddress(GLESv2Lib,'glUniform1iv');
      pointer(glUniform2f):=glGetProcAddress(GLESv2Lib,'glUniform2f');
      pointer(glUniform2fv):=glGetProcAddress(GLESv2Lib,'glUniform2fv');
      pointer(glUniform2i):=glGetProcAddress(GLESv2Lib,'glUniform2i');
      pointer(glUniform2iv):=glGetProcAddress(GLESv2Lib,'glUniform2iv');
      pointer(glUniform3f):=glGetProcAddress(GLESv2Lib,'glUniform3f');
      pointer(glUniform3fv):=glGetProcAddress(GLESv2Lib,'glUniform3fv');
      pointer(glUniform3i):=glGetProcAddress(GLESv2Lib,'glUniform3i');
      pointer(glUniform3iv):=glGetProcAddress(GLESv2Lib,'glUniform3iv');
      pointer(glUniform4f):=glGetProcAddress(GLESv2Lib,'glUniform4f');
      pointer(glUniform4fv):=glGetProcAddress(GLESv2Lib,'glUniform4fv');
      pointer(glUniform4i):=glGetProcAddress(GLESv2Lib,'glUniform4i');
      pointer(glUniform4iv):=glGetProcAddress(GLESv2Lib,'glUniform4iv');
      pointer(glUniformMatrix2fv):=glGetProcAddress(GLESv2Lib,'glUniformMatrix2fv');
      pointer(glUniformMatrix3fv):=glGetProcAddress(GLESv2Lib,'glUniformMatrix3fv');
      pointer(glUniformMatrix4fv):=glGetProcAddress(GLESv2Lib,'glUniformMatrix4fv');
      pointer(glUseProgram):=glGetProcAddress(GLESv2Lib,'glUseProgram');
      pointer(glValidateProgram):=glGetProcAddress(GLESv2Lib,'glValidateProgram');
      pointer(glVertexAttrib1f):=glGetProcAddress(GLESv2Lib,'glVertexAttrib1f');
      pointer(glVertexAttrib1fv):=glGetProcAddress(GLESv2Lib,'glVertexAttrib1fv');
      pointer(glVertexAttrib2f):=glGetProcAddress(GLESv2Lib,'glVertexAttrib2f');
      pointer(glVertexAttrib2fv):=glGetProcAddress(GLESv2Lib,'glVertexAttrib2fv');
      pointer(glVertexAttrib3f):=glGetProcAddress(GLESv2Lib,'glVertexAttrib3f');
      pointer(glVertexAttrib3fv):=glGetProcAddress(GLESv2Lib,'glVertexAttrib3fv');
      pointer(glVertexAttrib4f):=glGetProcAddress(GLESv2Lib,'glVertexAttrib4f');
      pointer(glVertexAttrib4fv):=glGetProcAddress(GLESv2Lib,'glVertexAttrib4fv');
      pointer(glVertexAttribPointer):=glGetProcAddress(GLESv2Lib,'glVertexAttribPointer');
      pointer(glViewport):=glGetProcAddress(GLESv2Lib,'glViewport');
      pointer(glEGLImageTargetTexture2DOES):=glGetProcAddress(GLESv2Lib,'glEGLImageTargetTexture2DOES');
      pointer(glEGLImageTargetRenderbufferStorageOES):=glGetProcAddress(GLESv2Lib,'glEGLImageTargetRenderbufferStorageOES');
      pointer(glGetProgramBinaryOES):=glGetProcAddress(GLESv2Lib,'glGetProgramBinaryOES');
      pointer(glProgramBinaryOES):=glGetProcAddress(GLESv2Lib,'glProgramBinaryOES');
      pointer(glMapBufferOES):=glGetProcAddress(GLESv2Lib,'glMapBufferOES');
      pointer(glUnmapBufferOES):=glGetProcAddress(GLESv2Lib,'glUnmapBufferOES');
      pointer(glGetBufferPointervOES):=glGetProcAddress(GLESv2Lib,'glGetBufferPointervOES');
      pointer(glTexImage3DOES):=glGetProcAddress(GLESv2Lib,'glTexImage3DOES');
      pointer(glTexSubImage3DOES):=glGetProcAddress(GLESv2Lib,'glTexSubImage3DOES');
      pointer(glCopyTexSubImage3DOES):=glGetProcAddress(GLESv2Lib,'glCopyTexSubImage3DOES');
      pointer(glCompressedTexImage3DOES):=glGetProcAddress(GLESv2Lib,'glCompressedTexImage3DOES');
      pointer(glCompressedTexSubImage3DOES):=glGetProcAddress(GLESv2Lib,'glCompressedTexSubImage3DOES');
      pointer(glFramebufferTexture3DOES):=glGetProcAddress(GLESv2Lib,'glFramebufferTexture3DOES');
      pointer(glGetPerfMonitorGroupsAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorGroupsAMD');
      pointer(glGetPerfMonitorCountersAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorCountersAMD');
      pointer(glGetPerfMonitorGroupStringAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorGroupStringAMD');
      pointer(glGetPerfMonitorCounterStringAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorCounterStringAMD');
      pointer(glGetPerfMonitorCounterInfoAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorCounterInfoAMD');
      pointer(glGenPerfMonitorsAMD):=glGetProcAddress(GLESv2Lib,'glGenPerfMonitorsAMD');
      pointer(glDeletePerfMonitorsAMD):=glGetProcAddress(GLESv2Lib,'glDeletePerfMonitorsAMD');
      pointer(glSelectPerfMonitorCountersAMD):=glGetProcAddress(GLESv2Lib,'glSelectPerfMonitorCountersAMD');
      pointer(glBeginPerfMonitorAMD):=glGetProcAddress(GLESv2Lib,'glBeginPerfMonitorAMD');
      pointer(glEndPerfMonitorAMD):=glGetProcAddress(GLESv2Lib,'glEndPerfMonitorAMD');
      pointer(glGetPerfMonitorCounterDataAMD):=glGetProcAddress(GLESv2Lib,'glGetPerfMonitorCounterDataAMD');
    end;

procedure GLES20Initialization;
begin
  {$ifdef EGL}
  LoadEGL(
    {$ifdef windows} 'libEGL.dll'
    { First try to access libEGL.so.1 (from libegl1-mesa package on Debian).
      The name libEGL.so is only available in -dev package. }
    {$else} 'libEGL.so.1', 'libEGL.so'
    {$endif});
  {$endif}

  LoadGLESv2(
    {$ifdef darwin} '/System/Library/Frameworks/OpenGLES.framework/OpenGLES'
    {$else}
      {$ifdef windows} 'libGLESv2.dll'
      { First try to access libGLESv2.so.2 (from libgles2-mesa package on Debian).
        The name libGLESv2.so is only available in -dev package. }
      {$else} 'libGLESv2.so.2', 'libGLESv2.so'
      {$endif}
    {$endif});
end;

initialization
  {$ifdef EGL}
  EGLLib:=0;
  {$endif}
  GLESv2Lib:=0;
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  GLES20Initialization;
  {$endif}
finalization
  FreeGLESv2;
{$ifdef EGL}
  FreeEGL;
{$endif}
end.
