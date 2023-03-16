{$ifdef CASTLE_DELPHI_PACKAGE}
  {$message fatal 'This unit should not be included in CGE Delphi package, which for now is only for Windows OpenGL.'}
{$endif}

{ OpenGL ES 3.x headers.

  A modified version of OpenGL ES 2.0 headers by Trung Le (kagamma), which adds
  support for OpenGL ES 3.x APIs.
  Currently it supports OpenGL ES 2.0 and OpenGL ES 3.0 APIs.
}

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
 ** GLESv3 part:
 **
 ** This document is licensed under the SGI Free Software B License Version
 ** 2.0. For details, see http://oss.sgi.com/projects/FreeB/
 **}

unit CastleGLES;

{$i castleconf.inc}

{$ifndef FPC}
  {$message fatal 'This unit is only for FPC, not Delphi.'}
{$endif}

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

{$ifdef EGL}
type
  PEGLConfig  = ^EGLConfig;
  PEGLint  = ^EGLint;
     EGLint = longint;

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
  { This is a generic function pointer type, whose name indicates it must
   * be cast to the proper type *and calling convention* before use.
    }

  type

     __eglMustCastToProperFunctionPointerType = procedure (_para1:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { Now, define eglGetProcAddress using the generic function ptr. type  }
(* Const before type ignored *)

  var
    eglGetProcAddress : function(procname:PAnsiChar):__eglMustCastToProperFunctionPointerType;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
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
  PGLchar = PAnsiChar;
  PPGLchar = PPAnsiChar;

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
     TGLintptr = GLintptr;

     GLsizeiptr = ptrint;
     TGLsizeiptr = GLsizeiptr;
  { OpenGL ES core versions  }
     GLuint64 = QWord;
     TGLuint64 = GLuint64;
     PGLuint64 = ^GLuint64;

     GLint64 = Int64;
     TGLint64 = GLint64;
     PGLint64 = ^GLint64;

     GLsync = Pointer;
     TGLsync = GLsync;

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
    glBindAttribLocation : procedure(_program:GLuint; index:GLuint; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
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
      _type:pGLenum; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniform : procedure(_program:GLuint; index:GLuint; bufsize:GLsizei; length:pGLsizei; size:pGLint;
      _type:pGLenum; name:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetAttachedShaders : procedure(_program:GLuint; maxcount:GLsizei; count:pGLsizei; shaders:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetAttribLocation : function(_program:GLuint; name:PAnsiChar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBooleanv : procedure(pname:GLenum; params:pGLboolean);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetError : function:GLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFloatv : procedure(pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFramebufferAttachmentParameteriv : procedure(target:GLenum; attachment:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetIntegerv : procedure(pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramiv : procedure(_program:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramInfoLog : procedure(_program:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetRenderbufferParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderiv : procedure(shader:GLuint; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderInfoLog : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; infolog:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderPrecisionFormat : procedure(shadertype:GLenum; precisiontype:GLenum; range:pGLint; precision:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetShaderSource : procedure(shader:GLuint; bufsize:GLsizei; length:pGLsizei; source:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetString : function(name:GLenum):PGLubyte;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameterfv : procedure(target:GLenum; pname:GLenum; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTexParameteriv : procedure(target:GLenum; pname:GLenum; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformfv : procedure(_program:GLuint; location:GLint; params:pGLfloat);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformiv : procedure(_program:GLuint; location:GLint; params:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
(* Const before type ignored *)
    glGetUniformLocation : function(_program:GLuint; name:PAnsiChar):longint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
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
    glShaderSource : procedure(shader:GLuint; count:GLsizei; _string:PPGLchar; length:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
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

    { OpenGL ES 3.0 APIs }
    glReadBuffer: procedure(Src: GLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawRangeElements: procedure(Mode: GLenum; Start, Endd: GLuint; Count: GLsizei; Kind: GLenum; Indices: Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexImage3D: procedure(target:TGLenum; level:TGLint; internalformat:TGLint; width:TGLsizei; height:TGLsizei;
              depth:TGLsizei; border:TGLint; format:TGLenum; _type:TGLenum; pixels:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexSubImage3D: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
              width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; _type:TGLenum;
              pixels:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyTexSubImage3D: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
              x:TGLint; y:TGLint; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCompressedTexImage3D: procedure(target:TGLenum; level:TGLint; internalformat:TGLenum; width:TGLsizei; height:TGLsizei;
              depth:TGLsizei; border:TGLint; imageSize:TGLsizei; data:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCompressedTexSubImage3D: procedure(target:TGLenum; level:TGLint; xoffset:TGLint; yoffset:TGLint; zoffset:TGLint;
              width:TGLsizei; height:TGLsizei; depth:TGLsizei; format:TGLenum; imageSize:TGLsizei;
              data:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenQueries: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteQueries: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsQuery: function(id:TGLuint): GLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBeginQuery: procedure(target:TGLenum; id:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEndQuery: procedure(target:TGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetQueryiv: procedure(target:TGLenum; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetQueryObjectuiv: procedure(id:TGLuint; pname:TGLenum; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUnmapBuffer: function(Target: GLuint): GLboolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferPointerv: procedure(target:TGLenum; pname:TGLenum; params:Ppointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawBuffers: procedure(n:TGLsizei; bufs:PGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix2x3fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix3x2fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix2x4fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix4x2fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix3x4fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformMatrix4x3fv: procedure(location:TGLint; count:TGLsizei; transpose:TGLboolean; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBlitFramebuffer: procedure(srcX0:TGLint; srcY0:TGLint; srcX1:TGLint; srcY1:TGLint; dstX0:TGLint;
              dstY0:TGLint; dstX1:TGLint; dstY1:TGLint; mask:TGLbitfield; filter:TGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glRenderbufferStorageMultisample: procedure(target:TGLenum; samples:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFramebufferTextureLayer: procedure(target:TGLenum; attachment:TGLenum; texture:TGLuint; level:TGLint; layer:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glMapBufferRange: function(Target: GLuint; Offset: GLintptr; Len: GLsizeiptr; Access: GLbitfield): Pointer;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFlushMappedBufferRange: procedure(target:TGLenum; offset:TGLintptr; length:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindVertexArray: procedure(A: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteVertexArrays: procedure(Count: GLuint; P: PGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenVertexArrays: procedure(Count: GLuint; P: PGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsVertexArray: function(arr:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetIntegeri_v: procedure(target:TGLenum; ind:TGLuint; data:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBeginTransformFeedback: procedure(Mode: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEndTransformFeedback: procedure();{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindBufferRange: procedure(target:TGLenum; ind:TGLuint; buffer:TGLuint; offset:TGLintptr; size:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindBufferBase: procedure(Target, Ind, Buffer: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTransformFeedbackVaryings: procedure(Prog: GLuint; Count: GLsizei; const Varyings: PPAnsiChar; BufferMode: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetTransformFeedbackVarying: procedure(prog:TGLuint; ind:TGLuint; bufSize:TGLsizei; length:PGLsizei; size:PGLsizei;
              _type:PGLenum; name:PGLchar); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribIPointer: procedure(ind:TGLuint; size:TGLint; _type:TGLenum; stride:TGLsizei; p:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribIiv: procedure(ind:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetVertexAttribIuiv: procedure(ind:TGLuint; pname:TGLenum; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribI4i: procedure(ind:TGLuint; x:TGLint; y:TGLint; z:TGLint; w:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribI4ui: procedure(ind:TGLuint; x:TGLuint; y:TGLuint; z:TGLuint; w:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribI4iv: procedure(ind:TGLuint; v:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribI4uiv: procedure(ind:TGLuint; v:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformuiv: procedure(prog:TGLuint; location:TGLint; params:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetFragDataLocation: function(prog:TGLuint; name:PGLchar):TGLint; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1ui: procedure(location:TGLint; v0:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2ui: procedure(location:TGLint; v0:TGLuint; v1:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3ui: procedure(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4ui: procedure(location:TGLint; v0:TGLuint; v1:TGLuint; v2:TGLuint; v3:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform1uiv: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform2uiv: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform3uiv: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniform4uiv: procedure(location:TGLint; count:TGLsizei; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearBufferiv: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearBufferuiv: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearBufferfv: procedure(buffer:TGLenum; drawbuffer:TGLint; value:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClearBufferfi: procedure(buffer:TGLenum; drawbuffer:TGLint; depth:TGLfloat; stencil:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetStringi: function(name:TGLenum; ind:TGLuint):PGLubyte; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glCopyBufferSubData: procedure(readTarget:TGLenum; writeTarget:TGLenum; readOffset:TGLintptr; writeOffset:TGLintptr; size:TGLsizeiptr); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformIndices: procedure(prog:TGLuint; uniformCount:TGLsizei; uniformNames:PPGLchar; uniformIndices:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniformsiv: procedure(prog:TGLuint; uniformCount:TGLsizei; uniformIndices:PGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetUniformBlockIndex: function(prog:TGLuint; uniformBlockName:PGLchar):TGLuint; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniformBlockiv: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetActiveUniformBlockName: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; bufSize:TGLsizei; length:PGLsizei; uniformBlockName:PGLchar); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glUniformBlockBinding: procedure(prog:TGLuint; uniformBlockIndex:TGLuint; uniformBlockBinding:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawArraysInstanced: procedure(Mode: GLuint; First, Count, InstanceCount: GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDrawElementsInstanced: procedure(Mode: GLuint; Count, Kind: GLsizei; Indices: PGLuint; InstanceCount: GLsizei);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glFenceSync: function(condition:TGLenum; flags:TGLbitfield):TGLsync; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsSync: function(sync:TGLsync):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteSync: procedure(sync:TGLsync); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glClientWaitSync: function(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64):TGLenum; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glWaitSync: procedure(sync:TGLsync; flags:TGLbitfield; timeout:TGLuint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetInteger64v: procedure(pname:TGLenum; data:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetSynciv: procedure(sync:TGLsync; pname:TGLenum; count:TGLsizei; length:PGLsizei; values:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetInteger64i_v: procedure(target:TGLenum; ind:TGLuint; data:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetBufferParameteri64v: procedure(target:TGLenum; pname:TGLenum; params:PGLint64); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenSamplers: procedure(count:TGLsizei; samplers:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteSamplers: procedure(count:TGLsizei; samplers:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsSampler: function(sampler:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindSampler: procedure(u:TGLuint; sampler:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSamplerParameteri: procedure(sampler:TGLuint; pname:TGLenum; param:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSamplerParameteriv: procedure(sampler:TGLuint; pname:TGLenum; param:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSamplerParameterf: procedure(sampler:TGLuint; pname:TGLenum; param:TGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSamplerParameterfv: procedure(sampler:TGLuint; pname:TGLenum; param:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetSamplerParameteriv: procedure(sampler:TGLuint; pname:TGLenum; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetSamplerParameterfv: procedure(sampler:TGLuint; pname:TGLenum; params:PGLfloat); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glVertexAttribDivisor: procedure(Ind, Divisor: GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBindTransformFeedback: procedure(target:TGLenum; id:TGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeleteTransformFeedbacks: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenTransformFeedbacks: procedure(n:TGLsizei; ids:PGLuint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glIsTransformFeedback: function(id:TGLuint):TGLboolean; {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glPauseTransformFeedback: procedure(); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glResumeTransformFeedback: procedure(); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetProgramBinary: procedure(prog:TGLuint; bufSize:TGLsizei; length:PGLsizei; binaryFormat:PGLenum; binary:pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glProgramBinary: procedure(prog:TGLuint; binaryFormat:TGLenum; binary:pointer; length:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glProgramParameteri: procedure(prog:TGLuint; pname:TGLenum; value:TGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glInvalidateFramebuffer: procedure(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glInvalidateSubFramebuffer: procedure(target:TGLenum; numAttachments:TGLsizei; attachments:PGLenum; x:TGLint; y:TGLint;
              width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexStorage2D: procedure(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glTexStorage3D: procedure(target:TGLenum; levels:TGLsizei; internalformat:TGLenum; width:TGLsizei; height:TGLsizei;
              depth:TGLsizei); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetInternalformativ: procedure(target:TGLenum; internalformat:TGLenum; pname:TGLenum; count:TGLsizei; params:PGLint); {$ifdef windows}stdcall;{$else}cdecl;{$endif}

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

  { OpenGL ES 3.0 constants }
  const
    GL_READ_BUFFER = $0C02;
    GL_UNPACK_ROW_LENGTH = $0CF2;
    GL_UNPACK_SKIP_ROWS = $0CF3;
    GL_UNPACK_SKIP_PIXELS = $0CF4;
    GL_PACK_ROW_LENGTH = $0D02;
    GL_PACK_SKIP_ROWS = $0D03;
    GL_PACK_SKIP_PIXELS = $0D04;
    GL_COLOR = $1800;
    GL_DEPTH = $1801;
    GL_STENCIL = $1802;
    GL_RED = $1903;
    GL_RGB8 = $8051;
    GL_RGBA8 = $8058;
    GL_RGB10_A2 = $8059;
    GL_TEXTURE_BINDING_3D = $806A;
    GL_UNPACK_SKIP_IMAGES = $806D;
    GL_UNPACK_IMAGE_HEIGHT = $806E;
    GL_TEXTURE_3D = $806F;
    GL_TEXTURE_WRAP_R = $8072;
    GL_MAX_3D_TEXTURE_SIZE = $8073;
    GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
    GL_MAX_ELEMENTS_VERTICES = $80E8;
    GL_MAX_ELEMENTS_INDICES = $80E9;
    GL_TEXTURE_MIN_LOD = $813A;
    GL_TEXTURE_MAX_LOD = $813B;
    GL_TEXTURE_BASE_LEVEL = $813C;
    GL_TEXTURE_MAX_LEVEL = $813D;
    GL_MIN = $8007;
    GL_MAX = $8008;
    GL_DEPTH_COMPONENT24 = $81A6;
    GL_MAX_TEXTURE_LOD_BIAS = $84FD;
    GL_TEXTURE_COMPARE_MODE = $884C;
    GL_TEXTURE_COMPARE_FUNC = $884D;
    GL_CURRENT_QUERY = $8865;
    GL_QUERY_RESULT = $8866;
    GL_QUERY_RESULT_AVAILABLE = $8867;
    GL_BUFFER_MAPPED = $88BC;
    GL_BUFFER_MAP_POINTER = $88BD;
    GL_STREAM_READ = $88E1;
    GL_STREAM_COPY = $88E2;
    GL_STATIC_READ = $88E5;
    GL_STATIC_COPY = $88E6;
    GL_DYNAMIC_READ = $88E9;
    GL_DYNAMIC_COPY = $88EA;
    GL_MAX_DRAW_BUFFERS = $8824;
    GL_DRAW_BUFFER0 = $8825;
    GL_DRAW_BUFFER1 = $8826;
    GL_DRAW_BUFFER2 = $8827;
    GL_DRAW_BUFFER3 = $8828;
    GL_DRAW_BUFFER4 = $8829;
    GL_DRAW_BUFFER5 = $882A;
    GL_DRAW_BUFFER6 = $882B;
    GL_DRAW_BUFFER7 = $882C;
    GL_DRAW_BUFFER8 = $882D;
    GL_DRAW_BUFFER9 = $882E;
    GL_DRAW_BUFFER10 = $882F;
    GL_DRAW_BUFFER11 = $8830;
    GL_DRAW_BUFFER12 = $8831;
    GL_DRAW_BUFFER13 = $8832;
    GL_DRAW_BUFFER14 = $8833;
    GL_DRAW_BUFFER15 = $8834;
    GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
    GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
    GL_SAMPLER_3D = $8B5F;
    GL_SAMPLER_2D_SHADOW = $8B62;
    GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
    GL_PIXEL_PACK_BUFFER = $88EB;
    GL_PIXEL_UNPACK_BUFFER = $88EC;
    GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
    GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
    GL_FLOAT_MAT2x3 = $8B65;
    GL_FLOAT_MAT2x4 = $8B66;
    GL_FLOAT_MAT3x2 = $8B67;
    GL_FLOAT_MAT3x4 = $8B68;
    GL_FLOAT_MAT4x2 = $8B69;
    GL_FLOAT_MAT4x3 = $8B6A;
    GL_SRGB = $8C40;
    GL_SRGB8 = $8C41;
    GL_SRGB8_ALPHA8 = $8C43;
    GL_COMPARE_REF_TO_TEXTURE = $884E;
    GL_MAJOR_VERSION = $821B;
    GL_MINOR_VERSION = $821C;
    GL_NUM_EXTENSIONS = $821D;
    GL_RGBA32F = $8814;
    GL_RGB32F = $8815;
    GL_RGBA16F = $881A;
    GL_RGB16F = $881B;
    GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
    GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
    GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
    GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
    GL_MAX_VARYING_COMPONENTS = $8B4B;
    GL_TEXTURE_2D_ARRAY = $8C1A;
    GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
    GL_R11F_G11F_B10F = $8C3A;
    GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
    GL_RGB9_E5 = $8C3D;
    GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
    GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
    GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
    GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
    GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
    GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
    GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
    GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
    GL_RASTERIZER_DISCARD = $8C89;
    GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
    GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
    GL_INTERLEAVED_ATTRIBS = $8C8C;
    GL_SEPARATE_ATTRIBS = $8C8D;
    GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
    GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
    GL_RGBA32UI = $8D70;
    GL_RGB32UI = $8D71;
    GL_RGBA16UI = $8D76;
    GL_RGB16UI = $8D77;
    GL_RGBA8UI = $8D7C;
    GL_RGB8UI = $8D7D;
    GL_RGBA32I = $8D82;
    GL_RGB32I = $8D83;
    GL_RGBA16I = $8D88;
    GL_RGB16I = $8D89;
    GL_RGBA8I = $8D8E;
    GL_RGB8I = $8D8F;
    GL_RED_INTEGER = $8D94;
    GL_RGB_INTEGER = $8D98;
    GL_RGBA_INTEGER = $8D99;
    GL_SAMPLER_2D_ARRAY = $8DC1;
    GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
    GL_SAMPLER_CUBE_SHADOW = $8DC5;
    GL_UNSIGNED_INT_VEC2 = $8DC6;
    GL_UNSIGNED_INT_VEC3 = $8DC7;
    GL_UNSIGNED_INT_VEC4 = $8DC8;
    GL_INT_SAMPLER_2D = $8DCA;
    GL_INT_SAMPLER_3D = $8DCB;
    GL_INT_SAMPLER_CUBE = $8DCC;
    GL_INT_SAMPLER_2D_ARRAY = $8DCF;
    GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
    GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
    GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
    GL_BUFFER_ACCESS_FLAGS = $911F;
    GL_BUFFER_MAP_LENGTH = $9120;
    GL_BUFFER_MAP_OFFSET = $9121;
    GL_DEPTH_COMPONENT32F = $8CAC;
    GL_DEPTH32F_STENCIL8 = $8CAD;
    GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
    GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
    GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
    GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
    GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
    GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
    GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
    GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
    GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
    GL_FRAMEBUFFER_DEFAULT = $8218;
    GL_FRAMEBUFFER_UNDEFINED = $8219;
    GL_DEPTH_STENCIL_ATTACHMENT = $821A;
    GL_DEPTH_STENCIL = $84F9;
    GL_UNSIGNED_INT_24_8 = $84FA;
    GL_DEPTH24_STENCIL8 = $88F0;
    GL_UNSIGNED_NORMALIZED = $8C17;
    GL_DRAW_FRAMEBUFFER_BINDING = $8CA6;
    GL_READ_FRAMEBUFFER = $8CA8;
    GL_DRAW_FRAMEBUFFER = $8CA9;
    GL_READ_FRAMEBUFFER_BINDING = $8CAA;
    GL_RENDERBUFFER_SAMPLES = $8CAB;
    GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
    GL_MAX_COLOR_ATTACHMENTS = $8CDF;
    GL_COLOR_ATTACHMENT1 = $8CE1;
    GL_COLOR_ATTACHMENT2 = $8CE2;
    GL_COLOR_ATTACHMENT3 = $8CE3;
    GL_COLOR_ATTACHMENT4 = $8CE4;
    GL_COLOR_ATTACHMENT5 = $8CE5;
    GL_COLOR_ATTACHMENT6 = $8CE6;
    GL_COLOR_ATTACHMENT7 = $8CE7;
    GL_COLOR_ATTACHMENT8 = $8CE8;
    GL_COLOR_ATTACHMENT9 = $8CE9;
    GL_COLOR_ATTACHMENT10 = $8CEA;
    GL_COLOR_ATTACHMENT11 = $8CEB;
    GL_COLOR_ATTACHMENT12 = $8CEC;
    GL_COLOR_ATTACHMENT13 = $8CED;
    GL_COLOR_ATTACHMENT14 = $8CEE;
    GL_COLOR_ATTACHMENT15 = $8CEF;
    GL_COLOR_ATTACHMENT16 = $8CF0;
    GL_COLOR_ATTACHMENT17 = $8CF1;
    GL_COLOR_ATTACHMENT18 = $8CF2;
    GL_COLOR_ATTACHMENT19 = $8CF3;
    GL_COLOR_ATTACHMENT20 = $8CF4;
    GL_COLOR_ATTACHMENT21 = $8CF5;
    GL_COLOR_ATTACHMENT22 = $8CF6;
    GL_COLOR_ATTACHMENT23 = $8CF7;
    GL_COLOR_ATTACHMENT24 = $8CF8;
    GL_COLOR_ATTACHMENT25 = $8CF9;
    GL_COLOR_ATTACHMENT26 = $8CFA;
    GL_COLOR_ATTACHMENT27 = $8CFB;
    GL_COLOR_ATTACHMENT28 = $8CFC;
    GL_COLOR_ATTACHMENT29 = $8CFD;
    GL_COLOR_ATTACHMENT30 = $8CFE;
    GL_COLOR_ATTACHMENT31 = $8CFF;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
    GL_MAX_SAMPLES = $8D57;
    GL_HALF_FLOAT = $140B;
    GL_MAP_READ_BIT = $0001;
    GL_MAP_WRITE_BIT = $0002;
    GL_MAP_INVALIDATE_RANGE_BIT = $0004;
    GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
    GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
    GL_MAP_UNSYNCHRONIZED_BIT = $0020;
    GL_RG = $8227;
    GL_RG_INTEGER = $8228;
    GL_R8 = $8229;
    GL_RG8 = $822B;
    GL_R16F = $822D;
    GL_R32F = $822E;
    GL_RG16F = $822F;
    GL_RG32F = $8230;
    GL_R8I = $8231;
    GL_R8UI = $8232;
    GL_R16I = $8233;
    GL_R16UI = $8234;
    GL_R32I = $8235;
    GL_R32UI = $8236;
    GL_RG8I = $8237;
    GL_RG8UI = $8238;
    GL_RG16I = $8239;
    GL_RG16UI = $823A;
    GL_RG32I = $823B;
    GL_RG32UI = $823C;
    GL_VERTEX_ARRAY_BINDING = $85B5;
    GL_R8_SNORM = $8F94;
    GL_RG8_SNORM = $8F95;
    GL_RGB8_SNORM = $8F96;
    GL_RGBA8_SNORM = $8F97;
    GL_SIGNED_NORMALIZED = $8F9C;
    GL_PRIMITIVE_RESTART_FIXED_INDEX = $8D69;
    GL_COPY_READ_BUFFER = $8F36;
    GL_COPY_WRITE_BUFFER = $8F37;
    GL_COPY_READ_BUFFER_BINDING = $8F36;
    GL_COPY_WRITE_BUFFER_BINDING = $8F37;
    GL_UNIFORM_BUFFER = $8A11;
    GL_UNIFORM_BUFFER_BINDING = $8A28;
    GL_UNIFORM_BUFFER_START = $8A29;
    GL_UNIFORM_BUFFER_SIZE = $8A2A;
    GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
    GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
    GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
    GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
    GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
    GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
    GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
    GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
    GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
    GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
    GL_UNIFORM_TYPE = $8A37;
    GL_UNIFORM_SIZE = $8A38;
    GL_UNIFORM_NAME_LENGTH = $8A39;
    GL_UNIFORM_BLOCK_INDEX = $8A3A;
    GL_UNIFORM_OFFSET = $8A3B;
    GL_UNIFORM_ARRAY_STRIDE = $8A3C;
    GL_UNIFORM_MATRIX_STRIDE = $8A3D;
    GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
    GL_UNIFORM_BLOCK_BINDING = $8A3F;
    GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
    GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
    GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
    GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
    GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
    GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
    GL_INVALID_INDEX = $FFFFFFFF;
    GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
    GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
    GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
    GL_OBJECT_TYPE = $9112;
    GL_SYNC_CONDITION = $9113;
    GL_SYNC_STATUS = $9114;
    GL_SYNC_FLAGS = $9115;
    GL_SYNC_FENCE = $9116;
    GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
    GL_UNSIGNALED = $9118;
    GL_SIGNALED = $9119;
    GL_ALREADY_SIGNALED = $911A;
    GL_TIMEOUT_EXPIRED = $911B;
    GL_CONDITION_SATISFIED = $911C;
    GL_WAIT_FAILED = $911D;
    GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
    GL_TIMEOUT_IGNORED = $FFFFFFFFFFFFFFFF;
    GL_VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
    GL_ANY_SAMPLES_PASSED = $8C2F;
    GL_ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
    GL_SAMPLER_BINDING = $8919;
    GL_RGB10_A2UI = $906F;
    GL_TEXTURE_SWIZZLE_R = $8E42;
    GL_TEXTURE_SWIZZLE_G = $8E43;
    GL_TEXTURE_SWIZZLE_B = $8E44;
    GL_TEXTURE_SWIZZLE_A = $8E45;
    GL_GREEN = $1904;
    GL_BLUE = $1905;
    GL_INT_2_10_10_10_REV = $8D9F;
    GL_TRANSFORM_FEEDBACK = $8E22;
    GL_TRANSFORM_FEEDBACK_PAUSED = $8E23;
    GL_TRANSFORM_FEEDBACK_ACTIVE = $8E24;
    GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
    GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
    GL_PROGRAM_BINARY_LENGTH = $8741;
    GL_NUM_PROGRAM_BINARY_FORMATS = $87FE;
    GL_PROGRAM_BINARY_FORMATS = $87FF;
    GL_COMPRESSED_R11_EAC = $9270;
    GL_COMPRESSED_SIGNED_R11_EAC = $9271;
    GL_COMPRESSED_RG11_EAC = $9272;
    GL_COMPRESSED_SIGNED_RG11_EAC = $9273;
    GL_COMPRESSED_RGB8_ETC2 = $9274;
    GL_COMPRESSED_SRGB8_ETC2 = $9275;
    GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9276;
    GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
    GL_COMPRESSED_RGBA8_ETC2_EAC = $9278;
    GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = $9279;
    GL_TEXTURE_IMMUTABLE_FORMAT = $912F;
    GL_MAX_ELEMENT_INDEX = $8D6B;
    GL_NUM_SAMPLE_COUNTS = $9380;
    GL_TEXTURE_IMMUTABLE_LEVELS = $82DF;

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
    glGetPerfMonitorGroupStringAMD : procedure(group:GLuint; bufSize:GLsizei; length:pGLsizei; groupString:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterStringAMD : procedure(group:GLuint; counter:GLuint; bufSize:GLsizei; length:pGLsizei; counterString:PAnsiChar);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterInfoAMD : procedure(group:GLuint; counter:GLuint; pname:GLenum; data:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGenPerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDeletePerfMonitorsAMD : procedure(n:GLsizei; monitors:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glSelectPerfMonitorCountersAMD : procedure(monitor:GLuint; enable:GLboolean; group:GLuint; numCounters:GLint; countersList:pGLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glBeginPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glEndPerfMonitorAMD : procedure(monitor:GLuint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glGetPerfMonitorCounterDataAMD : procedure(monitor:GLuint; pname:GLenum; dataSize:GLsizei; data:pGLuint; bytesWritten:pGLint);{$ifdef windows}stdcall;{$else}cdecl;{$endif}

  { GL_EXT_texture_filter_anisotropic  }

  const
     GL_EXT_texture_filter_anisotropic = 1;
  { GL_EXT_texture_type_2_10_10_10_REV  }
     GL_EXT_texture_type_2_10_10_10_REV = 1;

  { GL_KHR_debug }

  const
    GL_DEBUG_OUTPUT = $92E0;
    GL_CONTEXT_FLAG_DEBUG_BIT = $00000002;
    GL_STACK_OVERFLOW = $0503;
    GL_STACK_UNDERFLOW = $0504;
    GL_DEBUG_OUTPUT_SYNCHRONOUS = $8242;
    GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH = $8243;
    GL_DEBUG_CALLBACK_FUNCTION = $8244;
    GL_DEBUG_CALLBACK_USER_PARAM = $8245;
    GL_DEBUG_SOURCE_API = $8246;
    GL_DEBUG_SOURCE_WINDOW_SYSTEM = $8247;
    GL_DEBUG_SOURCE_SHADER_COMPILER = $8248;
    GL_DEBUG_SOURCE_THIRD_PARTY = $8249;
    GL_DEBUG_SOURCE_APPLICATION = $824A;
    GL_DEBUG_SOURCE_OTHER = $824B;
    GL_DEBUG_TYPE_ERROR = $824C;
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR = $824D;
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR = $824E;
    GL_DEBUG_TYPE_PORTABILITY = $824F;
    GL_DEBUG_TYPE_PERFORMANCE = $8250;
    GL_DEBUG_TYPE_OTHER = $8251;
    GL_DEBUG_TYPE_MARKER = $8268;
    GL_DEBUG_TYPE_PUSH_GROUP = $8269;
    GL_DEBUG_TYPE_POP_GROUP = $826A;
    GL_DEBUG_SEVERITY_NOTIFICATION = $826B;
    GL_MAX_DEBUG_GROUP_STACK_DEPTH = $826C;
    GL_DEBUG_GROUP_STACK_DEPTH = $826D;
    GL_BUFFER = $82E0;
    GL_SHADER = $82E1;
    GL_PROGRAM = $82E2;
    GL_QUERY = $82E3;
    GL_PROGRAM_PIPELINE = $82E4;
    GL_SAMPLER = $82E6;
    GL_DISPLAY_LIST = $82E7;
    GL_MAX_LABEL_LENGTH = $82E8;
    GL_MAX_DEBUG_MESSAGE_LENGTH = $9143;
    GL_MAX_DEBUG_LOGGED_MESSAGES = $9144;
    GL_DEBUG_LOGGED_MESSAGES = $9145;
    GL_DEBUG_SEVERITY_HIGH = $9146;
    GL_DEBUG_SEVERITY_MEDIUM = $9147;
    GL_DEBUG_SEVERITY_LOW = $9148;

  type
    GLDEBUGPROC = procedure (source:GLenum; _type:GLenum; id:GLuint; severity:GLenum; length:GLsizei; message:PGLchar; userParam:Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}

  var
    glDebugMessageCallback:procedure (callback:GLDEBUGPROC; userParam:Pointer); {$ifdef windows}stdcall;{$else}cdecl;{$endif}
    glDebugMessageControl :procedure (source:GLenum; _type:GLenum; severity:GLenum; count:GLsizei; ids:PGLuint; enabled:GLboolean); {$ifdef windows}stdcall;{$else}cdecl;{$endif}

function glGetProcAddress(ahlib:tlibhandle;ProcName:PAnsiChar):pointer;

procedure GLESInitialization;

implementation

  function glGetProcAddress(ahlib:tlibhandle;ProcName:PAnsiChar):pointer;
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
    GLESLib : tlibhandle;

  procedure FreeGLES;
    begin
      if GLESLib<>0 then
        FreeLibrary(GLESLib);

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
      //
      glReadBuffer := nil;
      glDrawRangeElements := nil;
      glTexImage3D := nil;
      glTexSubImage3D := nil;
      glCopyTexSubImage3D := nil;
      glCompressedTexImage3D := nil;
      glCompressedTexSubImage3D := nil;
      glGenQueries := nil;
      glDeleteQueries := nil;
      glIsQuery := nil;
      glBeginQuery := nil;
      glEndQuery := nil;
      glGetQueryiv := nil;
      glGetQueryObjectuiv := nil;
      glUnmapBuffer := nil;
      glGetBufferPointerv := nil;
      glDrawBuffers := nil;
      glUniformMatrix2x3fv := nil;
      glUniformMatrix3x2fv := nil;
      glUniformMatrix2x4fv := nil;
      glUniformMatrix4x2fv := nil;
      glUniformMatrix3x4fv := nil;
      glUniformMatrix4x3fv := nil;
      glBlitFramebuffer := nil;
      glRenderbufferStorageMultisample := nil;
      glFramebufferTextureLayer := nil;
      glMapBufferRange := nil;
      glFlushMappedBufferRange := nil;
      glBindVertexArray := nil;
      glDeleteVertexArrays := nil;
      glGenVertexArrays := nil;
      glIsVertexArray := nil;
      glGetIntegeri_v := nil;
      glBeginTransformFeedback := nil;
      glEndTransformFeedback := nil;
      glBindBufferRange := nil;
      glBindBufferBase := nil;
      glTransformFeedbackVaryings := nil;
      glGetTransformFeedbackVarying := nil;
      glVertexAttribIPointer := nil;
      glGetVertexAttribIiv := nil;
      glGetVertexAttribIuiv := nil;
      glVertexAttribI4i := nil;
      glVertexAttribI4ui := nil;
      glVertexAttribI4iv := nil;
      glVertexAttribI4uiv := nil;
      glGetUniformuiv := nil;
      glGetFragDataLocation := nil;
      glUniform1ui := nil;
      glUniform2ui := nil;
      glUniform3ui := nil;
      glUniform4ui := nil;
      glUniform1uiv := nil;
      glUniform2uiv := nil;
      glUniform3uiv := nil;
      glUniform4uiv := nil;
      glClearBufferiv := nil;
      glClearBufferuiv := nil;
      glClearBufferfv := nil;
      glClearBufferfi := nil;
      glGetStringi := nil;
      glCopyBufferSubData := nil;
      glGetUniformIndices := nil;
      glGetActiveUniformsiv := nil;
      glGetUniformBlockIndex := nil;
      glGetActiveUniformBlockiv := nil;
      glGetActiveUniformBlockName := nil;
      glUniformBlockBinding := nil;
      glDrawArraysInstanced := nil;
      glDrawElementsInstanced := nil;
      glFenceSync := nil;
      glIsSync := nil;
      glDeleteSync := nil;
      glClientWaitSync := nil;
      glWaitSync := nil;
      glGetInteger64v := nil;
      glGetSynciv := nil;
      glGetInteger64i_v := nil;
      glGetBufferParameteri64v := nil;
      glGenSamplers := nil;
      glDeleteSamplers := nil;
      glIsSampler := nil;
      glBindSampler := nil;
      glSamplerParameteri := nil;
      glSamplerParameteriv := nil;
      glSamplerParameterf := nil;
      glSamplerParameterfv := nil;
      glGetSamplerParameteriv := nil;
      glGetSamplerParameterfv := nil;
      glVertexAttribDivisor := nil;
      glBindTransformFeedback := nil;
      glDeleteTransformFeedbacks := nil;
      glGenTransformFeedbacks := nil;
      glIsTransformFeedback := nil;
      glPauseTransformFeedback := nil;
      glResumeTransformFeedback := nil;
      glGetProgramBinary := nil;
      glProgramBinary := nil;
      glProgramParameteri := nil;
      glInvalidateFramebuffer := nil;
      glInvalidateSubFramebuffer := nil;
      glTexStorage2D := nil;
      glTexStorage3D := nil;
      glGetInternalformativ := nil;
    end;


  procedure LoadGLES(const Lib: string; const AltLibName: string = '');
    begin
      FreeGLES;
{$ifdef OpenGLES}
      GLESLib:=dynlibs.LoadLibrary(Lib);
      if (GLESLib=0) and (AltLibName <> '') then
        GLESLib:=dynlibs.LoadLibrary(AltLibName);
      if GLESLib=0 then
        raise Exception.Create(format('Could not load library: %s',[Lib]));
{$else}
      Exit;
{$endif}
      { OpenGL ES 2.0 APIs }
      pointer(glActiveTexture):=glGetProcAddress(GLESLib,'glActiveTexture');
      pointer(glAttachShader):=glGetProcAddress(GLESLib,'glAttachShader');
      pointer(glBindAttribLocation):=glGetProcAddress(GLESLib,'glBindAttribLocation');
      pointer(glBindBuffer):=glGetProcAddress(GLESLib,'glBindBuffer');
      pointer(glBindFramebuffer):=glGetProcAddress(GLESLib,'glBindFramebuffer');
      pointer(glBindRenderbuffer):=glGetProcAddress(GLESLib,'glBindRenderbuffer');
      pointer(glBindTexture):=glGetProcAddress(GLESLib,'glBindTexture');
      pointer(glBlendColor):=glGetProcAddress(GLESLib,'glBlendColor');
      pointer(glBlendEquation):=glGetProcAddress(GLESLib,'glBlendEquation');
      pointer(glBlendEquationSeparate):=glGetProcAddress(GLESLib,'glBlendEquationSeparate');
      pointer(glBlendFunc):=glGetProcAddress(GLESLib,'glBlendFunc');
      pointer(glBlendFuncSeparate):=glGetProcAddress(GLESLib,'glBlendFuncSeparate');
      pointer(glBufferData):=glGetProcAddress(GLESLib,'glBufferData');
      pointer(glBufferSubData):=glGetProcAddress(GLESLib,'glBufferSubData');
      pointer(glCheckFramebufferStatus):=glGetProcAddress(GLESLib,'glCheckFramebufferStatus');
      pointer(glClear):=glGetProcAddress(GLESLib,'glClear');
      pointer(glClearColor):=glGetProcAddress(GLESLib,'glClearColor');
      pointer(glClearDepthf):=glGetProcAddress(GLESLib,'glClearDepthf');
      pointer(glClearStencil):=glGetProcAddress(GLESLib,'glClearStencil');
      pointer(glColorMask):=glGetProcAddress(GLESLib,'glColorMask');
      pointer(glCompileShader):=glGetProcAddress(GLESLib,'glCompileShader');
      pointer(glCompressedTexImage2D):=glGetProcAddress(GLESLib,'glCompressedTexImage2D');
      pointer(glCompressedTexSubImage2D):=glGetProcAddress(GLESLib,'glCompressedTexSubImage2D');
      pointer(glCopyTexImage2D):=glGetProcAddress(GLESLib,'glCopyTexImage2D');
      pointer(glCopyTexSubImage2D):=glGetProcAddress(GLESLib,'glCopyTexSubImage2D');
      pointer(glCreateProgram):=glGetProcAddress(GLESLib,'glCreateProgram');
      pointer(glCreateShader):=glGetProcAddress(GLESLib,'glCreateShader');
      pointer(glCullFace):=glGetProcAddress(GLESLib,'glCullFace');
      pointer(glDeleteBuffers):=glGetProcAddress(GLESLib,'glDeleteBuffers');
      pointer(glDeleteFramebuffers):=glGetProcAddress(GLESLib,'glDeleteFramebuffers');
      pointer(glDeleteProgram):=glGetProcAddress(GLESLib,'glDeleteProgram');
      pointer(glDeleteRenderbuffers):=glGetProcAddress(GLESLib,'glDeleteRenderbuffers');
      pointer(glDeleteShader):=glGetProcAddress(GLESLib,'glDeleteShader');
      pointer(glDeleteTextures):=glGetProcAddress(GLESLib,'glDeleteTextures');
      pointer(glDepthFunc):=glGetProcAddress(GLESLib,'glDepthFunc');
      pointer(glDepthMask):=glGetProcAddress(GLESLib,'glDepthMask');
      pointer(glDepthRangef):=glGetProcAddress(GLESLib,'glDepthRangef');
      pointer(glDetachShader):=glGetProcAddress(GLESLib,'glDetachShader');
      pointer(glDisable):=glGetProcAddress(GLESLib,'glDisable');
      pointer(glDisableVertexAttribArray):=glGetProcAddress(GLESLib,'glDisableVertexAttribArray');
      pointer(glDrawArrays):=glGetProcAddress(GLESLib,'glDrawArrays');
      pointer(glDrawElements):=glGetProcAddress(GLESLib,'glDrawElements');
      pointer(glEnable):=glGetProcAddress(GLESLib,'glEnable');
      pointer(glEnableVertexAttribArray):=glGetProcAddress(GLESLib,'glEnableVertexAttribArray');
      pointer(glFinish):=glGetProcAddress(GLESLib,'glFinish');
      pointer(glFlush):=glGetProcAddress(GLESLib,'glFlush');
      pointer(glFramebufferRenderbuffer):=glGetProcAddress(GLESLib,'glFramebufferRenderbuffer');
      pointer(glFramebufferTexture2D):=glGetProcAddress(GLESLib,'glFramebufferTexture2D');
      pointer(glFrontFace):=glGetProcAddress(GLESLib,'glFrontFace');
      pointer(glGenBuffers):=glGetProcAddress(GLESLib,'glGenBuffers');
      pointer(glGenerateMipmap):=glGetProcAddress(GLESLib,'glGenerateMipmap');
      pointer(glGenFramebuffers):=glGetProcAddress(GLESLib,'glGenFramebuffers');
      pointer(glGenRenderbuffers):=glGetProcAddress(GLESLib,'glGenRenderbuffers');
      pointer(glGenTextures):=glGetProcAddress(GLESLib,'glGenTextures');
      pointer(glGetActiveAttrib):=glGetProcAddress(GLESLib,'glGetActiveAttrib');
      pointer(glGetActiveUniform):=glGetProcAddress(GLESLib,'glGetActiveUniform');
      pointer(glGetAttachedShaders):=glGetProcAddress(GLESLib,'glGetAttachedShaders');
      pointer(glGetAttribLocation):=glGetProcAddress(GLESLib,'glGetAttribLocation');
      pointer(glGetBooleanv):=glGetProcAddress(GLESLib,'glGetBooleanv');
      pointer(glGetBufferParameteriv):=glGetProcAddress(GLESLib,'glGetBufferParameteriv');
      pointer(glGetError):=glGetProcAddress(GLESLib,'glGetError');
      pointer(glGetFloatv):=glGetProcAddress(GLESLib,'glGetFloatv');
      pointer(glGetFramebufferAttachmentParameteriv):=glGetProcAddress(GLESLib,'glGetFramebufferAttachmentParameteriv');
      pointer(glGetIntegerv):=glGetProcAddress(GLESLib,'glGetIntegerv');
      pointer(glGetProgramiv):=glGetProcAddress(GLESLib,'glGetProgramiv');
      pointer(glGetProgramInfoLog):=glGetProcAddress(GLESLib,'glGetProgramInfoLog');
      pointer(glGetRenderbufferParameteriv):=glGetProcAddress(GLESLib,'glGetRenderbufferParameteriv');
      pointer(glGetShaderiv):=glGetProcAddress(GLESLib,'glGetShaderiv');
      pointer(glGetShaderInfoLog):=glGetProcAddress(GLESLib,'glGetShaderInfoLog');
      pointer(glGetShaderPrecisionFormat):=glGetProcAddress(GLESLib,'glGetShaderPrecisionFormat');
      pointer(glGetShaderSource):=glGetProcAddress(GLESLib,'glGetShaderSource');
      pointer(glGetString):=glGetProcAddress(GLESLib,'glGetString');
      pointer(glGetTexParameterfv):=glGetProcAddress(GLESLib,'glGetTexParameterfv');
      pointer(glGetTexParameteriv):=glGetProcAddress(GLESLib,'glGetTexParameteriv');
      pointer(glGetUniformfv):=glGetProcAddress(GLESLib,'glGetUniformfv');
      pointer(glGetUniformiv):=glGetProcAddress(GLESLib,'glGetUniformiv');
      pointer(glGetUniformLocation):=glGetProcAddress(GLESLib,'glGetUniformLocation');
      pointer(glGetVertexAttribfv):=glGetProcAddress(GLESLib,'glGetVertexAttribfv');
      pointer(glGetVertexAttribiv):=glGetProcAddress(GLESLib,'glGetVertexAttribiv');
      pointer(glGetVertexAttribPointerv):=glGetProcAddress(GLESLib,'glGetVertexAttribPointerv');
      pointer(glHint):=glGetProcAddress(GLESLib,'glHint');
      pointer(glIsBuffer):=glGetProcAddress(GLESLib,'glIsBuffer');
      pointer(glIsEnabled):=glGetProcAddress(GLESLib,'glIsEnabled');
      pointer(glIsFramebuffer):=glGetProcAddress(GLESLib,'glIsFramebuffer');
      pointer(glIsProgram):=glGetProcAddress(GLESLib,'glIsProgram');
      pointer(glIsRenderbuffer):=glGetProcAddress(GLESLib,'glIsRenderbuffer');
      pointer(glIsShader):=glGetProcAddress(GLESLib,'glIsShader');
      pointer(glIsTexture):=glGetProcAddress(GLESLib,'glIsTexture');
      pointer(glLineWidth):=glGetProcAddress(GLESLib,'glLineWidth');
      pointer(glLinkProgram):=glGetProcAddress(GLESLib,'glLinkProgram');
      pointer(glPixelStorei):=glGetProcAddress(GLESLib,'glPixelStorei');
      pointer(glPolygonOffset):=glGetProcAddress(GLESLib,'glPolygonOffset');
      pointer(glReadPixels):=glGetProcAddress(GLESLib,'glReadPixels');
      pointer(glReleaseShaderCompiler):=glGetProcAddress(GLESLib,'glReleaseShaderCompiler');
      pointer(glRenderbufferStorage):=glGetProcAddress(GLESLib,'glRenderbufferStorage');
      pointer(glSampleCoverage):=glGetProcAddress(GLESLib,'glSampleCoverage');
      pointer(glScissor):=glGetProcAddress(GLESLib,'glScissor');
      pointer(glShaderBinary):=glGetProcAddress(GLESLib,'glShaderBinary');
      pointer(glShaderSource):=glGetProcAddress(GLESLib,'glShaderSource');
      pointer(glStencilFunc):=glGetProcAddress(GLESLib,'glStencilFunc');
      pointer(glStencilFuncSeparate):=glGetProcAddress(GLESLib,'glStencilFuncSeparate');
      pointer(glStencilMask):=glGetProcAddress(GLESLib,'glStencilMask');
      pointer(glStencilMaskSeparate):=glGetProcAddress(GLESLib,'glStencilMaskSeparate');
      pointer(glStencilOp):=glGetProcAddress(GLESLib,'glStencilOp');
      pointer(glStencilOpSeparate):=glGetProcAddress(GLESLib,'glStencilOpSeparate');
      pointer(glTexImage2D):=glGetProcAddress(GLESLib,'glTexImage2D');
      pointer(glTexParameterf):=glGetProcAddress(GLESLib,'glTexParameterf');
      pointer(glTexParameterfv):=glGetProcAddress(GLESLib,'glTexParameterfv');
      pointer(glTexParameteri):=glGetProcAddress(GLESLib,'glTexParameteri');
      pointer(glTexParameteriv):=glGetProcAddress(GLESLib,'glTexParameteriv');
      pointer(glTexSubImage2D):=glGetProcAddress(GLESLib,'glTexSubImage2D');
      pointer(glUniform1f):=glGetProcAddress(GLESLib,'glUniform1f');
      pointer(glUniform1fv):=glGetProcAddress(GLESLib,'glUniform1fv');
      pointer(glUniform1i):=glGetProcAddress(GLESLib,'glUniform1i');
      pointer(glUniform1iv):=glGetProcAddress(GLESLib,'glUniform1iv');
      pointer(glUniform2f):=glGetProcAddress(GLESLib,'glUniform2f');
      pointer(glUniform2fv):=glGetProcAddress(GLESLib,'glUniform2fv');
      pointer(glUniform2i):=glGetProcAddress(GLESLib,'glUniform2i');
      pointer(glUniform2iv):=glGetProcAddress(GLESLib,'glUniform2iv');
      pointer(glUniform3f):=glGetProcAddress(GLESLib,'glUniform3f');
      pointer(glUniform3fv):=glGetProcAddress(GLESLib,'glUniform3fv');
      pointer(glUniform3i):=glGetProcAddress(GLESLib,'glUniform3i');
      pointer(glUniform3iv):=glGetProcAddress(GLESLib,'glUniform3iv');
      pointer(glUniform4f):=glGetProcAddress(GLESLib,'glUniform4f');
      pointer(glUniform4fv):=glGetProcAddress(GLESLib,'glUniform4fv');
      pointer(glUniform4i):=glGetProcAddress(GLESLib,'glUniform4i');
      pointer(glUniform4iv):=glGetProcAddress(GLESLib,'glUniform4iv');
      pointer(glUniformMatrix2fv):=glGetProcAddress(GLESLib,'glUniformMatrix2fv');
      pointer(glUniformMatrix3fv):=glGetProcAddress(GLESLib,'glUniformMatrix3fv');
      pointer(glUniformMatrix4fv):=glGetProcAddress(GLESLib,'glUniformMatrix4fv');
      pointer(glUseProgram):=glGetProcAddress(GLESLib,'glUseProgram');
      pointer(glValidateProgram):=glGetProcAddress(GLESLib,'glValidateProgram');
      pointer(glVertexAttrib1f):=glGetProcAddress(GLESLib,'glVertexAttrib1f');
      pointer(glVertexAttrib1fv):=glGetProcAddress(GLESLib,'glVertexAttrib1fv');
      pointer(glVertexAttrib2f):=glGetProcAddress(GLESLib,'glVertexAttrib2f');
      pointer(glVertexAttrib2fv):=glGetProcAddress(GLESLib,'glVertexAttrib2fv');
      pointer(glVertexAttrib3f):=glGetProcAddress(GLESLib,'glVertexAttrib3f');
      pointer(glVertexAttrib3fv):=glGetProcAddress(GLESLib,'glVertexAttrib3fv');
      pointer(glVertexAttrib4f):=glGetProcAddress(GLESLib,'glVertexAttrib4f');
      pointer(glVertexAttrib4fv):=glGetProcAddress(GLESLib,'glVertexAttrib4fv');
      pointer(glVertexAttribPointer):=glGetProcAddress(GLESLib,'glVertexAttribPointer');
      pointer(glViewport):=glGetProcAddress(GLESLib,'glViewport');
      pointer(glEGLImageTargetTexture2DOES):=glGetProcAddress(GLESLib,'glEGLImageTargetTexture2DOES');
      pointer(glEGLImageTargetRenderbufferStorageOES):=glGetProcAddress(GLESLib,'glEGLImageTargetRenderbufferStorageOES');
      pointer(glGetProgramBinaryOES):=glGetProcAddress(GLESLib,'glGetProgramBinaryOES');
      pointer(glProgramBinaryOES):=glGetProcAddress(GLESLib,'glProgramBinaryOES');
      pointer(glMapBufferOES):=glGetProcAddress(GLESLib,'glMapBufferOES');
      pointer(glUnmapBufferOES):=glGetProcAddress(GLESLib,'glUnmapBufferOES');
      pointer(glGetBufferPointervOES):=glGetProcAddress(GLESLib,'glGetBufferPointervOES');
      pointer(glTexImage3DOES):=glGetProcAddress(GLESLib,'glTexImage3DOES');
      pointer(glTexSubImage3DOES):=glGetProcAddress(GLESLib,'glTexSubImage3DOES');
      pointer(glCopyTexSubImage3DOES):=glGetProcAddress(GLESLib,'glCopyTexSubImage3DOES');
      pointer(glCompressedTexImage3DOES):=glGetProcAddress(GLESLib,'glCompressedTexImage3DOES');
      pointer(glCompressedTexSubImage3DOES):=glGetProcAddress(GLESLib,'glCompressedTexSubImage3DOES');
      pointer(glFramebufferTexture3DOES):=glGetProcAddress(GLESLib,'glFramebufferTexture3DOES');
      pointer(glGetPerfMonitorGroupsAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorGroupsAMD');
      pointer(glGetPerfMonitorCountersAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorCountersAMD');
      pointer(glGetPerfMonitorGroupStringAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorGroupStringAMD');
      pointer(glGetPerfMonitorCounterStringAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorCounterStringAMD');
      pointer(glGetPerfMonitorCounterInfoAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorCounterInfoAMD');
      pointer(glGenPerfMonitorsAMD):=glGetProcAddress(GLESLib,'glGenPerfMonitorsAMD');
      pointer(glDeletePerfMonitorsAMD):=glGetProcAddress(GLESLib,'glDeletePerfMonitorsAMD');
      pointer(glSelectPerfMonitorCountersAMD):=glGetProcAddress(GLESLib,'glSelectPerfMonitorCountersAMD');
      pointer(glBeginPerfMonitorAMD):=glGetProcAddress(GLESLib,'glBeginPerfMonitorAMD');
      pointer(glEndPerfMonitorAMD):=glGetProcAddress(GLESLib,'glEndPerfMonitorAMD');
      pointer(glGetPerfMonitorCounterDataAMD):=glGetProcAddress(GLESLib,'glGetPerfMonitorCounterDataAMD');
      pointer(glDebugMessageCallback):=glGetProcAddress(GLESLib,'glDebugMessageCallback');
      pointer(glDebugMessageControl):=glGetProcAddress(GLESLib,'glDebugMessageControl');

      { OpenGL ES 3.0 APIs }
      pointer(glReadBuffer) := glGetProcAddress(GLESLib, 'glReadBuffer');
      pointer(glDrawRangeElements) := glGetProcAddress(GLESLib, 'glDrawRangeElements');
      pointer(glTexImage3D) := glGetProcAddress(GLESLib, 'glTexImage3D');
      pointer(glTexSubImage3D) := glGetProcAddress(GLESLib, 'glTexSubImage3D');
      pointer(glCopyTexSubImage3D) := glGetProcAddress(GLESLib, 'glCopyTexSubImage3D');
      pointer(glCompressedTexImage3D) := glGetProcAddress(GLESLib, 'glCompressedTexImage3D');
      pointer(glCompressedTexSubImage3D) := glGetProcAddress(GLESLib, 'glCompressedTexSubImage3D');
      pointer(glGenQueries) := glGetProcAddress(GLESLib, 'glGenQueries');
      pointer(glDeleteQueries) := glGetProcAddress(GLESLib, 'glDeleteQueries');
      pointer(glIsQuery) := glGetProcAddress(GLESLib, 'glIsQuery');
      pointer(glBeginQuery) := glGetProcAddress(GLESLib, 'glBeginQuery');
      pointer(glEndQuery) := glGetProcAddress(GLESLib, 'glEndQuery');
      pointer(glGetQueryiv) := glGetProcAddress(GLESLib, 'glGetQueryiv');
      pointer(glGetQueryObjectuiv) := glGetProcAddress(GLESLib, 'glGetQueryObjectuiv');
      pointer(glUnmapBuffer) := glGetProcAddress(GLESLib, 'glUnmapBuffer');
      pointer(glGetBufferPointerv) := glGetProcAddress(GLESLib, 'glGetBufferPointerv');
      pointer(glDrawBuffers) := glGetProcAddress(GLESLib, 'glDrawBuffers');
      pointer(glUniformMatrix2x3fv) := glGetProcAddress(GLESLib, 'glUniformMatrix2x3fv');
      pointer(glUniformMatrix3x2fv) := glGetProcAddress(GLESLib, 'glUniformMatrix3x2fv');
      pointer(glUniformMatrix2x4fv) := glGetProcAddress(GLESLib, 'glUniformMatrix2x4fv');
      pointer(glUniformMatrix4x2fv) := glGetProcAddress(GLESLib, 'glUniformMatrix4x2fv');
      pointer(glUniformMatrix3x4fv) := glGetProcAddress(GLESLib, 'glUniformMatrix3x4fv');
      pointer(glUniformMatrix4x3fv) := glGetProcAddress(GLESLib, 'glUniformMatrix4x3fv');
      pointer(glBlitFramebuffer) := glGetProcAddress(GLESLib, 'glBlitFramebuffer');
      pointer(glRenderbufferStorageMultisample) := glGetProcAddress(GLESLib, 'glRenderbufferStorageMultisample');
      pointer(glFramebufferTextureLayer) := glGetProcAddress(GLESLib, 'glFramebufferTextureLayer');
      pointer(glMapBufferRange) := glGetProcAddress(GLESLib, 'glMapBufferRange');
      pointer(glFlushMappedBufferRange) := glGetProcAddress(GLESLib, 'glFlushMappedBufferRange');
      pointer(glBindVertexArray) := glGetProcAddress(GLESLib, 'glBindVertexArray');
      pointer(glDeleteVertexArrays) := glGetProcAddress(GLESLib, 'glDeleteVertexArrays');
      pointer(glGenVertexArrays) := glGetProcAddress(GLESLib, 'glGenVertexArrays');
      pointer(glIsVertexArray) := glGetProcAddress(GLESLib, 'glIsVertexArray');
      pointer(glGetIntegeri_v) := glGetProcAddress(GLESLib, 'glGetIntegeri_v');
      pointer(glBeginTransformFeedback) := glGetProcAddress(GLESLib, 'glBeginTransformFeedback');
      pointer(glEndTransformFeedback) := glGetProcAddress(GLESLib, 'glEndTransformFeedback');
      pointer(glBindBufferRange) := glGetProcAddress(GLESLib, 'glBindBufferRange');
      pointer(glBindBufferBase) := glGetProcAddress(GLESLib, 'glBindBufferBase');
      pointer(glTransformFeedbackVaryings) := glGetProcAddress(GLESLib, 'glTransformFeedbackVaryings');
      pointer(glGetTransformFeedbackVarying) := glGetProcAddress(GLESLib, 'glGetTransformFeedbackVarying');
      pointer(glVertexAttribIPointer) := glGetProcAddress(GLESLib, 'glVertexAttribIPointer');
      pointer(glGetVertexAttribIiv) := glGetProcAddress(GLESLib, 'glGetVertexAttribIiv');
      pointer(glGetVertexAttribIuiv) := glGetProcAddress(GLESLib, 'glGetVertexAttribIuiv');
      pointer(glVertexAttribI4i) := glGetProcAddress(GLESLib, 'glVertexAttribI4i');
      pointer(glVertexAttribI4ui) := glGetProcAddress(GLESLib, 'glVertexAttribI4ui');
      pointer(glVertexAttribI4iv) := glGetProcAddress(GLESLib, 'glVertexAttribI4iv');
      pointer(glVertexAttribI4uiv) := glGetProcAddress(GLESLib, 'glVertexAttribI4uiv');
      pointer(glGetUniformuiv) := glGetProcAddress(GLESLib, 'glGetUniformuiv');
      pointer(glGetFragDataLocation) := glGetProcAddress(GLESLib, 'glGetFragDataLocation');
      pointer(glUniform1ui) := glGetProcAddress(GLESLib, 'glUniform1ui');
      pointer(glUniform2ui) := glGetProcAddress(GLESLib, 'glUniform2ui');
      pointer(glUniform3ui) := glGetProcAddress(GLESLib, 'glUniform3ui');
      pointer(glUniform4ui) := glGetProcAddress(GLESLib, 'glUniform4ui');
      pointer(glUniform1uiv) := glGetProcAddress(GLESLib, 'glUniform1uiv');
      pointer(glUniform2uiv) := glGetProcAddress(GLESLib, 'glUniform2uiv');
      pointer(glUniform3uiv) := glGetProcAddress(GLESLib, 'glUniform3uiv');
      pointer(glUniform4uiv) := glGetProcAddress(GLESLib, 'glUniform4uiv');
      pointer(glClearBufferiv) := glGetProcAddress(GLESLib, 'glClearBufferiv');
      pointer(glClearBufferuiv) := glGetProcAddress(GLESLib, 'glClearBufferuiv');
      pointer(glClearBufferfv) := glGetProcAddress(GLESLib, 'glClearBufferfv');
      pointer(glClearBufferfi) := glGetProcAddress(GLESLib, 'glClearBufferfi');
      pointer(glGetStringi) := glGetProcAddress(GLESLib, 'glGetStringi');
      pointer(glCopyBufferSubData) := glGetProcAddress(GLESLib, 'glCopyBufferSubData');
      pointer(glGetUniformIndices) := glGetProcAddress(GLESLib, 'glGetUniformIndices');
      pointer(glGetActiveUniformsiv) := glGetProcAddress(GLESLib, 'glGetActiveUniformsiv');
      pointer(glGetUniformBlockIndex) := glGetProcAddress(GLESLib, 'glGetUniformBlockIndex');
      pointer(glGetActiveUniformBlockiv) := glGetProcAddress(GLESLib, 'glGetActiveUniformBlockiv');
      pointer(glGetActiveUniformBlockName) := glGetProcAddress(GLESLib, 'glGetActiveUniformBlockName');
      pointer(glUniformBlockBinding) := glGetProcAddress(GLESLib, 'glUniformBlockBinding');
      pointer(glDrawArraysInstanced) := glGetProcAddress(GLESLib, 'glDrawArraysInstanced');
      pointer(glDrawElementsInstanced) := glGetProcAddress(GLESLib, 'glDrawElementsInstanced');
      pointer(glFenceSync) := glGetProcAddress(GLESLib, 'glFenceSync');
      pointer(glIsSync) := glGetProcAddress(GLESLib, 'glIsSync');
      pointer(glDeleteSync) := glGetProcAddress(GLESLib, 'glDeleteSync');
      pointer(glClientWaitSync) := glGetProcAddress(GLESLib, 'glClientWaitSync');
      pointer(glWaitSync) := glGetProcAddress(GLESLib, 'glWaitSync');
      pointer(glGetInteger64v) := glGetProcAddress(GLESLib, 'glGetInteger64v');
      pointer(glGetSynciv) := glGetProcAddress(GLESLib, 'glGetSynciv');
      pointer(glGetInteger64i_v) := glGetProcAddress(GLESLib, 'glGetInteger64i_v');
      pointer(glGetBufferParameteri64v) := glGetProcAddress(GLESLib, 'glGetBufferParameteri64v');
      pointer(glGenSamplers) := glGetProcAddress(GLESLib, 'glGenSamplers');
      pointer(glDeleteSamplers) := glGetProcAddress(GLESLib, 'glDeleteSamplers');
      pointer(glIsSampler) := glGetProcAddress(GLESLib, 'glIsSampler');
      pointer(glBindSampler) := glGetProcAddress(GLESLib, 'glBindSampler');
      pointer(glSamplerParameteri) := glGetProcAddress(GLESLib, 'glSamplerParameteri');
      pointer(glSamplerParameteriv) := glGetProcAddress(GLESLib, 'glSamplerParameteriv');
      pointer(glSamplerParameterf) := glGetProcAddress(GLESLib, 'glSamplerParameterf');
      pointer(glSamplerParameterfv) := glGetProcAddress(GLESLib, 'glSamplerParameterfv');
      pointer(glGetSamplerParameteriv) := glGetProcAddress(GLESLib, 'glGetSamplerParameteriv');
      pointer(glGetSamplerParameterfv) := glGetProcAddress(GLESLib, 'glGetSamplerParameterfv');
      pointer(glVertexAttribDivisor) := glGetProcAddress(GLESLib, 'glVertexAttribDivisor');
      pointer(glBindTransformFeedback) := glGetProcAddress(GLESLib, 'glBindTransformFeedback');
      pointer(glDeleteTransformFeedbacks) := glGetProcAddress(GLESLib, 'glDeleteTransformFeedbacks');
      pointer(glGenTransformFeedbacks) := glGetProcAddress(GLESLib, 'glGenTransformFeedbacks');
      pointer(glIsTransformFeedback) := glGetProcAddress(GLESLib, 'glIsTransformFeedback');
      pointer(glPauseTransformFeedback) := glGetProcAddress(GLESLib, 'glPauseTransformFeedback');
      pointer(glResumeTransformFeedback) := glGetProcAddress(GLESLib, 'glResumeTransformFeedback');
      pointer(glGetProgramBinary) := glGetProcAddress(GLESLib, 'glGetProgramBinary');
      pointer(glProgramBinary) := glGetProcAddress(GLESLib, 'glProgramBinary');
      pointer(glProgramParameteri) := glGetProcAddress(GLESLib, 'glProgramParameteri');
      pointer(glInvalidateFramebuffer) := glGetProcAddress(GLESLib, 'glInvalidateFramebuffer');
      pointer(glInvalidateSubFramebuffer) := glGetProcAddress(GLESLib, 'glInvalidateSubFramebuffer');
      pointer(glTexStorage2D) := glGetProcAddress(GLESLib, 'glTexStorage2D');
      pointer(glTexStorage3D) := glGetProcAddress(GLESLib, 'glTexStorage3D');
      pointer(glGetInternalformativ) := glGetProcAddress(GLESLib, 'glGetInternalformativ');
    end;

procedure GLESInitialization;
begin
  {$ifdef EGL}
  LoadEGL(
    {$ifdef windows} 'libEGL.dll'
    { First try to access libEGL.so.1 (from libegl1-mesa package on Debian).
      The name libEGL.so is only available in -dev package. }
    {$else} 'libEGL.so.1', 'libEGL.so'
    {$endif});
  {$endif}

  LoadGLES(
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
  GLESLib:=0;
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  GLESInitialization;
  {$endif}
finalization
  FreeGLES;
{$ifdef EGL}
  FreeEGL;
{$endif}
end.
