{
  Copyright 2001-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various low-level utilities for working with OpenGL.

  This unit contains a mixture of useful utilities.
  Simple wrappers for OpenGL procedures (like glVertexv, that is overloaded
  for various vector types, and calls appropriate version like glVertex3fv
  based on parameter type). Also simple drawing routines for basic primitives
  (boxes etc.).
  This unit does not assume that you initialized OpenGL in any particular
  way (e.g. using CastleWindow, Glut, SDL, or whatever).
}

unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses Math, GL, GLU, GLExt, SysUtils, CastleUtils, CastleVectors, CastleBoxes,
  CastleImages, Matrix;

{$define read_interface}

type
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

{$I glext_packed_depth_stencil.inc}
{$I glext_arb_framebuffer_object.inc}
{$I glext_ext_fog_coord.inc}

type
  { Types with leading "T" } { }
  TGLenum     = GLenum;
  TGLboolean  = GLboolean;
  TGLbitfield = GLbitfield;
  TGLbyte     = GLbyte;
  TGLshort    = GLshort;
  TGLint      = GLint;
  TGLsizei    = GLsizei;
  TGLubyte    = GLubyte;
  TGLushort   = GLushort;
  TGLuint     = GLuint;
  TGLfloat    = GLfloat;
  TGLclampf   = GLclampf;
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;

const
  { OpenGL so/dll filenames.
    These constants @italic(must) match constants used in implementation
    of GL and GLU units, so that we link to the same libraries. } { }
  OpenGLDLL =
    {$ifdef MSWINDOWS} {TODO: needs fix for WIN64?} 'opengl32.dll' {$endif}
    {$ifdef UNIX}
      {$ifdef DARWIN}
        '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib'
      {$else} 'libGL.so.1'
      {$endif}
    {$endif};
  GluDLL =
    {$ifdef MSWINDOWS} {TODO: needs fix for WIN64?} 'glu32.dll' {$endif}
    {$ifdef UNIX}
      {$ifdef DARWIN}
        '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib'
      {$else} 'libGLU.so.1'
      {$endif}
    {$endif};

{ OpenGL versions and extensions and features -------------------------------- }

var
  { OpenGL versions supported. Checked by looking at GL version string
    @italic(and) by checking whether actual entry points are available. }
  GL_version_1_2: boolean;
  GL_version_1_3: boolean;
  GL_version_1_4: boolean;
  GL_version_1_5: boolean;
  GL_version_2_0: boolean;

  {$define HAS_GL_VERSION_ABOVE_2}
  {$ifdef VER2_0}   {$undef HAS_GL_VERSION_ABOVE_2} {$endif}
  {$ifdef VER2_2}   {$undef HAS_GL_VERSION_ABOVE_2} {$endif}
  {$ifdef VER2_4_0} {$undef HAS_GL_VERSION_ABOVE_2} {$endif}
  {$ifdef VER2_4_2} {$undef HAS_GL_VERSION_ABOVE_2} {$endif}
  {$ifdef HAS_GL_VERSION_ABOVE_2}
  GL_version_2_1: boolean;
  GL_version_3_0: boolean;
  GL_version_3_1: boolean;
  GL_version_3_2: boolean;
  GL_version_3_3: boolean;
  GL_version_4_0: boolean;
  {$endif}

  GL_ARB_imaging: boolean;
  GL_ARB_transpose_matrix: boolean;
  GL_ARB_multisample: boolean;
  GL_ARB_texture_env_add: boolean;
  GL_ARB_texture_cube_map: boolean;
  GL_ARB_depth_texture: boolean;
  GL_ARB_point_parameters: boolean;
  GL_ARB_shadow: boolean;
  GL_ARB_shadow_ambient: boolean;
  GL_ARB_texture_border_clamp: boolean;
  GL_ARB_texture_compression: boolean;
  GL_ARB_texture_env_combine: boolean;
  GL_ARB_texture_env_crossbar: boolean;
  GL_ARB_texture_env_dot3: boolean;
  GL_ARB_texture_mirrored_repeat: boolean;
  GL_ARB_vertex_blend: boolean;
  GL_ARB_vertex_program: boolean;
  GL_ARB_window_pos: boolean;
  GL_EXT_422_pixels: boolean;
  GL_EXT_abgr: boolean;
  GL_EXT_bgra: boolean;
  GL_EXT_blend_color: boolean;
  GL_EXT_blend_func_separate: boolean;
  GL_EXT_blend_logic_op: boolean;
  GL_EXT_blend_minmax: boolean;
  GL_EXT_blend_subtract: boolean;
  GL_EXT_clip_volume_hint: boolean;
  GL_EXT_color_subtable: boolean;
  GL_EXT_compiled_vertex_array: boolean;
  GL_EXT_convolution: boolean;
  GL_EXT_fog_coord: boolean;
  GL_EXT_histogram: boolean;
  GL_EXT_multi_draw_arrays: boolean;
  GL_EXT_packed_pixels: boolean;
  GL_EXT_paletted_texture: boolean;
  GL_EXT_point_parameters: boolean;
  GL_EXT_polygon_offset: boolean;
  GL_EXT_secondary_color: boolean;
  GL_EXT_separate_specular_color: boolean;
  GL_EXT_shadow_funcs: boolean;
  GL_EXT_shared_texture_palette: boolean;
  GL_EXT_stencil_two_side: boolean;
  GL_EXT_stencil_wrap: boolean;
  GL_EXT_subtexture: boolean;
  GL_EXT_texture_compression_s3tc: boolean;
  GL_EXT_texture_env_add: boolean;
  GL_EXT_texture_env_combine: boolean;
  GL_EXT_texture_env_dot3: boolean;
  GL_EXT_texture_filter_anisotropic: boolean;
  GL_EXT_texture_lod_bias: boolean;
  GL_EXT_texture_object: boolean;
  GL_EXT_vertex_array: boolean;
  GL_EXT_vertex_shader: boolean;
  GL_EXT_vertex_weighting: boolean;
  GL_HP_occlusion_test: boolean;
  GL_NV_blend_square: boolean;
  GL_NV_copy_depth_to_color: boolean;
  GL_NV_depth_clamp: boolean;
  GL_NV_evaluators: boolean;
  GL_NV_fence: boolean;
  GL_NV_fog_distance: boolean;
  GL_NV_light_max_exponent: boolean;
  GL_NV_multisample_filter_hint: boolean;
  GL_NV_occlusion_query: boolean;
  GL_NV_packed_depth_stencil: boolean;
  GL_NV_point_sprite: boolean;
  GL_NV_register_combiners: boolean;
  GL_NV_register_combiners2: boolean;
  GL_NV_texgen_emboss: boolean;
  GL_NV_texgen_reflection: boolean;
  GL_NV_texture_compression_vtc: boolean;
  GL_NV_texture_env_combine4: boolean;
  GL_NV_texture_rectangle: boolean;
  GL_NV_texture_shader: boolean;
  GL_NV_texture_shader2: boolean;
  GL_NV_texture_shader3: boolean;
  GL_NV_vertex_array_range: boolean;
  GL_NV_vertex_array_range2: boolean;
  GL_NV_vertex_program: boolean;
  GL_NV_vertex_program1_1: boolean;
  GL_ATI_element_array: boolean;
  GL_ATI_envmap_bumpmap: boolean;
  GL_ATI_fragment_shader: boolean;
  GL_ATI_pn_triangles: boolean;
  GL_ATI_texture_mirror_once: boolean;
  GL_ATI_vertex_array_object: boolean;
  GL_ATI_vertex_streams: boolean;
  GL_3DFX_texture_compression_FXT1: boolean;
  GL_IBM_cull_vertex: boolean;
  GL_IBM_multimode_draw_arrays: boolean;
  GL_IBM_raster_pos_clip: boolean;
  GL_IBM_texture_mirrored_repeat: boolean;
  GL_IBM_vertex_array_lists: boolean;
  GL_MESA_resize_buffers: boolean;
  GL_MESA_window_pos: boolean;
  GL_OML_interlace: boolean;
  GL_OML_resample: boolean;
  GL_OML_subsample: boolean;
  GL_SGIS_generate_mipmap: boolean;
  GL_SGIS_multisample: boolean;
  GL_SGIS_pixel_texture: boolean;
  GL_SGIS_texture_border_clamp: boolean;
  GL_SGIS_texture_color_mask: boolean;
  GL_SGIS_texture_edge_clamp: boolean;
  GL_SGIS_texture_lod: boolean;
  GL_SGIS_depth_texture: boolean;
  GL_SGIX_fog_offset: boolean;
  GL_SGIX_interlace: boolean;
  GL_SGIX_shadow_ambient: boolean;
  GL_SGI_color_matrix: boolean;
  GL_SGI_color_table: boolean;
  GL_SGI_texture_color_table: boolean;
  GL_SUN_vertex: boolean;
  GL_ARB_fragment_program: boolean;
  GL_ATI_text_fragment_shader: boolean;
  GL_APPLE_client_storage: boolean;
  GL_APPLE_element_array: boolean;
  GL_APPLE_fence: boolean;
  GL_APPLE_vertex_array_object: boolean;
  GL_APPLE_vertex_array_range: boolean;
  GL_ARB_matrix_palette: boolean;
  GL_NV_element_array: boolean;
  GL_NV_float_buffer: boolean;
  GL_NV_fragment_program: boolean;
  GL_NV_primitive_restart: boolean;
  GL_NV_vertex_program2: boolean;
  GL_ATI_separate_stencil: boolean;
  GL_ARB_texture_non_power_of_two: boolean;
  GL_ARB_vertex_buffer_object: boolean;
  GL_ARB_occlusion_query: boolean;
  GL_EXT_packed_depth_stencil: boolean;
  GL_ATI_texture_float: boolean;
  GL_ARB_texture_float: boolean;
  GL_ARB_texture_rectangle: boolean;

var
  { Constant (for given context) OpenGL limits.
    Initialized once by LoadAllExtensions, this is usually most comfortable.
    Initialized to 0 if appropriate OpenGL extension is not available.
    @groupBegin }
  GLMaxTextureSize: Cardinal;
  GLMaxLights: Cardinal;
  GLMaxCubeMapTextureSizeARB: Cardinal;
  GLMax3DTextureSize: Cardinal;
  GLMaxTextureMaxAnisotropyEXT: Single;
  GLQueryCounterBits: TGLint;
  GLMaxRenderbufferSize: TGLuint;
  GLMaxRectangleTextureSize: Cardinal;
  GLMaxClipPlanes: Cardinal;
  { @groupEnd }

  { Numer of texture units available.
    Equal to glGetInteger(GL_MAX_TEXTURE_UNITS_ARB), if multi-texturing
    available. Equal to 1 (OpenGL supports always 1 texture) otherwise. }
  GLMaxTextureUnits: Cardinal;

  { Are all OpenGL multi-texturing extensions for
    VRML/X3D MultiTexture support available.

    This used to check a couple of multitexturing extensions,
    like ARB_multitexture. Right now, it simply checks for OpenGL 1.3 version.
    It is supported by virtually all existing GPUs.
    So it's acceptable to just check it, and write your code for 1.3,
    and eventual fallback code (when this is false) write only for really
    ancient GPUs. }
  GLUseMultiTexturing: boolean;

  { Are all OpenGL ARB extensions for GLSL available. }
  GLUseARBGLSL: boolean;

  { Are 3D textures supported by OpenGL.
    If they are, note that GL_TEXTURE_3D and GL_TEXTURE_3D_EXT are equal,
    so often both GL3DTextures = gsStandard and GL3DTextures = gsExtension
    cases may be handled by the same code. }
  GL3DTextures: TGLSupport;

  { Is Framebuffer supported. Value gsExtension means that EXT_framebuffer_object
    is used, gsStandard means that ARB_framebuffer_object (which is
    a "core extesion", present the same way in OpenGL 3 core) is available. }
  GLFramebuffer: TGLSupport;

  { Is multisampling possible for FBO buffers and textures.
    Although these are two orthogonal features of OpenGL,
    in practice you want to use multisample for both FBO buffers and textures,
    or for none --- otherwise, FBO can not be initialized correctly
    when you mix various multisample settings. }
  GLFBOMultiSampling: boolean;

  { How multi-sampling was initialized for this OpenGL context.
    Value = 1 means that no multi-sampling is initialized.
    Values > 1 mean that you have multi-sampling, with given number of samples
    per pixel.
    Contrast this with TCastleWindowBase.MultiSampling or TOpenGLControl.MultiSampling,
    that say @italic(how many samples you wanted to get). }
  GLCurrentMultiSampling: Cardinal;

  { Does OpenGL context have depth buffer packed with stencil buffer.
    See EXT_packed_depth_stencil extension for explanation.

    This is important for FBOs, as the depth/stencil have to be set up differently
    depending on GLPackedDepthStencil value.
    This is also important for all code using TGLRenderToTexture
    with TGLRenderToTexture.Buffer equal tbDepth or tbColorAndDepth:
    your depth texture must be prepared differently, to include both depth+stencil
    data, to work.

    For now, this is simply equal to GL_EXT_packed_depth_stencil.
    (TODO: for core OpenGL 3, how to detect should we use packed version?
    http://www.opengl.org/registry/specs/ARB/framebuffer_object.txt
    incorporates EXT_packed_depth_stencil, so forward-compatible contexts
    do not need to declare it.
    Should we assume that forward-compatible gl 3 contexts always have
    depth/stencil packed?) }
  GLPackedDepthStencil: boolean;

  { Does OpenGL context support shadow volumes.
    This simply checks do we have stencil buffer with at least 4 bits for now. }
  GLShadowVolumesPossible: boolean;

{ Initialize all extensions and OpenGL versions.

  Calls all Load_GLXxx routines from glext unit, so tries to init
  @unorderedList(
    @itemSpacing compact
    @item all GL 1.2, 1.3, 2.0 etc. function addresses and
    @item all gl extensions function addresses and
    @item inits all boolean extensions variables.
  )

  Note that variables GL_version_x_x, like GL_version_2_0, are initialized
  by checking both version string (glGetString(GL_VERSION) etc) @bold(and) by
  actually checking whether all entry points are non-nil.
  This is important because with buggy OpenGL implementations
  (see shitty ATI Linux closed drivers) version number in version string
  may be untrue: version string reports OpenGL 2.0,
  but actually lack entry points for pretty much all OpenGL 2.0 functions.
  So GLVersion.AtLeast(2, 0) returns @true, but in fact you can't have
  OpenGL 2.0 functions.

  Implementation: Load_GL_version_x_x from GLExt unit actually always checks
  whether all entry points are <> nil, that's good. GLVersion.AtLeast by
  definition only checks version string, so this is used to check version
  string.

  Note that it would be an error to check only whether entry points
  are non-nil, I should always check version string too. For example
  see @code(glStencilOpSeparate := nil) in TGLShadowVolumeRenderer.InitGLContext
  fix, on Mesa 6.x and NVidia legacy 96xx on Linux this is non-nil
  (i.e. entry point exists in GL library), but doesn't work
  (I didn't use GLExt back then, but OpenGLh).
  And OpenGL version string indicates gl 1.x version.
  This is OK, I think: and it means I should always check version string
  (not depend that all library entry points are actually implemented).

  So when e.g. GL_version_2_0 is @true, you are
  actually sure that all GL 2.0 entry points are really non-nil and they
  really should work.

  Inits also GLVersion and GLUVersion from GLVersionUnit. }
procedure LoadAllExtensions;

{ OpenGL vector/matrix types ------------------------------------------------- }

type
  { }
  TVector2f = TVector2Single;   PVector2f = PVector2Single;
  TVector2d = TVector2Double;   PVector2d = PVector2Double;
  TVector2ub = TVector2Byte;    PVector2ub = PVector2Byte;
  TVector2i = TVector2LongInt;  PVector2i = PVector2LongInt;

  TVector3f = TVector3Single;   PVector3f = PVector3Single;
  TVector3d = TVector3Double;   PVector3d = PVector3Double;
  TVector3ub = TVector3Byte;    PVector3ub = PVector3Byte;
  TVector3i = TVector3LongInt;  PVector3i = PVector3LongInt;

  TVector4f = TVector4Single;   PVector4f = PVector4Single;
  TVector4d = TVector4Double;   PVector4d = PVector4Double;
  TVector4ub = TVector4Byte;    PVector4ub = PVector4Byte;
  TVector4i = TVector4LongInt;  PVector4i = PVector4LongInt;

  TMatrix2f = TMatrix2Single;  PMatrix2f = PMatrix2Single;
  TMatrix2d = TMatrix2Double;  PMatrix2d = PMatrix2Double;

  TMatrix3f = TMatrix3Single;  PMatrix3f = PMatrix3Single;
  TMatrix3d = TMatrix3Double;  PMatrix3d = PMatrix3Double;

  TMatrix4f = TMatrix4Single;  PMatrix4f = PMatrix4Single;
  TMatrix4d = TMatrix4Double;  PMatrix4d = PMatrix4Double;

{ OpenGL error checking ------------------------------------------------------ }

type
  { }
  EOpenGLError = class(Exception)
  public
    ErrorCode: TGLenum;
    constructor Create(const AErrorCode: TGLenum;
      const AdditionalComment: string = '');
  end;

{ Check are any OpenGL errors recorded (in glGetError).
  If there are errors, our behavior depends on whether we were compiled
  with -dRELEASE. With -dRELEASE, we make OnWarning. This way eventual
  errors in release builds don't completely abort your program.

  Without -dRELEASE, we raise EOpenGLError. So a developer is strongly
  suggested to fix the code to not produce OpenGL errors, never ever.

  @raises EOpenGLError }
procedure CheckGLErrors(const AdditionalComment: string = '');

{ Raise EOpenGLError for given OpenGL error code.
  This has calling convention suitable for registering this procedure
  as GLU_TESS_ERROR for gluTessCallback, or GLU_ERROR for gluQuadricCallback. }
procedure GLErrorRaise(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}

{ ---------------------------------------------------------------------------- }

{ Comfortable wrappers for OpenGL glGet* that return a single value.

  Guarantee that result is zero in case of OpenGL error.
  (Otherwise, OpenGL could leave them undefined and only set glGetError.)
  @groupBegin }
function glGetFloat(pname: TGLEnum): TGLfloat;
function glGetInteger(pname: TGLEnum): TGLint;
function glGetBoolean(pname: TGLEnum): TGLboolean;
function glGetDouble(pname: TGLEnum): TGLdouble;
{ @groupEnd }

{ ------------------------------------------------------------------------------
  Comfortable wrappers around many OpenGL functions.
  Overloaded for our vector types.

  Note that functions here simply call appropriate OpenGL functions.
  Long time ago we tried using tricks to speed this up (eliminate
  function call overhead), by importing these functions from so/dll
  under different names, like

    procedure glVertexv(const V: TVector3Single); OPENGL_CALL overload; external OpenGLDLL name 'glVertex3fv';

  But this is problematic: it assumes that TVector3Single will be passed
  by reference. Which actually is not guaranteed by a compiler (FPC sometimes
  doesn't). In newer FPC versions, we could use "constref" for this.
  Or we could just declare these functions as "inline".
  However, speeding these functions is just not needed at all anymore
  (especially with new VBO renderer that passes everything through arrays).
}

{ }
procedure glColorv(const v: TVector3ub); overload;
procedure glColorv(const v: TVector4ub); overload;

procedure glTranslatev(const V: TVector3f); overload;
procedure glTranslatev(const V: TVector3d); overload;
procedure glTranslatev(const V: TVector3_Single); overload;
procedure glTranslatev(const V: TVector3_Double); overload;

procedure glScalev(const V: Single); overload;
procedure glScalev(const V: TVector3f); overload;
procedure glScalev(const V: TVector3d); overload;
procedure glScalev(const V: TVector3_Single); overload;
procedure glScalev(const V: TVector3_Double); overload;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); overload;
procedure glRotatev(const Angle: TGLdouble; const V: TVector3d); overload;

procedure glClipPlane(plane: GLenum; const V: TVector4d); overload;

procedure gluLookAtv(const Eye, Center, Up: TVector3Single);
procedure gluLookDirv(const Eye, Dir, Up: TVector3Single);

procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4f); overload;

procedure glClearColorv(const v: TVector3f; alpha: Single);

procedure glNormalv(const v: TVector3d); overload;
procedure glNormalv(const v: TVector3f); overload;

procedure glColorv(const v: TVector3d); overload;
procedure glColorv(const v: TVector3f); overload;

procedure glColorv(const v: TVector4d); overload;
procedure glColorv(const v: TVector4f); overload;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f); overload;

procedure glVertexv(const v: TVector2d); overload;
procedure glVertexv(const v: TVector2f); overload;
procedure glVertexv(const v: TVector2i); overload;
procedure glVertexv(const v: TVector3d); overload;
procedure glVertexv(const v: TVector3f); overload;
procedure glVertexv(const v: TVector3i); overload;
procedure glVertexv(const v: TVector4d); overload;
procedure glVertexv(const v: TVector4f); overload;
procedure glVertexv(const v: TVector4i); overload;

procedure glVertexv(const v: TVector2_Double); overload;
procedure glVertexv(const v: TVector2_Single); overload;
procedure glVertexv(const v: TVector3_Double); overload;
procedure glVertexv(const v: TVector3_Single); overload;
procedure glVertexv(const v: TVector4_Double); overload;
procedure glVertexv(const v: TVector4_Single); overload;

procedure glTexCoordv(const v: TVector2d); overload;
procedure glTexCoordv(const v: TVector2f); overload;
procedure glTexCoordv(const v: TVector3d); overload;
procedure glTexCoordv(const v: TVector3f); overload;
procedure glTexCoordv(const v: TVector4d); overload;
procedure glTexCoordv(const v: TVector4f); overload;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4d); overload;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f); overload;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f); overload;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); overload;

procedure glFogv(pname: TGLEnum; const params: TVector4f); overload;

procedure glMultMatrix(const m: TMatrix4f); overload;
procedure glMultMatrix(const m: TMatrix4d); overload;
procedure glLoadMatrix(const m: TMatrix4f); overload;
procedure glLoadMatrix(const m: TMatrix4d); overload;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); overload;

{ Simple save/restore of OpenGL pixel store ---------------------------------- }

type
  { }
  TPixelStoreUnpack = record
    UnpackSwapBytes,
    UnpackLSBFirst: TGLboolean;
    UnpackRowLength,
    UnpackSkipRows,
    UnpackSkipPixels: TGLint;
    UnpackAlignment: Cardinal;
  end;

procedure SavePixelStoreUnpack(out pixUnpack: TPixelStoreUnpack);
procedure LoadPixelStoreUnpack(const pixUnpack: TPixelStoreUnpack);

type
  TUnpackNotAlignedData = record
    Alignment: Cardinal;
  end;
  TPackNotAlignedData = TUnpackNotAlignedData;

{ Save/restore OpenGL pixel store for unpacking TRGBImage.
  Before you pass an TRGBImage to glDrawPixels, glTexImage1D, glTexImage2D,
  glBitmap, glPolygonStipple and such, call
  BeforeUnpackNotAlignedRGBImage, and later call
  AfterUnpackNotAlignedRGBImage to restore original state.
  @groupBegin }
procedure BeforeUnpackNotAlignedRGBImage(out unpackdata: TUnpackNotAlignedData; imageWidth: cardinal);
procedure AfterUnpackNotAlignedRGBImage(const unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
{ @groupEnd }

{ Save/restore OpenGL pixel store for packing TRGBImage.
  Use around glReadPixels and such with TRGBImage.
  @groupBegin }
procedure BeforePackNotAlignedRGBImage(out packdata: TPackNotAlignedData; imageWidth: cardinal);
procedure AfterPackNotAlignedRGBImage(const packData: TPackNotAlignedData; imageWidth: cardinal);
{ @groupEnd }

{ Save/restore OpenGL pixel store for unpacking / packing given TCastleImage.
  Before you pass this image to some OpenGL procedures
  (like glDrawPixels for unpacking, glReadPixels for packing),
  call BeforeXxx, and later call AfterXxx to restore original state.
  These will take care of setting/restoring pixel alignment.
  @groupBegin }
procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
{ @groupEnd }

{ Projection matrix -------------------------------------------------------- }

{ Load projection matrix to OpenGL GL_PROJECTION matrix.

  At the end, these unconditionally change matrix mode to GL_MODELVIEW.

  ZFar is allowed to have special ZFarInfinity value
  for PerspectiveProjection.
  Then we set special perspective matrix, that has far plane set
  at infinity --- useful for z-fail shadow volumes.

  @groupBegin }
function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
function OrthoProjection(const left, right, bottom, top: Single;
  const zNear: Single = -1; const zFar: Single = 1): TMatrix4Single;
{ @groupEnd }

{ ---------------------------------------------------------------------------- }

{ }
procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);

{ Draw vertical line using OpenGL.
  Uses current OpenGL color. }
procedure GLVerticalLine(x, y1, y2: TGLfloat);

{ Draw horizontal line using OpenGL.
  Uses current OpenGL color. }
procedure GLHorizontalLine(x1, x2, y: TGLfloat);

{ Draw rectangle, filled with one color and framed with other color.

  The vertex order is the same as for glRectf, so it's CCW from standard view.
  We assume that OpenGL polygon fill mode is "fill".
  Changes OpenGL current color. }
procedure GLRectangleWithBorder(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f);

{ Draw rectangle border.

  The vertex order is the same as for glRectf, so it's CCW from standard view.
  Uses current OpenGL color. }
procedure GLRectangleBorder(const x1, y1, x2, y2: TGLfloat); overload;

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).

  Everything is drawn CCW when seen from standard view (x grows right, y up).
  Uses current OpenGL color. }
procedure GLDrawArrow(HeadThickness: TGLfloat = 0.4;
  HeadLength: TGLfloat = 0.5);

{ Comfortable wrapper for gluNewQuadric. Sets all quadric parameters.
  Sets also the GLU_ERROR callback to ReportGLerror.
  @raises Exception If gluNewQuadric fails (returns nil). }
function NewGLUQuadric(
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL): PGLUQuadric; overload;

{ Render sphere in OpenGL. Radius, Slices, Stacks have the same meaning
  as for gluSphere (in case they are not self-explanatory...).
  Other parameters set glu quadric parameters, see glu quadric documentation. }
procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL);

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3Single; Size: Single);

{ Draw the wireframe box.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawBox3DWire(const Box: TBox3D);

{ Draw simple box. Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. Order is CCW outside
  (so if you want, you can turn on backface culling yourself).

  You @bold(must enable GL_VERTEX_ARRAY before using this).
  (It's not done automatically, as it's much faster to do it once
  for many glDrawBox3DSimple calls. Example --- bzwgen city view behind
  building 1, with occlusion query used: FPC 150 vs 110 when
  GL_VERTEX_ARRAY is activated once in OcclusionBoxStateBegin, not here.
  Tested on fglrx on Radeon X1600 (chantal).)

  It can be safely placed in a display list. }
procedure glDrawBox3DSimple(const Box: TBox3D);

type
  TProcData = procedure (Data: Pointer);

{ Temporarily switch to 2D OpenGL projection, call given callback,
  then restore original projection.

  2D projection is like gluOrtho2d(0, Viewport.width, 0, Viewport.height).
  Use more general glProjectionPushPop to provide any projection matrix.

  The current matrix mode doesn't have to be GL_PROJECTION at call time.
  We will take care to have the original matrix mode when calling Proc
  callback. }
procedure glProjectionPushPop2D(proc: TProcData; Data: Pointer);

procedure glProjectionPushPopOrtho(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top, ZNear, ZFar: TGLdouble);
procedure glProjectionPushPopOrtho2D(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top: TGLdouble);
procedure glProjectionPushPopPerspective(proc: TProcData; Data: Pointer;
  const FovyDeg, Aspect, ZNear, ZFar: TGLdouble);
procedure glProjectionPushPop(proc: TProcData; Data: Pointer;
  const projMatrix: TMatrix4f);

{ Draw a rectangle that modulates colors underneath,
  suddenly changing it to FadeColor and then fading to blackness and
  then fading back to normal, as FadeIntensity goes down from 1.0 to 0.0.
  This is nice to use for a screen effect when player is hurt.

  The rectangle is affected by current modelview matrix.
  Requires one attrib stack place. }
procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single;
  const FadeIntensity: Single);

{ Draw a rectangle with blending.

  The rectangle is affected by current modelview matrix.
  Requires one attrib stack place. }
procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor, DestinationFactor: TGLenum;
  const Color: TVector4Single);

{ Multiline string describing attributes of current OpenGL
  library. This simply queries OpenGL using glGet* functions
  about many things. Does not change OpenGL state in any way.

  Note that the last line of returned string does not terminate
  with a newline character (so e.g. you may want to do
  Writeln(GLInformationString) instead of just Write(GLInformationString)). }
function GLInformationString: string;

const
  GLDefaultLightModelAmbient: TVector4Single = (0.2, 0.2, 0.2, 1.0);

{ Utilities for display lists ---------------------------------------- }

type
  { }
  EOpenGLNoMoreDisplayLists = class(Exception)
  end;

{ Call glGenLists(range) and checks the result.

  @raises(EOpenGLNoMoreDisplayLists
    When glGenLists(Range) returned zero for non-zero Range.
    The exception's Message shows Place, which may describe
    where this is called --- makes it easier to debug.) }
function glGenListsCheck(range: TGLsizei; const Place: string): TGLuint;

{ If List <> 0 then it does glDeleteList on List and sets List to 0.
  In other words this is simply glDeleteList but
  @orderedList(
    @item only if List really should be deleted
    @item sets List to 0 after deletion
  ) }
procedure glFreeDisplayList(var list: TGLuint);

{ If Tex <> 0 then it does glDeleteTextures on Tex and sets Tex to 0.
  In other words, this is a simple wrapper over glDeleteTextures that
  @orderedList(
    @item checks if Tex really should be deleted
    @item sets Tex to 0 to not free it once again
  ) }
procedure glFreeTexture(var Tex: TGLuint);

{ Equivalent to glListBase but it's parameter is a signed integer.

  Original declararations of glListBase take unsigned integer,
  while actually a signed integer is also allowed. Actually,
  you should always call gListBase with range/overflow checking turned off.
  That's because argument to glListBase is used by OpenGL
  only in an expression like

    @longcode# Base + CurrentListNumber #

  so it's the "CurrentListNumber" that determines what Base actually means
  --- e.g. if Base = LongWord(-100) and CurrentListNumber = 1000 then
  the actual list number is 900, and this is all that matters.
  So you can say that Base was nagative.
  But if Base = LongWord(-100) and CurrentListNumber = 0 then
  the actual list number is LongWord(-100) = <some big integer around 4 * 10^9>.
  So you can say that Base was positive. }
var
  glListIBase: procedure(base: TGLint);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}

{ Set color and depth buffers writeable or not.
  This is just a shortcut for
  @longcode(#
    glDepthMask(Writeable);
    glColorMask(Writeable, Writeable, Writeable, Writeable);
  #) }
procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);

{ Sets raster position in window coordinates, also the raster position
  is never clipped.

  This is similar to glWindowPos and actually will simply call
  glWindowPos if available (if GL version >= 1.4 or ARB_window_pos or
  MESA_window_pos).

  If not available, it will fall back on a simple
  implementation that sets identity to projection and modelview and
  sets a special viewport (setting special viewport means that
  we can avoid clipping the raster pos, also it means that you
  don't have to pass here parameters like window width/height --- viewport
  will appropriately map to your window coordinates).

  What happens to depth value is not specified (although OpenGL
  ARB_window_pos specifies it, we don't want to specify it,
  to be able to pull our simple implementation when ARB_window_pos
  is not available).

  SetWindowPosZero is just a slightly optimized shortcut for SetWindowPos(0, 0).

  @groupBegin }
procedure SetWindowPos(const X, Y: TGLfloat); overload;
procedure SetWindowPos(const X, Y: TGLint); overload;
procedure SetWindowPosZero;
{ @groupEnd }

{ Return GL_CLAMP_TO_EDGE, if available in current OpenGL version.
  Otherwise returns GL_CLAMP.

  Use this (insteaf of direct GL_CLAMP_TO_EDGE)
  to work with @italic(really ancient) OpenGL versions before 1.2.
  Note that our engine officially supports only OpenGL >= 1.2,
  so don't expect everything to work smootly with such ancient OpenGL anyway! }
function CastleGL_CLAMP_TO_EDGE: TGLenum;

{ Call glColor, taking Opacity as separate Single argument.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
{ @groupEnd }

{$undef read_interface}

implementation

{$define read_implementation}

uses CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders, CastleGLImages,
  CastleLog, CastleWarnings, CastleColors;

{$I glext_packed_depth_stencil.inc}
{$I glext_arb_framebuffer_object.inc}
{$I glext_ext_fog_coord.inc}

procedure LoadAllExtensions;
var
  GL_EXT_texture3D: boolean;
  GL_EXT_framebuffer_object: boolean;
  GL_ARB_framebuffer_object: boolean;
begin
  FreeAndNil(GLVersion);
  GLVersion := TGLVersion.Create(glGetString(GL_VERSION),
    glGetString(GL_VENDOR), glGetString(GL_RENDERER));

  FreeAndNil(GLUVersion);
  { gluGetString is valid for version 1.1 or later }
  if Assigned(gluGetString) then
    GLUVersion := TGenericGLVersion.Create(gluGetString(GLU_VERSION)) else
    GLUVersion := TGenericGLVersion.Create('1.0');

  GL_version_1_2 := GLVersion.AtLeast(1, 2) and Load_GL_version_1_2;
  GL_version_1_3 := GLVersion.AtLeast(1, 3) and Load_GL_version_1_3;
  GL_version_1_4 := GLVersion.AtLeast(1, 4) and Load_GL_version_1_4;
  GL_version_1_5 := GLVersion.AtLeast(1, 5) and Load_GL_version_1_5;
  GL_version_2_0 := GLVersion.AtLeast(2, 0) and Load_GL_version_2_0;
  {$ifdef HAS_GL_VERSION_ABOVE_2}
  GL_version_2_1 := GLVersion.AtLeast(2, 1) and Load_GL_version_2_1;
  GL_version_3_0 := GLVersion.AtLeast(3, 0) and Load_GL_version_3_0;
  GL_version_3_1 := GLVersion.AtLeast(3, 1) and Load_GL_version_3_1;
  GL_version_3_2 := GLVersion.AtLeast(3, 2) and Load_GL_version_3_2;
  GL_version_3_3 := GLVersion.AtLeast(3, 3) and Load_GL_version_3_3;
  GL_version_4_0 := GLVersion.AtLeast(4, 0) and Load_GL_version_4_0;
  {$endif}

  GL_ARB_imaging := Load_GL_ARB_imaging;
  GL_ARB_transpose_matrix := Load_GL_ARB_transpose_matrix;
  GL_ARB_multisample := Load_GL_ARB_multisample;
  GL_ARB_texture_env_add := Load_GL_ARB_texture_env_add;
  GL_ARB_texture_cube_map := Load_GL_ARB_texture_cube_map;
  GL_ARB_depth_texture := Load_GL_ARB_depth_texture;
  GL_ARB_point_parameters := Load_GL_ARB_point_parameters;
  GL_ARB_shadow := Load_GL_ARB_shadow;
  GL_ARB_shadow_ambient := Load_GL_ARB_shadow_ambient;
  GL_ARB_texture_border_clamp := Load_GL_ARB_texture_border_clamp;
  GL_ARB_texture_compression := Load_GL_ARB_texture_compression;
  GL_ARB_texture_env_combine := Load_GL_ARB_texture_env_combine;
  GL_ARB_texture_env_crossbar := Load_GL_ARB_texture_env_crossbar;
  GL_ARB_texture_env_dot3 := Load_GL_ARB_texture_env_dot3;
  GL_ARB_texture_mirrored_repeat := Load_GL_ARB_texture_mirrored_repeat;
  GL_ARB_vertex_blend := Load_GL_ARB_vertex_blend;
  GL_ARB_vertex_program := Load_GL_ARB_vertex_program;
  GL_ARB_window_pos := Load_GL_ARB_window_pos;
  GL_EXT_422_pixels := Load_GL_EXT_422_pixels;
  GL_EXT_abgr := Load_GL_EXT_abgr;
  GL_EXT_bgra := Load_GL_EXT_bgra;
  GL_EXT_blend_color := Load_GL_EXT_blend_color;
  GL_EXT_blend_func_separate := Load_GL_EXT_blend_func_separate;
  GL_EXT_blend_logic_op := Load_GL_EXT_blend_logic_op;
  GL_EXT_blend_minmax := Load_GL_EXT_blend_minmax;
  GL_EXT_blend_subtract := Load_GL_EXT_blend_subtract;
  GL_EXT_clip_volume_hint := Load_GL_EXT_clip_volume_hint;
  GL_EXT_color_subtable := Load_GL_EXT_color_subtable;
  GL_EXT_compiled_vertex_array := Load_GL_EXT_compiled_vertex_array;
  GL_EXT_convolution := Load_GL_EXT_convolution;
  GL_EXT_fog_coord := Load_GL_EXT_fog_coord;
  GL_EXT_histogram := Load_GL_EXT_histogram;
  GL_EXT_multi_draw_arrays := Load_GL_EXT_multi_draw_arrays;
  GL_EXT_packed_pixels := Load_GL_EXT_packed_pixels;
  GL_EXT_paletted_texture := Load_GL_EXT_paletted_texture;
  GL_EXT_point_parameters := Load_GL_EXT_point_parameters;
  GL_EXT_polygon_offset := Load_GL_EXT_polygon_offset;
  GL_EXT_secondary_color := Load_GL_EXT_secondary_color;
  GL_EXT_separate_specular_color := Load_GL_EXT_separate_specular_color;
  GL_EXT_shadow_funcs := Load_GL_EXT_shadow_funcs;
  GL_EXT_shared_texture_palette := Load_GL_EXT_shared_texture_palette;
  GL_EXT_stencil_two_side := Load_GL_EXT_stencil_two_side;
  GL_EXT_stencil_wrap := Load_GL_EXT_stencil_wrap;
  GL_EXT_subtexture := Load_GL_EXT_subtexture;
  GL_EXT_texture3D := Load_GL_EXT_texture3D;
  GL_EXT_texture_compression_s3tc := Load_GL_EXT_texture_compression_s3tc;
  GL_EXT_texture_env_add := Load_GL_EXT_texture_env_add;
  GL_EXT_texture_env_combine := Load_GL_EXT_texture_env_combine;
  GL_EXT_texture_env_dot3 := Load_GL_EXT_texture_env_dot3;
  GL_EXT_texture_filter_anisotropic := Load_GL_EXT_texture_filter_anisotropic;
  GL_EXT_texture_lod_bias := Load_GL_EXT_texture_lod_bias;
  GL_EXT_texture_object := Load_GL_EXT_texture_object;
  GL_EXT_vertex_array := Load_GL_EXT_vertex_array;
  GL_EXT_vertex_shader := Load_GL_EXT_vertex_shader;
  GL_EXT_vertex_weighting := Load_GL_EXT_vertex_weighting;
  GL_HP_occlusion_test := Load_GL_HP_occlusion_test;
  GL_NV_blend_square := Load_GL_NV_blend_square;
  GL_NV_copy_depth_to_color := Load_GL_NV_copy_depth_to_color;
  GL_NV_depth_clamp := Load_GL_NV_depth_clamp;
  GL_NV_evaluators := Load_GL_NV_evaluators;
  GL_NV_fence := Load_GL_NV_fence;
  GL_NV_fog_distance := Load_GL_NV_fog_distance;
  GL_NV_light_max_exponent := Load_GL_NV_light_max_exponent;
  GL_NV_multisample_filter_hint := Load_GL_NV_multisample_filter_hint;
  GL_NV_occlusion_query := Load_GL_NV_occlusion_query;
  GL_NV_packed_depth_stencil := Load_GL_NV_packed_depth_stencil;
  GL_NV_point_sprite := Load_GL_NV_point_sprite;
  GL_NV_register_combiners := Load_GL_NV_register_combiners;
  GL_NV_register_combiners2 := Load_GL_NV_register_combiners2;
  GL_NV_texgen_emboss := Load_GL_NV_texgen_emboss;
  GL_NV_texgen_reflection := Load_GL_NV_texgen_reflection;
  GL_NV_texture_compression_vtc := Load_GL_NV_texture_compression_vtc;
  GL_NV_texture_env_combine4 := Load_GL_NV_texture_env_combine4;
  GL_NV_texture_rectangle := Load_GL_NV_texture_rectangle;
  GL_NV_texture_shader := Load_GL_NV_texture_shader;
  GL_NV_texture_shader2 := Load_GL_NV_texture_shader2;
  GL_NV_texture_shader3 := Load_GL_NV_texture_shader3;
  GL_NV_vertex_array_range := Load_GL_NV_vertex_array_range;
  GL_NV_vertex_array_range2 := Load_GL_NV_vertex_array_range2;
  GL_NV_vertex_program := Load_GL_NV_vertex_program;
  GL_NV_vertex_program1_1 := Load_GL_NV_vertex_program1_1;
  GL_ATI_element_array := Load_GL_ATI_element_array;
  GL_ATI_envmap_bumpmap := Load_GL_ATI_envmap_bumpmap;
  GL_ATI_fragment_shader := Load_GL_ATI_fragment_shader;
  GL_ATI_pn_triangles := Load_GL_ATI_pn_triangles;
  GL_ATI_texture_mirror_once := Load_GL_ATI_texture_mirror_once;
  GL_ATI_vertex_array_object := Load_GL_ATI_vertex_array_object;
  GL_ATI_vertex_streams := Load_GL_ATI_vertex_streams;
  GL_3DFX_texture_compression_FXT1 := Load_GL_3DFX_texture_compression_FXT1;
  GL_IBM_cull_vertex := Load_GL_IBM_cull_vertex;
  GL_IBM_multimode_draw_arrays := Load_GL_IBM_multimode_draw_arrays;
  GL_IBM_raster_pos_clip := Load_GL_IBM_raster_pos_clip;
  GL_IBM_texture_mirrored_repeat := Load_GL_IBM_texture_mirrored_repeat;
  GL_IBM_vertex_array_lists := Load_GL_IBM_vertex_array_lists;
  GL_MESA_resize_buffers := Load_GL_MESA_resize_buffers;
  GL_MESA_window_pos := Load_GL_MESA_window_pos;
  GL_OML_interlace := Load_GL_OML_interlace;
  GL_OML_resample := Load_GL_OML_resample;
  GL_OML_subsample := Load_GL_OML_subsample;
  GL_SGIS_generate_mipmap := Load_GL_SGIS_generate_mipmap;
  GL_SGIS_multisample := Load_GL_SGIS_multisample;
  GL_SGIS_pixel_texture := Load_GL_SGIS_pixel_texture;
  GL_SGIS_texture_border_clamp := Load_GL_SGIS_texture_border_clamp;
  GL_SGIS_texture_color_mask := Load_GL_SGIS_texture_color_mask;
  GL_SGIS_texture_edge_clamp := Load_GL_SGIS_texture_edge_clamp;
  GL_SGIS_texture_lod := Load_GL_SGIS_texture_lod;
  GL_SGIS_depth_texture := Load_GL_SGIS_depth_texture;
  GL_SGIX_fog_offset := Load_GL_SGIX_fog_offset;
  GL_SGIX_interlace := Load_GL_SGIX_interlace;
  GL_SGIX_shadow_ambient := Load_GL_SGIX_shadow_ambient;
  GL_SGI_color_matrix := Load_GL_SGI_color_matrix;
  GL_SGI_color_table := Load_GL_SGI_color_table;
  GL_SGI_texture_color_table := Load_GL_SGI_texture_color_table;
  GL_SUN_vertex := Load_GL_SUN_vertex;
  GL_ARB_fragment_program := Load_GL_ARB_fragment_program;
  GL_ATI_text_fragment_shader := Load_GL_ATI_text_fragment_shader;
  GL_APPLE_client_storage := Load_GL_APPLE_client_storage;
  GL_APPLE_element_array := Load_GL_APPLE_element_array;
  GL_APPLE_fence := Load_GL_APPLE_fence;
  GL_APPLE_vertex_array_object := Load_GL_APPLE_vertex_array_object;
  GL_APPLE_vertex_array_range := Load_GL_APPLE_vertex_array_range;
  GL_ARB_matrix_palette := Load_GL_ARB_matrix_palette;
  GL_NV_element_array := Load_GL_NV_element_array;
  GL_NV_float_buffer := Load_GL_NV_float_buffer;
  GL_NV_fragment_program := Load_GL_NV_fragment_program;
  GL_NV_primitive_restart := Load_GL_NV_primitive_restart;
  GL_NV_vertex_program2 := Load_GL_NV_vertex_program2;
  GL_ATI_separate_stencil := Load_GL_ATI_separate_stencil;
  GL_ARB_texture_non_power_of_two := Load_GL_ARB_texture_non_power_of_two;
  GL_ARB_vertex_buffer_object := Load_GL_ARB_vertex_buffer_object;
  GL_EXT_framebuffer_object := Load_GL_EXT_framebuffer_object;
  GL_ARB_occlusion_query := Load_GL_ARB_occlusion_query;
  GL_EXT_packed_depth_stencil := Load_GL_EXT_packed_depth_stencil;
  GL_ATI_texture_float := Load_GL_ATI_texture_float;
  GL_ARB_texture_float := Load_GL_ARB_texture_float;
  GL_ARB_texture_rectangle := Load_GL_ARB_texture_rectangle;
  GL_ARB_framebuffer_object := Load_GL_ARB_framebuffer_object;

  { Workaround http://bugs.freepascal.org/view.php?id=18613 }
  if GL_ARB_vertex_buffer_object then
    Pointer(glBufferSubDataARB) := Glext.wglGetProcAddress('glBufferSubDataARB');

  GLMaxTextureSize := glGetInteger(GL_MAX_TEXTURE_SIZE);
  GLMaxLights := glGetInteger(GL_MAX_LIGHTS);

  if GL_version_1_3 then
    GLMaxTextureUnits := glGetInteger(GL_MAX_TEXTURE_UNITS) else
    GLMaxTextureUnits := 1;

  if GL_ARB_texture_cube_map then
    GLMaxCubeMapTextureSizeARB := glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB) else
    GLMaxCubeMapTextureSizeARB := 0;

  if GL_version_1_2 then
    GL3DTextures := gsStandard else
  if GL_EXT_texture3D then
    GL3DTextures := gsExtension else
    GL3DTextures := gsNone;

  { calculate GLMax3DTextureSize, eventually correct GL3DTextures if buggy }
  case GL3DTextures of
    gsExtension: GLMax3DTextureSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE_EXT);
    gsStandard : GLMax3DTextureSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE);
    gsNone     : GLMax3DTextureSize := 0;
  end;
  if (GLMax3DTextureSize = 0) and (GL3DTextures <> gsNone) then
  begin
    GL3DTextures := gsNone;
    if Log then WritelnLog('OpenGL', 'Buggy OpenGL 3D texture support: reported as supported, but GL_MAX_3D_TEXTURE_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
  end;

  if GL_EXT_texture_filter_anisotropic then
    GLMaxTextureMaxAnisotropyEXT := glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT) else
    GLMaxTextureMaxAnisotropyEXT := 0.0;

  if GL_ARB_occlusion_query then
    glGetQueryivARB(GL_SAMPLES_PASSED_ARB, GL_QUERY_COUNTER_BITS_ARB, @GLQueryCounterBits) else
    GLQueryCounterBits := 0;

  { calculate GLFramebuffer }
  if {$ifdef HAS_GL_VERSION_ABOVE_2} GL_version_3_0 or {$endif} GL_ARB_framebuffer_object then
    GLFramebuffer := gsStandard else
  if GL_EXT_framebuffer_object then
    GLFramebuffer := gsExtension else
    GLFramebuffer := gsNone;

  if GLFramebuffer <> gsNone then
  begin
    GLMaxRenderbufferSize := glGetInteger(GL_MAX_RENDERBUFFER_SIZE { equal to GL_MAX_RENDERBUFFER_SIZE_EXT });
    if GLMaxRenderbufferSize = 0 then
    begin
      GLFramebuffer := gsNone;
      if Log then WritelnLog('OpenGL', 'Buggy OpenGL Framebuffer: reported as supported, but GL_MAX_RENDERBUFFER_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    GLMaxRenderbufferSize := 0;

  if GL_ARB_texture_rectangle then
    GLMaxRectangleTextureSize := glGetInteger(GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB) else
    GLMaxRectangleTextureSize := 0;

  GLMaxClipPlanes := glGetInteger(GL_MAX_CLIP_PLANES);

  { calculate GLUseMultiTexturing: check extensions required for multitexturing.

    We used to require a couple of extensions for this:
    - EXT_texture_env_combine
    - ARB_multitexture
    - ARB_texture_env_dot3
    But GL version >= 1.3 is actually required for GL_subtract,
    and includes all above extensions in core. }
  GLUseMultiTexturing := GL_version_1_3;

  GLUseARBGLSL := Load_GL_ARB_shader_objects and
                  Load_GL_ARB_vertex_shader and
                  Load_GL_ARB_fragment_shader and
                  Load_GL_ARB_shading_language_100;

  GLFBOMultiSampling :=
    { Is GL_ARB_framebuffer_object available? }
    ({$ifdef HAS_GL_VERSION_ABOVE_2} GL_version_3_0 or {$endif} GL_ARB_framebuffer_object) and
    Load_GL_ARB_texture_multisample and
    (not GLVersion.BuggyFBOMultiSampling);

  if GL_ARB_multisample and (glGetInteger(GL_SAMPLE_BUFFERS_ARB) <> 0) then
  begin
    GLCurrentMultiSampling := glGetInteger(GL_SAMPLES_ARB);
    if GLCurrentMultiSampling <= 1 then
    begin
      OnWarning(wtMinor, 'MultiSampling', Format('We successfully got multi-sampling buffer, but only %d samples per pixel. This doesn''t make much sense, assuming buggy OpenGL implementation, and anti-aliasing may not work.',
        [GLCurrentMultiSampling]));
      GLCurrentMultiSampling := 1;
    end;
  end else
    GLCurrentMultiSampling := 1;

  GLPackedDepthStencil := GL_EXT_packed_depth_stencil;

  GLShadowVolumesPossible := glGetInteger(GL_STENCIL_BITS) >= 4;
end;

{ EOpenGLError, CheckGLErrors ------------------------------------------------ }

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string): string;
begin
  if AdditionalComment <> '' then
    Result := AdditionalComment + nl else
    Result := '';
  Result += Format('OpenGL error (%d): %s', [ErrorCode, gluErrorString(ErrorCode)]);
end;

constructor EOpenGLError.Create(const AErrorCode: TGLenum; const AdditionalComment: string);
begin
  ErrorCode := AErrorCode;
  inherited Create(GLErrorString(ErrorCode, AdditionalComment));
end;

procedure CheckGLErrors(const AdditionalComment: string);
var
  ErrorCode: TGLenum;
begin
  ErrorCode := glGetError();
  if ErrorCode <> GL_NO_ERROR then
    {$ifdef RELEASE}
    OnWarning(wtMajor, 'OpenGL', GLErrorString(ErrorCode, AdditionalComment));
    {$else}
    raise EOpenGLError.Create(ErrorCode, AdditionalComment);
    {$endif}
end;

procedure GLErrorRaise(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}
begin
  raise EOpenGLError.Create(ErrorCode);
end;

{ usprawnienia glGet ---------------------------------------------------------------------}

function glGetFloat(pname: TGLEnum): TGLfloat;
begin
  FillChar(result, SizeOf(result), 0);
  glGetFloatv(pname, @result)
end;

function glGetInteger(pname: TGLEnum): TGLint;
begin
  { Just for test, to somewhat simulate hardware with lower
    GL_MAX_TEXTURE_UNITS_ARB,
  if pname = GL_MAX_TEXTURE_UNITS_ARB then
  begin
    Result := 2;
    Exit;
  end;}

  FillChar(result, SizeOf(result), 0);
  glGetIntegerv(pname, @result);
end;

function glGetBoolean(pname: TGLEnum): TGLboolean;
begin
  FillChar(result, SizeOf(result), 0);
  glGetBooleanv(pname, @result)
end;

function glGetDouble(pname: TGLEnum): TGLdouble;
begin
  FillChar(result, SizeOf(result), 0);
  glGetDoublev(pname, @result)
end;

{ ---------------------------------------------------- }

procedure glColorv(const v: TVector3ub); begin glColor3ubv(@v); end;
procedure glColorv(const v: TVector4ub); begin glColor4ubv(@v); end;

procedure glTranslatev(const V: TVector3f); begin glTranslatef(V[0], V[1], V[2]); end;
procedure glTranslatev(const V: TVector3d); begin glTranslated(V[0], V[1], V[2]); end;

procedure glTranslatev(const V: TVector3_Single); begin glTranslatef(V.Data[0], V.Data[1], V.Data[2]); end;
procedure glTranslatev(const V: TVector3_Double); begin glTranslated(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glScalev(const V: Single); begin glScalef(V, V, V); end;

procedure glScalev(const V: TVector3f); begin glScalef(V[0], V[1], V[2]); end;
procedure glScalev(const V: TVector3d); begin glScaled(V[0], V[1], V[2]); end;

procedure glScalev(const V: TVector3_Single); begin glScalef(V.Data[0], V.Data[1], V.Data[2]); end;
procedure glScalev(const V: TVector3_Double); begin glScaled(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); begin glRotatef(Angle, V[0], V[1], V[2]); end;
procedure glRotatev(const Angle: TGLdouble; const V: TVector3d); begin glRotated(Angle, V[0], V[1], V[2]); end;

procedure glVertexv(const v: TVector2_Double);  begin glVertex2dv(@v.Data); end;
procedure glVertexv(const v: TVector2_Single);  begin glVertex2fv(@v.Data); end;

procedure glVertexv(const v: TVector3_Double);  begin glVertex3dv(@v.Data); end;
procedure glVertexv(const v: TVector3_Single);  begin glVertex3fv(@v.Data); end;

procedure glVertexv(const v: TVector4_Double);  begin glVertex4dv(@v.Data); end;
procedure glVertexv(const v: TVector4_Single);  begin glVertex4fv(@v.Data); end;

procedure glClipPlane(plane: GLenum; const V: TVector4d);
begin
  GL.glClipPlane(plane, @V);
end;

procedure gluLookAtv(const Eye, Center, Up: TVector3Single);
begin
  gluLookAt(Eye   [0], Eye   [1], Eye   [2],
            Center[0], Center[1], Center[2],
            Up    [0], Up    [1], Up    [2]);
end;

procedure gluLookDirv(const Eye, Dir, Up: TVector3Single);
begin
  gluLookAt(Eye[0]         , Eye[1]         , Eye[2],
            Eye[0] + Dir[0], Eye[1] + Dir[1], Eye[2] +Dir[2],
            Up [0]         , Up [1]         , Up [2]);
end;

procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2d);  begin glMultiTexCoord2dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2f);  begin glMultiTexCoord2fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3d);  begin glMultiTexCoord3dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3f);  begin glMultiTexCoord3fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4d);  begin glMultiTexCoord4dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4f);  begin glMultiTexCoord4fv(Target, @v); end;

procedure glClearColorv(const v: TVector3Single; alpha: Single);
begin
  glClearColor(v[0], v[1], v[2], alpha);
end;

procedure glNormalv(const v: TVector3d); begin glNormal3dv(@v); end;
procedure glNormalv(const v: TVector3f); begin glNormal3fv(@v); end;

procedure glColorv(const v: TVector3d);  begin glColor3dv(@v); end;
procedure glColorv(const v: TVector3f);  begin glColor3fv(@v); end;

procedure glColorv(const v: TVector4d);  begin glColor4dv(@v); end;
procedure glColorv(const v: TVector4f);  begin glColor4fv(@v); end;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f);  begin glMaterialfv(face, pname, @params); end;

procedure glVertexv(const v: TVector2d);  begin glVertex2dv(@v); end;
procedure glVertexv(const v: TVector2f);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector2i);  begin glVertex2iv(@v); end;
procedure glVertexv(const v: TVector3d);  begin glVertex3dv(@v); end;
procedure glVertexv(const v: TVector3f);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector3i);  begin glVertex3iv(@v); end;
procedure glVertexv(const v: TVector4d);  begin glVertex4dv(@v); end;
procedure glVertexv(const v: TVector4f);  begin glVertex4fv(@v); end;
procedure glVertexv(const v: TVector4i);  begin glVertex4iv(@v); end;

procedure glTexCoordv(const v: TVector2d);  begin glTexCoord2dv(@v); end;
procedure glTexCoordv(const v: TVector2f);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector3d);  begin glTexCoord3dv(@v); end;
procedure glTexCoordv(const v: TVector3f);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector4d);  begin glTexCoord4dv(@v); end;
procedure glTexCoordv(const v: TVector4f);  begin glTexCoord4fv(@v); end;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4d);  begin glTexGendv(coord, pname, @params); end;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f);  begin glTexGenfv(coord, pname, @params); end;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f);  begin glLightfv(light, pname, @params); end;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); begin glLightModelfv(pname, @params); end;
procedure glLightModelv(pname: TGLenum; const params: TVector4i); begin glLightModeliv(pname, @params); end;

procedure glFogv(pname: TGLEnum; const params: TVector4f);  begin glFogfv(pname, @params); end;

procedure glMultMatrix(const m: TMatrix4f); begin glMultMatrixf(@m) end;
procedure glMultMatrix(const m: TMatrix4d); begin glMultMatrixd(@m) end;
procedure glLoadMatrix(const m: TMatrix4f); begin glLoadMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4d); begin glLoadMatrixd(@m) end;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); begin glTexEnvfv(target, pname, @params); end;

{ uproszczenia dla sejwowania / ladowania gl state : ---------------------------------- }

procedure SavePixelStoreUnpack(out pixUnpack: TPixelStoreUnpack);
begin
  with pixUnpack do
  begin
    UnpackSwapBytes := glGetBoolean(GL_UNPACK_SWAP_BYTES);
    UnpackLSBFirst := glGetBoolean(GL_UNPACK_LSB_FIRST);
    UnpackRowLength := glGetInteger(GL_UNPACK_ROW_LENGTH);
    UnpackSkipRows := glGetInteger(GL_UNPACK_SKIP_ROWS);
    UnpackSkipPixels := glGetInteger(GL_UNPACK_SKIP_PIXELS);
    UnpackAlignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  end;
end;

procedure LoadPixelStoreUnpack(const pixUnpack: TPixelStoreUnpack);
begin
  with pixUnpack do
  begin
    glPixelStorei(GL_UNPACK_SWAP_BYTES, UnpackSwapBytes);
    glPixelStorei(GL_UNPACK_LSB_FIRST, UnpackLSBFirst);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, UnpackRowLength);
    glPixelStorei(GL_UNPACK_SKIP_ROWS,  UnpackSkipRows);
    glPixelStorei(GL_UNPACK_SKIP_PIXELS, UnpackSkipPixels);
    glPixelStorei(GL_UNPACK_ALIGNMENT, UnpackAlignment);
  end;
end;

procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TCastleImage);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, unpackData.Alignment);
end;

procedure BeforeUnpackNotAlignedRGBImage(out unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if (imageWidth*3 mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackNotAlignedRGBImage(const unpackData: TUnpackNotAlignedData; imageWidth: cardinal);
begin
  if (imageWidth*3 mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, unpackData.Alignment);
end;

procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TCastleImage);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackImage(const packData: TPackNotAlignedData; image: TCastleImage);
begin
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, packData.Alignment);
end;

procedure BeforePackNotAlignedRGBImage(out packdata: TPackNotAlignedData; imageWidth: cardinal);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (imageWidth*3 mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackNotAlignedRGBImage(const packData: TPackNotAlignedData; imageWidth: cardinal);
begin
  if (imageWidth*3 mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, packData.Alignment);
end;

{ projection matrix ---------------------------------------------------------- }

function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
begin
  glMatrixMode(GL_PROJECTION);
  Result := PerspectiveProjMatrixDeg(fovy, aspect, zNear, zFar);
  glLoadMatrix(Result);
  glMatrixMode(GL_MODELVIEW);
end;

function OrthoProjection(const left, right, bottom, top, zNear, zFar: Single): TMatrix4Single;
begin
  glMatrixMode(GL_PROJECTION);
  Result := OrthoProjMatrix(left, right, bottom, top, zNear, zFar);
  glLoadMatrix(Result);
  glMatrixMode(GL_MODELVIEW);
end;

{ Various helpers ------------------------------------------------------------ }

procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);
begin
  if isEnabled then glEnable(value) else glDisable(value);
end;

procedure GLVerticalLine(x, y1, y2: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x, y1); glVertex2f(x, y2); glEnd;
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x1, y); glVertex2f(x2, y); glEnd;
end;

procedure GLRectangleWithBorder(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f);
begin
  glColorv(InsideCol);
  glRectf(x1, y1, x2, y2);
  glColorv(BorderCol);
  GLRectangleBorder(x1, y1, x2, y2);
end;

procedure GLRectangleBorder(const x1, y1, x2, y2: TGLfloat);
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
  glEnd;
end;

procedure GLDrawArrow(HeadThickness, HeadLength: TGLfloat);
begin
  HeadLength := 2*HeadLength; { mapuj HeadLength na zakres 0..2 }

  { TODO: tutaj powinienes przelaczac glEdgeFlag }

  glBegin(GL_TRIANGLES);
    glVertex2f(0, 2);
    glVertex2f(-1, HeadLength);
    glVertex2f(-HeadThickness, HeadLength);

    glVertex2f(0, 2);
    glVertex2f(-HeadThickness, HeadLength);
    glVertex2f(HeadThickness, HeadLength);

    glVertex2f(0, 2);
    glVertex2f(HeadThickness, HeadLength);
    glVertex2f(1, HeadLength);
  glEnd;

  glBegin(GL_QUADS);
    glVertex2f(-HeadThickness, HeadLength);
    glVertex2f(-HeadThickness, 0);
    glVertex2f(HeadThickness, 0);
    glVertex2f(HeadThickness, HeadLength);
  glEnd;
end;

function NewGLUQuadric(texture: boolean; normals: TGLenum;
  orientation: TGLenum; drawStyle: TGLenum): PGLUQuadric;
begin
  result := gluNewQuadric();
  if result = nil then
    raise Exception.Create('gluNewQuadric cannot be created');
  gluQuadricCallback(result, GLU_ERROR, TCallBack(@GLErrorRaise));
  gluQuadricTexture(result, Ord(texture));
  gluQuadricNormals(result, normals);
  gluQuadricOrientation(result, orientation);
  gluQuadricDrawStyle(result, drawStyle);
end;

procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean; Normals: TGLenum;
  Orientation: TGLenum; DrawStyle: TGLenum);
var
  Q: PGLUQuadric;
begin
  Q := NewGLUQuadric(Texture, Normals, Orientation, DrawStyle);
  try
    gluSphere(Q, Radius, Slices, Stacks);
  finally gluDeleteQuadric(Q); end;
end;

procedure glDrawAxisWire(const Position: TVector3Single; Size: Single);
begin
  Size /= 2;
  glBegin(GL_LINES);
    glVertexv(Position - Vector3Single(Size, 0, 0));
    glVertexv(Position + Vector3Single(Size, 0, 0));
    glVertexv(Position - Vector3Single(0, Size, 0));
    glVertexv(Position + Vector3Single(0, Size, 0));
    glVertexv(Position - Vector3Single(0, 0, Size));
    glVertexv(Position + Vector3Single(0, 0, Size));
  glEnd;
end;

procedure glDrawBox3DWire(const Box: TBox3D);

  { BoxVertex(0..3, 0) are the four vertexes of front face,
    BoxVertex(0..3, 1) are the four vertexes of back face
    (ordered in the same order, suitable for GL_LINE_LOOP). }
  procedure BoxVertex(Index: Integer; Z: Integer);
  const
    X: array [0..3] of Integer = (0, 1, 1, 0);
    Y: array [0..3] of Integer = (0, 0, 1, 1);
  begin
    glVertex3f(Box.Data[X[Index], 0], Box.Data[Y[Index], 1], Box.Data[Z, 2]);
  end;

begin
  glBegin(GL_LINE_LOOP);
    BoxVertex(0, 0);
    BoxVertex(1, 0);
    BoxVertex(2, 0);
    BoxVertex(3, 0);
  glEnd;

  glBegin(GL_LINE_LOOP);
    BoxVertex(0, 1);
    BoxVertex(1, 1);
    BoxVertex(2, 1);
    BoxVertex(3, 1);
  glEnd;

  glBegin(GL_LINES);
    BoxVertex(0, 0); BoxVertex(0, 1);
    BoxVertex(1, 0); BoxVertex(1, 1);
    BoxVertex(2, 0); BoxVertex(2, 1);
    BoxVertex(3, 0); BoxVertex(3, 1);
  glEnd;
end;

procedure glDrawBox3DSimple(const Box: TBox3D);
var
  Verts: array [0..7] of TVector3Single;
const
  VertsIndices: array [0..23] of TGLuint =
  (
    0, 1, 3, 2,
    1, 5, 7, 3,
    5, 4, 6, 7,
    4, 0, 2, 6,
    2, 3, 7, 6,
    0, 4, 5, 1
  );
begin
  if Box.IsEmpty then Exit;

  { Verts index in octal notation indicates which of 8 vertexes it is. }
  Verts[0] := Box.Data[0];
  Verts[1] := Box.Data[0]; Verts[1][0] := Box.Data[1][0];
  Verts[2] := Box.Data[0]; Verts[2][1] := Box.Data[1][1];
  Verts[4] := Box.Data[0]; Verts[4][2] := Box.Data[1][2];

  Verts[3] := Box.Data[1]; Verts[3][2] := Box.Data[0][2];
  Verts[5] := Box.Data[1]; Verts[5][1] := Box.Data[0][1];
  Verts[6] := Box.Data[1]; Verts[6][0] := Box.Data[0][0];
  Verts[7] := Box.Data[1];

  glVertexPointer(3, GL_FLOAT, 0, @Verts);

  if GL_EXT_compiled_vertex_array then
    glLockArraysEXT(0, 8);

  glDrawElements(GL_QUADS, 6 * 4, GL_UNSIGNED_INT, @VertsIndices);

  if GL_EXT_compiled_vertex_array then
    glUnlockArraysEXT;
end;

{$define PROJECTION_PUSH_POP_BEGIN:=
var
  oldMatrixMode: TGLenum;
begin
  oldMatrixMode := glGetInteger(GL_MATRIX_MODE);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  try}

{$define PROJECTION_PUSH_POP_END:=
    glMatrixMode(oldMatrixMode);
    proc(data);
  finally
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(oldMatrixMode);
  end;
end;}

procedure glProjectionPushPop(proc: TProcData; Data: Pointer;
  const projMatrix: TMatrix4f);
PROJECTION_PUSH_POP_BEGIN
  glLoadMatrixf(@projMatrix);
PROJECTION_PUSH_POP_END

procedure glProjectionPushPopOrtho(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top, ZNear, ZFar: TGLdouble);
PROJECTION_PUSH_POP_BEGIN
  glLoadIdentity;
  glOrtho(Left, Right, Bottom, Top, ZNear, ZFar);
PROJECTION_PUSH_POP_END

procedure glProjectionPushPopOrtho2D(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top: TGLdouble);
PROJECTION_PUSH_POP_BEGIN
  glLoadIdentity;
  gluOrtho2D(Left, Right, Bottom, Top);
PROJECTION_PUSH_POP_END

procedure glProjectionPushPopPerspective(proc: TProcData; Data: Pointer;
  const FovyDeg, Aspect, ZNear, ZFar: TGLdouble);
PROJECTION_PUSH_POP_BEGIN
  glLoadIdentity;
  gluPerspective(FovyDeg, Aspect, ZNear, ZFar);
PROJECTION_PUSH_POP_END

{$undef PROJECTION_PUSH_POP_BEGIN}
{$undef PROJECTION_PUSH_POP_END}

procedure glProjectionPushPop2D(proc: TProcData; Data: Pointer);
var
  Viewport: TVector4i;
begin
  glGetIntegerv(GL_VIEWPORT, @viewport);

  { Other version is to use here
      glProjectionPushPop(proc, data,
        OrthoProjMatrix(0, viewport[2], 0, viewport[3], -1, 1));
    but causing OpenGL to calculate it's matrices may be faster. }

  glProjectionPushPopOrtho2D(proc, data, 0, viewport[2], 0, viewport[3]);
end;

procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single; const FadeIntensity: Single);
const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  { We assume that MinScale is small enough that difference between
    "FadeColor * MinScale * screen color" and
    "MinScale * screen color" is not noticeable. }
  MinScale = 0.5;
  { Constants below make resulting screen color = glColor * previous screen color.
    Note that as long as all components of FadeColor are <= 1,
    then all components of our glColor are also always <= 1,
    and this means that we will always make the screen darker (or equal,
    but never brighter). }
  SourceFactor = GL_ZERO;
  DestinationFactor = GL_SRC_COLOR;
var
  Color4: TVector4Single;
  Color3: TVector3Single absolute Color4;
begin
  if FadeIntensity > 0 then
  begin
    { for FadeIntensity in 1...FullWhiteEnd (going down):
      screen color := FadeColor * screen color }
    if FadeIntensity > FullWhiteEnd then
      Color3 := FadeColor else
    { for FadeIntensity in FullWhiteEnd...FullBlack (going down):
      screen color := FadeColor * screen color ...
        FadeColor * MinScale * screen color }
    if FadeIntensity > FullBlack then
      Color3 := FadeColor * MapRange(FadeIntensity, FullWhiteEnd, FullBlack, 1, MinScale) else
    { for FadeIntensity in FullBlack...0 (going down):
      screen color := MinScale * screen color ...
        unchanged screen color }
      Color3 := White3Single * MapRange(FadeIntensity, FullBlack, 0, MinScale, 1);

    Color4[3] := 1.0; { alpha always 1.0 in this case }
    GLBlendRectangle(X1, Y1, X2, Y2, SourceFactor, DestinationFactor, Color4);
  end;
end;

procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor, DestinationFactor: TGLenum;
  const Color: TVector4Single);
begin
  glPushAttrib(GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT);
    glEnable(GL_BLEND);
      glBlendFunc(SourceFactor, DestinationFactor);
      glColorv(Color);
      glBegin(GL_QUADS);
        glVertex2i(X1, Y1);
        glVertex2i(X2, Y1);
        glVertex2i(X2, Y2);
        glVertex2i(X1, Y2);
      glEnd();
    glDisable(GL_BLEND);
  glPopAttrib;
end;

function GLInformationString: string;
const
  GLSupportNamesFBO: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard (or ARB "core extension")' );

  function GetInteger(param: TGLenum): string;
  begin
    Result := IntToStr(glGetInteger(param));
  end;

  function GetInteger2(param: TGLenum; const form: string): string;
  var
    v: packed array [0..1] of TGLint;
  begin
    glGetIntegerv(param, @v);
    result := Format(form, [v[0], v[1]]);
  end;

  function GetBoolean(param: TGLenum): string;
  begin
    Result := BoolToStr[glGetInteger(Param) = GL_TRUE];
  end;

  function VersionReport(Version: TGenericGLVersion): string;
  begin
    Result := Format('  Version parsed: major: %d, minor: %d, release exists: %s, ' +
      'release: %d, vendor-specific version: "%s"',
      [ Version.Major, Version.Minor, BoolToStr[Version.ReleaseExists],
        Version.Release,  Version.VendorVersion ]);
  end;

  function VendorReport(Version: TGLVersion): string;
  begin
    Result :=
      Format(
        '  Vendor NVidia: %s' +nl+
        '  Vendor ATI: %s (fglrx: %s)' +nl+
        '  Vendor Intel: %s' +nl+
        '  Mesa: %s (Mesa parsed version major: %d, minor: %d, release: %d)' +nl+
        nl+
        '  Buggy glPushAttrib(GL_POINT_SET): %s' +nl+
        '  Buggy glDrawPixels for odd widths: %s' +nl+
        '  Buggy glGenerateMipmapEXT: %s' +nl+
        '  Buggy GL_LIGHT_MODEL_TWO_SIDE: %s' +nl+
        '  Buggy VBO: %s' +nl+
        '  Buggy shader shadow map: %s' +nl+
        '  Buggy GLSL "const in gl_XxxParameters" declaration: %s' +nl+
        '  Buggy FBO rendering to multi-sampling texture: %s' +nl+
        '  Buggy FBO rendering to cube map texture: %s' +nl+
        '  Buggy swap buffers with non-standard glViewport: %s' +nl+
        '  Buggy 32-bit depth buffer: %s',
        [ BoolToStr[Version.VendorNVidia],
          BoolToStr[Version.VendorATI],
          BoolToStr[Version.Fglrx],
          BoolToStr[Version.VendorIntel],
          BoolToStr[Version.Mesa],
          Version.MesaMajor, Version.MesaMinor, Version.MesaRelease,

          BoolToStr[Version.BuggyPointSetAttrib],
          BoolToStr[Version.BuggyDrawOddWidth],
          BoolToStr[Version.BuggyGenerateMipmap],
          BoolToStr[Version.BuggyLightModelTwoSide],
          BoolToStr[Version.BuggyVBO],
          BoolToStr[Version.BuggyShaderShadowMap],
          BoolToStr[Version.BuggyGLSLConstStruct],
          BoolToStr[Version.BuggyFBOMultiSampling],
          BoolToStr[Version.BuggyFBOCubeMap],
          BoolToStr[Version.BuggySwapNonStandardViewport],
          BoolToStr[Version.BuggyDepth32]
        ]);
  end;

  function GetMaxCubeMapTextureSize: string;
  begin
    if GL_ARB_texture_cube_map then
      Result := IntToStr(GLMaxCubeMapTextureSizeARB) else
      Result := 'ARB_texture_cube_map not available';
  end;

  function GetMaxTexture3DSize: string;
  begin
    if GL3DTextures <> gsNone then
      Result := IntToStr(GLMax3DTextureSize) else
      Result := '3D textures not available';
  end;

  function GetMaxTextureMaxAnisotropy: string;
  begin
    if GL_EXT_texture_filter_anisotropic then
      Result := FloatToStr(GLMaxTextureMaxAnisotropyEXT) else
      Result := 'EXT_texture_filter_anisotropic not available';
  end;

  function GetSampleBuffers: string;
  begin
    if GL_ARB_multisample then
      Result := GetBoolean(GL_SAMPLE_BUFFERS_ARB) else
      Result := 'GL_ARB_multisample not available';
  end;

  function GetSamples: string;
  begin
    if GL_ARB_multisample then
      Result := GetInteger(GL_SAMPLES_ARB) else
      Result := 'GL_ARB_multisample not available';
  end;

  function GetQueryCounterBits: string;
  begin
    if GL_ARB_occlusion_query then
      Result := IntToStr(GLQueryCounterBits) else
      Result := 'ARB_occlusion_query not available';
  end;

  function GetMaxRenderbufferSize: string;
  begin
    if GLFramebuffer <> gsNone then
      Result := IntToStr(GLMaxRenderbufferSize) else
      Result := 'Framebuffer not available';
  end;

  function GetMaxRectangleTextureSize: string;
  begin
    if GL_ARB_texture_rectangle then
      Result := IntToStr(GLMaxRectangleTextureSize) else
      Result := 'ARB_texture_rectangle not available';
  end;

begin
  Result:=
    'OpenGL information (detected by ' + ProgramName +'):' +nl+
    nl+

    '--------' +nl+
    'Version:' +nl+
    '  Version string: ' +glGetString(GL_VERSION) +nl+
    VersionReport(GLVersion) +nl+
    '  Vendor: ' +glGetString(GL_VENDOR) +nl+
    '  Renderer: ' +glGetString(GL_RENDERER) +nl+
    VendorReport(GLVersion) +nl+
    nl+

    '------------------------' +nl+
    'Real versions available:' +nl+
    '(checks both version string and actual functions availability in GL library, to secure from buggy OpenGL implementations)' +nl+
    nl+
    '  1.2: ' + BoolToStr[GL_version_1_2] +nl+
    '  1.3: ' + BoolToStr[GL_version_1_3] +nl+
    '  1.4: ' + BoolToStr[GL_version_1_4] +nl+
    '  1.5: ' + BoolToStr[GL_version_1_5] +nl+
    '  2.0: ' + BoolToStr[GL_version_2_0] +nl+
    {$ifdef HAS_GL_VERSION_ABOVE_2}
    '  2.1: ' + BoolToStr[GL_version_2_1] +nl+
    '  3.0: ' + BoolToStr[GL_version_3_0] +nl+
    '  3.1: ' + BoolToStr[GL_version_3_1] +nl+
    '  3.2: ' + BoolToStr[GL_version_3_2] +nl+
    '  3.3: ' + BoolToStr[GL_version_3_3] +nl+
    '  4.0: ' + BoolToStr[GL_version_4_0] +nl+
    {$endif}
    nl+

    '---------' +nl+
    'Features:' +nl+
    '  GLSL shaders support: ' + GLSupportNames[TGLSLProgram.ClassSupport] +nl+
    '  Assembly ARB vertex program support: ' + GLSupportNames[TARBVertexProgram.ClassSupport] +nl+
    '  Assembly ARB fragment program support: ' + GLSupportNames[TARBFragmentProgram.ClassSupport] +nl+
    '  Multi-texturing: ' + BoolToStr[GLUseMultiTexturing] +nl+
    '  Framebuffer Object: ' + GLSupportNamesFBO[GLFramebuffer] +nl+
    '  Vertex Buffer Object: ' + BoolToStr[GL_ARB_vertex_buffer_object] +nl+
    '  GenerateMipmap available: ' + BoolToStr[HasGenerateMipmap] +nl+
    '  S3TC compressed textures: ' + BoolToStr[GL_ARB_texture_compression and GL_EXT_texture_compression_s3tc] +nl+
    '  3D textures: ' + GLSupportNames[GL3DTextures] +nl+
    '  Multi-sampling for FBO buffers and textures: ' + BoolToStr[GLFBOMultiSampling] +nl+
    nl+
    '  All extensions: ' +glGetString(GL_EXTENSIONS) +nl+
    nl+

    '-----------------------------' +nl+
    'OpenGL utility (GLU) version:' +nl+
    '  Version string: ' +gluGetString(GLU_VERSION) +nl+
    VersionReport(GLUVersion) +nl+
    '  Extensions: '+gluGetString(GLU_EXTENSIONS) +nl+
    nl+

    '---------------------------' +nl+
    'Current buffers bit depths:' +nl+
    '  Color (red / greeen / blue / alpha): '
      +GetInteger(GL_RED_BITS) +' / '
      +GetInteger(GL_GREEN_BITS) +' / '
      +GetInteger(GL_BLUE_BITS) +' / '
      +GetInteger(GL_ALPHA_BITS) +nl+
    '  Depth: ' +GetInteger(GL_DEPTH_BITS) +nl+
    '  Index: ' +GetInteger(GL_INDEX_BITS) +nl+
    '  Stencil: ' +GetInteger(GL_STENCIL_BITS) +nl+
    '  Accumulation (red / greeen / blue / alpha): '
      +GetInteger(GL_ACCUM_RED_BITS) +' / '
      +GetInteger(GL_ACCUM_GREEN_BITS) +' / '
      +GetInteger(GL_ACCUM_BLUE_BITS) +' / '
      +GetInteger(GL_ACCUM_ALPHA_BITS) +nl+
    '  Double buffer: ' + GetBoolean(GL_DOUBLEBUFFER) +nl+
    '  Multisampling (full-screen antialiasing):' +nl+
    '    Sample buffers: ' + GetSampleBuffers +nl+
    '    Samples: ' + GetSamples +nl+
    '    Summary: ' + IntToStr(GLCurrentMultiSampling) + ' samples per pixel' +nl+
    nl+

    '-------------' +nl+
    'Stack depths:' +nl+
    '  Attributes: ' +GetInteger(GL_MAX_ATTRIB_STACK_DEPTH) +nl+
    '  Client attributes: ' +GetInteger(GL_MAX_CLIENT_ATTRIB_STACK_DEPTH) +nl+
    '  Modelview: ' +GetInteger(GL_MAX_MODELVIEW_STACK_DEPTH) +nl+
    '  Projection: ' +GetInteger(GL_MAX_PROJECTION_STACK_DEPTH) +nl+
    '  Texture: ' +GetInteger(GL_MAX_TEXTURE_STACK_DEPTH) +nl+
    '  Name: ' +GetInteger(GL_MAX_NAME_STACK_DEPTH) +nl+
    nl+

    '-------' +nl+
    'Limits:' +nl+
    '  Max clip planes: ' + IntToStr(GLMaxClipPlanes) +nl+
    '  Max eval order: ' +GetInteger(GL_MAX_EVAL_ORDER) +nl+
    '  Max lights: ' + IntToStr(GLMaxLights) +nl+
    '  Max list nesting: ' +GetInteger(GL_MAX_LIST_NESTING) +nl+
    '  Max pixel map table: ' +GetInteger(GL_MAX_PIXEL_MAP_TABLE) +nl+
    '  Max texture size: ' + IntToStr(GLMaxTextureSize) +nl+
    '  Max viewport dims: ' +GetInteger2(GL_MAX_VIEWPORT_DIMS, 'width %d / height %d') +nl+
    '  Max texture units: ' + IntToStr(GLMaxTextureUnits) +nl+
    '  Max cube map texture size: ' + GetMaxCubeMapTextureSize +nl+
    '  Max 3d texture size: ' + GetMaxTexture3DSize +nl+
    '  Max rectangle texture size: ' + GetMaxRectangleTextureSize +nl+
    '  Max texture max anisotropy: ' + GetMaxTextureMaxAnisotropy +nl+
    '  Query counter bits (for occlusion query): ' + { for occlusion query  GL_SAMPLES_PASSED_ARB }
      GetQueryCounterBits +nl+
    '  Max renderbuffer size: ' + GetMaxRenderbufferSize;

   CheckGLErrors;
end;

function glGenListsCheck(range: TGLsizei; const Place: string): TGLuint;
begin
  Result := glGenLists(range);
  if (Result = 0) and (range <> 0) then
    raise EOpenGLNoMoreDisplayLists.CreateFmt(
      'No more OpenGL display lists available when trying to allocate new %d display lists from "%s". This may mean that GPU memory is full (low possibility, unless you used some ridiculous number of display lists), or that OpenGL context is not initialized yet', [range, Place]);
end;

procedure glFreeDisplayList(var list: TGLuint);
begin
 if list <> 0 then
 begin
  glDeleteLists(list, 1);
  list := 0;
 end;
end;

procedure glFreeTexture(var Tex: TGLuint);
begin
  if Tex <> 0 then
  begin
    glDeleteTextures(1, @Tex);
    Tex := 0;
  end;
end;

procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);
begin
  glDepthMask(Writeable);
  glColorMask(Writeable, Writeable, Writeable, Writeable);
end;

procedure SetWindowPos_HackBegin;
begin
  { Idea how to implement this --- see
    [http://www.opengl.org/resources/features/KilgardTechniques/oglpitfall/]. }

  glPushAttrib(GL_TRANSFORM_BIT);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
        glLoadIdentity;
end;

procedure SetWindowPos_HackEnd;
begin
      glPopMatrix;
      glMatrixMode(GL_PROJECTION);
    glPopMatrix;
  glPopAttrib;
end;

procedure SetWindowPos(const X, Y: TGLfloat);
begin
  if GL_version_1_4 then
  begin
    glWindowPos2f(X, Y);
    { tests: Writeln('using std'); }
  end else
  if GL_ARB_window_pos then
  begin
    glWindowPos2fARB(X, Y);
    { tests: Writeln('using ARB'); }
  end else
  if GL_MESA_window_pos then
  begin
    glWindowPos2fMESA(X, Y);
    { tests: Writeln('using MESA'); }
  end else
  begin
    SetWindowPos_HackBegin;

    glViewport(Floor(X) - 1, Floor(Y) - 1, 2, 2);
    glRasterPos4f(Frac(X), Frac(Y), 0, 1);

    SetWindowPos_HackEnd;
  end;
end;

procedure SetWindowPos(const X, Y: TGLint);
begin
  if GL_version_1_4 then
    glWindowPos2i(X, Y) else
  if GL_ARB_window_pos then
    glWindowPos2iARB(X, Y) else
  if GL_MESA_window_pos then
    glWindowPos2iMESA(X, Y) else
  begin
    SetWindowPos_HackBegin;

    glViewport(X - 1, Y - 1, 2, 2);
    glRasterPos2i(0, 0);

    SetWindowPos_HackEnd;
  end;
end;

procedure SetWindowPosZero;
begin
  if GL_version_1_4 then
    glWindowPos2i(0, 0) else
  if GL_ARB_window_pos then
    glWindowPos2iARB(0, 0) else
  if GL_MESA_window_pos then
    glWindowPos2iMESA(0, 0) else
  begin
    SetWindowPos_HackBegin;

    glViewport(-1, -1, 2, 2);
    glRasterPos2i(0, 0);

    SetWindowPos_HackEnd;
  end;
end;

function CastleGL_CLAMP_TO_EDGE: TGLenum;
begin
  if GL_version_1_2 then
    Result := GL_CLAMP_TO_EDGE else
    Result := GL_CLAMP;
end;

procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
begin
  glColor4f(Color[0], Color[1], Color[2], Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color[0] / 255, Color[1] / 255, Color[2] / 255, Opacity);
end;

initialization
  { This is needed for gllistIBase declaration to be correct. }
  Assert(SizeOf(TGLint) = SizeOf(TGLuint));

  { This Set8087CW is actually not needed, because FPC GL units,
    since version 2.2.2, already do this, for all necessary platforms,
    thanks to Michalis bug reports :) See
    - http://www.freepascal.org/mantis/view.php?id=5914
    - http://www.freepascal.org/mantis/view.php?id=7570
    - http://bugs.freepascal.org/view.php?id=10507

    The purpose of this Set8087CW is to mask (filter out, ignore)
    all floating point exceptions.
    This sucks, but it's the fault of OpenGL implementations and we can't
    do anything about it:

    - Windows:

      In GLUT faq we can read explanation: this is because Microsoft's
      OpenGL implementations can raise FPU exceptions doing floating
      point operations.
      Microsoft compilers, like Visual C++, by default ignore FPU
      exceptions so nothing needs to be done when you compile programs
      in Visual C++. But Borland's compilers (at least Delphi and
      C++Builder) and FPC don't mask FPU exceptions - by default, they convert
      them to normal C++/Pascal exceptions. So one should explicitly mask
      FPU exceptions before using any OpenGL routine under Windows.

    - Linux:

      Radeon open-source OpenGL driver may cause EDivByZero exceptions.
      Reported by Daniel Mantione with Radeon Mobility M7,
      EDivByZero was raised by glCallList inside TCastleScene (not used there
      anymore).
      Disabling fp exceptions fixed the problem.

      NVidia proprietary drivers exit with EDivByZero on the first
      glXMakeCurrent call (done also by glut in first glutCreateWindow,
      done also by GTK glarea in first gtk_glarea_make_current)
      when no depth buffer was requested.

    Mesa3d doesn't cause such problems.
    So maybe it's possible to make an efficient OpenGL
    implementation that doesn't require caller to disable fp exceptions?
  }
  Set8087CW($133F);

  Pointer(glListIBase) := glListBase;
end.
