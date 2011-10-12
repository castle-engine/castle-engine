{
  Copyright 2001-2011 Michalis Kamburelis.

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

uses Math, GL, GLU, GLExt, SysUtils, CastleUtils, VectorMath, Boxes3D,
  Images, Matrix, Rectangles;

{$define read_interface}

type
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

{$I glext_packed_depth_stencil.inc}
{$I glext_arb_framebuffer_object.inc}
{$I glext_ext_fog_coord.inc}

{ ------------------------------------------------------------ }
{ @section(Utils needed only when using GL, GLU, GLExt bindings.
  Not needed when using OpenGLh binding.) }

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

var
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

  { Are all OpenGL ARB extesions for GLSL available. }
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

  TPolygonStipple = packed array[0..(32*32 div 8)-1]of TGLubyte;
  PPolygonStipple = ^TPolygonStipple;

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
  Suitable for registering as GLU_TESS_ERROR for gluTessCallback,
  or GLU_ERROR for gluQuadricCallback. }
procedure ReportGLError(ErrorCode: TGLenum);
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

{ Save/restore OpenGL pixel store for unpacking / packing given TImage.
  Before you pass this image to some OpenGL procedures
  (like glDrawPixels for unpacking, glReadPixels for packing),
  call BeforeXxx, and later call AfterXxx to restore original state.
  These will take care of setting/restoring pixel alignment.
  @groupBegin }
procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TImage);
procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TImage);
procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TImage);
procedure AfterPackImage(const packData: TPackNotAlignedData; image: TImage);
{ @groupEnd }

{ Projection matrix -------------------------------------------------------- }

{ Load correspoding OpenGL matrices
  (gluPerspective, glOrtho), making sure we're in GL_PROJECTION matrix mode.

  First these change current matrix
  mode (if needed) to GL_PROJECTION. Then they load identity,
  and then call appropriate OpenGL functions (gluPerspective, glOrtho).
  And then (if needed) go back to previous matrix mode.
  So current matrix mode is never changed by these procedures.

  Also, ZFar is allowed to have special ZFarInfinity value
  for ProjectionGLPerspective.
  Then we set special perspective matrix, that has far plane set
  at infinity --- useful for z-fail shadow volumes.

  @groupBegin }
procedure ProjectionGLPerspective(const fovy, aspect, zNear, zFar: TGLdouble);
procedure ProjectionGLOrtho(const left, right, bottom, top: TGLdouble;
  const zNear: TGLdouble = -1; const zFar: TGLdouble = 1);
{ @groupEnd }

{ ------------------------------------------------------------ }
{ @section(Helpers for polygon stipple) }

const
  { }
  HalftoneStipple: TPolygonStipple=
  ( $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55,
    $AA, $AA, $AA, $AA,  $55, $55, $55, $55,  $AA, $AA, $AA, $AA,  $55, $55, $55, $55
  );

  ThreeQuartersStipple: TPolygonStipple=
  ( $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB,
    $DD, $DD, $DD, $DD,  $77, $77, $77, $77,  $EE, $EE, $EE, $EE,  $BB, $BB, $BB, $BB
  );

{ Generate random stipple, with a given probability of bit being 1. }
function RandomPolyStipple(const BlackChance: Extended): TPolygonStipple;

{ Generate random stipple with each quarter (16x16 pixels) equal.
  This makes more regular stipple than RandomPolyStipple. }
function RandomPolyStippleBy16(const BlackChance: Extended): TPolygonStipple;

{ Generate random stipple with each  8x8 part equal.
  This makes even more regular stipple than RandomPolyStippleBy16. }
function RandomPolyStippleBy8(const BlackChance: Extended): TPolygonStipple;

var
  { Equivalent to glPolygonStipple, but takes PPolygonStipple as a parameter. }
  CastleGLPolygonStipple: procedure(mask: PPolygonStipple);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}

{ ---------------------------------------------------------------------------- }

{ }
procedure SetGLEnabled(value: TGLenum; isEnabled: boolean);

{ Draw vertical line using OpenGL.
  This is just a shortcut for
  @longCode(#
    glBegin(GL_LINES); glVertex2f(x, y1); glVertex2f(x, y2); glEnd;
  #) }
procedure VerticalGLLine(x, y1, y2: TGLfloat);

{ Draw horizontal line using OpenGL.
  @seealso VerticalGLLine }
procedure HorizontalGLLine(x1, x2, y: TGLfloat);

{ Draw rectangle, filled with one color and framed with other color.
  The vertex order is the same as for glRectf.
  Requires one attrib stack place, because it makes sure
  that polygon mode FRONT_AND_BACK is GL_FILL.

  Changes OpenGL current color.

  Overloaded version with a Stipple parameter sets this stipple.
  If Stipple <> nil, then GL_POLYGON_STIPPLE will be enabled
  and set by glPolygonStipple. If Stipple = nil, then GL_POLYGON_STIPPLE
  will be disabled. Requires one more attrib stack place.

  @groupBegin }
procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f); overload;
procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f; Stipple: PPolygonStipple); overload;
{ @groupEnd }

{ Draw rectangle border.
  The vertex order is the same as for glRectf.

  Uses current OpenGL color.
  @groupBegin }
procedure DrawGLRectBorder(const x1, y1, x2, y2: TGLfloat); overload;
procedure DrawGLRectBorder(const Rectangle: TRectangle); overload;
{ @groupEnd }

function UnProjectGL(winx, winy, winz: TGLdouble): TVector3d;

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).
  Everything is drawn CCW when seen from standard view
  (x grows right, y up). }
procedure DrawArrow(HeadThickness: TGLfloat = 0.4;
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

{ Draw rectangle, on a given plane, possibly by using many smaller quads.

  ConstCoord (0, 1 or 2) determines
  the plane of the rectangle, all points have this coordinate = ConstValue.
  So parameters X1, Y1, X2, Y2 are not necessarily for XY plane:
  when ConstCoord = 0, then X1,X2 parameters are for Y plane,
  and Y1,Y2 are for Z plane. See RestOf3dCoords.

  Normal vector is generated, always from CCW side of the quads.
  Assuming that X1<=X2 and Y1<=Y2: If ConstCoordGivesNormal1 then
  the normal points to positive ConstCoord, otherwise to negative.

  If DetailLevel1 <> 0 or DetailLevel2 <> 0 then rectangle is actually
  rendered by many smaller polygons. This improved the look under
  Gouraud shading. We use DetailLevelX + 1 columns and
  DetailLevelY + 1 rows, so actually we render
  @code((DetailLevelX + 1) * (DetailLevelY + 1)) OpenGL quads.

  Texture coordinates are generated if MakeTextureCoords.

  Tex* parameters configure what texture coordinates are assigned to
  rectangle corners.

  Order_ST_XYZ means that the order of XYZ coordinates and texture ST
  coordinates is the same. For example, if ConstCoord = 2,
  then S texture coord with increase along X,
  and T texture coord with increase along Y.
  If Order_ST_XYZ, then T texture coord with increase along X,
  and S texture coord with increase along Y. }
procedure DrawGLPlane(X1, Y1, X2, Y2: TGLfloat; ConstValue: TGLfloat;
  ConstCoord, DetailLevelX, DetailLevelY: integer;
  ConstCoordGivesNormal1: boolean;
  MakeTextureCoords: boolean;
  texX1: TGLfloat = 0; texY1: TGLfloat = 0;
  texX2: TGLfloat = 1; texY2: TGLfloat = 1;
  order_ST_XYZ: boolean = true);

{ Draw axis-aligned box. Pass coordinates for box corners
  (X1 must be <= X2 etc.), or explicit TBox3D value.

  Sides are drawn by DrawGLPlane, so may be splitted into more quads for
  nice Gouraud shading if Detail* are <> 0.

  Correct normal vectors are generated, from CCW.
  If CcwOutside then CCW sides (and so the normals) point outside of the box,
  otherwise they point inside.

  Texture coordinates are generated if MakeTextureCoords. }
procedure DrawGLBox(const Box: TBox3D; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean; MakeTextureCoords: boolean); overload;
procedure DrawGLBox(const x1, y1, z1, x2, y2, z2: TGLfloat; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean; MakeTextureCoords: boolean); overload;

{ Draws a simple lines around this TBox3D.
  It doesn't generate any texture coords or normal vectors
  --- it only draws 8 lines. }
procedure glDrawBox3DWire(const Box: TBox3D);

{ Draw simple box. Nothing is generated besides vertexes position ---
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

{ Draw triangle with given corners and texture coordinates.
  Normal vector is generated from CCW side.

  The triangle may b split into smaller triangles for rendering,
  for nicer Gouraud shading. }
procedure DrawGLTriangle(const p1, p2, p3: TVector3f;
  const Tex1, Tex2, Tex3: TVector2f;
  DetailLev: Cardinal);

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

{ Draw rectangle that colors (using blending) everything underneath with
  given BlackOutColor. BlackOutIntensity is the effect intensity:
  1 is the total coverage by BlackOutColor, 0 (and below) means nothing.

  The rectangle is affected by current modelview matrix.
  Requires one attrib stack place.

  You can place this inside display list. }
procedure DrawGLBlackOutRect(const BlackOutColor: TVector3f;
  const BlackOutIntensity, x1, y1, x2, y2: TGLfloat);

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

{$undef read_interface}

implementation

{$define read_implementation}

uses CastleFilesUtils, CastleStringUtils, GLVersionUnit, GLShaders, GLImages,
  CastleLog, CastleWarnings;

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

  { Workaround http://bugs.freepascal.org/view.php?id=18613 }
  if GL_ARB_vertex_buffer_object then
    Pointer(glBufferSubDataARB) := Glext.wglGetProcAddress('glBufferSubDataARB');
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

procedure ReportGLError(ErrorCode: TGLenum);
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

procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TImage);
begin
  unpackData.Alignment := glGetInteger(GL_UNPACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod unpackData.Alignment) <> 0 then
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
end;

procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TImage);
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

procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TImage);
begin
  packData.Alignment := glGetInteger(GL_PACK_ALIGNMENT);
  if (image.Width * Image.PixelSize mod packData.Alignment) <> 0 then
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
end;

procedure AfterPackImage(const packData: TPackNotAlignedData; image: TImage);
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

{ manipulacje projection matrix ---------------------------------------------- }

{ If you define it, ProjectionGLPerspective and ProjectionGLOrtho
  functions will use VectorMath functions that calculatate matrices:
  PerspectiveProjMatrixDeg and OrthoProjMatrixDeg.

  This will work correct but may be slower than using OpenGL functions
  (gluPerspective, glOrtho) so define it only when you want to test
  VectorMath unit. }
{ $define TEST_VECTOR_MATH_MATRIX_FUNCTIONS}

procedure ProjectionGLPerspective(const fovy, aspect, zNear, zFar: TGLdouble);
var
  oldMatrixMode: TGLenum;
begin
  oldMatrixMode := glGetInteger(GL_MATRIX_MODE);
  glMatrixMode(GL_PROJECTION);

  {$ifdef TEST_VECTOR_MATH_MATRIX_FUNCTIONS}
  glLoadMatrix(PerspectiveProjMatrixDeg(fovy, aspect, zNear, zFar));
  {$else}
  if ZFar = ZFarInfinity then
  begin
    glLoadMatrix(PerspectiveProjMatrixDeg(fovy, aspect, zNear, zFar));
  end else
  begin
    glLoadIdentity;
    gluPerspective(fovy, aspect, zNear, zFar);
  end;
  {$endif}

  glMatrixMode(oldMatrixMode);
end;

procedure ProjectionGLOrtho(const left, right, bottom, top, zNear, zFar: TGLdouble);
var
  oldMatrixMode: TGLenum;
begin
  oldMatrixMode := glGetInteger(GL_MATRIX_MODE);
  glMatrixMode(GL_PROJECTION);

  {$ifdef TEST_VECTOR_MATH_MATRIX_FUNCTIONS}
  glLoadMatrix(OrthoProjMatrix(left, right, bottom, top, zNear, zFar));
  {$else}
  glLoadIdentity;
  glOrtho(left, right, bottom, top, zNear, zFar);
  {$endif}

  glMatrixMode(oldMatrixMode);
end;

{ poly stipple ------------------------------------------------------------ }

function RandomBitsByte(OneChance:Extended):byte;
var
  i:integer;
begin
  { notka - Wynik Random jest zawsze w przedziale [0,1). To wazne zeby tu bylo
    porownanie ostre "<". Wtedy dla szansy = dokladnie 0 nigdy nie bedzie Random<0.
    (dla szansy = 1 warunek zarowno z "<=" jak i z "<" gwarantowalby ze
    zawsze taka szansa zachodzi - bo zawsze zachodzilaby ostra nierownosc wiec
    i nieostra takze.) }
  result:=0;
  for i:=0 to 7 do if Random<OneChance then result := result + (1 shl i);
end;

function RandomPolyStipple(const BlackChance: Extended): TPolygonStipple;
var i: integer;
begin
  for i := 0 to High(result) do result[i] := RandomBitsByte(BlackChance);
end;

function RandomPolyStippleBy16(const BlackChance: Extended): TPolygonStipple;
var
  b: byte;
  x, y: integer;
begin
  for x := 0 to 1 do
    for y := 0 to 15 do
    begin
      b := RandomBitsByte(BlackChance);
      result[y*4 + x] := b;
      result[y*4 + x+2] := b;
      result[(y+16)*4 + x] := b;
      result[(y+16)*4 + x+2] := b;
    end;
end;

function RandomPolyStippleBy8(const BlackChance: Extended): TPolygonStipple;
var
  b: byte;
  y, i, j: integer;
begin
  for y := 0 to 7 do
  begin
    b := RandomBitsByte(BlackChance);

    for i := 0 to 3 do
      for j := 0 to 3 do
        result[(y+8*j)*4 + i] := b;
  end;
end;

{inne proste procedury pomocnicze
 --------------------------------------------------------------------------------------}

procedure SetGLenabled(value: TGLenum; isEnabled: boolean);
begin
  if isEnabled then glEnable(value) else glDisable(value);
end;

procedure VerticalGLLine(x, y1, y2: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x, y1); glVertex2f(x, y2); glEnd;
end;

procedure HorizontalGLLine(x1, x2, y: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x1, y); glVertex2f(x2, y); glEnd;
end;

{$define DRAW_GL_BORD_RECT_NO_PUSHPOP_IMPLEMENTATION:=
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL); (* this changes GL_POLYGON_BIT attrib *)
  glColorv(InsideCol);
  glRectf(x1, y1, x2, y2);
  glColorv(BorderCol);
  DrawGLRectBorder(x1, y1, x2, y2);
}

procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f);
begin
  glPushAttrib(GL_POLYGON_BIT);
    DRAW_GL_BORD_RECT_NO_PUSHPOP_IMPLEMENTATION
  glPopAttrib;
end;

procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f; Stipple: PPolygonStipple);
begin
  if Stipple <> nil then
  begin
    glPushAttrib(GL_POLYGON_STIPPLE_BIT or GL_POLYGON_BIT);
      CastleGLPolygonStipple(Stipple);
      glEnable(GL_POLYGON_STIPPLE);
      DRAW_GL_BORD_RECT_NO_PUSHPOP_IMPLEMENTATION
    glPopAttrib;
  end else
  begin
    glPushAttrib(GL_POLYGON_BIT);
      glDisable(GL_POLYGON_STIPPLE);
      DRAW_GL_BORD_RECT_NO_PUSHPOP_IMPLEMENTATION
    glPopAttrib;
  end;
end;

procedure DrawGLRectBorder(const x1, y1, x2, y2: TGLfloat);
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(x1, y1); glVertex2f(x2, y1); glVertex2f(x2, y2); glVertex2f(x1, y2);
  glEnd;
end;

procedure DrawGLRectBorder(const Rectangle: TRectangle);
begin
  DrawGLRectBorder(Rectangle.X0, Rectangle.Y0, Rectangle.X0 + Rectangle.Width, Rectangle.Y0 + Rectangle.Height);
end;

function UnProjectGL(winx, winy, winz :TGLdouble): TVector3d;
var
  modelMatrix, projMatrix: T16dArray;
  viewport: TViewPortArray;
begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @modelMatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @projMatrix);
  glGetIntegerv(GL_VIEWPORT, @viewport);
  Check( gluUnProject(winx, winy, winz, modelMatrix, projMatrix, viewport,
    @result[0], @result[1], @result[2]) = GL_TRUE, 'gluUnProject');
end;

procedure DrawArrow(HeadThickness, HeadLength: TGLfloat);
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
  gluQuadricCallback(result, GLU_ERROR, TCallBack(@ReportGLError));
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

procedure DrawGLPlane(x1, y1, x2, y2: TGLfloat; constValue: TGLfloat;
  constCoord, DetailLevelX, DetailLevelY: integer;
  constCoordGivesNormal1: boolean;
  MakeTextureCoords: boolean;
  texX1, texY1, texX2, texY2: TGLfloat; order_ST_XYZ: boolean);
var
  xstep, ystep, texXStep, texYStep, texY, y, ynext, texYnext: TGLfloat;
  indX, indY, i, j: integer;
  v: TVector3f;
  normalnaKolejnosc: boolean;

  procedure RysujDwojka(x, texX: TGLfloat);

    procedure TexCoordXYZ(x, y: TGLfloat);
    {x, y to wspolrzedne tekstury. x to wspolrzedna x-kowa, y y-kowa,
     to znaczy ze ich kolejnosc odzwierciedla kolejnosc wspolrzednych
     XYZ (bo tak naprawde to nie musza byc wspolrzedne x, y; to moga
     byc x, z lub y, z). Parametr order_ST_XYZ mowi czy x odpowiada S
     a y T czy na odwrot.}
    begin
      if MakeTextureCoords then
        if order_ST_XYZ then
          glTexCoord2f(x, y) else
          glTexCoord2f(y, x);
    end;

    procedure Nizej;
    begin
      v[indY] := y;
      TexCoordXYZ(texX, texY);
      glVertex3fv(@v);
    end;

    procedure Wyzej;
    begin
      v[indY] := ynext;
      TexCoordXYZ(texX, texYnext);
      glVertex3fv(@v);
    end;

  begin
    v[indX] := x;

    if normalnaKolejnosc then
      begin Wyzej; Nizej; end else
      begin Nizej; Wyzej; end;
  end;

const
  { jaki jest glNormal dla odpowiedniego normalnaKolejnosc i constIndex ? }
  normals3f: array[boolean, 0..2]of TVector3f =
  (( (-1,  0,  0), ( 0, +1,  0), ( 0,  0, -1) ),
   ( (+1,  0,  0), ( 0, -1,  0), ( 0,  0, +1) ));
begin
  RestOf3dCoords(constCoord, indX, indY);

  normalnaKolejnosc:=
    ((normals3f[true, constCoord][constCoord] > 0) and constCoordGivesNormal1) or
    ((normals3f[true, constCoord][constCoord] < 0) and (not constCoordGivesNormal1));

  glNormal3fv(@normals3f[normalnaKolejnosc, constCoord]);

  texXStep:=(texX2-texX1) / (DetailLevelX + 1);
  texYStep:=(texY2-texY1) / (DetailLevelY + 1);
  xstep:=(x2-x1) / (DetailLevelX + 1);
  ystep:=(y2-y1) / (DetailLevelY + 1);

  { TODO: zrobic aby wybor normalnego wektora dzialal bez wzgledu
    na to czy x1 <= x2 i y1 <= y2. }

  v[constCoord] := constValue;

  ynext := y1;
  texYnext := texY1;
  for j := 0 to DetailLevelY do
  begin
    glBegin(GL_QUAD_STRIP);

      y := ynext;
      texY := texYnext;
      if j = DetailLevelY then
      begin
        {podobnie jak ponizej przy x2, dbam tutaj zeby na koncu polygon dosiegnal
         y2 i texY2 - nie mozemy w tym celu polegac na tym ze
         y1 + ystep * (DetailY+1) da nam dokladnie y2 - drobny blad w obliczeniach
         i OpenGL zacznie nam renderowac obiekty ze szczelinami pomiedzy !}
        ynext := y2;
        texYnext := texY2;
      end else
      begin
        ynext := y1 + ystep * (j+1);
        texYnext := texY1 + texYStep * (j+1);
      end;

      {normalnie, zapisalbym ponizej jako
         for i := 0 to DetailLevelX+1 do
          RysujDwojka(x1 + xstep * i, texX1 + texXStep * i);
       ale w obliczeniach zmiennoprzecinkowych nie mozem byc pewni ze na koncu
       x1 + xstep * (DetailLevelX+1) da nam x2 - a powinno ! Drobne przesuniecie
       w liczbach zmiennoprzecinkowych moze spowodowac ze pomiedzy dwoma polygonami
       teoretycznie stykajacymi sie faktycznie powstaje szczelina - mniej lub
       bardziej widoczna, w zaleznosci od punktu patrzenia. }
      for i := 0 to DetailLevelX do
        RysujDwojka(x1 + xstep * i, texX1 + texXStep * i);
      RysujDwojka(x2, texX2);

    glEnd;
  end;
end;

procedure DrawGLBox(const Box: TBox3D; DetailX, DetailY, DetailZ: integer;
  ccwOutside: boolean; MakeTextureCoords: boolean);
begin
  DrawGLPlane(Box.Data[0, 1], Box.Data[0, 2], Box.Data[1, 1], Box.Data[1, 2], Box.Data[0, 0], 0, DetailY, DetailZ, not ccwOutside, MakeTextureCoords);
  DrawGLPlane(Box.Data[0, 1], Box.Data[0, 2], Box.Data[1, 1], Box.Data[1, 2], Box.Data[1, 0], 0, DetailY, DetailZ, ccwOutside    , MakeTextureCoords);

  DrawGLPlane(Box.Data[0, 0], Box.Data[0, 2], Box.Data[1, 0], Box.Data[1, 2], Box.Data[0, 1], 1, DetailX, DetailZ, not ccwOutside, MakeTextureCoords);
  DrawGLPlane(Box.Data[0, 0], Box.Data[0, 2], Box.Data[1, 0], Box.Data[1, 2], Box.Data[1, 1], 1, DetailX, DetailZ, ccwOutside    , MakeTextureCoords);

  DrawGLPlane(Box.Data[0, 0], Box.Data[0, 1], Box.Data[1, 0], Box.Data[1, 1], Box.Data[0, 2], 2, DetailX, DetailY, not ccwOutside, MakeTextureCoords);
  DrawGLPlane(Box.Data[0, 0], Box.Data[0, 1], Box.Data[1, 0], Box.Data[1, 1], Box.Data[1, 2], 2, DetailX, DetailY, ccwOutside    , MakeTextureCoords);
end;

procedure DrawGLBox(const x1, y1, z1, x2, y2, z2: TGLfloat;
  DetailX, DetailY, DetailZ: integer; ccwOutside: boolean;
  MakeTextureCoords: boolean);
begin
  DrawGLBox(Box3D(Vector3Single(x1, y1, z1), Vector3Single(x2, y2, z2)),
    DetailX, DetailY, DetailZ, ccwOutside, MakeTextureCoords);
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

procedure DrawGLTriangle(const p1, p2, p3: TVector3f;
  const Tex1, Tex2, Tex3: TVector2f;
  DetailLev: Cardinal);

  procedure PairMix(const v1, v2: TVector3f; const tex1, tex2: TVector2f;
    v2part: TGLfloat; var vresult: TVector3f; var texResult: TVector2f);
  begin
    vresult := Lerp(v2part, v1, v2);
    texResult := Lerp(v2part, tex1, tex2);
  end;

  procedure PairAssign(
    out v: TVector3f; out texv: TVector2f;
    const newv: TVector3f; const newtexv: TVector2f);
  begin
    v := newv; texv := newtexv;
  end;

  procedure glUsePair(const v: TVector3f; const texv: TVector2f);
  begin
    glTexCoordv(texv); glVertexv(v);
  end;

  procedure glUsePairMix(const v1, v2: TVector3f; const tex1, tex2: TVector2f; v2part: TGLfloat);
  var
    vresult: TVector3f;
    texResult: TVector2f;
  begin
    vresult := Lerp(v2part, v1, v2);
    texResult := Lerp(v2part, tex1, tex2);
    glTexCoordv(texResult); glVertexv(vresult);
  end;

var
  i, j: Cardinal;
  Left, Right, NextLeft, NextRight: TVector3f;
  TexLeft, TexRight, TexNextLeft, TexNextRight: TVector2f;
begin
  glNormalv( Normalized(VectorProduct(VectorSubtract(p2, p1), VectorSubtract(p2, p3))) );
  PairAssign(NextLeft, TexNextLeft, p1, Tex1);
  PairAssign(NextRight, TexNextRight, p1, Tex1);
  for i := 0 to DetailLev do
  begin
    {rysuj Triangle strip o i kreskach}
    PairAssign(Left, TexLeft, NextLeft, TexNextLeft);
    PairAssign(Right, TexRight, NextRight, TexNextRight);
    if i = DetailLev then
    begin
      PairAssign(NextLeft, TexNextLeft, p2, Tex2);
      PairAssign(NextRight, TexNextRight, p3, Tex3);
    end else
    begin
      {gdyby i = DetailLev to ponizsze wzorki TEORETYCZNIE dalyby dobre wartosci -
       w praktyce, mogloby byc ze NextLeft jest nieco rozny od p2 i podobnie
       NextRight i p3 i wtedy - katastrofa : w OpenGL'u zaczniemy widziec
       szczeliny miedzy dwoma trojkatami narysowanymi ta procedura.}
      PairMix(p1, p2, Tex1, Tex2, (i+1)/(DetailLev+1), NextLeft, TexNextLeft);
      PairMix(p1, p3, Tex1, Tex3, (i+1)/(DetailLev+1), NextRight, TexNextRight);
    end;

    glBegin(GL_TRIANGLE_STRIP);
      glUsePair(NextRight, TexNextRight);
      glUsePair(Right, TexRight);

      if i >= 1 then
      begin
        glUsePairMix(NextLeft, NextRight, TexNextLeft, TexNextRight, i/(i+1));

        {ponizsza petla for biegnie do i-1 zamiast do i bo chcemy vertex Left
         narysowac osobno (zeby uniknac bledu obliczen zmiennnoprzec.).
         (notka : powyzszy check na i >= 1 musi byc wykonany przed ta petla, bo
         gdy i = 0 instrukcja i-1 na Cardinalach powoduje RangeError.}
        for j := i-1 downto 1 do
        begin
          glUsePairMix(Left, Right, TexLeft, TexRight, j/i);
          glUsePairMix(NextLeft, NextRight, TexNextLeft, TexNextRight, j/(i+1));
        end;
        glUsePair(Left, TexLeft);
      end;
      glUsePair(NextLeft, TexNextLeft);
    glEnd;
  end;
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

procedure DrawGLBlackOutRect(const BlackOutColor: TVector3f;
  const BlackOutIntensity, x1, y1, x2, y2: TGLfloat);

{ ta procedura zabarwia caly ekran gry kolorem Color.
  Dokladny sposob zabarwiania (czyli blending) zostal wybrany tak aby dawac
  najbardziej ciekawy wizualnie efekt. Oto on :

  Ta procedura zmienia kazdy kolor na ekranie gry, powiedzmy K0=(R, G, B),
  na (R* Rb*skala, G *Gb*skala, B *Bb*skala)
  gdzie K=(Rb, Gb, Bb) to jakis kolor. Tym samym dopoki skala jest
  z przedzialu 0..1 i wszystkie komponenty koloru K tez sa poprawnie
  w 0..1 to wynikowy kolor jest zawsze ciemniejszy lub rowny K0.

  To ktore komponenty tego K0 sa sciemniane szybciej zalezy od K.
  (np. K = (1, 0, 0) oznacza ze komponent red jest zachowany (o ile
  skala = 1) a komponenty green i blue sa kasowane (bez wzgledu na
  skala)).
  Gdy skala powoli spada do zera, kolor coraz bardziej dazy do (0, 0, 0).
  Gdy skala = 0 caly ekran bedzie czarny, bez wzgledu na wartosc K.

  Ta idea jest uzywana tak :
  - BlackOutIntensity between 1 and FullWhiteEnd:
    Scale is constant 1 and K is BlackOutColor.
    So the world color is modulated to BlackOutColor.
  - BlackOutIntensity between FullWhiteEnd and FullBlack:
    Scale changes from 1 to MinScale, with K = BlackOutColor.
    So the world color changes from modulated to BlackOutColor to absolute
    black.
  - BlackOutIntensity between FullBlack and 0:
    Scale changes from MinScale to 1, with K = White.
    So the world color changes from pure black screen to normal.

  FullBlack to wlasnie ta pozycja na skali 1..0 ze w tym momencie gracz
  widzi zupelna ciemnosc (kazdy kolor jest zamieniony na Black3d).
  Im wieksze, tym krotsze jest przechodzenie
  od BlackOutColor do Black3f w porownaniu z przechodzeniem
  od Black3f do White3f (czyli normalnych kolorow wszystkiego).}

const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  MinScale = 0.5;
begin
  if BlackOutIntensity > 0 then
  begin
    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT);
      glEnable(GL_BLEND);
        glBlendFunc(GL_ZERO, GL_SRC_COLOR);

        if BlackOutIntensity > FullWhiteEnd then
          glColorv(BlackOutColor) else
        if BlackOutIntensity > FullBlack then
          glColorv( VectorScale( BlackOutColor,
            MapRange(BlackOutIntensity, FullWhiteEnd, FullBlack, 1, MinScale))) else
          glColorv( VectorScale( White3Single,
            MapRange(BlackOutIntensity, FullBlack, 0, MinScale, 1)));

        glRectf(x1, y1, x2, y2);
      glDisable(GL_BLEND);
    glPopAttrib;
  end;
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
        '  Vendor Mesa: %s (Mesa parsed version major: %d, minor: %d, release: %d)' +nl+
        '  Vendor ATI: %s (fglrx: %s)' +nl+
        nl+
        '  Buggy glPushAttrib(GL_POINT_SET): %s' +nl+
        '  Buggy glDrawPixels for odd widths: %s' +nl+
        '  Buggy glGenerateMipmapEXT: %s' +nl+
        '  Buggy GL_LIGHT_MODEL_TWO_SIDE: %s' +nl+
        '  Buggy VBO: %s' +nl+
        '  Buggy shader shadow map: %s',
        [ BoolToStr[Version.VendorNVidia],

          BoolToStr[Version.IsMesa],
          Version.MesaMajor, Version.MesaMinor, Version.MesaRelease,

          BoolToStr[Version.IsVendorATI], BoolToStr[Version.IsFglrx],

          BoolToStr[Version.BuggyPointSetAttrib],
          BoolToStr[Version.BuggyDrawOddWidth],
          BoolToStr[Version.BuggyGenerateMipmap],
          BoolToStr[Version.BuggyLightModelTwoSide],
          BoolToStr[Version.BuggyVBO],
          BoolToStr[Version.BuggyShaderShadowMap]
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
      EDivByZero was raised by glCallList inside T3DScene (not used there
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
  Pointer(CastleGLPolygonStipple) := glPolygonStipple;
end.
