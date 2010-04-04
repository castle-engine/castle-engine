{
  Copyright 2001-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Various OpenGL utilities.)

  Implements various low-level helpers for OpenGL programming.
  Simple wrappers for OpenGL procedures (like glVertexv, that is overloaded
  for various vector types, and calls appropriate version like glVertex3fv
  based on parameter type). Also simple drawing routines for basic primitives
  (boxes etc.).
  This unit does not assume that you initialized OpenGL in any particular
  way (e.g. using GLWindow, Glut, SDL, or whatever).
}

unit KambiGLUtils;

{$I kambiconf.inc}
{$I openglmac.inc}

{ This unit defines, among many other things, comfortable versions
  of some OpenGL functions. They introduce overloading
  (e.g. one name, glVertexv, for all glVertex* functtions)
  and allow passing records or arrays (instead of pointers)
  as parameters (this is an elegant way to allow things like
    glVertexv( Vector3Single(1, 2, 3)  );
  ).

  There are a couple ways to define such "aliases" to OpenGL functions:

  1) One, unsafe but fast, is to declare these functions like
       procedure glVertexv(const v: TVector3f); OPENGL_CALL overload;
         external OpenGLDLL name 'glVertex3fv';
     This results in quick code, but it has some disadvantages:
     - it assumes that v will be passed by reference
       (which seems to be not true under FPC 1.0.6 under Linux)
     - This links my program directly to OpenGL library,
       thus disabling any way to select OpenGL library at runtime

  2) Second way, safer, is to do some "stub": declare
     normal function
       procedure glVertexv(const v: TVector3f); overload;
     and in implementation just call appropriate function from GL, GLU, GLExt
       procedure glVertexv(const v: TVector3f);
       begin glVertex3fv(@v); end;
     This will work slightly slower, but it also proved to be much
     safer solution.

  3) There's also another solution: just copy function pointers.
     This is fast, but it
     - can't be used for overloaded functions (since you can't have many
       vars with the same name).
     - has part of the disadvantages of 1) : it relies that parameters are passed
       correctly.

  Define IMPLEMENT_OPENGL_STUBS to use 2), otherwise we'll try to use 1).
  For now, this is adviced.

  Old pl comments: Wyglada na to ze FPC ma wiekszy wstret do przekazywania parametrow
  "const" przez referencje niz np. kylix. To boli, bo gdy deklaruje
  glVertexv(const v: TVector3f) to oczywiscie WYMAGAM aby parametr
  zostal przekazany przez referencje, nigdy przez wartosc. Jest to problem
  Pascala fpc i delphi - nie ma metody aby powiedziec "const przez referencje".
  gpc jest tu madry i ma konstrukcje "protected var" ktora znaczy wlasnie
  "const koniecznie przez referencje".
  Chwilowo po prostu te deklaracje sa realizowane nie przez export ale przez
  wywolanie posrednie.
}
{$define IMPLEMENT_OPENGL_STUBS}

{ See
  http://www.freepascal.org/mantis/view.php?id=10460
  for NEEDS_FOG_COORD_FIX explanation. }
{$ifdef VER2_0} {$define NEEDS_FOG_COORD_FIX} {$endif}
{$ifdef VER2_2} {$define NEEDS_FOG_COORD_FIX} {$endif}

interface

uses Math, GL, GLU, GLExt,
  SysUtils, KambiUtils, VectorMath, Boxes3D, IntRects,
  Images, Matrix, Areas;

{$define read_interface}
{$I glext_packed_depth_stencil.inc}

{ ------------------------------------------------------------ }
{ @section(Utils needed only when using GL, GLU, GLExt bindings.
  Not needed when using OpenGLh binding.) }

{$ifndef USE_OLD_OPENGLH}
type
  { Types with leading "T" }
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

{$I opengltypes.inc}

const
  { These constants *must* match constants used in implementation of gl and glu
    units, so that we link to the same libraries. }
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

  GL_ARB_imaging: boolean;
  GL_ARB_multitexture: boolean;
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
  GL_EXT_texture3D: boolean;
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
  GL_EXT_framebuffer_object: boolean;
  GL_ARB_occlusion_query: boolean;
  GL_EXT_packed_depth_stencil: boolean;

{$ifdef NEEDS_FOG_COORD_FIX}
var
  glFogCoordfEXT: procedure(coord: GLfloat);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoorddEXT: procedure(coord: GLdouble);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoordfvEXT: procedure(coord: PGLfloat);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}
  glFogCoorddvEXT: procedure(coord: PGLdouble);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}
{$endif}

var
  { Constant (for given context) OpenGL limits.
    Initialized once by LoadAllExtensions, this is usually most comfortable.
    Initialized to 0 if appropriate OpenGL extension is not available.
    @groupBegin }
  GLMaxTextureSize: Cardinal;
  GLMaxTextureUnitsARB: Cardinal;
  GLMaxCubeMapTextureSizeARB: Cardinal;
  GLMax3DTextureSizeEXT: Cardinal;
  GLMaxTextureMaxAnisotropyEXT: Single;
  GLQueryCounterBits: TGLint;
  GLMaxRenderbufferSize: TGLuint;
  { @groupEnd }

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
{$endif}

{ sprawdzanie bledow gl ----------------------------------------------------------}

type
  EOpenGLError = class(Exception)
  public
    ErrorGLCode: TGLenum;
    constructor Create(ErrorGL: TGLenum { AdditionalComment = '' } ); overload;
    constructor Create(ErrorGL: TGLenum; const AdditionalComment: string); overload;
  end;

{ CheckGLErrors sprawdza czy sa jakies bledy OpenGL'a (glGetError),
  jesli tak - rzuca wyjatek EOpenGLError. }
procedure CheckGLErrors(const AdditionalComment: string {$ifdef DEFPARS} = '' {$endif}); overload;

{ ReportGLError = raise EOpenGLError.Create(ErroCode);
  ReportGLError jest dobra aby ja zerejestrowac jako GLU_TESS_ERROR
  przez gluTessCallback albo GLU_ERROR przez gluQuadricCallback. }
procedure ReportGLError(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}

{ ------------------------------------------------------------------------------
  wersje funkcyjne procedur glGet*. Moga byc uzywane tylko do zastepowania glGet*
  ktore zwracaja pojedyncze wartosci !

  Przy okazji gwarantuja nastepujace zachowanie : jezeli odpowiednie
  glGet*v bedzie blednym wywolaniem (wiec wedlug specyfikacji OpenGL'a
  zostanie zignorowane i zostanie ustawione odpowiednie glGetError)
  to result bedzie jakby po FillChar(result, SizeOf(result), 0).
  Np. w GLCapsString robimy glGetInteger(GL_MAX_CLIENT_ATTRIB_STACK),
  jezeli aktualna wersja OpenGL'a w ogole nie ma czegos takiego
  jak client attrib stack to zapytanie
  glGetIntegerv(GL_MAX_CLIENT_ATTRIB_STACK, wynik) spowoduje blad i nie zwroci
  nic pod wskazanym wynik. Ale wlasnie niniejsze get'y najpierw inicjuja
  result na 0 wiec wynikem bedzie 0 (a nie cos nieokreslonego). }

function glGetFloat(pname: TGLEnum): TGLfloat;
function glGetInteger(pname: TGLEnum): TGLint;
function glGetBoolean(pname: TGLEnum): TGLboolean;
function glGetDouble(pname: TGLEnum): TGLdouble;

{ ----------------------------------------------------------------------------------
  Dodatkowe deklaracje eksportujace funkcje OpenGL'a w nieco inny sposob.

  Podstawowe wersje tych procedur w module OpenGLa (jako v: PXxx) sa nieco bardziej
  elastyczne. Np. dla glLightfv adres moze wskazywac na 1 liczbe (np. dla GL_SPOT_CUTOFF),
  na 3 liczby (np. dla GL_SPOT_DIR) lub na 4 liczby (np. GL_AMBIENT). Ponizsze deklaracje
  z koniecznosci beda wiec powtarzaly nie raz eksportowanie tych samych proc. na inne
  sposoby.
  Wada 2 :  dll z ktorego sa eksportowane te proc. jest ustalony "na sztywno",
  nie mozna np. w czasie runtime zdecydowac sie czy ladujemy openGL'a z Microsoftu czy z SGI.
  To dlatego ze nie mozna robic przeciazanych zmiennych funkcyjnych (bo nie mozna przeciazac
  zmiennych...).

  Zalety jednak : ponizsze funkcje sa czesto wygodniejsze, mozna np. napisac glMaterialfv
   (GL_FRONT, GL_DIFFUSE, Vector4f(0.2, 0.2, 0.3, 1)) (i nie musimy deklarowac sobie na gorze
   tablicy TVector4f !).
  Kolekcja ponizszych funkcji nie jest skonczona, bede ja rozszerzal gdy bede czegos
   potrzebowal.
}

{ Some types should always behave like IMPLEMENT_OPENGL_STUBS was defined --
  because they are, both in FPC and Delphi, passed by value
  (not by reference) when they are used as constant parameters. }
procedure glColorv(const v: TVector3b); overload;
procedure glColorv(const v: TVector3ub); overload;
procedure glColorv(const v: TVector4b); overload;
procedure glColorv(const v: TVector4ub); overload;
procedure glNormalv(const v: TVector3b); overload;

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

procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1i); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1s); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2i); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2s); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3i); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3s); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4d); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4f); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4i); overload;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4s); overload;

{$ifdef IMPLEMENT_OPENGL_STUBS}

procedure glNormalv(const v: TVector3d); overload;
procedure glNormalv(const v: TVector3f); overload;
procedure glNormalv(const v: TVector3i); overload;
procedure glNormalv(const v: TVector3s); overload;

procedure glColorv(const v: TVector3d); overload;
procedure glColorv(const v: TVector3f); overload;
procedure glColorv(const v: TVector3i); overload;
procedure glColorv(const v: TVector3s); overload;
procedure glColorv(const v: TVector3ui); overload;
procedure glColorv(const v: TVector3us); overload;

procedure glColorv(const v: TVector4d); overload;
procedure glColorv(const v: TVector4f); overload;
procedure glColorv(const v: TVector4i); overload;
procedure glColorv(const v: TVector4s); overload;
procedure glColorv(const v: TVector4ui); overload;
procedure glColorv(const v: TVector4us); overload;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f); overload;
procedure glMaterialv(face, pname: TGLEnum; const params: TVector4i); overload;

procedure glVertexv(const v: TVector2d); overload;
procedure glVertexv(const v: TVector2f); overload;
procedure glVertexv(const v: TVector2i); overload;
procedure glVertexv(const v: TVector2s); overload;

procedure glVertexv(const v: TVector3d); overload;
procedure glVertexv(const v: TVector3f); overload;
procedure glVertexv(const v: TVector3i); overload;
procedure glVertexv(const v: TVector3s); overload;

procedure glVertexv(const v: TVector4d); overload;
procedure glVertexv(const v: TVector4f); overload;
procedure glVertexv(const v: TVector4i); overload;
procedure glVertexv(const v: TVector4s); overload;

procedure glVertexv(const v: TVector2_Double); overload;
procedure glVertexv(const v: TVector2_Single); overload;

procedure glVertexv(const v: TVector3_Double); overload;
procedure glVertexv(const v: TVector3_Single); overload;

procedure glVertexv(const v: TVector4_Double); overload;
procedure glVertexv(const v: TVector4_Single); overload;

procedure glTexCoordv(const v: TVector1d); overload;
procedure glTexCoordv(const v: TVector1f); overload;
procedure glTexCoordv(const v: TVector1i); overload;
procedure glTexCoordv(const v: TVector1s); overload;
procedure glTexCoordv(const v: TVector2d); overload;
procedure glTexCoordv(const v: TVector2f); overload;
procedure glTexCoordv(const v: TVector2i); overload;
procedure glTexCoordv(const v: TVector2s); overload;
procedure glTexCoordv(const v: TVector3d); overload;
procedure glTexCoordv(const v: TVector3f); overload;
procedure glTexCoordv(const v: TVector3i); overload;
procedure glTexCoordv(const v: TVector3s); overload;
procedure glTexCoordv(const v: TVector4d); overload;
procedure glTexCoordv(const v: TVector4f); overload;
procedure glTexCoordv(const v: TVector4i); overload;
procedure glTexCoordv(const v: TVector4s); overload;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4d); overload;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f); overload;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4i); overload;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector4i); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3i); overload;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); overload;
procedure glLightModelv(pname: TGLenum; const params: TVector4i); overload;

procedure glFogv(pname: TGLEnum; const params: TVector4f); overload;
procedure glFogv(pname: TGLEnum; const params: TVector4i); overload;

procedure glMultMatrix(const m: TMatrix4f); overload;
procedure glMultMatrix(const m: TMatrix4d); overload;
procedure glLoadMatrix(const m: TMatrix4f); overload;
procedure glLoadMatrix(const m: TMatrix4d); overload;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); overload;
procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4i); overload;

{$else IMPLEMENT_OPENGL_STUBS}

procedure glNormalv(const v: TVector3d); OPENGL_CALL overload; external openglDLL name 'glNormal3dv';
procedure glNormalv(const v: TVector3f); OPENGL_CALL overload; external openglDLL name 'glNormal3fv';
procedure glNormalv(const v: TVector3i); OPENGL_CALL overload; external openglDLL name 'glNormal3iv';
procedure glNormalv(const v: TVector3s); OPENGL_CALL overload; external openglDLL name 'glNormal3sv';

procedure glColorv(const v: TVector3d); OPENGL_CALL overload; external openglDLL name 'glColor3dv';
procedure glColorv(const v: TVector3f); OPENGL_CALL overload; external openglDLL name 'glColor3fv';
procedure glColorv(const v: TVector3i); OPENGL_CALL overload; external openglDLL name 'glColor3iv';
procedure glColorv(const v: TVector3s); OPENGL_CALL overload; external openglDLL name 'glColor3sv';
procedure glColorv(const v: TVector3ui); OPENGL_CALL overload; external openglDLL name 'glColor3uiv';
procedure glColorv(const v: TVector3us); OPENGL_CALL overload; external openglDLL name 'glColor3usv';

procedure glColorv(const v: TVector4d); OPENGL_CALL overload; external openglDLL name 'glColor4dv';
procedure glColorv(const v: TVector4f); OPENGL_CALL overload; external openglDLL name 'glColor4fv';
procedure glColorv(const v: TVector4i); OPENGL_CALL overload; external openglDLL name 'glColor4iv';
procedure glColorv(const v: TVector4s); OPENGL_CALL overload; external openglDLL name 'glColor4sv';
procedure glColorv(const v: TVector4ui); OPENGL_CALL overload; external openglDLL name 'glColor4uiv';
procedure glColorv(const v: TVector4us); OPENGL_CALL overload; external openglDLL name 'glColor4usv';

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f);  OPENGL_CALL overload; external openglDLL name 'glMaterialfv';
procedure glMaterialv(face, pname: TGLEnum; const params: TVector4i);  OPENGL_CALL overload; external openglDLL name 'glMaterialiv';

procedure glVertexv(const v: TVector2d); OPENGL_CALL overload; external openglDLL name 'glVertex2dv';
procedure glVertexv(const v: TVector2f); OPENGL_CALL overload; external openglDLL name 'glVertex2fv';
procedure glVertexv(const v: TVector2i); OPENGL_CALL overload; external openglDLL name 'glVertex2iv';
procedure glVertexv(const v: TVector2s); OPENGL_CALL overload; external openglDLL name 'glVertex2sv';

procedure glVertexv(const v: TVector3d); OPENGL_CALL overload; external openglDLL name 'glVertex3dv';
procedure glVertexv(const v: TVector3f); OPENGL_CALL overload; external openglDLL name 'glVertex3fv';
procedure glVertexv(const v: TVector3i); OPENGL_CALL overload; external openglDLL name 'glVertex3iv';
procedure glVertexv(const v: TVector3s); OPENGL_CALL overload; external openglDLL name 'glVertex3sv';

procedure glVertexv(const v: TVector4d); OPENGL_CALL overload; external openglDLL name 'glVertex4dv';
procedure glVertexv(const v: TVector4f); OPENGL_CALL overload; external openglDLL name 'glVertex4fv';
procedure glVertexv(const v: TVector4i); OPENGL_CALL overload; external openglDLL name 'glVertex4iv';
procedure glVertexv(const v: TVector4s); OPENGL_CALL overload; external openglDLL name 'glVertex4sv';

procedure glTexCoordv(const v: TVector1d); OPENGL_CALL overload; external openglDLL name 'glTexCoord1dv';
procedure glTexCoordv(const v: TVector1f); OPENGL_CALL overload; external openglDLL name 'glTexCoord1fv';
procedure glTexCoordv(const v: TVector1i); OPENGL_CALL overload; external openglDLL name 'glTexCoord1iv';
procedure glTexCoordv(const v: TVector1s); OPENGL_CALL overload; external openglDLL name 'glTexCoord1sv';
procedure glTexCoordv(const v: TVector2d); OPENGL_CALL overload; external openglDLL name 'glTexCoord2dv';
procedure glTexCoordv(const v: TVector2f); OPENGL_CALL overload; external openglDLL name 'glTexCoord2fv';
procedure glTexCoordv(const v: TVector2i); OPENGL_CALL overload; external openglDLL name 'glTexCoord2iv';
procedure glTexCoordv(const v: TVector2s); OPENGL_CALL overload; external openglDLL name 'glTexCoord2sv';
procedure glTexCoordv(const v: TVector3d); OPENGL_CALL overload; external openglDLL name 'glTexCoord3dv';
procedure glTexCoordv(const v: TVector3f); OPENGL_CALL overload; external openglDLL name 'glTexCoord3fv';
procedure glTexCoordv(const v: TVector3i); OPENGL_CALL overload; external openglDLL name 'glTexCoord3iv';
procedure glTexCoordv(const v: TVector3s); OPENGL_CALL overload; external openglDLL name 'glTexCoord3sv';
procedure glTexCoordv(const v: TVector4d); OPENGL_CALL overload; external openglDLL name 'glTexCoord4dv';
procedure glTexCoordv(const v: TVector4f); OPENGL_CALL overload; external openglDLL name 'glTexCoord4fv';
procedure glTexCoordv(const v: TVector4i); OPENGL_CALL overload; external openglDLL name 'glTexCoord4iv';
procedure glTexCoordv(const v: TVector4s); OPENGL_CALL overload; external openglDLL name 'glTexCoord4sv';

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4d); OPENGL_CALL overload; external openglDLL name 'glTexGendv';
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f); OPENGL_CALL overload; external openglDLL name 'glTexGenfv';
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4i); OPENGL_CALL overload; external openglDLL name 'glTexGeniv';

procedure glLightv(light, pname: TGLEnum; const params: TVector4f); OPENGL_CALL overload; external openglDLL name 'glLightfv';
procedure glLightv(light, pname: TGLEnum; const params: TVector4i); OPENGL_CALL overload; external openglDLL name 'glLightiv';
procedure glLightv(light, pname: TGLEnum; const params: TVector3f); OPENGL_CALL overload; external openglDLL name 'glLightfv';
procedure glLightv(light, pname: TGLEnum; const params: TVector3i); OPENGL_CALL overload; external openglDLL name 'glLightiv';

procedure glLightModelv(pname: TGLenum; const params: TVector4f); OPENGL_CALL overload; external openglDLL name 'glLightModelfv';
procedure glLightModelv(pname: TGLenum; const params: TVector4i); OPENGL_CALL overload; external openglDLL name 'glLightModeliv';

procedure glFogv(pname: TGLEnum; const params: TVector4f); OPENGL_CALL overload; external openglDLL name 'glFogfv';
procedure glFogv(pname: TGLEnum; const params: TVector4i); OPENGL_CALL overload; external openglDLL name 'glFogiv';

procedure glMultMatrix(const m: TMatrix4f); OPENGL_CALL overload; external openglDLL name 'glMultMatrixf';
procedure glMultMatrix(const m: TMatrix4d); OPENGL_CALL overload; external openglDLL name 'glMultMatrixd';
procedure glLoadMatrix(const m: TMatrix4f); OPENGL_CALL overload; external openglDLL name 'glLoadMatrixf';
procedure glLoadMatrix(const m: TMatrix4d); OPENGL_CALL overload; external openglDLL name 'glLoadMatrixd';

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); OPENGL_CALL overload; external openglDLL name 'glTexEnvfv';
procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4i); OPENGL_CALL overload; external openglDLL name 'glTexEnviv';

{$endif IMPLEMENT_OPENGL_STUBS}

{ uproszczenia dla sejwowania / ladowania gl state : ---------------------------------- }

type
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

{ otocz klauzulami Before/After Unpack Not Aligned Image fragmenty kodu w ktorych
  rozpakowujesz z pamieci do OpenGL'a (przy glDrawPixels, glTexImage1D, glTexImage2D,
  glBitmap, i glPolygonStipple) dane ktore zostaly ulozone prawidlowo przy
  zalozeniu ze GL_UNPACK_ALIGNMENT = 1, a
  nie wiesz jakie jest teraz GL_UNPACK_ALIGNMENT i nie chcesz tego zmieniac na stale,
  ale chcesz zeby ten obrazek zostal rozpakowany dobrze.

  Te procedury najpierw (w BEFORE) zasejwuja sobie aktualne GL_UNPACK_ALIGNMENT,
  potem je zmienia (jesli bedzie trzeba) tak zeby obrazek rozpakowal sie dobrze,
  i potem (w AFTER) ustawia je z powrotem tak jak byly. }
procedure BeforeUnpackNotAlignedRGBImage(out unpackdata: TUnpackNotAlignedData; imageWidth: cardinal);
procedure AfterUnpackNotAlignedRGBImage(const unpackData: TUnpackNotAlignedData; imageWidth: cardinal);

{ Before/After Pack : jak wyzej, ale martwia sie o GL_PACK_ALIGNMENT.
  Uzywaj naokolo glReadPixels. }
procedure BeforePackNotAlignedRGBImage(out packdata: TPackNotAlignedData; imageWidth: cardinal);
procedure AfterPackNotAlignedRGBImage(const packData: TPackNotAlignedData; imageWidth: cardinal);

{ wersje tych procedur z prostszymi nazwami i na typie TImage }
procedure BeforeUnpackImage(out unpackdata: TUnpackNotAlignedData; image: TImage);
procedure AfterUnpackImage(const unpackData: TUnpackNotAlignedData; image: TImage);
procedure BeforePackImage(out packdata: TPackNotAlignedData; image: TImage);
procedure AfterPackImage(const packData: TPackNotAlignedData; image: TImage);

{ manipulacje projection matrix -------------------------------------------------------- }

{ ProjectionGL* procedures load correspoding OpenGL matrices
  (gluPerspective, glOrtho), making sure we're in GL_PROJECTION matrix mode.

  More precise description: first these procedures change current matrix
  mode (if needed) to GL_PROJECTION. Then call load identity,
  and then call appropriate OpenGL functions (gluPerspective, glOrtho).
  And then (if needed) go back to previous matrix mode.
  So current matrix mode is never changed by these procedures.

  Also, ZFar is allowed to have special ZFarInfinity value
  for ProjectionGLPerspective.
  Then we set special perspective matrix, that has far plane set
  at infinity --- useful for z-fail shadow volumes.

  @groupBegin }
procedure ProjectionGLPerspective(const fovy, aspect, zNear, zFar: TGLdouble);
procedure ProjectionGLOrtho(const left, right, bottom, top, zNear, zFar: TGLdouble); overload;
procedure ProjectionGLOrtho(const left, right, bottom, top: TGLdouble); overload; { near = -1, far = 1 }

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

{ generuje Stipple ktore w kazdym bicie ma szanse BlackChange na bycie 1,
  else jest rowne 0. }
function RandomPolyStipple(const BlackChance: Extended): TPolygonStipple;

{ generuje Stipple ktorego cztery cwiartki sa rowne (tzn. losowana
  jest jedna cwiartka a cztery pozostale to jej kopie).
  To tworzy znacznie bardziej regularne stipple, co czasem moze byc
  uzyteczne. }
function RandomPolyStippleBy16(const BlackChance: Extended): TPolygonStipple;

{ jw. ale tu kawalki 8x8 sa rowne wiec jeszcze bardziej regularne }
function RandomPolyStippleBy8(const BlackChance: Extended): TPolygonStipple;

var
  { This is equivalent to glPolygonStipple, but takes
    PPolygonStipple as a parameter. }
  KamGLPolygonStipple: procedure(mask: PPolygonStipple);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}

{ others  --------------------------------------------------------------- }

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

{ DrawGLBorderedRectangle :
  Rysuje prostokat, wypelniony (GL_POLYGON) jednym kolorem i obrysowany
  (GL_LINE_LOOP) drugim. Uwagi co do kolejnosci vertexow : taka sama
  jak w glRectf. Req one attrib stack place, because it makes sure
  that polygon mode FRONT_AND_BACK is GL_FILL.

  Ze stanu OpenGL'a zmienia tylko current color.

  DrawGLRectBorder : wszystko j.w. ale rysuje tylko obrysowanie
  i nie wymaga attrib stack place, uzywa aktualnego koloru OpenGLa.

  Wersja ze stipple ustawia wlasne stipple (robi glPolygonStipple +
  glEnable(GL_POLYGON_STIPPLE) o ile Stipple <> nil i robi
  glDisable(GL_POLYGON_STIPPLE) jesli Stipple = nil, oczywiscie
  wszystko w odpowiednim push/pop attribow.
  Req one attrib stack place) }
procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f); overload;
procedure DrawGLBorderedRectangle(const x1, y1, x2, y2: TGLfloat;
  const InsideCol, BorderCol: TVector4f; Stipple: PPolygonStipple); overload;
procedure DrawGLRectBorder(const x1, y1, x2, y2: TGLfloat); overload;
procedure DrawGLRectBorder(const Area: TArea); overload;

{ TIntRect versions take x1 := R[0, 0], y1 := R[0, 1],
  x2 := R[1, 0], y2 := R[1, 1] (NOT x2 := R[1, 0]-1, y2 := R[1, 1]-1, because usually
  you don't want that when working with OpenGL. }
procedure DrawGLBorderedRectangle(const R: TIntRect;
  const InsideCol, BorderCol: TVector4f); overload;
procedure DrawGLBorderedRectangle(const R: TIntRect;
  const InsideCol, BorderCol: TVector4f; Stipple: PPolygonStipple); overload;
procedure DrawGLRectBorder(const R: TIntRect); overload;

function UnProjectGL(winx, winy, winz: TGLdouble): TVector3d;

{ niniejsza procedura renderuje dysk na wspolrzednych tak jak gluDisk.
  Nie sa generowane zadne normale (moze kiedys to tu dorobie, jak bedzie
  potrzebne), nie ma innerRadius i nie ma loops jak w gluDisk - bo ten dysk
  wlasnie FULL czyli nie ma wewnetrznego pierscienia.
  Co wobec tego zyskujemy w porownaniu z gluDisk ? Mianowicie, generowane sa
  wspolrzedne tekstury w dosc perfidny sposob : tekstura jest sciesniana tak
  aby cala kwadratowa tekstura zmiescila sie na kole. Innymi slowy rogi
  tekstury sa wpychane na powierzchnie dysku.
  Na krancach dysku wspolrzedne sa takie jak gluDisk :
   (r, 0, 0) is (1, 0.5),
   at (0, r, 0) it is (0.5, 1),
   at (-r, 0, 0) it is (0, 0.5), and
   at (0, -r, 0) it is (0.5, 0).
  natomiast na punktach posrednich na kole niniejsza procedura generuje
  wspolrzedne tekstury na krancach tekstury. (podczas gdy gluDisk scianala
  rogi tekstury aplikujac ja na dysk).

  Uwaga - ta procedure, podobnie jak quadrici glu, dobrze jest wrzucic do
  display listy - wykonujemy tutaj troche obliczen (w szczegolnosci,
  cosinusy i sinusy robimy normalnymi wywolaniami funkcji).
}
procedure fullGLDiskSqueezedTex(radius: TGLdouble; slices: Cardinal);

{ rysuje strzalke. z = const = 0, strzalka jest skierowana w gore, ma wysokosc = 2
  (tzn. od y = 0 do y = 2) i szerokosc 1 (x = -0.5 do 0.5).
  Wersja druga umozliwia podanie grubosci i szerokosci grota (obie wartosci
  od 0 do 1), wersja pierwsza przyjmuje sensowne wartosci dla "typowej"
  strzalki.
  Wszystkie elementy sa rysowane CCW gdy na nie patrzec ze standardowego
  widoku (x rosnie w prawo, y w gore). }
procedure DrawArrow; overload;
procedure DrawArrow(grotThickness, grotLength: TGLfloat); overload;

{ NewGLUQuadric to tylko opakowanie na gluNewQuadric.
  Pozwala jednym wywolaniem procedury ustalic wszystkie wlasciwosci
  quadrica, ustawia callback GLU_ERROR na ReportGLerror i
  sprawdza automatycznie (i rzuca Exception jesli nie) czy result <> nil. }
function NewGLUQuadric(
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL): PGLUQuadric; overload;

{ Render sphere in OpenGL. Radius, Slices, Stacks have the same meaning
  as for gluSphere (in case they are not self-explanatory...).
  Other parameters set glu quadric parameters, see glu quadric documentation. }
procedure KamGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL);

{ rysuje rectangle od x1, y1 do x2, y2. Wspolrzedna constCoordx jest stala
  i ma zawsze wartosc constValue. Normale (jeden
  normal dla calej sciany) sa generowane. Paranetry x1, y1, x2, y2 niekoniecznie
  oznaczaja rzeczywiscie x-y i y-ki : jezeli constCoord = 0 to oznaczaja
  one odpowiednio y i z, jezeli constCoord = 1 to oznaczaja x i z.

  Texture coordinates are generated if MakeTextureCoords.
  Texture S goes from 0 to 1 when X goes from X1 to X2,
  texture T goes from 0 to 1 when Y goes from Y1 to Y2,

  Jezeli DetailLevel1 <> 0 or DetailLevel2 <> 0 to rectangle jest rozbijany
  na wiele polygonow zeby mial w sobie duzo vertexow, co powoduje ze swiatlo
  jest duzo ladniej renderowane na prostokacie. Rozbijanie jest robione
  na paski i kolumny, tak ze powstaje nam taka kratka : plane ma
  DetailLevelX+1 kolumn i DetailLevelY+1 paskow. Wiec tak naprawde jeden
  plane sklada sie z (DetailLevelX+1)*(DetailLevelY+1) kwadracikow.

  O ile tylko x1 <= y1 i x2 <= y2 to :
  - Jezeli constCoordGivesNormal1 to
    normal bedzie rowny 1 na wspolrzednej constCoord (na pozostalych bedzie = 0)
    i wszystkie polygony tworzace prostokat beda zwrocone CCW w strone
    constCoorda rosnacego.
  - Wpp. normal bedzie rowny -1 na tej wspolrzednej w wszystkie polygony beda
    zwrocone w strone constCoorda malejacego. }
procedure DrawGLPlane(x1, y1, x2, y2: TGLfloat; constValue: TGLfloat;
  constCoord, DetailLevelX, DetailLevelY: integer;
  constCoordGivesNormal1: boolean;
  MakeTextureCoords: boolean);

{ DrawGLPlaneSpecialTex umozliwia podanie wspolrzednych tekstury na
  czterech rogach prostokata. texX1, texY1 to wspolrzedne tekstury na x1, y1
  i podobnie dla texX2, texY2. Na calym prostokacie wspolrzedne tekstury
  beda rownomiernie rozlozone w zadanych granicach.

  W ten sposob mozesz narysowac rectangla ktory ma scieta teksture albo
  powtorzona.

  Ponadto, mamy tu parametr order_ST_XYZ. Jezeli true to oznacza ze
  kolejnosc wspolrzednych xyz i st jest taka sama, innymni slowy -
  jezeli np. constCoord = 1 oznacza to ze wzdluz wspolrzednej x
  biegnie wspolrzedna tekstury S, a wzdluz z - wspolrzedna tekstury T.
  Jezeli false - to na odwrot.

  Wszystkie pozostale parametry jak w DrawGLPlane. DrawGLPlane
  po prostu wywoluje ta procedure z texX1, texY1, texX2, texY2 = 0, 0, 1, 1
  order_ST_XYZ = true.

  Jezeli MakeTextureCoords = @false to wspolrzedne tekstury nie sa generowane
  (i rownie dobrze moglbys uzyc zwyklego DrawGLPlane). }
procedure DrawGLPlaneSpecialTex(x1, y1, x2, y2: TGLfloat; constValue: TGLfloat;
  constCoord, DetailLevelX, DetailLevelY: integer;
  constCoordGivesNormal1: boolean;
  MakeTextureCoords: boolean;
  texX1, texY1, texX2, texY2: TGLfloat; order_ST_XYZ: boolean);

{ Rysuje Box3D. Starsze wersje pobieraja szesc parametrow zamiast jednego TBox3D,
  w ich przypadku nie musi byc x1 <= x2, y1 <= y2 itd. (x1, y1, z1) i (x2, y2, z2)
  musza tylko okreslac dwa przeciwlegle punkty prostopadloscianu (beda
  konwertowane Box3DOrderUp).

  Sciany sa rysowane przy pomocy DrawGLPlane i moga byc tam rozkladane na
  wieksza ilosc malych trojkatow (zeby cieniowanie Gourauda lepiej sie
  renderowalo na boxie) jezeli ktores z DetailX, Y lub Z jest <>0.

  Na boxie sa generowane wspolrzedne normale. Wszystkie sciany
  sa CCW z punktu widzenia zewnatrz i wszystkie normale wskazuja na zewnatrz
  jezeli ccwOutside, wpp. CCW jest do wewnatrz i wszystkie normale wskazuja
  tam.

  Texture coordinates are generated if MakeTextureCoords. }
procedure DrawGLBox(const Box: TBox3D; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean; MakeTextureCoords: boolean); overload;
procedure DrawGLBox(const x1, y1, z1, x2, y2, z2: TGLfloat; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean; MakeTextureCoords: boolean); overload;

(*
{ DrawGLBoxWire is like DrawGLBox, but draws only wireframe.
  It uses DetailX, DetailY, DetailZ parameters, and generates
  even normal vectors and texture coordinates like DrawGLBox.

  Requires one attrib stack place.

  Unfortunately, current implementation is not correct because of
  Radeon bug. To see the bug, just use DrawGLBoxWire(Box, 0, 0, 0, true)
  as an implementation for glDrawBox3DWire. Then rotate the DrawGLBoxWire
  (e.g. view3dscene draws scene bounding box using this routine).
  You'll see that at some angles of view, Radeon draws a strange
  diagonal lines inside GL_QUAD_STRIPs... obviously they draw
  each quad inside GL_QUAD_STRIP as two triangles and they don't
  do glEdgeFlag(GL_FALSE) where they should.

  NVidia drivers on NVidia cards work correctly. }
procedure DrawGLBoxWire(const Box: TBox3D; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean); overload;
procedure DrawGLBoxWire(const x1, y1, z1, x2, y2, z2: TGLfloat; DetailX, DetailY, DetailZ: integer; ccwOutside: boolean); overload;
*)

{ glDrawBox3DWire draws a simple lines around this TBox3D.
  It doesn't generate any texture coords or normal vectors
  --- it only draws 8 lines. }
procedure glDrawBox3DWire(const Box: TBox3D);

{ Simplest drawing of Box into OpenGL, just as a six planes.
  Nothing is generated besides vertexes position --- no normal vectors,
  no texture coords, nothing. Order is CCW outside (so if you want, you
  can turn on backface culling yourself).

  You @bold(must enable GL_VERTEX_ARRAY before using this).
  (It's not done automatically, as it's much faster to do it once
  for many glDrawBox3DSimple calls. Example --- bzwgen city view behind
  building 1, with occlusion query used: FPC 150 vs 110 when
  GL_VERTEX_ARRAY is activated once in OcclusionBoxStateBegin, not here.
  Tested on fglrx on Radeon X1600 (chantal).)

  It can be safely placed in a display list. }
procedure glDrawBox3DSimple(const Box: TBox3D);

{ rysuje trojkat o zadanych wierzcholkach i wspolrzednych tekstury,
    generuje tez normal jako Normalized(p2-p1, p2-p3) a wiec normal
    kieruje sie w strone z ktorej trojkat wyglada na CCW.
  Przy DetailLev > 0 trojkat jest rozbijany na mniejsze trojkaty (jak - patrz
    kartka). W ten sposob trojkat jest rysowany przy uzyciu wiekszej liczby
    vertexow niz trzeba co pozwala na ladniejsze renderowanie swiatla na
    trojkacie.
  Aby kontrolowac to w ktora strone jest CCW wiedz ze mimo ze tak
    naprawde narysowanych zostanie wiele trojkatow to wszystkie one beda
    tak samo zorientowane jak trojkat p1-p2-p3. }
procedure DrawGLTriangle(const p1, p2, p3: TVector3f;
  const Tex1, Tex2, Tex3: TVector2f;
  DetailLev: Cardinal);

{ glProjectionPushPop2D na chwile zmienia matryce na
  Ortho2d (0, Viewport.width, 0, Viewport.height), wywoluje proc,
  a potem przywraca poprzednia matryce projection.

  W momencie wywolania aktualna matryca nie musi byc GL_PROJECTION -
  w momencie wywolania proc aktualna matryca OpenGL'a bedzia ta sama co
  w momencie wywolania glProjectionPushPop2D, np. jesli wywolales
  glProjectionPushPop2D gdy aktualna matryca byla GL_MODELVIEW to
  w momencie wywolania proc i po wyjsciu z glProjectionPushPop2D aktualna
  matryca tez bedzie GL_MODELVIEW.

  Bardziej ogolne glProjectionPushPop umozliwia podanie dowolnej matrycy
  na ktora bedzie ustawiona matrix projection. }

type
  TProcData = procedure (Data: Pointer);

procedure glProjectionPushPop2D(proc: TProcData; Data: Pointer);

procedure glProjectionPushPopOrtho(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top, ZNear, ZFar: TGLdouble);
procedure glProjectionPushPopOrtho2D(proc: TProcData; Data: Pointer;
  const Left, Right, Bottom, Top: TGLdouble);
procedure glProjectionPushPopPerspective(proc: TProcData; Data: Pointer;
  const FovyDeg, Aspect, ZNear, ZFar: TGLdouble);
procedure glProjectionPushPop(proc: TProcData; Data: Pointer;
  const projMatrix: TMatrix4f);

{ BlackOutRect rysuje prostokat zabarwiony BlackOutem kolorem BlackOutColor.
  BlackOutIntensity okresla intensywnosc BlackOut'u - od 1 w dol.
  Gdy BlackOutIntensity <= 0 ta procedura nie zrobi kompletnie nic.
  Wpp. zostanie narysowany prostokat glRectf(x1, y1, x2, y2) ktory zanieczysci
  obszar jaki bedzie za nim na kolor BlackOutColor (pamietaj wiec ustawic
  najpierw matrix odpowiednio, bo bedzie wplywala na wynik glRect()).

  Wymaga 1 miejsca na stosie attribow, zachowuje stan OpenGL'a.

  Dokladnie rownania : czym jest to "zanieczyszczenie" kolorem BlackOutColor
  sa kwestia implementacji - zrobione sa tak zeby byly ladne.
  Typowa implementacja blackouta :
    w Draw wywolaj ta proc,
    w Idle zmniejszaj BlackOutIntensity (o ile jest >0),
    w dowolnym momencie aby wywolac blackouta zrob BlackOutIntensity := 1
      i ustaw BlackOutColor

  You can place this inside display list. }
procedure DrawGLBlackOutRect(const BlackOutColor: TVector3f;
  const BlackOutIntensity, x1, y1, x2, y2: TGLfloat);

{ Returns multiline string describing attributes of current OpenGL
  library. This simply queries OpenGL using glGet* functions
  about many things. Does not change OpenGL state in any way.

  Note that the last line of returned string does not terminate
  with newline character (so e.g. you may want to do
  Writeln(GLCapsString) instead of just Write(GLCapsString)). }
function GLCapsString: string;

{ Vector version of glClearColor, equivalent to
  glClearColor(v[0], v[1], v[2], alpha) }
procedure glClearColorv(const v: TVector3Single; alpha: Single);

const
  GLDefaultLightModelAmbient: TVector4Single = (0.2, 0.2, 0.2, 1.0);

{ Utilities for display lists ---------------------------------------- }

type
  EOpenGLNoMoreDisplayLists = class(Exception)
  end;

{ This calls glGenLists(range) and checks the result.

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

{ This is equivalent to glListBase but it's parameter is a signed integer.

  Original declararations of glListBase take unsigned integer,
  while actually a signed integer is also allowed. Actually,
  you should always call gListBase with range/overflow checking turned off.
  That's because argument to glListBase is used by OpenGL
  only in an expression like
    @longcode# Base + CurrentListNumber #
  so it's the "CurrentListNumber" that determines what Base actually means
  - e.g. if Base = LongWord(-100) and CurrentListNumber = 1000 then
  the actual list number is 900, and this is all that matters.
  So you can say that Base was nagative.
  But if Base = LongWord(-100) and CurrentListNumber = 0 then
  the actual list number is LongWord(-100) = <some big integer around 4 * 10^9>.
  So you can say that Base was positive. }
var
  glListIBase: procedure(base: TGLint);
    {$ifdef OPENGL_CDECL} cdecl; {$endif}
    {$ifdef OPENGL_STDCALL} stdcall; {$endif}

{ Set color buffer and depth buffer writeable or not writeable.
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
function KamGL_CLAMP_TO_EDGE: TGLenum;

{$undef read_interface}

implementation

{$define read_implementation}

uses KambiFilesUtils, KambiStringUtils, GLVersionUnit, GLShaders, GLImages,
  KambiLog;

{$I glext_packed_depth_stencil.inc}

{$ifndef USE_OLD_OPENGLH}
{$I opengltypes.inc}

procedure LoadAllExtensions;

  {$ifdef NEEDS_FOG_COORD_FIX}
  function Load_GL_EXT_fog_coord: Boolean;
  var
    extstring: String;
  begin

    Result := FALSE;
    extstring := String(PChar(glGetString(GL_EXTENSIONS)));

    if glext_ExtensionSupported('GL_EXT_fog_coord', extstring) then
    begin
      Pointer(glFogCoordfEXT) := wglGetProcAddress('glFogCoordfEXT');
      if not Assigned(glFogCoordfEXT) then Exit;
      Pointer(glFogCoorddEXT) := wglGetProcAddress('glFogCoorddEXT');
      if not Assigned(glFogCoorddEXT) then Exit;
      Pointer(glFogCoordfvEXT) := wglGetProcAddress('glFogCoordfvEXT');
      if not Assigned(glFogCoordfvEXT) then Exit;
      Pointer(glFogCoorddvEXT) := wglGetProcAddress('glFogCoorddvEXT');
      if not Assigned(glFogCoorddvEXT) then Exit;
      Pointer(glFogCoordPointerEXT) := wglGetProcAddress('glFogCoordPointerEXT');
      if not Assigned(glFogCoordPointerEXT) then Exit;
      Result := TRUE;
    end;

  end;
  {$endif}

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

 GL_ARB_imaging := Load_GL_ARB_imaging;
 GL_ARB_multitexture := Load_GL_ARB_multitexture;
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

 GLMaxTextureSize := glGetInteger(GL_MAX_TEXTURE_SIZE);

 if GL_ARB_multitexture then
   GLMaxTextureUnitsARB := glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) else
   GLMaxTextureUnitsARB := 0;

 if GL_ARB_texture_cube_map then
   GLMaxCubeMapTextureSizeARB := glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB) else
   GLMaxCubeMapTextureSizeARB := 0;

 if GL_EXT_texture3D then
 begin
   GLMax3DTextureSizeEXT := glGetInteger(GL_MAX_3D_TEXTURE_SIZE_EXT);
   if GLMax3DTextureSizeEXT = 0 then
   begin
     GL_EXT_texture3D := false;
     if Log then WritelnLog('OpenGL', 'Buggy OpenGL EXT_texture3D: reported as supported, but GL_MAX_3D_TEXTURE_SIZE_EXT is zero. (Bug may be found on Mesa 7.0.4.)');
   end;
 end else
   GLMax3DTextureSizeEXT := 0;

 if GL_EXT_texture_filter_anisotropic then
   GLMaxTextureMaxAnisotropyEXT := glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT) else
   GLMaxTextureMaxAnisotropyEXT := 0.0;

 if GL_ARB_occlusion_query then
   glGetQueryivARB(GL_SAMPLES_PASSED_ARB, GL_QUERY_COUNTER_BITS_ARB, @GLQueryCounterBits) else
   GLQueryCounterBits := 0;

 if GL_EXT_framebuffer_object then
 begin
   GLMaxRenderbufferSize := glGetInteger(GL_MAX_RENDERBUFFER_SIZE_EXT);
   if GLMaxRenderbufferSize = 0 then
   begin
     GL_EXT_framebuffer_object := false;
     if Log then WritelnLog('OpenGL', 'Buggy OpenGL EXT_framebuffer_object: reported as supported, but GL_MAX_RENDERBUFFER_SIZE_EXT is zero. (Bug may be found on Mesa 7.0.4.)');
   end;
 end else
   GLMaxRenderbufferSize := 0;
end;
{$endif}

{ klasa EOpenGLError i procedura CheckGLErrors
 ---------------------------------------------------------------------------------------}

constructor EOpenGLError.Create(ErrorGL: TGLenum);
begin
 Create(ErrorGL, '');
end;

constructor EOpenGLError.Create(ErrorGL: TGLenum; const AdditionalComment: string);
var MessagePrefix: string;
begin
 if AdditionalComment <> '' then
  MessagePrefix := AdditionalComment + nl else
  MessagePrefix := '';
 inherited Create(
   MessagePrefix + 'OpenGL error (number ' +IntToStr(ErrorGL) +
   ') reported : ' + gluErrorString(ErrorGL));
 ErrorGLCode := ErrorGL;
end;

procedure CheckGLErrors(const AdditionalComment: string);
var ErrorCode: TGLenum;
begin
 ErrorCode := glGetError();
 if ErrorCode <> GL_NO_ERROR then
  raise EOpenGLError.Create(ErrorCode, AdditionalComment);
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

procedure glColorv(const v: TVector3b);  begin glColor3bv(@v); end;
procedure glColorv(const v: TVector3ub); begin glColor3ubv(@v); end;
procedure glColorv(const v: TVector4b);  begin glColor4bv(@v); end;
procedure glColorv(const v: TVector4ub); begin glColor4ubv(@v); end;
procedure glNormalv(const v: TVector3b); begin glNormal3bv(@v); end;

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

procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1d);  begin glMultiTexCoord1dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1f);  begin glMultiTexCoord1fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1i);  begin glMultiTexCoord1iv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector1s);  begin glMultiTexCoord1sv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2d);  begin glMultiTexCoord2dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2f);  begin glMultiTexCoord2fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2i);  begin glMultiTexCoord2iv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector2s);  begin glMultiTexCoord2sv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3d);  begin glMultiTexCoord3dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3f);  begin glMultiTexCoord3fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3i);  begin glMultiTexCoord3iv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector3s);  begin glMultiTexCoord3sv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4d);  begin glMultiTexCoord4dv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4f);  begin glMultiTexCoord4fv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4i);  begin glMultiTexCoord4iv(Target, @v); end;
procedure glMultiTexCoordv(const Target: TGLEnum; const v: TVector4s);  begin glMultiTexCoord4sv(Target, @v); end;

{$ifdef IMPLEMENT_OPENGL_STUBS}

procedure glNormalv(const v: TVector3d); begin glNormal3dv(@v); end;
procedure glNormalv(const v: TVector3f); begin glNormal3fv(@v); end;
procedure glNormalv(const v: TVector3i); begin glNormal3iv(@v); end;
procedure glNormalv(const v: TVector3s); begin glNormal3sv(@v); end;

procedure glColorv(const v: TVector3d);  begin glColor3dv(@v); end;
procedure glColorv(const v: TVector3f);  begin glColor3fv(@v); end;
procedure glColorv(const v: TVector3i);  begin glColor3iv(@v); end;
procedure glColorv(const v: TVector3s);  begin glColor3sv(@v); end;
procedure glColorv(const v: TVector3ui);  begin glColor3uiv(@v); end;
procedure glColorv(const v: TVector3us);  begin glColor3usv(@v); end;

procedure glColorv(const v: TVector4d);  begin glColor4dv(@v); end;
procedure glColorv(const v: TVector4f);  begin glColor4fv(@v); end;
procedure glColorv(const v: TVector4i);  begin glColor4iv(@v); end;
procedure glColorv(const v: TVector4s);  begin glColor4sv(@v); end;
procedure glColorv(const v: TVector4ui);  begin glColor4uiv(@v); end;
procedure glColorv(const v: TVector4us);  begin glColor4usv(@v); end;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f);  begin glMaterialfv(face, pname, @params); end;
procedure glMaterialv(face, pname: TGLEnum; const params: TVector4i);  begin glMaterialiv(face, pname, @params); end;

procedure glVertexv(const v: TVector2d);  begin glVertex2dv(@v); end;
procedure glVertexv(const v: TVector2f);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector2i);  begin glVertex2iv(@v); end;
procedure glVertexv(const v: TVector2s);  begin glVertex2sv(@v); end;

procedure glVertexv(const v: TVector3d);  begin glVertex3dv(@v); end;
procedure glVertexv(const v: TVector3f);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector3i);  begin glVertex3iv(@v); end;
procedure glVertexv(const v: TVector3s);  begin glVertex3sv(@v); end;

procedure glVertexv(const v: TVector4d);  begin glVertex4dv(@v); end;
procedure glVertexv(const v: TVector4f);  begin glVertex4fv(@v); end;
procedure glVertexv(const v: TVector4i);  begin glVertex4iv(@v); end;
procedure glVertexv(const v: TVector4s);  begin glVertex4sv(@v); end;

procedure glTexCoordv(const v: TVector1d);  begin glTexCoord1dv(@v); end;
procedure glTexCoordv(const v: TVector1f);  begin glTexCoord1fv(@v); end;
procedure glTexCoordv(const v: TVector1i);  begin glTexCoord1iv(@v); end;
procedure glTexCoordv(const v: TVector1s);  begin glTexCoord1sv(@v); end;
procedure glTexCoordv(const v: TVector2d);  begin glTexCoord2dv(@v); end;
procedure glTexCoordv(const v: TVector2f);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector2i);  begin glTexCoord2iv(@v); end;
procedure glTexCoordv(const v: TVector2s);  begin glTexCoord2sv(@v); end;
procedure glTexCoordv(const v: TVector3d);  begin glTexCoord3dv(@v); end;
procedure glTexCoordv(const v: TVector3f);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector3i);  begin glTexCoord3iv(@v); end;
procedure glTexCoordv(const v: TVector3s);  begin glTexCoord3sv(@v); end;
procedure glTexCoordv(const v: TVector4d);  begin glTexCoord4dv(@v); end;
procedure glTexCoordv(const v: TVector4f);  begin glTexCoord4fv(@v); end;
procedure glTexCoordv(const v: TVector4i);  begin glTexCoord4iv(@v); end;
procedure glTexCoordv(const v: TVector4s);  begin glTexCoord4sv(@v); end;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4d);  begin glTexGendv(coord, pname, @params); end;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f);  begin glTexGenfv(coord, pname, @params); end;
procedure glTexGenv(coord, pname: TGLenum; const params: TVector4i);  begin glTexGeniv(coord, pname, @params); end;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector4i);  begin glLightiv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3i);  begin glLightiv(light, pname, @params); end;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); begin glLightModelfv(pname, @params); end;
procedure glLightModelv(pname: TGLenum; const params: TVector4i); begin glLightModeliv(pname, @params); end;

procedure glFogv(pname: TGLEnum; const params: TVector4f);  begin glFogfv(pname, @params); end;
procedure glFogv(pname: TGLEnum; const params: TVector4i);  begin glFogiv(pname, @params); end;

procedure glMultMatrix(const m: TMatrix4f); begin glMultMatrixf(@m) end;
procedure glMultMatrix(const m: TMatrix4d); begin glMultMatrixd(@m) end;
procedure glLoadMatrix(const m: TMatrix4f); begin glLoadMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4d); begin glLoadMatrixd(@m) end;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); begin glTexEnvfv(target, pname, @params); end;
procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4i); begin glTexEnviv(target, pname, @params); end;

{$endif IMPLEMENT_OPENGL_STUBS}

{ uproszczenia dla sejwowania / ladowania gl state : ---------------------------------- }

procedure SavePixelStoreUnpack(out pixUnpack: TPixelStoreUnpack);
begin
 with pixUnpack do begin
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
 with pixUnpack do begin
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
var oldMatrixMode: TGLenum;
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
var oldMatrixMode: TGLenum;
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

procedure ProjectionGLOrtho(const left, right, bottom, top: TGLdouble);
begin
 ProjectionGLOrtho(left, right, bottom, top, -1, 1);
end;

{ poly stipple ------------------------------------------------------------ }

function RandomPolyStipple(const BlackChance: Extended): TPolygonStipple;
var i: integer;
begin
 for i := 0 to High(result) do result[i] := RandomBitsByte(BlackChance);
end;

function RandomPolyStippleBy16(const BlackChance: Extended): TPolygonStipple;
var b: byte;
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
var b: byte;
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
    KamGLPolygonStipple(Stipple);
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

procedure DrawGLRectBorder(const Area: TArea);
begin
  DrawGLRectBorder(Area.X0, Area.Y0, Area.X0 + Area.Width, Area.Y0 + Area.Height);
end;

procedure DrawGLBorderedRectangle(const R: TIntRect;
  const InsideCol, BorderCol: TVector4f);
begin
 DrawGLBorderedRectangle(R[0, 0], R[0, 1], R[1, 0], R[1, 1],
   InsideCol, BorderCol);
end;

procedure DrawGLBorderedRectangle(const R: TIntRect;
  const InsideCol, BorderCol: TVector4f; Stipple: PPolygonStipple);
begin
 DrawGLBorderedRectangle(R[0, 0], R[0, 1], R[1, 0], R[1, 1],
   InsideCol, BorderCol, Stipple);
end;

procedure DrawGLRectBorder(const R: TIntRect);
begin
 DrawGLRectBorder(R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
end;

function UnProjectGL(winx, winy, winz :TGLdouble): TVector3d;
var
  modelMatrix, projMatrix:
    {$ifdef USE_OLD_OPENGLH} TMatrix4d {$else} T16dArray {$endif};
  viewport:
    {$ifdef USE_OLD_OPENGLH} TVector4i {$else} TViewPortArray {$endif};
begin
 glGetDoublev(GL_MODELVIEW_MATRIX, @modelMatrix);
 glGetDoublev(GL_PROJECTION_MATRIX, @projMatrix);
 glGetIntegerv(GL_VIEWPORT, @viewport);
 Check( gluUnProject(winx, winy, winz,
   {$ifdef USE_OLD_OPENGLH} @ {$endif} modelMatrix,
   {$ifdef USE_OLD_OPENGLH} @ {$endif} projMatrix,
   {$ifdef USE_OLD_OPENGLH} @ {$endif} viewport,
   @result[0], @result[1], @result[2]) = GL_TRUE, 'gluUnProject');
end;

procedure fullGLDiskSqueezedTex(radius: TGLdouble; slices: Cardinal);
var s, sna2: TGLfloat;

  function Mapuj(a: TGLdouble): TGLfloat;
  {mapuj a z zakresu -sna2..sna2 na zakres 0..1}
  begin
   result := a/s + 0.5;
  end;

var x, y, sliceAngle, angle: TGLdouble;
    i: cardinal;
begin
 sliceAngle := 360 / slices;
 sna2 := radius/sqrt2;
 s := 2*sna2;

 glBegin(GL_TRIANGLE_FAN);
 try
  {punkt w srodku}
  glTexCoord2f(0.5, 0.5);
  glVertex2f(0, 0);

  for i := 0 to slices do { nie ma tu bledu - narysujemy slices trojkatow. bo uzywamy TRIANGLE_FAN }
  begin
   angle := sliceAngle*i; { liczac angle w ten sposob (a nie sumujac kolejne sliceAngle)
                          unikamy kumulacji bledow dodawania }
   x := cos(DegToRad(angle))*radius;
   y := sin(DegToRad(angle))*radius;

   {w taki sposob moglibysmy tu otrzymac texture coords takie same jak gluDisk :
      glTexCoord2f(x/(2*radius) + 0.5, y/(2*radius) + 0.5);
    ale my chcemy generowac texture coords SQUEEZED}
   if (angle <= 45) or (angle > 360-45) then
    glTexCoord2f(1, Mapuj(y)) else
   if (angle > 45) and (angle <= 90+45) then
    glTexCoord2f(Mapuj(x), 1) else
   if (angle > 90+45) and (angle <= 180+45) then
    glTexCoord2f(0, Mapuj(y)) else
    glTexCoord2f(Mapuj(x), 0);

   glVertex2d(x, y);
  end;
 finally glEnd end;
end;

procedure DrawArrow;
begin
 DrawArrow(0.4, 0.5);
end;

procedure DrawArrow(grotThickness, grotLength: TGLfloat);
begin
 grotLength := 2*grotLength; { mapuj grotLength na zakres 0..2 }

 { TODO: tutaj powinienes przelaczac glEdgeFlag }

 glBegin(GL_TRIANGLES);
  glVertex2f(0, 2);
  glVertex2f(-1, grotLength);
  glVertex2f(-grotThickness, grotLength);

  glVertex2f(0, 2);
  glVertex2f(-grotThickness, grotLength);
  glVertex2f(grotThickness, grotLength);

  glVertex2f(0, 2);
  glVertex2f(grotThickness, grotLength);
  glVertex2f(1, grotLength);
 glEnd;

 glBegin(GL_QUADS);
  glVertex2f(-grotThickness, grotLength);
  glVertex2f(-grotThickness, 0);
  glVertex2f(grotThickness, 0);
  glVertex2f(grotThickness, grotLength);
 glEnd;
end;

function NewGLUQuadric(texture: boolean; normals: TGLenum;
  orientation: TGLenum; drawStyle: TGLenum): PGLUQuadric;
begin
 result := gluNewQuadric();
 Check(result <> nil, 'gluNewQuadric');
 gluQuadricCallback(result, GLU_ERROR,
   {$ifndef USE_OLD_OPENGLH} TCallBack {$endif} (@ReportGLError));
 gluQuadricTexture(result, Ord(texture));
 gluQuadricNormals(result, normals);
 gluQuadricOrientation(result, orientation);
 gluQuadricDrawStyle(result, drawStyle);
end;

procedure KamGluSphere(
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
  MakeTextureCoords: boolean);
begin
 DrawGLPlaneSpecialTex(x1, y1, x2, y2, constValue,
   constCoord, DetailLevelX, DetailLevelY,
   constCoordGivesNormal1,
   MakeTextureCoords, 0, 0, 1, 1, true);
end;

procedure DrawGLPlaneSpecialTex(x1, y1, x2, y2: TGLfloat; constValue: TGLfloat;
  constCoord, DetailLevelX, DetailLevelY: integer;
  constCoordGivesNormal1: boolean;
  MakeTextureCoords: boolean;
  texX1, texY1, texX2, texY2: TGLfloat; order_ST_XYZ: boolean);
var xstep, ystep, texXStep, texYStep, texY, y, ynext, texYnext: TGLfloat;
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
 DrawGLPlane(Box[0, 1], Box[0, 2], Box[1, 1], Box[1, 2], Box[0, 0], 0, DetailY, DetailZ, not ccwOutside, MakeTextureCoords);
 DrawGLPlane(Box[0, 1], Box[0, 2], Box[1, 1], Box[1, 2], Box[1, 0], 0, DetailY, DetailZ, ccwOutside    , MakeTextureCoords);

 DrawGLPlane(Box[0, 0], Box[0, 2], Box[1, 0], Box[1, 2], Box[0, 1], 1, DetailX, DetailZ, not ccwOutside, MakeTextureCoords);
 DrawGLPlane(Box[0, 0], Box[0, 2], Box[1, 0], Box[1, 2], Box[1, 1], 1, DetailX, DetailZ, ccwOutside    , MakeTextureCoords);

 DrawGLPlane(Box[0, 0], Box[0, 1], Box[1, 0], Box[1, 1], Box[0, 2], 2, DetailX, DetailY, not ccwOutside, MakeTextureCoords);
 DrawGLPlane(Box[0, 0], Box[0, 1], Box[1, 0], Box[1, 1], Box[1, 2], 2, DetailX, DetailY, ccwOutside    , MakeTextureCoords);
end;

procedure DrawGLBox(const x1, y1, z1, x2, y2, z2: TGLfloat;
  DetailX, DetailY, DetailZ: integer; ccwOutside: boolean;
  MakeTextureCoords: boolean);
begin
 DrawGLBox(Box3DOrderUp(Vector3Single(x1, y1, z1), Vector3Single(x2, y2, z2)),
   DetailX, DetailY, DetailZ, ccwOutside, MakeTextureCoords);
end;

(*
procedure DrawGLBoxWire(const Box: TBox3D; DetailX, DetailY, DetailZ: integer;
  ccwOutside: boolean);
begin
  glPushAttrib(GL_POLYGON_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    DrawGLBox(Box, DetailX, DetailY, DetailZ, ccwOutside);
  glPopAttrib;
end;

procedure DrawGLBoxWire(const x1, y1, z1, x2, y2, z2: TGLfloat;
  DetailX, DetailY, DetailZ: integer; ccwOutside: boolean);
begin
 DrawGLBoxWire(Box3DOrderUp(Vector3Single(x1, y1, z1), Vector3Single(x2, y2, z2)),
   DetailX, DetailY, DetailZ, ccwOutside);
end;
*)

procedure glDrawBox3DWire(const Box: TBox3D);

  { BoxVertex(0..3, 0) are the four vertexes of front face,
    BoxVertex(0..3, 1) are the four vertexes of back face
    (ordered in the same order, suitable for GL_LINE_LOOP). }
  procedure BoxVertex(Index: Integer; Z: Integer);
  const
    X: array [0..3] of Integer = (0, 1, 1, 0);
    Y: array [0..3] of Integer = (0, 0, 1, 1);
  begin
    glVertex3f(Box[X[Index], 0], Box[Y[Index], 1], Box[Z, 2]);
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
  if IsEmptyBox3D(Box) then Exit;

  { Verts index in octal notation indicates which of 8 vertexes it is. }
  Verts[0] := Box[0];
  Verts[1] := Box[0]; Verts[1][0] := Box[1][0];
  Verts[2] := Box[0]; Verts[2][1] := Box[1][1];
  Verts[4] := Box[0]; Verts[4][2] := Box[1][2];

  Verts[3] := Box[1]; Verts[3][2] := Box[0][2];
  Verts[5] := Box[1]; Verts[5][1] := Box[0][1];
  Verts[6] := Box[1]; Verts[6][0] := Box[0][0];
  Verts[7] := Box[1];

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
   vresult := Mix2Vectors(v1, v2, v2part);
   texResult := Mix2Vectors(tex1, tex2, v2part);
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
  var vresult: TVector3f;
      texResult: TVector2f;
  begin
   vresult := Mix2Vectors(v1, v2, v2part);
   texResult := Mix2Vectors(tex1, tex2, v2part);
   glTexCoordv(texResult); glVertexv(vresult);
  end;

var i, j: Cardinal;
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

function GLCapsString: string;

  function GetInteger(param: TGLenum): string;
  begin
   result := IntToStr(glGetInteger(param));
  end;

  function GetInteger2(param: TGLenum; const form: string): string;
  var v: packed array[0..1]of TGLint;
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
        '  Buggy glPushAttrib(GL_POINT_SET): %s' +nl+
        '  Buggy glDrawPixels for odd widths: %s',
        [ BoolToStr[Version.VendorNVidia],

          BoolToStr[Version.IsMesa],
          Version.MesaMajor, Version.MesaMinor, Version.MesaRelease,

          BoolToStr[Version.IsVendorATI], BoolToStr[Version.IsFglrx],

          BoolToStr[Version.BuggyPointSetAttrib],
          BoolToStr[Version.BuggyDrawOddWidth]
        ]);
  end;

  function GetMaxTextureUnits: string;
  begin
    if GL_ARB_multitexture then
      Result := IntToStr(GLMaxTextureUnitsARB) else
      Result := 'ARB_multitexture not available';
  end;

  function GetMaxCubeMapTextureSize: string;
  begin
    if GL_ARB_texture_cube_map then
      Result := IntToStr(GLMaxCubeMapTextureSizeARB) else
      Result := 'ARB_texture_cube_map not available';
  end;

  function GetMaxTexture3DSize: string;
  begin
    if GL_EXT_texture3D then
      Result := IntToStr(GLMax3DTextureSizeEXT) else
      Result := 'EXT_texture3D not available';
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
    if GL_EXT_framebuffer_object then
      Result := IntToStr(GLMaxRenderbufferSize) else
      Result := 'EXT_framebuffer_object not available';
  end;

begin
 result:=
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
  nl+

  '---------' +nl+
  'Features:' +nl+
  '  GLSL shaders support: ' + GLSupportNames[TGLSLProgram.ClassSupport] +nl+
  '  Assembly ARB vertex program support: ' + GLSupportNames[TARBVertexProgram.ClassSupport] +nl+
  '  Assembly ARB fragment program support: ' + GLSupportNames[TARBFragmentProgram.ClassSupport] +nl+
  '  GenerateMipmap available: ' + BoolToStr[HasGenerateMipmap] +nl+
  '  Extensions: ' +glGetString(GL_EXTENSIONS) +nl+
  nl+

  '----------------------------' +nl+
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
  '  Max clip planes: ' +GetInteger(GL_MAX_CLIP_PLANES) +nl+
  '  Max eval order: ' +GetInteger(GL_MAX_EVAL_ORDER) +nl+
  '  Max lights: ' +GetInteger(GL_MAX_LIGHTS) +nl+
  '  Max list nesting: ' +GetInteger(GL_MAX_LIST_NESTING) +nl+
  '  Max pixel map table: ' +GetInteger(GL_MAX_PIXEL_MAP_TABLE) +nl+
  '  Max texture size: ' + IntToStr(GLMaxTextureSize) +nl+
  '  Max viewport dims: ' +GetInteger2(GL_MAX_VIEWPORT_DIMS, 'width %d / height %d') +nl+
  '  Max texture units: ' + GetMaxTextureUnits +nl+
  '  Max cube map texture size: ' + GetMaxCubeMapTextureSize +nl+
  '  Max 3d texture size: ' + GetMaxTexture3DSize +nl+
  '  Max texture max anisotropy: ' + GetMaxTextureMaxAnisotropy +nl+
  '  Query counter bits (for occlusion query): ' + { for occlusion query  GL_SAMPLES_PASSED_ARB }
    GetQueryCounterBits +nl+
  '  Max renderbuffer size: ' + GetMaxRenderbufferSize;

 CheckGLErrors;
end;

procedure glClearColorv(const v: TVector3Single; alpha: Single);
begin
 glClearColor(v[0], v[1], v[2], alpha);
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

function KamGL_CLAMP_TO_EDGE: TGLenum;
begin
  if GL_version_1_2 then
    Result := GL_CLAMP_TO_EDGE else
    Result := GL_CLAMP;
end;

initialization
 { This is needed for gllistIBase declaration to be correct. }
 Assert(SizeOf(TGLint) = SizeOf(TGLuint));

  { If DISABLE_FP_EXCEPTIONS is defined then in the initialization
    this unit will mask all floating point exceptions.
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

      See also explanation in my bug report to FPC:
      [http://www.freepascal.org/mantis/view.php?id=5914]
      (note that it was written when I thought (incorrectly, see below)
      that this is Windows-only issue).

    - Linux:

      Radeon open-source OpenGL driver may cause EDivByZero exceptions.
      Reported by Daniel Mantione with Radeon Mobility M7,
      steps to reproduce: just run "The Castle" and enter "New Game",
      EDivByZero is raised from inside of glCallList inside
      TVRMLFLATSCENEGL__SSS_RENDERSHAPE, line 1213 of vrmlglscene.pas.
      Disabling fp exceptions fixed the problem.

      NVidia proprietary drivers exit with EDivByZero on the first
      glXMakeCurrent call (done also by glut in first glutCreateWindow,
      done also by GTK glarea in first gtk_glarea_make_current)
      when no depth buffer was requested.

    I want to mention here that Mesa3d doesn't cause such problems.
    So maybe it's possible to make an efficient OpenGL
    implementation that doesn't require caller to disable fp exceptions ?
    Other implementors: please take this good example. Thanks.

    For more reasoning see related bug reports I submitted to FPC:
    - see FPC bug [http://www.freepascal.org/mantis/view.php?id=5914]
      (old id is 3955)
    - see FPC bug [http://www.freepascal.org/mantis/view.php?id=7570]

    Although FPC 2.2.0 does this in GL unit, it's under ifdef x86...
    and x86 doesn't seem to be defined... see
    [http://bugs.freepascal.org/view.php?id=10507]. }
  {$define DISABLE_FP_EXCEPTIONS}

  {$ifdef DISABLE_FP_EXCEPTIONS}
  Set8087CW($133F);
  {$endif}

 Pointer(glListIBase) := glListBase;
 Pointer(KamGLPolygonStipple) := glPolygonStipple;
end.
