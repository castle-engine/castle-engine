{
  Copyright 2001-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various low-level utilities for working with OpenGL. }
unit CastleGLUtils;

{$I castleconf.inc}
{$I openglmac.inc}

interface

uses Math, CastleImages, CastleGL, SysUtils, CastleUtils, CastleVectors,
  Matrix, CastleRectangles, CastleColors;

{$define read_interface}

type
  TGLSupport = (gsNone, gsExtension, gsStandard);

const
  GLSupportNames: array [TGLSupport] of string =
  ( 'None', 'Extension', 'Standard' );

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
  {$ifndef OpenGLES}
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;
  {$endif not OpenGLES}

type
  { OpenGL features, analyzed based on OpenGL extensions and version.
    A single instance of this class is assigned to @link(GLFeatures)
    after the first OpenGL context was created.

    If you have multiple OpenGL contexts, our engine assumes they share resources
    and have equal features. }
  TGLFeatures = class
  public
    {$ifndef OpenGLES}
    { OpenGL versions supported. Checked by looking at GL version string
      @italic(and) by checking whether actual entry points are available.

      This is important because bad OpenGL implementations
      (like the horrible ATI Linux closed drivers) sometimes lie,
      claiming support in the GL version string but actually lacking proper
      function entry points.
      We check the actual presence of entry points (GLExt.Load_GL_version_x_x
      do that).

      We *also* check version string (TGLVersion.AtLeast),
      since OpenGL may have some buggy entry point in case of work-in-progress
      features (not yet advertised in GL version string),
      e.g. Mesa 6.x had such buggy glStencilOpSeparate. This is correct OpenGL
      behavior AFAIK, and we handle it. }
    Version_1_2: boolean;
    Version_1_3: boolean;
    Version_1_4: boolean;
    Version_1_5: boolean;
    Version_2_0: boolean;
    Version_2_1: boolean;
    Version_3_0: boolean;
    Version_3_1: boolean;
    Version_3_2: boolean;
    Version_3_3: boolean;
    Version_4_0: boolean;
    {$endif}

    { Is the extension below loaded.
      Note: we prefer to avoid exposing directly each extension presence
      like below.
      Instead most interesting extensions are wrapped in "features"
      like @link(UseMultiTexturing), see lower. }
    ARB_shadow: boolean;
    EXT_texture_filter_anisotropic: boolean;
    NV_multisample_filter_hint: boolean;
    ARB_occlusion_query: boolean;
    {$ifndef OpenGLES}
    EXT_fog_coord: boolean;
    ARB_window_pos: boolean;
    MESA_window_pos: boolean;
    {$endif}

    { GL_CLAMP_TO_EDGE, if available in current OpenGL version.
      Otherwise GL_CLAMP.

      Use this (insteaf of direct GL_CLAMP_TO_EDGE)
      to work with @italic(really ancient) OpenGL versions before 1.2.
      Note that our engine officially supports only OpenGL >= 1.2,
      so don't expect everything to work smootly with such ancient OpenGL anyway! }
    CLAMP_TO_EDGE: TGLenum;

    { Constant (for given context) OpenGL limits.
      Initialized to 0 if appropriate OpenGL functionality is not available.
      @groupBegin }
    MaxTextureSize: Cardinal;
    MaxLights: Cardinal;
    MaxCubeMapTextureSize: Cardinal;
    MaxTexture3DSize: Cardinal;
    MaxTextureMaxAnisotropyEXT: Single;
    QueryCounterBits: TGLint;
    MaxRenderbufferSize: TGLuint;
    MaxClipPlanes: Cardinal;
    { @groupEnd }

    Multisample: boolean;

    { Number of texture units available.
      Equal to glGetInteger(GL_MAX_TEXTURE_UNITS), if multi-texturing
      available. Equal to 1 (OpenGL supports always 1 texture) otherwise. }
    MaxTextureUnits: Cardinal;

    { Are all OpenGL multi-texturing extensions for
      VRML/X3D MultiTexture support available.

      This used to check a couple of multitexturing extensions,
      like ARB_multitexture. Right now, it simply checks for OpenGL 1.3 version.
      It is supported by virtually all existing GPUs.
      So it's acceptable to just check it, and write your code for 1.3,
      and eventual fallback code (when this is false) write only for really
      ancient GPUs. }
    UseMultiTexturing: boolean;

    { Are 3D textures supported by OpenGL.
      If they are, note that GL_TEXTURE_3D and GL_TEXTURE_3D_EXT are equal,
      so often both Texture3D = gsStandard and Texture3D = gsExtension
      cases may be handled by the same code. }
    Texture3D: TGLSupport;

    { Is Framebuffer supported. Value gsExtension means that EXT_framebuffer_object
      is used, gsStandard means that ARB_framebuffer_object (which is
      a "core extesion", present the same way in OpenGL 3 core,
      also in OpenGL ES >= 2.0 core) is available. }
    Framebuffer: TGLSupport;

    { Is multisampling possible for FBO buffers and textures.
      Although these are two orthogonal features of OpenGL,
      in practice you want to use multisample for both FBO buffers and textures,
      or for none --- otherwise, FBO can not be initialized correctly
      when you mix various multisample settings. }
    FBOMultiSampling: boolean;

    { How multi-sampling was initialized for this OpenGL context.
      Value = 1 means that no multi-sampling is initialized.
      Values > 1 mean that you have multi-sampling, with given number of samples
      per pixel.
      Contrast this with TCastleWindowCustom.MultiSampling or TOpenGLControl.MultiSampling,
      that say @italic(how many samples you wanted to get). }
    CurrentMultiSampling: Cardinal;

    { Does OpenGL context have depth buffer packed with stencil buffer.
      See EXT_packed_depth_stencil extension for explanation.

      This is important for FBOs, as the depth/stencil have to be set up differently
      depending on PackedDepthStencil value.
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
    PackedDepthStencil: boolean;

    { Does OpenGL context support shadow volumes.
      This simply checks do we have stencil buffer with at least 4 bits for now. }
    ShadowVolumesPossible: boolean;

    { Are non-power-of-2 textures supported. }
    TextureNonPowerOfTwo: boolean;

    { Are cubemaps supported.

      gsExtension means GL_ARB_texture_cube_map on core OpenGL.
      gsStandard means standard feature of OpenGL or OpenGL ES.
      Since the constants defined by ARB_texture_cube_map were promoted
      to core with the same values, the distinction between gsExtension
      and gsStandard in practice doesn't exist. }
    TextureCubeMap: TGLSupport;

    { Which texture compression formats are supported. }
    TextureCompression: TTextureCompressions;

    { VBO support (in OpenGL (ES) core). }
    VertexBufferObject: boolean;

    { glBlendColor and GL_CONSTANT_ALPHA support. }
    BlendConstant: boolean;

    { Support for float texture formats for glTexImage2d. }
    TextureFloat: boolean;

    { Support for depth texture formats for glTexImage2d. }
    TextureDepth: boolean;

    constructor Create;
  end;

{ Initialize GLVersion and GLUVersion and GLFeatures. }
procedure GLInformationInitialize;

var
  GLFeatures: TGLFeatures;

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
  { OpenGL error. Usually indicates a bug in your code (or shader code,
    depending on TUniformNotFoundAction and TUniformTypeMismatchAction;
    by default, they do not cause errors).

    When programming for platforms with limited GPU memory (Android, iOS...)
    you should prepare to handle EOpenGLOutOfMemoryError (corresponding to
    GL_OUT_OF_MEMORY). This can always happen for large GPU data,
    and you should be prepared to capture it (at least around TGameSceneManager.LoadLevel)
    and display some nice information for user.
    Alternatively, you can leave GLOutOfMemoryError = @false,
    and then EOpenGLOutOfMemoryError will not happen, but you risk all kinds
    of rendering artifacts. }
  EOpenGLError = class(Exception)
  public
    ErrorCode: TGLenum;
    constructor Create(const AErrorCode: TGLenum; const AdditionalComment: string = '');
  end;

  EOpenGLOutOfMemoryError = class(EOpenGLError)
  end;

var
  { When GPU runs out of memory, raise exception (EOpenGLOutOfMemoryError)
    or merely make a warning. Merely making a warning is very risky (you risk all kinds
    of rendering artifacts), but sometimes the rendering is actually smooth
    even though GPU complains. }
  GLOutOfMemoryError: boolean = false;

procedure GLOutOfMemory(const AdditionalComment: string = '');

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string = ''): string;

{ Check are any OpenGL errors recorded (in glGetError).
  If there are errors, our behavior depends on whether we were compiled
  with -dRELEASE. With -dRELEASE, we make WritelnWarning. This way eventual
  errors in release builds don't completely abort your program.

  Note that the behavior on GL_OUT_OF_MEMORY is different.
  -dRELEASE does not matter here. Only GLOutOfMemoryError boolean dictates
  if we should raise an exception or merely make warning, regardless of -dRELEASE
  state.

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

  TODO: almost all these functions belong to deprecated fixed-function
  and do immediate operations. They should be just removed, and everything
  using them fixed to use VBO.
}

{$ifndef OpenGLES}
{ }
procedure glColorv(const v: TVector3ub); overload;
procedure glColorv(const v: TVector4ub); overload;
procedure glColorv(const v: TVector3f); overload;
procedure glColorv(const v: TVector4f); overload;

procedure glTranslatev(const V: TVector3f); overload;
procedure glTranslatev(const V: TVector3_Single); overload;

procedure glScalev(const V: Single); overload;
procedure glScalev(const V: TVector3f); overload;
procedure glScalev(const V: TVector3_Single); overload;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); overload;

procedure glClipPlane(plane: GLenum; const V: TVector4d); overload;

procedure glNormalv(const v: TVector3f); overload;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f); overload;

procedure glVertexv(const v: TVector2f); overload;
procedure glVertexv(const v: TVector2i); overload;
procedure glVertexv(const v: TVector3f); overload;
procedure glVertexv(const v: TVector3i); overload;
procedure glVertexv(const v: TVector4f); overload;
procedure glVertexv(const v: TVector4i); overload;

procedure glVertexv(const v: TVector2_Single); overload;
procedure glVertexv(const v: TVector3_Single); overload;
procedure glVertexv(const v: TVector4_Single); overload;

procedure glTexCoordv(const v: TVector2f); overload;
procedure glTexCoordv(const v: TVector3f); overload;
procedure glTexCoordv(const v: TVector4f); overload;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f); overload;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f); overload;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f); overload;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); overload;

procedure glFogv(pname: TGLEnum; const params: TVector4f); overload;

procedure glMultMatrix(const m: TMatrix4f); overload;
procedure glLoadMatrix(const m: TMatrix4f); overload;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); overload;

{$endif}

procedure GLViewport(const Rect: TRectangle);

function GetCurrentColor: TCastleColor;
procedure SetCurrentColor(const Value: TCastleColor);

{ Current color, set by glColorv and used for TCastleFont font printing
  (in case you use deprecated TCastleFont.Print overloads without
  explicit colors).
  You should not depend on this in new programs, rather use TCastleFont.Print
  with explicit Color parameter. }
property CurrentColor: TCastleColor read GetCurrentColor write SetCurrentColor;

{ Projection matrix -------------------------------------------------------- }

function GetProjectionMatrix: TMatrix4Single;
procedure SetProjectionMatrix(const Value: TMatrix4Single);

{ Current projection matrix.

  For OpenGLES, this is merely a global ProjectionMatrix variable.
  It must be passed to various shaders to honour the projection.

  For desktop OpenGL, setting this also sets fixed-function projection matrix.
  The OpenGL matrix mode is temporarily changed to GL_PROJECTION,
  then changed back to GL_MODELVIEW. }
property ProjectionMatrix: TMatrix4Single
  read GetProjectionMatrix write SetProjectionMatrix;

{ Set ProjectionMatrix to perspective or orthogonal.

  For PerspectiveProjection, ZFar may have special ZFarInfinity value
  to create a perspective projection with far plane set at infinity.
  Useful e.g. for z-fail shadow volumes.

  @groupBegin }
function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
function OrthoProjection(const left, right, bottom, top: Single;
  const zNear: Single = -1; const zFar: Single = 1): TMatrix4Single;
{ @groupEnd }

var
  { Viewport size for 2D rendering functions: DrawRectangle and TGLImageCore.Draw.
    UI container (like TCastleWindowCustom or TCastleControlCustom)
    must take care to set this before rendering. }
  Viewport2DSize: TVector2Single;

{ ---------------------------------------------------------------------------- }

{ }
procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);

{$ifndef OpenGLES}

{ Draw vertical line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TGLImageCore.Draw. }
procedure GLVerticalLine(x, y1, y2: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw horizontal line using OpenGL. Uses current OpenGL color.

  Deprecated, do not draw lines directly like this,
  instead use DrawPrimitive2D or UI interface drawing like Theme.Draw and TGLImageCore.Draw. }
procedure GLHorizontalLine(x1, x2, y: TGLfloat); deprecated 'use DrawPrimitive2D';

{ Draw arrow shape. Arrow is placed on Z = 0 plane, points to the up,
  has height = 2 (from y = 0 to y = 2) and width 1 (from x = -0.5 to 0.5).

  Everything is drawn CCW when seen from standard view (x grows right, y up).
  Uses current OpenGL color. }
procedure GLDrawArrow(HeadThickness: TGLfloat = 0.4;
  HeadLength: TGLfloat = 0.5); deprecated 'use DrawPrimitive2D to draw shapes';

{ Comfortable wrapper for gluNewQuadric. Sets all quadric parameters.
  Sets also the GLU_ERROR callback to ReportGLerror.
  @raises Exception If gluNewQuadric fails (returns nil). }
function NewGLUQuadric(
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL): PGLUQuadric; deprecated 'use TCastleScene to draw 3D stuff';

{ Render sphere in OpenGL. Radius, Slices, Stacks have the same meaning
  as for gluSphere (in case they are not self-explanatory...).
  Other parameters set glu quadric parameters, see glu quadric documentation. }
procedure CastleGluSphere(
  const Radius: TGLdouble;
  const Slices, Stacks: TGLint;
  Texture: boolean = true;
  Normals: TGLenum = GLU_NONE;
  Orientation: TGLenum = GLU_OUTSIDE;
  DrawStyle: TGLenum = GLU_FILL); deprecated 'use TCastleScene to draw 3D stuff';

{ Draw axis (3 lines) around given position.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawAxisWire(const Position: TVector3Single; Size: Single); deprecated 'use TCastleScene to draw 3D stuff';

{ Call glColor, taking Opacity as separate Single argument.
  Deprecated, do not use colors like that, instead pass TCastleColor
  to appropriate routines like TCastleFont.Print.
  @groupBegin }
procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single); deprecated;
procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single); deprecated;
{ @groupEnd }
{$endif}

type
  { Primitive to draw using DrawPrimitive2D.
    The modes correspond to OpenGL drawing modes, see
    https://www.opengl.org/wiki/Primitive
    https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml }
  TPrimitiveMode = (
    pmPoints,
    pmLineStrip,
    pmLineLoop,
    pmLines,
    pmTriangleStrip,
    pmTriangleFan,
    pmTriangles
  );
  TBlendingSourceFactor = (
    bsSrcAlpha,
    bsOneMinusSrcAlpha,
    bsZero,
    bsOne,

    bsDstColor,
    bsSrcColor, //< As a source factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bsDstAlpha,
    bsOneMinusDstColor,
    bsOneMinusSrcColor, //< As a source factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bsOneMinusDstAlpha,

    bsSrcAlphaSaturate,

    bsConstantColor,
    bsOneMinusConstantColor,
    bsConstantAlpha,
    bsOneMinusConstantAlpha
  );
  TBlendingDestinationFactor = (
    bdSrcAlpha,
    bdOneMinusSrcAlpha,
    bdZero,
    bdOne,

    bdDstColor, //< As a destination factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bdSrcColor,
    bdDstAlpha,
    bdOneMinusDstColor, //< As a destination factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bdOneMinusSrcColor,
    bdOneMinusDstAlpha,

    // not supported by OpenGL for destination factor: bsSrcAlphaSaturate
    { }
    bdConstantColor,
    bdOneMinusConstantColor,
    bdConstantAlpha,
    bdOneMinusConstantAlpha
  );

{ Draw a rectangle that modulates colors underneath,
  making nice animation to FadeColor while FadeIntensity changes from 1.0
  down to 0.0.

  The GLFadeRectangleLight version makes a flash to FadeColor,
  then goes back to normal.
  The GLFadeRectangle version makes additional flash to blackness
  in the middle (so it goes from no modulation, to FadeColor,
  to pure black, and then back to normal).
  So it's a little more impressive when you're flashing with a dark color.

  These are nice as a screen effect, to flash some color (e.g. flash
  red color when the player is hurt).

  Only RGB portion of FadeColor is used. }
procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single;
  const FadeIntensity: Single); deprecated 'use TFlashEffect';
procedure GLFadeRectangle(const Rect: TRectangle;
  const FadeColor: TCastleColor;
  const FadeIntensity: Single); deprecated 'use TFlashEffect';
procedure GLFadeRectangleLight(const Rect: TRectangle;
  const FadeColor: TCastleColor;
  const FadeIntensity: Single); deprecated 'use TFlashEffect';

{ Draw a rectangle with blending.
  @deprecated Deprecated, use DrawRectangle instead. }
procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor;
  const Color: TVector4Single); deprecated;
procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4Single); deprecated;

{ Draw a simple rectangle filled with a color.
  Blending is automatically used if Color alpha < 1.

  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.  }
procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false);

{ Draw a simple 2D primitive with a given color.
  This can be used to draw a series of points, lines or triangles,
  depending on the @code(Mode) parameter.

  Blending is automatically used if Color alpha < 1.
  ForceBlending forces the usage of blending. When it is @false,
  we use blending only if Color[3] (alpha) < 1.

  The LineWidth is only used when Mode indicates lines.
  The PointSize is only used when Mode indicates points,
  and only on desktop OpenGL (not available on mobile OpenGLES).
  Moreover, their interpretation may be limited by the implementation
  if anti-aliasing is enabled (and may be even limited to 1,
  which is common on OpenGLES).
  See https://www.opengl.org/sdk/docs/man2/xhtml/glPointSize.xml ,
  https://www.opengl.org/sdk/docs/man/html/glLineWidth.xhtml . }
procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2SmallInt;
  const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor = bsSrcAlpha;
  const BlendingDestinationFactor: TBlendingDestinationFactor = bdOneMinusSrcAlpha;
  const ForceBlending: boolean = false;
  const LineWidth: Cardinal = 1;
  const PointSize: Cardinal = 1);

{ Utilities for display lists ---------------------------------------- }
{ Deprecated: all display list usage will be removed, since it doesn't
  exist in modern OpenGL and OpenGL ES. }

{$ifndef OpenGLES}

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

{$endif}

{ If Buffer <> 0 then it does glDeleteBuffers and sets Buffer to 0. }
procedure glFreeBuffer(var Buffer: TGLuint);

{ Set color and depth buffers writeable or not.
  This is just a shortcut for
  @longcode(#
    glDepthMask(Writeable);
    glColorMask(Writeable, Writeable, Writeable, Writeable);
  #) }
procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);

{ Draw the 2D GUI stuff (like following GUI images and TCastleFont)
  with lower-left corner in the X,Y pixel.
  It's not adviced to use this, better use TGLImageCore.Draw(X,Y)
  or TCastleFont.Print(X,Y,string) methods.
  @groupBegin }
procedure SetWindowPos(const X, Y: TGLint);
procedure SetWindowPos(const Value: TVector2i);
procedure SetWindowPosF(const X, Y: TGLfloat); deprecated;
procedure SetWindowPosZero; deprecated;
{ @groupEnd }

function GetWindowPos: TVector2i;
property WindowPos: TVector2i read GetWindowPos write SetWindowPos;

type
  TDepthRange = (drFull, drNear, drFar);

function GetDepthRange: TDepthRange;
procedure SetDepthRange(const Value: TDepthRange);

{ Use this to operate on OpenGL glDepthRange. For now, our engine has
  very simple use for this, for TPlayer.RenderOnTop. }
property DepthRange: TDepthRange read GetDepthRange write SetDepthRange;

type
  TEnableTextureTarget = (etNone, et2D, etCubeMap, et3D);

{ Enable exactly one (or none, for Target=etNone) OpenGL texture target.
  Always use this instead of manually calling @code(glDisable(GL_TEXTURE_2D)),
  @code(glEnable(GL_TEXTURE_2D)) and such. This makes sure to have at most
  one texture target enabled, and disable others.

  Remember that this state is different for every texture unit in OpenGL,
  in case you use multi-texturing.

  Remember that not all texture targets are guaranteed to be supported by OpenGL.
  Target=etNone and Target=et2D are always supported.
  For the rest, check appropriate GLFeatures property (before even
  creating a texture with such type). If you pass an unsupported target type
  to this procedure, it will be ignored (all targets will be disabled,
  like for Target=etNone).

  Note that this is only for fixed-function OpenGL pipeline.
  Shader pipeline completely ignores the enabled state of texture units. }
function GLEnableTexture(const Target: TEnableTextureTarget): boolean;

type
  { Scissor to clip displayed things, in addition to the global scissor
    affected by ScissorEnable / ScissorDisable.
    Always disable an enabled scissor (destructor does it automatically). }
  TScissor = class
  strict private
    FEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    { Rectangle to which we clip rendering. Empty by default (will clip everything,
      if you don't assign this!). Do not change this when scissor is enabled. }
    Rect: TRectangle;
    constructor Create;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

{ Enable or disable scissor.
  Always do it using these procedures, do not call glScissor or
  glEnable(GL_SCISSOR_TEST) / glDisable(GL_SCISSOR_TEST) yourself,
  or push/pop attrib. }
procedure ScissorEnable(const Rect: TRectangle);
procedure ScissorDisable;

function GetGlobalAmbient: TVector3Single;
procedure SetGlobalAmbient(const Value: TVector3Single);

{ Global ambient lighting. This is added to every 3D object color,
  multiplied by material ambient.

  The default value is (0.2, 0.2, 0.2). It matches default
  GL_LIGHT_MODEL_AMBIENT in fixed-function OpenGL.
  It also matches the required value of VRML 1.0 specification.
  For VRML 2.0 / X3D, lighting equations suggest that it should be zero. }
property GlobalAmbient: TVector3Single
  read GetGlobalAmbient write SetGlobalAmbient;

procedure GLBlendFunction(const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor);

{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_context.inc}

{$undef read_interface}

implementation

{$define read_implementation}

uses
  { Because of FPC 2.6.4 bugs (not present in FPC >= 3.0.0) we cannot use here
    the FGL unit. It breaks compilation of Lazarus packages, as compiling
    castle_window.lpk then accidentally wants to recompile CastleGLShaders too.
    In consequence, we use TFPObjectList instead of generic TFPGObjectList below.
    It works cool with FPC 3.0.0, so this will be remedied once we drop FPC 2.6.4
    compatibility. }
  Contnrs,
  CastleFilesUtils, CastleStringUtils, CastleGLVersion, CastleGLShaders,
  CastleLog, CastleApplicationProperties;

{$I castleglutils_information.inc}
{$I castleglutils_mipmaps.inc}
{$I castleglutils_context.inc}

procedure GLInformationInitialize;
begin
  FreeAndNil(GLVersion);
  GLVersion := TGLVersion.Create(PChar(glGetString(GL_VERSION)),
    PChar(glGetString(GL_VENDOR)), PChar(glGetString(GL_RENDERER)));

  {$ifndef OpenGLES}
  FreeAndNil(GLUVersion);
  { gluGetString is valid for version 1.1 or later }
  if Assigned(gluGetString) then
    GLUVersion := TGenericGLVersion.Create(gluGetString(GLU_VERSION)) else
    GLUVersion := TGenericGLVersion.Create('1.0');
  {$endif}

  FreeAndNil(GLFeatures);
  GLFeatures := TGLFeatures.Create;
end;

{$ifdef OpenGLES}
{ Based on GLExt unit in FPC. This function is missing from GLES header,
  which does not check for extensions presence at all. }
function glext_ExtensionSupported(const extension: String;
  const searchIn: String): Boolean;
var
  extensions: PChar;
  start: PChar;
  where, terminator: PChar;
begin
  if (Pos(' ', extension) <> 0) or (extension = '') then
  begin
    Result := FALSE;
    Exit;
  end;

  if searchIn = '' then extensions := PChar(glGetString(GL_EXTENSIONS))
  else extensions := PChar(searchIn);
  start := extensions;
  while TRUE do
  begin
    where := StrPos(start, PChar(extension));
    if where = nil then Break;
    terminator := Pointer(PtrUInt(where) + PtrUInt(Length(extension)));
    {$warnings off} { Stop warning about unportable PtrUInt conversions }
    if (where = start) or (PChar(Pointer(PtrUInt(where) - PtrUInt(1)))^ = ' ') then
    {$warnings on}
    begin
      if (terminator^ = ' ') or (terminator^ = #0) then
      begin
        Result := TRUE;
        Exit;
      end;
    end;
    start := terminator;
  end;
  Result := FALSE;
end;
{$endif}

constructor TGLFeatures.Create;
var
  SupportedExtensions: string;
begin
  inherited;

  {$ifndef OpenGLES}
  Version_1_2 := GLVersion.AtLeast(1, 2) and Load_GL_version_1_2;
  Version_1_3 := GLVersion.AtLeast(1, 3) and Load_GL_version_1_3;
  Version_1_4 := GLVersion.AtLeast(1, 4) and Load_GL_version_1_4;
  Version_1_5 := GLVersion.AtLeast(1, 5) and Load_GL_version_1_5;
  Version_2_0 := GLVersion.AtLeast(2, 0) and Load_GL_version_2_0;
  Version_2_1 := GLVersion.AtLeast(2, 1) and Load_GL_version_2_1;
  Version_3_0 := GLVersion.AtLeast(3, 0) and Load_GL_version_3_0;
  Version_3_1 := GLVersion.AtLeast(3, 1) and Load_GL_version_3_1;
  Version_3_2 := GLVersion.AtLeast(3, 2) and Load_GL_version_3_2;
  Version_3_3 := GLVersion.AtLeast(3, 3) and Load_GL_version_3_3;
  Version_4_0 := GLVersion.AtLeast(4, 0) and Load_GL_version_4_0;

  ARB_window_pos := Load_GL_ARB_window_pos;
  MESA_window_pos := Load_GL_MESA_window_pos;

  ARB_shadow := Load_GL_ARB_shadow;
  EXT_fog_coord := Load_GL_EXT_fog_coord;
  NV_multisample_filter_hint := Load_GL_NV_multisample_filter_hint;

  { We want to be able to render any scene --- so we have to be prepared
    that fog interpolation has to be corrected for perspective.
    TODO: this should be moved elsewhere, but where? }
  glHint(GL_FOG_HINT, GL_NICEST);
  {$endif}

  {$ifdef OpenGLES}
  CLAMP_TO_EDGE := GL_CLAMP_TO_EDGE;
  {$else}
  if Version_1_2 then
    CLAMP_TO_EDGE := GL_CLAMP_TO_EDGE else
    CLAMP_TO_EDGE := GL_CLAMP;
  {$endif}

  MaxTextureSize := glGetInteger(GL_MAX_TEXTURE_SIZE);

  { TODO: for OpenGLES, as well as non-shader pipeline, this can be actually
    infinite (in theory, of course even 8 lights at the same shape is slow).
    Make it configurable somewhere, e.g. at Attribtes.MaxLightsPerShape ? }
  MaxLights := {$ifdef OpenGLES} 8 {$else} glGetInteger(GL_MAX_LIGHTS) {$endif};

  {$ifdef OpenGLES}
  MaxTextureUnits := Max(
    glGetInteger(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS),
    glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS));
  {$else}
  if Version_1_3 then
    MaxTextureUnits := glGetInteger(GL_MAX_TEXTURE_UNITS) else
    MaxTextureUnits := 1;
  {$endif}

  MaxCubeMapTextureSize := 0;
  {$ifdef OpenGLES}
  TextureCubeMap := gsStandard;
  {$else}
  if Version_1_3 then
    TextureCubeMap := gsStandard else
  if Load_GL_ARB_texture_cube_map then
    TextureCubeMap := gsExtension else
    TextureCubeMap := gsNone;
  {$endif}
  if TextureCubeMap <> gsNone then
    MaxCubeMapTextureSize := glGetInteger(GL_MAX_CUBE_MAP_TEXTURE_SIZE);

  {$ifndef OpenGLES}
  if Version_1_2 then
    Texture3D := gsStandard else
  if Load_GL_EXT_texture3D then
    Texture3D := gsExtension else
  {$endif}
    Texture3D := gsNone;

  { calculate MaxTexture3DSize, eventually correct Texture3D if buggy }
  case Texture3D of
    {$ifndef OpenGLES}
    gsExtension: MaxTexture3DSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE_EXT);
    gsStandard : MaxTexture3DSize := glGetInteger(GL_MAX_3D_TEXTURE_SIZE);
    {$endif}
    gsNone     : MaxTexture3DSize := 0;
  end;
  if (MaxTexture3DSize = 0) and (Texture3D <> gsNone) then
  begin
    Texture3D := gsNone;
    if Log then WritelnLog('OpenGL', 'Buggy OpenGL 3D texture support: reported as supported, but GL_MAX_3D_TEXTURE_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
  end;

  // TODO: there is also such extension for OpenGL ES
  EXT_texture_filter_anisotropic := {$ifdef OpenGLES} false {$else}
    Load_GL_EXT_texture_filter_anisotropic {$endif};
  if EXT_texture_filter_anisotropic then
    MaxTextureMaxAnisotropyEXT := glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT) else
    MaxTextureMaxAnisotropyEXT := 0.0;

  ARB_occlusion_query := false;
  QueryCounterBits := 0;
  {$ifndef OpenGLES}
  ARB_occlusion_query := Load_GL_ARB_occlusion_query;
  if ARB_occlusion_query then
    glGetQueryivARB(GL_SAMPLES_PASSED_ARB, GL_QUERY_COUNTER_BITS_ARB, @QueryCounterBits);
  {$endif}

  { calculate GLFramebuffer }
  {$ifdef OpenGLES}
  Framebuffer := gsStandard;
  {$else}
  if Version_3_0 or Load_GL_ARB_framebuffer_object then
    Framebuffer := gsStandard else
  if Load_GL_EXT_framebuffer_object then
    Framebuffer := gsExtension else
    Framebuffer := gsNone;
  {$endif}

  if Framebuffer <> gsNone then
  begin
    MaxRenderbufferSize := glGetInteger(GL_MAX_RENDERBUFFER_SIZE { equal to GL_MAX_RENDERBUFFER_SIZE_EXT });
    if MaxRenderbufferSize = 0 then
    begin
      Framebuffer := gsNone;
      if Log then WritelnLog('OpenGL', 'Buggy OpenGL Framebuffer: reported as supported, but GL_MAX_RENDERBUFFER_SIZE[_EXT] is zero. (Bug may be found on Mesa 7.0.4.)');
    end;
  end else
    MaxRenderbufferSize := 0;

  { TODO: this should be completely configurable in shader pipeline.
    Make it configurable at Attributes.MaxClipPlanes? }
  MaxClipPlanes := {$ifdef OpenGLES} 8 {$else} glGetInteger(GL_MAX_CLIP_PLANES) {$endif};

  { calculate UseMultiTexturing: check extensions required for multitexturing.

    We used to require a couple of extensions for this:
    - EXT_texture_env_combine
    - ARB_multitexture
    - ARB_texture_env_dot3
    But GL version >= 1.3 is actually required for GL_subtract,
    and includes all above extensions in core. }
  UseMultiTexturing := {$ifdef OpenGLES} true {$else} Version_1_3 {$endif};

  FBOMultiSampling := {$ifdef OpenGLES} false {$else}
    { Is GL_ARB_framebuffer_object available? }
    (Framebuffer = gsStandard) and
    Load_GL_ARB_texture_multisample and
    (not GLVersion.BuggyFBOMultiSampling) {$endif};

  Multisample := {$ifdef OpenGLES} true {$else} Load_GL_ARB_multisample {$endif};
  if Multisample and (glGetInteger({$ifdef OpenGLES} GL_SAMPLE_BUFFERS {$else} GL_SAMPLE_BUFFERS_ARB {$endif}) <> 0) then
  begin
    CurrentMultiSampling := glGetInteger({$ifdef OpenGLES} GL_SAMPLES {$else} GL_SAMPLES_ARB {$endif});
    if CurrentMultiSampling <= 1 then
    begin
      WritelnWarning('MultiSampling', Format('We successfully got multi-sampling buffer, but only %d samples per pixel. This doesn''t make much sense, assuming buggy OpenGL implementation, and anti-aliasing may not work.',
        [CurrentMultiSampling]));
      CurrentMultiSampling := 1;
    end;
  end else
    CurrentMultiSampling := 1;

  SupportedExtensions := PChar(glGetString(GL_EXTENSIONS));

  PackedDepthStencil :=
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_packed_depth_stencil', SupportedExtensions)
    {$else} Load_GL_EXT_packed_depth_stencil
    {$endif};

  ShadowVolumesPossible := glGetInteger(GL_STENCIL_BITS) >= 4;

  TextureNonPowerOfTwo := {$ifdef OpenGLES} true {$else}
    Load_GL_ARB_texture_non_power_of_two or Version_2_0 {$endif};

  TextureCompression := [];

  {$ifndef OpenGLES}
  { on non-OpenGLES, we require ARB_texture_compression for *any* compression
    format, to have the necessary glCompressedTexImage2DARB call available }
  if Load_GL_ARB_texture_compression then
  {$endif}
  begin
    { See http://stackoverflow.com/questions/9148795/android-opengl-texture-compression
      and http://developer.android.com/guide/topics/graphics/opengl.html
      for possible GPU extensions for compression formats. }

    if glext_ExtensionSupported('GL_OES_texture_compression_S3TC', SupportedExtensions) or
       glext_ExtensionSupported('GL_EXT_texture_compression_s3tc', SupportedExtensions) or
       glext_ExtensionSupported('GL_NV_texture_compression_s3tc', SupportedExtensions) then
      TextureCompression += [tcDxt1_RGB, tcDxt1_RGBA, tcDxt3, tcDxt5];

    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt1', SupportedExtensions) then
      TextureCompression += [tcDxt1_RGB, tcDxt1_RGBA];
    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt3', SupportedExtensions) then
      TextureCompression += [tcDxt3];
    if glext_ExtensionSupported('GL_EXT_texture_compression_dxt5', SupportedExtensions) then
      TextureCompression += [tcDxt5];

    if glext_ExtensionSupported('GL_IMG_texture_compression_pvrtc', SupportedExtensions) then
      TextureCompression += [
        tcPvrtc1_4bpp_RGB,
        tcPvrtc1_2bpp_RGB,
        tcPvrtc1_4bpp_RGBA,
        tcPvrtc1_2bpp_RGBA];

    if glext_ExtensionSupported('GL_IMG_texture_compression_pvrtc2', SupportedExtensions) then
      TextureCompression += [
        tcPvrtc2_4bpp,
        tcPvrtc2_2bpp];

    if glext_ExtensionSupported('GL_AMD_compressed_ATC_texture', SupportedExtensions) or
       glext_ExtensionSupported('GL_ATI_texture_compression_atitc', SupportedExtensions) then
      TextureCompression += [tcATITC_RGB,
        tcATITC_RGBA_ExplicitAlpha, tcATITC_RGBA_InterpolatedAlpha];

    if glext_ExtensionSupported('GL_OES_compressed_ETC1_RGB8_texture', SupportedExtensions) then
      TextureCompression += [tcETC1];
  end;

  VertexBufferObject := {$ifdef OpenGLES} true {$else}
    Version_1_5 and not GLVersion.BuggyVBO {$endif};

  BlendConstant := {$ifdef OpenGLES} true {$else}
    { GL_CONSTANT_ALPHA is available as part of ARB_imaging, since GL 1.4
      as standard. glBlendColor is available since 1.2 as standard. }
    ((Version_1_2 and Load_GL_ARB_imaging) or Version_1_4) and not GLVersion.Fglrx {$endif};

  TextureFloat :=
    {$ifdef OpenGLES} false
    {$else} Load_GL_ATI_texture_float or Load_GL_ARB_texture_float
    {$endif};

  TextureDepth :=
    {$ifdef OpenGLES} glext_ExtensionSupported('GL_OES_depth_texture', SupportedExtensions)
    {$else} Load_GL_ARB_depth_texture
    {$endif};
end;

{ EOpenGLError, CheckGLErrors ------------------------------------------------ }

function GLErrorString(const ErrorCode: TGLenum; const AdditionalComment: string): string;
var
  S: string;
begin
  { Do not use gluErrorString, not available in OpenGL ES.
    Error decriptions below from
    http://www.khronos.org/opengles/sdk/docs/man/xhtml/glGetError.xml }
  case ErrorCode of
    GL_NO_ERROR: S := 'No error has been recorded.';
    GL_INVALID_ENUM: S := 'An unacceptable value is specified for an enumerated argument.';
    GL_INVALID_VALUE: S := 'A numeric argument is out of range.';
    GL_INVALID_OPERATION: S := 'The specified operation is not allowed in the current state.';
    GL_INVALID_FRAMEBUFFER_OPERATION: S := 'The command is trying to render to or read from the framebuffer while the currently bound framebuffer is not framebuffer complete (i.e. the return value from glCheckFramebufferStatus is not GL_FRAMEBUFFER_COMPLETE).';
    GL_OUT_OF_MEMORY: S := 'There is not enough memory left to execute the command.';
    else S := 'Unknown error.';
  end;

  if AdditionalComment <> '' then
    Result := AdditionalComment + nl else
    Result := '';
  Result += Format('OpenGL error (%d): %s', [ErrorCode, S]);
end;

constructor EOpenGLError.Create(const AErrorCode: TGLenum; const AdditionalComment: string);
begin
  ErrorCode := AErrorCode;
  inherited Create(GLErrorString(ErrorCode, AdditionalComment));
end;

procedure GLOutOfMemory(const AdditionalComment: string);
const
  ErrorCode = GL_OUT_OF_MEMORY;
begin
  if GLOutOfMemoryError then
    raise EOpenGLOutOfMemoryError.Create(ErrorCode, AdditionalComment) else
    WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
end;

procedure CheckGLErrors(const AdditionalComment: string);
var
  ErrorCode: TGLenum;
begin
  ErrorCode := glGetError();
  if ErrorCode <> GL_NO_ERROR then
  begin
    if ErrorCode = GL_OUT_OF_MEMORY then
      GLOutOfMemory(AdditionalComment) else
      {$ifdef RELEASE}
      WritelnWarning('OpenGL', GLErrorString(ErrorCode, AdditionalComment));
      {$else}
      raise EOpenGLError.Create(ErrorCode, AdditionalComment);
      {$endif}
  end;
end;

procedure GLErrorRaise(ErrorCode: TGLenum);
  {$ifdef OPENGL_CALLBACK_CDECL} cdecl; {$endif}
  {$ifdef OPENGL_CALLBACK_STDCALL} stdcall; {$endif}
begin
  if ErrorCode = GL_OUT_OF_MEMORY then
    GLOutOfMemory else
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

{ ---------------------------------------------------- }

var
  FCurrentColor: TCastleColor;

function GetCurrentColor: TCastleColor;
begin
  Result := FCurrentColor;
end;

procedure SetCurrentColor(const Value: TCastleColor);
begin
  FCurrentColor := Value;
end;

{$ifndef OpenGLES}

procedure glColorv(const v: TVector3ub);
begin
  glColor3ubv(@v);
  FCurrentColor := Vector4Single(Vector4Byte(V, 255));
end;

procedure glColorv(const v: TVector4ub);
begin
  glColor4ubv(@v);
  FCurrentColor := Vector4Single(V);
end;

procedure glColorv(const v: TVector3f);
begin
  glColor3fv(@v);
  FCurrentColor := Vector4Single(V);
end;

procedure glColorv(const v: TVector4f);
begin
  glColor4fv(@v);
  FCurrentColor := V;
end;

procedure glTranslatev(const V: TVector3f); begin glTranslatef(V[0], V[1], V[2]); end;

procedure glTranslatev(const V: TVector3_Single); begin glTranslatef(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glScalev(const V: Single); begin glScalef(V, V, V); end;

procedure glScalev(const V: TVector3f); begin glScalef(V[0], V[1], V[2]); end;

procedure glScalev(const V: TVector3_Single); begin glScalef(V.Data[0], V.Data[1], V.Data[2]); end;

procedure glRotatev(const Angle: TGLfloat;  const V: TVector3f); begin glRotatef(Angle, V[0], V[1], V[2]); end;

procedure glVertexv(const v: TVector2_Single);  begin glVertex2fv(@v.Data); end;

procedure glVertexv(const v: TVector3_Single);  begin glVertex3fv(@v.Data); end;

procedure glVertexv(const v: TVector4_Single);  begin glVertex4fv(@v.Data); end;

procedure glClipPlane(plane: GLenum; const V: TVector4d);
begin
  GL.glClipPlane(plane, @V);
end;

procedure glNormalv(const v: TVector3d); begin glNormal3dv(@v); end;
procedure glNormalv(const v: TVector3f); begin glNormal3fv(@v); end;

procedure glMaterialv(face, pname: TGLEnum; const params: TVector4f);  begin glMaterialfv(face, pname, @params); end;

procedure glVertexv(const v: TVector2f);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector2i);  begin glVertex2iv(@v); end;
procedure glVertexv(const v: TVector3f);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector3i);  begin glVertex3iv(@v); end;
procedure glVertexv(const v: TVector4f);  begin glVertex4fv(@v); end;
procedure glVertexv(const v: TVector4i);  begin glVertex4iv(@v); end;

procedure glTexCoordv(const v: TVector2f);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector3f);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector4f);  begin glTexCoord4fv(@v); end;

procedure glTexGenv(coord, pname: TGLenum; const params: TVector4f);  begin glTexGenfv(coord, pname, @params); end;

procedure glLightv(light, pname: TGLEnum; const params: TVector4f);  begin glLightfv(light, pname, @params); end;
procedure glLightv(light, pname: TGLEnum; const params: TVector3f);  begin glLightfv(light, pname, @params); end;

procedure glLightModelv(pname: TGLenum; const params: TVector4f); begin glLightModelfv(pname, @params); end;
procedure glLightModelv(pname: TGLenum; const params: TVector4i); begin glLightModeliv(pname, @params); end;

procedure glFogv(pname: TGLEnum; const params: TVector4f);  begin glFogfv(pname, @params); end;

procedure glMultMatrix(const m: TMatrix4f); begin glMultMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4f); begin glLoadMatrixf(@m) end;

procedure glTexEnvv(target, pname: TGLEnum; const params: TVector4f); begin glTexEnvfv(target, pname, @params); end;

{$endif}

procedure GLViewport(const Rect: TRectangle);
begin
  {$ifndef OpenGLES} GL {$else} CastleGLES20 {$endif}
    .glViewport(Rect.Left, Rect.Bottom, Rect.Width, Rect.Height);
end;

{ projection matrix ---------------------------------------------------------- }

var
  FProjectionMatrix: TMatrix4Single;

function GetProjectionMatrix: TMatrix4Single;
begin
  Result := FProjectionMatrix;
end;

procedure SetProjectionMatrix(const Value: TMatrix4Single);
begin
  FProjectionMatrix := Value;

  {$ifndef OpenGLES}
  glMatrixMode(GL_PROJECTION);
  glLoadMatrix(Value);
  glMatrixMode(GL_MODELVIEW);
  {$endif}
end;

function PerspectiveProjection(const fovy, aspect, zNear, zFar: Single): TMatrix4Single;
begin
  Result := PerspectiveProjMatrixDeg(fovy, aspect, zNear, zFar);
  ProjectionMatrix := Result;
end;

function OrthoProjection(const left, right, bottom, top, zNear, zFar: Single): TMatrix4Single;
begin
  Result := OrthoProjMatrix(left, right, bottom, top, zNear, zFar);
  ProjectionMatrix := Result;
end;

{ Various helpers ------------------------------------------------------------ }

procedure GLSetEnabled(value: TGLenum; isEnabled: boolean);
begin
  if isEnabled then glEnable(value) else glDisable(value);
end;

{$ifndef OpenGLES}
procedure GLVerticalLine(x, y1, y2: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x, y1); glVertex2f(x, y2); glEnd;
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  glBegin(GL_LINES); glVertex2f(x1, y); glVertex2f(x2, y); glEnd;
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

(* // These versions would work with OpenGLES. But they ignore OpenGL matrix state like glTransform etc.

procedure GLVerticalLine(x, y1, y2: TGLfloat);
begin
  DrawPrimitive2D(pmLines,
    [Vector2SmallInt(Round(x), Round(y1)),
     Vector2SmallInt(Round(x), Round(y2))],
    CurrentColor);
end;

procedure GLHorizontalLine(x1, x2, y: TGLfloat);
begin
  DrawPrimitive2D(pmLines,
    [Vector2SmallInt(Round(x1), Round(y)),
     Vector2SmallInt(Round(x2), Round(y))],
    CurrentColor);
end;

procedure GLDrawArrow(HeadThickness, HeadLength: TGLfloat);
begin
  HeadLength := 2*HeadLength; { mapuj HeadLength na zakres 0..2 }

  DrawPrimitive2D(pmTriangles,
    [Vector2SmallInt(0, 2),
     Vector2SmallInt(-1, Round(HeadLength)),
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),

     Vector2SmallInt(0, 2),
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength)),

     Vector2SmallInt(0, 2),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(1, Round(HeadLength)),

     // quad
     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(-Round(HeadThickness), 0),
     Vector2SmallInt(Round(HeadThickness), 0),

     Vector2SmallInt(-Round(HeadThickness), Round(HeadLength)),
     Vector2SmallInt(Round(HeadThickness), 0),
     Vector2SmallInt(Round(HeadThickness), Round(HeadLength))
    ],
    CurrentColor);
end;
*)

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
  {$warnings off} { deliberately using deprecated stuff inside another deprecated }
  Q := NewGLUQuadric(Texture, Normals, Orientation, DrawStyle);
  {$warnings on}
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

procedure glColorOpacity(const Color: TVector3Single; const Opacity: Single);
begin
  glColor4f(Color[0], Color[1], Color[2], Opacity);
end;

procedure glColorOpacity(const Color: TVector3Byte; const Opacity: Single);
begin
  glColor4f(Color[0] / 255, Color[1] / 255, Color[2] / 255, Opacity);
end;
{$endif}

procedure GLBlendRectangle(const X1, Y1, X2, Y2: Integer;
  const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor;
  const Color: TVector4Single);
begin
  DrawRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1), Color,
    SourceFactor, DestinationFactor, true);
end;

procedure GLBlendRectangle(const Rect: TRectangle;
  const Color: TVector4Single);
begin
  DrawRectangle(Rect, Color, bsOne, bdSrcAlpha, true);
end;

procedure GLFadeRectangle(const X1, Y1, X2, Y2: Integer;
  const FadeColor: TVector3Single; const FadeIntensity: Single);
begin
  {$warnings off}
  GLFadeRectangle(Rectangle(X1, Y1, X2 - X1, Y2 - Y1),
    Vector4Single(FadeColor, 1.0), FadeIntensity);
  {$warnings on}
end;

procedure GLFadeRectangle(const Rect: TRectangle;
  const FadeColor: TCastleColor; const FadeIntensity: Single);
const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  { We assume that MinScale is small enough that difference between
    "FadeColor * MinScale * screen color" and
    "MinScale * screen color" is not noticeable. }
  MinScale = 0.1;
  { Constants below make resulting screen color = glColor * previous screen color.
    Note that as long as all components of FadeColor are <= 1,
    then all components of our glColor are also always <= 1,
    and this means that we will always make the screen darker (or equal,
    but never brighter). }
  SourceFactor = bsZero;
  DestinationFactor = bdSrcColor;
var
  Color: TCastleColor;
begin
  if FadeIntensity > 0 then
  begin
    { for FadeIntensity in 1...FullWhiteEnd (going down):
      screen color := FadeColor * original screen color }
    if FadeIntensity > FullWhiteEnd then
      Color := FadeColor else
    { for FadeIntensity in FullWhiteEnd...FullBlack (going down):
      final screen color changes:
      - from screen color := FadeColor * original screen color
      - to   screen color := FadeColor * MinScale * original screen color }
    if FadeIntensity > FullBlack then
      Color := FadeColor * MapRange(FadeIntensity, FullWhiteEnd, FullBlack, 1, MinScale) else
    { for FadeIntensity in FullBlack...0 (going down):
      final screen color changes:
      - from screen color := MinScale * original screen color
      - to   screen color := original screen color }
      Color := White * MapRange(FadeIntensity, FullBlack, 0, MinScale, 1);

    Color[3] := 1.0; { alpha always 1.0 in this case }
    DrawRectangle(Rect, Color, SourceFactor, DestinationFactor, true);
  end;
end;

procedure GLFadeRectangleLight(const Rect: TRectangle;
  const FadeColor: TCastleColor; const FadeIntensity: Single);
const
  FullTime = 0.9;
  SourceFactor = bsZero;
  DestinationFactor = bdSrcColor;
var
  Color: TCastleColor;
  Intensity: Single;
begin
  if FadeIntensity > 0 then
  begin
    if FadeIntensity < FullTime then
      Intensity := MapRange(FadeIntensity, 0, FullTime, 0, 1) else
      Intensity := MapRange(FadeIntensity, FullTime, 1, 1, 0);
    Color := FadeColor;
    Color[3] := Intensity; { alpha always 1.0 in this case }
    DrawRectangle(Rect, Color, bsSrcAlpha, bdOneMinusSrcAlpha, true);
  end;
end;

{ DrawPrimitive2D ---------------------------------------------------------------- }

const
  BlendingSourceFactorToGL: array [TBlendingSourceFactor] of TGLEnum = (
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_ZERO,
    GL_ONE,

    GL_DST_COLOR,
    GL_SRC_COLOR,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_ONE_MINUS_DST_ALPHA,

    GL_SRC_ALPHA_SATURATE,

    GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR,
    GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA
  );
  BlendingDestinationFactorToGL: array [TBlendingDestinationFactor] of TGLEnum = (
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_ZERO,
    GL_ONE,

    GL_DST_COLOR,
    GL_SRC_COLOR,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_ONE_MINUS_DST_ALPHA,

    // GL_SRC_ALPHA_SATURATE, // not supported as destination factor

    GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR,
    GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA
  );
  PrimitiveModeToGL: array [TPrimitiveMode] of TGLEnum = (
    GL_POINTS,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_TRIANGLES
  );

var
  {$ifdef GLImageUseShaders}
  Primitive2DProgram: TGLSLProgram;
  Primitive2DUniformViewportSize, Primitive2DUniformColor: TGLSLUniform;
  Primitive2DAttribVertex: TGLSLAttribute;
  {$endif}
  Primitive2DVbo: TGLuint;
  Primitive2DPointPtr: Pointer;
  Primitive2DPointPtrSize: Integer;

procedure DrawPrimitive2D(const Mode: TPrimitiveMode;
  const Points: array of TVector2SmallInt; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean;
  const LineWidth: Cardinal;
  const PointSize: Cardinal);
var
  Blending: boolean;
  I, RequiredPrimitive2DPointPtrSize: Integer;
begin
  {$ifdef GLImageUseShaders}
  if Primitive2DProgram = nil then
  begin
    Primitive2DProgram := TGLSLProgram.Create;
    Primitive2DProgram.AttachVertexShader({$I primitive_2.vs.inc});
    Primitive2DProgram.AttachFragmentShader({$I primitive_2.fs.inc});
    Primitive2DProgram.Link;

    Primitive2DUniformViewportSize := Primitive2DProgram.Uniform('viewport_size');
    Primitive2DUniformColor := Primitive2DProgram.Uniform('color');
    Primitive2DAttribVertex := Primitive2DProgram.Attribute('vertex');
  end;
  {$endif}

  { apply LineWidth, PointSize.
    Their setters avoid doing anything when they already have the requested
    values, so we can just assign them here not worrying about performance. }
  RenderContext.LineWidth := LineWidth;
  RenderContext.PointSize := PointSize;

  Blending := ForceBlending or (Color[3] < 1);
  if Blending then
  begin
    glBlendFunc(
      BlendingSourceFactorToGL[BlendingSourceFactor],
      BlendingDestinationFactorToGL[BlendingDestinationFactor]);
    glEnable(GL_BLEND);
  end;

  if (Primitive2DVbo = 0) and GLFeatures.VertexBufferObject then
    glGenBuffers(1, @Primitive2DVbo);

  { make Primitive2DPointPtr have necessary size }
  RequiredPrimitive2DPointPtrSize := SizeOf(TVector2SmallInt) * (High(Points) + 1);
  if Primitive2DPointPtrSize <> RequiredPrimitive2DPointPtrSize then
  begin
    if Primitive2DPointPtr <> nil then
      FreeMem(Primitive2DPointPtr);
    Primitive2DPointPtr := GetMem(RequiredPrimitive2DPointPtrSize);
    Primitive2DPointPtrSize := RequiredPrimitive2DPointPtrSize;
  end;

  { copy Points to Primitive2DPointPtr }
  for I := 0 to High(Points) do
    Move(Points[I],
      PVector2SmallInt(PtrUInt(Primitive2DPointPtr) + SizeOf(TVector2SmallInt) * I)^,
      SizeOf(TVector2SmallInt));

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, Primitive2DVbo);
    glBufferData(GL_ARRAY_BUFFER, Primitive2DPointPtrSize, Primitive2DPointPtr, GL_STREAM_DRAW);
  end;

  {$ifdef GLImageUseShaders}
  CurrentProgram := Primitive2DProgram;
  Primitive2DAttribVertex.EnableArray(0, 2, GL_SHORT, GL_FALSE, SizeOf(TVector2SmallInt), 0);
  Primitive2DUniformViewportSize.SetValue(Viewport2DSize);
  Primitive2DUniformColor.SetValue(Color);

  {$else}
  CurrentProgram := nil;
  glLoadIdentity();
  glColorv(Color);

  glEnableClientState(GL_VERTEX_ARRAY);
  if GLFeatures.VertexBufferObject then
    glVertexPointer(2, GL_SHORT, SizeOf(TVector2SmallInt), nil) else
    glVertexPointer(2, GL_SHORT, SizeOf(TVector2SmallInt), Primitive2DPointPtr);
  {$endif}

  glDrawArrays(PrimitiveModeToGL[Mode], 0, High(Points) + 1);

  {$ifdef GLImageUseShaders}
  // Primitive2DProgram.Disable; // no need to disable, keep it enabled to save speed
  { attribute arrays are enabled independent from GLSL program, so we need
    to disable them separately }
  Primitive2DAttribVertex.DisableArray;
  {$else}
  glDisableClientState(GL_VERTEX_ARRAY);
  {$endif}

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;

  if Blending then
    glDisable(GL_BLEND);
end;

{ DrawRectangle ---------------------------------------------------------------- }

procedure DrawRectangle(const R: TRectangle; const Color: TCastleColor;
  const BlendingSourceFactor: TBlendingSourceFactor;
  const BlendingDestinationFactor: TBlendingDestinationFactor;
  const ForceBlending: boolean);
var
  RectanglePoint: array [0..3] of TVector2SmallInt;
begin
  RectanglePoint[0] := Vector2SmallInt(R.Left          , R.Bottom);
  RectanglePoint[1] := Vector2SmallInt(R.Left + R.Width, R.Bottom);
  RectanglePoint[2] := Vector2SmallInt(R.Left + R.Width, R.Bottom + R.Height);
  RectanglePoint[3] := Vector2SmallInt(R.Left          , R.Bottom + R.Height);

  DrawPrimitive2D(pmTriangleFan, RectanglePoint,
    Color, BlendingSourceFactor, BlendingDestinationFactor, ForceBlending);
end;

{$ifndef OpenGLES}

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

{$endif}

procedure glFreeBuffer(var Buffer: TGLuint);
begin
  if Buffer <> 0 then
  begin
    glDeleteBuffers(1, @Buffer);
    Buffer := 0;
  end;
end;

procedure glSetDepthAndColorWriteable(Writeable: TGLboolean);
begin
  glDepthMask(Writeable);
  glColorMask(Writeable, Writeable, Writeable, Writeable);
end;

var
  FWindowPos: TVector2Integer;

procedure SetWindowPosF(const X, Y: TGLfloat);
begin
  { Hack, only to somewhat support SetWindowPosF for old programs.
    SetWindowPosF should not be used in new code. }
  FWindowPos[0] := Floor(X);
  FWindowPos[1] := Floor(Y);
end;

procedure SetWindowPos(const X, Y: TGLint);
begin
  FWindowPos[0] := X;
  FWindowPos[1] := Y;
end;

procedure SetWindowPos(const Value: TVector2i);
begin
  SetWindowPos(Value[0], Value[1]);
end;

procedure SetWindowPosZero;
begin
  SetWindowPos(0, 0);
end;

function GetWindowPos: TVector2i;
begin
  Result := FWindowPos;
end;

var
  FDepthRange: TDepthRange = drFull;

function GetDepthRange: TDepthRange;
begin
  Result := FDepthRange;
end;

procedure SetDepthRange(const Value: TDepthRange);
begin
  if FDepthRange <> Value then
  begin
    {$ifdef OpenGLES} {$define glDepthRange := glDepthRangef} {$endif}
    FDepthRange := Value;
    case Value of
      drFull: glDepthRange(0  , 1);
      drNear: glDepthRange(0  , 0.1);
      drFar : glDepthRange(0.1, 1);
    end;
  end;
end;

function GLEnableTexture(const Target: TEnableTextureTarget): boolean;
begin
  Result := true;

  {$ifndef OpenGLES}

  case Target of
    etNone: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    et2D: begin
        glEnable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    etCubeMap: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glEnable(GL_TEXTURE_CUBE_MAP) else Result := false;
        if GLFeatures.Texture3D <> gsNone then glDisable(GL_TEXTURE_3D);
      end;
    et3D: begin
        glDisable(GL_TEXTURE_2D);
        if GLFeatures.TextureCubeMap <> gsNone then glDisable(GL_TEXTURE_CUBE_MAP);
        if GLFeatures.Texture3D <> gsNone then glEnable(GL_TEXTURE_3D) else Result := false;
      end;
    else raise EInternalError.Create('GLEnableTexture:Target?');
  end;

  {$endif}
end;

{ scissors ------------------------------------------------------------------- }

type
  TScissorList = class(TFPObjectList)
  //class(specialize TFPGObjectList<TScissor>) // see comments in "uses" clause -- we cannot use FGL unit here
  public
    procedure Update;
  end;

var
  EnabledScissors: TScissorList;

procedure TScissorList.Update;
var
  R: TRectangle;
  I: Integer;
begin
  if Count <> 0 then
  begin
    R := TScissor(Items[0]).Rect;
    for I := 1 to Count - 1 do
      R := R * TScissor(Items[I]).Rect;
    glScissor(R.Left, R.Bottom, R.Width, R.Height);
    glEnable(GL_SCISSOR_TEST);
  end else
    glDisable(GL_SCISSOR_TEST);
end;

constructor TScissor.Create;
begin
  inherited;
  Rect := TRectangle.Empty;
end;

destructor TScissor.Destroy;
begin
  Enabled := false;
  inherited;
end;

procedure TScissor.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if EnabledScissors <> nil then
    begin
      if Value then
        EnabledScissors.Add(Self) else
        EnabledScissors.Remove(Self);
      EnabledScissors.Update;
    end;
  end;
end;

var
  FGlobalScissor: TScissor;

procedure ScissorEnable(const Rect: TRectangle);
begin
  if FGlobalScissor = nil then
    FGlobalScissor := TScissor.Create else
    FGlobalScissor.Enabled := false; // disable previously enabled scissor, if any
  FGlobalScissor.Rect := Rect;
  FGlobalScissor.Enabled := true;
end;

procedure ScissorDisable;
begin
  if FGlobalScissor <> nil then // secure in case FGlobalScissor was already fred
    FGlobalScissor.Enabled := false;
end;

{ ---------------------------------------------------------------------------- }

var
  FGlobalAmbient: TVector3Single = (0.2, 0.2, 0.2);

function GetGlobalAmbient: TVector3Single;
begin
  Result := FGlobalAmbient;
end;

procedure SetGlobalAmbient(const Value: TVector3Single);
begin
  FGlobalAmbient := Value;

  {$ifndef OpenGLES}
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(FGlobalAmbient, 1.0));
  {$endif}
end;

procedure GLBlendFunction(const SourceFactor: TBlendingSourceFactor;
  const DestinationFactor: TBlendingDestinationFactor);
begin
  glBlendFunc(
    BlendingSourceFactorToGL[SourceFactor],
    BlendingDestinationFactorToGL[DestinationFactor]);
end;

procedure ContextClose;
begin
  glFreeBuffer(Primitive2DVbo);
  {$ifdef GLImageUseShaders}
  FreeAndNil(Primitive2DProgram);
  {$endif}
  FreeMem(Primitive2DPointPtr); // not really tied to OpenGL
  Primitive2DPointPtrSize := 0;

  { free things created by GLInformationInitialize }
  FreeAndNil(GLVersion);
  {$ifndef OpenGLES}
  FreeAndNil(GLUVersion);
  {$endif}
  FreeAndNil(GLFeatures);
end;

initialization
  { Our GLVersion, GLFeatures should be freed at the very end,
    as a lot of code uses them. So place ContextClose to be called last,
    OnGLContextClose[0].
    Every other unit initializion does OnGLContextClose.Add,
    so our initialization will stay as OnGLContextClose[0]. }
  ApplicationProperties.OnGLContextClose.Insert(0, @ContextClose);
  EnabledScissors := TScissorList.Create(false);
finalization
  FreeAndNil(EnabledScissors);
  FreeAndNil(FGlobalScissor);
end.
